pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);
static WINDER_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug)]
struct Winder {
    id: u64,
    in_thunk: Value,
    out_thunk: Value,
}

#[derive(Clone, Debug)]
struct ExHandler {
    handler: Value,
    guard_k: Option<K>,          // Some for guard handlers (return continuation)
    guard_winders: Option<Vec<Winder>>, // winder state at guard site
    guard_var: Option<String>,    // variable name for guard
    guard_clauses: Option<Vec<(Expr, Vec<Expr>)>>, // (test, body) pairs
    guard_env: Option<Env>,       // environment for guard clauses
}

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}.__{}", base, n)
}

#[derive(Debug, Clone, Copy, Default)]
struct Span {
    line: usize,
    col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, d > 0)
    Boolean(bool),
    Char(char),
    Str(String, bool), // (content, mutable)
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<RefCell<(Value, Value)>>),
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Expr>, Expr)>,
        def_env: Env,
    },
    CaseLambda {
        name: Option<String>,
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>, Env)>,
    },
    Continuation(Rc<Cont>, Vec<Winder>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    Void,
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

fn make_list(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

fn value_to_vec(v: &Value) -> Option<Vec<Value>> {
    let mut result = Vec::new();
    let mut current = v.clone();
    let mut seen = HashSet::new();
    loop {
        match &current {
            Value::List(elems) => {
                result.extend(elems.iter().cloned());
                return Some(result);
            }
            Value::Pair(p) => {
                let ptr = Rc::as_ptr(p) as usize;
                if !seen.insert(ptr) {
                    return None; // cycle
                }
                let (car, cdr) = {
                    let b = p.borrow();
                    (b.0.clone(), b.1.clone())
                };
                result.push(car);
                current = cdr;
            }
            _ => return None, // improper list
        }
    }
}

fn is_null(v: &Value) -> bool {
    matches!(v, Value::List(elems) if elems.is_empty())
}

fn is_pair(v: &Value) -> bool {
    match v {
        Value::Pair(_) => true,
        Value::List(elems) => !elems.is_empty(),
        _ => false,
    }
}

fn pair_car(v: &Value, span: Span) -> Result<Value, EvalError> {
    match v {
        Value::Pair(p) => {
            let b = p.borrow();
            Ok(b.0.clone())
        }
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::Type(format!("car: not a pair at {span}"))),
    }
}

fn pair_cdr(v: &Value, span: Span) -> Result<Value, EvalError> {
    match v {
        Value::Pair(p) => {
            let b = p.borrow();
            Ok(b.1.clone())
        }
        Value::List(elems) if !elems.is_empty() => {
            Ok(Value::List(elems[1..].to_vec()))
        }
        _ => Err(EvalError::Type(format!("cdr: not a pair at {span}"))),
    }
}

fn is_list_safe(v: &Value) -> bool {
    let mut current = v.clone();
    let mut seen = HashSet::new();
    loop {
        match &current {
            Value::List(_) => return true,
            Value::Pair(p) => {
                let ptr = Rc::as_ptr(p) as usize;
                if !seen.insert(ptr) {
                    return false; // cycle
                }
                let cdr = {
                    let b = p.borrow();
                    b.1.clone()
                };
                current = cdr;
            }
            _ => return false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(v) => {
                if v.fract() == 0.0 && v.is_finite() {
                    write!(f, "{:.1}", v)
                } else {
                    write!(f, "{}", v)
                }
            }
            Value::Rational(n, d) => write!(f, "{}/{}", n, d),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Char(c) => match c {
                '\n' => write!(f, "#\\newline"),
                ' ' => write!(f, "#\\space"),
                _ => write!(f, "#\\{c}"),
            },
            Value::Str(s, _) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Pair(p) => {
                let mut seen = HashSet::new();
                seen.insert(Rc::as_ptr(p) as usize);
                let (car, cdr) = {
                    let b = p.borrow();
                    (b.0.clone(), b.1.clone())
                };
                write!(f, "({car}")?;
                let mut current = cdr;
                loop {
                    match &current {
                        Value::List(elems) if elems.is_empty() => break,
                        Value::List(elems) => {
                            for e in elems {
                                write!(f, " {e}")?;
                            }
                            break;
                        }
                        Value::Pair(np) => {
                            let ptr = Rc::as_ptr(np) as usize;
                            if !seen.insert(ptr) {
                                write!(f, " ...")?;
                                break;
                            }
                            let (ncar, ncdr) = {
                                let b = np.borrow();
                                (b.0.clone(), b.1.clone())
                            };
                            write!(f, " {ncar}")?;
                            current = ncdr;
                        }
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Vector(elems) => {
                let elems = elems.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Record { type_name, fields, .. } => {
                write!(f, "#<record:{type_name}")?;
                for (name, val) in fields {
                    write!(f, " {name}={val}")?;
                }
                write!(f, ">")
            }
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::CaseLambda { .. } => write!(f, "#<procedure>"),
            Value::Continuation(..) => write!(f, "#<continuation>"),
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

impl Expr {
    fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        let cur_span = Span { line, col };
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(Token { kind: TokenKind::LParen, span: cur_span }); i += 1; col += 1; }
            ')' => { tokens.push(Token { kind: TokenKind::RParen, span: cur_span }); i += 1; col += 1; }
            '\'' => { tokens.push(Token { kind: TokenKind::Quote, span: cur_span }); i += 1; col += 1; }
            '"' => {
                i += 1; col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1; col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        if chars[i] == '\n' { line += 1; col = 0; }
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("unterminated string at {cur_span}")));
                }
                i += 1; col += 1;
                tokens.push(Token { kind: TokenKind::Str(s), span: cur_span });
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token { kind: TokenKind::Boolean(true), span: cur_span });
                            i += 2; col += 2;
                        }
                        'f' => {
                            tokens.push(Token { kind: TokenKind::Boolean(false), span: cur_span });
                            i += 2; col += 2;
                        }
                        '\\' => {
                            // Character literal: #\x, #\newline, #\space
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse(format!("unexpected end of input in character literal at {cur_span}")));
                            }
                            // Try to read a named character or single character
                            let start = i;
                            if chars[i].is_alphabetic() {
                                while i < chars.len() && chars[i].is_alphabetic() {
                                    i += 1; col += 1;
                                }
                                let name: String = chars[start..i].iter().collect();
                                let ch = if name.len() == 1 {
                                    name.chars().next().unwrap()
                                } else {
                                    match name.as_str() {
                                        "newline" => '\n',
                                        "space" => ' ',
                                        "tab" => '\t',
                                        _ => return Err(EvalError::Parse(format!("unknown character name: {name} at {cur_span}"))),
                                    }
                                };
                                tokens.push(Token { kind: TokenKind::Char(ch), span: cur_span });
                            } else {
                                tokens.push(Token { kind: TokenKind::Char(chars[i]), span: cur_span });
                                i += 1; col += 1;
                            }
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{} at {cur_span}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse(format!("unexpected # at {cur_span}")));
                }
            }
            c if c == '-' || c == '+' => {
                if i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    let start = i;
                    let sign: i64 = if c == '-' { -1 } else { 1 };
                    i += 1; col += 1;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        i += 1; col += 1;
                    }
                    if i < chars.len() && chars[i] == '/' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                        let numer_str: String = chars[start..i].iter().collect();
                        i += 1; col += 1;
                        let denom_start = i;
                        while i < chars.len() && chars[i].is_ascii_digit() { i += 1; col += 1; }
                        let denom_str: String = chars[denom_start..i].iter().collect();
                        let numer: i64 = numer_str.parse().map_err(|_| EvalError::Parse(format!("invalid number at {cur_span}")))?;
                        let denom: i64 = denom_str.parse().map_err(|_| EvalError::Parse(format!("invalid number at {cur_span}")))?;
                        tokens.push(Token { kind: TokenKind::Rational(numer, denom), span: cur_span });
                    } else if i < chars.len() && chars[i] == '.' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                        i += 1; col += 1;
                        while i < chars.len() && chars[i].is_ascii_digit() { i += 1; col += 1; }
                        let float_str: String = chars[start..i].iter().collect();
                        let f: f64 = float_str.parse().map_err(|_| EvalError::Parse(format!("invalid float at {cur_span}")))?;
                        tokens.push(Token { kind: TokenKind::Float(f), span: cur_span });
                    } else {
                        let num_str: String = chars[start..i].iter().collect();
                        tokens.push(Token {
                            kind: TokenKind::Integer(num_str.parse().map_err(|_| {
                                EvalError::Parse(format!("invalid number: {num_str} at {cur_span}"))
                            })?),
                            span: cur_span,
                        });
                    }
                } else {
                    let start = i;
                    i += 1; col += 1;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1; col += 1;
                    }
                    let sym: String = chars[start..i].iter().collect();
                    tokens.push(Token { kind: TokenKind::Symbol(sym), span: cur_span });
                }
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1; col += 1;
                }
                if i < chars.len() && chars[i] == '/' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    let numer_str: String = chars[start..i].iter().collect();
                    i += 1; col += 1;
                    let denom_start = i;
                    while i < chars.len() && chars[i].is_ascii_digit() { i += 1; col += 1; }
                    let denom_str: String = chars[denom_start..i].iter().collect();
                    let numer: i64 = numer_str.parse().map_err(|_| EvalError::Parse(format!("invalid number at {cur_span}")))?;
                    let denom: i64 = denom_str.parse().map_err(|_| EvalError::Parse(format!("invalid number at {cur_span}")))?;
                    tokens.push(Token { kind: TokenKind::Rational(numer, denom), span: cur_span });
                } else if i < chars.len() && chars[i] == '.' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    i += 1; col += 1;
                    while i < chars.len() && chars[i].is_ascii_digit() { i += 1; col += 1; }
                    let float_str: String = chars[start..i].iter().collect();
                    let f: f64 = float_str.parse().map_err(|_| EvalError::Parse(format!("invalid float at {cur_span}")))?;
                    tokens.push(Token { kind: TokenKind::Float(f), span: cur_span });
                } else {
                    let num_str: String = chars[start..i].iter().collect();
                    tokens.push(Token {
                        kind: TokenKind::Integer(num_str.parse().map_err(|_| {
                            EvalError::Parse(format!("invalid number: {num_str} at {cur_span}"))
                        })?),
                        span: cur_span,
                    });
                }
            }
            '.' => {
                if i + 2 < chars.len() && chars[i + 1] == '.' && chars[i + 2] == '.' {
                    tokens.push(Token { kind: TokenKind::Symbol("...".into()), span: cur_span });
                    i += 3; col += 3;
                } else {
                    tokens.push(Token { kind: TokenKind::Symbol(".".into()), span: cur_span });
                    i += 1; col += 1;
                }
            }
            c if is_symbol_start(c) => {
                let start = i;
                while i < chars.len() && is_symbol_char(chars[i]) {
                    i += 1; col += 1;
                }
                let sym: String = chars[start..i].iter().collect();
                tokens.push(Token { kind: TokenKind::Symbol(sym), span: cur_span });
            }
            c => return Err(EvalError::Parse(format!("unexpected character: {c} at {cur_span}"))),
        }
    }
    Ok(tokens)
}

fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || "!$%&*/<=>?^_~".contains(c)
}

fn is_symbol_char(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit() || "+-.:@#".contains(c)
}

// --- Parser ---

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let span = tokens[*pos].span;
    match &tokens[*pos].kind {
        TokenKind::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::new(ExprKind::Integer(n), span)) }
        TokenKind::Float(f) => { let f = *f; *pos += 1; Ok(Expr::new(ExprKind::Float(f), span)) }
        TokenKind::Rational(n, d) => { let (n, d) = (*n, *d); *pos += 1; Ok(Expr::new(ExprKind::Rational(n, d), span)) }
        TokenKind::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::new(ExprKind::Boolean(b), span)) }
        TokenKind::Char(c) => { let c = *c; *pos += 1; Ok(Expr::new(ExprKind::Char(c), span)) }
        TokenKind::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Str(s), span)) }
        TokenKind::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Symbol(s), span)) }
        TokenKind::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::new(ExprKind::List(vec![Expr::new(ExprKind::Symbol("quote".into()), span), inner]), span))
        }
        TokenKind::LParen => {
            *pos += 1;
            let mut list = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos].kind, TokenKind::RParen) {
                list.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("missing closing paren at {span}")));
            }
            *pos += 1;
            Ok(Expr::new(ExprKind::List(list), span))
        }
        TokenKind::RParen => Err(EvalError::Parse(format!("unexpected ) at {span}"))),
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Environment ---

#[derive(Debug, Clone)]
struct Env {
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Env>>,
}

impl Env {
    fn new() -> Self {
        Env { bindings: Rc::new(RefCell::new(HashMap::new())), parent: None }
    }

    fn with_parent(parent: Env) -> Self {
        Env { bindings: Rc::new(RefCell::new(HashMap::new())), parent: Some(Rc::new(parent)) }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.bindings.borrow().get(name) {
            Some(v.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    fn set_existing(&self, name: &str, val: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.set_existing(name, val)
        } else {
            false
        }
    }
}

fn default_env() -> Env {
    let env = Env::new();
    for name in [
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "string-copy",
        "string->list", "list->string",
        "char->integer", "integer->char",
        "apply",
        "eq?", "equal?", "map",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?",
        "string-upcase", "string-downcase",
        "integer?", "rational?", "exact?", "inexact?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
        "procedure?", "eqv?",
        "vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
        "vector->list", "list->vector",
        "set-car!", "set-cdr!",
        "for-each",
        "caar", "cadr", "cdar", "cddr",
        "assv", "gcd", "lcm", "truncate", "round",
        "make-string", "string",
        "string>?", "string<=?", "string>=?",
        "member", "reverse",
        "error",
        "call/cc", "call-with-current-continuation",
        "dynamic-wind",
        "raise", "with-exception-handler",
    ] {
        env.set(name.into(), Value::Builtin(name.into()));
    }
    env
}

// --- Evaluator ---

fn display_value(v: &Value, out: &mut String) {
    display_value_safe(v, out, &mut HashSet::new());
}

fn display_value_safe(v: &Value, out: &mut String, seen: &mut HashSet<usize>) {
    match v {
        Value::Str(s, _) => out.push_str(s),
        Value::List(elems) => {
            out.push('(');
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                display_value_safe(e, out, seen);
            }
            out.push(')');
        }
        Value::Pair(p) => {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                out.push_str("(...)");
                return;
            }
            let (car, cdr) = {
                let b = p.borrow();
                (b.0.clone(), b.1.clone())
            };
            out.push('(');
            display_value_safe(&car, out, seen);
            let mut current = cdr;
            loop {
                match &current {
                    Value::List(elems) if elems.is_empty() => break,
                    Value::List(elems) => {
                        for e in elems {
                            out.push(' ');
                            display_value_safe(e, out, seen);
                        }
                        break;
                    }
                    Value::Pair(np) => {
                        let nptr = Rc::as_ptr(np) as usize;
                        if !seen.insert(nptr) {
                            out.push_str(" ...");
                            break;
                        }
                        let (ncar, ncdr) = {
                            let b = np.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        out.push(' ');
                        display_value_safe(&ncar, out, seen);
                        current = ncdr;
                    }
                    other => {
                        out.push_str(" . ");
                        display_value_safe(other, out, seen);
                        break;
                    }
                }
            }
            out.push(')');
        }
        Value::Vector(elems) => {
            let elems = elems.borrow();
            out.push_str("#(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                display_value(e, out);
            }
            out.push(')');
        }
        other => out.push_str(&other.to_string()),
    }
}

fn write_value_safe(v: &Value, out: &mut String, seen: &mut HashSet<usize>, write_mode: bool) {
    match v {
        Value::Str(s, _) if write_mode => {
            out.push('"');
            out.push_str(s);
            out.push('"');
        }
        Value::Str(s, _) => out.push_str(s),
        Value::List(elems) => {
            out.push('(');
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                write_value_safe(e, out, seen, write_mode);
            }
            out.push(')');
        }
        Value::Pair(p) => {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                out.push_str("(...)");
                return;
            }
            let (car, cdr) = {
                let b = p.borrow();
                (b.0.clone(), b.1.clone())
            };
            out.push('(');
            write_value_safe(&car, out, seen, write_mode);
            let mut current = cdr;
            loop {
                match &current {
                    Value::List(elems) if elems.is_empty() => break,
                    Value::List(elems) => {
                        for e in elems {
                            out.push(' ');
                            write_value_safe(e, out, seen, write_mode);
                        }
                        break;
                    }
                    Value::Pair(np) => {
                        let nptr = Rc::as_ptr(np) as usize;
                        if !seen.insert(nptr) {
                            out.push_str(" ...");
                            break;
                        }
                        let (ncar, ncdr) = {
                            let b = np.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        out.push(' ');
                        write_value_safe(&ncar, out, seen, write_mode);
                        current = ncdr;
                    }
                    other => {
                        out.push_str(" . ");
                        write_value_safe(other, out, seen, write_mode);
                        break;
                    }
                }
            }
            out.push(')');
        }
        Value::Vector(elems) => {
            let elems = elems.borrow();
            out.push_str("#(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                write_value_safe(e, out, seen, write_mode);
            }
            out.push(')');
        }
        other => out.push_str(&other.to_string()),
    }
}

fn setup_lambda_env(
    func: &Value,
    name: &Option<String>,
    params: &[String],
    rest_param: &Option<String>,
    body: &[Expr],
    closure_env: &Env,
    args: &[Value],
    call_span: Span,
    output: &mut String,
) -> Result<(Env, usize), EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {} at {call_span}", params.len(), args.len()
            )));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "expected {} arguments, got {} at {call_span}", params.len(), args.len()
        )));
    }
    let mut local_env = Env::with_parent(closure_env.clone());
    if let Some(n) = name {
        local_env.set(n.clone(), func.clone());
    }
    for (p, a) in params.iter().zip(args.iter()) {
        local_env.set(p.clone(), a.clone());
    }
    if let Some(rp) = rest_param {
        local_env.set(rp.clone(), make_list(args[params.len()..].to_vec()));
    }
    // Handle internal defines
    let mut define_count = 0;
    for expr in body {
        if let ExprKind::List(elems) = &expr.kind {
            if let Some(first) = elems.first() {
                if let ExprKind::Symbol(s) = &first.kind {
                    if s == "define" {
                        define_count += 1;
                        continue;
                    }
                }
            }
        }
        break;
    }
    for expr in &body[..define_count] {
        eval(expr, &mut local_env, output)?;
    }
    if define_count > 1 {
        let define_names: Vec<String> = body[..define_count].iter().filter_map(|expr| {
            if let ExprKind::List(elems) = &expr.kind {
                match &elems[1].kind {
                    ExprKind::Symbol(n) => Some(n.clone()),
                    ExprKind::List(sig) if !sig.is_empty() => {
                        if let ExprKind::Symbol(n) = &sig[0].kind { Some(n.clone()) } else { None }
                    }
                    _ => None,
                }
            } else { None }
        }).collect();
        let final_bindings: Vec<(String, Value)> = define_names.iter()
            .filter_map(|n| local_env.bindings.borrow().get(n).map(|v| (n.clone(), v.clone())))
            .collect();
        for dn in &define_names {
            let mut val = local_env.bindings.borrow_mut().remove(dn);
            if let Some(Value::Lambda { env: ref mut ce, .. }) = val {
                for (sib_name, sib_val) in &final_bindings {
                    ce.set(sib_name.clone(), sib_val.clone());
                }
            }
            if let Some(v) = val {
                local_env.bindings.borrow_mut().insert(dn.clone(), v);
            }
        }
    }
    Ok((local_env, define_count))
}

// --- CEK machine types for first-class continuations ---

type K = Rc<Cont>;

#[derive(Clone)]
enum Cont {
    Halt,
    Seq(Vec<Expr>, Env, K),
    Def(String, Env, K),
    SetK(String, Env, Span, K),
    IfK(Expr, Option<Expr>, Env, K),
    EvOp(Vec<Expr>, Env, Span, K),
    EvArg(Value, Vec<Value>, Vec<Expr>, Env, Span, K),
    AndK(Vec<Expr>, Env, K),
    OrK(Vec<Expr>, Env, K),
    LetBind {
        rest: Vec<(String, Expr)>,
        names: Vec<String>,
        vals: Vec<Value>,
        body: Vec<Expr>,
        named: Option<String>,
        outer_env: Env,
        next: K,
    },
    LetStarBind(String, Vec<(String, Expr)>, Vec<Expr>, Env, K),
    LetrecBind {
        rest_inits: Vec<Expr>,
        done_vals: Vec<Value>,
        names: Vec<String>,
        body: Vec<Expr>,
        env: Env,
        next: K,
    },
    LetrecStarBind(String, Vec<(String, Expr)>, Vec<Expr>, Env, K),
    CaseK(Vec<Expr>, Env, Span, K),
    CondK {
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: Env,
        span: Span,
        next: K,
    },
    StrSetIdxK(String, Expr, Env, Span, K),
    StrSetCharK(String, usize, Env, Span, K),
    CallCCK(K),
    WithExHandlerBody(K),  // body returned; pop handler, continue
    GuardBody(K),          // guard body returned normally; pop handler, continue
    GuardEvalClause {
        var: String,
        exn: Value,
        clauses: Vec<(Expr, Vec<Expr>)>, // remaining (test, body) pairs
        env: Env,
        next: K,
    },
    GuardTestResult {
        var: String,
        exn: Value,
        body: Vec<Expr>,             // body of current clause
        remaining: Vec<(Expr, Vec<Expr>)>, // remaining clauses
        env: Env,
        next: K,
    },
    DynWindBody(Value, Value, Value, K),  // (in_thunk, body_thunk, out_thunk, next)
    DynWindOut(Value, Value, K),          // (body_result, out_thunk, next)
    DynWindDone(Value, K),               // (body_result, next)
    DynWindDoUnwind {
        out_thunks: Vec<Value>,
        rewind_winders: Vec<Winder>,
        target_winders: Vec<Winder>,
        val: Value,
        saved_k: K,
    },
    DynWindDoRewind {
        remaining: Vec<Winder>,
        target_winders: Vec<Winder>,
        val: Value,
        saved_k: K,
    },
}

impl fmt::Debug for Cont {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<cont>")
    }
}

enum CekState {
    Eval(Expr, Env),
    Ret(Value),
}

fn setup_lambda_env_basic(
    func: &Value,
    name: &Option<String>,
    params: &[String],
    rest_param: &Option<String>,
    closure_env: &Env,
    args: &[Value],
    call_span: Span,
) -> Result<Env, EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {} at {call_span}", params.len(), args.len()
            )));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "expected {} arguments, got {} at {call_span}", params.len(), args.len()
        )));
    }
    let mut local_env = Env::with_parent(closure_env.clone());
    if let Some(n) = name {
        local_env.set(n.clone(), func.clone());
    }
    for (p, a) in params.iter().zip(args.iter()) {
        local_env.set(p.clone(), a.clone());
    }
    if let Some(rp) = rest_param {
        local_env.set(rp.clone(), make_list(args[params.len()..].to_vec()));
    }
    Ok(local_env)
}

fn cek_apply_func(
    func: &Value, args: &[Value], span: Span,
    st: &mut CekState, k: &mut K, winders: &mut Vec<Winder>, handlers: &mut Vec<ExHandler>, output: &mut String,
) -> Result<(), EvalError> {
    match func {
        Value::Lambda { name, params, rest_param, body, env: closure_env } => {
            let local_env = setup_lambda_env_basic(func, name, params, rest_param, closure_env, args, span)?;
            if body.is_empty() {
                *st = CekState::Ret(Value::Void);
            } else if body.len() == 1 {
                *st = CekState::Eval(body[0].clone(), local_env);
            } else {
                *k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), k.clone()));
                *st = CekState::Eval(body[0].clone(), local_env);
            }
            Ok(())
        }
        Value::CaseLambda { name, clauses } => {
            for (params, rest_param, body, clause_env) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let lambda = Value::Lambda {
                        name: name.clone(),
                        params: params.clone(),
                        rest_param: rest_param.clone(),
                        body: body.clone(),
                        env: clause_env.clone(),
                    };
                    return cek_apply_func(&lambda, args, span, st, k, winders, handlers, output);
                }
            }
            Err(EvalError::Arity(format!(
                "no matching clause for {} arguments at {span}", args.len()
            )))
        }
        Value::Builtin(ref bname) if bname == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("apply requires at least 2 arguments at {span}")));
            }
            let f = &args[0];
            let last = &args[args.len() - 1];
            let tail = value_to_vec(last).ok_or_else(|| EvalError::Type(format!("apply: last argument must be a list at {span}")))?;
            let mut combined: Vec<Value> = args[1..args.len() - 1].to_vec();
            combined.extend(tail);
            cek_apply_func(f, &combined, span, st, k, winders, handlers, output)
        }
        Value::Builtin(ref bname) if bname == "call/cc" || bname == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("call/cc requires 1 argument at {span}")));
            }
            let continuation_val = Value::Continuation(k.clone(), winders.clone());
            cek_apply_func(&args[0], &[continuation_val], span, st, k, winders, handlers, output)
        }
        Value::Builtin(ref bname) if bname == "raise" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("raise requires 1 argument at {span}")));
            }
            let exn = args[0].clone();
            if let Some(handler) = handlers.pop() {
                if let Some(guard_k) = handler.guard_k {
                    // Guard handler: unwind to guard site, then evaluate clauses
                    let guard_winders = handler.guard_winders.unwrap();
                    let guard_var = handler.guard_var.unwrap();
                    let guard_clauses = handler.guard_clauses.unwrap();
                    let guard_env = handler.guard_env.unwrap();
                    // Unwind dynamic-wind to guard's winder state
                    let common_len = winders.iter().zip(guard_winders.iter())
                        .take_while(|(a, b)| a.id == b.id)
                        .count();
                    let out_thunks: Vec<Value> = winders[common_len..].iter().rev()
                        .map(|w| w.out_thunk.clone()).collect();
                    winders.truncate(common_len);
                    // After unwinding, evaluate guard clauses
                    let clause_k = Rc::new(Cont::GuardEvalClause {
                        var: guard_var,
                        exn: exn.clone(),
                        clauses: guard_clauses,
                        env: guard_env,
                        next: guard_k,
                    });
                    if !out_thunks.is_empty() {
                        // Need to unwind first
                        *k = Rc::new(Cont::DynWindDoUnwind {
                            out_thunks: out_thunks[1..].to_vec(),
                            rewind_winders: vec![],
                            target_winders: guard_winders,
                            val: exn,
                            saved_k: clause_k,
                        });
                        cek_apply_func(&out_thunks[0], &[], span, st, k, winders, handlers, output)
                    } else {
                        *winders = guard_winders;
                        *k = clause_k;
                        // Trigger clause evaluation by returning a dummy value
                        *st = CekState::Ret(Value::Void);
                        Ok(())
                    }
                } else {
                    // with-exception-handler: invoke handler procedure with exn
                    let handler_fn = handler.handler;
                    cek_apply_func(&handler_fn, &[exn], span, st, k, winders, handlers, output)
                }
            } else {
                Err(EvalError::Raised(Box::new(exn)))
            }
        }
        Value::Builtin(ref bname) if bname == "with-exception-handler" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("with-exception-handler requires 2 arguments at {span}")));
            }
            let handler_fn = args[0].clone();
            let body_thunk = args[1].clone();
            handlers.push(ExHandler {
                handler: handler_fn,
                guard_k: None,
                guard_winders: None,
                guard_var: None,
                guard_clauses: None,
                guard_env: None,
            });
            *k = Rc::new(Cont::WithExHandlerBody(k.clone()));
            cek_apply_func(&body_thunk, &[], span, st, k, winders, handlers, output)
        }
        Value::Builtin(ref bname) if bname == "dynamic-wind" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("dynamic-wind requires 3 arguments at {span}")));
            }
            let (in_thunk, body_thunk, out_thunk) = (args[0].clone(), args[1].clone(), args[2].clone());
            *k = Rc::new(Cont::DynWindBody(in_thunk.clone(), body_thunk, out_thunk, k.clone()));
            cek_apply_func(&in_thunk, &[], span, st, k, winders, handlers, output)
        }
        Value::Builtin(ref bname) => {
            let result = apply_builtin(bname, args, span, output)?;
            *st = CekState::Ret(result);
            Ok(())
        }
        Value::Continuation(saved_k, saved_winders) => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("continuation requires 1 argument at {span}")));
            }
            let val = args[0].clone();
            // Compute common prefix of winders
            let common_len = winders.iter().zip(saved_winders.iter())
                .take_while(|(a, b)| a.id == b.id)
                .count();
            // Out-thunks to call: current winders beyond common prefix, innermost first
            let out_thunks: Vec<Value> = winders[common_len..].iter().rev()
                .map(|w| w.out_thunk.clone()).collect();
            // Winders to rewind: saved winders beyond common prefix, outermost first
            let rewind_winders: Vec<Winder> = saved_winders[common_len..].to_vec();
            let target_winders = saved_winders.clone();
            if out_thunks.is_empty() && rewind_winders.is_empty() {
                *k = saved_k.clone();
                *st = CekState::Ret(val);
            } else if !out_thunks.is_empty() {
                // Start unwinding: pop current winders and call out-thunks
                winders.truncate(common_len);
                let first_out = out_thunks[0].clone();
                *k = Rc::new(Cont::DynWindDoUnwind {
                    out_thunks: out_thunks[1..].to_vec(),
                    rewind_winders,
                    target_winders,
                    val,
                    saved_k: saved_k.clone(),
                });
                cek_apply_func(&first_out, &[], span, st, k, winders, handlers, output)?;
            } else {
                // No unwinding needed, start rewinding
                let first_winder = rewind_winders[0].clone();
                winders.push(first_winder.clone());
                *k = Rc::new(Cont::DynWindDoRewind {
                    remaining: rewind_winders[1..].to_vec(),
                    target_winders,
                    val,
                    saved_k: saved_k.clone(),
                });
                cek_apply_func(&first_winder.in_thunk, &[], span, st, k, winders, handlers, output)?;
            }
            Ok(())
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func} at {span}"))),
    }
}

fn desugar_do(elems: &[Expr], span: Span) -> Result<Expr, EvalError> {
    if elems.len() < 3 {
        return Err(EvalError::Syntax(format!("do requires var-clauses and test at {span}")));
    }
    let var_clauses = match &elems[1].kind {
        ExprKind::List(cs) => cs,
        _ => return Err(EvalError::Syntax(format!("do: expected var clauses at {span}"))),
    };
    let test_clause = match &elems[2].kind {
        ExprKind::List(tc) if !tc.is_empty() => tc,
        _ => return Err(EvalError::Syntax(format!("do: expected test clause at {span}"))),
    };
    let body = &elems[3..];
    let loop_name = "__do_loop";
    let mut bindings = Vec::new();
    let mut step_exprs = Vec::new();
    for vc in var_clauses {
        match &vc.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let var = parts[0].clone();
                let init = parts[1].clone();
                let step = if parts.len() >= 3 { parts[2].clone() } else { var.clone() };
                bindings.push(Expr::new(ExprKind::List(vec![var, init]), span));
                step_exprs.push(step);
            }
            _ => return Err(EvalError::Syntax(format!("do: bad var clause at {span}"))),
        }
    }
    let then_branch = if test_clause.len() > 1 {
        let mut v = vec![Expr::new(ExprKind::Symbol("begin".into()), span)];
        v.extend(test_clause[1..].iter().cloned());
        Expr::new(ExprKind::List(v), span)
    } else {
        Expr::new(ExprKind::List(vec![Expr::new(ExprKind::Symbol("begin".into()), span)]), span)
    };
    let mut loop_call = vec![Expr::new(ExprKind::Symbol(loop_name.into()), span)];
    loop_call.extend(step_exprs);
    let loop_expr = Expr::new(ExprKind::List(loop_call), span);
    let else_branch = {
        let mut v = vec![Expr::new(ExprKind::Symbol("begin".into()), span)];
        v.extend(body.iter().cloned());
        v.push(loop_expr);
        Expr::new(ExprKind::List(v), span)
    };
    let if_expr = Expr::new(ExprKind::List(vec![
        Expr::new(ExprKind::Symbol("if".into()), span),
        test_clause[0].clone(),
        then_branch,
        else_branch,
    ]), span);
    Ok(Expr::new(ExprKind::List(vec![
        Expr::new(ExprKind::Symbol("let".into()), span),
        Expr::new(ExprKind::Symbol(loop_name.into()), span),
        Expr::new(ExprKind::List(bindings), span),
        if_expr,
    ]), span))
}

fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut st = CekState::Eval(expr.clone(), env.clone());
    let mut k: K = Rc::new(Cont::Halt);
    let mut winders: Vec<Winder> = Vec::new();
    let mut handlers: Vec<ExHandler> = Vec::new();

    loop {
        let cur_st = std::mem::replace(&mut st, CekState::Ret(Value::Void));
        match cur_st {
            CekState::Eval(e, mut cur_env) => {
                let span = e.span;
                match e.kind {
                    ExprKind::Integer(n) => { st = CekState::Ret(Value::Integer(n)); }
                    ExprKind::Float(f) => { st = CekState::Ret(Value::Float(f)); }
                    ExprKind::Rational(n, d) => { st = CekState::Ret(make_rational(n, d)); }
                    ExprKind::Boolean(b) => { st = CekState::Ret(Value::Boolean(b)); }
                    ExprKind::Char(c) => { st = CekState::Ret(Value::Char(c)); }
                    ExprKind::Str(s) => { st = CekState::Ret(Value::Str(s, false)); }
                    ExprKind::Symbol(s) => {
                        let val = cur_env.get(&s).ok_or_else(|| EvalError::Unbound(format!("{s} at {span}")))?;
                        st = CekState::Ret(val);
                    }
                    ExprKind::List(elems) => {
                        if elems.is_empty() {
                            return Err(EvalError::Syntax(format!("empty application at {span}")));
                        }
                        if let ExprKind::Symbol(ref head) = elems[0].kind {
                            match head.as_str() {
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Syntax(format!("quote requires 1 argument at {span}")));
                        }
                        st = CekState::Ret(expr_to_value(&elems[1]));
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Syntax(format!("if requires 2 or 3 arguments at {span}")));
                        }
                        k = Rc::new(Cont::IfK(elems[2].clone(), elems.get(3).cloned(), cur_env.clone(), k));
                        st = CekState::Eval(elems[1].clone(), cur_env);
                    }
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("define requires at least 2 arguments at {span}")));
                        }
                        match &elems[1].kind {
                            ExprKind::Symbol(name) => {
                                k = Rc::new(Cont::Def(name.clone(), cur_env.clone(), k));
                                st = CekState::Eval(elems[2].clone(), cur_env);
                            }
                            ExprKind::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Syntax(format!("define: empty signature at {span}")));
                                }
                                let name = match &sig[0].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Syntax(format!("define: expected function name at {span}"))),
                                };
                                let (params, rest_param) = parse_params(&sig[1..], span)?;
                                let body = elems[2..].to_vec();
                                let lambda = Value::Lambda {
                                    name: Some(name.clone()),
                                    params,
                                    rest_param,
                                    body,
                                    env: cur_env.clone(),
                                };
                                cur_env.set(name, lambda);
                                st = CekState::Ret(Value::Void);
                            }
                            _ => return Err(EvalError::Syntax(format!("define: expected symbol or list at {span}"))),
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("lambda requires params and body at {span}")));
                        }
                        let (params, rest_param) = match &elems[1].kind {
                            ExprKind::List(ps) => parse_params(ps, span)?,
                            ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                            _ => return Err(EvalError::Syntax(format!("lambda: expected parameter list at {span}"))),
                        };
                        let body = elems[2..].to_vec();
                        st = CekState::Ret(Value::Lambda {
                            name: None,
                            params,
                            rest_param,
                            body,
                            env: cur_env.clone(),
                        });
                    }
                    "case-lambda" => {
                        if elems.len() < 2 {
                            return Err(EvalError::Syntax(format!("case-lambda requires at least one clause at {span}")));
                        }
                        let mut clauses = Vec::new();
                        for clause_expr in &elems[1..] {
                            if let ExprKind::List(clause_elems) = &clause_expr.kind {
                                if clause_elems.len() < 2 {
                                    return Err(EvalError::Syntax(format!("case-lambda: clause requires params and body at {span}")));
                                }
                                let (params, rest_param) = match &clause_elems[0].kind {
                                    ExprKind::List(ps) => parse_params(ps, span)?,
                                    ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                                    _ => return Err(EvalError::Syntax(format!("case-lambda: expected parameter list at {span}"))),
                                };
                                let body = clause_elems[1..].to_vec();
                                clauses.push((params, rest_param, body, cur_env.clone()));
                            } else {
                                return Err(EvalError::Syntax(format!("case-lambda: expected clause list at {span}")));
                            }
                        }
                        st = CekState::Ret(Value::CaseLambda { name: None, clauses });
                    }
                    "and" => {
                        if elems.len() == 1 {
                            st = CekState::Ret(Value::Boolean(true));
                        } else if elems.len() == 2 {
                            st = CekState::Eval(elems[1].clone(), cur_env);
                        } else {
                            k = Rc::new(Cont::AndK(elems[2..].to_vec(), cur_env.clone(), k));
                            st = CekState::Eval(elems[1].clone(), cur_env);
                        }
                    }
                    "or" => {
                        if elems.len() == 1 {
                            st = CekState::Ret(Value::Boolean(false));
                        } else if elems.len() == 2 {
                            st = CekState::Eval(elems[1].clone(), cur_env);
                        } else {
                            k = Rc::new(Cont::OrK(elems[2..].to_vec(), cur_env.clone(), k));
                            st = CekState::Eval(elems[1].clone(), cur_env);
                        }
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("let requires bindings and body at {span}")));
                        }
                        let (named, bindings_expr, body_start) = match &elems[1].kind {
                            ExprKind::Symbol(n) => {
                                if elems.len() < 4 {
                                    return Err(EvalError::Syntax(format!("named let requires bindings and body at {span}")));
                                }
                                (Some(n.clone()), &elems[2], 3)
                            }
                            ExprKind::List(_) => (None, &elems[1], 2),
                            _ => return Err(EvalError::Syntax(format!("let: expected bindings list at {span}"))),
                        };
                        let bindings_list = match &bindings_expr.kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("let: expected bindings list at {span}"))),
                        };
                        let mut binding_pairs: Vec<(String, Expr)> = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            binding_pairs.push((s.clone(), pair[1].clone()));
                                        }
                                        _ => return Err(EvalError::Syntax(format!("let: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("let: bad binding at {span}"))),
                            }
                        }
                        let body = elems[body_start..].to_vec();
                        if binding_pairs.is_empty() {
                            // No bindings — just evaluate body
                            let local_env = if named.is_some() {
                                let le = Env::with_parent(cur_env.clone());
                                if let Some(ref ln) = named {
                                    let lambda = Value::Lambda {
                                        name: Some(ln.clone()), params: vec![], rest_param: None,
                                        body: body.clone(), env: le.clone(),
                                    };
                                    le.set(ln.clone(), lambda);
                                }
                                le
                            } else {
                                Env::with_parent(cur_env)
                            };
                            if body.is_empty() { st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { st = CekState::Eval(body[0].clone(), local_env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), k));
                                st = CekState::Eval(body[0].clone(), local_env);
                            }
                        } else {
                            let first_expr = binding_pairs[0].1.clone();
                            let first_name = binding_pairs[0].0.clone();
                            let rest = binding_pairs[1..].to_vec();
                            k = Rc::new(Cont::LetBind {
                                rest,
                                names: vec![first_name],
                                vals: vec![],
                                body,
                                named,
                                outer_env: cur_env.clone(),
                                next: k,
                            });
                            st = CekState::Eval(first_expr, cur_env);
                        }
                    }
                    "let*" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("let* requires bindings and body at {span}")));
                        }
                        let bindings_list = match &elems[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("let*: expected bindings list at {span}"))),
                        };
                        let mut binding_pairs: Vec<(String, Expr)> = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => binding_pairs.push((s.clone(), pair[1].clone())),
                                        _ => return Err(EvalError::Syntax(format!("let*: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("let*: bad binding at {span}"))),
                            }
                        }
                        let body = elems[2..].to_vec();
                        let local_env = Env::with_parent(cur_env.clone());
                        if binding_pairs.is_empty() {
                            if body.is_empty() { st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { st = CekState::Eval(body[0].clone(), local_env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), k));
                                st = CekState::Eval(body[0].clone(), local_env);
                            }
                        } else {
                            let first_name = binding_pairs[0].0.clone();
                            let first_expr = binding_pairs[0].1.clone();
                            let rest = binding_pairs[1..].to_vec();
                            k = Rc::new(Cont::LetStarBind(first_name, rest, body, local_env.clone(), k));
                            st = CekState::Eval(first_expr, local_env);
                        }
                    }
                    "begin" => {
                        if elems.len() <= 1 { st = CekState::Ret(Value::Void); }
                        else if elems.len() == 2 { st = CekState::Eval(elems[1].clone(), cur_env); }
                        else {
                            k = Rc::new(Cont::Seq(elems[2..].to_vec(), cur_env.clone(), k));
                            st = CekState::Eval(elems[1].clone(), cur_env);
                        }
                    }
                    "cond" => {
                        let clauses = &elems[1..];
                        if clauses.is_empty() {
                            st = CekState::Ret(Value::Void);
                        } else {
                            let clause = &clauses[0];
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            if parts.len() <= 1 { st = CekState::Ret(Value::Void); }
                                            else if parts.len() == 2 { st = CekState::Eval(parts[1].clone(), cur_env); }
                                            else {
                                                k = Rc::new(Cont::Seq(parts[2..].to_vec(), cur_env.clone(), k));
                                                st = CekState::Eval(parts[1].clone(), cur_env);
                                            }
                                            continue;
                                        }
                                    }
                                    k = Rc::new(Cont::CondK {
                                        body: parts[1..].to_vec(),
                                        remaining: clauses[1..].to_vec(),
                                        env: cur_env.clone(),
                                        span,
                                        next: k,
                                    });
                                    st = CekState::Eval(parts[0].clone(), cur_env);
                                }
                                _ => return Err(EvalError::Syntax(format!("cond: bad clause at {span}"))),
                            }
                        }
                    }
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Syntax(format!("set! requires 2 arguments at {span}")));
                        }
                        let var_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("set!: expected variable name at {span}"))),
                        };
                        k = Rc::new(Cont::SetK(var_name, cur_env.clone(), span, k));
                        st = CekState::Eval(elems[2].clone(), cur_env);
                    }
                    "string-set!" => {
                        if elems.len() != 4 {
                            return Err(EvalError::Syntax(format!("string-set! requires 3 arguments at {span}")));
                        }
                        let var_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type(format!("string-set!: strings are immutable at {span}"))),
                        };
                        k = Rc::new(Cont::StrSetIdxK(var_name, elems[3].clone(), cur_env.clone(), span, k));
                        st = CekState::Eval(elems[2].clone(), cur_env);
                    }
                    "define-record-type" => {
                        if elems.len() < 4 {
                            return Err(EvalError::Syntax(format!("define-record-type requires at least 3 arguments at {span}")));
                        }
                        let type_name_str = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("define-record-type: expected type name at {span}"))),
                        };
                        let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);
                        let (ctor_name, ctor_fields) = match &elems[2].kind {
                            ExprKind::List(parts) if !parts.is_empty() => {
                                let name = match &parts[0].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Syntax(format!("define-record-type: expected constructor name at {span}"))),
                                };
                                let fields: Vec<String> = parts[1..].iter().map(|p| match &p.kind {
                                    ExprKind::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax(format!("define-record-type: expected field name at {span}"))),
                                }).collect::<Result<_, _>>()?;
                                (name, fields)
                            }
                            _ => return Err(EvalError::Syntax(format!("define-record-type: expected constructor at {span}"))),
                        };
                        let pred_name = match &elems[3].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("define-record-type: expected predicate name at {span}"))),
                        };
                        let mut field_accessors: Vec<(String, String)> = Vec::new();
                        for field_spec in &elems[4..] {
                            match &field_spec.kind {
                                ExprKind::List(parts) if parts.len() >= 2 => {
                                    let field_name = match &parts[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Syntax(format!("define-record-type: expected field name at {span}"))),
                                    };
                                    let accessor_name = match &parts[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Syntax(format!("define-record-type: expected accessor name at {span}"))),
                                    };
                                    field_accessors.push((field_name, accessor_name));
                                }
                                _ => return Err(EvalError::Syntax(format!("define-record-type: bad field spec at {span}"))),
                            }
                        }
                        let fields_str = ctor_fields.join(",");
                        let ctor_builtin = format!("__rctor_{}_{}_{}", type_id, type_name_str, fields_str);
                        cur_env.set(ctor_name, Value::Builtin(ctor_builtin));
                        let pred_builtin = format!("__rpred_{}", type_id);
                        cur_env.set(pred_name, Value::Builtin(pred_builtin));
                        for (field_name, accessor_name) in &field_accessors {
                            let acc_builtin = format!("__racc_{}_{}", type_id, field_name);
                            cur_env.set(accessor_name.clone(), Value::Builtin(acc_builtin));
                        }
                        st = CekState::Ret(Value::Void);
                    }
                    "letrec" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("letrec requires bindings and body at {span}")));
                        }
                        let bindings_list = match &elems[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("letrec: expected bindings list at {span}"))),
                        };
                        let mut local_env = Env::with_parent(cur_env);
                        let mut names = Vec::new();
                        let mut init_exprs = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            names.push(s.clone());
                                            init_exprs.push(pair[1].clone());
                                            local_env.set(s.clone(), Value::Void);
                                        }
                                        _ => return Err(EvalError::Syntax(format!("letrec: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("letrec: bad binding at {span}"))),
                            }
                        }
                        let body = elems[2..].to_vec();
                        if init_exprs.is_empty() {
                            if body.is_empty() { st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { st = CekState::Eval(body[0].clone(), local_env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), k));
                                st = CekState::Eval(body[0].clone(), local_env);
                            }
                        } else {
                            let first = init_exprs[0].clone();
                            let rest = init_exprs[1..].to_vec();
                            k = Rc::new(Cont::LetrecBind {
                                rest_inits: rest,
                                done_vals: vec![],
                                names,
                                body,
                                env: local_env.clone(),
                                next: k,
                            });
                            st = CekState::Eval(first, local_env);
                        }
                    }
                    "letrec*" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("letrec* requires bindings and body at {span}")));
                        }
                        let bindings_list = match &elems[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("letrec*: expected bindings list at {span}"))),
                        };
                        let mut binding_pairs: Vec<(String, Expr)> = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => binding_pairs.push((s.clone(), pair[1].clone())),
                                        _ => return Err(EvalError::Syntax(format!("letrec*: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("letrec*: bad binding at {span}"))),
                            }
                        }
                        let body = elems[2..].to_vec();
                        let local_env = Env::with_parent(cur_env);
                        if binding_pairs.is_empty() {
                            if body.is_empty() { st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { st = CekState::Eval(body[0].clone(), local_env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), k));
                                st = CekState::Eval(body[0].clone(), local_env);
                            }
                        } else {
                            let first_name = binding_pairs[0].0.clone();
                            let first = binding_pairs[0].1.clone();
                            let rest = binding_pairs[1..].to_vec();
                            k = Rc::new(Cont::LetrecStarBind(first_name, rest, body, local_env.clone(), k));
                            st = CekState::Eval(first, local_env);
                        }
                    }
                    "case" => {
                        if elems.len() < 2 {
                            return Err(EvalError::Syntax(format!("case requires key and clauses at {span}")));
                        }
                        k = Rc::new(Cont::CaseK(elems[2..].to_vec(), cur_env.clone(), span, k));
                        st = CekState::Eval(elems[1].clone(), cur_env);
                    }
                    "do" => {
                        let desugared = desugar_do(&elems, span)?;
                        st = CekState::Eval(desugared, cur_env);
                    }
                    "define-syntax" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Syntax(format!("define-syntax requires 2 arguments at {span}")));
                        }
                        let macro_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("define-syntax: expected name at {span}"))),
                        };
                        let sr_elems = match &elems[2].kind {
                            ExprKind::List(v) => v,
                            _ => return Err(EvalError::Syntax(format!("define-syntax: expected syntax-rules at {span}"))),
                        };
                        if sr_elems.len() < 2 || !matches!(&sr_elems[0].kind, ExprKind::Symbol(ref s) if s == "syntax-rules") {
                            return Err(EvalError::Syntax(format!("define-syntax: expected syntax-rules at {span}")));
                        }
                        let lits: Vec<String> = match &sr_elems[1].kind {
                            ExprKind::List(ls) => ls.iter().map(|e| match &e.kind {
                                ExprKind::Symbol(s) => Ok(s.clone()),
                                _ => Err(EvalError::Syntax(format!("define-syntax: bad literal at {span}"))),
                            }).collect::<Result<_, _>>()?,
                            _ => return Err(EvalError::Syntax(format!("define-syntax: expected literals list at {span}"))),
                        };
                        let mut rules = Vec::new();
                        for rule_expr in &sr_elems[2..] {
                            let parts = match &rule_expr.kind {
                                ExprKind::List(v) if v.len() == 2 => v,
                                _ => return Err(EvalError::Syntax(format!("define-syntax: bad rule at {span}"))),
                            };
                            let pat_elems = match &parts[0].kind {
                                ExprKind::List(p) => if p.is_empty() { vec![] } else { p[1..].to_vec() },
                                _ => return Err(EvalError::Syntax(format!("define-syntax: bad pattern at {span}"))),
                            };
                            rules.push((pat_elems, parts[1].clone()));
                        }
                        let macro_val = Value::Macro { literals: lits, rules, def_env: cur_env.clone() };
                        cur_env.set(macro_name, macro_val);
                        st = CekState::Ret(Value::Void);
                    }
                    "guard" => {
                        // (guard (var clause ...) body ...)
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("guard requires variable and clauses at {span}")));
                        }
                        let clause_list = match &elems[1].kind {
                            ExprKind::List(v) if v.len() >= 1 => v,
                            _ => return Err(EvalError::Syntax(format!("guard: bad clause list at {span}"))),
                        };
                        let var_name = match &clause_list[0].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("guard: expected variable at {span}"))),
                        };
                        // Parse clauses: each is (test body ...) or (else body ...)
                        let mut clauses: Vec<(Expr, Vec<Expr>)> = Vec::new();
                        for clause in &clause_list[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    let test = parts[0].clone();
                                    let body = parts[1..].to_vec();
                                    clauses.push((test, body));
                                }
                                _ => return Err(EvalError::Syntax(format!("guard: bad clause at {span}"))),
                            }
                        }
                        // Push guard handler
                        handlers.push(ExHandler {
                            handler: Value::Void, // not used for guard
                            guard_k: Some(k.clone()),
                            guard_winders: Some(winders.clone()),
                            guard_var: Some(var_name),
                            guard_clauses: Some(clauses),
                            guard_env: Some(cur_env.clone()),
                        });
                        // Evaluate body with guard handler active
                        k = Rc::new(Cont::GuardBody(k));
                        let body = &elems[2..];
                        if body.len() == 1 {
                            st = CekState::Eval(body[0].clone(), cur_env);
                        } else {
                            k = Rc::new(Cont::Seq(body[1..].to_vec(), cur_env.clone(), k));
                            st = CekState::Eval(body[0].clone(), cur_env);
                        }
                    }
                    "call/cc" | "call-with-current-continuation" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Syntax(format!("call/cc requires 1 argument at {span}")));
                        }
                        k = Rc::new(Cont::CallCCK(k));
                        st = CekState::Eval(elems[1].clone(), cur_env);
                    }
                    _ => {
                        // Check for macro invocation
                        if let Some(Value::Macro { literals: mac_lits, rules: mac_rules, def_env: mac_def_env }) = cur_env.get(head) {
                            let input_elems = &elems[1..];
                            let mut expanded_result = None;
                            for (pattern, template) in &mac_rules {
                                let mut bindings = HashMap::new();
                                if match_syntax_pattern(pattern, input_elems, &mac_lits, &mut bindings) {
                                    let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
                                    let mut renames: HashMap<String, String> = HashMap::new();
                                    collect_introduced_symbols(&template, &pattern_vars, &mut renames);
                                    let expanded = expand_template(&template, &bindings, &renames);
                                    for (original, renamed) in &renames {
                                        if let Some(val) = mac_def_env.get(original.as_str()) {
                                            cur_env.set(renamed.clone(), val);
                                        }
                                    }
                                    expanded_result = Some(expanded);
                                    break;
                                }
                            }
                            if let Some(expanded) = expanded_result {
                                st = CekState::Eval(expanded, cur_env);
                            } else {
                                return Err(EvalError::Syntax(format!("no matching pattern for macro {head} at {span}")));
                            }
                        } else {
                            // Not a macro — function application (right-to-left arg eval)
                            let mut rev_args = elems[1..].to_vec();
                            rev_args.reverse();
                            k = Rc::new(Cont::EvOp(rev_args, cur_env.clone(), span, k));
                            st = CekState::Eval(elems[0].clone(), cur_env);
                        }
                    }
                            }
                        } else {
                            // Head is not a symbol — function application (right-to-left arg eval)
                            let mut rev_args = elems[1..].to_vec();
                            rev_args.reverse();
                            k = Rc::new(Cont::EvOp(rev_args, cur_env.clone(), span, k));
                            st = CekState::Eval(elems[0].clone(), cur_env);
                        }
                    }
                }
            }
            CekState::Ret(val) => {
                let frame = match Rc::try_unwrap(k) {
                    Ok(c) => c,
                    Err(rc) => (*rc).clone(),
                };
                match frame {
                    Cont::Halt => return Ok(val),
                    Cont::Seq(rest, env, next) => {
                        if rest.len() == 1 {
                            k = next;
                            st = CekState::Eval(rest[0].clone(), env);
                        } else {
                            k = Rc::new(Cont::Seq(rest[1..].to_vec(), env.clone(), next));
                            st = CekState::Eval(rest[0].clone(), env);
                        }
                    }
                    Cont::Def(name, env, next) => {
                        env.set(name, val);
                        k = next;
                        st = CekState::Ret(Value::Void);
                    }
                    Cont::SetK(name, env, span, next) => {
                        if !env.set_existing(&name, val) {
                            return Err(EvalError::Unbound(format!("{name} at {span}")));
                        }
                        k = next;
                        st = CekState::Ret(Value::Void);
                    }
                    Cont::IfK(then_e, else_e, env, next) => {
                        k = next;
                        if is_truthy(&val) {
                            st = CekState::Eval(then_e, env);
                        } else if let Some(ee) = else_e {
                            st = CekState::Eval(ee, env);
                        } else {
                            st = CekState::Ret(Value::Void);
                        }
                    }
                    Cont::EvOp(rev_arg_exprs, env, span, next) => {
                        // rev_arg_exprs is reversed: we evaluate rightmost argument first
                        if rev_arg_exprs.is_empty() {
                            k = next;
                            cek_apply_func(&val, &[], span, &mut st, &mut k, &mut winders, &mut handlers, output)?;
                        } else {
                            k = Rc::new(Cont::EvArg(val, vec![], rev_arg_exprs[1..].to_vec(), env.clone(), span, next));
                            st = CekState::Eval(rev_arg_exprs[0].clone(), env);
                        }
                    }
                    Cont::EvArg(func, mut done, rest, env, span, next) => {
                        done.push(val);
                        if rest.is_empty() {
                            // done is in reverse order (rightmost first), reverse to get original order
                            done.reverse();
                            k = next;
                            cek_apply_func(&func, &done, span, &mut st, &mut k, &mut winders, &mut handlers, output)?;
                        } else {
                            k = Rc::new(Cont::EvArg(func, done, rest[1..].to_vec(), env.clone(), span, next));
                            st = CekState::Eval(rest[0].clone(), env);
                        }
                    }
                    Cont::AndK(rest, env, next) => {
                        if !is_truthy(&val) {
                            k = next;
                            st = CekState::Ret(val);
                        } else if rest.len() == 1 {
                            k = next;
                            st = CekState::Eval(rest[0].clone(), env);
                        } else {
                            k = Rc::new(Cont::AndK(rest[1..].to_vec(), env.clone(), next));
                            st = CekState::Eval(rest[0].clone(), env);
                        }
                    }
                    Cont::OrK(rest, env, next) => {
                        if is_truthy(&val) {
                            k = next;
                            st = CekState::Ret(val);
                        } else if rest.len() == 1 {
                            k = next;
                            st = CekState::Eval(rest[0].clone(), env);
                        } else {
                            k = Rc::new(Cont::OrK(rest[1..].to_vec(), env.clone(), next));
                            st = CekState::Eval(rest[0].clone(), env);
                        }
                    }
                    Cont::LetBind { rest, mut names, mut vals, body, named, outer_env, next } => {
                        vals.push(val);
                        if rest.is_empty() {
                            // All bindings evaluated
                            if let Some(loop_name) = named {
                                let lambda = Value::Lambda {
                                    name: Some(loop_name.clone()),
                                    params: names.clone(),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: outer_env.clone(),
                                };
                                let local_env = setup_lambda_env_basic(&lambda, &Some(loop_name), &names, &None, &outer_env, &vals, Span::default())?;
                                if body.is_empty() { k = next; st = CekState::Ret(Value::Void); }
                                else if body.len() == 1 { k = next; st = CekState::Eval(body[0].clone(), local_env); }
                                else {
                                    k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), next));
                                    st = CekState::Eval(body[0].clone(), local_env);
                                }
                            } else {
                                let local_env = Env::with_parent(outer_env);
                                for (n, v) in names.iter().zip(vals.iter()) {
                                    local_env.set(n.clone(), v.clone());
                                }
                                if body.is_empty() { k = next; st = CekState::Ret(Value::Void); }
                                else if body.len() == 1 { k = next; st = CekState::Eval(body[0].clone(), local_env); }
                                else {
                                    k = Rc::new(Cont::Seq(body[1..].to_vec(), local_env.clone(), next));
                                    st = CekState::Eval(body[0].clone(), local_env);
                                }
                            }
                        } else {
                            names.push(rest[0].0.clone());
                            let next_expr = rest[0].1.clone();
                            k = Rc::new(Cont::LetBind {
                                rest: rest[1..].to_vec(),
                                names,
                                vals,
                                body,
                                named,
                                outer_env: outer_env.clone(),
                                next,
                            });
                            st = CekState::Eval(next_expr, outer_env);
                        }
                    }
                    Cont::LetStarBind(cur_name, rest, body, env, next) => {
                        env.set(cur_name, val);
                        if rest.is_empty() {
                            if body.is_empty() { k = next; st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { k = next; st = CekState::Eval(body[0].clone(), env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), env.clone(), next));
                                st = CekState::Eval(body[0].clone(), env);
                            }
                        } else {
                            let next_name = rest[0].0.clone();
                            let next_expr = rest[0].1.clone();
                            k = Rc::new(Cont::LetStarBind(next_name, rest[1..].to_vec(), body, env.clone(), next));
                            st = CekState::Eval(next_expr, env);
                        }
                    }
                    Cont::LetrecBind { rest_inits, mut done_vals, names, body, env, next } => {
                        done_vals.push(val);
                        if rest_inits.is_empty() {
                            for (name, v) in names.iter().zip(done_vals.iter()) {
                                env.set(name.clone(), v.clone());
                            }
                            if body.is_empty() { k = next; st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { k = next; st = CekState::Eval(body[0].clone(), env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), env.clone(), next));
                                st = CekState::Eval(body[0].clone(), env);
                            }
                        } else {
                            let next_init = rest_inits[0].clone();
                            k = Rc::new(Cont::LetrecBind {
                                rest_inits: rest_inits[1..].to_vec(),
                                done_vals,
                                names,
                                body,
                                env: env.clone(),
                                next,
                            });
                            st = CekState::Eval(next_init, env);
                        }
                    }
                    Cont::LetrecStarBind(cur_name, rest, body, env, next) => {
                        env.set(cur_name, val);
                        if rest.is_empty() {
                            if body.is_empty() { k = next; st = CekState::Ret(Value::Void); }
                            else if body.len() == 1 { k = next; st = CekState::Eval(body[0].clone(), env); }
                            else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), env.clone(), next));
                                st = CekState::Eval(body[0].clone(), env);
                            }
                        } else {
                            let next_name = rest[0].0.clone();
                            let next_expr = rest[0].1.clone();
                            k = Rc::new(Cont::LetrecStarBind(next_name, rest[1..].to_vec(), body, env.clone(), next));
                            st = CekState::Eval(next_expr, env);
                        }
                    }
                    Cont::CaseK(clauses, env, span, next) => {
                        k = next;
                        let key = val;
                        let mut found = false;
                        for clause in &clauses {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            if parts.len() <= 1 { st = CekState::Ret(Value::Void); }
                                            else if parts.len() == 2 { st = CekState::Eval(parts[1].clone(), env.clone()); }
                                            else {
                                                k = Rc::new(Cont::Seq(parts[2..].to_vec(), env.clone(), k));
                                                st = CekState::Eval(parts[1].clone(), env.clone());
                                            }
                                            found = true;
                                            break;
                                        }
                                    }
                                    let datums = match &parts[0].kind {
                                        ExprKind::List(ds) => ds,
                                        _ => return Err(EvalError::Syntax(format!("case: expected datum list at {span}"))),
                                    };
                                    let matched = datums.iter().any(|d| {
                                        let datum_val = expr_to_value(d);
                                        scheme_eqv(&key, &datum_val)
                                    });
                                    if matched {
                                        if parts.len() <= 1 { st = CekState::Ret(Value::Void); }
                                        else if parts.len() == 2 { st = CekState::Eval(parts[1].clone(), env.clone()); }
                                        else {
                                            k = Rc::new(Cont::Seq(parts[2..].to_vec(), env.clone(), k));
                                            st = CekState::Eval(parts[1].clone(), env.clone());
                                        }
                                        found = true;
                                        break;
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("case: bad clause at {span}"))),
                            }
                        }
                        if !found {
                            st = CekState::Ret(Value::Void);
                        }
                    }
                    Cont::CondK { body, remaining, env, span, next } => {
                        if is_truthy(&val) {
                            k = next;
                            if body.is_empty() {
                                st = CekState::Ret(val);
                            } else if body.len() == 1 {
                                st = CekState::Eval(body[0].clone(), env);
                            } else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), env.clone(), k));
                                st = CekState::Eval(body[0].clone(), env);
                            }
                        } else if remaining.is_empty() {
                            k = next;
                            st = CekState::Ret(Value::Void);
                        } else {
                            let clause = &remaining[0];
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            k = next;
                                            if parts.len() <= 1 { st = CekState::Ret(Value::Void); }
                                            else if parts.len() == 2 { st = CekState::Eval(parts[1].clone(), env); }
                                            else {
                                                k = Rc::new(Cont::Seq(parts[2..].to_vec(), env.clone(), k));
                                                st = CekState::Eval(parts[1].clone(), env);
                                            }
                                            continue;
                                        }
                                    }
                                    k = Rc::new(Cont::CondK {
                                        body: parts[1..].to_vec(),
                                        remaining: remaining[1..].to_vec(),
                                        env: env.clone(),
                                        span,
                                        next,
                                    });
                                    st = CekState::Eval(parts[0].clone(), env);
                                }
                                _ => return Err(EvalError::Syntax(format!("cond: bad clause at {span}"))),
                            }
                        }
                    }
                    Cont::StrSetIdxK(var_name, char_expr, env, span, next) => {
                        let idx = match val {
                            Value::Integer(n) => n as usize,
                            _ => return Err(EvalError::Type(format!("string-set!: expected integer index at {span}"))),
                        };
                        k = Rc::new(Cont::StrSetCharK(var_name, idx, env.clone(), span, next));
                        st = CekState::Eval(char_expr, env);
                    }
                    Cont::StrSetCharK(var_name, idx, env, span, next) => {
                        let ch = match val {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type(format!("string-set!: expected char at {span}"))),
                        };
                        let current = env.get(&var_name).ok_or_else(|| EvalError::Unbound(format!("{var_name} at {span}")))?;
                        match current {
                            Value::Str(s, true) => {
                                let mut chars: Vec<char> = s.chars().collect();
                                if idx >= chars.len() {
                                    return Err(EvalError::Type(format!("string-set!: index out of range at {span}")));
                                }
                                chars[idx] = ch;
                                let new_s: String = chars.into_iter().collect();
                                env.set_existing(&var_name, Value::Str(new_s, true));
                                k = next;
                                st = CekState::Ret(Value::Void);
                            }
                            Value::Str(_, false) => return Err(EvalError::Type(format!("string-set!: strings are immutable at {span}"))),
                            _ => return Err(EvalError::Type(format!("string-set!: not a string at {span}"))),
                        }
                    }
                    Cont::CallCCK(next) => {
                        let continuation_val = Value::Continuation(next.clone(), winders.clone());
                        k = next;
                        cek_apply_func(&val, &[continuation_val], Span::default(), &mut st, &mut k, &mut winders, &mut handlers, output)?;
                    }
                    Cont::DynWindBody(in_thunk, body_thunk, out_thunk, next) => {
                        // in-thunk finished, push winder, call body
                        let winder_id = WINDER_COUNTER.fetch_add(1, Ordering::Relaxed);
                        winders.push(Winder { id: winder_id, in_thunk, out_thunk: out_thunk.clone() });
                        k = Rc::new(Cont::DynWindOut(val, out_thunk, next));
                        cek_apply_func(&body_thunk, &[], Span::default(), &mut st, &mut k, &mut winders, &mut handlers, output)?;
                    }
                    Cont::DynWindOut(_in_result, out_thunk, next) => {
                        // body-thunk returned val; pop winder, call out-thunk
                        winders.pop();
                        k = Rc::new(Cont::DynWindDone(val, next));
                        cek_apply_func(&out_thunk, &[], Span::default(), &mut st, &mut k, &mut winders, &mut handlers, output)?;
                    }
                    Cont::DynWindDone(body_val, next) => {
                        // out-thunk finished, return saved body value
                        let _ = val; // out-thunk result, unused
                        k = next;
                        st = CekState::Ret(body_val);
                    }
                    Cont::DynWindDoUnwind { out_thunks, rewind_winders, target_winders, val: cont_val, saved_k } => {
                        let _ = val; // out-thunk result, unused
                        if !out_thunks.is_empty() {
                            let first_out = out_thunks[0].clone();
                            k = Rc::new(Cont::DynWindDoUnwind {
                                out_thunks: out_thunks[1..].to_vec(),
                                rewind_winders,
                                target_winders,
                                val: cont_val,
                                saved_k,
                            });
                            cek_apply_func(&first_out, &[], Span::default(), &mut st, &mut k, &mut winders, &mut handlers, output)?;
                        } else if !rewind_winders.is_empty() {
                            // Start rewinding
                            let first_winder = rewind_winders[0].clone();
                            winders.push(first_winder.clone());
                            k = Rc::new(Cont::DynWindDoRewind {
                                remaining: rewind_winders[1..].to_vec(),
                                target_winders,
                                val: cont_val,
                                saved_k,
                            });
                            cek_apply_func(&first_winder.in_thunk, &[], Span::default(), &mut st, &mut k, &mut winders, &mut handlers, output)?;
                        } else {
                            // Done unwinding, no rewinding needed
                            winders = target_winders;
                            k = saved_k;
                            st = CekState::Ret(cont_val);
                        }
                    }
                    Cont::DynWindDoRewind { remaining, target_winders, val: cont_val, saved_k } => {
                        let _ = val; // in-thunk result, unused
                        if !remaining.is_empty() {
                            let next_winder = remaining[0].clone();
                            winders.push(next_winder.clone());
                            k = Rc::new(Cont::DynWindDoRewind {
                                remaining: remaining[1..].to_vec(),
                                target_winders,
                                val: cont_val,
                                saved_k,
                            });
                            cek_apply_func(&next_winder.in_thunk, &[], Span::default(), &mut st, &mut k, &mut winders, &mut handlers, output)?;
                        } else {
                            // Done rewinding, restore continuation
                            winders = target_winders;
                            k = saved_k;
                            st = CekState::Ret(cont_val);
                        }
                    }
                    Cont::WithExHandlerBody(next) => {
                        // Body of with-exception-handler returned normally; pop handler
                        handlers.pop();
                        k = next;
                        st = CekState::Ret(val);
                    }
                    Cont::GuardBody(next) => {
                        // Guard body returned normally; pop handler, return value
                        handlers.pop();
                        k = next;
                        st = CekState::Ret(val);
                    }
                    Cont::GuardEvalClause { var, exn, clauses, env, next } => {
                        // We arrive here after unwinding (val is ignored).
                        // Bind var to exn and start evaluating clauses.
                        let _ = val;
                        let mut clause_env = env.clone();
                        clause_env.set(var.clone(), exn.clone());
                        if clauses.is_empty() {
                            // No clauses — re-raise
                            return Err(EvalError::Raised(Box::new(exn)));
                        }
                        let (test, body) = &clauses[0];
                        let remaining = clauses[1..].to_vec();
                        if matches!(&test.kind, ExprKind::Symbol(s) if s == "else") {
                            if body.is_empty() {
                                k = next;
                                st = CekState::Ret(Value::Void);
                            } else if body.len() == 1 {
                                k = next;
                                st = CekState::Eval(body[0].clone(), clause_env);
                            } else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), clause_env.clone(), next));
                                st = CekState::Eval(body[0].clone(), clause_env);
                            }
                        } else {
                            k = Rc::new(Cont::GuardTestResult {
                                var,
                                exn,
                                body: body.clone(),
                                remaining,
                                env,
                                next,
                            });
                            st = CekState::Eval(test.clone(), clause_env);
                        }
                    }
                    Cont::GuardTestResult { var, exn, body, remaining, env, next } => {
                        // val = result of evaluating test expression
                        if is_truthy(&val) {
                            // Clause matched — evaluate body
                            let mut clause_env = env.clone();
                            clause_env.set(var, exn);
                            if body.is_empty() {
                                // No body — return test value
                                k = next;
                                st = CekState::Ret(val);
                            } else if body.len() == 1 {
                                k = next;
                                st = CekState::Eval(body[0].clone(), clause_env);
                            } else {
                                k = Rc::new(Cont::Seq(body[1..].to_vec(), clause_env.clone(), next));
                                st = CekState::Eval(body[0].clone(), clause_env);
                            }
                        } else {
                            // Clause didn't match — try remaining
                            if remaining.is_empty() {
                                return Err(EvalError::Raised(Box::new(exn)));
                            }
                            let mut clause_env = env.clone();
                            clause_env.set(var.clone(), exn.clone());
                            let (test, body) = &remaining[0];
                            let rest = remaining[1..].to_vec();
                            if matches!(&test.kind, ExprKind::Symbol(s) if s == "else") {
                                if body.is_empty() {
                                    k = next;
                                    st = CekState::Ret(Value::Void);
                                } else if body.len() == 1 {
                                    k = next;
                                    st = CekState::Eval(body[0].clone(), clause_env);
                                } else {
                                    k = Rc::new(Cont::Seq(body[1..].to_vec(), clause_env.clone(), next));
                                    st = CekState::Eval(body[0].clone(), clause_env);
                                }
                            } else {
                                k = Rc::new(Cont::GuardTestResult {
                                    var,
                                    exn,
                                    body: body.clone(),
                                    remaining: rest,
                                    env,
                                    next,
                                });
                                st = CekState::Eval(test.clone(), clause_env);
                            }
                        }
                    }
                }
            }
        }
    }
}

fn apply_func(func: &Value, args: &[Value], call_span: Span, output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args, call_span, output),
        Value::Lambda { name, params, rest_param, body, env } => {
            let mut local_env = setup_lambda_env_basic(func, name, params, rest_param, env, args, call_span)?;
            let mut result = Value::Void;
            for e in body {
                result = eval(e, &mut local_env, output)?;
            }
            Ok(result)
        }
        Value::CaseLambda { name, clauses } => {
            for (params, rest_param, body, clause_env) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let lambda = Value::Lambda {
                        name: name.clone(),
                        params: params.clone(),
                        rest_param: rest_param.clone(),
                        body: body.clone(),
                        env: clause_env.clone(),
                    };
                    return apply_func(&lambda, args, call_span, output);
                }
            }
            Err(EvalError::Arity(format!(
                "no matching clause for {} arguments at {call_span}", args.len()
            )))
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func} at {call_span}"))),
    }
}

fn scheme_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn scheme_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        _ => false,
    }
}

fn scheme_equal(a: &Value, b: &Value) -> bool {
    scheme_equal_depth(a, b, 0)
}

fn scheme_equal_depth(a: &Value, b: &Value, depth: usize) -> bool {
    if depth > 10000 { return false; }
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Str(x, _), Value::Str(y, _)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| scheme_equal_depth(a, b, depth + 1))
        }
        (Value::Pair(p1), Value::Pair(p2)) => {
            if Rc::ptr_eq(p1, p2) { return true; }
            let (a1, b1) = { let b = p1.borrow(); (b.0.clone(), b.1.clone()) };
            let (a2, b2) = { let b = p2.borrow(); (b.0.clone(), b.1.clone()) };
            scheme_equal_depth(&a1, &a2, depth + 1) && scheme_equal_depth(&b1, &b2, depth + 1)
        }
        // Cross-type: List vs Pair chain comparison
        (Value::List(_), Value::Pair(_)) | (Value::Pair(_), Value::List(_)) => {
            let pair_val = if matches!(a, Value::Pair(_)) { a } else { b };
            let list_val = if matches!(a, Value::List(_)) { a } else { b };
            match (value_to_vec(pair_val), value_to_vec(list_val)) {
                (Some(pv), Some(lv)) => {
                    pv.len() == lv.len() && pv.iter().zip(lv.iter()).all(|(a, b)| scheme_equal_depth(a, b, depth + 1))
                }
                _ => false,
            }
        }
        (Value::Vector(x), Value::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| scheme_equal_depth(a, b, depth + 1))
        }
        _ => false,
    }
}

fn is_proper_list(v: &Value) -> bool {
    is_list_safe(v)
}

fn apply_builtin(name: &str, args: &[Value], call_span: Span, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" => {
            for a in args { if !is_numeric(a) { return Err(EvalError::Type(format!("expected number, got {a} at {call_span}"))); } }
            if has_inexact(args) {
                let mut sum = 0.0_f64;
                for a in args { sum += value_to_f64(a, call_span)?; }
                Ok(Value::Float(sum))
            } else {
                let mut acc = Value::Integer(0);
                for a in args { acc = exact_add(&acc, a); }
                Ok(acc)
            }
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("- requires at least 1 argument at {call_span}")));
            }
            for a in args { if !is_numeric(a) { return Err(EvalError::Type(format!("expected number, got {a} at {call_span}"))); } }
            if has_inexact(args) {
                if args.len() == 1 {
                    Ok(Value::Float(-value_to_f64(&args[0], call_span)?))
                } else {
                    let mut result = value_to_f64(&args[0], call_span)?;
                    for a in &args[1..] { result -= value_to_f64(a, call_span)?; }
                    Ok(Value::Float(result))
                }
            } else if args.len() == 1 {
                match &args[0] {
                    Value::Integer(n) => Ok(Value::Integer(-n)),
                    Value::Rational(n, d) => Ok(Value::Rational(-n, *d)),
                    _ => Err(EvalError::Type(format!("expected number at {call_span}"))),
                }
            } else {
                let mut acc = args[0].clone();
                for a in &args[1..] { acc = exact_sub(&acc, a); }
                Ok(acc)
            }
        }
        "*" => {
            for a in args { if !is_numeric(a) { return Err(EvalError::Type(format!("expected number, got {a} at {call_span}"))); } }
            if has_inexact(args) {
                let mut product = 1.0_f64;
                for a in args { product *= value_to_f64(a, call_span)?; }
                Ok(Value::Float(product))
            } else {
                let mut acc = Value::Integer(1);
                for a in args { acc = exact_mul(&acc, a); }
                Ok(acc)
            }
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("/ requires at least 1 argument at {call_span}")));
            }
            for a in args { if !is_numeric(a) { return Err(EvalError::Type(format!("expected number, got {a} at {call_span}"))); } }
            if has_inexact(args) {
                let mut result = value_to_f64(&args[0], call_span)?;
                for a in &args[1..] {
                    let d = value_to_f64(a, call_span)?;
                    if d == 0.0 { return Err(EvalError::DivisionByZero(call_span.to_string())); }
                    result /= d;
                }
                Ok(Value::Float(result))
            } else if args.len() == 1 {
                exact_div(&Value::Integer(1), &args[0], call_span)
            } else {
                let mut acc = args[0].clone();
                for a in &args[1..] { acc = exact_div(&acc, a, call_span)?; }
                Ok(acc)
            }
        }
        "<" => {
            let vals = args_to_f64s(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            let vals = args_to_f64s(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] > w[1])))
        }
        "=" => {
            let vals = args_to_f64s(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] == w[1])))
        }
        "<=" => {
            let vals = args_to_f64s(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            let vals = args_to_f64s(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] >= w[1])))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("not requires 1 argument at {call_span}")));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("cons requires 2 arguments at {call_span}")));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("car requires 1 argument at {call_span}")));
            }
            pair_car(&args[0], call_span)
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("cdr requires 1 argument at {call_span}")));
            }
            pair_cdr(&args[0], call_span)
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("null? requires 1 argument at {call_span}")));
            }
            Ok(Value::Boolean(is_null(&args[0])))
        }
        "list" => {
            Ok(make_list(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("length requires 1 argument at {call_span}")));
            }
            match value_to_vec(&args[0]) {
                Some(v) => Ok(Value::Integer(v.len() as i64)),
                None => Err(EvalError::Type(format!("length: not a proper list at {call_span}"))),
            }
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::List(vec![]));
            }
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i < args.len() - 1 {
                    match value_to_vec(arg) {
                        Some(elems) => result.extend(elems),
                        None => return Err(EvalError::Type(format!("append: not a list at {call_span}"))),
                    }
                } else {
                    // Last argument: if it's a list, extend; otherwise create improper list
                    match value_to_vec(arg) {
                        Some(elems) => {
                            result.extend(elems);
                            return Ok(make_list(result));
                        }
                        None => {
                            // improper list: build pair chain ending with this value
                            let mut tail = arg.clone();
                            for item in result.into_iter().rev() {
                                tail = make_pair(item, tail);
                            }
                            return Ok(tail);
                        }
                    }
                }
            }
            Ok(make_list(result))
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_, _))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("number? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(is_numeric(&args[0])))
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("integer? requires 1 argument at {call_span}"))); }
            let result = match &args[0] {
                Value::Integer(_) => true,
                Value::Rational(n, d) => n % d == 0,
                Value::Float(f) => f.fract() == 0.0 && f.is_finite(),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("rational? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("exact? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("inexact? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("exact->inexact requires 1 argument at {call_span}"))); }
            Ok(Value::Float(value_to_f64(&args[0], call_span)?))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("inexact->exact requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, d) => Ok(make_rational(*n, *d)),
                Value::Float(f) => {
                    // Convert float to exact rational using continued fraction approximation
                    // Simple approach: multiply by power of 10, simplify
                    if f.fract() == 0.0 {
                        Ok(Value::Integer(*f as i64))
                    } else {
                        // Use the standard approach: represent as n/2^53 then simplify
                        let bits = 53;
                        let denom = 1i64 << bits;
                        let numer = (*f * denom as f64).round() as i64;
                        Ok(make_rational(numer, denom))
                    }
                }
                _ => Err(EvalError::Type(format!("inexact->exact: not a number at {call_span}"))),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("numerator requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type(format!("numerator: not a rational at {call_span}"))),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("denominator requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type(format!("denominator: not a rational at {call_span}"))),
            }
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("boolean? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("pair? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(is_pair(&args[0])))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("symbol? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "procedure?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("procedure? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Lambda { .. } | Value::CaseLambda { .. } | Value::Builtin(_) | Value::Continuation(..))))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("display requires 1 argument at {call_span}"))); }
            display_value(&args[0], output);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("write requires 1 argument at {call_span}"))); }
            write_value_safe(&args[0], output, &mut HashSet::new(), true);
            Ok(Value::Void)
        }
        "caar" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("caar requires 1 argument at {call_span}"))); }
            pair_car(&pair_car(&args[0], call_span)?, call_span)
        }
        "cadr" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("cadr requires 1 argument at {call_span}"))); }
            pair_car(&pair_cdr(&args[0], call_span)?, call_span)
        }
        "cdar" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("cdar requires 1 argument at {call_span}"))); }
            pair_cdr(&pair_car(&args[0], call_span)?, call_span)
        }
        "cddr" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("cddr requires 1 argument at {call_span}"))); }
            pair_cdr(&pair_cdr(&args[0], call_span)?, call_span)
        }
        "assv" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("assv requires 2 arguments at {call_span}"))); }
            let key = &args[0];
            let elems = value_to_vec(&args[1]).ok_or_else(|| EvalError::Type(format!("assv: not a list at {call_span}")))?;
            for entry in &elems {
                if is_pair(entry) {
                    let entry_car = pair_car(entry, call_span)?;
                    if scheme_eqv(key, &entry_car) {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        "gcd" => {
            if args.is_empty() { return Ok(Value::Integer(0)); }
            let mut result = as_int(&args[0], call_span)?.abs();
            for a in &args[1..] {
                result = gcd(result, as_int(a, call_span)?);
            }
            Ok(Value::Integer(result))
        }
        "lcm" => {
            if args.is_empty() { return Ok(Value::Integer(1)); }
            let mut result = as_int(&args[0], call_span)?.abs();
            for a in &args[1..] {
                let b = as_int(a, call_span)?.abs();
                if result == 0 && b == 0 { result = 0; }
                else { result = result / gcd(result, b) * b; }
            }
            Ok(Value::Integer(result))
        }
        "truncate" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("truncate requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer(n / d)),
                _ => Err(EvalError::Type(format!("truncate: not a number at {call_span}"))),
            }
        }
        "round" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("round requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer((*n as f64 / *d as f64).round() as i64)),
                _ => Err(EvalError::Type(format!("round: not a number at {call_span}"))),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!("make-string requires 1 or 2 arguments at {call_span}")));
            }
            let len = as_int(&args[0], call_span)? as usize;
            let ch = if args.len() == 2 {
                match &args[1] { Value::Char(c) => *c, _ => return Err(EvalError::Type(format!("make-string: expected char at {call_span}"))) }
            } else { '\0' };
            Ok(Value::Str(std::iter::repeat(ch).take(len).collect(), true))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                match a {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type(format!("string: expected char at {call_span}"))),
                }
            }
            Ok(Value::Str(s, true))
        }
        "string>?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string>? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a > b)),
                _ => Err(EvalError::Type(format!("string>?: not strings at {call_span}"))),
            }
        }
        "string<=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string<=? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a <= b)),
                _ => Err(EvalError::Type(format!("string<=?: not strings at {call_span}"))),
            }
        }
        "string>=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string>=? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a >= b)),
                _ => Err(EvalError::Type(format!("string>=?: not strings at {call_span}"))),
            }
        }
        "member" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("member requires 2 arguments at {call_span}"))); }
            let obj = &args[0];
            let mut current = args[1].clone();
            loop {
                if is_null(&current) {
                    return Ok(Value::Boolean(false));
                }
                if !is_pair(&current) {
                    return Err(EvalError::Type(format!("member: not a proper list at {call_span}")));
                }
                let car = pair_car(&current, call_span)?;
                if scheme_equal(obj, &car) {
                    return Ok(current);
                }
                current = pair_cdr(&current, call_span)?;
            }
        }
        "reverse" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("reverse requires 1 argument at {call_span}"))); }
            let elems = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(format!("reverse: not a list at {call_span}")))?;
            let mut reversed = elems;
            reversed.reverse();
            Ok(make_list(reversed))
        }
        "set-car!" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("set-car! requires 2 arguments at {call_span}"))); }
            match &args[0] {
                Value::Pair(p) => {
                    p.borrow_mut().0 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type(format!("set-car!: not a mutable pair at {call_span}"))),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("set-cdr! requires 2 arguments at {call_span}"))); }
            match &args[0] {
                Value::Pair(p) => {
                    p.borrow_mut().1 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type(format!("set-cdr!: not a mutable pair at {call_span}"))),
            }
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity(format!("newline requires 0 arguments at {call_span}"))); }
            output.push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s, _) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!("string-append: not a string at {call_span}"))),
                }
            }
            Ok(Value::Str(result, true))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-length requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Integer(s.chars().count() as i64)),
                _ => Err(EvalError::Type(format!("string-length: not a string at {call_span}"))),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity(format!("substring requires 3 arguments at {call_span}"))); }
            let s = match &args[0] {
                Value::Str(s, _) => s,
                _ => return Err(EvalError::Type(format!("substring: not a string at {call_span}"))),
            };
            let start = as_int(&args[1], call_span)? as usize;
            let end = as_int(&args[2], call_span)? as usize;
            let chars: Vec<char> = s.chars().collect();
            if end > chars.len() || start > end {
                return Err(EvalError::Type(format!("substring: index out of range at {call_span}")));
            }
            Ok(Value::Str(chars[start..end].iter().collect(), true))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string->number requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => {
                    if let Ok(n) = s.parse::<i64>() {
                        Ok(Value::Integer(n))
                    } else if let Ok(f) = s.parse::<f64>() {
                        Ok(Value::Float(f))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                },
                _ => Err(EvalError::Type(format!("string->number: not a string at {call_span}"))),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("number->string requires 1 argument at {call_span}"))); }
            Ok(Value::Str(args[0].to_string(), true))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("symbol->string requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone(), true)),
                _ => Err(EvalError::Type(format!("symbol->string: not a symbol at {call_span}"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string->symbol requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!("string->symbol: not a string at {call_span}"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string-ref requires 2 arguments at {call_span}"))); }
            let s = match &args[0] {
                Value::Str(s, _) => s,
                _ => return Err(EvalError::Type(format!("string-ref: not a string at {call_span}"))),
            };
            let idx = as_int(&args[1], call_span)? as usize;
            let chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type(format!("string-ref: index out of range at {call_span}")));
            }
            Ok(Value::Char(chars[idx]))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-copy requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Str(s.clone(), true)),
                _ => Err(EvalError::Type(format!("string-copy: not a string at {call_span}"))),
            }
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string->list requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => Ok(make_list(s.chars().map(Value::Char).collect())),
                _ => Err(EvalError::Type(format!("string->list: not a string at {call_span}"))),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("list->string requires 1 argument at {call_span}"))); }
            let elems = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(format!("list->string: not a list at {call_span}")))?;
            let mut s = String::new();
            for e in &elems {
                match e {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type(format!("list->string: list must contain only characters at {call_span}"))),
                }
            }
            Ok(Value::Str(s, true))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char->integer requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type(format!("char->integer: not a char at {call_span}"))),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("integer->char requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Integer(n) => {
                    let c = char::from_u32(*n as u32).ok_or_else(|| EvalError::Type(format!("integer->char: invalid code point at {call_span}")))?;
                    Ok(Value::Char(c))
                }
                _ => Err(EvalError::Type(format!("integer->char: not an integer at {call_span}"))),
            }
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("apply requires at least 2 arguments at {call_span}")));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = value_to_vec(last).ok_or_else(|| EvalError::Type(format!("apply: last argument must be a list at {call_span}")))?;
            let mut combined_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            combined_args.extend(tail);
            apply_func(func, &combined_args, call_span, output)
        }
        // --- L09: eq? / equal? / map ---
        "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("eq? requires 2 arguments at {call_span}"))); }
            Ok(Value::Boolean(scheme_eq(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("equal? requires 2 arguments at {call_span}"))); }
            Ok(Value::Boolean(scheme_equal(&args[0], &args[1])))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity(format!("map requires at least 2 arguments at {call_span}"))); }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                value_to_vec(a).ok_or_else(|| EvalError::Type(format!("map: expected list at {call_span}")))
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_func(func, &call_args, call_span, output)?);
            }
            Ok(make_list(result))
        }
        "for-each" => {
            if args.len() < 2 { return Err(EvalError::Arity(format!("for-each requires at least 2 arguments at {call_span}"))); }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                value_to_vec(a).ok_or_else(|| EvalError::Type(format!("for-each: expected list at {call_span}")))
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply_func(func, &call_args, call_span, output)?;
            }
            Ok(Value::Void)
        }
        // --- L09: Numeric utilities ---
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("abs requires 1 argument at {call_span}"))); }
            Ok(Value::Integer(as_int(&args[0], call_span)?.abs()))
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("modulo requires 2 arguments at {call_span}"))); }
            let a = as_int(&args[0], call_span)?;
            let b = as_int(&args[1], call_span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(call_span.to_string())); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("remainder requires 2 arguments at {call_span}"))); }
            let a = as_int(&args[0], call_span)?;
            let b = as_int(&args[1], call_span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(call_span.to_string())); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("quotient requires 2 arguments at {call_span}"))); }
            let a = as_int(&args[0], call_span)?;
            let b = as_int(&args[1], call_span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(call_span.to_string())); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity(format!("min requires at least 1 argument at {call_span}"))); }
            let mut m = as_int(&args[0], call_span)?;
            for a in &args[1..] { m = m.min(as_int(a, call_span)?); }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity(format!("max requires at least 1 argument at {call_span}"))); }
            let mut m = as_int(&args[0], call_span)?;
            for a in &args[1..] { m = m.max(as_int(a, call_span)?); }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("expt requires 2 arguments at {call_span}"))); }
            let base = as_int(&args[0], call_span)?;
            let exp = as_int(&args[1], call_span)?;
            if exp < 0 { return Err(EvalError::Type(format!("expt: negative exponent at {call_span}"))); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("zero? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(as_int(&args[0], call_span)? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("positive? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(as_int(&args[0], call_span)? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("negative? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(as_int(&args[0], call_span)? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("odd? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(as_int(&args[0], call_span)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("even? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(as_int(&args[0], call_span)? % 2 == 0))
        }
        // --- L09: List utilities ---
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("list-ref requires 2 arguments at {call_span}"))); }
            let idx = as_int(&args[1], call_span)? as usize;
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = pair_cdr(&current, call_span)?;
            }
            pair_car(&current, call_span)
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("list-tail requires 2 arguments at {call_span}"))); }
            let idx = as_int(&args[1], call_span)? as usize;
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = pair_cdr(&current, call_span)?;
            }
            Ok(current)
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("list? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(is_list_safe(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("assoc requires 2 arguments at {call_span}"))); }
            let key = &args[0];
            let elems = value_to_vec(&args[1]).ok_or_else(|| EvalError::Type(format!("assoc: not a list at {call_span}")))?;
            for entry in &elems {
                if is_pair(entry) {
                    let entry_car = pair_car(entry, call_span)?;
                    if scheme_equal(key, &entry_car) {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        // --- L09: Character utilities ---
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-alphabetic? requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!("char-alphabetic?: not a char at {call_span}"))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-numeric? requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!("char-numeric?: not a char at {call_span}"))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-upcase requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type(format!("char-upcase: not a char at {call_span}"))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-downcase requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type(format!("char-downcase: not a char at {call_span}"))),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("char=? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("char=?: not chars at {call_span}"))),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("char<? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("char<?: not chars at {call_span}"))),
            }
        }
        // --- L09: String utilities ---
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string=? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("string=?: not strings at {call_span}"))),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string<? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("string<?: not strings at {call_span}"))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string-ci=? requires 2 arguments at {call_span}"))); }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type(format!("string-ci=?: not strings at {call_span}"))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-upcase requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Str(s.to_uppercase(), true)),
                _ => Err(EvalError::Type(format!("string-upcase: not a string at {call_span}"))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-downcase requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Str(s.to_lowercase(), true)),
                _ => Err(EvalError::Type(format!("string-downcase: not a string at {call_span}"))),
            }
        }
        "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("eqv? requires 2 arguments at {call_span}"))); }
            Ok(Value::Boolean(scheme_eqv(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!("make-vector requires 1 or 2 arguments at {call_span}")));
            }
            let len = as_int(&args[0], call_span)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("vector-ref requires 2 arguments at {call_span}"))); }
            let vec = match &args[0] {
                Value::Vector(v) => v.clone(),
                _ => return Err(EvalError::Type(format!("vector-ref: not a vector at {call_span}"))),
            };
            let idx = as_int(&args[1], call_span)? as usize;
            let borrowed = vec.borrow();
            if idx >= borrowed.len() { return Err(EvalError::Type(format!("vector-ref: index out of range at {call_span}"))); }
            Ok(borrowed[idx].clone())
        }
        "vector-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity(format!("vector-set! requires 3 arguments at {call_span}"))); }
            let vec = match &args[0] {
                Value::Vector(v) => v.clone(),
                _ => return Err(EvalError::Type(format!("vector-set!: not a vector at {call_span}"))),
            };
            let idx = as_int(&args[1], call_span)? as usize;
            let mut borrowed = vec.borrow_mut();
            if idx >= borrowed.len() { return Err(EvalError::Type(format!("vector-set!: index out of range at {call_span}"))); }
            borrowed[idx] = args[2].clone();
            Ok(Value::Void)
        }
        "vector-length" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("vector-length requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type(format!("vector-length: not a vector at {call_span}"))),
            }
        }
        "vector?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("vector? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("vector->list requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Vector(v) => Ok(make_list(v.borrow().clone())),
                _ => Err(EvalError::Type(format!("vector->list: not a vector at {call_span}"))),
            }
        }
        "list->vector" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("list->vector requires 1 argument at {call_span}"))); }
            let elems = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(format!("list->vector: not a list at {call_span}")))?;
            Ok(Value::Vector(Rc::new(RefCell::new(elems))))
        }
        "error" => {
            let msg: String = args.iter().map(|a| match a {
                Value::Str(s, _) => s.clone(),
                Value::Boolean(false) => "#f".to_string(),
                other => other.to_string(),
            }).collect::<Vec<_>>().join("");
            Err(EvalError::Type(format!("error: {msg}")))
        }
        _ if name.starts_with("__rctor_") => {
            // __rctor_<type_id>_<type_name>_<field1>,<field2>,...
            let rest = &name["__rctor_".len()..];
            let mut parts = rest.splitn(3, '_');
            let type_id: u64 = parts.next().unwrap().parse().unwrap();
            let type_name = parts.next().unwrap().to_string();
            let fields_str = parts.next().unwrap_or("");
            let field_names: Vec<&str> = if fields_str.is_empty() { vec![] } else { fields_str.split(',').collect() };
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{type_name} constructor expects {} arguments, got {} at {call_span}", field_names.len(), args.len()
                )));
            }
            let fields = field_names.iter().zip(args.iter())
                .map(|(name, val)| (name.to_string(), val.clone()))
                .collect();
            Ok(Value::Record { type_id, type_name, fields })
        }
        _ if name.starts_with("__rpred_") => {
            // __rpred_<type_id>
            let type_id: u64 = name["__rpred_".len()..].parse().unwrap();
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("predicate requires 1 argument at {call_span}")));
            }
            match &args[0] {
                Value::Record { type_id: tid, .. } if *tid == type_id => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            }
        }
        _ if name.starts_with("__racc_") => {
            // __racc_<type_id>_<field_name>
            let rest = &name["__racc_".len()..];
            let sep = rest.find('_').unwrap();
            let type_id: u64 = rest[..sep].parse().unwrap();
            let field_name = &rest[sep + 1..];
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("accessor requires 1 argument at {call_span}")));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if *tid == type_id => {
                    for (fname, val) in fields {
                        if fname == field_name {
                            return Ok(val.clone());
                        }
                    }
                    Err(EvalError::Type(format!("record has no field {field_name} at {call_span}")))
                }
                _ => Err(EvalError::Type(format!("accessor: not a matching record at {call_span}"))),
            }
        }
        _ => Err(EvalError::Unbound(format!("{name} at {call_span}"))),
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => Value::Str(s.clone(), false),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(elems) => make_list(elems.iter().map(expr_to_value).collect()),
    }
}

/// Parse a parameter list that may contain dot notation for rest params.
/// Returns (fixed_params, rest_param).
fn parse_params(exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    // Look for a dot symbol
    let dot_pos = exprs.iter().position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "."));
    if let Some(dp) = dot_pos {
        if dp + 1 != exprs.len() - 1 {
            return Err(EvalError::Syntax(format!("bad dot in parameter list at {span}")));
        }
        let fixed: Vec<String> = exprs[..dp].iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Syntax(format!("expected parameter name at {span}"))),
        }).collect::<Result<_, _>>()?;
        let rest = match &exprs[dp + 1].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Syntax(format!("expected rest parameter name at {span}"))),
        };
        Ok((fixed, Some(rest)))
    } else {
        let params: Vec<String> = exprs.iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Syntax(format!("expected parameter name at {span}"))),
        }).collect::<Result<_, _>>()?;
        Ok((params, None))
    }
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 { let t = b; b = a % b; a = t; }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    if d == 0 { return Value::Integer(0); } // shouldn't happen, caller checks
    let g = gcd(n, d);
    let (mut n, mut d) = (n / g, d / g);
    if d < 0 { n = -n; d = -d; }
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

fn value_to_f64(v: &Value, span: Span) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type(format!("expected number, got {v} at {span}"))),
    }
}

fn is_numeric(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

fn has_inexact(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Float(_)))
}

fn has_rational(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Rational(_, _)))
}

// Perform addition on two exact values (Integer or Rational), returning exact result
fn exact_add(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
        (Value::Integer(x), Value::Rational(n, d)) | (Value::Rational(n, d), Value::Integer(x)) => {
            make_rational(x * d + n, *d)
        }
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            make_rational(n1 * d2 + n2 * d1, d1 * d2)
        }
        _ => unreachable!(),
    }
}

fn exact_sub(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
        (Value::Integer(x), Value::Rational(n, d)) => make_rational(x * d - n, *d),
        (Value::Rational(n, d), Value::Integer(x)) => make_rational(n - x * d, *d),
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            make_rational(n1 * d2 - n2 * d1, d1 * d2)
        }
        _ => unreachable!(),
    }
}

fn exact_mul(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y),
        (Value::Integer(x), Value::Rational(n, d)) | (Value::Rational(n, d), Value::Integer(x)) => {
            make_rational(x * n, *d)
        }
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            make_rational(n1 * n2, d1 * d2)
        }
        _ => unreachable!(),
    }
}

fn exact_div(a: &Value, b: &Value, span: Span) -> Result<Value, EvalError> {
    let (an, ad) = match a {
        Value::Integer(x) => (*x, 1i64),
        Value::Rational(n, d) => (*n, *d),
        _ => unreachable!(),
    };
    let (bn, bd) = match b {
        Value::Integer(x) => (*x, 1i64),
        Value::Rational(n, d) => (*n, *d),
        _ => unreachable!(),
    };
    if bn == 0 {
        return Err(EvalError::DivisionByZero(span.to_string()));
    }
    Ok(make_rational(an * bd, ad * bn))
}

fn as_int(v: &Value, span: Span) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        Value::Rational(n, d) if n % d == 0 => Ok(n / d),
        _ => Err(EvalError::Type(format!("expected integer, got {v} at {span}"))),
    }
}

fn args_to_f64s(args: &[Value], span: Span) -> Result<Vec<f64>, EvalError> {
    args.iter().map(|a| value_to_f64(a, span)).collect()
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

// --- Macro support ---

#[derive(Debug, Clone)]
enum PatternBinding {
    Single(Expr),
    Ellipsis(Vec<Expr>),
}

fn is_macro_special(s: &str) -> bool {
    matches!(s, "quote" | "if" | "define" | "lambda" | "case-lambda" | "and" | "or"
        | "let" | "begin" | "cond" | "set!" | "string-set!" | "define-syntax" | "define-record-type"
        | "letrec" | "letrec*" | "case" | "do" | "...")
}

fn match_syntax_pattern(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        let has_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1].kind, ExprKind::Symbol(ref s) if s == "...");
        if has_ellipsis {
            let remaining = count_fixed_after(&pattern[pi + 2..]);
            if input.len() < ii + remaining {
                return false;
            }
            let available = input.len() - ii - remaining;
            match &pattern[pi].kind {
                ExprKind::Symbol(s) if !literals.contains(s) => {
                    bindings.insert(s.clone(), PatternBinding::Ellipsis(input[ii..ii + available].to_vec()));
                }
                _ => return false,
            }
            ii += available;
            pi += 2;
        } else {
            if ii >= input.len() {
                return false;
            }
            match &pattern[pi].kind {
                ExprKind::Symbol(s) if literals.contains(s) => {
                    if !matches!(&input[ii].kind, ExprKind::Symbol(ref is) if is == s) {
                        return false;
                    }
                }
                ExprKind::Symbol(s) if s != "_" => {
                    bindings.insert(s.clone(), PatternBinding::Single(input[ii].clone()));
                }
                ExprKind::List(sub_pat) => {
                    if let ExprKind::List(ref sub_input) = input[ii].kind {
                        if !match_syntax_pattern(sub_pat, sub_input, literals, bindings) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                _ => {}
            }
            pi += 1;
            ii += 1;
        }
    }
    ii == input.len()
}

fn count_fixed_after(pattern: &[Expr]) -> usize {
    let mut count = 0;
    let mut i = 0;
    while i < pattern.len() {
        if i + 1 < pattern.len() && matches!(&pattern[i + 1].kind, ExprKind::Symbol(ref s) if s == "...") {
            i += 2;
        } else {
            count += 1;
            i += 1;
        }
    }
    count
}

fn find_ellipsis_vars(template: &Expr, bindings: &HashMap<String, PatternBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    match &template.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(PatternBinding::Ellipsis(_))) {
                vars.push(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                vars.extend(find_ellipsis_vars(e, bindings));
            }
        }
        _ => {}
    }
    vars
}

fn collect_introduced_symbols(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
) {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if !pattern_vars.contains(s) && !is_macro_special(s) && !renames.contains_key(s) {
                renames.insert(s.clone(), gensym(s));
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_introduced_symbols(e, pattern_vars, renames);
            }
        }
        _ => {}
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    PatternBinding::Single(e) => e.clone(),
                    PatternBinding::Ellipsis(_) => template.clone(),
                }
            } else if let Some(renamed) = renames.get(s) {
                Expr::new(ExprKind::Symbol(renamed.clone()), template.span)
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && matches!(&elems[i + 1].kind, ExprKind::Symbol(ref s) if s == "...") {
                    let evars = find_ellipsis_vars(&elems[i], bindings);
                    if let Some(first_var) = evars.first() {
                        if let Some(PatternBinding::Ellipsis(items)) = bindings.get(first_var) {
                            let count = items.len();
                            for idx in 0..count {
                                let mut local = bindings.clone();
                                for evar in &evars {
                                    if let Some(PatternBinding::Ellipsis(eitems)) = bindings.get(evar) {
                                        if idx < eitems.len() {
                                            local.insert(evar.clone(), PatternBinding::Single(eitems[idx].clone()));
                                        }
                                    }
                                }
                                result.push(expand_template(&elems[i], &local, renames));
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_template(&elems[i], bindings, renames));
                    i += 1;
                }
            }
            Expr::new(ExprKind::List(result), template.span)
        }
        _ => template.clone(),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut env = default_env();
    let mut output = String::new();
    // Evaluate all top-level expressions as a single sequence so that
    // continuations captured by call/cc span across top-level expressions.
    let top = if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        let span = exprs[0].span;
        let mut elems = vec![Expr::new(ExprKind::Symbol("begin".into()), span)];
        elems.extend(exprs);
        Expr::new(ExprKind::List(elems), span)
    };
    let result = eval(&top, &mut env, &mut output)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut env = default_env();
    let mut output = String::new();
    let top = if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        let span = exprs[0].span;
        let mut elems = vec![Expr::new(ExprKind::Symbol("begin".into()), span)];
        elems.extend(exprs);
        Expr::new(ExprKind::List(elems), span)
    };
    let result = eval(&top, &mut env, &mut output)?;
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
