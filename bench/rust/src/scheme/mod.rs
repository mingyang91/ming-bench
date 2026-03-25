pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

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

fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    'tco: loop {
        let span = cur_expr.span;
        match cur_expr.kind {
            ExprKind::Integer(n) => return Ok(Value::Integer(n)),
            ExprKind::Float(f) => return Ok(Value::Float(f)),
            ExprKind::Rational(n, d) => return Ok(make_rational(n, d)),
            ExprKind::Boolean(b) => return Ok(Value::Boolean(b)),
            ExprKind::Char(c) => return Ok(Value::Char(c)),
            ExprKind::Str(s) => return Ok(Value::Str(s, false)),
            ExprKind::Symbol(s) => {
                return cur_env.get(&s).ok_or_else(|| EvalError::Unbound(format!("{s} at {span}")));
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Err(EvalError::Syntax(format!("empty application at {span}")));
                }

                // Check for special forms
                if let ExprKind::Symbol(ref head) = elems[0].kind {
                    match head.as_str() {
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Syntax(format!("quote requires 1 argument at {span}")));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Syntax(format!("if requires 2 or 3 arguments at {span}")));
                        }
                        let cond = eval(&elems[1], &mut cur_env, output)?;
                        if is_truthy(&cond) {
                            cur_expr = elems[2].clone();
                            continue 'tco;
                        } else if elems.len() == 4 {
                            cur_expr = elems[3].clone();
                            continue 'tco;
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("define requires at least 2 arguments at {span}")));
                        }
                        match &elems[1].kind {
                            ExprKind::Symbol(name) => {
                                let val = eval(&elems[2], &mut cur_env, output)?;
                                cur_env.set(name.clone(), val);
                                return Ok(Value::Void);
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
                                return Ok(Value::Void);
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
                            ExprKind::Symbol(s) => {
                                (vec![], Some(s.clone()))
                            }
                            _ => return Err(EvalError::Syntax(format!("lambda: expected parameter list at {span}"))),
                        };
                        let body = elems[2..].to_vec();
                        return Ok(Value::Lambda {
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
                        return Ok(Value::CaseLambda {
                            name: None,
                            clauses,
                        });
                    }
                    "and" => {
                        if elems.len() == 1 {
                            return Ok(Value::Boolean(true));
                        }
                        for a in &elems[1..elems.len()-1] {
                            let result = eval(a, &mut cur_env, output)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        // Last expression is tail position
                        cur_expr = elems.last().unwrap().clone();
                        continue 'tco;
                    }
                    "or" => {
                        if elems.len() == 1 {
                            return Ok(Value::Boolean(false));
                        }
                        for a in &elems[1..elems.len()-1] {
                            let result = eval(a, &mut cur_env, output)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        // Last expression is tail position
                        cur_expr = elems.last().unwrap().clone();
                        continue 'tco;
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("let requires bindings and body at {span}")));
                        }
                        let (name, bindings_expr, body_start) = match &elems[1].kind {
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
                        let mut params = Vec::new();
                        let mut init_vals = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            params.push(s.clone());
                                            init_vals.push(eval(&pair[1], &mut cur_env, output)?);
                                        }
                                        _ => return Err(EvalError::Syntax(format!("let: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("let: bad binding at {span}"))),
                            }
                        }
                        let body = elems[body_start..].to_vec();
                        if let Some(loop_name) = name {
                            // Named let: create lambda and apply with TCO
                            let lambda = Value::Lambda {
                                name: Some(loop_name.clone()),
                                params: params.clone(),
                                rest_param: None,
                                body: body.clone(),
                                env: cur_env.clone(),
                            };
                            let (mut local_env, skip) = setup_lambda_env(&lambda, &Some(loop_name), &params, &None, &body, &cur_env, &init_vals, span, output)?;
                            let rest = &body[skip..];
                            if rest.is_empty() { return Ok(Value::Void); }
                            for e in &rest[..rest.len()-1] {
                                eval(e, &mut local_env, output)?;
                            }
                            cur_expr = rest.last().unwrap().clone();
                            cur_env = local_env;
                            continue 'tco;
                        }
                        let mut local_env = Env::with_parent(cur_env.clone());
                        for (p, v) in params.iter().zip(init_vals.iter()) {
                            local_env.set(p.clone(), v.clone());
                        }
                        if body.is_empty() { return Ok(Value::Void); }
                        for expr in &body[..body.len()-1] {
                            eval(expr, &mut local_env, output)?;
                        }
                        cur_expr = body.last().unwrap().clone();
                        cur_env = local_env;
                        continue 'tco;
                    }
                    "let*" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("let* requires bindings and body at {span}")));
                        }
                        let bindings_list = match &elems[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("let*: expected bindings list at {span}"))),
                        };
                        let mut local_env = Env::with_parent(cur_env.clone());
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            let val = eval(&pair[1], &mut local_env, output)?;
                                            local_env.set(s.clone(), val);
                                        }
                                        _ => return Err(EvalError::Syntax(format!("let*: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("let*: bad binding at {span}"))),
                            }
                        }
                        let body = &elems[2..];
                        if body.is_empty() { return Ok(Value::Void); }
                        for expr in &body[..body.len()-1] {
                            eval(expr, &mut local_env, output)?;
                        }
                        cur_expr = body.last().unwrap().clone();
                        cur_env = local_env;
                        continue 'tco;
                    }
                    "begin" => {
                        if elems.len() <= 1 { return Ok(Value::Void); }
                        for expr in &elems[1..elems.len()-1] {
                            eval(expr, &mut cur_env, output)?;
                        }
                        cur_expr = elems.last().unwrap().clone();
                        continue 'tco;
                    }
                    "cond" => {
                        for clause in &elems[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            if parts.len() <= 1 { return Ok(Value::Void); }
                                            for expr in &parts[1..parts.len()-1] {
                                                eval(expr, &mut cur_env, output)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            continue 'tco;
                                        }
                                    }
                                    let test = eval(&parts[0], &mut cur_env, output)?;
                                    if is_truthy(&test) {
                                        if parts.len() <= 1 { return Ok(test); }
                                        for expr in &parts[1..parts.len()-1] {
                                            eval(expr, &mut cur_env, output)?;
                                        }
                                        cur_expr = parts.last().unwrap().clone();
                                        continue 'tco;
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("cond: bad clause at {span}"))),
                            }
                        }
                        return Ok(Value::Void);
                    }
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Syntax(format!("set! requires 2 arguments at {span}")));
                        }
                        let var_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("set!: expected variable name at {span}"))),
                        };
                        let val = eval(&elems[2], &mut cur_env, output)?;
                        if !cur_env.set_existing(&var_name, val) {
                            return Err(EvalError::Unbound(format!("{var_name} at {span}")));
                        }
                        return Ok(Value::Void);
                    }
                    "string-set!" => {
                        if elems.len() != 4 {
                            return Err(EvalError::Syntax(format!("string-set! requires 3 arguments at {span}")));
                        }
                        let var_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type(format!("string-set!: strings are immutable at {span}"))),
                        };
                        let idx = match eval(&elems[2], &mut cur_env, output)? {
                            Value::Integer(n) => n as usize,
                            _ => return Err(EvalError::Type(format!("string-set!: expected integer index at {span}"))),
                        };
                        let ch = match eval(&elems[3], &mut cur_env, output)? {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type(format!("string-set!: expected char at {span}"))),
                        };
                        let current = cur_env.get(&var_name).ok_or_else(|| EvalError::Unbound(format!("{var_name} at {span}")))?;
                        match current {
                            Value::Str(s, true) => {
                                let mut chars: Vec<char> = s.chars().collect();
                                if idx >= chars.len() {
                                    return Err(EvalError::Type(format!("string-set!: index out of range at {span}")));
                                }
                                chars[idx] = ch;
                                let new_s: String = chars.into_iter().collect();
                                cur_env.set_existing(&var_name, Value::Str(new_s, true));
                                return Ok(Value::Void);
                            }
                            Value::Str(_, false) => {
                                return Err(EvalError::Type(format!("string-set!: strings are immutable at {span}")));
                            }
                            _ => return Err(EvalError::Type(format!("string-set!: not a string at {span}"))),
                        }
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
                        return Ok(Value::Void);
                    }
                    "letrec" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("letrec requires bindings and body at {span}")));
                        }
                        let bindings_list = match &elems[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("letrec: expected bindings list at {span}"))),
                        };
                        let mut local_env = Env::with_parent(cur_env.clone());
                        let mut names = Vec::new();
                        let mut init_exprs = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            names.push(s.clone());
                                            init_exprs.push(&pair[1]);
                                            local_env.set(s.clone(), Value::Void);
                                        }
                                        _ => return Err(EvalError::Syntax(format!("letrec: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("letrec: bad binding at {span}"))),
                            }
                        }
                        let vals: Vec<Value> = init_exprs.iter()
                            .map(|e| eval(e, &mut local_env, output))
                            .collect::<Result<_, _>>()?;
                        for (name, val) in names.iter().zip(vals.iter()) {
                            local_env.set(name.clone(), val.clone());
                        }
                        let body = &elems[2..];
                        if body.is_empty() { return Ok(Value::Void); }
                        for expr in &body[..body.len()-1] {
                            eval(expr, &mut local_env, output)?;
                        }
                        cur_expr = body.last().unwrap().clone();
                        cur_env = local_env;
                        continue 'tco;
                    }
                    "letrec*" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("letrec* requires bindings and body at {span}")));
                        }
                        let bindings_list = match &elems[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("letrec*: expected bindings list at {span}"))),
                        };
                        let mut local_env = Env::with_parent(cur_env.clone());
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            let val = eval(&pair[1], &mut local_env, output)?;
                                            local_env.set(s.clone(), val);
                                        }
                                        _ => return Err(EvalError::Syntax(format!("letrec*: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("letrec*: bad binding at {span}"))),
                            }
                        }
                        let body = &elems[2..];
                        if body.is_empty() { return Ok(Value::Void); }
                        for expr in &body[..body.len()-1] {
                            eval(expr, &mut local_env, output)?;
                        }
                        cur_expr = body.last().unwrap().clone();
                        cur_env = local_env;
                        continue 'tco;
                    }
                    "case" => {
                        if elems.len() < 2 {
                            return Err(EvalError::Syntax(format!("case requires key and clauses at {span}")));
                        }
                        let key = eval(&elems[1], &mut cur_env, output)?;
                        for clause in &elems[2..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            let mut result = Value::Void;
                                            for expr in &parts[1..] {
                                                result = eval(expr, &mut cur_env, output)?;
                                            }
                                            return Ok(result);
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
                                        let mut result = Value::Void;
                                        for expr in &parts[1..] {
                                            result = eval(expr, &mut cur_env, output)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("case: bad clause at {span}"))),
                            }
                        }
                        return Ok(Value::Void);
                    }
                    "do" => {
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
                        let mut var_names = Vec::new();
                        let mut step_exprs: Vec<Option<&Expr>> = Vec::new();
                        let mut local_env = Env::with_parent(cur_env.clone());
                        for vc in var_clauses {
                            match &vc.kind {
                                ExprKind::List(parts) if parts.len() >= 2 => {
                                    let name = match &parts[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Syntax(format!("do: expected variable name at {span}"))),
                                    };
                                    let init = eval(&parts[1], &mut cur_env, output)?;
                                    local_env.set(name.clone(), init);
                                    var_names.push(name);
                                    step_exprs.push(if parts.len() >= 3 { Some(&parts[2]) } else { None });
                                }
                                _ => return Err(EvalError::Syntax(format!("do: bad var clause at {span}"))),
                            }
                        }
                        let body = &elems[3..];
                        loop {
                            let test_result = eval(&test_clause[0], &mut local_env, output)?;
                            if is_truthy(&test_result) {
                                let mut result = Value::Void;
                                for expr in &test_clause[1..] {
                                    result = eval(expr, &mut local_env, output)?;
                                }
                                return Ok(result);
                            }
                            for expr in body {
                                eval(expr, &mut local_env, output)?;
                            }
                            let new_vals: Vec<Option<Value>> = step_exprs.iter()
                                .map(|step| {
                                    match step {
                                        Some(expr) => Ok(Some(eval(expr, &mut local_env, output)?)),
                                        None => Ok(None),
                                    }
                                })
                                .collect::<Result<_, EvalError>>()?;
                            for (name, new_val) in var_names.iter().zip(new_vals.into_iter()) {
                                if let Some(v) = new_val {
                                    local_env.set(name.clone(), v);
                                }
                            }
                        }
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
                        return Ok(Value::Void);
                    }
                    _ => {
                        // Check for macro invocation
                        if let Some(Value::Macro { literals: mac_lits, rules: mac_rules, def_env: mac_def_env }) = cur_env.get(head) {
                            let input_elems = &elems[1..];
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
                                    cur_expr = expanded;
                                    continue 'tco;
                                }
                            }
                            return Err(EvalError::Syntax(format!("no matching pattern for macro {head} at {span}")));
                        }
                    }
                    }
                }

                // Function application
                let func = eval(&elems[0], &mut cur_env, output)?;
                let args: Vec<Value> = elems[1..].iter()
                    .map(|a| eval(a, &mut cur_env, output))
                    .collect::<Result<_, _>>()?;

                // TCO: inline lambda application for tail calls
                if let Value::Lambda { ref name, ref params, ref rest_param, ref body, env: ref closure_env } = func {
                    let (mut local_env, skip) = setup_lambda_env(&func, name, params, rest_param, body, closure_env, &args, span, output)?;
                    let rest = &body[skip..];
                    if rest.is_empty() { return Ok(Value::Void); }
                    for e in &rest[..rest.len()-1] {
                        eval(e, &mut local_env, output)?;
                    }
                    cur_expr = rest.last().unwrap().clone();
                    cur_env = local_env;
                    continue 'tco;
                }
                if let Value::CaseLambda { ref name, ref clauses } = func {
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
                            let (mut local_env, skip) = setup_lambda_env(&lambda, name, params, rest_param, body, clause_env, &args, span, output)?;
                            let rest = &body[skip..];
                            if rest.is_empty() { return Ok(Value::Void); }
                            for e in &rest[..rest.len()-1] {
                                eval(e, &mut local_env, output)?;
                            }
                            cur_expr = rest.last().unwrap().clone();
                            cur_env = local_env;
                            continue 'tco;
                        }
                    }
                    return Err(EvalError::Arity(format!(
                        "no matching clause for {} arguments at {span}", args.len()
                    )));
                }

                return apply_func(&func, &args, span, output);
            }
        }
    }
}

fn apply_func(func: &Value, args: &[Value], call_span: Span, output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args, call_span, output),
        Value::Lambda { name, params, rest_param, body, env } => {
            let (mut local_env, skip) = setup_lambda_env(func, name, params, rest_param, body, env, args, call_span, output)?;
            let rest = &body[skip..];
            let mut result = Value::Void;
            for e in rest {
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
            Ok(Value::Boolean(matches!(&args[0], Value::Lambda { .. } | Value::CaseLambda { .. } | Value::Builtin(_))))
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
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
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
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
