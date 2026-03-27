pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);
static CALLCC_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("#{}#{}", base, n)
}

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
    static CALLCC_RETURN: RefCell<Option<Value>> = RefCell::new(None);
    static CONT_FRAMES: RefCell<Vec<ContinuationFrame>> = RefCell::new(Vec::new());
    static RESUME_FRAMES: RefCell<Vec<ContinuationFrame>> = RefCell::new(Vec::new());
    static CONTINUATION_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    static PENDING_CONTINUATION: RefCell<Option<Rc<ContinuationData>>> = RefCell::new(None);
    static RAISED_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    static EXCEPTION_HANDLERS: RefCell<Vec<Value>> = RefCell::new(Vec::new());
}

#[derive(Debug, Clone)]
struct ContinuationFrame {
    exprs: Vec<Expr>,
    env: Env,
}

#[derive(Debug, Clone)]
struct ContinuationData {
    id: usize,
    frames: Vec<ContinuationFrame>,
}

fn output_write(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

// ── Environment ─────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

#[derive(Debug, Clone)]
struct Env(Rc<RefCell<EnvInner>>);

impl Env {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    fn with_parent(parent: &Env) -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(parent.clone()),
        })))
    }

    fn get(&self, name: &str) -> Option<Value> {
        let inner = self.0.borrow();
        if let Some(v) = inner.bindings.get(name) {
            Some(v.clone())
        } else if let Some(ref parent) = inner.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    fn set_existing(&self, name: &str, val: Value) -> bool {
        let mut inner = self.0.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), val);
            return true;
        }
        if let Some(ref parent) = inner.parent {
            parent.set_existing(name, val)
        } else {
            false
        }
    }
}

// ── Source Position ──────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq)]
struct Span {
    line: usize,
    col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ── Macros ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct SyntaxRulesMacro {
    literals: Vec<String>,
    rules: Vec<(Expr, Expr)>, // (pattern, template) pairs
    def_env: Env,
}

// ── Values ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Float(f64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Rc<RefCell<(Value, Value)>>),
    Lambda(Vec<String>, Option<String>, Vec<Expr>, Env),
    Builtin(String),
    Macro(SyntaxRulesMacro),
    CaseLambda(Vec<(Vec<String>, Option<String>, Vec<Expr>, Env)>), // clauses: (params, rest, body, env)
    Record(usize, Vec<Value>), // type_id, field values
    Vector(Rc<RefCell<Vec<Value>>>),
    Void,
    TailCall(Box<(Expr, Env)>),
    Continuation(Rc<ContinuationData>),
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

fn vec_to_pair_list(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

fn pair_list_to_vec(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::List(items) => Some(items.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = v.clone();
            let mut seen = HashSet::new();
            loop {
                match &current {
                    Value::List(items) if items.is_empty() => return Some(result),
                    Value::List(items) => {
                        result.extend(items.iter().cloned());
                        return Some(result);
                    }
                    Value::Pair(p) => {
                        let ptr = Rc::as_ptr(p) as usize;
                        if !seen.insert(ptr) { return None; }
                        let (car, cdr) = { let b = p.borrow(); (b.0.clone(), b.1.clone()) };
                        result.push(car);
                        current = cdr;
                    }
                    _ => return None,
                }
            }
        }
        _ => None,
    }
}

fn value_car(v: &Value) -> Result<Value, EvalError> {
    match v {
        Value::Pair(p) => Ok(p.borrow().0.clone()),
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::List(_) => Err(EvalError::Type("car: empty list".into())),
        _ => Err(EvalError::Type("car: not a pair".into())),
    }
}

fn value_cdr(v: &Value) -> Result<Value, EvalError> {
    match v {
        Value::Pair(p) => Ok(p.borrow().1.clone()),
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::List(_) => Err(EvalError::Type("cdr: empty list".into())),
        _ => Err(EvalError::Type("cdr: not a pair".into())),
    }
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    if d == 0 { panic!("division by zero in make_rational"); }
    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
            (Value::Record(id1, f1), Value::Record(id2, f2)) => id1 == id2 && f1 == f2,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Void, Value::Void) => true,
            (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id,
            (Value::Macro(_), Value::Macro(_)) => false,
            (Value::TailCall(_), _) | (_, Value::TailCall(_)) => false,
            _ => false,
        }
    }
}

fn format_value(val: &Value, seen: &mut HashSet<usize>) -> String {
    match val {
        Value::Integer(n) => format!("{}", n),
        Value::Rational(n, d) => format!("{}/{}", n, d),
        Value::Float(v) => {
            if v.fract() == 0.0 && v.is_finite() {
                format!("{:.1}", v)
            } else {
                format!("{}", v)
            }
        }
        Value::Boolean(true) => "#t".to_string(),
        Value::Boolean(false) => "#f".to_string(),
        Value::Str(s) => format!("\"{}\"", s),
        Value::Symbol(s) => s.clone(),
        Value::Char(c) => match c {
            ' ' => "#\\space".to_string(),
            '\n' => "#\\newline".to_string(),
            '\t' => "#\\tab".to_string(),
            _ => format!("#\\{}", c),
        },
        Value::Pair(p) => {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                return "(...)".to_string();
            }
            let mut s = String::from("(");
            let (car, cdr) = { let b = p.borrow(); (b.0.clone(), b.1.clone()) };
            s.push_str(&format_value(&car, seen));
            format_pair_tail(&cdr, &mut s, seen);
            s.push(')');
            seen.remove(&ptr);
            s
        }
        Value::List(items) => {
            let mut s = String::from("(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&format_value(item, seen));
            }
            s.push(')');
            s
        }
        Value::Lambda(..) | Value::CaseLambda(..) | Value::Builtin(..) | Value::Continuation(..) => "#<procedure>".to_string(),
        Value::Record(_, _) => "#<record>".to_string(),
        Value::Vector(items) => {
            let items = items.borrow();
            let mut s = String::from("#(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&format_value(item, seen));
            }
            s.push(')');
            s
        }
        Value::Macro(_) => "#<macro>".to_string(),
        Value::Void => String::new(),
        Value::TailCall(_) => "#<tail-call>".to_string(),
    }
}

fn format_pair_tail(val: &Value, s: &mut String, seen: &mut HashSet<usize>) {
    match val {
        Value::Pair(p) => {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                s.push_str(" . (...)");
                return;
            }
            let (car, cdr) = { let b = p.borrow(); (b.0.clone(), b.1.clone()) };
            s.push(' ');
            s.push_str(&format_value(&car, seen));
            format_pair_tail(&cdr, s, seen);
            seen.remove(&ptr);
        }
        Value::List(items) if items.is_empty() => {}
        Value::List(items) => {
            for item in items {
                s.push(' ');
                s.push_str(&format_value(item, seen));
            }
        }
        other => {
            s.push_str(" . ");
            s.push_str(&format_value(other, seen));
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_value(self, &mut HashSet::new()))
    }
}

// ── Tokenizer ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    Quote,
}

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    span: Span,
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
}

fn tokenize(input: &str) -> Result<Vec<SpannedToken>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' { i += 1; col += 1; }
            }
            '(' => { tokens.push(SpannedToken { token: Token::LParen, span: Span { line, col } }); i += 1; col += 1; }
            ')' => { tokens.push(SpannedToken { token: Token::RParen, span: Span { line, col } }); i += 1; col += 1; }
            '\'' => { tokens.push(SpannedToken { token: Token::Quote, span: Span { line, col } }); i += 1; col += 1; }
            '"' => {
                let start_span = Span { line, col };
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
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), span: start_span });
            }
            '#' => {
                let start_span = Span { line, col };
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            if i + 2 >= chars.len() || is_delimiter(chars[i + 2]) {
                                tokens.push(SpannedToken { token: Token::Boolean(true), span: start_span });
                                i += 2; col += 2;
                            } else {
                                return Err(EvalError::Parse("unexpected character after #t".into()));
                            }
                        }
                        'f' => {
                            if i + 2 >= chars.len() || is_delimiter(chars[i + 2]) {
                                tokens.push(SpannedToken { token: Token::Boolean(false), span: start_span });
                                i += 2; col += 2;
                            } else {
                                return Err(EvalError::Parse("unexpected character after #f".into()));
                            }
                        }
                        '\\' => {
                            // Character literal: #\x, #\space, #\newline
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse("unexpected end of character literal".into()));
                            }
                            let start_ch = i;
                            // Read the character name (could be multi-char like "space", "newline")
                            if chars[i].is_alphabetic() && i + 1 < chars.len() && chars[i + 1].is_alphabetic() {
                                // Multi-character name
                                while i < chars.len() && !is_delimiter(chars[i]) && chars[i] != ')' {
                                    i += 1; col += 1;
                                }
                                let name: String = chars[start_ch..i].iter().collect();
                                let c = match name.as_str() {
                                    "space" => ' ',
                                    "newline" => '\n',
                                    "tab" => '\t',
                                    _ => return Err(EvalError::Parse(format!("unknown character name: {}", name))),
                                };
                                tokens.push(SpannedToken { token: Token::Char(c), span: start_span });
                            } else {
                                // Single character
                                let c = chars[i];
                                i += 1; col += 1;
                                tokens.push(SpannedToken { token: Token::Char(c), span: start_span });
                            }
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected character after #: {}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                let start_span = Span { line, col };
                let start = i;
                while i < chars.len() && !is_delimiter(chars[i]) {
                    i += 1; col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(SpannedToken { token: Token::Integer(n), span: start_span });
                } else if let Some(slash_pos) = word.find('/') {
                    if slash_pos > 0 && slash_pos < word.len() - 1 {
                        if let (Ok(n), Ok(d)) = (word[..slash_pos].parse::<i64>(), word[slash_pos+1..].parse::<i64>()) {
                            tokens.push(SpannedToken { token: Token::Rational(n, d), span: start_span });
                        } else {
                            tokens.push(SpannedToken { token: Token::Symbol(word), span: start_span });
                        }
                    } else {
                        tokens.push(SpannedToken { token: Token::Symbol(word), span: start_span });
                    }
                } else if word.contains('.') {
                    if let Ok(f) = word.parse::<f64>() {
                        tokens.push(SpannedToken { token: Token::Float(f), span: start_span });
                    } else {
                        tokens.push(SpannedToken { token: Token::Symbol(word), span: start_span });
                    }
                } else {
                    tokens.push(SpannedToken { token: Token::Symbol(word), span: start_span });
                }
            }
        }
    }
    Ok(tokens)
}

// ── Parser ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Expr>),
}

fn parse(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let st = &tokens[*pos];
    let span = st.span;
    match &st.token {
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr { kind: ExprKind::Integer(n), span }) }
        Token::Rational(n, d) => { let (n, d) = (*n, *d); *pos += 1; Ok(Expr { kind: ExprKind::Rational(n, d), span }) }
        Token::Float(f) => { let f = *f; *pos += 1; Ok(Expr { kind: ExprKind::Float(f), span }) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr { kind: ExprKind::Boolean(b), span }) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr { kind: ExprKind::Str(s), span }) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr { kind: ExprKind::Symbol(s), span }) }
        Token::Char(c) => { let c = *c; *pos += 1; Ok(Expr { kind: ExprKind::Char(c), span }) }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), span },
                    inner,
                ]),
                span,
            })
        }
        Token::LParen => {
            *pos += 1;
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                items.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing parenthesis".into()));
            }
            *pos += 1; // consume RParen
            Ok(Expr { kind: ExprKind::List(items), span })
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
    }
}

fn parse_all(tokens: &[SpannedToken]) -> Result<Vec<Expr>, EvalError> {
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ───────────────────────────────────────────────────────

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

/// Wrap an error with span info if it doesn't already have position info.
fn with_span(err: EvalError, span: Span) -> EvalError {
    if matches!(&err, EvalError::ContinuationEscape(_) | EvalError::RaisedException) {
        return err;
    }
    let msg = err.to_string();
    // Don't double-annotate — check for pattern like "at N:N"
    if msg.contains(&format!("at {}", span)) {
        return err;
    }
    EvalError::Generic(format!("{} at {}", msg, span))
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let mut result = eval_inner(expr, env).map_err(|e| with_span(e, expr.span))?;
    while let Value::TailCall(tc) = result {
        let (next_expr, next_env) = *tc;
        result = eval_inner(&next_expr, &next_env).map_err(|e| with_span(e, next_expr.span))?;
    }
    Ok(result)
}

fn eval_inner(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Rational(n, d) => Ok(make_rational(*n, *d)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => {
            env.get(s).ok_or_else(|| EvalError::UnboundVariable(s.clone()))
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires exactly 1 argument".into()));
                        }
                        return Ok(expr_to_value(&items[1]));
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires params and body".into()));
                        }
                        let (params, rest) = match &items[1].kind {
                            ExprKind::List(ps) => parse_params(ps)?,
                            ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                            _ => return Err(EvalError::Parse("lambda params must be a list or symbol".into())),
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(params, rest, body, env.clone()));
                    }
                    "case-lambda" => {
                        if items.len() < 2 {
                            return Err(EvalError::Arity("case-lambda requires at least one clause".into()));
                        }
                        let mut clauses = Vec::new();
                        for clause in &items[1..] {
                            if let ExprKind::List(clause_items) = &clause.kind {
                                if clause_items.len() < 2 {
                                    return Err(EvalError::Parse("case-lambda clause requires params and body".into()));
                                }
                                let (params, rest) = match &clause_items[0].kind {
                                    ExprKind::List(ps) => parse_params(ps)?,
                                    ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                                    _ => return Err(EvalError::Parse("case-lambda clause params must be a list or symbol".into())),
                                };
                                let body = clause_items[1..].to_vec();
                                clauses.push((params, rest, body, env.clone()));
                            } else {
                                return Err(EvalError::Parse("case-lambda clause must be a list".into()));
                            }
                        }
                        return Ok(Value::CaseLambda(clauses));
                    }
                    "procedure?" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("procedure? requires exactly 1 argument".into()));
                        }
                        let val = eval(&items[1], env)?;
                        return Ok(Value::Boolean(matches!(val, Value::Lambda(..) | Value::CaseLambda(..) | Value::Builtin(..) | Value::Continuation(..))));
                    }
                    "+" => return eval_add(&items[1..], env),
                    "-" => return eval_sub(&items[1..], env),
                    "*" => return eval_mul(&items[1..], env),
                    "/" => return eval_div(&items[1..], env),
                    "<" => return eval_cmp(&items[1..], env, |a: f64, b: f64| a < b),
                    ">" => return eval_cmp(&items[1..], env, |a: f64, b: f64| a > b),
                    "=" => return eval_cmp(&items[1..], env, |a: f64, b: f64| a == b),
                    "<=" => return eval_cmp(&items[1..], env, |a: f64, b: f64| a <= b),
                    ">=" => return eval_cmp(&items[1..], env, |a: f64, b: f64| a >= b),
                    "not" => return eval_not(&items[1..], env),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    "begin" => return eval_begin(&items[1..], env),
                    "cond" => return eval_cond(&items[1..], env),
                    "let" => return eval_let(&items[1..], env),
                    "cons" => return eval_cons(&items[1..], env),
                    "car" => return eval_car(&items[1..], env),
                    "cdr" => return eval_cdr(&items[1..], env),
                    "null?" => return eval_null(&items[1..], env),
                    "list" => return eval_list(&items[1..], env),
                    "length" => return eval_length(&items[1..], env),
                    "append" => return eval_append(&items[1..], env),
                    "number?" => return eval_type_pred(&items[1..], env, "number"),
                    "string?" => return eval_type_pred(&items[1..], env, "string"),
                    "boolean?" => return eval_type_pred(&items[1..], env, "boolean"),
                    "pair?" => return eval_type_pred(&items[1..], env, "pair"),
                    "symbol?" => return eval_type_pred(&items[1..], env, "symbol"),
                    "char?" => return eval_type_pred(&items[1..], env, "char"),
                    "display" => return eval_display(&items[1..], env),
                    "write" => return eval_write(&items[1..], env),
                    "newline" => return eval_newline(&items[1..]),
                    "string-append" => return eval_string_append(&items[1..], env),
                    "string-length" => return eval_string_length(&items[1..], env),
                    "substring" => return eval_substring(&items[1..], env),
                    "string->number" => return eval_string_to_number(&items[1..], env),
                    "number->string" => return eval_number_to_string(&items[1..], env),
                    "symbol->string" => return eval_symbol_to_string(&items[1..], env),
                    "string->symbol" => return eval_string_to_symbol(&items[1..], env),
                    "string-ref" => return eval_string_ref(&items[1..], env),
                    "string-copy" => return eval_string_copy(&items[1..], env),
                    "string-set!" => return eval_string_set(&items[1..], env),
                    "string->list" => return eval_string_to_list(&items[1..], env),
                    "list->string" => return eval_list_to_string(&items[1..], env),
                    "char->integer" => return eval_char_to_integer(&items[1..], env),
                    "integer->char" => return eval_integer_to_char(&items[1..], env),
                    "let*" => return eval_let_star(&items[1..], env),
                    "letrec" => return eval_letrec(&items[1..], env),
                    "letrec*" => return eval_letrec_star(&items[1..], env),
                    "case" => return eval_case(&items[1..], env),
                    "do" => return eval_do(&items[1..], env),
                    "when" => return eval_when(&items[1..], env),
                    "unless" => return eval_unless(&items[1..], env),
                    "set!" => return eval_set(&items[1..], env),
                    "define-syntax" => return eval_define_syntax(&items[1..], env),
                    "define-record-type" => return eval_define_record_type(&items[1..], env),
                    "error" => {
                        let msg: Vec<String> = items[1..].iter().map(|a| {
                            match eval(a, env) {
                                Ok(v) => display_value(&v),
                                Err(e) => format!("<error: {}>", e),
                            }
                        }).collect();
                        return Err(EvalError::Generic(format!("error: {}", msg.join(" "))));
                    }
                    "dynamic-wind" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity("dynamic-wind requires 3 arguments".into()));
                        }
                        let in_thunk = eval(&items[1], env)?;
                        let body_thunk = eval(&items[2], env)?;
                        let out_thunk = eval(&items[3], env)?;

                        // Protect in-thunk from consuming continuation resume state
                        let saved_resume = RESUME_FRAMES.with(|rf| std::mem::take(&mut *rf.borrow_mut()));
                        let saved_callcc = CALLCC_RETURN.with(|r| r.borrow_mut().take());
                        apply(&in_thunk, &[])?;
                        RESUME_FRAMES.with(|rf| *rf.borrow_mut() = saved_resume);
                        CALLCC_RETURN.with(|r| *r.borrow_mut() = saved_callcc);

                        // Call body-thunk (may consume resume state)
                        let body_result = apply(&body_thunk, &[]);

                        match body_result {
                            Ok(val) => {
                                // Normal return: run out-thunk
                                apply(&out_thunk, &[])?;
                                return Ok(val);
                            }
                            Err(EvalError::ContinuationEscape(id)) => {
                                // Non-local exit: run out-thunk then re-throw
                                apply(&out_thunk, &[])?;
                                return Err(EvalError::ContinuationEscape(id));
                            }
                            Err(e) => {
                                // Other error: run out-thunk then re-throw
                                let _ = apply(&out_thunk, &[]);
                                return Err(e);
                            }
                        }
                    }
                    "call/cc" | "call-with-current-continuation" => {
                        let override_val = CALLCC_RETURN.with(|r| r.borrow_mut().take());
                        if let Some(val) = override_val {
                            return Ok(val);
                        }
                        if items.len() != 2 {
                            return Err(EvalError::Arity("call/cc requires exactly 1 argument".into()));
                        }
                        let proc = eval(&items[1], env)?;
                        return eval_callcc_with_proc(&proc);
                    }
                    "guard" => {
                        // (guard (var clause ...) body ...)
                        // clause = (test expr ...) or (test => expr) or (else expr ...)
                        if items.len() < 3 {
                            return Err(EvalError::Arity("guard requires clauses and body".into()));
                        }
                        let clauses_expr = match &items[1].kind {
                            ExprKind::List(c) if c.len() >= 1 => c,
                            _ => return Err(EvalError::Parse("guard: invalid clause form".into())),
                        };
                        let var_name = match &clauses_expr[0].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Parse("guard: first element must be variable name".into())),
                        };
                        let clauses = &clauses_expr[1..];
                        let body = &items[2..];

                        // Evaluate body, catching any raised exception
                        let body_result = {
                            let mut result = Ok(Value::Void);
                            for expr in body {
                                result = eval(expr, env);
                                if result.is_err() {
                                    break;
                                }
                            }
                            result
                        };

                        match body_result {
                            Ok(val) => return Ok(val),
                            Err(EvalError::RaisedException) => {
                                let exn = RAISED_VALUE.with(|r| r.borrow_mut().take())
                                    .unwrap_or(Value::Void);
                                // Bind exception to var and try clauses
                                let guard_env = Env::with_parent(env);
                                guard_env.set(var_name.clone(), exn.clone());

                                let mut matched = false;
                                for clause in clauses {
                                    match &clause.kind {
                                        ExprKind::List(parts) if !parts.is_empty() => {
                                            if let ExprKind::Symbol(s) = &parts[0].kind {
                                                if s == "else" {
                                                    // else clause
                                                    let mut val = Value::Void;
                                                    for expr in &parts[1..] {
                                                        val = eval(expr, &guard_env)?;
                                                    }
                                                    return Ok(val);
                                                }
                                            }
                                            let test = eval(&parts[0], &guard_env)?;
                                            if is_truthy(&test) {
                                                if parts.len() == 1 {
                                                    return Ok(test);
                                                }
                                                let mut val = Value::Void;
                                                for expr in &parts[1..] {
                                                    val = eval(expr, &guard_env)?;
                                                }
                                                return Ok(val);
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                                if !matched {
                                    // No clause matched, re-raise
                                    RAISED_VALUE.with(|r| *r.borrow_mut() = Some(exn));
                                    return Err(EvalError::RaisedException);
                                }
                                unreachable!()
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    "set-car!" | "set-cdr!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity(format!("{} requires exactly 2 arguments", op)));
                        }
                        let pair_val = eval(&items[1], env)?;
                        let new_val = eval(&items[2], env)?;
                        match &pair_val {
                            Value::Pair(p) => {
                                if op == "set-car!" {
                                    p.borrow_mut().0 = new_val;
                                } else {
                                    p.borrow_mut().1 = new_val;
                                }
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type(format!("{}: not a mutable pair", op))),
                        }
                    }
                    "eq?" | "eqv?" | "equal?" | "abs" | "modulo" | "remainder" | "quotient"
                    | "min" | "max" | "expt" | "zero?" | "positive?" | "negative?"
                    | "odd?" | "even?" | "list-ref" | "list-tail" | "list?" | "assoc"
                    | "map" | "for-each"
                    | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
                    | "char=?" | "char<?"
                    | "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase"
                    | "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
                    | "vector?" | "vector->list" | "list->vector"
                    | "caar" | "cadr" | "cdar" | "cddr" | "caddr" | "cdddr" | "cadddr" | "caddar"
                    | "caaar" | "caadr" | "cdaar" | "cdadr" | "cddar"
                    | "caaaar" | "caaadr" | "caadar" | "caaddr" | "cadaar" | "cadadr"
                    | "cdaaar" | "cdaadr" | "cdadar" | "cdaddr" | "cddaar" | "cddadr" | "cdddar" | "cddddr"
                    | "memq" | "memv" | "assq" | "reverse"
                    | "member" | "assv" | "gcd" | "lcm" | "truncate" | "round"
                    | "make-string" | "string" | "string>?" | "string<=?" | "string>=?"
                    | "cadar"
                    | "string->number" | "string-ref" | "substring" | "string-copy"
                    | "string->list" | "list->string" | "char->integer" | "integer->char"
                    | "procedure?" => {
                        // Handled via apply_builtin
                        let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
                        return apply_builtin(op, &args?);
                    }
                    _ => {}
                }
            }
            // Check for macro invocation
            if let ExprKind::Symbol(op) = &items[0].kind {
                if let Some(Value::Macro(mac)) = env.get(op) {
                    return expand_and_eval_macro(&mac, items, env);
                }
            }
            // Function application (tail position — may return TailCall)
            let func = eval(&items[0], env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply_tail(&func, &args)
        }
    }
}

fn parse_params(exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = exprs.iter().position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "."));
    if let Some(pos) = dot_pos {
        let fixed: Vec<String> = exprs[..pos].iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse("param must be symbol".into())),
        }).collect::<Result<_, _>>()?;
        if pos + 2 != exprs.len() {
            return Err(EvalError::Parse("expected exactly one symbol after dot".into()));
        }
        let rest = match &exprs[pos + 1].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse("rest param must be symbol".into())),
        };
        Ok((fixed, Some(rest)))
    } else {
        let params: Vec<String> = exprs.iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse("param must be symbol".into())),
        }).collect::<Result<_, _>>()?;
        Ok((params, None))
    }
}

fn apply_tail(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Continuation(data) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires exactly 1 argument".into()));
            }
            CONTINUATION_VALUE.with(|v| *v.borrow_mut() = Some(args[0].clone()));
            PENDING_CONTINUATION.with(|pc| *pc.borrow_mut() = Some(data.clone()));
            Err(EvalError::ContinuationEscape(data.id))
        }
        Value::Lambda(params, rest, body, closure_env) => {
            if let Some(_rest_name) = rest {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} args, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} args, got {}", params.len(), args.len()
                )));
            }
            // Check for resume frame
            let resume = RESUME_FRAMES.with(|rf| {
                let mut frames = rf.borrow_mut();
                if !frames.is_empty() {
                    Some(frames.remove(0))
                } else {
                    None
                }
            });
            if let Some(frame) = resume {
                return eval_body_with_frames(&frame.exprs, &frame.env);
            }
            let call_env = Env::with_parent(closure_env);
            for (p, a) in params.iter().zip(args) {
                call_env.set(p.clone(), a.clone());
            }
            if let Some(rest_name) = rest {
                let rest_args = args[params.len()..].to_vec();
                call_env.set(rest_name.clone(), vec_to_pair_list(rest_args));
            }
            if body.is_empty() {
                return Ok(Value::Void);
            }
            if body.len() > 1 {
                CONT_FRAMES.with(|f| {
                    f.borrow_mut().push(ContinuationFrame {
                        exprs: body.clone(),
                        env: call_env.clone(),
                    });
                });
                for (i, expr) in body[..body.len()-1].iter().enumerate() {
                    CONT_FRAMES.with(|f| {
                        if let Some(frame) = f.borrow_mut().last_mut() {
                            frame.exprs = body[i..].to_vec();
                        }
                    });
                    eval(expr, &call_env)?;
                }
                CONT_FRAMES.with(|f| f.borrow_mut().pop());
            }
            Ok(Value::TailCall(Box::new((body.last().unwrap().clone(), call_env))))
        }
        Value::CaseLambda(clauses) => {
            for (params, rest, body, closure_env) in clauses {
                let matches = if rest.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let call_env = Env::with_parent(closure_env);
                    for (p, a) in params.iter().zip(args) {
                        call_env.set(p.clone(), a.clone());
                    }
                    if let Some(rest_name) = rest {
                        let rest_args = args[params.len()..].to_vec();
                        call_env.set(rest_name.clone(), vec_to_pair_list(rest_args));
                    }
                    if body.is_empty() {
                        return Ok(Value::Void);
                    }
                    for expr in &body[..body.len()-1] {
                        eval(expr, &call_env)?;
                    }
                    return Ok(Value::TailCall(Box::new((body.last().unwrap().clone(), call_env))));
                }
            }
            Err(EvalError::Arity(format!("no matching case-lambda clause for {} args", args.len())))
        }
        Value::Builtin(name) => {
            if name == "call/cc" || name == "call-with-current-continuation" {
                let override_val = CALLCC_RETURN.with(|r| r.borrow_mut().take());
                if let Some(val) = override_val {
                    return Ok(val);
                }
                if args.len() != 1 {
                    return Err(EvalError::Arity("call/cc requires exactly 1 argument".into()));
                }
                return eval_callcc_with_proc(&args[0]);
            }
            apply_builtin(name, args)
        }
        other => Err(EvalError::Type(format!("not a procedure: {}", other))),
    }
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let result = apply_tail(func, args)?;
    match result {
        Value::TailCall(tc) => {
            let (expr, env) = *tc;
            eval(&expr, &env)
        }
        v => Ok(v),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut acc = NumVal::Int(0);
            for a in args { acc = num_add(acc, to_num(a)?); }
            Ok(num_to_value(acc))
        }
        "-" => {
            if args.is_empty() { return Err(EvalError::Arity("- requires at least 1 argument".into())); }
            let first = to_num(&args[0])?;
            if args.len() == 1 { return Ok(num_to_value(match first { NumVal::Int(n) => NumVal::Int(-n), NumVal::Rat(n,d) => NumVal::Rat(-n,d), NumVal::Flt(f) => NumVal::Flt(-f) })); }
            let mut acc = first;
            for a in &args[1..] { acc = num_sub(acc, to_num(a)?); }
            Ok(num_to_value(acc))
        }
        "*" => {
            let mut acc = NumVal::Int(1);
            for a in args { acc = num_mul(acc, to_num(a)?); }
            Ok(num_to_value(acc))
        }
        "/" => {
            if args.is_empty() { return Err(EvalError::Arity("/ requires at least 1 argument".into())); }
            let first = to_num(&args[0])?;
            if args.len() == 1 { return Ok(num_to_value(num_div(NumVal::Int(1), first)?)); }
            let mut acc = first;
            for a in &args[1..] { acc = num_div(acc, to_num(a)?)?; }
            Ok(num_to_value(acc))
        }
        "=" => {
            if args.len() < 2 { return Err(EvalError::Arity("= requires at least 2 arguments".into())); }
            let mut prev = num_to_f64(to_num(&args[0])?);
            for a in &args[1..] { let c = num_to_f64(to_num(a)?); if prev != c { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        "<" => {
            if args.len() < 2 { return Err(EvalError::Arity("< requires at least 2 arguments".into())); }
            let mut prev = num_to_f64(to_num(&args[0])?);
            for a in &args[1..] { let c = num_to_f64(to_num(a)?); if !(prev < c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        ">" => {
            if args.len() < 2 { return Err(EvalError::Arity("> requires at least 2 arguments".into())); }
            let mut prev = num_to_f64(to_num(&args[0])?);
            for a in &args[1..] { let c = num_to_f64(to_num(a)?); if !(prev > c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        "<=" => {
            if args.len() < 2 { return Err(EvalError::Arity("<= requires at least 2 arguments".into())); }
            let mut prev = num_to_f64(to_num(&args[0])?);
            for a in &args[1..] { let c = num_to_f64(to_num(a)?); if !(prev <= c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        ">=" => {
            if args.len() < 2 { return Err(EvalError::Arity(">= requires at least 2 arguments".into())); }
            let mut prev = num_to_f64(to_num(&args[0])?);
            for a in &args[1..] { let c = num_to_f64(to_num(a)?); if !(prev >= c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        "not" => {
            if args.len() != 1 { return Err(EvalError::Arity("not requires exactly 1 argument".into())); }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 { return Err(EvalError::Arity("cons requires exactly 2 arguments".into())); }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::Arity("car requires exactly 1 argument".into())); }
            value_car(&args[0])
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cdr requires exactly 1 argument".into())); }
            value_cdr(&args[0])
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::Arity("null? requires exactly 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(vec_to_pair_list(args.to_vec())),
        "length" => {
            if args.len() != 1 { return Err(EvalError::Arity("length requires exactly 1 argument".into())); }
            match pair_list_to_vec(&args[0]) {
                Some(items) => Ok(Value::Integer(items.len() as i64)),
                None => Err(EvalError::Type("length: not a proper list".into())),
            }
        }
        "append" => {
            if args.is_empty() { return Ok(Value::List(vec![])); }
            let mut result = Vec::new();
            for (i, a) in args.iter().enumerate() {
                if i == args.len() - 1 {
                    // Last arg can be non-list (improper append)
                    match pair_list_to_vec(a) {
                        Some(items) => result.extend(items),
                        None => {
                            if result.is_empty() {
                                return Ok(a.clone());
                            }
                            // Build pair chain ending with non-list
                            let mut tail = a.clone();
                            for item in result.into_iter().rev() {
                                tail = make_pair(item, tail);
                            }
                            return Ok(tail);
                        }
                    }
                } else {
                    match pair_list_to_vec(a) {
                        Some(items) => result.extend(items),
                        None => return Err(EvalError::Type("append: not a proper list".into())),
                    }
                }
            }
            Ok(vec_to_pair_list(result))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires exactly 1 argument".into())); }
            output_write(&display_value(&args[0]));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires exactly 1 argument".into())); }
            output_write(&args[0].to_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline takes no arguments".into())); }
            output_write("\n");
            Ok(Value::Void)
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
            }
            let proc = &args[0];
            let last = &args[args.len() - 1];
            let tail = pair_list_to_vec(last)
                .ok_or_else(|| EvalError::Type("apply: last argument must be a list".into()))?;
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail);
            apply_tail(proc, &all_args)
        }
        "number?" => { if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _) | Value::Float(_)))) }
        "string?" => { if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Str(_)))) }
        "boolean?" => { if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Boolean(_)))) }
        "pair?" => { if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty()) || matches!(&args[0], Value::Pair(_)))) }
        "symbol?" => { if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Symbol(_)))) }
        "char?" => { if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Char(_)))) }
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer? requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Boolean(true)),
                Value::Rational(_, d) => Ok(Value::Boolean(*d == 1)),
                Value::Float(f) => Ok(Value::Boolean(f.fract() == 0.0)),
                _ => Ok(Value::Boolean(false)),
            }
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity("rational? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact->inexact requires 1 argument".into())); }
            let n = to_num(&args[0])?;
            Ok(Value::Float(num_to_f64(n)))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact->exact requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, d) => Ok(make_rational(*n, *d)),
                Value::Float(f) => {
                    // Convert float to rational via continued fraction approximation
                    let (n, d) = float_to_rational(*f);
                    Ok(make_rational(n, d))
                }
                other => Err(EvalError::Type(format!("inexact->exact: expected number, got {}", other))),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                other => Err(EvalError::Type(format!("numerator: expected rational, got {}", other))),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                other => Err(EvalError::Type(format!("denominator: expected rational, got {}", other))),
            }
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a { Value::Str(s) => result.push_str(s), other => return Err(EvalError::Type(format!("string-append: expected string, got {}", other))) }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
            match &args[0] { Value::Str(s) => Ok(Value::Integer(s.len() as i64)), other => Err(EvalError::Type(format!("string-length: expected string, got {}", other))) }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
            Ok(Value::Str(args[0].to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
            match &args[0] { Value::Symbol(s) => Ok(Value::Str(s.clone())), other => Err(EvalError::Type(format!("symbol->string: expected symbol, got {}", other))) }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
            match &args[0] { Value::Str(s) => Ok(Value::Symbol(s.clone())), other => Err(EvalError::Type(format!("string->symbol: expected string, got {}", other))) }
        }
        // ── L09: eq? / equal? ──
        "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); }
            let r = match (&args[0], &args[1]) {
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                (Value::Void, Value::Void) => true,
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(r))
        }
        "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity("eqv? requires 2 arguments".into())); }
            let r = match (&args[0], &args[1]) {
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
                (Value::Float(a), Value::Float(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(r))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        // ── L09: numeric utilities ──
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); }
            Ok(Value::Integer(require_int(&args[0])?.abs()))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a / b)) // truncates toward zero in Rust
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a % b)) // Rust % has sign of dividend = remainder semantics
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(((a % b) + b) % b)) // sign of divisor
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
            let mut m = require_int(&args[0])?;
            for a in &args[1..] { let v = require_int(a)?; if v < m { m = v; } }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
            let mut m = require_int(&args[0])?;
            for a in &args[1..] { let v = require_int(a)?; if v > m { m = v; } }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); }
            let base = require_int(&args[0])?;
            let exp = require_int(&args[1])?;
            if exp < 0 { return Ok(Value::Integer(0)); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? % 2 == 0))
        }
        // ── L09: list utilities ──
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); }
            let idx = require_int(&args[1])? as usize;
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = value_cdr(&current)?;
            }
            value_car(&current)
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
            let idx = require_int(&args[1])? as usize;
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = value_cdr(&current)?;
            }
            Ok(current)
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); }
            Ok(Value::Boolean(is_proper_list(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
            let key = &args[0];
            let alist = require_list(&args[1])?;
            for item in &alist {
                match item {
                    Value::Pair(p) => {
                        let car = p.borrow().0.clone();
                        if values_equal(&car, key) {
                            return Ok(item.clone());
                        }
                    }
                    Value::List(pair) if !pair.is_empty() => {
                        if values_equal(&pair[0], key) {
                            return Ok(item.clone());
                        }
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
            let func = &args[0];
            let lists: Result<Vec<Vec<Value>>, _> = args[1..].iter().map(|a| require_list(a)).collect();
            let lists = lists?;
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len { return Err(EvalError::Generic("map: lists must have same length".into())); }
            }
            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply(func, &call_args)?);
            }
            Ok(vec_to_pair_list(result))
        }
        "for-each" => {
            if args.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into())); }
            let func = &args[0];
            let lists: Result<Vec<Vec<Value>>, _> = args[1..].iter().map(|a| require_list(a)).collect();
            let lists = lists?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply(func, &call_args)?;
            }
            Ok(Value::Void)
        }
        "set-car!" => {
            if args.len() != 2 { return Err(EvalError::Arity("set-car! requires 2 arguments".into())); }
            match &args[0] {
                Value::Pair(p) => { p.borrow_mut().0 = args[1].clone(); Ok(Value::Void) }
                _ => Err(EvalError::Type("set-car!: not a mutable pair".into())),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 { return Err(EvalError::Arity("set-cdr! requires 2 arguments".into())); }
            match &args[0] {
                Value::Pair(p) => { p.borrow_mut().1 = args[1].clone(); Ok(Value::Void) }
                _ => Err(EvalError::Type("set-cdr!: not a mutable pair".into())),
            }
        }
        "caar" => { if args.len() != 1 { return Err(EvalError::Arity("caar requires 1 argument".into())); } value_car(&value_car(&args[0])?) }
        "cadr" => { if args.len() != 1 { return Err(EvalError::Arity("cadr requires 1 argument".into())); } value_car(&value_cdr(&args[0])?) }
        "cdar" => { if args.len() != 1 { return Err(EvalError::Arity("cdar requires 1 argument".into())); } value_cdr(&value_car(&args[0])?) }
        "cddr" => { if args.len() != 1 { return Err(EvalError::Arity("cddr requires 1 argument".into())); } value_cdr(&value_cdr(&args[0])?) }
        "caddr" => { if args.len() != 1 { return Err(EvalError::Arity("caddr requires 1 argument".into())); } value_car(&value_cdr(&value_cdr(&args[0])?)?) }
        "cdddr" => { if args.len() != 1 { return Err(EvalError::Arity("cdddr requires 1 argument".into())); } value_cdr(&value_cdr(&value_cdr(&args[0])?)?) }
        "cadddr" => { if args.len() != 1 { return Err(EvalError::Arity("cadddr requires 1 argument".into())); } value_car(&value_cdr(&value_cdr(&value_cdr(&args[0])?)?)?) }
        "caddar" => { if args.len() != 1 { return Err(EvalError::Arity("caddar requires 1 argument".into())); } value_car(&value_cdr(&value_cdr(&value_car(&args[0])?)?)?) }
        "caaar" => { if args.len() != 1 { return Err(EvalError::Arity("caaar requires 1 argument".into())); } value_car(&value_car(&value_car(&args[0])?)?) }
        "caadr" => { if args.len() != 1 { return Err(EvalError::Arity("caadr requires 1 argument".into())); } value_car(&value_car(&value_cdr(&args[0])?)?) }
        "cdaar" => { if args.len() != 1 { return Err(EvalError::Arity("cdaar requires 1 argument".into())); } value_cdr(&value_car(&value_car(&args[0])?)?) }
        "cdadr" => { if args.len() != 1 { return Err(EvalError::Arity("cdadr requires 1 argument".into())); } value_cdr(&value_car(&value_cdr(&args[0])?)?) }
        "cddar" => { if args.len() != 1 { return Err(EvalError::Arity("cddar requires 1 argument".into())); } value_cdr(&value_cdr(&value_car(&args[0])?)?) }
        "caaaar" => { if args.len() != 1 { return Err(EvalError::Arity("caaaar requires 1 argument".into())); } value_car(&value_car(&value_car(&value_car(&args[0])?)?)?) }
        "caaadr" => { if args.len() != 1 { return Err(EvalError::Arity("caaadr requires 1 argument".into())); } value_car(&value_car(&value_car(&value_cdr(&args[0])?)?)?) }
        "caadar" => { if args.len() != 1 { return Err(EvalError::Arity("caadar requires 1 argument".into())); } value_car(&value_car(&value_cdr(&value_car(&args[0])?)?)?) }
        "caaddr" => { if args.len() != 1 { return Err(EvalError::Arity("caaddr requires 1 argument".into())); } value_car(&value_car(&value_cdr(&value_cdr(&args[0])?)?)?) }
        "cadaar" => { if args.len() != 1 { return Err(EvalError::Arity("cadaar requires 1 argument".into())); } value_car(&value_cdr(&value_car(&value_car(&args[0])?)?)?) }
        "cadadr" => { if args.len() != 1 { return Err(EvalError::Arity("cadadr requires 1 argument".into())); } value_car(&value_cdr(&value_car(&value_cdr(&args[0])?)?)?) }
        "cdaaar" | "cdaadr" | "cdadar" | "cdaddr" | "cddaar" | "cddadr" | "cdddar" | "cddddr" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("{} requires 1 argument", name))); }
            // Generic 4-level CXR
            let ops: Vec<char> = name[1..name.len()-1].chars().collect();
            let mut val = args[0].clone();
            for op in ops.iter().rev() {
                match op {
                    'a' => val = value_car(&val)?,
                    'd' => val = value_cdr(&val)?,
                    _ => return Err(EvalError::Generic(format!("unknown cxr op: {}", op))),
                }
            }
            Ok(val)
        }
        "memq" => {
            if args.len() != 2 { return Err(EvalError::Arity("memq requires 2 arguments".into())); }
            let key = &args[0];
            let mut current = args[1].clone();
            loop {
                match &current {
                    Value::List(items) if items.is_empty() => return Ok(Value::Boolean(false)),
                    Value::Pair(p) => {
                        let (car, cdr) = { let b = p.borrow(); (b.0.clone(), b.1.clone()) };
                        let is_eq = match (key, &car) {
                            (Value::Symbol(a), Value::Symbol(b)) => a == b,
                            (Value::Integer(a), Value::Integer(b)) => a == b,
                            (Value::Boolean(a), Value::Boolean(b)) => a == b,
                            (Value::Char(a), Value::Char(b)) => a == b,
                            (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                            _ => false,
                        };
                        if is_eq { return Ok(current.clone()); }
                        current = cdr;
                    }
                    Value::List(items) => {
                        for (i, item) in items.iter().enumerate() {
                            let is_eq = match (key, item) {
                                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                                (Value::Integer(a), Value::Integer(b)) => a == b,
                                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                                (Value::Char(a), Value::Char(b)) => a == b,
                                _ => false,
                            };
                            if is_eq { return Ok(Value::List(items[i..].to_vec())); }
                        }
                        return Ok(Value::Boolean(false));
                    }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "memv" => {
            if args.len() != 2 { return Err(EvalError::Arity("memv requires 2 arguments".into())); }
            let key = &args[0];
            let items = require_list(&args[1])?;
            for (i, item) in items.iter().enumerate() {
                let is_eqv = match (key, item) {
                    (Value::Symbol(a), Value::Symbol(b)) => a == b,
                    (Value::Integer(a), Value::Integer(b)) => a == b,
                    (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
                    (Value::Float(a), Value::Float(b)) => a == b,
                    (Value::Boolean(a), Value::Boolean(b)) => a == b,
                    (Value::Char(a), Value::Char(b)) => a == b,
                    _ => false,
                };
                if is_eqv { return Ok(vec_to_pair_list(items[i..].to_vec())); }
            }
            Ok(Value::Boolean(false))
        }
        "assq" => {
            if args.len() != 2 { return Err(EvalError::Arity("assq requires 2 arguments".into())); }
            let key = &args[0];
            let alist = require_list(&args[1])?;
            for item in &alist {
                let car = value_car(item).ok();
                if let Some(ref car_val) = car {
                    let is_eq = match (key, car_val) {
                        (Value::Symbol(a), Value::Symbol(b)) => a == b,
                        (Value::Integer(a), Value::Integer(b)) => a == b,
                        (Value::Boolean(a), Value::Boolean(b)) => a == b,
                        (Value::Char(a), Value::Char(b)) => a == b,
                        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                        _ => false,
                    };
                    if is_eq { return Ok(item.clone()); }
                }
            }
            Ok(Value::Boolean(false))
        }
        "reverse" => {
            if args.len() != 1 { return Err(EvalError::Arity("reverse requires 1 argument".into())); }
            let items = require_list(&args[0])?;
            let mut reversed = items;
            reversed.reverse();
            Ok(vec_to_pair_list(reversed))
        }
        "member" => {
            if args.len() != 2 { return Err(EvalError::Arity("member requires 2 arguments".into())); }
            let key = &args[0];
            let items = require_list(&args[1])?;
            for (i, item) in items.iter().enumerate() {
                if values_equal(key, item) {
                    return Ok(vec_to_pair_list(items[i..].to_vec()));
                }
            }
            Ok(Value::Boolean(false))
        }
        "assv" => {
            if args.len() != 2 { return Err(EvalError::Arity("assv requires 2 arguments".into())); }
            let key = &args[0];
            let alist = require_list(&args[1])?;
            for item in &alist {
                let car = value_car(item).ok();
                if let Some(ref car_val) = car {
                    let is_eqv = match (key, car_val) {
                        (Value::Symbol(a), Value::Symbol(b)) => a == b,
                        (Value::Integer(a), Value::Integer(b)) => a == b,
                        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
                        (Value::Float(a), Value::Float(b)) => a == b,
                        (Value::Boolean(a), Value::Boolean(b)) => a == b,
                        (Value::Char(a), Value::Char(b)) => a == b,
                        _ => false,
                    };
                    if is_eqv { return Ok(item.clone()); }
                }
            }
            Ok(Value::Boolean(false))
        }
        "gcd" => {
            if args.len() == 0 { return Ok(Value::Integer(0)); }
            let mut result = require_int(&args[0])?.abs();
            for a in &args[1..] {
                result = gcd(result, require_int(a)?.abs());
            }
            Ok(Value::Integer(result))
        }
        "lcm" => {
            if args.len() == 0 { return Ok(Value::Integer(1)); }
            let mut result = require_int(&args[0])?.abs();
            for a in &args[1..] {
                let b = require_int(a)?.abs();
                if result == 0 && b == 0 { result = 0; }
                else { result = result / gcd(result, b) * b; }
            }
            Ok(Value::Integer(result))
        }
        "truncate" => {
            if args.len() != 1 { return Err(EvalError::Arity("truncate requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer(n / d)),
                other => Err(EvalError::Type(format!("truncate: expected number, got {}", other))),
            }
        }
        "round" => {
            if args.len() != 1 { return Err(EvalError::Arity("round requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer((*n as f64 / *d as f64).round() as i64)),
                other => Err(EvalError::Type(format!("round: expected number, got {}", other))),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into())); }
            let len = require_int(&args[0])? as usize;
            let ch = if args.len() == 2 { require_char(&args[1])? } else { ' ' };
            Ok(Value::Str(std::iter::repeat(ch).take(len).collect()))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                s.push(require_char(a)?);
            }
            Ok(Value::Str(s))
        }
        "string>?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string>? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? > require_str(&args[1])?))
        }
        "string<=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? <= require_str(&args[1])?))
        }
        "string>=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string>=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? >= require_str(&args[1])?))
        }
        "cadar" => {
            if args.len() != 1 { return Err(EvalError::Arity("cadar requires 1 argument".into())); }
            value_car(&value_cdr(&value_car(&args[0])?)?)
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                other => Err(EvalError::Type(format!("string->number: expected string, got {}", other))),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
            let s = require_str(&args[0])?;
            let idx = require_int(&args[1])? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Generic(format!("string-ref: index {} out of range", idx))),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
            let s = require_str(&args[0])?;
            let start = require_int(&args[1])? as usize;
            let end = require_int(&args[2])? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::Generic("substring: index out of range".into()));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                other => Err(EvalError::Type(format!("string-copy: expected string, got {}", other))),
            }
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->list requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(vec_to_pair_list(s.chars().map(Value::Char).collect())),
                other => Err(EvalError::Type(format!("string->list: expected string, got {}", other))),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->string requires 1 argument".into())); }
            let items = require_list(&args[0])?;
            let mut s = String::new();
            for item in &items {
                match item {
                    Value::Char(c) => s.push(*c),
                    other => return Err(EvalError::Type(format!("list->string: expected char, got {}", other))),
                }
            }
            Ok(Value::Str(s))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into())); }
            Ok(Value::Integer(require_char(&args[0])? as i64))
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into())); }
            let n = require_int(&args[0])?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Generic(format!("integer->char: invalid code point {}", n))),
            }
        }
        "dynamic-wind" => {
            if args.len() != 3 { return Err(EvalError::Arity("dynamic-wind requires 3 arguments".into())); }
            let in_thunk = &args[0];
            let body_thunk = &args[1];
            let out_thunk = &args[2];
            // Protect in-thunk from consuming continuation resume state
            let saved_resume = RESUME_FRAMES.with(|rf| std::mem::take(&mut *rf.borrow_mut()));
            let saved_callcc = CALLCC_RETURN.with(|r| r.borrow_mut().take());
            apply(in_thunk, &[])?;
            RESUME_FRAMES.with(|rf| *rf.borrow_mut() = saved_resume);
            CALLCC_RETURN.with(|r| *r.borrow_mut() = saved_callcc);
            let body_result = apply(body_thunk, &[]);
            match body_result {
                Ok(val) => {
                    apply(out_thunk, &[])?;
                    Ok(val)
                }
                Err(EvalError::ContinuationEscape(id)) => {
                    apply(out_thunk, &[])?;
                    Err(EvalError::ContinuationEscape(id))
                }
                Err(e) => {
                    let _ = apply(out_thunk, &[]);
                    Err(e)
                }
            }
        }
        "procedure?" => {
            if args.len() != 1 { return Err(EvalError::Arity("procedure? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Lambda(..) | Value::CaseLambda(..) | Value::Builtin(..) | Value::Continuation(..))))
        }
        "raise" => {
            if args.len() != 1 { return Err(EvalError::Arity("raise requires exactly 1 argument".into())); }
            let val = args[0].clone();
            let handler = EXCEPTION_HANDLERS.with(|h| h.borrow().last().cloned());
            if let Some(handler) = handler {
                EXCEPTION_HANDLERS.with(|h| h.borrow_mut().pop());
                let result = apply(&handler, &[val]);
                EXCEPTION_HANDLERS.with(|h| h.borrow_mut().push(handler));
                return result;
            }
            RAISED_VALUE.with(|r| *r.borrow_mut() = Some(val));
            Err(EvalError::RaisedException)
        }
        "raise-continuable" => {
            if args.len() != 1 { return Err(EvalError::Arity("raise-continuable requires exactly 1 argument".into())); }
            let val = args[0].clone();
            let handler = EXCEPTION_HANDLERS.with(|h| h.borrow().last().cloned());
            if let Some(handler) = handler {
                EXCEPTION_HANDLERS.with(|h| h.borrow_mut().pop());
                let result = apply(&handler, &[val]);
                EXCEPTION_HANDLERS.with(|h| h.borrow_mut().push(handler));
                return result;
            }
            RAISED_VALUE.with(|r| *r.borrow_mut() = Some(val));
            Err(EvalError::RaisedException)
        }
        "with-exception-handler" => {
            if args.len() != 2 { return Err(EvalError::Arity("with-exception-handler requires 2 arguments".into())); }
            let handler = args[0].clone();
            let thunk = args[1].clone();
            EXCEPTION_HANDLERS.with(|h| h.borrow_mut().push(handler.clone()));
            let result = apply(&thunk, &[]);
            EXCEPTION_HANDLERS.with(|h| h.borrow_mut().pop());
            match result {
                Ok(val) => Ok(val),
                Err(EvalError::RaisedException) => {
                    let exn = RAISED_VALUE.with(|r| r.borrow_mut().take())
                        .unwrap_or(Value::Void);
                    apply(&handler, &[exn])
                }
                Err(e) => Err(e),
            }
        }
        // ── L09: char operations ──
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); }
            Ok(Value::Boolean(require_char(&args[0])?.is_alphabetic()))
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); }
            Ok(Value::Boolean(require_char(&args[0])?.is_ascii_digit()))
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); }
            Ok(Value::Char(require_char(&args[0])?.to_ascii_uppercase()))
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); }
            Ok(Value::Char(require_char(&args[0])?.to_ascii_lowercase()))
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_char(&args[0])? == require_char(&args[1])?))
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_char(&args[0])? < require_char(&args[1])?))
        }
        // ── L09: string operations ──
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? == require_str(&args[1])?))
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? < require_str(&args[1])?))
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])?.to_lowercase() == require_str(&args[1])?.to_lowercase()))
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
            Ok(Value::Str(require_str(&args[0])?.to_uppercase()))
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
            Ok(Value::Str(require_str(&args[0])?.to_lowercase()))
        }
        // ── L14: vector operations ──
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into())); }
            let len = require_int(&args[0])? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("vector-ref requires 2 arguments".into())); }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = require_int(&args[1])? as usize;
                    let v = v.borrow();
                    v.get(idx).cloned().ok_or_else(|| EvalError::Generic(format!("vector-ref: index {} out of range", idx)))
                }
                other => Err(EvalError::Type(format!("vector-ref: expected vector, got {}", other))),
            }
        }
        "vector-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity("vector-set! requires 3 arguments".into())); }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = require_int(&args[1])? as usize;
                    let mut v = v.borrow_mut();
                    if idx >= v.len() { return Err(EvalError::Generic(format!("vector-set!: index {} out of range", idx))); }
                    v[idx] = args[2].clone();
                    Ok(Value::Void)
                }
                other => Err(EvalError::Type(format!("vector-set!: expected vector, got {}", other))),
            }
        }
        "vector-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector-length requires 1 argument".into())); }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                other => Err(EvalError::Type(format!("vector-length: expected vector, got {}", other))),
            }
        }
        "vector?" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector->list requires 1 argument".into())); }
            match &args[0] {
                Value::Vector(v) => Ok(vec_to_pair_list(v.borrow().clone())),
                other => Err(EvalError::Type(format!("vector->list: expected vector, got {}", other))),
            }
        }
        "list->vector" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->vector requires 1 argument".into())); }
            let items = require_list(&args[0])?;
            Ok(Value::Vector(Rc::new(RefCell::new(items))))
        }
        _ if name.starts_with("__record-construct-") => {
            // __record-construct-{type_id}-{nfields}
            let rest = &name["__record-construct-".len()..];
            let dash = rest.find('-').unwrap();
            let type_id: usize = rest[..dash].parse().unwrap();
            let nfields: usize = rest[dash+1..].parse().unwrap();
            if args.len() != nfields {
                return Err(EvalError::Arity(format!("record constructor expects {} args, got {}", nfields, args.len())));
            }
            Ok(Value::Record(type_id, args.to_vec()))
        }
        _ if name.starts_with("__record-predicate-") => {
            let type_id: usize = name["__record-predicate-".len()..].parse().unwrap();
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record(id, _) if *id == type_id)))
        }
        _ if name.starts_with("__record-accessor-") => {
            // __record-accessor-{type_id}-{field_idx}
            let rest = &name["__record-accessor-".len()..];
            let dash = rest.find('-').unwrap();
            let type_id: usize = rest[..dash].parse().unwrap();
            let field_idx: usize = rest[dash+1..].parse().unwrap();
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor requires 1 argument".into()));
            }
            match &args[0] {
                Value::Record(id, fields) if *id == type_id => {
                    Ok(fields[field_idx].clone())
                }
                _ => Err(EvalError::Type("record accessor: wrong type".into())),
            }
        }
        _ => Err(EvalError::Generic(format!("unknown builtin: {}", name))),
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(parts) => {
            // (define (f x y) body...) => (define f (lambda (x y) body...))
            // (define (f x . rest) body...) => (define f (lambda (x . rest) body...))
            if parts.is_empty() {
                return Err(EvalError::Parse("define: empty name list".into()));
            }
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: name must be symbol".into())),
            };
            let (params, rest) = parse_params(&parts[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda(params, rest, body, env.clone());
            env.set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: first argument must be symbol or list".into())),
    }
}

fn eval_set(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("set! requires exactly 2 arguments".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("set!: first argument must be a symbol".into())),
    };
    let val = eval(&args[1], env)?;
    if !env.set_existing(&name, val) {
        return Err(EvalError::UnboundVariable(name));
    }
    Ok(Value::Void)
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        Ok(Value::TailCall(Box::new((args[1].clone(), env.clone()))))
    } else if args.len() == 3 {
        Ok(Value::TailCall(Box::new((args[2].clone(), env.clone()))))
    } else {
        Ok(Value::Void)
    }
}

// ── Numeric Tower ───────────────────────────────────────────────────

#[derive(Debug, Clone, Copy)]
enum NumVal {
    Int(i64),
    Rat(i64, i64),
    Flt(f64),
}

fn to_num(v: &Value) -> Result<NumVal, EvalError> {
    match v {
        Value::Integer(n) => Ok(NumVal::Int(*n)),
        Value::Rational(n, d) => Ok(NumVal::Rat(*n, *d)),
        Value::Float(f) => Ok(NumVal::Flt(*f)),
        other => Err(EvalError::Type(format!("expected number, got {}", other))),
    }
}

fn num_to_value(n: NumVal) -> Value {
    match n {
        NumVal::Int(i) => Value::Integer(i),
        NumVal::Rat(n, d) => make_rational(n, d),
        NumVal::Flt(f) => Value::Float(f),
    }
}

fn num_to_f64(n: NumVal) -> f64 {
    match n {
        NumVal::Int(i) => i as f64,
        NumVal::Rat(n, d) => n as f64 / d as f64,
        NumVal::Flt(f) => f,
    }
}

fn num_add(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x + y),
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => {
            let n = an * bd + bn * ad;
            let d = ad * bd;
            let g = gcd(n.abs(), d.abs());
            let d2 = d / g;
            let n2 = n / g;
            if d2 == 1 { NumVal::Int(n2) } else if d2 < 0 { NumVal::Rat(-n2, -d2) } else { NumVal::Rat(n2, d2) }
        }
        (NumVal::Int(x), NumVal::Rat(bn, bd)) | (NumVal::Rat(bn, bd), NumVal::Int(x)) => {
            num_add(NumVal::Rat(x, 1), NumVal::Rat(bn, bd))
        }
        (NumVal::Flt(x), b) => NumVal::Flt(x + num_to_f64(b)),
        (a, NumVal::Flt(y)) => NumVal::Flt(num_to_f64(a) + y),
    }
}

fn num_sub(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x - y),
        (a, b) => {
            let neg_b = match b {
                NumVal::Int(y) => NumVal::Int(-y),
                NumVal::Rat(n, d) => NumVal::Rat(-n, d),
                NumVal::Flt(f) => NumVal::Flt(-f),
            };
            num_add(a, neg_b)
        }
    }
}

fn num_mul(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x * y),
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => {
            let n = an * bn;
            let d = ad * bd;
            let g = gcd(n.abs(), d.abs());
            let d2 = d / g;
            let n2 = n / g;
            if d2 == 1 { NumVal::Int(n2) } else if d2 < 0 { NumVal::Rat(-n2, -d2) } else { NumVal::Rat(n2, d2) }
        }
        (NumVal::Int(x), NumVal::Rat(bn, bd)) | (NumVal::Rat(bn, bd), NumVal::Int(x)) => {
            num_mul(NumVal::Rat(x, 1), NumVal::Rat(bn, bd))
        }
        (NumVal::Flt(x), b) => NumVal::Flt(x * num_to_f64(b)),
        (a, NumVal::Flt(y)) => NumVal::Flt(num_to_f64(a) * y),
    }
}

fn simplify_rat(n: i64, d: i64) -> NumVal {
    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 { NumVal::Int(n) } else { NumVal::Rat(n, d) }
}

fn num_div(a: NumVal, b: NumVal) -> Result<NumVal, EvalError> {
    match (a, b) {
        (_, NumVal::Int(0)) | (_, NumVal::Rat(0, _)) => Err(EvalError::DivisionByZero),
        (NumVal::Int(x), NumVal::Int(y)) => Ok(simplify_rat(x, y)),
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => {
            // (an/ad) / (bn/bd) = an*bd / ad*bn
            Ok(simplify_rat(an * bd, ad * bn))
        }
        (NumVal::Int(x), NumVal::Rat(bn, bd)) => {
            Ok(simplify_rat(x * bd, bn))
        }
        (NumVal::Rat(an, ad), NumVal::Int(y)) => {
            Ok(simplify_rat(an, ad * y))
        }
        (_, NumVal::Flt(f)) if f == 0.0 => Err(EvalError::DivisionByZero),
        (NumVal::Flt(x), b) => Ok(NumVal::Flt(x / num_to_f64(b))),
        (a, NumVal::Flt(y)) => Ok(NumVal::Flt(num_to_f64(a) / y)),
    }
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 { return (0, 1); }
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    // Simple approach: multiply by powers of 2 until we get an integer, then simplify
    let mut n = f;
    let mut d = 1i64;
    for _ in 0..53 {
        if n == n.floor() { break; }
        n *= 2.0;
        d *= 2;
    }
    let ni = n.round() as i64;
    let g = gcd(ni, d);
    (sign * ni / g, d / g)
}

fn require_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::Type(format!("expected integer, got {}", other))),
    }
}

fn require_char(v: &Value) -> Result<char, EvalError> {
    match v {
        Value::Char(c) => Ok(*c),
        other => Err(EvalError::Type(format!("expected char, got {}", other))),
    }
}

fn require_str(v: &Value) -> Result<String, EvalError> {
    match v {
        Value::Str(s) => Ok(s.clone()),
        other => Err(EvalError::Type(format!("expected string, got {}", other))),
    }
}

fn require_list(v: &Value) -> Result<Vec<Value>, EvalError> {
    match pair_list_to_vec(v) {
        Some(items) => Ok(items),
        None => Err(EvalError::Type(format!("expected list, got {}", v))),
    }
}

fn is_proper_list(v: &Value) -> bool {
    match v {
        Value::List(_) => true,
        Value::Pair(_) => {
            // Tortoise and hare cycle detection
            let mut slow = v.clone();
            let mut fast = v.clone();
            loop {
                // Advance slow by 1
                let next_slow = match &slow {
                    Value::List(_) => return true,
                    Value::Pair(p) => p.borrow().1.clone(),
                    _ => return false,
                };
                slow = next_slow;
                // Advance fast by 2
                for _ in 0..2 {
                    let next_fast = match &fast {
                        Value::List(_) => return true,
                        Value::Pair(p) => p.borrow().1.clone(),
                        _ => return false,
                    };
                    fast = next_fast;
                }
                // Check for cycle
                if let (Value::Pair(a), Value::Pair(b)) = (&slow, &fast) {
                    if Rc::ptr_eq(a, b) { return false; }
                }
            }
        }
        _ => false,
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    values_equal_inner(a, b, &mut HashSet::new())
}

fn values_equal_inner(a: &Value, b: &Value, seen: &mut HashSet<(usize, usize)>) -> bool {
    match (a, b) {
        (Value::Integer(_) | Value::Rational(_, _) | Value::Float(_),
         Value::Integer(_) | Value::Rational(_, _) | Value::Float(_)) => {
            let na = to_num(a).unwrap();
            let nb = to_num(b).unwrap();
            num_to_f64(na) == num_to_f64(nb)
        }
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.len() == y.len() && x.iter().zip(y).all(|(a, b)| values_equal_inner(a, b, seen)),
        (Value::Pair(pa), Value::Pair(pb)) => {
            let key = (Rc::as_ptr(pa) as usize, Rc::as_ptr(pb) as usize);
            if !seen.insert(key) { return true; } // assume equal for cycles
            let (a1, a2) = { let b = pa.borrow(); (b.0.clone(), b.1.clone()) };
            let (b1, b2) = { let b = pb.borrow(); (b.0.clone(), b.1.clone()) };
            let result = values_equal_inner(&a1, &b1, seen) && values_equal_inner(&a2, &b2, seen);
            seen.remove(&key);
            result
        }
        // Cross-type list/pair equality
        (Value::List(items), Value::Pair(_)) | (Value::Pair(_), Value::List(items)) if items.is_empty() => false,
        (Value::List(items), other) | (other, Value::List(items)) if !items.is_empty() => {
            if let (Ok(car), Ok(cdr)) = (value_car(other), value_cdr(other)) {
                values_equal_inner(&items[0], &car, seen)
                    && values_equal_inner(&Value::List(items[1..].to_vec()), &cdr, seen)
            } else {
                false
            }
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal_inner(x, y, seen))
        }
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn eval_add(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut acc = NumVal::Int(0);
    for a in args {
        acc = num_add(acc, to_num(&eval(a, env)?)?);
    }
    Ok(num_to_value(acc))
}

fn eval_sub(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    let first = to_num(&eval(&args[0], env)?)?;
    if args.len() == 1 {
        return Ok(num_to_value(match first {
            NumVal::Int(n) => NumVal::Int(-n),
            NumVal::Rat(n, d) => NumVal::Rat(-n, d),
            NumVal::Flt(f) => NumVal::Flt(-f),
        }));
    }
    let mut acc = first;
    for a in &args[1..] {
        acc = num_sub(acc, to_num(&eval(a, env)?)?);
    }
    Ok(num_to_value(acc))
}

fn eval_mul(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut acc = NumVal::Int(1);
    for a in args {
        acc = num_mul(acc, to_num(&eval(a, env)?)?);
    }
    Ok(num_to_value(acc))
}

fn eval_div(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
    }
    let first = to_num(&eval(&args[0], env)?)?;
    if args.len() == 1 {
        return Ok(num_to_value(num_div(NumVal::Int(1), first)?));
    }
    let mut acc = first;
    for a in &args[1..] {
        acc = num_div(acc, to_num(&eval(a, env)?)?)?;
    }
    Ok(num_to_value(acc))
}

fn eval_cmp(args: &[Expr], env: &Env, cmp: fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = num_to_f64(to_num(&eval(&args[0], env)?)?);
    for a in &args[1..] {
        let curr = num_to_f64(to_num(&eval(a, env)?)?);
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn eval_not(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires exactly 1 argument".into()));
    }
    let v = eval(&args[0], env)?;
    Ok(Value::Boolean(!is_truthy(&v)))
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for a in &args[..args.len()-1] {
        let result = eval(a, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::TailCall(Box::new((args.last().unwrap().clone(), env.clone()))))
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for a in &args[..args.len()-1] {
        let result = eval(a, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::TailCall(Box::new((args.last().unwrap().clone(), env.clone()))))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Void);
    }
    let resume = RESUME_FRAMES.with(|rf| {
        let mut frames = rf.borrow_mut();
        if !frames.is_empty() {
            Some(frames.remove(0))
        } else {
            None
        }
    });
    if let Some(frame) = resume {
        return eval_body_with_frames(&frame.exprs, &frame.env);
    }
    if args.len() > 1 {
        CONT_FRAMES.with(|f| {
            f.borrow_mut().push(ContinuationFrame {
                exprs: args.to_vec(),
                env: env.clone(),
            });
        });
        for (i, a) in args[..args.len()-1].iter().enumerate() {
            CONT_FRAMES.with(|f| {
                if let Some(frame) = f.borrow_mut().last_mut() {
                    frame.exprs = args[i..].to_vec();
                }
            });
            eval(a, env)?;
        }
        CONT_FRAMES.with(|f| f.borrow_mut().pop());
    }
    Ok(Value::TailCall(Box::new((args.last().unwrap().clone(), env.clone()))))
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        if parts.len() <= 1 {
                            return Ok(Value::Void);
                        }
                        for expr in &parts[1..parts.len()-1] {
                            eval(expr, env)?;
                        }
                        return Ok(Value::TailCall(Box::new((parts.last().unwrap().clone(), env.clone()))));
                    }
                }
                let test = eval(&parts[0], env)?;
                if is_truthy(&test) {
                    if parts.len() <= 1 {
                        return Ok(test);
                    }
                    for expr in &parts[1..parts.len()-1] {
                        eval(expr, env)?;
                    }
                    return Ok(Value::TailCall(Box::new((parts.last().unwrap().clone(), env.clone()))));
                }
            }
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Check for resume frame BEFORE creating bindings
    let resume = RESUME_FRAMES.with(|rf| {
        let mut frames = rf.borrow_mut();
        if !frames.is_empty() {
            Some(frames.remove(0))
        } else {
            None
        }
    });
    if let Some(frame) = resume {
        return eval_body_with_frames(&frame.exprs, &frame.env);
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 2 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse("let: bindings must be a list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    match &pair[0].kind {
                        ExprKind::Symbol(s) => params.push(s.clone()),
                        _ => return Err(EvalError::Parse("let: binding name must be symbol".into())),
                    }
                    inits.push(&pair[1]);
                }
                _ => return Err(EvalError::Parse("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let lambda = Value::Lambda(params.clone(), None, body, env.clone());
        let let_env = Env::with_parent(env);
        let_env.set(name.clone(), lambda.clone());
        // Re-create lambda with let_env so it can see itself
        let body2 = match &lambda { Value::Lambda(_, _, b, _) => b.clone(), _ => unreachable!() };
        let lambda2 = Value::Lambda(params, None, body2, let_env.clone());
        let_env.set(name.clone(), lambda2.clone());
        let init_vals: Result<Vec<Value>, _> = inits.iter().map(|e| eval(e, env)).collect();
        return apply_tail(&lambda2, &init_vals?);
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("let: bindings must be a list".into())),
    };
    let let_env = Env::with_parent(env);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("let: binding name must be symbol".into())),
                };
                let val = eval(&pair[1], env)?;
                let_env.set(name, val);
            }
            _ => return Err(EvalError::Parse("let: invalid binding".into())),
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(Value::Void);
    }
    if body.len() > 1 {
        let body_vec: Vec<Expr> = body.to_vec();
        CONT_FRAMES.with(|f| {
            f.borrow_mut().push(ContinuationFrame {
                exprs: body_vec.clone(),
                env: let_env.clone(),
            });
        });
        for (i, expr) in body[..body.len()-1].iter().enumerate() {
            CONT_FRAMES.with(|f| {
                if let Some(frame) = f.borrow_mut().last_mut() {
                    frame.exprs = body_vec[i..].to_vec();
                }
            });
            eval(expr, &let_env)?;
        }
        CONT_FRAMES.with(|f| f.borrow_mut().pop());
    }
    Ok(Value::TailCall(Box::new((body.last().unwrap().clone(), let_env))))
}

fn eval_let_star(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("let*: bindings must be a list".into())),
    };
    let let_env = Env::with_parent(env);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("let*: binding name must be symbol".into())),
                };
                let val = eval(&pair[1], &let_env)?;
                let_env.set(name, val);
            }
            _ => return Err(EvalError::Parse("let*: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &let_env)?;
    }
    Ok(result)
}

fn eval_letrec(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("letrec: bindings must be a list".into())),
    };
    let rec_env = Env::with_parent(env);
    // First pass: bind all names to Void (so they're visible)
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("letrec: binding name must be symbol".into())),
                };
                rec_env.set(name.clone(), Value::Void);
                names.push(name);
                init_exprs.push(&pair[1]);
            }
            _ => return Err(EvalError::Parse("letrec: invalid binding".into())),
        }
    }
    // Second pass: evaluate inits in rec_env and set
    let vals: Result<Vec<Value>, _> = init_exprs.iter().map(|e| eval(e, &rec_env)).collect();
    let vals = vals?;
    for (name, val) in names.iter().zip(vals) {
        rec_env.set(name.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &rec_env)?;
    }
    Ok(result)
}

fn eval_letrec_star(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("letrec*: bindings must be a list".into())),
    };
    let rec_env = Env::with_parent(env);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("letrec*: binding name must be symbol".into())),
                };
                let val = eval(&pair[1], &rec_env)?;
                rec_env.set(name, val);
            }
            _ => return Err(EvalError::Parse("letrec*: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &rec_env)?;
    }
    Ok(result)
}

fn eqv_match(key: &Value, datum: &Value) -> bool {
    match (key, datum) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

fn eval_case(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires key and clauses".into()));
    }
    let key = eval(&args[0], env)?;
    for clause in &args[1..] {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                // Check datums
                let datums = match &parts[0].kind {
                    ExprKind::List(d) => d,
                    _ => return Err(EvalError::Parse("case: clause datums must be a list".into())),
                };
                let matched = datums.iter().any(|d| eqv_match(&key, &expr_to_value(d)));
                if matched {
                    let mut result = Value::Void;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("case: invalid clause".into())),
        }
    }
    // No match and no else: return void
    Ok(Value::Void)
}

fn eval_do(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable bindings and test clause".into()));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(specs) => specs,
        _ => return Err(EvalError::Parse("do: variable specs must be a list".into())),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => parts,
        _ => return Err(EvalError::Parse("do: test clause must be a non-empty list".into())),
    };
    let body = &args[2..];

    // Parse variable specs
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<&Expr>> = Vec::new();
    let do_env = Env::with_parent(env);

    for spec in var_specs {
        match &spec.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("do: variable name must be symbol".into())),
                };
                let init_val = eval(&parts[1], env)?;
                do_env.set(name.clone(), init_val);
                var_names.push(name);
                if parts.len() >= 3 {
                    step_exprs.push(Some(&parts[2]));
                } else {
                    step_exprs.push(None);
                }
            }
            _ => return Err(EvalError::Parse("do: invalid variable spec".into())),
        }
    }

    // Iteration loop
    loop {
        // Test
        let test_val = eval(&test_clause[0], &do_env)?;
        if is_truthy(&test_val) {
            // Evaluate result expressions
            if test_clause.len() > 1 {
                let mut result = Value::Void;
                for expr in &test_clause[1..] {
                    result = eval(expr, &do_env)?;
                }
                return Ok(result);
            } else {
                return Ok(Value::Void);
            }
        }

        // Execute body
        for expr in body {
            eval(expr, &do_env)?;
        }

        // Evaluate all step expressions using current values (parallel update)
        let new_vals: Vec<Option<Value>> = step_exprs.iter().map(|step| {
            match step {
                Some(expr) => Ok(Some(eval(expr, &do_env)?)),
                None => Ok(None),
            }
        }).collect::<Result<Vec<_>, EvalError>>()?;

        // Update all variables simultaneously
        for (name, new_val) in var_names.iter().zip(new_vals) {
            if let Some(val) = new_val {
                do_env.set(name.clone(), val);
            }
        }
    }
}

fn eval_when(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("when requires test and body".into()));
    }
    let test = eval(&args[0], env)?;
    if is_truthy(&test) {
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, env)?;
        }
        Ok(result)
    } else {
        Ok(Value::Void)
    }
}

fn eval_unless(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("unless requires test and body".into()));
    }
    let test = eval(&args[0], env)?;
    if !is_truthy(&test) {
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, env)?;
        }
        Ok(result)
    } else {
        Ok(Value::Void)
    }
}

fn eval_cons(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
    }
    let head = eval(&args[0], env)?;
    let tail = eval(&args[1], env)?;
    Ok(make_pair(head, tail))
}

fn eval_car(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires exactly 1 argument".into()));
    }
    value_car(&eval(&args[0], env)?)
}

fn eval_cdr(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
    }
    value_cdr(&eval(&args[0], env)?)
}

fn eval_null(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) => Ok(Value::Boolean(items.is_empty())),
        _ => Ok(Value::Boolean(false)),
    }
}

fn eval_list(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let items: Result<Vec<Value>, _> = args.iter().map(|a| eval(a, env)).collect();
    Ok(vec_to_pair_list(items?))
}

fn eval_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires exactly 1 argument".into()));
    }
    let val = eval(&args[0], env)?;
    match pair_list_to_vec(&val) {
        Some(items) => Ok(Value::Integer(items.len() as i64)),
        None => Err(EvalError::Type("length: not a proper list".into())),
    }
}

fn eval_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() { return Ok(Value::List(vec![])); }
    let mut result = Vec::new();
    let mut vals: Vec<Value> = Vec::new();
    for a in args {
        vals.push(eval(a, env)?);
    }
    for (i, v) in vals.iter().enumerate() {
        if i == vals.len() - 1 {
            match pair_list_to_vec(v) {
                Some(items) => result.extend(items),
                None => {
                    let mut tail = v.clone();
                    for item in result.into_iter().rev() {
                        tail = make_pair(item, tail);
                    }
                    return Ok(tail);
                }
            }
        } else {
            match pair_list_to_vec(v) {
                Some(items) => result.extend(items),
                None => return Err(EvalError::Type("append: not a proper list".into())),
            }
        }
    }
    Ok(vec_to_pair_list(result))
}

fn eval_type_pred(args: &[Expr], env: &Env, kind: &str) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{}? requires exactly 1 argument", kind)));
    }
    let val = eval(&args[0], env)?;
    let result = match kind {
        "number" => matches!(val, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_)),
        "string" => matches!(val, Value::Str(_)),
        "boolean" => matches!(val, Value::Boolean(_)),
        "pair" => matches!(val, Value::List(ref items) if !items.is_empty()) || matches!(val, Value::Pair(_)),
        "symbol" => matches!(val, Value::Symbol(_)),
        "char" => matches!(val, Value::Char(_)),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

// ── Display / Write / Newline ────────────────────────────────────────

fn display_value(val: &Value) -> String {
    display_value_inner(val, &mut HashSet::new())
}

fn display_value_inner(val: &Value, seen: &mut HashSet<usize>) -> String {
    match val {
        Value::Str(s) => s.clone(), // no quotes
        Value::Char(c) => c.to_string(),
        Value::Pair(p) => {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                return "(...)".to_string();
            }
            let mut s = String::from("(");
            let (car, cdr) = { let b = p.borrow(); (b.0.clone(), b.1.clone()) };
            s.push_str(&display_value_inner(&car, seen));
            display_pair_tail_display(&cdr, &mut s, seen);
            s.push(')');
            seen.remove(&ptr);
            s
        }
        Value::List(items) => {
            let mut s = String::from("(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value_inner(item, seen));
            }
            s.push(')');
            s
        }
        Value::Vector(items) => {
            let items = items.borrow();
            let mut s = String::from("#(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value_inner(item, seen));
            }
            s.push(')');
            s
        }
        other => format_value(other, seen),
    }
}

fn display_pair_tail_display(val: &Value, s: &mut String, seen: &mut HashSet<usize>) {
    match val {
        Value::Pair(p) => {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                s.push_str(" . (...)");
                return;
            }
            let (car, cdr) = { let b = p.borrow(); (b.0.clone(), b.1.clone()) };
            s.push(' ');
            s.push_str(&display_value_inner(&car, seen));
            display_pair_tail_display(&cdr, s, seen);
            seen.remove(&ptr);
        }
        Value::List(items) if items.is_empty() => {}
        Value::List(items) => {
            for item in items {
                s.push(' ');
                s.push_str(&display_value_inner(item, seen));
            }
        }
        other => {
            s.push_str(" . ");
            s.push_str(&display_value_inner(other, seen));
        }
    }
}

fn eval_display(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("display requires exactly 1 argument".into()));
    }
    let val = eval(&args[0], env)?;
    output_write(&display_value(&val));
    Ok(Value::Void)
}

fn eval_write(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("write requires exactly 1 argument".into()));
    }
    let val = eval(&args[0], env)?;
    output_write(&val.to_string());
    Ok(Value::Void)
}

fn eval_newline(args: &[Expr]) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity("newline takes no arguments".into()));
    }
    output_write("\n");
    Ok(Value::Void)
}

// ── String / Symbol / Char operations ───────────────────────────────

fn eval_string_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match eval(a, env)? {
            Value::Str(s) => result.push_str(&s),
            other => return Err(EvalError::Type(format!("string-append: expected string, got {}", other))),
        }
    }
    Ok(Value::Str(result))
}

fn eval_string_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-length requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        other => Err(EvalError::Type(format!("string-length: expected string, got {}", other))),
    }
}

fn eval_substring(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring requires exactly 3 arguments".into()));
    }
    let s = match eval(&args[0], env)? {
        Value::Str(s) => s,
        other => return Err(EvalError::Type(format!("substring: expected string, got {}", other))),
    };
    let start = require_int(&eval(&args[1], env)?)? as usize;
    let end = require_int(&eval(&args[2], env)?)? as usize;
    if start > end || end > s.len() {
        return Err(EvalError::Generic(format!("substring: index out of range")));
    }
    Ok(Value::Str(s[start..end].to_string()))
}

fn eval_string_to_number(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->number requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => {
            match s.parse::<i64>() {
                Ok(n) => Ok(Value::Integer(n)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        other => Err(EvalError::Type(format!("string->number: expected string, got {}", other))),
    }
}

fn eval_number_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string requires exactly 1 argument".into()));
    }
    let v = eval(&args[0], env)?;
    Ok(Value::Str(v.to_string()))
}

fn eval_symbol_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol->string requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Symbol(s) => Ok(Value::Str(s)),
        other => Err(EvalError::Type(format!("symbol->string: expected symbol, got {}", other))),
    }
}

fn eval_string_to_symbol(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->symbol requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => Ok(Value::Symbol(s)),
        other => Err(EvalError::Type(format!("string->symbol: expected string, got {}", other))),
    }
}

fn eval_string_ref(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref requires exactly 2 arguments".into()));
    }
    let s = match eval(&args[0], env)? {
        Value::Str(s) => s,
        other => return Err(EvalError::Type(format!("string-ref: expected string, got {}", other))),
    };
    let idx = require_int(&eval(&args[1], env)?)? as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalError::Generic(format!("string-ref: index {} out of range", idx))),
    }
}

fn eval_string_copy(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => Ok(Value::Str(s)),
        other => Err(EvalError::Type(format!("string-copy: expected string, got {}", other))),
    }
}

fn eval_string_set(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires exactly 3 arguments".into()));
    }
    // First arg must be a variable name (string literals are immutable)
    let var_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("string-set!: string is immutable".into())),
    };
    let idx = require_int(&eval(&args[1], env)?)? as usize;
    let ch = match eval(&args[2], env)? {
        Value::Char(c) => c,
        other => return Err(EvalError::Type(format!("string-set!: expected char, got {}", other))),
    };
    let s = match env.get(&var_name) {
        Some(Value::Str(s)) => s,
        Some(other) => return Err(EvalError::Type(format!("string-set!: expected string, got {}", other))),
        None => return Err(EvalError::UnboundVariable(var_name)),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Generic(format!("string-set!: index {} out of range", idx)));
    }
    chars[idx] = ch;
    let new_s: String = chars.into_iter().collect();
    if !env.set_existing(&var_name, Value::Str(new_s.clone())) {
        env.set(var_name, Value::Str(new_s));
    }
    Ok(Value::Void)
}

fn eval_string_to_list(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->list requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => {
            let chars: Vec<Value> = s.chars().map(Value::Char).collect();
            Ok(Value::List(chars))
        }
        other => Err(EvalError::Type(format!("string->list: expected string, got {}", other))),
    }
}

fn eval_list_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("list->string requires exactly 1 argument".into()));
    }
    let val = eval(&args[0], env)?;
    let items = require_list(&val)?;
    let mut s = String::new();
    for item in &items {
        match item {
            Value::Char(c) => s.push(*c),
            other => return Err(EvalError::Type(format!("list->string: expected char, got {}", other))),
        }
    }
    Ok(Value::Str(s))
}

fn eval_char_to_integer(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("char->integer requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Char(c) => Ok(Value::Integer(c as i64)),
        other => Err(EvalError::Type(format!("char->integer: expected char, got {}", other))),
    }
}

fn eval_integer_to_char(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("integer->char requires exactly 1 argument".into()));
    }
    let n = require_int(&eval(&args[0], env)?)?;
    match char::from_u32(n as u32) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalError::Generic(format!("integer->char: invalid code point {}", n))),
    }
}

// ── Macros (define-syntax / syntax-rules) ───────────────────────────

fn eval_define_record_type(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field-names...) predicate (field accessor)...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()));
    }
    // Parse type name (ignored for runtime, but we need it for error messages)
    let _type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("define-record-type: first arg must be a symbol".into())),
    };

    // Parse constructor: (constructor-name field-name ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => {
            let cname = match &items[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define-record-type: constructor name must be a symbol".into())),
            };
            let fields: Vec<String> = items[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("define-record-type: field name must be a symbol".into())),
            }).collect::<Result<_, _>>()?;
            (cname, fields)
        }
        _ => return Err(EvalError::Parse("define-record-type: constructor must be a list".into())),
    };

    // Parse predicate name
    let predicate_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("define-record-type: predicate must be a symbol".into())),
    };

    // Allocate a unique type ID
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::SeqCst);

    // Define constructor
    let nfields = constructor_fields.len();
    env.set(constructor_name, Value::Builtin(format!("__record-construct-{}-{}", type_id, nfields)));

    // Define predicate
    env.set(predicate_name, Value::Builtin(format!("__record-predicate-{}", type_id)));

    // Parse field specs: (field-name accessor-name)
    for field_spec in &args[3..] {
        match &field_spec.kind {
            ExprKind::List(items) if items.len() == 2 => {
                let field_name = match &items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("define-record-type: field name must be a symbol".into())),
                };
                let accessor_name = match &items[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("define-record-type: accessor name must be a symbol".into())),
                };
                // Find the field index in the constructor fields
                let field_idx = constructor_fields.iter().position(|f| f == &field_name)
                    .ok_or_else(|| EvalError::Parse(format!("define-record-type: field '{}' not in constructor", field_name)))?;
                env.set(accessor_name, Value::Builtin(format!("__record-accessor-{}-{}", type_id, field_idx)));
            }
            _ => return Err(EvalError::Parse("define-record-type: field spec must be (name accessor)".into())),
        }
    }

    Ok(Value::Void)
}

fn eval_define_syntax(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("define-syntax requires exactly 2 arguments".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("define-syntax: name must be a symbol".into())),
    };
    let sr = match &args[1].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Parse("define-syntax: expected syntax-rules".into())),
    };
    if sr.is_empty() || !matches!(&sr[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Parse("define-syntax: expected syntax-rules".into()));
    }
    if sr.len() < 2 {
        return Err(EvalError::Parse("syntax-rules: expected literals list".into()));
    }
    let literals = match &sr[1].kind {
        ExprKind::List(lits) => lits.iter().map(|l| match &l.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse("syntax-rules: literals must be symbols".into())),
        }).collect::<Result<Vec<_>, _>>()?,
        _ => return Err(EvalError::Parse("syntax-rules: expected literals list".into())),
    };
    let mut rules = Vec::new();
    for rule in &sr[2..] {
        match &rule.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(EvalError::Parse("syntax-rules: each rule must be (pattern template)".into())),
        }
    }
    env.set(name, Value::Macro(SyntaxRulesMacro {
        literals,
        rules,
        def_env: env.clone(),
    }));
    Ok(Value::Void)
}

#[derive(Debug, Clone)]
struct MacroBindings {
    single: HashMap<String, Expr>,
    ellipsis: HashMap<String, Vec<Expr>>,
}

fn match_macro_pattern(pattern: &Expr, input: &[Expr], literals: &[String]) -> Option<MacroBindings> {
    let pat_items = match &pattern.kind {
        ExprKind::List(items) => items,
        _ => return None,
    };
    // Skip first element (macro name) in both
    let pat = &pat_items[1..];
    let inp = &input[1..];
    let mut bindings = MacroBindings { single: HashMap::new(), ellipsis: HashMap::new() };
    if match_pat_elements(pat, inp, literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_pat_elements(pat: &[Expr], inp: &[Expr], literals: &[String], bindings: &mut MacroBindings) -> bool {
    let ellipsis_pos = pat.iter().position(|p| matches!(&p.kind, ExprKind::Symbol(s) if s == "..."));

    if let Some(epos) = ellipsis_pos {
        if epos == 0 { return false; }
        let before = &pat[..epos - 1];
        let ellipsis_var = &pat[epos - 1];
        let after = &pat[epos + 1..];
        let min_len = before.len() + after.len();
        if inp.len() < min_len { return false; }

        for (p, i) in before.iter().zip(inp.iter()) {
            if !match_pat_single(p, i, literals, bindings) { return false; }
        }
        let ellipsis_start = before.len();
        let ellipsis_end = inp.len() - after.len();
        if let ExprKind::Symbol(var_name) = &ellipsis_var.kind {
            bindings.ellipsis.insert(var_name.clone(), inp[ellipsis_start..ellipsis_end].to_vec());
        } else {
            return false;
        }
        for (p, i) in after.iter().zip(inp[ellipsis_end..].iter()) {
            if !match_pat_single(p, i, literals, bindings) { return false; }
        }
        true
    } else {
        if pat.len() != inp.len() { return false; }
        for (p, i) in pat.iter().zip(inp.iter()) {
            if !match_pat_single(p, i, literals, bindings) { return false; }
        }
        true
    }
}

fn match_pat_single(pat: &Expr, inp: &Expr, literals: &[String], bindings: &mut MacroBindings) -> bool {
    match &pat.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&inp.kind, ExprKind::Symbol(s2) if s == s2)
        }
        ExprKind::Symbol(s) => {
            bindings.single.insert(s.clone(), inp.clone());
            true
        }
        ExprKind::List(pat_items) => {
            match &inp.kind {
                ExprKind::List(inp_items) => match_pat_elements(pat_items, inp_items, literals, bindings),
                _ => false,
            }
        }
        ExprKind::Integer(n) => matches!(&inp.kind, ExprKind::Integer(m) if n == m),
        ExprKind::Rational(n, d) => matches!(&inp.kind, ExprKind::Rational(n2, d2) if n == n2 && d == d2),
        ExprKind::Float(f) => matches!(&inp.kind, ExprKind::Float(g) if f == g),
        ExprKind::Boolean(b) => matches!(&inp.kind, ExprKind::Boolean(c) if b == c),
        ExprKind::Str(s) => matches!(&inp.kind, ExprKind::Str(t) if s == t),
        ExprKind::Char(c) => matches!(&inp.kind, ExprKind::Char(d) if c == d),
    }
}

fn collect_pattern_vars(pattern: &Expr) -> HashSet<String> {
    let mut vars = HashSet::new();
    collect_pattern_vars_inner(pattern, &mut vars);
    vars
}

fn collect_pattern_vars_inner(expr: &Expr, vars: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::Symbol(s) if s != "..." && s != "_" => { vars.insert(s.clone()); }
        ExprKind::List(items) => { for item in items { collect_pattern_vars_inner(item, vars); } }
        _ => {}
    }
}

fn collect_template_free_vars(template: &Expr, pattern_vars: &HashSet<String>, free: &mut HashSet<String>) {
    match &template.kind {
        ExprKind::Symbol(s) if s != "..." && !pattern_vars.contains(s) => { free.insert(s.clone()); }
        ExprKind::List(items) => { for item in items { collect_template_free_vars(item, pattern_vars, free); } }
        _ => {}
    }
}

fn find_ellipsis_var(expr: &Expr, ellipsis_vars: &HashMap<String, Vec<Expr>>) -> Option<String> {
    match &expr.kind {
        ExprKind::Symbol(s) if ellipsis_vars.contains_key(s) => Some(s.clone()),
        ExprKind::List(items) => {
            for item in items {
                if let Some(v) = find_ellipsis_var(item, ellipsis_vars) { return Some(v); }
            }
            None
        }
        _ => None,
    }
}

fn expand_template(template: &Expr, bindings: &MacroBindings, gensym_map: &HashMap<String, String>) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(expr) = bindings.single.get(s) {
                expr.clone()
            } else if let Some(gs) = gensym_map.get(s) {
                Expr { kind: ExprKind::Symbol(gs.clone()), span: template.span }
            } else {
                template.clone()
            }
        }
        ExprKind::List(items) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() && matches!(&items[i + 1].kind, ExprKind::Symbol(s) if s == "...") {
                    // Ellipsis expansion
                    if let Some(var_name) = find_ellipsis_var(&items[i], &bindings.ellipsis) {
                        let values = &bindings.ellipsis[&var_name];
                        for v in values {
                            let mut sub_bindings = bindings.clone();
                            sub_bindings.single.insert(var_name.clone(), v.clone());
                            result.push(expand_template(&items[i], &sub_bindings, gensym_map));
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_template(&items[i], bindings, gensym_map));
                    i += 1;
                }
            }
            Expr { kind: ExprKind::List(result), span: template.span }
        }
        _ => template.clone(),
    }
}

fn expand_and_eval_macro(mac: &SyntaxRulesMacro, input: &[Expr], use_env: &Env) -> Result<Value, EvalError> {
    for (pattern, template) in &mac.rules {
        if let Some(bindings) = match_macro_pattern(pattern, input, &mac.literals) {
            let pattern_vars = collect_pattern_vars(pattern);
            let mut free_vars = HashSet::new();
            collect_template_free_vars(template, &pattern_vars, &mut free_vars);

            // Gensym free vars that are bound in definition env (hygiene)
            let mut gensym_map = HashMap::new();
            for fv in &free_vars {
                if mac.def_env.get(fv).is_some() {
                    gensym_map.insert(fv.clone(), gensym(fv));
                }
            }

            let expanded = expand_template(template, &bindings, &gensym_map);

            // Inject definition-site bindings for gensymed vars
            for (orig, gs) in &gensym_map {
                if let Some(val) = mac.def_env.get(orig) {
                    use_env.set(gs.clone(), val);
                }
            }

            return Ok(Value::TailCall(Box::new((expanded, use_env.clone()))));
        }
    }
    Err(EvalError::Generic("no matching syntax-rules pattern".into()))
}

fn seed_builtins(env: &Env) {
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "not", "cons", "car", "cdr", "null?", "list", "length", "append",
        "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline", "apply",
        "string-append", "string-length", "number->string",
        "symbol->string", "string->symbol",
        // L09
        "eq?", "equal?",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc", "map", "for-each",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
        // L11
        "exact?", "inexact?", "integer?", "rational?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
        // L14
        "eqv?",
        "vector", "make-vector", "vector-ref", "vector-set!", "vector-length",
        "vector?", "vector->list", "list->vector",
        // L18
        "call/cc", "call-with-current-continuation",
        "dynamic-wind",
        "raise", "raise-continuable", "with-exception-handler",
        // L17
        "set-car!", "set-cdr!",
        "caar", "cadr", "cdar", "cddr", "caddr", "cdddr", "cadddr", "caddar",
        "caaar", "caadr", "cdaar", "cdadr", "cddar",
        "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr",
        "caddar", "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
        "memq", "memv", "assq", "reverse",
        "member", "assv", "gcd", "lcm", "truncate", "round",
        "make-string", "string", "string>?", "string<=?", "string>=?",
        "cadar",
        "string->number", "string-ref", "substring", "string-copy",
        "string->list", "list->string", "char->integer", "integer->char",
        "procedure?",
    ] {
        env.set(name.to_string(), Value::Builtin(name.to_string()));
    }
}

// ── Continuations ───────────────────────────────────────────────────

fn eval_callcc_with_proc(proc: &Value) -> Result<Value, EvalError> {
    let id = CALLCC_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    let frames = CONT_FRAMES.with(|f| f.borrow().clone());
    let cont_data = Rc::new(ContinuationData { id, frames });
    let k = Value::Continuation(cont_data);
    let result = apply(proc, &[k]);
    match result {
        Ok(v) => Ok(v),
        Err(EvalError::ContinuationEscape(ret_id)) if ret_id == id => {
            let val = CONTINUATION_VALUE.with(|v| v.borrow_mut().take()).unwrap();
            PENDING_CONTINUATION.with(|pc| *pc.borrow_mut() = None);
            Ok(val)
        }
        Err(e) => Err(e),
    }
}

fn eval_body_with_frames(body: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Ok(Value::Void);
    }
    CONT_FRAMES.with(|f| {
        f.borrow_mut().push(ContinuationFrame {
            exprs: body.to_vec(),
            env: env.clone(),
        });
    });
    let mut last = Value::Void;
    for (i, expr) in body.iter().enumerate() {
        CONT_FRAMES.with(|f| {
            if let Some(frame) = f.borrow_mut().last_mut() {
                frame.exprs = body[i..].to_vec();
            }
        });
        last = eval(expr, env)?;
    }
    CONT_FRAMES.with(|f| f.borrow_mut().pop());
    Ok(last)
}

fn init_callcc_state() {
    CALLCC_RETURN.with(|r| *r.borrow_mut() = None);
    CONT_FRAMES.with(|f| f.borrow_mut().clear());
    RESUME_FRAMES.with(|rf| rf.borrow_mut().clear());
    CONTINUATION_VALUE.with(|v| *v.borrow_mut() = None);
    PENDING_CONTINUATION.with(|pc| *pc.borrow_mut() = None);
}

fn eval_top_level_loop(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut current_exprs: Vec<Expr> = exprs.to_vec();
    let mut current_env = env.clone();

    loop {
        CONT_FRAMES.with(|f| {
            f.borrow_mut().clear();
            f.borrow_mut().push(ContinuationFrame {
                exprs: current_exprs.clone(),
                env: current_env.clone(),
            });
        });

        let mut last = Value::Void;
        let mut error = None;

        for (i, expr) in current_exprs.iter().enumerate() {
            CONT_FRAMES.with(|f| {
                if let Some(frame) = f.borrow_mut().last_mut() {
                    frame.exprs = current_exprs[i..].to_vec();
                }
            });
            match eval(expr, &current_env) {
                Ok(v) => last = v,
                Err(e) => { error = Some(e); break; }
            }
        }

        CONT_FRAMES.with(|f| f.borrow_mut().pop());

        match error {
            None => return Ok(last),
            Some(EvalError::ContinuationEscape(_id)) => {
                let val = CONTINUATION_VALUE.with(|v| v.borrow_mut().take()).unwrap();
                let cont_data = PENDING_CONTINUATION.with(|pc| pc.borrow_mut().take()).unwrap();
                CALLCC_RETURN.with(|r| *r.borrow_mut() = Some(val));
                RESUME_FRAMES.with(|rf| {
                    rf.borrow_mut().clear();
                    if cont_data.frames.len() > 1 {
                        *rf.borrow_mut() = cont_data.frames[1..].to_vec();
                    }
                });
                current_exprs = cont_data.frames[0].exprs.clone();
                current_env = cont_data.frames[0].env.clone();
                continue;
            }
            Some(e) => return Err(e),
        }
    }
}

// ── Public API ──────────────────────────────────────────────────────

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let tokens = tokenize(input)?;
    let exprs = parse_all(&tokens)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    seed_builtins(&env);
    init_callcc_state();
    let last = eval_top_level_loop(&exprs, &env)?;
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((last.to_string(), output))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input)?;
    let exprs = parse_all(&tokens)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    seed_builtins(&env);
    init_callcc_state();
    let last = eval_top_level_loop(&exprs, &env)?;
    Ok(last.to_string())
}

#[cfg(test)]
mod tests;
