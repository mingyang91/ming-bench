pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

// ── Span ──

#[derive(Debug, Clone, Copy, PartialEq)]
struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    fn fmt(&self, msg: &str) -> String {
        format!("{}:{}: {}", self.line, self.col, msg)
    }
}

// ── Value ──

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always reduced, denom > 0)
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Pair(Rc<RefCell<(Value, Value)>>),
    Builtin(String),
    Continuation { id: u64, top_level_idx: usize, callcc_span: Span },
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Values(Vec<Value>),
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<Value>,
    },
    Syntax(Box<Expr>),
    MacroTransformer(Box<Value>),
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    if d == 0 { panic!("division by zero in make_rational"); }
    let sign = if (n < 0) != (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

/// Convert a Value to an f64 for inexact arithmetic
fn val_to_f64(v: &Value, span: Span) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type(span.fmt("expected number"))),
    }
}

fn is_number(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

fn is_exact(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(_, _))
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

fn value_to_vec(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(items) => Some(items.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = val.clone();
            let mut seen = HashSet::new();
            loop {
                let next = match &current {
                    Value::Pair(c) => {
                        let ptr = Rc::as_ptr(c) as usize;
                        if !seen.insert(ptr) { return None; } // cycle
                        let borrowed = c.borrow();
                        result.push(borrowed.0.clone());
                        Some(borrowed.1.clone())
                    }
                    Value::List(items) if items.is_empty() => return Some(result),
                    _ => return None,
                };
                if let Some(n) = next { current = n; }
            }
        }
        _ => None,
    }
}

/// Check if any value in the slice is inexact
fn any_inexact(args: &[Value]) -> bool {
    args.iter().any(|v| matches!(v, Value::Float(_)))
}

type Output = Rc<RefCell<std::string::String>>;

thread_local! {
    static CONT_RETURN_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    static CALLCC_OVERRIDE: RefCell<Option<(Span, Value)>> = RefCell::new(None);
    static CONT_CALLCC_SPAN: RefCell<Option<Span>> = RefCell::new(None);
    static NEXT_CONT_ID: Cell<u64> = Cell::new(0);
    static TOP_LEVEL_IDX: Cell<usize> = Cell::new(0);
    static GENSYM_COUNTER: Cell<u64> = Cell::new(0);
    static RAISED_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    static NEXT_RECORD_TYPE_ID: Cell<u64> = Cell::new(0);
    static SYNTAX_BINDINGS: RefCell<Vec<HashMap<String, MacroBinding>>> = RefCell::new(Vec::new());
    static SYNTAX_DEF_ENV: RefCell<Option<Env>> = RefCell::new(None);
    static SYNTAX_CALL_ENV: RefCell<Option<Env>> = RefCell::new(None);
}

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.with(|c| { let v = c.get(); c.set(v + 1); v });
    format!("{}##{}", base, n)
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                if f.is_infinite() {
                    if *f > 0.0 { "+inf.0".into() } else { "-inf.0".into() }
                } else if f.is_nan() {
                    "+nan.0".into()
                } else {
                    let s = format!("{}", f);
                    // Ensure there's a decimal point
                    if s.contains('.') { s } else { format!("{}.0", s) }
                }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(b) => if *b { "#t".into() } else { "#f".into() },
            Value::Char(c) => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{}", c),
            },
            Value::String(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => {
                let mut parts = Vec::new();
                let mut current = self.clone();
                let mut seen = HashSet::new();
                let mut is_dotted = false;
                let mut tail = String::new();
                loop {
                    let next = match &current {
                        Value::Pair(c) => {
                            let ptr = Rc::as_ptr(c) as usize;
                            if !seen.insert(ptr) { None }
                            else {
                                let borrowed = c.borrow();
                                parts.push(borrowed.0.display());
                                Some(borrowed.1.clone())
                            }
                        }
                        Value::List(items) if items.is_empty() => None,
                        other => {
                            is_dotted = true;
                            tail = other.display();
                            None
                        }
                    };
                    match next {
                        Some(n) => current = n,
                        None => break,
                    }
                }
                if is_dotted {
                    format!("({} . {})", parts.join(" "), tail)
                } else {
                    format!("({})", parts.join(" "))
                }
            }
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Continuation { .. } => "#<continuation>".into(),
            Value::SyntaxRules { .. } => "#<syntax>".into(),
            Value::Syntax(_) => "#<syntax>".into(),
            Value::MacroTransformer(_) => "#<syntax>".into(),
            Value::Vector(v) => {
                let items: Vec<String> = v.borrow().iter().map(|v| v.display()).collect();
                format!("#({})", items.join(" "))
            }
            Value::Values(vals) => {
                let inner: Vec<String> = vals.iter().map(|v| v.display()).collect();
                inner.join("\n")
            }
            Value::Record { type_name, .. } => format!("#<record:{}>", type_name),
        }
    }

    /// Format for `display` — no quotes on strings, chars as raw chars
    fn display_output(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Pair(_) => self.display(),
            Value::Vector(_) => self.display(),
            _ => self.display(),
        }
    }

    /// Format for `write` — strings get quotes
    fn write_output(&self) -> String {
        self.display()
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// ── Environment ──

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl std::fmt::Debug for EnvInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EnvInner")
            .field("bindings", &self.bindings.keys().collect::<Vec<_>>())
            .finish()
    }
}

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        Some(v.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

// ── Parser ──

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
    while i < chars.len() {
        match chars[i] {
            '\n' => { i += 1; line += 1; col = 1; }
            ' ' | '\t' | '\r' => { i += 1; col += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(Token { text: "(".into(), span: Span::new(line, col) });
                i += 1; col += 1;
            }
            ')' => {
                tokens.push(Token { text: ")".into(), span: Span::new(line, col) });
                i += 1; col += 1;
            }
            '\'' => {
                tokens.push(Token { text: "'".into(), span: Span::new(line, col) });
                i += 1; col += 1;
            }
            '"' => {
                let start_col = col;
                let mut s = std::string::String::new();
                s.push('"');
                i += 1; col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2; col += 2;
                    } else {
                        if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1; col += 1;
                }
                tokens.push(Token { text: s, span: Span::new(line, start_col) });
            }
            _ => {
                let start_col = col;
                // Handle #' (syntax quote)
                if chars[i] == '#' && i + 1 < chars.len() && chars[i + 1] == '\'' {
                    tokens.push(Token { text: "#'".into(), span: Span::new(line, col) });
                    i += 2; col += 2;
                } else {
                    let start = i;
                    while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                        i += 1; col += 1;
                    }
                    tokens.push(Token {
                        text: chars[start..i].iter().collect(),
                        span: Span::new(line, start_col),
                    });
                }
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let span = tokens[*pos].span;
    let text = &tokens[*pos].text;
    if text == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("quote".into()), span },
                inner,
            ]),
            span,
        })
    } else if text == "#'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("syntax".into()), span },
                inner,
            ]),
            span,
        })
    } else if text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse(span.fmt("missing closing paren")));
        }
        *pos += 1;
        Ok(Expr { kind: ExprKind::List(list), span })
    } else if text == ")" {
        Err(EvalError::Parse(span.fmt("unexpected )")))
    } else {
        *pos += 1;
        Ok(parse_atom(text, span))
    }
}

fn parse_atom(token: &str, span: Span) -> Expr {
    let kind = if token == "#t" {
        ExprKind::Boolean(true)
    } else if token == "#f" {
        ExprKind::Boolean(false)
    } else if token.starts_with("#\\") {
        let ch_str = &token[2..];
        let ch = match ch_str {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Expr { kind: ExprKind::Symbol(token.into()), span },
        };
        ExprKind::Char(ch)
    } else if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        ExprKind::String(inner.into())
    } else if let Ok(n) = token.parse::<i64>() {
        ExprKind::Integer(n)
    } else if token.contains('/') && !token.starts_with('/') {
        // Try rational literal: n/d
        let parts: Vec<&str> = token.splitn(2, '/').collect();
        if parts.len() == 2 {
            if let (Ok(n), Ok(d)) = (parts[0].parse::<i64>(), parts[1].parse::<i64>()) {
                if d != 0 {
                    return Expr { kind: ExprKind::Rational(n, d), span };
                }
            }
        }
        ExprKind::Symbol(token.into())
    } else if let Ok(f) = token.parse::<f64>() {
        ExprKind::Float(f)
    } else {
        ExprKind::Symbol(token.into())
    };
    Expr { kind, span }
}

fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ──

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::String(s) => Value::String(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => {
            if items.is_empty() {
                Value::List(vec![])
            } else {
                vec_to_pair_list(items.iter().map(expr_to_value).collect())
            }
        }
    }
}

fn eqv_match(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

fn eval(expr: &Expr, env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    loop {
        let span = cur_expr.span;
        let kind = cur_expr.kind.clone();
        match kind {
            ExprKind::Integer(n) => return Ok(Value::Integer(n)),
            ExprKind::Float(f) => return Ok(Value::Float(f)),
            ExprKind::Rational(n, d) => return Ok(make_rational(n, d)),
            ExprKind::Boolean(b) => return Ok(Value::Boolean(b)),
            ExprKind::Char(c) => return Ok(Value::Char(c)),
            ExprKind::String(s) => return Ok(Value::String(s)),
            ExprKind::Symbol(ref name) => {
                return env_get(&cur_env, name)
                    .ok_or_else(|| EvalError::UnboundVariable(span.fmt(name)));
            }
            ExprKind::List(items) => {
                if items.is_empty() {
                    return Err(EvalError::Type(span.fmt("empty application")));
                }
                if let ExprKind::Symbol(ref op) = items[0].kind {
                    match op.as_str() {
                        "define" => return eval_define(&items[1..], &cur_env, span, out),
                        "if" => {
                            let args = &items[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::Arity(span.fmt("if requires 2 or 3 arguments")));
                            }
                            let cond = eval(&args[0], &cur_env, out)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                                continue;
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                                continue;
                            } else {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        "quote" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("quote requires 1 argument")));
                            }
                            return Ok(expr_to_value(&items[1]));
                        }
                        "lambda" => return eval_lambda(&items[1..], &cur_env, span),
                        "let" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity(span.fmt("let requires bindings and body")));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let ExprKind::Symbol(ref name) = args[0].kind {
                                let bindings = match &args[1].kind {
                                    ExprKind::List(b) => b,
                                    _ => return Err(EvalError::Type(span.fmt("named let: expected binding list"))),
                                };
                                let mut params = Vec::new();
                                let mut init_vals = Vec::new();
                                for binding in bindings {
                                    match &binding.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Type(binding.span.fmt("let: expected symbol"))),
                                            };
                                            let val = eval(&pair[1], &cur_env, out)?;
                                            params.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => return Err(EvalError::Type(binding.span.fmt("let: bad binding"))),
                                    }
                                }
                                let body = args[2..].to_vec();
                                let local_env = new_env(Some(cur_env.clone()));
                                let lambda = Value::Lambda { params: params.clone(), rest_param: None, body: body.clone(), env: local_env.clone() };
                                env_set(&local_env, name.clone(), lambda);
                                for (p, v) in params.iter().zip(init_vals.iter()) {
                                    env_set(&local_env, p.clone(), v.clone());
                                }
                                for expr in &body[..body.len() - 1] {
                                    eval(expr, &local_env, out)?;
                                }
                                cur_expr = body.last().unwrap().clone();
                                cur_env = local_env;
                                continue;
                            }
                            // Regular let
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(EvalError::Type(span.fmt("let: expected binding list"))),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Type(binding.span.fmt("let: expected symbol"))),
                                        };
                                        let val = eval(&pair[1], &cur_env, out)?;
                                        env_set(&local_env, name, val);
                                    }
                                    _ => return Err(EvalError::Type(binding.span.fmt("let: bad binding"))),
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue;
                        }
                        "letrec" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity(span.fmt("letrec requires bindings and body")));
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(EvalError::Type(span.fmt("letrec: expected binding list"))),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            // First pass: bind all names to undefined (false)
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Type(binding.span.fmt("letrec: expected symbol"))),
                                        };
                                        env_set(&local_env, name, Value::Boolean(false));
                                    }
                                    _ => return Err(EvalError::Type(binding.span.fmt("letrec: bad binding"))),
                                }
                            }
                            // Second pass: evaluate inits in the local env and set!
                            for binding in bindings {
                                if let ExprKind::List(pair) = &binding.kind {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => unreachable!(),
                                    };
                                    let val = eval(&pair[1], &local_env, out)?;
                                    env_set(&local_env, name, val);
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue;
                        }
                        "letrec*" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity(span.fmt("letrec* requires bindings and body")));
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(EvalError::Type(span.fmt("letrec*: expected binding list"))),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Type(binding.span.fmt("letrec*: expected symbol"))),
                                        };
                                        let val = eval(&pair[1], &local_env, out)?;
                                        env_set(&local_env, name, val);
                                    }
                                    _ => return Err(EvalError::Type(binding.span.fmt("letrec*: bad binding"))),
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue;
                        }
                        "case" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity(span.fmt("case requires key and clauses")));
                            }
                            let key = eval(&args[0], &cur_env, out)?;
                            let clauses = &args[1..];
                            let mut found = false;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let is_else = matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else");
                                        if is_else {
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval(expr, &cur_env, out)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                        // Match datums: ((d1 d2 ...) body ...)
                                        if let ExprKind::List(datums) = &parts[0].kind {
                                            let matched = datums.iter().any(|d| {
                                                eqv_match(&key, &expr_to_value(d))
                                            });
                                            if matched {
                                                for expr in &parts[1..parts.len() - 1] {
                                                    eval(expr, &cur_env, out)?;
                                                }
                                                cur_expr = parts.last().unwrap().clone();
                                                found = true;
                                                break;
                                            }
                                        }
                                    }
                                    _ => return Err(EvalError::Type(clause.span.fmt("case: bad clause"))),
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Value::Boolean(false));
                        }
                        "begin" => {
                            let body = &items[1..];
                            if body.is_empty() {
                                return Ok(Value::Boolean(false));
                            }
                            for item in &body[..body.len() - 1] {
                                eval(item, &cur_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            continue;
                        }
                        "cond" => {
                            let clauses = &items[1..];
                            let mut found = false;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let is_else = matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else");
                                        if is_else || eval(&parts[0], &cur_env, out)?.is_truthy() {
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval(expr, &cur_env, out)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                    }
                                    _ => return Err(EvalError::Type(clause.span.fmt("cond: bad clause"))),
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Value::Boolean(false));
                        }
                        "and" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(true));
                            }
                            for arg in &args[..args.len() - 1] {
                                let val = eval(arg, &cur_env, out)?;
                                if !val.is_truthy() {
                                    return Ok(val);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue;
                        }
                        "or" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(false));
                            }
                            for arg in &args[..args.len() - 1] {
                                let val = eval(arg, &cur_env, out)?;
                                if val.is_truthy() {
                                    return Ok(val);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue;
                        }
                        "call/cc" | "call-with-current-continuation" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("call/cc requires 1 argument")));
                            }
                            let override_val = CALLCC_OVERRIDE.with(|o| {
                                let mut guard = o.borrow_mut();
                                if let Some((ref s, _)) = *guard {
                                    if *s == span { return guard.take().map(|(_, v)| v); }
                                }
                                None
                            });
                            if let Some(val) = override_val {
                                return Ok(val);
                            }
                            let arg = eval(&items[1], &cur_env, out)?;
                            return eval_callcc(arg, span, out);
                        }
                        "define-syntax" => {
                            if items.len() != 3 {
                                return Err(EvalError::Arity(span.fmt("define-syntax requires name and transformer")));
                            }
                            let name = match &items[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Type(span.fmt("define-syntax: expected symbol"))),
                            };
                            // Try syntax-rules first, fall back to evaluating as expression (lambda)
                            let transformer = match parse_syntax_rules(&items[2], &cur_env, span) {
                                Ok(sr) => sr,
                                Err(_) => {
                                    let val = eval(&items[2], &cur_env, out)?;
                                    Value::MacroTransformer(Box::new(val))
                                }
                            };
                            env_set(&cur_env, name, transformer);
                            return Ok(Value::Boolean(false));
                        }
                        "syntax-case" => {
                            return eval_syntax_case(&items[1..], &cur_env, span, out);
                        }
                        "syntax" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("syntax requires 1 argument")));
                            }
                            return eval_syntax_template(&items[1], span);
                        }
                        "with-syntax" => {
                            return eval_with_syntax(&items[1..], &cur_env, span, out);
                        }
                        "define-record-type" => {
                            return eval_define_record_type(&items[1..], &cur_env, span);
                        }
                        "dynamic-wind" => {
                            if items.len() != 4 {
                                return Err(EvalError::Arity(span.fmt("dynamic-wind requires 3 arguments")));
                            }
                            let in_thunk = eval(&items[1], &cur_env, out)?;
                            let body_thunk = eval(&items[2], &cur_env, out)?;
                            let out_thunk = eval(&items[3], &cur_env, out)?;
                            return eval_dynamic_wind(in_thunk, body_thunk, out_thunk, span, out);
                        }
                        "guard" => {
                            return eval_guard(&items[1..], &cur_env, span, out);
                        }
                        "raise" if env_get(&cur_env, "raise").map_or(true, |v| matches!(v, Value::Builtin(ref n) if n == "raise")) => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("raise requires 1 argument")));
                            }
                            let val = eval(&items[1], &cur_env, out)?;
                            RAISED_VALUE.with(|v| *v.borrow_mut() = Some(val));
                            return Err(EvalError::Raise);
                        }
                        "with-exception-handler" if env_get(&cur_env, "with-exception-handler").map_or(true, |v| matches!(v, Value::Builtin(ref n) if n == "with-exception-handler")) => {
                            if items.len() != 3 {
                                return Err(EvalError::Arity(span.fmt("with-exception-handler requires 2 arguments")));
                            }
                            let handler = eval(&items[1], &cur_env, out)?;
                            let thunk = eval(&items[2], &cur_env, out)?;
                            return eval_with_exception_handler(handler, thunk, span, out);
                        }
                        "set!" => return eval_set(&items[1..], &cur_env, span, out),
                        "string-set!" => return eval_string_set(&items[1..], &cur_env, span, out),
                        "display" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("display requires 1 argument")));
                            }
                            let val = eval(&items[1], &cur_env, out)?;
                            out.borrow_mut().push_str(&val.display_output());
                            return Ok(Value::Boolean(false));
                        }
                        "write" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("write requires 1 argument")));
                            }
                            let val = eval(&items[1], &cur_env, out)?;
                            out.borrow_mut().push_str(&val.write_output());
                            return Ok(Value::Boolean(false));
                        }
                        "newline" => {
                            if items.len() != 1 {
                                return Err(EvalError::Arity(span.fmt("newline takes 0 arguments")));
                            }
                            out.borrow_mut().push('\n');
                            return Ok(Value::Boolean(false));
                        }
                        "map" => {
                            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, out)).collect::<Result<_, _>>()?;
                            return eval_map(&args, span, out);
                        }
                        "for-each" => {
                            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, out)).collect::<Result<_, _>>()?;
                            return eval_for_each(&args, span, out);
                        }
                        "cons" | "car" | "cdr" | "cadr" | "cddr" | "caar" | "cdar"
                        | "null?" | "list" | "length"
                        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
                        | "exact?" | "inexact?" | "integer?" | "rational?"
                        | "exact->inexact" | "inexact->exact"
                        | "numerator" | "denominator"
                        | "string-append" | "string-length" | "substring"
                        | "string->number" | "number->string"
                        | "symbol->string" | "string->symbol"
                        | "string-ref" | "string-copy" | "string->list" | "list->string"
                        | "char->integer" | "integer->char"
                        | "eq?" | "eqv?" | "equal?" | "append" | "reverse"
                        | "abs" | "modulo" | "remainder" | "quotient" | "expt"
                        | "min" | "max"
                        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
                        | "list-ref" | "list-tail" | "list?" | "assoc"
                        | "char-alphabetic?" | "char-numeric?"
                        | "char-upcase" | "char-downcase"
                        | "char=?" | "char<?"
                        | "string=?" | "string<?" | "string-ci=?"
                        | "string-upcase" | "string-downcase"
                        | "vector" | "make-vector" | "vector-ref" | "vector-set!"
                        | "vector-length" | "vector?" | "vector->list" | "list->vector"
                        | "set-car!" | "set-cdr!"
                        | "syntax->datum" | "datum->syntax"
                        | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" => {
                            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, out)).collect::<Result<_, _>>()?;
                            return eval_builtin(op, &args, span);
                        }
                        _ => {
                            if let Some(val) = env_get(&cur_env, op) {
                                if let Value::SyntaxRules { ref literals, ref rules, env: ref def_env } = val {
                                    let expanded = expand_syntax_rules(&items, literals, rules, def_env, &cur_env, span)?;
                                    cur_expr = expanded;
                                    continue;
                                }
                                if let Value::MacroTransformer(ref transformer) = val {
                                    let expanded = expand_macro_transformer(transformer, &items, &cur_env, span, out)?;
                                    cur_expr = expanded;
                                    continue;
                                }
                            }
                        }
                    }
                }
                // Function application with TCO
                let func = eval(&items[0], &cur_env, out)?;
                let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, out)).collect::<Result<_, _>>()?;
                match func {
                    Value::Lambda { ref params, ref rest_param, ref body, ref env } => {
                        let local_env = apply_lambda(params, rest_param, body, env, &args, span)?;
                        for expr in &body[..body.len() - 1] {
                            eval(expr, &local_env, out)?;
                        }
                        cur_expr = body.last().unwrap().clone();
                        cur_env = local_env;
                        continue;
                    }
                    Value::Builtin(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity(span.fmt("call/cc requires 1 argument")));
                        }
                        let override_val = CALLCC_OVERRIDE.with(|o| {
                            let mut guard = o.borrow_mut();
                            if let Some((ref s, _)) = *guard {
                                if *s == span { return guard.take().map(|(_, v)| v); }
                            }
                            None
                        });
                        if let Some(val) = override_val {
                            return Ok(val);
                        }
                        return eval_callcc(args.into_iter().next().unwrap(), span, out);
                    }
                    Value::Builtin(ref name) if name == "raise" => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity(span.fmt("raise requires 1 argument")));
                        }
                        RAISED_VALUE.with(|v| *v.borrow_mut() = Some(args.into_iter().next().unwrap()));
                        return Err(EvalError::Raise);
                    }
                    Value::Builtin(ref name) if name == "with-exception-handler" => {
                        if args.len() != 2 {
                            return Err(EvalError::Arity(span.fmt("with-exception-handler requires 2 arguments")));
                        }
                        let mut it = args.into_iter();
                        let handler = it.next().unwrap();
                        let thunk = it.next().unwrap();
                        return eval_with_exception_handler(handler, thunk, span, out);
                    }
                    Value::Builtin(ref name) if name == "apply" => {
                        return eval_apply(&args, span, out);
                    }
                    Value::Builtin(ref name) if name == "map" => {
                        return eval_map(&args, span, out);
                    }
                    Value::Builtin(ref name) if name == "for-each" => {
                        return eval_for_each(&args, span, out);
                    }
                    Value::Builtin(ref name) if name == "call-with-values" => {
                        return eval_call_with_values(&args, span, out);
                    }
                    Value::Builtin(ref name) => {
                        return eval_builtin(name, &args, span);
                    }
                    Value::Continuation { id, top_level_idx, callcc_span } => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity(span.fmt("continuation requires 1 argument")));
                        }
                        CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = Some(args[0].clone()));
                        CONT_CALLCC_SPAN.with(|v| *v.borrow_mut() = Some(callcc_span));
                        return Err(EvalError::ContinuationInvoked(id, top_level_idx));
                    }
                    _ => return Err(EvalError::Type(span.fmt("not a procedure"))),
                }
            }
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(span.fmt("define requires arguments")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(span.fmt("define requires a value")));
            }
            let val = eval(&args[1], env, out)?;
            env_set(env, name.clone(), val);
            Ok(Value::Boolean(false))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Arity(span.fmt("define: empty signature")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => return Err(EvalError::Type(span.fmt("define: expected symbol"))),
            };
            let (params, rest_param) = parse_param_list(&sig[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type(span.fmt("define: expected symbol or list"))),
    }
}


fn eval_define_record_type(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    // args[0] = type name, args[1] = constructor spec, args[2] = predicate, args[3..] = field specs
    if args.len() < 3 {
        return Err(EvalError::Arity(span.fmt("define-record-type requires at least 3 arguments")));
    }

    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(span.fmt("define-record-type: expected type name symbol"))),
    };

    // Parse constructor: (constructor-name field1 field2 ...)
    let (ctor_name, ctor_fields) = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => {
            let name = match &items[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type(span.fmt("define-record-type: expected constructor name"))),
            };
            let fields: Vec<String> = items[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(span.fmt("define-record-type: expected field name"))),
            }).collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Type(span.fmt("define-record-type: expected constructor spec"))),
    };

    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(span.fmt("define-record-type: expected predicate name"))),
    };

    // Parse field specs: (field-name accessor-name)
    let mut all_fields: Vec<String> = Vec::new();
    let mut accessors: Vec<(String, usize)> = Vec::new(); // (accessor-name, field-index)

    for field_spec in &args[3..] {
        match &field_spec.kind {
            ExprKind::List(items) if items.len() >= 2 => {
                let field_name = match &items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(span.fmt("define-record-type: expected field name"))),
                };
                let accessor_name = match &items[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(span.fmt("define-record-type: expected accessor name"))),
                };
                let idx = all_fields.len();
                all_fields.push(field_name);
                accessors.push((accessor_name, idx));
            }
            _ => return Err(EvalError::Type(span.fmt("define-record-type: expected field spec"))),
        }
    }

    // Allocate a unique type ID
    let type_id = NEXT_RECORD_TYPE_ID.with(|c| { let v = c.get(); c.set(v + 1); v });

    // Build a mapping from constructor field names to all_fields indices
    let ctor_field_indices: Vec<usize> = ctor_fields.iter().map(|f| {
        all_fields.iter().position(|af| af == f)
            .ok_or_else(|| EvalError::Type(span.fmt(&format!("define-record-type: constructor field '{}' not in field specs", f))))
    }).collect::<Result<_, _>>()?;

    // Define constructor
    let ctor_builtin_name = format!("%%record-ctor%%{}%%{}%%{}", type_id, type_name,
        ctor_field_indices.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(","));
    env_set(env, ctor_name, Value::Builtin(ctor_builtin_name));

    // Define predicate
    let pred_builtin_name = format!("%%record-pred%%{}", type_id);
    env_set(env, pred_name, Value::Builtin(pred_builtin_name));

    // Define accessors
    for (accessor_name, field_idx) in &accessors {
        let acc_builtin_name = format!("%%record-acc%%{}%%{}", type_id, field_idx);
        env_set(env, accessor_name.clone(), Value::Builtin(acc_builtin_name));
    }

    Ok(Value::Boolean(false))
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("lambda requires params and body")));
    }
    match &args[0].kind {
        ExprKind::List(param_exprs) => {
            let (params, rest_param) = parse_param_list(param_exprs)?;
            let body = args[1..].to_vec();
            Ok(Value::Lambda { params, rest_param, body, env: env.clone() })
        }
        // (lambda args body) — single symbol captures all args as rest
        ExprKind::Symbol(s) => {
            let body = args[1..].to_vec();
            Ok(Value::Lambda { params: vec![], rest_param: Some(s.clone()), body, env: env.clone() })
        }
        _ => Err(EvalError::Type(span.fmt("lambda: expected parameter list"))),
    }
}

/// Parse a parameter list that may contain dot notation for rest params.
/// E.g. `(x y . rest)` → (vec!["x","y"], Some("rest"))
/// E.g. `(x y)` → (vec!["x","y"], None)
fn parse_param_list(exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                // Next symbol is the rest param
                if i + 1 >= exprs.len() {
                    return Err(EvalError::Parse("expected rest parameter after dot".into()));
                }
                match &exprs[i + 1].kind {
                    ExprKind::Symbol(rest) => rest_param = Some(rest.clone()),
                    _ => return Err(EvalError::Type(exprs[i + 1].span.fmt("expected symbol after dot"))),
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(EvalError::Type(exprs[i].span.fmt("expected parameter name"))),
        }
    }
    Ok((params, rest_param))
}

/// Bind parameters (including rest param) and return the new local env.
fn apply_lambda(
    params: &[String],
    rest_param: &Option<String>,
    _body: &[Expr],
    closure_env: &Env,
    args: &[Value],
    span: Span,
) -> Result<Env, EvalError> {
    if let Some(ref _rest) = rest_param {
        if args.len() < params.len() {
            return Err(EvalError::Arity(span.fmt(&format!(
                "expected at least {} arguments, got {}", params.len(), args.len()
            ))));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(span.fmt(&format!(
            "expected {} arguments, got {}", params.len(), args.len()
        ))));
    }
    let local_env = new_env(Some(closure_env.clone()));
    for (p, a) in params.iter().zip(args.iter()) {
        env_set(&local_env, p.clone(), a.clone());
    }
    if let Some(ref rest) = rest_param {
        let rest_args = args[params.len()..].to_vec();
        env_set(&local_env, rest.clone(), vec_to_pair_list(rest_args));
    }
    Ok(local_env)
}

/// Implement (apply fn arg1 ... argN list)
fn eval_apply(args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("apply requires at least 2 arguments")));
    }
    let func = &args[0];
    // Last arg must be a list; prefix args are prepended
    let last = &args[args.len() - 1];
    let tail = match value_to_vec(last) {
        Some(items) => items,
        None => return Err(EvalError::Type(span.fmt("apply: last argument must be a list"))),
    };
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail);

    match func {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = apply_lambda(params, rest_param, body, env, &all_args, span)?;
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Builtin(ref name) if name == "apply" => {
            eval_apply(&all_args, span, out)
        }
        Value::Builtin(ref name) => {
            eval_builtin(name, &all_args, span)
        }
        _ => {
            Err(EvalError::Type(span.fmt("apply: not a procedure")))
        }
    }
}


fn eval_callcc(proc: Value, span: Span, out: &Output) -> Result<Value, EvalError> {
    let id = NEXT_CONT_ID.with(|c| { let v = c.get(); c.set(v + 1); v });
    let top_idx = TOP_LEVEL_IDX.with(|c| c.get());
    let cont = Value::Continuation { id, top_level_idx: top_idx, callcc_span: span };

    let result = match proc {
        Value::Lambda { ref params, ref rest_param, ref body, ref env } => {
            let local_env = apply_lambda(params, rest_param, body, env, &[cont], span)?;
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Builtin(ref name) => {
            // (call/cc some-builtin) — call the builtin with the continuation
            eval_builtin(name, &[cont], span)
        }
        _ => Err(EvalError::Type(span.fmt("call/cc: expected procedure")))
    };

    match result {
        Ok(val) => Ok(val),
        Err(EvalError::ContinuationInvoked(inv_id, _)) if inv_id == id => {
            let val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take()).unwrap();
            Ok(val)
        }
        Err(e) => Err(e),
    }
}

fn call_thunk(thunk: &Value, span: Span, out: &Output) -> Result<Value, EvalError> {
    match thunk {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = apply_lambda(params, rest_param, body, env, &[], span)?;
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Builtin(name) => eval_builtin(name, &[], span),
        _ => Err(EvalError::Type(span.fmt("dynamic-wind: expected thunk")))
    }
}

fn eval_dynamic_wind(in_thunk: Value, body_thunk: Value, out_thunk: Value, span: Span, out: &Output) -> Result<Value, EvalError> {
    // Run in-thunk
    call_thunk(&in_thunk, span, out)?;

    // Run body-thunk, catching continuation invocations
    let body_result = call_thunk(&body_thunk, span, out);

    // Run out-thunk (always, even on non-local exit)
    match body_result {
        Ok(val) => {
            call_thunk(&out_thunk, span, out)?;
            Ok(val)
        }
        Err(EvalError::ContinuationInvoked(id, top_idx)) => {
            // Save the continuation value, run out-thunk, then re-propagate
            let saved_val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take());
            let saved_span = CONT_CALLCC_SPAN.with(|v| v.borrow_mut().take());
            call_thunk(&out_thunk, span, out)?;
            // Restore continuation state and re-propagate
            CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = saved_val);
            CONT_CALLCC_SPAN.with(|v| *v.borrow_mut() = saved_span);
            Err(EvalError::ContinuationInvoked(id, top_idx))
        }
        Err(EvalError::Raise) => {
            let saved_raised = RAISED_VALUE.with(|v| v.borrow_mut().take());
            call_thunk(&out_thunk, span, out)?;
            RAISED_VALUE.with(|v| *v.borrow_mut() = saved_raised);
            Err(EvalError::Raise)
        }
        Err(e) => {
            Err(e)
        }
    }
}

fn eval_with_exception_handler(handler: Value, thunk: Value, span: Span, out: &Output) -> Result<Value, EvalError> {
    let result = call_thunk(&thunk, span, out);
    match result {
        Ok(val) => Ok(val),
        Err(EvalError::Raise) => {
            let raised = RAISED_VALUE.with(|v| v.borrow_mut().take())
                .unwrap_or(Value::Boolean(false));
            // Call the handler with the raised value
            match handler {
                Value::Lambda { ref params, ref rest_param, ref body, ref env } => {
                    let local_env = apply_lambda(params, rest_param, body, env, &[raised], span)?;
                    let mut result = Value::Boolean(false);
                    for expr in body {
                        result = eval(expr, &local_env, out)?;
                    }
                    Ok(result)
                }
                Value::Builtin(ref name) => eval_builtin(name, &[raised], span),
                _ => Err(EvalError::Type(span.fmt("with-exception-handler: expected procedure"))),
            }
        }
        Err(e) => Err(e),
    }
}

fn eval_guard(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    // (guard (var clause1 clause2 ...) body ...)
    if args.is_empty() {
        return Err(EvalError::Arity(span.fmt("guard requires clauses and body")));
    }
    let clauses_expr = &args[0];
    let body = &args[1..];

    let clause_items = match &clauses_expr.kind {
        ExprKind::List(items) if items.len() >= 2 => items,
        _ => return Err(EvalError::Type(span.fmt("guard: expected (var clause ...)"))),
    };

    let var_name = match &clause_items[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(span.fmt("guard: expected symbol as variable"))),
    };

    let clauses = &clause_items[1..];

    // Evaluate body, catching Raise
    let mut body_result = Ok(Value::Boolean(false));
    for expr in body {
        body_result = eval(expr, env, out);
        if body_result.is_err() {
            break;
        }
    }

    match body_result {
        Ok(val) => Ok(val),
        Err(EvalError::Raise) => {
            let raised = RAISED_VALUE.with(|v| v.borrow_mut().take())
                .unwrap_or(Value::Boolean(false));
            // Bind the raised value to var_name and test clauses
            let guard_env = new_env(Some(env.clone()));
            env_set(&guard_env, var_name, raised.clone());

            for clause in clauses {
                match &clause.kind {
                    ExprKind::List(parts) if !parts.is_empty() => {
                        // Check for else clause
                        if let ExprKind::Symbol(s) = &parts[0].kind {
                            if s == "else" {
                                // Evaluate else body
                                let mut result = Value::Boolean(false);
                                for expr in &parts[1..] {
                                    result = eval(expr, &guard_env, out)?;
                                }
                                return Ok(result);
                            }
                        }
                        // Test the clause condition
                        let test_val = eval(&parts[0], &guard_env, out)?;
                        if test_val.is_truthy() {
                            if parts.len() > 1 {
                                let mut result = Value::Boolean(false);
                                for expr in &parts[1..] {
                                    result = eval(expr, &guard_env, out)?;
                                }
                                return Ok(result);
                            } else {
                                return Ok(test_val);
                            }
                        }
                    }
                    _ => {}
                }
            }
            // No clause matched — re-raise
            RAISED_VALUE.with(|v| *v.borrow_mut() = Some(raised));
            Err(EvalError::Raise)
        }
        Err(e) => Err(e),
    }
}

fn eval_set(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(span.fmt("set! requires 2 arguments")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(span.fmt("set!: expected symbol"))),
    };
    let val = eval(&args[1], env, out)?;
    fn env_update(env: &Env, name: &str, val: Value) -> Result<(), ()> {
        let mut inner = env.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), val);
            Ok(())
        } else if let Some(ref parent) = inner.parent {
            env_update(parent, name, val)
        } else {
            Err(())
        }
    }
    env_update(env, &name, val).map_err(|_| EvalError::UnboundVariable(span.fmt(&name)))?;
    Ok(Value::Boolean(false))
}

fn eval_string_set(_args: &[Expr], _env: &Env, span: Span, _out: &Output) -> Result<Value, EvalError> {
    Err(EvalError::Type(span.fmt("string-set!: strings are immutable")))
}


// ── Macro support (define-syntax / syntax-rules) ──

#[derive(Clone)]
enum MacroBinding {
    Single(Expr),
    List(Vec<Expr>),
}

fn parse_syntax_rules(expr: &Expr, env: &Env, span: Span) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::List(items) => {
            if items.is_empty() || !matches!(&items[0].kind, ExprKind::Symbol(ref s) if s == "syntax-rules") {
                return Err(EvalError::Type(span.fmt("expected syntax-rules")));
            }
            let literals = match &items[1].kind {
                ExprKind::List(lits) => {
                    lits.iter().map(|l| match &l.kind {
                        ExprKind::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Type(l.span.fmt("expected symbol in literals"))),
                    }).collect::<Result<Vec<_>, _>>()?
                }
                _ => return Err(EvalError::Type(span.fmt("syntax-rules: expected literals list"))),
            };
            let mut rules = Vec::new();
            for rule in &items[2..] {
                match &rule.kind {
                    ExprKind::List(parts) if parts.len() == 2 => {
                        rules.push((parts[0].clone(), parts[1].clone()));
                    }
                    _ => return Err(EvalError::Type(rule.span.fmt("syntax-rules: bad rule"))),
                }
            }
            Ok(Value::SyntaxRules { literals, rules, env: env.clone() })
        }
        _ => Err(EvalError::Type(span.fmt("expected syntax-rules"))),
    }
}

fn expand_syntax_rules(
    input: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    call_env: &Env,
    span: Span,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if match_pattern(pattern, input, literals, &mut bindings) {
            return expand_template(template, &bindings, def_env, call_env, span);
        }
    }
    Err(EvalError::Type(span.fmt("no matching syntax-rules pattern")))
}

fn match_pattern(
    pattern: &Expr,
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::List(pat_items) if !pat_items.is_empty() => {
            // pat_items[0] is the macro name — skip it
            match_pattern_elements(&pat_items[1..], &input[1..], literals, bindings)
        }
        _ => false,
    }
}

fn match_pattern_elements(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let mut i = 0;
    let mut j = 0;
    while i < patterns.len() {
        let has_ellipsis = i + 1 < patterns.len()
            && matches!(&patterns[i + 1].kind, ExprKind::Symbol(ref s) if s == "...");
        if has_ellipsis {
            let remaining_fixed = count_fixed_after(patterns, i + 2);
            let available = if inputs.len() >= j + remaining_fixed {
                inputs.len() - j - remaining_fixed
            } else {
                return false;
            };
            match &patterns[i].kind {
                ExprKind::Symbol(s) if !literals.contains(s) && s != "_" => {
                    let matched: Vec<Expr> = inputs[j..j + available].to_vec();
                    bindings.insert(s.clone(), MacroBinding::List(matched));
                    j += available;
                }
                _ => return false,
            }
            i += 2;
        } else {
            if j >= inputs.len() {
                return false;
            }
            if !match_single_pattern(&patterns[i], &inputs[j], literals, bindings) {
                return false;
            }
            i += 1;
            j += 1;
        }
    }
    j == inputs.len()
}

fn count_fixed_after(patterns: &[Expr], start: usize) -> usize {
    let mut count = 0;
    let mut i = start;
    while i < patterns.len() {
        if i + 1 < patterns.len() && matches!(&patterns[i + 1].kind, ExprKind::Symbol(ref s) if s == "...") {
            i += 2;
        } else {
            count += 1;
            i += 1;
        }
    }
    count
}

fn match_single_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&input.kind, ExprKind::Symbol(ref t) if t == s)
        }
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        ExprKind::List(pat_items) => {
            match &input.kind {
                ExprKind::List(inp_items) => {
                    match_pattern_elements(pat_items, inp_items, literals, bindings)
                }
                _ => false,
            }
        }
        ExprKind::Integer(n) => matches!(&input.kind, ExprKind::Integer(m) if m == n),
        ExprKind::Boolean(b) => matches!(&input.kind, ExprKind::Boolean(ref c) if c == b),
        ExprKind::String(s) => matches!(&input.kind, ExprKind::String(ref t) if t == s),
        _ => false,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    def_env: &Env,
    call_env: &Env,
    span: Span,
) -> Result<Expr, EvalError> {
    // Step 1: Find introduced bindings (let/lambda bound vars) and gensym them
    let mut introduced = HashMap::new();
    find_introduced_bindings(template, bindings, &mut introduced);

    // Step 2: Find free variables and inject definition-site values into call_env
    let mut free_vars = HashMap::new();
    find_free_vars(template, bindings, &introduced, &mut free_vars, def_env);
    for (name, gname) in &free_vars {
        if let Some(val) = env_get(def_env, name) {
            env_set(call_env, gname.clone(), val);
        }
    }

    // Merge renames
    let mut renames = introduced;
    renames.extend(free_vars);

    // Step 3: Expand template with substitutions
    do_expand(template, bindings, &renames, span)
}

fn find_introduced_bindings(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    introduced: &mut HashMap<String, String>,
) {
    if let ExprKind::List(items) = &template.kind {
        if items.len() >= 2 {
            if let ExprKind::Symbol(ref op) = items[0].kind {
                if op == "let" && items.len() >= 3 {
                    if let ExprKind::List(ref bind_list) = items[1].kind {
                        for bind in bind_list {
                            if let ExprKind::List(ref pair) = bind.kind {
                                if !pair.is_empty() {
                                    if let ExprKind::Symbol(ref name) = pair[0].kind {
                                        if !bindings.contains_key(name) && !introduced.contains_key(name) {
                                            introduced.insert(name.clone(), gensym(name));
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if op == "lambda" && items.len() >= 3 {
                    if let ExprKind::List(ref params) = items[1].kind {
                        for p in params {
                            if let ExprKind::Symbol(ref name) = p.kind {
                                if !bindings.contains_key(name) && !introduced.contains_key(name) {
                                    introduced.insert(name.clone(), gensym(name));
                                }
                            }
                        }
                    }
                }
            }
        }
        for item in items {
            find_introduced_bindings(item, bindings, introduced);
        }
    }
}

fn is_special_form(s: &str) -> bool {
    matches!(s, "if" | "begin" | "set!" | "let" | "lambda" | "define" | "quote"
        | "cond" | "and" | "or" | "call/cc" | "call-with-current-continuation"
        | "display" | "write" | "newline" | "define-syntax" | "syntax-rules"
        | "string-set!" | "dynamic-wind" | "guard" | "define-record-type")
}

fn find_free_vars(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    introduced: &HashMap<String, String>,
    free_vars: &mut HashMap<String, String>,
    def_env: &Env,
) {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if !bindings.contains_key(s)
                && !introduced.contains_key(s)
                && !free_vars.contains_key(s)
                && !is_special_form(s)
                && s != "..."
                && env_get(def_env, s).is_some()
            {
                free_vars.insert(s.clone(), gensym(s));
            }
        }
        ExprKind::List(items) => {
            for item in items {
                find_free_vars(item, bindings, introduced, free_vars, def_env);
            }
        }
        _ => {}
    }
}

fn do_expand(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
    span: Span,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(expr) => Ok(expr.clone()),
                    MacroBinding::List(_) => {
                        Err(EvalError::Type(span.fmt("pattern variable used without ellipsis")))
                    }
                }
            } else if let Some(renamed) = renames.get(s) {
                Ok(Expr { kind: ExprKind::Symbol(renamed.clone()), span: template.span })
            } else {
                Ok(template.clone())
            }
        }
        ExprKind::List(items) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() && matches!(&items[i + 1].kind, ExprKind::Symbol(ref s) if s == "...") {
                    // Expand with ellipsis — splice list binding
                    if let ExprKind::Symbol(ref s) = items[i].kind {
                        if let Some(MacroBinding::List(exprs)) = bindings.get(s) {
                            result.extend(exprs.iter().cloned());
                        }
                        // Single binding or unknown: produces nothing
                    }
                    i += 2;
                } else {
                    let expanded = do_expand(&items[i], bindings, renames, span)?;
                    result.push(expanded);
                    i += 1;
                }
            }
            Ok(Expr { kind: ExprKind::List(result), span: template.span })
        }
        _ => Ok(template.clone()),
    }
}

// ── syntax-case support ──

fn value_to_expr(val: &Value, span: Span) -> Expr {
    match val {
        Value::Integer(n) => Expr { kind: ExprKind::Integer(*n), span },
        Value::Float(f) => Expr { kind: ExprKind::Float(*f), span },
        Value::Rational(n, d) => Expr { kind: ExprKind::Rational(*n, *d), span },
        Value::Boolean(b) => Expr { kind: ExprKind::Boolean(*b), span },
        Value::Char(c) => Expr { kind: ExprKind::Char(*c), span },
        Value::String(s) => Expr { kind: ExprKind::String(s.clone()), span },
        Value::Symbol(s) => Expr { kind: ExprKind::Symbol(s.clone()), span },
        Value::Syntax(e) => (**e).clone(),
        Value::List(items) => {
            let exprs: Vec<Expr> = items.iter().map(|v| value_to_expr(v, span)).collect();
            Expr { kind: ExprKind::List(exprs), span }
        }
        _ => Expr { kind: ExprKind::Symbol(val.display()), span },
    }
}

fn exprs_to_syntax(items: &[Expr], span: Span) -> Value {
    Value::Syntax(Box::new(Expr { kind: ExprKind::List(items.to_vec()), span }))
}

fn expand_macro_transformer(
    transformer: &Value,
    input: &[Expr],
    call_env: &Env,
    span: Span,
    out: &Output,
) -> Result<Expr, EvalError> {
    // Create syntax object from the input form
    let stx = exprs_to_syntax(input, span);

    // Extract the def_env from the lambda
    let def_env = match transformer {
        Value::Lambda { env, .. } => env.clone(),
        _ => call_env.clone(),
    };

    // Save environments for syntax template expansion
    let old_def = SYNTAX_DEF_ENV.with(|e| e.borrow_mut().replace(def_env));
    let old_call = SYNTAX_CALL_ENV.with(|e| e.borrow_mut().replace(call_env.clone()));

    // Call the transformer lambda with the syntax object
    let result = call_value(transformer.clone(), &[stx], span, out);

    // Restore environments
    SYNTAX_DEF_ENV.with(|e| *e.borrow_mut() = old_def);
    SYNTAX_CALL_ENV.with(|e| *e.borrow_mut() = old_call);

    let result = result?;

    // Extract the Expr from the resulting Syntax value
    match result {
        Value::Syntax(expr) => Ok(*expr),
        _ => Err(EvalError::Type(span.fmt("macro transformer must return a syntax object"))),
    }
}

fn call_value(func: Value, args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { ref params, ref rest_param, ref body, ref env } => {
            let local_env = apply_lambda(params, rest_param, body, env, args, span)?;
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Builtin(ref name) => eval_builtin(name, args, span),
        _ => Err(EvalError::Type(span.fmt("expected procedure"))),
    }
}

fn eval_syntax_case(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    // (syntax-case stx (literals...) clause ...)
    // clause = (pattern template) or (pattern fender template)
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("syntax-case requires at least stx and literals")));
    }

    let stx_val = eval(&args[0], env, out)?;
    let stx_expr = match &stx_val {
        Value::Syntax(e) => (**e).clone(),
        _ => return Err(EvalError::Type(span.fmt("syntax-case: expected syntax object"))),
    };

    let literals = match &args[1].kind {
        ExprKind::List(lits) => {
            lits.iter().map(|l| match &l.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(l.span.fmt("expected symbol in literals"))),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type(span.fmt("syntax-case: expected literals list"))),
    };

    // The stx_expr should be a list for pattern matching
    let input_items = match &stx_expr.kind {
        ExprKind::List(items) => items.clone(),
        _ => vec![stx_expr.clone()],
    };

    for clause in &args[2..] {
        let clause_items = match &clause.kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type(clause.span.fmt("syntax-case: bad clause"))),
        };

        if clause_items.len() < 2 || clause_items.len() > 3 {
            return Err(EvalError::Type(clause.span.fmt("syntax-case: clause needs 2 or 3 elements")));
        }

        let pattern = &clause_items[0];
        let mut bindings = HashMap::new();

        if match_pattern(pattern, &input_items, &literals, &mut bindings) {
            // If 3 elements, middle is fender (guard)
            let (has_fender, template_idx) = if clause_items.len() == 3 { (true, 2) } else { (false, 1) };

            if has_fender {
                // Push bindings for fender evaluation
                SYNTAX_BINDINGS.with(|b| b.borrow_mut().push(bindings.clone()));
                let fender_result = eval(&clause_items[1], env, out);
                SYNTAX_BINDINGS.with(|b| b.borrow_mut().pop());
                match fender_result {
                    Ok(val) if !val.is_truthy() => continue, // fender failed, try next clause
                    Err(e) => return Err(e),
                    _ => {}
                }
            }

            // Push bindings for template evaluation
            SYNTAX_BINDINGS.with(|b| b.borrow_mut().push(bindings));

            let result = eval(&clause_items[template_idx], env, out);

            SYNTAX_BINDINGS.with(|b| b.borrow_mut().pop());

            return result;
        }
    }

    Err(EvalError::Type(span.fmt("no matching syntax-case pattern")))
}

fn eval_syntax_template(template: &Expr, span: Span) -> Result<Value, EvalError> {
    // Collect all bindings from the stack
    let bindings = SYNTAX_BINDINGS.with(|b| {
        let stack = b.borrow();
        let mut result = HashMap::new();
        for frame in stack.iter() {
            for (k, v) in frame {
                result.insert(k.clone(), v.clone());
            }
        }
        result
    });

    let def_env = SYNTAX_DEF_ENV.with(|e| e.borrow().clone());
    let call_env = SYNTAX_CALL_ENV.with(|e| e.borrow().clone());

    let expanded = if let (Some(ref de), Some(ref ce)) = (&def_env, &call_env) {
        expand_template(template, &bindings, de, ce, span)?
    } else {
        // No env context - just do simple substitution
        do_expand(template, &bindings, &HashMap::new(), span)?
    };

    Ok(Value::Syntax(Box::new(expanded)))
}

fn eval_with_syntax(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    // (with-syntax ((pattern expr) ...) body ...)
    if args.is_empty() {
        return Err(EvalError::Arity(span.fmt("with-syntax requires bindings and body")));
    }

    let binding_list = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(span.fmt("with-syntax: expected binding list"))),
    };

    let mut bindings = HashMap::new();
    for bind in binding_list {
        let pair = match &bind.kind {
            ExprKind::List(items) if items.len() == 2 => items,
            _ => return Err(EvalError::Type(bind.span.fmt("with-syntax: bad binding"))),
        };
        let val = eval(&pair[1], env, out)?;
        let expr = match &val {
            Value::Syntax(e) => (**e).clone(),
            _ => value_to_expr(&val, span),
        };
        match &pair[0].kind {
            ExprKind::Symbol(name) => {
                bindings.insert(name.clone(), MacroBinding::Single(expr));
            }
            _ => return Err(EvalError::Type(pair[0].span.fmt("with-syntax: expected symbol pattern"))),
        }
    }

    // Push bindings
    SYNTAX_BINDINGS.with(|b| b.borrow_mut().push(bindings));

    let mut result = Value::Boolean(false);
    for body_expr in &args[1..] {
        result = eval(body_expr, env, out)?;
    }

    SYNTAX_BINDINGS.with(|b| b.borrow_mut().pop());

    Ok(result)
}

fn as_int(v: &Value, span: Span) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(span.fmt("expected integer"))),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(xs), Value::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Pair(c1), Value::Pair(c2)) => {
            let b1 = c1.borrow();
            let b2 = c2.borrow();
            values_equal(&b1.0, &b2.0) && values_equal(&b1.1, &b2.1)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => false,
    }
}

fn call_func(func: &Value, args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = apply_lambda(params, rest_param, body, env, args, span)?;
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Builtin(ref name) if name == "call-with-values" => {
            eval_call_with_values(args, span, out)
        }
        Value::Builtin(ref name) if name == "values" => {
            if args.len() == 1 {
                Ok(args[0].clone())
            } else {
                Ok(Value::Values(args.to_vec()))
            }
        }
        Value::Builtin(ref name) => eval_builtin(name, args, span),
        _ => Err(EvalError::Type(span.fmt("not a procedure"))),
    }
}

fn eval_map(args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("map requires at least 2 arguments")));
    }
    let func = &args[0];
    let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
        value_to_vec(a).ok_or_else(|| EvalError::Type(span.fmt("map: expected list")))
    }).collect::<Result<_, _>>()?;
    let len = lists[0].len();
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        result.push(call_func(func, &call_args, span, out)?);
    }
    Ok(vec_to_pair_list(result))
}

fn eval_for_each(args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("for-each requires at least 2 arguments")));
    }
    let func = &args[0];
    let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
        value_to_vec(a).ok_or_else(|| EvalError::Type(span.fmt("for-each: expected list")))
    }).collect::<Result<_, _>>()?;
    let len = lists[0].len();
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        call_func(func, &call_args, span, out)?;
    }
    Ok(Value::Boolean(false))
}

fn eval_call_with_values(args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(span.fmt("call-with-values requires 2 arguments")));
    }
    let producer = &args[0];
    let consumer = &args[1];
    let produced = call_func(producer, &[], span, out)?;
    let call_args = match produced {
        Value::Values(vals) => vals,
        other => vec![other],
    };
    call_func(consumer, &call_args, span, out)
}

/// Add two exact numbers (integer or rational), returning exact result
fn num_add(a: &Value, b: &Value, span: Span) -> Result<Value, EvalError> {
    if any_inexact(&[a.clone(), b.clone()]) {
        return Ok(Value::Float(val_to_f64(a, span)? + val_to_f64(b, span)?));
    }
    let (an, ad) = to_rational(a, span)?;
    let (bn, bd) = to_rational(b, span)?;
    Ok(make_rational(an * bd + bn * ad, ad * bd))
}

fn num_sub(a: &Value, b: &Value, span: Span) -> Result<Value, EvalError> {
    if any_inexact(&[a.clone(), b.clone()]) {
        return Ok(Value::Float(val_to_f64(a, span)? - val_to_f64(b, span)?));
    }
    let (an, ad) = to_rational(a, span)?;
    let (bn, bd) = to_rational(b, span)?;
    Ok(make_rational(an * bd - bn * ad, ad * bd))
}

fn num_mul(a: &Value, b: &Value, span: Span) -> Result<Value, EvalError> {
    if any_inexact(&[a.clone(), b.clone()]) {
        return Ok(Value::Float(val_to_f64(a, span)? * val_to_f64(b, span)?));
    }
    let (an, ad) = to_rational(a, span)?;
    let (bn, bd) = to_rational(b, span)?;
    Ok(make_rational(an * bn, ad * bd))
}

fn num_div(a: &Value, b: &Value, span: Span) -> Result<Value, EvalError> {
    if any_inexact(&[a.clone(), b.clone()]) {
        let d = val_to_f64(b, span)?;
        if d == 0.0 { return Err(EvalError::Type(span.fmt("division by zero"))); }
        return Ok(Value::Float(val_to_f64(a, span)? / d));
    }
    let (an, ad) = to_rational(a, span)?;
    let (bn, bd) = to_rational(b, span)?;
    if bn == 0 { return Err(EvalError::Type(span.fmt("division by zero"))); }
    Ok(make_rational(an * bd, ad * bn))
}

fn to_rational(v: &Value, span: Span) -> Result<(i64, i64), EvalError> {
    match v {
        Value::Integer(n) => Ok((*n, 1)),
        Value::Rational(n, d) => Ok((*n, *d)),
        _ => Err(EvalError::Type(span.fmt("expected number"))),
    }
}

fn num_cmp(a: &Value, b: &Value, span: Span) -> Result<std::cmp::Ordering, EvalError> {
    // If either is inexact, compare as f64
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        let fa = val_to_f64(a, span)?;
        let fb = val_to_f64(b, span)?;
        return fa.partial_cmp(&fb).ok_or_else(|| EvalError::Type(span.fmt("cannot compare NaN")));
    }
    let (an, ad) = to_rational(a, span)?;
    let (bn, bd) = to_rational(b, span)?;
    // Compare an/ad vs bn/bd => an*bd vs bn*ad
    Ok((an * bd).cmp(&(bn * ad)))
}

fn num_negate(v: &Value, span: Span) -> Result<Value, EvalError> {
    match v {
        Value::Integer(n) => Ok(Value::Integer(-n)),
        Value::Float(f) => Ok(Value::Float(-f)),
        Value::Rational(n, d) => Ok(Value::Rational(-n, *d)),
        _ => Err(EvalError::Type(span.fmt("expected number"))),
    }
}

fn eval_builtin(op: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    // Handle record-type builtins
    if let Some(rest) = op.strip_prefix("%%record-ctor%%") {
        // Format: type_id%%type_name%%idx1,idx2,...
        let parts: Vec<&str> = rest.splitn(3, "%%").collect();
        let type_id: u64 = parts[0].parse().unwrap();
        let type_name = parts[1].to_string();
        let indices: Vec<usize> = if parts[2].is_empty() {
            vec![]
        } else {
            parts[2].split(',').map(|s| s.parse().unwrap()).collect()
        };
        if args.len() != indices.len() {
            return Err(EvalError::Arity(span.fmt(&format!("{} requires {} arguments", type_name, indices.len()))));
        }
        // Find total field count (max index + 1, or 0 if no fields)
        let n_fields = indices.iter().copied().max().map_or(0, |m| m + 1);
        let mut fields = vec![Value::Boolean(false); n_fields];
        for (arg_idx, &field_idx) in indices.iter().enumerate() {
            fields[field_idx] = args[arg_idx].clone();
        }
        return Ok(Value::Record { type_id, type_name, fields });
    }
    if let Some(rest) = op.strip_prefix("%%record-pred%%") {
        let type_id: u64 = rest.parse().unwrap();
        if args.len() != 1 {
            return Err(EvalError::Arity(span.fmt("record predicate requires 1 argument")));
        }
        return Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id)));
    }
    if let Some(rest) = op.strip_prefix("%%record-acc%%") {
        let parts: Vec<&str> = rest.splitn(2, "%%").collect();
        let type_id: u64 = parts[0].parse().unwrap();
        let field_idx: usize = parts[1].parse().unwrap();
        if args.len() != 1 {
            return Err(EvalError::Arity(span.fmt("record accessor requires 1 argument")));
        }
        match &args[0] {
            Value::Record { type_id: tid, fields, .. } if *tid == type_id => {
                return Ok(fields[field_idx].clone());
            }
            _ => return Err(EvalError::Type(span.fmt("record accessor: wrong record type"))),
        }
    }
    match op {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(span.fmt("cons requires 2 arguments")));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("car requires 1 argument"))); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(c) => Ok(c.borrow().0.clone()),
                _ => Err(EvalError::Type(span.fmt("car: expected pair"))),
            }
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("cdr requires 1 argument"))); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(vec_to_pair_list(items[1..].to_vec())),
                Value::Pair(c) => Ok(c.borrow().1.clone()),
                _ => Err(EvalError::Type(span.fmt("cdr: expected pair"))),
            }
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("null? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(vec_to_pair_list(args.to_vec())),
        "length" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("length requires 1 argument"))); }
            match value_to_vec(&args[0]) {
                Some(items) => Ok(Value::Integer(items.len() as i64)),
                None => Err(EvalError::Type(span.fmt("length: expected list"))),
            }
        }
        "cadr" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("cadr requires 1 argument"))); }
            let cdr_val = match &args[0] {
                Value::Pair(c) => c.borrow().1.clone(),
                Value::List(items) if !items.is_empty() => vec_to_pair_list(items[1..].to_vec()),
                _ => return Err(EvalError::Type(span.fmt("cadr: expected pair"))),
            };
            match &cdr_val {
                Value::Pair(c) => Ok(c.borrow().0.clone()),
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type(span.fmt("cadr: expected pair"))),
            }
        }
        "cddr" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("cddr requires 1 argument"))); }
            let cdr_val = match &args[0] {
                Value::Pair(c) => c.borrow().1.clone(),
                Value::List(items) if !items.is_empty() => vec_to_pair_list(items[1..].to_vec()),
                _ => return Err(EvalError::Type(span.fmt("cddr: expected pair"))),
            };
            match &cdr_val {
                Value::Pair(c) => Ok(c.borrow().1.clone()),
                Value::List(items) if !items.is_empty() => Ok(vec_to_pair_list(items[1..].to_vec())),
                _ => Err(EvalError::Type(span.fmt("cddr: expected pair"))),
            }
        }
        "caar" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("caar requires 1 argument"))); }
            let car_val = match &args[0] {
                Value::Pair(c) => c.borrow().0.clone(),
                Value::List(items) if !items.is_empty() => items[0].clone(),
                _ => return Err(EvalError::Type(span.fmt("caar: expected pair"))),
            };
            match &car_val {
                Value::Pair(c) => Ok(c.borrow().0.clone()),
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type(span.fmt("caar: expected pair"))),
            }
        }
        "cdar" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("cdar requires 1 argument"))); }
            let car_val = match &args[0] {
                Value::Pair(c) => c.borrow().0.clone(),
                Value::List(items) if !items.is_empty() => items[0].clone(),
                _ => return Err(EvalError::Type(span.fmt("cdar: expected pair"))),
            };
            match &car_val {
                Value::Pair(c) => Ok(c.borrow().1.clone()),
                Value::List(items) if !items.is_empty() => Ok(vec_to_pair_list(items[1..].to_vec())),
                _ => Err(EvalError::Type(span.fmt("cdar: expected pair"))),
            }
        }
        "set-car!" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("set-car! requires 2 arguments"))); }
            match &args[0] {
                Value::Pair(c) => { c.borrow_mut().0 = args[1].clone(); Ok(Value::Boolean(false)) }
                _ => Err(EvalError::Type(span.fmt("set-car!: expected pair"))),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("set-cdr! requires 2 arguments"))); }
            match &args[0] {
                Value::Pair(c) => { c.borrow_mut().1 = args[1].clone(); Ok(Value::Boolean(false)) }
                _ => Err(EvalError::Type(span.fmt("set-cdr!: expected pair"))),
            }
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::String(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("number? requires 1 argument"))); }
            Ok(Value::Boolean(is_number(&args[0])))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("boolean? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("pair? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty()) || matches!(&args[0], Value::Pair(_))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("symbol? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "+" => {
            let mut result: Value = Value::Integer(0);
            for a in args { result = num_add(&result, a, span)?; }
            Ok(result)
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(span.fmt("- requires at least 1 argument")));
            }
            if args.len() == 1 {
                num_negate(&args[0], span)
            } else {
                let mut result = args[0].clone();
                for a in &args[1..] { result = num_sub(&result, a, span)?; }
                Ok(result)
            }
        }
        "*" => {
            let mut result: Value = Value::Integer(1);
            for a in args { result = num_mul(&result, a, span)?; }
            Ok(result)
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(span.fmt("/ requires at least 2 arguments")));
            }
            let mut result = args[0].clone();
            for a in &args[1..] { result = num_div(&result, a, span)?; }
            Ok(result)
        }
        "<" => cmp_vals_num(args, |o| o == std::cmp::Ordering::Less, span),
        ">" => cmp_vals_num(args, |o| o == std::cmp::Ordering::Greater, span),
        "=" => cmp_vals_num(args, |o| o == std::cmp::Ordering::Equal, span),
        "<=" => cmp_vals_num(args, |o| o != std::cmp::Ordering::Greater, span),
        ">=" => cmp_vals_num(args, |o| o != std::cmp::Ordering::Less, span),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(span.fmt("not requires 1 argument")));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("exact? requires 1 argument"))); }
            Ok(Value::Boolean(is_exact(&args[0])))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("inexact? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("integer? requires 1 argument"))); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Boolean(true)),
                Value::Float(f) => Ok(Value::Boolean(f.fract() == 0.0)),
                Value::Rational(n, d) => Ok(Value::Boolean(n % d == 0)),
                _ => Ok(Value::Boolean(false)),
            }
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("rational? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("exact->inexact requires 1 argument"))); }
            Ok(Value::Float(val_to_f64(&args[0], span)?))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("inexact->exact requires 1 argument"))); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, d) => Ok(Value::Rational(*n, *d)),
                Value::Float(f) => {
                    // Convert f64 to exact rational using continued fraction approach
                    // For simple cases like 0.5 -> 1/2
                    let denom = 1_000_000_000_i64;
                    let numer = (*f * denom as f64).round() as i64;
                    Ok(make_rational(numer, denom))
                }
                _ => Err(EvalError::Type(span.fmt("inexact->exact: expected number"))),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("numerator requires 1 argument"))); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type(span.fmt("numerator: expected rational"))),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("denominator requires 1 argument"))); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type(span.fmt("denominator: expected rational"))),
            }
        }
        "string-append" => {
            let mut result = std::string::String::new();
            for a in args {
                match a {
                    Value::String(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(span.fmt("string-append: expected string"))),
                }
            }
            Ok(Value::String(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string-length requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(span.fmt("string-length: expected string"))),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity(span.fmt("substring requires 3 arguments"))); }
            let s = match &args[0] {
                Value::String(s) => s,
                _ => return Err(EvalError::Type(span.fmt("substring: expected string"))),
            };
            let start = as_int(&args[1], span)? as usize;
            let end = as_int(&args[2], span)? as usize;
            Ok(Value::String(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string->number requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => {
                    match s.parse::<i64>() {
                        Ok(n) => Ok(Value::Integer(n)),
                        Err(_) => Ok(Value::Boolean(false)),
                    }
                }
                _ => Err(EvalError::Type(span.fmt("string->number: expected string"))),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("number->string requires 1 argument"))); }
            Ok(Value::String(args[0].display()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("symbol->string requires 1 argument"))); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::String(s.clone())),
                _ => Err(EvalError::Type(span.fmt("symbol->string: expected symbol"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string->symbol requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(span.fmt("string->symbol: expected string"))),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string-copy requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.clone())),
                _ => Err(EvalError::Type(span.fmt("string-copy: expected string"))),
            }
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string->list requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(vec_to_pair_list(s.chars().map(Value::Char).collect())),
                _ => Err(EvalError::Type(span.fmt("string->list: expected string"))),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("list->string requires 1 argument"))); }
            let items = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(span.fmt("list->string: expected list")))?;
            let mut s = String::new();
            for item in &items {
                match item {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type(span.fmt("list->string: expected list of chars"))),
                }
            }
            Ok(Value::String(s))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char->integer requires 1 argument"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type(span.fmt("char->integer: expected char"))),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("integer->char requires 1 argument"))); }
            match &args[0] {
                Value::Integer(n) => {
                    let c = char::from_u32(*n as u32).ok_or_else(|| EvalError::Type(span.fmt("integer->char: invalid code point")))?;
                    Ok(Value::Char(c))
                }
                _ => Err(EvalError::Type(span.fmt("integer->char: expected integer"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("string-ref requires 2 arguments"))); }
            let s = match &args[0] {
                Value::String(s) => s,
                _ => return Err(EvalError::Type(span.fmt("string-ref: expected string"))),
            };
            let idx = as_int(&args[1], span)? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| EvalError::Type(span.fmt("string-ref: index out of bounds")))?))
        }
        "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("eq? requires 2 arguments"))); }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("eqv? requires 2 arguments"))); }
            Ok(Value::Boolean(eqv_match(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("equal? requires 2 arguments"))); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "reverse" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("reverse requires 1 argument"))); }
            let items = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(span.fmt("reverse: expected list")))?;
            let mut reversed = items;
            reversed.reverse();
            Ok(vec_to_pair_list(reversed))
        }
        "append" => {
            if args.is_empty() { return Ok(Value::List(vec![])); }
            let mut result = Vec::new();
            for (i, a) in args.iter().enumerate() {
                if i == args.len() - 1 {
                    match value_to_vec(a) {
                        Some(items) => result.extend(items),
                        None => {
                            // improper last arg: build a dotted list
                            let tail = a.clone();
                            let mut r = tail;
                            for item in result.into_iter().rev() {
                                r = make_pair(item, r);
                            }
                            return Ok(r);
                        }
                    }
                } else {
                    let items = value_to_vec(a).ok_or_else(|| EvalError::Type(span.fmt("append: expected list")))?;
                    result.extend(items);
                }
            }
            Ok(vec_to_pair_list(result))
        }
        // Numeric utilities
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("abs requires 1 argument"))); }
            Ok(Value::Integer(as_int(&args[0], span)?.abs()))
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("modulo requires 2 arguments"))); }
            let a = as_int(&args[0], span)?;
            let b = as_int(&args[1], span)?;
            if b == 0 { return Err(EvalError::Type(span.fmt("modulo: division by zero"))); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("remainder requires 2 arguments"))); }
            let a = as_int(&args[0], span)?;
            let b = as_int(&args[1], span)?;
            if b == 0 { return Err(EvalError::Type(span.fmt("remainder: division by zero"))); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("quotient requires 2 arguments"))); }
            let a = as_int(&args[0], span)?;
            let b = as_int(&args[1], span)?;
            if b == 0 { return Err(EvalError::Type(span.fmt("quotient: division by zero"))); }
            Ok(Value::Integer(a / b))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("expt requires 2 arguments"))); }
            let base = as_int(&args[0], span)?;
            let exp = as_int(&args[1], span)?;
            if exp < 0 { return Err(EvalError::Type(span.fmt("expt: negative exponent"))); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity(span.fmt("min requires at least 1 argument"))); }
            let mut result = as_int(&args[0], span)?;
            for a in &args[1..] { result = result.min(as_int(a, span)?); }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity(span.fmt("max requires at least 1 argument"))); }
            let mut result = as_int(&args[0], span)?;
            for a in &args[1..] { result = result.max(as_int(a, span)?); }
            Ok(Value::Integer(result))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("zero? requires 1 argument"))); }
            let r = num_cmp(&args[0], &Value::Integer(0), span)?;
            Ok(Value::Boolean(r == std::cmp::Ordering::Equal))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("positive? requires 1 argument"))); }
            let r = num_cmp(&args[0], &Value::Integer(0), span)?;
            Ok(Value::Boolean(r == std::cmp::Ordering::Greater))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("negative? requires 1 argument"))); }
            let r = num_cmp(&args[0], &Value::Integer(0), span)?;
            Ok(Value::Boolean(r == std::cmp::Ordering::Less))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("odd? requires 1 argument"))); }
            Ok(Value::Boolean(as_int(&args[0], span)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("even? requires 1 argument"))); }
            Ok(Value::Boolean(as_int(&args[0], span)? % 2 == 0))
        }
        // List utilities
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("list-ref requires 2 arguments"))); }
            let items = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(span.fmt("list-ref: expected list")))?;
            let idx = as_int(&args[1], span)? as usize;
            items.get(idx).cloned().ok_or_else(|| EvalError::Type(span.fmt("list-ref: index out of bounds")))
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("list-tail requires 2 arguments"))); }
            let items = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(span.fmt("list-tail: expected list")))?;
            let idx = as_int(&args[1], span)? as usize;
            Ok(vec_to_pair_list(items[idx..].to_vec()))
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("list? requires 1 argument"))); }
            // Use tortoise-and-hare cycle detection
            let result = match &args[0] {
                Value::List(_) => true,
                Value::Pair(_) => {
                    let mut slow = args[0].clone();
                    let mut fast = args[0].clone();
                    let mut is_list = true;
                    loop {
                        // Advance fast by 2
                        for _ in 0..2 {
                            let next = match &fast {
                                Value::Pair(c) => Some(c.borrow().1.clone()),
                                Value::List(items) if items.is_empty() => { break; }
                                _ => { is_list = false; break; }
                            };
                            match next {
                                Some(n) => fast = n,
                                None => break,
                            }
                        }
                        if !is_list { break; }
                        if matches!(&fast, Value::List(items) if items.is_empty()) { break; }
                        // Advance slow by 1
                        let next_slow = match &slow {
                            Value::Pair(c) => c.borrow().1.clone(),
                            _ => break,
                        };
                        slow = next_slow;
                        // Check if slow == fast (cycle)
                        if let (Value::Pair(a), Value::Pair(b)) = (&slow, &fast) {
                            if Rc::ptr_eq(a, b) { is_list = false; break; }
                        }
                    }
                    is_list
                }
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("assoc requires 2 arguments"))); }
            let key = &args[0];
            let alist = value_to_vec(&args[1]).ok_or_else(|| EvalError::Type(span.fmt("assoc: expected list")))?;
            for item in &alist {
                if let Some(pair) = value_to_vec(item) {
                    if !pair.is_empty() && values_equal(&pair[0], key) {
                        return Ok(item.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        // Character functions
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char-alphabetic? requires 1 argument"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(span.fmt("char-alphabetic?: expected char"))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char-numeric? requires 1 argument"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(span.fmt("char-numeric?: expected char"))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char-upcase requires 1 argument"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type(span.fmt("char-upcase: expected char"))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char-downcase requires 1 argument"))); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type(span.fmt("char-downcase: expected char"))),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("char=? requires 2 arguments"))); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(span.fmt("char=?: expected chars"))),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("char<? requires 2 arguments"))); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(span.fmt("char<?: expected chars"))),
            }
        }
        // String functions
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("string=? requires 2 arguments"))); }
            match (&args[0], &args[1]) {
                (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(span.fmt("string=?: expected strings"))),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("string<? requires 2 arguments"))); }
            match (&args[0], &args[1]) {
                (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(span.fmt("string<?: expected strings"))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("string-ci=? requires 2 arguments"))); }
            match (&args[0], &args[1]) {
                (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type(span.fmt("string-ci=?: expected strings"))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string-upcase requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_uppercase())),
                _ => Err(EvalError::Type(span.fmt("string-upcase: expected string"))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string-downcase requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_lowercase())),
                _ => Err(EvalError::Type(span.fmt("string-downcase: expected string"))),
            }
        }
        // Vector operations
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity(span.fmt("make-vector requires 1 or 2 arguments"))); }
            let len = as_int(&args[0], span)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("vector-ref requires 2 arguments"))); }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = as_int(&args[1], span)? as usize;
                    let v = v.borrow();
                    v.get(idx).cloned().ok_or_else(|| EvalError::Type(span.fmt("vector-ref: index out of bounds")))
                }
                _ => Err(EvalError::Type(span.fmt("vector-ref: expected vector"))),
            }
        }
        "vector-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity(span.fmt("vector-set! requires 3 arguments"))); }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = as_int(&args[1], span)? as usize;
                    let mut v = v.borrow_mut();
                    if idx >= v.len() { return Err(EvalError::Type(span.fmt("vector-set!: index out of bounds"))); }
                    v[idx] = args[2].clone();
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type(span.fmt("vector-set!: expected vector"))),
            }
        }
        "vector-length" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("vector-length requires 1 argument"))); }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type(span.fmt("vector-length: expected vector"))),
            }
        }
        "vector?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("vector? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("vector->list requires 1 argument"))); }
            match &args[0] {
                Value::Vector(v) => Ok(vec_to_pair_list(v.borrow().clone())),
                _ => Err(EvalError::Type(span.fmt("vector->list: expected vector"))),
            }
        }
        "list->vector" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("list->vector requires 1 argument"))); }
            let items = value_to_vec(&args[0]).ok_or_else(|| EvalError::Type(span.fmt("list->vector: expected list")))?;
            Ok(Value::Vector(Rc::new(RefCell::new(items))))
        }
        "values" => {
            if args.len() == 1 {
                Ok(args[0].clone())
            } else {
                Ok(Value::Values(args.to_vec()))
            }
        }
        "syntax->datum" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("syntax->datum requires 1 argument"))); }
            match &args[0] {
                Value::Syntax(expr) => Ok(expr_to_value(expr)),
                other => Ok(other.clone()),
            }
        }
        "datum->syntax" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("datum->syntax requires 2 arguments"))); }
            let expr = value_to_expr(&args[1], span);
            Ok(Value::Syntax(Box::new(expr)))
        }
        _ => Err(EvalError::UnboundVariable(span.fmt(op))),
    }
}

fn cmp_vals_num(args: &[Value], f: fn(std::cmp::Ordering) -> bool, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("comparison requires at least 2 arguments")));
    }
    for i in 0..args.len() - 1 {
        let ord = num_cmp(&args[i], &args[i + 1], span)?;
        if !f(ord) { return Ok(Value::Boolean(false)); }
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
fn make_default_env() -> Env {
    let env = new_env(None);
    env_set(&env, "apply".into(), Value::Builtin("apply".into()));
    env_set(&env, "call/cc".into(), Value::Builtin("call/cc".into()));
    env_set(&env, "call-with-current-continuation".into(), Value::Builtin("call/cc".into()));
    // Register all builtins as first-class values
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "cadr", "cddr", "caar", "cdar",
        "null?", "list", "length",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "exact?", "inexact?", "integer?", "rational?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "string-copy", "string->list", "list->string",
        "char->integer", "integer->char",
        "eq?", "eqv?", "equal?", "append", "reverse",
        "abs", "modulo", "remainder", "quotient", "expt", "min", "max",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc",
        "map", "for-each",
        "char-alphabetic?", "char-numeric?",
        "char-upcase", "char-downcase", "char=?", "char<?",
        "string=?", "string<?", "string-ci=?",
        "string-upcase", "string-downcase",
        "vector", "make-vector", "vector-ref", "vector-set!",
        "vector-length", "vector?", "vector->list", "list->vector",
        "set-car!", "set-cdr!",
        "raise", "with-exception-handler",
        "values", "call-with-values",
        "syntax->datum", "datum->syntax",
    ] {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let env = make_default_env();
    let out: Output = Rc::new(RefCell::new(std::string::String::new()));
    NEXT_CONT_ID.with(|c| c.set(0));
    GENSYM_COUNTER.with(|c| c.set(0));

    let mut start_idx = 0;
    loop {
        let mut result = Value::Boolean(false);
        let mut cont_err = None;
        for (i, expr) in exprs.iter().enumerate().skip(start_idx) {
            TOP_LEVEL_IDX.with(|c| c.set(i));
            match eval(expr, &env, &out) {
                Ok(val) => result = val,
                Err(EvalError::ContinuationInvoked(id, top_idx)) => {
                    cont_err = Some((id, top_idx));
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        if let Some((_id, top_idx)) = cont_err {
            let value = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take()).unwrap();
            let cc_span = CONT_CALLCC_SPAN.with(|v| v.borrow_mut().take()).unwrap();
            CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = Some((cc_span, value)));
            start_idx = top_idx;
            continue;
        }
        return Ok(result.display());
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse(input)?;
    let env = make_default_env();
    let out: Output = Rc::new(RefCell::new(std::string::String::new()));
    NEXT_CONT_ID.with(|c| c.set(0));
    GENSYM_COUNTER.with(|c| c.set(0));

    let mut start_idx = 0;
    loop {
        let mut result = Value::Boolean(false);
        let mut cont_err = None;
        for (i, expr) in exprs.iter().enumerate().skip(start_idx) {
            TOP_LEVEL_IDX.with(|c| c.set(i));
            match eval(expr, &env, &out) {
                Ok(val) => result = val,
                Err(EvalError::ContinuationInvoked(id, top_idx)) => {
                    cont_err = Some((id, top_idx));
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        if let Some((_id, top_idx)) = cont_err {
            let value = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take()).unwrap();
            let cc_span = CONT_CALLCC_SPAN.with(|v| v.borrow_mut().take()).unwrap();
            CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = Some((cc_span, value)));
            start_idx = top_idx;
            continue;
        }
        let output = out.borrow().clone();
        return Ok((result.display(), output));
    }
}

#[cfg(test)]
mod tests;
