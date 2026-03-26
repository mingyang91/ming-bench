pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::rc::Rc;

type Output = Rc<RefCell<String>>;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Char(char),
    Str(Rc<RefCell<Vec<char>>>),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Void,
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Record { type_id: u64, fields: Vec<Value> },
    RecordConstructor { type_id: u64, n_fields: usize },
    RecordPredicate { type_id: u64 },
    RecordAccessor { type_id: u64, index: usize },
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 { let t = b; b = a % t; a = t; }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d.abs();
    let g = gcd(n.abs(), d);
    let (n, d) = (n / g, d / g);
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

fn val_to_f64(v: &Value) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        other => Err(EvalError::Runtime(format!("expected number, got {}", other.display()))),
    }
}

fn val_to_rational(v: &Value) -> Option<(i64, i64)> {
    match v {
        Value::Integer(n) => Some((*n, 1)),
        Value::Rational(n, d) => Some((*n, *d)),
        _ => None,
    }
}

fn is_numeric(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

fn float_to_rational(f: f64) -> (i64, i64) {
    // Simple approach: multiply by power of 2 to get exact fraction
    // f64 is a binary fraction, so we can represent it exactly as n/2^k
    if f == f.floor() {
        return (f as i64, 1);
    }
    // Use continued fraction approximation
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    let mut p0: i64 = 0; let mut q0: i64 = 1;
    let mut p1: i64 = 1; let mut q1: i64 = 0;
    let mut x = f;
    for _ in 0..64 {
        let a = x.floor() as i64;
        let p2 = a * p1 + p0;
        let q2 = a * q1 + q0;
        p0 = p1; q0 = q1;
        p1 = p2; q1 = q2;
        let approx = p1 as f64 / q1 as f64;
        if (approx - f).abs() < 1e-15 {
            break;
        }
        let frac = x - a as f64;
        if frac.abs() < 1e-15 { break; }
        x = 1.0 / frac;
    }
    (sign * p1, q1)
}

fn any_inexact(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Float(_)))
}

fn make_str(s: &str) -> Value {
    Value::Str(Rc::new(RefCell::new(s.chars().collect())))
}

impl Value {
    /// Scheme `write`-style: strings get quotes, chars get #\ prefix.
    fn display(&self) -> String {
        self.fmt(true)
    }

    /// Scheme `display`-style: strings without quotes, chars as bare character.
    fn display_fmt(&self) -> String {
        self.fmt(false)
    }

    fn fmt(&self, write_mode: bool) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                let s = format!("{}", f);
                if s.contains('.') || s.contains('e') || s.contains('E') { s } else { format!("{}.0", s) }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Char(c) if write_mode => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{}", c),
            },
            Value::Char(c) => c.to_string(),
            Value::Str(s) if write_mode => {
                let chars = s.borrow();
                format!("\"{}\"", chars.iter().collect::<String>())
            }
            Value::Str(s) => s.borrow().iter().collect(),
            Value::Symbol(s) => s.clone(),
            Value::Nil => "()".into(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first { out.push(' '); }
                            first = false;
                            out.push_str(&car.fmt(write_mode));
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.fmt(write_mode));
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Void => "".into(),
            Value::Macro { .. } => "#<macro>".into(),
            Value::Record { .. } => "#<record>".into(),
            Value::RecordConstructor { .. } | Value::RecordPredicate { .. } | Value::RecordAccessor { .. } => "#<procedure>".into(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Runtime(format!("expected integer, got {}", other.display()))),
        }
    }
}

// ---------- Environment ----------

type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
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

fn env_set_existing(env: &Env, name: &str, val: Value) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        true
    } else if let Some(ref parent) = inner.parent {
        env_set_existing(parent, name, val)
    } else {
        false
    }
}

fn global_env() -> Env {
    let env = new_env(None);
    for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                   "cons", "car", "cdr", "list", "length",
                   "null?", "boolean?", "number?", "integer?", "rational?",
                   "string?", "pair?", "symbol?", "char?",
                   "exact?", "inexact?", "exact->inexact", "inexact->exact",
                   "numerator", "denominator",
                   "append",
                   "display", "write", "newline",
                   "string-append", "string-length", "substring",
                   "string->number", "number->string",
                   "symbol->string", "string->symbol",
                   "string-ref", "string-set!", "string-copy",
                   "apply",
                   "abs", "modulo", "remainder", "quotient",
                   "min", "max", "expt",
                   "zero?", "positive?", "negative?", "odd?", "even?",
                   "list-ref", "list-tail", "list?", "assoc", "map",
                   "char-alphabetic?", "char-numeric?",
                   "char-upcase", "char-downcase", "char=?", "char<?",
                   "string=?", "string<?", "string-ci=?",
                   "string-upcase", "string-downcase",
                   "equal?", "eq?"] {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

// ---------- AST ----------

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
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
        Self { kind, span }
    }
}

fn err_at(span: Span, msg: impl std::fmt::Display) -> EvalError {
    EvalError::Runtime(format!("{} at {}", msg, span))
}

fn parse_err_at(span: Span, msg: impl std::fmt::Display) -> EvalError {
    EvalError::Parse(format!("{} at {}", msg, span))
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone)]
struct Token {
    text: String,
    line: usize,
    col: usize,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(Token { text: "(".into(), line, col });
                i += 1; col += 1;
            }
            ')' => {
                tokens.push(Token { text: ")".into(), line, col });
                i += 1; col += 1;
            }
            '\'' => {
                tokens.push(Token { text: "'".into(), line, col });
                i += 1; col += 1;
            }
            '"' => {
                let start_col = col;
                let start_line = line;
                let mut s = String::from('"');
                i += 1; col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2; col += 2;
                    } else {
                        if chars[i] == '\n' {
                            line += 1; col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1; col += 1;
                }
                tokens.push(Token { text: s, line: start_line, col: start_col });
            }
            _ => {
                let start_col = col;
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                    i += 1; col += 1;
                }
                tokens.push(Token { text: chars[start..i].iter().collect(), line, col: start_col });
            }
        }
    }
    tokens
}

// ---------- Parser ----------

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[*pos];
    let span = Span::new(tok.line, tok.col);

    if tok.text == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr::new(
            ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".into()), span),
                inner,
            ]),
            span,
        ))
    } else if tok.text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(parse_err_at(span, "missing closing parenthesis"));
        }
        *pos += 1;
        Ok(Expr::new(ExprKind::List(list), span))
    } else if tok.text == ")" {
        Err(parse_err_at(span, "unexpected ')'"))
    } else {
        *pos += 1;
        Ok(parse_atom(&tok.text, span))
    }
}

fn parse_atom(token: &str, span: Span) -> Expr {
    if token == "#t" {
        Expr::new(ExprKind::Boolean(true), span)
    } else if token == "#f" {
        Expr::new(ExprKind::Boolean(false), span)
    } else if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        let s = inner.replace("\\n", "\n").replace("\\\"", "\"").replace("\\\\", "\\");
        Expr::new(ExprKind::Str(s), span)
    } else if token.starts_with("#\\") {
        let rest = &token[2..];
        let c = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Expr::new(ExprKind::Symbol(token.to_string()), span),
        };
        Expr::new(ExprKind::Char(c), span)
    } else if let Ok(n) = token.parse::<i64>() {
        Expr::new(ExprKind::Integer(n), span)
    } else if let Some(idx) = token.find('/') {
        // Try rational literal n/d
        if idx > 0 && idx < token.len() - 1 {
            if let (Ok(n), Ok(d)) = (token[..idx].parse::<i64>(), token[idx+1..].parse::<i64>()) {
                if d != 0 {
                    return Expr::new(ExprKind::Rational(n, d), span);
                }
            }
        }
        Expr::new(ExprKind::Symbol(token.to_string()), span)
    } else if let Ok(f) = token.parse::<f64>() {
        Expr::new(ExprKind::Float(f), span)
    } else {
        Expr::new(ExprKind::Symbol(token.to_string()), span)
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------- Evaluator ----------

fn eval(expr: &Expr, env: &Env, out: &Output) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Rational(n, d) => Ok(make_rational(*n, *d)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Str(s) => Ok(make_str(s)),
        ExprKind::Symbol(name) => {
            env_get(env, name).ok_or_else(|| err_at(span, format!("unbound variable: {}", name)))
        }
        ExprKind::List(list) => {
            if list.is_empty() {
                return Err(err_at(span, "empty application"));
            }
            if let ExprKind::Symbol(ref op) = list[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&list[1..], env, span, out),
                    "if" => return eval_if(&list[1..], env, span, out),
                    "quote" => return eval_quote(&list[1..], span),
                    "lambda" => return eval_lambda(&list[1..], env, span),
                    "and" => return eval_and(&list[1..], env, out),
                    "or" => return eval_or(&list[1..], env, out),
                    "let" => return eval_let(&list[1..], env, span, out),
                    "begin" => return eval_begin(&list[1..], env, out),
                    "cond" => return eval_cond(&list[1..], env, span, out),
                    "set!" => return eval_set(&list[1..], env, span, out),
                    "define-syntax" => return eval_define_syntax(&list[1..], env, span),
                    "define-record-type" => return eval_define_record_type(&list[1..], env, span),
                    _ => {
                        if let Some(Value::Macro { ref literals, ref rules, ref def_env }) = env_get(env, op) {
                            let expanded = expand_macro(list, &literals, &rules, &def_env, span, env)?;
                            return eval(&expanded, env, out);
                        }
                    }
                }
            }
            let func = eval(&list[0], env, out)?;
            let args: Result<Vec<Value>, _> = list[1..].iter().map(|a| eval(a, env, out)).collect();
            let args = args?;
            apply_func(&func, &args, span, out)
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(err_at(span, "define: bad syntax"));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(err_at(span, "define: expected 2 parts"));
            }
            let val = eval(&args[1], env, out)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(err_at(span, "define: bad syntax"));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(err_at(span, "define: expected symbol")),
            };
            let (params, rest_param) = parse_params(&sig[1..], span, "define")?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(err_at(span, "define: bad syntax")),
    }
}

fn eval_if(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(err_at(span, "if: expected 2 or 3 parts"));
    }
    let cond = eval(&args[0], env, out)?;
    if cond.is_truthy() {
        eval(&args[1], env, out)
    } else if args.len() == 3 {
        eval(&args[2], env, out)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(err_at(span, "quote: expected 1 argument"));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => make_str(s),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = Value::Pair(Box::new(expr_to_value(item)), Box::new(result));
            }
            result
        }
    }
}

fn parse_params(param_exprs: &[Expr], span: Span, ctx: &str) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(err_at(span, format!("{}: expected rest parameter after dot", ctx)));
                }
                match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(r) => rest_param = Some(r.clone()),
                    _ => return Err(err_at(span, format!("{}: expected symbol for rest parameter", ctx))),
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(err_at(span, format!("{}: expected parameter name", ctx))),
        }
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, "lambda: expected params and body"));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, span, "lambda")?,
        ExprKind::Symbol(s) => {
            // (lambda args body) — single symbol means all args go to rest
            (Vec::new(), Some(s.clone()))
        }
        _ => return Err(err_at(span, "lambda: expected parameter list")),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, rest_param, body, env: env.clone() })
}

fn eval_and(args: &[Expr], env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env, out)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env, out)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, "let: expected bindings and body"));
    }
    let (name, bindings_expr, body) = match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() < 3 {
                return Err(err_at(span, "let: expected bindings and body"));
            }
            let b = match &args[1].kind {
                ExprKind::List(b) => b,
                _ => return Err(err_at(span, "let: expected bindings list")),
            };
            (Some(name.clone()), b.as_slice(), &args[2..])
        }
        ExprKind::List(b) => (None, b.as_slice(), &args[1..]),
        _ => return Err(err_at(span, "let: expected bindings list")),
    };

    let mut param_names = Vec::new();
    let mut init_vals = Vec::new();
    for binding in bindings_expr {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let pname = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(err_at(span, "let: expected symbol in binding")),
                };
                let val = eval(&pair[1], env, out)?;
                param_names.push(pname);
                init_vals.push(val);
            }
            _ => return Err(err_at(span, "let: bad binding")),
        }
    }

    let local = new_env(Some(env.clone()));

    if let Some(loop_name) = name {
        let body_vec = body.to_vec();
        let lambda = Value::Lambda {
            params: param_names.clone(),
            rest_param: None,
            body: body_vec,
            env: local.clone(),
        };
        env_set(&local, loop_name, lambda);
    }

    for (p, v) in param_names.iter().zip(init_vals.iter()) {
        env_set(&local, p.clone(), v.clone());
    }

    let mut result = Value::Void;
    for expr in body {
        result = eval(expr, &local, out)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env, out)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(ref s) = parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env, out)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env, out)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(err_at(span, "cond: bad clause")),
        }
    }
    Ok(Value::Void)
}

fn eval_set(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(err_at(span, "set!: expected 2 parts"));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s,
        _ => return Err(err_at(span, "set!: expected symbol")),
    };
    let val = eval(&args[1], env, out)?;
    if !env_set_existing(env, name, val) {
        return Err(err_at(span, format!("set!: unbound variable: {}", name)));
    }
    Ok(Value::Void)
}

// ---------- Macros (syntax-rules) ----------

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("#{}#{}", base, n)
}

const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "and", "or", "let", "begin",
    "cond", "set!", "define-syntax", "syntax-rules", "define-record-type",
];

fn eval_define_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(err_at(span, "define-syntax: bad syntax"));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err_at(span, "define-syntax: expected symbol")),
    };
    let sr = match &args[1].kind {
        ExprKind::List(parts) => parts,
        _ => return Err(err_at(span, "define-syntax: expected syntax-rules")),
    };
    if sr.len() < 2 {
        return Err(err_at(span, "syntax-rules: expected literals list"));
    }
    match &sr[0].kind {
        ExprKind::Symbol(s) if s == "syntax-rules" => {}
        _ => return Err(err_at(span, "define-syntax: expected syntax-rules")),
    }
    let literals = match &sr[1].kind {
        ExprKind::List(lits) => {
            let mut ls = Vec::new();
            for l in lits {
                match &l.kind {
                    ExprKind::Symbol(s) => ls.push(s.clone()),
                    _ => return Err(err_at(span, "syntax-rules: literals must be identifiers")),
                }
            }
            ls
        }
        _ => return Err(err_at(span, "syntax-rules: expected literals list")),
    };
    let mut rules = Vec::new();
    for rule_expr in &sr[2..] {
        match &rule_expr.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(err_at(span, "syntax-rules: bad rule")),
        }
    }
    env_set(env, name, Value::Macro { literals, rules, def_env: env.clone() });
    Ok(Value::Void)
}

fn eval_define_record_type(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    // (define-record-type <type-name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(err_at(span, "define-record-type: bad syntax"));
    }
    // type name (ignored as a binding, just used for error messages)
    let _type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err_at(span, "define-record-type: expected type name")),
    };
    // constructor: (ctor-name field-name ...)
    let (ctor_name, ctor_fields) = match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(err_at(span, "define-record-type: expected constructor name")),
            };
            let mut fields = Vec::new();
            for p in &parts[1..] {
                match &p.kind {
                    ExprKind::Symbol(s) => fields.push(s.clone()),
                    _ => return Err(err_at(span, "define-record-type: expected field name")),
                }
            }
            (name, fields)
        }
        _ => return Err(err_at(span, "define-record-type: expected constructor")),
    };
    // predicate name
    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err_at(span, "define-record-type: expected predicate name")),
    };
    // field specs: (field-name accessor-name)
    let mut accessors: Vec<(String, String)> = Vec::new(); // (field_name, accessor_name)
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let field = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(err_at(span, "define-record-type: expected field name")),
                };
                let accessor = match &parts[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(err_at(span, "define-record-type: expected accessor name")),
                };
                accessors.push((field, accessor));
            }
            _ => return Err(err_at(span, "define-record-type: expected field spec")),
        }
    }

    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let n_fields = ctor_fields.len();

    // Define constructor
    env_set(env, ctor_name, Value::RecordConstructor { type_id, n_fields });

    // Define predicate
    env_set(env, pred_name, Value::RecordPredicate { type_id });

    // Define accessors
    for (field_name, accessor_name) in &accessors {
        let index = ctor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| err_at(span, format!("define-record-type: field {} not in constructor", field_name)))?;
        env_set(env, accessor_name.clone(), Value::RecordAccessor { type_id, index });
    }

    Ok(Value::Void)
}

#[derive(Debug, Clone)]
enum MatchBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

fn match_list_pattern(
    pat_elems: &[Expr],
    inp_elems: &[Expr],
    bindings: &mut HashMap<String, MatchBinding>,
    literals: &[String],
) -> bool {
    // Find ellipsis position
    let ellipsis_pos = pat_elems.iter().position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."));

    if let Some(epos) = ellipsis_pos {
        if epos == 0 { return false; }
        let before = &pat_elems[..epos - 1];
        let ellipsis_pat = &pat_elems[epos - 1];
        let after = &pat_elems[epos + 1..];

        if inp_elems.len() < before.len() + after.len() {
            return false;
        }

        for (p, i) in before.iter().zip(inp_elems.iter()) {
            if !match_pattern(p, i, bindings, literals) {
                return false;
            }
        }

        let after_start = inp_elems.len() - after.len();
        for (p, i) in after.iter().zip(inp_elems[after_start..].iter()) {
            if !match_pattern(p, i, bindings, literals) {
                return false;
            }
        }

        let ellipsis_inputs = &inp_elems[before.len()..after_start];
        match &ellipsis_pat.kind {
            ExprKind::Symbol(name) if !literals.contains(name) && name != "_" => {
                bindings.insert(name.clone(), MatchBinding::Many(ellipsis_inputs.to_vec()));
                true
            }
            _ => false,
        }
    } else {
        if pat_elems.len() != inp_elems.len() {
            return false;
        }
        for (p, i) in pat_elems.iter().zip(inp_elems.iter()) {
            if !match_pattern(p, i, bindings, literals) {
                return false;
            }
        }
        true
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    bindings: &mut HashMap<String, MatchBinding>,
    literals: &[String],
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(name) => {
            if name == "_" {
                true
            } else if literals.contains(name) {
                matches!(&input.kind, ExprKind::Symbol(s) if s == name)
            } else {
                bindings.insert(name.clone(), MatchBinding::Single(input.clone()));
                true
            }
        }
        ExprKind::Integer(n) => matches!(&input.kind, ExprKind::Integer(m) if m == n),
        ExprKind::Float(f) => matches!(&input.kind, ExprKind::Float(g) if g == f),
        ExprKind::Rational(n, d) => matches!(&input.kind, ExprKind::Rational(n2, d2) if n2 == n && d2 == d),
        ExprKind::Boolean(b) => matches!(&input.kind, ExprKind::Boolean(b2) if b2 == b),
        ExprKind::List(pat_elems) => {
            match &input.kind {
                ExprKind::List(inp_elems) => match_list_pattern(pat_elems, inp_elems, bindings, literals),
                _ => false,
            }
        }
        _ => false,
    }
}

fn collect_pattern_vars(pattern: &Expr, literals: &[String], vars: &mut HashSet<String>) {
    match &pattern.kind {
        ExprKind::Symbol(name) if name != "..." && name != "_" && !literals.contains(name) => {
            vars.insert(name.clone());
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_pattern_vars(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn collect_template_symbols(template: &Expr, pattern_vars: &HashSet<String>, result: &mut HashSet<String>) {
    match &template.kind {
        ExprKind::Symbol(name) if name != "..." && !pattern_vars.contains(name) => {
            result.insert(name.clone());
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_template_symbols(e, pattern_vars, result);
            }
        }
        _ => {}
    }
}

fn find_ellipsis_var(template: &Expr, bindings: &HashMap<String, MatchBinding>) -> Option<String> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(MatchBinding::Many(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                if let Some(v) = find_ellipsis_var(e, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    gensym_map: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    MatchBinding::Single(e) => e.clone(),
                    MatchBinding::Many(_) => template.clone(),
                }
            } else if let Some(gname) = gensym_map.get(name) {
                Expr::new(ExprKind::Symbol(gname.clone()), template.span)
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() {
                    if let ExprKind::Symbol(ref s) = elems[i + 1].kind {
                        if s == "..." {
                            if let Some(var_name) = find_ellipsis_var(&elems[i], bindings) {
                                if let Some(MatchBinding::Many(items)) = bindings.get(&var_name) {
                                    for item in items {
                                        let mut local_bindings = bindings.clone();
                                        local_bindings.insert(var_name.clone(), MatchBinding::Single(item.clone()));
                                        result.push(expand_template(&elems[i], &local_bindings, gensym_map));
                                    }
                                }
                            }
                            i += 2;
                            continue;
                        }
                    }
                }
                result.push(expand_template(&elems[i], bindings, gensym_map));
                i += 1;
            }
            Expr::new(ExprKind::List(result), template.span)
        }
        _ => template.clone(),
    }
}

fn expand_macro(
    input_elems: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    span: Span,
    env: &Env,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => &e[..],
            _ => continue,
        };
        if pat_elems.is_empty() { continue; }

        let mut bindings = HashMap::new();
        if match_list_pattern(&pat_elems[1..], &input_elems[1..], &mut bindings, literals) {
            let mut pattern_vars = HashSet::new();
            collect_pattern_vars(pattern, literals, &mut pattern_vars);
            // Remove the keyword position symbol from pattern vars
            if let ExprKind::Symbol(ref kw) = pat_elems[0].kind {
                pattern_vars.remove(kw);
            }

            let mut template_syms = HashSet::new();
            collect_template_symbols(template, &pattern_vars, &mut template_syms);

            let mut gensym_map = HashMap::new();
            for sym in &template_syms {
                if !SPECIAL_FORMS.contains(&sym.as_str()) {
                    gensym_map.insert(sym.clone(), gensym(sym));
                }
            }

            // Inject definition-site bindings for gensym'd names
            for (orig, gsym) in &gensym_map {
                if let Some(val) = env_get(def_env, orig) {
                    env_set(env, gsym.clone(), val);
                }
            }

            return Ok(expand_template(template, &bindings, &gensym_map));
        }
    }
    Err(err_at(span, "no matching syntax-rules pattern"))
}

fn apply_func(func: &Value, args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            if let Some(ref _rp) = rest_param {
                if args.len() < params.len() {
                    return Err(err_at(span, format!(
                        "wrong number of arguments: expected at least {}, got {}", params.len(), args.len()
                    )));
                }
            } else if params.len() != args.len() {
                return Err(err_at(span, format!(
                    "wrong number of arguments: expected {}, got {}", params.len(), args.len()
                )));
            }
            let local = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local, p.clone(), a.clone());
            }
            if let Some(ref rp) = rest_param {
                let mut rest = Value::Nil;
                for a in args[params.len()..].iter().rev() {
                    rest = Value::Pair(Box::new(a.clone()), Box::new(rest));
                }
                env_set(&local, rp.clone(), rest);
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local, out)?;
            }
            Ok(result)
        }
        Value::Builtin(name) => apply_builtin(name, args, span, out),
        Value::RecordConstructor { type_id, n_fields } => {
            if args.len() != *n_fields {
                return Err(err_at(span, format!("record constructor: expected {} args, got {}", n_fields, args.len())));
            }
            Ok(Value::Record { type_id: *type_id, fields: args.to_vec() })
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(err_at(span, "record predicate: expected 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)))
        }
        Value::RecordAccessor { type_id, index } => {
            if args.len() != 1 {
                return Err(err_at(span, "record accessor: expected 1 argument"));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields } if tid == type_id => {
                    Ok(fields[*index].clone())
                }
                _ => Err(err_at(span, "record accessor: wrong record type")),
            }
        }
        _ => Err(err_at(span, format!("not a procedure: {}", func.display()))),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        // cross-type numeric equality
        (Value::Integer(n), Value::Rational(rn, rd)) | (Value::Rational(rn, rd), Value::Integer(n)) => *rd == 1 && *n == *rn,
        (Value::Float(f), other) | (other, Value::Float(f)) if is_numeric(other) => {
            val_to_f64(other).map_or(false, |v| v == *f)
        }
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => *x.borrow() == *y.borrow(),
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => values_equal(a1, b1) && values_equal(a2, b2),
        _ => false,
    }
}

fn apply_builtin(name: &str, args: &[Value], span: Span, out: &Output) -> Result<Value, EvalError> {
    match name {
        "+" => {
            if args.is_empty() { return Ok(Value::Integer(0)); }
            for a in args.iter() { if !is_numeric(a) { return Err(err_at(span, format!("+ expected number, got {}", a.display()))); } }
            if any_inexact(args) {
                let mut sum = 0.0f64;
                for a in args { sum += val_to_f64(a)?; }
                Ok(Value::Float(sum))
            } else {
                let mut n: i64 = 0;
                let mut d: i64 = 1;
                for a in args {
                    let (an, ad) = val_to_rational(a).unwrap();
                    n = n * ad + an * d;
                    d *= ad;
                    let g = gcd(n.abs(), d.abs());
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "-" => {
            if args.is_empty() { return Err(err_at(span, "- requires at least 1 argument")); }
            for a in args.iter() { if !is_numeric(a) { return Err(err_at(span, format!("- expected number, got {}", a.display()))); } }
            if any_inexact(args) {
                if args.len() == 1 { return Ok(Value::Float(-val_to_f64(&args[0])?)); }
                let mut result = val_to_f64(&args[0])?;
                for a in &args[1..] { result -= val_to_f64(a)?; }
                Ok(Value::Float(result))
            } else {
                let (mut n, mut d) = val_to_rational(&args[0]).unwrap();
                if args.len() == 1 { return Ok(make_rational(-n, d)); }
                for a in &args[1..] {
                    let (an, ad) = val_to_rational(a).unwrap();
                    n = n * ad - an * d;
                    d *= ad;
                    let g = gcd(n.abs(), d.abs());
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "*" => {
            if args.is_empty() { return Ok(Value::Integer(1)); }
            for a in args.iter() { if !is_numeric(a) { return Err(err_at(span, format!("* expected number, got {}", a.display()))); } }
            if any_inexact(args) {
                let mut product = 1.0f64;
                for a in args { product *= val_to_f64(a)?; }
                Ok(Value::Float(product))
            } else {
                let mut n: i64 = 1;
                let mut d: i64 = 1;
                for a in args {
                    let (an, ad) = val_to_rational(a).unwrap();
                    n *= an; d *= ad;
                    let g = gcd(n.abs(), d.abs());
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "/" => {
            if args.is_empty() { return Err(err_at(span, "/ requires at least 1 argument")); }
            for a in args.iter() { if !is_numeric(a) { return Err(err_at(span, format!("/ expected number, got {}", a.display()))); } }
            if any_inexact(args) {
                let mut result = val_to_f64(&args[0])?;
                if args.len() == 1 { return Ok(Value::Float(1.0 / result)); }
                for a in &args[1..] {
                    let dv = val_to_f64(a)?;
                    if dv == 0.0 { return Err(err_at(span, "division by zero")); }
                    result /= dv;
                }
                Ok(Value::Float(result))
            } else {
                let (mut n, mut d) = val_to_rational(&args[0]).unwrap();
                if args.len() == 1 { return Ok(make_rational(d, n)); }
                for a in &args[1..] {
                    let (an, ad) = val_to_rational(a).unwrap();
                    if an == 0 { return Err(err_at(span, "division by zero")); }
                    n *= ad; d *= an;
                    let g = gcd(n.abs(), d.abs());
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "<" => builtin_cmp(args, |a, b| a < b, span),
        ">" => builtin_cmp(args, |a, b| a > b, span),
        "=" => builtin_cmp(args, |a, b| a == b, span),
        "<=" => builtin_cmp(args, |a, b| a <= b, span),
        ">=" => builtin_cmp(args, |a, b| a >= b, span),
        "not" => {
            if args.len() != 1 {
                return Err(err_at(span, "not requires 1 argument"));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(err_at(span, "cons requires 2 arguments"));
            }
            Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
        }
        "car" => {
            if args.len() != 1 {
                return Err(err_at(span, "car requires 1 argument"));
            }
            match &args[0] {
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(err_at(span, "car: not a pair")),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(err_at(span, "cdr requires 1 argument"));
            }
            match &args[0] {
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(err_at(span, "cdr: not a pair")),
            }
        }
        "list" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                result = Value::Pair(Box::new(a.clone()), Box::new(result));
            }
            Ok(result)
        }
        "length" => {
            if args.len() != 1 {
                return Err(err_at(span, "length requires 1 argument"));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(_, cdr) => { count += 1; cur = cdr; }
                    _ => return Err(err_at(span, "length: not a proper list")),
                }
            }
            Ok(Value::Integer(count))
        }
        "null?" => {
            if args.len() != 1 {
                return Err(err_at(span, "null? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Nil)))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(err_at(span, "boolean? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(err_at(span, "number? requires 1 argument"));
            }
            Ok(Value::Boolean(is_numeric(&args[0])))
        }
        "integer?" => {
            if args.len() != 1 { return Err(err_at(span, "integer? requires 1 argument")); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
        }
        "rational?" => {
            if args.len() != 1 { return Err(err_at(span, "rational? requires 1 argument")); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact?" => {
            if args.len() != 1 { return Err(err_at(span, "exact? requires 1 argument")); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(err_at(span, "inexact? requires 1 argument")); }
            Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(err_at(span, "exact->inexact requires 1 argument")); }
            Ok(Value::Float(val_to_f64(&args[0])?))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(err_at(span, "inexact->exact requires 1 argument")); }
            match &args[0] {
                Value::Integer(_) => Ok(args[0].clone()),
                Value::Rational(_, _) => Ok(args[0].clone()),
                Value::Float(f) => {
                    // Convert float to exact rational via continued fraction approximation
                    // For simple cases like 0.5 -> 1/2
                    let (n, d) = float_to_rational(*f);
                    Ok(make_rational(n, d))
                }
                other => Err(err_at(span, format!("inexact->exact: expected number, got {}", other.display()))),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(err_at(span, "numerator requires 1 argument")); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                other => Err(err_at(span, format!("numerator: expected rational, got {}", other.display()))),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(err_at(span, "denominator requires 1 argument")); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                other => Err(err_at(span, format!("denominator: expected rational, got {}", other.display()))),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(err_at(span, "string? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(err_at(span, "pair? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(err_at(span, "symbol? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
        }
        "append" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                match a {
                    Value::Nil => {}
                    Value::Pair(_, _) => {
                        let mut elems = Vec::new();
                        let mut cur = a;
                        loop {
                            match cur {
                                Value::Pair(car, cdr) => {
                                    elems.push(car.as_ref().clone());
                                    cur = cdr;
                                }
                                Value::Nil => break,
                                _ => {
                                    if matches!(result, Value::Nil) {
                                        result = cur.clone();
                                    }
                                    break;
                                }
                            }
                        }
                        for e in elems.into_iter().rev() {
                            result = Value::Pair(Box::new(e), Box::new(result));
                        }
                    }
                    _ => {
                        if matches!(result, Value::Nil) {
                            result = a.clone();
                        } else {
                            return Err(err_at(span, "append: not a list"));
                        }
                    }
                }
            }
            Ok(result)
        }
        "char?" => {
            if args.len() != 1 {
                return Err(err_at(span, "char? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(err_at(span, "display requires 1 argument"));
            }
            out.borrow_mut().push_str(&args[0].display_fmt());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(err_at(span, "write requires 1 argument"));
            }
            out.borrow_mut().push_str(&args[0].display());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(err_at(span, "newline takes 0 arguments"));
            }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(&s.borrow().iter().collect::<String>()),
                    _ => return Err(err_at(span, format!("string-append: expected string, got {}", a.display()))),
                }
            }
            Ok(make_str(&result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(err_at(span, "string-length requires 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.borrow().len() as i64)),
                _ => Err(err_at(span, "string-length: expected string")),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(err_at(span, "substring requires 3 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(err_at(span, "substring: expected string")),
            };
            let start = args[1].as_integer()? as usize;
            let end = args[2].as_integer()? as usize;
            let chars = s.borrow();
            if end > chars.len() || start > end {
                return Err(err_at(span, "substring: index out of range"));
            }
            let sub: String = chars[start..end].iter().collect();
            Ok(make_str(&sub))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(err_at(span, "string->number requires 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => {
                    let st: String = s.borrow().iter().collect();
                    if let Ok(n) = st.parse::<i64>() {
                        Ok(Value::Integer(n))
                    } else if let Ok(f) = st.parse::<f64>() {
                        Ok(Value::Float(f))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                _ => Err(err_at(span, "string->number: expected string")),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(err_at(span, "number->string requires 1 argument"));
            }
            Ok(make_str(&args[0].display()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(err_at(span, "symbol->string requires 1 argument"));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(make_str(s)),
                _ => Err(err_at(span, "symbol->string: expected symbol")),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(err_at(span, "string->symbol requires 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.borrow().iter().collect())),
                _ => Err(err_at(span, "string->symbol: expected string")),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(err_at(span, "string-ref requires 2 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(err_at(span, "string-ref: expected string")),
            };
            let idx = args[1].as_integer()? as usize;
            let chars = s.borrow();
            if idx >= chars.len() {
                return Err(err_at(span, "string-ref: index out of range"));
            }
            Ok(Value::Char(chars[idx]))
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(err_at(span, "string-set! requires 3 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(err_at(span, "string-set!: expected string")),
            };
            let idx = args[1].as_integer()? as usize;
            let c = match &args[2] {
                Value::Char(c) => *c,
                _ => return Err(err_at(span, "string-set!: expected char")),
            };
            let mut chars = s.borrow_mut();
            if idx >= chars.len() {
                return Err(err_at(span, "string-set!: index out of range"));
            }
            chars[idx] = c;
            Ok(Value::Void)
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(err_at(span, "string-copy requires 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(Rc::new(RefCell::new(s.borrow().clone())))),
                _ => Err(err_at(span, "string-copy: expected string")),
            }
        }
        "apply" => {
            if args.len() < 2 {
                return Err(err_at(span, "apply: expected at least 2 arguments"));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            // Collect prefix args + flatten the final list
            let mut all_args = Vec::new();
            for a in &args[1..args.len() - 1] {
                all_args.push(a.clone());
            }
            // Flatten the last argument (must be a list)
            let mut cur = last;
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(car, cdr) => {
                        all_args.push(*car.clone());
                        cur = cdr;
                    }
                    _ => return Err(err_at(span, "apply: last argument must be a list")),
                }
            }
            apply_func(func, &all_args, span, out)
        }
        "abs" => {
            if args.len() != 1 { return Err(err_at(span, "abs requires 1 argument")); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(n.abs())),
                Value::Float(f) => Ok(Value::Float(f.abs())),
                Value::Rational(n, d) => Ok(make_rational(n.abs(), *d)),
                other => Err(err_at(span, format!("abs: expected number, got {}", other.display()))),
            }
        }
        "modulo" => {
            if args.len() != 2 { return Err(err_at(span, "modulo requires 2 arguments")); }
            let a = args[0].as_integer()?;
            let b = args[1].as_integer()?;
            if b == 0 { return Err(err_at(span, "division by zero")); }
            let r = a % b;
            let result = if r != 0 && (r > 0) != (b > 0) { r + b } else { r };
            Ok(Value::Integer(result))
        }
        "remainder" => {
            if args.len() != 2 { return Err(err_at(span, "remainder requires 2 arguments")); }
            let a = args[0].as_integer()?;
            let b = args[1].as_integer()?;
            if b == 0 { return Err(err_at(span, "division by zero")); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 { return Err(err_at(span, "quotient requires 2 arguments")); }
            let a = args[0].as_integer()?;
            let b = args[1].as_integer()?;
            if b == 0 { return Err(err_at(span, "division by zero")); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(err_at(span, "min requires at least 1 argument")); }
            let mut m = args[0].as_integer()?;
            for a in &args[1..] { let v = a.as_integer()?; if v < m { m = v; } }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(err_at(span, "max requires at least 1 argument")); }
            let mut m = args[0].as_integer()?;
            for a in &args[1..] { let v = a.as_integer()?; if v > m { m = v; } }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(err_at(span, "expt requires 2 arguments")); }
            let base = args[0].as_integer()?;
            let exp = args[1].as_integer()?;
            if exp < 0 { return Err(err_at(span, "expt: negative exponent")); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(err_at(span, "zero? requires 1 argument")); }
            Ok(Value::Boolean(val_to_f64(&args[0])? == 0.0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(err_at(span, "positive? requires 1 argument")); }
            Ok(Value::Boolean(val_to_f64(&args[0])? > 0.0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(err_at(span, "negative? requires 1 argument")); }
            Ok(Value::Boolean(val_to_f64(&args[0])? < 0.0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(err_at(span, "odd? requires 1 argument")); }
            Ok(Value::Boolean(args[0].as_integer()? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(err_at(span, "even? requires 1 argument")); }
            Ok(Value::Boolean(args[0].as_integer()? % 2 == 0))
        }
        "list-ref" => {
            if args.len() != 2 { return Err(err_at(span, "list-ref requires 2 arguments")); }
            let idx = args[1].as_integer()? as usize;
            let mut cur = &args[0];
            for _ in 0..idx {
                match cur {
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => return Err(err_at(span, "list-ref: index out of range")),
                }
            }
            match cur {
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(err_at(span, "list-ref: index out of range")),
            }
        }
        "list-tail" => {
            if args.len() != 2 { return Err(err_at(span, "list-tail requires 2 arguments")); }
            let idx = args[1].as_integer()? as usize;
            let mut cur = &args[0];
            for _ in 0..idx {
                match cur {
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => return Err(err_at(span, "list-tail: index out of range")),
                }
            }
            Ok(cur.clone())
        }
        "list?" => {
            if args.len() != 1 { return Err(err_at(span, "list? requires 1 argument")); }
            let mut cur = &args[0];
            let result = loop {
                match cur {
                    Value::Nil => break true,
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => break false,
                }
            };
            Ok(Value::Boolean(result))
        }
        "assoc" => {
            if args.len() != 2 { return Err(err_at(span, "assoc requires 2 arguments")); }
            let key = &args[0];
            let mut cur = &args[1];
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(car, cdr) => {
                        if let Value::Pair(k, _) = car.as_ref() {
                            if values_equal(k, key) {
                                return Ok(*car.clone());
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(err_at(span, "assoc: not a proper list")),
                }
            }
        }
        "map" => {
            if args.len() < 2 { return Err(err_at(span, "map requires at least 2 arguments")); }
            let func = &args[0];
            let mut lists: Vec<&Value> = args[1..].iter().collect();
            let mut result_elems = Vec::new();
            loop {
                // Check if any list is exhausted
                let mut call_args = Vec::new();
                let mut new_lists = Vec::new();
                let mut done = false;
                for lst in &lists {
                    match lst {
                        Value::Nil => { done = true; break; }
                        Value::Pair(car, cdr) => {
                            call_args.push(*car.clone());
                            new_lists.push(cdr.as_ref());
                        }
                        _ => return Err(err_at(span, "map: not a proper list")),
                    }
                }
                if done { break; }
                result_elems.push(apply_func(func, &call_args, span, out)?);
                lists = new_lists;
            }
            let mut result = Value::Nil;
            for e in result_elems.into_iter().rev() {
                result = Value::Pair(Box::new(e), Box::new(result));
            }
            Ok(result)
        }
        "equal?" => {
            if args.len() != 2 { return Err(err_at(span, "equal? requires 2 arguments")); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "eq?" => {
            if args.len() != 2 { return Err(err_at(span, "eq? requires 2 arguments")); }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
                (Value::Float(a), Value::Float(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Nil, Value::Nil) => true,
                _ => std::ptr::eq(
                    &args[0] as *const Value,
                    &args[1] as *const Value,
                ),
            };
            Ok(Value::Boolean(result))
        }
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(err_at(span, "char-alphabetic? requires 1 argument")); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(err_at(span, "char-alphabetic?: expected char")),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(err_at(span, "char-numeric? requires 1 argument")); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(err_at(span, "char-numeric?: expected char")),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(err_at(span, "char-upcase requires 1 argument")); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(err_at(span, "char-upcase: expected char")),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(err_at(span, "char-downcase requires 1 argument")); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(err_at(span, "char-downcase: expected char")),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(err_at(span, "char=? requires 2 arguments")); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(err_at(span, "char=?: expected chars")),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(err_at(span, "char<? requires 2 arguments")); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(err_at(span, "char<?: expected chars")),
            }
        }
        "string=?" => {
            if args.len() != 2 { return Err(err_at(span, "string=? requires 2 arguments")); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(*a.borrow() == *b.borrow())),
                _ => Err(err_at(span, "string=?: expected strings")),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(err_at(span, "string<? requires 2 arguments")); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(*a.borrow() < *b.borrow())),
                _ => Err(err_at(span, "string<?: expected strings")),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(err_at(span, "string-ci=? requires 2 arguments")); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    let al: Vec<char> = a.borrow().iter().map(|c| c.to_ascii_lowercase()).collect();
                    let bl: Vec<char> = b.borrow().iter().map(|c| c.to_ascii_lowercase()).collect();
                    Ok(Value::Boolean(al == bl))
                }
                _ => Err(err_at(span, "string-ci=?: expected strings")),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(err_at(span, "string-upcase requires 1 argument")); }
            match &args[0] {
                Value::Str(s) => {
                    let upper: String = s.borrow().iter().map(|c| c.to_ascii_uppercase()).collect();
                    Ok(make_str(&upper))
                }
                _ => Err(err_at(span, "string-upcase: expected string")),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(err_at(span, "string-downcase requires 1 argument")); }
            match &args[0] {
                Value::Str(s) => {
                    let lower: String = s.borrow().iter().map(|c| c.to_ascii_lowercase()).collect();
                    Ok(make_str(&lower))
                }
                _ => Err(err_at(span, "string-downcase: expected string")),
            }
        }
        _ => Err(err_at(span, format!("unknown procedure: {}", name))),
    }
}

fn builtin_cmp(args: &[Value], cmp: fn(f64, f64) -> bool, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, "comparison requires at least 2 arguments"));
    }
    let mut prev = val_to_f64(&args[0]).map_err(|_| err_at(span, format!("comparison expected number, got {}", args[0].display())))?;
    for a in &args[1..] {
        let cur = val_to_f64(a).map_err(|_| err_at(span, format!("comparison expected number, got {}", a.display())))?;
        if !cmp(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = global_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &out)?;
    }
    Ok(last.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    let env = global_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &out)?;
    }
    let output = out.borrow().clone();
    Ok((last.display(), output))
}

#[cfg(test)]
mod tests;
