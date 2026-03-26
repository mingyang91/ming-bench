pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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

// ── Values ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda(Vec<String>, Vec<Expr>, Env),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Lambda(..) => write!(f, "#<procedure>"),
            Value::Void => Ok(()),
        }
    }
}

// ── Tokenizer ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
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
    Boolean(bool),
    Str(String),
    Symbol(String),
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
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr { kind: ExprKind::Boolean(b), span }) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr { kind: ExprKind::Str(s), span }) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr { kind: ExprKind::Symbol(s), span }) }
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
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

/// Wrap an error with span info if it doesn't already have position info.
fn with_span(err: EvalError, span: Span) -> EvalError {
    let msg = err.to_string();
    // Don't double-annotate — check for pattern like "at N:N"
    if msg.contains(&format!("at {}", span)) {
        return err;
    }
    EvalError::Generic(format!("{} at {}", msg, span))
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    eval_inner(expr, env).map_err(|e| with_span(e, expr.span))
}

fn eval_inner(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
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
                        let params = match &items[1].kind {
                            ExprKind::List(ps) => {
                                let mut names = Vec::new();
                                for p in ps {
                                    match &p.kind {
                                        ExprKind::Symbol(s) => names.push(s.clone()),
                                        _ => return Err(EvalError::Parse("lambda param must be symbol".into())),
                                    }
                                }
                                names
                            }
                            _ => return Err(EvalError::Parse("lambda params must be a list".into())),
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(params, body, env.clone()));
                    }
                    "+" => return eval_add(&items[1..], env),
                    "-" => return eval_sub(&items[1..], env),
                    "*" => return eval_mul(&items[1..], env),
                    "/" => return eval_div(&items[1..], env),
                    "<" => return eval_cmp(&items[1..], env, |a, b| a < b),
                    ">" => return eval_cmp(&items[1..], env, |a, b| a > b),
                    "=" => return eval_cmp(&items[1..], env, |a, b| a == b),
                    "<=" => return eval_cmp(&items[1..], env, |a, b| a <= b),
                    ">=" => return eval_cmp(&items[1..], env, |a, b| a >= b),
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
                    _ => {}
                }
            }
            // Function application
            let func = eval(&items[0], env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply(&func, &args)
        }
    }
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda(params, body, closure_env) => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} args, got {}", params.len(), args.len()
                )));
            }
            let call_env = Env::with_parent(closure_env);
            for (p, a) in params.iter().zip(args) {
                call_env.set(p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &call_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::Type(format!("not a procedure: {}", other))),
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
            if parts.is_empty() {
                return Err(EvalError::Parse("define: empty name list".into()));
            }
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: name must be symbol".into())),
            };
            let params: Vec<String> = parts[1..].iter().map(|p| {
                match &p.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("define: param must be symbol".into())),
                }
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda(params, body, env.clone());
            env.set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: first argument must be symbol or list".into())),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn require_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::Type(format!("expected integer, got {}", other))),
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn eval_add(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut sum = 0i64;
    for a in args {
        sum += require_int(&eval(a, env)?)?;
    }
    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    let first = require_int(&eval(&args[0], env)?)?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for a in &args[1..] {
        result -= require_int(&eval(a, env)?)?;
    }
    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut product = 1i64;
    for a in args {
        product *= require_int(&eval(a, env)?)?;
    }
    Ok(Value::Integer(product))
}

fn eval_div(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
    }
    let first = require_int(&eval(&args[0], env)?)?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for a in &args[1..] {
        let d = require_int(&eval(a, env)?)?;
        if d == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn eval_cmp(args: &[Expr], env: &Env, cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = require_int(&eval(&args[0], env)?)?;
    for a in &args[1..] {
        let curr = require_int(&eval(a, env)?)?;
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
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for a in args {
        result = eval(a, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if is_truthy(&test) {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
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
        let lambda = Value::Lambda(params.clone(), body, env.clone());
        let let_env = Env::with_parent(env);
        let_env.set(name.clone(), lambda.clone());
        // Re-create lambda with let_env so it can see itself
        let body2 = match &lambda { Value::Lambda(_, b, _) => b.clone(), _ => unreachable!() };
        let lambda2 = Value::Lambda(params, body2, let_env.clone());
        let_env.set(name.clone(), lambda2.clone());
        let init_vals: Result<Vec<Value>, _> = inits.iter().map(|e| eval(e, env)).collect();
        return apply(&lambda2, &init_vals?);
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
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &let_env)?;
    }
    Ok(result)
}

fn eval_cons(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
    }
    let head = eval(&args[0], env)?;
    let tail = eval(&args[1], env)?;
    match tail {
        Value::List(mut items) => {
            items.insert(0, head);
            Ok(Value::List(items))
        }
        _ => Err(EvalError::Type("cons: second argument must be a list".into())),
    }
}

fn eval_car(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::List(_) => Err(EvalError::Type("car: empty list".into())),
        _ => Err(EvalError::Type("car: not a pair".into())),
    }
}

fn eval_cdr(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::List(_) => Err(EvalError::Type("cdr: empty list".into())),
        _ => Err(EvalError::Type("cdr: not a pair".into())),
    }
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
    Ok(Value::List(items?))
}

fn eval_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type("length: not a list".into())),
    }
}

fn eval_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for a in args {
        match eval(a, env)? {
            Value::List(items) => result.extend(items),
            _ => return Err(EvalError::Type("append: not a list".into())),
        }
    }
    Ok(Value::List(result))
}

fn eval_type_pred(args: &[Expr], env: &Env, kind: &str) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{}? requires exactly 1 argument", kind)));
    }
    let val = eval(&args[0], env)?;
    let result = match kind {
        "number" => matches!(val, Value::Integer(_)),
        "string" => matches!(val, Value::Str(_)),
        "boolean" => matches!(val, Value::Boolean(_)),
        "pair" => matches!(val, Value::List(ref items) if !items.is_empty()),
        "symbol" => matches!(val, Value::Symbol(_)),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

// ── Public API ──────────────────────────────────────────────────────

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(input)?;
    Ok((result, String::new()))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input)?;
    let exprs = parse_all(&tokens)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.to_string())
}

#[cfg(test)]
mod tests;
