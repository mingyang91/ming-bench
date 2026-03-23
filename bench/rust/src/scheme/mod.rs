pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(BuiltinFn),
    Void,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Str(s) => write!(f, "Str({})", s),
            Value::Symbol(s) => write!(f, "Symbol({})", s),
            Value::List(l) => write!(f, "List({:?})", l),
            Value::Lambda { params, .. } => write!(f, "Lambda({:?})", params),
            Value::Builtin(_) => write!(f, "Builtin"),
            Value::Void => write!(f, "Void"),
        }
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } | Value::Builtin(_) => "#<procedure>".to_string(),
            Value::Void => "".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

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

// ---------- Environment ----------

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

type Env = Rc<RefCell<EnvInner>>;

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

// ---------- Parser ----------

#[derive(Debug, Clone)]
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
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1; col += 1;
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span { line, col } }); col += 1; i += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span { line, col } }); col += 1; i += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span { line, col } }); col += 1; i += 1; }
            '"' => {
                let start_col = col;
                let start_line = line;
                let mut s = String::new();
                s.push('"');
                i += 1; col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        let next = chars[i + 1];
                        match next {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            _ => { s.push('\\'); s.push(next); }
                        }
                        i += 2; col += 2;
                    } else {
                        if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                s.push('"');
                if i < chars.len() { i += 1; col += 1; }
                tokens.push(Token { text: s, span: Span { line: start_line, col: start_col } });
            }
            _ => {
                let start_col = col;
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                    i += 1; col += 1;
                }
                tokens.push(Token { text: chars[start..i].iter().collect(), span: Span { line, col: start_col } });
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[*pos];
    if tok.text == "(" {
        let span = tok.span;
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing parenthesis".into()));
        }
        *pos += 1;
        Ok(Expr { kind: ExprKind::List(list), span })
    } else if tok.text == "'" {
        let span = tok.span;
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr { kind: ExprKind::List(vec![
            Expr { kind: ExprKind::Symbol("quote".into()), span },
            inner,
        ]), span })
    } else if tok.text == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else {
        let span = tok.span;
        *pos += 1;
        Ok(parse_atom(&tok.text, span))
    }
}

fn parse_atom(token: &str, span: Span) -> Expr {
    let kind = if token == "#t" {
        ExprKind::Boolean(true)
    } else if token == "#f" {
        ExprKind::Boolean(false)
    } else if token.starts_with('"') && token.ends_with('"') {
        ExprKind::Str(token[1..token.len()-1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        ExprKind::Integer(n)
    } else {
        ExprKind::Symbol(token.to_string())
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

// ---------- Evaluator ----------

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let span = expr.span;
    let result = match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()).with_position(span.line, span.col));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &elems[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, span),
                    "if" => return eval_if(&elems[1..], env, span),
                    "quote" => return eval_quote(&elems[1..], span),
                    "lambda" => return eval_lambda(&elems[1..], env, span),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, span),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    _ => {}
                }
            }
            // Function application
            let func = eval(&elems[0], env)?;
            let args: Result<Vec<Value>, _> = elems[1..].iter().map(|a| eval(a, env)).collect();
            apply(&func, &args?)
        }
    };
    result.map_err(|e| e.with_position(span.line, span.col))
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} args, got {}", params.len(), args.len()
                )));
            }
            let local = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args) {
                env_set(&local, p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local)?;
            }
            Ok(result)
        }
        Value::Builtin(f) => f(args),
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()).with_position(span.line, span.col));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires a value".into()).with_position(span.line, span.col));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()).with_position(span.line, span.col));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => return Err(EvalError::Parse("define: expected function name".into()).with_position(span.line, span.col)),
            };
            let params: Result<Vec<String>, _> = sig[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("define: expected parameter name".into()).with_position(span.line, span.col)),
            }).collect();
            let lambda = Value::Lambda {
                params: params?,
                body: args[1..].to_vec(),
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: expected symbol or list".into()).with_position(span.line, span.col)),
    }
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()).with_position(span.line, span.col));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()).with_position(span.line, span.col));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()).with_position(span.line, span.col));
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            let mut params = Vec::new();
            for p in param_exprs {
                match &p.kind {
                    ExprKind::Symbol(s) => params.push(s.clone()),
                    _ => return Err(EvalError::Parse("lambda: expected parameter name".into()).with_position(span.line, span.col)),
                }
            }
            params
        }
        _ => return Err(EvalError::Parse("lambda: expected parameter list".into()).with_position(span.line, span.col)),
    };
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()).with_position(span.line, span.col));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        let bindings_expr = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse("let: expected bindings list".into()).with_position(span.line, span.col)),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Parse("let: expected variable name".into()).with_position(span.line, span.col));
                    }
                }
                _ => return Err(EvalError::Parse("let: expected (var init) pair".into()).with_position(span.line, span.col)),
            }
        }
        let local = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: params.clone(),
            body: args[2..].to_vec(),
            env: local.clone(),
        };
        env_set(&local, name.clone(), lambda);
        for (p, v) in params.iter().zip(&inits) {
            env_set(&local, p.clone(), v.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval(expr, &local)?;
        }
        return Ok(result);
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("let: expected bindings list".into()).with_position(span.line, span.col)),
    };
    let local = new_env(Some(env.clone()));
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env)?;
                    env_set(&local, s.clone(), val);
                } else {
                    return Err(EvalError::Parse("let: expected variable name".into()).with_position(span.line, span.col));
                }
            }
            _ => return Err(EvalError::Parse("let: expected (var init) pair".into()).with_position(span.line, span.col)),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in args {
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
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("cond: expected clause".into())),
        }
    }
    Ok(Value::Void)
}

// ---------- Builtins ----------

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for a in args { sum += as_int(a)?; }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if args.len() == 1 {
        return Ok(Value::Integer(-as_int(&args[0])?));
    }
    let mut r = as_int(&args[0])?;
    for a in &args[1..] { r -= as_int(a)?; }
    Ok(Value::Integer(r))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut prod: i64 = 1;
    for a in args { prod *= as_int(a)?; }
    Ok(Value::Integer(prod))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    let mut r = as_int(&args[0])?;
    for a in &args[1..] {
        let d = as_int(a)?;
        if d == 0 { return Err(EvalError::Runtime("division by zero".into())); }
        r /= d;
    }
    Ok(Value::Integer(r))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a < b) }
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a > b) }
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a == b) }
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a <= b) }
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a >= b) }

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires 2 arguments".into()));
    }
    match &args[1] {
        Value::List(tail) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(tail.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => Err(EvalError::Type("cons: second argument must be a list".into())),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for a in args {
        match a {
            Value::List(items) => result.extend(items.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Value::List(result))
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
}

fn builtin_is_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires 1 argument".into()));
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn cmp_op(args: &[Value], f: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_int(&args[0])?;
    for a in &args[1..] {
        let curr = as_int(a)?;
        if !f(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn make_global_env() -> Env {
    let env = new_env(None);
    let builtins: &[(&str, BuiltinFn)] = &[
        ("+", builtin_add),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("<", builtin_lt),
        (">", builtin_gt),
        ("=", builtin_eq),
        ("<=", builtin_le),
        (">=", builtin_ge),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("append", builtin_append),
        ("string?", builtin_is_string),
        ("number?", builtin_is_number),
        ("boolean?", builtin_is_boolean),
        ("pair?", builtin_is_pair),
        ("symbol?", builtin_is_symbol),
    ];
    for (name, f) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(*f));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let env = make_global_env();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
