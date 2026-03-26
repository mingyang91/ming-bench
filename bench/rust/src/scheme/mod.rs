pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Void,
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
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
                            out.push_str(&car.display());
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.display());
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
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Runtime(format!("expected number, got {}", other.display()))),
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

fn global_env() -> Env {
    let env = new_env(None);
    for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                   "cons", "car", "cdr", "list", "length",
                   "null?", "boolean?", "number?", "string?", "pair?", "symbol?",
                   "append"] {
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
    Boolean(bool),
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
    } else if let Ok(n) = token.parse::<i64>() {
        Expr::new(ExprKind::Integer(n), span)
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

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(name) => {
            env_get(env, name).ok_or_else(|| err_at(span, format!("unbound variable: {}", name)))
        }
        ExprKind::List(list) => {
            if list.is_empty() {
                return Err(err_at(span, "empty application"));
            }
            if let ExprKind::Symbol(ref op) = list[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&list[1..], env, span),
                    "if" => return eval_if(&list[1..], env, span),
                    "quote" => return eval_quote(&list[1..], span),
                    "lambda" => return eval_lambda(&list[1..], env, span),
                    "and" => return eval_and(&list[1..], env),
                    "or" => return eval_or(&list[1..], env),
                    "let" => return eval_let(&list[1..], env, span),
                    "begin" => return eval_begin(&list[1..], env),
                    "cond" => return eval_cond(&list[1..], env, span),
                    _ => {}
                }
            }
            let func = eval(&list[0], env)?;
            let args: Result<Vec<Value>, _> = list[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply_func(&func, &args, span)
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(err_at(span, "define: bad syntax"));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(err_at(span, "define: expected 2 parts"));
            }
            let val = eval(&args[1], env)?;
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
            let params: Result<Vec<String>, _> = sig[1..].iter().map(|p| {
                match &p.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(err_at(span, "define: expected parameter name")),
                }
            }).collect();
            let params = params?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(err_at(span, "define: bad syntax")),
    }
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(err_at(span, "if: expected 2 or 3 parts"));
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
        return Err(err_at(span, "quote: expected 1 argument"));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
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

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, "lambda: expected params and body"));
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            let mut params = Vec::new();
            for p in param_exprs {
                match &p.kind {
                    ExprKind::Symbol(s) => params.push(s.clone()),
                    _ => return Err(err_at(span, "lambda: expected parameter name")),
                }
            }
            params
        }
        _ => return Err(err_at(span, "lambda: expected parameter list")),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, body, env: env.clone() })
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
                let val = eval(&pair[1], env)?;
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

fn eval_cond(clauses: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(ref s) = parts[0].kind {
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
            _ => return Err(err_at(span, "cond: bad clause")),
        }
    }
    Ok(Value::Void)
}

fn apply_func(func: &Value, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if params.len() != args.len() {
                return Err(err_at(span, format!(
                    "wrong number of arguments: expected {}, got {}", params.len(), args.len()
                )));
            }
            let local = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local, p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local)?;
            }
            Ok(result)
        }
        Value::Builtin(name) => apply_builtin(name, args, span),
        _ => Err(err_at(span, format!("not a procedure: {}", func.display()))),
    }
}

fn apply_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args { sum += a.as_integer().map_err(|_| err_at(span, format!("+ expected number, got {}", a.display())))?; }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(err_at(span, "- requires at least 1 argument"));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-args[0].as_integer().map_err(|_| err_at(span, format!("- expected number, got {}", args[0].display())))?))
            } else {
                let mut result = args[0].as_integer().map_err(|_| err_at(span, format!("- expected number, got {}", args[0].display())))?;
                for a in &args[1..] { result -= a.as_integer().map_err(|_| err_at(span, format!("- expected number, got {}", a.display())))?; }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args { product *= a.as_integer().map_err(|_| err_at(span, format!("* expected number, got {}", a.display())))?; }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(err_at(span, "/ requires at least 1 argument"));
            }
            let mut result = args[0].as_integer().map_err(|_| err_at(span, format!("/ expected number, got {}", args[0].display())))?;
            for a in &args[1..] {
                let d = a.as_integer().map_err(|_| err_at(span, format!("/ expected number, got {}", a.display())))?;
                if d == 0 {
                    return Err(err_at(span, "division by zero"));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
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
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
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
        _ => Err(err_at(span, format!("unknown procedure: {}", name))),
    }
}

fn builtin_cmp(args: &[Value], cmp: fn(i64, i64) -> bool, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, "comparison requires at least 2 arguments"));
    }
    let mut prev = args[0].as_integer().map_err(|_| err_at(span, format!("comparison expected number, got {}", args[0].display())))?;
    for a in &args[1..] {
        let cur = a.as_integer().map_err(|_| err_at(span, format!("comparison expected number, got {}", a.display())))?;
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
