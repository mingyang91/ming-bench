pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// ── Value ──

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(b) => if *b { "#t".into() } else { "#f".into() },
            Value::String(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".into(),
        }
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
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => { tokens.push("(".into()); i += 1; }
            ')' => { tokens.push(")".into()); i += 1; }
            '\'' => { tokens.push("'".into()); i += 1; }
            '"' => {
                let mut s = std::string::String::new();
                s.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                }
                tokens.push(s);
            }
            _ => {
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                    i += 1;
                }
                tokens.push(chars[start..i].iter().collect());
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[String], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[*pos];
    if token == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
    } else if token == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos] != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1; // skip )
        Ok(Expr::List(list))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else {
        *pos += 1;
        Ok(parse_atom(token))
    }
}

fn parse_atom(token: &str) -> Expr {
    if token == "#t" {
        Expr::Boolean(true)
    } else if token == "#f" {
        Expr::Boolean(false)
    } else if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        Expr::String(inner.into())
    } else if let Ok(n) = token.parse::<i64>() {
        Expr::Integer(n)
    } else {
        Expr::Symbol(token.into())
    }
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
    match expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::String(s) => Value::String(s.clone()),
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Type("empty application".into()));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &items[0] {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into()));
                        }
                        return Ok(expr_to_value(&items[1]));
                    }
                    "lambda" => return eval_lambda(&items[1..], env),
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &items[1..] {
                            result = eval(arg, env)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &items[1..] {
                            result = eval(arg, env)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" => {
                        let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                        return eval_builtin(op, &args);
                    }
                    _ => {}
                }
            }
            // Function application
            let func = eval(&items[0], env)?;
            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply(&func, &args)
        }
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires a value".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Boolean(false)) // undefined, return something
        }
        // (define (f params...) body...)
        Expr::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Arity("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Expr::Symbol(n) => n.clone(),
                _ => return Err(EvalError::Type("define: expected symbol".into())),
            };
            let params: Vec<String> = sig[1..].iter().map(|e| match e {
                Expr::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define: expected parameter name".into())),
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => {
            param_exprs.iter().map(|e| match e {
                Expr::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("lambda: expected parameter name".into())),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, body, env: env.clone() })
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local_env, p.clone(), a.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn eval_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args { sum += as_int(a)?; }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-as_int(&args[0])?))
            } else {
                let mut result = as_int(&args[0])?;
                for a in &args[1..] { result -= as_int(a)?; }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args { product *= as_int(a)?; }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                let d = as_int(a)?;
                if d == 0 { return Err(EvalError::Type("division by zero".into())); }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => cmp_vals(args, |a, b| a < b),
        ">" => cmp_vals(args, |a, b| a > b),
        "=" => cmp_vals(args, |a, b| a == b),
        "<=" => cmp_vals(args, |a, b| a <= b),
        ">=" => cmp_vals(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

fn cmp_vals(args: &[Value], f: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_int(&args[0])?;
    for a in &args[1..] {
        let curr = as_int(a)?;
        if !f(prev, curr) { return Ok(Value::Boolean(false)); }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let env = new_env(None);
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
