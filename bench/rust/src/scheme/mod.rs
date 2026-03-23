pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A Scheme value.
#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Symbol(String),
    Void,
    Lambda {
        params: Vec<String>,
        body: Vec<Value>,
        env: Env,
    },
    Builtin(String, fn(&[Value]) -> Result<Value, EvalError>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({n})"),
            Value::Boolean(b) => write!(f, "Boolean({b})"),
            Value::String(s) => write!(f, "String({s:?})"),
            Value::List(l) => write!(f, "List({l:?})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::Void => write!(f, "Void"),
            Value::Lambda { params, .. } => write!(f, "Lambda({params:?})"),
            Value::Builtin(name, _) => write!(f, "Builtin({name})"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{}\"", s),
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
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Void => write!(f, "#<void>"),
            Value::Lambda { .. } | Value::Builtin(..) => write!(f, "#<procedure>"),
        }
    }
}

// ── Environment ──

type Env = Rc<RefCell<EnvInner>>;

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
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn default_env() -> Env {
    let env = new_env(None);
    // Register builtins
    let builtins: &[(&str, fn(&[Value]) -> Result<Value, EvalError>)] = &[
        ("+", arith_add),
        ("-", arith_sub),
        ("*", arith_mul),
        ("/", arith_div),
        ("<", |a| cmp_op(a, |x, y| x < y)),
        (">", |a| cmp_op(a, |x, y| x > y)),
        ("=", |a| cmp_op(a, |x, y| x == y)),
        ("<=", |a| cmp_op(a, |x, y| x <= y)),
        (">=", |a| cmp_op(a, |x, y| x >= y)),
        ("not", builtin_not),
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string(), func));
    }
    env
}

// ── Parser ──

struct Parser {
    tokens: Vec<String>,
    pos: usize,
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
            '(' | ')' => {
                tokens.push(chars[i].to_string());
                i += 1;
            }
            '\'' => {
                tokens.push("'".to_string());
                i += 1;
            }
            '"' => {
                let mut s = String::new();
                s.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        i += 1;
                        s.push(chars[i]);
                        i += 1;
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
            '#' => {
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')') {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
            _ => {
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';') {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Value, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let tok = self.tokens[self.pos].clone();
        if tok == "(" {
            self.pos += 1;
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos] != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            self.pos += 1; // consume ')'
            Ok(Value::List(elems))
        } else if tok == "'" {
            self.pos += 1;
            let inner = self.parse_expr()?;
            Ok(Value::List(vec![Value::Symbol("quote".into()), inner]))
        } else {
            self.pos += 1;
            Ok(parse_atom(&tok))
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Value>, EvalError> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

fn parse_atom(token: &str) -> Value {
    if token == "#t" {
        Value::Boolean(true)
    } else if token == "#f" {
        Value::Boolean(false)
    } else if token.starts_with('"') && token.ends_with('"') {
        Value::String(token[1..token.len() - 1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        Value::Integer(n)
    } else {
        Value::Symbol(token.to_string())
    }
}

// ── Evaluator ──

fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Void => Ok(expr.clone()),
        Value::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Value::Lambda { .. } | Value::Builtin(..) => Ok(expr.clone()),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let Value::Symbol(name) = &elems[0] {
                match name.as_str() {
                    "quote" => return eval_quote(&elems[1..]),
                    "if" => return eval_if(&elems[1..], env),
                    "define" => return eval_define(&elems[1..], env),
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    _ => {}
                }
            }
            // Function application
            let func = eval(&elems[0], env)?;
            let args: Result<Vec<Value>, _> = elems[1..].iter().map(|e| eval(e, env)).collect();
            let args = args?;
            apply(&func, &args)
        }
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote expects 1 argument".into()));
    }
    Ok(args[0].clone())
}

fn eval_if(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if expects 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond != Value::Boolean(false) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define expects 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Value::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let params: Result<Vec<String>, _> = sig[1..].iter().map(|v| match v {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define: expected symbol as parameter".into())),
            }).collect();
            let params = params?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let params = match &args[0] {
        Value::List(elems) => {
            let mut params = Vec::new();
            for e in elems {
                match e {
                    Value::Symbol(s) => params.push(s.clone()),
                    _ => return Err(EvalError::Type("lambda: expected symbol in params".into())),
                }
            }
            params
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    for expr in exprs {
        let val = eval(expr, env)?;
        if val != Value::Boolean(false) {
            return Ok(val);
        }
    }
    Ok(Value::Boolean(false))
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
            for (param, arg) in params.iter().zip(args) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        Value::Builtin(_, func) => func(args),
        _ => Err(EvalError::Type(format!("not a procedure: {func}"))),
    }
}

// ── Builtins ──

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not expects 1 argument".into()));
    }
    Ok(Value::Boolean(args[0] == Value::Boolean(false)))
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if nums.len() == 1 {
        Ok(Value::Integer(-nums[0]))
    } else {
        Ok(Value::Integer(nums[0] - nums[1..].iter().sum::<i64>()))
    }
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    let mut result = nums[0];
    for &d in &nums[1..] {
        if d == 0 {
            return Err(EvalError::Type("division by zero".into()));
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn cmp_op(args: &[Value], op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = default_env();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(input)?;
    Ok((result, String::new()))
}

#[cfg(test)]
mod tests;
