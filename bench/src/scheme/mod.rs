pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Box<Value>,
        env: Env,
    },
}

#[derive(Debug, Clone, PartialEq)]
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

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
        }
    }
}

/// Tokenize input into a list of tokens.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '"' => {
                let mut s = String::new();
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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' | ')' | '\'' => {
                tokens.push(chars[i].to_string());
                i += 1;
            }
            _ => {
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    s.push(chars[i]);
                    i += 1;
                }
                tokens.push(s);
            }
        }
    }
    tokens
}

/// Parse a single expression from the token stream.
fn parse(tokens: &[String], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    let token = &tokens[pos];

    // Boolean literals
    if token == "#t" || token == "#true" {
        return Ok((Value::Boolean(true), pos + 1));
    }
    if token == "#f" || token == "#false" {
        return Ok((Value::Boolean(false), pos + 1));
    }

    // String literal
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        // Process escape sequences
        let mut s = String::new();
        let cs: Vec<char> = inner.chars().collect();
        let mut j = 0;
        while j < cs.len() {
            if cs[j] == '\\' && j + 1 < cs.len() {
                match cs[j + 1] {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    other => {
                        s.push('\\');
                        s.push(other);
                    }
                }
                j += 2;
            } else {
                s.push(cs[j]);
                j += 1;
            }
        }
        return Ok((Value::Str(s), pos + 1));
    }

    // Integer literal
    if let Ok(n) = token.parse::<i64>() {
        return Ok((Value::Integer(n), pos + 1));
    }

    // List
    if token == "(" {
        let mut elems = Vec::new();
        let mut p = pos + 1;
        loop {
            if p >= tokens.len() {
                return Err(EvalError::Parse("unclosed parenthesis".to_string()));
            }
            if tokens[p] == ")" {
                return Ok((Value::List(elems), p + 1));
            }
            let (val, next) = parse(tokens, p)?;
            elems.push(val);
            p = next;
        }
    }

    // Quote shorthand
    if token == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        return Ok((Value::List(vec![Value::Symbol("quote".to_string()), inner]), next));
    }

    // Symbol
    Ok((Value::Symbol(token.clone()), pos + 1))
}

/// Parse all expressions from input.
fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let env = new_env(None);
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.display())
}

fn eval(expr: Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda { .. } => Ok(expr),
        Value::Symbol(s) => env_get(env, &s)
            .ok_or_else(|| EvalError::Runtime(format!("unbound symbol: {}", s))),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Runtime("empty application".to_string()));
            }
            let first = &elems[0];
            match first {
                Value::Symbol(op) => match op.as_str() {
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Runtime(
                                "define requires 2 arguments".to_string(),
                            ));
                        }
                        // Shorthand: (define (f x) body) => (define f (lambda (x) body))
                        match &elems[1] {
                            Value::Symbol(name) => {
                                let val = eval(elems[2].clone(), env)?;
                                env_set(env, name.clone(), val.clone());
                                Ok(val)
                            }
                            Value::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Runtime(
                                        "define: empty signature".to_string(),
                                    ));
                                }
                                let name = match &sig[0] {
                                    Value::Symbol(s) => s.clone(),
                                    _ => {
                                        return Err(EvalError::Runtime(
                                            "define: expected symbol".to_string(),
                                        ))
                                    }
                                };
                                let params: Result<Vec<String>, _> = sig[1..]
                                    .iter()
                                    .map(|v| match v {
                                        Value::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::Runtime(
                                            "define: expected symbol in params".to_string(),
                                        )),
                                    })
                                    .collect();
                                let lambda = Value::Lambda {
                                    params: params?,
                                    body: Box::new(elems[2].clone()),
                                    env: env.clone(),
                                };
                                env_set(env, name, lambda.clone());
                                Ok(lambda)
                            }
                            _ => Err(EvalError::Runtime(
                                "define: first argument must be a symbol or list".to_string(),
                            )),
                        }
                    }
                    "lambda" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Runtime(
                                "lambda requires 2 arguments".to_string(),
                            ));
                        }
                        let params = match &elems[1] {
                            Value::List(ps) => {
                                let mut names = Vec::new();
                                for p in ps {
                                    match p {
                                        Value::Symbol(s) => names.push(s.clone()),
                                        _ => {
                                            return Err(EvalError::Runtime(
                                                "lambda: expected symbol in params".to_string(),
                                            ))
                                        }
                                    }
                                }
                                names
                            }
                            _ => {
                                return Err(EvalError::Runtime(
                                    "lambda: expected parameter list".to_string(),
                                ))
                            }
                        };
                        Ok(Value::Lambda {
                            params,
                            body: Box::new(elems[2].clone()),
                            env: env.clone(),
                        })
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Runtime(
                                "if requires 2 or 3 arguments".to_string(),
                            ));
                        }
                        let cond = eval(elems[1].clone(), env)?;
                        if cond != Value::Boolean(false) {
                            eval(elems[2].clone(), env)
                        } else if elems.len() == 4 {
                            eval(elems[3].clone(), env)
                        } else {
                            Ok(Value::Boolean(false))
                        }
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Runtime(
                                "quote requires 1 argument".to_string(),
                            ));
                        }
                        Ok(elems[1].clone())
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval(arg.clone(), env)?;
                            if result == Value::Boolean(false) {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        Ok(result)
                    }
                    "or" => {
                        for arg in &elems[1..] {
                            let result = eval(arg.clone(), env)?;
                            if result != Value::Boolean(false) {
                                return Ok(result);
                            }
                        }
                        Ok(Value::Boolean(false))
                    }
                    _ => {
                        let mut args = Vec::new();
                        for arg in &elems[1..] {
                            args.push(eval(arg.clone(), env)?);
                        }
                        // Try as variable (user-defined procedure) first, then builtin
                        if let Some(proc) = env_get(env, op) {
                            apply_proc(&proc, op, &args, env)
                        } else {
                            apply_builtin_vals(op, &args)
                        }
                    }
                },
                _ => {
                    // Evaluate the operator position (e.g., ((lambda ...) args))
                    let proc = eval(elems[0].clone(), env)?;
                    let mut args = Vec::new();
                    for arg in &elems[1..] {
                        args.push(eval(arg.clone(), env)?);
                    }
                    apply_proc(&proc, "<anonymous>", &args, env)
                }
            }
        }
    }
}

fn apply_proc(proc: &Value, name: &str, args: &[Value], _env: &Env) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda {
            params,
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Runtime(format!(
                    "{}: expected {} arguments, got {}",
                    name,
                    params.len(),
                    args.len()
                )));
            }
            let call_env = new_env(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&call_env, param.clone(), arg.clone());
            }
            eval(body.as_ref().clone(), &call_env)
        }
        _ => {
            // Try as builtin
            apply_builtin_vals(name, args)
        }
    }
}

fn apply_builtin_vals(op: &str, vals: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for v in vals {
                sum += expect_int(v)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if vals.is_empty() {
                return Err(EvalError::Runtime("- requires at least 1 argument".to_string()));
            }
            if vals.len() == 1 {
                return Ok(Value::Integer(-expect_int(&vals[0])?));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                result -= expect_int(v)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for v in vals {
                product *= expect_int(v)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if vals.is_empty() {
                return Err(EvalError::Runtime("/ requires at least 1 argument".to_string()));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                let d = expect_int(v)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".to_string()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("< requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? < expect_int(&vals[1])?))
        }
        ">" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("> requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? > expect_int(&vals[1])?))
        }
        "=" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? == expect_int(&vals[1])?))
        }
        "<=" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("<= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? <= expect_int(&vals[1])?))
        }
        ">=" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime(">= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? >= expect_int(&vals[1])?))
        }
        "not" => {
            if vals.len() != 1 {
                return Err(EvalError::Runtime("not requires 1 argument".to_string()));
            }
            Ok(Value::Boolean(vals[0] == Value::Boolean(false)))
        }
        _ => Err(EvalError::Runtime(format!("unknown procedure: {}", op))),
    }
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Runtime("expected integer".to_string())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
