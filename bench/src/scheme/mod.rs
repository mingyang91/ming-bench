pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

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

type Env = HashMap<String, Value>;

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", parts.join(" "))
            }
        }
    }
}

/// Tokenize input into a list of token strings.
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

/// Parse a single expression from tokens, returning (value, next_index).
fn parse(tokens: &[String], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[pos];
    if token == "(" {
        let mut items = Vec::new();
        let mut i = pos + 1;
        while i < tokens.len() && tokens[i] != ")" {
            let (val, next) = parse(tokens, i)?;
            items.push(val);
            i = next;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        Ok((Value::List(items), i + 1))
    } else if token == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        Ok((Value::List(vec![Value::Symbol("quote".into()), inner]), next))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else if token == "#t" {
        Ok((Value::Boolean(true), pos + 1))
    } else if token == "#f" {
        Ok((Value::Boolean(false), pos + 1))
    } else if token.starts_with('"') {
        let inner = &token[1..token.len() - 1];
        Ok((Value::Str(inner.to_string()), pos + 1))
    } else if let Ok(n) = token.parse::<i64>() {
        Ok((Value::Integer(n), pos + 1))
    } else {
        Ok((Value::Symbol(token.clone()), pos + 1))
    }
}

/// Evaluate a parsed Scheme expression.
fn eval(value: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda { .. } => {
            Ok(value.clone())
        }
        Value::Symbol(s) => env
            .get(s)
            .cloned()
            .ok_or_else(|| EvalError::UndefinedVariable(s.clone())),
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms by symbol name
            if let Value::Symbol(op) = &items[0] {
                match op.as_str() {
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity);
                        }
                        match &items[1] {
                            Value::Symbol(name) => {
                                if items.len() != 3 {
                                    return Err(EvalError::Arity);
                                }
                                let val = eval(&items[2], env)?;
                                env.insert(name.clone(), val);
                                return Ok(Value::Symbol("ok".into()));
                            }
                            Value::List(sig) => {
                                // (define (name params...) body)
                                if sig.is_empty() {
                                    return Err(EvalError::Parse(
                                        "define: empty signature".into(),
                                    ));
                                }
                                let name = match &sig[0] {
                                    Value::Symbol(s) => s.clone(),
                                    _ => {
                                        return Err(EvalError::TypeError(
                                            "define expects symbol".into(),
                                        ))
                                    }
                                };
                                let params: Result<Vec<String>, _> = sig[1..]
                                    .iter()
                                    .map(|v| match v {
                                        Value::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::TypeError(
                                            "parameter must be symbol".into(),
                                        )),
                                    })
                                    .collect();
                                let params = params?;
                                let body = items[2].clone();
                                // Insert a placeholder first so the closure captures itself
                                let lambda = Value::Lambda {
                                    params: params.clone(),
                                    body: Box::new(body.clone()),
                                    env: env.clone(),
                                };
                                env.insert(name.clone(), lambda);
                                // Recreate with updated env so closure has self-reference
                                let lambda = Value::Lambda {
                                    params,
                                    body: Box::new(body),
                                    env: env.clone(),
                                };
                                env.insert(name, lambda);
                                return Ok(Value::Symbol("ok".into()));
                            }
                            _ => {
                                return Err(EvalError::TypeError(
                                    "define expects symbol or list".into(),
                                ))
                            }
                        }
                    }
                    "lambda" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity);
                        }
                        let param_list = match &items[1] {
                            Value::List(ps) => ps,
                            _ => {
                                return Err(EvalError::TypeError(
                                    "lambda params must be a list".into(),
                                ))
                            }
                        };
                        let params: Result<Vec<String>, _> = param_list
                            .iter()
                            .map(|v| match v {
                                Value::Symbol(s) => Ok(s.clone()),
                                _ => Err(EvalError::TypeError(
                                    "parameter must be symbol".into(),
                                )),
                            })
                            .collect();
                        return Ok(Value::Lambda {
                            params: params?,
                            body: Box::new(items[2].clone()),
                            env: env.clone(),
                        });
                    }
                    "if" => {
                        let cond = eval(&items[1], env)?;
                        if cond != Value::Boolean(false) {
                            return eval(&items[2], env);
                        } else if items.len() > 3 {
                            return eval(&items[3], env);
                        } else {
                            return Ok(Value::Symbol("ok".into()));
                        }
                    }
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity);
                        }
                        return Ok(items[1].clone());
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for a in &items[1..] {
                            result = eval(a, env)?;
                            if result == Value::Boolean(false) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for a in &items[1..] {
                            result = eval(a, env)?;
                            if result != Value::Boolean(false) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }
            // General application: evaluate operator and arguments
            let args: Result<Vec<Value>, _> =
                items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            // Try builtin first if operator is a symbol not in env
            if let Value::Symbol(s) = &items[0] {
                if !env.contains_key(s.as_str()) {
                    return apply_builtin(s, &args);
                }
            }
            let func = eval(&items[0], env)?;
            match func {
                Value::Lambda {
                    params,
                    body,
                    env: closed_env,
                } => {
                    if args.len() != params.len() {
                        return Err(EvalError::Arity);
                    }
                    // Start with caller's env, overlay closure env, then params
                    let mut local_env = env.clone();
                    for (k, v) in &closed_env {
                        local_env.insert(k.clone(), v.clone());
                    }
                    for (p, a) in params.iter().zip(args) {
                        local_env.insert(p.clone(), a);
                    }
                    eval(&body, &mut local_env)
                }
                _ => Err(EvalError::NotAProcedure),
            }
        }
    }
}

fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::TypeError("expected integer".into())),
    }
}

fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity);
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_integer(&args[0])?));
            }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                result -= expect_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity);
            }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                let d = expect_integer(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? < expect_integer(&args[1])?))
        }
        ">" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? > expect_integer(&args[1])?))
        }
        "=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? == expect_integer(&args[1])?))
        }
        "<=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? <= expect_integer(&args[1])?))
        }
        ">=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? >= expect_integer(&args[1])?))
        }
        "not" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(args[0] == Value::Boolean(false)))
        }
        _ => Err(EvalError::UndefinedVariable(op.to_string())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut last_result = None;
    let mut env = Env::new();
    while pos < tokens.len() {
        let (val, next) = parse(&tokens, pos)?;
        let result = eval(&val, &mut env)?;
        last_result = Some(result);
        pos = next;
    }
    match last_result {
        Some(v) => Ok(v.to_scheme_string()),
        None => Err(EvalError::Parse("empty input".into())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
