pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", parts.join(" "))
            }
        }
    }
}

fn parse_atom(input: &str) -> Result<Value, EvalError> {
    if let Ok(n) = input.parse::<i64>() {
        return Ok(Value::Integer(n));
    }
    if input == "#t" {
        return Ok(Value::Boolean(true));
    }
    if input == "#f" {
        return Ok(Value::Boolean(false));
    }
    Ok(Value::Symbol(input.to_string()))
}

fn parse_string(input: &str) -> Result<(Value, &str), EvalError> {
    // input starts after opening "
    let mut chars = input.char_indices();
    while let Some((i, ch)) = chars.next() {
        if ch == '\\' {
            chars.next(); // skip escaped char
        } else if ch == '"' {
            let s = &input[..i];
            return Ok((Value::Str(s.to_string()), &input[i + 1..]));
        }
    }
    Err(EvalError::Parse("unterminated string".to_string()))
}

fn parse_expr(input: &str) -> Result<(Value, &str), EvalError> {
    let input = input.trim_start();
    if input.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    if input.starts_with('"') {
        return parse_string(&input[1..]);
    }
    if input.starts_with('(') {
        return parse_list(&input[1..]);
    }
    // Read a token up to whitespace or end
    let end = input
        .find(|c: char| c.is_whitespace() || c == '(' || c == ')' || c == '"')
        .unwrap_or(input.len());
    let token = &input[..end];
    let rest = &input[end..];
    Ok((parse_atom(token)?, rest))
}

fn parse_list(input: &str) -> Result<(Value, &str), EvalError> {
    let mut items = Vec::new();
    let mut rest = input;
    loop {
        rest = rest.trim_start();
        if rest.is_empty() {
            return Err(EvalError::Parse("unterminated list".to_string()));
        }
        if rest.starts_with(')') {
            return Ok((Value::List(items), &rest[1..]));
        }
        let (val, r) = parse_expr(rest)?;
        items.push(val);
        rest = r;
    }
}

fn eval(val: Value) -> Result<Value, EvalError> {
    match val {
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".to_string()));
            }
            let func = &items[0];
            match func {
                Value::Symbol(name) => {
                    // Special forms with short-circuit semantics
                    match name.as_str() {
                        "and" => return eval_and(&items[1..]),
                        "or" => return eval_or(&items[1..]),
                        _ => {}
                    }
                    apply_builtin(name, &items[1..])
                }
                _ => Err(EvalError::Parse(format!(
                    "not a procedure: {}",
                    func.to_scheme_string()
                ))),
            }
        }
        other => Ok(other),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    let eval_args: Vec<Value> = args.iter().map(|a| eval(a.clone())).collect::<Result<_, _>>()?;
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in &eval_args {
                sum += expect_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if eval_args.is_empty() {
                return Err(EvalError::Parse("- requires at least 1 argument".to_string()));
            }
            if eval_args.len() == 1 {
                return Ok(Value::Integer(-expect_integer(&eval_args[0])?));
            }
            let mut result = expect_integer(&eval_args[0])?;
            for a in &eval_args[1..] {
                result -= expect_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in &eval_args {
                product *= expect_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if eval_args.is_empty() {
                return Err(EvalError::Parse("/ requires at least 1 argument".to_string()));
            }
            let mut result = expect_integer(&eval_args[0])?;
            for a in &eval_args[1..] {
                let d = expect_integer(a)?;
                if d == 0 {
                    return Err(EvalError::Parse("division by zero".to_string()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if eval_args.len() != 2 {
                return Err(EvalError::Parse("< requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_integer(&eval_args[0])? < expect_integer(&eval_args[1])?))
        }
        ">" => {
            if eval_args.len() != 2 {
                return Err(EvalError::Parse("> requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_integer(&eval_args[0])? > expect_integer(&eval_args[1])?))
        }
        "=" => {
            if eval_args.len() != 2 {
                return Err(EvalError::Parse("= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_integer(&eval_args[0])? == expect_integer(&eval_args[1])?))
        }
        "<=" => {
            if eval_args.len() != 2 {
                return Err(EvalError::Parse("<= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_integer(&eval_args[0])? <= expect_integer(&eval_args[1])?))
        }
        "not" => {
            if eval_args.len() != 1 {
                return Err(EvalError::Parse("not requires 1 argument".to_string()));
            }
            Ok(Value::Boolean(is_falsy(&eval_args[0])))
        }
        _ => Err(EvalError::Parse(format!("unknown procedure: {}", name))),
    }
}

fn is_falsy(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn eval_and(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg.clone())?;
        if is_falsy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg.clone())?;
        if !is_falsy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn expect_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::Parse(format!(
            "expected integer, got {}",
            other.to_scheme_string()
        ))),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut remaining = input;
    let mut last_value = None;
    while !remaining.trim().is_empty() {
        let (val, rest) = parse_expr(remaining)?;
        last_value = Some(eval(val)?);
        remaining = rest;
    }
    match last_value {
        Some(v) => Ok(v.to_scheme_string()),
        None => Err(EvalError::Parse("empty input".to_string())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
