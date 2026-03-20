pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
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
    Err(EvalError::Parse(format!("unexpected token: {}", input)))
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
    // Read a token up to whitespace or end
    let end = input
        .find(|c: char| c.is_whitespace() || c == '(' || c == ')' || c == '"')
        .unwrap_or(input.len());
    let token = &input[..end];
    let rest = &input[end..];
    Ok((parse_atom(token)?, rest))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut remaining = input;
    let mut last_value = None;
    while !remaining.trim().is_empty() {
        let (val, rest) = parse_expr(remaining)?;
        last_value = Some(val);
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
