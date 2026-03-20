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

fn parse_expr(input: &str) -> Result<Value, EvalError> {
    let input = input.trim();

    if input == "#t" {
        return Ok(Value::Boolean(true));
    }
    if input == "#f" {
        return Ok(Value::Boolean(false));
    }
    if let Ok(n) = input.parse::<i64>() {
        return Ok(Value::Integer(n));
    }
    if input.starts_with('"') && input.ends_with('"') && input.len() >= 2 {
        let inner = &input[1..input.len() - 1];
        return Ok(Value::Str(inner.to_string()));
    }

    Err(EvalError::Parse(format!("unexpected input: {}", input)))
}

fn eval(input: &str) -> Result<Value, EvalError> {
    let input = input.trim();
    if input.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    // For now, just parse a single atom
    parse_expr(input)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let result = eval(input)?;
    Ok(result.to_scheme_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
