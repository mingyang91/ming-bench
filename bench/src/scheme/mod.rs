pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::String(s) => format!("\"{s}\""),
        }
    }
}

fn parse(input: &str) -> Result<Value, EvalError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    if trimmed == "#t" {
        return Ok(Value::Boolean(true));
    }
    if trimmed == "#f" {
        return Ok(Value::Boolean(false));
    }

    if let Ok(n) = trimmed.parse::<i64>() {
        return Ok(Value::Integer(n));
    }

    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        let inner = &trimmed[1..trimmed.len() - 1];
        return Ok(Value::String(inner.to_string()));
    }

    Err(EvalError::UnexpectedToken {
        token: trimmed.to_string(),
    })
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let value = parse(input)?;
    Ok(value.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
