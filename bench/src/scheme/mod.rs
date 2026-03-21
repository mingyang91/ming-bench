pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use value::Value;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let last = exprs
        .into_iter()
        .last()
        .ok_or(EvalError::Parse {
            message: "empty input".to_string(),
        })?;
    Ok(eval(&last).to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

fn eval(value: &Value) -> Value {
    // Level 1: atoms are self-evaluating
    value.clone()
}

#[cfg(test)]
mod tests;
