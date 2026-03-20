pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use value::Value;

/// Evaluate a single parsed Scheme value.
fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(_) | Value::List(_) => Err(EvalError::Parse {
            message: format!("unsupported expression: {expr}"),
        }),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = parser::parse(input)?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }
    let result = expressions
        .iter()
        .map(eval)
        .next_back()
        .expect("non-empty expressions guaranteed above")?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
