pub mod error;

pub use error::EvalError;

mod parser;
mod eval;
mod value;

use eval::Evaluator;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let mut evaluator = Evaluator::new();
    let mut result = value::Value::Void;
    for expr in &exprs {
        result = evaluator.eval(expr)?;
    }
    match result {
        value::Value::Void => Ok("".into()),
        _ => Ok(result.to_display_string()),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
