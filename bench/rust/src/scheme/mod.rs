mod env;
pub mod error;
mod expr;
mod interpreter;
mod reader;
mod source;
mod value;

pub use error::EvalError;
use interpreter::evaluate_program;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    evaluate_program(input).map(|(value, _)| value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    evaluate_program(input).map(|(value, output)| (value.render(), output))
}

#[cfg(test)]
mod tests;
