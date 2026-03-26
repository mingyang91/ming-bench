pub mod error;

mod parser;
mod runtime;

pub use error::EvalError;
use runtime::Evaluator;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut evaluator = Evaluator::new();
    evaluator.eval(input).map(|(result, _)| result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut evaluator = Evaluator::new();
    evaluator.eval(input)
}

#[cfg(test)]
mod tests;
