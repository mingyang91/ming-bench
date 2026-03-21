mod ast;
mod builtins;
mod environment;
pub mod error;
mod evaluator;
mod parser;
mod value;

pub use error::EvalError;
use evaluator::{eval_program, eval_program_with_output};
use parser::parse_program;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
///
/// # Errors
///
/// Returns [`EvalError`] when parsing fails or evaluation encounters an error.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let program = parse_program(input)?;
    let value = eval_program(&program)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
///
/// # Errors
///
/// Returns [`EvalError`] when parsing fails or evaluation encounters an error.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let program = parse_program(input)?;
    let (value, output) = eval_program_with_output(&program)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;
