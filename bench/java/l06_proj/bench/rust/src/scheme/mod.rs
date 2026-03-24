mod environment;
mod expr;
mod interpreter;
mod parser;
mod position;
mod value;

pub mod error;

pub use error::EvalError;

use interpreter::Interpreter;
use parser::Parser;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    Ok(eval_str_with_output(input)?.0)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let program = Parser::new(input).parse_program()?;
    let mut interpreter = Interpreter::new();
    let result = interpreter.eval_program(&program)?;
    Ok((result.render(), interpreter.captured_output()))
}

#[cfg(test)]
mod tests;
