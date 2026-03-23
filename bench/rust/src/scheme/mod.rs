pub mod error;
mod env;
mod eval;
mod parser;
mod value;

use std::cell::RefCell;
use std::rc::Rc;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (result, _) = eval_str_with_output(input)?;
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = eval::default_env();
    let output = Rc::new(RefCell::new(String::new()));
    let last = eval::eval_program(&exprs, &env, &output)?;
    let output_str = output.borrow().clone();
    Ok((last.to_string(), output_str))
}

#[cfg(test)]
mod tests;
