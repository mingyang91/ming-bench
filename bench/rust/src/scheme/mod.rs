pub mod error;

pub use error::EvalError;

mod value;
mod parser;
mod env;
mod eval;

use env::Env;
use eval::eval_expr;
use parser::Parser;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = Env::default_env();
    let mut result = value::Value::Void;
    for expr in exprs {
        result = eval_expr(&expr, &env)?;
    }
    match result {
        value::Value::Void => Ok("".into()),
        other => Ok(other.to_display_string()),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
