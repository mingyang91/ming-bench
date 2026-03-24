pub mod error;

pub use error::EvalError;

mod parser;
mod value;
mod env;
mod eval;

use env::Env;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = Env::default_env();
    let mut result = value::Value::Void;
    for expr in exprs {
        result = eval::eval(&expr, &env)?;
    }
    Ok(result.to_display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
