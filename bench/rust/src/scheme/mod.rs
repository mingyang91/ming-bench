pub mod error;
pub(crate) mod env;
mod eval;
mod macros;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let last = eval::eval_body(&exprs, &env)?;
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let last = eval::eval_body(&exprs, &env)?;
    let output = env.take_output();
    Ok((last.to_string(), output))
}

#[cfg(test)]
mod tests;
