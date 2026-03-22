pub mod error;
mod env;
mod eval;
mod parser;
pub mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let global = env::Env::new();
    let mut out = String::new();
    let cc = eval::CcCtx::new();
    let last = eval::eval_toplevel(&exprs, &global, &mut out, &cc)?;
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let global = env::Env::new();
    let mut out = String::new();
    let cc = eval::CcCtx::new();
    let last = eval::eval_toplevel(&exprs, &global, &mut out, &cc)?;
    Ok((last.to_string(), out))
}

#[cfg(test)]
mod tests;
