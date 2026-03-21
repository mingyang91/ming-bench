pub mod error;
pub mod env;
mod eval;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let mut result = None;
    for expr in &exprs {
        result = Some(eval::eval(expr, &env)?);
    }
    result
        .map(|v| v.to_string())
        .ok_or_else(|| EvalError::Parse("empty input".into()))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(input)?;
    Ok((result, String::new()))
}

#[cfg(test)]
mod tests;
