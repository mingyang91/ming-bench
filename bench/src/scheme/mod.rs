pub mod error;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let last = exprs
        .last()
        .ok_or(EvalError::Parse {
            msg: "empty input".into(),
        })?;
    Ok(last.to_string())
}

#[cfg(test)]
mod tests;
