pub mod error;
mod eval;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let mut last = None;
    for expr in &exprs {
        last = Some(eval::eval(expr)?);
    }
    let result = last.ok_or(EvalError::Parse {
        msg: "empty input".into(),
    })?;
    Ok(result.to_string())
}

#[cfg(test)]
mod tests;
