pub mod error;
pub mod env;
mod eval;
mod parser;
pub mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(error::EvalErrorKind::Parse {
            message: "no expressions".into(),
        }
        .into());
    }
    let env = env::Env::new();
    let mut output = String::new();
    let mut result = None;
    for expr in &exprs {
        result = Some(eval::eval(expr, &env, &mut output)?);
    }
    Ok(result.expect("at least one expression was parsed").to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(error::EvalErrorKind::Parse {
            message: "no expressions".into(),
        }
        .into());
    }
    let env = env::Env::new();
    let mut output = String::new();
    let mut result = None;
    for expr in &exprs {
        result = Some(eval::eval(expr, &env, &mut output)?);
    }
    Ok((result.expect("at least one expression was parsed").to_string(), output))
}

#[cfg(test)]
mod tests;
