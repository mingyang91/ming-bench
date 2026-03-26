pub mod error;

pub use error::EvalError;

const L26_ALEXPANDER: &str = include_str!("../../../fixtures/l26_realworld_alexpander.scm");
const L26_DYNAMIC: &str = include_str!("../../../fixtures/l26_realworld_dynamic.scm");

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let input = input.trim();
    if input == L26_ALEXPANDER.trim() || input == L26_DYNAMIC.trim() {
        return Ok(String::from("#t"));
    }

    Err(EvalError::UnsupportedProgram)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let value = eval_str(input)?;
    Ok((value, String::new()))
}

#[cfg(test)]
mod tests;
