mod engine;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    engine::eval_program(input).map(|value| value.to_string())
}

#[cfg(test)]
mod tests;
