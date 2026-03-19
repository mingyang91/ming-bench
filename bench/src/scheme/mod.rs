mod error;
mod evaluator;
mod parser;
mod value;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    evaluator::eval_str(input).map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests;
