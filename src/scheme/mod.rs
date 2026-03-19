/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(_input: &str) -> Result<String, String> {
    todo!()
}

#[cfg(test)]
mod tests;
