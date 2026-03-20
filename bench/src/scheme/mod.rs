mod builtin;
mod cont;
mod env;
mod error;
mod eval;
mod form;
mod parse;
mod syntax;
mod value;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    run_eval(input).map_err(|e| e.to_string())
}

fn run_eval(input: &str) -> Result<String, error::SchemeError> {
    let exprs = parse::parse_all(input)?;
    let global = env::new_env(None);
    builtin::install(&global);
    let result = eval::run(exprs, global)?;
    Ok(format!("{result}"))
}

#[cfg(test)]
mod tests;
