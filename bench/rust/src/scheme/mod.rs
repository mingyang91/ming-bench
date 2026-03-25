mod builtins;
mod core;
pub mod error;
mod eval;
mod macros;
mod number;
mod parser;

pub use error::EvalError;

use core::Runtime;
use eval::eval_program;
use parser::parse_program;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (result, _) = evaluate(input)?;
    Ok(result.render())
}

/// Evaluate Scheme expressions with a maximum number of eval dispatches.
pub fn eval_str_with_limit(input: &str, max_steps: usize) -> Result<String, EvalError> {
    let (result, _) = evaluate_with_runtime(input, Runtime::with_step_limit(max_steps))?;
    Ok(result.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (result, runtime) = evaluate(input)?;
    Ok((result.render_display(), runtime.into_output()))
}

fn evaluate(input: &str) -> Result<(core::Value, Runtime), EvalError> {
    evaluate_with_runtime(input, Runtime::default())
}

fn evaluate_with_runtime(input: &str, mut runtime: Runtime) -> Result<(core::Value, Runtime), EvalError> {
    let expressions = parse_program(input)?;
    let result = eval_program(&expressions, &mut runtime)?;
    Ok((result, runtime))
}

#[cfg(test)]
mod tests;
