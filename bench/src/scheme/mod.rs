use std::cell::RefCell;

pub mod error;
mod env;
mod eval;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let output = RefCell::new(String::new());
    let mut last = value::Value::Void;
    for (expr, span) in &exprs {
        last = eval::eval(expr, &env, *span, &output)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let output = RefCell::new(String::new());
    let mut last = value::Value::Void;
    for (expr, span) in &exprs {
        last = eval::eval(expr, &env, *span, &output)?;
    }
    Ok((last.to_string(), output.into_inner()))
}

#[cfg(test)]
mod tests;
