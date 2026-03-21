pub mod error;
mod env;
mod eval;
mod parser;
mod value;

pub use error::EvalError;

use std::cell::RefCell;
use std::rc::Rc;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let out = Rc::new(RefCell::new(String::new()));
    let mut result = value::Value::Void;
    for (ref expr, (line, col)) in exprs {
        result = eval::eval(expr, &env, &out).map_err(|e| EvalError::Positioned {
            line,
            col,
            inner: Box::new(e),
        })?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let out = Rc::new(RefCell::new(String::new()));
    let mut result = value::Value::Void;
    for (ref expr, (line, col)) in exprs {
        result = eval::eval(expr, &env, &out).map_err(|e| EvalError::Positioned {
            line,
            col,
            inner: Box::new(e),
        })?;
    }
    let output = out.borrow().clone();
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
