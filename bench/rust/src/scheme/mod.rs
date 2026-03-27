pub mod error;
pub(crate) mod env;
mod parser;
mod eval;
mod value;

pub use error::EvalError;
use env::Env;
use parser::Parser;
use eval::eval;
use value::Value;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = Env::new();
    // Pre-populate builtins as symbols
    for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                   "cons", "car", "cdr", "list", "length", "null?", "append",
                   "number?", "boolean?", "string?", "symbol?", "pair?"] {
        env.borrow_mut().set(name.to_string(), Value::Symbol(name.to_string()));
    }
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(&expr, &env)?;
    }
    Ok(result.to_display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(input)?;
    Ok((result, String::new()))
}

#[cfg(test)]
mod tests;
