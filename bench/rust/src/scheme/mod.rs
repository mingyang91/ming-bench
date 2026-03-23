pub mod error;

pub use error::EvalError;

mod value;
mod parser;
mod env;
mod eval;

use env::Env;
use eval::eval_top_level;
use parser::Parser;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = Env::default_env();
    let result = eval_top_level(&exprs, &env)?;
    match result {
        value::Value::Void => Ok("".into()),
        other => Ok(other.to_display_string()),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let output_buf = std::rc::Rc::new(std::cell::RefCell::new(String::new()));
    let env = Env::default_env_with_output(output_buf);
    let result = eval_top_level(&exprs, &env)?;
    let output = env.take_output();
    let result_str = match result {
        value::Value::Void => String::new(),
        other => other.to_display_string(),
    };
    Ok((result_str, output))
}

#[cfg(test)]
mod tests;
