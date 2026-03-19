mod eval;
mod parser;
mod types;

use parser::Parser;
use types::{Env, Value};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, String> {
    let exprs = Parser::new(input).parse_all().map_err(|e| e.to_string())?;
    let env = Env::new();
    let mut result = Value::Void;
    for expr in exprs {
        result = eval::eval(&expr, &env)?;
    }
    match result {
        Value::Void => Err("no expression to evaluate".into()),
        _ => Ok(result.to_string()),
    }
}

#[cfg(test)]
mod tests;
