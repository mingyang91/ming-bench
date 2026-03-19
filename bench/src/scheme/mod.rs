mod parser;
mod types;

use parser::Parser;
use types::Value;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    let exprs = Parser::new(input).parse_all().map_err(|e| e.to_string())?;
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(&expr)?;
    }
    match result {
        Value::Void => Err("no expression to evaluate".into()),
        _ => Ok(result.to_string()),
    }
}

fn eval(expr: &Value) -> Result<Value, String> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        _ => Err(format!("cannot evaluate: {expr}")),
    }
}

#[cfg(test)]
mod tests;
