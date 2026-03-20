mod env;
pub mod error;
mod eval;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    eval::register_builtins(&env);
    let mut last = None;
    for expr in &exprs {
        let val = eval::eval(expr, &env)?;
        if !is_void(&val) {
            last = Some(val);
        }
    }
    let result = last.ok_or(EvalError::Parse {
        msg: "empty input".into(),
    })?;
    Ok(result.to_string())
}

fn is_void(val: &value::Value) -> bool {
    matches!(val, value::Value::Symbol(s) if s == "void")
}

#[cfg(test)]
mod tests;
