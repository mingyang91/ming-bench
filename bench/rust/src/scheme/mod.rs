pub mod error;

pub use error::EvalError;

mod parser;
mod eval;

use eval::Env;

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = Env::default_env();
    let mut last = eval::Value::Void;
    for expr in &exprs {
        last = eval::eval(expr, &env)?;
    }
    Ok(last.to_display())
}

pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
