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
    eval::eval_program(exprs, env)
}

#[cfg(test)]
mod tests;
