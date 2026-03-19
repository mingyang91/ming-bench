mod eval;
mod macros;
mod parser;
mod types;

use parser::Parser;
use types::{Env, Value};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, String> {
    let exprs = Parser::new(input).parse_all().map_err(|e| e.to_string())?;
    let env = Env::new();
    match eval_seq(&exprs, &env)? {
        Value::Void => Err("no expression to evaluate".into()),
        val => Ok(val.to_string()),
    }
}

fn eval_seq(exprs: &[Value], env: &Env) -> Result<Value, String> {
    let mut result = Value::Void;
    for i in 0..exprs.len() {
        eval::set_top_level_ctx(exprs[i].clone(), exprs[i + 1..].to_vec(), env.clone());
        match eval::eval(&exprs[i], env) {
            Ok(val) => result = val,
            Err(e) if eval::is_continuation_jump(&e) => {
                return eval::dispatch_and_resolve(e);
            }
            Err(e) => return Err(e),
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests;
