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
    eval_top_level(exprs, env)
}

enum EvalSeqResult {
    Done(Value),
    Replay { exprs: Vec<Value>, env: Env },
}

fn eval_seq(exprs: &[Value], env: &Env) -> Result<EvalSeqResult, String> {
    let mut result = Value::Void;
    for i in 0..exprs.len() {
        eval::set_top_level_ctx(exprs[i].clone(), exprs[i + 1..].to_vec(), env.clone());
        match eval::eval(&exprs[i], env) {
            Ok(val) => result = val,
            Err(e) if eval::is_continuation_jump(&e) => {
                return handle_cont_jump(e);
            }
            Err(e) => return Err(e),
        }
    }
    Ok(EvalSeqResult::Done(result))
}

fn handle_cont_jump(err: String) -> Result<EvalSeqResult, String> {
    let (value, replay_expr, remaining, env) = eval::take_cont_jump().ok_or(err)?;
    eval::set_callcc_return(value);
    let mut exprs = vec![replay_expr];
    exprs.extend(remaining);
    Ok(EvalSeqResult::Replay { exprs, env })
}

fn eval_top_level(mut exprs: Vec<Value>, mut env: Env) -> Result<String, String> {
    loop {
        match eval_seq(&exprs, &env)? {
            EvalSeqResult::Done(Value::Void) => return Err("no expression to evaluate".into()),
            EvalSeqResult::Done(val) => return Ok(val.to_string()),
            EvalSeqResult::Replay {
                exprs: new_exprs,
                env: new_env,
            } => {
                exprs = new_exprs;
                env = new_env;
            }
        }
    }
}

#[cfg(test)]
mod tests;
