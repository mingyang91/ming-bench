mod builtins;
mod expr;
mod forms;
mod parser;
mod trampoline;

use builtins::BUILTIN_NAMES;
use expr::{Env, Expr};
use parser::Parser;
use trampoline::{
    clear_continuation_state, eval_step, run_top_level, Bounce,
};

pub fn eval_str(input: &str) -> Result<String, String> {
    let tokens = parser::tokenize(input)?;
    let mut parser = Parser::new(&tokens);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err("no expression".into());
    }
    let env = Env::new();
    for &name in BUILTIN_NAMES {
        env.insert(name.to_string(), Expr::Builtin(name.to_string()));
    }
    env.insert("call/cc".to_string(), Expr::Builtin("call/cc".to_string()));
    env.insert(
        "call-with-current-continuation".to_string(),
        Expr::Builtin("call/cc".to_string()),
    );
    clear_continuation_state();
    run_top_level(&exprs, &env)
}

pub(crate) fn eval(expr: &Expr, env: &Env) -> Result<Expr, String> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match eval_step(&current_expr, &current_env)? {
            Bounce::Done(val) => return Ok(val),
            Bounce::TailCall { expr, env } => {
                current_expr = expr;
                current_env = env;
            }
        }
    }
}

#[cfg(test)]
mod tests;
