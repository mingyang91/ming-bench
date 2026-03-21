pub mod error;
pub mod env;
mod eval;
mod macros;
mod parser;
mod syntax_case;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let result = eval_exprs(&exprs, &env)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let result = eval_exprs(&exprs, &env)?;
    let output = env.take_output();
    Ok((result.to_string(), output))
}

/// Evaluate a sequence of expressions, handling continuation re-execution.
fn eval_exprs(
    exprs: &[parser::Expr],
    env: &env::Env,
) -> Result<value::Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut start = 0;
    let mut result = value::Value::Void;
    loop {
        env.reset_callcc_counter();
        match eval_exprs_range(exprs, env, start, &mut result)? {
            Some(idx) => start = idx,
            None => return Ok(result),
        }
    }
}

/// Evaluate expressions from `start`, returning `Some(restart_index)` on
/// continuation capture or `None` when all expressions have been evaluated.
fn eval_exprs_range(
    exprs: &[parser::Expr],
    env: &env::Env,
    start: usize,
    result: &mut value::Value,
) -> Result<Option<usize>, EvalError> {
    for (i, expr) in exprs.iter().enumerate().skip(start) {
        env.set_current_expr_index(i);
        match eval::eval(expr, env) {
            Ok(val) => *result = val,
            Err(EvalError::ContinuationReturn {
                cont_id,
                value,
                expr_index,
            }) => {
                env.set_pending_cont(cont_id, *value);
                return Ok(Some(expr_index));
            }
            Err(e) => return Err(e),
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests;
