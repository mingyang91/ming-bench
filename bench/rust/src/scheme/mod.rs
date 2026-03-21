pub mod error;
mod env;
mod eval;
mod macros;
mod parser;
mod value;

pub use error::EvalError;

use std::cell::RefCell;
use std::rc::Rc;

enum EvalStep {
    Restart(usize),
    Done(value::Value),
}

fn eval_one_expr(
    expr: &value::Value,
    line: usize,
    col: usize,
    idx: usize,
    env: &Rc<RefCell<env::Env>>,
    out: &Rc<RefCell<eval::InterpState>>,
) -> Result<value::Value, EvalError> {
    out.borrow_mut().current_expr_idx = idx;
    eval::eval(expr, env, out).map_err(|e| match e {
        EvalError::ContinuationReturn { .. } | EvalError::RaisedException { .. } => e,
        other => EvalError::Positioned {
            line,
            col,
            inner: Box::new(other),
        },
    })
}

fn handle_continuation(
    id: u64,
    value: value::Value,
    out: &Rc<RefCell<eval::InterpState>>,
) -> usize {
    let st = out.borrow();
    let expr_idx = st
        .cont_captures
        .get(&id)
        .copied()
        .expect("continuation not registered");
    let path = st
        .cont_paths
        .get(&id)
        .cloned()
        .unwrap_or_default();
    drop(st);
    out.borrow_mut().resume = Some((path, value));
    expr_idx
}

fn run_exprs(
    exprs: &[(value::Value, (usize, usize))],
    env: &Rc<RefCell<env::Env>>,
    out: &Rc<RefCell<eval::InterpState>>,
) -> Result<value::Value, EvalError> {
    let mut start_idx = 0;
    loop {
        match eval_expr_sequence(exprs, start_idx, env, out)? {
            EvalStep::Restart(idx) => start_idx = idx,
            EvalStep::Done(val) => return Ok(val),
        }
    }
}

fn eval_expr_sequence(
    exprs: &[(value::Value, (usize, usize))],
    start_idx: usize,
    env: &Rc<RefCell<env::Env>>,
    out: &Rc<RefCell<eval::InterpState>>,
) -> Result<EvalStep, EvalError> {
    let mut result = value::Value::Void;
    for (i, (expr, (line, col))) in exprs.iter().enumerate().skip(start_idx) {
        match eval_one_expr(expr, *line, *col, i, env, out) {
            Ok(val) => {
                result = val;
                // Clear any stale resume that was never consumed during
                // this expression's evaluation.
                out.borrow_mut().resume = None;
            }
            Err(EvalError::ContinuationReturn { id, value }) => {
                out.borrow_mut().body_stack.clear();
                let idx = handle_continuation(id, *value, out);
                return Ok(EvalStep::Restart(idx));
            }
            Err(e) => return Err(e),
        }
    }
    Ok(EvalStep::Done(result))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    eval::seed_builtins(&env);
    let out = Rc::new(RefCell::new(eval::InterpState::new()));
    let result = run_exprs(&exprs, &env, &out)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    eval::seed_builtins(&env);
    let out = Rc::new(RefCell::new(eval::InterpState::new()));
    let result = run_exprs(&exprs, &env, &out)?;
    let output = out.borrow().output.clone();
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
