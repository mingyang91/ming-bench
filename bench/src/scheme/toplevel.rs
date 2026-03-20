use std::rc::Rc;

use crate::scheme::continuation;
use crate::scheme::env::Env;
use crate::scheme::error::{self, EvalError};
use crate::scheme::eval;
use crate::scheme::value::Value;

/// Retrieve a captured continuation and conditionally set the resume value.
/// Returns the capture so the caller can replay from `start_index`.
fn resume_continuation(id: u64, value: Value) -> continuation::ContinuationCapture {
    let capture = continuation::get_capture(id).expect("continuation capture not found");
    let current_idx = continuation::current_top_level_index();
    // Only set resume_value when replaying back to an earlier
    // top-level expression (e.g., reentrant loop via saved k).
    // When the continuation was invoked from within the same
    // expression (e.g., coroutine scheduler calling resume thunks),
    // skip resume_value so unrelated call/cc's aren't affected.
    if current_idx.is_none_or(|ci| ci > capture.start_index) {
        continuation::set_resume_value(value);
    }
    capture
}

/// Evaluate a sequence of top-level expressions, handling reentrant continuations.
pub(crate) fn eval_top_level(
    exprs: &[(Value, error::Span)],
    env: &Rc<Env>,
) -> Result<Value, EvalError> {
    let mut current_exprs: Vec<(Value, error::Span)> = exprs.to_vec();
    let mut current_env = Rc::clone(env);
    let mut base_index: usize = 0;

    loop {
        match eval_exprs_with_ctx(&current_exprs, &current_env, base_index) {
            Ok(val) => return Ok(val),
            Err(EvalError::ContinuationReturn { id, value }) => {
                let capture = resume_continuation(id, value);
                base_index = capture.start_index;
                current_exprs = capture.exprs;
                current_env = capture.env;
            }
            Err(e) => return Err(e),
        }
    }
}

/// Evaluate a sequence of top-level expressions, setting continuation context
/// before each one so call/cc can capture remaining expressions.
fn eval_exprs_with_ctx(
    exprs: &[(Value, error::Span)],
    env: &Rc<Env>,
    base_index: usize,
) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for (i, (expr, span)) in exprs.iter().enumerate() {
        continuation::set_top_level_ctx(exprs, i, env);
        // Store absolute index for continuation resume comparison.
        continuation::set_absolute_index(base_index + i);
        last = match eval(expr, env) {
            Ok(v) => v,
            Err(EvalError::ContinuationReturn { id, value }) => {
                return Err(EvalError::ContinuationReturn { id, value });
            }
            Err(e) => {
                return Err(EvalError::AtPosition {
                    error: Box::new(e),
                    line: span.line,
                    col: span.col,
                });
            }
        };
    }
    Ok(last)
}
