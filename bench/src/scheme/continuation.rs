use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

static CONT_COUNTER: AtomicU64 = AtomicU64::new(1);

pub(crate) fn next_cont_id() -> u64 {
    CONT_COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Captured state for a reentrant continuation.
pub(crate) struct ContinuationCapture {
    pub exprs: Vec<(Value, Span)>,
    pub env: Rc<Env>,
    pub start_index: usize,
}

thread_local! {
    /// Registry of captured continuation contexts (for reentrant invocation).
    static CAPTURES: RefCell<HashMap<u64, ContinuationCapture>> = RefCell::new(HashMap::new());
    /// Pending resume value for the next call/cc re-execution.
    static RESUME_VALUE: RefCell<Option<Value>> = const { RefCell::new(None) };
    /// Current top-level expression context (remaining exprs + env).
    static TOP_LEVEL_CTX: RefCell<Option<TopLevelCtx>> = const { RefCell::new(None) };
    /// Absolute index of the currently evaluating top-level expression.
    static ABSOLUTE_INDEX: RefCell<Option<usize>> = const { RefCell::new(None) };
}

struct TopLevelCtx {
    all_exprs: Vec<(Value, Span)>,
    current_index: usize,
    env: Rc<Env>,
}

/// Called by eval_top_level before evaluating each top-level expression
/// to set the context that call/cc can capture.
pub(crate) fn set_top_level_ctx(
    all_exprs: &[(Value, Span)],
    current_index: usize,
    env: &Rc<Env>,
) {
    TOP_LEVEL_CTX.with(|ctx| {
        *ctx.borrow_mut() = Some(TopLevelCtx {
            all_exprs: all_exprs.to_vec(),
            current_index,
            env: Rc::clone(env),
        });
    });
}

/// Register a capture for a continuation, using the current top-level context.
pub(crate) fn register_capture_from_ctx(id: u64) {
    let capture = TOP_LEVEL_CTX.with(|ctx| {
        let ctx = ctx.borrow();
        ctx.as_ref().map(|tlc| ContinuationCapture {
            exprs: tlc.all_exprs[tlc.current_index..].to_vec(),
            env: Rc::clone(&tlc.env),
            start_index: tlc.current_index,
        })
    });
    if let Some(cap) = capture {
        CAPTURES.with(|c| c.borrow_mut().insert(id, cap));
    }
}

/// Get a clone of the capture for a continuation (keeps it in the registry).
pub(crate) fn get_capture(id: u64) -> Option<ContinuationCapture> {
    CAPTURES.with(|c| {
        c.borrow().get(&id).map(|cap| ContinuationCapture {
            exprs: cap.exprs.clone(),
            env: Rc::clone(&cap.env),
            start_index: cap.start_index,
        })
    })
}

/// Return the absolute index of the top-level expression currently being evaluated.
pub(crate) fn current_top_level_index() -> Option<usize> {
    ABSOLUTE_INDEX.with(|idx| *idx.borrow())
}

/// Set the absolute index of the currently evaluating top-level expression.
pub(crate) fn set_absolute_index(idx: usize) {
    ABSOLUTE_INDEX.with(|ai| *ai.borrow_mut() = Some(idx));
}

pub(crate) fn set_resume_value(val: Value) {
    RESUME_VALUE.with(|r| *r.borrow_mut() = Some(val));
}

pub(crate) fn take_resume_value() -> Option<Value> {
    RESUME_VALUE.with(|r| r.borrow_mut().take())
}

/// Execute call/cc: create a continuation, call proc with it, catch escape returns.
pub(crate) fn eval_callcc(proc: Value) -> Result<Value, EvalError> {
    // If we're resuming a continuation, return the resume value directly.
    if let Some(val) = take_resume_value() {
        return Ok(val);
    }

    let id = next_cont_id();
    let cont = Value::Continuation(id);

    // Register capture for potential reentrant invocation.
    register_capture_from_ctx(id);

    // Call proc(k). Catch ContinuationReturn if it's our continuation (escape).
    match crate::scheme::apply::apply_lambda(proc, &[cont]) {
        Ok(val) => Ok(val),
        Err(EvalError::ContinuationReturn {
            id: ret_id,
            value,
        }) if ret_id == id => Ok(value),
        Err(e) => Err(e),
    }
}
