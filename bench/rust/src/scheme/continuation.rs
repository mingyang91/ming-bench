use std::cell::RefCell;
use std::rc::Rc;

use super::macros::MacroEnvRef;
use super::{BindingRef, EnvRef, EvalError, Expr, SourcePos, Value};

pub(super) type ContinuationRef = Rc<Continuation>;
pub(super) type DynamicWindFrameRef = Rc<DynamicWindFrame>;
pub(super) type ContinuationHandleRef = Rc<RefCell<ContinuationHandleState>>;
pub(super) type EvalResult<T> = Result<T, EvalSignal>;

#[derive(Clone)]
pub(super) struct RaisedException {
    pub(super) value: Value,
    pub(super) position: Option<SourcePos>,
}

#[derive(Clone)]
pub(super) struct CapturedContinuation {
    pub(super) continuation: ContinuationRef,
    pub(super) wind_stack: Vec<DynamicWindFrameRef>,
}

#[derive(Clone)]
pub(super) enum ContinuationHandleState {
    Active(Rc<CapturedContinuation>),
    Expired,
}

#[derive(Clone)]
pub(super) struct DynamicWindFrame {
    pub(super) before: Value,
    pub(super) before_position: SourcePos,
    pub(super) after: Value,
    pub(super) after_position: SourcePos,
}

thread_local! {
    static CURRENT_WIND_STACK: RefCell<Vec<DynamicWindFrameRef>> = const { RefCell::new(Vec::new()) };
}

pub(super) struct WindStackGuard {
    saved: Vec<DynamicWindFrameRef>,
}

impl Drop for WindStackGuard {
    fn drop(&mut self) {
        CURRENT_WIND_STACK.with(|stack| {
            *stack.borrow_mut() = std::mem::take(&mut self.saved);
        });
    }
}

#[derive(Clone)]
pub(super) enum Continuation {
    Final,
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
        macro_env: MacroEnvRef,
        next: ContinuationRef,
    },
    Define {
        env: EnvRef,
        name: String,
        next: ContinuationRef,
    },
    SetSymbol {
        env: EnvRef,
        name: String,
        next: ContinuationRef,
    },
    SetCaptured {
        binding: BindingRef,
        next: ContinuationRef,
    },
    Application {
        callable: Value,
        pending_args: Vec<Expr>,
        evaluated_suffix: Vec<Value>,
        env: EnvRef,
        macro_env: MacroEnvRef,
        head_position: SourcePos,
        next: ContinuationRef,
    },
    CallWithValues {
        consumer: Value,
        next: ContinuationRef,
    },
    DynamicWindEnter {
        frame: DynamicWindFrameRef,
        body: Value,
        body_position: SourcePos,
        next: ContinuationRef,
    },
    DynamicWind {
        frame: DynamicWindFrameRef,
        next: ContinuationRef,
    },
    DynamicWindExit {
        return_value: Value,
        next: ContinuationRef,
    },
    WindTransition {
        saved_value: Value,
        remaining_exits: Vec<DynamicWindFrameRef>,
        remaining_entries: Vec<DynamicWindFrameRef>,
        target: ContinuationRef,
    },
    WindTransitionEnter {
        frame: DynamicWindFrameRef,
        saved_value: Value,
        remaining_entries: Vec<DynamicWindFrameRef>,
        target: ContinuationRef,
    },
}

pub(super) enum EvalSignal {
    Error(EvalError),
    Raise(RaisedException),
    Jump {
        continuation: ContinuationRef,
        wind_stack: Vec<DynamicWindFrameRef>,
        value: Value,
    },
}

impl From<EvalError> for EvalSignal {
    fn from(error: EvalError) -> Self {
        Self::Error(error)
    }
}

impl EvalSignal {
    pub(super) fn with_position(self, position: SourcePos) -> Self {
        match self {
            Self::Error(error) => Self::Error(error.with_position(position)),
            Self::Raise(mut exception) => {
                if exception.position.is_none() {
                    exception.position = Some(position);
                }
                Self::Raise(exception)
            }
            Self::Jump {
                continuation,
                wind_stack,
                value,
            } => Self::Jump {
                continuation,
                wind_stack,
                value,
            },
        }
    }
}

impl RaisedException {
    pub(super) fn new(value: Value) -> Self {
        Self {
            value,
            position: None,
        }
    }
}

pub(super) fn final_continuation() -> ContinuationRef {
    Rc::new(Continuation::Final)
}

pub(super) fn reset_wind_stack() -> WindStackGuard {
    let saved = CURRENT_WIND_STACK.with(|stack| std::mem::take(&mut *stack.borrow_mut()));
    WindStackGuard { saved }
}

pub(super) fn current_wind_stack() -> Vec<DynamicWindFrameRef> {
    CURRENT_WIND_STACK.with(|stack| stack.borrow().clone())
}

pub(super) fn push_wind_frame(frame: &DynamicWindFrameRef) {
    CURRENT_WIND_STACK.with(|stack| {
        stack.borrow_mut().push(Rc::clone(frame));
    });
}

pub(super) fn pop_wind_frame(frame: &DynamicWindFrameRef) {
    CURRENT_WIND_STACK.with(|stack| {
        let popped = stack
            .borrow_mut()
            .pop()
            .expect("dynamic-wind stack underflow");
        assert!(
            Rc::ptr_eq(&popped, frame),
            "dynamic-wind stack mismatch while unwinding"
        );
    });
}

pub(super) fn make_dynamic_wind_frame(
    before: Value,
    before_position: SourcePos,
    after: Value,
    after_position: SourcePos,
) -> DynamicWindFrameRef {
    Rc::new(DynamicWindFrame {
        before,
        before_position,
        after,
        after_position,
    })
}

pub(super) fn invoke_continuation(
    continuation: Rc<CapturedContinuation>,
    value: Value,
) -> EvalSignal {
    EvalSignal::Jump {
        continuation: Rc::clone(&continuation.continuation),
        wind_stack: continuation.wind_stack.clone(),
        value,
    }
}

fn capture_current_continuation(continuation: &ContinuationRef) -> Rc<CapturedContinuation> {
    Rc::new(CapturedContinuation {
        continuation: Rc::clone(continuation),
        wind_stack: current_wind_stack(),
    })
}

pub(super) fn current_continuation_handle(
    continuation: &ContinuationRef,
) -> ContinuationHandleRef {
    Rc::new(RefCell::new(ContinuationHandleState::Active(
        capture_current_continuation(continuation),
    )))
}

pub(super) fn snapshot_continuation_handle(
    handle: &ContinuationHandleRef,
) -> Option<Rc<CapturedContinuation>> {
    match &*handle.borrow() {
        ContinuationHandleState::Active(continuation) => Some(Rc::clone(continuation)),
        ContinuationHandleState::Expired => None,
    }
}

pub(super) fn expire_continuation_handle(handle: &ContinuationHandleRef) {
    *handle.borrow_mut() = ContinuationHandleState::Expired;
}

pub(super) fn sequence_continuation(
    remaining: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    next: &ContinuationRef,
) -> ContinuationRef {
    Rc::new(Continuation::Sequence {
        remaining: remaining.to_vec(),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
        next: Rc::clone(next),
    })
}

pub(super) fn define_continuation(
    env: &EnvRef,
    name: String,
    next: &ContinuationRef,
) -> ContinuationRef {
    Rc::new(Continuation::Define {
        env: Rc::clone(env),
        name,
        next: Rc::clone(next),
    })
}

pub(super) fn set_symbol_continuation(
    env: &EnvRef,
    name: String,
    next: &ContinuationRef,
) -> ContinuationRef {
    Rc::new(Continuation::SetSymbol {
        env: Rc::clone(env),
        name,
        next: Rc::clone(next),
    })
}

pub(super) fn set_captured_continuation(
    binding: &BindingRef,
    next: &ContinuationRef,
) -> ContinuationRef {
    Rc::new(Continuation::SetCaptured {
        binding: Rc::clone(binding),
        next: Rc::clone(next),
    })
}

pub(super) fn resume_continuation_jump(
    continuation: ContinuationRef,
    wind_stack: Vec<DynamicWindFrameRef>,
    value: Value,
) -> (ContinuationRef, Value) {
    let current_stack = current_wind_stack();
    let shared_depth = current_stack
        .iter()
        .zip(wind_stack.iter())
        .take_while(|(lhs, rhs)| Rc::ptr_eq(lhs, rhs))
        .count();

    let remaining_exits = current_stack[shared_depth..].to_vec();
    let remaining_entries = wind_stack[shared_depth..]
        .iter()
        .rev()
        .cloned()
        .collect::<Vec<_>>();

    if remaining_exits.is_empty() && remaining_entries.is_empty() {
        (continuation, value)
    } else {
        (
            Rc::new(Continuation::WindTransition {
                saved_value: value,
                remaining_exits,
                remaining_entries,
                target: continuation,
            }),
            Value::Void,
        )
    }
}
