use std::rc::Rc;

use super::macros::MacroEnvRef;
use super::{BindingRef, EnvRef, EvalError, Expr, SourcePos, Value};

pub(super) type ContinuationRef = Rc<Continuation>;
pub(super) type EvalResult<T> = Result<T, EvalSignal>;

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
}

pub(super) enum EvalSignal {
    Error(EvalError),
    Jump {
        continuation: ContinuationRef,
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
            Self::Jump {
                continuation,
                value,
            } => Self::Jump {
                continuation,
                value,
            },
        }
    }
}

pub(super) fn final_continuation() -> ContinuationRef {
    Rc::new(Continuation::Final)
}

pub(super) fn invoke_continuation(continuation: ContinuationRef, value: Value) -> EvalSignal {
    EvalSignal::Jump {
        continuation,
        value,
    }
}

pub(super) fn current_continuation_value(continuation: &ContinuationRef) -> Value {
    Value::Continuation(Rc::clone(continuation))
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

pub(super) fn define_continuation(env: &EnvRef, name: String, next: &ContinuationRef) -> ContinuationRef {
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
