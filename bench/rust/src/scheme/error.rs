/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("arity error: {0}")]
    Arity(String),
    #[error("division by zero at {0}")]
    DivisionByZero(String),
    /// Internal: a continuation was invoked, escaping to handler id.
    /// The second field is an opaque boxed value (Box<Value>).
    #[error("continuation escape")]
    #[allow(private_interfaces)]
    ContinuationEscape(usize, Box<crate::scheme::Value>),
    /// A Scheme-level exception raised via `raise`.
    #[error("raised exception")]
    #[allow(private_interfaces)]
    RaisedValue(Box<crate::scheme::Value>),
    #[error("step limit exceeded")]
    StepLimitExceeded,
    /// Internal: CPS trampoline bounce — carries body, env, and continuation
    /// to avoid stack overflow in deep CPS recursion.
    #[error("cps bounce")]
    #[allow(private_interfaces)]
    CpsBounce(Vec<crate::scheme::Expr>, crate::scheme::Env, crate::scheme::ContFn),
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EvalError::Parse(a), EvalError::Parse(b)) => a == b,
            (EvalError::UnboundVariable(a), EvalError::UnboundVariable(b)) => a == b,
            (EvalError::Type(a), EvalError::Type(b)) => a == b,
            (EvalError::Arity(a), EvalError::Arity(b)) => a == b,
            (EvalError::DivisionByZero(a), EvalError::DivisionByZero(b)) => a == b,
            (EvalError::ContinuationEscape(a, _), EvalError::ContinuationEscape(b, _)) => a == b,
            (EvalError::RaisedValue(_), EvalError::RaisedValue(_)) => true,
            (EvalError::StepLimitExceeded, EvalError::StepLimitExceeded) => true,
            (EvalError::CpsBounce(_, _, _), EvalError::CpsBounce(_, _, _)) => true,
            _ => false,
        }
    }
}
