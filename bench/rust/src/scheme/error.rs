/// Source position (1-based line and column).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
#[allow(private_interfaces)]
pub enum EvalError {
    #[error("parse error at {1}: {0}")]
    Parse(String, Span),
    #[error("type error at {1}: {0}")]
    Type(String, Span),
    #[error("arity error at {1}: {0}")]
    Arity(String, Span),
    #[error("unbound variable at {1}: {0}")]
    UnboundVariable(String, Span),
    #[error("division by zero at {0}")]
    DivisionByZero(Span),
    #[error("internal: continuation invoked")]
    ContinuationInvoked,
    #[error("uncaught exception: {0}")]
    SchemeRaise(String),
    #[error("step limit exceeded")]
    StepLimitExceeded,
}
