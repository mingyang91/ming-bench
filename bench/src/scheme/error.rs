use std::fmt;

/// Source position in the input.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error at {1}: {0}")]
    Parse(String, Span),
    #[error("unbound variable at {1}: {0}")]
    UnboundVariable(String, Span),
    #[error("not a procedure at {1}: {0}")]
    NotAProcedure(String, Span),
    #[error("wrong number of arguments at {at}: expected {expected}, got {got}")]
    WrongArgCount { expected: String, got: usize, at: Span },
    #[error("type error at {1}: {0}")]
    TypeError(String, Span),
    #[error("division by zero at {0}")]
    DivisionByZero(Span),
    #[error("continuation invoked")]
    ContinuationReturn,
}
