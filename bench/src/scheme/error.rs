use std::fmt;

/// Source position in the input.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{span}: parse error: {message}")]
    Parse { message: String, span: Span },

    #[error("{span}: type error: {message}")]
    TypeError { message: String, span: Span },

    #[error("{span}: unbound variable: {name}")]
    UnboundVariable { name: String, span: Span },

    #[error("{span}: wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount {
        expected: usize,
        got: usize,
        span: Span,
    },

    #[error("{span}: division by zero")]
    DivisionByZero { span: Span },

    #[error("continuation return")]
    ContinuationReturn,
}
