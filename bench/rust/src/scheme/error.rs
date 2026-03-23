use std::fmt;

/// Source position in the input.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
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
    #[error("parse error at {span}: {message}")]
    Parse { message: String, span: Span },

    #[error("type mismatch at {span}: expected {expected}, got {got}")]
    TypeMismatch {
        expected: String,
        got: String,
        span: Span,
    },

    #[error("unbound variable at {span}: {name}")]
    UnboundVariable { name: String, span: Span },

    #[error("wrong number of arguments at {span}: expected {expected}, got {got}")]
    WrongArgCount {
        expected: String,
        got: usize,
        span: Span,
    },

    #[error("division by zero at {span}")]
    DivisionByZero { span: Span },

    #[error("not a procedure at {span}: {value}")]
    NotAProcedure { value: String, span: Span },
}
