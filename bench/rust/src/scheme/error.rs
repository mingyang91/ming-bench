/// Source position in the input.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),

    #[error("unknown operator: {name}")]
    UnknownOperator { name: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("{inner} at {line}:{col}")]
    Positioned {
        inner: Box<EvalError>,
        line: usize,
        col: usize,
    },
}

impl EvalError {
    pub fn at(self, span: Span) -> Self {
        if matches!(self, EvalError::Positioned { .. }) {
            return self;
        }
        EvalError::Positioned {
            inner: Box::new(self),
            line: span.line,
            col: span.col,
        }
    }
}
