use crate::scheme::value::Span;

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {msg}")]
    Parse { msg: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type mismatch: expected {expected}, got {got}")]
    TypeMismatch { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },

    #[error("string is immutable")]
    ImmutableString,

    #[error("{line}:{col}: {inner}")]
    WithPosition {
        line: usize,
        col: usize,
        inner: Box<EvalError>,
    },
}

impl EvalError {
    pub fn at(self, span: Option<Span>) -> Self {
        match span {
            Some(s) => match self {
                EvalError::WithPosition { .. } => self,
                _ => EvalError::WithPosition {
                    line: s.line,
                    col: s.col,
                    inner: Box::new(self),
                },
            },
            None => self,
        }
    }
}
