use crate::scheme::source::SourcePos;

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error("{message}")]
    Message { message: String },

    #[error("{line}:{column}: {message}")]
    At {
        line: usize,
        column: usize,
        message: String,
    },
}

impl EvalError {
    pub fn message(message: impl Into<String>) -> Self {
        Self::Message {
            message: message.into(),
        }
    }

    pub fn at(position: SourcePos, message: impl Into<String>) -> Self {
        Self::At {
            line: position.line,
            column: position.column,
            message: message.into(),
        }
    }
}
