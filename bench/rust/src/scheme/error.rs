/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, Clone, thiserror::Error)]
pub enum EvalError {
    #[error("{message}")]
    Message { message: String },

    #[error("{message} at {line}:{column}")]
    WithPosition {
        message: String,
        line: usize,
        column: usize,
    },
}

impl EvalError {
    pub fn message(message: impl Into<String>) -> Self {
        Self::Message {
            message: message.into(),
        }
    }

    pub fn with_position(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::WithPosition {
            message: message.into(),
            line,
            column,
        }
    }
}
