/// Evaluation error type for the Scheme interpreter.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{message} at {line}:{col}")]
    WithPosition {
        message: String,
        line: usize,
        col: usize,
    },

    #[error("{message}")]
    Message { message: String },
}

impl EvalError {
    pub(crate) fn with_position(message: impl Into<String>, line: usize, col: usize) -> Self {
        Self::WithPosition {
            message: message.into(),
            line,
            col,
        }
    }

    pub(crate) fn message(message: impl Into<String>) -> Self {
        Self::Message {
            message: message.into(),
        }
    }
}

pub(crate) type EvalResult<T> = Result<T, EvalError>;
