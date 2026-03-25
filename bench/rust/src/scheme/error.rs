/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{message}")]
    Message { message: String },
    #[error("{message} at {line}:{column}")]
    MessageWithPosition {
        message: String,
        line: usize,
        column: usize,
    },
}

impl EvalError {
    pub fn new(message: impl Into<String>) -> Self {
        Self::Message {
            message: message.into(),
        }
    }

    pub fn at(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::MessageWithPosition {
            message: message.into(),
            line,
            column,
        }
    }

    pub fn has_position(&self) -> bool {
        matches!(self, Self::MessageWithPosition { .. })
    }

    pub fn with_position(self, line: usize, column: usize) -> Self {
        match self {
            Self::Message { message } => Self::MessageWithPosition {
                message,
                line,
                column,
            },
            positioned => positioned,
        }
    }

    pub fn detail(&self) -> &str {
        match self {
            Self::Message { message } | Self::MessageWithPosition { message, .. } => message,
        }
    }
}
