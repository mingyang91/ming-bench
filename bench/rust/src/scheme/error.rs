#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{message}")]
    Message { message: String },
}

impl EvalError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self::Message {
            message: message.into(),
        }
    }

    pub(crate) fn at(line: usize, column: usize, message: impl Into<String>) -> Self {
        Self::Message {
            message: format!("{line}:{column}: {}", message.into()),
        }
    }
}
