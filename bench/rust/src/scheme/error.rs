/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{0}")]
    Message(String),
}

impl EvalError {
    pub fn message(message: impl Into<String>) -> Self {
        Self::Message(message.into())
    }
}
