/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{0}")]
    Message(String),
}

impl EvalError {
    pub fn message(message: impl Into<String>) -> Self {
        Self::Message(message.into())
    }
}
