/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{0}")]
    Message(String),
    #[error("step limit exceeded after {max_steps} eval dispatch(es)")]
    StepLimitExceeded { max_steps: usize },
}

impl EvalError {
    pub fn message(message: impl Into<String>) -> Self {
        Self::Message(message.into())
    }

    pub fn step_limit_exceeded(max_steps: usize) -> Self {
        Self::StepLimitExceeded { max_steps }
    }
}
