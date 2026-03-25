/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{message} at {line}:{column}")]
    Message {
        message: String,
        line: usize,
        column: usize,
    },
}

impl EvalError {
    pub fn at(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::Message {
            message: message.into(),
            line,
            column,
        }
    }
}
