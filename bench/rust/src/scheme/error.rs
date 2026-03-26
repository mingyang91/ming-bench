/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error("{line}:{col}: {message}")]
    At {
        line: usize,
        col: usize,
        message: String,
    },
}

impl EvalError {
    pub fn at(line: usize, col: usize, message: impl Into<String>) -> Self {
        Self::At {
            line,
            col,
            message: message.into(),
        }
    }
}
