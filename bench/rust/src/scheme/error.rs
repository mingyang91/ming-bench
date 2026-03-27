/// Source position used for parse and evaluation errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
}

impl SourcePosition {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error("{message}")]
    Message { message: String },

    #[error("{message} at {line}:{column}")]
    Positioned {
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

    pub fn new_with_position(message: impl Into<String>, position: SourcePosition) -> Self {
        Self::Positioned {
            message: message.into(),
            line: position.line,
            column: position.column,
        }
    }

    pub fn with_position(self, position: SourcePosition) -> Self {
        match self {
            Self::Message { message } => Self::Positioned {
                message,
                line: position.line,
                column: position.column,
            },
            positioned => positioned,
        }
    }
}

impl From<&str> for EvalError {
    fn from(message: &str) -> Self {
        Self::new(message)
    }
}

impl From<String> for EvalError {
    fn from(message: String) -> Self {
        Self::new(message)
    }
}
