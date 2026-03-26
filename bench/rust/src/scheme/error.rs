use std::error::Error;
use std::fmt;

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    Message {
        message: String,
        line: Option<usize>,
        column: Option<usize>,
    },
}

impl EvalError {
    pub fn new(message: impl Into<String>) -> Self {
        Self::Message {
            message: message.into(),
            line: None,
            column: None,
        }
    }

    pub fn with_position(self, line: usize, column: usize) -> Self {
        if self.has_position() {
            return self;
        }

        let Self::Message { message, .. } = self;
        Self::Message {
            message,
            line: Some(line),
            column: Some(column),
        }
    }

    pub fn has_position(&self) -> bool {
        matches!(
            self,
            Self::Message {
                line: Some(_),
                column: Some(_),
                ..
            }
        )
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Message {
                message,
                line: Some(line),
                column: Some(column),
            } => write!(f, "{line}:{column}: {message}"),
            Self::Message { message, .. } => f.write_str(message),
        }
    }
}

impl Error for EvalError {}
