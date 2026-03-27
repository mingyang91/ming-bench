use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourcePos {
    pub line: usize,
    pub col: usize,
}

impl SourcePos {
    pub const fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl Default for SourcePos {
    fn default() -> Self {
        Self::new(1, 1)
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{pos}: {message}")]
    Message { message: String, pos: SourcePos },
}

impl EvalError {
    pub fn new(message: impl Into<String>, pos: SourcePos) -> Self {
        Self::Message {
            message: message.into(),
            pos,
        }
    }
}
