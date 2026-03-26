use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
}

impl SourcePos {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{position}: {error}")]
    Positioned {
        position: SourcePos,
        #[source]
        error: Box<EvalError>,
    },

    #[error("empty input")]
    EmptyInput,

    #[error("syntax error: {message}")]
    Syntax { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("{name}: expected {expected}, got {got}")]
    TypeMismatch {
        name: String,
        expected: String,
        got: String,
    },

    #[error("{name}: expected {expected} arguments, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("division by zero")]
    DivisionByZero,

    #[error("application: expected procedure, got {got}")]
    NotAProcedure { got: String },
}

impl EvalError {
    pub fn with_position(self, position: SourcePos) -> Self {
        match self {
            Self::Positioned { .. } => self,
            error => Self::Positioned {
                position,
                error: Box::new(error),
            },
        }
    }
}
