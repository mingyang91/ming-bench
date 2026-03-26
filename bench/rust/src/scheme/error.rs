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

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("syntax error: {message}")]
    SyntaxError { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("unknown operator: {name}")]
    UnknownOperator { name: String },

    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("expected {expected}, got {found}")]
    TypeMismatch {
        expected: &'static str,
        found: String,
    },

    #[error("attempted to call non-procedure: {found}")]
    NotCallable { found: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("integer overflow")]
    IntegerOverflow,

    #[error("{inner} at {position}")]
    WithPosition {
        inner: Box<EvalError>,
        position: SourcePos,
    },
}

impl EvalError {
    pub fn with_position(self, position: SourcePos) -> Self {
        match self {
            Self::WithPosition { .. } => self,
            other => Self::WithPosition {
                inner: Box::new(other),
                position,
            },
        }
    }
}
