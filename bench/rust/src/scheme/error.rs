use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    #[error("{pos}: {inner}")]
    Located {
        pos: SourcePos,
        #[source]
        inner: Box<EvalError>,
    },
    #[error("syntax error: {0}")]
    Syntax(String),
    #[error("unbound symbol: {0}")]
    UnboundSymbol(String),
    #[error("unknown procedure: {0}")]
    UnknownProcedure(String),
    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgumentCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("wrong value count: expected {expected}, got {got}")]
    WrongValueCount { expected: String, got: usize },
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: String, found: String },
    #[error("division by zero")]
    DivisionByZero,
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("index out of bounds: index {index}, length {len}")]
    IndexOutOfBounds { index: i64, len: usize },
    #[error("invalid range: start {start}, end {end}, length {len}")]
    InvalidRange { start: i64, end: i64, len: usize },
    #[error("invalid argument: {0}")]
    InvalidArgument(String),
    #[error("string is immutable")]
    ImmutableString,
    #[error("invalid character code point: {0}")]
    InvalidCharCodePoint(i64),
    #[error("not a procedure: {0}")]
    NotAProcedure(String),
    #[error("uncaught exception: {0}")]
    UncaughtException(String),
    #[error("exception handler returned")]
    ExceptionHandlerReturned,
}

impl EvalError {
    pub fn with_position(self, pos: SourcePos) -> Self {
        match self {
            Self::Located { .. } => self,
            other => Self::Located {
                pos,
                inner: Box::new(other),
            },
        }
    }
}
