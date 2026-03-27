use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct SourcePos {
    pub(crate) line: usize,
    pub(crate) col: usize,
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
    #[error("parse error: {0}")]
    Parse(String),
    #[error("unbound symbol: {0}")]
    UnboundSymbol(String),
    #[error("expected a procedure application")]
    InvalidApplication,
    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("{name} expects numeric arguments")]
    ExpectedNumber { name: String },
    #[error("{name} expects string arguments")]
    ExpectedString { name: String },
    #[error("{name} expects character arguments")]
    ExpectedChar { name: String },
    #[error("{name} expects symbol arguments")]
    ExpectedSymbol { name: String },
    #[error("{name} expects a list")]
    ExpectedList { name: String },
    #[error("{name} expects a non-empty list")]
    ExpectedPair { name: String },
    #[error("{name} expects a vector")]
    ExpectedVector { name: String },
    #[error("{name} expects a {record_type} record")]
    ExpectedRecord { name: String, record_type: String },
    #[error("{name} cannot mutate immutable strings")]
    ImmutableString { name: String },
    #[error("{name} index {index} out of bounds for length {len}")]
    IndexOutOfBounds {
        name: String,
        index: i64,
        len: usize,
    },
    #[error("{name} range [{start}, {end}) out of bounds for length {len}")]
    InvalidRange {
        name: String,
        start: i64,
        end: i64,
        len: usize,
    },
    #[error("{name} invalid argument: {message}")]
    InvalidArgument { name: String, message: String },
    #[error("{name} invalid character code: {code}")]
    InvalidCharacterCode { name: String, code: i64 },
    #[error("expected a single value, got {got}")]
    ExpectedSingleValue { got: usize },
    #[error("division by zero")]
    DivisionByZero,
    #[error("uncaught exception: {value}")]
    UncaughtException { value: String },
    #[error("exception handler returned for a non-continuable raise")]
    HandlerReturned,
    #[error("{pos}: {source}")]
    Positioned {
        pos: SourcePos,
        #[source]
        source: Box<EvalError>,
    },
}

impl EvalError {
    pub(crate) fn with_position(self, pos: SourcePos) -> Self {
        match self {
            Self::Positioned { .. } => self,
            _ => Self::Positioned {
                pos,
                source: Box::new(self),
            },
        }
    }
}
