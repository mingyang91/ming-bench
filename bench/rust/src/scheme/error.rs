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

    #[error("expected non-negative exponent, got {value}")]
    NegativeExponent { value: i64 },

    #[error("expected non-negative integer, got {value}")]
    NegativeIndex { value: i64 },

    #[error("index out of bounds: index {index}, length {len}")]
    IndexOutOfBounds { index: usize, len: usize },

    #[error("circular list")]
    CircularList,

    #[error("{name} expected lists of equal length, got {expected} and {got}")]
    LengthMismatch {
        name: String,
        expected: usize,
        got: usize,
    },

    #[error("invalid substring range: start {start}, end {end}, length {len}")]
    InvalidRange {
        start: usize,
        end: usize,
        len: usize,
    },

    #[error("string is immutable")]
    ImmutableString,

    #[error("invalid character code point: {value}")]
    InvalidCodePoint { value: i64 },

    #[error("expected {expected} record, got {found}")]
    RecordTypeMismatch { expected: String, found: String },

    #[error("uninitialized binding: {name}")]
    UninitializedBinding { name: String },

    #[error("uncaught exception: {value}")]
    UncaughtException { value: String },

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
