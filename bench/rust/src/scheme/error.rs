use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourcePos {
    line: usize,
    col: usize,
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
    #[error("{pos}: {source}")]
    WithPosition {
        pos: SourcePos,
        source: Box<EvalError>,
    },

    #[error("empty input")]
    EmptyInput,

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected token: {token}")]
    UnexpectedToken { token: String },

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("invalid integer literal: {value}")]
    InvalidInteger { value: String },

    #[error("invalid syntax: {message}")]
    InvalidSyntax { message: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },

    #[error("{name}: wrong argument count, expected {expected}, got {got}")]
    WrongArgCount {
        name: &'static str,
        expected: &'static str,
        got: usize,
    },

    #[error("{name}: expected number, found {found}")]
    ExpectedNumber { name: &'static str, found: String },

    #[error("{name}: expected string, found {found}")]
    ExpectedString { name: &'static str, found: String },

    #[error("{name}: expected char, found {found}")]
    ExpectedChar { name: &'static str, found: String },

    #[error("{name}: expected symbol, found {found}")]
    ExpectedSymbol { name: &'static str, found: String },

    #[error("{name}: expected pair, found {found}")]
    ExpectedPair { name: &'static str, found: String },

    #[error("{name}: expected list, found {found}")]
    ExpectedList { name: &'static str, found: String },

    #[error("{name}: index out of bounds: {index} (length {len})")]
    IndexOutOfBounds {
        name: &'static str,
        index: i64,
        len: usize,
    },

    #[error("{name}: invalid range {start}..{end} (length {len})")]
    InvalidRange {
        name: &'static str,
        start: i64,
        end: i64,
        len: usize,
    },

    #[error("division by zero")]
    DivisionByZero,
}

impl EvalError {
    pub(crate) fn with_position(self, pos: SourcePos) -> Self {
        match self {
            Self::WithPosition { .. } => self,
            other => Self::WithPosition {
                pos,
                source: Box::new(other),
            },
        }
    }
}
