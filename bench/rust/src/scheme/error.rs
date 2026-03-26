use std::fmt;

use super::model::{ContinuationProc, Value};

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

#[doc(hidden)]
pub struct ContinuationJumpData {
    continuation: ContinuationProc,
    value: Value,
}

impl ContinuationJumpData {
    pub(super) fn new(continuation: ContinuationProc, value: Value) -> Self {
        Self {
            continuation,
            value,
        }
    }

    pub(super) fn into_parts(self) -> (ContinuationProc, Value) {
        (self.continuation, self.value)
    }
}

/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(thiserror::Error)]
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

    #[error("{name}: numeric overflow")]
    NumericOverflow { name: String },

    #[error("{name}: index {index} out of bounds for length {len}")]
    IndexOutOfBounds {
        name: String,
        index: i64,
        len: usize,
    },

    #[error("{name}: invalid range {start}..{end} for length {len}")]
    InvalidRange {
        name: String,
        start: i64,
        end: i64,
        len: usize,
    },

    #[error("{name}: circular list")]
    CircularList { name: String },

    #[error("{name}: cannot mutate immutable string")]
    ImmutableString { name: String },

    #[error("{name}: invalid character code {code}")]
    InvalidCharCode { name: String, code: i64 },

    #[error("application: expected procedure, got {got}")]
    NotAProcedure { got: String },

    #[error("uncaught exception: {value}")]
    UncaughtException { value: String },

    #[error("internal continuation jump")]
    ContinuationJump { jump: ContinuationJumpData },
}

impl EvalError {
    pub fn with_position(self, position: SourcePos) -> Self {
        match self {
            Self::Positioned { .. } | Self::ContinuationJump { .. } => self,
            error => Self::Positioned {
                position,
                error: Box::new(error),
            },
        }
    }
}

impl fmt::Debug for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Positioned {
                    position: left_position,
                    error: left_error,
                },
                Self::Positioned {
                    position: right_position,
                    error: right_error,
                },
            ) => left_position == right_position && left_error == right_error,
            (Self::EmptyInput, Self::EmptyInput) => true,
            (Self::Syntax { message: left }, Self::Syntax { message: right }) => left == right,
            (Self::UnexpectedEof, Self::UnexpectedEof) => true,
            (Self::UnboundVariable { name: left }, Self::UnboundVariable { name: right }) => {
                left == right
            }
            (
                Self::TypeMismatch {
                    name: left_name,
                    expected: left_expected,
                    got: left_got,
                },
                Self::TypeMismatch {
                    name: right_name,
                    expected: right_expected,
                    got: right_got,
                },
            ) => {
                left_name == right_name && left_expected == right_expected && left_got == right_got
            }
            (
                Self::WrongArgCount {
                    name: left_name,
                    expected: left_expected,
                    got: left_got,
                },
                Self::WrongArgCount {
                    name: right_name,
                    expected: right_expected,
                    got: right_got,
                },
            ) => {
                left_name == right_name && left_expected == right_expected && left_got == right_got
            }
            (Self::DivisionByZero, Self::DivisionByZero) => true,
            (Self::NumericOverflow { name: left }, Self::NumericOverflow { name: right }) => {
                left == right
            }
            (
                Self::IndexOutOfBounds {
                    name: left_name,
                    index: left_index,
                    len: left_len,
                },
                Self::IndexOutOfBounds {
                    name: right_name,
                    index: right_index,
                    len: right_len,
                },
            ) => left_name == right_name && left_index == right_index && left_len == right_len,
            (
                Self::InvalidRange {
                    name: left_name,
                    start: left_start,
                    end: left_end,
                    len: left_len,
                },
                Self::InvalidRange {
                    name: right_name,
                    start: right_start,
                    end: right_end,
                    len: right_len,
                },
            ) => {
                left_name == right_name
                    && left_start == right_start
                    && left_end == right_end
                    && left_len == right_len
            }
            (Self::CircularList { name: left }, Self::CircularList { name: right }) => {
                left == right
            }
            (Self::ImmutableString { name: left }, Self::ImmutableString { name: right }) => {
                left == right
            }
            (
                Self::InvalidCharCode {
                    name: left_name,
                    code: left_code,
                },
                Self::InvalidCharCode {
                    name: right_name,
                    code: right_code,
                },
            ) => left_name == right_name && left_code == right_code,
            (Self::NotAProcedure { got: left }, Self::NotAProcedure { got: right }) => {
                left == right
            }
            (Self::UncaughtException { value: left }, Self::UncaughtException { value: right }) => {
                left == right
            }
            (Self::ContinuationJump { .. }, Self::ContinuationJump { .. }) => false,
            _ => false,
        }
    }
}
