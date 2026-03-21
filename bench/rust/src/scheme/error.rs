use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourcePos {
    pub line: usize,
    pub col: usize,
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
    #[error("syntax error at {pos}: {message}")]
    SyntaxError { pos: SourcePos, message: String },

    #[error("unexpected end of input at {pos}")]
    UnexpectedEof { pos: SourcePos },

    #[error("unbound variable at {pos}: {name}")]
    UnboundVariable { pos: SourcePos, name: String },

    #[error("unknown operator at {pos}: {name}")]
    UnknownOperator { pos: SourcePos, name: String },

    #[error("wrong number of arguments at {pos} for {name}: expected {expected}, got {got}")]
    WrongArity {
        pos: SourcePos,
        name: String,
        expected: String,
        got: usize,
    },

    #[error("type error at {pos}: expected {expected}, got {found}")]
    TypeError {
        pos: SourcePos,
        expected: &'static str,
        found: &'static str,
    },

    #[error("division by zero at {pos}")]
    DivisionByZero { pos: SourcePos },

    #[error("index out of bounds at {pos}: index {index}, length {len}")]
    IndexOutOfBounds {
        pos: SourcePos,
        index: i64,
        len: usize,
    },

    #[error("invalid range at {pos}: start {start}, end {end}, length {len}")]
    InvalidRange {
        pos: SourcePos,
        start: i64,
        end: i64,
        len: usize,
    },

    #[error("cannot call non-function at {pos}: {found}")]
    NotCallable { pos: SourcePos, found: &'static str },

    #[error("immutable string at {pos}")]
    ImmutableString { pos: SourcePos },

    #[error("invalid character code at {pos}: {value}")]
    InvalidCharacterCode { pos: SourcePos, value: i64 },

    #[error("invalid length at {pos}: {len}")]
    InvalidLength { pos: SourcePos, len: i64 },

    #[error("uninitialized binding at {pos}: {name}")]
    UninitializedBinding { pos: SourcePos, name: String },
}
