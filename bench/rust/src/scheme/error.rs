/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{line}:{col}: {source}")]
    WithPosition {
        line: usize,
        col: usize,
        source: Box<EvalError>,
    },

    #[error("empty program")]
    EmptyProgram,

    #[error("parse error: {message}")]
    ParseError { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unbound symbol: {name}")]
    UnboundSymbol { name: String },

    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },

    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgCount {
        name: &'static str,
        expected: &'static str,
        got: usize,
    },

    #[error("type mismatch: expected {expected}, got {found}")]
    TypeMismatch {
        expected: &'static str,
        found: String,
    },

    #[error("division by zero")]
    DivisionByZero,

    #[error("index out of bounds: index {index}, length {len}")]
    IndexOutOfBounds { index: i64, len: usize },

    #[error("invalid substring range: start {start}, end {end}, length {len}")]
    InvalidSubstringRange { start: i64, end: i64, len: usize },
}

impl EvalError {
    pub(crate) fn with_position(self, line: usize, col: usize) -> Self {
        match self {
            Self::WithPosition { .. } => self,
            _ => Self::WithPosition {
                line,
                col,
                source: Box::new(self),
            },
        }
    }
}
