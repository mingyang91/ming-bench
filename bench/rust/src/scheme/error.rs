/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("{source} at {line}:{col}")]
    WithPosition {
        line: usize,
        col: usize,
        #[source]
        source: Box<EvalError>,
    },

    #[error("syntax error: {message}")]
    SyntaxError { message: String },

    #[error("unbound symbol: {name}")]
    UnboundSymbol { name: String },

    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },

    #[error("wrong argument count for {name}: expected {expected}, got {actual}")]
    WrongArgCount {
        name: String,
        expected: String,
        actual: usize,
    },

    #[error("type mismatch: expected {expected}, got {found}")]
    TypeMismatch { expected: String, found: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("integer division produced a non-integer result")]
    NonIntegerDivision,

    #[error("negative exponent: {exponent}")]
    NegativeExponent { exponent: i64 },

    #[error("index out of bounds: index {index}, length {len}")]
    IndexOutOfBounds { index: i64, len: usize },

    #[error("invalid substring range: start {start}, end {end}, length {len}")]
    InvalidSubstringRange { start: i64, end: i64, len: usize },
}

impl EvalError {
    pub(crate) fn with_position(self, line: usize, col: usize) -> Self {
        match self {
            Self::WithPosition { .. } => self,
            other => Self::WithPosition {
                line,
                col,
                source: Box::new(other),
            },
        }
    }
}
