/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{source} at {line}:{column}")]
    WithPosition {
        line: usize,
        column: usize,
        #[source]
        source: Box<EvalError>,
    },

    #[error("syntax error: {message}")]
    SyntaxError { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("unsupported form: {name}")]
    UnsupportedForm { name: String },

    #[error("macro error in {name}: {message}")]
    MacroError { name: String, message: String },

    #[error("attempted to call a non-procedure")]
    NotAProcedure,

    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgumentCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("type mismatch: expected {expected}, got {actual}")]
    TypeMismatch {
        expected: &'static str,
        actual: &'static str,
    },

    #[error("division by zero")]
    DivisionByZero,

    #[error("numeric overflow in {operation}")]
    NumericOverflow { operation: &'static str },

    #[error("negative exponent for expt: {value}")]
    NegativeExponent { value: i64 },

    #[error("mismatched list lengths for {name}: expected {expected}, got {got}")]
    MismatchedListLengths {
        name: String,
        expected: usize,
        got: usize,
    },

    #[error("expected non-negative integer for {kind}, got {value}")]
    NegativeIndex { kind: &'static str, value: i64 },

    #[error("index out of bounds for {kind}: index {index}, length {length}")]
    IndexOutOfBounds {
        kind: &'static str,
        index: usize,
        length: usize,
    },

    #[error("invalid range for {kind}: start {start}, end {end}, length {length}")]
    InvalidRange {
        kind: &'static str,
        start: usize,
        end: usize,
        length: usize,
    },
}

impl EvalError {
    pub fn with_position(self, line: usize, column: usize) -> Self {
        match self {
            Self::WithPosition { .. } => self,
            source => Self::WithPosition {
                line,
                column,
                source: Box::new(source),
            },
        }
    }
}
