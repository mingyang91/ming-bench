/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{line}:{column}: {source}")]
    WithPosition {
        line: usize,
        column: usize,
        #[source]
        source: Box<EvalError>,
    },

    #[error("input did not contain any expressions")]
    EmptyInput,

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected token: {token}")]
    UnexpectedToken { token: String },

    #[error("invalid number literal: {literal}")]
    InvalidNumber { literal: String },

    #[error("invalid boolean literal: {literal}")]
    InvalidBoolean { literal: String },

    #[error("invalid character literal: {literal}")]
    InvalidCharacter { literal: String },

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("binding accessed before initialization: {name}")]
    UninitializedBinding { name: String },

    #[error("unknown procedure: {name}")]
    UnknownProcedure { name: String },

    #[error("attempted to call a non-procedure")]
    NotAProcedure,

    #[error("invalid {name} form: {message}")]
    InvalidForm {
        name: &'static str,
        message: &'static str,
    },

    #[error("macro did not match any syntax-rules clause: {name}")]
    NoMatchingSyntaxRule { name: String },

    #[error("invalid macro template for {name}: {message}")]
    InvalidMacroTemplate { name: String, message: &'static str },

    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: &'static str,
        expected: &'static str,
        got: usize,
    },

    #[error("{name} expected {expected}, got {got}")]
    WrongArgCountDynamic {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("{name} expected a number, got {found}")]
    ExpectedNumber {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a string, got {found}")]
    ExpectedString {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a character, got {found}")]
    ExpectedCharacter {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a symbol, got {found}")]
    ExpectedSymbol {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a list, got {found}")]
    ExpectedList {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a vector, got {found}")]
    ExpectedVector {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a pair, got {found}")]
    ExpectedPair {
        name: &'static str,
        found: &'static str,
    },

    #[error("{name} expected a {expected} record, got {found}")]
    ExpectedRecordType {
        name: String,
        expected: String,
        found: String,
    },

    #[error("{name} invalid argument: {message}")]
    InvalidArgument {
        name: &'static str,
        message: &'static str,
    },

    #[error("{name} index out of bounds: {index}")]
    IndexOutOfBounds { name: &'static str, index: i64 },

    #[error("{name} expected a valid range, got {start}..{end}")]
    InvalidRange {
        name: &'static str,
        start: i64,
        end: i64,
    },

    #[error("{name} cannot mutate an immutable string")]
    ImmutableString { name: &'static str },

    #[error("division by zero")]
    DivisionByZero,
}

impl EvalError {
    pub fn with_position(self, line: usize, column: usize) -> Self {
        match self {
            Self::WithPosition { .. } => self,
            other => Self::WithPosition {
                line,
                column,
                source: Box::new(other),
            },
        }
    }
}
