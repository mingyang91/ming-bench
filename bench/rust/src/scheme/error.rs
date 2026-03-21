use crate::scheme::ast::SourceLocation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgCount {
    Exactly(usize),
    AtLeast(usize),
}

impl std::fmt::Display for ArgCount {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exactly(value) => write!(formatter, "exactly {value}"),
            Self::AtLeast(value) => write!(formatter, "at least {value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ParseError {
    #[error("{location}: unexpected end of input")]
    UnexpectedEndOfInput { location: SourceLocation },
    #[error("{location}: unexpected closing parenthesis")]
    UnexpectedClosingParenthesis { location: SourceLocation },
    #[error("{location}: unterminated string literal")]
    UnterminatedStringLiteral { location: SourceLocation },
    #[error("{location}: invalid escape sequence: \\{escape}")]
    InvalidEscapeSequence {
        location: SourceLocation,
        escape: char,
    },
    #[error("{location}: invalid character literal: {literal}")]
    InvalidCharacterLiteral {
        location: SourceLocation,
        literal: String,
    },
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error("program did not contain any expressions")]
    EmptyProgram,
    #[error("{location}: cannot evaluate an empty application")]
    EmptyApplication { location: SourceLocation },
    #[error("{location}: unbound variable: {name}")]
    UnboundVariable {
        location: SourceLocation,
        name: String,
    },
    #[error("{location}: uninitialized variable: {name}")]
    UninitializedVariable {
        location: SourceLocation,
        name: String,
    },
    #[error("{location}: unknown procedure: {name}")]
    UnknownProcedure {
        location: SourceLocation,
        name: String,
    },
    #[error("{location}: operator is not callable: {expression}")]
    NotCallable {
        location: SourceLocation,
        expression: String,
    },
    #[error("{location}: malformed special form: {form}")]
    MalformedSpecialForm {
        location: SourceLocation,
        form: &'static str,
    },
    #[error("{location}: {form} requires at least one body expression")]
    MissingBody {
        location: SourceLocation,
        form: &'static str,
    },
    #[error("{location}: {form} requires a parameter list")]
    InvalidParameterList {
        location: SourceLocation,
        form: &'static str,
    },
    #[error("{location}: {form} parameters must be symbols")]
    NonSymbolParameter {
        location: SourceLocation,
        form: &'static str,
    },
    #[error("{location}: wrong argument count for {procedure}: expected {expected}, got {got}")]
    WrongArgumentCount {
        location: SourceLocation,
        procedure: &'static str,
        expected: ArgCount,
        got: usize,
    },
    #[error("{location}: wrong argument count for {procedure}: expected {expected}, got {got}")]
    WrongArgumentCountNamed {
        location: SourceLocation,
        procedure: String,
        expected: ArgCount,
        got: usize,
    },
    #[error("{location}: wrong number of values: expected {expected}, got {got}")]
    WrongValueCount {
        location: SourceLocation,
        expected: ArgCount,
        got: usize,
    },
    #[error("{location}: expected {expected}, found {found}")]
    TypeMismatch {
        location: SourceLocation,
        expected: &'static str,
        found: &'static str,
    },
    #[error("{location}: string index {index} out of bounds for length {length}")]
    StringIndexOutOfBounds {
        location: SourceLocation,
        index: i64,
        length: usize,
    },
    #[error("{location}: list index {index} out of bounds")]
    ListIndexOutOfBounds {
        location: SourceLocation,
        index: i64,
    },
    #[error("{location}: invalid substring range [{start}, {end}) for string length {length}")]
    InvalidSubstringRange {
        location: SourceLocation,
        start: i64,
        end: i64,
        length: usize,
    },
    #[error("{location}: invalid character code point: {value}")]
    InvalidCharacterCodePoint {
        location: SourceLocation,
        value: i64,
    },
    #[error("{location}: invalid vector length: {length}")]
    InvalidVectorLength {
        location: SourceLocation,
        length: i64,
    },
    #[error("{location}: vector index {index} out of bounds for length {length}")]
    VectorIndexOutOfBounds {
        location: SourceLocation,
        index: i64,
        length: usize,
    },
    #[error("{location}: cannot mutate immutable string with {procedure}")]
    ImmutableString {
        location: SourceLocation,
        procedure: &'static str,
    },
    #[error("{location}: invalid syntax-rules: {detail}")]
    InvalidSyntaxRules {
        location: SourceLocation,
        detail: &'static str,
    },
    #[error("{location}: expected record of type {expected}, found {found}")]
    RecordTypeMismatch {
        location: SourceLocation,
        expected: String,
        found: String,
    },
    #[error("{location}: no matching syntax-rules clause for macro {name}")]
    MacroNoMatchingRule {
        location: SourceLocation,
        name: String,
    },
    #[error("{location}: uncaught exception: {value}")]
    UncaughtException {
        location: SourceLocation,
        value: String,
    },
    #[error("{location}: division by zero")]
    DivisionByZero { location: SourceLocation },
    #[error("{location}: numeric overflow")]
    NumericOverflow { location: SourceLocation },
    #[error("{location}: unsupported exponent: {exponent}")]
    InvalidExponent {
        location: SourceLocation,
        exponent: i64,
    },
    #[error("{location}: cannot convert inexact number to exact: {value}")]
    InexactToExactFailed {
        location: SourceLocation,
        value: String,
    },
}
