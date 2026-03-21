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
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("unexpected closing parenthesis")]
    UnexpectedClosingParenthesis,
    #[error("unterminated string literal")]
    UnterminatedStringLiteral,
    #[error("invalid escape sequence: \\{escape}")]
    InvalidEscapeSequence { escape: char },
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error("program did not contain any expressions")]
    EmptyProgram,
    #[error("cannot evaluate an empty application")]
    EmptyApplication,
    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },
    #[error("unknown procedure: {name}")]
    UnknownProcedure { name: String },
    #[error("operator is not callable: {expression}")]
    NotCallable { expression: String },
    #[error("wrong argument count for {procedure}: expected {expected}, got {got}")]
    WrongArgumentCount {
        procedure: &'static str,
        expected: ArgCount,
        got: usize,
    },
    #[error("expected {expected}, found {found}")]
    TypeMismatch {
        expected: &'static str,
        found: &'static str,
    },
    #[error("division by zero")]
    DivisionByZero,
}
