/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("division by zero")]
    DivisionByZero,
}

/// Parse error type for the Scheme reader.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected character: {ch}")]
    UnexpectedChar { ch: char },

    #[error("unterminated string")]
    UnterminatedString,
}

impl From<ParseError> for EvalError {
    fn from(e: ParseError) -> Self {
        EvalError::Parse {
            message: e.to_string(),
        }
    }
}
