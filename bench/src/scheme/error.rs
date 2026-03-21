use crate::scheme::value::Span;

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error at {span}: {message}")]
    Parse { message: String, span: Span },

    #[error("unbound variable '{name}' at {span}")]
    UnboundVariable { name: String, span: Span },

    #[error("wrong number of arguments at {span}: expected {expected}, got {got}")]
    WrongArgCount {
        expected: usize,
        got: usize,
        span: Span,
    },

    #[error("type error at {span}: expected {expected}, got {got}")]
    TypeError {
        expected: String,
        got: String,
        span: Span,
    },

    #[error("division by zero at {span}")]
    DivisionByZero { span: Span },
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
            span: Span { line: 1, col: 1 },
        }
    }
}
