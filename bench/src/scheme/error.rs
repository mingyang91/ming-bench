use thiserror::Error;

use crate::scheme::value::Value;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub(crate) enum SchemeError {
    #[error("input did not contain any expressions")]
    EmptyInput,
    #[error("unexpected end of input while parsing {context}")]
    UnexpectedEndOfInput { context: &'static str },
    #[error("unexpected `)` at byte {index}")]
    UnexpectedCloseParen { index: usize },
    #[error("invalid token starting with `{ch}` at byte {index}")]
    InvalidTokenStart { ch: char, index: usize },
    #[error("invalid boolean literal `{literal}` at byte {index}")]
    InvalidBooleanLiteral { literal: String, index: usize },
    #[error("invalid integer literal `{literal}` at byte {index}")]
    InvalidInteger { literal: String, index: usize },
    #[error("unterminated string literal starting at byte {start}")]
    UnterminatedString { start: usize },
    #[error("invalid escape sequence `\\{escape}` at byte {index}")]
    InvalidEscape { escape: char, index: usize },
    #[error("application requires at least one expression")]
    EmptyApplication,
    #[error("unbound symbol `{name}`")]
    UnboundSymbol { name: String },
    #[error("cannot call a {kind}")]
    NonCallable { kind: &'static str },
    #[error("`{operator}` expected a number but got {found}")]
    ExpectedNumber {
        operator: &'static str,
        found: Value,
    },
    #[error("`{operator}` expected at least {min} argument(s) but got {actual}")]
    TooFewArguments {
        operator: &'static str,
        min: usize,
        actual: usize,
    },
    #[error("cannot divide by zero")]
    DivisionByZero,
}
