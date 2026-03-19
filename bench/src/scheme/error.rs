use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub(crate) enum SchemeError {
    #[error("input did not contain any expressions")]
    EmptyInput,
    #[error("unexpected end of input while parsing {context}")]
    UnexpectedEndOfInput { context: &'static str },
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
}
