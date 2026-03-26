/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected token: {token}")]
    UnexpectedToken { token: String },

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("invalid integer literal: {value}")]
    InvalidInteger { value: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },

    #[error("{name}: wrong argument count, expected {expected}, got {got}")]
    WrongArgCount {
        name: &'static str,
        expected: &'static str,
        got: usize,
    },

    #[error("{name}: expected number, found {found}")]
    ExpectedNumber { name: &'static str, found: String },

    #[error("division by zero")]
    DivisionByZero,
}
