/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
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

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("unknown procedure: {name}")]
    UnknownProcedure { name: String },

    #[error("attempted to call a non-procedure")]
    NotAProcedure,

    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: &'static str,
        expected: &'static str,
        got: usize,
    },

    #[error("{name} expected a number, got {found}")]
    ExpectedNumber {
        name: &'static str,
        found: &'static str,
    },

    #[error("division by zero")]
    DivisionByZero,
}
