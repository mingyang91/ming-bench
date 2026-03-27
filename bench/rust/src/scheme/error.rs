/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,
    #[error("parse error: {0}")]
    Parse(String),
    #[error("unbound symbol: {0}")]
    UnboundSymbol(String),
    #[error("expected a procedure application")]
    InvalidApplication,
    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("{name} expects numeric arguments")]
    ExpectedNumber { name: String },
    #[error("{name} expects a list")]
    ExpectedList { name: String },
    #[error("{name} expects a non-empty list")]
    ExpectedPair { name: String },
    #[error("division by zero")]
    DivisionByZero,
}
