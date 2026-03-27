/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("wrong number of arguments: {0}")]
    Arity(String),
    #[error("division by zero")]
    DivisionByZero,
    #[error("{error} at {line}:{col}")]
    WithPosition {
        error: Box<EvalError>,
        line: usize,
        col: usize,
    },
    #[error("unhandled exception: {0}")]
    Raised(String),
}
