#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("arity error: {0}")]
    Arity(String),
    #[error("division by zero: {0}")]
    DivisionByZero(String),
}
