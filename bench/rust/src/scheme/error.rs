/// Evaluation error type for the Scheme interpreter.
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
    #[error("division by zero {0}")]
    DivisionByZero(String),
    #[error("{0}")]
    Custom(String),
}
