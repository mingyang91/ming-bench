#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("unbound variable: {0}")]
    Unbound(String),
    #[error("bad syntax: {0}")]
    Syntax(String),
    #[error("division by zero")]
    DivisionByZero,
    #[error("wrong number of arguments: {0}")]
    Arity(String),
}
