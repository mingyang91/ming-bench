#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("bad syntax: {0}")]
    BadSyntax(String),
    #[error("division by zero")]
    DivisionByZero,
    #[error("arity mismatch: {0}")]
    Arity(String),
}
