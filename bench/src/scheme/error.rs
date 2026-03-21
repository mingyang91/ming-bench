/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("not a procedure: {0}")]
    NotAProcedure(String),
    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: String, got: usize },
    #[error("type error: {0}")]
    TypeError(String),
    #[error("division by zero")]
    DivisionByZero,
}
