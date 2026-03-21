/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),

    #[error("unknown operator: {name}")]
    UnknownOperator { name: String },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,
}
