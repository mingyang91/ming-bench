/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("type mismatch: expected {expected}, got {got}")]
    TypeMismatch { expected: String, got: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: String, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },
}
