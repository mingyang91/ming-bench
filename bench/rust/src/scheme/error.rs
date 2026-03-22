/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("type error: expected {expected}, got {got}")]
    Type { expected: String, got: String },

    #[error("arity error: {name} expects {expected} arguments, got {got}")]
    Arity {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },
}
