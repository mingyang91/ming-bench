/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type error: {message}")]
    TypeError { message: String },

    #[error("arity error: {procedure} expects {expected} args, got {got}")]
    Arity {
        procedure: String,
        expected: String,
        got: usize,
    },

    #[error("division by zero")]
    DivisionByZero,
}
