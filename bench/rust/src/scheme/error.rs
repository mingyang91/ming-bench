/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("type error: {message}")]
    Type { message: String },

    #[error("arity error: {message}")]
    Arity { message: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("division by zero")]
    DivisionByZero,
}
