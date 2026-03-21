/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type error: {0}")]
    Type(String),

    #[error("arity error: {0}")]
    Arity(String),

    #[error("runtime error: {0}")]
    Runtime(String),
}
