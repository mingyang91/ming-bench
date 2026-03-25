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

    #[error("division by zero")]
    DivisionByZero,

    #[error("{0} at {1}:{2}")]
    WithPosition(Box<EvalError>, usize, usize),

    #[error("step limit exceeded")]
    StepLimitExceeded,
}
