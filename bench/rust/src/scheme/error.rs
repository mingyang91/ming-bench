use super::Span;

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),

    #[error("unbound variable: {0}")]
    UnboundVariable(String),

    #[error("type error: {0}")]
    Type(String),

    #[error("arity error: {0}")]
    Arity(String),

    #[error("at {0}: division by zero")]
    DivisionByZero(Span),
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}
