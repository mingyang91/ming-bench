#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("unbound variable: {0}")]
    Unbound(String),
    #[error("bad syntax: {0}")]
    Syntax(String),
    #[error("division by zero at {0}")]
    DivisionByZero(String),
    #[error("wrong number of arguments: {0}")]
    Arity(String),
    #[error("uncaught exception")]
    Raised(Box<super::Value>),
    #[error("step limit exceeded")]
    StepLimitExceeded,
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Parse(a), Self::Parse(b)) => a == b,
            (Self::Type(a), Self::Type(b)) => a == b,
            (Self::Unbound(a), Self::Unbound(b)) => a == b,
            (Self::Syntax(a), Self::Syntax(b)) => a == b,
            (Self::DivisionByZero(a), Self::DivisionByZero(b)) => a == b,
            (Self::Arity(a), Self::Arity(b)) => a == b,
            (Self::Raised(_), Self::Raised(_)) => false,
            (Self::StepLimitExceeded, Self::StepLimitExceeded) => true,
            _ => false,
        }
    }
}
