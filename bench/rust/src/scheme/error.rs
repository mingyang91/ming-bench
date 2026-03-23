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
    #[error("runtime error: {0}")]
    Runtime(String),
    #[error("{msg} at {line}:{col}")]
    Positioned { msg: String, line: usize, col: usize },
    #[error("continuation return")]
    ContinuationReturn { cont_id: usize },
    #[error("continuation result")]
    ContinuationResult,
    #[error("scheme exception")]
    SchemeException(super::Value),
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Parse(a), Self::Parse(b)) => a == b,
            (Self::UnboundVariable(a), Self::UnboundVariable(b)) => a == b,
            (Self::Type(a), Self::Type(b)) => a == b,
            (Self::Arity(a), Self::Arity(b)) => a == b,
            (Self::Runtime(a), Self::Runtime(b)) => a == b,
            (
                Self::Positioned { msg: a, line: al, col: ac },
                Self::Positioned { msg: b, line: bl, col: bc },
            ) => a == b && al == bl && ac == bc,
            _ => false,
        }
    }
}

impl EvalError {
    pub fn with_position(self, line: usize, col: usize) -> EvalError {
        match self {
            EvalError::Positioned { .. } => self,
            EvalError::ContinuationReturn { .. } => self,
            EvalError::ContinuationResult => self,
            EvalError::SchemeException(_) => self,
            other => EvalError::Positioned {
                msg: other.to_string(),
                line,
                col,
            },
        }
    }
}
