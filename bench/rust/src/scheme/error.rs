/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("runtime error: {0}")]
    Runtime(String),
    #[error("continuation return")]
    ContinuationReturn(u64),
}

impl Eq for EvalError {}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EvalError::Parse(a), EvalError::Parse(b)) => a == b,
            (EvalError::Runtime(a), EvalError::Runtime(b)) => a == b,
            (EvalError::ContinuationReturn(a), EvalError::ContinuationReturn(b)) => a == b,
            _ => false,
        }
    }
}
