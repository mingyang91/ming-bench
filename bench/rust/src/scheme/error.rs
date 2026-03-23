/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
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
}

impl EvalError {
    pub fn with_position(self, line: usize, col: usize) -> EvalError {
        match self {
            EvalError::Positioned { .. } => self,
            other => EvalError::Positioned {
                msg: other.to_string(),
                line,
                col,
            },
        }
    }
}
