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
    #[error("{inner} at {line}:{col}")]
    WithPosition {
        inner: Box<EvalError>,
        line: usize,
        col: usize,
    },
}

impl EvalError {
    pub fn at(self, line: usize, col: usize) -> Self {
        if matches!(&self, EvalError::WithPosition { .. }) {
            self
        } else {
            EvalError::WithPosition {
                inner: Box::new(self),
                line,
                col,
            }
        }
    }
}
