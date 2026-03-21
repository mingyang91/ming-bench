/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("type error: {0}")]
    TypeError(String),
    #[error("not a procedure")]
    NotAProcedure,
    #[error("wrong number of arguments")]
    Arity,
    #[error("division by zero")]
    DivisionByZero,
    #[error("{msg} at {line}:{col}")]
    WithPosition { msg: String, line: usize, col: usize },
}

impl EvalError {
    pub fn with_position(self, line: usize, col: usize) -> EvalError {
        match self {
            EvalError::WithPosition { .. } => self,
            other => EvalError::WithPosition {
                msg: other.to_string(),
                line,
                col,
            },
        }
    }
}
