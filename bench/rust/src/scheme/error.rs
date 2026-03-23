/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("arity error: {0}")]
    Arity(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("division by zero{0}")]
    DivisionByZero(String),
    #[error("step limit exceeded")]
    StepLimitExceeded,
}

impl EvalError {
    /// Wrap this error with source position info (line:col prefix).
    pub fn at(self, line: u32, col: u32) -> Self {
        let pos = format!("{line}:{col}");
        match self {
            EvalError::Parse(msg) => EvalError::Parse(format!("{pos}: {msg}")),
            EvalError::Type(msg) => EvalError::Type(format!("{pos}: {msg}")),
            EvalError::Arity(msg) => EvalError::Arity(format!("{pos}: {msg}")),
            EvalError::UnboundVariable(name) => {
                EvalError::UnboundVariable(format!("{pos}: {name}"))
            }
            EvalError::DivisionByZero(_) => {
                EvalError::DivisionByZero(format!(" at {pos}"))
            }
            EvalError::StepLimitExceeded => EvalError::StepLimitExceeded,
        }
    }
}
