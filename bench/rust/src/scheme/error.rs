/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error at {line}:{col}: {message}")]
    Parse {
        message: String,
        line: usize,
        col: usize,
    },

    #[error("unbound variable at {line}:{col}: {name}")]
    UnboundVariable {
        name: String,
        line: usize,
        col: usize,
    },

    #[error("type error at {line}:{col}: {message}")]
    TypeError {
        message: String,
        line: usize,
        col: usize,
    },

    #[error("arity error at {line}:{col}: {procedure} expects {expected} args, got {got}")]
    Arity {
        procedure: String,
        expected: String,
        got: usize,
        line: usize,
        col: usize,
    },

    #[error("division by zero at {line}:{col}")]
    DivisionByZero { line: usize, col: usize },
}

impl EvalError {
    /// Set position on this error if it doesn't already have one (i.e., position is 0:0).
    pub fn with_position(self, new_line: usize, new_col: usize) -> Self {
        match self {
            EvalError::Parse {
                message,
                line: 0,
                col: 0,
            } => EvalError::Parse {
                message,
                line: new_line,
                col: new_col,
            },
            EvalError::Parse { .. } => self,
            EvalError::UnboundVariable {
                name,
                line: 0,
                col: 0,
            } => EvalError::UnboundVariable {
                name,
                line: new_line,
                col: new_col,
            },
            EvalError::UnboundVariable { .. } => self,
            EvalError::TypeError {
                message,
                line: 0,
                col: 0,
            } => EvalError::TypeError {
                message,
                line: new_line,
                col: new_col,
            },
            EvalError::TypeError { .. } => self,
            EvalError::Arity {
                procedure,
                expected,
                got,
                line: 0,
                col: 0,
            } => EvalError::Arity {
                procedure,
                expected,
                got,
                line: new_line,
                col: new_col,
            },
            EvalError::Arity { .. } => self,
            EvalError::DivisionByZero { line: 0, col: 0 } => EvalError::DivisionByZero {
                line: new_line,
                col: new_col,
            },
            EvalError::DivisionByZero { .. } => self,
        }
    }
}
