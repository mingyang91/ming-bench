/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{message} at {line}:{column}")]
    At {
        message: String,
        line: usize,
        column: usize,
    },
}

impl EvalError {
    pub(crate) fn at(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::At {
            message: message.into(),
            line,
            column,
        }
    }
}
