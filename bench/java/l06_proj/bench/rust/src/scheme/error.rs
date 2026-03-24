use super::position::SourcePos;

/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{0}")]
    Message(String),
}

impl EvalError {
    pub(crate) fn syntax(pos: SourcePos, message: impl Into<String>) -> Self {
        Self::Message(format!("{pos}: syntax error: {}", message.into()))
    }

    pub(crate) fn type_error(pos: SourcePos, message: impl Into<String>) -> Self {
        Self::Message(format!("{pos}: type error: {}", message.into()))
    }

    pub(crate) fn arity(pos: SourcePos, name: &str, message: impl Into<String>) -> Self {
        Self::Message(format!("{pos}: {name}: {}", message.into()))
    }

    pub(crate) fn at(pos: SourcePos, message: impl Into<String>) -> Self {
        Self::Message(format!("{pos}: {}", message.into()))
    }
}
