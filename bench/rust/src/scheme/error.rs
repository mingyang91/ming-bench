/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("parse error: {0}")]
    Parse(String),

    #[error("unbound variable: {0}")]
    UnboundVariable(String),

    #[error("invalid form: {0}")]
    InvalidForm(String),

    #[error("wrong arity for {name}: expected {expected}, got {got}")]
    WrongArity {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("type error: {0}")]
    Type(String),

    #[error("attempted to mutate an immutable string")]
    ImmutableString,

    #[error("{0}")]
    Message(String),
}
