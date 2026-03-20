/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("empty input")]
    EmptyInput,

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: String, got: usize },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("empty list in application position")]
    EmptyList,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },

    #[error("bad syntax in {form}")]
    BadSyntax { form: String },
}
