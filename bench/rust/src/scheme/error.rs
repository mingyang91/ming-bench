/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("syntax error: {message}")]
    Syntax { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("{name}: expected {expected}, got {got}")]
    TypeMismatch {
        name: String,
        expected: String,
        got: String,
    },

    #[error("{name}: expected {expected} arguments, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("division by zero")]
    DivisionByZero,

    #[error("application: expected procedure, got {got}")]
    NotAProcedure { got: String },
}
