/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("syntax error: {message}")]
    Syntax { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("{found} is not a procedure")]
    NotAProcedure { found: String },

    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("{name} expected {expected}, got {found}")]
    TypeMismatch {
        name: String,
        expected: String,
        found: String,
    },

    #[error("division by zero")]
    DivisionByZero,
}
