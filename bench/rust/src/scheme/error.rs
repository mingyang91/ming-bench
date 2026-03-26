/// Evaluation error type for the Scheme interpreter.
///
/// The benchmark only asserts on `EvalError` itself for early levels, but
/// keeping a few structured variants makes later extensions easier.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("{name} expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("{context} expected {expected}, got {got}")]
    WrongValueCount {
        context: String,
        expected: String,
        got: usize,
    },

    #[error("{0}")]
    TypeMismatch(String),

    #[error("division by zero")]
    DivisionByZero,

    #[error("{0}")]
    InvalidForm(String),

    #[error("not a procedure: {0}")]
    NotAProcedure(String),
}
