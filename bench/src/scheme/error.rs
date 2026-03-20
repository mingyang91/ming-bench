/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {msg}")]
    Parse { msg: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("arity error: {name} expects {expected} args, got {actual}")]
    ArityError {
        name: String,
        expected: usize,
        actual: usize,
    },

    #[error("division by zero")]
    DivisionByZero,
}
