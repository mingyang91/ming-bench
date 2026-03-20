/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty program")]
    EmptyProgram,
    #[error("parse error: {message}")]
    Parse { message: String },
    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },
    #[error("type error: expected {expected}, found {found}")]
    TypeError { expected: String, found: String },
    #[error("arity error: expected {expected}, got {got}")]
    ArityError { expected: String, got: usize },
    #[error("invalid form: {message}")]
    InvalidForm { message: String },
    #[error("division by zero")]
    DivisionByZero,
    #[error("arithmetic overflow")]
    ArithmeticOverflow,
    #[error("macro expansion failed: {message}")]
    MacroExpansion { message: String },
}
