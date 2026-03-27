/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty program")]
    EmptyProgram,

    #[error("parse error: {message}")]
    ParseError { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unbound symbol: {name}")]
    UnboundSymbol { name: String },

    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },

    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgCount {
        name: &'static str,
        expected: &'static str,
        got: usize,
    },

    #[error("type mismatch: expected {expected}, got {found}")]
    TypeMismatch {
        expected: &'static str,
        found: String,
    },

    #[error("division by zero")]
    DivisionByZero,
}
