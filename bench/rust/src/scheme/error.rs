/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("syntax error: {0}")]
    Syntax(String),
    #[error("unbound symbol: {0}")]
    UnboundSymbol(String),
    #[error("unknown procedure: {0}")]
    UnknownProcedure(String),
    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgumentCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: String, found: String },
    #[error("division by zero")]
    DivisionByZero,
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("not a procedure: {0}")]
    NotAProcedure(String),
}
