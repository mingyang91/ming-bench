/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("syntax error: {message}")]
    SyntaxError { message: String },

    #[error("unbound symbol: {name}")]
    UnboundSymbol { name: String },

    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },

    #[error("wrong argument count for {name}: expected {expected}, got {actual}")]
    WrongArgCount {
        name: String,
        expected: String,
        actual: usize,
    },

    #[error("type mismatch: expected {expected}, got {found}")]
    TypeMismatch { expected: String, found: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("integer division produced a non-integer result")]
    NonIntegerDivision,
}
