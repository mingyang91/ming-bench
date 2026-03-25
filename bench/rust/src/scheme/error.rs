/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("syntax error: {message}")]
    SyntaxError { message: String },

    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("unsupported form: {name}")]
    UnsupportedForm { name: String },

    #[error("attempted to call a non-procedure")]
    NotAProcedure,

    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgumentCount {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("type mismatch: expected {expected}, got {actual}")]
    TypeMismatch {
        expected: &'static str,
        actual: &'static str,
    },

    #[error("division by zero")]
    DivisionByZero,
}
