/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("syntax error: expected at least one expression")]
    EmptyProgram,
    #[error("syntax error: unexpected end of input")]
    UnexpectedEof,
    #[error("syntax error: unexpected ')'")]
    UnexpectedCloseParen,
    #[error("syntax error: cannot evaluate empty list")]
    EmptyList,
    #[error("syntax error: invalid escape sequence \\{escape}")]
    InvalidEscape { escape: char },
    #[error("syntax error: {message}")]
    InvalidSyntax { message: String },
    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },
    #[error("unknown procedure: {name}")]
    UnknownProcedure { name: String },
    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },
    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("type mismatch: expected {expected}, got {found}")]
    TypeMismatch { expected: String, found: String },
    #[error("division by zero")]
    DivisionByZero,
}
