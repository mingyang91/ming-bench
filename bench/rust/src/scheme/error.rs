/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("syntax error: {0}")]
    Syntax(String),
    #[error("unbound variable: {0}")]
    UnboundVariable(String),
    #[error("{name}: expected {expected} argument(s), got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("{name}: expected {expected}, found {found}")]
    TypeMismatch {
        name: String,
        expected: String,
        found: String,
    },
    #[error("not a procedure: {0}")]
    NotAProcedure(String),
    #[error("division by zero")]
    DivisionByZero,
    #[error("step limit exceeded after {max_steps} step(s)")]
    StepLimitExceeded { max_steps: usize },
    #[error("{0}")]
    Runtime(String),
}
