/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("unknown procedure: {name}")]
    UnknownProcedure { name: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("string-set!: strings are immutable in R7RS")]
    ImmutableString,

    #[error("{line}:{col}: {inner}")]
    Positioned {
        line: usize,
        col: usize,
        inner: Box<EvalError>,
    },

    #[error("continuation return")]
    ContinuationReturn {
        id: u64,
        value: Box<crate::scheme::value::Value>,
    },
}
