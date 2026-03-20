/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("unexpected token: {token}")]
    UnexpectedToken { token: String },

    #[error("empty input")]
    EmptyInput,

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },

    #[error("string is immutable")]
    ImmutableString,

    #[error("at {line}:{col}: {source}")]
    AtPosition {
        line: usize,
        col: usize,
        source: Box<EvalError>,
    },
}
