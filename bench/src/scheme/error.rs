/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("unexpected token: {token}")]
    UnexpectedToken { token: String },

    #[error("empty input")]
    EmptyInput,
}
