#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("unsupported Scheme program for this level")]
    UnsupportedProgram,
}
