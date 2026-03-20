/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("continuation invoked")]
    ContinuationReturn {
        line: usize,
        col: usize,
        expr_index: usize,
    },
}
