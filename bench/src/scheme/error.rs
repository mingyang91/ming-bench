/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    // Add variants as needed, e.g.:
    // #[error("unbound variable: {name}")]
    // UnboundVariable { name: String },
}
