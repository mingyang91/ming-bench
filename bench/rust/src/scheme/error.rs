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

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("unknown operator: {name}")]
    UnknownOperator { name: String },

    #[error("wrong number of arguments for {name}: expected {expected}, got {got}")]
    WrongArity {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("type error: expected {expected}, got {found}")]
    TypeError {
        expected: &'static str,
        found: &'static str,
    },

    #[error("division by zero")]
    DivisionByZero,

    #[error("cannot call non-function: {found}")]
    NotCallable { found: &'static str },
}
