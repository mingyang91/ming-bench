#[derive(Debug, Clone, thiserror::Error)]
pub enum SchemeError {
    #[error("parse error: unexpected end of input")]
    UnexpectedEof,
    #[error("parse error: unexpected '{ch}'")]
    UnexpectedChar { ch: char },
    #[error("parse error: unterminated string")]
    UnterminatedString,
    #[error("unbound variable '{name}'")]
    UnboundVariable { name: String },
    #[error("not a procedure: {display}")]
    NotAProcedure { display: String },
    #[error("{name}: expected {expected} args, got {got}")]
    ArityMismatch {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("{op}: expected {expected}, got {got}")]
    TypeError {
        op: String,
        expected: String,
        got: String,
    },
    #[error("division by zero")]
    DivisionByZero,
    #[error("set!: unbound variable '{name}'")]
    SetUnbound { name: String },
    #[error("bad syntax in {form}")]
    BadSyntax { form: String },
}
