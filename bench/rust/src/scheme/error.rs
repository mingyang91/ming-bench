/// Source position information.
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

/// Evaluation error kind.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalErrorKind {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("type error: expected {expected}, got {got}")]
    Type { expected: String, got: String },

    #[error("arity error: {name} expects {expected} arguments, got {got}")]
    Arity {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },
}

impl EvalErrorKind {
    pub fn at(self, span: &Span) -> EvalError {
        EvalError {
            kind: self,
            span: Some(span.clone()),
        }
    }
}

/// Evaluation error with optional source position.
#[derive(Debug, PartialEq)]
pub struct EvalError {
    pub kind: EvalErrorKind,
    pub span: Option<Span>,
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.span {
            Some(span) => write!(f, "{} at {}:{}", self.kind, span.line, span.col),
            None => write!(f, "{}", self.kind),
        }
    }
}

impl std::error::Error for EvalError {}

impl EvalError {
    pub fn with_span(mut self, span: &Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span.clone());
        }
        self
    }
}

impl From<EvalErrorKind> for EvalError {
    fn from(kind: EvalErrorKind) -> Self {
        EvalError { kind, span: None }
    }
}
