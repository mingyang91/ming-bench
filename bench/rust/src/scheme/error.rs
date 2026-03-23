use std::fmt;

/// Source position (1-based line and column).
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

/// Classification of evaluation errors.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ErrorKind {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type mismatch: expected {expected}, got {got}")]
    TypeMismatch { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("not a procedure: {value}")]
    NotAProcedure { value: String },

    #[error("bad syntax in {form}: {message}")]
    BadSyntax { form: String, message: String },

    #[error("string is immutable")]
    ImmutableString,

    #[error("unhandled exception: {value}")]
    UserRaise { value: String },
}

/// Evaluation error with optional source position.
#[derive(Debug, PartialEq)]
pub struct EvalError {
    pub kind: ErrorKind,
    pub span: Span,
}

impl EvalError {
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Attach a span if this error has no position yet.
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.line == 0 {
            self.span = span;
        }
        self
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.span.line > 0 {
            write!(f, "{}:{}: {}", self.span.line, self.span.col, self.kind)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

impl std::error::Error for EvalError {}

impl From<ErrorKind> for EvalError {
    fn from(kind: ErrorKind) -> Self {
        Self { kind, span: Span::default() }
    }
}
