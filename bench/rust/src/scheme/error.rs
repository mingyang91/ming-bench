use std::fmt;

/// Source position for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn new(line: usize, col: usize) -> Self {
        Span { line, col }
    }
}


/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq)]
pub struct EvalError {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Parse { message: String },
    UnboundVariable { name: String },
    Type { message: String },
    Arity { message: String },
    DivisionByZero,
    ContinuationReturn { id: u64, value: Box<crate::scheme::value::Value> },
}

impl EvalError {
    pub fn parse(message: impl Into<String>) -> Self {
        EvalError { kind: ErrorKind::Parse { message: message.into() }, span: Span::default() }
    }

    pub fn unbound(name: impl Into<String>) -> Self {
        EvalError { kind: ErrorKind::UnboundVariable { name: name.into() }, span: Span::default() }
    }

    pub fn type_err(message: impl Into<String>) -> Self {
        EvalError { kind: ErrorKind::Type { message: message.into() }, span: Span::default() }
    }

    pub fn arity(message: impl Into<String>) -> Self {
        EvalError { kind: ErrorKind::Arity { message: message.into() }, span: Span::default() }
    }

    pub fn div_zero() -> Self {
        EvalError { kind: ErrorKind::DivisionByZero, span: Span::default() }
    }

    pub fn continuation_return(id: u64, value: crate::scheme::value::Value) -> Self {
        EvalError {
            kind: ErrorKind::ContinuationReturn { id, value: Box::new(value) },
            span: Span::default(),
        }
    }

    pub fn at(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pos = format!("{}:{}", self.span.line, self.span.col);
        match &self.kind {
            ErrorKind::Parse { message } => write!(f, "parse error at {pos}: {message}"),
            ErrorKind::UnboundVariable { name } => write!(f, "unbound variable at {pos}: {name}"),
            ErrorKind::Type { message } => write!(f, "type error at {pos}: {message}"),
            ErrorKind::Arity { message } => write!(f, "arity error at {pos}: {message}"),
            ErrorKind::DivisionByZero => write!(f, "division by zero at {pos}"),
            ErrorKind::ContinuationReturn { .. } => write!(f, "unhandled continuation return"),
        }
    }
}

impl std::error::Error for EvalError {}
