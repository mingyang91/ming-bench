use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
}

impl SourcePos {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("{pos}: syntax error: {message}")]
    Syntax { pos: SourcePos, message: String },

    #[error("{pos}: unexpected end of input")]
    UnexpectedEof { pos: SourcePos },

    #[error("{pos}: unbound variable: {name}")]
    UnboundVariable { pos: SourcePos, name: String },

    #[error("{pos}: uninitialized binding: {name}")]
    UninitializedBinding { pos: SourcePos, name: String },

    #[error("{pos}: uncaught exception: {value}")]
    UncaughtException { pos: SourcePos, value: String },

    #[error("{pos}: exception handler returned")]
    ExceptionHandlerReturned { pos: SourcePos },

    #[error("{pos}: {found} is not a procedure")]
    NotAProcedure { pos: SourcePos, found: String },

    #[error("{pos}: {name} expected {expected}, got {got}")]
    WrongArgCount {
        pos: SourcePos,
        name: String,
        expected: String,
        got: usize,
    },

    #[error("{pos}: {name} expected {expected}, got {found}")]
    TypeMismatch {
        pos: SourcePos,
        name: String,
        expected: String,
        found: String,
    },

    #[error("{pos}: division by zero")]
    DivisionByZero { pos: SourcePos },

    #[error("{pos}: {name}: {message}")]
    InvalidArgument {
        pos: SourcePos,
        name: String,
        message: String,
    },

    #[error("{pos}: evaluation step limit exceeded ({max_steps} steps)")]
    StepLimitExceeded { pos: SourcePos, max_steps: usize },
}
