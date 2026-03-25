use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourcePos {
    pub line: usize,
    pub col: usize,
}

impl SourcePos {
    pub const fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("empty input")]
    EmptyInput,

    #[error("{position}: syntax error: {message}")]
    Syntax { message: String, position: SourcePos },

    #[error("{position}: unbound variable: {name}")]
    UnboundVariable { name: String, position: SourcePos },

    #[error("{position}: wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
        position: SourcePos,
    },

    #[error("{position}: type mismatch: expected {expected}, got {got}")]
    TypeMismatch {
        expected: String,
        got: String,
        position: SourcePos,
    },

    #[error("{position}: not a procedure: {found}")]
    NotCallable { found: String, position: SourcePos },

    #[error("{position}: division by zero")]
    DivisionByZero { position: SourcePos },

    #[error("{position}: integer overflow")]
    IntegerOverflow { position: SourcePos },
}

impl EvalError {
    pub fn syntax(message: impl Into<String>, position: SourcePos) -> Self {
        Self::Syntax {
            message: message.into(),
            position,
        }
    }

    pub fn unbound_variable(name: impl Into<String>, position: SourcePos) -> Self {
        Self::UnboundVariable {
            name: name.into(),
            position,
        }
    }

    pub fn wrong_arg_count(
        name: impl Into<String>,
        expected: impl Into<String>,
        got: usize,
        position: SourcePos,
    ) -> Self {
        Self::WrongArgCount {
            name: name.into(),
            expected: expected.into(),
            got,
            position,
        }
    }

    pub fn type_mismatch(
        expected: impl Into<String>,
        got: impl Into<String>,
        position: SourcePos,
    ) -> Self {
        Self::TypeMismatch {
            expected: expected.into(),
            got: got.into(),
            position,
        }
    }

    pub fn not_callable(found: impl Into<String>, position: SourcePos) -> Self {
        Self::NotCallable {
            found: found.into(),
            position,
        }
    }

    pub fn division_by_zero(position: SourcePos) -> Self {
        Self::DivisionByZero { position }
    }

    pub fn integer_overflow(position: SourcePos) -> Self {
        Self::IntegerOverflow { position }
    }
}
