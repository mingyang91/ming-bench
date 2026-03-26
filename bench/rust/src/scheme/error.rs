/// A 1-based source location in the input program.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("syntax error: {message} at {line}:{column}")]
    Syntax {
        message: String,
        line: usize,
        column: usize,
    },

    #[error("undefined variable `{name}` at {line}:{column}")]
    UndefinedVariable {
        name: String,
        line: usize,
        column: usize,
    },

    #[error(
        "wrong argument count for {name}: expected {expected}, got {actual} at {line}:{column}"
    )]
    WrongArgCount {
        name: String,
        expected: String,
        actual: usize,
        line: usize,
        column: usize,
    },

    #[error("type mismatch: {message} at {line}:{column}")]
    TypeMismatch {
        message: String,
        line: usize,
        column: usize,
    },

    #[error("division by zero at {line}:{column}")]
    DivisionByZero { line: usize, column: usize },

    #[error("not a procedure: {found} at {line}:{column}")]
    NotAProcedure {
        found: String,
        line: usize,
        column: usize,
    },
}

impl EvalError {
    pub fn syntax(message: impl Into<String>, pos: Position) -> Self {
        Self::Syntax {
            message: message.into(),
            line: pos.line,
            column: pos.column,
        }
    }

    pub fn undefined_variable(name: impl Into<String>, pos: Position) -> Self {
        Self::UndefinedVariable {
            name: name.into(),
            line: pos.line,
            column: pos.column,
        }
    }

    pub fn wrong_arg_count(
        name: impl Into<String>,
        expected: impl Into<String>,
        actual: usize,
        pos: Position,
    ) -> Self {
        Self::WrongArgCount {
            name: name.into(),
            expected: expected.into(),
            actual,
            line: pos.line,
            column: pos.column,
        }
    }

    pub fn type_mismatch(message: impl Into<String>, pos: Position) -> Self {
        Self::TypeMismatch {
            message: message.into(),
            line: pos.line,
            column: pos.column,
        }
    }

    pub fn division_by_zero(pos: Position) -> Self {
        Self::DivisionByZero {
            line: pos.line,
            column: pos.column,
        }
    }

    pub fn not_a_procedure(found: impl Into<String>, pos: Position) -> Self {
        Self::NotAProcedure {
            found: found.into(),
            line: pos.line,
            column: pos.column,
        }
    }
}
