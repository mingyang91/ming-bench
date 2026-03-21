/// Source position (line, column), 1-based.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("parse error at {pos}: {msg}")]
    Parse { msg: String, pos: Pos },

    #[error("unbound variable at {pos}: {name}")]
    UnboundVariable { name: String, pos: Pos },

    #[error("type error at {pos}: {msg}")]
    Type { msg: String, pos: Pos },

    #[error("arity error at {pos}: {msg}")]
    Arity { msg: String, pos: Pos },

    #[error("runtime error at {pos}: {msg}")]
    Runtime { msg: String, pos: Pos },

    #[error("continuation invoked")]
    ContinuationReturn,
}
