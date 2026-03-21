/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {message}")]
    Parse { message: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("immutable: {message}")]
    Immutable { message: String },

    #[error("at {line}:{col}: {source}")]
    AtPosition {
        line: usize,
        col: usize,
        #[source]
        source: Box<EvalError>,
    },

    #[error("continuation invoked")]
    ContinuationReturn { id: u64 },
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Parse { message: a }, Self::Parse { message: b }) => a == b,
            (Self::UnboundVariable { name: a }, Self::UnboundVariable { name: b }) => a == b,
            (
                Self::TypeError {
                    expected: e1,
                    got: g1,
                },
                Self::TypeError {
                    expected: e2,
                    got: g2,
                },
            ) => e1 == e2 && g1 == g2,
            (
                Self::WrongArgCount {
                    expected: e1,
                    got: g1,
                },
                Self::WrongArgCount {
                    expected: e2,
                    got: g2,
                },
            ) => e1 == e2 && g1 == g2,
            (Self::DivisionByZero, Self::DivisionByZero) => true,
            (Self::Immutable { message: a }, Self::Immutable { message: b }) => a == b,
            (
                Self::AtPosition {
                    line: l1,
                    col: c1,
                    source: s1,
                },
                Self::AtPosition {
                    line: l2,
                    col: c2,
                    source: s2,
                },
            ) => l1 == l2 && c1 == c2 && s1 == s2,
            (Self::ContinuationReturn { id: a }, Self::ContinuationReturn { id: b }) => a == b,
            _ => false,
        }
    }
}
