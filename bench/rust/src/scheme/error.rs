/// Source position in the input.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

use crate::scheme::value::Value;

/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("parse error: {0}")]
    Parse(String),

    #[error("unknown operator: {name}")]
    UnknownOperator { name: String },

    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },

    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: String, got: String },

    #[error("wrong number of arguments: expected {expected}, got {got}")]
    WrongArgCount { expected: usize, got: usize },

    #[error("division by zero")]
    DivisionByZero,

    #[error("{inner} at {line}:{col}")]
    Positioned {
        inner: Box<EvalError>,
        line: usize,
        col: usize,
    },

    #[error("continuation invoked")]
    ContinuationReturn {
        cont_id: u64,
        value: Box<Value>,
        expr_index: usize,
    },
}

impl PartialEq for EvalError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EvalError::Parse(a), EvalError::Parse(b)) => a == b,
            (EvalError::UnknownOperator { name: a }, EvalError::UnknownOperator { name: b }) => {
                a == b
            }
            (EvalError::UnboundVariable { name: a }, EvalError::UnboundVariable { name: b }) => {
                a == b
            }
            (
                EvalError::TypeError {
                    expected: ae,
                    got: ag,
                },
                EvalError::TypeError {
                    expected: be,
                    got: bg,
                },
            ) => ae == be && ag == bg,
            (
                EvalError::WrongArgCount {
                    expected: ae,
                    got: ag,
                },
                EvalError::WrongArgCount {
                    expected: be,
                    got: bg,
                },
            ) => ae == be && ag == bg,
            (EvalError::DivisionByZero, EvalError::DivisionByZero) => true,
            (
                EvalError::Positioned {
                    inner: ai,
                    line: al,
                    col: ac,
                },
                EvalError::Positioned {
                    inner: bi,
                    line: bl,
                    col: bc,
                },
            ) => ai == bi && al == bl && ac == bc,
            _ => false,
        }
    }
}

impl EvalError {
    pub fn at(self, span: Span) -> Self {
        if matches!(
            self,
            EvalError::Positioned { .. } | EvalError::ContinuationReturn { .. }
        ) {
            return self;
        }
        EvalError::Positioned {
            inner: Box::new(self),
            line: span.line,
            col: span.col,
        }
    }
}
