use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::environment::Environment;
use crate::scheme::value::Value;

#[derive(Clone)]
pub struct CapturedContinuation {
    frames: Vec<Frame>,
}

impl CapturedContinuation {
    pub(crate) fn new(frames: Vec<Frame>) -> Self {
        Self { frames }
    }

    pub(crate) fn frames(&self) -> Vec<Frame> {
        self.frames.clone()
    }
}

#[derive(Clone)]
pub(crate) enum Frame {
    Sequence {
        remaining_rev: Vec<Expr>,
        environment: Environment,
    },
    If {
        consequent: Expr,
        alternate: Option<Expr>,
        environment: Environment,
    },
    Define {
        name: String,
        environment: Environment,
    },
    Set {
        name: String,
        name_location: SourceLocation,
        environment: Environment,
    },
    And {
        remaining_rev: Vec<Expr>,
        environment: Environment,
    },
    Or {
        remaining_rev: Vec<Expr>,
        environment: Environment,
    },
    ApplyOperator {
        arguments: Vec<Expr>,
        environment: Environment,
        location: SourceLocation,
    },
    ApplyArgument {
        callable: Value,
        pending: Vec<Expr>,
        evaluated_rev: Vec<Value>,
        environment: Environment,
        location: SourceLocation,
    },
    StandardLet {
        names: Vec<String>,
        pending: Vec<Expr>,
        evaluated_rev: Vec<Value>,
        body: Vec<Expr>,
        environment: Environment,
        location: SourceLocation,
    },
    NamedLet {
        name: String,
        parameters: Vec<String>,
        pending: Vec<Expr>,
        evaluated_rev: Vec<Value>,
        body: Vec<Expr>,
        environment: Environment,
        location: SourceLocation,
    },
    CondClause {
        body: Vec<Expr>,
        remaining_clauses_rev: Vec<Expr>,
        environment: Environment,
    },
    RecursiveLet {
        binding_name: String,
        pending_rev: Vec<(String, Expr)>,
        body: Vec<Expr>,
        environment: Environment,
        location: SourceLocation,
        form: &'static str,
    },
    Case {
        clauses_rev: Vec<Expr>,
        environment: Environment,
    },
}
