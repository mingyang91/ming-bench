use crate::scheme::environment::Environment;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct Continuation {
    frames: Vec<Frame>,
}

impl Continuation {
    pub(crate) fn new(frames: Vec<Frame>) -> Self {
        Self { frames }
    }

    pub(crate) fn cloned_frames(&self) -> Vec<Frame> {
        self.frames.clone()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Frame {
    Sequence {
        remaining: Vec<Expr>,
        environment: Environment,
    },
    Define {
        name: String,
        environment: Environment,
    },
    Set {
        name: String,
        environment: Environment,
    },
    If {
        consequent: Expr,
        alternative: Expr,
        environment: Environment,
    },
    And {
        remaining: Vec<Expr>,
        environment: Environment,
    },
    Or {
        remaining: Vec<Expr>,
        environment: Environment,
    },
    Cond {
        body: Vec<Expr>,
        remaining_clauses: Vec<Expr>,
        environment: Environment,
    },
    ApplyOperator {
        operands: Vec<Expr>,
        environment: Environment,
    },
    ApplyArguments {
        callable: Value,
        evaluated_rev: Vec<Value>,
        remaining: Vec<Expr>,
        environment: Environment,
    },
    Let {
        current_name: String,
        evaluated_rev: Vec<(String, Value)>,
        remaining: Vec<LetBinding>,
        body: Vec<Expr>,
        environment: Environment,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct LetBinding {
    name: String,
    expression: Expr,
}

impl LetBinding {
    pub(crate) fn new(name: String, expression: Expr) -> Self {
        Self { name, expression }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn into_parts(self) -> (String, Expr) {
        (self.name, self.expression)
    }
}
