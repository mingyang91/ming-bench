use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::environment::Environment;
use crate::scheme::value::Value;
use std::rc::Rc;

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
pub(crate) struct DynamicWind {
    before: Value,
    after: Value,
    location: SourceLocation,
}

impl DynamicWind {
    pub(crate) fn new(before: Value, after: Value, location: SourceLocation) -> Self {
        Self {
            before,
            after,
            location,
        }
    }

    pub(crate) fn before(&self) -> Value {
        self.before.clone()
    }

    pub(crate) fn after(&self) -> Value {
        self.after.clone()
    }

    pub(crate) fn location(&self) -> SourceLocation {
        self.location
    }
}

#[derive(Clone)]
pub(crate) struct RaisedException {
    value: Value,
    location: SourceLocation,
}

impl RaisedException {
    pub(crate) fn new(value: Value, location: SourceLocation) -> Self {
        Self { value, location }
    }

    pub(crate) fn value(&self) -> &Value {
        &self.value
    }

    pub(crate) fn location(&self) -> SourceLocation {
        self.location
    }
}

#[derive(Clone)]
pub(crate) enum ExceptionHandler {
    Procedure {
        callable: Value,
        location: SourceLocation,
    },
    Guard {
        variable: String,
        clauses: Vec<Expr>,
        environment: Environment,
    },
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
    DynamicWindBefore {
        body: Value,
        wind: Rc<DynamicWind>,
    },
    CallWithValues {
        consumer: Value,
        location: SourceLocation,
    },
    DynamicWindExit {
        wind: Rc<DynamicWind>,
    },
    DynamicWindAfter {
        value: Value,
    },
    DynamicWindContext {
        wind: Rc<DynamicWind>,
    },
    ExceptionHandler {
        handler: ExceptionHandler,
    },
    ExceptionTransition {
        active_winds: Vec<Rc<DynamicWind>>,
        exit_winds: Vec<Rc<DynamicWind>>,
        target_continuation: Vec<Frame>,
        handler: ExceptionHandler,
        exception: RaisedException,
    },
    ContinuationTransition {
        active_winds: Vec<Rc<DynamicWind>>,
        exit_winds: Vec<Rc<DynamicWind>>,
        enter_winds: Vec<Rc<DynamicWind>>,
        target_continuation: Vec<Frame>,
        value: Value,
    },
    GuardClause {
        body: Vec<Expr>,
        remaining_clauses_rev: Vec<Expr>,
        environment: Environment,
        exception: RaisedException,
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
