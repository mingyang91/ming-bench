use crate::scheme::environment::Environment;
use crate::scheme::parser::Expr;

#[derive(Debug, Clone)]
pub(crate) struct Parameters {
    fixed: Vec<String>,
    rest: Option<String>,
}

impl Parameters {
    pub(crate) fn new(fixed: Vec<String>, rest: Option<String>) -> Self {
        Self { fixed, rest }
    }

    pub(crate) fn fixed(&self) -> &[String] {
        &self.fixed
    }

    pub(crate) fn rest(&self) -> Option<&str> {
        self.rest.as_deref()
    }

    pub(crate) fn minimum_arity(&self) -> usize {
        self.fixed.len()
    }

    pub(crate) fn has_rest(&self) -> bool {
        self.rest.is_some()
    }

    pub(crate) fn accepts_argument_count(&self, actual: usize) -> bool {
        actual == self.fixed.len() || (self.rest.is_some() && actual >= self.fixed.len())
    }
}

#[derive(Debug)]
pub(crate) struct Procedure {
    parameters: Parameters,
    body: Vec<Expr>,
    environment: Environment,
}

impl Procedure {
    pub(crate) fn new(parameters: Parameters, body: Vec<Expr>, environment: Environment) -> Self {
        Self {
            parameters,
            body,
            environment,
        }
    }

    pub(crate) fn parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub(crate) fn body(&self) -> &[Expr] {
        &self.body
    }

    pub(crate) fn environment(&self) -> &Environment {
        &self.environment
    }
}
