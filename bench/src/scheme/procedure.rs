use crate::scheme::environment::Environment;
use crate::scheme::parser::Expr;

#[derive(Debug)]
pub(crate) struct Procedure {
    parameters: Vec<String>,
    body: Vec<Expr>,
    environment: Environment,
}

impl Procedure {
    pub(crate) fn new(parameters: Vec<String>, body: Vec<Expr>, environment: Environment) -> Self {
        Self {
            parameters,
            body,
            environment,
        }
    }

    pub(crate) fn parameters(&self) -> &[String] {
        &self.parameters
    }

    pub(crate) fn body(&self) -> &[Expr] {
        &self.body
    }

    pub(crate) fn environment(&self) -> &Environment {
        &self.environment
    }
}
