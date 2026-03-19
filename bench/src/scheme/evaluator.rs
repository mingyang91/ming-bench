use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::value::Value;

pub(crate) fn eval_str(input: &str) -> Result<String, SchemeError> {
    let program = parser::parse_program(input)?;
    let value = eval_program(program)?;
    Ok(value.to_string())
}

fn eval_program(program: Vec<Expr>) -> Result<Value, SchemeError> {
    program
        .into_iter()
        .next_back()
        .map(Value::from)
        .ok_or(SchemeError::EmptyInput)
}
