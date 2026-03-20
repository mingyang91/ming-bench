use crate::scheme::error::EvalError;
use crate::scheme::evaluator::Evaluator;
use crate::scheme::parser::parse_program;
use crate::scheme::runtime::render_value;

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let forms = parse_program(input)?;
    let mut evaluator = Evaluator::new();
    let value = evaluator.evaluate(forms)?;
    Ok(render_value(&value))
}
