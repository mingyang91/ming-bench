use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::value::Value;

pub(crate) fn eval_str(input: &str) -> Result<String, SchemeError> {
    let program = parser::parse_program(input)?;
    let value = eval_program(&program)?;
    Ok(value.to_string())
}

fn eval_program(program: &[Expr]) -> Result<Value, SchemeError> {
    let mut last_value = None;

    for expression in program {
        last_value = Some(eval_expr(expression)?);
    }

    last_value.ok_or(SchemeError::EmptyInput)
}

fn eval_expr(expression: &Expr) -> Result<Value, SchemeError> {
    match expression {
        Expr::Symbol(name) => Err(SchemeError::UnboundSymbol { name: name.clone() }),
        Expr::List(expressions) => eval_application(expressions),
        _ => Value::from_literal(expression).ok_or(SchemeError::NonCallable {
            kind: expression_kind(expression),
        }),
    }
}

fn eval_application(expressions: &[Expr]) -> Result<Value, SchemeError> {
    let (operator, operands) = expressions
        .split_first()
        .ok_or(SchemeError::EmptyApplication)?;

    match operator {
        Expr::Symbol(name) => apply_builtin(name, operands),
        _ => {
            let value = eval_expr(operator)?;
            Err(SchemeError::NonCallable { kind: value.kind() })
        }
    }
}

fn apply_builtin(operator: &str, operands: &[Expr]) -> Result<Value, SchemeError> {
    match operator {
        "+" => eval_addition(operands),
        "-" => eval_subtraction(operands),
        "*" => eval_multiplication(operands),
        "/" => eval_division(operands),
        _ => Err(SchemeError::UnboundSymbol {
            name: operator.to_owned(),
        }),
    }
}

fn eval_addition(operands: &[Expr]) -> Result<Value, SchemeError> {
    let sum = eval_numbers("+", operands)?.into_iter().sum();
    Ok(Value::Integer(sum))
}

fn eval_subtraction(operands: &[Expr]) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("-", operands)?;

    match numbers.as_slice() {
        [] => Err(SchemeError::TooFewArguments {
            operator: "-",
            min: 1,
            actual: 0,
        }),
        [value] => Ok(Value::Integer(-*value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_multiplication(operands: &[Expr]) -> Result<Value, SchemeError> {
    let product = eval_numbers("*", operands)?.into_iter().product();
    Ok(Value::Integer(product))
}

fn eval_division(operands: &[Expr]) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("/", operands)?;

    match numbers.as_slice() {
        [] | [_] => Err(SchemeError::TooFewArguments {
            operator: "/",
            min: 2,
            actual: numbers.len(),
        }),
        [first, rest @ ..] => {
            let quotient = rest
                .iter()
                .try_fold(*first, |acc, value| divide_numbers(acc, *value))?;
            Ok(Value::Integer(quotient))
        }
    }
}

fn eval_numbers(operator: &'static str, operands: &[Expr]) -> Result<Vec<i64>, SchemeError> {
    operands
        .iter()
        .map(|operand| eval_expr(operand).and_then(|value| expect_number(operator, value)))
        .collect()
}

fn expect_number(operator: &'static str, value: Value) -> Result<i64, SchemeError> {
    match value {
        Value::Integer(number) => Ok(number),
        _ => Err(SchemeError::ExpectedNumber {
            operator,
            found: value,
        }),
    }
}

fn divide_numbers(lhs: i64, rhs: i64) -> Result<i64, SchemeError> {
    if rhs == 0 {
        Err(SchemeError::DivisionByZero)
    } else {
        Ok(lhs / rhs)
    }
}

fn expression_kind(expression: &Expr) -> &'static str {
    match expression {
        Expr::Integer(_) => "number",
        Expr::Boolean(_) => "boolean",
        Expr::String(_) => "string",
        Expr::Symbol(_) => "symbol",
        Expr::List(_) => "list",
    }
}
