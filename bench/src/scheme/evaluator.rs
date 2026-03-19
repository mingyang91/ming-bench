use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::value::Value;

pub(crate) fn eval_str(input: &str) -> Result<String, SchemeError> {
    let program = parser::parse_program(input)?;
    let value = eval_program(&program)?;
    Ok(value.to_string())
}

fn eval_program(program: &[Expr]) -> Result<Value, SchemeError> {
    let mut environment = Environment::new();
    let mut last_value = None;

    for expression in program {
        last_value = Some(eval_expr(expression, &mut environment)?);
    }

    last_value.ok_or(SchemeError::EmptyInput)
}

fn eval_expr(expression: &Expr, environment: &mut Environment) -> Result<Value, SchemeError> {
    match expression {
        Expr::Symbol(name) => environment
            .get(name)
            .ok_or_else(|| SchemeError::UnboundSymbol { name: name.clone() }),
        Expr::List(expressions) => eval_application(expressions, environment),
        _ => Value::from_literal(expression).ok_or(SchemeError::NonCallable {
            kind: expression_kind(expression),
        }),
    }
}

fn eval_application(
    expressions: &[Expr],
    environment: &mut Environment,
) -> Result<Value, SchemeError> {
    let (operator, operands) = expressions
        .split_first()
        .ok_or(SchemeError::EmptyApplication)?;

    if let Expr::Symbol(name) = operator {
        if let Some(value) = eval_special_form(name, operands, environment)? {
            return Ok(value);
        }

        if is_builtin(name) {
            return apply_builtin(name, operands, environment);
        }
    }

    let value = eval_expr(operator, environment)?;
    Err(SchemeError::NonCallable { kind: value.kind() })
}

fn eval_special_form(
    operator: &str,
    operands: &[Expr],
    environment: &mut Environment,
) -> Result<Option<Value>, SchemeError> {
    match operator {
        "define" => eval_define(operands, environment).map(Some),
        "if" => eval_if(operands, environment).map(Some),
        "quote" => eval_quote(operands).map(Some),
        _ => Ok(None),
    }
}

fn eval_define(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    match operands {
        [Expr::Symbol(name), value_expression] => {
            let value = eval_expr(value_expression, environment)?;
            environment.define(name, value);
            Ok(Value::Void)
        }
        [target, _] => Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(target),
        }),
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "define",
            expected: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_if(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    match operands {
        [condition, consequent, alternative] => {
            let value = eval_expr(condition, environment)?;
            if value.is_truthy() {
                eval_expr(consequent, environment)
            } else {
                eval_expr(alternative, environment)
            }
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "if",
            expected: 3,
            actual: operands.len(),
        }),
    }
}

fn eval_quote(operands: &[Expr]) -> Result<Value, SchemeError> {
    match operands {
        [expression] => Ok(Value::from_quoted_expr(expression)),
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "quote",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn apply_builtin(
    operator: &str,
    operands: &[Expr],
    environment: &mut Environment,
) -> Result<Value, SchemeError> {
    match operator {
        "+" => eval_addition(operands, environment),
        "-" => eval_subtraction(operands, environment),
        "*" => eval_multiplication(operands, environment),
        "/" => eval_division(operands, environment),
        "<" => eval_comparison("<", operands, environment, |lhs, rhs| lhs < rhs),
        ">" => eval_comparison(">", operands, environment, |lhs, rhs| lhs > rhs),
        "=" => eval_comparison("=", operands, environment, |lhs, rhs| lhs == rhs),
        "<=" => eval_comparison("<=", operands, environment, |lhs, rhs| lhs <= rhs),
        "not" => eval_not(operands, environment),
        "and" => eval_and(operands, environment),
        "or" => eval_or(operands, environment),
        _ => Err(SchemeError::UnboundSymbol {
            name: operator.to_owned(),
        }),
    }
}

fn eval_addition(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    let sum = eval_numbers("+", operands, environment)?.into_iter().sum();
    Ok(Value::Integer(sum))
}

fn eval_subtraction(
    operands: &[Expr],
    environment: &mut Environment,
) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("-", operands, environment)?;

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

fn eval_multiplication(
    operands: &[Expr],
    environment: &mut Environment,
) -> Result<Value, SchemeError> {
    let product = eval_numbers("*", operands, environment)?
        .into_iter()
        .product();
    Ok(Value::Integer(product))
}

fn eval_division(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("/", operands, environment)?;

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

fn eval_comparison<F>(
    operator: &'static str,
    operands: &[Expr],
    environment: &mut Environment,
    compare: F,
) -> Result<Value, SchemeError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_numbers(operator, operands, environment)?;

    match numbers.as_slice() {
        [] | [_] => Err(SchemeError::TooFewArguments {
            operator,
            min: 2,
            actual: numbers.len(),
        }),
        _ => Ok(Value::Boolean(
            numbers.windows(2).all(|pair| compare(pair[0], pair[1])),
        )),
    }
}

fn eval_not(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
            Ok(Value::Boolean(!value.is_truthy()))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "not",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_and(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    let mut last_value = Value::Boolean(true);

    for operand in operands {
        let value = eval_expr(operand, environment)?;

        if !value.is_truthy() {
            return Ok(value);
        }

        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(operands: &[Expr], environment: &mut Environment) -> Result<Value, SchemeError> {
    for operand in operands {
        let value = eval_expr(operand, environment)?;

        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_numbers(
    operator: &'static str,
    operands: &[Expr],
    environment: &mut Environment,
) -> Result<Vec<i64>, SchemeError> {
    let mut numbers = Vec::with_capacity(operands.len());

    for operand in operands {
        let value = eval_expr(operand, environment)?;
        numbers.push(expect_number(operator, value)?);
    }

    Ok(numbers)
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

fn is_builtin(operator: &str) -> bool {
    matches!(
        operator,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | "not" | "and" | "or"
    )
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
