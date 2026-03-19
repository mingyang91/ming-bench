use std::collections::HashSet;

use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::procedure::Procedure;
use crate::scheme::value::Value;

pub(crate) fn eval_str(input: &str) -> Result<String, SchemeError> {
    let program = parser::parse_program(input)?;
    let value = eval_program(&program)?;
    Ok(value.to_string())
}

fn eval_program(program: &[Expr]) -> Result<Value, SchemeError> {
    let environment = Environment::new();
    eval_sequence(program, &environment)
}

fn eval_sequence(expressions: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let mut last_value = None;

    for expression in expressions {
        last_value = Some(eval_expr(expression, environment)?);
    }

    last_value.ok_or(SchemeError::EmptyInput)
}

fn eval_expr(expression: &Expr, environment: &Environment) -> Result<Value, SchemeError> {
    if let Some(value) = Value::from_literal(expression) {
        return Ok(value);
    }

    match expression {
        Expr::Symbol(name) => environment
            .get(name)
            .ok_or_else(|| SchemeError::UnboundSymbol { name: name.clone() }),
        Expr::List(expressions) => eval_application(expressions, environment),
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => unreachable!(),
    }
}

fn eval_application(
    expressions: &[Expr],
    environment: &Environment,
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

    let callable = eval_expr(operator, environment)?;
    apply_callable(callable, operands, environment)
}

fn eval_special_form(
    operator: &str,
    operands: &[Expr],
    environment: &Environment,
) -> Result<Option<Value>, SchemeError> {
    match operator {
        "define" => eval_define(operands, environment).map(Some),
        "if" => eval_if(operands, environment).map(Some),
        "quote" => eval_quote(operands).map(Some),
        "lambda" => eval_lambda(operands, environment).map(Some),
        _ => Ok(None),
    }
}

fn eval_define(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [Expr::Symbol(name), value_expression] => {
            let value = eval_expr(value_expression, environment)?;
            environment.define(name, value);
            Ok(Value::Void)
        }
        [Expr::Symbol(_), ..] => Err(SchemeError::WrongArgumentCount {
            operator: "define",
            expected: 2,
            actual: operands.len(),
        }),
        [Expr::List(signature), body @ ..] if !body.is_empty() => {
            define_function(signature, body, environment)
        }
        [Expr::List(_)] => Err(SchemeError::TooFewArguments {
            operator: "define",
            min: 2,
            actual: operands.len(),
        }),
        [target, _] => Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(target),
        }),
        [target, ..] => Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(target),
        }),
        _ => Err(SchemeError::TooFewArguments {
            operator: "define",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    let (name, parameters) = signature
        .split_first()
        .ok_or(SchemeError::InvalidDefinitionTarget { found: "list" })?;

    let Expr::Symbol(name) = name else {
        return Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(name),
        });
    };

    let parameters = parse_parameter_names(parameters, "define")?;
    let procedure = Value::procedure(parameters, body.to_vec(), environment.clone());
    environment.define(name, procedure);
    Ok(Value::Void)
}

fn eval_if(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
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

fn eval_lambda(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [parameters, body @ ..] if !body.is_empty() => {
            let parameters = parse_parameter_list(parameters, "lambda")?;
            Ok(Value::procedure(parameters, body.to_vec(), environment.clone()))
        }
        _ => Err(SchemeError::TooFewArguments {
            operator: "lambda",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn parse_parameter_list(
    parameters: &Expr,
    operator: &'static str,
) -> Result<Vec<String>, SchemeError> {
    match parameters {
        Expr::List(parameters) => parse_parameter_names(parameters, operator),
        _ => Err(SchemeError::InvalidParameterList {
            operator,
            found: expression_kind(parameters),
        }),
    }
}

fn parse_parameter_names(
    parameters: &[Expr],
    operator: &'static str,
) -> Result<Vec<String>, SchemeError> {
    let mut names = Vec::with_capacity(parameters.len());
    let mut seen = HashSet::with_capacity(parameters.len());

    for parameter in parameters {
        let Expr::Symbol(name) = parameter else {
            return Err(SchemeError::InvalidParameterName {
                operator,
                found: expression_kind(parameter),
            });
        };

        if !seen.insert(name.clone()) {
            return Err(SchemeError::DuplicateParameter {
                operator,
                name: name.clone(),
            });
        }

        names.push(name.clone());
    }

    Ok(names)
}

fn apply_builtin(
    operator: &str,
    operands: &[Expr],
    environment: &Environment,
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
        "cons" => eval_cons(operands, environment),
        "car" => eval_car(operands, environment),
        "cdr" => eval_cdr(operands, environment),
        "null?" => eval_null(operands, environment),
        "list" => eval_list(operands, environment),
        "length" => eval_length(operands, environment),
        _ => Err(SchemeError::UnboundSymbol {
            name: operator.to_owned(),
        }),
    }
}

fn apply_callable(
    callable: Value,
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    if let Some(procedure) = callable.as_procedure() {
        return apply_procedure(procedure, operands, environment);
    }

    Err(SchemeError::NonCallable {
        kind: callable.kind(),
    })
}

fn apply_procedure(
    procedure: &Procedure,
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    if operands.len() != procedure.parameters().len() {
        return Err(SchemeError::WrongProcedureArgumentCount {
            expected: procedure.parameters().len(),
            actual: operands.len(),
        });
    }

    let arguments = eval_values(operands, environment)?;
    let call_environment = procedure.environment().child();

    for (parameter, argument) in procedure.parameters().iter().zip(arguments) {
        call_environment.define(parameter, argument);
    }

    eval_sequence(procedure.body(), &call_environment)
}

fn eval_addition(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let sum = eval_numbers("+", operands, environment)?.into_iter().sum();
    Ok(Value::Integer(sum))
}

fn eval_subtraction(
    operands: &[Expr],
    environment: &Environment,
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
    environment: &Environment,
) -> Result<Value, SchemeError> {
    let product = eval_numbers("*", operands, environment)?
        .into_iter()
        .product();
    Ok(Value::Integer(product))
}

fn eval_division(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
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
    environment: &Environment,
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

fn eval_not(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
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

fn eval_and(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
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

fn eval_or(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    for operand in operands {
        let value = eval_expr(operand, environment)?;

        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_cons(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [car_expression, cdr_expression] => {
            let car = eval_expr(car_expression, environment)?;
            let cdr = eval_expr(cdr_expression, environment)?;
            Ok(Value::pair(car, cdr))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "cons",
            expected: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_car(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => match eval_expr(operand, environment)? {
            Value::Pair(pair) => Ok(pair.car().clone()),
            found => Err(SchemeError::ExpectedPair {
                operator: "car",
                found,
            }),
        },
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "car",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_cdr(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => match eval_expr(operand, environment)? {
            Value::Pair(pair) => Ok(pair.cdr().clone()),
            found => Err(SchemeError::ExpectedPair {
                operator: "cdr",
                found,
            }),
        },
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "cdr",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_null(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
            Ok(Value::Boolean(value.is_null()))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "null?",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_list(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    Ok(Value::list(eval_values(operands, environment)?))
}

fn eval_length(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
            let length = value.list_length().map_err(|found| SchemeError::ExpectedList {
                operator: "length",
                found,
            })?;
            Ok(Value::Integer(length as i64))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "length",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_values(operands: &[Expr], environment: &Environment) -> Result<Vec<Value>, SchemeError> {
    let mut values = Vec::with_capacity(operands.len());

    for operand in operands {
        values.push(eval_expr(operand, environment)?);
    }

    Ok(values)
}

fn eval_numbers(
    operator: &'static str,
    operands: &[Expr],
    environment: &Environment,
) -> Result<Vec<i64>, SchemeError> {
    eval_values(operands, environment)?
        .into_iter()
        .map(|value| expect_number(operator, value))
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

fn is_builtin(operator: &str) -> bool {
    matches!(
        operator,
        "+"
            | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | "not"
            | "and"
            | "or"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
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
