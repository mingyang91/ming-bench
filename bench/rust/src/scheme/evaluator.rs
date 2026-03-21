use crate::scheme::ast::Expr;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::value::Value;

pub fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    let Some((last_expression, prefix)) = expressions.split_last() else {
        return Err(EvalError::EmptyProgram);
    };

    prefix
        .iter()
        .try_for_each(|expression| eval_expr(expression).map(|_| ()))?;

    eval_expr(last_expression)
}

fn eval_expr(expression: &Expr) -> Result<Value, EvalError> {
    match expression {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::EmptyApplication);
    };

    let Expr::Symbol(name) = operator else {
        return Err(EvalError::NotCallable {
            expression: format!("{operator:?}"),
        });
    };

    match name.as_str() {
        "and" => eval_and(arguments),
        "or" => eval_or(arguments),
        "+" => eval_add(arguments),
        "-" => eval_sub(arguments),
        "*" => eval_mul(arguments),
        "/" => eval_div(arguments),
        "<" => eval_comparison("<", arguments, |left, right| left < right),
        ">" => eval_comparison(">", arguments, |left, right| left > right),
        "=" => eval_comparison("=", arguments, |left, right| left == right),
        "<=" => eval_comparison("<=", arguments, |left, right| left <= right),
        "not" => eval_not(arguments),
        _ => Err(EvalError::UnknownProcedure { name: name.clone() }),
    }
}

fn eval_and(arguments: &[Expr]) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr]) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(false);

    for argument in arguments {
        let value = eval_expr(argument)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_add(arguments: &[Expr]) -> Result<Value, EvalError> {
    let total: i64 = eval_number_arguments(arguments)?.into_iter().sum();
    Ok(Value::Integer(total))
}

fn eval_sub(arguments: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_arguments(arguments)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgumentCount {
            procedure: "-",
            expected: ArgCount::AtLeast(1),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |total, value| total - value),
        )),
    }
}

fn eval_mul(arguments: &[Expr]) -> Result<Value, EvalError> {
    let product: i64 = eval_number_arguments(arguments)?.into_iter().product();
    Ok(Value::Integer(product))
}

fn eval_div(arguments: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_arguments(arguments)?;

    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            procedure: "/",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            procedure: "/",
            expected: ArgCount::AtLeast(2),
            got: 1,
        });
    }

    rest.iter()
        .try_fold(*first, |quotient, value| divide(quotient, *value))
        .map(Value::Integer)
}

fn eval_comparison(
    procedure: &'static str,
    arguments: &[Expr],
    compare: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = eval_number_arguments(arguments)?;

    let [first, second, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            procedure,
            expected: ArgCount::AtLeast(2),
            got: numbers.len(),
        });
    };

    let is_sorted = std::iter::once((*first, *second))
        .chain(rest.iter().scan(*second, |left, value| {
            let pair = (*left, *value);
            *left = *value;
            Some(pair)
        }))
        .all(|(left, right)| compare(left, right));

    Ok(Value::Boolean(is_sorted))
}

fn eval_not(arguments: &[Expr]) -> Result<Value, EvalError> {
    let [argument] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            procedure: "not",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(Value::Boolean(!eval_expr(argument)?.is_truthy()))
}

fn eval_number_arguments(arguments: &[Expr]) -> Result<Vec<i64>, EvalError> {
    arguments
        .iter()
        .map(eval_expr)
        .map(|value| value.and_then(|value| value.expect_number()))
        .collect()
}

fn divide(left: i64, right: i64) -> Result<i64, EvalError> {
    if right == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(left / right)
}
