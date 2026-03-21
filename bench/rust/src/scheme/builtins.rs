use crate::scheme::environment::Environment;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum BuiltinProcedure {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
}

impl BuiltinProcedure {
    pub fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
        }
    }
}

pub fn install_builtins(environment: &Environment) {
    [
        BuiltinProcedure::Add,
        BuiltinProcedure::Sub,
        BuiltinProcedure::Mul,
        BuiltinProcedure::Div,
        BuiltinProcedure::LessThan,
        BuiltinProcedure::GreaterThan,
        BuiltinProcedure::Equal,
        BuiltinProcedure::LessEqual,
        BuiltinProcedure::Not,
    ]
    .into_iter()
    .for_each(|procedure| {
        environment.define(procedure.name(), Value::Builtin(procedure));
    });
}

pub fn apply_builtin(procedure: BuiltinProcedure, arguments: &[Value]) -> Result<Value, EvalError> {
    match procedure {
        BuiltinProcedure::Add => eval_add(arguments),
        BuiltinProcedure::Sub => eval_sub(arguments),
        BuiltinProcedure::Mul => eval_mul(arguments),
        BuiltinProcedure::Div => eval_div(arguments),
        BuiltinProcedure::LessThan => eval_comparison("<", arguments, |left, right| left < right),
        BuiltinProcedure::GreaterThan => {
            eval_comparison(">", arguments, |left, right| left > right)
        }
        BuiltinProcedure::Equal => eval_comparison("=", arguments, |left, right| left == right),
        BuiltinProcedure::LessEqual => {
            eval_comparison("<=", arguments, |left, right| left <= right)
        }
        BuiltinProcedure::Not => eval_not(arguments),
    }
}

fn eval_add(arguments: &[Value]) -> Result<Value, EvalError> {
    let total: i64 = number_arguments(arguments)?.into_iter().sum();
    Ok(Value::Integer(total))
}

fn eval_sub(arguments: &[Value]) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments)?;

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

fn eval_mul(arguments: &[Value]) -> Result<Value, EvalError> {
    let product: i64 = number_arguments(arguments)?.into_iter().product();
    Ok(Value::Integer(product))
}

fn eval_div(arguments: &[Value]) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments)?;

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
    arguments: &[Value],
    compare: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments)?;

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

fn eval_not(arguments: &[Value]) -> Result<Value, EvalError> {
    let [argument] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            procedure: "not",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(Value::Boolean(!argument.is_truthy()))
}

fn number_arguments(arguments: &[Value]) -> Result<Vec<i64>, EvalError> {
    arguments.iter().map(Value::expect_number).collect()
}

fn divide(left: i64, right: i64) -> Result<i64, EvalError> {
    if right == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(left / right)
}
