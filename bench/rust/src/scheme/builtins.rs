use crate::scheme::ast::SourceLocation;
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
    Cons,
    Car,
    Cdr,
    Null,
    List,
    Length,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
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
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::Null => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
        }
    }
}

pub fn install_builtins(environment: &Environment) {
    const BUILTIN_PROCEDURES: &[BuiltinProcedure] = &[
        BuiltinProcedure::Add,
        BuiltinProcedure::Sub,
        BuiltinProcedure::Mul,
        BuiltinProcedure::Div,
        BuiltinProcedure::LessThan,
        BuiltinProcedure::GreaterThan,
        BuiltinProcedure::Equal,
        BuiltinProcedure::LessEqual,
        BuiltinProcedure::Not,
        BuiltinProcedure::Cons,
        BuiltinProcedure::Car,
        BuiltinProcedure::Cdr,
        BuiltinProcedure::Null,
        BuiltinProcedure::List,
        BuiltinProcedure::Length,
        BuiltinProcedure::StringPred,
        BuiltinProcedure::NumberPred,
        BuiltinProcedure::BooleanPred,
        BuiltinProcedure::PairPred,
        BuiltinProcedure::SymbolPred,
    ];

    BUILTIN_PROCEDURES.iter().copied().for_each(|procedure| {
        environment.define(procedure.name(), Value::Builtin(procedure));
    });
}

pub fn apply_builtin(
    procedure: BuiltinProcedure,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    match procedure {
        BuiltinProcedure::Add => eval_add(arguments, location),
        BuiltinProcedure::Sub => eval_sub(arguments, location),
        BuiltinProcedure::Mul => eval_mul(arguments, location),
        BuiltinProcedure::Div => eval_div(arguments, location),
        BuiltinProcedure::LessThan => {
            eval_comparison("<", arguments, location, |left, right| left < right)
        }
        BuiltinProcedure::GreaterThan => {
            eval_comparison(">", arguments, location, |left, right| left > right)
        }
        BuiltinProcedure::Equal => {
            eval_comparison("=", arguments, location, |left, right| left == right)
        }
        BuiltinProcedure::LessEqual => {
            eval_comparison("<=", arguments, location, |left, right| left <= right)
        }
        BuiltinProcedure::Not => eval_not(arguments, location),
        BuiltinProcedure::Cons => eval_cons(arguments, location),
        BuiltinProcedure::Car => eval_car(arguments, location),
        BuiltinProcedure::Cdr => eval_cdr(arguments, location),
        BuiltinProcedure::Null => eval_null(arguments, location),
        BuiltinProcedure::List => Ok(eval_list(arguments)),
        BuiltinProcedure::Length => eval_length(arguments, location),
        BuiltinProcedure::StringPred => {
            eval_type_predicate("string?", arguments, location, is_string)
        }
        BuiltinProcedure::NumberPred => {
            eval_type_predicate("number?", arguments, location, is_number)
        }
        BuiltinProcedure::BooleanPred => {
            eval_type_predicate("boolean?", arguments, location, is_boolean)
        }
        BuiltinProcedure::PairPred => eval_type_predicate("pair?", arguments, location, is_pair),
        BuiltinProcedure::SymbolPred => {
            eval_type_predicate("symbol?", arguments, location, is_symbol)
        }
    }
}

fn eval_add(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let total: i64 = number_arguments(arguments, location)?.into_iter().sum();
    Ok(Value::Integer(total))
}

fn eval_sub(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments, location)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgumentCount {
            location,
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

fn eval_mul(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let product: i64 = number_arguments(arguments, location)?.into_iter().product();
    Ok(Value::Integer(product))
}

fn eval_div(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments, location)?;

    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "/",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "/",
            expected: ArgCount::AtLeast(2),
            got: 1,
        });
    }

    rest.iter()
        .try_fold(*first, |quotient, value| divide(quotient, *value, location))
        .map(Value::Integer)
}

fn eval_comparison(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    compare: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments, location)?;

    let [first, second, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
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

fn eval_not(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [argument] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "not",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(Value::Boolean(!argument.is_truthy()))
}

fn eval_cons(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [car, cdr] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "cons",
            expected: ArgCount::Exactly(2),
            got: arguments.len(),
        });
    };

    Ok(Value::Pair(Box::new(car.clone()), Box::new(cdr.clone())))
}

fn eval_car(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let pair = unary_argument("car", arguments, location)?;
    let Value::Pair(car, _) = pair else {
        return Err(EvalError::TypeMismatch {
            location,
            expected: "pair",
            found: pair.type_name(),
        });
    };

    Ok((**car).clone())
}

fn eval_cdr(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let pair = unary_argument("cdr", arguments, location)?;
    let Value::Pair(_, cdr) = pair else {
        return Err(EvalError::TypeMismatch {
            location,
            expected: "pair",
            found: pair.type_name(),
        });
    };

    Ok((**cdr).clone())
}

fn eval_null(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    eval_type_predicate("null?", arguments, location, |value| {
        matches!(value, Value::EmptyList)
    })
}

fn eval_list(arguments: &[Value]) -> Value {
    arguments
        .iter()
        .rev()
        .cloned()
        .fold(Value::EmptyList, |tail, value| {
            Value::Pair(Box::new(value), Box::new(tail))
        })
}

fn eval_length(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let list = unary_argument("length", arguments, location)?;
    let length = list_length(list, location)?;

    Ok(Value::Integer(
        i64::try_from(length).expect("list length should fit in i64"),
    ))
}

fn eval_type_predicate(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let argument = unary_argument(procedure, arguments, location)?;
    Ok(Value::Boolean(predicate(argument)))
}

fn unary_argument<'a>(
    procedure: &'static str,
    arguments: &'a [Value],
    location: SourceLocation,
) -> Result<&'a Value, EvalError> {
    let [argument] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure,
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(argument)
}

fn number_arguments(arguments: &[Value], location: SourceLocation) -> Result<Vec<i64>, EvalError> {
    arguments
        .iter()
        .map(|argument| argument.expect_number(location))
        .collect()
}

fn list_length(value: &Value, location: SourceLocation) -> Result<usize, EvalError> {
    let mut length = 0usize;
    let mut current = value;

    loop {
        match current {
            Value::EmptyList => return Ok(length),
            Value::Pair(_, next) => {
                length += 1;
                current = next.as_ref();
            }
            _ => {
                return Err(EvalError::TypeMismatch {
                    location,
                    expected: "list",
                    found: current.type_name(),
                });
            }
        }
    }
}

fn divide(left: i64, right: i64, location: SourceLocation) -> Result<i64, EvalError> {
    if right == 0 {
        return Err(EvalError::DivisionByZero { location });
    }

    Ok(left / right)
}

fn is_string(value: &Value) -> bool {
    matches!(value, Value::String(_))
}

fn is_number(value: &Value) -> bool {
    matches!(value, Value::Integer(_))
}

fn is_boolean(value: &Value) -> bool {
    matches!(value, Value::Boolean(_))
}

fn is_pair(value: &Value) -> bool {
    matches!(value, Value::Pair(_, _))
}

fn is_symbol(value: &Value) -> bool {
    matches!(value, Value::Symbol(_))
}
