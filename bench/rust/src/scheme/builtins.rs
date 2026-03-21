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
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    CharPred,
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
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::CharPred => "char?",
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
        BuiltinProcedure::Display,
        BuiltinProcedure::Write,
        BuiltinProcedure::Newline,
        BuiltinProcedure::StringAppend,
        BuiltinProcedure::StringLength,
        BuiltinProcedure::Substring,
        BuiltinProcedure::StringToNumber,
        BuiltinProcedure::NumberToString,
        BuiltinProcedure::SymbolToString,
        BuiltinProcedure::StringToSymbol,
        BuiltinProcedure::StringRef,
        BuiltinProcedure::CharPred,
    ];

    BUILTIN_PROCEDURES.iter().copied().for_each(|procedure| {
        environment.define(procedure.name(), Value::Builtin(procedure));
    });
}

pub fn apply_builtin(
    procedure: BuiltinProcedure,
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
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
        BuiltinProcedure::Display => eval_display(arguments, location, output),
        BuiltinProcedure::Write => eval_write(arguments, location, output),
        BuiltinProcedure::Newline => eval_newline(arguments, location, output),
        BuiltinProcedure::StringAppend => eval_string_append(arguments, location),
        BuiltinProcedure::StringLength => eval_string_length(arguments, location),
        BuiltinProcedure::Substring => eval_substring(arguments, location),
        BuiltinProcedure::StringToNumber => eval_string_to_number(arguments, location),
        BuiltinProcedure::NumberToString => eval_number_to_string(arguments, location),
        BuiltinProcedure::SymbolToString => eval_symbol_to_string(arguments, location),
        BuiltinProcedure::StringToSymbol => eval_string_to_symbol(arguments, location),
        BuiltinProcedure::StringRef => eval_string_ref(arguments, location),
        BuiltinProcedure::CharPred => eval_type_predicate("char?", arguments, location, is_char),
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

fn eval_display(
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    let value = unary_argument("display", arguments, location)?;
    output.push_str(&value.render_display());
    Ok(Value::Void)
}

fn eval_write(
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    let value = unary_argument("write", arguments, location)?;
    output.push_str(&value.render());
    Ok(Value::Void)
}

fn eval_newline(
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    if !arguments.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "newline",
            expected: ArgCount::Exactly(0),
            got: arguments.len(),
        });
    }

    output.push('\n');
    Ok(Value::Void)
}

fn eval_string_append(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let combined = arguments
        .iter()
        .try_fold(String::new(), |mut combined, argument| {
            combined.push_str(argument.expect_string(location)?);
            Ok::<_, EvalError>(combined)
        })?;

    Ok(Value::String(combined))
}

fn eval_string_length(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let string = unary_argument("string-length", arguments, location)?.expect_string(location)?;
    let length = string.chars().count();

    Ok(Value::Integer(
        i64::try_from(length).expect("string length should fit in i64"),
    ))
}

fn eval_substring(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [string_value, start_value, end_value] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "substring",
            expected: ArgCount::Exactly(3),
            got: arguments.len(),
        });
    };

    let string = string_value.expect_string(location)?;
    let start = start_value.expect_number(location)?;
    let end = end_value.expect_number(location)?;
    let characters: Vec<_> = string.chars().collect();
    let length = characters.len();
    let Some(start_index) = usize::try_from(start).ok() else {
        return Err(EvalError::InvalidSubstringRange {
            location,
            start,
            end,
            length,
        });
    };
    let Some(end_index) = usize::try_from(end).ok() else {
        return Err(EvalError::InvalidSubstringRange {
            location,
            start,
            end,
            length,
        });
    };

    if start_index > end_index || end_index > length {
        return Err(EvalError::InvalidSubstringRange {
            location,
            start,
            end,
            length,
        });
    }

    Ok(Value::String(
        characters[start_index..end_index].iter().collect(),
    ))
}

fn eval_string_to_number(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let string = unary_argument("string->number", arguments, location)?.expect_string(location)?;

    match string.parse::<i64>() {
        Ok(value) => Ok(Value::Integer(value)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let number = unary_argument("number->string", arguments, location)?.expect_number(location)?;
    Ok(Value::String(number.to_string()))
}

fn eval_symbol_to_string(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let symbol = unary_argument("symbol->string", arguments, location)?.expect_symbol(location)?;
    Ok(Value::String(symbol.to_string()))
}

fn eval_string_to_symbol(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let string = unary_argument("string->symbol", arguments, location)?.expect_string(location)?;
    Ok(Value::Symbol(string.to_string()))
}

fn eval_string_ref(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [string_value, index_value] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "string-ref",
            expected: ArgCount::Exactly(2),
            got: arguments.len(),
        });
    };

    let string = string_value.expect_string(location)?;
    let index = index_value.expect_number(location)?;
    let characters: Vec<_> = string.chars().collect();
    let length = characters.len();
    let Some(index) = usize::try_from(index).ok() else {
        return Err(EvalError::StringIndexOutOfBounds {
            location,
            index: index_value.expect_number(location)?,
            length,
        });
    };

    characters
        .get(index)
        .copied()
        .map(Value::Character)
        .ok_or(EvalError::StringIndexOutOfBounds {
            location,
            index: index_value.expect_number(location)?,
            length,
        })
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

fn is_char(value: &Value) -> bool {
    matches!(value, Value::Character(_))
}
