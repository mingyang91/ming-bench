use crate::scheme::ast::SourceLocation;
use crate::scheme::environment::Environment;
use crate::scheme::equality::{is_eq, is_equal, is_eqv};
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::evaluator::apply_callable;
use crate::scheme::string_value::StringMutationError;
use crate::scheme::value::{list_from_values, Value};
use crate::scheme::vector_value::VectorMutationError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinProcedure {
    Add,
    Sub,
    Mul,
    Div,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    GreaterEqual,
    ZeroPred,
    PositivePred,
    NegativePred,
    OddPred,
    EvenPred,
    Not,
    Cons,
    Car,
    Cdr,
    Null,
    List,
    ListRef,
    ListTail,
    ListPred,
    Length,
    Reverse,
    Assoc,
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
    StringCopy,
    StringSet,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLessThan,
    StringToList,
    ListToString,
    CharToInteger,
    IntegerToChar,
    StringEqual,
    StringLessThan,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Map,
    Apply,
    Eq,
    Eqv,
    EqualPred,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
}

impl BuiltinProcedure {
    pub fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Abs => "abs",
            Self::Modulo => "modulo",
            Self::Remainder => "remainder",
            Self::Quotient => "quotient",
            Self::Min => "min",
            Self::Max => "max",
            Self::Expt => "expt",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::GreaterEqual => ">=",
            Self::ZeroPred => "zero?",
            Self::PositivePred => "positive?",
            Self::NegativePred => "negative?",
            Self::OddPred => "odd?",
            Self::EvenPred => "even?",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::Null => "null?",
            Self::List => "list",
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPred => "list?",
            Self::Length => "length",
            Self::Reverse => "reverse",
            Self::Assoc => "assoc",
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
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::CharPred => "char?",
            Self::CharAlphabeticPred => "char-alphabetic?",
            Self::CharNumericPred => "char-numeric?",
            Self::CharUpcase => "char-upcase",
            Self::CharDowncase => "char-downcase",
            Self::CharEqual => "char=?",
            Self::CharLessThan => "char<?",
            Self::StringToList => "string->list",
            Self::ListToString => "list->string",
            Self::CharToInteger => "char->integer",
            Self::IntegerToChar => "integer->char",
            Self::StringEqual => "string=?",
            Self::StringLessThan => "string<?",
            Self::StringCiEqual => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::Map => "map",
            Self::Apply => "apply",
            Self::Eq => "eq?",
            Self::Eqv => "eqv?",
            Self::EqualPred => "equal?",
            Self::Vector => "vector",
            Self::MakeVector => "make-vector",
            Self::VectorRef => "vector-ref",
            Self::VectorSet => "vector-set!",
            Self::VectorLength => "vector-length",
            Self::VectorPred => "vector?",
            Self::VectorToList => "vector->list",
            Self::ListToVector => "list->vector",
        }
    }
}

pub fn install_builtins(environment: &Environment) {
    const BUILTIN_PROCEDURES: &[BuiltinProcedure] = &[
        BuiltinProcedure::Add,
        BuiltinProcedure::Sub,
        BuiltinProcedure::Mul,
        BuiltinProcedure::Div,
        BuiltinProcedure::Abs,
        BuiltinProcedure::Modulo,
        BuiltinProcedure::Remainder,
        BuiltinProcedure::Quotient,
        BuiltinProcedure::Min,
        BuiltinProcedure::Max,
        BuiltinProcedure::Expt,
        BuiltinProcedure::LessThan,
        BuiltinProcedure::GreaterThan,
        BuiltinProcedure::Equal,
        BuiltinProcedure::LessEqual,
        BuiltinProcedure::GreaterEqual,
        BuiltinProcedure::ZeroPred,
        BuiltinProcedure::PositivePred,
        BuiltinProcedure::NegativePred,
        BuiltinProcedure::OddPred,
        BuiltinProcedure::EvenPred,
        BuiltinProcedure::Not,
        BuiltinProcedure::Cons,
        BuiltinProcedure::Car,
        BuiltinProcedure::Cdr,
        BuiltinProcedure::Null,
        BuiltinProcedure::List,
        BuiltinProcedure::ListRef,
        BuiltinProcedure::ListTail,
        BuiltinProcedure::ListPred,
        BuiltinProcedure::Length,
        BuiltinProcedure::Reverse,
        BuiltinProcedure::Assoc,
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
        BuiltinProcedure::StringCopy,
        BuiltinProcedure::StringSet,
        BuiltinProcedure::CharPred,
        BuiltinProcedure::CharAlphabeticPred,
        BuiltinProcedure::CharNumericPred,
        BuiltinProcedure::CharUpcase,
        BuiltinProcedure::CharDowncase,
        BuiltinProcedure::CharEqual,
        BuiltinProcedure::CharLessThan,
        BuiltinProcedure::StringToList,
        BuiltinProcedure::ListToString,
        BuiltinProcedure::CharToInteger,
        BuiltinProcedure::IntegerToChar,
        BuiltinProcedure::StringEqual,
        BuiltinProcedure::StringLessThan,
        BuiltinProcedure::StringCiEqual,
        BuiltinProcedure::StringUpcase,
        BuiltinProcedure::StringDowncase,
        BuiltinProcedure::Map,
        BuiltinProcedure::Apply,
        BuiltinProcedure::Eq,
        BuiltinProcedure::Eqv,
        BuiltinProcedure::EqualPred,
        BuiltinProcedure::Vector,
        BuiltinProcedure::MakeVector,
        BuiltinProcedure::VectorRef,
        BuiltinProcedure::VectorSet,
        BuiltinProcedure::VectorLength,
        BuiltinProcedure::VectorPred,
        BuiltinProcedure::VectorToList,
        BuiltinProcedure::ListToVector,
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
        BuiltinProcedure::Abs => eval_abs(arguments, location),
        BuiltinProcedure::Modulo => eval_modulo(arguments, location),
        BuiltinProcedure::Remainder => eval_remainder(arguments, location),
        BuiltinProcedure::Quotient => eval_quotient(arguments, location),
        BuiltinProcedure::Min => eval_min(arguments, location),
        BuiltinProcedure::Max => eval_max(arguments, location),
        BuiltinProcedure::Expt => eval_expt(arguments, location),
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
        BuiltinProcedure::GreaterEqual => {
            eval_comparison(">=", arguments, location, |left, right| left >= right)
        }
        BuiltinProcedure::ZeroPred => {
            eval_number_predicate("zero?", arguments, location, |value| value == 0)
        }
        BuiltinProcedure::PositivePred => {
            eval_number_predicate("positive?", arguments, location, |value| value > 0)
        }
        BuiltinProcedure::NegativePred => {
            eval_number_predicate("negative?", arguments, location, |value| value < 0)
        }
        BuiltinProcedure::OddPred => {
            eval_number_predicate("odd?", arguments, location, |value| value % 2 != 0)
        }
        BuiltinProcedure::EvenPred => {
            eval_number_predicate("even?", arguments, location, |value| value % 2 == 0)
        }
        BuiltinProcedure::Not => eval_not(arguments, location),
        BuiltinProcedure::Cons => eval_cons(arguments, location),
        BuiltinProcedure::Car => eval_car(arguments, location),
        BuiltinProcedure::Cdr => eval_cdr(arguments, location),
        BuiltinProcedure::Null => eval_null(arguments, location),
        BuiltinProcedure::List => Ok(eval_list(arguments)),
        BuiltinProcedure::ListRef => eval_list_ref(arguments, location),
        BuiltinProcedure::ListTail => eval_list_tail(arguments, location),
        BuiltinProcedure::ListPred => eval_list_pred(arguments, location),
        BuiltinProcedure::Length => eval_length(arguments, location),
        BuiltinProcedure::Reverse => eval_reverse(arguments, location),
        BuiltinProcedure::Assoc => eval_assoc(arguments, location),
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
        BuiltinProcedure::StringCopy => eval_string_copy(arguments, location),
        BuiltinProcedure::StringSet => eval_string_set(arguments, location),
        BuiltinProcedure::CharPred => eval_type_predicate("char?", arguments, location, is_char),
        BuiltinProcedure::CharAlphabeticPred => {
            eval_char_predicate("char-alphabetic?", arguments, location, |value| {
                value.is_alphabetic()
            })
        }
        BuiltinProcedure::CharNumericPred => {
            eval_char_predicate("char-numeric?", arguments, location, |value| {
                value.is_numeric()
            })
        }
        BuiltinProcedure::CharUpcase => {
            eval_char_transform("char-upcase", arguments, location, |value| {
                value.to_ascii_uppercase()
            })
        }
        BuiltinProcedure::CharDowncase => {
            eval_char_transform("char-downcase", arguments, location, |value| {
                value.to_ascii_lowercase()
            })
        }
        BuiltinProcedure::CharEqual => {
            eval_char_comparison("char=?", arguments, location, |left, right| left == right)
        }
        BuiltinProcedure::CharLessThan => {
            eval_char_comparison("char<?", arguments, location, |left, right| left < right)
        }
        BuiltinProcedure::StringToList => eval_string_to_list(arguments, location),
        BuiltinProcedure::ListToString => eval_list_to_string(arguments, location),
        BuiltinProcedure::CharToInteger => eval_char_to_integer(arguments, location),
        BuiltinProcedure::IntegerToChar => eval_integer_to_char(arguments, location),
        BuiltinProcedure::StringEqual => {
            eval_string_comparison("string=?", arguments, location, |left, right| left == right)
        }
        BuiltinProcedure::StringLessThan => {
            eval_string_comparison("string<?", arguments, location, |left, right| left < right)
        }
        BuiltinProcedure::StringCiEqual => {
            eval_string_comparison("string-ci=?", arguments, location, |left, right| {
                left.to_lowercase() == right.to_lowercase()
            })
        }
        BuiltinProcedure::StringUpcase => {
            eval_string_transform("string-upcase", arguments, location, |value| {
                value.to_uppercase()
            })
        }
        BuiltinProcedure::StringDowncase => {
            eval_string_transform("string-downcase", arguments, location, |value| {
                value.to_lowercase()
            })
        }
        BuiltinProcedure::Map => eval_map(arguments, location, output),
        BuiltinProcedure::Apply => eval_apply(arguments, location, output),
        BuiltinProcedure::Eq => eval_binary_value_predicate("eq?", arguments, location, is_eq),
        BuiltinProcedure::Eqv => eval_binary_value_predicate("eqv?", arguments, location, is_eqv),
        BuiltinProcedure::EqualPred => {
            eval_binary_value_predicate("equal?", arguments, location, is_equal)
        }
        BuiltinProcedure::Vector => Ok(eval_vector(arguments)),
        BuiltinProcedure::MakeVector => eval_make_vector(arguments, location),
        BuiltinProcedure::VectorRef => eval_vector_ref(arguments, location),
        BuiltinProcedure::VectorSet => eval_vector_set(arguments, location),
        BuiltinProcedure::VectorLength => eval_vector_length(arguments, location),
        BuiltinProcedure::VectorPred => {
            eval_type_predicate("vector?", arguments, location, is_vector)
        }
        BuiltinProcedure::VectorToList => eval_vector_to_list(arguments, location),
        BuiltinProcedure::ListToVector => eval_list_to_vector(arguments, location),
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

fn eval_abs(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let value = unary_number_argument("abs", arguments, location)?;
    Ok(Value::Integer(value.abs()))
}

fn eval_modulo(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_number_arguments("modulo", arguments, location)?;
    let remainder = divide_remainder(dividend, divisor, location)?;
    let result = if remainder != 0 && (remainder > 0) != (divisor > 0) {
        remainder + divisor
    } else {
        remainder
    };

    Ok(Value::Integer(result))
}

fn eval_remainder(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_number_arguments("remainder", arguments, location)?;
    Ok(Value::Integer(divide_remainder(
        dividend, divisor, location,
    )?))
}

fn eval_quotient(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_number_arguments("quotient", arguments, location)?;
    divide(dividend, divisor, location).map(Value::Integer)
}

fn eval_min(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    eval_extremum("min", arguments, location, |left, right| left.min(right))
}

fn eval_max(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    eval_extremum("max", arguments, location, |left, right| left.max(right))
}

fn eval_expt(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (base, exponent) = binary_number_arguments("expt", arguments, location)?;
    let Ok(exponent) = u32::try_from(exponent) else {
        return Err(EvalError::InvalidExponent { location, exponent });
    };

    Ok(Value::Integer(base.pow(exponent)))
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
    list_from_values(arguments)
}

fn eval_reverse(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let list = unary_argument("reverse", arguments, location)?;
    let mut items = proper_list_items(list, location)?;
    items.reverse();
    Ok(list_from_values(&items))
}

fn eval_length(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let list = unary_argument("length", arguments, location)?;
    let length = list_length(list, location)?;

    Ok(Value::Integer(
        i64::try_from(length).expect("list length should fit in i64"),
    ))
}

fn eval_list_ref(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (list, index) = list_argument_index("list-ref", arguments, location)?;
    let tail = list_tail_at(list, index, location)?;
    match tail {
        Value::Pair(item, _) => Ok((**item).clone()),
        Value::EmptyList => Err(EvalError::ListIndexOutOfBounds { location, index }),
        _ => Err(EvalError::TypeMismatch {
            location,
            expected: "pair",
            found: tail.type_name(),
        }),
    }
}

fn eval_list_tail(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (list, index) = list_argument_index("list-tail", arguments, location)?;
    if !matches!(list, Value::EmptyList | Value::Pair(_, _)) {
        return Err(EvalError::TypeMismatch {
            location,
            expected: "list",
            found: list.type_name(),
        });
    }

    list_tail_at(list, index, location).cloned()
}

fn eval_list_pred(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let value = unary_argument("list?", arguments, location)?;
    Ok(Value::Boolean(is_proper_list(value)))
}

fn eval_assoc(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let (key, association_list) = binary_arguments("assoc", arguments, location)?;
    let mut current = association_list;

    loop {
        let Some((entry, remainder)) = assoc_list_node(current, location)? else {
            return Ok(Value::Boolean(false));
        };

        if is_equal(key, assoc_entry_key(entry, location)?) {
            return Ok(entry.clone());
        }

        current = remainder;
    }
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
            combined.push_str(&argument.expect_string(location)?.as_string());
            Ok::<_, EvalError>(combined)
        })?;

    Ok(Value::immutable_string(combined))
}

fn eval_string_length(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let string = unary_argument("string-length", arguments, location)?.expect_string(location)?;
    let length = string.len();

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
    let length = string.len();
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

    Ok(Value::String(string.substring(start_index, end_index)))
}

fn eval_string_to_number(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let string = unary_argument("string->number", arguments, location)?.expect_string(location)?;

    match string.as_string().parse::<i64>() {
        Ok(value) => Ok(Value::Integer(value)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let number = unary_argument("number->string", arguments, location)?.expect_number(location)?;
    Ok(Value::immutable_string(number.to_string()))
}

fn eval_symbol_to_string(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let symbol = unary_argument("symbol->string", arguments, location)?.expect_symbol(location)?;
    Ok(Value::immutable_string(symbol.to_string()))
}

fn eval_string_to_symbol(
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let string = unary_argument("string->symbol", arguments, location)?.expect_string(location)?;
    Ok(Value::Symbol(string.as_string()))
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
    let length = string.len();
    let Some(index) = usize::try_from(index).ok() else {
        return Err(EvalError::StringIndexOutOfBounds {
            location,
            index,
            length,
        });
    };

    string
        .char_at(index)
        .map(Value::Character)
        .ok_or(EvalError::StringIndexOutOfBounds {
            location,
            index: index_value.expect_number(location)?,
            length,
        })
}

fn eval_string_copy(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let string = unary_argument("string-copy", arguments, location)?.expect_string(location)?;
    Ok(Value::immutable_string(string.as_string()))
}

fn eval_string_set(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [string_value, index_value, character_value] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "string-set!",
            expected: ArgCount::Exactly(3),
            got: arguments.len(),
        });
    };

    let string = string_value.expect_string(location)?;
    let index = index_value.expect_number(location)?;
    let character = character_value.expect_char(location)?;
    let Some(index) = usize::try_from(index).ok() else {
        return Err(EvalError::StringIndexOutOfBounds {
            location,
            index,
            length: string.len(),
        });
    };

    match string.set_char(index, character) {
        Ok(()) => Ok(Value::Void),
        Err(StringMutationError::Immutable) => Err(EvalError::ImmutableString {
            location,
            procedure: "string-set!",
        }),
        Err(StringMutationError::IndexOutOfBounds { length }) => {
            Err(EvalError::StringIndexOutOfBounds {
                location,
                index: index_value.expect_number(location)?,
                length,
            })
        }
    }
}

fn eval_string_to_list(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let string = unary_argument("string->list", arguments, location)?.expect_string(location)?;
    let characters: Vec<_> = string.as_string().chars().map(Value::Character).collect();
    Ok(eval_list(&characters))
}

fn eval_list_to_string(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let list = unary_argument("list->string", arguments, location)?;
    let characters = proper_list_items(list, location)?
        .into_iter()
        .map(|value| value.expect_char(location))
        .collect::<Result<String, _>>()?;

    Ok(Value::immutable_string(characters))
}

fn eval_char_to_integer(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let character = unary_argument("char->integer", arguments, location)?.expect_char(location)?;
    Ok(Value::Integer(i64::from(u32::from(character))))
}

fn eval_integer_to_char(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let value = unary_argument("integer->char", arguments, location)?.expect_number(location)?;
    let Some(code_point) = u32::try_from(value).ok() else {
        return Err(EvalError::InvalidCharacterCodePoint { location, value });
    };
    let Some(character) = char::from_u32(code_point) else {
        return Err(EvalError::InvalidCharacterCodePoint { location, value });
    };

    Ok(Value::Character(character))
}

fn eval_char_predicate(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    predicate: impl Fn(char) -> bool,
) -> Result<Value, EvalError> {
    let value = unary_argument(procedure, arguments, location)?.expect_char(location)?;
    Ok(Value::Boolean(predicate(value)))
}

fn eval_char_transform(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    transform: impl Fn(char) -> char,
) -> Result<Value, EvalError> {
    let value = unary_argument(procedure, arguments, location)?.expect_char(location)?;
    Ok(Value::Character(transform(value)))
}

fn eval_char_comparison(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    compare: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let characters = char_arguments(procedure, arguments, location)?;
    Ok(Value::Boolean(
        characters.windows(2).all(|pair| compare(pair[0], pair[1])),
    ))
}

fn eval_string_comparison(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    compare: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let strings = string_arguments(procedure, arguments, location)?;
    Ok(Value::Boolean(
        strings
            .windows(2)
            .all(|pair| compare(pair[0].as_str(), pair[1].as_str())),
    ))
}

fn eval_string_transform(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    transform: impl Fn(String) -> String,
) -> Result<Value, EvalError> {
    let value = unary_argument(procedure, arguments, location)?.expect_string(location)?;
    Ok(Value::immutable_string(transform(value.as_string())))
}

fn eval_map(
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    let Some((procedure, list_arguments)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "map",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };
    if list_arguments.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "map",
            expected: ArgCount::AtLeast(2),
            got: 1,
        });
    }

    let mut iterators = list_arguments
        .iter()
        .map(|list| proper_list_items(list, location).map(|items| items.into_iter()))
        .collect::<Result<Vec<_>, _>>()?;
    let mut mapped_values = Vec::new();

    loop {
        let current_arguments = iterators.iter_mut().map(Iterator::next).collect::<Vec<_>>();
        if current_arguments.iter().any(Option::is_none) {
            break;
        }

        let current_arguments = current_arguments
            .into_iter()
            .map(|argument| {
                argument.expect("map arguments should be present after shortest-list check")
            })
            .collect::<Vec<_>>();
        mapped_values.push(
            apply_callable(procedure.clone(), &current_arguments, location, output)?
                .expect_single()?,
        );
    }

    Ok(eval_list(&mapped_values))
}

fn eval_apply(
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    let Some((procedure, list_and_prefix_arguments)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "apply",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };
    let Some((list_argument, prefix_arguments)) = list_and_prefix_arguments.split_last() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "apply",
            expected: ArgCount::AtLeast(2),
            got: 1,
        });
    };

    let applied_arguments = prefix_arguments
        .iter()
        .cloned()
        .chain(proper_list_items(list_argument, location)?)
        .collect::<Vec<_>>();

    apply_callable(procedure.clone(), &applied_arguments, location, output)
}

fn eval_binary_value_predicate(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    predicate: impl Fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let [left, right] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure,
            expected: ArgCount::Exactly(2),
            got: arguments.len(),
        });
    };

    Ok(Value::Boolean(predicate(left, right)))
}

fn eval_vector(arguments: &[Value]) -> Value {
    Value::Vector(crate::scheme::vector_value::SchemeVector::new(
        arguments.to_vec(),
    ))
}

fn eval_make_vector(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    match arguments {
        [length] => {
            let length = vector_length_value(length, location)?;
            Ok(Value::Vector(
                crate::scheme::vector_value::SchemeVector::make(length, Value::Void),
            ))
        }
        [length, fill] => {
            let length = vector_length_value(length, location)?;
            Ok(Value::Vector(
                crate::scheme::vector_value::SchemeVector::make(length, fill.clone()),
            ))
        }
        _ => Err(EvalError::WrongArgumentCount {
            location,
            procedure: "make-vector",
            expected: ArgCount::AtLeast(1),
            got: arguments.len(),
        }),
    }
}

fn eval_vector_ref(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [vector_value, index_value] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "vector-ref",
            expected: ArgCount::Exactly(2),
            got: arguments.len(),
        });
    };

    let vector = vector_value.expect_vector(location)?;
    let index = vector_index(index_value, vector.len(), location)?;
    vector.get(index).ok_or(EvalError::VectorIndexOutOfBounds {
        location,
        index: index_value.expect_number(location)?,
        length: vector.len(),
    })
}

fn eval_vector_set(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let [vector_value, index_value, new_value] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "vector-set!",
            expected: ArgCount::Exactly(3),
            got: arguments.len(),
        });
    };

    let vector = vector_value.expect_vector(location)?;
    let index = vector_index(index_value, vector.len(), location)?;

    match vector.set(index, new_value.clone()) {
        Ok(()) => Ok(Value::Void),
        Err(VectorMutationError::IndexOutOfBounds { length }) => {
            Err(EvalError::VectorIndexOutOfBounds {
                location,
                index: index_value.expect_number(location)?,
                length,
            })
        }
    }
}

fn eval_vector_length(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let vector = unary_argument("vector-length", arguments, location)?.expect_vector(location)?;
    Ok(Value::Integer(
        i64::try_from(vector.len()).expect("vector length should fit in i64"),
    ))
}

fn eval_vector_to_list(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let vector = unary_argument("vector->list", arguments, location)?.expect_vector(location)?;
    Ok(list_from_values(&vector.items()))
}

fn eval_list_to_vector(arguments: &[Value], location: SourceLocation) -> Result<Value, EvalError> {
    let list = unary_argument("list->vector", arguments, location)?;
    let elements = proper_list_items(list, location)?;
    Ok(Value::Vector(
        crate::scheme::vector_value::SchemeVector::new(elements),
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

fn unary_number_argument(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<i64, EvalError> {
    unary_argument(procedure, arguments, location)?.expect_number(location)
}

fn binary_arguments<'a>(
    procedure: &'static str,
    arguments: &'a [Value],
    location: SourceLocation,
) -> Result<(&'a Value, &'a Value), EvalError> {
    let [left, right] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure,
            expected: ArgCount::Exactly(2),
            got: arguments.len(),
        });
    };

    Ok((left, right))
}

fn binary_number_arguments(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<(i64, i64), EvalError> {
    let (left, right) = binary_arguments(procedure, arguments, location)?;
    Ok((
        left.expect_number(location)?,
        right.expect_number(location)?,
    ))
}

fn number_arguments(arguments: &[Value], location: SourceLocation) -> Result<Vec<i64>, EvalError> {
    arguments
        .iter()
        .map(|argument| argument.expect_number(location))
        .collect()
}

fn string_arguments(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Vec<String>, EvalError> {
    require_argument_count_at_least(procedure, arguments, location, 2)?;
    arguments
        .iter()
        .map(|argument| {
            argument
                .expect_string(location)
                .map(|value| value.as_string())
        })
        .collect()
}

fn char_arguments(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Vec<char>, EvalError> {
    require_argument_count_at_least(procedure, arguments, location, 2)?;
    arguments
        .iter()
        .map(|argument| argument.expect_char(location))
        .collect()
}

fn require_argument_count_at_least(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    minimum: usize,
) -> Result<(), EvalError> {
    if arguments.len() < minimum {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure,
            expected: ArgCount::AtLeast(minimum),
            got: arguments.len(),
        });
    }

    Ok(())
}

fn eval_number_predicate(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    predicate: impl Fn(i64) -> bool,
) -> Result<Value, EvalError> {
    let value = unary_number_argument(procedure, arguments, location)?;
    Ok(Value::Boolean(predicate(value)))
}

fn eval_extremum(
    procedure: &'static str,
    arguments: &[Value],
    location: SourceLocation,
    choose: impl Fn(i64, i64) -> i64,
) -> Result<Value, EvalError> {
    let numbers = number_arguments(arguments, location)?;
    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure,
            expected: ArgCount::AtLeast(1),
            got: 0,
        });
    };

    Ok(Value::Integer(
        rest.iter()
            .fold(*first, |current, value| choose(current, *value)),
    ))
}

fn vector_length_value(argument: &Value, location: SourceLocation) -> Result<usize, EvalError> {
    let length = argument.expect_number(location)?;
    usize::try_from(length).map_err(|_| EvalError::InvalidVectorLength { location, length })
}

fn vector_index(
    argument: &Value,
    length: usize,
    location: SourceLocation,
) -> Result<usize, EvalError> {
    let index = argument.expect_number(location)?;
    let Some(index) = usize::try_from(index).ok() else {
        return Err(EvalError::VectorIndexOutOfBounds {
            location,
            index,
            length,
        });
    };
    if index >= length {
        return Err(EvalError::VectorIndexOutOfBounds {
            location,
            index: argument.expect_number(location)?,
            length,
        });
    }

    Ok(index)
}

fn proper_list_items(value: &Value, location: SourceLocation) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut current = value;

    loop {
        match current {
            Value::EmptyList => return Ok(items),
            Value::Pair(car, cdr) => {
                items.push((**car).clone());
                current = cdr.as_ref();
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

fn list_argument_index<'a>(
    procedure: &'static str,
    arguments: &'a [Value],
    location: SourceLocation,
) -> Result<(&'a Value, i64), EvalError> {
    let (list, index) = binary_arguments(procedure, arguments, location)?;
    Ok((list, index.expect_number(location)?))
}

fn assoc_list_node(
    current: &Value,
    location: SourceLocation,
) -> Result<Option<(&Value, &Value)>, EvalError> {
    match current {
        Value::EmptyList => Ok(None),
        Value::Pair(entry, remainder) => Ok(Some((entry.as_ref(), remainder.as_ref()))),
        _ => Err(EvalError::TypeMismatch {
            location,
            expected: "list",
            found: current.type_name(),
        }),
    }
}

fn assoc_entry_key(entry: &Value, location: SourceLocation) -> Result<&Value, EvalError> {
    match entry {
        Value::Pair(entry_key, _) => Ok(entry_key.as_ref()),
        _ => Err(EvalError::TypeMismatch {
            location,
            expected: "pair",
            found: entry.type_name(),
        }),
    }
}

fn list_tail_at(value: &Value, index: i64, location: SourceLocation) -> Result<&Value, EvalError> {
    let Some(mut remaining) = usize::try_from(index).ok() else {
        return Err(EvalError::ListIndexOutOfBounds { location, index });
    };
    let mut current = value;

    while remaining > 0 {
        match current {
            Value::Pair(_, cdr) => {
                current = cdr.as_ref();
                remaining -= 1;
            }
            Value::EmptyList => {
                return Err(EvalError::ListIndexOutOfBounds { location, index });
            }
            _ => {
                return Err(EvalError::TypeMismatch {
                    location,
                    expected: "pair",
                    found: current.type_name(),
                });
            }
        }
    }

    Ok(current)
}

fn is_proper_list(value: &Value) -> bool {
    let mut current = value;

    loop {
        match current {
            Value::EmptyList => return true,
            Value::Pair(_, cdr) => current = cdr.as_ref(),
            _ => return false,
        }
    }
}

fn divide(left: i64, right: i64, location: SourceLocation) -> Result<i64, EvalError> {
    if right == 0 {
        return Err(EvalError::DivisionByZero { location });
    }

    Ok(left / right)
}

fn divide_remainder(left: i64, right: i64, location: SourceLocation) -> Result<i64, EvalError> {
    if right == 0 {
        return Err(EvalError::DivisionByZero { location });
    }

    Ok(left % right)
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

fn is_vector(value: &Value) -> bool {
    matches!(value, Value::Vector(_))
}
