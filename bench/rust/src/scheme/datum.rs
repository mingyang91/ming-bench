use std::collections::HashSet;

use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::error::EvalError;
use crate::scheme::pair_value::SchemePair;
use crate::scheme::value::Value;

pub(crate) fn expr_to_datum(expression: &Expr) -> Value {
    match expression {
        Expr::Number { value, .. } => Value::Number(*value),
        Expr::Boolean { value, .. } => Value::Boolean(*value),
        Expr::String { value, .. } => Value::immutable_string(value.clone()),
        Expr::Character { value, .. } => Value::Character(*value),
        Expr::Symbol { name, .. } => Value::Symbol(name.clone()),
        Expr::List { items, .. } => items.iter().rev().fold(Value::EmptyList, |tail, item| {
            Value::pair(expr_to_datum(item), tail)
        }),
    }
}

pub(crate) fn datum_to_expr(value: &Value, location: SourceLocation) -> Result<Expr, EvalError> {
    datum_to_expr_with_seen(value, location, &mut HashSet::new())
}

fn datum_to_expr_with_seen(
    value: &Value,
    location: SourceLocation,
    seen_pairs: &mut HashSet<usize>,
) -> Result<Expr, EvalError> {
    match value {
        Value::Number(value) => Ok(Expr::number(*value, location)),
        Value::Boolean(value) => Ok(Expr::boolean(*value, location)),
        Value::String(value) => Ok(Expr::string(value.as_string(), location)),
        Value::Character(value) => Ok(Expr::character(*value, location)),
        Value::Symbol(value) => Ok(Expr::symbol(value.clone(), location)),
        Value::EmptyList => Ok(Expr::list(Vec::new(), location)),
        Value::Pair(pair) => pair_to_expr(pair, location, seen_pairs),
        _ => Err(EvalError::InvalidSyntaxDatum {
            location,
            detail: "datum->syntax requires a datum made from numbers, booleans, strings, characters, symbols, or proper lists",
        }),
    }
}

fn pair_to_expr(
    pair: &SchemePair,
    location: SourceLocation,
    seen_pairs: &mut HashSet<usize>,
) -> Result<Expr, EvalError> {
    let pair_id = pair.id();
    if !seen_pairs.insert(pair_id) {
        return Err(EvalError::InvalidSyntaxDatum {
            location,
            detail: "datum->syntax cannot convert a circular list",
        });
    }

    let result =
        pair_to_expr_items(pair, location, seen_pairs).map(|items| Expr::list(items, location));
    seen_pairs.remove(&pair_id);
    result
}

fn pair_to_expr_items(
    pair: &SchemePair,
    location: SourceLocation,
    seen_pairs: &mut HashSet<usize>,
) -> Result<Vec<Expr>, EvalError> {
    let (car, cdr) = pair.parts();
    let mut items = vec![datum_to_expr_with_seen(&car, location, seen_pairs)?];
    items.extend(cdr_to_expr_items(&cdr, location, seen_pairs)?);
    Ok(items)
}

fn cdr_to_expr_items(
    cdr: &Value,
    location: SourceLocation,
    seen_pairs: &mut HashSet<usize>,
) -> Result<Vec<Expr>, EvalError> {
    match cdr {
        Value::EmptyList => Ok(Vec::new()),
        Value::Pair(pair) => pair_to_expr_items(pair, location, seen_pairs),
        _ => Err(EvalError::InvalidSyntaxDatum {
            location,
            detail: "datum->syntax requires a proper list datum",
        }),
    }
}
