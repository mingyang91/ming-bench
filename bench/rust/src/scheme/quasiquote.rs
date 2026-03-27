use std::cell::RefCell;
use std::rc::Rc;

use super::error::EvalError;
use super::{
    collect_list, eval, list_from_vec, make_immutable_str, Env, Expr, ExprKind, Value,
};
use super::numeric::make_rational;

pub(crate) fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()));
    }
    Ok(expr_to_value(&args[0]))
}

fn try_eval_unquote_splicing(item: &Expr, env: &mut Env, output: &mut String) -> Result<Option<Vec<Value>>, EvalError> {
    let ExprKind::List(sub) = &item.kind else { return Ok(None) };
    if sub.len() != 2 { return Ok(None) }
    let ExprKind::Symbol(s) = &sub[0].kind else { return Ok(None) };
    if s != "unquote-splicing" { return Ok(None) }
    let val = eval(&sub[1], env, output)?;
    match collect_list(&val) {
        Some(elems) => Ok(Some(elems)),
        None => Err(EvalError::Type("unquote-splicing: expected list".into())),
    }
}

pub(crate) fn eval_quasiquote(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(s) = &items[0].kind {
                if s == "unquote" && items.len() == 2 {
                    return eval(&items[1], env, output);
                }
            }
            let mut result = Vec::new();
            for item in items {
                if let Some(spliced) = try_eval_unquote_splicing(item, env, output)? {
                    result.extend(spliced);
                    continue;
                }
                result.push(eval_quasiquote(item, env, output)?);
            }
            Ok(list_from_vec(&result))
        }
        ExprKind::DottedList(heads, tail) => {
            if let ExprKind::Symbol(s) = &heads.first().map(|e| &e.kind).unwrap_or(&ExprKind::Boolean(false)) {
                if s == "unquote" && heads.len() == 1 {
                    return eval_quasiquote(tail, env, output);
                }
            }
            let mut result_items = Vec::new();
            for item in heads {
                if let Some(spliced) = try_eval_unquote_splicing(item, env, output)? {
                    result_items.extend(spliced);
                    continue;
                }
                result_items.push(eval_quasiquote(item, env, output)?);
            }
            let tail_val = eval_quasiquote(tail, env, output)?;
            let mut result = tail_val;
            for item in result_items.into_iter().rev() {
                result = Value::Pair(Rc::new(RefCell::new((item, result))));
            }
            Ok(result)
        }
        _ => Ok(expr_to_value(expr)),
    }
}

pub(crate) fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => make_immutable_str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
        ExprKind::DottedList(heads, tail) => {
            let tail_val = expr_to_value(tail);
            let mut result = tail_val;
            for item in heads.iter().rev() {
                result = Value::Pair(Rc::new(RefCell::new((expr_to_value(item), result))));
            }
            result
        }
    }
}
