use std::cell::RefCell;

use super::{ApplyFn, Pos, Value};
use crate::scheme::EvalError;

pub(super) fn expect_int(val: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!(
            "{call_pos}: expected integer, got {}",
            val.display_scheme()
        ))),
    }
}

pub(super) fn apply_string_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: string-append: expected string"
                        )))
                    }
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-length requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-length: expected string"
                ))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: substring requires 3 arguments"
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    let start = expect_int(&args[1], call_pos)? as usize;
                    let end = expect_int(&args[2], call_pos)? as usize;
                    if end > s.len() || start > end {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: substring: index out of range"
                        )));
                    }
                    Ok(Value::Str(s[start..end].to_string()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: substring: expected string"
                ))),
            }
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string->number requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string->number: expected string"
                ))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: number->string requires 1 argument"
                )));
            }
            Ok(Value::Str(expect_int(&args[0], call_pos)?.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: symbol->string requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: symbol->string: expected symbol"
                ))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string->symbol requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string->symbol: expected string"
                ))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-ref requires 2 arguments"
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    let idx = expect_int(&args[1], call_pos)? as usize;
                    if idx >= s.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: string-ref: index out of range"
                        )));
                    }
                    Ok(Value::Char(s.as_bytes()[idx] as char))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-ref: expected string"
                ))),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-copy requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-copy: expected string"
                ))),
            }
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string=? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string=?: expected strings"
                ))),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string<? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string<?: expected strings"
                ))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-ci=? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-ci=?: expected strings"
                ))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-upcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-upcase: expected string"
                ))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-downcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-downcase: expected string"
                ))),
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-alphabetic? requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-alphabetic?: expected char"
                ))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-numeric? requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-numeric?: expected char"
                ))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-upcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_uppercase().next().unwrap_or(*c))),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-upcase: expected char"
                ))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-downcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_lowercase().next().unwrap_or(*c))),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-downcase: expected char"
                ))),
            }
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char=? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char=?: expected chars"
                ))),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char<? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char<?: expected chars"
                ))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_numeric_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_int(a, call_pos)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: - requires at least 1 argument"
                )));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_int(&args[0], call_pos)?));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                result -= expect_int(a, call_pos)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_int(a, call_pos)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: / requires at least 2 arguments"
                )));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                let d = expect_int(a, call_pos)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(format!(
                        "{call_pos}: division by zero"
                    )));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: < requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(
                expect_int(&args[0], call_pos)? < expect_int(&args[1], call_pos)?,
            ))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: > requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(
                expect_int(&args[0], call_pos)? > expect_int(&args[1], call_pos)?,
            ))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: = requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(
                expect_int(&args[0], call_pos)? == expect_int(&args[1], call_pos)?,
            ))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: <= requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(
                expect_int(&args[0], call_pos)? <= expect_int(&args[1], call_pos)?,
            ))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: >= requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(
                expect_int(&args[0], call_pos)? >= expect_int(&args[1], call_pos)?,
            ))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: abs requires 1 argument"
                )));
            }
            Ok(Value::Integer(expect_int(&args[0], call_pos)?.abs()))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: modulo requires 2 arguments"
                )));
            }
            let a = expect_int(&args[0], call_pos)?;
            let b = expect_int(&args[1], call_pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!(
                    "{call_pos}: division by zero"
                )));
            }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: remainder requires 2 arguments"
                )));
            }
            let a = expect_int(&args[0], call_pos)?;
            let b = expect_int(&args[1], call_pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!(
                    "{call_pos}: division by zero"
                )));
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: quotient requires 2 arguments"
                )));
            }
            let a = expect_int(&args[0], call_pos)?;
            let b = expect_int(&args[1], call_pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!(
                    "{call_pos}: division by zero"
                )));
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: min requires at least 1 argument"
                )));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                result = result.min(expect_int(a, call_pos)?);
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: max requires at least 1 argument"
                )));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                result = result.max(expect_int(a, call_pos)?);
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: expt requires 2 arguments"
                )));
            }
            let base = expect_int(&args[0], call_pos)?;
            let exp = expect_int(&args[1], call_pos)?;
            if exp < 0 {
                return Err(EvalError::Type(format!(
                    "{call_pos}: expt: negative exponent"
                )));
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: zero? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: positive? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: negative? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: odd? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: even? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? % 2 == 0))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_list_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
    out: &RefCell<String>,
    apply_func: ApplyFn,
) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: cons requires 2 arguments"
                )));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Ok(Value::Pair(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: car requires 1 argument"
                )));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: car: expected non-empty list"
                ))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: cdr requires 1 argument"
                )));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: cdr: expected non-empty list"
                ))),
            }
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: length requires 1 argument"
                )));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: length: expected list"
                ))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: null? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(items) if items.is_empty()
            )))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: pair? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(
                matches!(&args[0], Value::List(items) if !items.is_empty())
                    || matches!(&args[0], Value::Pair(_, _)),
            ))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
        }
        "append" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: append requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::List(a), Value::List(b)) => {
                    let mut result = a.clone();
                    result.extend(b.iter().cloned());
                    Ok(Value::List(result))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: append: expected lists"
                ))),
            }
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list-ref requires 2 arguments"
                )));
            }
            let idx = expect_int(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(items) => {
                    if idx >= items.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: list-ref: index out of range"
                        )));
                    }
                    Ok(items[idx].clone())
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: list-ref: expected list"
                ))),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list-tail requires 2 arguments"
                )));
            }
            let idx = expect_int(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(items) => {
                    if idx > items.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: list-tail: index out of range"
                        )));
                    }
                    Ok(Value::List(items[idx..].to_vec()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: list-tail: expected list"
                ))),
            }
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: assoc requires 2 arguments"
                )));
            }
            let key = &args[0];
            match &args[1] {
                Value::List(alist) => {
                    for item in alist {
                        if let Value::List(pair) = item {
                            if !pair.is_empty() && pair[0] == *key {
                                return Ok(item.clone());
                            }
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: assoc: expected list"
                ))),
            }
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: map requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..]
                .iter()
                .map(|a| match a {
                    Value::List(items) => Ok(items),
                    _ => Err(EvalError::Type(format!(
                        "{call_pos}: map: expected list"
                    ))),
                })
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len {
                    return Err(EvalError::Type(format!(
                        "{call_pos}: map: lists must have same length"
                    )));
                }
            }
            let mut result = Vec::new();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_func(func, &call_args, call_pos, out)?);
            }
            Ok(Value::List(result))
        }
        "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: for-each requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..]
                .iter()
                .map(|a| match a {
                    Value::List(items) => Ok(items),
                    _ => Err(EvalError::Type(format!(
                        "{call_pos}: for-each: expected list"
                    ))),
                })
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply_func(func, &call_args, call_pos, out)?;
            }
            Ok(Value::Void)
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: apply requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(items) => items.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "{call_pos}: apply: last argument must be a list"
                    )))
                }
            };
            let mut final_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            final_args.extend(tail);
            apply_func(func, &final_args, call_pos, out)
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_builtin_by_name(
    name: &str,
    args: &[Value],
    call_pos: Pos,
    out: &RefCell<String>,
    apply_func: ApplyFn,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "abs" | "modulo"
        | "remainder" | "quotient" | "min" | "max" | "expt" | "zero?" | "positive?"
        | "negative?" | "odd?" | "even?" => apply_numeric_builtin(name, args, call_pos),
        "cons" | "car" | "cdr" | "list" | "length" | "null?" | "pair?" | "list?" | "append"
        | "list-ref" | "list-tail" | "assoc" | "map" | "for-each" | "apply" => {
            apply_list_builtin(name, args, call_pos, out, apply_func)
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: not requires 1 argument"
                )));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: number? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: boolean? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: symbol? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: display requires 1 argument"
                )));
            }
            let s = args[0].display_output();
            out.borrow_mut().push_str(&s);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: write requires 1 argument"
                )));
            }
            let s = args[0].display_scheme();
            out.borrow_mut().push_str(&s);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: newline requires 0 arguments"
                )));
            }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref" | "char?"
        | "string-copy" | "string=?" | "string<?" | "string-ci=?" | "string-upcase"
        | "string-downcase" | "char-alphabetic?" | "char-numeric?" | "char-upcase"
        | "char-downcase" | "char=?" | "char<?" => apply_string_builtin(name, args, call_pos),
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: equal? requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(args[0] == args[1]))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: eq? requires 2 arguments"
                )));
            }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                (Value::Void, Value::Void) => true,
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}
