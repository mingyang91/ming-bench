use std::rc::Rc;

use crate::scheme::env::{self, Env};
use crate::scheme::error::SchemeError;
use crate::scheme::value::{Builtin, BuiltinFn, Value};

type Entry = (&'static str, BuiltinFn);

/// Populate `env` with all built-in procedures.
pub fn install(env: &Env) {
    let procs: &[Entry] = &[
        ("+", add),
        ("-", sub),
        ("*", mul),
        ("/", div),
        ("<", lt),
        (">", gt),
        ("=", num_eq),
        ("<=", le),
        (">=", ge),
        ("not", not),
        ("cons", cons),
        ("car", car),
        ("cdr", cdr),
        ("null?", is_null),
        ("list", list),
        ("length", length),
        ("string?", is_string),
        ("number?", is_number),
        ("boolean?", is_boolean),
        ("pair?", is_pair),
        ("symbol?", is_symbol),
        ("procedure?", is_procedure),
        ("equal?", equal),
        ("display", display_val),
        ("newline", newline_val),
    ];
    for &(name, func) in procs {
        let name_static: &'static str = leak_str(name);
        env::define(
            env,
            name.to_string(),
            Value::Builtin(Builtin {
                name: name_static,
                func,
            }),
        );
    }
    install_special(env);
}

/// Install `call/cc` and `apply` as sentinel builtins (handled in eval).
fn install_special(env: &Env) {
    fn noop(_: &[Value]) -> Result<Value, SchemeError> {
        Ok(Value::Void)
    }
    for name in ["call/cc", "call-with-current-continuation", "apply"] {
        let name_static: &'static str = leak_str(name);
        env::define(
            env,
            name.to_string(),
            Value::Builtin(Builtin {
                name: name_static,
                func: noop,
            }),
        );
    }
}

fn leak_str(s: &str) -> &'static str {
    // Small set of known strings; acceptable for a benchmark.
    Box::leak(s.to_string().into_boxed_str())
}

// ---------------------------------------------------------------------------
// Arithmetic
// ---------------------------------------------------------------------------

fn add(args: &[Value]) -> Result<Value, SchemeError> {
    let mut sum: i64 = 0;
    for a in args {
        sum = sum.wrapping_add(a.as_integer("+")?);
    }
    Ok(Value::Integer(sum))
}

fn sub(args: &[Value]) -> Result<Value, SchemeError> {
    if args.is_empty() {
        return Err(arity("-", "1+", 0));
    }
    if args.len() == 1 {
        return Ok(Value::Integer(-args[0].as_integer("-")?));
    }
    let mut acc = args[0].as_integer("-")?;
    for a in &args[1..] {
        acc = acc.wrapping_sub(a.as_integer("-")?);
    }
    Ok(Value::Integer(acc))
}

fn mul(args: &[Value]) -> Result<Value, SchemeError> {
    let mut product: i64 = 1;
    for a in args {
        product = product.wrapping_mul(a.as_integer("*")?);
    }
    Ok(Value::Integer(product))
}

fn div(args: &[Value]) -> Result<Value, SchemeError> {
    if args.len() < 2 {
        return Err(arity("/", "2+", args.len()));
    }
    let mut acc = args[0].as_integer("/")?;
    for a in &args[1..] {
        let d = a.as_integer("/")?;
        if d == 0 {
            return Err(SchemeError::DivisionByZero);
        }
        acc /= d;
    }
    Ok(Value::Integer(acc))
}

// ---------------------------------------------------------------------------
// Comparison
// ---------------------------------------------------------------------------

fn lt(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("<", 2, args)?;
    Ok(Value::Boolean(
        args[0].as_integer("<")? < args[1].as_integer("<")?,
    ))
}

fn gt(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity(">", 2, args)?;
    Ok(Value::Boolean(
        args[0].as_integer(">")? > args[1].as_integer(">")?,
    ))
}

fn num_eq(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("=", 2, args)?;
    Ok(Value::Boolean(
        args[0].as_integer("=")? == args[1].as_integer("=")?,
    ))
}

fn le(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("<=", 2, args)?;
    Ok(Value::Boolean(
        args[0].as_integer("<=")? <= args[1].as_integer("<=")?,
    ))
}

fn ge(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity(">=", 2, args)?;
    Ok(Value::Boolean(
        args[0].as_integer(">=")? >= args[1].as_integer(">=")?,
    ))
}

fn not(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("not", 1, args)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

// ---------------------------------------------------------------------------
// Lists
// ---------------------------------------------------------------------------

fn cons(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("cons", 2, args)?;
    Ok(Value::Pair(
        Rc::new(args[0].clone()),
        Rc::new(args[1].clone()),
    ))
}

fn car(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("car", 1, args)?;
    match &args[0] {
        Value::Pair(a, _) => Ok((**a).clone()),
        other => Err(SchemeError::TypeError {
            op: "car".into(),
            expected: "pair".into(),
            got: other.type_name().into(),
        }),
    }
}

fn cdr(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("cdr", 1, args)?;
    match &args[0] {
        Value::Pair(_, d) => Ok((**d).clone()),
        other => Err(SchemeError::TypeError {
            op: "cdr".into(),
            expected: "pair".into(),
            got: other.type_name().into(),
        }),
    }
}

fn is_null(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("null?", 1, args)?;
    Ok(Value::Boolean(matches!(args[0], Value::Nil)))
}

fn list(args: &[Value]) -> Result<Value, SchemeError> {
    Ok(crate::scheme::value::from_vec(args.to_vec()))
}

fn length(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("length", 1, args)?;
    let items = crate::scheme::value::to_vec(&args[0])?;
    Ok(Value::Integer(items.len() as i64))
}

// ---------------------------------------------------------------------------
// Type predicates
// ---------------------------------------------------------------------------

fn is_string(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("string?", 1, args)?;
    Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
}

fn is_number(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("number?", 1, args)?;
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}

fn is_boolean(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("boolean?", 1, args)?;
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

fn is_pair(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("pair?", 1, args)?;
    Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
}

fn is_symbol(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("symbol?", 1, args)?;
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn is_procedure(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("procedure?", 1, args)?;
    Ok(Value::Boolean(matches!(
        args[0],
        Value::Lambda(_) | Value::Builtin(_) | Value::Continuation(_)
    )))
}

fn equal(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("equal?", 2, args)?;
    Ok(Value::Boolean(scheme_equal(&args[0], &args[1])))
}

fn scheme_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            scheme_equal(a1, b1) && scheme_equal(a2, b2)
        }
        _ => false,
    }
}

fn display_val(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("display", 1, args)?;
    Ok(Value::Void)
}

fn newline_val(args: &[Value]) -> Result<Value, SchemeError> {
    check_arity("newline", 0, args)?;
    Ok(Value::Void)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn check_arity(name: &str, expected: usize, args: &[Value]) -> Result<(), SchemeError> {
    if args.len() != expected {
        return Err(arity(name, &expected.to_string(), args.len()));
    }
    Ok(())
}

fn arity(name: &str, expected: &str, got: usize) -> SchemeError {
    SchemeError::ArityMismatch {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}
