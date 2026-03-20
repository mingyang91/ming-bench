pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use std::collections::HashMap;
use value::Value;

/// A variable environment (flat for now — no closures yet).
type Env = HashMap<String, Value>;

/// Extract an integer from a Value, returning a TypeError if not an integer.
fn expect_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{other}"),
        }),
    }
}

/// Apply an arithmetic operator to evaluated arguments.
fn apply_arithmetic(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let nums: Vec<i64> = args.iter().map(expect_integer).collect::<Result<_, _>>()?;

    match (op, nums.as_slice()) {
        ("+", ns) => Ok(Value::Integer(ns.iter().sum())),
        ("*", ns) => Ok(Value::Integer(ns.iter().product())),
        ("-", []) => Err(EvalError::WrongArgCount {
            expected: "at least 1".into(),
            got: 0,
        }),
        ("-", [x]) => Ok(Value::Integer(-x)),
        ("-", [first, rest @ ..]) => Ok(Value::Integer(rest.iter().fold(*first, |a, b| a - b))),
        ("/", []) => Err(EvalError::WrongArgCount {
            expected: "at least 1".into(),
            got: 0,
        }),
        ("/", [x]) => Ok(Value::Integer(1 / x)),
        ("/", [first, rest @ ..]) => Ok(Value::Integer(rest.iter().fold(*first, |a, b| a / b))),
        _ => Err(EvalError::UnboundVariable { name: op.into() }),
    }
}

/// Apply a comparison operator to evaluated arguments.
fn apply_comparison(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    }
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!("invalid comparison op: {op}"),
    };
    Ok(Value::Boolean(result))
}

/// Check if a value is truthy (everything except #f is truthy in Scheme).
fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

/// Evaluate `(not expr)` — returns #t if expr is falsy, #f otherwise.
fn eval_not(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::Boolean(!is_truthy(&val)))
}

/// Evaluate `(and expr ...)` — short-circuit, returns last truthy or first falsy.
fn eval_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate `(or expr ...)` — short-circuit, returns first truthy or last falsy.
fn eval_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate `(define name expr)` — binds name in env, returns Void.
fn eval_define(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [Value::Symbol(name), expr] = args else {
        return Err(EvalError::BadSyntax {
            form: "define".into(),
        });
    };
    let val = eval(expr, env)?;
    env.insert(name.clone(), val);
    Ok(Value::Void)
}

/// Evaluate `(if cond then else?)`.
fn eval_if(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match args {
        [cond, consequent, alternate] => {
            if is_truthy(&eval(cond, env)?) {
                eval(consequent, env)
            } else {
                eval(alternate, env)
            }
        }
        [cond, consequent] => {
            if is_truthy(&eval(cond, env)?) {
                eval(consequent, env)
            } else {
                Ok(Value::Void)
            }
        }
        _ => Err(EvalError::BadSyntax { form: "if".into() }),
    }
}

/// Evaluate a single parsed Scheme value.
fn eval(expr: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Void => Ok(expr.clone()),
        Value::Symbol(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Value::List(elements) => {
            let [operator, args @ ..] = elements.as_slice() else {
                return Err(EvalError::EmptyList);
            };
            match operator {
                Value::Symbol(op) if op == "quote" => match args {
                    [datum] => Ok(datum.clone()),
                    _ => Err(EvalError::BadSyntax {
                        form: "quote".into(),
                    }),
                },
                Value::Symbol(op) if op == "if" => eval_if(args, env),
                Value::Symbol(op) if op == "define" => eval_define(args, env),
                Value::Symbol(op) if matches!(op.as_str(), "+" | "-" | "*" | "/") => {
                    let evaluated: Vec<Value> =
                        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                    apply_arithmetic(op, &evaluated)
                }
                Value::Symbol(op)
                    if matches!(op.as_str(), "<" | ">" | "=" | "<=" | ">=") =>
                {
                    let evaluated: Vec<Value> =
                        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                    apply_comparison(op, &evaluated)
                }
                Value::Symbol(op) if op == "not" => eval_not(args, env),
                Value::Symbol(op) if op == "and" => eval_and(args, env),
                Value::Symbol(op) if op == "or" => eval_or(args, env),
                _ => Err(EvalError::NotAProcedure {
                    value: format!("{operator}"),
                }),
            }
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = parser::parse(input)?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }
    let mut env = Env::new();
    let mut last = Value::Void;
    for expr in &expressions {
        last = eval(expr, &mut env)?;
    }
    match last {
        Value::Void => Err(EvalError::EmptyInput),
        val => Ok(val.to_string()),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
