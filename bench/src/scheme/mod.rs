pub mod env;
pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use env::Env;
use std::rc::Rc;
use value::Value;

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
fn eval_not(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(!is_truthy(&val)))
}

/// Evaluate `(and expr ...)` — short-circuit, returns last truthy or first falsy.
fn eval_and(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
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
fn eval_or(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Extract parameter names from a list of symbols.
fn extract_params(params: &[Value], form: &str) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::BadSyntax { form: form.into() }),
        })
        .collect()
}

/// Evaluate `(define ...)` — supports both `(define name expr)` and `(define (name params...) body...)`.
fn eval_define(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    match args {
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env)?;
            env.set(name.clone(), val);
            Ok(Value::Void)
        }
        [Value::List(name_and_params), body @ ..] if !body.is_empty() => {
            let [Value::Symbol(name), params @ ..] = name_and_params.as_slice() else {
                return Err(EvalError::BadSyntax {
                    form: "define".into(),
                });
            };
            let param_names = extract_params(params, "define")?;
            let lambda = Value::Lambda {
                params: param_names,
                body: body.to_vec(),
                env: Rc::clone(env),
            };
            env.set(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::BadSyntax {
            form: "define".into(),
        }),
    }
}

/// Evaluate `(lambda (params...) body...)`.
fn eval_lambda(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [Value::List(params), body @ ..] = args else {
        return Err(EvalError::BadSyntax {
            form: "lambda".into(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::BadSyntax {
            form: "lambda".into(),
        });
    }
    let param_names = extract_params(params, "lambda")?;
    Ok(Value::Lambda {
        params: param_names,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

/// Evaluate `(if cond then else?)`.
fn eval_if(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
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

/// Apply a lambda to evaluated arguments.
fn apply_lambda(func: Value, args: &[Value]) -> Result<Value, EvalError> {
    let Value::Lambda { params, body, env } = func else {
        return Err(EvalError::NotAProcedure {
            value: format!("{func}"),
        });
    };
    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len().to_string(),
            got: args.len(),
        });
    }
    let child = Env::child(&env);
    for (param, arg) in params.iter().zip(args) {
        child.set(param.clone(), arg.clone());
    }
    eval_body(&body, &child)
}

/// Evaluate a sequence of expressions, returning the last result.
fn eval_body(exprs: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    exprs
        .iter()
        .try_fold(Value::Void, |_, expr| eval(expr, env))
}

/// Evaluate a single parsed Scheme value.
fn eval(expr: &Value, env: &Rc<Env>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Void => Ok(expr.clone()),
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Symbol(name) => env
            .get(name)
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
                Value::Symbol(op) if op == "lambda" => eval_lambda(args, env),
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
                _ => {
                    let func = eval(operator, env)?;
                    let evaluated_args: Vec<Value> =
                        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                    apply_lambda(func, &evaluated_args)
                }
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
    let env = Env::new();
    let last = eval_body(&expressions, &env)?;
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
