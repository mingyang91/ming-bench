pub mod error;
mod parser;
mod value;

pub use error::EvalError;

use std::rc::Rc;

use parser::{parse_program, Expr};
use value::{is_truthy, list_from_vec, render_value, Builtin, Closure, Env, Value};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let result = eval_program(input)?;
    Ok(render_value(&result))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_program(input)?;
    Ok((render_value(&result), String::new()))
}

#[cfg(test)]
mod tests;

fn eval_program(input: &str) -> Result<Value, EvalError> {
    let exprs = parse_program(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Syntax {
            message: "empty input".into(),
        });
    }

    let env = base_env();
    let mut result = Value::Void;

    for expr in &exprs {
        result = eval_expr(expr, &env)?;
    }

    Ok(result)
}

fn base_env() -> Env {
    let env = Env::new_root();

    install_builtin(&env, "+", builtin_add);
    install_builtin(&env, "-", builtin_sub);
    install_builtin(&env, "*", builtin_mul);
    install_builtin(&env, "/", builtin_div);
    install_builtin(&env, "<", builtin_lt);
    install_builtin(&env, ">", builtin_gt);
    install_builtin(&env, "=", builtin_eq);
    install_builtin(&env, "<=", builtin_le);
    install_builtin(&env, "not", builtin_not);

    env
}

fn install_builtin(env: &Env, name: &'static str, func: fn(&[Value]) -> Result<Value, EvalError>) {
    env.define(name, Value::Builtin(Builtin { name, func }));
}

fn eval_expr(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env.get(name),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let Some((head, rest)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    match head {
        Expr::Symbol(symbol) => match symbol.as_str() {
            "define" => eval_define(rest, env),
            "if" => eval_if(rest, env),
            "quote" => eval_quote(rest),
            "lambda" => eval_lambda(rest, env),
            "and" => eval_and(rest, env),
            "or" => eval_or(rest, env),
            _ => eval_call(head, rest, env),
        },
        _ => eval_call(head, rest, env),
    }
}

fn eval_define(rest: &[Expr], env: &Env) -> Result<Value, EvalError> {
    match rest {
        [Expr::Symbol(name), value_expr] => {
            let value = eval_expr(value_expr, env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] => {
            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(EvalError::Syntax {
                    message: "define requires a function name".into(),
                });
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::Syntax {
                    message: "function name must be a symbol".into(),
                });
            };

            if body.is_empty() {
                return Err(EvalError::Syntax {
                    message: "function definition requires a body".into(),
                });
            }

            let params = parse_params(params_exprs)?;
            let closure = Value::Closure(Rc::new(Closure {
                params,
                body: body.to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "invalid define form".into(),
        }),
    }
}

fn eval_if(rest: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [condition, then_branch, else_branch] = rest else {
        return Err(EvalError::WrongArgumentCount {
            name: "if".into(),
            expected: "3".into(),
            got: rest.len(),
        });
    };

    if is_truthy(&eval_expr(condition, env)?) {
        eval_expr(then_branch, env)
    } else {
        eval_expr(else_branch, env)
    }
}

fn eval_quote(rest: &[Expr]) -> Result<Value, EvalError> {
    let [datum] = rest else {
        return Err(EvalError::WrongArgumentCount {
            name: "quote".into(),
            expected: "1".into(),
            got: rest.len(),
        });
    };

    Ok(datum_to_value(datum))
}

fn eval_lambda(rest: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = rest.split_first() else {
        return Err(EvalError::Syntax {
            message: "lambda requires parameters and a body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda requires a body".into(),
        });
    }

    let Expr::List(params_exprs) = params_expr else {
        return Err(EvalError::Syntax {
            message: "lambda parameters must be a list".into(),
        });
    };

    Ok(Value::Closure(Rc::new(Closure {
        params: parse_params(params_exprs)?,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn eval_and(rest: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for expr in rest {
        let value = eval_expr(expr, env)?;
        if !is_truthy(&value) {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(rest: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);

    for expr in rest {
        let value = eval_expr(expr, env)?;
        if is_truthy(&value) {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_call(head: &Expr, rest: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let function = eval_expr(head, env)?;
    let mut args = Vec::with_capacity(rest.len());
    for expr in rest {
        args.push(eval_expr(expr, env)?);
    }
    apply(function, &args)
}

fn apply(function: Value, args: &[Value]) -> Result<Value, EvalError> {
    match function {
        Value::Builtin(builtin) => (builtin.func)(args),
        Value::Closure(closure) => {
            if closure.params.len() != args.len() {
                return Err(EvalError::WrongArgumentCount {
                    name: "lambda".into(),
                    expected: closure.params.len().to_string(),
                    got: args.len(),
                });
            }

            let call_env = closure.env.child();
            for (name, value) in closure.params.iter().zip(args.iter()) {
                call_env.define(name.clone(), value.clone());
            }

            eval_sequence(&closure.body, &call_env)
        }
        other => Err(EvalError::NotCallable {
            found: other.type_name(),
        }),
    }
}

fn eval_sequence(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = eval_expr(expr, env)?;
    }

    Ok(result)
}

fn parse_params(params_exprs: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(params_exprs.len());

    for expr in params_exprs {
        let Expr::Symbol(name) = expr else {
            return Err(EvalError::Syntax {
                message: "parameter names must be symbols".into(),
            });
        };
        params.push(name.clone());
    }

    Ok(params)
}

fn datum_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value) => Value::Int(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => {
            let values = items.iter().map(datum_to_value).collect();
            list_from_vec(values)
        }
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;
    for value in args {
        total += expect_int(value)?;
    }
    Ok(Value::Int(total))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgumentCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        }),
        [value] => Ok(Value::Int(-expect_int(value)?)),
        [first, rest @ ..] => {
            let mut total = expect_int(first)?;
            for value in rest {
                total -= expect_int(value)?;
            }
            Ok(Value::Int(total))
        }
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i64;
    for value in args {
        total *= expect_int(value)?;
    }
    Ok(Value::Int(total))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgumentCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 1,
        });
    }

    let mut total = expect_int(first)?;
    for value in rest {
        let divisor = expect_int(value)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= divisor;
    }

    Ok(Value::Int(total))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    compare_chain("<", args, |left, right| left < right)
}

fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    compare_chain(">", args, |left, right| left > right)
}

fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    compare_chain("=", args, |left, right| left == right)
}

fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
    compare_chain("<=", args, |left, right| left <= right)
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgumentCount {
            name: "not".into(),
            expected: "1".into(),
            got: args.len(),
        });
    };

    Ok(Value::Bool(!is_truthy(value)))
}

fn compare_chain(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }

    let mut numbers = args.iter().map(expect_int);
    let mut previous = numbers.next().expect("length checked above")?;

    for current in numbers {
        let current = current?;
        if !predicate(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }

    Ok(Value::Bool(true))
}

fn expect_int(value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        other => Err(EvalError::TypeMismatch {
            expected: "number",
            found: other.type_name(),
        }),
    }
}
