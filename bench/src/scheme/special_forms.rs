use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;
use crate::scheme::apply::{extract_params, is_truthy};
use crate::scheme::{eval, eval_body_tco, Trampoline};

/// Evaluate `(not expr)` — returns #t if expr is falsy, #f otherwise.
pub(crate) fn eval_not(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
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
/// TCO: the last expression is returned as a Bounce.
pub(crate) fn eval_and_tco(args: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    }
    let [init @ .., last] = args else {
        unreachable!();
    };
    for arg in init {
        let result = eval(arg, env)?;
        if !is_truthy(&result) {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::Bounce {
        expr: last.clone(),
        env: Rc::clone(env),
    })
}

/// Evaluate `(or expr ...)` — short-circuit, returns first truthy or last falsy.
/// TCO: the last expression is returned as a Bounce.
pub(crate) fn eval_or_tco(args: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    }
    let [init @ .., last] = args else {
        unreachable!();
    };
    for arg in init {
        let result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::Bounce {
        expr: last.clone(),
        env: Rc::clone(env),
    })
}

/// Evaluate `(define ...)` — supports both `(define name expr)` and `(define (name params...) body...)`.
pub(crate) fn eval_define(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
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
            let (param_names, rest_param) = extract_params(params, "define")?;
            let lambda = Value::Lambda {
                params: param_names,
                rest_param,
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
pub(crate) fn eval_lambda(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
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
    let (param_names, rest_param) = extract_params(params, "lambda")?;
    Ok(Value::Lambda {
        params: param_names,
        rest_param,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

/// Evaluate `(if cond then else?)` — TCO: returns Bounce for the chosen branch.
pub(crate) fn eval_if_tco(args: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    match args {
        [cond, consequent, alternate] => {
            let branch = if is_truthy(&eval(cond, env)?) {
                consequent
            } else {
                alternate
            };
            Ok(Trampoline::Bounce {
                expr: branch.clone(),
                env: Rc::clone(env),
            })
        }
        [cond, consequent] => {
            if is_truthy(&eval(cond, env)?) {
                Ok(Trampoline::Bounce {
                    expr: consequent.clone(),
                    env: Rc::clone(env),
                })
            } else {
                Ok(Trampoline::Done(Value::Void))
            }
        }
        _ => Err(EvalError::BadSyntax { form: "if".into() }),
    }
}

/// Evaluate `(let ...) ` — TCO: last body expression is a Bounce.
/// Supports both plain `(let ((bindings...)) body...)` and named `(let name ((bindings...)) body...)`.
pub(crate) fn eval_let_tco(args: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    match args {
        // Named let: (let name ((var init) ...) body ...)
        [Value::Symbol(name), Value::List(bindings), body @ ..] if !body.is_empty() => {
            eval_named_let(name, bindings, body, env)
        }
        // Plain let: (let ((var init) ...) body ...)
        [Value::List(bindings), body @ ..] if !body.is_empty() => {
            eval_plain_let(bindings, body, env)
        }
        _ => Err(EvalError::BadSyntax {
            form: "let".into(),
        }),
    }
}

fn eval_plain_let(
    bindings: &[Value],
    body: &[Value],
    env: &Rc<Env>,
) -> Result<Trampoline, EvalError> {
    let child = Env::child(env);
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::BadSyntax {
                form: "let".into(),
            });
        };
        let [Value::Symbol(name), expr] = pair.as_slice() else {
            return Err(EvalError::BadSyntax {
                form: "let".into(),
            });
        };
        let val = eval(expr, env)?;
        child.set(name.clone(), val);
    }
    eval_body_tco(body, &child)
}

fn eval_named_let(
    name: &str,
    bindings: &[Value],
    body: &[Value],
    env: &Rc<Env>,
) -> Result<Trampoline, EvalError> {
    let mut param_names = Vec::with_capacity(bindings.len());
    let mut init_vals = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::BadSyntax {
                form: "let".into(),
            });
        };
        let [Value::Symbol(var), expr] = pair.as_slice() else {
            return Err(EvalError::BadSyntax {
                form: "let".into(),
            });
        };
        param_names.push(var.clone());
        init_vals.push(eval(expr, env)?);
    }
    // Create a child env and bind the loop procedure
    let child = Env::child(env);
    let lambda = Value::Lambda {
        params: param_names,
        rest_param: None,
        body: body.to_vec(),
        env: Rc::clone(&child),
    };
    child.set(name.to_string(), lambda);
    // Bind initial values
    for (binding, val) in bindings.iter().zip(init_vals) {
        let Value::List(pair) = binding else { unreachable!() };
        let Value::Symbol(var) = &pair[0] else { unreachable!() };
        child.set(var.clone(), val);
    }
    eval_body_tco(body, &child)
}

/// Evaluate `(set! name expr)` — mutate an existing binding.
pub(crate) fn eval_set(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [Value::Symbol(name), expr] = args else {
        return Err(EvalError::BadSyntax {
            form: "set!".into(),
        });
    };
    let val = eval(expr, env)?;
    if !env.set_existing(name, val) {
        return Err(EvalError::UnboundVariable { name: name.clone() });
    }
    Ok(Value::Void)
}

/// Evaluate `(cond ...)` — TCO: matching clause body tail is a Bounce.
pub(crate) fn eval_cond_tco(clauses: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::BadSyntax {
                form: "cond".into(),
            });
        };
        let [test, body @ ..] = parts.as_slice() else {
            return Err(EvalError::BadSyntax {
                form: "cond".into(),
            });
        };
        if matches!(test, Value::Symbol(s) if s == "else") {
            return eval_body_tco(body, env);
        }
        let val = eval(test, env)?;
        if !is_truthy(&val) {
            continue;
        }
        return if body.is_empty() {
            Ok(Trampoline::Done(val))
        } else {
            eval_body_tco(body, env)
        };
    }
    Ok(Trampoline::Done(Value::Void))
}
