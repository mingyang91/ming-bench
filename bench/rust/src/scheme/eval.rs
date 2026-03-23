use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate a parsed expression in the given environment.
pub fn eval_expr(expr: &Expr, env: &Rc<Env>) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => env.get(name).ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Expr::List(elems) => eval_list(elems, env),
    }
}

fn eval_list(elems: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Ok(Value::List(Vec::new()));
    }

    // Check for special forms first
    if let Expr::Symbol(name) = &elems[0] {
        match name.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "define" => return eval_define(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            "lambda" => return eval_lambda(&elems[1..], env),
            _ => {}
        }
    }

    let func = eval_expr(&elems[0], env)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval_expr(e, env))
        .collect::<Result<Vec<_>, _>>()?;

    apply_function(&func, &args)
}

fn eval_if(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::BadSyntax {
            form: "if".into(),
            message: "expected 2 or 3 arguments".into(),
        });
    }
    let cond = eval_expr(&args[0], env)?;
    if cond.is_truthy() {
        eval_expr(&args[1], env)
    } else if args.len() == 3 {
        eval_expr(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::BadSyntax {
            form: "define".into(),
            message: "missing name".into(),
        });
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::BadSyntax {
                    form: "define".into(),
                    message: "expected (define name expr)".into(),
                });
            }
            let val = eval_expr(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        Expr::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(EvalError::BadSyntax {
                    form: "define".into(),
                    message: "missing function name".into(),
                });
            }
            let name = match &name_and_params[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(EvalError::BadSyntax {
                    form: "define".into(),
                    message: "function name must be a symbol".into(),
                }),
            };
            let params: Vec<String> = name_and_params[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::BadSyntax {
                        form: "define".into(),
                        message: "parameter must be a symbol".into(),
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                closure_env: Rc::clone(env),
            };
            env.define(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::BadSyntax {
            form: "define".into(),
            message: "invalid define syntax".into(),
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::BadSyntax {
            form: "quote".into(),
            message: "expected 1 argument".into(),
        });
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => Ok(Value::Symbol(s.clone())),
        Expr::List(elems) => {
            let vals: Vec<Value> = elems
                .iter()
                .map(expr_to_value)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::BadSyntax {
            form: "lambda".into(),
            message: "expected (lambda (params) body...)".into(),
        });
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => {
            param_exprs
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::BadSyntax {
                        form: "lambda".into(),
                        message: "parameter must be a symbol".into(),
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::BadSyntax {
            form: "lambda".into(),
            message: "expected parameter list".into(),
        }),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        closure_env: Rc::clone(env),
    })
}

fn eval_and(exprs: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval_expr(expr, env)?;
        if !val.is_truthy() {
            return Ok(val);
        }
    }
    eval_expr(&exprs[exprs.len() - 1], env)
}

fn eval_or(exprs: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval_expr(expr, env)?;
        if val.is_truthy() {
            return Ok(val);
        }
    }
    eval_expr(&exprs[exprs.len() - 1], env)
}

pub fn apply_function(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args),
        Value::Lambda { params, body, closure_env } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                });
            }
            let local_env = Env::extend(closure_env, params.clone(), args.to_vec());
            let mut result = Value::Void;
            for expr in body {
                result = eval_expr(expr, &local_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::NotAProcedure {
            value: other.to_display_string(),
        }),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b)),
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            if args.len() == 1 {
                let n = require_int(&args[0])?;
                return Ok(Value::Integer(-n));
            }
            let first = require_int(&args[0])?;
            let rest_sum: i64 = args[1..]
                .iter()
                .map(require_int)
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .sum();
            Ok(Value::Integer(first - rest_sum))
        }
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b)),
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let first = require_int(&args[0])?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for arg in &args[1..] {
                let n = require_int(arg)?;
                if n == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= n;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }),
    }
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeMismatch {
            expected: "integer".into(),
            got: other.to_display_string(),
        }),
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    let mut result = identity;
    for arg in args {
        let n = require_int(arg)?;
        result = op(result, n)?;
    }
    Ok(Value::Integer(result))
}

fn compare_nums(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    for pair in args.windows(2) {
        let a = require_int(&pair[0])?;
        let b = require_int(&pair[1])?;
        if !cmp(a, b) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}
