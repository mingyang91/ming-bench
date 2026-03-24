use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::value::Value;
use crate::scheme::EvalError;

pub fn eval(expr: &Value, env: &Rc<Env>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Runtime("empty application".into()));
            }
            // Check for special forms
            if let Value::Symbol(ref s) = elems[0] {
                match s.as_str() {
                    "define" => return eval_define(&elems[1..], env),
                    "if" => return eval_if(&elems[1..], env),
                    "quote" => return eval_quote(&elems[1..]),
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    _ => {}
                }
            }
            // Function application
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..].iter()
                .map(|e| eval(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            apply(&func, &args)
        }
        Value::Void => Ok(Value::Void),
    }
}

fn eval_define(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Syntax("define requires at least 2 arguments".into()));
    }
    match &args[0] {
        Value::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f x y) body...) => (define f (lambda (x y) body...))
        Value::List(parts) => {
            if parts.is_empty() {
                return Err(EvalError::Syntax("define: empty name list".into()));
            }
            let name = match &parts[0] {
                Value::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Syntax("define: expected symbol as function name".into())),
            };
            let params: Vec<String> = parts[1..].iter().map(|p| {
                match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Syntax("define: expected symbol as parameter".into())),
                }
            }).collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, body, env: Rc::clone(env) };
            env.set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Syntax("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Syntax("quote requires exactly 1 argument".into()));
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Syntax("lambda requires parameters and body".into()));
    }
    let params = match &args[0] {
        Value::List(elems) => {
            elems.iter().map(|p| {
                match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Syntax("lambda: expected symbol as parameter".into())),
                }
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Syntax("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, body, env: Rc::clone(env) })
}

fn eval_and(exprs: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Symbol(name) => apply_builtin(name, args),
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = Env::new(Some(Rc::clone(env)));
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.set(param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer().ok_or_else(|| EvalError::Type("+ expects numbers".into()))?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = args[0].as_integer().ok_or_else(|| EvalError::Type("- expects numbers".into()))?;
            if args.len() == 1 {
                Ok(Value::Integer(-first))
            } else {
                let mut result = first;
                for a in &args[1..] {
                    result -= a.as_integer().ok_or_else(|| EvalError::Type("- expects numbers".into()))?;
                }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= a.as_integer().ok_or_else(|| EvalError::Type("* expects numbers".into()))?;
            }
            Ok(Value::Integer(prod))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = args[0].as_integer().ok_or_else(|| EvalError::Type("/ expects numbers".into()))?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::Runtime("division by zero".into()));
                }
                Ok(Value::Integer(1 / first))
            } else {
                let mut result = first;
                for a in &args[1..] {
                    let d = a.as_integer().ok_or_else(|| EvalError::Type("/ expects numbers".into()))?;
                    if d == 0 {
                        return Err(EvalError::Runtime("division by zero".into()));
                    }
                    result /= d;
                }
                Ok(Value::Integer(result))
            }
        }
        "<" => cmp_op(args, |a, b| a < b),
        ">" => cmp_op(args, |a, b| a > b),
        "=" => cmp_op(args, |a, b| a == b),
        "<=" => cmp_op(args, |a, b| a <= b),
        ">=" => cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires exactly 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(name.to_string())),
    }
}

fn cmp_op(args: &[Value], op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let nums: Vec<i64> = args.iter()
        .map(|a| a.as_integer().ok_or_else(|| EvalError::Type("comparison expects numbers".into())))
        .collect::<Result<Vec<_>, _>>()?;
    for w in nums.windows(2) {
        if !op(w[0], w[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}
