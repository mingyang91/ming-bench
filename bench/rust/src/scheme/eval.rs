use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::value::Value;
use crate::scheme::EvalError;

pub fn eval(expr: &Value, env: &Rc<Env>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
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
            apply_builtin(&func, &args)
        }
        Value::Void => Ok(Value::Void),
    }
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

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Symbol(s) => s.as_str(),
        _ => return Err(EvalError::Type(format!("not a procedure: {}", func))),
    };
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
