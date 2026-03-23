use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

const BUILTINS: &[&str] = &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"];

pub fn default_env() -> Rc<RefCell<Env>> {
    let env = Env::new();
    for &name in BUILTINS {
        env.borrow_mut().define(name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match expr {
        Value::Int(_) | Value::Bool(_) | Value::String(_) | Value::Builtin(_) => Ok(expr.clone()),
        Value::Closure { .. } => Ok(expr.clone()),
        Value::Symbol(name) => env.borrow().get(name),
        Value::List(elems) => eval_list(elems, env),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse { msg: "empty application".into() });
    }

    if let Value::Symbol(op) = &elems[0] {
        match op.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "define" => return eval_define(&elems[1..], env),
            "lambda" => return eval_lambda(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            _ => {}
        }
    }

    let func = eval(&elems[0], env)?;
    let args: Vec<Value> = elems[1..].iter().map(|e| eval(e, env)).collect::<Result<_, _>>()?;

    apply(&func, &args)
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(ref name) => apply_builtin(name, args),
        Value::Closure { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                });
            }
            let call_env = Env::with_parent(env);
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.borrow_mut().define(param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &call_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::NotAProcedure { value: other.to_string() }),
    }
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse { msg: "if requires 2 or 3 arguments".into() });
    }
    let cond = eval(&args[0], env)?;
    if !is_false(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse { msg: "define requires arguments".into() });
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Parse { msg: "define requires exactly 2 arguments".into() });
            }
            let val = eval(&args[1], env)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse { msg: "define: empty signature".into() });
            }
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::Parse { msg: "define: expected function name".into() });
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        msg: format!("define: expected parameter name, got {other}"),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Parse { msg: "define: empty body".into() });
            }
            let closure = Value::Closure {
                params,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), closure);
            Ok(Value::Void)
        }
        other => Err(EvalError::Parse {
            msg: format!("define: expected symbol or list, got {other}"),
        }),
    }
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse { msg: "lambda requires params and body".into() });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::Parse { msg: "lambda: expected parameter list".into() });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::Parse {
                msg: format!("lambda: expected parameter name, got {other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    let body = args[1..].to_vec();
    Ok(Value::Closure {
        params,
        body,
        env: Rc::clone(env),
    })
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse { msg: "quote requires exactly 1 argument".into() });
    }
    Ok(args[0].clone())
}

fn eval_and(exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(true));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env)?;
        if is_false(&val) {
            return Ok(val);
        }
    }
    eval(&exprs[exprs.len() - 1], env)
}

fn eval_or(exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(false));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env)?;
        if !is_false(&val) {
            return Ok(val);
        }
    }
    eval(&exprs[exprs.len() - 1], env)
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Bool(false))
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| match a {
            Value::Int(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".into(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().sum()))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            if nums.len() == 1 {
                Ok(Value::Int(-nums[0]))
            } else {
                let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
                Ok(Value::Int(result))
            }
        }
        "*" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().product()))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            for &n in &nums[1..] {
                if n == 0 {
                    return Err(EvalError::DivisionByZero);
                }
            }
            let result = nums[1..].iter().fold(nums[0], |acc, n| acc / n);
            Ok(Value::Int(result))
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
            Ok(Value::Bool(is_false(&args[0])))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn compare_nums(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let nums = require_ints(args)?;
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Bool(result))
}
