use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;
use crate::scheme::dispatch::apply_value_tco;
use crate::scheme::eval;
use crate::scheme::Trampoline;

/// Apply a function value (Lambda or Builtin) to evaluated arguments.
fn apply_any(func: Value, args: &[Value]) -> Result<Value, EvalError> {
    match apply_value_tco(func, args)? {
        Trampoline::Done(v) => Ok(v),
        Trampoline::Bounce { expr, env } => eval(&expr, &env),
    }
}

pub(crate) fn eval_map(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".into(),
            got: args.len(),
        });
    }
    let func = eval(&args[0], env)?;
    let lists: Vec<Vec<Value>> = args[1..]
        .iter()
        .map(|a| {
            let val = eval(a, env)?;
            match val {
                Value::List(elems) => Ok(elems),
                other => Err(EvalError::TypeError {
                    expected: "list".into(),
                    got: format!("{other}"),
                }),
            }
        })
        .collect::<Result<_, _>>()?;
    let len = lists[0].len();
    let results: Vec<Value> = (0..len)
        .map(|i| {
            let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
            apply_any(func.clone(), &call_args)
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::List(results))
}

pub(crate) fn eval_display(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    env.write_output(&val.display_str());
    Ok(Value::Void)
}

pub(crate) fn eval_write(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    env.write_output(&val.write_str());
    Ok(Value::Void)
}

pub(crate) fn eval_newline(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "0".into(),
            got: args.len(),
        });
    }
    env.write_output("\n");
    Ok(Value::Void)
}
