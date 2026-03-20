use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;
use crate::scheme::apply::apply_lambda;
use crate::scheme::eval;

pub(crate) fn eval_map(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [func_expr, list_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let func = eval(func_expr, env)?;
    let list_val = eval(list_expr, env)?;
    let Value::List(elems) = list_val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{list_val}"),
        });
    };
    let results: Vec<Value> = elems
        .iter()
        .map(|e| apply_lambda(func.clone(), std::slice::from_ref(e)))
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
