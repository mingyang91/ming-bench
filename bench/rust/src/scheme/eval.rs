use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Lambda { .. } => {
            Ok(expr.clone())
        }
        Value::Symbol(name) => env.borrow().get(name),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            if let Value::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env),
                    "if" => return eval_if(&elems[1..], env),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into()));
                        }
                        return Ok(elems[1].clone());
                    }
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    _ => {}
                }
            }
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|a| eval(a, env))
                .collect::<Result<_, _>>()?;
            apply(&func, &args)
        }
        Value::Void => Ok(Value::Void),
    }
}

fn eval_define(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Value::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as name".into())),
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type("define: expected symbol as parameter".into())),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
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

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let params = match &args[0] {
        Value::List(elems) => elems
            .iter()
            .map(|p| match p {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("lambda: expected symbol as parameter".into())),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: Rc::clone(env),
    })
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let local_env = Env::with_parent(env);
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.borrow_mut().set(param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        Value::Symbol(op) => apply_builtin(op, args),
        _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
    }
}

fn apply_builtin(op: &str, vals: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for v in vals {
                sum += expect_int(v)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if vals.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if vals.len() == 1 {
                return Ok(Value::Integer(-expect_int(&vals[0])?));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                result -= expect_int(v)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for v in vals {
                product *= expect_int(v)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if vals.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                let d = expect_int(v)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(vals, |a, b| a < b),
        ">" => compare_nums(vals, |a, b| a > b),
        "=" => compare_nums(vals, |a, b| a == b),
        "<=" => compare_nums(vals, |a, b| a <= b),
        ">=" => compare_nums(vals, |a, b| a >= b),
        "not" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!vals[0].is_truthy()))
        }
        "cons" => {
            if vals.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            match &vals[1] {
                Value::List(elems) => {
                    let mut new = vec![vals[0].clone()];
                    new.extend(elems.iter().cloned());
                    Ok(Value::List(new))
                }
                _ => {
                    // Dotted pair - represent as 2-element list for now
                    Ok(Value::List(vec![vals[0].clone(), vals[1].clone()]))
                }
            }
        }
        "car" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type("car: expected non-empty list".into())),
            }
        }
        "cdr" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
            }
        }
        "list" => Ok(Value::List(vals.to_vec())),
        "length" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match &vals[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::Type("length: expected list".into())),
            }
        }
        "null?" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::List(e) if e.is_empty())))
        }
        "append" => {
            let mut result = Vec::new();
            for (i, v) in vals.iter().enumerate() {
                if i == vals.len() - 1 {
                    match v {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => result.push(v.clone()),
                    }
                } else {
                    match v {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
            }
            Ok(Value::List(result))
        }
        "number?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_))))
        }
        "boolean?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Boolean(_))))
        }
        "string?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::String(_))))
        }
        "symbol?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Symbol(_))))
        }
        "pair?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::List(e) if !e.is_empty())))
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

fn eval_let(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings_list = match &args[1] {
            Value::List(b) => b,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings_list {
            match binding {
                Value::List(pair) if pair.len() == 2 => {
                    if let Value::Symbol(var) = &pair[0] {
                        params.push(var.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Type("let: expected symbol in binding".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let loop_env = Env::with_parent(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            body,
            env: Rc::clone(&loop_env),
        };
        loop_env.borrow_mut().set(name.clone(), lambda);
        // Call with initial values
        let func = loop_env.borrow().get(name)?;
        apply(&func, &inits)
    } else {
        let bindings_list = match &args[0] {
            Value::List(b) => b,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let local_env = Env::with_parent(env);
        for binding in bindings_list {
            match binding {
                Value::List(pair) if pair.len() == 2 => {
                    if let Value::Symbol(var) = &pair[0] {
                        let val = eval(&pair[1], env)?;
                        local_env.borrow_mut().set(var.clone(), val);
                    } else {
                        return Err(EvalError::Type("let: expected symbol in binding".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, &local_env)?;
        }
        Ok(result)
    }
}

fn eval_begin(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Value::List(parts) if !parts.is_empty() => {
                if let Value::Symbol(s) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn eval_and(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg, env)?;
        if !val.is_truthy() {
            return Ok(val);
        }
    }
    eval(&args[args.len() - 1], env)
}

fn eval_or(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg, env)?;
        if val.is_truthy() {
            return Ok(val);
        }
    }
    eval(&args[args.len() - 1], env)
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected number, got {}", v))),
    }
}

fn compare_nums(vals: &[Value], pred: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if vals.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let mut prev = expect_int(&vals[0])?;
    for v in &vals[1..] {
        let curr = expect_int(v)?;
        if !pred(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}
