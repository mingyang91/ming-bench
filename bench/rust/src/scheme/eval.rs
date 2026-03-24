use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::value::{Value, ValueKind, Pos};
use crate::scheme::EvalError;

fn fmt_pos(pos: Pos) -> String {
    format!("{}:{}", pos.0, pos.1)
}

pub fn eval(expr: &Value, env: &Rc<Env>) -> Result<Value, EvalError> {
    match &expr.kind {
        ValueKind::Integer(_) | ValueKind::Boolean(_) | ValueKind::Str(_) => Ok(expr.clone()),
        ValueKind::Lambda { .. } => Ok(expr.clone()),
        ValueKind::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(
                format!("{} at {}", name, fmt_pos(expr.pos))
            ))
        }
        ValueKind::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Runtime(format!("empty application at {}", fmt_pos(expr.pos))));
            }
            if let ValueKind::Symbol(ref s) = elems[0].kind {
                match s.as_str() {
                    "define" => return eval_define(&elems[1..], expr.pos, env),
                    "if" => return eval_if(&elems[1..], expr.pos, env),
                    "quote" => return eval_quote(&elems[1..], expr.pos),
                    "lambda" => return eval_lambda(&elems[1..], expr.pos, env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], expr.pos, env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    _ => {}
                }
            }
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..].iter()
                .map(|e| eval(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            apply(&func, &args, expr.pos)
        }
        ValueKind::Void => Ok(expr.clone()),
    }
}

fn eval_define(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Syntax(format!("define requires at least 2 arguments at {}", fmt_pos(pos))));
    }
    match &args[0].kind {
        ValueKind::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::unpos(ValueKind::Void))
        }
        ValueKind::List(parts) => {
            if parts.is_empty() {
                return Err(EvalError::Syntax(format!("define: empty name list at {}", fmt_pos(pos))));
            }
            let name = match &parts[0].kind {
                ValueKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Syntax(format!("define: expected symbol as function name at {}", fmt_pos(pos)))),
            };
            let params: Vec<String> = parts[1..].iter().map(|p| {
                match &p.kind {
                    ValueKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Syntax(format!("define: expected symbol as parameter at {}", fmt_pos(p.pos)))),
                }
            }).collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::new(ValueKind::Lambda { params, body, env: Rc::clone(env) }, pos);
            env.set(name, lambda);
            Ok(Value::unpos(ValueKind::Void))
        }
        _ => Err(EvalError::Syntax(format!("define: expected symbol or list at {}", fmt_pos(pos)))),
    }
}

fn eval_if(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Syntax(format!("if requires 2 or 3 arguments at {}", fmt_pos(pos))));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::unpos(ValueKind::Void))
    }
}

fn eval_quote(args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Syntax(format!("quote requires exactly 1 argument at {}", fmt_pos(pos))));
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Syntax(format!("lambda requires parameters and body at {}", fmt_pos(pos))));
    }
    let params = match &args[0].kind {
        ValueKind::List(elems) => {
            elems.iter().map(|p| {
                match &p.kind {
                    ValueKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Syntax(format!("lambda: expected symbol as parameter at {}", fmt_pos(p.pos)))),
                }
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Syntax(format!("lambda: expected parameter list at {}", fmt_pos(args[0].pos)))),
    };
    let body = args[1..].to_vec();
    Ok(Value::new(ValueKind::Lambda { params, body, env: Rc::clone(env) }, pos))
}

fn eval_and(exprs: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::unpos(ValueKind::Boolean(true)));
    }
    let mut result = Value::unpos(ValueKind::Boolean(true));
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
        return Ok(Value::unpos(ValueKind::Boolean(false)));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::unpos(ValueKind::Boolean(false)))
}

fn eval_let(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Syntax(format!("let requires bindings and body at {}", fmt_pos(pos))));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ValueKind::Symbol(ref name) = args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Syntax(format!("named let requires bindings and body at {}", fmt_pos(pos))));
        }
        let bindings = match &args[1].kind {
            ValueKind::List(b) => b,
            _ => return Err(EvalError::Syntax(format!("let: expected binding list at {}", fmt_pos(args[1].pos)))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings {
            match &binding.kind {
                ValueKind::List(pair) if pair.len() == 2 => {
                    params.push(match &pair[0].kind {
                        ValueKind::Symbol(s) => s.clone(),
                        _ => return Err(EvalError::Syntax(format!("let: expected symbol at {}", fmt_pos(pair[0].pos)))),
                    });
                    inits.push(eval(&pair[1], env)?);
                }
                _ => return Err(EvalError::Syntax(format!("let: bad binding at {}", fmt_pos(binding.pos)))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = Env::new(Some(Rc::clone(env)));
        let lambda = Value::new(ValueKind::Lambda { params: params.clone(), body, env: Rc::clone(&local_env) }, pos);
        local_env.set(name.clone(), lambda);
        let func = local_env.get(name).unwrap();
        return apply(&func, &inits, pos);
    }
    let bindings = match &args[0].kind {
        ValueKind::List(b) => b,
        _ => return Err(EvalError::Syntax(format!("let: expected binding list at {}", fmt_pos(args[0].pos)))),
    };
    let local_env = Env::new(Some(Rc::clone(env)));
    for binding in bindings {
        match &binding.kind {
            ValueKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ValueKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Syntax(format!("let: expected symbol in binding at {}", fmt_pos(pair[0].pos)))),
                };
                let val = eval(&pair[1], env)?;
                local_env.set(name, val);
            }
            _ => return Err(EvalError::Syntax(format!("let: bad binding at {}", fmt_pos(binding.pos)))),
        }
    }
    let mut result = Value::unpos(ValueKind::Void);
    for expr in &args[1..] {
        result = eval(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let mut result = Value::unpos(ValueKind::Void);
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    for clause in clauses {
        let parts = match &clause.kind {
            ValueKind::List(p) => p,
            _ => return Err(EvalError::Syntax(format!("cond: expected list clause at {}", fmt_pos(clause.pos)))),
        };
        if parts.is_empty() {
            return Err(EvalError::Syntax(format!("cond: empty clause at {}", fmt_pos(clause.pos))));
        }
        if let ValueKind::Symbol(ref s) = parts[0].kind {
            if s == "else" {
                let mut result = Value::unpos(ValueKind::Void);
                for expr in &parts[1..] {
                    result = eval(expr, env)?;
                }
                return Ok(result);
            }
        }
        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(test);
            }
            let mut result = Value::unpos(ValueKind::Void);
            for expr in &parts[1..] {
                result = eval(expr, env)?;
            }
            return Ok(result);
        }
    }
    Ok(Value::unpos(ValueKind::Void))
}

fn apply(func: &Value, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match &func.kind {
        ValueKind::Symbol(name) => apply_builtin(name, args, call_pos),
        ValueKind::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {} at {}", params.len(), args.len(), fmt_pos(call_pos)
                )));
            }
            let local_env = Env::new(Some(Rc::clone(env)));
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.set(param.clone(), arg.clone());
            }
            let mut result = Value::unpos(ValueKind::Void);
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {} at {}", func, fmt_pos(call_pos)))),
    }
}

fn apply_builtin(name: &str, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer().ok_or_else(|| EvalError::Type(format!("+ expects numbers at {}", fmt_pos(call_pos))))?;
            }
            Ok(Value::unpos(ValueKind::Integer(sum)))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("- requires at least 1 argument at {}", fmt_pos(call_pos))));
            }
            let first = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("- expects numbers at {}", fmt_pos(call_pos))))?;
            if args.len() == 1 {
                Ok(Value::unpos(ValueKind::Integer(-first)))
            } else {
                let mut result = first;
                for a in &args[1..] {
                    result -= a.as_integer().ok_or_else(|| EvalError::Type(format!("- expects numbers at {}", fmt_pos(call_pos))))?;
                }
                Ok(Value::unpos(ValueKind::Integer(result)))
            }
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= a.as_integer().ok_or_else(|| EvalError::Type(format!("* expects numbers at {}", fmt_pos(call_pos))))?;
            }
            Ok(Value::unpos(ValueKind::Integer(prod)))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("/ requires at least 1 argument at {}", fmt_pos(call_pos))));
            }
            let first = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("/ expects numbers at {}", fmt_pos(call_pos))))?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::Runtime(format!("division by zero at {}", fmt_pos(call_pos))));
                }
                Ok(Value::unpos(ValueKind::Integer(1 / first)))
            } else {
                let mut result = first;
                for a in &args[1..] {
                    let d = a.as_integer().ok_or_else(|| EvalError::Type(format!("/ expects numbers at {}", fmt_pos(call_pos))))?;
                    if d == 0 {
                        return Err(EvalError::Runtime(format!("division by zero at {}", fmt_pos(call_pos))));
                    }
                    result /= d;
                }
                Ok(Value::unpos(ValueKind::Integer(result)))
            }
        }
        "<" => cmp_op(args, |a, b| a < b, call_pos),
        ">" => cmp_op(args, |a, b| a > b, call_pos),
        "=" => cmp_op(args, |a, b| a == b, call_pos),
        "<=" => cmp_op(args, |a, b| a <= b, call_pos),
        ">=" => cmp_op(args, |a, b| a >= b, call_pos),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("not requires exactly 1 argument at {}", fmt_pos(call_pos))));
            }
            Ok(Value::unpos(ValueKind::Boolean(!args[0].is_truthy())))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("cons requires exactly 2 arguments at {}", fmt_pos(call_pos))));
            }
            match &args[1].kind {
                ValueKind::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::unpos(ValueKind::List(new_list)))
                }
                _ => {
                    Ok(Value::unpos(ValueKind::List(vec![
                        args[0].clone(),
                        Value::unpos(ValueKind::Symbol(".".into())),
                        args[1].clone(),
                    ])))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("car requires exactly 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type(format!("car: not a pair at {}", fmt_pos(call_pos)))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("cdr requires exactly 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::List(elems) if !elems.is_empty() => {
                    Ok(Value::unpos(ValueKind::List(elems[1..].to_vec())))
                }
                _ => Err(EvalError::Type(format!("cdr: not a pair at {}", fmt_pos(call_pos)))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("null? requires exactly 1 argument at {}", fmt_pos(call_pos))));
            }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::List(e) if e.is_empty()))))
        }
        "list" => {
            Ok(Value::unpos(ValueKind::List(args.to_vec())))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("length requires exactly 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::List(elems) => Ok(Value::unpos(ValueKind::Integer(elems.len() as i64))),
                _ => Err(EvalError::Type(format!("length: not a list at {}", fmt_pos(call_pos)))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                match &arg.kind {
                    ValueKind::List(elems) => result.extend(elems.iter().cloned()),
                    _ if i == args.len() - 1 => {
                        result.push(arg.clone());
                    }
                    _ => return Err(EvalError::Type(format!("append: not a list at {}", fmt_pos(call_pos)))),
                }
            }
            Ok(Value::unpos(ValueKind::List(result)))
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Str(_)))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("number? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Integer(_)))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("boolean? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Boolean(_)))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("pair? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::List(e) if !e.is_empty()))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("symbol? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Symbol(_)))))
        }
        _ => Err(EvalError::UnboundVariable(format!("{} at {}", name, fmt_pos(call_pos)))),
    }
}

fn cmp_op(args: &[Value], op: fn(i64, i64) -> bool, call_pos: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("comparison requires at least 2 arguments at {}", fmt_pos(call_pos))));
    }
    let nums: Vec<i64> = args.iter()
        .map(|a| a.as_integer().ok_or_else(|| EvalError::Type(format!("comparison expects numbers at {}", fmt_pos(call_pos)))))
        .collect::<Result<Vec<_>, _>>()?;
    for w in nums.windows(2) {
        if !op(w[0], w[1]) {
            return Ok(Value::unpos(ValueKind::Boolean(false)));
        }
    }
    Ok(Value::unpos(ValueKind::Boolean(true)))
}
