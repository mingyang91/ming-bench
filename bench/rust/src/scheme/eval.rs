use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::value::{Value, ValueKind, Pos};
use crate::scheme::EvalError;

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

pub fn with_output_capture<F, T>(f: F) -> (T, String)
where
    F: FnOnce() -> T,
{
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let result = f();
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    (result, output)
}

fn push_output(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

fn fmt_pos(pos: Pos) -> String {
    format!("{}:{}", pos.0, pos.1)
}

pub fn eval(expr: &Value, env: &Rc<Env>) -> Result<Value, EvalError> {
    match &expr.kind {
        ValueKind::Integer(_) | ValueKind::Boolean(_) | ValueKind::Str(_) | ValueKind::Char(_) => Ok(expr.clone()),
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
                    "set!" => return eval_set(&elems[1..], expr.pos, env),
                    "string-set!" => return eval_string_set(&elems[1..], expr.pos, env),
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
            let (params, rest_param) = parse_params(&parts[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::new(ValueKind::Lambda { params, rest_param, body, env: Rc::clone(env) }, pos);
            env.set(name, lambda);
            Ok(Value::unpos(ValueKind::Void))
        }
        _ => Err(EvalError::Syntax(format!("define: expected symbol or list at {}", fmt_pos(pos)))),
    }
}

fn eval_set(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Syntax(format!("set! requires exactly 2 arguments at {}", fmt_pos(pos))));
    }
    let name = match &args[0].kind {
        ValueKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Syntax(format!("set!: expected symbol at {}", fmt_pos(pos)))),
    };
    let val = eval(&args[1], env)?;
    if !env.set_existing(&name, val) {
        return Err(EvalError::UnboundVariable(format!("{} at {}", name, fmt_pos(pos))));
    }
    Ok(Value::unpos(ValueKind::Void))
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

fn parse_params(elems: &[Value]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < elems.len() {
        match &elems[i].kind {
            ValueKind::Symbol(s) if s == "." => {
                if i + 1 >= elems.len() || i + 2 != elems.len() {
                    return Err(EvalError::Syntax("bad dot in parameter list".into()));
                }
                rest_param = Some(match &elems[i + 1].kind {
                    ValueKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Syntax(format!("expected symbol after dot at {}", elems[i + 1].fmt_pos()))),
                });
                break;
            }
            ValueKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Syntax(format!("expected symbol as parameter at {}", elems[i].fmt_pos()))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Syntax(format!("lambda requires parameters and body at {}", fmt_pos(pos))));
    }
    let (params, rest_param) = match &args[0].kind {
        ValueKind::List(elems) => parse_params(elems)?,
        ValueKind::Symbol(s) => {
            // (lambda args body) — single rest param
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Syntax(format!("lambda: expected parameter list at {}", fmt_pos(args[0].pos)))),
    };
    let body = args[1..].to_vec();
    Ok(Value::new(ValueKind::Lambda { params, rest_param, body, env: Rc::clone(env) }, pos))
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
        let lambda = Value::new(ValueKind::Lambda { params: params.clone(), rest_param: None, body, env: Rc::clone(&local_env) }, pos);
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

fn eval_string_set(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Syntax(format!("string-set! requires 3 arguments at {}", fmt_pos(pos))));
    }
    let var_name = match &args[0].kind {
        ValueKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Syntax(format!("string-set! expects a variable at {}", fmt_pos(pos)))),
    };
    let s_val = env.get(&var_name).ok_or_else(|| EvalError::UnboundVariable(format!("{} at {}", var_name, fmt_pos(pos))))?;
    let mut s = match &s_val.kind {
        ValueKind::Str(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("string-set!: not a string at {}", fmt_pos(pos)))),
    };
    let idx = eval(&args[1], env)?.as_integer().ok_or_else(|| EvalError::Type(format!("string-set!: index not a number at {}", fmt_pos(pos))))? as usize;
    let ch = match eval(&args[2], env)?.kind {
        ValueKind::Char(c) => c,
        _ => return Err(EvalError::Type(format!("string-set!: not a char at {}", fmt_pos(pos)))),
    };
    if idx >= s.len() {
        return Err(EvalError::Runtime(format!("string-set!: index out of range at {}", fmt_pos(pos))));
    }
    unsafe { s.as_bytes_mut()[idx] = ch as u8; }
    env.set_existing(&var_name, Value::unpos(ValueKind::Str(s)));
    Ok(Value::unpos(ValueKind::Void))
}

fn apply(func: &Value, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match &func.kind {
        ValueKind::Symbol(name) => apply_builtin(name, args, call_pos),
        ValueKind::Lambda { params, rest_param, body, env } => {
            if let Some(ref rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}", params.len(), args.len(), fmt_pos(call_pos)
                    )));
                }
                let local_env = Env::new(Some(Rc::clone(env)));
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_env.set(param.clone(), arg.clone());
                }
                let rest_args = args[params.len()..].to_vec();
                local_env.set(rest.clone(), Value::unpos(ValueKind::List(rest_args)));
                let mut result = Value::unpos(ValueKind::Void);
                for expr in body {
                    result = eval(expr, &local_env)?;
                }
                Ok(result)
            } else {
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
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("display requires 1 argument at {}", fmt_pos(call_pos))));
            }
            push_output(&args[0].to_display_output());
            Ok(Value::unpos(ValueKind::Void))
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("write requires 1 argument at {}", fmt_pos(call_pos))));
            }
            push_output(&args[0].to_write_output());
            Ok(Value::unpos(ValueKind::Void))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!("newline takes 0 arguments at {}", fmt_pos(call_pos))));
            }
            push_output("\n");
            Ok(Value::unpos(ValueKind::Void))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match &a.kind {
                    ValueKind::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!("string-append: not a string at {}", fmt_pos(call_pos)))),
                }
            }
            Ok(Value::unpos(ValueKind::Str(result)))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("string-length requires 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::Str(s) => Ok(Value::unpos(ValueKind::Integer(s.len() as i64))),
                _ => Err(EvalError::Type(format!("string-length: not a string at {}", fmt_pos(call_pos)))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("substring requires 3 arguments at {}", fmt_pos(call_pos))));
            }
            let s = match &args[0].kind {
                ValueKind::Str(s) => s,
                _ => return Err(EvalError::Type(format!("substring: not a string at {}", fmt_pos(call_pos)))),
            };
            let start = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("substring: not a number at {}", fmt_pos(call_pos))))? as usize;
            let end = args[2].as_integer().ok_or_else(|| EvalError::Type(format!("substring: not a number at {}", fmt_pos(call_pos))))? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::Runtime(format!("substring: index out of range at {}", fmt_pos(call_pos))));
            }
            Ok(Value::unpos(ValueKind::Str(s[start..end].to_string())))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("string->number requires 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::Str(s) => {
                    match s.parse::<i64>() {
                        Ok(n) => Ok(Value::unpos(ValueKind::Integer(n))),
                        Err(_) => Ok(Value::unpos(ValueKind::Boolean(false))),
                    }
                }
                _ => Err(EvalError::Type(format!("string->number: not a string at {}", fmt_pos(call_pos)))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("number->string requires 1 argument at {}", fmt_pos(call_pos))));
            }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("number->string: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Str(n.to_string())))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("symbol->string requires 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::Symbol(s) => Ok(Value::unpos(ValueKind::Str(s.clone()))),
                _ => Err(EvalError::Type(format!("symbol->string: not a symbol at {}", fmt_pos(call_pos)))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("string->symbol requires 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::Str(s) => Ok(Value::unpos(ValueKind::Symbol(s.clone()))),
                _ => Err(EvalError::Type(format!("string->symbol: not a string at {}", fmt_pos(call_pos)))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("string-ref requires 2 arguments at {}", fmt_pos(call_pos))));
            }
            let s = match &args[0].kind {
                ValueKind::Str(s) => s,
                _ => return Err(EvalError::Type(format!("string-ref: not a string at {}", fmt_pos(call_pos)))),
            };
            let idx = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("string-ref: not a number at {}", fmt_pos(call_pos))))? as usize;
            if idx >= s.len() {
                return Err(EvalError::Runtime(format!("string-ref: index out of range at {}", fmt_pos(call_pos))));
            }
            Ok(Value::unpos(ValueKind::Char(s.as_bytes()[idx] as char)))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("string-copy requires 1 argument at {}", fmt_pos(call_pos))));
            }
            match &args[0].kind {
                ValueKind::Str(s) => Ok(Value::unpos(ValueKind::Str(s.clone()))),
                _ => Err(EvalError::Type(format!("string-copy: not a string at {}", fmt_pos(call_pos)))),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("char? requires 1 argument at {}", fmt_pos(call_pos))));
            }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Char(_)))))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("apply requires at least 2 arguments at {}", fmt_pos(call_pos))));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match &last.kind {
                ValueKind::List(elems) => elems.clone(),
                _ => return Err(EvalError::Type(format!("apply: last argument must be a list at {}", fmt_pos(call_pos)))),
            };
            let mut combined = args[1..args.len() - 1].to_vec();
            combined.extend(tail);
            apply(func, &combined, call_pos)
        }
        // L09: Numeric utilities
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("abs requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("abs: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Integer(n.abs())))
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("modulo requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let a = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("modulo: not a number at {}", fmt_pos(call_pos))))?;
            let b = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("modulo: not a number at {}", fmt_pos(call_pos))))?;
            if b == 0 { return Err(EvalError::Runtime(format!("modulo: division by zero at {}", fmt_pos(call_pos)))); }
            let r = a % b;
            let result = if r == 0 || (r > 0) == (b > 0) { r } else { r + b };
            Ok(Value::unpos(ValueKind::Integer(result)))
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("remainder requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let a = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("remainder: not a number at {}", fmt_pos(call_pos))))?;
            let b = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("remainder: not a number at {}", fmt_pos(call_pos))))?;
            if b == 0 { return Err(EvalError::Runtime(format!("remainder: division by zero at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Integer(a % b)))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("quotient requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let a = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("quotient: not a number at {}", fmt_pos(call_pos))))?;
            let b = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("quotient: not a number at {}", fmt_pos(call_pos))))?;
            if b == 0 { return Err(EvalError::Runtime(format!("quotient: division by zero at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Integer(a / b)))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity(format!("min requires at least 1 argument at {}", fmt_pos(call_pos)))); }
            let mut result = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("min: not a number at {}", fmt_pos(call_pos))))?;
            for a in &args[1..] {
                let n = a.as_integer().ok_or_else(|| EvalError::Type(format!("min: not a number at {}", fmt_pos(call_pos))))?;
                if n < result { result = n; }
            }
            Ok(Value::unpos(ValueKind::Integer(result)))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity(format!("max requires at least 1 argument at {}", fmt_pos(call_pos)))); }
            let mut result = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("max: not a number at {}", fmt_pos(call_pos))))?;
            for a in &args[1..] {
                let n = a.as_integer().ok_or_else(|| EvalError::Type(format!("max: not a number at {}", fmt_pos(call_pos))))?;
                if n > result { result = n; }
            }
            Ok(Value::unpos(ValueKind::Integer(result)))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("expt requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let base = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("expt: not a number at {}", fmt_pos(call_pos))))?;
            let exp = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("expt: not a number at {}", fmt_pos(call_pos))))?;
            if exp < 0 {
                Ok(Value::unpos(ValueKind::Integer(0)))
            } else {
                Ok(Value::unpos(ValueKind::Integer(base.pow(exp as u32))))
            }
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("zero? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("zero?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n == 0)))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("positive? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("positive?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n > 0)))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("negative? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("negative?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n < 0)))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("odd? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("odd?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n % 2 != 0)))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("even? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("even?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n % 2 == 0)))
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("integer? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Integer(_)))))
        }
        // L09: List utilities
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("list-ref requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let elems = match &args[0].kind {
                ValueKind::List(e) => e,
                _ => return Err(EvalError::Type(format!("list-ref: not a list at {}", fmt_pos(call_pos)))),
            };
            let idx = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("list-ref: not a number at {}", fmt_pos(call_pos))))? as usize;
            if idx >= elems.len() {
                return Err(EvalError::Runtime(format!("list-ref: index out of range at {}", fmt_pos(call_pos))));
            }
            Ok(elems[idx].clone())
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("list-tail requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let elems = match &args[0].kind {
                ValueKind::List(e) => e,
                _ => return Err(EvalError::Type(format!("list-tail: not a list at {}", fmt_pos(call_pos)))),
            };
            let idx = args[1].as_integer().ok_or_else(|| EvalError::Type(format!("list-tail: not a number at {}", fmt_pos(call_pos))))? as usize;
            if idx > elems.len() {
                return Err(EvalError::Runtime(format!("list-tail: index out of range at {}", fmt_pos(call_pos))));
            }
            Ok(Value::unpos(ValueKind::List(elems[idx..].to_vec())))
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("list? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let result = match &args[0].kind {
                ValueKind::List(elems) => {
                    // A proper list has no dot notation
                    !(elems.len() >= 3 && matches!(&elems[elems.len() - 2].kind, ValueKind::Symbol(s) if s == "."))
                }
                _ => false,
            };
            Ok(Value::unpos(ValueKind::Boolean(result)))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("assoc requires 2 arguments at {}", fmt_pos(call_pos)))); }
            let key = &args[0];
            let alist = match &args[1].kind {
                ValueKind::List(e) => e,
                _ => return Err(EvalError::Type(format!("assoc: not a list at {}", fmt_pos(call_pos)))),
            };
            for pair in alist {
                if let ValueKind::List(p) = &pair.kind {
                    if !p.is_empty() && p[0] == *key {
                        return Ok(pair.clone());
                    }
                }
            }
            Ok(Value::unpos(ValueKind::Boolean(false)))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("equal? requires 2 arguments at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(args[0] == args[1])))
        }
        "eqv?" | "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("{} requires 2 arguments at {}", name, fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(args[0] == args[1])))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity(format!("map requires at least 2 arguments at {}", fmt_pos(call_pos)))); }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match &a.kind {
                ValueKind::List(e) => Ok(e),
                _ => Err(EvalError::Type(format!("map: not a list at {}", fmt_pos(call_pos)))),
            }).collect::<Result<Vec<_>, _>>()?;
            let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
            let mut result = Vec::new();
            for i in 0..min_len {
                let func_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply(func, &func_args, call_pos)?);
            }
            Ok(Value::unpos(ValueKind::List(result)))
        }
        "for-each" => {
            if args.len() < 2 { return Err(EvalError::Arity(format!("for-each requires at least 2 arguments at {}", fmt_pos(call_pos)))); }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match &a.kind {
                ValueKind::List(e) => Ok(e),
                _ => Err(EvalError::Type(format!("for-each: not a list at {}", fmt_pos(call_pos)))),
            }).collect::<Result<Vec<_>, _>>()?;
            let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
            for i in 0..min_len {
                let func_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply(func, &func_args, call_pos)?;
            }
            Ok(Value::unpos(ValueKind::Void))
        }
        "reverse" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("reverse requires 1 argument at {}", fmt_pos(call_pos)))); }
            let elems = match &args[0].kind {
                ValueKind::List(e) => e,
                _ => return Err(EvalError::Type(format!("reverse: not a list at {}", fmt_pos(call_pos)))),
            };
            let mut rev = elems.clone();
            rev.reverse();
            Ok(Value::unpos(ValueKind::List(rev)))
        }
        // L09: Character utilities
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-alphabetic? requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Char(c) => Ok(Value::unpos(ValueKind::Boolean(c.is_alphabetic()))),
                _ => Err(EvalError::Type(format!("char-alphabetic?: not a char at {}", fmt_pos(call_pos)))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-numeric? requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Char(c) => Ok(Value::unpos(ValueKind::Boolean(c.is_ascii_digit()))),
                _ => Err(EvalError::Type(format!("char-numeric?: not a char at {}", fmt_pos(call_pos)))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-upcase requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Char(c) => Ok(Value::unpos(ValueKind::Char(c.to_ascii_uppercase()))),
                _ => Err(EvalError::Type(format!("char-upcase: not a char at {}", fmt_pos(call_pos)))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char-downcase requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Char(c) => Ok(Value::unpos(ValueKind::Char(c.to_ascii_lowercase()))),
                _ => Err(EvalError::Type(format!("char-downcase: not a char at {}", fmt_pos(call_pos)))),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("char=? requires 2 arguments at {}", fmt_pos(call_pos)))); }
            match (&args[0].kind, &args[1].kind) {
                (ValueKind::Char(a), ValueKind::Char(b)) => Ok(Value::unpos(ValueKind::Boolean(a == b))),
                _ => Err(EvalError::Type(format!("char=?: not chars at {}", fmt_pos(call_pos)))),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("char<? requires 2 arguments at {}", fmt_pos(call_pos)))); }
            match (&args[0].kind, &args[1].kind) {
                (ValueKind::Char(a), ValueKind::Char(b)) => Ok(Value::unpos(ValueKind::Boolean(a < b))),
                _ => Err(EvalError::Type(format!("char<?: not chars at {}", fmt_pos(call_pos)))),
            }
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char->integer requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Char(c) => Ok(Value::unpos(ValueKind::Integer(*c as i64))),
                _ => Err(EvalError::Type(format!("char->integer: not a char at {}", fmt_pos(call_pos)))),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("integer->char requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("integer->char: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Char(char::from_u32(n as u32).unwrap_or('\0'))))
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity(format!("make-string requires 1-2 arguments at {}", fmt_pos(call_pos)))); }
            let len = args[0].as_integer().ok_or_else(|| EvalError::Type(format!("make-string: not a number at {}", fmt_pos(call_pos))))? as usize;
            let ch = if args.len() == 2 {
                match &args[1].kind { ValueKind::Char(c) => *c, _ => return Err(EvalError::Type(format!("make-string: not a char at {}", fmt_pos(call_pos)))) }
            } else { '\0' };
            Ok(Value::unpos(ValueKind::Str(std::iter::repeat(ch).take(len).collect())))
        }
        // L09: String utilities
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string=? requires 2 arguments at {}", fmt_pos(call_pos)))); }
            match (&args[0].kind, &args[1].kind) {
                (ValueKind::Str(a), ValueKind::Str(b)) => Ok(Value::unpos(ValueKind::Boolean(a == b))),
                _ => Err(EvalError::Type(format!("string=?: not strings at {}", fmt_pos(call_pos)))),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string<? requires 2 arguments at {}", fmt_pos(call_pos)))); }
            match (&args[0].kind, &args[1].kind) {
                (ValueKind::Str(a), ValueKind::Str(b)) => Ok(Value::unpos(ValueKind::Boolean(a < b))),
                _ => Err(EvalError::Type(format!("string<?: not strings at {}", fmt_pos(call_pos)))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string-ci=? requires 2 arguments at {}", fmt_pos(call_pos)))); }
            match (&args[0].kind, &args[1].kind) {
                (ValueKind::Str(a), ValueKind::Str(b)) => Ok(Value::unpos(ValueKind::Boolean(a.to_lowercase() == b.to_lowercase()))),
                _ => Err(EvalError::Type(format!("string-ci=?: not strings at {}", fmt_pos(call_pos)))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-upcase requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Str(s) => Ok(Value::unpos(ValueKind::Str(s.to_uppercase()))),
                _ => Err(EvalError::Type(format!("string-upcase: not a string at {}", fmt_pos(call_pos)))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-downcase requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Str(s) => Ok(Value::unpos(ValueKind::Str(s.to_lowercase()))),
                _ => Err(EvalError::Type(format!("string-downcase: not a string at {}", fmt_pos(call_pos)))),
            }
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
