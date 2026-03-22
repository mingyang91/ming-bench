use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "string?", "number?", "boolean?", "pair?", "symbol?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "char?",
    "string-copy",
    "apply",
];

fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name)
}

/// Evaluate a Scheme expression in the given environment.
/// Uses a trampoline loop for tail call optimization.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &mut String) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = Rc::clone(env);

    loop {
        match current_expr {
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) => {
                return Ok(current_expr);
            }
            Value::Symbol(ref name) => {
                return match current_env.borrow().get(name) {
                    Ok(val) => Ok(val),
                    Err(_) if is_builtin(name) => Ok(current_expr),
                    Err(e) => Err(e),
                };
            }
            Value::List(ref items) => {
                if items.is_empty() {
                    return Err(EvalError::parse("empty application"));
                }
                let items = items.clone();
                match eval_list_tco(&items, &current_env, out)? {
                    Trampoline::Done(val) => return Ok(val),
                    Trampoline::TailCall { expr, env } => {
                        current_expr = expr;
                        current_env = env;
                        continue;
                    }
                }
            }
            Value::Lambda { .. } => return Ok(current_expr),
            Value::Void => return Ok(Value::Void),
        }
    }
}

/// Result of evaluating in a tail-call-aware context.
enum Trampoline {
    Done(Value),
    TailCall {
        expr: Value,
        env: Rc<RefCell<Env>>,
    },
}

fn eval_list_tco(
    items: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    // Check for special forms first
    if let Value::Symbol(op) = &items[0] {
        match op.as_str() {
            "and" => return eval_and_tco(&items[1..], env, out),
            "or" => return eval_or_tco(&items[1..], env, out),
            "if" => return eval_if_tco(&items[1..], env, out),
            "define" => {
                let val = eval_define(&items[1..], env, out)?;
                return Ok(Trampoline::Done(val));
            }
            "quote" => return Ok(Trampoline::Done(eval_quote(&items[1..])?)),
            "lambda" => return Ok(Trampoline::Done(eval_lambda(&items[1..], env)?)),
            "let" => return eval_let_tco(&items[1..], env, out),
            "begin" => return eval_begin_tco(&items[1..], env, out),
            "cond" => return eval_cond_tco(&items[1..], env, out),
            "set!" => {
                let val = eval_set(&items[1..], env, out)?;
                return Ok(Trampoline::Done(val));
            }
            "string-set!" => {
                let val = eval_string_set(&items[1..], env, out)?;
                return Ok(Trampoline::Done(val));
            }
            _ => {}
        }
    }

    // Evaluate operator
    let operator = eval(&items[0], env, out)?;

    // Evaluate arguments
    let args: Vec<Value> = items[1..]
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<Vec<_>, _>>()?;

    apply_tco(&operator, &args, out)
}

fn apply_tco(
    operator: &Value,
    args: &[Value],
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    match operator {
        Value::Symbol(ref name) if name == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::arity("apply requires at least 2 arguments"));
            }
            let proc = &args[0];
            let last = &args[args.len() - 1];
            let Value::List(tail_args) = last else {
                return Err(EvalError::type_err(format!(
                    "apply: last argument must be a list, got {last}"
                )));
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail_args.iter().cloned());
            apply_tco(proc, &all_args, out)
        }
        Value::Symbol(name) => {
            let val = apply_builtin(name, args, out)?;
            Ok(Trampoline::Done(val))
        }
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => {
            if let Some(ref rest_name) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::arity(format!(
                        "expected at least {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local = Env::with_parent(closure);
                for (param, arg) in params.iter().zip(args.iter()) {
                    local.borrow_mut().set(param.clone(), arg.clone());
                }
                let rest = Value::List(args[params.len()..].to_vec());
                local.borrow_mut().set(rest_name.clone(), rest);
                for expr in &body[..body.len() - 1] {
                    eval(expr, &local, out)?;
                }
                Ok(Trampoline::TailCall {
                    expr: body[body.len() - 1].clone(),
                    env: local,
                })
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::arity(format!(
                        "expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local = Env::with_parent(closure);
                for (param, arg) in params.iter().zip(args.iter()) {
                    local.borrow_mut().set(param.clone(), arg.clone());
                }
                for expr in &body[..body.len() - 1] {
                    eval(expr, &local, out)?;
                }
                Ok(Trampoline::TailCall {
                    expr: body[body.len() - 1].clone(),
                    env: local,
                })
            }
        }
        _ => Err(EvalError::type_err(format!("not a procedure: {operator}"))),
    }
}

fn apply_builtin(name: &str, args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += require_int(a, "+")?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::arity("- requires at least 1 argument"));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-require_int(&args[0], "-")?));
            }
            let mut result = require_int(&args[0], "-")?;
            for a in &args[1..] {
                result -= require_int(a, "-")?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= require_int(a, "*")?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::arity("/ requires at least 1 argument"));
            }
            let mut result = require_int(&args[0], "/")?;
            for a in &args[1..] {
                let divisor = require_int(a, "/")?;
                if divisor == 0 {
                    return Err(EvalError::div_zero());
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, "<", |a, b| a < b),
        ">" => compare_nums(args, ">", |a, b| a > b),
        "=" => compare_nums(args, "=", |a, b| a == b),
        "<=" => compare_nums(args, "<=", |a, b| a <= b),
        ">=" => compare_nums(args, ">=", |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::arity("not requires exactly 1 argument"));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::arity("cons requires exactly 2 arguments"));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Err(EvalError::type_err(format!(
                    "cons: second argument must be a list, got {}", args[1]
                ))),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::arity("car requires exactly 1 argument"));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::type_err(format!(
                    "car: expected non-empty list, got {}", args[0]
                ))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::arity("cdr requires exactly 1 argument"));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => {
                    Ok(Value::List(items[1..].to_vec()))
                }
                _ => Err(EvalError::type_err(format!(
                    "cdr: expected non-empty list, got {}", args[0]
                ))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("null? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::arity("length requires exactly 1 argument"));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::type_err(format!(
                    "length: expected list, got {}", args[0]
                ))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(items) => result.extend(items.iter().cloned()),
                    _ => {
                        return Err(EvalError::type_err(format!(
                            "append: expected list, got {arg}"
                        )));
                    }
                }
            }
            Ok(Value::List(result))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("number? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("boolean? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("pair? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("symbol? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        // I/O builtins
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::arity("display requires exactly 1 argument"));
            }
            args[0].display_fmt(out);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::arity("write requires exactly 1 argument"));
            }
            args[0].write_fmt(out);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::arity("newline requires 0 arguments"));
            }
            out.push('\n');
            Ok(Value::Void)
        }
        // String builtins
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "char?" | "string-copy" => apply_string_builtin(name, args),
        _ => Err(EvalError::unbound(name)),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::type_err(format!(
                        "string-append: expected string, got {a}"
                    ))),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string-length requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::type_err(format!(
                    "string-length: expected string, got {}", args[0]
                ))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::arity("substring requires exactly 3 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::type_err(format!(
                    "substring: expected string, got {}", args[0]
                ))),
            };
            let start = require_int(&args[1], "substring")? as usize;
            let end = require_int(&args[2], "substring")? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::type_err(format!(
                    "substring: index out of range for string of length {}", s.len()
                )));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string->number requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::type_err(format!(
                    "string->number: expected string, got {}", args[0]
                ))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::arity("number->string requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Str(n.to_string())),
                _ => Err(EvalError::type_err(format!(
                    "number->string: expected number, got {}", args[0]
                ))),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::arity("symbol->string requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::type_err(format!(
                    "symbol->string: expected symbol, got {}", args[0]
                ))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string->symbol requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::type_err(format!(
                    "string->symbol: expected string, got {}", args[0]
                ))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::arity("string-ref requires exactly 2 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::type_err(format!(
                    "string-ref: expected string, got {}", args[0]
                ))),
            };
            let idx = require_int(&args[1], "string-ref")? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::type_err(format!(
                    "string-ref: index {idx} out of range for string of length {}", s.len()
                ))),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("char? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string-copy requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::type_err(format!(
                    "string-copy: expected string, got {}", args[0]
                ))),
            }
        }
        _ => Err(EvalError::unbound(name)),
    }
}

fn eval_and_tco(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let result = eval(expr, env, out)?;
        if !result.is_truthy() {
            return Ok(Trampoline::Done(result));
        }
    }
    // Tail position: last expression
    Ok(Trampoline::TailCall {
        expr: exprs[exprs.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_or_tco(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let result = eval(expr, env, out)?;
        if result.is_truthy() {
            return Ok(Trampoline::Done(result));
        }
    }
    // Tail position: last expression
    Ok(Trampoline::TailCall {
        expr: exprs[exprs.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_if_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::arity("if requires 2 or 3 arguments"));
    }
    let cond = eval(&args[0], env, out)?;
    if cond.is_truthy() {
        Ok(Trampoline::TailCall {
            expr: args[1].clone(),
            env: Rc::clone(env),
        })
    } else if args.len() == 3 {
        Ok(Trampoline::TailCall {
            expr: args[2].clone(),
            env: Rc::clone(env),
        })
    } else {
        Ok(Trampoline::Done(Value::Void))
    }
}

fn eval_define(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::arity("define requires at least 2 arguments"));
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::arity("define requires exactly 2 arguments"));
            }
            let val = eval(&args[1], env, out)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::parse("define: empty signature"));
            }
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::type_err("define: expected function name"));
            };
            let (params, rest_param) = parse_params(&sig[1..], "define")?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                closure: Rc::clone(env),
            };
            env.borrow_mut().set(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::type_err("define: expected symbol or list")),
    }
}

/// Parse a parameter list, handling dot notation for rest params.
/// `(x y . rest)` → params=["x","y"], rest=Some("rest")
/// `(x y)` → params=["x","y"], rest=None
fn parse_params(param_list: &[Value], context: &str) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = param_list.iter().position(|v| matches!(v, Value::Symbol(s) if s == "."));
    match dot_pos {
        Some(pos) => {
            if pos + 2 != param_list.len() {
                return Err(EvalError::parse(format!("{context}: invalid dot notation")));
            }
            let params: Vec<String> = param_list[..pos]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::type_err(format!("{context}: expected parameter name"))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let Value::Symbol(rest) = &param_list[pos + 1] else {
                return Err(EvalError::type_err(format!("{context}: expected rest parameter name")));
            };
            Ok((params, Some(rest.clone())))
        }
        None => {
            let params: Vec<String> = param_list
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::type_err(format!("{context}: expected parameter name"))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok((params, None))
        }
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::arity("quote requires exactly 1 argument"));
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::arity("lambda requires at least 2 arguments"));
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::type_err("lambda: expected parameter list"));
    };
    let (params, rest_param) = parse_params(param_list, "lambda")?;
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        closure: Rc::clone(env),
    })
}

fn eval_let_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::arity("let requires at least 2 arguments"));
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::arity("named let requires bindings and body"));
        }
        let Value::List(bindings) = &args[1] else {
            return Err(EvalError::type_err("named let: expected bindings list"));
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            let Value::List(pair) = binding else {
                return Err(EvalError::type_err("let: expected binding pair"));
            };
            if pair.len() != 2 {
                return Err(EvalError::arity("let: binding must have 2 elements"));
            }
            let Value::Symbol(param) = &pair[0] else {
                return Err(EvalError::type_err("let: expected variable name"));
            };
            params.push(param.clone());
            init_vals.push(eval(&pair[1], env, out)?);
        }
        let body = args[2..].to_vec();
        let local = Env::with_parent(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            closure: Rc::clone(&local),
        };
        local.borrow_mut().set(name.clone(), lambda);
        // Bind init values and tail-call into body
        let func = local.borrow().get(name).expect("just defined");
        return apply_tco(&func, &init_vals, out);
    }

    // Regular let: (let ((var init) ...) body ...)
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::type_err("let: expected bindings list"));
    };
    let local = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::type_err("let: expected binding pair"));
        };
        if pair.len() != 2 {
            return Err(EvalError::arity("let: binding must have 2 elements"));
        }
        let Value::Symbol(name) = &pair[0] else {
            return Err(EvalError::type_err("let: expected variable name"));
        };
        let val = eval(&pair[1], env, out)?;
        local.borrow_mut().set(name.clone(), val);
    }
    // Evaluate all but last, then tail-call last
    let body = &args[1..];
    for expr in &body[..body.len() - 1] {
        eval(expr, &local, out)?;
    }
    Ok(Trampoline::TailCall {
        expr: body[body.len() - 1].clone(),
        env: local,
    })
}

fn eval_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::arity("set! requires exactly 2 arguments"));
    }
    let Value::Symbol(name) = &args[0] else {
        return Err(EvalError::type_err("set!: first argument must be a symbol"));
    };
    let val = eval(&args[1], env, out)?;
    env.borrow_mut().update(name, val)?;
    Ok(Value::Void)
}

fn eval_string_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::arity("string-set! requires exactly 3 arguments"));
    }
    let Value::Symbol(var_name) = &args[0] else {
        return Err(EvalError::type_err("string-set!: first argument must be a variable"));
    };
    let idx_val = eval(&args[1], env, out)?;
    let idx = require_int(&idx_val, "string-set!")? as usize;
    let char_val = eval(&args[2], env, out)?;
    let Value::Char(ch) = char_val else {
        return Err(EvalError::type_err(format!(
            "string-set!: expected char, got {char_val}"
        )));
    };
    let current = env.borrow().get(var_name)?;
    let Value::Str(s) = current else {
        return Err(EvalError::type_err(format!(
            "string-set!: expected string, got {current}"
        )));
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::type_err(format!(
            "string-set!: index {idx} out of range for string of length {}", chars.len()
        )));
    }
    chars[idx] = ch;
    let new_str: String = chars.into_iter().collect();
    env.borrow_mut().set(var_name.clone(), Value::Str(new_str));
    Ok(Value::Void)
}

fn eval_begin_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Void));
    }
    for expr in &args[..args.len() - 1] {
        eval(expr, env, out)?;
    }
    Ok(Trampoline::TailCall {
        expr: args[args.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_cond_tco(
    clauses: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::type_err("cond: expected clause"));
        };
        if parts.is_empty() {
            return Err(EvalError::arity("cond: empty clause"));
        }
        // Check for else clause
        if let Value::Symbol(s) = &parts[0] {
            if s == "else" {
                if parts.len() == 1 {
                    return Ok(Trampoline::Done(Value::Void));
                }
                for expr in &parts[1..parts.len() - 1] {
                    eval(expr, env, out)?;
                }
                return Ok(Trampoline::TailCall {
                    expr: parts[parts.len() - 1].clone(),
                    env: Rc::clone(env),
                });
            }
        }
        let test = eval(&parts[0], env, out)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(Trampoline::Done(test));
            }
            for expr in &parts[1..parts.len() - 1] {
                eval(expr, env, out)?;
            }
            return Ok(Trampoline::TailCall {
                expr: parts[parts.len() - 1].clone(),
                env: Rc::clone(env),
            });
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn require_int(val: &Value, op: &str) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::type_err(format!("{op} requires integer, got {val}"))),
    }
}

fn compare_nums(
    args: &[Value],
    op: &str,
    cmp: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::arity(format!("{op} requires at least 2 arguments")));
    }
    let first = require_int(&args[0], op)?;
    let second = require_int(&args[1], op)?;
    Ok(Value::Boolean(cmp(first, second)))
}
