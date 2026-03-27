use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub type Output = Rc<RefCell<String>>;

pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_)
        | Value::Char(_) | Value::Lambda { .. } | Value::Pair(_, _) => {
            Ok(expr.clone())
        }
        Value::Symbol(name) => env.borrow().get(name),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            if let Value::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, out),
                    "if" => return eval_if(&elems[1..], env, out),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into()));
                        }
                        return Ok(elems[1].clone());
                    }
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env, out),
                    "or" => return eval_or(&elems[1..], env, out),
                    "let" => return eval_let(&elems[1..], env, out),
                    "begin" => return eval_begin(&elems[1..], env, out),
                    "cond" => return eval_cond(&elems[1..], env, out),
                    "set!" => return eval_set_bang(&elems[1..], env, out),
                    "string-set!" => return eval_string_set(&elems[1..], env, out),
                    _ => {}
                }
            }
            let func = eval(&elems[0], env, out)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|a| eval(a, env, out))
                .collect::<Result<_, _>>()?;
            apply(&func, &args, out)
        }
        Value::Void => Ok(Value::Void),
    }
}

fn eval_set_bang(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("set! requires 2 arguments".into()));
    }
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("set!: first argument must be a symbol".into())),
    };
    let val = eval(&args[1], env, out)?;
    Env::set_existing(env, &name, val)?;
    Ok(Value::Void)
}

fn eval_define(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires 2 arguments".into()));
            }
            let val = eval(&args[1], env, out)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Value::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as name".into())),
            };
            let (params, rest_param) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env, out)?;
    if cond.is_truthy() {
        eval(&args[1], env, out)
    } else if args.len() == 3 {
        eval(&args[2], env, out)
    } else {
        Ok(Value::Void)
    }
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let (params, rest_param) = match &args[0] {
        Value::List(elems) => parse_params(elems)?,
        Value::Symbol(s) => {
            // (lambda args body) — all args collected as rest
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: Rc::clone(env),
    })
}

fn parse_params(sig: &[Value]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < sig.len() {
        match &sig[i] {
            Value::Symbol(s) if s == "." => {
                if i + 1 != sig.len() - 1 {
                    return Err(EvalError::Parse("invalid dot notation in parameters".into()));
                }
                match &sig[i + 1] {
                    Value::Symbol(r) => rest_param = Some(r.clone()),
                    _ => return Err(EvalError::Type("expected symbol after dot".into())),
                }
                break;
            }
            Value::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Type("expected symbol as parameter".into())),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn apply(func: &Value, args: &[Value], out: &Output) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            if let Some(ref _rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
            } else if args.len() != params.len() {
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
            if let Some(ref rest) = rest_param {
                let rest_args = args[params.len()..].to_vec();
                local_env.borrow_mut().set(rest.clone(), Value::List(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Symbol(op) => {
            if op == "apply" {
                return builtin_apply(args, out);
            }
            apply_builtin(op, args, out)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
    }
}

fn builtin_apply(args: &[Value], out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail = match last {
        Value::List(elems) => elems.clone(),
        _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
    };
    let mut full_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    full_args.extend(tail);
    apply(func, &full_args, out)
}

fn apply_builtin(op: &str, vals: &[Value], out: &Output) -> Result<Value, EvalError> {
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
                    Ok(Value::Pair(Box::new(vals[0].clone()), Box::new(vals[1].clone())))
                }
            }
        }
        "car" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::Pair(a, _) => Ok(*a.clone()),
                _ => Err(EvalError::Type("car: expected non-empty list".into())),
            }
        }
        "cdr" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                Value::Pair(_, b) => Ok(*b.clone()),
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
            Ok(Value::Boolean(matches!(&vals[0], Value::List(e) if !e.is_empty()) || matches!(&vals[0], Value::Pair(_, _))))
        }
        "char?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Char(_))))
        }
        // L05: display, write, newline
        "display" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("display requires 1 argument".into()));
            }
            out.borrow_mut().push_str(&vals[0].to_display_repr());
            Ok(Value::Void)
        }
        "write" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("write requires 1 argument".into()));
            }
            out.borrow_mut().push_str(&vals[0].to_display());
            Ok(Value::Void)
        }
        "newline" => {
            if !vals.is_empty() {
                return Err(EvalError::Arity("newline requires 0 arguments".into()));
            }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        // L05: string operations
        "string-append" => {
            let mut result = std::string::String::new();
            for v in vals {
                match v {
                    Value::String(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(Value::String(result))
        }
        "string-length" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string-length requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if vals.len() != 3 {
                return Err(EvalError::Arity("substring requires 3 arguments".into()));
            }
            let s = match &vals[0] {
                Value::String(s) => s,
                _ => return Err(EvalError::Type("substring: expected string".into())),
            };
            let start = expect_int(&vals[1])? as usize;
            let end = expect_int(&vals[2])? as usize;
            Ok(Value::String(s[start..end].to_string()))
        }
        "string->number" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string->number requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("number->string requires 1 argument".into()));
            }
            let n = expect_int(&vals[0])?;
            Ok(Value::String(n.to_string()))
        }
        "symbol->string" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("symbol->string requires 1 argument".into()));
            }
            match &vals[0] {
                Value::Symbol(s) => Ok(Value::String(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string->symbol requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-copy" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string-copy requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s) => Ok(Value::String(s.clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "string-ref" => {
            if vals.len() != 2 {
                return Err(EvalError::Arity("string-ref requires 2 arguments".into()));
            }
            let s = match &vals[0] {
                Value::String(s) => s,
                _ => return Err(EvalError::Type("string-ref: expected string".into())),
            };
            let idx = expect_int(&vals[1])? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Type("string-ref: index out of bounds".into())),
            }
        }
        // L09: numeric utilities
        "abs" => {
            if vals.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); }
            Ok(Value::Integer(expect_int(&vals[0])?.abs()))
        }
        "modulo" => {
            if vals.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); }
            let a = expect_int(&vals[0])?;
            let b = expect_int(&vals[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if vals.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); }
            let a = expect_int(&vals[0])?;
            let b = expect_int(&vals[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if vals.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); }
            let a = expect_int(&vals[0])?;
            let b = expect_int(&vals[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if vals.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
            let mut m = expect_int(&vals[0])?;
            for v in &vals[1..] { m = m.min(expect_int(v)?); }
            Ok(Value::Integer(m))
        }
        "max" => {
            if vals.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
            let mut m = expect_int(&vals[0])?;
            for v in &vals[1..] { m = m.max(expect_int(v)?); }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if vals.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); }
            let base = expect_int(&vals[0])?;
            let exp = expect_int(&vals[1])?;
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&vals[0])? == 0))
        }
        "positive?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&vals[0])? > 0))
        }
        "negative?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&vals[0])? < 0))
        }
        "odd?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&vals[0])? % 2 != 0))
        }
        "even?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&vals[0])? % 2 == 0))
        }
        // L09: list utilities
        "list-ref" => {
            if vals.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); }
            let elems = match &vals[0] {
                Value::List(e) => e,
                _ => return Err(EvalError::Type("list-ref: expected list".into())),
            };
            let idx = expect_int(&vals[1])? as usize;
            elems.get(idx).cloned().ok_or_else(|| EvalError::Type("list-ref: index out of bounds".into()))
        }
        "list-tail" => {
            if vals.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
            let elems = match &vals[0] {
                Value::List(e) => e,
                _ => return Err(EvalError::Type("list-tail: expected list".into())),
            };
            let idx = expect_int(&vals[1])? as usize;
            if idx > elems.len() { return Err(EvalError::Type("list-tail: index out of bounds".into())); }
            Ok(Value::List(elems[idx..].to_vec()))
        }
        "list?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::List(_))))
        }
        "assoc" => {
            if vals.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
            let key = &vals[0];
            let alist = match &vals[1] {
                Value::List(e) => e,
                _ => return Err(EvalError::Type("assoc: expected list".into())),
            };
            for entry in alist {
                if let Value::List(pair) = entry {
                    if !pair.is_empty() && pair[0] == *key {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        "eq?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); }
            Ok(Value::Boolean(vals[0] == vals[1]))
        }
        "equal?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); }
            Ok(Value::Boolean(vals[0] == vals[1]))
        }
        "map" => {
            if vals.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
            let func = &vals[0];
            let lists: Vec<&Vec<Value>> = vals[1..].iter().map(|v| match v {
                Value::List(e) => Ok(e),
                _ => Err(EvalError::Type("map: expected list".into())),
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for l in &lists {
                if l.len() != len { return Err(EvalError::Arity("map: lists must have same length".into())); }
            }
            let mut result = Vec::new();
            for i in 0..len {
                let args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply(func, &args, out)?);
            }
            Ok(Value::List(result))
        }
        // L09: char utilities
        "char-alphabetic?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type("char-alphabetic?: expected char".into())),
            }
        }
        "char-numeric?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type("char-numeric?: expected char".into())),
            }
        }
        "char-upcase" => {
            if vals.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type("char-upcase: expected char".into())),
            }
        }
        "char-downcase" => {
            if vals.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type("char-downcase: expected char".into())),
            }
        }
        "char=?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("char=?: expected chars".into())),
            }
        }
        "char<?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: expected chars".into())),
            }
        }
        // L09: string comparison/case
        "string=?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("string=?: expected strings".into())),
            }
        }
        "string<?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("string<?: expected strings".into())),
            }
        }
        "string-ci=?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
            }
        }
        "string-upcase" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
            match &vals[0] {
                Value::String(s) => Ok(Value::String(s.to_uppercase())),
                _ => Err(EvalError::Type("string-upcase: expected string".into())),
            }
        }
        "string-downcase" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
            match &vals[0] {
                Value::String(s) => Ok(Value::String(s.to_lowercase())),
                _ => Err(EvalError::Type("string-downcase: expected string".into())),
            }
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

fn eval_string_set(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires 3 arguments".into()));
    }
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("string-set!: first argument must be a variable".into())),
    };
    let idx = expect_int(&eval(&args[1], env, out)?)? as usize;
    let ch = match eval(&args[2], env, out)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: third argument must be a char".into())),
    };
    let mut s = match env.borrow().get(&name)? {
        Value::String(s) => s,
        _ => return Err(EvalError::Type("string-set!: expected string".into())),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Type("string-set!: index out of bounds".into()));
    }
    chars[idx] = ch;
    s = chars.into_iter().collect();
    env.borrow_mut().set(name, Value::String(s));
    Ok(Value::Void)
}

fn eval_let(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
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
                        inits.push(eval(&pair[1], env, out)?);
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
            rest_param: None,
            body,
            env: Rc::clone(&loop_env),
        };
        loop_env.borrow_mut().set(name.clone(), lambda);
        let func = loop_env.borrow().get(name)?;
        apply(&func, &inits, out)
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
                        let val = eval(&pair[1], env, out)?;
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
            result = eval(expr, &local_env, out)?;
        }
        Ok(result)
    }
}

fn eval_begin(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env, out)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Value::List(parts) if !parts.is_empty() => {
                if let Value::Symbol(s) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env, out)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env, out)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn eval_and(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg, env, out)?;
        if !val.is_truthy() {
            return Ok(val);
        }
    }
    eval(&args[args.len() - 1], env, out)
}

fn eval_or(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg, env, out)?;
        if val.is_truthy() {
            return Ok(val);
        }
    }
    eval(&args[args.len() - 1], env, out)
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
