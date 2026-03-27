use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub type Output = Rc<RefCell<String>>;

pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Rational(_, _) | Value::Float(_)
        | Value::Boolean(_) | Value::String(..)
        | Value::Char(_) | Value::Lambda { .. } | Value::Pair(_, _)
        | Value::SyntaxRules { .. } | Value::Record { .. } | Value::RecordProc { .. }
        | Value::CaseLambda { .. } | Value::Vector(_) => {
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
                    "case-lambda" => return eval_case_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env, out),
                    "or" => return eval_or(&elems[1..], env, out),
                    "let" => return eval_let(&elems[1..], env, out),
                    "begin" => return eval_begin(&elems[1..], env, out),
                    "cond" => return eval_cond(&elems[1..], env, out),
                    "set!" => return eval_set_bang(&elems[1..], env, out),
                    "string-set!" => return eval_string_set(&elems[1..], env, out),
                    "vector-set!" => return eval_vector_set(&elems[1..], env, out),
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, out),
                    "define-record-type" => return eval_define_record_type(&elems[1..], env),
                    "letrec" => return eval_letrec(&elems[1..], env, out),
                    "letrec*" => return eval_letrec_star(&elems[1..], env, out),
                    "let*" => return eval_let_star(&elems[1..], env, out),
                    "case" => return eval_case(&elems[1..], env, out),
                    "do" => return eval_do(&elems[1..], env, out),
                    "when" => return eval_when(&elems[1..], env, out),
                    _ => {
                        // Check if symbol is bound to a macro
                        // Clone to release borrow before eval
                        let maybe_macro = env.borrow().get(op).ok();
                        if let Some(Value::SyntaxRules { ref literals, ref rules, ref def_env }) = maybe_macro {
                            let expanded = expand_macro(literals, rules, def_env, elems)?;
                            return eval(&expanded, env, out);
                        }
                    }
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

fn eval_case_lambda(clauses: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut parsed = Vec::new();
    for clause in clauses {
        let elems = match clause {
            Value::List(e) => e,
            _ => return Err(EvalError::Type("case-lambda: expected clause list".into())),
        };
        if elems.len() < 2 {
            return Err(EvalError::Arity("case-lambda: clause needs params and body".into()));
        }
        let (params, rest_param) = match &elems[0] {
            Value::List(p) => parse_params(p)?,
            Value::Symbol(s) => (vec![], Some(s.clone())),
            _ => return Err(EvalError::Type("case-lambda: expected parameter list".into())),
        };
        let body = elems[1..].to_vec();
        parsed.push((params, rest_param, body, Rc::clone(env)));
    }
    Ok(Value::CaseLambda { clauses: parsed })
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
        Value::CaseLambda { clauses } => {
            for (params, rest_param, body, env) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
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
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments", args.len()
            )))
        }
        Value::RecordProc { type_id, kind } => {
            use crate::scheme::value::RecordProcKind;
            match kind {
                RecordProcKind::Constructor { field_names } => {
                    if args.len() != field_names.len() {
                        return Err(EvalError::Arity(format!(
                            "record constructor expected {} arguments, got {}",
                            field_names.len(), args.len()
                        )));
                    }
                    Ok(Value::Record { type_id: *type_id, fields: args.to_vec() })
                }
                RecordProcKind::Predicate => {
                    if args.len() != 1 {
                        return Err(EvalError::Arity("record predicate requires 1 argument".into()));
                    }
                    Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)))
                }
                RecordProcKind::Accessor { field_index } => {
                    if args.len() != 1 {
                        return Err(EvalError::Arity("record accessor requires 1 argument".into()));
                    }
                    match &args[0] {
                        Value::Record { type_id: tid, fields } if tid == type_id => {
                            Ok(fields[*field_index].clone())
                        }
                        _ => Err(EvalError::Type("record accessor: wrong record type".into())),
                    }
                }
            }
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
            let mut acc = Num::Exact(0, 1);
            for v in vals {
                acc = num_add(acc, to_num(v)?);
            }
            Ok(num_to_value(acc))
        }
        "-" => {
            if vals.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if vals.len() == 1 {
                return Ok(num_to_value(num_neg(to_num(&vals[0])?)));
            }
            let mut acc = to_num(&vals[0])?;
            for v in &vals[1..] {
                acc = num_sub(acc, to_num(v)?);
            }
            Ok(num_to_value(acc))
        }
        "*" => {
            let mut acc = Num::Exact(1, 1);
            for v in vals {
                acc = num_mul(acc, to_num(v)?);
            }
            Ok(num_to_value(acc))
        }
        "/" => {
            if vals.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut acc = to_num(&vals[0])?;
            for v in &vals[1..] {
                acc = num_div(acc, to_num(v)?)?;
            }
            Ok(num_to_value(acc))
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
            Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))))
        }
        "integer?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("integer? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_))))
        }
        "rational?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("rational? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("exact? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("inexact? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if vals.len() != 1 { return Err(EvalError::Arity("exact->inexact requires 1 argument".into())); }
            match &vals[0] {
                Value::Integer(n) => Ok(Value::Float(*n as f64)),
                Value::Rational(n, d) => Ok(Value::Float(*n as f64 / *d as f64)),
                Value::Float(f) => Ok(Value::Float(*f)),
                _ => Err(EvalError::Type("exact->inexact: expected number".into())),
            }
        }
        "inexact->exact" => {
            if vals.len() != 1 { return Err(EvalError::Arity("inexact->exact requires 1 argument".into())); }
            match &vals[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, d) => Ok(Value::Rational(*n, *d)),
                Value::Float(f) => {
                    // Convert float to rational via continued fraction / simple approach
                    // For 0.5 => 1/2, etc.
                    let (num, den) = float_to_rational(*f);
                    Ok(Value::make_rational(num, den))
                }
                _ => Err(EvalError::Type("inexact->exact: expected number".into())),
            }
        }
        "numerator" => {
            if vals.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into())); }
            match &vals[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type("numerator: expected rational".into())),
            }
        }
        "denominator" => {
            if vals.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into())); }
            match &vals[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type("denominator: expected rational".into())),
            }
        }
        "boolean?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Boolean(_))))
        }
        "string?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::String(..))))
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
        "procedure?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("procedure? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Lambda { .. } | Value::CaseLambda { .. } | Value::RecordProc { .. })))
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
                    Value::String(s, _) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(Value::String(result, true))
        }
        "string-length" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string-length requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s, _) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if vals.len() != 3 {
                return Err(EvalError::Arity("substring requires 3 arguments".into()));
            }
            let s = match &vals[0] {
                Value::String(s, _) => s,
                _ => return Err(EvalError::Type("substring: expected string".into())),
            };
            let start = expect_int(&vals[1])? as usize;
            let end = expect_int(&vals[2])? as usize;
            Ok(Value::String(s[start..end].to_string(), true))
        }
        "string->number" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string->number requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s, _) => match s.parse::<i64>() {
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
            Ok(Value::String(vals[0].to_display(), true))
        }
        "symbol->string" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("symbol->string requires 1 argument".into()));
            }
            match &vals[0] {
                Value::Symbol(s) => Ok(Value::String(s.clone(), false)),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string->symbol requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s, _) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-copy" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string-copy requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s, _) => Ok(Value::String(s.clone(), true)),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "string->list" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("string->list requires 1 argument".into()));
            }
            match &vals[0] {
                Value::String(s, _) => {
                    let chars: Vec<Value> = s.chars().map(Value::Char).collect();
                    Ok(Value::List(chars))
                }
                _ => Err(EvalError::Type("string->list: expected string".into())),
            }
        }
        "list->string" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("list->string requires 1 argument".into()));
            }
            let items = match &vals[0] {
                Value::List(l) => l.clone(),
                other => {
                    let mut items = Vec::new();
                    let mut cur = other.clone();
                    loop {
                        match cur {
                            Value::Pair(a, b) => {
                                items.push(*a);
                                cur = *b;
                            }
                            Value::List(l) if l.is_empty() => break,
                            _ => return Err(EvalError::Type("list->string: expected list of chars".into())),
                        }
                    }
                    items
                }
            };
            let mut s = std::string::String::new();
            for item in &items {
                match item {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: expected list of chars".into())),
                }
            }
            Ok(Value::String(s, true))
        }
        "char->integer" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("char->integer requires 1 argument".into()));
            }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type("char->integer: expected char".into())),
            }
        }
        "integer->char" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("integer->char requires 1 argument".into()));
            }
            let n = expect_int(&vals[0])?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Type("integer->char: invalid code point".into())),
            }
        }
        "string-ref" => {
            if vals.len() != 2 {
                return Err(EvalError::Arity("string-ref requires 2 arguments".into()));
            }
            let s = match &vals[0] {
                Value::String(s, _) => s,
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
            Ok(Value::Boolean(deep_equal(&vals[0], &vals[1])))
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
                (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("string=?: expected strings".into())),
            }
        }
        "string<?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("string<?: expected strings".into())),
            }
        }
        "string-ci=?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
            match (&vals[0], &vals[1]) {
                (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
            }
        }
        "string-upcase" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
            match &vals[0] {
                Value::String(s, _) => Ok(Value::String(s.to_uppercase(), true)),
                _ => Err(EvalError::Type("string-upcase: expected string".into())),
            }
        }
        "string-downcase" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
            match &vals[0] {
                Value::String(s, _) => Ok(Value::String(s.to_lowercase(), true)),
                _ => Err(EvalError::Type("string-downcase: expected string".into())),
            }
        }
        // L14: eqv?
        "eqv?" => {
            if vals.len() != 2 { return Err(EvalError::Arity("eqv? requires 2 arguments".into())); }
            Ok(Value::Boolean(eqv(&vals[0], &vals[1])))
        }
        // L14: vector operations
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(vals.to_vec()))))
        }
        "make-vector" => {
            if vals.is_empty() || vals.len() > 2 {
                return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into()));
            }
            let len = expect_int(&vals[0])? as usize;
            let fill = if vals.len() == 2 { vals[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if vals.len() != 2 { return Err(EvalError::Arity("vector-ref requires 2 arguments".into())); }
            match &vals[0] {
                Value::Vector(v) => {
                    let idx = expect_int(&vals[1])? as usize;
                    let elems = v.borrow();
                    elems.get(idx).cloned().ok_or_else(|| EvalError::Type("vector-ref: index out of bounds".into()))
                }
                _ => Err(EvalError::Type("vector-ref: expected vector".into())),
            }
        }
        "vector-length" => {
            if vals.len() != 1 { return Err(EvalError::Arity("vector-length requires 1 argument".into())); }
            match &vals[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type("vector-length: expected vector".into())),
            }
        }
        "vector?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("vector? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Vector(_))))
        }
        "vector->list" => {
            if vals.len() != 1 { return Err(EvalError::Arity("vector->list requires 1 argument".into())); }
            match &vals[0] {
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
                _ => Err(EvalError::Type("vector->list: expected vector".into())),
            }
        }
        "list->vector" => {
            if vals.len() != 1 { return Err(EvalError::Arity("list->vector requires 1 argument".into())); }
            match &vals[0] {
                Value::List(elems) => Ok(Value::Vector(Rc::new(RefCell::new(elems.clone())))),
                _ => Err(EvalError::Type("list->vector: expected list".into())),
            }
        }
        "for-each" => {
            if vals.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into())); }
            let func = &vals[0];
            let lists: Vec<&Vec<Value>> = vals[1..].iter().map(|v| match v {
                Value::List(e) => Ok(e),
                _ => Err(EvalError::Type("for-each: expected list".into())),
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply(func, &args, out)?;
            }
            Ok(Value::Void)
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
    let current = env.borrow().get(&name)?;
    match current {
        Value::String(s, mutable) => {
            if !mutable {
                return Err(EvalError::Type("string-set!: strings are immutable".into()));
            }
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of bounds".into()));
            }
            chars[idx] = ch;
            let new_s: String = chars.into_iter().collect();
            Env::set_existing(env, &name, Value::String(new_s, true))?;
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string".into())),
    }
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
        _ => Err(EvalError::Type(format!("expected integer, got {}", v))),
    }
}

/// Extract a number as (numerator, denominator) for exact, or f64 for inexact
enum Num {
    Exact(i64, i64), // num, den
    Inexact(f64),
}

fn to_num(v: &Value) -> Result<Num, EvalError> {
    match v {
        Value::Integer(n) => Ok(Num::Exact(*n, 1)),
        Value::Rational(n, d) => Ok(Num::Exact(*n, *d)),
        Value::Float(f) => Ok(Num::Inexact(*f)),
        _ => Err(EvalError::Type(format!("expected number, got {}", v))),
    }
}

fn num_to_value(n: Num) -> Value {
    match n {
        Num::Exact(num, den) => Value::make_rational(num, den),
        Num::Inexact(f) => Value::Float(f),
    }
}

fn num_add(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bd + bn * ad, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a + b),
        (Num::Exact(n, d), Num::Inexact(f)) | (Num::Inexact(f), Num::Exact(n, d)) => {
            Num::Inexact(n as f64 / d as f64 + f)
        }
    }
}

fn num_sub(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bd - bn * ad, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a - b),
        (Num::Exact(n, d), Num::Inexact(f)) => Num::Inexact(n as f64 / d as f64 - f),
        (Num::Inexact(f), Num::Exact(n, d)) => Num::Inexact(f - n as f64 / d as f64),
    }
}

fn num_mul(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bn, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a * b),
        (Num::Exact(n, d), Num::Inexact(f)) | (Num::Inexact(f), Num::Exact(n, d)) => {
            Num::Inexact(n as f64 / d as f64 * f)
        }
    }
}

fn num_div(a: Num, b: Num) -> Result<Num, EvalError> {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => {
            if bn == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Num::Exact(an * bd, ad * bn))
        }
        (Num::Inexact(a), Num::Inexact(b)) => {
            if b == 0.0 { return Err(EvalError::DivisionByZero); }
            Ok(Num::Inexact(a / b))
        }
        (Num::Exact(n, d), Num::Inexact(f)) => {
            if f == 0.0 { return Err(EvalError::DivisionByZero); }
            Ok(Num::Inexact(n as f64 / d as f64 / f))
        }
        (Num::Inexact(f), Num::Exact(n, d)) => {
            if n == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Num::Inexact(f / (n as f64 / d as f64)))
        }
    }
}

fn num_to_f64(n: &Num) -> f64 {
    match n {
        Num::Exact(num, den) => *num as f64 / *den as f64,
        Num::Inexact(f) => *f,
    }
}

fn num_neg(a: Num) -> Num {
    match a {
        Num::Exact(n, d) => Num::Exact(-n, d),
        Num::Inexact(f) => Num::Inexact(-f),
    }
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 {
        return (0, 1);
    }
    // Use the fact that f64 has limited precision - multiply by power of 2
    // Simple approach: scale to integer
    let sign = if f < 0.0 { -1i64 } else { 1 };
    let f = f.abs();
    // Find a denominator that makes this exact
    // Try powers of 10 first for common decimal fractions
    let mut den = 1i64;
    let mut approx = f;
    for _ in 0..15 {
        if (approx - approx.round()).abs() < 1e-10 {
            return (sign * approx.round() as i64, den);
        }
        den *= 10;
        approx = f * den as f64;
    }
    // Fallback
    let den = 1_000_000_000i64;
    let num = (f * den as f64).round() as i64;
    (sign * num, den)
}

// --- Record types (L12) ---

fn eval_define_record_type(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()));
    }
    let type_id = crate::scheme::value::next_record_type_id();

    // Parse constructor: (ctor-name field1 field2 ...)
    let ctor_elems = match &args[1] {
        Value::List(e) => e,
        _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into())),
    };
    if ctor_elems.is_empty() {
        return Err(EvalError::Parse("define-record-type: empty constructor".into()));
    }
    let ctor_name = match &ctor_elems[0] {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected constructor name".into())),
    };
    let ctor_fields: Vec<String> = ctor_elems[1..].iter().map(|v| match v {
        Value::Symbol(s) => Ok(s.clone()),
        _ => Err(EvalError::Type("define-record-type: expected field name".into())),
    }).collect::<Result<_, _>>()?;

    // Parse predicate
    let pred_name = match &args[2] {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into())),
    };

    // Define constructor
    env.borrow_mut().set(ctor_name, Value::RecordProc {
        type_id,
        kind: crate::scheme::value::RecordProcKind::Constructor { field_names: ctor_fields.clone() },
    });

    // Define predicate
    env.borrow_mut().set(pred_name, Value::RecordProc {
        type_id,
        kind: crate::scheme::value::RecordProcKind::Predicate,
    });

    // Parse field specs: (field-name accessor-name)
    for field_spec in &args[3..] {
        let parts = match field_spec {
            Value::List(e) => e,
            _ => return Err(EvalError::Type("define-record-type: expected field spec".into())),
        };
        if parts.len() < 2 {
            return Err(EvalError::Arity("define-record-type: field spec needs name and accessor".into()));
        }
        let field_name = match &parts[0] {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type("define-record-type: expected field name".into())),
        };
        let accessor_name = match &parts[1] {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type("define-record-type: expected accessor name".into())),
        };
        let field_index = ctor_fields.iter().position(|f| f == &field_name)
            .ok_or_else(|| EvalError::Type(format!("define-record-type: unknown field {}", field_name)))?;
        env.borrow_mut().set(accessor_name, Value::RecordProc {
            type_id,
            kind: crate::scheme::value::RecordProcKind::Accessor { field_index },
        });
    }

    Ok(Value::Void)
}

// --- Macro support (L10) ---

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Value),
    Ellipsis(Vec<Value>),
}

fn eval_define_syntax(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("define-syntax requires 2 arguments".into()));
    }
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
    };
    let transformer = eval_syntax_rules(&args[1], env)?;
    env.borrow_mut().set(name, transformer);
    let _ = out; // unused but kept for consistency
    Ok(Value::Void)
}

fn eval_syntax_rules(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let elems = match expr {
        Value::List(e) => e,
        _ => return Err(EvalError::Type("syntax-rules: expected list".into())),
    };
    if elems.len() < 3 {
        return Err(EvalError::Arity("syntax-rules requires literals and rules".into()));
    }
    if !matches!(&elems[0], Value::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Type("expected syntax-rules".into()));
    }
    let literals = match &elems[1] {
        Value::List(lits) => {
            let mut result = Vec::new();
            for lit in lits {
                match lit {
                    Value::Symbol(s) => result.push(s.clone()),
                    _ => return Err(EvalError::Type("syntax-rules: literals must be symbols".into())),
                }
            }
            result
        }
        _ => return Err(EvalError::Type("syntax-rules: expected literals list".into())),
    };
    let mut rules = Vec::new();
    for rule in &elems[2..] {
        match rule {
            Value::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(EvalError::Type("syntax-rules: each rule must be (pattern template)".into())),
        }
    }
    Ok(Value::SyntaxRules {
        literals,
        rules,
        def_env: Rc::clone(env),
    })
}

fn expand_macro(
    literals: &[String],
    rules: &[(Value, Value)],
    def_env: &Rc<RefCell<Env>>,
    input: &[Value],
) -> Result<Value, EvalError> {
    let input_list = Value::List(input.to_vec());
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if match_syntax_rule(pattern, &input_list, literals, &mut bindings) {
            return instantiate_template(template, &bindings, def_env);
        }
    }
    Err(EvalError::Type("no matching syntax-rules pattern".into()))
}

fn match_syntax_rule(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match (pattern, input) {
        (Value::List(pat_elems), Value::List(inp_elems)) => {
            if pat_elems.is_empty() {
                return inp_elems.is_empty();
            }
            // Skip the macro name (first element of pattern)
            match_elements(&pat_elems[1..], &inp_elems[1..], literals, bindings)
        }
        _ => false,
    }
}

fn match_elements(
    patterns: &[Value],
    inputs: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;

    while pi < patterns.len() {
        let has_ellipsis = pi + 1 < patterns.len()
            && matches!(&patterns[pi + 1], Value::Symbol(s) if s == "...");

        if has_ellipsis {
            let pat = &patterns[pi];
            let mut matches = Vec::new();
            // Ellipsis greedily matches remaining elements (minus what's needed for remaining patterns)
            let remaining_fixed = count_fixed_patterns(&patterns[pi + 2..]);
            let available = if inputs.len() >= ii + remaining_fixed {
                inputs.len() - remaining_fixed
            } else {
                ii
            };
            while ii < available {
                let mut sub_bindings = HashMap::new();
                if match_single(pat, &inputs[ii], literals, &mut sub_bindings) {
                    matches.push(inputs[ii].clone());
                    ii += 1;
                } else {
                    break;
                }
            }
            if let Value::Symbol(s) = pat {
                if !literals.contains(s) {
                    bindings.insert(s.clone(), MacroBinding::Ellipsis(matches));
                }
            }
            pi += 2; // skip pattern and ellipsis
        } else {
            if ii >= inputs.len() {
                return false;
            }
            if !match_single(&patterns[pi], &inputs[ii], literals, bindings) {
                return false;
            }
            pi += 1;
            ii += 1;
        }
    }
    ii == inputs.len()
}

fn count_fixed_patterns(patterns: &[Value]) -> usize {
    let mut count = 0;
    let mut i = 0;
    while i < patterns.len() {
        let has_ellipsis = i + 1 < patterns.len()
            && matches!(&patterns[i + 1], Value::Symbol(s) if s == "...");
        if has_ellipsis {
            i += 2;
        } else {
            count += 1;
            i += 1;
        }
    }
    count
}

fn match_single(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match pattern {
        Value::Symbol(s) if s == "_" => true,
        Value::Symbol(s) if literals.contains(s) => {
            matches!(input, Value::Symbol(t) if t == s)
        }
        Value::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        Value::List(pat_elems) => {
            if let Value::List(inp_elems) = input {
                match_elements(pat_elems, inp_elems, literals, bindings)
            } else {
                false
            }
        }
        _ => pattern == input,
    }
}

fn instantiate_template(
    template: &Value,
    bindings: &HashMap<String, MacroBinding>,
    def_env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(v) => Ok(v.clone()),
                    MacroBinding::Ellipsis(_) => Ok(template.clone()),
                }
            } else {
                // Hygiene: for free variables, check definition-site env
                // Only inline self-evaluating values (Integer, Boolean, String, Char)
                if let Ok(val) = def_env.borrow().get(s) {
                    match &val {
                        Value::Integer(_) | Value::Boolean(_)
                        | Value::String(..) | Value::Char(_) => Ok(val),
                        _ => Ok(template.clone()),
                    }
                } else {
                    Ok(template.clone())
                }
            }
        }
        Value::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                let has_ellipsis = i + 1 < elems.len()
                    && matches!(&elems[i + 1], Value::Symbol(s) if s == "...");

                if has_ellipsis {
                    let ellipsis_vars = collect_ellipsis_vars(&elems[i], bindings);
                    if let Some(var_name) = ellipsis_vars.first() {
                        if let Some(MacroBinding::Ellipsis(vals)) = bindings.get(var_name) {
                            for val in vals {
                                let mut local_bindings = bindings.clone();
                                // Override all ellipsis vars for this iteration
                                local_bindings.insert(var_name.clone(), MacroBinding::Single(val.clone()));
                                result.push(instantiate_template(&elems[i], &local_bindings, def_env)?);
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(instantiate_template(&elems[i], bindings, def_env)?);
                    i += 1;
                }
            }
            Ok(Value::List(result))
        }
        _ => Ok(template.clone()),
    }
}

fn collect_ellipsis_vars(template: &Value, bindings: &HashMap<String, MacroBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    match template {
        Value::Symbol(s) => {
            if let Some(MacroBinding::Ellipsis(_)) = bindings.get(s) {
                vars.push(s.clone());
            }
        }
        Value::List(elems) => {
            for e in elems {
                vars.extend(collect_ellipsis_vars(e, bindings));
            }
        }
        _ => {}
    }
    vars
}

fn eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(a), Value::List(b)) if a.is_empty() && b.is_empty() => true,
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        _ => false,
    }
}

fn deep_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::String(x, _), Value::String(y, _)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| deep_equal(x, y))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => deep_equal(a1, b1) && deep_equal(a2, b2),
        (Value::Vector(x), Value::Vector(y)) => {
            let xv = x.borrow();
            let yv = y.borrow();
            xv.len() == yv.len() && xv.iter().zip(yv.iter()).all(|(a, b)| deep_equal(a, b))
        }
        _ => false,
    }
}

fn eval_vector_set(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("vector-set! requires 3 arguments".into()));
    }
    let vec_val = eval(&args[0], env, out)?;
    let idx = expect_int(&eval(&args[1], env, out)?)? as usize;
    let val = eval(&args[2], env, out)?;
    match &vec_val {
        Value::Vector(v) => {
            let mut elems = v.borrow_mut();
            if idx >= elems.len() {
                return Err(EvalError::Type("vector-set!: index out of bounds".into()));
            }
            elems[idx] = val;
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector".into())),
    }
}

fn eval_letrec(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings_list = match &args[0] {
        Value::List(b) => b,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let local_env = Env::with_parent(env);
    // First, bind all names to Void
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for binding in bindings_list {
        match binding {
            Value::List(pair) if pair.len() == 2 => {
                if let Value::Symbol(var) = &pair[0] {
                    names.push(var.clone());
                    init_exprs.push(pair[1].clone());
                    local_env.borrow_mut().set(var.clone(), Value::Void);
                } else {
                    return Err(EvalError::Type("letrec: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    // Evaluate all inits in the local env, then assign
    let vals: Vec<Value> = init_exprs.iter()
        .map(|e| eval(e, &local_env, out))
        .collect::<Result<_, _>>()?;
    for (name, val) in names.iter().zip(vals) {
        local_env.borrow_mut().set(name.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_letrec_star(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings_list = match &args[0] {
        Value::List(b) => b,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let local_env = Env::with_parent(env);
    for binding in bindings_list {
        match binding {
            Value::List(pair) if pair.len() == 2 => {
                if let Value::Symbol(var) = &pair[0] {
                    let val = eval(&pair[1], &local_env, out)?;
                    local_env.borrow_mut().set(var.clone(), val);
                } else {
                    return Err(EvalError::Type("letrec*: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_let_star(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bindings_list = match &args[0] {
        Value::List(b) => b,
        _ => return Err(EvalError::Type("let*: expected bindings list".into())),
    };
    let local_env = Env::with_parent(env);
    for binding in bindings_list {
        match binding {
            Value::List(pair) if pair.len() == 2 => {
                if let Value::Symbol(var) = &pair[0] {
                    let val = eval(&pair[1], &local_env, out)?;
                    local_env.borrow_mut().set(var.clone(), val);
                } else {
                    return Err(EvalError::Type("let*: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("let*: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_case(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires key and clauses".into()));
    }
    let key = eval(&args[0], env, out)?;
    for clause in &args[1..] {
        let parts = match clause {
            Value::List(p) if !p.is_empty() => p,
            _ => return Err(EvalError::Type("case: invalid clause".into())),
        };
        // Check for else clause
        if let Value::Symbol(s) = &parts[0] {
            if s == "else" {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env, out)?;
                }
                return Ok(result);
            }
        }
        // Datums list
        let datums = match &parts[0] {
            Value::List(d) => d,
            _ => return Err(EvalError::Type("case: expected datum list".into())),
        };
        for datum in datums {
            if eqv(&key, datum) {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env, out)?;
                }
                return Ok(result);
            }
        }
    }
    Ok(Value::Void)
}

fn eval_do(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable specs and test".into()));
    }
    let var_specs = match &args[0] {
        Value::List(v) => v,
        _ => return Err(EvalError::Type("do: expected variable spec list".into())),
    };
    let test_clause = match &args[1] {
        Value::List(t) if !t.is_empty() => t,
        _ => return Err(EvalError::Type("do: expected test clause".into())),
    };
    let body = &args[2..];

    // Parse variable specs
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Value>> = Vec::new();

    let loop_env = Env::with_parent(env);
    for spec in var_specs {
        let parts = match spec {
            Value::List(p) => p,
            _ => return Err(EvalError::Type("do: expected variable spec".into())),
        };
        if parts.len() < 2 || parts.len() > 3 {
            return Err(EvalError::Arity("do: variable spec needs (var init) or (var init step)".into()));
        }
        let name = match &parts[0] {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type("do: expected symbol for variable name".into())),
        };
        let init = eval(&parts[1], env, out)?;
        let step = if parts.len() == 3 { Some(parts[2].clone()) } else { None };
        loop_env.borrow_mut().set(name.clone(), init);
        var_names.push(name);
        step_exprs.push(step);
    }

    loop {
        // Test
        let test_result = eval(&test_clause[0], &loop_env, out)?;
        if test_result.is_truthy() {
            // Evaluate test exprs and return last
            let mut result = Value::Void;
            for expr in &test_clause[1..] {
                result = eval(expr, &loop_env, out)?;
            }
            return Ok(result);
        }
        // Execute body
        for expr in body {
            eval(expr, &loop_env, out)?;
        }
        // Parallel step: evaluate all steps using current values, then update
        let new_vals: Vec<Option<Value>> = step_exprs.iter()
            .map(|step| match step {
                Some(e) => eval(e, &loop_env, out).map(Some),
                None => Ok(None),
            })
            .collect::<Result<_, _>>()?;
        for (name, val) in var_names.iter().zip(new_vals) {
            if let Some(v) = val {
                loop_env.borrow_mut().set(name.clone(), v);
            }
        }
    }
}

fn eval_when(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("when requires test and body".into()));
    }
    let test = eval(&args[0], env, out)?;
    if test.is_truthy() {
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, env, out)?;
        }
        Ok(result)
    } else {
        Ok(Value::Void)
    }
}

fn compare_nums(vals: &[Value], pred: fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if vals.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let mut prev = num_to_f64(&to_num(&vals[0])?);
    for v in &vals[1..] {
        let curr = num_to_f64(&to_num(v)?);
        if !pred(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}
