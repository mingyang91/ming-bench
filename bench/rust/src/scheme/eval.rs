use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::value::{Value, ValueKind, Pos, NumVal, make_rational_kind, next_record_type_id};
use crate::scheme::macros;
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
        ValueKind::Integer(_) | ValueKind::Rational(_, _) | ValueKind::Float(_) | ValueKind::Boolean(_) | ValueKind::Str(_) | ValueKind::Char(_) => Ok(expr.clone()),
        ValueKind::Lambda { .. } | ValueKind::CaseLambda { .. } | ValueKind::SyntaxRules { .. } | ValueKind::Record { .. } | ValueKind::RecordConstructor { .. } | ValueKind::RecordPredicate { .. } | ValueKind::RecordAccessor { .. } => Ok(expr.clone()),
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
                    "define-syntax" => return eval_define_syntax(&elems[1..], expr.pos, env),
                    "define-record-type" => return eval_define_record_type(&elems[1..], expr.pos, env),
                    "case-lambda" => return eval_case_lambda(&elems[1..], expr.pos, env),
                    _ => {}
                }
                // Check if symbol is bound to a macro
                if let Some(val) = env.get(s) {
                    if let ValueKind::SyntaxRules { ref literals, ref rules, ref def_env } = val.kind {
                        let expanded = macros::expand_syntax_rules(rules, literals, elems, def_env, env)?;
                        return eval(&expanded, env);
                    }
                }
            }
            let func = eval(&elems[0], env)?;
            // Check if evaluated head is a macro (e.g. via gensym rename)
            if let ValueKind::SyntaxRules { ref literals, ref rules, ref def_env } = func.kind {
                let expanded = macros::expand_syntax_rules(rules, literals, elems, def_env, env)?;
                return eval(&expanded, env);
            }
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

fn eval_define_syntax(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Syntax(format!("define-syntax requires 2 arguments at {}", fmt_pos(pos))));
    }
    let name = match &args[0].kind {
        ValueKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Syntax(format!("define-syntax: expected symbol at {}", fmt_pos(pos)))),
    };
    // args[1] should be (syntax-rules (literals...) (pattern template) ...)
    let sr = match &args[1].kind {
        ValueKind::List(elems) => elems,
        _ => return Err(EvalError::Syntax(format!("define-syntax: expected syntax-rules at {}", fmt_pos(pos)))),
    };
    if sr.is_empty() || !matches!(&sr[0].kind, ValueKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Syntax(format!("define-syntax: expected syntax-rules at {}", fmt_pos(pos))));
    }
    if sr.len() < 2 {
        return Err(EvalError::Syntax(format!("syntax-rules requires literals and rules at {}", fmt_pos(pos))));
    }
    let literals = match &sr[1].kind {
        ValueKind::List(lits) => {
            lits.iter().map(|l| match &l.kind {
                ValueKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Syntax(format!("syntax-rules: literal must be symbol at {}", fmt_pos(l.pos)))),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Syntax(format!("syntax-rules: expected literal list at {}", fmt_pos(sr[1].pos)))),
    };
    let mut rules = Vec::new();
    for rule in &sr[2..] {
        match &rule.kind {
            ValueKind::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(EvalError::Syntax(format!("syntax-rules: bad rule at {}", fmt_pos(rule.pos)))),
        }
    }
    env.set(name, Value::unpos(ValueKind::SyntaxRules {
        literals,
        rules,
        def_env: Rc::clone(env),
    }));
    Ok(Value::unpos(ValueKind::Void))
}

fn eval_define_record_type(args: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field...) predicate (field accessor)...)
    if args.len() < 3 {
        return Err(EvalError::Syntax(format!("define-record-type requires at least 3 arguments at {}", fmt_pos(pos))));
    }
    let type_name = match &args[0].kind {
        ValueKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Syntax(format!("define-record-type: expected type name at {}", fmt_pos(pos)))),
    };
    let type_id = next_record_type_id();

    // Parse constructor: (constructor-name field-name ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        ValueKind::List(elems) if !elems.is_empty() => {
            let cname = match &elems[0].kind {
                ValueKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Syntax(format!("define-record-type: expected constructor name at {}", fmt_pos(pos)))),
            };
            let fields: Vec<String> = elems[1..].iter().map(|e| match &e.kind {
                ValueKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Syntax(format!("define-record-type: expected field name at {}", fmt_pos(e.pos)))),
            }).collect::<Result<Vec<_>, _>>()?;
            (cname, fields)
        }
        _ => return Err(EvalError::Syntax(format!("define-record-type: expected constructor spec at {}", fmt_pos(pos)))),
    };

    // Parse predicate
    let predicate_name = match &args[2].kind {
        ValueKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Syntax(format!("define-record-type: expected predicate name at {}", fmt_pos(pos)))),
    };

    // Parse field accessors: (field-name accessor-name)
    for field_spec in &args[3..] {
        match &field_spec.kind {
            ValueKind::List(elems) if elems.len() >= 2 => {
                let field_name = match &elems[0].kind {
                    ValueKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Syntax(format!("define-record-type: expected field name at {}", fmt_pos(field_spec.pos)))),
                };
                let accessor_name = match &elems[1].kind {
                    ValueKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Syntax(format!("define-record-type: expected accessor name at {}", fmt_pos(field_spec.pos)))),
                };
                env.set(accessor_name, Value::unpos(ValueKind::RecordAccessor {
                    type_id,
                    type_name: type_name.clone(),
                    field_name,
                }));
            }
            _ => return Err(EvalError::Syntax(format!("define-record-type: expected field spec at {}", fmt_pos(field_spec.pos)))),
        }
    }

    // Define constructor
    env.set(constructor_name, Value::unpos(ValueKind::RecordConstructor {
        type_id,
        type_name: type_name.clone(),
        field_names: constructor_fields,
    }));

    // Define predicate
    env.set(predicate_name, Value::unpos(ValueKind::RecordPredicate { type_id }));

    Ok(Value::unpos(ValueKind::Void))
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

fn eval_case_lambda(clauses: &[Value], pos: Pos, env: &Rc<Env>) -> Result<Value, EvalError> {
    let mut parsed_clauses = Vec::new();
    for clause in clauses {
        match &clause.kind {
            ValueKind::List(elems) if elems.len() >= 2 => {
                let (params, rest_param) = match &elems[0].kind {
                    ValueKind::List(param_elems) => parse_params(param_elems)?,
                    ValueKind::Symbol(s) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Syntax(format!("case-lambda: expected parameter list at {}", fmt_pos(elems[0].pos)))),
                };
                let body = elems[1..].to_vec();
                parsed_clauses.push((params, rest_param, body, Rc::clone(env)));
            }
            _ => return Err(EvalError::Syntax(format!("case-lambda: bad clause at {}", fmt_pos(clause.pos)))),
        }
    }
    Ok(Value::new(ValueKind::CaseLambda { clauses: parsed_clauses }, pos))
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
        ValueKind::CaseLambda { clauses } => {
            for (params, rest_param, body, closure_env) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = Env::new(Some(Rc::clone(closure_env)));
                    for (param, arg) in params.iter().zip(args.iter()) {
                        local_env.set(param.clone(), arg.clone());
                    }
                    if let Some(ref rest) = rest_param {
                        let rest_args = args[params.len()..].to_vec();
                        local_env.set(rest.clone(), Value::unpos(ValueKind::List(rest_args)));
                    }
                    let mut result = Value::unpos(ValueKind::Void);
                    for expr in body {
                        result = eval(expr, &local_env)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments at {}", args.len(), fmt_pos(call_pos)
            )))
        }
        ValueKind::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{} constructor expects {} arguments, got {} at {}",
                    type_name, field_names.len(), args.len(), fmt_pos(call_pos)
                )));
            }
            let fields: Vec<(String, Value)> = field_names.iter().zip(args.iter())
                .map(|(name, val)| (name.clone(), val.clone()))
                .collect();
            Ok(Value::unpos(ValueKind::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields,
            }))
        }
        ValueKind::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "record predicate expects 1 argument, got {} at {}", args.len(), fmt_pos(call_pos)
                )));
            }
            let is_match = matches!(&args[0].kind, ValueKind::Record { type_id: tid, .. } if tid == type_id);
            Ok(Value::unpos(ValueKind::Boolean(is_match)))
        }
        ValueKind::RecordAccessor { type_id, type_name, field_name } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "record accessor expects 1 argument, got {} at {}", args.len(), fmt_pos(call_pos)
                )));
            }
            match &args[0].kind {
                ValueKind::Record { type_id: tid, fields, .. } if tid == type_id => {
                    fields.iter()
                        .find(|(name, _)| name == field_name)
                        .map(|(_, val)| val.clone())
                        .ok_or_else(|| EvalError::Runtime(format!(
                            "record {} has no field {} at {}", type_name, field_name, fmt_pos(call_pos)
                        )))
                }
                _ => Err(EvalError::Type(format!(
                    "accessor {}: expected {} record at {}", field_name, type_name, fmt_pos(call_pos)
                ))),
            }
        }
        _ => Err(EvalError::Type(format!("not a procedure: {} at {}", func, fmt_pos(call_pos)))),
    }
}

fn numval_to_value(n: NumVal) -> Value {
    match n {
        NumVal::Int(i) => Value::unpos(ValueKind::Integer(i)),
        NumVal::Rat(num, den) => Value::unpos(make_rational_kind(num, den)),
        NumVal::Flt(f) => Value::unpos(ValueKind::Float(f)),
    }
}

fn num_add(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Flt(x), other) | (other, NumVal::Flt(x)) => NumVal::Flt(x + other.to_f64()),
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x + y),
        (NumVal::Int(x), NumVal::Rat(n, d)) | (NumVal::Rat(n, d), NumVal::Int(x)) => {
            NumVal::Rat(n + x * d, d)
        }
        (NumVal::Rat(n1, d1), NumVal::Rat(n2, d2)) => {
            NumVal::Rat(n1 * d2 + n2 * d1, d1 * d2)
        }
    }
}

fn num_sub(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Flt(x), y) => NumVal::Flt(x - y.to_f64()),
        (x, NumVal::Flt(y)) => NumVal::Flt(x.to_f64() - y),
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x - y),
        (NumVal::Int(x), NumVal::Rat(n, d)) => NumVal::Rat(x * d - n, d),
        (NumVal::Rat(n, d), NumVal::Int(x)) => NumVal::Rat(n - x * d, d),
        (NumVal::Rat(n1, d1), NumVal::Rat(n2, d2)) => {
            NumVal::Rat(n1 * d2 - n2 * d1, d1 * d2)
        }
    }
}

fn num_mul(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Flt(x), other) | (other, NumVal::Flt(x)) => NumVal::Flt(x * other.to_f64()),
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x * y),
        (NumVal::Int(x), NumVal::Rat(n, d)) | (NumVal::Rat(n, d), NumVal::Int(x)) => {
            NumVal::Rat(n * x, d)
        }
        (NumVal::Rat(n1, d1), NumVal::Rat(n2, d2)) => {
            NumVal::Rat(n1 * n2, d1 * d2)
        }
    }
}

fn num_div(a: NumVal, b: NumVal) -> Option<NumVal> {
    match (a, b) {
        (NumVal::Flt(x), y) => Some(NumVal::Flt(x / y.to_f64())),
        (x, NumVal::Flt(y)) => {
            if y == 0.0 { return None; }
            Some(NumVal::Flt(x.to_f64() / y))
        }
        (NumVal::Int(x), NumVal::Int(y)) => {
            if y == 0 { return None; }
            Some(NumVal::Rat(x, y))
        }
        (NumVal::Int(x), NumVal::Rat(n, d)) => {
            if n == 0 { return None; }
            Some(NumVal::Rat(x * d, n))
        }
        (NumVal::Rat(n, d), NumVal::Int(y)) => {
            if y == 0 { return None; }
            Some(NumVal::Rat(n, d * y))
        }
        (NumVal::Rat(n1, d1), NumVal::Rat(n2, d2)) => {
            if n2 == 0 { return None; }
            Some(NumVal::Rat(n1 * d2, d1 * n2))
        }
    }
}

/// Simplify a NumVal (reduce rationals)
fn num_simplify(n: NumVal) -> NumVal {
    match n {
        NumVal::Rat(num, den) => {
            let (num, den) = if den < 0 { (-num, -den) } else { (num, den) };
            let g = crate::scheme::value::gcd(num.abs(), den);
            let (num, den) = (num / g, den / g);
            if den == 1 { NumVal::Int(num) } else { NumVal::Rat(num, den) }
        }
        other => other,
    }
}

fn num_cmp(a: NumVal, b: NumVal) -> f64 {
    // Returns a - b as f64 for comparison
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => (x - y) as f64,
        (NumVal::Flt(x), other) => x - other.to_f64(),
        (other, NumVal::Flt(y)) => other.to_f64() - y,
        (NumVal::Rat(n1, d1), NumVal::Rat(n2, d2)) => {
            (n1 * d2 - n2 * d1) as f64
        }
        (NumVal::Int(x), NumVal::Rat(n, d)) => (x * d - n) as f64,
        (NumVal::Rat(n, d), NumVal::Int(y)) => (n - y * d) as f64,
    }
}

fn apply_builtin(name: &str, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut acc = NumVal::Int(0);
            for a in args {
                let n = a.as_num().ok_or_else(|| EvalError::Type(format!("+ expects numbers at {}", fmt_pos(call_pos))))?;
                acc = num_add(acc, n);
            }
            Ok(numval_to_value(num_simplify(acc)))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("- requires at least 1 argument at {}", fmt_pos(call_pos))));
            }
            let first = args[0].as_num().ok_or_else(|| EvalError::Type(format!("- expects numbers at {}", fmt_pos(call_pos))))?;
            if args.len() == 1 {
                let neg = match first {
                    NumVal::Int(n) => NumVal::Int(-n),
                    NumVal::Rat(n, d) => NumVal::Rat(-n, d),
                    NumVal::Flt(f) => NumVal::Flt(-f),
                };
                Ok(numval_to_value(neg))
            } else {
                let mut acc = first;
                for a in &args[1..] {
                    let n = a.as_num().ok_or_else(|| EvalError::Type(format!("- expects numbers at {}", fmt_pos(call_pos))))?;
                    acc = num_sub(acc, n);
                }
                Ok(numval_to_value(num_simplify(acc)))
            }
        }
        "*" => {
            let mut acc = NumVal::Int(1);
            for a in args {
                let n = a.as_num().ok_or_else(|| EvalError::Type(format!("* expects numbers at {}", fmt_pos(call_pos))))?;
                acc = num_mul(acc, n);
            }
            Ok(numval_to_value(num_simplify(acc)))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("/ requires at least 1 argument at {}", fmt_pos(call_pos))));
            }
            let first = args[0].as_num().ok_or_else(|| EvalError::Type(format!("/ expects numbers at {}", fmt_pos(call_pos))))?;
            if args.len() == 1 {
                let r = num_div(NumVal::Int(1), first)
                    .ok_or_else(|| EvalError::Runtime(format!("division by zero at {}", fmt_pos(call_pos))))?;
                Ok(numval_to_value(num_simplify(r)))
            } else {
                let mut acc = first;
                for a in &args[1..] {
                    let n = a.as_num().ok_or_else(|| EvalError::Type(format!("/ expects numbers at {}", fmt_pos(call_pos))))?;
                    acc = num_div(acc, n)
                        .ok_or_else(|| EvalError::Runtime(format!("division by zero at {}", fmt_pos(call_pos))))?;
                }
                Ok(numval_to_value(num_simplify(acc)))
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
            Ok(Value::unpos(ValueKind::Boolean(args[0].as_num().is_some())))
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
        "procedure?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("procedure? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind,
                ValueKind::Lambda { .. } | ValueKind::CaseLambda { .. } |
                ValueKind::RecordConstructor { .. } | ValueKind::RecordPredicate { .. } | ValueKind::RecordAccessor { .. }
            ))))
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
            let s = match &args[0].kind {
                ValueKind::Integer(n) => n.to_string(),
                ValueKind::Rational(n, d) => format!("{}/{}", n, d),
                ValueKind::Float(f) => format!("{}", f),
                _ => return Err(EvalError::Type(format!("number->string: not a number at {}", fmt_pos(call_pos)))),
            };
            Ok(Value::unpos(ValueKind::Str(s)))
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
            let n = args[0].as_num().ok_or_else(|| EvalError::Type(format!("zero?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n.to_f64() == 0.0)))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("positive? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_num().ok_or_else(|| EvalError::Type(format!("positive?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n.to_f64() > 0.0)))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("negative? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_num().ok_or_else(|| EvalError::Type(format!("negative?: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Boolean(n.to_f64() < 0.0)))
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
            let is_int = match &args[0].kind {
                ValueKind::Integer(_) => true,
                ValueKind::Rational(_, _) => false, // simplified rationals with den=1 become Integer
                ValueKind::Float(f) => f.fract() == 0.0,
                _ => false,
            };
            Ok(Value::unpos(ValueKind::Boolean(is_int)))
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
        // L11: Exact arithmetic & rationals
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("exact? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let is_exact = matches!(&args[0].kind, ValueKind::Integer(_) | ValueKind::Rational(_, _));
            Ok(Value::unpos(ValueKind::Boolean(is_exact)))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("inexact? requires 1 argument at {}", fmt_pos(call_pos)))); }
            Ok(Value::unpos(ValueKind::Boolean(matches!(&args[0].kind, ValueKind::Float(_)))))
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("rational? requires 1 argument at {}", fmt_pos(call_pos)))); }
            let is_rat = matches!(&args[0].kind, ValueKind::Integer(_) | ValueKind::Rational(_, _));
            Ok(Value::unpos(ValueKind::Boolean(is_rat)))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("exact->inexact requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_num().ok_or_else(|| EvalError::Type(format!("exact->inexact: not a number at {}", fmt_pos(call_pos))))?;
            Ok(Value::unpos(ValueKind::Float(n.to_f64())))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("inexact->exact requires 1 argument at {}", fmt_pos(call_pos)))); }
            let n = args[0].as_num().ok_or_else(|| EvalError::Type(format!("inexact->exact: not a number at {}", fmt_pos(call_pos))))?;
            match n {
                NumVal::Int(i) => Ok(Value::unpos(ValueKind::Integer(i))),
                NumVal::Rat(num, den) => Ok(Value::unpos(make_rational_kind(num, den))),
                NumVal::Flt(f) => {
                    // Convert float to exact rational via continued fraction / simple approach
                    // For 0.5 → 1/2, etc.
                    if f.fract() == 0.0 {
                        Ok(Value::unpos(ValueKind::Integer(f as i64)))
                    } else {
                        // Use the fact that f = n/d, find simplest rational
                        // Multiply by power of 10 to clear decimals, then simplify
                        let mut num = f;
                        let mut den = 1i64;
                        // Scale up to integer
                        while (num * den as f64).fract().abs() > 1e-10 && den < 1_000_000_000 {
                            den *= 10;
                        }
                        let inum = (f * den as f64).round() as i64;
                        Ok(Value::unpos(make_rational_kind(inum, den)))
                    }
                }
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("numerator requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Integer(n) => Ok(Value::unpos(ValueKind::Integer(*n))),
                ValueKind::Rational(n, _) => Ok(Value::unpos(ValueKind::Integer(*n))),
                _ => Err(EvalError::Type(format!("numerator: not a rational at {}", fmt_pos(call_pos)))),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("denominator requires 1 argument at {}", fmt_pos(call_pos)))); }
            match &args[0].kind {
                ValueKind::Integer(_) => Ok(Value::unpos(ValueKind::Integer(1))),
                ValueKind::Rational(_, d) => Ok(Value::unpos(ValueKind::Integer(*d))),
                _ => Err(EvalError::Type(format!("denominator: not a rational at {}", fmt_pos(call_pos)))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{} at {}", name, fmt_pos(call_pos)))),
    }
}

fn cmp_op(args: &[Value], op: fn(f64, f64) -> bool, call_pos: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("comparison requires at least 2 arguments at {}", fmt_pos(call_pos))));
    }
    let nums: Vec<NumVal> = args.iter()
        .map(|a| a.as_num().ok_or_else(|| EvalError::Type(format!("comparison expects numbers at {}", fmt_pos(call_pos)))))
        .collect::<Result<Vec<_>, _>>()?;
    for w in nums.windows(2) {
        let diff = num_cmp(w[0], w[1]);
        if !op(diff, 0.0) {
            return Ok(Value::unpos(ValueKind::Boolean(false)));
        }
    }
    Ok(Value::unpos(ValueKind::Boolean(true)))
}
