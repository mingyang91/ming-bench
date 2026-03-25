use std::rc::Rc;

use super::{
    ast_to_value, cek_eval_cond, eqv, vec_to_list, Ast, AstKind, CekState, Environment, Env,
    EvalError, Frame, Kont, Value,
};

/// Helper: set up body evaluation in the CEK machine
pub(crate) fn cek_eval_body(body: Vec<Ast>, env: Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if body.is_empty() {
        return Ok(CekState::Apply(Value::Void));
    }
    if body.len() > 1 {
        kont.push(Frame::Seq { remaining: body[1..].to_vec(), env: env.clone() });
    }
    Ok(CekState::Eval(body[0].clone(), env))
}

/// Helper: bind parameters in a new environment
pub(crate) fn bind_params(params: &[String], rest_param: &Option<String>, args: &[Value], env: &Env) -> Result<Env, EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!("expected at least {} arguments, got {}", params.len(), args.len())));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!("expected {} arguments, got {}", params.len(), args.len())));
    }
    let local_env = Environment::with_parent(env);
    for (p, a) in params.iter().zip(args.iter()) {
        local_env.borrow_mut().set(p.clone(), a.clone());
    }
    if let Some(rest) = rest_param {
        let rest_args = args[params.len()..].to_vec();
        local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
    }
    Ok(local_env)
}

pub(crate) fn cek_apply_frame(frame: Frame, val: Value, kont: &mut Kont, output: &mut String) -> Result<CekState, EvalError> {
    match frame {
        Frame::If { then_br, else_br, env } => {
            if val.is_truthy() {
                Ok(CekState::Eval(then_br, env))
            } else if let Some(else_br) = else_br {
                Ok(CekState::Eval(else_br, env))
            } else {
                Ok(CekState::Apply(Value::Void))
            }
        }
        Frame::Seq { remaining, env } => {
            if remaining.len() == 1 {
                Ok(CekState::Eval(remaining[0].clone(), env))
            } else {
                kont.push(Frame::Seq {
                    remaining: remaining[1..].to_vec(),
                    env: env.clone(),
                });
                Ok(CekState::Eval(remaining[0].clone(), env))
            }
        }
        Frame::Define { name, env } => {
            env.borrow_mut().set(name, val);
            Ok(CekState::Apply(Value::Void))
        }
        Frame::Set { name, env } => {
            if !env.borrow_mut().set_existing(&name, val) {
                return Err(EvalError::UnboundVariable(name));
            }
            Ok(CekState::Apply(Value::Void))
        }
        Frame::And { remaining, env } => {
            if !val.is_truthy() {
                Ok(CekState::Apply(val))
            } else if remaining.len() == 1 {
                Ok(CekState::Eval(remaining[0].clone(), env))
            } else {
                kont.push(Frame::And {
                    remaining: remaining[1..].to_vec(),
                    env: env.clone(),
                });
                Ok(CekState::Eval(remaining[0].clone(), env))
            }
        }
        Frame::Or { remaining, env } => {
            if val.is_truthy() {
                Ok(CekState::Apply(val))
            } else if remaining.len() == 1 {
                Ok(CekState::Eval(remaining[0].clone(), env))
            } else {
                kont.push(Frame::Or {
                    remaining: remaining[1..].to_vec(),
                    env: env.clone(),
                });
                Ok(CekState::Eval(remaining[0].clone(), env))
            }
        }
        Frame::EvalFunc { args, env } => {
            // Right-to-left evaluation: evaluate last arg first
            if args.is_empty() {
                cek_apply_func(val, Vec::new(), kont, output)
            } else {
                let n = args.len();
                kont.push(Frame::Args {
                    func: val,
                    done: Vec::new(),
                    remaining: args[..n-1].to_vec(),
                    env: env.clone(),
                });
                Ok(CekState::Eval(args[n-1].clone(), env))
            }
        }
        Frame::Args { func, mut done, remaining, env } => {
            done.push(val);
            if remaining.is_empty() {
                done.reverse(); // restore left-to-right order
                cek_apply_func(func, done, kont, output)
            } else {
                let n = remaining.len();
                kont.push(Frame::Args {
                    func,
                    done,
                    remaining: remaining[..n-1].to_vec(),
                    env: env.clone(),
                });
                Ok(CekState::Eval(remaining[n-1].clone(), env))
            }
        }
        Frame::CallCC => {
            // val is the procedure to call with the current continuation
            let cont = Value::Continuation(kont.clone());
            cek_apply_func(val, vec![cont], kont, output)
        }
        Frame::LetBind { name, remaining, mut values, body, eval_env } => {
            values.push((name, val));
            if remaining.is_empty() {
                let local_env = Environment::with_parent(&eval_env);
                for (n, v) in values {
                    local_env.borrow_mut().set(n, v);
                }
                cek_eval_body(body, local_env, kont)
            } else {
                let next = remaining[0].clone();
                kont.push(Frame::LetBind {
                    name: next.0,
                    remaining: remaining[1..].to_vec(),
                    values,
                    body,
                    eval_env: eval_env.clone(),
                });
                Ok(CekState::Eval(next.1, eval_env))
            }
        }
        Frame::LetStarBind { name, remaining, body, local_env } => {
            local_env.borrow_mut().set(name, val);
            if remaining.is_empty() {
                cek_eval_body(body, local_env, kont)
            } else {
                let next = remaining[0].clone();
                kont.push(Frame::LetStarBind {
                    name: next.0,
                    remaining: remaining[1..].to_vec(),
                    body,
                    local_env: local_env.clone(),
                });
                Ok(CekState::Eval(next.1, local_env))
            }
        }
        Frame::NamedLetBind { loop_name, param, remaining, mut values, body, eval_env } => {
            values.push((param, val));
            if remaining.is_empty() {
                let local_env = Environment::with_parent(&eval_env);
                let params: Vec<String> = values.iter().map(|(n, _)| n.clone()).collect();
                let lambda = Value::Lambda {
                    params,
                    rest_param: None,
                    body: body.clone(),
                    env: Rc::clone(&local_env),
                };
                local_env.borrow_mut().set(loop_name, lambda);
                for (n, v) in values {
                    local_env.borrow_mut().set(n, v);
                }
                cek_eval_body(body, local_env, kont)
            } else {
                let next = remaining[0].clone();
                kont.push(Frame::NamedLetBind {
                    loop_name,
                    param: next.0,
                    remaining: remaining[1..].to_vec(),
                    values,
                    body,
                    eval_env: eval_env.clone(),
                });
                Ok(CekState::Eval(next.1, eval_env))
            }
        }
        Frame::LetrecBind { name, remaining, body, local_env } => {
            local_env.borrow_mut().set(name, val);
            if remaining.is_empty() {
                cek_eval_body(body, local_env, kont)
            } else {
                let next = remaining[0].clone();
                kont.push(Frame::LetrecBind {
                    name: next.0,
                    remaining: remaining[1..].to_vec(),
                    body,
                    local_env: local_env.clone(),
                });
                Ok(CekState::Eval(next.1, local_env))
            }
        }
        Frame::LetrecStarBind { name, remaining, body, local_env } => {
            local_env.borrow_mut().set(name, val);
            if remaining.is_empty() {
                cek_eval_body(body, local_env, kont)
            } else {
                let next = remaining[0].clone();
                kont.push(Frame::LetrecStarBind {
                    name: next.0,
                    remaining: remaining[1..].to_vec(),
                    body,
                    local_env: local_env.clone(),
                });
                Ok(CekState::Eval(next.1, local_env))
            }
        }
        Frame::CondClause { body, remaining, env } => {
            if val.is_truthy() {
                if body.is_empty() {
                    Ok(CekState::Apply(val))
                } else {
                    cek_eval_body(body, env, kont)
                }
            } else {
                cek_eval_cond(&remaining, &env, kont)
            }
        }
        Frame::CaseKey { clauses, env } => {
            for clause in &clauses {
                let items = match &clause.kind {
                    AstKind::List(items) if items.len() >= 2 => items,
                    _ => return Err(EvalError::Type("case: invalid clause".into())),
                };
                if matches!(&items[0].kind, AstKind::Symbol(s) if s == "else") {
                    return cek_eval_body(items[1..].to_vec(), env, kont);
                }
                if let AstKind::List(datums) = &items[0].kind {
                    let matched = datums.iter().any(|d| eqv(&val, &ast_to_value(d)));
                    if matched {
                        return cek_eval_body(items[1..].to_vec(), env, kont);
                    }
                }
            }
            Ok(CekState::Apply(Value::Void))
        }
        Frame::StringSetIdx { var_name, char_expr, env } => {
            let idx = val.as_integer()? as usize;
            kont.push(Frame::StringSetChar { var_name, idx, env: env.clone() });
            Ok(CekState::Eval(char_expr, env))
        }
        Frame::StringSetChar { var_name, idx, env } => {
            let ch = match val {
                Value::Char(c) => c,
                _ => return Err(EvalError::Type("string-set!: expected char".into())),
            };
            let current = env.borrow().get(&var_name)
                .ok_or_else(|| EvalError::UnboundVariable(var_name.clone()))?;
            let s = match current {
                Value::Str(ref s) => s.clone(),
                _ => return Err(EvalError::Type("string-set!: expected string".into())),
            };
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of bounds".into()));
            }
            chars[idx] = ch;
            let new_s: String = chars.into_iter().collect();
            env.borrow_mut().set_existing(&var_name, Value::Str(new_s));
            Ok(CekState::Apply(Value::Void))
        }
    }
}

pub(crate) fn cek_apply_func(func: Value, args: Vec<Value>, kont: &mut Kont, output: &mut String) -> Result<CekState, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = bind_params(&params, &rest_param, &args, &env)?;
            if body.is_empty() {
                return Ok(CekState::Apply(Value::Void));
            }
            if body.len() > 1 {
                kont.push(Frame::Seq { remaining: body[1..].to_vec(), env: local_env.clone() });
            }
            Ok(CekState::Eval(body[0].clone(), local_env))
        }
        Value::CaseLambda { clauses, env } => {
            let matched = clauses.into_iter().find(|(params, rest_param, _)| {
                if rest_param.is_some() { args.len() >= params.len() } else { args.len() == params.len() }
            });
            if let Some((params, rest_param, body)) = matched {
                let local_env = bind_params(&params, &rest_param, &args, &env)?;
                if body.is_empty() {
                    return Ok(CekState::Apply(Value::Void));
                }
                if body.len() > 1 {
                    kont.push(Frame::Seq { remaining: body[1..].to_vec(), env: local_env.clone() });
                }
                Ok(CekState::Eval(body[0].clone(), local_env))
            } else {
                Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", args.len())))
            }
        }
        Value::Builtin(f) => {
            let result = f(&args, output)?;
            Ok(CekState::Apply(result))
        }
        Value::CallCC => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires exactly 1 argument".into()));
            }
            let proc = args.into_iter().next().expect("arity checked above");
            let cont = Value::Continuation(kont.clone());
            cek_apply_func(proc, vec![cont], kont, output)
        }
        Value::Continuation(saved_kont) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires exactly 1 argument".into()));
            }
            *kont = saved_kont;
            Ok(CekState::Apply(args.into_iter().next().expect("arity checked above")))
        }
        Value::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{} constructor expects {} arguments, got {}", type_name, field_names.len(), args.len()
                )));
            }
            let fields: Vec<(String, Value)> = field_names.iter().zip(args.iter())
                .map(|(n, v)| (n.clone(), v.clone()))
                .collect();
            Ok(CekState::Apply(Value::Record { type_id, type_name, fields }))
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate expects 1 argument".into()));
            }
            Ok(CekState::Apply(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id))))
        }
        Value::RecordAccessor { type_id, type_name, field_name, field_index } => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor expects 1 argument".into()));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if *tid == type_id => {
                    Ok(CekState::Apply(fields[field_index].1.clone()))
                }
                _ => Err(EvalError::Type(format!("{}: expected {}", field_name, type_name))),
            }
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func.display_value()))),
    }
}
