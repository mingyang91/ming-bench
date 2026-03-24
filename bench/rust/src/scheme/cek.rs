use std::cell::RefCell;
use std::rc::Rc;

use super::{
    Value, Spanned, Env, Output, Kont, DUMMY_SPAN,
    env_get, env_set, env_update, new_env, bind_lambda_env,
    apply, eval_define_record_type, eval_define_syntax,
    eval_string_set_standalone, eval_do, expand_macro,
};
use super::error::{EvalError, Span};
use super::parser::parse_params;
use super::builtins::{apply_builtin, values_eqv};

pub(super) enum CekState {
    Eval(Spanned, Env, Rc<Kont>),
    ApplyKont(Rc<Kont>, Value),
    ResumeKont(Rc<Kont>, Value),
}

pub(super) enum CekStep {
    Continue(CekState),
    Done(Value),
}

thread_local! {
    pub(super) static CONT_JUMP: RefCell<Option<(Rc<Kont>, Value)>> = const { RefCell::new(None) };
}

pub(super) fn cek_eval(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let kont = Rc::new(Kont::Halt);
    let kont = if exprs.len() > 1 {
        Rc::new(Kont::Seq { remaining: exprs[1..].to_vec(), env: env.clone(), next: kont })
    } else {
        kont
    };
    let mut state = CekState::Eval(exprs[0].clone(), env.clone(), kont);

    loop {
        match cek_step(state, out) {
            Ok(CekStep::Continue(next)) => state = next,
            Ok(CekStep::Done(v)) => return Ok(v),
            Err(EvalError::ContinuationInvoked) => {
                let (kont, value) = CONT_JUMP.with(|c| c.borrow_mut().take().expect("CONT_JUMP must be set after ContinuationInvoked"));
                state = CekState::ResumeKont(kont, value);
            }
            Err(e) => return Err(e),
        }
    }
}

fn cek_step(state: CekState, out: &Output) -> Result<CekStep, EvalError> {
    match state {
        CekState::Eval(expr, env, kont) => cek_eval_expr(expr, env, kont, out),
        CekState::ApplyKont(kont, value) => cek_apply_kont(&kont, value, out, false),
        CekState::ResumeKont(kont, value) => cek_apply_kont(&kont, value, out, true),
    }
}

fn cek_eval_expr(expr: Spanned, env: Env, kont: Rc<Kont>, out: &Output) -> Result<CekStep, EvalError> {
    let span = expr.span;
    match &expr.val {
        Value::Integer(_) | Value::Float(_) | Value::Rational(..) | Value::Boolean(_)
        | Value::Str(_) | Value::Char(_) | Value::Pair(..) | Value::Lambda(..)
        | Value::CaseLambda(..) | Value::SyntaxRules { .. } | Value::Vector(..)
        | Value::Record(..) | Value::RecordConstructor(..) | Value::RecordPredicate(..)
        | Value::RecordAccessor(..) | Value::Continuation(..) => {
            Ok(CekStep::Continue(CekState::ApplyKont(kont, expr.val.clone())))
        }

        Value::Symbol(name) => {
            let val = env_get(&env, name)
                .ok_or_else(|| EvalError::UnboundVariable(name.clone(), span))?;
            Ok(CekStep::Continue(CekState::ApplyKont(kont, val)))
        }

        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into(), span));
            }
            let head = &items[0];

            if let Value::Symbol(name) = &head.val {
                match name.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into(), span));
                        }
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, items[1].val.clone())));
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity("if requires 2 or 3 arguments".into(), span));
                        }
                        let else_br = if items.len() == 4 { Some(items[3].clone()) } else { None };
                        let k = Rc::new(Kont::IfDecide { then_br: items[2].clone(), else_br, env: env.clone(), next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("define requires at least 2 arguments".into(), span));
                        }
                        match &items[1].val {
                            Value::Symbol(var_name) => {
                                let k = Rc::new(Kont::DefineVar { name: var_name.clone(), env: env.clone(), next: kont });
                                return Ok(CekStep::Continue(CekState::Eval(items[2].clone(), env, k)));
                            }
                            Value::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Parse("define: empty signature".into(), span));
                                }
                                let func_name = match &sig[0].val {
                                    Value::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Type("define: expected symbol for function name".into(), span)),
                                };
                                let (params, rest) = parse_params(&sig[1..], "define", span)?;
                                let body = items[2..].to_vec();
                                let lambda = Value::Lambda(params, rest, body, env.clone());
                                env_set(&env, func_name, lambda);
                                return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Void)));
                            }
                            _ => return Err(EvalError::Type("define: expected symbol or list".into(), span)),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires at least 2 arguments".into(), span));
                        }
                        let (params, rest) = match &items[1].val {
                            Value::List(param_list) => parse_params(param_list, "lambda", span)?,
                            Value::Symbol(s) => (vec![], Some(s.clone())),
                            _ => return Err(EvalError::Type("lambda: expected parameter list".into(), span)),
                        };
                        let body = items[2..].to_vec();
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Lambda(params, rest, body, env))));
                    }
                    "case-lambda" => {
                        let mut clauses = Vec::new();
                        for clause in &items[1..] {
                            let Value::List(parts) = &clause.val else {
                                return Err(EvalError::Type("case-lambda: clause must be a list".into(), span));
                            };
                            if parts.is_empty() {
                                return Err(EvalError::Arity("case-lambda: clause must have formals and body".into(), span));
                            }
                            let (params, rest) = match &parts[0].val {
                                Value::List(param_list) => parse_params(param_list, "case-lambda", span)?,
                                Value::Symbol(s) => (vec![], Some(s.clone())),
                                _ => return Err(EvalError::Type("case-lambda: expected parameter list".into(), span)),
                            };
                            let body = parts[1..].to_vec();
                            clauses.push((params, rest, body, env.clone()));
                        }
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::CaseLambda(clauses))));
                    }
                    "let" => return cek_eval_let(&items[1..], env, kont, out, span),
                    "begin" => {
                        if items.len() <= 1 {
                            return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Void)));
                        }
                        return eval_body_cek(&items[1..], env, kont);
                    }
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into(), span));
                        }
                        let Value::Symbol(name) = &items[1].val else {
                            return Err(EvalError::Type("set!: first argument must be a symbol".into(), span));
                        };
                        let k = Rc::new(Kont::SetVar { name: name.clone(), env: env.clone(), span, next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[2].clone(), env, k)));
                    }
                    "cond" => return cek_eval_cond(&items[1..], &env, span, kont),
                    "and" => {
                        if items.len() <= 1 {
                            return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Boolean(true))));
                        }
                        if items.len() == 2 {
                            return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, kont)));
                        }
                        let k = Rc::new(Kont::And { remaining: items[2..].to_vec(), env: env.clone(), next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "or" => {
                        if items.len() <= 1 {
                            return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Boolean(false))));
                        }
                        if items.len() == 2 {
                            return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, kont)));
                        }
                        let k = Rc::new(Kont::Or { remaining: items[2..].to_vec(), env: env.clone(), next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "not" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("not requires 1 argument".into(), span));
                        }
                        let k = Rc::new(Kont::Not { next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "when" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("when requires test and body".into(), span));
                        }
                        let k = Rc::new(Kont::WhenTest { body: items[2..].to_vec(), env: env.clone(), next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "case" => {
                        if items.len() < 2 {
                            return Err(EvalError::Arity("case requires key and clauses".into(), span));
                        }
                        let k = Rc::new(Kont::CaseKey { clauses: items[2..].to_vec(), env: env.clone(), span, next: kont });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "let*" => return cek_eval_let_star(&items[1..], env, kont, span),
                    "letrec" | "letrec*" => return cek_eval_letrec(&items[1..], env, kont, out, span),
                    "call/cc" | "call-with-current-continuation" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("call/cc requires 1 argument".into(), span));
                        }
                        let k = Rc::new(Kont::CallCC { captured: kont, span });
                        return Ok(CekStep::Continue(CekState::Eval(items[1].clone(), env, k)));
                    }
                    "string-set!" => {
                        // Fall back to old evaluator for string-set!
                        let result = eval_string_set_standalone(items, &env, out, span)?;
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, result)));
                    }
                    "do" => {
                        let result = eval_do(&items[1..], &env, out, span)?;
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, result)));
                    }
                    "define-record-type" => {
                        eval_define_record_type(items, &env, span)?;
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Void)));
                    }
                    "define-syntax" => {
                        eval_define_syntax(items, &env, span)?;
                        return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Void)));
                    }
                    _ => {
                        if let Some(Value::SyntaxRules { ref literals, ref rules, ref def_env }) = env_get(&env, name) {
                            let expanded = expand_macro(items, literals, rules, def_env, &env, span)?;
                            return Ok(CekStep::Continue(CekState::Eval(expanded, env, kont)));
                        }
                    }
                }
            }

            // Function application
            let head_expr = items[0].clone();
            let arg_exprs = items[1..].to_vec();
            let k = Rc::new(Kont::EvalHead { arg_exprs, env: env.clone(), span, next: kont });
            Ok(CekStep::Continue(CekState::Eval(head_expr, env, k)))
        }

        Value::Void => Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Void))),
    }
}

fn cek_apply_kont(kont: &Rc<Kont>, value: Value, out: &Output, is_resume: bool) -> Result<CekStep, EvalError> {
    match &**kont {
        Kont::Halt => Ok(CekStep::Done(value)),

        Kont::Seq { remaining, env, next } => {
            // Value from previous expression is discarded
            if remaining.is_empty() {
                Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), value)))
            } else {
                eval_body_cek(remaining, env.clone(), next.clone())
            }
        }

        Kont::IfDecide { then_br, else_br, env, next } => {
            if value.is_truthy() {
                Ok(CekStep::Continue(CekState::Eval(then_br.clone(), env.clone(), next.clone())))
            } else if let Some(eb) = else_br {
                Ok(CekStep::Continue(CekState::Eval(eb.clone(), env.clone(), next.clone())))
            } else {
                Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), Value::Void)))
            }
        }

        Kont::DefineVar { name, env, next } => {
            env_set(env, name.clone(), value);
            Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), Value::Void)))
        }

        Kont::SetVar { name, env, span, next } => {
            if !env_update(env, name, value) {
                return Err(EvalError::UnboundVariable(name.clone(), *span));
            }
            Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), Value::Void)))
        }

        Kont::EvalHead { arg_exprs, env, span, next } => {
            let func = value;
            if arg_exprs.is_empty() {
                cek_apply_func(&func, &[], next.clone(), out, *span)
            } else {
                let k = Rc::new(Kont::EvalArgs {
                    func,
                    all_arg_exprs: arg_exprs.clone(),
                    done_vals: vec![],
                    remaining: arg_exprs[1..].to_vec(),
                    env: env.clone(),
                    span: *span,
                    next: next.clone(),
                });
                Ok(CekStep::Continue(CekState::Eval(arg_exprs[0].clone(), env.clone(), k)))
            }
        }

        Kont::EvalArgs { func, all_arg_exprs, done_vals, remaining, env, span, next } => {
            if is_resume {
                // Continuation was invoked. Re-evaluate all args with the call/cc
                // position replaced by the provided value.
                let callcc_idx = done_vals.len();
                let mut new_exprs = all_arg_exprs.clone();
                new_exprs[callcc_idx] = Spanned::new(value, DUMMY_SPAN);

                if new_exprs.is_empty() {
                    return cek_apply_func(func, &[], next.clone(), out, *span);
                }
                let k = Rc::new(Kont::EvalArgs {
                    func: func.clone(),
                    all_arg_exprs: new_exprs.clone(),
                    done_vals: vec![],
                    remaining: new_exprs[1..].to_vec(),
                    env: env.clone(),
                    span: *span,
                    next: next.clone(),
                });
                return Ok(CekStep::Continue(CekState::Eval(new_exprs[0].clone(), env.clone(), k)));
            }

            let mut new_done = done_vals.clone();
            new_done.push(value);

            if remaining.is_empty() {
                cek_apply_func(func, &new_done, next.clone(), out, *span)
            } else {
                let k = Rc::new(Kont::EvalArgs {
                    func: func.clone(),
                    all_arg_exprs: all_arg_exprs.clone(),
                    done_vals: new_done,
                    remaining: remaining[1..].to_vec(),
                    env: env.clone(),
                    span: *span,
                    next: next.clone(),
                });
                Ok(CekStep::Continue(CekState::Eval(remaining[0].clone(), env.clone(), k)))
            }
        }

        Kont::CallCC { captured, span } => {
            let cont_val = Value::Continuation(captured.clone());
            cek_apply_func(&value, &[cont_val], captured.clone(), out, *span)
        }

        Kont::And { remaining, env, next } => {
            if !value.is_truthy() {
                return Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), value)));
            }
            if remaining.is_empty() {
                return Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), value)));
            }
            if remaining.len() == 1 {
                return Ok(CekStep::Continue(CekState::Eval(remaining[0].clone(), env.clone(), next.clone())));
            }
            let k = Rc::new(Kont::And { remaining: remaining[1..].to_vec(), env: env.clone(), next: next.clone() });
            Ok(CekStep::Continue(CekState::Eval(remaining[0].clone(), env.clone(), k)))
        }

        Kont::Or { remaining, env, next } => {
            if value.is_truthy() {
                return Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), value)));
            }
            if remaining.is_empty() {
                return Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), value)));
            }
            if remaining.len() == 1 {
                return Ok(CekStep::Continue(CekState::Eval(remaining[0].clone(), env.clone(), next.clone())));
            }
            let k = Rc::new(Kont::Or { remaining: remaining[1..].to_vec(), env: env.clone(), next: next.clone() });
            Ok(CekStep::Continue(CekState::Eval(remaining[0].clone(), env.clone(), k)))
        }

        Kont::Not { next } => {
            Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), Value::Boolean(!value.is_truthy()))))
        }

        Kont::WhenTest { body, env, next } => {
            if value.is_truthy() {
                eval_body_cek(body, env.clone(), next.clone())
            } else {
                Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), Value::Void)))
            }
        }

        Kont::CondTest { clause_body, remaining_clauses, env, span, next } => {
            if value.is_truthy() {
                if clause_body.is_empty() {
                    Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), value)))
                } else {
                    eval_body_cek(clause_body, env.clone(), next.clone())
                }
            } else {
                cek_eval_cond(remaining_clauses, env, *span, next.clone())
            }
        }

        Kont::CaseKey { clauses, env, span, next } => {
            for clause in clauses {
                let Value::List(parts) = &clause.val else {
                    return Err(EvalError::Type("case: expected clause list".into(), *span));
                };
                if parts.is_empty() {
                    return Err(EvalError::Arity("case: empty clause".into(), *span));
                }
                if let Value::Symbol(s) = &parts[0].val {
                    if s == "else" {
                        return eval_body_cek(&parts[1..], env.clone(), next.clone());
                    }
                }
                let Value::List(datums) = &parts[0].val else {
                    return Err(EvalError::Type("case: expected datum list".into(), *span));
                };
                for datum in datums {
                    if values_eqv(&value, &datum.val) {
                        return eval_body_cek(&parts[1..], env.clone(), next.clone());
                    }
                }
            }
            Ok(CekStep::Continue(CekState::ApplyKont(next.clone(), Value::Void)))
        }

        Kont::LetBind { current_name, remaining, local_env, eval_env, body, next } => {
            env_set(local_env, current_name.clone(), value);
            if remaining.is_empty() {
                eval_body_cek(body, local_env.clone(), next.clone())
            } else {
                let (next_name, next_expr) = &remaining[0];
                let k = Rc::new(Kont::LetBind {
                    current_name: next_name.clone(),
                    remaining: remaining[1..].to_vec(),
                    local_env: local_env.clone(),
                    eval_env: eval_env.clone(),
                    body: body.clone(),
                    next: next.clone(),
                });
                Ok(CekStep::Continue(CekState::Eval(next_expr.clone(), eval_env.clone(), k)))
            }
        }

        Kont::LetStarBind { current_name, remaining, local_env, body, next } => {
            env_set(local_env, current_name.clone(), value);
            if remaining.is_empty() {
                eval_body_cek(body, local_env.clone(), next.clone())
            } else {
                let (next_name, next_expr) = &remaining[0];
                let k = Rc::new(Kont::LetStarBind {
                    current_name: next_name.clone(),
                    remaining: remaining[1..].to_vec(),
                    local_env: local_env.clone(),
                    body: body.clone(),
                    next: next.clone(),
                });
                Ok(CekStep::Continue(CekState::Eval(next_expr.clone(), local_env.clone(), k)))
            }
        }

        Kont::LetrecBind { idx, names, remaining_inits, local_env, body, next } => {
            env_set(local_env, names[*idx].clone(), value);
            if remaining_inits.is_empty() {
                eval_body_cek(body, local_env.clone(), next.clone())
            } else {
                let k = Rc::new(Kont::LetrecBind {
                    idx: idx + 1,
                    names: names.clone(),
                    remaining_inits: remaining_inits[1..].to_vec(),
                    local_env: local_env.clone(),
                    body: body.clone(),
                    next: next.clone(),
                });
                Ok(CekStep::Continue(CekState::Eval(remaining_inits[0].clone(), local_env.clone(), k)))
            }
        }

        Kont::NamedLetBind { loop_name, params, done_inits, remaining_inits, body, eval_env, next } => {
            let mut new_done = done_inits.clone();
            new_done.push(value);
            if remaining_inits.is_empty() {
                // All inits evaluated, set up loop
                let loop_env = new_env(Some(eval_env.clone()));
                let lambda = Value::Lambda(params.clone(), None, body.clone(), loop_env.clone());
                env_set(&loop_env, loop_name.clone(), lambda);
                let call_env = new_env(Some(loop_env));
                for (p, v) in params.iter().zip(new_done.iter()) {
                    env_set(&call_env, p.clone(), v.clone());
                }
                eval_body_cek(body, call_env, next.clone())
            } else {
                let k = Rc::new(Kont::NamedLetBind {
                    loop_name: loop_name.clone(),
                    params: params.clone(),
                    done_inits: new_done,
                    remaining_inits: remaining_inits[1..].to_vec(),
                    body: body.clone(),
                    eval_env: eval_env.clone(),
                    next: next.clone(),
                });
                Ok(CekStep::Continue(CekState::Eval(remaining_inits[0].clone(), eval_env.clone(), k)))
            }
        }
    }
}

fn cek_apply_func(func: &Value, args: &[Value], kont: Rc<Kont>, out: &Output, span: Span) -> Result<CekStep, EvalError> {
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            let local_env = bind_lambda_env(params, rest, args, closure_env, span)?;
            eval_body_cek(body, local_env, kont)
        }
        Value::CaseLambda(clauses) => {
            for (params, rest, body, closure_env) in clauses {
                let matches = if rest.is_some() { args.len() >= params.len() } else { args.len() == params.len() };
                if matches {
                    let local_env = bind_lambda_env(params, rest, args, closure_env, span)?;
                    return eval_body_cek(body, local_env, kont);
                }
            }
            Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", args.len()), span))
        }
        Value::Continuation(saved_kont) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into(), span));
            }
            Ok(CekStep::Continue(CekState::ResumeKont(saved_kont.clone(), args[0].clone())))
        }
        Value::RecordConstructor(type_id, field_count) => {
            if args.len() != *field_count {
                return Err(EvalError::Arity(format!("record constructor expects {} arguments, got {}", field_count, args.len()), span));
            }
            Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Record(*type_id, args.to_vec()))))
        }
        Value::RecordPredicate(type_id) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate requires 1 argument".into(), span));
            }
            Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Boolean(matches!(&args[0], Value::Record(tid, _) if tid == type_id)))))
        }
        Value::RecordAccessor(type_id, idx) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::Record(tid, fields) if tid == type_id => Ok(CekStep::Continue(CekState::ApplyKont(kont, fields[*idx].clone()))),
                _ => Err(EvalError::Type("record accessor: wrong record type".into(), span)),
            }
        }
        Value::Symbol(name) if name == "call/cc" || name == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires 1 argument".into(), span));
            }
            let cont_val = Value::Continuation(kont.clone());
            cek_apply_func(&args[0], &[cont_val], kont, out, span)
        }
        Value::Symbol(name) => {
            let result = apply_builtin(name, args, out, span, apply)?;
            Ok(CekStep::Continue(CekState::ApplyKont(kont, result)))
        }
        _ => Err(EvalError::Type("not a procedure".into(), span)),
    }
}

fn eval_body_cek(body: &[Spanned], env: Env, kont: Rc<Kont>) -> Result<CekStep, EvalError> {
    if body.is_empty() {
        return Ok(CekStep::Continue(CekState::ApplyKont(kont, Value::Void)));
    }
    if body.len() == 1 {
        return Ok(CekStep::Continue(CekState::Eval(body[0].clone(), env, kont)));
    }
    let k = Rc::new(Kont::Seq { remaining: body[1..].to_vec(), env: env.clone(), next: kont });
    Ok(CekStep::Continue(CekState::Eval(body[0].clone(), env, k)))
}

fn cek_eval_cond(clauses: &[Spanned], env: &Env, span: Span, next: Rc<Kont>) -> Result<CekStep, EvalError> {
    if clauses.is_empty() {
        return Ok(CekStep::Continue(CekState::ApplyKont(next, Value::Void)));
    }
    let Value::List(parts) = &clauses[0].val else {
        return Err(EvalError::Type("cond: expected list clause".into(), span));
    };
    if parts.is_empty() {
        return Err(EvalError::Arity("cond: empty clause".into(), span));
    }
    if let Value::Symbol(s) = &parts[0].val {
        if s == "else" {
            return eval_body_cek(&parts[1..], env.clone(), next);
        }
    }
    let k = Rc::new(Kont::CondTest {
        clause_body: parts[1..].to_vec(),
        remaining_clauses: clauses[1..].to_vec(),
        env: env.clone(),
        span,
        next,
    });
    Ok(CekStep::Continue(CekState::Eval(parts[0].clone(), env.clone(), k)))
}

fn cek_eval_let(args: &[Spanned], env: Env, kont: Rc<Kont>, _out: &Output, span: Span) -> Result<CekStep, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into(), span));
    }
    // Named let
    if let Value::Symbol(name) = &args[0].val {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into(), span));
        }
        let Value::List(bindings) = &args[1].val else {
            return Err(EvalError::Type("named let: expected bindings list".into(), span));
        };
        let mut params = Vec::new();
        let mut init_exprs = Vec::new();
        for b in bindings {
            let Value::List(pair) = &b.val else {
                return Err(EvalError::Type("let: binding must be a list".into(), span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Arity("let: binding must have 2 elements".into(), span));
            }
            let Value::Symbol(p) = &pair[0].val else {
                return Err(EvalError::Type("let: expected symbol in binding".into(), span));
            };
            params.push(p.clone());
            init_exprs.push(pair[1].clone());
        }
        let body = args[2..].to_vec();
        if init_exprs.is_empty() {
            // No bindings, just set up loop and eval body
            let loop_env = new_env(Some(env.clone()));
            let lambda = Value::Lambda(params.clone(), None, body.clone(), loop_env.clone());
            env_set(&loop_env, name.clone(), lambda);
            return eval_body_cek(&body, loop_env, kont);
        }
        let k = Rc::new(Kont::NamedLetBind {
            loop_name: name.clone(),
            params,
            done_inits: vec![],
            remaining_inits: init_exprs[1..].to_vec(),
            body,
            eval_env: env.clone(),
            next: kont,
        });
        return Ok(CekStep::Continue(CekState::Eval(init_exprs[0].clone(), env, k)));
    }
    // Regular let
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("let: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env.clone()));
    let mut binding_pairs: Vec<(String, Spanned)> = Vec::new();
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("let: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("let: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("let: expected symbol in binding".into(), span));
        };
        binding_pairs.push((name.clone(), pair[1].clone()));
    }
    let body = args[1..].to_vec();
    if binding_pairs.is_empty() {
        return eval_body_cek(&body, local_env, kont);
    }
    let (first_name, first_expr) = &binding_pairs[0];
    let k = Rc::new(Kont::LetBind {
        current_name: first_name.clone(),
        remaining: binding_pairs[1..].to_vec(),
        local_env,
        eval_env: env.clone(),
        body,
        next: kont,
    });
    Ok(CekStep::Continue(CekState::Eval(first_expr.clone(), env, k)))
}

fn cek_eval_let_star(args: &[Spanned], env: Env, kont: Rc<Kont>, span: Span) -> Result<CekStep, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("let*: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env));
    let mut binding_pairs: Vec<(String, Spanned)> = Vec::new();
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("let*: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("let*: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("let*: expected symbol in binding".into(), span));
        };
        binding_pairs.push((name.clone(), pair[1].clone()));
    }
    let body = args[1..].to_vec();
    if binding_pairs.is_empty() {
        return eval_body_cek(&body, local_env, kont);
    }
    let (first_name, first_expr) = &binding_pairs[0];
    let k = Rc::new(Kont::LetStarBind {
        current_name: first_name.clone(),
        remaining: binding_pairs[1..].to_vec(),
        local_env: local_env.clone(),
        body,
        next: kont,
    });
    Ok(CekStep::Continue(CekState::Eval(first_expr.clone(), local_env, k)))
}

fn cek_eval_letrec(args: &[Spanned], env: Env, kont: Rc<Kont>, _out: &Output, span: Span) -> Result<CekStep, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("letrec requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("letrec: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env));
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("letrec: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("letrec: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("letrec: expected symbol in binding".into(), span));
        };
        names.push(name.clone());
        init_exprs.push(pair[1].clone());
        env_set(&local_env, name.clone(), Value::Void);
    }
    let body = args[1..].to_vec();
    if init_exprs.is_empty() {
        return eval_body_cek(&body, local_env, kont);
    }
    let k = Rc::new(Kont::LetrecBind {
        idx: 0,
        names,
        remaining_inits: init_exprs[1..].to_vec(),
        local_env: local_env.clone(),
        body,
        next: kont,
    });
    Ok(CekStep::Continue(CekState::Eval(init_exprs[0].clone(), local_env, k)))
}

/// Check if an expression (or any sub-expression) references call/cc.
pub(super) fn expr_uses_callcc(expr: &Spanned) -> bool {
    match &expr.val {
        Value::Symbol(s) => s == "call/cc" || s == "call-with-current-continuation",
        Value::List(items) => items.iter().any(expr_uses_callcc),
        _ => false,
    }
}

/// Resume the CEK machine from a captured continuation.
pub(super) fn cek_resume(kont: Rc<Kont>, value: Value, out: &Output) -> Result<Value, EvalError> {
    let mut state = CekState::ApplyKont(kont, value);
    loop {
        match cek_step(state, out) {
            Ok(CekStep::Continue(next)) => state = next,
            Ok(CekStep::Done(v)) => return Ok(v),
            Err(EvalError::ContinuationInvoked) => {
                let (kont, value) = CONT_JUMP.with(|c| c.borrow_mut().take().expect("CONT_JUMP must be set after ContinuationInvoked"));
                state = CekState::ResumeKont(kont, value);
            }
            Err(e) => return Err(e),
        }
    }
}
