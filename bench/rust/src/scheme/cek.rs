use std::rc::Rc;

use super::builtins::eval_builtin;
use super::error::EvalError;
use super::macros::{eval_define_syntax, expand_macro_only, expand_transformer};
use super::numeric::make_rational;
use super::{
    collect_list, env_define, env_lookup, env_set, eval_case, eval_case_lambda,
    eval_define_record_type, eval_do, eval_lambda, expr_to_value, is_builtin, is_truthy,
    make_immutable_str, new_frame, parse_params, with_span, Env, Expr, ExprKind, Kont, Span,
    Value,
};

enum Handler {
    Proc(Value),
    Guard {
        var: String,
        clauses: Vec<Expr>,
        env: Env,
        guard_k: Rc<Kont>,
        guard_winders: Vec<Rc<(Value, Value)>>,
    },
}

enum Ctrl {
    Eval(Expr, Env),
    Val(Value),
    Apply(Value, Vec<Value>),
}

fn is_callcc(name: &str) -> bool {
    name == "call/cc" || name == "call-with-current-continuation"
}

fn cek_seq(exprs: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if exprs.is_empty() {
        Ok((Ctrl::Val(Value::Boolean(false)), k))
    } else if exprs.len() == 1 {
        Ok((Ctrl::Eval(exprs[0].clone(), env), k))
    } else {
        Ok((Ctrl::Eval(exprs[0].clone(), env.clone()),
            Rc::new(Kont::Seq { rest: exprs[1..].to_vec(), env, next: k })))
    }
}

fn cek_call(items: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    Ok((Ctrl::Eval(items[0].clone(), env.clone()),
        Rc::new(Kont::Ev1 { args: items[1..].to_vec(), env, next: k })))
}

fn cek_parse_bindings(args: &[Expr]) -> Result<Vec<(String, Expr)>, EvalError> {
    let list = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("expected bindings list".into())),
    };
    let mut result = Vec::new();
    for b in list {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    result.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Type("binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("invalid binding".into())),
        }
    }
    Ok(result)
}

fn cek_define(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            Ok((Ctrl::Eval(args[1].clone(), env.clone()),
                Rc::new(Kont::Def { name: name.clone(), env, next: k })))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let (params, rest) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let proc = Value::Procedure(params, rest, body, env.clone());
            env_define(&env, name, proc);
            Ok((Ctrl::Val(Value::Boolean(false)), k))
        }
        ExprKind::DottedList(sig, tail) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let mut params = Vec::new();
            for item in &sig[1..] {
                match &item.kind {
                    ExprKind::Symbol(s) => params.push(s.clone()),
                    _ => return Err(EvalError::Type("define: parameter must be a symbol".into())),
                }
            }
            let rest = match &tail.kind {
                ExprKind::Symbol(s) => Some(s.clone()),
                _ => return Err(EvalError::Type("define: rest parameter must be a symbol".into())),
            };
            let body = args[1..].to_vec();
            let proc = Value::Procedure(params, rest, body, env.clone());
            env_define(&env, name, proc);
            Ok((Ctrl::Val(Value::Boolean(false)), k))
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn cek_and(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.is_empty() {
        return Ok((Ctrl::Val(Value::Boolean(true)), k));
    }
    if args.len() == 1 {
        return Ok((Ctrl::Eval(args[0].clone(), env), k));
    }
    Ok((Ctrl::Eval(args[0].clone(), env.clone()),
        Rc::new(Kont::And { rest: args[1..].to_vec(), env, next: k })))
}

fn cek_or(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.is_empty() {
        return Ok((Ctrl::Val(Value::Boolean(false)), k));
    }
    if args.len() == 1 {
        return Ok((Ctrl::Eval(args[0].clone(), env), k));
    }
    Ok((Ctrl::Eval(args[0].clone(), env.clone()),
        Rc::new(Kont::Or { rest: args[1..].to_vec(), env, next: k })))
}

fn cek_let(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = cek_parse_bindings(&args[1..])?;
        let params: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
        let init_exprs: Vec<Expr> = bindings.into_iter().map(|(_, e)| e).collect();
        let body = args[2..].to_vec();
        let mut let_env = env.clone();
        let frame = new_frame();
        let_env.push(frame.clone());
        let proc = Value::Procedure(params, None, body, let_env);
        frame.borrow_mut().insert(name.clone(), proc.clone());
        if init_exprs.is_empty() {
            return Ok((Ctrl::Apply(proc, vec![]), k));
        }
        let n = init_exprs.len();
        return Ok((Ctrl::Eval(init_exprs[n - 1].clone(), env.clone()),
            Rc::new(Kont::EvN {
                func: proc,
                done: vec![],
                rest: init_exprs[..n - 1].to_vec(),
                env,
                next: k,
            })));
    }
    // Regular let
    let bindings = cek_parse_bindings(args)?;
    let body = args[1..].to_vec();
    if bindings.is_empty() {
        let mut e = env;
        e.push(new_frame());
        return cek_seq(&body, e, k);
    }
    let frame = new_frame();
    let (first_var, first_expr) = bindings[0].clone();
    let remaining = bindings[1..].to_vec();
    Ok((Ctrl::Eval(first_expr, env.clone()),
        Rc::new(Kont::LetInit {
            var: first_var,
            rem: remaining,
            frame,
            body,
            eval_env: env,
            next: k,
        })))
}

fn cek_let_star(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bindings = cek_parse_bindings(args)?;
    let body = args[1..].to_vec();
    let mut new_env = env;
    new_env.push(new_frame());
    if bindings.is_empty() {
        return cek_seq(&body, new_env, k);
    }
    let (first_var, first_expr) = bindings[0].clone();
    let remaining = bindings[1..].to_vec();
    Ok((Ctrl::Eval(first_expr, new_env.clone()),
        Rc::new(Kont::SeqBind {
            var: first_var,
            rem: remaining,
            body,
            env: new_env,
            next: k,
            use_set: false,
        })))
}

fn cek_letrec(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings = cek_parse_bindings(args)?;
    let body = args[1..].to_vec();
    let frame = new_frame();
    let mut new_env = env;
    new_env.push(frame.clone());
    for (name, _) in &bindings {
        frame.borrow_mut().insert(name.clone(), Value::Boolean(false));
    }
    if bindings.is_empty() {
        return cek_seq(&body, new_env, k);
    }
    let (first_var, first_expr) = bindings[0].clone();
    let remaining = bindings[1..].to_vec();
    Ok((Ctrl::Eval(first_expr, new_env.clone()),
        Rc::new(Kont::SeqBind {
            var: first_var,
            rem: remaining,
            body,
            env: new_env,
            next: k,
            use_set: true,
        })))
}

fn cek_letrec_star(args: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings = cek_parse_bindings(args)?;
    let body = args[1..].to_vec();
    let mut new_env = env;
    new_env.push(new_frame());
    if bindings.is_empty() {
        return cek_seq(&body, new_env, k);
    }
    let (first_var, first_expr) = bindings[0].clone();
    let remaining = bindings[1..].to_vec();
    Ok((Ctrl::Eval(first_expr, new_env.clone()),
        Rc::new(Kont::SeqBind {
            var: first_var,
            rem: remaining,
            body,
            env: new_env,
            next: k,
            use_set: false,
        })))
}

fn cek_cond(clauses: &[Expr], env: Env, k: Rc<Kont>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if clauses.is_empty() {
        return Ok((Ctrl::Val(Value::Boolean(false)), k));
    }
    let items = match &clauses[0].kind {
        ExprKind::List(items) if !items.is_empty() => items,
        _ => return Err(EvalError::Type("cond: invalid clause".into())),
    };
    if let ExprKind::Symbol(s) = &items[0].kind {
        if s == "else" {
            return cek_seq(&items[1..], env, k);
        }
    }
    Ok((Ctrl::Eval(items[0].clone(), env.clone()),
        Rc::new(Kont::CondK {
            body: items[1..].to_vec(),
            rest: clauses[1..].to_vec(),
            env,
            next: k,
        })))
}

fn try_eval_simple(expr: &Expr, env: &Env, output: &mut String) -> Option<Result<Value, EvalError>> {
    match &expr.kind {
        ExprKind::Integer(n) => Some(Ok(Value::Integer(*n))),
        ExprKind::Rational(n, d) => Some(Ok(make_rational(*n, *d))),
        ExprKind::Float(f) => Some(Ok(Value::Float(*f))),
        ExprKind::Boolean(b) => Some(Ok(Value::Boolean(*b))),
        ExprKind::Char(c) => Some(Ok(Value::Char(*c))),
        ExprKind::Str(s) => Some(Ok(make_immutable_str(s.clone()))),
        ExprKind::Symbol(name) => {
            if is_callcc(name) { return None; }
            match env_lookup(env, name) {
                Ok(v) => {
                    if matches!(v, Value::Macro { .. }) { return None; }
                    Some(Ok(v))
                }
                Err(_) if is_builtin(name) => Some(Ok(Value::Builtin(name.clone()))),
                Err(e) => Some(Err(e)),
            }
        }
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(op) = &items[0].kind {
                if op == "quote" && items.len() == 2 {
                    return Some(Ok(expr_to_value(&items[1])));
                }
                if is_builtin(op) && op != "dynamic-wind" && op != "raise" && op != "with-exception-handler" && op != "values" && op != "call-with-values" {
                    let mut args = Vec::with_capacity(items.len() - 1);
                    for item in &items[1..] {
                        match try_eval_simple(item, env, output)? {
                            Ok(v) => args.push(v),
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(eval_builtin(op, &args, output));
                }
            }
            None
        }
        _ => None,
    }
}

fn try_fast_call(items: &[Expr], env: &Env, k: Rc<Kont>, output: &mut String) -> Option<Result<(Ctrl, Rc<Kont>), EvalError>> {
    let func = match try_eval_simple(&items[0], env, output)? {
        Ok(v) => v,
        Err(e) => return Some(Err(e)),
    };
    if matches!(&func, Value::Macro { .. }) { return None; }
    let mut args = Vec::with_capacity(items.len() - 1);
    for item in &items[1..] {
        match try_eval_simple(item, env, output)? {
            Ok(v) => args.push(v),
            Err(e) => return Some(Err(e)),
        }
    }
    Some(Ok((Ctrl::Apply(func, args), k)))
}

fn guard_clause_matched(
    body: &[Expr],
    guard_env: &Env,
    guard_k: &Rc<Kont>,
    guard_winders: &[Rc<(Value, Value)>],
    winders: &mut Vec<Rc<(Value, Value)>>,
) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    let common_len = winders.iter().zip(guard_winders.iter())
        .take_while(|(a, b)| Rc::ptr_eq(a, b))
        .count();
    let mut ops: Vec<(bool, Rc<(Value, Value)>)> = Vec::new();
    for i in (common_len..winders.len()).rev() {
        ops.push((false, winders[i].clone()));
    }
    for w in guard_winders.iter().skip(common_len) {
        ops.push((true, w.clone()));
    }
    if ops.is_empty() {
        cek_seq(body, guard_env.clone(), guard_k.clone())
    } else {
        let body_k = Rc::new(Kont::GuardBody {
            body: body.to_vec(),
            env: guard_env.clone(),
            next: guard_k.clone(),
        });
        let (is_rewind, entry) = ops[0].clone();
        let remaining = ops[1..].to_vec();
        let next_k = Rc::new(Kont::WindShift {
            ops: remaining,
            val: Value::Boolean(false),
            saved_k: body_k,
        });
        if is_rewind {
            winders.push(entry.clone());
            Ok((Ctrl::Apply(entry.0.clone(), vec![]), next_k))
        } else {
            winders.pop();
            Ok((Ctrl::Apply(entry.1.clone(), vec![]), next_k))
        }
    }
}

fn start_guard_clause(
    exn: &Value,
    clauses: &[Expr],
    guard_env: &Env,
    guard_k: &Rc<Kont>,
    guard_winders: &[Rc<(Value, Value)>],
    winders: &mut Vec<Rc<(Value, Value)>>,
) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    if clauses.is_empty() {
        // Re-raise: no matching clause
        return Ok((
            Ctrl::Apply(Value::Builtin("raise".into()), vec![exn.clone()]),
            Rc::new(Kont::RaiseReturn),
        ));
    }
    let items = match &clauses[0].kind {
        ExprKind::List(items) if !items.is_empty() => items,
        _ => return Err(EvalError::Type("guard: invalid clause".into())),
    };
    // Check for else
    if let ExprKind::Symbol(s) = &items[0].kind {
        if s == "else" {
            return guard_clause_matched(&items[1..], guard_env, guard_k, guard_winders, winders);
        }
    }
    let body = items[1..].to_vec();
    let rest = clauses[1..].to_vec();
    Ok((
        Ctrl::Eval(items[0].clone(), guard_env.clone()),
        Rc::new(Kont::GuardTest {
            exn: exn.clone(),
            body,
            rest_clauses: rest,
            guard_env: guard_env.clone(),
            guard_k: guard_k.clone(),
            guard_winders: guard_winders.to_vec(),
        }),
    ))
}

fn cek_step_eval(expr: Expr, env: Env, k: Rc<Kont>, output: &mut String, winders: &[Rc<(Value, Value)>], handlers: &mut Vec<Handler>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    let span = expr.span;
    let result = cek_step_eval_inner(&expr, env, k, output, winders, handlers);
    result.map_err(|e| with_span(span, e))
}

fn cek_step_eval_inner(expr: &Expr, env: Env, k: Rc<Kont>, output: &mut String, winders: &[Rc<(Value, Value)>], handlers: &mut Vec<Handler>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok((Ctrl::Val(Value::Integer(*n)), k)),
        ExprKind::Rational(n, d) => Ok((Ctrl::Val(make_rational(*n, *d)), k)),
        ExprKind::Float(f) => Ok((Ctrl::Val(Value::Float(*f)), k)),
        ExprKind::Boolean(b) => Ok((Ctrl::Val(Value::Boolean(*b)), k)),
        ExprKind::Char(c) => Ok((Ctrl::Val(Value::Char(*c)), k)),
        ExprKind::Str(s) => Ok((Ctrl::Val(make_immutable_str(s.clone())), k)),
        ExprKind::Symbol(name) => {
            match env_lookup(&env, name) {
                Ok(v) => Ok((Ctrl::Val(v), k)),
                Err(_) if is_builtin(name) || is_callcc(name) => {
                    Ok((Ctrl::Val(Value::Builtin(name.clone())), k))
                }
                Err(e) => Err(e),
            }
        }
        ExprKind::List(items) if items.is_empty() => {
            Err(EvalError::Parse("empty application".into()))
        }
        ExprKind::List(items) => {
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => cek_define(&items[1..], env, k),
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into()));
                        }
                        let name = match &items[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("set! requires a symbol".into())),
                        };
                        Ok((Ctrl::Eval(items[2].clone(), env.clone()),
                            Rc::new(Kont::Set { name, env, next: k })))
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
                        }
                        if let Some(test_result) = try_eval_simple(&items[1], &env, output) {
                            let test_val = test_result?;
                            return if is_truthy(&test_val) {
                                Ok((Ctrl::Eval(items[2].clone(), env), k))
                            } else if items.len() == 4 {
                                Ok((Ctrl::Eval(items[3].clone(), env), k))
                            } else {
                                Ok((Ctrl::Val(Value::Boolean(false)), k))
                            };
                        }
                        Ok((Ctrl::Eval(items[1].clone(), env.clone()),
                            Rc::new(Kont::If {
                                then_e: items[2].clone(),
                                else_e: items.get(3).cloned(),
                                env,
                                next: k,
                            })))
                    }
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into()));
                        }
                        Ok((Ctrl::Val(expr_to_value(&items[1])), k))
                    }
                    "quasiquote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quasiquote requires 1 argument".into()));
                        }
                        let mut env_mut = env;
                        let val = super::eval_quasiquote(&items[1], &mut env_mut, output)?;
                        Ok((Ctrl::Val(val), k))
                    }
                    "lambda" => Ok((Ctrl::Val(eval_lambda(&items[1..], &env)?), k)),
                    "case-lambda" => Ok((Ctrl::Val(eval_case_lambda(&items[1..], &env)?), k)),
                    "begin" => cek_seq(&items[1..], env, k),
                    "and" => cek_and(&items[1..], env, k),
                    "or" => cek_or(&items[1..], env, k),
                    "let" => cek_let(&items[1..], env, k),
                    "let*" => cek_let_star(&items[1..], env, k),
                    "letrec" => cek_letrec(&items[1..], env, k),
                    "letrec*" => cek_letrec_star(&items[1..], env, k),
                    "cond" => cek_cond(&items[1..], env, k),
                    "case" => {
                        let mut env_mut = env;
                        let val = eval_case(&items[1..], &mut env_mut, output)?;
                        Ok((Ctrl::Val(val), k))
                    }
                    "do" => {
                        let mut env_mut = env;
                        let val = eval_do(&items[1..], &mut env_mut, output)?;
                        Ok((Ctrl::Val(val), k))
                    }
                    "define-syntax" => {
                        let mut env_mut = env;
                        let val = eval_define_syntax(&items[1..], &mut env_mut)?;
                        Ok((Ctrl::Val(val), k))
                    }
                    "define-record-type" => {
                        let mut env_mut = env;
                        let val = eval_define_record_type(&items[1..], &mut env_mut)?;
                        Ok((Ctrl::Val(val), k))
                    }
                    "call/cc" | "call-with-current-continuation" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("call/cc requires 1 argument".into()));
                        }
                        Ok((Ctrl::Eval(items[1].clone(), env),
                            Rc::new(Kont::CallCC { next: k })))
                    }
                    "guard" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("guard requires clauses and body".into()));
                        }
                        let clause_header = match &items[1].kind {
                            ExprKind::List(parts) if parts.len() >= 2 => parts,
                            _ => return Err(EvalError::Type("guard: invalid clause header".into())),
                        };
                        let var = match &clause_header[0].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("guard: expected variable name".into())),
                        };
                        let clauses = clause_header[1..].to_vec();
                        let body = items[2..].to_vec();
                        let guard_k = k;
                        let guard_winders = winders.to_vec();
                        let mut guard_env = env.clone();
                        guard_env.push(new_frame());
                        handlers.push(Handler::Guard {
                            var,
                            clauses,
                            env: guard_env,
                            guard_k: guard_k.clone(),
                            guard_winders,
                        });
                        cek_seq(&body, env, Rc::new(Kont::PopHandler { next: guard_k }))
                    }
                    "syntax-case" | "syntax" | "with-syntax" => {
                        let mut env_mut = env;
                        let val = super::eval(&super::Expr { kind: ExprKind::List(items.to_vec()), span: items[0].span }, &mut env_mut, output)?;
                        Ok((Ctrl::Val(val), k))
                    }
                    _ => {
                        if let Ok(macro_val @ Value::Macro { .. }) = env_lookup(&env, op) {
                            let (expanded, hygiene_frame) = expand_macro_only(&macro_val, items, &env)?;
                            let mut new_env = env;
                            let idx = new_env.len().saturating_sub(1);
                            new_env.insert(idx, hygiene_frame);
                            Ok((Ctrl::Eval(expanded, new_env), k))
                        } else if let Ok(Value::TransformerMacro(proc)) = env_lookup(&env, op) {
                            let (expanded, hygiene_frame) = expand_transformer(&proc, items, output)?;
                            let mut new_env = env;
                            let idx = new_env.len().saturating_sub(1);
                            new_env.insert(idx, hygiene_frame);
                            Ok((Ctrl::Eval(expanded, new_env), k))
                        } else if let Some(result) = try_fast_call(items, &env, k.clone(), output) {
                            result
                        } else {
                            cek_call(items, env, k)
                        }
                    }
                }
            } else if let Some(result) = try_fast_call(items, &env, k.clone(), output) {
                result
            } else {
                cek_call(items, env, k)
            }
        }
        ExprKind::DottedList(..) => Err(EvalError::Type("improper list in expression context".into())),
    }
}

fn cek_step_val(val: Value, k: Rc<Kont>, _output: &mut String, winders: &mut Vec<Rc<(Value, Value)>>, handlers: &mut Vec<Handler>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    match &*k {
        Kont::Halt => unreachable!(),
        Kont::Seq { rest, env, next } => {
            if rest.is_empty() {
                Ok((Ctrl::Val(val), next.clone()))
            } else if rest.len() == 1 {
                Ok((Ctrl::Eval(rest[0].clone(), env.clone()), next.clone()))
            } else {
                Ok((Ctrl::Eval(rest[0].clone(), env.clone()),
                    Rc::new(Kont::Seq { rest: rest[1..].to_vec(), env: env.clone(), next: next.clone() })))
            }
        }
        Kont::Def { name, env, next } => {
            env_define(env, name.clone(), val);
            Ok((Ctrl::Val(Value::Boolean(false)), next.clone()))
        }
        Kont::Set { name, env, next } => {
            env_set(env, name, val)?;
            Ok((Ctrl::Val(Value::Boolean(false)), next.clone()))
        }
        Kont::If { then_e, else_e, env, next } => {
            if is_truthy(&val) {
                Ok((Ctrl::Eval(then_e.clone(), env.clone()), next.clone()))
            } else if let Some(e) = else_e {
                Ok((Ctrl::Eval(e.clone(), env.clone()), next.clone()))
            } else {
                Ok((Ctrl::Val(Value::Boolean(false)), next.clone()))
            }
        }
        Kont::Ev1 { args, env, next } => {
            if args.is_empty() {
                Ok((Ctrl::Apply(val, vec![]), next.clone()))
            } else {
                let n = args.len();
                Ok((Ctrl::Eval(args[n - 1].clone(), env.clone()),
                    Rc::new(Kont::EvN {
                        func: val, done: vec![], rest: args[..n - 1].to_vec(),
                        env: env.clone(), next: next.clone(),
                    })))
            }
        }
        Kont::EvN { func, done, rest, env, next } => {
            let mut new_done = vec![val];
            new_done.extend(done.iter().cloned());
            if rest.is_empty() {
                Ok((Ctrl::Apply(func.clone(), new_done), next.clone()))
            } else {
                let n = rest.len();
                Ok((Ctrl::Eval(rest[n - 1].clone(), env.clone()),
                    Rc::new(Kont::EvN {
                        func: func.clone(), done: new_done,
                        rest: rest[..n - 1].to_vec(), env: env.clone(), next: next.clone(),
                    })))
            }
        }
        Kont::CallCC { next } => {
            let cont_val = Value::Continuation(next.clone(), winders.clone());
            Ok((Ctrl::Apply(val, vec![cont_val]), next.clone()))
        }
        Kont::And { rest, env, next } => {
            if !is_truthy(&val) {
                Ok((Ctrl::Val(val), next.clone()))
            } else if rest.len() == 1 {
                Ok((Ctrl::Eval(rest[0].clone(), env.clone()), next.clone()))
            } else {
                Ok((Ctrl::Eval(rest[0].clone(), env.clone()),
                    Rc::new(Kont::And { rest: rest[1..].to_vec(), env: env.clone(), next: next.clone() })))
            }
        }
        Kont::Or { rest, env, next } => {
            if is_truthy(&val) {
                Ok((Ctrl::Val(val), next.clone()))
            } else if rest.len() == 1 {
                Ok((Ctrl::Eval(rest[0].clone(), env.clone()), next.clone()))
            } else {
                Ok((Ctrl::Eval(rest[0].clone(), env.clone()),
                    Rc::new(Kont::Or { rest: rest[1..].to_vec(), env: env.clone(), next: next.clone() })))
            }
        }
        Kont::LetInit { var, rem, frame, body, eval_env, next } => {
            frame.borrow_mut().insert(var.clone(), val);
            if rem.is_empty() {
                let mut body_env = eval_env.clone();
                body_env.push(frame.clone());
                cek_seq(body, body_env, next.clone())
            } else {
                let (next_var, next_expr) = rem[0].clone();
                Ok((Ctrl::Eval(next_expr, eval_env.clone()),
                    Rc::new(Kont::LetInit {
                        var: next_var,
                        rem: rem[1..].to_vec(),
                        frame: frame.clone(),
                        body: body.clone(),
                        eval_env: eval_env.clone(),
                        next: next.clone(),
                    })))
            }
        }
        Kont::SeqBind { var, rem, body, env, next, use_set } => {
            if *use_set {
                env_set(env, var, val)?;
            } else {
                env_define(env, var.clone(), val);
            }
            if rem.is_empty() {
                cek_seq(body, env.clone(), next.clone())
            } else {
                let (next_var, next_expr) = rem[0].clone();
                Ok((Ctrl::Eval(next_expr, env.clone()),
                    Rc::new(Kont::SeqBind {
                        var: next_var,
                        rem: rem[1..].to_vec(),
                        body: body.clone(),
                        env: env.clone(),
                        next: next.clone(),
                        use_set: *use_set,
                    })))
            }
        }
        Kont::CondK { body, rest, env, next } => {
            if is_truthy(&val) {
                if body.len() == 2 {
                    if let ExprKind::Symbol(s) = &body[0].kind {
                        if s == "=>" {
                            return Ok((Ctrl::Eval(body[1].clone(), env.clone()),
                                Rc::new(Kont::CondArrow { test_val: val, next: next.clone() })));
                        }
                    }
                }
                if body.is_empty() {
                    Ok((Ctrl::Val(val), next.clone()))
                } else {
                    cek_seq(body, env.clone(), next.clone())
                }
            } else {
                cek_cond(rest, env.clone(), next.clone())
            }
        }
        Kont::DynWindAfterIn { body_thunk, entry, next } => {
            winders.push(entry.clone());
            Ok((Ctrl::Apply(body_thunk.clone(), vec![]),
                Rc::new(Kont::DynWindAfterBody {
                    entry: entry.clone(),
                    next: next.clone(),
                })))
        }
        Kont::DynWindAfterBody { entry, next } => {
            winders.pop();
            Ok((Ctrl::Apply(entry.1.clone(), vec![]),
                Rc::new(Kont::DynWindAfterOut {
                    result: val,
                    next: next.clone(),
                })))
        }
        Kont::DynWindAfterOut { result, next } => {
            Ok((Ctrl::Val(result.clone()), next.clone()))
        }
        Kont::WindShift { ops, val: wind_val, saved_k } => {
            // Ignore thunk return value; process next wind operation
            if ops.is_empty() {
                Ok((Ctrl::Val(wind_val.clone()), saved_k.clone()))
            } else {
                let (is_rewind, entry) = ops[0].clone();
                let remaining = ops[1..].to_vec();
                let next_k = Rc::new(Kont::WindShift {
                    ops: remaining,
                    val: wind_val.clone(),
                    saved_k: saved_k.clone(),
                });
                if is_rewind {
                    winders.push(entry.clone());
                    Ok((Ctrl::Apply(entry.0.clone(), vec![]), next_k))
                } else {
                    winders.pop();
                    Ok((Ctrl::Apply(entry.1.clone(), vec![]), next_k))
                }
            }
        }
        Kont::PopHandler { next } => {
            handlers.pop();
            Ok((Ctrl::Val(val), next.clone()))
        }
        Kont::RaiseReturn => {
            Err(EvalError::Type("raise: exception handler returned".into()))
        }
        Kont::GuardTest { exn, body, rest_clauses, guard_env, guard_k, guard_winders } => {
            if is_truthy(&val) {
                guard_clause_matched(body, guard_env, guard_k, guard_winders, winders)
            } else {
                start_guard_clause(exn, rest_clauses, guard_env, guard_k, guard_winders, winders)
            }
        }
        Kont::GuardBody { body, env, next } => {
            cek_seq(body, env.clone(), next.clone())
        }
        Kont::Cwv { consumer, next } => {
            let args = match val {
                Value::Values(vals) => vals,
                other => vec![other],
            };
            Ok((Ctrl::Apply(consumer.clone(), args), next.clone()))
        }
        Kont::CondArrow { test_val, next } => {
            Ok((Ctrl::Apply(val, vec![test_val.clone()]), next.clone()))
        }
    }
}

fn cek_step_apply(func: Value, args: Vec<Value>, k: Rc<Kont>, output: &mut String, winders: &mut Vec<Rc<(Value, Value)>>, handlers: &mut Vec<Handler>) -> Result<(Ctrl, Rc<Kont>), EvalError> {
    match func {
        Value::Procedure(ref params, ref rest, ref body, ref closure_env) => {
            if rest.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let mut new_env = closure_env.clone();
            let frame = new_frame();
            for (p, a) in params.iter().zip(args.iter()) {
                frame.borrow_mut().insert(p.clone(), a.clone());
            }
            if let Some(rest_name) = rest {
                let rest_args = args[params.len()..].to_vec();
                frame.borrow_mut().insert(rest_name.clone(), Value::List(rest_args));
            }
            new_env.push(frame);
            if body.is_empty() {
                Ok((Ctrl::Val(Value::Boolean(false)), k))
            } else {
                cek_seq(body, new_env, k)
            }
        }
        Value::CaseLambda(ref clauses) => {
            for (params, rest, body, closure_env) in clauses {
                let matches = if rest.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let proc = Value::Procedure(params.clone(), rest.clone(), body.clone(), closure_env.clone());
                    return Ok((Ctrl::Apply(proc, args), k));
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments", args.len()
            )))
        }
        Value::Builtin(ref name) => {
            if is_callcc(name) {
                if args.len() != 1 {
                    return Err(EvalError::Arity("call/cc requires 1 argument".into()));
                }
                let cont_val = Value::Continuation(k.clone(), winders.clone());
                Ok((Ctrl::Apply(args[0].clone(), vec![cont_val]), k))
            } else if name == "dynamic-wind" {
                if args.len() != 3 {
                    return Err(EvalError::Arity("dynamic-wind requires 3 arguments".into()));
                }
                let in_thunk = args[0].clone();
                let body_thunk = args[1].clone();
                let out_thunk = args[2].clone();
                let entry = Rc::new((in_thunk.clone(), out_thunk));
                Ok((Ctrl::Apply(in_thunk, vec![]),
                    Rc::new(Kont::DynWindAfterIn {
                        body_thunk,
                        entry,
                        next: k,
                    })))
            } else if name == "raise" {
                if args.len() != 1 {
                    return Err(EvalError::Arity("raise requires 1 argument".into()));
                }
                if handlers.is_empty() {
                    return Err(EvalError::Type(format!("unhandled exception: {}", args[0])));
                }
                let handler = handlers.pop().expect("handlers checked non-empty above");
                match handler {
                    Handler::Proc(f) => {
                        Ok((Ctrl::Apply(f, args), Rc::new(Kont::RaiseReturn)))
                    }
                    Handler::Guard { var, clauses, env: guard_env, guard_k, guard_winders } => {
                        env_define(&guard_env, var, args[0].clone());
                        start_guard_clause(&args[0], &clauses, &guard_env, &guard_k, &guard_winders, winders)
                    }
                }
            } else if name == "with-exception-handler" {
                if args.len() != 2 {
                    return Err(EvalError::Arity("with-exception-handler requires 2 arguments".into()));
                }
                let handler = args[0].clone();
                let thunk = args[1].clone();
                handlers.push(Handler::Proc(handler));
                Ok((Ctrl::Apply(thunk, vec![]), Rc::new(Kont::PopHandler { next: k })))
            } else if name == "values" {
                if args.len() == 1 {
                    Ok((Ctrl::Val(args.into_iter().next().expect("values: len checked == 1")), k))
                } else {
                    Ok((Ctrl::Val(Value::Values(args)), k))
                }
            } else if name == "call-with-values" {
                if args.len() != 2 {
                    return Err(EvalError::Arity("call-with-values requires 2 arguments".into()));
                }
                let producer = args[0].clone();
                let consumer = args[1].clone();
                Ok((Ctrl::Apply(producer, vec![]), Rc::new(Kont::Cwv { consumer, next: k })))
            } else if name == "apply" {
                if args.len() < 2 {
                    return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
                }
                let func = args[0].clone();
                let last = collect_list(&args[args.len() - 1])
                    .ok_or_else(|| EvalError::Type("apply: last argument must be a list".into()))?;
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                all_args.extend(last);
                Ok((Ctrl::Apply(func, all_args), k))
            } else {
                let result = eval_builtin(name, &args, output)?;
                Ok((Ctrl::Val(result), k))
            }
        }
        Value::Continuation(saved_k, saved_winders) => {
            let val = if args.len() == 1 {
                args[0].clone()
            } else {
                Value::Values(args)
            };
            // Find common prefix of current and saved winders
            let common_len = winders.iter().zip(saved_winders.iter())
                .take_while(|(a, b)| Rc::ptr_eq(a, b))
                .count();
            // Build wind shift operations
            let mut ops: Vec<(bool, Rc<(Value, Value)>)> = Vec::new();
            // Unwind: from top of current down to common prefix
            for i in (common_len..winders.len()).rev() {
                ops.push((false, winders[i].clone()));
            }
            // Rewind: from common prefix up to top of target
            for winder in saved_winders.iter().skip(common_len) {
                ops.push((true, winder.clone()));
            }
            if ops.is_empty() {
                Ok((Ctrl::Val(val), saved_k))
            } else {
                let (is_rewind, entry) = ops[0].clone();
                let remaining = ops[1..].to_vec();
                let next_k = Rc::new(Kont::WindShift {
                    ops: remaining,
                    val,
                    saved_k,
                });
                if is_rewind {
                    winders.push(entry.clone());
                    Ok((Ctrl::Apply(entry.0.clone(), vec![]), next_k))
                } else {
                    winders.pop();
                    Ok((Ctrl::Apply(entry.1.clone(), vec![]), next_k))
                }
            }
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func}"))),
    }
}

pub(crate) fn cek_run(exprs: Vec<Expr>, env: Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let halt = Rc::new(Kont::Halt);
    let (mut ctrl, mut k) = if exprs.len() == 1 {
        (Ctrl::Eval(exprs.into_iter().next().expect("non-empty checked above"), env), halt)
    } else {
        let mut iter = exprs.into_iter();
        let first = iter.next().expect("non-empty checked above");
        let rest: Vec<Expr> = iter.collect();
        let k = Rc::new(Kont::Seq { rest, env: env.clone(), next: halt });
        (Ctrl::Eval(first, env), k)
    };
    let mut last_span = Span::default();
    let mut winders: Vec<Rc<(Value, Value)>> = Vec::new();
    let mut handlers: Vec<Handler> = Vec::new();

    loop {
        if let Ctrl::Val(ref v) = ctrl {
            if matches!(&*k, Kont::Halt) {
                return Ok(v.clone());
            }
        }
        let (new_ctrl, new_k) = match ctrl {
            Ctrl::Eval(ref expr, _) => {
                last_span = expr.span;
                let Ctrl::Eval(expr, env) = ctrl else { unreachable!() };
                cek_step_eval(expr, env, k, output, &winders, &mut handlers)?
            }
            Ctrl::Val(val) => cek_step_val(val, k, output, &mut winders, &mut handlers)?,
            Ctrl::Apply(func, args) => cek_step_apply(func, args, k, output, &mut winders, &mut handlers)
                .map_err(|e| with_span(last_span, e))?,
        };
        ctrl = new_ctrl;
        k = new_k;
    }
}
