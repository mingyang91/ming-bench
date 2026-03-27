use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::{Value, ContData};

pub type Output = Rc<RefCell<String>>;

// ---------------------------------------------------------------------------
// dynamic-wind support: winder stack
// ---------------------------------------------------------------------------

type Winders = Vec<(usize, Value, Value)>; // (id, in_thunk, out_thunk)

static WINDER_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
fn next_winder_id() -> usize {
    WINDER_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

fn winder_common_prefix(a: &Winders, b: &Winders) -> usize {
    a.iter().zip(b.iter()).take_while(|(x, y)| x.0 == y.0).count()
}

// ---------------------------------------------------------------------------
// Continuation (explicit frame stack for the CEK machine)
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub(crate) enum Cont {
    Halt,
    EvalArgs {
        evaluated: Vec<Value>,
        remaining: Vec<Value>,
        all_exprs: Vec<Value>,  // original unevaluated [func, arg1, arg2, ...]
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    Seq {
        remaining: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    If {
        then_expr: Value,
        else_expr: Option<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    Define {
        name: String,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    SetBang {
        name: String,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    And {
        remaining: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    Or {
        remaining: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    CondTest {
        body: Vec<Value>,
        remaining_clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    When {
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    CaseKey {
        clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    LetInit {
        name: String,
        remaining: Vec<(String, Value)>,
        local_env: Rc<RefCell<Env>>,
        eval_env: Rc<RefCell<Env>>,
        body: Vec<Value>,
        next: Rc<Cont>,
    },
    LetStarInit {
        name: String,
        remaining: Vec<(String, Value)>,
        local_env: Rc<RefCell<Env>>,
        body: Vec<Value>,
        next: Rc<Cont>,
    },
    LetrecCollect {
        names: Vec<String>,
        collected: Vec<Value>,
        remaining_inits: Vec<Value>,
        local_env: Rc<RefCell<Env>>,
        body: Vec<Value>,
        next: Rc<Cont>,
    },
    CallCC {
        next: Rc<Cont>,
    },
    /// Re-evaluating args frame: used by captured continuations so that
    /// sibling arguments to call/cc are re-evaluated (getting current env values).
    ReEvalArgs {
        func_expr: Value,
        pre_exprs: Vec<Value>,   // arg expressions before the call/cc position
        post_exprs: Vec<Value>,  // arg expressions after the call/cc position
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    /// dynamic-wind: in-thunk just evaluated, push winder, call body-thunk
    DynWindIn {
        in_thunk: Value,
        body_thunk: Value,
        out_thunk: Value,
        next: Rc<Cont>,
    },
    /// dynamic-wind: body-thunk returned, pop winder, call out-thunk
    DynWindBody {
        out_thunk: Value,
        next: Rc<Cont>,
    },
    /// dynamic-wind: out-thunk returned, deliver saved body value
    DynWindOut {
        body_val: Value,
        next: Rc<Cont>,
    },
    /// Chain of thunks to call during continuation unwind/rewind
    DynWindDoThunks {
        thunks: Vec<Value>,
        final_val: Value,
        final_cont: Rc<Cont>,
        final_winders: Winders,
        final_handlers: Handlers,
    },
    /// raise: argument evaluated, now invoke handler
    RaiseVal {
        next: Rc<Cont>,
    },
    /// guard: exception delivered (via continuation), test clauses
    Guard {
        var: String,
        clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
        next: Rc<Cont>,
    },
    /// Pop exception handler after thunk/body completes normally
    PopHandler {
        next: Rc<Cont>,
    },
    /// call-with-values: producer returned, now apply consumer
    CallWithValues {
        consumer: Value,
        next: Rc<Cont>,
    },
}

enum CekAction {
    Value(Value),
    Eval,
}

/// Transform a captured continuation so that the nearest EvalArgs frame
/// re-evaluates its arguments (instead of using baked-in values).
/// This is needed for reentrant continuations where sibling arguments
/// to call/cc should see the current environment state.
fn transform_cont_for_capture(k: &Rc<Cont>) -> Rc<Cont> {
    match &**k {
        Cont::EvalArgs { evaluated, all_exprs, env, next, .. } => {
            // `evaluated` has [func_val, arg0_val, ...] that were already computed.
            // The call/cc result will be at position evaluated.len() in all_exprs.
            let callcc_idx = evaluated.len(); // position of the call/cc arg in all_exprs
            Rc::new(Cont::ReEvalArgs {
                func_expr: all_exprs[0].clone(),
                pre_exprs: all_exprs[1..callcc_idx].to_vec(),
                post_exprs: if callcc_idx + 1 < all_exprs.len() {
                    all_exprs[callcc_idx + 1..].to_vec()
                } else {
                    vec![]
                },
                env: Rc::clone(env),
                next: Rc::clone(next),
            })
        }
        // For other frames, the continuation is fine as-is
        _ => Rc::clone(k),
    }
}

type Handlers = Vec<Value>;
type CapturedContData = (Rc<Cont>, Winders, Handlers);

// Helper: wrap Rc<Cont> + winders + handlers into Value::Continuation
fn cont_to_value(k: &Rc<Cont>, winders: &Winders, handlers: &Handlers) -> Value {
    let data: CapturedContData = (Rc::clone(k), winders.clone(), handlers.clone());
    let boxed: Rc<dyn std::any::Any> = Rc::new(data);
    Value::Continuation(ContData(boxed))
}

// Helper: extract Rc<Cont> + winders + handlers from Value::Continuation
fn value_to_cont(v: &Value) -> (Rc<Cont>, Winders, Handlers) {
    match v {
        Value::Continuation(cd) => {
            cd.0.downcast_ref::<CapturedContData>().expect("invalid continuation data").clone()
        }
        _ => panic!("not a continuation"),
    }
}

// ---------------------------------------------------------------------------
// Public eval interface
// ---------------------------------------------------------------------------

pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    cek_eval(expr.clone(), Rc::clone(env), out, Rc::new(Cont::Halt))
}

/// Evaluate a sequence of expressions in a single CEK machine pass.
/// Continuations captured by call/cc span the entire sequence.
pub fn eval_sequence(exprs: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let mut kont = Rc::new(Cont::Halt);
    if exprs.len() > 1 {
        kont = Rc::new(Cont::Seq {
            remaining: exprs[1..].to_vec(),
            env: Rc::clone(env),
            next: kont,
        });
    }
    cek_eval(exprs[0].clone(), Rc::clone(env), out, kont)
}

// ---------------------------------------------------------------------------
// apply — for use by builtins (map, for-each, etc.)
// ---------------------------------------------------------------------------

fn apply(func: &Value, args: &[Value], out: &Output) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = setup_lambda_env(params, rest_param, args, env)?;
            eval_body(body, &local_env, out)
        }
        Value::CaseLambda { clauses } => {
            for (params, rest_param, body, cenv) in clauses {
                let ok = if rest_param.is_some() { args.len() >= params.len() } else { args.len() == params.len() };
                if ok {
                    let local_env = setup_lambda_env(params, rest_param, args, cenv)?;
                    return eval_body(body, &local_env, out);
                }
            }
            Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", args.len())))
        }
        Value::RecordProc { type_id, kind } => {
            apply_record_proc(*type_id, kind, args)
        }
        Value::Symbol(op) | Value::Builtin(op) => {
            if op == "apply" {
                return builtin_apply(args, out);
            }
            apply_builtin(op, args, out)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
    }
}

fn eval_body(body: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if body.is_empty() { return Ok(Value::Void); }
    let mut kont = Rc::new(Cont::Halt);
    if body.len() > 1 {
        kont = Rc::new(Cont::Seq {
            remaining: body[1..].to_vec(),
            env: Rc::clone(env),
            next: kont,
        });
    }
    cek_eval(body[0].clone(), Rc::clone(env), out, kont)
}

fn builtin_apply(args: &[Value], out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail = value_to_vec(last)?;
    let mut full_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    full_args.extend(tail);
    apply(func, &full_args, out)
}

fn apply_record_proc(type_id: usize, kind: &crate::scheme::value::RecordProcKind, args: &[Value]) -> Result<Value, EvalError> {
    use crate::scheme::value::RecordProcKind;
    match kind {
        RecordProcKind::Constructor { field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "record constructor expected {} arguments, got {}", field_names.len(), args.len()
                )));
            }
            Ok(Value::Record { type_id, fields: args.to_vec() })
        }
        RecordProcKind::Predicate => {
            if args.len() != 1 { return Err(EvalError::Arity("record predicate requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id)))
        }
        RecordProcKind::Accessor { field_index } => {
            if args.len() != 1 { return Err(EvalError::Arity("record accessor requires 1 argument".into())); }
            match &args[0] {
                Value::Record { type_id: tid, fields } if *tid == type_id => Ok(fields[*field_index].clone()),
                _ => Err(EvalError::Type("record accessor: wrong record type".into())),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// apply_cek — apply a function inside the CEK machine
// Returns CekAction::Value(v) to stay in the apply phase,
//         CekAction::Eval when ctrl/env/kont have been set for the eval phase.
// ---------------------------------------------------------------------------

fn apply_cek(
    func: &Value,
    args: &[Value],
    out: &Output,
    ctrl: &mut Value,
    env: &mut Rc<RefCell<Env>>,
    kont: &mut Rc<Cont>,
    winders: &mut Winders,
    handlers: &mut Handlers,
) -> Result<CekAction, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env: lenv } => {
            let local_env = setup_lambda_env(params, rest_param, args, lenv)?;
            if body.is_empty() { return Ok(CekAction::Value(Value::Void)); }
            if body.len() > 1 {
                *kont = Rc::new(Cont::Seq {
                    remaining: body[1..].to_vec(),
                    env: Rc::clone(&local_env),
                    next: Rc::clone(kont),
                });
            }
            *ctrl = body[0].clone();
            *env = local_env;
            Ok(CekAction::Eval)
        }
        Value::CaseLambda { clauses } => {
            for (params, rest_param, body, cenv) in clauses {
                let ok = if rest_param.is_some() { args.len() >= params.len() } else { args.len() == params.len() };
                if ok {
                    let local_env = setup_lambda_env(params, rest_param, args, cenv)?;
                    if body.is_empty() { return Ok(CekAction::Value(Value::Void)); }
                    if body.len() > 1 {
                        *kont = Rc::new(Cont::Seq {
                            remaining: body[1..].to_vec(),
                            env: Rc::clone(&local_env),
                            next: Rc::clone(kont),
                        });
                    }
                    *ctrl = body[0].clone();
                    *env = local_env;
                    return Ok(CekAction::Eval);
                }
            }
            Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", args.len())))
        }
        Value::RecordProc { type_id, kind } => {
            Ok(CekAction::Value(apply_record_proc(*type_id, kind, args)?))
        }
        Value::Continuation(_) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into()));
            }
            let (target_cont, target_winders, target_handlers) = value_to_cont(func);
            let target_val = args[0].clone();

            let common = winder_common_prefix(winders, &target_winders);

            // Build thunk sequence: unwind current (out-thunks, innermost first),
            // then rewind target (in-thunks, outermost first)
            let mut thunks = Vec::new();
            for i in (common..winders.len()).rev() {
                thunks.push(winders[i].2.clone());
            }
            for i in common..target_winders.len() {
                thunks.push(target_winders[i].1.clone());
            }

            if thunks.is_empty() {
                *kont = target_cont;
                *winders = target_winders;
                *handlers = target_handlers;
                Ok(CekAction::Value(target_val))
            } else {
                let first = thunks.remove(0);
                *kont = Rc::new(Cont::DynWindDoThunks {
                    thunks,
                    final_val: target_val,
                    final_cont: target_cont,
                    final_winders: target_winders,
                    final_handlers: target_handlers,
                });
                apply_cek(&first, &[], out, ctrl, env, kont, winders, handlers)
            }
        }
        Value::Symbol(op) | Value::Builtin(op) => {
            if op == "apply" {
                if args.len() < 2 {
                    return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
                }
                let inner = &args[0];
                let last = &args[args.len() - 1];
                let tail = value_to_vec(last)?;
                let mut full: Vec<Value> = args[1..args.len() - 1].to_vec();
                full.extend(tail);
                return apply_cek(inner, &full, out, ctrl, env, kont, winders, handlers);
            }
            if op == "call/cc" || op == "call-with-current-continuation" {
                // First-class call/cc: (apply call/cc (list f)) or similar
                if args.len() != 1 {
                    return Err(EvalError::Arity("call/cc requires 1 argument".into()));
                }
                let continuation = cont_to_value(kont, winders, handlers);
                return apply_cek(&args[0], &[continuation], out, ctrl, env, kont, winders, handlers);
            }
            Ok(CekAction::Value(apply_builtin(op, args, out)?))
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
    }
}

// ---------------------------------------------------------------------------
// Fast-path helper: could an expression invoke a continuation?
// Any function call `(f ...)` might transitively invoke a captured
// continuation, so we must use the slow (continuation-aware) path for
// those.  Only lambda/case-lambda/quote are known safe.
// ---------------------------------------------------------------------------

fn may_contain_callcc(expr: &Value) -> bool {
    match expr {
        Value::Symbol(s) => s == "call/cc" || s == "call-with-current-continuation",
        Value::List(elems) if !elems.is_empty() => {
            if let Value::Symbol(s) = &elems[0] {
                match s.as_str() {
                    "lambda" | "case-lambda" | "quote" => return false,
                    _ => {}
                }
            }
            // Any function call could invoke a continuation — use slow path
            true
        }
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// CEK machine — main evaluation loop
// ---------------------------------------------------------------------------

fn cek_eval(
    init_ctrl: Value,
    init_env: Rc<RefCell<Env>>,
    out: &Output,
    init_kont: Rc<Cont>,
) -> Result<Value, EvalError> {
    let mut ctrl = init_ctrl;
    let mut env = init_env;
    let mut kont = init_kont;
    let mut winders: Winders = Vec::new();
    let mut handlers: Handlers = Vec::new();

    loop {
        // ===== EVAL PHASE: reduce ctrl to a value =====
        // Take ctrl by value to avoid deep-cloning expression trees.
        let mut val = loop {
            match std::mem::replace(&mut ctrl, Value::Void) {
                Value::Symbol(name) => break env.borrow().get(&name)?,
                Value::List(mut elems) => {
                    if elems.is_empty() {
                        return Err(EvalError::Parse("empty application".into()));
                    }
                    // Clone just the operator name (cheap) so we can move from elems later.
                    let op_str: Option<String> = match &elems[0] {
                        Value::Symbol(s) => Some(s.clone()),
                        _ => None,
                    };
                    if let Some(ref op) = op_str {
                        match op.as_str() {
                            // ---- immediate forms ----
                            "quote" => {
                                if elems.len() != 2 { return Err(EvalError::Arity("quote requires 1 argument".into())); }
                                break elems.swap_remove(1);
                            }
                            "lambda" => break eval_lambda(&elems[1..], &env)?,
                            "case-lambda" => break eval_case_lambda(&elems[1..], &env)?,
                            "define-record-type" => break eval_define_record_type(&elems[1..], &env)?,

                            // ---- syntax template (like quote but with substitution) ----
                            "syntax" => {
                                if elems.len() != 2 { return Err(EvalError::Arity("syntax requires 1 argument".into())); }
                                let template = elems.swap_remove(1);
                                let empty_env = Env::new();
                                let result = SYNTAX_ENV.with(|se| {
                                    let stack = se.borrow();
                                    if let Some(bindings) = stack.last() {
                                        instantiate_template(&template, bindings, &empty_env)
                                    } else {
                                        Ok(template)
                                    }
                                })?;
                                break result;
                            }

                            // ---- delegated forms (use recursive eval) ----
                            "syntax-case" => break eval_syntax_case(&elems[1..], &env, out)?,
                            "with-syntax" => break eval_with_syntax(&elems[1..], &env, out)?,
                            "define-syntax" => break eval_define_syntax(&elems[1..], &env, out)?,
                            "string-set!" => break eval_string_set(&elems[1..], &env, out)?,
                            "vector-set!" => break eval_vector_set(&elems[1..], &env, out)?,
                            "do" => break eval_do(&elems[1..], &env, out)?,

                            // ---- define ----
                            "define" => {
                                if elems.len() < 2 { return Err(EvalError::Arity("define requires arguments".into())); }
                                let first = elems.remove(1);
                                match first {
                                    Value::Symbol(name) => {
                                        if elems.len() != 2 { return Err(EvalError::Arity("define requires 2 arguments".into())); }
                                        kont = Rc::new(Cont::Define { name, env: Rc::clone(&env), next: kont });
                                        ctrl = elems.pop().unwrap();
                                        continue;
                                    }
                                    Value::List(sig) => {
                                        if sig.is_empty() { return Err(EvalError::Parse("define: empty signature".into())); }
                                        let name = match &sig[0] {
                                            Value::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Type("define: expected symbol as name".into())),
                                        };
                                        let (params, rest_param) = parse_params(&sig[1..])?;
                                        let body: Vec<Value> = elems.drain(1..).collect();
                                        let lambda = Value::Lambda { params, rest_param, body, env: Rc::clone(&env) };
                                        env.borrow_mut().set(name, lambda);
                                        break Value::Void;
                                    }
                                    _ => return Err(EvalError::Type("define: expected symbol or list".into())),
                                }
                            }

                            // ---- set! ----
                            "set!" => {
                                if elems.len() != 3 { return Err(EvalError::Arity("set! requires 2 arguments".into())); }
                                let name = match &elems[1] {
                                    Value::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Type("set!: first argument must be a symbol".into())),
                                };
                                kont = Rc::new(Cont::SetBang { name, env: Rc::clone(&env), next: kont });
                                ctrl = elems.pop().unwrap();
                                continue;
                            }

                            // ---- if (optimized: move elements instead of cloning) ----
                            "if" => {
                                let nargs = elems.len() - 1;
                                if nargs < 2 || nargs > 3 { return Err(EvalError::Arity("if requires 2 or 3 arguments".into())); }
                                let else_expr = if elems.len() > 3 { Some(elems.pop().unwrap()) } else { None };
                                let then_expr = elems.pop().unwrap();
                                let test = elems.pop().unwrap();
                                kont = Rc::new(Cont::If {
                                    then_expr,
                                    else_expr,
                                    env: Rc::clone(&env),
                                    next: kont,
                                });
                                ctrl = test;
                                continue;
                            }

                            // ---- begin ----
                            "begin" => {
                                elems.remove(0);
                                if elems.is_empty() { break Value::Void; }
                                let first = elems.remove(0);
                                if !elems.is_empty() {
                                    kont = Rc::new(Cont::Seq { remaining: elems, env: Rc::clone(&env), next: kont });
                                }
                                ctrl = first;
                                continue;
                            }

                            // ---- cond ----
                            "cond" => {
                                let clauses = &elems[1..];
                                match start_cond(clauses, &env, &mut kont)? {
                                    CondStart::Immediate(v) => break v,
                                    CondStart::Eval(c) => { ctrl = c; continue; }
                                }
                            }

                            // ---- and ----
                            "and" => {
                                elems.remove(0);
                                if elems.is_empty() { break Value::Boolean(true); }
                                let first = elems.remove(0);
                                if !elems.is_empty() {
                                    kont = Rc::new(Cont::And { remaining: elems, env: Rc::clone(&env), next: kont });
                                }
                                ctrl = first;
                                continue;
                            }

                            // ---- or ----
                            "or" => {
                                elems.remove(0);
                                if elems.is_empty() { break Value::Boolean(false); }
                                let first = elems.remove(0);
                                if !elems.is_empty() {
                                    kont = Rc::new(Cont::Or { remaining: elems, env: Rc::clone(&env), next: kont });
                                }
                                ctrl = first;
                                continue;
                            }

                            // ---- when ----
                            "when" => {
                                if elems.len() < 3 { return Err(EvalError::Arity("when requires test and body".into())); }
                                let test = elems.remove(1);
                                elems.remove(0);
                                kont = Rc::new(Cont::When { body: elems, env: Rc::clone(&env), next: kont });
                                ctrl = test;
                                continue;
                            }

                            // ---- case ----
                            "case" => {
                                if elems.len() < 2 { return Err(EvalError::Arity("case requires key and clauses".into())); }
                                let key = elems.remove(1);
                                elems.remove(0);
                                kont = Rc::new(Cont::CaseKey { clauses: elems, env: Rc::clone(&env), next: kont });
                                ctrl = key;
                                continue;
                            }

                            // ---- let ----
                            "let" => {
                                let args = &elems[1..];
                                if args.len() < 2 { return Err(EvalError::Arity("let requires bindings and body".into())); }
                                if let Value::Symbol(name) = &args[0] {
                                    match start_named_let(name, &args[1..], &env, &mut kont)? {
                                        Some((c, new_env)) => { ctrl = c; env = new_env; continue; }
                                        None => break Value::Void,
                                    }
                                }
                                match start_let(&args[0], &args[1..], &env, &mut kont)? {
                                    Some(c) => { ctrl = c; continue; }
                                    None => break Value::Void,
                                }
                            }

                            // ---- let* ----
                            "let*" => {
                                let args = &elems[1..];
                                if args.len() < 2 { return Err(EvalError::Arity("let* requires bindings and body".into())); }
                                match start_let_star(&args[0], &args[1..], &env, &mut kont)? {
                                    Some(c) => { ctrl = c; continue; }
                                    None => break Value::Void,
                                }
                            }

                            // ---- letrec ----
                            "letrec" => {
                                let args = &elems[1..];
                                if args.len() < 2 { return Err(EvalError::Arity("letrec requires bindings and body".into())); }
                                match start_letrec(&args[0], &args[1..], &env, &mut kont)? {
                                    Some(c) => { ctrl = c; env = letrec_env_from_kont(&kont); continue; }
                                    None => break Value::Void,
                                }
                            }

                            // ---- letrec* ----
                            "letrec*" => {
                                let args = &elems[1..];
                                if args.len() < 2 { return Err(EvalError::Arity("letrec* requires bindings and body".into())); }
                                match start_letrec_star(&args[0], &args[1..], &env, &mut kont)? {
                                    Some((c, new_env)) => { ctrl = c; env = new_env; continue; }
                                    None => break Value::Void,
                                }
                            }

                            // ---- call/cc ----
                            "call/cc" | "call-with-current-continuation" => {
                                if elems.len() != 2 { return Err(EvalError::Arity("call/cc requires 1 argument".into())); }
                                kont = Rc::new(Cont::CallCC { next: kont });
                                ctrl = elems.swap_remove(1);
                                continue;
                            }

                            // ---- dynamic-wind ----
                            "dynamic-wind" => {
                                if elems.len() != 4 {
                                    return Err(EvalError::Arity("dynamic-wind requires 3 arguments".into()));
                                }
                                let in_thunk = eval(&elems[1], &env, out)?;
                                let body_thunk = eval(&elems[2], &env, out)?;
                                let out_thunk = eval(&elems[3], &env, out)?;
                                kont = Rc::new(Cont::DynWindIn {
                                    in_thunk: in_thunk.clone(),
                                    body_thunk,
                                    out_thunk,
                                    next: kont,
                                });
                                match apply_cek(&in_thunk, &[], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                                    CekAction::Value(v) => break v,
                                    CekAction::Eval => continue,
                                }
                            }

                            // ---- raise ----
                            "raise" if env.borrow().get("raise").is_err() => {
                                if elems.len() != 2 { return Err(EvalError::Arity("raise requires 1 argument".into())); }
                                kont = Rc::new(Cont::RaiseVal { next: kont });
                                ctrl = elems.swap_remove(1);
                                continue;
                            }

                            // ---- guard ----
                            "guard" => {
                                if elems.len() < 3 { return Err(EvalError::Arity("guard requires clauses and body".into())); }
                                let header = match &elems[1] {
                                    Value::List(parts) if !parts.is_empty() => parts.clone(),
                                    _ => return Err(EvalError::Type("guard: expected (var clause ...) ".into())),
                                };
                                let var = match &header[0] {
                                    Value::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Type("guard: expected symbol".into())),
                                };
                                let clauses: Vec<Value> = header[1..].to_vec();
                                let body: Vec<Value> = elems[2..].to_vec();

                                // Guard frame: receives exception value, tests clauses
                                let guard_frame = Rc::new(Cont::Guard {
                                    var,
                                    clauses,
                                    env: Rc::clone(&env),
                                    next: Rc::clone(&kont),
                                });

                                // Capture continuation at Guard frame as exception handler
                                let captured = cont_to_value(&guard_frame, &winders, &handlers);
                                handlers.push(captured);

                                // PopHandler delivers normal return directly to kont (skipping Guard)
                                kont = Rc::new(Cont::PopHandler { next: kont });

                                // Evaluate body
                                if body.len() > 1 {
                                    kont = Rc::new(Cont::Seq {
                                        remaining: body[1..].to_vec(),
                                        env: Rc::clone(&env),
                                        next: kont,
                                    });
                                }
                                ctrl = body[0].clone();
                                continue;
                            }

                            // ---- with-exception-handler ----
                            "with-exception-handler" if env.borrow().get("with-exception-handler").is_err() => {
                                if elems.len() != 3 {
                                    return Err(EvalError::Arity("with-exception-handler requires 2 arguments".into()));
                                }
                                let handler = eval(&elems[1], &env, out)?;
                                let thunk = eval(&elems[2], &env, out)?;
                                handlers.push(handler);
                                kont = Rc::new(Cont::PopHandler { next: kont });
                                match apply_cek(&thunk, &[], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                                    CekAction::Value(v) => break v,
                                    CekAction::Eval => continue,
                                }
                            }

                            // ---- call-with-values ----
                            "call-with-values" => {
                                if elems.len() != 3 {
                                    return Err(EvalError::Arity("call-with-values requires 2 arguments".into()));
                                }
                                let producer = eval(&elems[1], &env, out)?;
                                let consumer = eval(&elems[2], &env, out)?;
                                kont = Rc::new(Cont::CallWithValues {
                                    consumer,
                                    next: kont,
                                });
                                match apply_cek(&producer, &[], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                                    CekAction::Value(v) => break v,
                                    CekAction::Eval => continue,
                                }
                            }

                            // ---- macro / fall-through ----
                            _ => {
                                let maybe_macro = env.borrow().get(op).ok();
                                if let Some(Value::SyntaxRules { ref literals, ref rules, ref def_env }) = maybe_macro {
                                    ctrl = expand_macro(literals, rules, def_env, &elems)?;
                                    continue;
                                }
                                if let Some(Value::MacroTransformer(ref transformer)) = maybe_macro {
                                    let input_form = Value::List(elems);
                                    let expanded = apply(transformer, &[input_form], out)?;
                                    ctrl = expanded;
                                    continue;
                                }
                            }
                        }
                    }

                    // --- function application ---
                    // Fast path: when no argument contains a literal call/cc,
                    // evaluate args eagerly and inline Lambda/builtin apply.
                    if !elems.iter().any(|e| may_contain_callcc(e)) {
                        let func_val = match elems.remove(0) {
                            Value::Symbol(s) => env.borrow().get(&s)?,
                            v if v.is_self_evaluating() => v,
                            v => eval(&v, &env, out)?,
                        };
                        let mut args = Vec::with_capacity(elems.len());
                        for arg_expr in elems {
                            args.push(match arg_expr {
                                Value::Symbol(s) => env.borrow().get(&s)?,
                                v if v.is_self_evaluating() => v,
                                v => eval(&v, &env, out)?,
                            });
                        }
                        // Inline Lambda to move body (no clone)
                        match func_val {
                            Value::Lambda { params, rest_param, mut body, env: lenv } => {
                                let local_env = setup_lambda_env(&params, &rest_param, &args, &lenv)?;
                                if body.is_empty() { break Value::Void; }
                                if body.len() > 1 {
                                    kont = Rc::new(Cont::Seq {
                                        remaining: body.split_off(1),
                                        env: Rc::clone(&local_env),
                                        next: kont,
                                    });
                                }
                                ctrl = body.into_iter().next().unwrap();
                                env = local_env;
                                continue;
                            }
                            Value::Symbol(ref op) | Value::Builtin(ref op) if op != "apply" && op != "call/cc" && op != "call-with-current-continuation" => {
                                break apply_builtin(op, &args, out)?;
                            }
                            other => {
                                match apply_cek(&other, &args, out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                                    CekAction::Value(v) => break v,
                                    CekAction::Eval => continue,
                                }
                            }
                        }
                    }
                    // Slow path: call/cc may be present — use continuation frames.
                    ctrl = elems[0].clone();
                    kont = Rc::new(Cont::EvalArgs {
                        evaluated: vec![],
                        remaining: elems[1..].to_vec(),
                        all_exprs: elems,
                        env: Rc::clone(&env),
                        next: kont,
                    });
                    continue;
                }
                // --- self-evaluating or Void ---
                other => break other,
            }
        };

        // ===== APPLY PHASE: deliver val to the current continuation =====
        loop {
            let k = Rc::clone(&kont);
            match &*k {
                Cont::Halt => return Ok(std::mem::replace(&mut val, Value::Void)),

                // --- EvalArgs ---
                Cont::EvalArgs { evaluated, remaining, all_exprs, env: eenv, next } => {
                    let mut ev = evaluated.clone();
                    ev.push(std::mem::replace(&mut val, Value::Void));
                    if remaining.is_empty() {
                        let func = ev.remove(0);
                        let args = ev;
                        kont = Rc::clone(next);
                        match apply_cek(&func, &args, out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                            CekAction::Value(v) => { val = v; continue; }
                            CekAction::Eval => { break; }
                        }
                    } else {
                        let next_arg = remaining[0].clone();
                        kont = Rc::new(Cont::EvalArgs {
                            evaluated: ev,
                            remaining: remaining[1..].to_vec(),
                            all_exprs: all_exprs.clone(),
                            env: Rc::clone(eenv),
                            next: Rc::clone(next),
                        });
                        ctrl = next_arg;
                        env = Rc::clone(eenv);
                        break;
                    }
                }

                // --- Seq ---
                Cont::Seq { remaining, env: senv, next } => {
                    if remaining.len() == 1 {
                        kont = Rc::clone(next);
                    } else {
                        kont = Rc::new(Cont::Seq {
                            remaining: remaining[1..].to_vec(),
                            env: Rc::clone(senv),
                            next: Rc::clone(next),
                        });
                    }
                    ctrl = remaining[0].clone();
                    env = Rc::clone(senv);
                    break;
                }

                // --- If ---
                Cont::If { then_expr, else_expr, env: ienv, next } => {
                    kont = Rc::clone(next);
                    if val.is_truthy() {
                        ctrl = then_expr.clone();
                    } else if let Some(e) = else_expr {
                        ctrl = e.clone();
                    } else {
                        val = Value::Void;
                        continue;
                    }
                    env = Rc::clone(ienv);
                    break;
                }

                // --- Define ---
                Cont::Define { name, env: denv, next } => {
                    denv.borrow_mut().set(name.clone(), std::mem::replace(&mut val, Value::Void));
                    kont = Rc::clone(next);
                    continue;
                }

                // --- SetBang ---
                Cont::SetBang { name, env: senv, next } => {
                    Env::set_existing(senv, name, std::mem::replace(&mut val, Value::Void))?;
                    kont = Rc::clone(next);
                    val = Value::Void;
                    continue;
                }

                // --- And ---
                Cont::And { remaining, env: aenv, next } => {
                    if !val.is_truthy() {
                        kont = Rc::clone(next);
                        continue; // short-circuit
                    }
                    if remaining.len() == 1 {
                        kont = Rc::clone(next);
                    } else {
                        kont = Rc::new(Cont::And {
                            remaining: remaining[1..].to_vec(),
                            env: Rc::clone(aenv),
                            next: Rc::clone(next),
                        });
                    }
                    ctrl = remaining[0].clone();
                    env = Rc::clone(aenv);
                    break;
                }

                // --- Or ---
                Cont::Or { remaining, env: oenv, next } => {
                    if val.is_truthy() {
                        kont = Rc::clone(next);
                        continue; // short-circuit
                    }
                    if remaining.len() == 1 {
                        kont = Rc::clone(next);
                    } else {
                        kont = Rc::new(Cont::Or {
                            remaining: remaining[1..].to_vec(),
                            env: Rc::clone(oenv),
                            next: Rc::clone(next),
                        });
                    }
                    ctrl = remaining[0].clone();
                    env = Rc::clone(oenv);
                    break;
                }

                // --- CondTest ---
                Cont::CondTest { body, remaining_clauses, env: cenv, next } => {
                    if val.is_truthy() {
                        if body.is_empty() {
                            kont = Rc::clone(next);
                            continue; // return test value
                        }
                        kont = Rc::clone(next);
                        if body.len() > 1 {
                            kont = Rc::new(Cont::Seq {
                                remaining: body[1..].to_vec(),
                                env: Rc::clone(cenv),
                                next: kont,
                            });
                        }
                        ctrl = body[0].clone();
                        env = Rc::clone(cenv);
                        break;
                    }
                    // false — try next clause
                    kont = Rc::clone(next);
                    match start_cond(remaining_clauses, cenv, &mut kont)? {
                        CondStart::Immediate(v) => { val = v; continue; }
                        CondStart::Eval(c) => { ctrl = c; env = Rc::clone(cenv); break; }
                    }
                }

                // --- When ---
                Cont::When { body, env: wenv, next } => {
                    if val.is_truthy() {
                        kont = Rc::clone(next);
                        if body.len() > 1 {
                            kont = Rc::new(Cont::Seq {
                                remaining: body[1..].to_vec(),
                                env: Rc::clone(wenv),
                                next: kont,
                            });
                        }
                        ctrl = body[0].clone();
                        env = Rc::clone(wenv);
                        break;
                    }
                    kont = Rc::clone(next);
                    val = Value::Void;
                    continue;
                }

                // --- CaseKey ---
                Cont::CaseKey { clauses, env: cenv, next } => {
                    let key = val;
                    let mut found_body: Option<Vec<Value>> = None;
                    for clause in clauses.iter() {
                        let parts = match clause {
                            Value::List(p) if !p.is_empty() => p,
                            _ => return Err(EvalError::Type("case: invalid clause".into())),
                        };
                        let matched = if let Value::Symbol(s) = &parts[0] {
                            s == "else"
                        } else {
                            match &parts[0] {
                                Value::List(d) => d.iter().any(|datum| eqv(&key, datum)),
                                _ => return Err(EvalError::Type("case: expected datum list".into())),
                            }
                        };
                        if matched {
                            found_body = Some(parts[1..].to_vec());
                            break;
                        }
                    }
                    kont = Rc::clone(next);
                    if let Some(body) = found_body {
                        if body.is_empty() {
                            val = Value::Void;
                            continue;
                        }
                        if body.len() > 1 {
                            kont = Rc::new(Cont::Seq {
                                remaining: body[1..].to_vec(),
                                env: Rc::clone(cenv),
                                next: kont,
                            });
                        }
                        ctrl = body[0].clone();
                        env = Rc::clone(cenv);
                        break;
                    }
                    val = Value::Void;
                    continue;
                }

                // --- LetInit ---
                Cont::LetInit { name, remaining, local_env, eval_env, body, next } => {
                    local_env.borrow_mut().set(name.clone(), std::mem::replace(&mut val, Value::Void));
                    if remaining.is_empty() {
                        kont = Rc::clone(next);
                        enter_body(body, local_env, &mut ctrl, &mut env, &mut kont, &mut val);
                        if matches!(ctrl, Value::Void) && body.is_empty() { continue; } else { break; }
                    }
                    let mut rem = remaining.clone();
                    let (nn, ni) = rem.remove(0);
                    kont = Rc::new(Cont::LetInit {
                        name: nn,
                        remaining: rem,
                        local_env: Rc::clone(local_env),
                        eval_env: Rc::clone(eval_env),
                        body: body.clone(),
                        next: Rc::clone(next),
                    });
                    ctrl = ni;
                    env = Rc::clone(eval_env);
                    break;
                }

                // --- LetStarInit ---
                Cont::LetStarInit { name, remaining, local_env, body, next } => {
                    local_env.borrow_mut().set(name.clone(), std::mem::replace(&mut val, Value::Void));
                    if remaining.is_empty() {
                        kont = Rc::clone(next);
                        enter_body(body, local_env, &mut ctrl, &mut env, &mut kont, &mut val);
                        if matches!(ctrl, Value::Void) && body.is_empty() { continue; } else { break; }
                    }
                    let mut rem = remaining.clone();
                    let (nn, ni) = rem.remove(0);
                    kont = Rc::new(Cont::LetStarInit {
                        name: nn,
                        remaining: rem,
                        local_env: Rc::clone(local_env),
                        body: body.clone(),
                        next: Rc::clone(next),
                    });
                    ctrl = ni;
                    env = Rc::clone(local_env);
                    break;
                }

                // --- LetrecCollect ---
                Cont::LetrecCollect { names, collected, remaining_inits, local_env, body, next } => {
                    let mut coll = collected.clone();
                    coll.push(std::mem::replace(&mut val, Value::Void));
                    if remaining_inits.is_empty() {
                        for (name, v) in names.iter().zip(coll) {
                            local_env.borrow_mut().set(name.clone(), v);
                        }
                        kont = Rc::clone(next);
                        enter_body(body, local_env, &mut ctrl, &mut env, &mut kont, &mut val);
                        if matches!(ctrl, Value::Void) && body.is_empty() { continue; } else { break; }
                    }
                    let next_init = remaining_inits[0].clone();
                    kont = Rc::new(Cont::LetrecCollect {
                        names: names.clone(),
                        collected: coll,
                        remaining_inits: remaining_inits[1..].to_vec(),
                        local_env: Rc::clone(local_env),
                        body: body.clone(),
                        next: Rc::clone(next),
                    });
                    ctrl = next_init;
                    env = Rc::clone(local_env);
                    break;
                }

                // --- CallCC ---
                Cont::CallCC { next } => {
                    let proc = std::mem::replace(&mut val, Value::Void);
                    let transformed = transform_cont_for_capture(next);
                    let captured = cont_to_value(&transformed, &winders, &handlers);
                    kont = Rc::clone(next);
                    match apply_cek(&proc, &[captured], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                        CekAction::Value(v) => { val = v; continue; }
                        CekAction::Eval => { break; }
                    }
                }

                // --- ReEvalArgs (used by captured continuations) ---
                Cont::ReEvalArgs { func_expr, pre_exprs, post_exprs, env: renv, next } => {
                    let callcc_val = std::mem::replace(&mut val, Value::Void);
                    // Re-evaluate the function
                    let func_val = eval(func_expr, renv, out)?;
                    // Re-evaluate pre-callcc args
                    let mut args = Vec::new();
                    for expr in pre_exprs {
                        args.push(eval(expr, renv, out)?);
                    }
                    args.push(callcc_val);
                    // Re-evaluate post-callcc args
                    for expr in post_exprs {
                        args.push(eval(expr, renv, out)?);
                    }
                    kont = Rc::clone(next);
                    match apply_cek(&func_val, &args, out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                        CekAction::Value(v) => { val = v; continue; }
                        CekAction::Eval => { break; }
                    }
                }

                // --- DynWindIn: in-thunk returned, push winder, call body-thunk ---
                Cont::DynWindIn { in_thunk, body_thunk, out_thunk, next } => {
                    let wid = next_winder_id();
                    winders.push((wid, in_thunk.clone(), out_thunk.clone()));
                    let body_thunk = body_thunk.clone();
                    kont = Rc::new(Cont::DynWindBody {
                        out_thunk: out_thunk.clone(),
                        next: Rc::clone(next),
                    });
                    match apply_cek(&body_thunk, &[], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                        CekAction::Value(v) => { val = v; continue; }
                        CekAction::Eval => { break; }
                    }
                }

                // --- DynWindBody: body returned, pop winder, call out-thunk ---
                Cont::DynWindBody { out_thunk, next } => {
                    let body_val = std::mem::replace(&mut val, Value::Void);
                    winders.pop();
                    let out_thunk = out_thunk.clone();
                    kont = Rc::new(Cont::DynWindOut {
                        body_val,
                        next: Rc::clone(next),
                    });
                    match apply_cek(&out_thunk, &[], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                        CekAction::Value(v) => { val = v; continue; }
                        CekAction::Eval => { break; }
                    }
                }

                // --- DynWindOut: out-thunk returned, deliver saved body value ---
                Cont::DynWindOut { body_val, next } => {
                    val = body_val.clone();
                    kont = Rc::clone(next);
                    continue;
                }

                // --- DynWindDoThunks: chain of thunks for unwind/rewind ---
                Cont::DynWindDoThunks { thunks, final_val, final_cont, final_winders, final_handlers } => {
                    if thunks.is_empty() {
                        winders = final_winders.clone();
                        handlers = final_handlers.clone();
                        val = final_val.clone();
                        kont = Rc::clone(final_cont);
                        continue;
                    }
                    let next_thunk = thunks[0].clone();
                    let remaining = thunks[1..].to_vec();
                    kont = Rc::new(Cont::DynWindDoThunks {
                        thunks: remaining,
                        final_val: final_val.clone(),
                        final_cont: Rc::clone(final_cont),
                        final_winders: final_winders.clone(),
                        final_handlers: final_handlers.clone(),
                    });
                    match apply_cek(&next_thunk, &[], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                        CekAction::Value(v) => { val = v; continue; }
                        CekAction::Eval => { break; }
                    }
                }

                // --- RaiseVal: argument evaluated, invoke exception handler ---
                Cont::RaiseVal { next } => {
                    let exn = std::mem::replace(&mut val, Value::Void);
                    kont = Rc::clone(next);
                    if let Some(handler) = handlers.pop() {
                        match apply_cek(&handler, &[exn], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                            CekAction::Value(v) => { val = v; continue; }
                            CekAction::Eval => { break; }
                        }
                    } else {
                        return Err(EvalError::Raised(format!("{}", exn)));
                    }
                }

                // --- Guard: exception delivered via continuation, test clauses ---
                Cont::Guard { var, clauses, env: genv, next } => {
                    let exn = std::mem::replace(&mut val, Value::Void);
                    let guard_env = Env::with_parent(genv);
                    guard_env.borrow_mut().set(var.clone(), exn.clone());

                    // Find matching clause
                    let mut found_eval = false;
                    let mut found_val = false;
                    for clause in clauses {
                        let parts = match clause {
                            Value::List(parts) if !parts.is_empty() => parts,
                            _ => return Err(EvalError::Type("guard: invalid clause".into())),
                        };
                        let is_else = matches!(&parts[0], Value::Symbol(s) if s == "else");
                        let test_true = if is_else {
                            true
                        } else {
                            eval(&parts[0], &guard_env, out)?.is_truthy()
                        };
                        if test_true {
                            let body = if is_else { &parts[1..] } else { &parts[1..] };
                            kont = Rc::clone(next);
                            if body.is_empty() {
                                val = if is_else { Value::Void } else { Value::Boolean(true) };
                                found_val = true;
                            } else {
                                if body.len() > 1 {
                                    kont = Rc::new(Cont::Seq {
                                        remaining: body[1..].to_vec(),
                                        env: Rc::clone(&guard_env),
                                        next: kont,
                                    });
                                }
                                ctrl = body[0].clone();
                                env = Rc::clone(&guard_env);
                                found_eval = true;
                            }
                            break;
                        }
                    }
                    if found_eval { break; }
                    if found_val { continue; }
                    // No clause matched, re-raise
                    kont = Rc::clone(next);
                    if let Some(handler) = handlers.pop() {
                        match apply_cek(&handler, &[exn], out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                            CekAction::Value(v) => { val = v; continue; }
                            CekAction::Eval => { break; }
                        }
                    } else {
                        return Err(EvalError::Raised(format!("{}", exn)));
                    }
                }

                // --- PopHandler: pop exception handler, deliver value ---
                Cont::PopHandler { next } => {
                    handlers.pop();
                    kont = Rc::clone(next);
                    continue;
                }

                // --- CallWithValues: producer returned, apply consumer ---
                Cont::CallWithValues { consumer, next } => {
                    let consumer = consumer.clone();
                    kont = Rc::clone(next);
                    let args = match val {
                        Value::Values(vs) => vs,
                        single => vec![single],
                    };
                    match apply_cek(&consumer, &args, out, &mut ctrl, &mut env, &mut kont, &mut winders, &mut handlers)? {
                        CekAction::Value(v) => { val = v; continue; }
                        CekAction::Eval => { break; }
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers for starting let/letrec/cond forms
// ---------------------------------------------------------------------------

fn enter_body(
    body: &[Value],
    body_env: &Rc<RefCell<Env>>,
    ctrl: &mut Value,
    env: &mut Rc<RefCell<Env>>,
    kont: &mut Rc<Cont>,
    val: &mut Value,
) {
    if body.is_empty() {
        *val = Value::Void;
        *ctrl = Value::Void; // sentinel
        return;
    }
    if body.len() > 1 {
        *kont = Rc::new(Cont::Seq {
            remaining: body[1..].to_vec(),
            env: Rc::clone(body_env),
            next: Rc::clone(kont),
        });
    }
    *ctrl = body[0].clone();
    *env = Rc::clone(body_env);
}

enum CondStart {
    Immediate(Value),
    Eval(Value), // ctrl to evaluate
}

fn start_cond(clauses: &[Value], cenv: &Rc<RefCell<Env>>, kont: &mut Rc<Cont>) -> Result<CondStart, EvalError> {
    if clauses.is_empty() {
        return Ok(CondStart::Immediate(Value::Void));
    }
    let clause = match &clauses[0] {
        Value::List(parts) if !parts.is_empty() => parts,
        _ => return Err(EvalError::Type("cond: invalid clause".into())),
    };
    if let Value::Symbol(s) = &clause[0] {
        if s == "else" {
            let body = &clause[1..];
            if body.is_empty() { return Ok(CondStart::Immediate(Value::Void)); }
            if body.len() > 1 {
                *kont = Rc::new(Cont::Seq {
                    remaining: body[1..].to_vec(),
                    env: Rc::clone(cenv),
                    next: Rc::clone(kont),
                });
            }
            return Ok(CondStart::Eval(body[0].clone()));
        }
    }
    *kont = Rc::new(Cont::CondTest {
        body: clause[1..].to_vec(),
        remaining_clauses: clauses[1..].to_vec(),
        env: Rc::clone(cenv),
        next: Rc::clone(kont),
    });
    Ok(CondStart::Eval(clause[0].clone()))
}

fn parse_let_bindings(bindings_val: &Value) -> Result<Vec<(String, Value)>, EvalError> {
    let list = match bindings_val {
        Value::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let mut result = Vec::new();
    for binding in list {
        match binding {
            Value::List(pair) if pair.len() == 2 => {
                if let Value::Symbol(var) = &pair[0] {
                    result.push((var.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Type("let: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    Ok(result)
}

fn start_let(bindings_val: &Value, rest: &[Value], cur_env: &Rc<RefCell<Env>>, kont: &mut Rc<Cont>) -> Result<Option<Value>, EvalError> {
    let mut bindings = parse_let_bindings(bindings_val)?;
    let body = rest.to_vec();
    let local_env = Env::with_parent(cur_env);
    if bindings.is_empty() {
        if body.is_empty() { return Ok(None); }
        if body.len() > 1 {
            *kont = Rc::new(Cont::Seq { remaining: body[1..].to_vec(), env: Rc::clone(&local_env), next: Rc::clone(kont) });
        }
        // caller must also set env = local_env — but we can't do that here
        // We'll push a Seq with the body which carries local_env
        // Actually for 0 bindings, just push full body as Seq
        *kont = Rc::new(Cont::Seq { remaining: body, env: Rc::clone(&local_env), next: Rc::clone(kont) });
        // return a dummy value that will be discarded by Seq
        return Ok(Some(Value::Void));
    }
    let (first_name, first_init) = bindings.remove(0);
    *kont = Rc::new(Cont::LetInit {
        name: first_name,
        remaining: bindings,
        local_env,
        eval_env: Rc::clone(cur_env),
        body,
        next: Rc::clone(kont),
    });
    Ok(Some(first_init))
}

fn start_let_star(bindings_val: &Value, rest: &[Value], cur_env: &Rc<RefCell<Env>>, kont: &mut Rc<Cont>) -> Result<Option<Value>, EvalError> {
    let mut bindings = parse_let_bindings(bindings_val)?;
    let body = rest.to_vec();
    let local_env = Env::with_parent(cur_env);
    if bindings.is_empty() {
        if body.is_empty() { return Ok(None); }
        *kont = Rc::new(Cont::Seq { remaining: body, env: Rc::clone(&local_env), next: Rc::clone(kont) });
        return Ok(Some(Value::Void));
    }
    let (first_name, first_init) = bindings.remove(0);
    *kont = Rc::new(Cont::LetStarInit {
        name: first_name,
        remaining: bindings,
        local_env: Rc::clone(&local_env),
        body,
        next: Rc::clone(kont),
    });
    Ok(Some(first_init))
}

fn start_letrec(bindings_val: &Value, rest: &[Value], cur_env: &Rc<RefCell<Env>>, kont: &mut Rc<Cont>) -> Result<Option<Value>, EvalError> {
    let bindings = parse_let_bindings(bindings_val)?;
    let body = rest.to_vec();
    let local_env = Env::with_parent(cur_env);
    // Pre-bind all names to Void
    for (name, _) in &bindings {
        local_env.borrow_mut().set(name.clone(), Value::Void);
    }
    if bindings.is_empty() {
        if body.is_empty() { return Ok(None); }
        *kont = Rc::new(Cont::Seq { remaining: body, env: Rc::clone(&local_env), next: Rc::clone(kont) });
        return Ok(Some(Value::Void));
    }
    let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let inits: Vec<Value> = bindings.into_iter().map(|(_, v)| v).collect();
    let first_init = inits[0].clone();
    *kont = Rc::new(Cont::LetrecCollect {
        names,
        collected: vec![],
        remaining_inits: inits[1..].to_vec(),
        local_env,
        body,
        next: Rc::clone(kont),
    });
    Ok(Some(first_init))
}

fn letrec_env_from_kont(kont: &Rc<Cont>) -> Rc<RefCell<Env>> {
    match &**kont {
        Cont::LetrecCollect { local_env, .. } => Rc::clone(local_env),
        _ => panic!("expected LetrecCollect"),
    }
}

fn start_letrec_star(bindings_val: &Value, rest: &[Value], cur_env: &Rc<RefCell<Env>>, kont: &mut Rc<Cont>) -> Result<Option<(Value, Rc<RefCell<Env>>)>, EvalError> {
    let mut bindings = parse_let_bindings(bindings_val)?;
    let body = rest.to_vec();
    let local_env = Env::with_parent(cur_env);
    if bindings.is_empty() {
        if body.is_empty() { return Ok(None); }
        *kont = Rc::new(Cont::Seq { remaining: body, env: Rc::clone(&local_env), next: Rc::clone(kont) });
        return Ok(Some((Value::Void, local_env)));
    }
    let (first_name, first_init) = bindings.remove(0);
    *kont = Rc::new(Cont::LetStarInit {
        name: first_name,
        remaining: bindings,
        local_env: Rc::clone(&local_env),
        body,
        next: Rc::clone(kont),
    });
    Ok(Some((first_init, local_env)))
}

fn start_named_let(name: &str, args: &[Value], cur_env: &Rc<RefCell<Env>>, kont: &mut Rc<Cont>) -> Result<Option<(Value, Rc<RefCell<Env>>)>, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("named let requires bindings and body".into())); }
    let bindings = parse_let_bindings(&args[0])?;
    let params: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let inits: Vec<Value> = bindings.into_iter().map(|(_, v)| v).collect();
    let body = args[1..].to_vec();
    let loop_env = Env::with_parent(cur_env);
    let lambda = Value::Lambda {
        params: params.clone(),
        rest_param: None,
        body,
        env: Rc::clone(&loop_env),
    };
    loop_env.borrow_mut().set(name.to_string(), lambda.clone());

    if inits.is_empty() {
        // Apply lambda with no args — enter body directly
        let local_env = setup_lambda_env(&params, &None, &[], &loop_env)?;
        if let Value::Lambda { body, .. } = &lambda {
            if body.is_empty() { return Ok(None); }
            if body.len() > 1 {
                *kont = Rc::new(Cont::Seq {
                    remaining: body[1..].to_vec(),
                    env: Rc::clone(&local_env),
                    next: Rc::clone(kont),
                });
            }
            return Ok(Some((body[0].clone(), local_env)));
        }
        return Ok(None);
    }

    // Evaluate inits in cur_env, then apply lambda
    let first = inits[0].clone();
    let mut all = vec![Value::Symbol(name.to_string())]; // placeholder for func
    all.extend(inits.iter().cloned());
    *kont = Rc::new(Cont::EvalArgs {
        evaluated: vec![lambda],
        remaining: inits[1..].to_vec(),
        all_exprs: all,
        env: Rc::clone(cur_env),
        next: Rc::clone(kont),
    });
    Ok(Some((first, Rc::clone(cur_env))))
}

// ---------------------------------------------------------------------------
// Lambda / params helpers (unchanged)
// ---------------------------------------------------------------------------

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let (params, rest_param) = match &args[0] {
        Value::List(elems) => parse_params(elems)?,
        Value::Symbol(s) => (vec![], Some(s.clone())),
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, rest_param, body, env: Rc::clone(env) })
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

fn setup_lambda_env(params: &[String], rest_param: &Option<String>, args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Rc<RefCell<Env>>, EvalError> {
    if let Some(ref _rest) = rest_param {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!("expected at least {} arguments, got {}", params.len(), args.len())));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!("expected {} arguments, got {}", params.len(), args.len())));
    }
    let local_env = Env::with_parent(env);
    for (param, arg) in params.iter().zip(args.iter()) {
        local_env.borrow_mut().set(param.clone(), arg.clone());
    }
    if let Some(ref rest) = rest_param {
        let rest_args = args[params.len()..].to_vec();
        local_env.borrow_mut().set(rest.clone(), vec_to_pair_list(rest_args));
    }
    Ok(local_env)
}

// ---------------------------------------------------------------------------
// apply_builtin (all builtins — unchanged except procedure? updated)
// ---------------------------------------------------------------------------

fn apply_builtin(op: &str, vals: &[Value], out: &Output) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut acc = Num::Exact(0, 1);
            for v in vals { acc = num_add(acc, to_num(v)?); }
            Ok(num_to_value(acc))
        }
        "-" => {
            if vals.is_empty() { return Err(EvalError::Arity("- requires at least 1 argument".into())); }
            if vals.len() == 1 { return Ok(num_to_value(num_neg(to_num(&vals[0])?))); }
            let mut acc = to_num(&vals[0])?;
            for v in &vals[1..] { acc = num_sub(acc, to_num(v)?); }
            Ok(num_to_value(acc))
        }
        "*" => {
            let mut acc = Num::Exact(1, 1);
            for v in vals { acc = num_mul(acc, to_num(v)?); }
            Ok(num_to_value(acc))
        }
        "/" => {
            if vals.len() < 2 { return Err(EvalError::Arity("/ requires at least 2 arguments".into())); }
            let mut acc = to_num(&vals[0])?;
            for v in &vals[1..] { acc = num_div(acc, to_num(v)?)?; }
            Ok(num_to_value(acc))
        }
        "<" => compare_nums(vals, |a, b| a < b),
        ">" => compare_nums(vals, |a, b| a > b),
        "=" => compare_nums(vals, |a, b| a == b),
        "<=" => compare_nums(vals, |a, b| a <= b),
        ">=" => compare_nums(vals, |a, b| a >= b),
        "not" => {
            if vals.len() != 1 { return Err(EvalError::Arity("not requires 1 argument".into())); }
            Ok(Value::Boolean(!vals[0].is_truthy()))
        }
        "cons" => {
            if vals.len() != 2 { return Err(EvalError::Arity("cons requires 2 arguments".into())); }
            Ok(Value::new_pair(vals[0].clone(), vals[1].clone()))
        }
        "car" => {
            if vals.len() != 1 { return Err(EvalError::Arity("car requires 1 argument".into())); }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::Pair(p) => Ok(p.borrow().0.clone()),
                _ => Err(EvalError::Type("car: expected non-empty list".into())),
            }
        }
        "cdr" => {
            if vals.len() != 1 { return Err(EvalError::Arity("cdr requires 1 argument".into())); }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => {
                    let rest = elems[1..].to_vec();
                    if rest.is_empty() { Ok(Value::List(vec![])) } else { Ok(vec_to_pair_list(rest)) }
                }
                Value::Pair(p) => Ok(p.borrow().1.clone()),
                _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
            }
        }
        "caar" => { if vals.len() != 1 { return Err(EvalError::Arity("caar requires 1 argument".into())); } let a = apply_builtin("car", vals, out)?; apply_builtin("car", &[a], out) }
        "cadr" => { if vals.len() != 1 { return Err(EvalError::Arity("cadr requires 1 argument".into())); } let d = apply_builtin("cdr", vals, out)?; apply_builtin("car", &[d], out) }
        "cdar" => { if vals.len() != 1 { return Err(EvalError::Arity("cdar requires 1 argument".into())); } let a = apply_builtin("car", vals, out)?; apply_builtin("cdr", &[a], out) }
        "cddr" => { if vals.len() != 1 { return Err(EvalError::Arity("cddr requires 1 argument".into())); } let d = apply_builtin("cdr", vals, out)?; apply_builtin("cdr", &[d], out) }
        "caaar" => { if vals.len() != 1 { return Err(EvalError::Arity("caaar requires 1 argument".into())); } let v = apply_builtin("car", vals, out)?; let v = apply_builtin("car", &[v], out)?; apply_builtin("car", &[v], out) }
        "caadr" => { if vals.len() != 1 { return Err(EvalError::Arity("caadr requires 1 argument".into())); } let v = apply_builtin("cdr", vals, out)?; let v = apply_builtin("car", &[v], out)?; apply_builtin("car", &[v], out) }
        "caddr" => { if vals.len() != 1 { return Err(EvalError::Arity("caddr requires 1 argument".into())); } let v = apply_builtin("cdr", vals, out)?; let v = apply_builtin("cdr", &[v], out)?; apply_builtin("car", &[v], out) }
        "cdddr" => { if vals.len() != 1 { return Err(EvalError::Arity("cdddr requires 1 argument".into())); } let v = apply_builtin("cdr", vals, out)?; let v = apply_builtin("cdr", &[v], out)?; apply_builtin("cdr", &[v], out) }
        "caddar" => { if vals.len() != 1 { return Err(EvalError::Arity("caddar requires 1 argument".into())); } let v = apply_builtin("car", vals, out)?; let v = apply_builtin("cdr", &[v], out)?; let v = apply_builtin("cdr", &[v], out)?; apply_builtin("car", &[v], out) }
        "list" => Ok(vec_to_pair_list(vals.to_vec())),
        "length" => {
            if vals.len() != 1 { return Err(EvalError::Arity("length requires 1 argument".into())); }
            let elems = value_to_vec(&vals[0])?;
            Ok(Value::Integer(elems.len() as i64))
        }
        "null?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("null? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::List(e) if e.is_empty())))
        }
        "append" => {
            if vals.is_empty() { return Ok(Value::List(vec![])); }
            let mut all_elems = Vec::new();
            for v in &vals[..vals.len() - 1] { all_elems.extend(value_to_vec(v)?); }
            let last = &vals[vals.len() - 1];
            let mut result = last.clone();
            for item in all_elems.into_iter().rev() { result = Value::new_pair(item, result); }
            Ok(result)
        }
        "number?" => { if vals.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_) | Value::Rational(_, _) | Value::Float(_)))) }
        "integer?" => { if vals.len() != 1 { return Err(EvalError::Arity("integer? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_)))) }
        "rational?" => { if vals.len() != 1 { return Err(EvalError::Arity("rational? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_) | Value::Rational(_, _)))) }
        "exact?" => { if vals.len() != 1 { return Err(EvalError::Arity("exact? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_) | Value::Rational(_, _)))) }
        "inexact?" => { if vals.len() != 1 { return Err(EvalError::Arity("inexact? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Float(_)))) }
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
                Value::Float(f) => { let (num, den) = float_to_rational(*f); Ok(Value::make_rational(num, den)) }
                _ => Err(EvalError::Type("inexact->exact: expected number".into())),
            }
        }
        "numerator" => { if vals.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into())); } match &vals[0] { Value::Integer(n) => Ok(Value::Integer(*n)), Value::Rational(n, _) => Ok(Value::Integer(*n)), _ => Err(EvalError::Type("numerator: expected rational".into())) } }
        "denominator" => { if vals.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into())); } match &vals[0] { Value::Integer(_) => Ok(Value::Integer(1)), Value::Rational(_, d) => Ok(Value::Integer(*d)), _ => Err(EvalError::Type("denominator: expected rational".into())) } }
        "boolean?" => { if vals.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Boolean(_)))) }
        "string?" => { if vals.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::String(..)))) }
        "symbol?" => { if vals.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Symbol(_)))) }
        "pair?" => { if vals.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::List(e) if !e.is_empty()) || matches!(&vals[0], Value::Pair(_)))) }
        "char?" => { if vals.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Char(_)))) }
        "procedure?" => {
            if vals.len() != 1 { return Err(EvalError::Arity("procedure? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&vals[0], Value::Lambda { .. } | Value::CaseLambda { .. } | Value::RecordProc { .. } | Value::Continuation(_) | Value::Builtin(_))))
        }
        "display" => { if vals.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into())); } out.borrow_mut().push_str(&vals[0].to_display_repr()); Ok(Value::Void) }
        "write" => { if vals.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into())); } out.borrow_mut().push_str(&vals[0].to_display()); Ok(Value::Void) }
        "newline" => { if !vals.is_empty() { return Err(EvalError::Arity("newline requires 0 arguments".into())); } out.borrow_mut().push('\n'); Ok(Value::Void) }
        "string-append" => {
            let mut result = std::string::String::new();
            for v in vals { match v { Value::String(s, _) => result.push_str(s), _ => return Err(EvalError::Type("string-append: expected string".into())) } }
            Ok(Value::String(result, true))
        }
        "string-length" => { if vals.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); } match &vals[0] { Value::String(s, _) => Ok(Value::Integer(s.len() as i64)), _ => Err(EvalError::Type("string-length: expected string".into())) } }
        "substring" => {
            if vals.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
            let s = match &vals[0] { Value::String(s, _) => s, _ => return Err(EvalError::Type("substring: expected string".into())) };
            let start = expect_int(&vals[1])? as usize;
            let end = expect_int(&vals[2])? as usize;
            Ok(Value::String(s[start..end].to_string(), true))
        }
        "string->number" => {
            if vals.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
            match &vals[0] { Value::String(s, _) => match s.parse::<i64>() { Ok(n) => Ok(Value::Integer(n)), Err(_) => Ok(Value::Boolean(false)) }, _ => Err(EvalError::Type("string->number: expected string".into())) }
        }
        "number->string" => { if vals.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); } Ok(Value::String(vals[0].to_display(), true)) }
        "syntax->datum" => { if vals.len() != 1 { return Err(EvalError::Arity("syntax->datum requires 1 argument".into())); } Ok(vals[0].clone()) }
        "datum->syntax" => { if vals.len() != 2 { return Err(EvalError::Arity("datum->syntax requires 2 arguments".into())); } Ok(vals[1].clone()) }
        "symbol->string" => { if vals.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); } match &vals[0] { Value::Symbol(s) => Ok(Value::String(s.clone(), false)), _ => Err(EvalError::Type("symbol->string: expected symbol".into())) } }
        "string->symbol" => { if vals.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); } match &vals[0] { Value::String(s, _) => Ok(Value::Symbol(s.clone())), _ => Err(EvalError::Type("string->symbol: expected string".into())) } }
        "string-copy" => { if vals.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); } match &vals[0] { Value::String(s, _) => Ok(Value::String(s.clone(), true)), _ => Err(EvalError::Type("string-copy: expected string".into())) } }
        "string->list" => { if vals.len() != 1 { return Err(EvalError::Arity("string->list requires 1 argument".into())); } match &vals[0] { Value::String(s, _) => Ok(vec_to_pair_list(s.chars().map(Value::Char).collect())), _ => Err(EvalError::Type("string->list: expected string".into())) } }
        "list->string" => {
            if vals.len() != 1 { return Err(EvalError::Arity("list->string requires 1 argument".into())); }
            let items = value_to_vec(&vals[0])?;
            let mut s = std::string::String::new();
            for item in &items { match item { Value::Char(c) => s.push(*c), _ => return Err(EvalError::Type("list->string: expected list of chars".into())) } }
            Ok(Value::String(s, true))
        }
        "char->integer" => { if vals.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into())); } match &vals[0] { Value::Char(c) => Ok(Value::Integer(*c as i64)), _ => Err(EvalError::Type("char->integer: expected char".into())) } }
        "integer->char" => { if vals.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into())); } let n = expect_int(&vals[0])?; match char::from_u32(n as u32) { Some(c) => Ok(Value::Char(c)), None => Err(EvalError::Type("integer->char: invalid code point".into())) } }
        "string-ref" => {
            if vals.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
            let s = match &vals[0] { Value::String(s, _) => s, _ => return Err(EvalError::Type("string-ref: expected string".into())) };
            let idx = expect_int(&vals[1])? as usize;
            match s.chars().nth(idx) { Some(c) => Ok(Value::Char(c)), None => Err(EvalError::Type("string-ref: index out of bounds".into())) }
        }
        "abs" => { if vals.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); } Ok(Value::Integer(expect_int(&vals[0])?.abs())) }
        "modulo" => { if vals.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); } let a = expect_int(&vals[0])?; let b = expect_int(&vals[1])?; if b == 0 { return Err(EvalError::DivisionByZero); } Ok(Value::Integer(((a % b) + b) % b)) }
        "remainder" => { if vals.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); } let a = expect_int(&vals[0])?; let b = expect_int(&vals[1])?; if b == 0 { return Err(EvalError::DivisionByZero); } Ok(Value::Integer(a % b)) }
        "quotient" => { if vals.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); } let a = expect_int(&vals[0])?; let b = expect_int(&vals[1])?; if b == 0 { return Err(EvalError::DivisionByZero); } Ok(Value::Integer(a / b)) }
        "min" => { if vals.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); } let mut m = expect_int(&vals[0])?; for v in &vals[1..] { m = m.min(expect_int(v)?); } Ok(Value::Integer(m)) }
        "max" => { if vals.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); } let mut m = expect_int(&vals[0])?; for v in &vals[1..] { m = m.max(expect_int(v)?); } Ok(Value::Integer(m)) }
        "expt" => { if vals.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); } let base = expect_int(&vals[0])?; let exp = expect_int(&vals[1])?; Ok(Value::Integer(base.pow(exp as u32))) }
        "zero?" => { if vals.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); } Ok(Value::Boolean(expect_int(&vals[0])? == 0)) }
        "positive?" => { if vals.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); } Ok(Value::Boolean(expect_int(&vals[0])? > 0)) }
        "negative?" => { if vals.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); } Ok(Value::Boolean(expect_int(&vals[0])? < 0)) }
        "odd?" => { if vals.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); } Ok(Value::Boolean(expect_int(&vals[0])? % 2 != 0)) }
        "even?" => { if vals.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); } Ok(Value::Boolean(expect_int(&vals[0])? % 2 == 0)) }
        "list-ref" => { if vals.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); } let elems = value_to_vec(&vals[0])?; let idx = expect_int(&vals[1])? as usize; elems.get(idx).cloned().ok_or_else(|| EvalError::Type("list-ref: index out of bounds".into())) }
        "list-tail" => {
            if vals.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
            let idx = expect_int(&vals[1])? as usize;
            let mut cur = vals[0].clone();
            for _ in 0..idx {
                cur = match &cur {
                    Value::Pair(p) => p.borrow().1.clone(),
                    Value::List(e) if !e.is_empty() => { let rest = e[1..].to_vec(); if rest.is_empty() { Value::List(vec![]) } else { vec_to_pair_list(rest) } }
                    _ => return Err(EvalError::Type("list-tail: index out of bounds".into())),
                };
            }
            Ok(cur)
        }
        "list?" => { if vals.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); } Ok(Value::Boolean(is_proper_list(&vals[0]))) }
        "assoc" => {
            if vals.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
            let key = &vals[0]; let alist = value_to_vec(&vals[1])?;
            for entry in &alist { let car = match entry { Value::List(pair) if !pair.is_empty() => pair[0].clone(), Value::Pair(p) => p.borrow().0.clone(), _ => continue }; if car == *key { return Ok(entry.clone()); } }
            Ok(Value::Boolean(false))
        }
        "assq" => {
            if vals.len() != 2 { return Err(EvalError::Arity("assq requires 2 arguments".into())); }
            let key = &vals[0]; let alist = value_to_vec(&vals[1])?;
            for entry in &alist { let car = match entry { Value::List(pair) if !pair.is_empty() => pair[0].clone(), Value::Pair(p) => p.borrow().0.clone(), _ => continue }; if eqv(key, &car) { return Ok(entry.clone()); } }
            Ok(Value::Boolean(false))
        }
        "memq" => {
            if vals.len() != 2 { return Err(EvalError::Arity("memq requires 2 arguments".into())); }
            let key = &vals[0];
            let mut cur = vals[1].clone();
            loop {
                match &cur {
                    Value::List(e) if e.is_empty() => return Ok(Value::Boolean(false)),
                    Value::Pair(p) => { let pair = p.borrow(); if eqv(key, &pair.0) { return Ok(cur.clone()); } let next = pair.1.clone(); drop(pair); cur = next; }
                    Value::List(e) => { for (i, elem) in e.iter().enumerate() { if eqv(key, elem) { return Ok(vec_to_pair_list(e[i..].to_vec())); } } return Ok(Value::Boolean(false)); }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "memv" => {
            if vals.len() != 2 { return Err(EvalError::Arity("memv requires 2 arguments".into())); }
            let key = &vals[0];
            let mut cur = vals[1].clone();
            loop {
                match &cur {
                    Value::List(e) if e.is_empty() => return Ok(Value::Boolean(false)),
                    Value::Pair(p) => { let pair = p.borrow(); if eqv(key, &pair.0) { return Ok(cur.clone()); } let next = pair.1.clone(); drop(pair); cur = next; }
                    Value::List(e) => { for (i, elem) in e.iter().enumerate() { if eqv(key, elem) { return Ok(vec_to_pair_list(e[i..].to_vec())); } } return Ok(Value::Boolean(false)); }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "eq?" => { if vals.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); } Ok(Value::Boolean(vals[0] == vals[1])) }
        "equal?" => { if vals.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); } Ok(Value::Boolean(deep_equal(&vals[0], &vals[1]))) }
        "map" => {
            if vals.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
            let func = &vals[0];
            let lists: Vec<Vec<Value>> = vals[1..].iter().map(|v| value_to_vec(v)).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for l in &lists { if l.len() != len { return Err(EvalError::Arity("map: lists must have same length".into())); } }
            let mut result = Vec::new();
            for i in 0..len { let args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect(); result.push(apply(func, &args, out)?); }
            Ok(vec_to_pair_list(result))
        }
        "char-alphabetic?" => { if vals.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); } match &vals[0] { Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())), _ => Err(EvalError::Type("char-alphabetic?: expected char".into())) } }
        "char-numeric?" => { if vals.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); } match &vals[0] { Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())), _ => Err(EvalError::Type("char-numeric?: expected char".into())) } }
        "char-upcase" => { if vals.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); } match &vals[0] { Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())), _ => Err(EvalError::Type("char-upcase: expected char".into())) } }
        "char-downcase" => { if vals.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); } match &vals[0] { Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())), _ => Err(EvalError::Type("char-downcase: expected char".into())) } }
        "char=?" => { if vals.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)), _ => Err(EvalError::Type("char=?: expected chars".into())) } }
        "char<?" => { if vals.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)), _ => Err(EvalError::Type("char<?: expected chars".into())) } }
        "string=?" => { if vals.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a == b)), _ => Err(EvalError::Type("string=?: expected strings".into())) } }
        "string<?" => { if vals.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a < b)), _ => Err(EvalError::Type("string<?: expected strings".into())) } }
        "string-ci=?" => { if vals.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())), _ => Err(EvalError::Type("string-ci=?: expected strings".into())) } }
        "string-upcase" => { if vals.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); } match &vals[0] { Value::String(s, _) => Ok(Value::String(s.to_uppercase(), true)), _ => Err(EvalError::Type("string-upcase: expected string".into())) } }
        "string-downcase" => { if vals.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); } match &vals[0] { Value::String(s, _) => Ok(Value::String(s.to_lowercase(), true)), _ => Err(EvalError::Type("string-downcase: expected string".into())) } }
        "eqv?" => { if vals.len() != 2 { return Err(EvalError::Arity("eqv? requires 2 arguments".into())); } Ok(Value::Boolean(eqv(&vals[0], &vals[1]))) }
        "vector" => Ok(Value::Vector(Rc::new(RefCell::new(vals.to_vec())))),
        "make-vector" => {
            if vals.is_empty() || vals.len() > 2 { return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into())); }
            let len = expect_int(&vals[0])? as usize;
            let fill = if vals.len() == 2 { vals[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if vals.len() != 2 { return Err(EvalError::Arity("vector-ref requires 2 arguments".into())); }
            match &vals[0] { Value::Vector(v) => { let idx = expect_int(&vals[1])? as usize; let elems = v.borrow(); elems.get(idx).cloned().ok_or_else(|| EvalError::Type("vector-ref: index out of bounds".into())) } _ => Err(EvalError::Type("vector-ref: expected vector".into())) }
        }
        "vector-length" => { if vals.len() != 1 { return Err(EvalError::Arity("vector-length requires 1 argument".into())); } match &vals[0] { Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)), _ => Err(EvalError::Type("vector-length: expected vector".into())) } }
        "vector?" => { if vals.len() != 1 { return Err(EvalError::Arity("vector? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&vals[0], Value::Vector(_)))) }
        "vector->list" => { if vals.len() != 1 { return Err(EvalError::Arity("vector->list requires 1 argument".into())); } match &vals[0] { Value::Vector(v) => Ok(vec_to_pair_list(v.borrow().clone())), _ => Err(EvalError::Type("vector->list: expected vector".into())) } }
        "list->vector" => { if vals.len() != 1 { return Err(EvalError::Arity("list->vector requires 1 argument".into())); } let elems = value_to_vec(&vals[0])?; Ok(Value::Vector(Rc::new(RefCell::new(elems)))) }
        "for-each" => {
            if vals.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into())); }
            let func = &vals[0];
            let lists: Vec<Vec<Value>> = vals[1..].iter().map(|v| value_to_vec(v)).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len { let args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect(); apply(func, &args, out)?; }
            Ok(Value::Void)
        }
        "set-car!" => { if vals.len() != 2 { return Err(EvalError::Arity("set-car! requires 2 arguments".into())); } match &vals[0] { Value::Pair(p) => { p.borrow_mut().0 = vals[1].clone(); Ok(Value::Void) } _ => Err(EvalError::Type("set-car!: expected pair".into())) } }
        "set-cdr!" => { if vals.len() != 2 { return Err(EvalError::Arity("set-cdr! requires 2 arguments".into())); } match &vals[0] { Value::Pair(p) => { p.borrow_mut().1 = vals[1].clone(); Ok(Value::Void) } _ => Err(EvalError::Type("set-cdr!: expected pair".into())) } }
        "reverse" => { if vals.len() != 1 { return Err(EvalError::Arity("reverse requires 1 argument".into())); } let mut elems = value_to_vec(&vals[0])?; elems.reverse(); Ok(vec_to_pair_list(elems)) }
        "member" => {
            if vals.len() != 2 { return Err(EvalError::Arity("member requires 2 arguments".into())); }
            let key = &vals[0]; let mut cur = vals[1].clone();
            loop {
                match &cur {
                    Value::List(e) if e.is_empty() => return Ok(Value::Boolean(false)),
                    Value::Pair(p) => { let (car, cdr) = { let pair = p.borrow(); (pair.0.clone(), pair.1.clone()) }; if deep_equal(key, &car) { return Ok(cur); } cur = cdr; }
                    Value::List(e) => { for (i, elem) in e.iter().enumerate() { if deep_equal(key, elem) { return Ok(vec_to_pair_list(e[i..].to_vec())); } } return Ok(Value::Boolean(false)); }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "assv" => {
            if vals.len() != 2 { return Err(EvalError::Arity("assv requires 2 arguments".into())); }
            let key = &vals[0]; let alist = value_to_vec(&vals[1])?;
            for entry in &alist { let car = match entry { Value::List(pair) if !pair.is_empty() => pair[0].clone(), Value::Pair(p) => p.borrow().0.clone(), _ => continue }; if eqv(key, &car) { return Ok(entry.clone()); } }
            Ok(Value::Boolean(false))
        }
        "gcd" => { if vals.len() != 2 { return Err(EvalError::Arity("gcd requires 2 arguments".into())); } let a = expect_int(&vals[0])?.unsigned_abs(); let b = expect_int(&vals[1])?.unsigned_abs(); Ok(Value::Integer(crate::scheme::value::gcd(a, b) as i64)) }
        "lcm" => { if vals.len() != 2 { return Err(EvalError::Arity("lcm requires 2 arguments".into())); } let a = expect_int(&vals[0])?.unsigned_abs(); let b = expect_int(&vals[1])?.unsigned_abs(); let g = crate::scheme::value::gcd(a, b); if g == 0 { return Ok(Value::Integer(0)); } Ok(Value::Integer((a / g * b) as i64)) }
        "truncate" => { if vals.len() != 1 { return Err(EvalError::Arity("truncate requires 1 argument".into())); } match &vals[0] { Value::Integer(n) => Ok(Value::Integer(*n)), Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)), Value::Rational(n, d) => Ok(Value::Integer(n / d)), _ => Err(EvalError::Type("truncate: expected number".into())) } }
        "round" => { if vals.len() != 1 { return Err(EvalError::Arity("round requires 1 argument".into())); } match &vals[0] { Value::Integer(n) => Ok(Value::Integer(*n)), Value::Float(f) => Ok(Value::Integer(f.round() as i64)), Value::Rational(n, d) => Ok(Value::Integer((*n as f64 / *d as f64).round() as i64)), _ => Err(EvalError::Type("round: expected number".into())) } }
        "floor" => {
            if vals.len() != 1 { return Err(EvalError::Arity("floor requires 1 argument".into())); }
            match &vals[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.floor() as i64)),
                Value::Rational(n, d) => { let div = n / d; if (*n < 0) != (*d < 0) && n % d != 0 { Ok(Value::Integer(div - 1)) } else { Ok(Value::Integer(div)) } }
                _ => Err(EvalError::Type("floor: expected number".into())),
            }
        }
        "ceiling" => { if vals.len() != 1 { return Err(EvalError::Arity("ceiling requires 1 argument".into())); } match &vals[0] { Value::Integer(n) => Ok(Value::Integer(*n)), Value::Float(f) => Ok(Value::Integer(f.ceil() as i64)), _ => Err(EvalError::Type("ceiling: expected number".into())) } }
        "make-string" => {
            if vals.is_empty() || vals.len() > 2 { return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into())); }
            let n = expect_int(&vals[0])? as usize;
            let ch = if vals.len() == 2 { match &vals[1] { Value::Char(c) => *c, _ => return Err(EvalError::Type("make-string: expected char".into())) } } else { ' ' };
            Ok(Value::String(std::iter::repeat(ch).take(n).collect(), true))
        }
        "string" => { let mut s = std::string::String::new(); for v in vals { match v { Value::Char(c) => s.push(*c), _ => return Err(EvalError::Type("string: expected char".into())) } } Ok(Value::String(s, true)) }
        "string>?" => { if vals.len() != 2 { return Err(EvalError::Arity("string>? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a > b)), _ => Err(EvalError::Type("string>?: expected strings".into())) } }
        "string<=?" => { if vals.len() != 2 { return Err(EvalError::Arity("string<=? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a <= b)), _ => Err(EvalError::Type("string<=?: expected strings".into())) } }
        "string>=?" => { if vals.len() != 2 { return Err(EvalError::Arity("string>=? requires 2 arguments".into())); } match (&vals[0], &vals[1]) { (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a >= b)), _ => Err(EvalError::Type("string>=?: expected strings".into())) } }
        "vector-set!" => {
            if vals.len() != 3 { return Err(EvalError::Arity("vector-set! requires 3 arguments".into())); }
            match &vals[0] { Value::Vector(v) => { let idx = expect_int(&vals[1])? as usize; let mut elems = v.borrow_mut(); if idx >= elems.len() { return Err(EvalError::Type("vector-set!: index out of bounds".into())); } elems[idx] = vals[2].clone(); Ok(Value::Void) } _ => Err(EvalError::Type("vector-set!: expected vector".into())) }
        }
        "string-set!" => {
            if vals.len() != 3 { return Err(EvalError::Arity("string-set! requires 3 arguments".into())); }
            Err(EvalError::Type("string-set!: cannot mutate string via builtin call".into()))
        }
        "values" => {
            if vals.len() == 1 {
                Ok(vals[0].clone())
            } else {
                Ok(Value::Values(vals.to_vec()))
            }
        }
        _ => {
            if op.starts_with('c') && op.ends_with('r') && op.len() >= 3 {
                let middle = &op[1..op.len()-1];
                if middle.chars().all(|c| c == 'a' || c == 'd') {
                    if vals.len() != 1 { return Err(EvalError::Arity(format!("{} requires 1 argument", op))); }
                    let mut val = vals[0].clone();
                    for ch in middle.chars().rev() {
                        if ch == 'a' { val = apply_builtin("car", &[val], out)?; } else { val = apply_builtin("cdr", &[val], out)?; }
                    }
                    return Ok(val);
                }
            }
            Err(EvalError::UnboundVariable(op.into()))
        }
    }
}

// ---------------------------------------------------------------------------
// Delegated special forms (use recursive eval)
// ---------------------------------------------------------------------------

fn eval_string_set(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("string-set! requires 3 arguments".into())); }
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
            if !mutable { return Err(EvalError::Type("string-set!: strings are immutable".into())); }
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() { return Err(EvalError::Type("string-set!: index out of bounds".into())); }
            chars[idx] = ch;
            let new_s: String = chars.into_iter().collect();
            Env::set_existing(env, &name, Value::String(new_s, true))?;
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string".into())),
    }
}

fn eval_vector_set(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("vector-set! requires 3 arguments".into())); }
    let vec_val = eval(&args[0], env, out)?;
    let idx = expect_int(&eval(&args[1], env, out)?)? as usize;
    let val = eval(&args[2], env, out)?;
    match &vec_val {
        Value::Vector(v) => {
            let mut elems = v.borrow_mut();
            if idx >= elems.len() { return Err(EvalError::Type("vector-set!: index out of bounds".into())); }
            elems[idx] = val;
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector".into())),
    }
}

fn eval_do(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("do requires variable specs and test".into())); }
    let var_specs = match &args[0] { Value::List(v) => v, _ => return Err(EvalError::Type("do: expected variable spec list".into())) };
    let test_clause = match &args[1] { Value::List(t) if !t.is_empty() => t, _ => return Err(EvalError::Type("do: expected test clause".into())) };
    let body = &args[2..];
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Value>> = Vec::new();
    let loop_env = Env::with_parent(env);
    for spec in var_specs {
        let parts = match spec { Value::List(p) => p, _ => return Err(EvalError::Type("do: expected variable spec".into())) };
        if parts.len() < 2 || parts.len() > 3 { return Err(EvalError::Arity("do: variable spec needs (var init) or (var init step)".into())); }
        let name = match &parts[0] { Value::Symbol(s) => s.clone(), _ => return Err(EvalError::Type("do: expected symbol for variable name".into())) };
        let init = eval(&parts[1], env, out)?;
        let step = if parts.len() == 3 { Some(parts[2].clone()) } else { None };
        loop_env.borrow_mut().set(name.clone(), init);
        var_names.push(name);
        step_exprs.push(step);
    }
    loop {
        let test_result = eval(&test_clause[0], &loop_env, out)?;
        if test_result.is_truthy() {
            let mut result = Value::Void;
            for expr in &test_clause[1..] { result = eval(expr, &loop_env, out)?; }
            return Ok(result);
        }
        for expr in body { eval(expr, &loop_env, out)?; }
        let new_vals: Vec<Option<Value>> = step_exprs.iter()
            .map(|step| match step { Some(e) => eval(e, &loop_env, out).map(Some), None => Ok(None) })
            .collect::<Result<_, _>>()?;
        for (name, val) in var_names.iter().zip(new_vals) {
            if let Some(v) = val { loop_env.borrow_mut().set(name.clone(), v); }
        }
    }
}

// ---------------------------------------------------------------------------
// Record types (L12)
// ---------------------------------------------------------------------------

fn eval_define_record_type(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 3 { return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into())); }
    let type_id = crate::scheme::value::next_record_type_id();
    let ctor_elems = match &args[1] { Value::List(e) => e, _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into())) };
    if ctor_elems.is_empty() { return Err(EvalError::Parse("define-record-type: empty constructor".into())); }
    let ctor_name = match &ctor_elems[0] { Value::Symbol(s) => s.clone(), _ => return Err(EvalError::Type("define-record-type: expected constructor name".into())) };
    let ctor_fields: Vec<String> = ctor_elems[1..].iter().map(|v| match v { Value::Symbol(s) => Ok(s.clone()), _ => Err(EvalError::Type("define-record-type: expected field name".into())) }).collect::<Result<_, _>>()?;
    let pred_name = match &args[2] { Value::Symbol(s) => s.clone(), _ => return Err(EvalError::Type("define-record-type: expected predicate name".into())) };
    env.borrow_mut().set(ctor_name, Value::RecordProc { type_id, kind: crate::scheme::value::RecordProcKind::Constructor { field_names: ctor_fields.clone() } });
    env.borrow_mut().set(pred_name, Value::RecordProc { type_id, kind: crate::scheme::value::RecordProcKind::Predicate });
    for field_spec in &args[3..] {
        let parts = match field_spec { Value::List(e) => e, _ => return Err(EvalError::Type("define-record-type: expected field spec".into())) };
        if parts.len() < 2 { return Err(EvalError::Arity("define-record-type: field spec needs name and accessor".into())); }
        let field_name = match &parts[0] { Value::Symbol(s) => s.clone(), _ => return Err(EvalError::Type("define-record-type: expected field name".into())) };
        let accessor_name = match &parts[1] { Value::Symbol(s) => s.clone(), _ => return Err(EvalError::Type("define-record-type: expected accessor name".into())) };
        let field_index = ctor_fields.iter().position(|f| f == &field_name).ok_or_else(|| EvalError::Type(format!("define-record-type: unknown field {}", field_name)))?;
        env.borrow_mut().set(accessor_name, Value::RecordProc { type_id, kind: crate::scheme::value::RecordProcKind::Accessor { field_index } });
    }
    Ok(Value::Void)
}

// ---------------------------------------------------------------------------
// Macro support (L10)
// ---------------------------------------------------------------------------

use std::collections::HashMap;

thread_local! {
    static SYNTAX_ENV: RefCell<Vec<HashMap<String, MacroBinding>>> = RefCell::new(Vec::new());
}

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Value),
    Ellipsis(Vec<Value>),
}

fn eval_define_syntax(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("define-syntax requires 2 arguments".into())); }
    let name = match &args[0] { Value::Symbol(s) => s.clone(), _ => return Err(EvalError::Type("define-syntax: expected symbol".into())) };
    // Check if RHS is (syntax-rules ...)
    if let Value::List(elems) = &args[1] {
        if !elems.is_empty() {
            if let Value::Symbol(s) = &elems[0] {
                if s == "syntax-rules" {
                    let transformer = eval_syntax_rules(&args[1], env)?;
                    env.borrow_mut().set(name, transformer);
                    return Ok(Value::Void);
                }
            }
        }
    }
    // Otherwise evaluate the RHS (should produce a Lambda for syntax-case transformers)
    let transformer = eval(&args[1], env, out)?;
    env.borrow_mut().set(name, Value::MacroTransformer(Box::new(transformer)));
    Ok(Value::Void)
}

fn eval_syntax_case(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("syntax-case requires at least 2 arguments".into())); }
    // Evaluate the syntax object
    let stx = eval(&args[0], env, out)?;
    // Parse literals
    let literals: Vec<String> = match &args[1] {
        Value::List(lits) => {
            let mut result = Vec::new();
            for lit in lits { if let Value::Symbol(s) = lit { result.push(s.clone()); } }
            result
        }
        _ => return Err(EvalError::Type("syntax-case: expected literals list".into())),
    };
    // Try each clause
    for clause in &args[2..] {
        let clause_elems = match clause {
            Value::List(e) => e,
            _ => return Err(EvalError::Type("syntax-case: invalid clause".into())),
        };
        if clause_elems.len() < 2 || clause_elems.len() > 3 {
            return Err(EvalError::Type("syntax-case: clause must have 2 or 3 elements".into()));
        }
        let pattern = &clause_elems[0];
        let (fender, body) = if clause_elems.len() == 3 {
            (Some(&clause_elems[1]), &clause_elems[2])
        } else {
            (None, &clause_elems[1])
        };
        let mut bindings = HashMap::new();
        if match_syntax_case(pattern, &stx, &literals, &mut bindings) {
            // Check fender if present
            if let Some(fender_expr) = fender {
                let fender_env = Env::with_parent(env);
                for (name, binding) in &bindings {
                    match binding {
                        MacroBinding::Single(v) => fender_env.borrow_mut().set(name.clone(), v.clone()),
                        MacroBinding::Ellipsis(vs) => fender_env.borrow_mut().set(name.clone(), Value::List(vs.clone())),
                    }
                }
                let fender_result = eval(fender_expr, &fender_env, out)?;
                if !fender_result.is_truthy() { continue; }
            }
            // Push syntax bindings for (syntax ...) to use
            SYNTAX_ENV.with(|se| se.borrow_mut().push(bindings.clone()));
            // Create env with pattern vars for body evaluation
            let body_env = Env::with_parent(env);
            for (name, binding) in &bindings {
                match binding {
                    MacroBinding::Single(v) => body_env.borrow_mut().set(name.clone(), v.clone()),
                    MacroBinding::Ellipsis(vs) => body_env.borrow_mut().set(name.clone(), Value::List(vs.clone())),
                }
            }
            let result = eval(body, &body_env, out);
            SYNTAX_ENV.with(|se| se.borrow_mut().pop());
            return result;
        }
    }
    Err(EvalError::Type("no matching syntax-case pattern".into()))
}

fn eval_with_syntax(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("with-syntax requires bindings and body".into())); }
    let binding_list = match &args[0] {
        Value::List(b) => b,
        _ => return Err(EvalError::Type("with-syntax: expected bindings list".into())),
    };
    // Start from parent syntax bindings (if any)
    let mut bindings: HashMap<String, MacroBinding> = SYNTAX_ENV.with(|se| {
        let stack = se.borrow();
        stack.last().cloned().unwrap_or_default()
    });
    let body_env = Env::with_parent(env);
    for binding in binding_list {
        let parts = match binding {
            Value::List(p) if p.len() == 2 => p,
            _ => return Err(EvalError::Type("with-syntax: each binding must be (pattern expr)".into())),
        };
        let expr_val = eval(&parts[1], env, out)?;
        match &parts[0] {
            Value::Symbol(name) => {
                bindings.insert(name.clone(), MacroBinding::Single(expr_val.clone()));
                body_env.borrow_mut().set(name.clone(), expr_val);
            }
            _ => return Err(EvalError::Type("with-syntax: expected symbol pattern".into())),
        }
    }
    SYNTAX_ENV.with(|se| se.borrow_mut().push(bindings));
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &body_env, out)?;
    }
    SYNTAX_ENV.with(|se| se.borrow_mut().pop());
    Ok(result)
}

fn match_syntax_case(pattern: &Value, input: &Value, literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    match (pattern, input) {
        (Value::List(pat_elems), Value::List(inp_elems)) => {
            match_elements(pat_elems, inp_elems, literals, bindings)
        }
        (Value::List(pat_elems), Value::Pair(_)) => {
            // Convert pair to list for matching
            let inp_vec = match value_to_vec_safe(input) {
                Some(v) => v,
                None => return false,
            };
            match_elements(pat_elems, &inp_vec, literals, bindings)
        }
        (Value::Symbol(s), _) if s == "_" => true,
        (Value::Symbol(s), _) if literals.contains(s) => matches!(input, Value::Symbol(t) if t == s),
        (Value::Symbol(s), _) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        _ => pattern == input,
    }
}

fn value_to_vec_safe(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(v) => Some(v.clone()),
        Value::Pair(p) => {
            let mut result = Vec::new();
            let mut cur = Rc::clone(p);
            loop {
                let (car, cdr) = {
                    let pair = cur.borrow();
                    (pair.0.clone(), pair.1.clone())
                };
                result.push(car);
                match cdr {
                    Value::Pair(next) => cur = next,
                    Value::List(ref elems) if elems.is_empty() => return Some(result),
                    _ => return None, // improper list
                }
            }
        }
        _ => None,
    }
}

fn eval_syntax_rules(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let elems = match expr { Value::List(e) => e, _ => return Err(EvalError::Type("syntax-rules: expected list".into())) };
    if elems.len() < 3 { return Err(EvalError::Arity("syntax-rules requires literals and rules".into())); }
    if !matches!(&elems[0], Value::Symbol(s) if s == "syntax-rules") { return Err(EvalError::Type("expected syntax-rules".into())); }
    let literals = match &elems[1] {
        Value::List(lits) => {
            let mut result = Vec::new();
            for lit in lits { match lit { Value::Symbol(s) => result.push(s.clone()), _ => return Err(EvalError::Type("syntax-rules: literals must be symbols".into())) } }
            result
        }
        _ => return Err(EvalError::Type("syntax-rules: expected literals list".into())),
    };
    let mut rules = Vec::new();
    for rule in &elems[2..] { match rule { Value::List(parts) if parts.len() == 2 => rules.push((parts[0].clone(), parts[1].clone())), _ => return Err(EvalError::Type("syntax-rules: each rule must be (pattern template)".into())) } }
    Ok(Value::SyntaxRules { literals, rules, def_env: Rc::clone(env) })
}

fn expand_macro(literals: &[String], rules: &[(Value, Value)], def_env: &Rc<RefCell<Env>>, input: &[Value]) -> Result<Value, EvalError> {
    let input_list = Value::List(input.to_vec());
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if match_syntax_rule(pattern, &input_list, literals, &mut bindings) {
            return instantiate_template(template, &bindings, def_env);
        }
    }
    Err(EvalError::Type("no matching syntax-rules pattern".into()))
}

fn match_syntax_rule(pattern: &Value, input: &Value, literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    match (pattern, input) {
        (Value::List(pat_elems), Value::List(inp_elems)) => {
            if pat_elems.is_empty() { return inp_elems.is_empty(); }
            match_elements(&pat_elems[1..], &inp_elems[1..], literals, bindings)
        }
        _ => false,
    }
}

fn match_elements(patterns: &[Value], inputs: &[Value], literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < patterns.len() {
        let has_ellipsis = pi + 1 < patterns.len() && matches!(&patterns[pi + 1], Value::Symbol(s) if s == "...");
        if has_ellipsis {
            let pat = &patterns[pi];
            let mut matches = Vec::new();
            let remaining_fixed = count_fixed_patterns(&patterns[pi + 2..]);
            let available = if inputs.len() >= ii + remaining_fixed { inputs.len() - remaining_fixed } else { ii };
            while ii < available {
                let mut sub_bindings = HashMap::new();
                if match_single(pat, &inputs[ii], literals, &mut sub_bindings) { matches.push(inputs[ii].clone()); ii += 1; } else { break; }
            }
            if let Value::Symbol(s) = pat { if !literals.contains(s) { bindings.insert(s.clone(), MacroBinding::Ellipsis(matches)); } }
            pi += 2;
        } else {
            if ii >= inputs.len() { return false; }
            if !match_single(&patterns[pi], &inputs[ii], literals, bindings) { return false; }
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
        let has_ellipsis = i + 1 < patterns.len() && matches!(&patterns[i + 1], Value::Symbol(s) if s == "...");
        if has_ellipsis { i += 2; } else { count += 1; i += 1; }
    }
    count
}

fn match_single(pattern: &Value, input: &Value, literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    match pattern {
        Value::Symbol(s) if s == "_" => true,
        Value::Symbol(s) if literals.contains(s) => matches!(input, Value::Symbol(t) if t == s),
        Value::Symbol(s) => { bindings.insert(s.clone(), MacroBinding::Single(input.clone())); true }
        Value::List(pat_elems) => { if let Value::List(inp_elems) = input { match_elements(pat_elems, inp_elems, literals, bindings) } else { false } }
        _ => pattern == input,
    }
}

fn instantiate_template(template: &Value, bindings: &HashMap<String, MacroBinding>, def_env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding { MacroBinding::Single(v) => Ok(v.clone()), MacroBinding::Ellipsis(_) => Ok(template.clone()) }
            } else {
                if let Ok(val) = def_env.borrow().get(s) {
                    match &val { Value::Integer(_) | Value::Boolean(_) | Value::String(..) | Value::Char(_) => Ok(val), _ => Ok(template.clone()) }
                } else { Ok(template.clone()) }
            }
        }
        Value::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                let has_ellipsis = i + 1 < elems.len() && matches!(&elems[i + 1], Value::Symbol(s) if s == "...");
                if has_ellipsis {
                    let ellipsis_vars = collect_ellipsis_vars(&elems[i], bindings);
                    if let Some(var_name) = ellipsis_vars.first() {
                        if let Some(MacroBinding::Ellipsis(vals)) = bindings.get(var_name) {
                            for val in vals {
                                let mut local_bindings = bindings.clone();
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
        Value::Symbol(s) => { if let Some(MacroBinding::Ellipsis(_)) = bindings.get(s) { vars.push(s.clone()); } }
        Value::List(elems) => { for e in elems { vars.extend(collect_ellipsis_vars(e, bindings)); } }
        _ => {}
    }
    vars
}

// ---------------------------------------------------------------------------
// Numeric helpers
// ---------------------------------------------------------------------------

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v { Value::Integer(n) => Ok(*n), _ => Err(EvalError::Type(format!("expected integer, got {}", v))) }
}

enum Num { Exact(i64, i64), Inexact(f64) }

fn to_num(v: &Value) -> Result<Num, EvalError> {
    match v {
        Value::Integer(n) => Ok(Num::Exact(*n, 1)),
        Value::Rational(n, d) => Ok(Num::Exact(*n, *d)),
        Value::Float(f) => Ok(Num::Inexact(*f)),
        _ => Err(EvalError::Type(format!("expected number, got {}", v))),
    }
}

fn num_to_value(n: Num) -> Value { match n { Num::Exact(num, den) => Value::make_rational(num, den), Num::Inexact(f) => Value::Float(f) } }

fn num_add(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bd + bn * ad, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a + b),
        (Num::Exact(n, d), Num::Inexact(f)) | (Num::Inexact(f), Num::Exact(n, d)) => Num::Inexact(n as f64 / d as f64 + f),
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
        (Num::Exact(n, d), Num::Inexact(f)) | (Num::Inexact(f), Num::Exact(n, d)) => Num::Inexact(n as f64 / d as f64 * f),
    }
}

fn num_div(a: Num, b: Num) -> Result<Num, EvalError> {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => { if bn == 0 { return Err(EvalError::DivisionByZero); } Ok(Num::Exact(an * bd, ad * bn)) }
        (Num::Inexact(a), Num::Inexact(b)) => { if b == 0.0 { return Err(EvalError::DivisionByZero); } Ok(Num::Inexact(a / b)) }
        (Num::Exact(n, d), Num::Inexact(f)) => { if f == 0.0 { return Err(EvalError::DivisionByZero); } Ok(Num::Inexact(n as f64 / d as f64 / f)) }
        (Num::Inexact(f), Num::Exact(n, d)) => { if n == 0 { return Err(EvalError::DivisionByZero); } Ok(Num::Inexact(f / (n as f64 / d as f64))) }
    }
}

fn num_to_f64(n: &Num) -> f64 { match n { Num::Exact(num, den) => *num as f64 / *den as f64, Num::Inexact(f) => *f } }

fn num_neg(a: Num) -> Num { match a { Num::Exact(n, d) => Num::Exact(-n, d), Num::Inexact(f) => Num::Inexact(-f) } }

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 { return (0, 1); }
    let sign = if f < 0.0 { -1i64 } else { 1 };
    let f = f.abs();
    let mut den = 1i64;
    let mut approx = f;
    for _ in 0..15 {
        if (approx - approx.round()).abs() < 1e-10 { return (sign * approx.round() as i64, den); }
        den *= 10;
        approx = f * den as f64;
    }
    let den = 1_000_000_000i64;
    let num = (f * den as f64).round() as i64;
    (sign * num, den)
}

fn compare_nums(vals: &[Value], pred: fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if vals.len() < 2 { return Err(EvalError::Arity("comparison requires at least 2 arguments".into())); }
    let mut prev = num_to_f64(&to_num(&vals[0])?);
    for v in &vals[1..] { let curr = num_to_f64(&to_num(v)?); if !pred(prev, curr) { return Ok(Value::Boolean(false)); } prev = curr; }
    Ok(Value::Boolean(true))
}

// ---------------------------------------------------------------------------
// Utility helpers
// ---------------------------------------------------------------------------

fn eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(a), Value::List(b)) if a.is_empty() && b.is_empty() => true,
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
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
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(al), Value::List(bl)) => al.len() == bl.len() && al.iter().zip(bl.iter()).all(|(x, y)| deep_equal(x, y)),
        (Value::Pair(a_rc), Value::Pair(b_rc)) => {
            if Rc::ptr_eq(a_rc, b_rc) { return true; }
            let ap = a_rc.borrow(); let bp = b_rc.borrow();
            deep_equal(&ap.0, &bp.0) && deep_equal(&ap.1, &bp.1)
        }
        (Value::Pair(_), Value::List(elems)) => deep_equal_pair_list(a, elems),
        (Value::List(elems), Value::Pair(_)) => deep_equal_pair_list(b, elems),
        (Value::Vector(x), Value::Vector(y)) => { let xv = x.borrow(); let yv = y.borrow(); xv.len() == yv.len() && xv.iter().zip(yv.iter()).all(|(a, b)| deep_equal(a, b)) }
        _ => false,
    }
}

fn deep_equal_pair_list(pair_val: &Value, elems: &[Value]) -> bool {
    let mut cur = pair_val.clone();
    let mut i = 0;
    loop {
        match &cur {
            Value::Pair(p) => {
                if i >= elems.len() { return false; }
                let (car, cdr) = { let pair = p.borrow(); (pair.0.clone(), pair.1.clone()) };
                if !deep_equal(&car, &elems[i]) { return false; }
                cur = cdr; i += 1;
            }
            Value::List(rest) if rest.is_empty() => return i == elems.len(),
            Value::List(rest) => {
                if rest.len() != elems.len() - i { return false; }
                return rest.iter().zip(elems[i..].iter()).all(|(a, b)| deep_equal(a, b));
            }
            _ => return false,
        }
    }
}

fn value_to_vec(v: &Value) -> Result<Vec<Value>, EvalError> {
    match v {
        Value::List(elems) => Ok(elems.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut cur = v.clone();
            loop {
                match &cur {
                    Value::Pair(p) => { let (car, cdr) = { let pair = p.borrow(); (pair.0.clone(), pair.1.clone()) }; result.push(car); cur = cdr; }
                    Value::List(elems) if elems.is_empty() => return Ok(result),
                    Value::List(elems) => { result.extend(elems.iter().cloned()); return Ok(result); }
                    _ => return Err(EvalError::Type("expected proper list".into())),
                }
                if result.len() > 10_000_000 { return Err(EvalError::Type("list too long or circular".into())); }
            }
        }
        _ => Err(EvalError::Type("expected list".into())),
    }
}

fn vec_to_pair_list(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() { result = Value::new_pair(item, result); }
    result
}

fn is_proper_list(val: &Value) -> bool {
    match val {
        Value::List(_) => true,
        Value::Pair(_) => {
            let mut slow = val.clone();
            let mut fast = val.clone();
            loop {
                for _ in 0..2 {
                    let next = match &fast {
                        Value::Pair(p) => p.borrow().1.clone(),
                        Value::List(_) => return true,
                        _ => return false,
                    };
                    fast = next;
                }
                let next = match &slow { Value::Pair(p) => p.borrow().1.clone(), _ => return false };
                slow = next;
                if let (Value::Pair(s), Value::Pair(f)) = (&slow, &fast) { if Rc::ptr_eq(s, f) { return false; } }
            }
        }
        _ => false,
    }
}
