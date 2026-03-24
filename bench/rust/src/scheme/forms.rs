//! Special-form evaluation helpers for the CEK machine.

use std::rc::Rc;
use std::sync::atomic::Ordering;

use crate::scheme::error::EvalError;
use crate::scheme::parser::{Expr, Pos};
use crate::scheme::values::values_eqv;

use super::{
    env_set, eval_body_state, eval_simple, is_truthy, make_rational, new_env, CaseLambdaClause,
    Env, Kont, KontFrame, State, Value, RECORD_TYPE_COUNTER,
};

pub(super) fn make_begin(exprs: &[Expr], p: Pos) -> Expr {
    if exprs.len() == 1 {
        exprs[0].clone()
    } else {
        let mut v = vec![Expr::Symbol("begin".to_string(), p)];
        v.extend(exprs.iter().cloned());
        Expr::List(v, p)
    }
}

pub(super) fn cek_eval_define(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "{p}: define requires at least 2 arguments"
        )));
    }
    match &args[0] {
        Expr::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{p}: define requires exactly 2 arguments"
                )));
            }
            let kont = Rc::new(KontFrame::EvDefine {
                name: name.clone(),
                env: env.clone(),
                next: kont,
            });
            Ok(State::Eval(args[1].clone(), env.clone(), kont))
        }
        Expr::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{p}: define: empty signature")));
            }
            let name = match &sig[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => {
                    return Err(EvalError::Parse(format!(
                        "{p}: define: expected function name"
                    )))
                }
            };
            let (params, rest_param) = parse_params(&sig[1..], p)?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Arity(format!("{p}: define: empty body")));
            }
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(State::Apply(Value::Void, kont))
        }
        Expr::DottedList(sig, rest_sym, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{p}: define: empty signature")));
            }
            let name = match &sig[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => {
                    return Err(EvalError::Parse(format!(
                        "{p}: define: expected function name"
                    )))
                }
            };
            let (params, _) = parse_params(&sig[1..], p)?;
            let rest_param = match rest_sym.as_ref() {
                Expr::Symbol(s, _) => Some(s.clone()),
                _ => return Err(EvalError::Parse(format!(
                    "{p}: define: expected symbol for rest parameter"
                ))),
            };
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Arity(format!("{p}: define: empty body")));
            }
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(State::Apply(Value::Void, kont))
        }
        _ => Err(EvalError::Parse(format!(
            "{p}: define: expected symbol or list"
        ))),
    }
}

pub(super) fn cek_eval_let(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "{p}: let requires bindings and body"
        )));
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(loop_name, _) = &args[0] {
        return cek_eval_named_let(loop_name, &args[1..], p, env, kont);
    }

    // Regular let
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: let: expected bindings list"
            )))
        }
    };
    let binding_pairs = parse_bindings(bindings_expr, p, "let")?;
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env.clone()));

    if binding_pairs.is_empty() {
        return Ok(eval_body_state(&body, local_env, kont));
    }
    let (first_name, first_init) = binding_pairs[0].clone();
    let rest = binding_pairs[1..].to_vec();
    let kont = Rc::new(KontFrame::EvLetBind {
        name: first_name,
        rest,
        outer_env: env.clone(),
        local_env,
        body,
        next: kont,
    });
    Ok(State::Eval(first_init, env.clone(), kont))
}

fn cek_eval_named_let(
    loop_name: &str,
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "{p}: named let requires bindings and body"
        )));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: let: expected bindings list"
            )))
        }
    };
    let mut params = Vec::new();
    let mut binding_pairs: Vec<(String, Expr)> = Vec::new();
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    params.push(s.clone());
                    binding_pairs.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Parse(format!(
                        "{p}: let: expected symbol in binding"
                    )));
                }
            }
            _ => return Err(EvalError::Parse(format!("{p}: let: invalid binding"))),
        }
    }
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env.clone()));
    let lambda = Value::Lambda {
        params: params.clone(),
        rest_param: None,
        body: body.clone(),
        env: local_env.clone(),
    };
    env_set(&local_env, loop_name.to_string(), lambda);

    if binding_pairs.is_empty() {
        return Ok(eval_body_state(&body, local_env, kont));
    }
    let (first_name, first_init) = binding_pairs[0].clone();
    let rest = binding_pairs[1..].to_vec();
    let kont = Rc::new(KontFrame::EvLetBind {
        name: first_name,
        rest,
        outer_env: env.clone(),
        local_env,
        body,
        next: kont,
    });
    Ok(State::Eval(first_init, env.clone(), kont))
}

fn parse_bindings(
    bindings_expr: &[Expr],
    p: Pos,
    form_name: &str,
) -> Result<Vec<(String, Expr)>, EvalError> {
    let mut binding_pairs = Vec::new();
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    binding_pairs.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Parse(format!(
                        "{p}: {form_name}: expected symbol in binding"
                    )));
                }
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "{p}: {form_name}: invalid binding"
                )))
            }
        }
    }
    Ok(binding_pairs)
}

pub(super) fn cek_eval_let_star(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "{p}: let* requires bindings and body"
        )));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: let*: expected bindings list"
            )))
        }
    };
    let binding_pairs = parse_bindings(bindings_expr, p, "let*")?;
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env.clone()));

    if binding_pairs.is_empty() {
        return Ok(eval_body_state(&body, local_env, kont));
    }
    let (first_name, first_init) = binding_pairs[0].clone();
    let rest = binding_pairs[1..].to_vec();
    let kont = Rc::new(KontFrame::EvLetSeqBind {
        name: first_name,
        rest,
        local_env: local_env.clone(),
        body,
        next: kont,
    });
    Ok(State::Eval(first_init, local_env, kont))
}

pub(super) fn cek_eval_letrec(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "{p}: letrec requires bindings and body"
        )));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: letrec: expected bindings list"
            )))
        }
    };
    let local_env = new_env(Some(env.clone()));
    let mut binding_pairs: Vec<(String, Expr)> = Vec::new();
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    env_set(&local_env, s.clone(), Value::Void);
                    binding_pairs.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Parse(format!(
                        "{p}: letrec: expected symbol"
                    )));
                }
            }
            _ => return Err(EvalError::Parse(format!("{p}: letrec: invalid binding"))),
        }
    }
    let body = args[1..].to_vec();

    if binding_pairs.is_empty() {
        return Ok(eval_body_state(&body, local_env, kont));
    }
    let (first_name, first_init) = binding_pairs[0].clone();
    let rest = binding_pairs[1..].to_vec();
    let kont = Rc::new(KontFrame::EvLetSeqBind {
        name: first_name,
        rest,
        local_env: local_env.clone(),
        body,
        next: kont,
    });
    Ok(State::Eval(first_init, local_env, kont))
}

pub(super) fn cek_eval_letrec_star(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    // Same implementation as letrec (evaluate inits in local_env sequentially)
    cek_eval_letrec(args, p, env, kont)
}

pub(super) fn cek_eval_and(
    exprs: &[Expr],
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if exprs.is_empty() {
        return Ok(State::Apply(Value::Boolean(true), kont));
    }
    if exprs.len() == 1 {
        return Ok(State::Eval(exprs[0].clone(), env.clone(), kont));
    }
    let kont = Rc::new(KontFrame::EvAnd {
        rest: exprs[1..].to_vec(),
        env: env.clone(),
        next: kont,
    });
    Ok(State::Eval(exprs[0].clone(), env.clone(), kont))
}

pub(super) fn cek_eval_or(
    exprs: &[Expr],
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if exprs.is_empty() {
        return Ok(State::Apply(Value::Boolean(false), kont));
    }
    if exprs.len() == 1 {
        return Ok(State::Eval(exprs[0].clone(), env.clone(), kont));
    }
    let kont = Rc::new(KontFrame::EvOr {
        rest: exprs[1..].to_vec(),
        env: env.clone(),
        next: kont,
    });
    Ok(State::Eval(exprs[0].clone(), env.clone(), kont))
}

pub(super) fn cek_eval_cond(
    clauses: &[Expr],
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    let mut idx = 0;
    while idx < clauses.len() {
        let clause = &clauses[idx];
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        return Ok(eval_body_state(&parts[1..], env.clone(), kont));
                    }
                }
                // Check for (test => proc) form
                let is_arrow = parts.len() == 3
                    && matches!(&parts[1], Expr::Symbol(s, _) if s == "=>");
                let mut dummy_output = String::new();
                if let Some(test_result) = eval_simple(&parts[0], env, &mut dummy_output) {
                    let test_val = test_result?;
                    if is_truthy(&test_val) {
                        if is_arrow {
                            // (test => proc): evaluate proc then apply to test_val
                            let kont = Rc::new(KontFrame::EvCondArrow {
                                test_val: test_val.clone(),
                                next: kont,
                            });
                            return Ok(State::Eval(parts[2].clone(), env.clone(), kont));
                        }
                        if parts.len() <= 1 {
                            return Ok(State::Apply(test_val, kont));
                        }
                        return Ok(eval_body_state(&parts[1..], env.clone(), kont));
                    }
                    idx += 1;
                    continue;
                }
                let body = parts[1..].to_vec();
                let rest = clauses[idx + 1..].to_vec();
                let kont = Rc::new(KontFrame::EvCondTest {
                    body,
                    rest_clauses: rest,
                    env: env.clone(),
                    next: kont,
                });
                return Ok(State::Eval(parts[0].clone(), env.clone(), kont));
            }
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(State::Apply(Value::Void, kont))
}

pub(super) fn cek_eval_case(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "{p}: case requires key and clauses"
        )));
    }
    let kont = Rc::new(KontFrame::EvCaseKey {
        clauses: args[1..].to_vec(),
        env: env.clone(),
        next: kont,
    });
    Ok(State::Eval(args[0].clone(), env.clone(), kont))
}

pub(super) fn cek_eval_string_set(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!(
            "{p}: string-set! requires 3 arguments"
        )));
    }
    let var_name = match &args[0] {
        Expr::Symbol(name, _) => name.clone(),
        _ => {
            return Err(EvalError::Type(format!(
                "{p}: string-set!: first argument must be a variable"
            )))
        }
    };
    let kont = Rc::new(KontFrame::EvStrSetIdx {
        var_name,
        char_expr: args[2].clone(),
        env: env.clone(),
        pos: p,
        next: kont,
    });
    Ok(State::Eval(args[1].clone(), env.clone(), kont))
}

pub(super) fn cek_eval_do(
    args: &[Expr],
    p: Pos,
    env: &Env,
    kont: Kont,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "{p}: do requires bindings and test"
        )));
    }
    let bindings_list = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: do: expected bindings list"
            )))
        }
    };
    let test_clause = match &args[1] {
        Expr::List(parts, _) if !parts.is_empty() => parts,
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: do: expected test clause"
            )))
        }
    };
    let body = &args[2..];

    let mut var_names = Vec::new();
    let mut init_exprs = Vec::new();
    let mut step_exprs = Vec::new();

    for b in bindings_list {
        match b {
            Expr::List(parts, _) if parts.len() >= 2 => {
                let name = match &parts[0] {
                    Expr::Symbol(n, _) => n.clone(),
                    _ => {
                        return Err(EvalError::Parse(format!(
                            "{p}: do: expected variable name"
                        )))
                    }
                };
                init_exprs.push(parts[1].clone());
                if parts.len() >= 3 {
                    step_exprs.push(parts[2].clone());
                } else {
                    step_exprs.push(Expr::Symbol(name.clone(), p));
                }
                var_names.push(name);
            }
            _ => return Err(EvalError::Parse(format!("{p}: do: invalid binding"))),
        }
    }

    let loop_name = "__do_loop__";

    let let_bindings: Vec<Expr> = var_names
        .iter()
        .zip(init_exprs.iter())
        .map(|(name, init)| Expr::List(vec![Expr::Symbol(name.clone(), p), init.clone()], p))
        .collect();

    let result_body = if test_clause.len() == 1 {
        Expr::List(vec![Expr::Symbol("begin".to_string(), p)], p)
    } else if test_clause.len() == 2 {
        test_clause[1].clone()
    } else {
        let mut begin = vec![Expr::Symbol("begin".to_string(), p)];
        begin.extend(test_clause[1..].iter().cloned());
        Expr::List(begin, p)
    };

    let mut loop_call_args = vec![Expr::Symbol(loop_name.to_string(), p)];
    loop_call_args.extend(step_exprs);
    let loop_call = Expr::List(loop_call_args, p);

    let else_body = if body.is_empty() {
        loop_call
    } else {
        let mut begin = vec![Expr::Symbol("begin".to_string(), p)];
        begin.extend(body.iter().cloned());
        begin.push(loop_call);
        Expr::List(begin, p)
    };

    let if_expr = Expr::List(
        vec![
            Expr::Symbol("if".to_string(), p),
            test_clause[0].clone(),
            result_body,
            else_body,
        ],
        p,
    );

    let named_let = Expr::List(
        vec![
            Expr::Symbol("let".to_string(), p),
            Expr::Symbol(loop_name.to_string(), p),
            Expr::List(let_bindings, p),
            if_expr,
        ],
        p,
    );

    let Expr::List(let_elems, _) = &named_let else {
        unreachable!()
    };
    cek_eval_let(&let_elems[1..], p, env, kont)
}

// ---------- Helpers ----------

pub(super) fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Float(f, _) => Value::Float(*f),
        Expr::Rational(n, d, _) => make_rational(*n, *d),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::Char(c, _) => Value::Char(*c),
        Expr::List(elems, _) => Value::List(elems.iter().map(expr_to_value).collect()),
        Expr::DottedList(elems, tail, _) => {
            // Build nested pairs: (a b . c) -> Pair(a, Pair(b, c))
            let tail_val = expr_to_value(tail);
            elems.iter().rev().fold(tail_val, |acc, e| {
                Value::Pair(std::rc::Rc::new(std::cell::RefCell::new((expr_to_value(e), acc))))
            })
        }
    }
}

fn parse_params(
    param_exprs: &[Expr],
    call_pos: Pos,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i] {
            Expr::Symbol(s, _) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(EvalError::Parse(format!(
                        "{call_pos}: expected rest parameter after ."
                    )));
                }
                rest_param = Some(match &param_exprs[i + 1] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => {
                        return Err(EvalError::Parse(format!(
                            "{call_pos}: expected symbol for rest parameter"
                        )))
                    }
                });
                break;
            }
            Expr::Symbol(s, _) => params.push(s.clone()),
            _ => {
                return Err(EvalError::Parse(format!(
                    "{call_pos}: expected parameter name"
                )))
            }
        }
        i += 1;
    }
    Ok((params, rest_param))
}

pub(super) fn eval_lambda(
    args: &[Expr],
    call_pos: Pos,
    env: &Env,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "{call_pos}: lambda requires params and body"
        )));
    }
    let (params, rest_param) = match &args[0] {
        Expr::List(elems, _) => parse_params(elems, call_pos)?,
        Expr::DottedList(elems, tail, _) => {
            // (lambda (a b . rest) body) — dotted pair notation for variadic
            let (params, _) = parse_params(elems, call_pos)?;
            let rest = match tail.as_ref() {
                Expr::Symbol(s, _) => s.clone(),
                _ => return Err(EvalError::Parse(format!(
                    "{call_pos}: lambda: expected symbol for rest parameter"
                ))),
            };
            (params, Some(rest))
        }
        Expr::Symbol(s, _) => {
            // (lambda args body) — single symbol catches all args as rest
            (vec![], Some(s.clone()))
        }
        _ => {
            return Err(EvalError::Parse(format!(
                "{call_pos}: lambda: expected parameter list"
            )))
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

pub(super) fn eval_case_lambda(
    args: &[Expr],
    call_pos: Pos,
    env: &Env,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "{call_pos}: case-lambda requires at least one clause"
        )));
    }
    let mut clauses = Vec::new();
    for arg in args {
        match arg {
            Expr::List(clause_elems, _) => {
                if clause_elems.len() < 2 {
                    return Err(EvalError::Parse(format!(
                        "{call_pos}: case-lambda clause needs params and body"
                    )));
                }
                let (params, rest_param) = match &clause_elems[0] {
                    Expr::List(elems, _) => parse_params(elems, call_pos)?,
                    Expr::DottedList(elems, tail, _) => {
                        let (params, _) = parse_params(elems, call_pos)?;
                        let rest = match tail.as_ref() {
                            Expr::Symbol(s, _) => s.clone(),
                            _ => return Err(EvalError::Parse(format!(
                                "{call_pos}: case-lambda: expected symbol for rest parameter"
                            ))),
                        };
                        (params, Some(rest))
                    }
                    _ => {
                        return Err(EvalError::Parse(format!(
                            "{call_pos}: case-lambda: expected parameter list"
                        )))
                    }
                };
                let body = clause_elems[1..].to_vec();
                clauses.push(CaseLambdaClause {
                    params,
                    rest_param,
                    body,
                    env: env.clone(),
                });
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "{call_pos}: case-lambda: expected clause list"
                )))
            }
        }
    }
    Ok(Value::CaseLambda { clauses })
}

// ---------- define-record-type ----------

pub(super) fn eval_define_record_type(
    args: &[Expr],
    p: Pos,
    env: &Env,
) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::Arity(format!(
            "{p}: define-record-type requires at least 3 args"
        )));
    }
    let type_name = match &args[0] {
        Expr::Symbol(name, _) => name.clone(),
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: define-record-type: expected type name"
            )))
        }
    };
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let (ctor_name, ctor_fields) = match &args[1] {
        Expr::List(elems, _) if !elems.is_empty() => {
            let name = match &elems[0] {
                Expr::Symbol(n, _) => n.clone(),
                _ => {
                    return Err(EvalError::Parse(format!(
                        "{p}: define-record-type: expected constructor name"
                    )))
                }
            };
            let fields: Vec<String> = elems[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(n, _) => Ok(n.clone()),
                    _ => Err(EvalError::Parse(format!(
                        "{p}: define-record-type: expected field name"
                    ))),
                })
                .collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: define-record-type: expected constructor"
            )))
        }
    };
    let pred_name = match &args[2] {
        Expr::Symbol(name, _) => name.clone(),
        _ => {
            return Err(EvalError::Parse(format!(
                "{p}: define-record-type: expected predicate name"
            )))
        }
    };
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match arg {
            Expr::List(elems, _) if elems.len() == 2 => {
                let field = match &elems[0] {
                    Expr::Symbol(n, _) => n.clone(),
                    _ => {
                        return Err(EvalError::Parse(format!(
                            "{p}: define-record-type: expected field name in accessor"
                        )))
                    }
                };
                let accessor = match &elems[1] {
                    Expr::Symbol(n, _) => n.clone(),
                    _ => {
                        return Err(EvalError::Parse(format!(
                            "{p}: define-record-type: expected accessor name"
                        )))
                    }
                };
                field_accessors.push((field, accessor));
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "{p}: define-record-type: expected (field accessor)"
                )))
            }
        }
    }
    env_set(env, ctor_name, Value::RecordConstructor {
        type_id,
        type_name: type_name.clone(),
        field_names: ctor_fields,
    });
    env_set(env, pred_name, Value::RecordPredicate { type_id });
    for (field_name, accessor_name) in &field_accessors {
        env_set(env, accessor_name.clone(), Value::RecordAccessor {
            type_id,
            field_name: field_name.clone(),
        });
    }
    Ok(Value::Void)
}

pub(super) fn case_clause_matches(key: &Value, parts: &[Expr]) -> bool {
    if let Expr::List(datums, _) = &parts[0] {
        datums.iter().any(|d| values_eqv(key, &expr_to_value(d)))
    } else {
        false
    }
}
