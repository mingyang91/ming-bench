use std::rc::Rc;

use crate::scheme::cont::Frame;
use crate::scheme::env::{self, Env};
use crate::scheme::error::SchemeError;
use crate::scheme::form;
use crate::scheme::value::{self, LambdaData, Value};

/// CEK machine states.
pub enum State {
    Eval(Value, Env),
    Return(Value),
    Apply(Value, Vec<Value>),
}

/// Run a sequence of expressions, returning the last result.
pub fn run(exprs: Vec<Value>, env: Env) -> Result<Value, SchemeError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let mut cont: Vec<Frame> = Vec::new();
    let state = form::eval_body(&exprs, env, &mut cont)?;
    trampoline(state, cont)
}

/// Main trampoline loop — no Rust-stack recursion.
fn trampoline(init: State, mut cont: Vec<Frame>) -> Result<Value, SchemeError> {
    let mut state = init;
    loop {
        state = match state {
            State::Eval(expr, env) => eval_step(expr, env, &mut cont)?,
            State::Return(val) => match cont.pop() {
                None => return Ok(val),
                Some(frame) => apply_frame(frame, val, &mut cont)?,
            },
            State::Apply(proc, args) => apply_proc(proc, args, &mut cont)?,
        };
    }
}

// ---------------------------------------------------------------------------
// Eval dispatch
// ---------------------------------------------------------------------------

fn eval_step(
    expr: Value,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    match expr {
        Value::Integer(_)
        | Value::Boolean(_)
        | Value::Str(_)
        | Value::Nil
        | Value::Void => Ok(State::Return(expr)),
        Value::Symbol(ref name) => {
            let val = env::lookup(&env, name)?;
            Ok(State::Return(val))
        }
        Value::Pair(_, _) => eval_list(expr, env, cont),
        // Lambda / Builtin / Continuation / SyntaxRules are self-evaluating
        _ => Ok(State::Return(expr)),
    }
}

fn eval_list(
    expr: Value,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let items = value::to_vec(&expr)?;
    if items.is_empty() {
        return Err(SchemeError::BadSyntax {
            form: "empty application".into(),
        });
    }
    if let Value::Symbol(ref name) = items[0] {
        if let Some(st) = try_special_form(name, &items, env.clone(), cont)? {
            return Ok(st);
        }
        if let Ok(Value::SyntaxRules(ref mac)) = env::lookup(&env, name) {
            let expanded = crate::scheme::syntax::expand(mac, &items)?;
            return Ok(State::Eval(expanded, env));
        }
    }
    start_call(items, env, cont)
}

fn try_special_form(
    name: &str,
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<Option<State>, SchemeError> {
    let st = match name {
        "quote" => form::eval_quote(items)?,
        "if" => form::eval_if(items, env, cont)?,
        "define" => form::eval_define(items, env, cont)?,
        "lambda" => form::eval_lambda(items, env)?,
        "begin" => form::eval_begin(items, env, cont)?,
        "set!" => form::eval_set(items, env, cont)?,
        "and" => form::eval_and(items, env, cont)?,
        "or" => form::eval_or(items, env, cont)?,
        "cond" => form::eval_cond(items, env, cont)?,
        "let" => form::eval_let(items, env, cont)?,
        "define-syntax" => form::eval_define_syntax(items, env)?,
        _ => return Ok(None),
    };
    Ok(Some(st))
}

/// Begin evaluating a function call (right-to-left arg evaluation).
pub fn start_call(
    items: Vec<Value>,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let mut iter = items.into_iter();
    let Some(op_expr) = iter.next() else {
        return Err(SchemeError::BadSyntax {
            form: "empty call".into(),
        });
    };
    let args: Vec<Value> = iter.collect();
    cont.push(Frame::EvalOp {
        todo: args,
        env: env.clone(),
    });
    Ok(State::Eval(op_expr, env))
}

// ---------------------------------------------------------------------------
// Apply continuation frame
// ---------------------------------------------------------------------------

fn apply_frame(
    frame: Frame,
    val: Value,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    match frame {
        Frame::EvalOp { todo, env } => apply_eval_op(val, todo, env, cont),
        Frame::EvalArgs { op, done, todo, env } => {
            apply_eval_args(op, val, done, todo, env, cont)
        }
        Frame::Define { name, env } => {
            env::define(&env, name, val);
            Ok(State::Return(Value::Void))
        }
        Frame::If {
            then_expr,
            else_expr,
            env,
        } => {
            let branch = if val.is_truthy() { then_expr } else { else_expr };
            Ok(State::Eval(branch, env))
        }
        Frame::SetBang { name, env } => {
            env::set(&env, &name, val)?;
            Ok(State::Return(Value::Void))
        }
        Frame::Seq { remaining, env } => apply_seq(remaining, env, cont),
        Frame::And { remaining, env } => apply_and(val, remaining, env, cont),
        Frame::Or { remaining, env } => apply_or(val, remaining, env, cont),
    }
}

fn apply_eval_op(
    op: Value,
    mut todo: Vec<Value>,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    match todo.pop() {
        None => Ok(State::Apply(op, vec![])),
        Some(next) => {
            cont.push(Frame::EvalArgs {
                op,
                done: vec![],
                todo,
                env: env.clone(),
            });
            Ok(State::Eval(next, env))
        }
    }
}

fn apply_eval_args(
    op: Value,
    val: Value,
    mut done: Vec<Value>,
    mut todo: Vec<Value>,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    done.push(val);
    match todo.pop() {
        None => {
            done.reverse();
            Ok(State::Apply(op, done))
        }
        Some(next) => {
            cont.push(Frame::EvalArgs {
                op,
                done,
                todo,
                env: env.clone(),
            });
            Ok(State::Eval(next, env))
        }
    }
}

fn apply_seq(
    mut remaining: Vec<Value>,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let next = remaining.remove(0);
    if !remaining.is_empty() {
        cont.push(Frame::Seq { remaining, env: env.clone() });
    }
    Ok(State::Eval(next, env))
}

fn apply_and(
    val: Value,
    remaining: Vec<Value>,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if !val.is_truthy() {
        return Ok(State::Return(Value::Boolean(false)));
    }
    if remaining.len() == 1 {
        return Ok(State::Eval(remaining.into_iter().next().expect("len==1"), env));
    }
    cont.push(Frame::And {
        remaining: remaining[1..].to_vec(),
        env: env.clone(),
    });
    Ok(State::Eval(remaining[0].clone(), env))
}

fn apply_or(
    val: Value,
    remaining: Vec<Value>,
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if val.is_truthy() {
        return Ok(State::Return(val));
    }
    if remaining.len() == 1 {
        return Ok(State::Eval(remaining.into_iter().next().expect("len==1"), env));
    }
    cont.push(Frame::Or {
        remaining: remaining[1..].to_vec(),
        env: env.clone(),
    });
    Ok(State::Eval(remaining[0].clone(), env))
}

// ---------------------------------------------------------------------------
// Apply procedure
// ---------------------------------------------------------------------------

fn apply_proc(
    proc: Value,
    args: Vec<Value>,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    match proc {
        Value::Builtin(ref b) if is_callcc(b.name) => apply_callcc(args, cont),
        Value::Builtin(ref b) if b.name == "apply" => apply_apply(args),
        Value::Builtin(b) => {
            let result = (b.func)(&args)?;
            Ok(State::Return(result))
        }
        Value::Lambda(lam) => apply_lambda(&lam, args, cont),
        Value::Continuation(saved) => apply_continuation(saved, args, cont),
        other => Err(SchemeError::NotAProcedure {
            display: format!("{other}"),
        }),
    }
}

fn is_callcc(name: &str) -> bool {
    name == "call/cc" || name == "call-with-current-continuation"
}

fn apply_callcc(
    args: Vec<Value>,
    cont: &[Frame],
) -> Result<State, SchemeError> {
    if args.len() != 1 {
        return Err(SchemeError::ArityMismatch {
            name: "call/cc".into(),
            expected: "1".into(),
            got: args.len(),
        });
    }
    let func = args.into_iter().next().expect("len==1");
    let captured = Value::Continuation(Rc::new(cont.to_vec()));
    Ok(State::Apply(func, vec![captured]))
}

fn apply_apply(
    mut args: Vec<Value>,
) -> Result<State, SchemeError> {
    if args.len() < 2 {
        return Err(SchemeError::ArityMismatch {
            name: "apply".into(),
            expected: "2+".into(),
            got: args.len(),
        });
    }
    let func = args.remove(0);
    let last = args.pop().expect("len>=2");
    let tail = value::to_vec(&last)?;
    args.extend(tail);
    Ok(State::Apply(func, args))
}

fn apply_lambda(
    lam: &LambdaData,
    args: Vec<Value>,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let new_env = env::new_env(Some(lam.env.clone()));
    bind_params(lam, args, &new_env)?;
    form::eval_body(&lam.body, new_env, cont)
}

fn apply_continuation(
    saved: Rc<Vec<Frame>>,
    args: Vec<Value>,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if args.len() != 1 {
        return Err(SchemeError::ArityMismatch {
            name: "continuation".into(),
            expected: "1".into(),
            got: args.len(),
        });
    }
    let val = args.into_iter().next().expect("len==1");
    *cont = (*saved).clone();
    Ok(State::Return(val))
}

fn bind_params(
    lam: &LambdaData,
    args: Vec<Value>,
    env: &Env,
) -> Result<(), SchemeError> {
    let n_required = lam.params.len();
    if lam.rest.is_some() {
        if args.len() < n_required {
            return Err(SchemeError::ArityMismatch {
                name: "lambda".into(),
                expected: format!("{n_required}+"),
                got: args.len(),
            });
        }
    } else if args.len() != n_required {
        return Err(SchemeError::ArityMismatch {
            name: "lambda".into(),
            expected: n_required.to_string(),
            got: args.len(),
        });
    }
    for (name, val) in lam.params.iter().zip(args.iter()) {
        env::define(env, name.clone(), val.clone());
    }
    if let Some(ref rest_name) = lam.rest {
        let rest_vals = args[n_required..].to_vec();
        env::define(env, rest_name.clone(), value::from_vec(rest_vals));
    }
    Ok(())
}
