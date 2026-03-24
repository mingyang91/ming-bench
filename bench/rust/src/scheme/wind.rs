use std::rc::Rc;

use super::{
    env_set, new_env, EvalError, Kont, KontFrame, State, Value, WindEntry,
};
use super::parser::{Expr, Pos};

/// Process one step of wind/unwind during continuation invocation.
pub(super) fn apply_wind_step(
    unwind_outs: &[Value],
    rewind_ins: &[Value],
    target_ws: &[WindEntry],
    val: &Value,
    target_kont: &Kont,
    pos: Pos,
    wind_stack: &mut Vec<WindEntry>,
) -> Result<State, EvalError> {
    if !unwind_outs.is_empty() {
        wind_stack.pop();
        let out_thunk = unwind_outs[0].clone();
        let step = Rc::new(KontFrame::EvWindStep {
            unwind_outs: unwind_outs[1..].to_vec(),
            rewind_ins: rewind_ins.to_vec(),
            target_ws: target_ws.to_vec(),
            val: val.clone(),
            target_kont: target_kont.clone(),
            pos,
        });
        Ok(State::Invoke(out_thunk, vec![], pos, step))
    } else if !rewind_ins.is_empty() {
        let entry = target_ws[wind_stack.len()].clone();
        wind_stack.push(entry);
        let in_thunk = rewind_ins[0].clone();
        let step = Rc::new(KontFrame::EvWindStep {
            unwind_outs: vec![],
            rewind_ins: rewind_ins[1..].to_vec(),
            target_ws: target_ws.to_vec(),
            val: val.clone(),
            target_kont: target_kont.clone(),
            pos,
        });
        Ok(State::Invoke(in_thunk, vec![], pos, step))
    } else {
        *wind_stack = target_ws.to_vec();
        apply_continuation(val.clone(), target_kont)
    }
}

/// Apply a value to a captured continuation.
/// When the continuation's top frame is EvCallArgs (meaning the call/cc was invoked
/// during function argument evaluation), re-evaluate all argument expressions so that
/// mutable bindings are re-read. This ensures correct behavior for reentrant continuations.
pub(super) fn apply_continuation(val: Value, k: &Kont) -> Result<State, EvalError> {
    match k.as_ref() {
        KontFrame::EvCallArgs { func, all_arg_exprs, eval_idx, env, pos, next, .. } => {
            let pre_exprs = &all_arg_exprs[..*eval_idx];
            let post_exprs = &all_arg_exprs[eval_idx + 1..];

            let temp_name = format!("__cc_v_{}", *eval_idx);
            let resume_env = new_env(Some(env.clone()));
            env_set(&resume_env, temp_name.clone(), val);

            let mut all_new: Vec<Expr> = pre_exprs.to_vec();
            all_new.push(Expr::Symbol(temp_name, *pos));
            all_new.extend(post_exprs.iter().cloned());

            if all_new.is_empty() {
                return Ok(State::Invoke(func.clone(), vec![], *pos, next.clone()));
            }
            let kont = Rc::new(KontFrame::EvCallArgs {
                func: func.clone(),
                all_arg_exprs: all_new.clone(),
                eval_idx: 0,
                done: vec![],
                env: resume_env.clone(),
                pos: *pos,
                next: next.clone(),
            });
            Ok(State::Eval(all_new[0].clone(), resume_env, kont))
        }
        _ => Ok(State::Apply(val, k.clone())),
    }
}
