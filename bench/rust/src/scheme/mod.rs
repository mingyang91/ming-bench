pub mod env;
pub mod error;
mod eval;
mod macro_expand;
mod parser;
pub mod value;

pub use error::EvalError;

use error::EvalErrorKind;
use value::ContCtx;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "no expressions".into(),
        }
        .into());
    }
    let env = env::Env::new();
    env.define("apply".into(), value::Value::Builtin("apply".into()));
    env.define("call/cc".into(), value::Value::Builtin("call/cc".into()));
    env.define(
        "call-with-current-continuation".into(),
        value::Value::Builtin("call/cc".into()),
    );
    let mut output = String::new();
    let mut ctx = ContCtx::new();

    let mut result = eval::eval_sequence(&exprs, &env, &mut output, &mut ctx);
    loop {
        match result {
            Ok(val) => return Ok(val.to_string()),
            Err(e) => {
                if let EvalErrorKind::ContinuationReturn { .. } = &e.kind {
                    // Saved continuation invoked outside its call/cc scope.
                    // The value is in ctx.pending, the frames in ctx.resume_frames.
                    let resume_value = ctx
                        .pending
                        .take()
                        .expect("continuation value should be set");
                    let frames = ctx
                        .resume_frames
                        .take()
                        .expect("resume frames should be set");
                    if let Some(frame) = frames.into_iter().next() {
                        ctx.frames.clear();
                        ctx.pending = Some(resume_value);
                        result =
                            eval::eval_sequence(&frame.exprs, &frame.env, &mut output, &mut ctx);
                        continue;
                    }
                }
                return Err(e);
            }
        }
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "no expressions".into(),
        }
        .into());
    }
    let env = env::Env::new();
    env.define("apply".into(), value::Value::Builtin("apply".into()));
    env.define("call/cc".into(), value::Value::Builtin("call/cc".into()));
    env.define(
        "call-with-current-continuation".into(),
        value::Value::Builtin("call/cc".into()),
    );
    let mut output = String::new();
    let mut ctx = ContCtx::new();

    let mut result = eval::eval_sequence(&exprs, &env, &mut output, &mut ctx);
    loop {
        match result {
            Ok(val) => return Ok((val.to_string(), output)),
            Err(e) => {
                if let EvalErrorKind::ContinuationReturn { .. } = &e.kind {
                    let resume_value = ctx
                        .pending
                        .take()
                        .expect("continuation value should be set");
                    let frames = ctx
                        .resume_frames
                        .take()
                        .expect("resume frames should be set");
                    if let Some(frame) = frames.into_iter().next() {
                        ctx.frames.clear();
                        ctx.pending = Some(resume_value);
                        result =
                            eval::eval_sequence(&frame.exprs, &frame.env, &mut output, &mut ctx);
                        continue;
                    }
                }
                return Err(e);
            }
        }
    }
}

#[cfg(test)]
mod tests;
