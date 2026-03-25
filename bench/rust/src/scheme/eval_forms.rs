//! Helper functions for the CEK evaluator.

use super::{vec_to_cons, Env, EvalError, Span, Val};

/// Bind lambda params and rest param into a new environment frame.
pub(crate) fn bind_lambda_args(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Val],
    lambda_env: &Env,
    span: Span,
) -> Result<Env, EvalError> {
    if let Some(ref _rest) = rest_param {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {} at {span}",
                params.len(),
                args.len()
            )));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "expected {} arguments, got {} at {span}",
            params.len(),
            args.len()
        )));
    }
    let new_env = lambda_env.push();
    for (p, a) in params.iter().zip(args.iter()) {
        new_env.define(p.clone(), a.clone());
    }
    if let Some(rest) = rest_param {
        new_env.define(rest.clone(), vec_to_cons(args[params.len()..].to_vec()));
    }
    Ok(new_env)
}
