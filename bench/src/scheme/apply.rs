use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;
use crate::scheme::{eval_body_tco, Trampoline};
/// Check if a value is truthy (everything except #f is truthy in Scheme).
pub(crate) fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

/// Extract parameter names from a list of symbols, handling optional rest parameter.
/// Returns (fixed_params, rest_param).
/// E.g. `(x y . rest)` → `(["x", "y"], Some("rest"))`.
pub(crate) fn extract_params(
    params: &[Value],
    form: &str,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = params
        .iter()
        .position(|p| matches!(p, Value::Symbol(s) if s == "."));
    match dot_pos {
        Some(pos) => {
            let [rest_sym] = &params[pos + 1..] else {
                return Err(EvalError::BadSyntax { form: form.into() });
            };
            let Value::Symbol(rest_name) = rest_sym else {
                return Err(EvalError::BadSyntax { form: form.into() });
            };
            let fixed: Vec<String> = params[..pos]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::BadSyntax { form: form.into() }),
                })
                .collect::<Result<_, _>>()?;
            Ok((fixed, Some(rest_name.clone())))
        }
        None => {
            let fixed: Vec<String> = params
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::BadSyntax { form: form.into() }),
                })
                .collect::<Result<_, _>>()?;
            Ok((fixed, None))
        }
    }
}

/// Apply a lambda, returning a Trampoline for TCO.
pub(super) fn apply_lambda_tco(func: Value, args: &[Value]) -> Result<Trampoline, EvalError> {
    let Value::Lambda {
        params,
        rest_param,
        body,
        env,
    } = func
    else {
        return Err(EvalError::NotAProcedure {
            value: format!("{func}"),
        });
    };
    match &rest_param {
        Some(rest_name) => {
            if args.len() < params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: format!("at least {}", params.len()),
                    got: args.len(),
                });
            }
            let child = Env::child(&env);
            for (param, arg) in params.iter().zip(args) {
                child.set(param.clone(), arg.clone());
            }
            let rest_values = args[params.len()..].to_vec();
            child.set(rest_name.clone(), Value::List(rest_values));
            eval_body_tco(&body, &child)
        }
        None => {
            if params.len() != args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len().to_string(),
                    got: args.len(),
                });
            }
            let child = Env::child(&env);
            for (param, arg) in params.iter().zip(args) {
                child.set(param.clone(), arg.clone());
            }
            eval_body_tco(&body, &child)
        }
    }
}

/// Apply a lambda to evaluated arguments (non-TCO, for use in non-tail contexts).
pub(crate) fn apply_lambda(func: Value, args: &[Value]) -> Result<Value, EvalError> {
    match apply_lambda_tco(func, args)? {
        Trampoline::Done(v) => Ok(v),
        Trampoline::Bounce { expr, env } => crate::scheme::eval(&expr, &env),
    }
}
