use crate::scheme::apply::{apply_lambda, apply_lambda_tco, is_truthy};
use crate::scheme::builtins::{
    apply_abs, apply_arithmetic, apply_comparison, apply_expt, apply_max, apply_min,
    apply_modulo, apply_numeric_pred, apply_quotient, apply_remainder,
};
use crate::scheme::continuation;
use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::list_ops::{
    eval_assoc_values, eval_car_values, eval_cdr_values, eval_cons_values,
    eval_length_values, eval_list_pred_values, eval_list_ref_values,
    eval_list_tail_values, eval_null_q_values,
};
use crate::scheme::vector_ops;
use crate::scheme::value::Value;
use crate::scheme::{eval, Trampoline};
use std::rc::Rc;

/// Convert a Value::List into a flat Vec.
fn list_to_vec(val: &Value) -> Result<Vec<Value>, EvalError> {
    match val {
        Value::List(elems) => Ok(elems.clone()),
        _ => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{val}"),
        }),
    }
}

/// Evaluate `(apply proc arg1 ... args-list)`.
pub(super) fn eval_apply(args: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".into(),
            got: args.len(),
        });
    }
    let func = eval(&args[0], env)?;
    let prefix: Vec<Value> = args[1..args.len() - 1]
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;
    let last = eval(&args[args.len() - 1], env)?;
    let tail = list_to_vec(&last)?;
    let mut all_args = prefix;
    all_args.extend(tail);
    apply_value_tco(func, &all_args)
}

/// Apply a value (Lambda, Builtin, or Continuation) to evaluated arguments, with TCO.
pub(crate) fn apply_value_tco(func: Value, args: &[Value]) -> Result<Trampoline, EvalError> {
    match &func {
        Value::Lambda { .. } => apply_lambda_tco(func, args),
        Value::Builtin(name) => dispatch_builtin(name, args).map(Trampoline::Done),
        Value::Continuation(id) => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                });
            };
            Err(EvalError::ContinuationReturn {
                id: *id,
                value: arg.clone(),
            })
        }
        _ => Err(EvalError::NotAProcedure {
            value: format!("{func}"),
        }),
    }
}

/// Dispatch a builtin function by name.
fn dispatch_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => apply_arithmetic(name, args),
        "<" | ">" | "=" | "<=" | ">=" => apply_comparison(name, args),
        "cons" => eval_cons_values(args),
        "car" => eval_car_values(args),
        "cdr" => eval_cdr_values(args),
        "null?" => eval_null_q_values(args),
        "list" => Ok(Value::List(args.to_vec())),
        "length" => eval_length_values(args),
        "list?" => eval_list_pred_values(args),
        "list-ref" => eval_list_ref_values(args),
        "list-tail" => eval_list_tail_values(args),
        "assoc" => eval_assoc_values(args),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                });
            };
            Ok(Value::Boolean(!is_truthy(arg)))
        }
        "equal?" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: "2".into(),
                    got: args.len(),
                });
            };
            Ok(Value::Boolean(a == b))
        }
        "eq?" | "eqv?" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: "2".into(),
                    got: args.len(),
                });
            };
            Ok(Value::Boolean(a == b))
        }
        "call/cc" | "call-with-current-continuation" => {
            let [proc] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                });
            };
            continuation::eval_callcc(proc.clone())
        }
        "zero?" | "positive?" | "negative?" | "odd?" | "even?" => apply_numeric_pred(name, args),
        "abs" => apply_abs(args),
        "quotient" => apply_quotient(args),
        "remainder" => apply_remainder(args),
        "modulo" => apply_modulo(args),
        "min" => apply_min(args),
        "max" => apply_max(args),
        "expt" => apply_expt(args),
        "vector" => vector_ops::vector_new(args),
        "make-vector" => vector_ops::make_vector(args),
        "vector-ref" => vector_ops::vector_ref(args),
        "vector-set!" => vector_ops::vector_set(args),
        "vector-length" => vector_ops::vector_length(args),
        "vector?" => vector_ops::vector_pred(args),
        "vector->list" => vector_ops::vector_to_list(args),
        "list->vector" => vector_ops::list_to_vector(args),
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "at least 2".into(),
                    got: args.len(),
                });
            }
            let func = args[0].clone();
            let prefix = &args[1..args.len() - 1];
            let tail = list_to_vec(&args[args.len() - 1])?;
            let mut all_args: Vec<Value> = prefix.to_vec();
            all_args.extend(tail);
            match &func {
                Value::Lambda { .. } => apply_lambda(func, &all_args),
                Value::Builtin(n) => dispatch_builtin(n, &all_args),
                _ => Err(EvalError::NotAProcedure {
                    value: format!("{func}"),
                }),
            }
        }
        _ => Err(EvalError::NotAProcedure {
            value: format!("#<builtin:{name}>"),
        }),
    }
}

/// Register builtin procedures in the given environment.
pub(super) fn register_builtins(env: &Rc<Env>) {
    for name in [
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "cons", "car", "cdr", "null?", "list",
        "length", "not", "equal?", "eq?", "eqv?", "apply", "call/cc",
        "call-with-current-continuation",
        "vector", "make-vector", "vector-ref", "vector-set!", "vector-length",
        "vector?", "vector->list", "list->vector",
        "list?", "list-ref", "list-tail", "assoc",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "abs", "quotient", "remainder", "modulo", "min", "max", "expt",
    ] {
        env.set(name.into(), Value::Builtin(name.into()));
    }
}
