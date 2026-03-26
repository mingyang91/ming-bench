use std::rc::Rc;

mod builtins;
mod continuation_runtime;
mod environment;
pub mod error;
mod evaluator;
mod macros;
mod model;
mod number;
mod parser;
mod records;

use builtins::eqv_values;
use continuation_runtime::{apply_cps, CpsRuntime, CpsRuntimeRef};
pub use error::EvalError;
use evaluator::{
    apply, build_case_lambda, build_lambda, case_lambda_clauses, eval_define_syntax, eval_quote,
    eval_sequence, lambda_parts, new_procedure, parse_do_bindings, parse_do_test_clause,
    parse_let_bindings, parse_param_list, quote_expr, wrong_arg_count, DoLoopState,
};
use macros::{env_with_expansion_aliases, expand_macro_call};
use model::{ContinuationProc, Env, EnvRef, Expr, Params, SchemeString, Value};
use parser::Parser;
use records::eval_define_record_type;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = environment::initial_env();
    let mut output = String::new();
    let value = if program_uses_continuations(&exprs) {
        run_cps_program(exprs.clone(), env.clone(), &mut output)?
    } else {
        eval_sequence(&exprs, &env, &mut output)?
    };
    Ok((value, output))
}

type Continuation = ContinuationProc;
type ValuesContinuation = Rc<dyn Fn(Vec<Value>, &mut String) -> Result<Value, EvalError>>;

#[derive(Clone)]
struct LetrecBindingsState {
    bindings: Vec<(String, Expr)>,
    body: Vec<Expr>,
    letrec_env: EnvRef,
    k: Continuation,
    runtime: CpsRuntimeRef,
}

enum CpsWork {
    EvalSequence {
        exprs: Vec<Expr>,
        env: EnvRef,
        continuation: Continuation,
    },
    InvokeContinuation {
        continuation: Continuation,
        value: Value,
    },
}

fn terminal_continuation() -> Continuation {
    Rc::new(|value, _output| Ok(value))
}

fn run_cps_program(exprs: Vec<Expr>, env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let runtime = CpsRuntime::new();
    runtime.reset_winders();

    let mut work = CpsWork::EvalSequence {
        exprs,
        env,
        continuation: terminal_continuation(),
    };

    let result = loop {
        let result = match work {
            CpsWork::EvalSequence {
                exprs,
                env,
                continuation,
            } => eval_sequence_cps(exprs, env, output, continuation, runtime.clone()),
            CpsWork::InvokeContinuation {
                continuation,
                value,
            } => continuation(value, output),
        };

        match result {
            Err(EvalError::ContinuationJump { jump }) => {
                let (continuation, value) = jump.into_parts();
                work = CpsWork::InvokeContinuation {
                    continuation,
                    value,
                };
            }
            other => break other,
        }
    };

    runtime.reset_winders();
    result
}

fn program_uses_continuations(exprs: &[Expr]) -> bool {
    exprs.iter().any(expr_uses_continuations)
}

fn expr_uses_continuations(expr: &Expr) -> bool {
    match expr {
        Expr::Symbol(name, _) => {
            matches!(
                name.as_str(),
                "call/cc" | "call-with-current-continuation" | "dynamic-wind"
            )
        }
        Expr::List(items, _) => items.iter().any(expr_uses_continuations),
        _ => false,
    }
}

fn eval_sequence_cps(
    exprs: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((first, rest)) = exprs.split_first() else {
        return k(Value::Void, output);
    };

    if rest.is_empty() {
        return eval_cps(first.clone(), env, output, k, runtime);
    }

    let rest_exprs = rest.to_vec();
    let rest_env = env.clone();
    let rest_k = k.clone();
    let next_runtime = runtime.clone();
    eval_cps(
        first.clone(),
        env,
        output,
        Rc::new(move |_value, output| {
            eval_sequence_cps(
                rest_exprs.clone(),
                rest_env.clone(),
                output,
                rest_k.clone(),
                next_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn eval_exprs_to_values_cps(
    exprs: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: ValuesContinuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    eval_exprs_to_values_acc_cps(Vec::new(), exprs, env, output, k, runtime)
}

fn eval_exprs_to_values_acc_cps(
    values: Vec<Value>,
    exprs: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: ValuesContinuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return k(values, output);
    };

    let prefix_exprs = prefix.to_vec();
    let rest_env = env.clone();
    let rest_k = k.clone();
    let next_runtime = runtime.clone();
    eval_cps(
        last.clone(),
        env,
        output,
        Rc::new(move |value, output| {
            let mut next_values = Vec::with_capacity(values.len() + 1);
            next_values.push(value);
            next_values.extend(values.clone());
            eval_exprs_to_values_acc_cps(
                next_values,
                prefix_exprs.clone(),
                rest_env.clone(),
                output,
                rest_k.clone(),
                next_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn eval_cps(
    expr: Expr,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value, _) => k(Value::Number(value), output),
        Expr::Boolean(value, _) => k(Value::Boolean(value), output),
        Expr::String(value, _) => k(Value::String(SchemeString::literal(&value)), output),
        Expr::Char(value, _) => k(Value::Char(value), output),
        Expr::Symbol(name, pos) => env
            .lookup(&name)
            .ok_or(EvalError::UnboundVariable { name })
            .map_err(|error| error.with_position(pos))
            .and_then(|value| k(value, output)),
        Expr::List(items, pos) => {
            eval_list_cps(items, env, output, k, runtime).map_err(|error| error.with_position(pos))
        }
    }
}

fn eval_list_cps(
    items: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define_cps(tail.to_vec(), env, output, k, runtime),
            "define-syntax" => {
                return eval_define_syntax(tail, &env).and_then(|value| k(value, output));
            }
            "define-record-type" => {
                return eval_define_record_type(tail, &env).and_then(|value| k(value, output));
            }
            "set!" => return eval_set_cps(tail.to_vec(), env, output, k, runtime),
            "if" => return eval_if_cps(tail.to_vec(), env, output, k, runtime),
            "quote" => return eval_quote(tail).and_then(|value| k(value, output)),
            "lambda" => {
                return build_lambda(tail, &env, None).and_then(|value| k(value, output));
            }
            "case-lambda" => {
                return build_case_lambda(tail, &env, None).and_then(|value| k(value, output));
            }
            "and" => return eval_and_cps(tail.to_vec(), env, output, k, runtime),
            "or" => return eval_or_cps(tail.to_vec(), env, output, k, runtime),
            "begin" => return eval_sequence_cps(tail.to_vec(), env, output, k, runtime),
            "cond" => return eval_cond_cps(tail.to_vec(), env, output, k, runtime),
            "let" => return eval_let_cps(tail.to_vec(), env, output, k, runtime),
            "let*" => return eval_let_star_cps(tail.to_vec(), env, output, k, runtime),
            "letrec" => return eval_letrec_cps(tail.to_vec(), env, output, k, runtime, false),
            "letrec*" => return eval_letrec_cps(tail.to_vec(), env, output, k, runtime, true),
            "case" => return eval_case_cps(tail.to_vec(), env, output, k, runtime),
            "do" => return eval_do_cps(tail.to_vec(), env, output, k, runtime),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let expansion = expand_macro_call(&items, &transformer)?;
            let expanded_env = env_with_expansion_aliases(&env, &expansion);
            return eval_cps(expansion.expr, expanded_env, output, k, runtime);
        }
    }

    let arg_exprs = tail.to_vec();
    let arg_env = env.clone();
    let arg_k = k.clone();
    let arg_runtime = runtime.clone();
    eval_cps(
        head.clone(),
        env,
        output,
        Rc::new(move |callable, output| {
            let callable_for_apply = callable.clone();
            let apply_k = arg_k.clone();
            let eval_args_runtime = arg_runtime.clone();
            let apply_runtime = arg_runtime.clone();
            eval_exprs_to_values_cps(
                arg_exprs.clone(),
                arg_env.clone(),
                output,
                Rc::new(move |args, output| {
                    apply_cps(
                        callable_for_apply.clone(),
                        args,
                        output,
                        apply_k.clone(),
                        apply_runtime.clone(),
                    )
                }),
                eval_args_runtime,
            )
        }),
        runtime,
    )
}

fn eval_define_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    match args.as_slice() {
        [Expr::Symbol(name, _), value_expr] => {
            if let Some(parts) = lambda_parts(value_expr) {
                let value = build_lambda(parts, &env, Some(name.clone()))?;
                env.define(name.clone(), value);
                k(Value::Void, output)
            } else if let Some(clauses) = case_lambda_clauses(value_expr) {
                let value = build_case_lambda(clauses, &env, Some(name.clone()))?;
                env.define(name.clone(), value);
                k(Value::Void, output)
            } else {
                let define_env = env.clone();
                let define_name = name.clone();
                let define_k = k.clone();
                eval_cps(
                    value_expr.clone(),
                    env,
                    output,
                    Rc::new(move |value, output| {
                        define_env.define(define_name.clone(), value);
                        define_k.clone()(Value::Void, output)
                    }),
                    runtime,
                )
            }
        }
        [Expr::List(signature, _), body @ ..] => {
            let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
                return Err(EvalError::Syntax {
                    message: "define: expected function name".into(),
                });
            };
            if body.is_empty() {
                return Err(EvalError::Syntax {
                    message: "define: expected function body".into(),
                });
            }

            let value = new_procedure(Some(name.clone()), parse_param_list(params)?, body, &env);
            env.define(name.clone(), value);
            k(Value::Void, output)
        }
        _ => Err(EvalError::Syntax {
            message: "define: invalid syntax".into(),
        }),
    }
}

fn eval_set_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    match args.as_slice() {
        [Expr::Symbol(name, _), value_expr] => {
            let set_env = env.clone();
            let set_name = name.clone();
            let set_k = k.clone();
            eval_cps(
                value_expr.clone(),
                env,
                output,
                Rc::new(move |value, output| {
                    if set_env.set(&set_name, value) {
                        set_k.clone()(Value::Void, output)
                    } else {
                        Err(EvalError::UnboundVariable {
                            name: set_name.clone(),
                        })
                    }
                }),
                runtime,
            )
        }
        [_, _] => Err(EvalError::Syntax {
            message: "set!: expected variable name".into(),
        }),
        _ => Err(wrong_arg_count("set!", "2", args.len())),
    }
}

fn eval_if_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    match args.as_slice() {
        [condition, then_branch] => {
            let then_expr = then_branch.clone();
            let branch_env = env.clone();
            let branch_k = k.clone();
            let branch_runtime = runtime.clone();
            eval_cps(
                condition.clone(),
                env,
                output,
                Rc::new(move |value, output| {
                    if value.is_truthy() {
                        eval_cps(
                            then_expr.clone(),
                            branch_env.clone(),
                            output,
                            branch_k.clone(),
                            branch_runtime.clone(),
                        )
                    } else {
                        branch_k.clone()(Value::Void, output)
                    }
                }),
                runtime,
            )
        }
        [condition, then_branch, else_branch] => {
            let then_expr = then_branch.clone();
            let else_expr = else_branch.clone();
            let branch_env = env.clone();
            let branch_k = k.clone();
            let branch_runtime = runtime.clone();
            eval_cps(
                condition.clone(),
                env,
                output,
                Rc::new(move |value, output| {
                    let branch = if value.is_truthy() {
                        then_expr.clone()
                    } else {
                        else_expr.clone()
                    };
                    eval_cps(
                        branch,
                        branch_env.clone(),
                        output,
                        branch_k.clone(),
                        branch_runtime.clone(),
                    )
                }),
                runtime,
            )
        }
        _ => Err(wrong_arg_count("if", "2 or 3", args.len())),
    }
}

fn eval_and_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return k(Value::Boolean(true), output);
    };

    let rest_exprs = rest.to_vec();
    let rest_env = env.clone();
    let rest_k = k.clone();
    let rest_runtime = runtime.clone();
    eval_cps(
        first.clone(),
        env,
        output,
        Rc::new(move |value, output| {
            if !value.is_truthy() || rest_exprs.is_empty() {
                rest_k.clone()(value, output)
            } else {
                eval_and_cps(
                    rest_exprs.clone(),
                    rest_env.clone(),
                    output,
                    rest_k.clone(),
                    rest_runtime.clone(),
                )
            }
        }),
        runtime,
    )
}

fn eval_or_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return k(Value::Boolean(false), output);
    };

    let rest_exprs = rest.to_vec();
    let rest_env = env.clone();
    let rest_k = k.clone();
    let rest_runtime = runtime.clone();
    eval_cps(
        first.clone(),
        env,
        output,
        Rc::new(move |value, output| {
            if value.is_truthy() {
                rest_k.clone()(value, output)
            } else if rest_exprs.is_empty() {
                rest_k.clone()(Value::Boolean(false), output)
            } else {
                eval_or_cps(
                    rest_exprs.clone(),
                    rest_env.clone(),
                    output,
                    rest_k.clone(),
                    rest_runtime.clone(),
                )
            }
        }),
        runtime,
    )
}

fn eval_cond_cps(
    clauses: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((clause, rest)) = clauses.split_first() else {
        return k(Value::Void, output);
    };

    let Expr::List(items, _) = clause else {
        return Err(EvalError::Syntax {
            message: "cond: expected clause".into(),
        });
    };
    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cond: expected clause".into(),
        });
    };

    if matches!(test, Expr::Symbol(name, _) if name == "else") {
        if !rest.is_empty() {
            return Err(EvalError::Syntax {
                message: "cond: else must be last".into(),
            });
        }
        return eval_sequence_cps(body.to_vec(), env, output, k, runtime);
    }

    let clause_body = body.to_vec();
    let rest_clauses = rest.to_vec();
    let cond_env = env.clone();
    let cond_k = k.clone();
    let cond_runtime = runtime.clone();
    eval_cps(
        test.clone(),
        env,
        output,
        Rc::new(move |value, output| {
            if value.is_truthy() {
                if clause_body.is_empty() {
                    cond_k.clone()(value, output)
                } else {
                    eval_sequence_cps(
                        clause_body.clone(),
                        cond_env.clone(),
                        output,
                        cond_k.clone(),
                        cond_runtime.clone(),
                    )
                }
            } else {
                eval_cond_cps(
                    rest_clauses.clone(),
                    cond_env.clone(),
                    output,
                    cond_k.clone(),
                    cond_runtime.clone(),
                )
            }
        }),
        runtime,
    )
}

fn eval_let_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    match args.as_slice() {
        [Expr::Symbol(name, _), bindings, body @ ..] => {
            eval_named_let_cps(name, bindings.clone(), body.to_vec(), env, output, k, runtime)
        }
        [bindings, body @ ..] => {
            eval_plain_let_cps(bindings.clone(), body.to_vec(), env, output, k, runtime)
        }
        _ => Err(EvalError::Syntax {
            message: "let: invalid syntax".into(),
        }),
    }
}

fn eval_plain_let_cps(
    bindings_expr: Expr,
    body: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(&bindings_expr)?;
    let value_exprs = bindings
        .iter()
        .map(|(_, value_expr)| value_expr.clone())
        .collect::<Vec<_>>();
    let let_parent = env.clone();
    let let_bindings = bindings.clone();
    let let_body = body.clone();
    let let_k = k.clone();
    let let_runtime = runtime.clone();
    eval_exprs_to_values_cps(
        value_exprs,
        env,
        output,
        Rc::new(move |values, output| {
            let let_env = Env::new(Some(let_parent.clone()));
            for ((name, _), value) in let_bindings.iter().cloned().zip(values.into_iter()) {
                let_env.define(name, value);
            }
            eval_sequence_cps(
                let_body.clone(),
                let_env,
                output,
                let_k.clone(),
                let_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn eval_named_let_cps(
    name: &str,
    bindings_expr: Expr,
    body: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(&bindings_expr)?;
    let value_exprs = bindings
        .iter()
        .map(|(_, value_expr)| value_expr.clone())
        .collect::<Vec<_>>();
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let let_parent = env.clone();
    let let_name = name.to_string();
    let let_body = body.clone();
    let let_k = k.clone();
    let let_runtime = runtime.clone();
    eval_exprs_to_values_cps(
        value_exprs,
        env,
        output,
        Rc::new(move |values, output| {
            let let_env = Env::new(Some(let_parent.clone()));
            let procedure = new_procedure(
                Some(let_name.clone()),
                Params::fixed(params.clone()),
                &let_body,
                &let_env,
            );
            let_env.define(let_name.clone(), procedure.clone());
            apply_cps(procedure, values, output, let_k.clone(), let_runtime.clone())
        }),
        runtime,
    )
}

fn eval_let_star_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args.as_slice() else {
        return Err(EvalError::Syntax {
            message: "let*: invalid syntax".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let*: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let let_env = Env::new(Some(env));
    eval_let_star_bindings_cps(bindings, 0, let_env, body.to_vec(), output, k, runtime)
}

fn eval_let_star_bindings_cps(
    bindings: Vec<(String, Expr)>,
    index: usize,
    let_env: EnvRef,
    body: Vec<Expr>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    if index == bindings.len() {
        return eval_sequence_cps(body, let_env, output, k, runtime);
    }

    let (name, value_expr) = bindings[index].clone();
    let rest_bindings = bindings.clone();
    let rest_env = let_env.clone();
    let rest_body = body.clone();
    let rest_k = k.clone();
    let rest_runtime = runtime.clone();
    eval_cps(
        value_expr,
        let_env.clone(),
        output,
        Rc::new(move |value, output| {
            rest_env.define(name.clone(), value);
            eval_let_star_bindings_cps(
                rest_bindings.clone(),
                index + 1,
                rest_env.clone(),
                rest_body.clone(),
                output,
                rest_k.clone(),
                rest_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn eval_letrec_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
    sequential: bool,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args.as_slice() else {
        return Err(EvalError::Syntax {
            message: "letrec: invalid syntax".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "letrec: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let letrec_env = Env::new(Some(env));

    if sequential {
        eval_letrec_star_bindings_cps(bindings, 0, letrec_env, body.to_vec(), output, k, runtime)
    } else {
        let state = LetrecBindingsState {
            bindings,
            body: body.to_vec(),
            letrec_env: letrec_env.clone(),
            k,
            runtime,
        };
        for (name, _) in &state.bindings {
            state.letrec_env.define(name.clone(), Value::Void);
        }
        eval_letrec_bindings_cps(state, 0, Vec::new(), output)
    }
}

fn eval_letrec_star_bindings_cps(
    bindings: Vec<(String, Expr)>,
    index: usize,
    letrec_env: EnvRef,
    body: Vec<Expr>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    if index == bindings.len() {
        return eval_sequence_cps(body, letrec_env, output, k, runtime);
    }

    let (name, value_expr) = bindings[index].clone();
    letrec_env.define(name.clone(), Value::Void);

    let rest_bindings = bindings.clone();
    let rest_env = letrec_env.clone();
    let rest_body = body.clone();
    let rest_k = k.clone();
    let rest_runtime = runtime.clone();
    eval_letrec_initializer_cps(
        name.clone(),
        value_expr,
        letrec_env,
        output,
        Rc::new(move |value, output| {
            let updated = rest_env.set(&name, value);
            debug_assert!(updated, "letrec* binding defined before initialization");
            eval_letrec_star_bindings_cps(
                rest_bindings.clone(),
                index + 1,
                rest_env.clone(),
                rest_body.clone(),
                output,
                rest_k.clone(),
                rest_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn eval_letrec_bindings_cps(
    state: LetrecBindingsState,
    index: usize,
    values: Vec<Value>,
    output: &mut String,
) -> Result<Value, EvalError> {
    if index == state.bindings.len() {
        for ((name, _), value) in state.bindings.iter().zip(values.into_iter()) {
            let updated = state.letrec_env.set(name, value);
            debug_assert!(updated, "letrec binding defined before initialization");
        }
        return eval_sequence_cps(
            state.body.clone(),
            state.letrec_env.clone(),
            output,
            state.k.clone(),
            state.runtime.clone(),
        );
    }

    let (name, value_expr) = state.bindings[index].clone();
    let next_state = state.clone();
    eval_letrec_initializer_cps(
        name,
        value_expr,
        state.letrec_env.clone(),
        output,
        Rc::new(move |value, output| {
            let mut next_values = values.clone();
            next_values.push(value);
            eval_letrec_bindings_cps(next_state.clone(), index + 1, next_values, output)
        }),
        state.runtime,
    )
}

fn eval_letrec_initializer_cps(
    name: String,
    value_expr: Expr,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    if let Some(parts) = lambda_parts(&value_expr) {
        let value = build_lambda(parts, &env, Some(name))?;
        k(value, output)
    } else if let Some(clauses) = case_lambda_clauses(&value_expr) {
        let value = build_case_lambda(clauses, &env, Some(name))?;
        k(value, output)
    } else {
        eval_cps(value_expr, env, output, k, runtime)
    }
}

fn eval_case_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let [key_expr, clauses @ ..] = args.as_slice() else {
        return Err(EvalError::Syntax {
            message: "case: invalid syntax".into(),
        });
    };

    let case_clauses = clauses.to_vec();
    let case_env = env.clone();
    let case_k = k.clone();
    let case_runtime = runtime.clone();
    eval_cps(
        key_expr.clone(),
        env,
        output,
        Rc::new(move |key, output| {
            eval_case_clauses_cps(
                key,
                case_clauses.clone(),
                case_env.clone(),
                output,
                case_k.clone(),
                case_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn eval_case_clauses_cps(
    key: Value,
    clauses: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let Some((clause, rest)) = clauses.split_first() else {
        return k(Value::Void, output);
    };

    let Expr::List(items, _) = clause else {
        return Err(EvalError::Syntax {
            message: "case: expected clause".into(),
        });
    };
    let Some((datum_expr, body)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "case: expected clause".into(),
        });
    };

    if matches!(datum_expr, Expr::Symbol(name, _) if name == "else") {
        if !rest.is_empty() {
            return Err(EvalError::Syntax {
                message: "case: else must be last".into(),
            });
        }
        return if body.is_empty() {
            k(Value::Void, output)
        } else {
            eval_sequence_cps(body.to_vec(), env, output, k, runtime)
        };
    }

    let Expr::List(datums, _) = datum_expr else {
        return Err(EvalError::Syntax {
            message: "case: expected datum list".into(),
        });
    };

    if datums
        .iter()
        .map(quote_expr)
        .any(|datum| eqv_values(&key, &datum))
    {
        if body.is_empty() {
            k(Value::Void, output)
        } else {
            eval_sequence_cps(body.to_vec(), env, output, k, runtime)
        }
    } else {
        eval_case_clauses_cps(key, rest.to_vec(), env, output, k, runtime)
    }
}

fn eval_do_cps(
    args: Vec<Expr>,
    env: EnvRef,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let [bindings_expr, test_clause_expr, body @ ..] = args.as_slice() else {
        return Err(EvalError::Syntax {
            message: "do: invalid syntax".into(),
        });
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_test_clause(test_clause_expr)?;
    let loop_env = Env::new(Some(env.clone()));
    let loop_state = DoLoopState {
        bindings,
        test_expr,
        result_exprs,
        body: body.to_vec(),
        loop_env,
        k,
    };
    let init_exprs = loop_state
        .bindings
        .iter()
        .map(|binding| binding.init.clone())
        .collect::<Vec<_>>();
    let initial_state = loop_state.clone();
    let loop_runtime = runtime.clone();
    eval_exprs_to_values_cps(
        init_exprs,
        env,
        output,
        Rc::new(move |values, output| {
            initial_state.define_initial_values(values);
            run_do_loop_cps(initial_state.clone(), output, loop_runtime.clone())
        }),
        runtime,
    )
}

fn run_do_loop_cps(
    state: DoLoopState,
    output: &mut String,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    let test_expr = state.test_expr.clone();
    let test_results = state.result_exprs.clone();
    let test_body = state.body.clone();
    let test_env = state.loop_env.clone();
    let test_k = state.k.clone();
    let continue_state = state.clone();
    let test_runtime = runtime.clone();
    eval_cps(
        test_expr.clone(),
        state.loop_env,
        output,
        Rc::new(move |value, output| {
            if value.is_truthy() {
                if test_results.is_empty() {
                    test_k.clone()(Value::Void, output)
                } else {
                    eval_sequence_cps(
                        test_results.clone(),
                        test_env.clone(),
                        output,
                        test_k.clone(),
                        test_runtime.clone(),
                    )
                }
            } else {
                let next_loop_state = continue_state.clone();
                let continue_runtime = test_runtime.clone();
                let continue_loop: Continuation = Rc::new(move |_value, output| {
                    eval_do_steps_cps(
                        next_loop_state.clone(),
                        0,
                        Vec::new(),
                        output,
                        continue_runtime.clone(),
                    )
                });
                if test_body.is_empty() {
                    continue_loop(Value::Void, output)
                } else {
                    eval_sequence_cps(
                        test_body.clone(),
                        test_env.clone(),
                        output,
                        continue_loop,
                        test_runtime.clone(),
                    )
                }
            }
        }),
        runtime,
    )
}

fn eval_do_steps_cps(
    state: DoLoopState,
    index: usize,
    next_values: Vec<Value>,
    output: &mut String,
    runtime: CpsRuntimeRef,
) -> Result<Value, EvalError> {
    if index == state.bindings.len() {
        state.update_step_values(next_values);
        return run_do_loop_cps(state, output, runtime);
    }

    let binding = state.bindings[index].clone();
    match binding.step {
        Some(step_expr) => {
            let step_env = state.loop_env.clone();
            let next_state = state.clone();
            let next_runtime = runtime.clone();
            eval_cps(
                step_expr,
                step_env,
                output,
                Rc::new(move |value, output| {
                    let mut updated_values = next_values.clone();
                    updated_values.push(value);
                    eval_do_steps_cps(
                        next_state.clone(),
                        index + 1,
                        updated_values,
                        output,
                        next_runtime.clone(),
                    )
                }),
                runtime,
            )
        }
        None => {
            let current = state
                .loop_env
                .lookup(&binding.name)
                .expect("do binding is always present");
            let mut updated_values = next_values;
            updated_values.push(current);
            eval_do_steps_cps(state, index + 1, updated_values, output, runtime)
        }
    }
}

#[cfg(test)]
mod tests;
