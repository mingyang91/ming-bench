use std::rc::Rc;

use super::builtins::{apply_builtin, eqv_values};
use super::error::EvalError;
use super::macros::{env_with_expansion_aliases, expand_macro_call, parse_syntax_rules};
use super::model::{
    list_from_values, ContinuationProc, Env, EnvRef, Expr, Params, Procedure, ProcedureClause,
    ProcedureKind, SchemeString, Value,
};
use super::records::{apply_record_procedure, eval_define_record_type};

pub(super) fn eval_sequence(
    exprs: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = eval(expr, env, output)?;
    }

    Ok(result)
}

enum TailAction {
    Return(Value),
    Continue { expr: Expr, env: EnvRef },
}

fn tail_sequence(
    exprs: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(TailAction::Return(Value::Void));
    };

    for expr in prefix {
        eval(expr, env, output)?;
    }

    Ok(TailAction::Continue {
        expr: last.clone(),
        env: env.clone(),
    })
}

fn eval_tail(expr: Expr, env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut current_expr = expr;
    let mut current_env = env;

    loop {
        let pos = current_expr.pos();

        match current_expr {
            Expr::Number(value, _) => return Ok(Value::Number(value)),
            Expr::Boolean(value, _) => return Ok(Value::Boolean(value)),
            Expr::String(value, _) => {
                return Ok(Value::String(SchemeString::literal(&value)));
            }
            Expr::Char(value, _) => return Ok(Value::Char(value)),
            Expr::Symbol(name, _) => {
                return current_env
                    .lookup(&name)
                    .ok_or(EvalError::UnboundVariable { name })
                    .map_err(|error| error.with_position(pos));
            }
            Expr::List(items, _) => {
                match eval_tail_list(items, &current_env, output)
                    .map_err(|error| error.with_position(pos))?
                {
                    TailAction::Return(value) => return Ok(value),
                    TailAction::Continue { expr, env } => {
                        current_expr = expr;
                        current_env = env;
                    }
                }
            }
        }
    }
}

fn eval_tail_list(
    items: Vec<Expr>,
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env, output).map(TailAction::Return),
            "define-syntax" => return eval_define_syntax(tail, env).map(TailAction::Return),
            "define-record-type" => {
                return eval_define_record_type(tail, env).map(TailAction::Return);
            }
            "set!" => return eval_set(tail, env, output).map(TailAction::Return),
            "if" => return eval_tail_if(tail, env, output),
            "quote" => return eval_quote(tail).map(TailAction::Return),
            "lambda" => return build_lambda(tail, env, None).map(TailAction::Return),
            "case-lambda" => return build_case_lambda(tail, env, None).map(TailAction::Return),
            "and" => return eval_tail_and(tail, env, output),
            "or" => return eval_tail_or(tail, env, output),
            "begin" => return eval_tail_begin(tail, env, output),
            "cond" => return eval_tail_cond(tail, env, output),
            "let" => return eval_tail_let(tail, env, output),
            "let*" => return eval_tail_let_star(tail, env, output),
            "letrec" => return eval_tail_letrec(tail, env, output, false),
            "letrec*" => return eval_tail_letrec(tail, env, output, true),
            "case" => return eval_tail_case(tail, env, output),
            "do" => return eval_tail_do(tail, env, output),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let expansion = expand_macro_call(&items, &transformer)?;
            let expanded_env = env_with_expansion_aliases(env, &expansion);
            return Ok(TailAction::Continue {
                expr: expansion.expr,
                env: expanded_env,
            });
        }
    }

    let callable = eval(head, env, output)?;
    let args = eval_args(tail, env, output)?;
    apply_tail(callable, &args, output)
}

fn eval_tail_if(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<TailAction, EvalError> {
    match args {
        [condition, then_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                Ok(TailAction::Continue {
                    expr: then_branch.clone(),
                    env: env.clone(),
                })
            } else {
                Ok(TailAction::Return(Value::Void))
            }
        }
        [condition, then_branch, else_branch] => {
            let branch = if eval(condition, env, output)?.is_truthy() {
                then_branch
            } else {
                else_branch
            };
            Ok(TailAction::Continue {
                expr: branch.clone(),
                env: env.clone(),
            })
        }
        _ => Err(wrong_arg_count("if", "2 or 3", args.len())),
    }
}

fn eval_tail_and(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailAction::Return(Value::Boolean(true)));
    };

    for expr in prefix {
        let value = eval(expr, env, output)?;
        if !value.is_truthy() {
            return Ok(TailAction::Return(value));
        }
    }

    Ok(TailAction::Continue {
        expr: last.clone(),
        env: env.clone(),
    })
}

fn eval_tail_or(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<TailAction, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailAction::Return(Value::Boolean(false)));
    };

    for expr in prefix {
        let value = eval(expr, env, output)?;
        if value.is_truthy() {
            return Ok(TailAction::Return(value));
        }
    }

    Ok(TailAction::Continue {
        expr: last.clone(),
        env: env.clone(),
    })
}

fn eval_tail_begin(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    tail_sequence(args, env, output)
}

fn eval_tail_cond(
    clauses: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
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
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "cond: else must be last".into(),
                });
            }
            return tail_sequence(body, env, output);
        }

        let value = eval(test, env, output)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(TailAction::Return(value))
            } else {
                tail_sequence(body, env, output)
            };
        }
    }

    Ok(TailAction::Return(Value::Void))
}

fn eval_tail_let(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    match args {
        [Expr::Symbol(name, _), bindings, body @ ..] => {
            eval_tail_named_let(name, bindings, body, env, output)
        }
        [bindings, body @ ..] => eval_tail_plain_let(bindings, body, env, output),
        _ => Err(EvalError::Syntax {
            message: "let: invalid syntax".into(),
        }),
    }
}

fn eval_tail_let_star(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    let [bindings_expr, body @ ..] = args else {
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
    let let_env = Env::new(Some(env.clone()));
    for (name, value_expr) in bindings {
        let value = eval(&value_expr, &let_env, output)?;
        let_env.define(name, value);
    }

    tail_sequence(body, &let_env, output)
}

fn eval_tail_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut values = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        values.push(eval(value_expr, env, output)?);
    }

    let let_env = Env::new(Some(env.clone()));
    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    tail_sequence(body, &let_env, output)
}

fn eval_tail_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut args = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        args.push(eval(value_expr, env, output)?);
    }
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();

    let let_env = Env::new(Some(env.clone()));
    let procedure = new_procedure(Some(name.into()), Params::fixed(params), body, &let_env);
    let_env.define(name.into(), procedure.clone());
    apply_tail(procedure, &args, output)
}

fn eval_tail_letrec(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
    sequential: bool,
) -> Result<TailAction, EvalError> {
    let [bindings_expr, body @ ..] = args else {
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
    let letrec_env = Env::new(Some(env.clone()));

    if sequential {
        for (name, value_expr) in bindings {
            letrec_env.define(name.clone(), Value::Void);
            let value = eval_letrec_initializer(&name, &value_expr, &letrec_env, output)?;
            let updated = letrec_env.set(&name, value);
            debug_assert!(updated, "letrec* binding defined before initialization");
        }
    } else {
        for (name, _) in &bindings {
            letrec_env.define(name.clone(), Value::Void);
        }

        let mut values = Vec::with_capacity(bindings.len());
        for (name, value_expr) in &bindings {
            values.push(eval_letrec_initializer(
                name,
                value_expr,
                &letrec_env,
                output,
            )?);
        }

        for ((name, _), value) in bindings.into_iter().zip(values) {
            let updated = letrec_env.set(&name, value);
            debug_assert!(updated, "letrec binding defined before initialization");
        }
    }

    tail_sequence(body, &letrec_env, output)
}

fn eval_tail_case(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<TailAction, EvalError> {
    let [key_expr, clauses @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "case: invalid syntax".into(),
        });
    };

    let key = eval(key_expr, env, output)?;

    for (index, clause) in clauses.iter().enumerate() {
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
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "case: else must be last".into(),
                });
            }
            return if body.is_empty() {
                Ok(TailAction::Return(Value::Void))
            } else {
                tail_sequence(body, env, output)
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
            return if body.is_empty() {
                Ok(TailAction::Return(Value::Void))
            } else {
                tail_sequence(body, env, output)
            };
        }
    }

    Ok(TailAction::Return(Value::Void))
}

fn eval_tail_do(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<TailAction, EvalError> {
    let [bindings_expr, test_clause_expr, body @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "do: invalid syntax".into(),
        });
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_test_clause(test_clause_expr)?;

    let loop_env = Env::new(Some(env.clone()));
    let mut initial_values = Vec::with_capacity(bindings.len());
    for binding in &bindings {
        initial_values.push(eval(&binding.init, env, output)?);
    }
    for (binding, value) in bindings.iter().zip(initial_values) {
        loop_env.define(binding.name.clone(), value);
    }

    loop {
        if eval(&test_expr, &loop_env, output)?.is_truthy() {
            return tail_sequence(&result_exprs, &loop_env, output);
        }

        if !body.is_empty() {
            eval_sequence(body, &loop_env, output)?;
        }

        let mut next_values = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let value = match &binding.step {
                Some(step) => eval(step, &loop_env, output)?,
                None => loop_env
                    .lookup(&binding.name)
                    .expect("do binding is always present"),
            };
            next_values.push(value);
        }

        for (binding, value) in bindings.iter().zip(next_values) {
            let updated = loop_env.set(&binding.name, value);
            debug_assert!(updated, "do binding defined before loop step");
        }
    }
}

fn eval(expr: &Expr, env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let pos = expr.pos();

    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(SchemeString::literal(value))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })
            .map_err(|error| error.with_position(pos)),
        Expr::List(items, _) => {
            eval_list(items, env, output).map_err(|error| error.with_position(pos))
        }
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env, output),
            "define-syntax" => return eval_define_syntax(tail, env),
            "define-record-type" => return eval_define_record_type(tail, env),
            "set!" => return eval_set(tail, env, output),
            "if" => return eval_if(tail, env, output),
            "quote" => return eval_quote(tail),
            "lambda" => return build_lambda(tail, env, None),
            "case-lambda" => return build_case_lambda(tail, env, None),
            "and" => return eval_and(tail, env, output),
            "or" => return eval_or(tail, env, output),
            "begin" => return eval_begin(tail, env, output),
            "cond" => return eval_cond(tail, env, output),
            "let" => return eval_let(tail, env, output),
            "let*" => return eval_let_star(tail, env, output),
            "letrec" => return eval_letrec(tail, env, output, false),
            "letrec*" => return eval_letrec(tail, env, output, true),
            "case" => return eval_case(tail, env, output),
            "do" => return eval_do(tail, env, output),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let expansion = expand_macro_call(items, &transformer)?;
            let expanded_env = env_with_expansion_aliases(env, &expansion);
            return eval(&expansion.expr, &expanded_env, output);
        }
    }

    let callable = eval(head, env, output)?;
    let args = eval_args(tail, env, output)?;
    apply(callable, &args, output)
}

fn eval_define(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = if let Some(parts) = lambda_parts(value_expr) {
                build_lambda(parts, env, Some(name.clone()))?
            } else if let Some(clauses) = case_lambda_clauses(value_expr) {
                build_case_lambda(clauses, env, Some(name.clone()))?
            } else {
                eval(value_expr, env, output)?
            };
            env.define(name.clone(), value);
            Ok(Value::Void)
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

            let value = new_procedure(Some(name.clone()), parse_param_list(params)?, body, env);
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "define: invalid syntax".into(),
        }),
    }
}

pub(super) fn eval_define_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), transformer_expr] => {
            let transformer = parse_syntax_rules(transformer_expr, env)?;
            env.define_macro(name.clone(), transformer);
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::Syntax {
            message: "define-syntax: expected transformer name".into(),
        }),
        _ => Err(wrong_arg_count("define-syntax", "2", args.len())),
    }
}

fn eval_set(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = eval(value_expr, env, output)?;
            if env.set(name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() })
            }
        }
        [_, _] => Err(EvalError::Syntax {
            message: "set!: expected variable name".into(),
        }),
        _ => Err(wrong_arg_count("set!", "2", args.len())),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, then_branch, else_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                eval(else_branch, env, output)
            }
        }
        _ => Err(wrong_arg_count("if", "2 or 3", args.len())),
    }
}

pub(super) fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote_expr(expr)),
        _ => Err(wrong_arg_count("quote", "1", args.len())),
    }
}

fn eval_args(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env, output)?);
    }
    Ok(values)
}

fn eval_and(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for arg in args {
        let value = eval(arg, env, output)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval(arg, env, output)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    eval_sequence(args, env, output)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
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
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "cond: else must be last".into(),
                });
            }
            return eval_sequence(body, env, output);
        }

        let value = eval(test, env, output)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), bindings, body @ ..] => {
            eval_named_let(name, bindings, body, env, output)
        }
        [bindings, body @ ..] => eval_plain_let(bindings, body, env, output),
        _ => Err(EvalError::Syntax {
            message: "let: invalid syntax".into(),
        }),
    }
}

fn eval_let_star(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
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
    let let_env = Env::new(Some(env.clone()));
    for (name, value_expr) in bindings {
        let value = eval(&value_expr, &let_env, output)?;
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env, output)
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut values = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        values.push(eval(value_expr, env, output)?);
    }

    let let_env = Env::new(Some(env.clone()));
    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env, output)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut args = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        args.push(eval(value_expr, env, output)?);
    }
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();

    let let_env = Env::new(Some(env.clone()));
    let procedure = new_procedure(Some(name.into()), Params::fixed(params), body, &let_env);
    let_env.define(name.into(), procedure.clone());
    apply(procedure, &args, output)
}

pub(super) fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "let: expected bindings".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            Expr::List(parts, _) => match parts.as_slice() {
                [Expr::Symbol(name, _), value_expr] => {
                    parsed.push((name.clone(), value_expr.clone()));
                }
                _ => {
                    return Err(EvalError::Syntax {
                        message: "let: expected binding pair".into(),
                    });
                }
            },
            _ => {
                return Err(EvalError::Syntax {
                    message: "let: expected binding pair".into(),
                });
            }
        }
    }

    Ok(parsed)
}

fn eval_letrec(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
    sequential: bool,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
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
    let letrec_env = Env::new(Some(env.clone()));

    if sequential {
        for (name, value_expr) in bindings {
            letrec_env.define(name.clone(), Value::Void);
            let value = eval_letrec_initializer(&name, &value_expr, &letrec_env, output)?;
            let updated = letrec_env.set(&name, value);
            debug_assert!(updated, "letrec* binding defined before initialization");
        }
    } else {
        for (name, _) in &bindings {
            letrec_env.define(name.clone(), Value::Void);
        }

        let mut values = Vec::with_capacity(bindings.len());
        for (name, value_expr) in &bindings {
            values.push(eval_letrec_initializer(
                name,
                value_expr,
                &letrec_env,
                output,
            )?);
        }

        for ((name, _), value) in bindings.into_iter().zip(values) {
            let updated = letrec_env.set(&name, value);
            debug_assert!(updated, "letrec binding defined before initialization");
        }
    }

    eval_sequence(body, &letrec_env, output)
}

fn eval_letrec_initializer(
    name: &str,
    value_expr: &Expr,
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if let Some(parts) = lambda_parts(value_expr) {
        build_lambda(parts, env, Some(name.into()))
    } else if let Some(clauses) = case_lambda_clauses(value_expr) {
        build_case_lambda(clauses, env, Some(name.into()))
    } else {
        eval(value_expr, env, output)
    }
}

fn eval_case(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [key_expr, clauses @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "case: invalid syntax".into(),
        });
    };

    let key = eval(key_expr, env, output)?;

    for (index, clause) in clauses.iter().enumerate() {
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
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "case: else must be last".into(),
                });
            }
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, output)
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
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_do(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, test_clause_expr, body @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "do: invalid syntax".into(),
        });
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_test_clause(test_clause_expr)?;

    let loop_env = Env::new(Some(env.clone()));
    let mut initial_values = Vec::with_capacity(bindings.len());
    for binding in &bindings {
        initial_values.push(eval(&binding.init, env, output)?);
    }
    for (binding, value) in bindings.iter().zip(initial_values) {
        loop_env.define(binding.name.clone(), value);
    }

    loop {
        if eval(&test_expr, &loop_env, output)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(&result_exprs, &loop_env, output)
            };
        }

        if !body.is_empty() {
            eval_sequence(body, &loop_env, output)?;
        }

        let mut next_values = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let value = match &binding.step {
                Some(step) => eval(step, &loop_env, output)?,
                None => loop_env
                    .lookup(&binding.name)
                    .expect("do binding is always present"),
            };
            next_values.push(value);
        }

        for (binding, value) in bindings.iter().zip(next_values) {
            let updated = loop_env.set(&binding.name, value);
            debug_assert!(updated, "do binding defined before loop step");
        }
    }
}

#[derive(Clone)]
pub(super) struct DoBinding {
    pub(super) name: String,
    pub(super) init: Expr,
    pub(super) step: Option<Expr>,
}

#[derive(Clone)]
pub(super) struct DoLoopState {
    pub(super) bindings: Vec<DoBinding>,
    pub(super) test_expr: Expr,
    pub(super) result_exprs: Vec<Expr>,
    pub(super) body: Vec<Expr>,
    pub(super) loop_env: EnvRef,
    pub(super) k: ContinuationProc,
}

impl DoLoopState {
    pub(super) fn define_initial_values(&self, values: Vec<Value>) {
        for (binding, value) in self.bindings.iter().zip(values) {
            self.loop_env.define(binding.name.clone(), value);
        }
    }

    pub(super) fn update_step_values(&self, values: Vec<Value>) {
        for (binding, value) in self.bindings.iter().zip(values) {
            let updated = self.loop_env.set(&binding.name, value);
            debug_assert!(updated, "do binding defined before loop step");
        }
    }
}

pub(super) fn parse_do_bindings(bindings_expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "do: expected bindings".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            Expr::List(parts, _) => match parts.as_slice() {
                [Expr::Symbol(name, _), init] => parsed.push(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: None,
                }),
                [Expr::Symbol(name, _), init, step] => parsed.push(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: Some(step.clone()),
                }),
                _ => {
                    return Err(EvalError::Syntax {
                        message: "do: expected (name init [step])".into(),
                    });
                }
            },
            _ => {
                return Err(EvalError::Syntax {
                    message: "do: expected (name init [step])".into(),
                });
            }
        }
    }

    Ok(parsed)
}

pub(super) fn parse_do_test_clause(
    test_clause_expr: &Expr,
) -> Result<(Expr, Vec<Expr>), EvalError> {
    let Expr::List(items, _) = test_clause_expr else {
        return Err(EvalError::Syntax {
            message: "do: expected termination clause".into(),
        });
    };
    let Some((test_expr, result_exprs)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "do: expected termination test".into(),
        });
    };

    Ok((test_expr.clone(), result_exprs.to_vec()))
}

pub(super) fn build_lambda(
    parts: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = parts else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameters and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda: expected body".into(),
        });
    }

    Ok(new_procedure(
        name,
        parse_params_expr(params_expr)?,
        body,
        env,
    ))
}

pub(super) fn build_case_lambda(
    clauses: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    if clauses.is_empty() {
        return Err(EvalError::Syntax {
            message: "case-lambda: expected at least one clause".into(),
        });
    }

    let mut parsed_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::Syntax {
                message: "case-lambda: expected clause".into(),
            });
        };

        let [params_expr, body @ ..] = items.as_slice() else {
            return Err(EvalError::Syntax {
                message: "case-lambda: expected parameters and body".into(),
            });
        };

        if body.is_empty() {
            return Err(EvalError::Syntax {
                message: "case-lambda: expected body".into(),
            });
        }

        parsed_clauses.push(ProcedureClause {
            params: parse_params_expr(params_expr)?,
            body: body.to_vec(),
        });
    }

    Ok(new_case_procedure(name, parsed_clauses, env))
}

pub(super) fn new_procedure(
    name: Option<String>,
    params: Params,
    body: &[Expr],
    env: &EnvRef,
) -> Value {
    Value::Procedure(Rc::new(Procedure {
        kind: ProcedureKind::Lambda,
        name,
        clauses: vec![ProcedureClause {
            params,
            body: body.to_vec(),
        }],
        env: env.clone(),
    }))
}

fn new_case_procedure(name: Option<String>, clauses: Vec<ProcedureClause>, env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        kind: ProcedureKind::CaseLambda,
        name,
        clauses,
        env: env.clone(),
    }))
}

fn parse_params_expr(params: &Expr) -> Result<Params, EvalError> {
    let Expr::List(items, _) = params else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameter list".into(),
        });
    };

    parse_param_list(items)
}

pub(super) fn parse_param_list(params: &[Expr]) -> Result<Params, EvalError> {
    let mut required = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                if index + 2 != params.len() {
                    return Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    });
                }

                return match &params[index + 1] {
                    Expr::Symbol(name, _) if name != "." => Ok(Params {
                        required,
                        rest: Some(name.clone()),
                    }),
                    _ => Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    }),
                };
            }
            Expr::Symbol(name, _) => required.push(name.clone()),
            _ => {
                return Err(EvalError::Syntax {
                    message: "lambda: expected parameter name".into(),
                });
            }
        }

        index += 1;
    }

    Ok(Params {
        required,
        rest: None,
    })
}

pub(super) fn lambda_parts(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items, _) = expr else {
        return None;
    };
    let (Expr::Symbol(name, _), tail) = items.split_first()? else {
        return None;
    };

    if name == "lambda" {
        Some(tail)
    } else {
        None
    }
}

pub(super) fn case_lambda_clauses(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items, _) = expr else {
        return None;
    };
    let (Expr::Symbol(name, _), tail) = items.split_first()? else {
        return None;
    };

    if name == "case-lambda" {
        Some(tail)
    } else {
        None
    }
}

pub(super) fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value, _) => Value::Number(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(SchemeString::literal(value)),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => list_from_values(items.iter().map(quote_expr)),
    }
}

pub(super) fn apply(
    callable: Value,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, args, output),
        Value::Procedure(procedure) => apply_procedure(&procedure, args, output),
        Value::Continuation(continuation) => match args {
            [value] => continuation(value.clone(), output),
            _ => Err(wrong_arg_count("continuation", "1", args.len())),
        },
        Value::RecordProcedure(procedure) => apply_record_procedure(&procedure, args),
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }),
    }
}

fn apply_tail(
    callable: Value,
    args: &[Value],
    output: &mut String,
) -> Result<TailAction, EvalError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, args, output).map(TailAction::Return),
        Value::Procedure(procedure) => prepare_tail_procedure(&procedure, args, output),
        Value::Continuation(continuation) => match args {
            [value] => continuation(value.clone(), output).map(TailAction::Return),
            _ => Err(wrong_arg_count("continuation", "1", args.len())),
        },
        Value::RecordProcedure(procedure) => {
            apply_record_procedure(&procedure, args).map(TailAction::Return)
        }
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }),
    }
}

fn apply_procedure(
    procedure: &Procedure,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    match prepare_tail_procedure(procedure, args, output)? {
        TailAction::Return(value) => Ok(value),
        TailAction::Continue { expr, env } => eval_tail(expr, env, output),
    }
}

fn prepare_tail_procedure(
    procedure: &Procedure,
    args: &[Value],
    output: &mut String,
) -> Result<TailAction, EvalError> {
    let Some(clause) = procedure
        .clauses
        .iter()
        .find(|clause| clause.params.matches_arity(args.len()))
    else {
        let expected = procedure.expected_args();
        return Err(wrong_arg_count(
            procedure.error_name(),
            &expected,
            args.len(),
        ));
    };

    let call_env = Env::new(Some(procedure.env.clone()));
    for (param, arg) in clause.params.required.iter().zip(args.iter()) {
        call_env.define(param.clone(), arg.clone());
    }
    if let Some(rest) = &clause.params.rest {
        call_env.define(
            rest.clone(),
            list_from_values(args[clause.params.required.len()..].iter().cloned()),
        );
    }

    tail_sequence(&clause.body, &call_env, output)
}

pub(super) fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}
