use std::collections::HashSet;
use std::rc::Rc;

use super::continuation::{
    define_continuation, invoke_continuation, make_dynamic_wind_frame, pop_wind_frame,
    push_wind_frame, sequence_continuation, set_captured_continuation, set_symbol_continuation,
    Continuation, ContinuationRef, EvalResult, EvalSignal, RaisedException,
};
use super::macros::{expand_syntax_template, match_syntax_pattern, MacroEnvRef, MacroEnvironment};
use super::value_ops::{list_from_vec, values_eqv};
use super::{
    env_define, env_lookup, env_set, eval_expr, eval_sequence, eval_target, make_procedure,
    run_expr_in_cont, single_clause_procedure, tail_borrowed_expr, tail_borrowed_sequence,
    tail_owned_expr, EnvRef, Environment, EvalError, EvalStep, Expr, OwnedExprRef, Procedure,
    ProcedureClause, SourcePos, Value,
};

pub(super) fn eval_define(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    if let [Expr::Symbol(name, _), value_expr] = args {
        let value = run_expr_in_cont(
            value_expr,
            env,
            macro_env,
            define_continuation(env, name.clone(), continuation),
        )?;
        env_define(env, name.clone(), value);
        return Ok(Value::Void);
    }

    if let Some((Expr::List(signature, _), body)) = args.split_first() {
        let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "invalid function definition".into(),
            }
            .into());
        };

        if body.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "function definition requires a body".into(),
            }
            .into());
        }

        let (params, rest_param) = parse_param_list_items(params)?;
        let procedure = single_clause_procedure(params, rest_param, body.to_vec(), env, macro_env);

        env_define(env, name.clone(), procedure);
        return Ok(Value::Void);
    }

    Err(EvalError::SyntaxError {
        message: "invalid define".into(),
    }
    .into())
}

pub(super) fn eval_if<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    match args {
        [condition, consequent] => {
            if eval_expr(condition, env, macro_env, continuation)?.is_truthy() {
                Ok(tail_borrowed_expr(consequent, env, macro_env))
            } else {
                Ok(EvalStep::Value(Value::Void))
            }
        }
        [condition, consequent, alternate] => {
            if eval_expr(condition, env, macro_env, continuation)?.is_truthy() {
                Ok(tail_borrowed_expr(consequent, env, macro_env))
            } else {
                Ok(tail_borrowed_expr(alternate, env, macro_env))
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3 arguments".into(),
            got: args.len(),
        }
        .into()),
    }
}

fn eval_owned_list_sequence<'a>(
    expr: &OwnedExprRef,
    start: usize,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned list sequence helper requires a list");
    };

    if start >= items.len() {
        return Ok(EvalStep::Value(Value::Void));
    }

    let last_index = items.len() - 1;
    for index in start..last_index {
        run_expr_in_cont(
            &items[index],
            env,
            macro_env,
            sequence_continuation(&items[index + 1..], env, macro_env, continuation),
        )?;
    }

    Ok(tail_owned_expr(expr.child(last_index), env, macro_env))
}

fn eval_owned_nested_list_sequence<'a>(
    expr: &OwnedExprRef,
    child_index: usize,
    start: usize,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let child = expr.child(child_index);
    let Expr::List(items, _) = child.current() else {
        unreachable!("owned nested list sequence helper requires a list");
    };

    if start >= items.len() {
        return Ok(EvalStep::Value(Value::Void));
    }

    let last_index = items.len() - 1;
    for index in start..last_index {
        run_expr_in_cont(
            &items[index],
            env,
            macro_env,
            sequence_continuation(&items[index + 1..], env, macro_env, continuation),
        )?;
    }

    Ok(tail_owned_expr(child.child(last_index), env, macro_env))
}

pub(super) fn eval_owned_if<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned if helper requires a list");
    };

    match items.as_slice() {
        [_, condition, _] => {
            if eval_expr(condition, env, macro_env, continuation)?.is_truthy() {
                Ok(tail_owned_expr(expr.child(2), env, macro_env))
            } else {
                Ok(EvalStep::Value(Value::Void))
            }
        }
        [_, condition, _, _] => {
            if eval_expr(condition, env, macro_env, continuation)?.is_truthy() {
                Ok(tail_owned_expr(expr.child(2), env, macro_env))
            } else {
                Ok(tail_owned_expr(expr.child(3), env, macro_env))
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3 arguments".into(),
            got: items.len().saturating_sub(1),
        }
        .into()),
    }
}

pub(super) fn eval_owned_and<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned and helper requires a list");
    };

    if items.len() == 1 {
        return Ok(EvalStep::Value(Value::Boolean(true)));
    }

    for item in &items[1..items.len() - 1] {
        let value = eval_expr(item, env, macro_env, continuation)?;
        if !value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(tail_owned_expr(expr.child(items.len() - 1), env, macro_env))
}

pub(super) fn eval_owned_or<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned or helper requires a list");
    };

    if items.len() == 1 {
        return Ok(EvalStep::Value(Value::Boolean(false)));
    }

    for item in &items[1..items.len() - 1] {
        let value = eval_expr(item, env, macro_env, continuation)?;
        if value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(tail_owned_expr(expr.child(items.len() - 1), env, macro_env))
}

pub(super) fn eval_owned_begin<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    eval_owned_list_sequence(expr, 1, env, macro_env, continuation)
}

pub(super) fn eval_owned_cond<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned cond helper requires a list");
    };

    for clause_index in 1..items.len() {
        let Expr::List(clause_items, _) = &items[clause_index] else {
            return Err(EvalError::SyntaxError {
                message: "cond clauses must be lists".into(),
            }
            .into());
        };

        let Some((test, body)) = clause_items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "cond clause cannot be empty".into(),
            }
            .into());
        };

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if clause_index + 1 != items.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                }
                .into());
            }

            return if body.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                eval_owned_nested_list_sequence(expr, clause_index, 1, env, macro_env, continuation)
            };
        }

        let test_value = eval_expr(test, env, macro_env, continuation)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(EvalStep::Value(test_value))
            } else {
                eval_owned_nested_list_sequence(expr, clause_index, 1, env, macro_env, continuation)
            };
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

pub(super) fn eval_owned_let<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned let helper requires a list");
    };

    match items.as_slice() {
        [_, Expr::List(bindings, _), _body @ ..] => {
            eval_owned_plain_let(expr, bindings, 2, env, macro_env, continuation)
        }
        [_, Expr::Symbol(name, _), Expr::List(bindings, _), _body @ ..] => {
            eval_owned_named_let(expr, name, bindings, 3, env, macro_env, continuation)
        }
        _ => Err(EvalError::SyntaxError {
            message: "invalid let".into(),
        }
        .into()),
    }
}

pub(super) fn eval_owned_let_star<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned let* helper requires a list");
    };

    let [_, Expr::List(bindings, _), body @ ..] = items.as_slice() else {
        return Err(EvalError::SyntaxError {
            message: "invalid let*".into(),
        }
        .into());
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let* requires a body".into(),
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, "let*")?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    let let_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for (name, value_expr) in bindings {
        let value = eval_expr(value_expr, &let_env, &let_macro_env, continuation)?;
        env_define(&let_env, name, value);
    }

    eval_owned_list_sequence(expr, 2, &let_env, &let_macro_env, continuation)
}

pub(super) fn eval_owned_letrec<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    sequential: bool,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned letrec helper requires a list");
    };

    let [_, Expr::List(bindings, _), body @ ..] = items.as_slice() else {
        return Err(EvalError::SyntaxError {
            message: if sequential {
                "invalid letrec*".into()
            } else {
                "invalid letrec".into()
            },
        }
        .into());
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: if sequential {
                "letrec* requires a body".into()
            } else {
                "letrec requires a body".into()
            },
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, if sequential { "letrec*" } else { "letrec" })?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    let let_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for (name, _) in &bindings {
        env_define(&let_env, name.clone(), Value::Uninitialized);
    }

    if sequential {
        for (name, value_expr) in &bindings {
            let value = eval_expr(value_expr, &let_env, &let_macro_env, continuation)?;
            env_set(&let_env, name, value);
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for (_, value_expr) in &bindings {
            values.push(eval_expr(
                value_expr,
                &let_env,
                &let_macro_env,
                continuation,
            )?);
        }

        for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
            env_set(&let_env, name, value);
        }
    }

    eval_owned_list_sequence(expr, 2, &let_env, &let_macro_env, continuation)
}

pub(super) fn eval_owned_case<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned case helper requires a list");
    };

    if items.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        }
        .into());
    }

    let key = eval_expr(&items[1], env, macro_env, continuation)?;

    for clause_index in 2..items.len() {
        let Expr::List(clause_items, _) = &items[clause_index] else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must be lists".into(),
            }
            .into());
        };

        let Some((datums, body)) = clause_items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "case clause cannot be empty".into(),
            }
            .into());
        };

        if matches!(datums, Expr::Symbol(symbol, _) if symbol == "else") {
            if clause_index + 1 != items.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                }
                .into());
            }

            return if body.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                eval_owned_nested_list_sequence(expr, clause_index, 1, env, macro_env, continuation)
            };
        }

        let Expr::List(datums, _) = datums else {
            return Err(EvalError::SyntaxError {
                message: "case clause datums must be in a list".into(),
            }
            .into());
        };

        for datum in datums {
            let datum = quote_to_value(datum)?;
            if values_eqv(&key, &datum) {
                return if body.is_empty() {
                    Ok(EvalStep::Value(Value::Void))
                } else {
                    eval_owned_nested_list_sequence(
                        expr,
                        clause_index,
                        1,
                        env,
                        macro_env,
                        continuation,
                    )
                };
            }
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

fn eval_owned_plain_let<'a>(
    expr: &OwnedExprRef,
    bindings: &[Expr],
    body_start: usize,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned plain let helper requires a list");
    };

    if body_start >= items.len() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, "let")?;
    let params = bindings
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>();
    let arg_exprs = bindings
        .iter()
        .map(|(_, expr)| (*expr).clone())
        .collect::<Vec<_>>();
    let procedure =
        single_clause_procedure(params, None, items[body_start..].to_vec(), env, macro_env);

    eval_callable_with_expr_args(
        procedure,
        &arg_exprs,
        env,
        macro_env,
        bindings
            .first()
            .map(|(_, expr)| expr.position())
            .unwrap_or_else(|| items[body_start].position()),
        continuation,
    )
}

fn eval_owned_named_let<'a>(
    expr: &OwnedExprRef,
    name: &str,
    bindings: &[Expr],
    body_start: usize,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Expr::List(items, _) = expr.current() else {
        unreachable!("owned named let helper requires a list");
    };

    if body_start >= items.len() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, "let")?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let arg_exprs = bindings
        .iter()
        .map(|(_, expr)| (*expr).clone())
        .collect::<Vec<_>>();

    let let_env = Environment::new(Some(Rc::clone(env)));
    let procedure = single_clause_procedure(
        params,
        None,
        items[body_start..].to_vec(),
        &let_env,
        macro_env,
    );
    env_define(&let_env, name.to_string(), procedure.clone());

    eval_callable_with_expr_args(
        procedure,
        &arg_exprs,
        env,
        macro_env,
        bindings
            .first()
            .map(|(_, expr)| expr.position())
            .unwrap_or_else(|| items[body_start].position()),
        continuation,
    )
}

pub(super) fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [datum] => quote_to_value(datum),
        _ => Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

pub(super) fn eval_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [template] => expand_syntax_template(template, env),
        _ => Err(EvalError::WrongArgCount {
            name: "syntax".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

pub(super) fn eval_syntax_case(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let Some((input_expr, rest)) = args.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case requires an input, literals, and at least one clause".into(),
        }
        .into());
    };

    let Some((literal_list, clause_exprs)) = rest.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case requires a literal list and at least one clause".into(),
        }
        .into());
    };

    if clause_exprs.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "syntax-case requires at least one clause".into(),
        }
        .into());
    }

    let Expr::List(literal_items, _) = literal_list else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case literal list must be a list".into(),
        }
        .into());
    };

    let literals = literal_items
        .iter()
        .map(|literal| match literal {
            Expr::Symbol(name, _) if name != "..." => Ok(name.clone()),
            _ => Err(EvalError::SyntaxError {
                message: "syntax-case literals must be symbols".into(),
            }),
        })
        .collect::<Result<HashSet<_>, _>>()?;

    let input = eval_expr(input_expr, env, macro_env, continuation)?;

    for clause in clause_exprs {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "syntax-case clauses must be lists".into(),
            }
            .into());
        };

        let (pattern, fender, template) = match items.as_slice() {
            [pattern, template] => (pattern, None, template),
            [pattern, fender, template] => (pattern, Some(fender), template),
            _ => return Err(EvalError::SyntaxError {
                message:
                    "syntax-case clauses must be (pattern template) or (pattern fender template)"
                        .into(),
            }
            .into()),
        };

        let Some(bindings) = match_syntax_pattern(pattern, &input, &literals)? else {
            continue;
        };

        let clause_env = Environment::new(Some(Rc::clone(env)));
        for (name, binding) in bindings {
            env_define(&clause_env, name, Value::Syntax(binding));
        }

        if let Some(fender) = fender {
            if !eval_expr(fender, &clause_env, macro_env, continuation)?.is_truthy() {
                continue;
            }
        }

        return eval_expr(template, &clause_env, macro_env, continuation);
    }

    Err(EvalError::SyntaxError {
        message: "no matching syntax-case clause".into(),
    }
    .into())
}

pub(super) fn eval_with_syntax(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "with-syntax requires bindings and a body".into(),
        }
        .into());
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "with-syntax requires a body".into(),
        }
        .into());
    }

    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::SyntaxError {
            message: "with-syntax bindings must be a list".into(),
        }
        .into());
    };

    let syntax_env = Environment::new(Some(Rc::clone(env)));
    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: "with-syntax bindings must be (name expr) pairs".into(),
            }
            .into());
        };

        let [Expr::Symbol(name, _), value_expr] = items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: "with-syntax bindings must be (name expr) pairs".into(),
            }
            .into());
        };

        let value = eval_expr(value_expr, env, macro_env, continuation)?;
        match value {
            Value::Syntax(_) => env_define(&syntax_env, name.clone(), value),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "syntax",
                    found: other.type_name().into(),
                }
                .into())
            }
        }
    }

    eval_sequence(body, &syntax_env, macro_env, continuation)
}

fn quote_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => Ok(Value::Symbol(name.clone())),
        Expr::CapturedSymbol(name, _, _) => Ok(Value::Symbol(name.clone())),
        Expr::List(items, _) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_to_value(item)?);
            }
            Ok(list_from_vec(values))
        }
    }
}

pub(super) fn eval_lambda(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a parameter list and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a body".into(),
        });
    }

    let (params, rest_param) = parse_param_list(params_expr)?;
    Ok(single_clause_procedure(
        params,
        rest_param,
        body.to_vec(),
        env,
        macro_env,
    ))
}

pub(super) fn eval_case_lambda(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "case-lambda requires at least one clause".into(),
        });
    }

    let mut clauses = Vec::with_capacity(args.len());
    for clause in args {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clauses must be lists".into(),
            });
        };

        let Some((params_expr, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clause cannot be empty".into(),
            });
        };

        if body.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clause requires a body".into(),
            });
        }

        let (params, rest_param) = parse_param_list(params_expr)?;
        clauses.push(ProcedureClause::new(params, rest_param, body.to_vec()));
    }

    Ok(make_procedure(clauses, env, macro_env))
}

pub(super) fn eval_set(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    match args {
        [Expr::Symbol(name, position), value_expr] => {
            let value = run_expr_in_cont(
                value_expr,
                env,
                macro_env,
                set_symbol_continuation(env, name.clone(), continuation),
            )?;
            if env_set(env, name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() }
                    .with_position(*position)
                    .into())
            }
        }
        [Expr::CapturedSymbol(_, binding, _), value_expr] => {
            let value = run_expr_in_cont(
                value_expr,
                env,
                macro_env,
                set_captured_continuation(binding, continuation),
            )?;
            *binding.borrow_mut() = value;
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::SyntaxError {
            message: "set! target must be a symbol".into(),
        }
        .into()),
        _ => Err(EvalError::WrongArgCount {
            name: "set!".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        }
        .into()),
    }
}

fn parse_param_list(params_expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match params_expr {
        Expr::List(items, _) => parse_param_list_items(items),
        Expr::Symbol(name, _) if name != "." => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(EvalError::SyntaxError {
            message: "parameter list must be a symbol or list of symbols".into(),
        }),
    }
}

fn parse_param_list_items(items: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_index = items
        .iter()
        .position(|item| matches!(item, Expr::Symbol(name, _) if name == "."));

    let Some(dot_index) = dot_index else {
        return Ok((parse_required_param_names(items)?, None));
    };

    if items[dot_index + 1..]
        .iter()
        .any(|item| matches!(item, Expr::Symbol(name, _) if name == "."))
        || dot_index + 2 != items.len()
    {
        return Err(EvalError::SyntaxError {
            message: "invalid dotted parameter list".into(),
        });
    }

    let params = parse_required_param_names(&items[..dot_index])?;
    let rest_param = parse_param_name(&items[dot_index + 1])?;
    Ok((params, Some(rest_param)))
}

pub(super) fn parse_required_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        params.push(parse_param_name(item)?);
    }
    Ok(params)
}

fn parse_param_name(item: &Expr) -> Result<String, EvalError> {
    match item {
        Expr::Symbol(name, _) if name != "." => Ok(name.clone()),
        _ => Err(EvalError::SyntaxError {
            message: "parameter names must be symbols".into(),
        }),
    }
}

pub(super) fn apply_callable(
    callable: Value,
    args: &[Value],
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    match apply_callable_result(callable, args, continuation)? {
        EvalStep::Value(value) => Ok(value),
        EvalStep::Tail {
            target,
            env,
            macro_env,
        } => eval_target(target, env, macro_env, continuation),
    }
}

pub(super) fn apply_callable_result<'a>(
    callable: Value,
    args: &[Value],
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    match callable {
        Value::Procedure(procedure) => apply_procedure_result(&procedure, args).map_err(Into::into),
        Value::NativeProcedure(procedure) => procedure
            .apply(args)
            .map(EvalStep::Value)
            .map_err(Into::into),
        Value::Builtin(builtin) => builtin.apply(args, continuation).map(EvalStep::Value),
        Value::Continuation(continuation) => Err(invoke_continuation(
            continuation,
            Value::from_values(args.to_vec()),
        )),
        other => Err(EvalError::NotCallable {
            found: other.type_name().into(),
        }
        .into()),
    }
}

fn apply_procedure_result<'a>(
    procedure: &Procedure,
    args: &[Value],
) -> Result<EvalStep<'a>, EvalError> {
    let clause = procedure
        .clauses
        .iter()
        .find(|clause| clause.matches_arity(args.len()))
        .ok_or_else(|| EvalError::WrongArgCount {
            name: if procedure.clauses.len() > 1 {
                "case-lambda".into()
            } else {
                "lambda".into()
            },
            expected: procedure_expected_arity(procedure),
            got: args.len(),
        })?;

    let required = clause.params.len();
    let call_env = Environment::new(Some(Rc::clone(&procedure.env)));
    for (param, arg) in clause.params.iter().zip(args.iter().take(required)) {
        env_define(&call_env, param.clone(), arg.clone());
    }
    if let Some(rest_param) = &clause.rest_param {
        env_define(
            &call_env,
            rest_param.clone(),
            list_from_vec(args[required..].to_vec()),
        );
    }

    let call_macro_env = MacroEnvironment::new(Some(Rc::clone(&procedure.macro_env)));
    Ok(tail_owned_expr(
        OwnedExprRef::new(Rc::clone(&clause.body)),
        &call_env,
        &call_macro_env,
    ))
}

fn procedure_expected_arity(procedure: &Procedure) -> String {
    if let [clause] = procedure.clauses.as_slice() {
        return clause.expected_arity();
    }

    let mut arities = Vec::with_capacity(procedure.clauses.len());
    for clause in &procedure.clauses {
        let expected = clause.expected_arity();
        if !arities.contains(&expected) {
            arities.push(expected);
        }
    }

    format!("one of {}", arities.join(", "))
}

pub(super) fn eval_and<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(EvalStep::Value(Value::Boolean(true)));
    };

    for expr in prefix {
        let value = eval_expr(expr, env, macro_env, continuation)?;
        if !value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(tail_borrowed_expr(last, env, macro_env))
}

pub(super) fn eval_or<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(EvalStep::Value(Value::Boolean(false)));
    };

    for expr in prefix {
        let value = eval_expr(expr, env, macro_env, continuation)?;
        if value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(tail_borrowed_expr(last, env, macro_env))
}

pub(super) fn eval_begin<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    _continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    Ok(tail_borrowed_sequence(args, env, macro_env))
}

pub(super) fn eval_dynamic_wind(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let [before_expr, body_expr, after_expr] = args else {
        return Err(EvalError::WrongArgCount {
            name: "dynamic-wind".into(),
            expected: "exactly 3 arguments".into(),
            got: args.len(),
        }
        .into());
    };

    let before = eval_expr(before_expr, env, macro_env, continuation)?;
    let body = eval_expr(body_expr, env, macro_env, continuation)?;
    let after = eval_expr(after_expr, env, macro_env, continuation)?;
    let frame = make_dynamic_wind_frame(
        before.clone(),
        before_expr.position(),
        after,
        after_expr.position(),
    );
    let enter_continuation = Rc::new(Continuation::DynamicWindEnter {
        frame: Rc::clone(&frame),
        body: body.clone(),
        body_position: body_expr.position(),
        next: Rc::clone(continuation),
    });

    apply_callable(before, &[], &enter_continuation)?;

    push_wind_frame(&frame);
    let body_result = apply_callable(
        body,
        &[],
        &Rc::new(Continuation::DynamicWind {
            frame: Rc::clone(&frame),
            next: Rc::clone(continuation),
        }),
    );

    let body_value = match body_result {
        Ok(body_value) => {
            pop_wind_frame(&frame);
            apply_callable(
                frame.after.clone(),
                &[],
                &Rc::new(Continuation::DynamicWindExit {
                    return_value: body_value.clone(),
                    next: Rc::clone(continuation),
                }),
            )?;
            body_value
        }
        Err(EvalSignal::Raise(exception)) => {
            pop_wind_frame(&frame);
            apply_callable(frame.after.clone(), &[], continuation)?;
            return Err(EvalSignal::Raise(exception));
        }
        Err(signal) => return Err(signal),
    };

    Ok(body_value)
}

pub(super) fn eval_guard(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let [Expr::List(spec, _), body @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: "invalid guard".into(),
        }
        .into());
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "guard requires a body".into(),
        }
        .into());
    }

    let Some((Expr::Symbol(name, _), clauses)) = spec.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "guard requires an exception variable".into(),
        }
        .into());
    };

    match eval_sequence(body, env, macro_env, continuation) {
        Ok(value) => Ok(value),
        Err(EvalSignal::Raise(exception)) => {
            eval_guard_clauses(name, clauses, exception, env, macro_env, continuation)
        }
        Err(signal) => Err(signal),
    }
}

fn eval_guard_clauses(
    exception_name: &str,
    clauses: &[Expr],
    exception: RaisedException,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let guard_env = Environment::new(Some(Rc::clone(env)));
    env_define(&guard_env, exception_name.into(), exception.value.clone());
    let guard_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "guard clauses must be lists".into(),
            }
            .into());
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "guard clause cannot be empty".into(),
            }
            .into());
        };

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                }
                .into());
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, &guard_env, &guard_macro_env, continuation)
            };
        }

        let test_value = eval_expr(test, &guard_env, &guard_macro_env, continuation)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, &guard_env, &guard_macro_env, continuation)
            };
        }
    }

    Err(EvalSignal::Raise(exception))
}

pub(super) fn eval_cond<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "cond clauses must be lists".into(),
            }
            .into());
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "cond clause cannot be empty".into(),
            }
            .into());
        };

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                }
                .into());
            }

            return if body.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                Ok(tail_borrowed_sequence(body, env, macro_env))
            };
        }

        let test_value = eval_expr(test, env, macro_env, continuation)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(EvalStep::Value(test_value))
            } else {
                Ok(tail_borrowed_sequence(body, env, macro_env))
            };
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

pub(super) fn eval_let<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    match args {
        [Expr::List(bindings, _), body @ ..] => {
            eval_plain_let(bindings, body, env, macro_env, continuation)
        }
        [Expr::Symbol(name, _), Expr::List(bindings, _), body @ ..] => {
            eval_named_let(name, bindings, body, env, macro_env, continuation)
        }
        _ => Err(EvalError::SyntaxError {
            message: "invalid let".into(),
        }
        .into()),
    }
}

pub(super) fn eval_let_star<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let [Expr::List(bindings, _), body @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: "invalid let*".into(),
        }
        .into());
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let* requires a body".into(),
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, "let*")?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    let let_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for (name, value_expr) in bindings {
        let value = eval_expr(value_expr, &let_env, &let_macro_env, continuation)?;
        env_define(&let_env, name, value);
    }

    Ok(tail_borrowed_sequence(body, &let_env, &let_macro_env))
}

pub(super) fn eval_letrec<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    sequential: bool,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let [Expr::List(bindings, _), body @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: if sequential {
                "invalid letrec*".into()
            } else {
                "invalid letrec".into()
            },
        }
        .into());
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: if sequential {
                "letrec* requires a body".into()
            } else {
                "letrec requires a body".into()
            },
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, if sequential { "letrec*" } else { "letrec" })?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    let let_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for (name, _) in &bindings {
        env_define(&let_env, name.clone(), Value::Uninitialized);
    }

    if sequential {
        for (name, value_expr) in &bindings {
            let value = eval_expr(value_expr, &let_env, &let_macro_env, continuation)?;
            env_set(&let_env, name, value);
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for (_, value_expr) in &bindings {
            values.push(eval_expr(
                value_expr,
                &let_env,
                &let_macro_env,
                continuation,
            )?);
        }

        for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
            env_set(&let_env, name, value);
        }
    }

    Ok(tail_borrowed_sequence(body, &let_env, &let_macro_env))
}

pub(super) fn eval_case<'a>(
    args: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        }
        .into());
    };

    let key = eval_expr(key_expr, env, macro_env, continuation)?;

    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must be lists".into(),
            }
            .into());
        };

        let Some((datums, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "case clause cannot be empty".into(),
            }
            .into());
        };

        if matches!(datums, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                }
                .into());
            }

            return if body.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                Ok(tail_borrowed_sequence(body, env, macro_env))
            };
        }

        let Expr::List(datums, _) = datums else {
            return Err(EvalError::SyntaxError {
                message: "case clause datums must be in a list".into(),
            }
            .into());
        };

        for datum in datums {
            let datum = quote_to_value(datum)?;
            if values_eqv(&key, &datum) {
                return if body.is_empty() {
                    Ok(EvalStep::Value(Value::Void))
                } else {
                    Ok(tail_borrowed_sequence(body, env, macro_env))
                };
            }
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

pub(super) fn eval_do(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let [Expr::List(bindings, _), Expr::List(test_clause, _), body @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: "invalid do".into(),
        }
        .into());
    };

    let Some((test_expr, result_exprs)) = test_clause.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "do test clause cannot be empty".into(),
        }
        .into());
    };

    let bindings = parse_do_bindings(bindings)?;
    let do_env = Environment::new(Some(Rc::clone(env)));
    let do_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for binding in &bindings {
        let value = eval_expr(binding.init_expr, env, macro_env, continuation)?;
        env_define(&do_env, binding.name.clone(), value);
    }

    loop {
        if eval_expr(test_expr, &do_env, &do_macro_env, continuation)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(result_exprs, &do_env, &do_macro_env, continuation)
            };
        }

        if !body.is_empty() {
            eval_sequence(body, &do_env, &do_macro_env, continuation)?;
        }

        let mut updates = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let value = if let Some(step_expr) = binding.step_expr {
                eval_expr(step_expr, &do_env, &do_macro_env, continuation)?
            } else {
                env_lookup(&do_env, &binding.name).ok_or_else(|| EvalError::UnboundVariable {
                    name: binding.name.clone(),
                })?
            };
            updates.push((binding.name.clone(), value));
        }

        for (name, value) in updates {
            env_set(&do_env, &name, value);
        }
    }
}

fn eval_plain_let<'a>(
    bindings: &[Expr],
    body: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, "let")?;
    let params = bindings
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>();
    let arg_exprs = bindings
        .iter()
        .map(|(_, expr)| (*expr).clone())
        .collect::<Vec<_>>();
    let procedure = single_clause_procedure(params, None, body.to_vec(), env, macro_env);

    eval_callable_with_expr_args(
        procedure,
        &arg_exprs,
        env,
        macro_env,
        bindings
            .first()
            .map(|(_, expr)| expr.position())
            .unwrap_or_else(|| body[0].position()),
        continuation,
    )
}

fn eval_named_let<'a>(
    name: &str,
    bindings: &[Expr],
    body: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        }
        .into());
    }

    let bindings = parse_binding_exprs(bindings, "let")?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let arg_exprs = bindings
        .iter()
        .map(|(_, expr)| (*expr).clone())
        .collect::<Vec<_>>();

    let let_env = Environment::new(Some(Rc::clone(env)));
    let procedure = single_clause_procedure(params, None, body.to_vec(), &let_env, macro_env);
    env_define(&let_env, name.to_string(), procedure.clone());

    eval_callable_with_expr_args(
        procedure,
        &arg_exprs,
        env,
        macro_env,
        bindings
            .first()
            .map(|(_, expr)| expr.position())
            .unwrap_or_else(|| body[0].position()),
        continuation,
    )
}

fn eval_callable_with_expr_args<'a>(
    callable: Value,
    arg_exprs: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    head_position: SourcePos,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let mut args = Vec::with_capacity(arg_exprs.len());

    for index in (0..arg_exprs.len()).rev() {
        let arg = run_expr_in_cont(
            &arg_exprs[index],
            env,
            macro_env,
            Rc::new(Continuation::Application {
                callable: callable.clone(),
                pending_args: arg_exprs[..index].to_vec(),
                evaluated_suffix: args.clone(),
                env: Rc::clone(env),
                macro_env: Rc::clone(macro_env),
                head_position,
                next: Rc::clone(continuation),
            }),
        )?;
        args.insert(0, arg);
    }

    apply_callable_result(callable, &args, continuation)
        .map_err(|signal| signal.with_position(head_position))
}

fn parse_binding_exprs<'a>(
    bindings: &'a [Expr],
    form_name: &str,
) -> Result<Vec<(String, &'a Expr)>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: format!("{form_name} bindings must be lists"),
            });
        };

        let [Expr::Symbol(name, _), value_expr] = items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: format!("{form_name} bindings must be (name value) pairs"),
            });
        };

        parsed.push((name.clone(), value_expr));
    }

    Ok(parsed)
}

struct DoBinding<'a> {
    name: String,
    init_expr: &'a Expr,
    step_expr: Option<&'a Expr>,
}

fn parse_do_bindings(bindings: &[Expr]) -> Result<Vec<DoBinding<'_>>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: "do bindings must be lists".into(),
            });
        };

        match items.as_slice() {
            [Expr::Symbol(name, _), init_expr] => parsed.push(DoBinding {
                name: name.clone(),
                init_expr,
                step_expr: None,
            }),
            [Expr::Symbol(name, _), init_expr, step_expr] => parsed.push(DoBinding {
                name: name.clone(),
                init_expr,
                step_expr: Some(step_expr),
            }),
            _ => {
                return Err(EvalError::SyntaxError {
                    message: "do bindings must be (name init [step])".into(),
                });
            }
        }
    }

    Ok(parsed)
}
