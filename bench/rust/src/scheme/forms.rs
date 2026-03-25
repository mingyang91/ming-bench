use super::value_ops::eqv_value;
use super::{
    apply_outcome, collect_list, env_define, env_lookup_cell, env_set, eval_expr,
    eval_expr_outcome, eval_sequence, eval_sequence_outcome, is_proper_list,
    make_immutable_string_value, make_list_value, syntax_error, type_mismatch, wrong_arg_count,
    CaseLambda, Env, EnvRef, EvalContext, EvalError, EvalOutcome, Expr, ExprKind, Lambda,
    LambdaParams, Procedure, SourcePos, Value,
};
use std::rc::Rc;

pub(super) fn eval_begin(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    eval_sequence_outcome(args, env, pos, context, tail)
}

pub(super) fn eval_set(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match args {
        [name_expr, value_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "set! target must be a symbol"))?;
            let value = eval_expr(value_expr, env, context)?;

            if env_set(env, name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable {
                    pos: name_expr.pos,
                    name: name.to_string(),
                })
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "set!",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

pub(super) fn parse_define_signature(
    signature: &Expr,
) -> Result<(String, LambdaParams), EvalError> {
    let items = signature
        .list_items()
        .ok_or_else(|| syntax_error(signature.pos, "invalid define form"))?;

    let (name_expr, params) = items
        .split_first()
        .ok_or_else(|| syntax_error(signature.pos, "invalid define form"))?;

    let name = name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(name_expr.pos, "function name must be a symbol"))?
        .to_string();

    let params = parse_params(params)?;
    Ok((name, params))
}

fn parse_params(params: &[Expr]) -> Result<LambdaParams, EvalError> {
    let mut required = Vec::new();
    let mut rest = None;
    let mut iter = params.iter().peekable();

    while let Some(expr) = iter.next() {
        match expr.symbol_name() {
            Some(".") => {
                let rest_expr = iter
                    .next()
                    .ok_or_else(|| syntax_error(expr.pos, "rest parameter requires a name"))?;
                let rest_name = rest_expr
                    .symbol_name()
                    .filter(|name| *name != ".")
                    .ok_or_else(|| {
                        syntax_error(rest_expr.pos, "parameter name must be a symbol")
                    })?;

                if iter.next().is_some() {
                    return Err(syntax_error(expr.pos, "rest parameter must be last"));
                }

                rest = Some(rest_name.to_string());
                break;
            }
            Some(name) => required.push(name.to_string()),
            None => return Err(syntax_error(expr.pos, "parameter name must be a symbol")),
        }
    }

    Ok(LambdaParams { required, rest })
}

pub(super) fn eval_cond(
    clauses: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let items = clause
            .list_items()
            .ok_or_else(|| syntax_error(clause.pos, "cond clauses must be lists"))?;

        let (test, body) = items
            .split_first()
            .ok_or_else(|| syntax_error(clause.pos, "cond clause cannot be empty"))?;

        if test.symbol_name() == Some("else") {
            if index + 1 != clauses.len() {
                return Err(syntax_error(test.pos, "else clause must be last"));
            }
            return eval_sequence_outcome(body, env, clause.pos, context, tail);
        }

        let test_value = eval_expr(test, env, context)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(EvalOutcome::Value(test_value))
            } else {
                eval_sequence_outcome(body, env, clause.pos, context, tail)
            };
        }
    }

    Ok(EvalOutcome::Value(Value::Void))
}

pub(super) fn eval_case(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let (key_expr, clauses) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "case requires a key and at least one clause"))?;

    if clauses.is_empty() {
        return Err(syntax_error(pos, "case requires at least one clause"));
    }

    let key = eval_expr(key_expr, env, context)?;

    for (index, clause) in clauses.iter().enumerate() {
        let items = clause
            .list_items()
            .ok_or_else(|| syntax_error(clause.pos, "case clauses must be lists"))?;
        let (head, body) = items
            .split_first()
            .ok_or_else(|| syntax_error(clause.pos, "case clause cannot be empty"))?;

        if head.symbol_name() == Some("else") {
            if index + 1 != clauses.len() {
                return Err(syntax_error(head.pos, "else clause must be last"));
            }

            return if body.is_empty() {
                Ok(EvalOutcome::Value(Value::Void))
            } else {
                eval_sequence_outcome(body, env, clause.pos, context, tail)
            };
        }

        let datums = head
            .list_items()
            .ok_or_else(|| syntax_error(head.pos, "case datums must be a list"))?;

        for datum in datums {
            if eqv_value(&key, &quote_expr(datum)?) {
                return if body.is_empty() {
                    Ok(EvalOutcome::Value(Value::Void))
                } else {
                    eval_sequence_outcome(body, env, clause.pos, context, tail)
                };
            }
        }
    }

    Ok(EvalOutcome::Value(Value::Void))
}

pub(super) fn eval_if(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    match args {
        [condition, when_true] => {
            if eval_expr(condition, env, context)?.is_truthy() {
                eval_expr_outcome(when_true, env, context, tail)
            } else {
                Ok(EvalOutcome::Value(Value::Void))
            }
        }
        [condition, when_true, when_false] => {
            if eval_expr(condition, env, context)?.is_truthy() {
                eval_expr_outcome(when_true, env, context, tail)
            } else {
                eval_expr_outcome(when_false, env, context, tail)
            }
        }
        _ => Err(wrong_arg_count(pos, "if", "2 or 3 arguments", args.len())),
    }
}

pub(super) fn eval_do(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let (bindings_expr, rest) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "do requires bindings and a test clause"))?;
    let (test_clause_expr, body) = rest
        .split_first()
        .ok_or_else(|| syntax_error(pos, "do requires a test clause"))?;

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "do bindings must be a list"))?;
    let test_clause = test_clause_expr
        .list_items()
        .ok_or_else(|| syntax_error(test_clause_expr.pos, "do test clause must be a list"))?;
    let (test_expr, exit_exprs) = test_clause
        .split_first()
        .ok_or_else(|| syntax_error(test_clause_expr.pos, "do test clause cannot be empty"))?;

    let bindings = bindings
        .iter()
        .map(parse_do_binding)
        .collect::<Result<Vec<_>, _>>()?;
    let init_values = bindings
        .iter()
        .map(|(_, init, _)| eval_expr(init, env, context))
        .collect::<Result<Vec<_>, _>>()?;

    let do_env = Env::new_child(env);
    let mut cells = Vec::with_capacity(bindings.len());

    for ((name, _, _), value) in bindings.iter().zip(init_values.into_iter()) {
        env_define(&do_env, name.clone(), value);
        let cell = env_lookup_cell(&do_env, name).expect("binding defined in current scope");
        cells.push(cell);
    }

    loop {
        if eval_expr(test_expr, &do_env, context)?.is_truthy() {
            return if exit_exprs.is_empty() {
                Ok(EvalOutcome::Value(Value::Void))
            } else {
                eval_sequence_outcome(exit_exprs, &do_env, test_clause_expr.pos, context, tail)
            };
        }

        if !body.is_empty() {
            let _ = eval_sequence(body, &do_env, pos, context)?;
        }

        let next_values = bindings
            .iter()
            .zip(cells.iter())
            .map(|((_, _, step), cell)| match step {
                Some(step) => eval_expr(step, &do_env, context),
                None => Ok(cell.borrow().clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (cell, value) in cells.iter().zip(next_values.into_iter()) {
            *cell.borrow_mut() = value;
        }
    }
}

pub(super) fn eval_let(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let (head, rest) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "let requires bindings and a body"))?;

    if let Some(bindings) = head.list_items() {
        eval_plain_let(bindings, rest, env, pos, context, tail)
    } else if let Some(name) = head.symbol_name() {
        eval_named_let(name, rest, env, head.pos, context, tail)
    } else {
        Err(syntax_error(head.pos, "invalid let form"))
    }
}

fn eval_plain_let(
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "let requires a body"));
    }

    let bindings = eval_bindings(bindings, env, context)?;
    let let_env = Env::new_child(env);

    for (name, value) in bindings {
        env_define(&let_env, name, value);
    }

    eval_sequence_outcome(body, &let_env, pos, context, tail)
}

pub(super) fn eval_let_star(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "let* requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "let* requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "let* bindings must be a list"))?;
    let let_env = Env::new_child(env);

    for binding in bindings {
        let (name, value_expr) = parse_value_binding(binding, "let*")?;
        let value = eval_expr(&value_expr, &let_env, context)?;
        env_define(&let_env, name, value);
    }

    eval_sequence_outcome(body, &let_env, pos, context, tail)
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "named let requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "named let requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "let bindings must be a list"))?;
    let bindings = eval_bindings(bindings, env, context)?;

    let (params, values): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let let_env = Env::new_child(env);
    let lambda = make_lambda(
        Some(name.to_string()),
        LambdaParams::fixed(params),
        body,
        &let_env,
        pos,
    )?;
    env_define(&let_env, name.to_string(), lambda.clone());
    apply_outcome(lambda, values, pos, context, tail)
}

pub(super) fn eval_letrec(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    sequential: bool,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "letrec requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "letrec requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "letrec bindings must be a list"))?;
    let bindings = bindings
        .iter()
        .map(|binding| parse_value_binding(binding, "letrec"))
        .collect::<Result<Vec<_>, _>>()?;

    let letrec_env = Env::new_child(env);
    for (name, _) in &bindings {
        env_define(&letrec_env, name.clone(), Value::Uninitialized);
    }

    if sequential {
        for (name, value_expr) in &bindings {
            let value = eval_expr(value_expr, &letrec_env, context)?;
            env_set(&letrec_env, name, value);
        }
    } else {
        let values = bindings
            .iter()
            .map(|(_, value_expr)| eval_expr(value_expr, &letrec_env, context))
            .collect::<Result<Vec<_>, _>>()?;

        for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
            env_set(&letrec_env, name, value);
        }
    }

    eval_sequence_outcome(body, &letrec_env, pos, context, tail)
}

fn eval_bindings(
    bindings: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Vec<(String, Value)>, EvalError> {
    bindings
        .iter()
        .map(|binding| {
            let items = binding
                .list_items()
                .ok_or_else(|| syntax_error(binding.pos, "let bindings must be lists"))?;

            match items {
                [name_expr, value_expr] => {
                    let name = name_expr
                        .symbol_name()
                        .ok_or_else(|| {
                            syntax_error(
                                binding.pos,
                                "each let binding must contain a name and value",
                            )
                        })?
                        .to_string();
                    let value = eval_expr(value_expr, env, context)?;
                    Ok((name, value))
                }
                _ => Err(syntax_error(
                    binding.pos,
                    "each let binding must contain a name and value",
                )),
            }
        })
        .collect()
}

pub(super) fn parse_value_binding(
    binding: &Expr,
    form_name: &str,
) -> Result<(String, Expr), EvalError> {
    let items = binding
        .list_items()
        .ok_or_else(|| syntax_error(binding.pos, format!("{form_name} bindings must be lists")))?;

    match items {
        [name_expr, value_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "binding name must be a symbol"))?
                .to_string();
            Ok((name, value_expr.clone()))
        }
        _ => Err(syntax_error(
            binding.pos,
            format!("each {form_name} binding must contain a name and value"),
        )),
    }
}

pub(super) fn parse_do_binding(binding: &Expr) -> Result<(String, Expr, Option<Expr>), EvalError> {
    let items = binding
        .list_items()
        .ok_or_else(|| syntax_error(binding.pos, "do bindings must be lists"))?;

    match items {
        [name_expr, init_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "do binding name must be a symbol"))?
                .to_string();
            Ok((name, init_expr.clone(), None))
        }
        [name_expr, init_expr, step_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "do binding name must be a symbol"))?
                .to_string();
            Ok((name, init_expr.clone(), Some(step_expr.clone())))
        }
        _ => Err(syntax_error(
            binding.pos,
            "each do binding must contain a name, init, and optional step",
        )),
    }
}

pub(super) fn eval_quote(args: &[Expr], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [expr] => quote_expr(expr),
        _ => Err(wrong_arg_count(
            pos,
            "quote",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

pub(super) fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Character(value) => Ok(Value::Character(*value)),
        ExprKind::String(value) => Ok(make_immutable_string_value(value.clone())),
        ExprKind::Symbol(value) => Ok(Value::Symbol(value.clone())),
        ExprKind::List(items) => items
            .iter()
            .map(quote_expr)
            .collect::<Result<Vec<_>, _>>()
            .map(make_list_value),
    }
}

pub(super) fn value_to_expr(value: &Value, pos: SourcePos) -> Result<Expr, EvalError> {
    match value {
        Value::Number(number) => Ok(Expr::new(pos, ExprKind::Number(*number))),
        Value::Boolean(boolean) => Ok(Expr::new(pos, ExprKind::Boolean(*boolean))),
        Value::Character(character) => Ok(Expr::new(pos, ExprKind::Character(*character))),
        Value::String(string) => Ok(Expr::new(pos, ExprKind::String(string.borrow().clone()))),
        Value::Symbol(symbol) => Ok(Expr::new(pos, ExprKind::Symbol(symbol.clone()))),
        Value::Syntax(syntax) => Ok(syntax.expr.clone()),
        value if is_proper_list(value) => {
            let items = collect_list(value)
                .expect("proper lists must produce their collected elements")
                .into_iter()
                .map(|item| value_to_expr(&item, pos))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::new(pos, ExprKind::List(items)))
        }
        other => Err(type_mismatch(
            pos,
            "datum->syntax",
            "datum",
            other.type_name(),
        )),
    }
}

pub(super) fn eval_lambda(
    name: Option<String>,
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let (params_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "lambda requires parameters and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    let params = params_expr
        .list_items()
        .ok_or_else(|| syntax_error(params_expr.pos, "lambda parameters must be a list"))?;
    let params = parse_params(params)?;

    make_lambda(name, params, body, env, pos)
}

pub(super) fn eval_case_lambda(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(syntax_error(
            pos,
            "case-lambda requires at least one clause",
        ));
    }

    let clauses = args
        .iter()
        .map(|clause| parse_case_lambda_clause(clause, env))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Procedure(Procedure::CaseLambda(Rc::new(
        CaseLambda { clauses },
    ))))
}

fn parse_case_lambda_clause(clause: &Expr, env: &EnvRef) -> Result<Rc<Lambda>, EvalError> {
    let items = clause
        .list_items()
        .ok_or_else(|| syntax_error(clause.pos, "case-lambda clauses must be lists"))?;
    let (params_expr, body) = items
        .split_first()
        .ok_or_else(|| syntax_error(clause.pos, "case-lambda clause cannot be empty"))?;

    if body.is_empty() {
        return Err(syntax_error(
            clause.pos,
            "case-lambda clause requires a body",
        ));
    }

    let params = params_expr.list_items().ok_or_else(|| {
        syntax_error(
            params_expr.pos,
            "case-lambda clause parameters must be a list",
        )
    })?;
    let params = parse_params(params)?;

    Ok(Rc::new(Lambda {
        name: None,
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    }))
}

pub(super) fn make_lambda(
    name: Option<String>,
    params: LambdaParams,
    body: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    Ok(Value::Procedure(Procedure::Lambda(Rc::new(Lambda {
        name,
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    }))))
}

pub(super) fn eval_and(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(EvalOutcome::Value(Value::Boolean(true)));
    };

    for expr in prefix {
        let value = eval_expr(expr, env, context)?;
        if !value.is_truthy() {
            return Ok(EvalOutcome::Value(value));
        }
    }

    eval_expr_outcome(last, env, context, tail)
}

pub(super) fn eval_or(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
    tail: bool,
) -> Result<EvalOutcome, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(EvalOutcome::Value(Value::Boolean(false)));
    };

    for expr in prefix {
        let value = eval_expr(expr, env, context)?;
        if value.is_truthy() {
            return Ok(EvalOutcome::Value(value));
        }
    }

    eval_expr_outcome(last, env, context, tail)
}
