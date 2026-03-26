use std::rc::Rc;

use super::builtins::value_eq;
use super::syntax::parse_syntax_rules;
use super::{
    apply_procedure, eval, eval_sequence, eval_sequence_tco, eval_tail, expr_plain_symbol_name,
    expr_symbol_name, list_from_values, make_string, CallRequest, Closure, ClosureClause, Env,
    EnvRef, EvalContext, EvalError, Expr, ExprKind, RecordProcedure, RecordProcedureKind,
    RecordType, TailOutcome, Value,
};

struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

type Bindings = Vec<(String, Expr)>;
type LetrecSetup<'a> = (Bindings, EnvRef, &'a [Expr]);
type DoSetup<'a> = (Vec<DoBinding>, &'a Expr, &'a [Expr], EnvRef);

#[derive(Clone, Copy)]
enum LetrecMode {
    Parallel,
    Sequential,
}

impl LetrecMode {
    fn form_name(self) -> &'static str {
        match self {
            Self::Parallel => "letrec",
            Self::Sequential => "letrec*",
        }
    }
}

pub(super) fn eval_special_form(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Option<Result<Value, EvalError>> {
    match name {
        "define" => Some(eval_define(args, env, ctx)),
        "define-record-type" => Some(eval_define_record_type(args, env)),
        "define-syntax" => Some(eval_define_syntax(args, env)),
        "set!" => Some(eval_set(args, env, ctx)),
        "if" => Some(eval_if(args, env, ctx)),
        "quote" => Some(eval_quote(args)),
        "lambda" => Some(eval_lambda(args, env)),
        "case-lambda" => Some(eval_case_lambda(args, env)),
        "and" => Some(eval_and(args, env, ctx)),
        "or" => Some(eval_or(args, env, ctx)),
        "begin" => Some(eval_begin(args, env, ctx)),
        "guard" => Some(eval_guard(args, env, ctx)),
        "cond" => Some(eval_cond(args, env, ctx)),
        "let" => Some(eval_let(args, env, ctx)),
        "let*" => Some(eval_let_star(args, env, ctx)),
        "letrec" => Some(eval_letrec(args, env, ctx)),
        "letrec*" => Some(eval_letrec_star(args, env, ctx)),
        "case" => Some(eval_case(args, env, ctx)),
        "do" => Some(eval_do(args, env, ctx)),
        _ => None,
    }
}

pub(super) fn eval_special_form_tail(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Option<Result<TailOutcome, EvalError>> {
    match name {
        "define" => Some(value_outcome(eval_define(args, env, ctx))),
        "define-record-type" => Some(value_outcome(eval_define_record_type(args, env))),
        "define-syntax" => Some(value_outcome(eval_define_syntax(args, env))),
        "set!" => Some(value_outcome(eval_set(args, env, ctx))),
        "if" => Some(eval_if_tail(args, env, ctx)),
        "quote" => Some(value_outcome(eval_quote(args))),
        "lambda" => Some(value_outcome(eval_lambda(args, env))),
        "case-lambda" => Some(value_outcome(eval_case_lambda(args, env))),
        "and" => Some(eval_and_tail(args, env, ctx)),
        "or" => Some(eval_or_tail(args, env, ctx)),
        "begin" => Some(eval_begin_tail(args, env, ctx)),
        "guard" => Some(eval_guard_tail(args, env, ctx)),
        "cond" => Some(eval_cond_tail(args, env, ctx)),
        "let" => Some(eval_let_tail(args, env, ctx)),
        "let*" => Some(eval_let_star_tail(args, env, ctx)),
        "letrec" => Some(eval_letrec_tail(args, env, ctx)),
        "letrec*" => Some(eval_letrec_star_tail(args, env, ctx)),
        "case" => Some(eval_case_tail(args, env, ctx)),
        "do" => Some(eval_do_tail(args, env, ctx)),
        _ => None,
    }
}

fn value_outcome(result: Result<Value, EvalError>) -> Result<TailOutcome, EvalError> {
    result.map(TailOutcome::Value)
}

fn eval_define(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(target) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define requires a target".to_string(),
        });
    };

    match &target.kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2",
                    got: args.len(),
                });
            }

            let value = eval(&args[1], env.clone(), ctx)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let (name_expr, params_exprs) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: "define requires a binding name".to_string(),
                    })?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "function name must be a symbol".to_string(),
                });
            };

            if args.len() < 2 {
                return Err(EvalError::InvalidSyntax {
                    message: "function definition requires a body".to_string(),
                });
            }

            let (params, rest_param) = parse_param_slice(params_exprs)?;
            let closure = Value::Closure(Rc::new(Closure::new_single(
                params,
                rest_param,
                args[1..].to_vec(),
                env.clone(),
            )));
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::CapturedSymbol(_, _) => Err(EvalError::InvalidSyntax {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn eval_define_syntax(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "define-syntax",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let Some(name) = expr_plain_symbol_name(&args[0]) else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax requires a symbol name".to_string(),
        });
    };

    let rules = parse_syntax_rules(name, &args[1], env.clone())?;
    env.define_syntax(name.to_string(), rules);
    Ok(Value::Void)
}

fn eval_define_record_type(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type requires a type, constructor, and predicate".to_string(),
        });
    }

    let Some(type_name) = expr_plain_symbol_name(&args[0]) else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type requires a symbolic type name".to_string(),
        });
    };

    let (constructor_name, constructor_arity) = parse_record_constructor_spec(&args[1])?;
    let Some(predicate_name) = expr_plain_symbol_name(&args[2]) else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type requires a predicate name".to_string(),
        });
    };

    let accessor_names = args[3..]
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;
    if constructor_arity != accessor_names.len() {
        return Err(EvalError::InvalidSyntax {
            message: format!(
                "define-record-type constructor declares {constructor_arity} fields, but {} accessor specs were provided",
                accessor_names.len()
            ),
        });
    }

    let record_type = Rc::new(RecordType {
        name: type_name.to_string(),
        field_count: accessor_names.len(),
    });

    env.define(
        constructor_name.clone(),
        Value::RecordProc(Rc::new(RecordProcedure {
            name: constructor_name,
            record_type: record_type.clone(),
            kind: RecordProcedureKind::Constructor,
        })),
    );
    env.define(
        predicate_name.to_string(),
        Value::RecordProc(Rc::new(RecordProcedure {
            name: predicate_name.to_string(),
            record_type: record_type.clone(),
            kind: RecordProcedureKind::Predicate,
        })),
    );

    for (index, accessor_name) in accessor_names.into_iter().enumerate() {
        env.define(
            accessor_name.clone(),
            Value::RecordProc(Rc::new(RecordProcedure {
                name: accessor_name,
                record_type: record_type.clone(),
                kind: RecordProcedureKind::Accessor(index),
            })),
        );
    }

    Ok(Value::Void)
}

fn eval_set(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let (name, target_env) = match &args[0].kind {
        ExprKind::Symbol(name) => (name.clone(), env.clone()),
        ExprKind::CapturedSymbol(name, captured_env) => (name.clone(), captured_env.clone()),
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::List(_) => {
            return Err(EvalError::InvalidSyntax {
                message: "set! requires a symbol target".to_string(),
            });
        }
    };

    let value = eval(&args[1], env.clone(), ctx)?;
    if target_env.set(&name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundVariable { name }.with_position(args[0].pos))
    }
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        });
    }

    if eval(&args[0], env.clone(), ctx)?.is_truthy() {
        eval(&args[1], env, ctx)
    } else if let Some(alternate) = args.get(2) {
        eval(alternate, env, ctx)
    } else {
        Ok(Value::Void)
    }
}

fn eval_if_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        });
    }

    if eval(&args[0], env.clone(), ctx)?.is_truthy() {
        eval_tail(&args[1], env, ctx)
    } else if let Some(alternate) = args.get(2) {
        eval_tail(alternate, env, ctx)
    } else {
        Ok(TailOutcome::Value(Value::Void))
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    quote_expr(&args[0])
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let (params, rest_param) = parse_param_list(&args[0])?;
    Ok(Value::Closure(Rc::new(Closure::new_single(
        params,
        rest_param,
        args[1..].to_vec(),
        env,
    ))))
}

fn eval_case_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "case-lambda requires at least one clause".to_string(),
        });
    }

    let clauses = args
        .iter()
        .map(parse_case_lambda_clause)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::Closure(Rc::new(Closure { clauses, env })))
}

fn parse_case_lambda_clause(clause: &Expr) -> Result<ClosureClause, EvalError> {
    let ExprKind::List(items) = &clause.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "case-lambda clauses must be lists".to_string(),
        });
    };

    let Some((params_expr, body)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "case-lambda clauses cannot be empty".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "case-lambda clauses require a body".to_string(),
        });
    }

    let (params, rest_param) = parse_param_list(params_expr)?;
    Ok(ClosureClause {
        params,
        rest_param,
        body: body.to_vec(),
    })
}

fn eval_and(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);
    for expr in args {
        let value = eval(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_and_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Boolean(true)));
    };

    for expr in init {
        let value = eval(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    eval_tail(last, env, ctx)
}

fn eval_or(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_or_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Boolean(false)));
    };

    for expr in init {
        let value = eval(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    eval_tail(last, env, ctx)
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_begin_tail(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    eval_sequence_tco(args, env, ctx)
}

fn eval_guard(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some((spec, body)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "guard requires a variable, clauses, and a body".to_string(),
        });
    };

    require_body("guard", body)?;
    let (name, clauses) = parse_guard_spec(spec)?;

    match eval_sequence(body, env.clone(), ctx) {
        Ok(value) => Ok(value),
        Err(EvalError::Raised { id, value }) => {
            let Some(exception) = ctx.take_exception(id) else {
                return Err(EvalError::InvalidSyntax {
                    message: "internal missing exception payload".to_string(),
                });
            };

            match eval_guard_clauses(&name, &clauses, exception.clone(), env, ctx)? {
                Some(result) => Ok(result),
                None => {
                    ctx.restore_exception(id, exception);
                    Err(EvalError::Raised { id, value })
                }
            }
        }
        Err(err) => Err(err),
    }
}

fn eval_guard_tail(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((spec, body)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "guard requires a variable, clauses, and a body".to_string(),
        });
    };

    require_body("guard", body)?;
    let (name, clauses) = parse_guard_spec(spec)?;

    match eval_sequence_tco(body, env.clone(), ctx) {
        Ok(result) => Ok(result),
        Err(EvalError::Raised { id, value }) => {
            let Some(exception) = ctx.take_exception(id) else {
                return Err(EvalError::InvalidSyntax {
                    message: "internal missing exception payload".to_string(),
                });
            };

            match eval_guard_clauses_tail(&name, &clauses, exception.clone(), env, ctx)? {
                Some(result) => Ok(result),
                None => {
                    ctx.restore_exception(id, exception);
                    Err(EvalError::Raised { id, value })
                }
            }
        }
        Err(err) => Err(err),
    }
}

fn eval_cond(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "cond clauses cannot be empty".to_string(),
            })?;

        if expr_symbol_name(test).is_some_and(|name| name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond else clause must be last".to_string(),
                });
            }
            return eval_sequence(body, env, ctx);
        }

        let test_value = eval(test, env.clone(), ctx)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, ctx)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_cond_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "cond clauses cannot be empty".to_string(),
            })?;

        if expr_symbol_name(test).is_some_and(|name| name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond else clause must be last".to_string(),
                });
            }
            return eval_sequence_tco(body, env, ctx);
        }

        let test_value = eval(test, env.clone(), ctx)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(TailOutcome::Value(test_value))
            } else {
                eval_sequence_tco(body, env, ctx)
            };
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn parse_guard_spec(expr: &Expr) -> Result<(String, Vec<Expr>), EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "guard requires a variable and clause list".to_string(),
        });
    };

    let Some((name_expr, clauses)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "guard requires a variable and at least zero clauses".to_string(),
        });
    };
    let Some(name) = expr_plain_symbol_name(name_expr) else {
        return Err(EvalError::InvalidSyntax {
            message: "guard variable must be a symbol".to_string(),
        });
    };

    Ok((name.to_string(), clauses.to_vec()))
}

fn eval_guard_clauses(
    name: &str,
    clauses: &[Expr],
    exception: Value,
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Option<Value>, EvalError> {
    let frame = Env::new(Some(env));
    frame.define(name.to_string(), exception);

    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "guard clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "guard clauses cannot be empty".to_string(),
            })?;

        if expr_symbol_name(test).is_some_and(|symbol| symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "guard else clause must be last".to_string(),
                });
            }
            return eval_sequence(body, frame, ctx).map(Some);
        }

        let test_value = eval(test, frame.clone(), ctx)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(Some(test_value))
            } else {
                eval_sequence(body, frame, ctx).map(Some)
            };
        }
    }

    Ok(None)
}

fn eval_guard_clauses_tail(
    name: &str,
    clauses: &[Expr],
    exception: Value,
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Option<TailOutcome>, EvalError> {
    let frame = Env::new(Some(env));
    frame.define(name.to_string(), exception);

    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "guard clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "guard clauses cannot be empty".to_string(),
            })?;

        if expr_symbol_name(test).is_some_and(|symbol| symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "guard else clause must be last".to_string(),
                });
            }
            return eval_sequence_tco(body, frame, ctx).map(Some);
        }

        let test_value = eval(test, frame.clone(), ctx)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(Some(TailOutcome::Value(test_value)))
            } else {
                eval_sequence_tco(body, frame, ctx).map(Some)
            };
        }
    }

    Ok(None)
}

fn eval_case(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "case requires a key and at least zero clauses".to_string(),
        });
    };

    let key = eval(key_expr, env.clone(), ctx)?;
    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "case clauses must be lists".to_string(),
            });
        };

        let Some((datums_expr, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "case clauses cannot be empty".to_string(),
            });
        };

        if expr_symbol_name(datums_expr).is_some_and(|name| name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "case else clause must be last".to_string(),
                });
            }
            return eval_sequence(body, env, ctx);
        }

        if case_clause_matches(&key, datums_expr)? {
            return eval_sequence(body, env, ctx);
        }
    }

    Ok(Value::Void)
}

fn eval_case_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "case requires a key and at least zero clauses".to_string(),
        });
    };

    let key = eval(key_expr, env.clone(), ctx)?;
    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "case clauses must be lists".to_string(),
            });
        };

        let Some((datums_expr, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "case clauses cannot be empty".to_string(),
            });
        };

        if expr_symbol_name(datums_expr).is_some_and(|name| name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "case else clause must be last".to_string(),
                });
            }
            return eval_sequence_tco(body, env, ctx);
        }

        if case_clause_matches(&key, datums_expr)? {
            return eval_sequence_tco(body, env, ctx);
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn case_clause_matches(key: &Value, datums_expr: &Expr) -> Result<bool, EvalError> {
    let ExprKind::List(datums) = &datums_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "case clause datums must be a list".to_string(),
        });
    };

    datums.iter().try_fold(false, |matched, datum| {
        if matched {
            Ok(true)
        } else {
            Ok(value_eq(key, &quote_expr(datum)?))
        }
    })
}

fn eval_let(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(first) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let requires bindings".to_string(),
        });
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let(name, &args[1..], env, ctx),
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::CapturedSymbol(_, _)
        | ExprKind::List(_) => eval_plain_let(first, &args[1..], env, ctx),
    }
}

fn eval_let_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    let Some(first) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let requires bindings".to_string(),
        });
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let_tail(name, &args[1..], env, ctx),
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::CapturedSymbol(_, _)
        | ExprKind::List(_) => eval_plain_let_tail(first, &args[1..], env, ctx),
    }
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    require_body("let", body)?;
    let frame = build_let_frame(bindings_expr, env, ctx)?;
    eval_sequence(body, frame, ctx)
}

fn eval_plain_let_tail(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    require_body("let", body)?;
    let frame = build_let_frame(bindings_expr, env, ctx)?;
    eval_sequence_tco(body, frame, ctx)
}

fn build_let_frame(
    bindings_expr: &Expr,
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<EnvRef, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone(), ctx)?;
    let frame = Env::new(Some(env));

    for ((name, _), value) in bindings.into_iter().zip(values.into_iter()) {
        frame.define(name, value);
    }

    Ok(frame)
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let CallRequest {
        procedure, args, ..
    } = build_named_let_call(name, args, env, ctx)?;
    apply_procedure(procedure, &args, ctx)
}

fn eval_named_let_tail(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    build_named_let_call(name, args, env, ctx).map(TailOutcome::Apply)
}

fn build_named_let_call(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<CallRequest, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "named let requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_bindings(&args[0])?;
    let values = eval_binding_values(&bindings, env.clone(), ctx)?;
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect::<Vec<_>>();
    let frame = Env::new(Some(env));
    let closure = Value::Closure(Rc::new(Closure::new_single(
        params,
        None,
        args[1..].to_vec(),
        frame.clone(),
    )));

    frame.define(name.to_string(), closure.clone());
    Ok(CallRequest {
        procedure: closure,
        args: values,
        pos: None,
    })
}

fn eval_let_star(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let* requires bindings".to_string(),
        });
    };

    require_body("let*", body)?;
    let frame = build_let_star_frame(bindings_expr, env, ctx)?;
    eval_sequence(body, frame, ctx)
}

fn eval_let_star_tail(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let* requires bindings".to_string(),
        });
    };

    require_body("let*", body)?;
    let frame = build_let_star_frame(bindings_expr, env, ctx)?;
    eval_sequence_tco(body, frame, ctx)
}

fn build_let_star_frame(
    bindings_expr: &Expr,
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<EnvRef, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let frame = Env::new(Some(env));

    for (name, expr) in bindings {
        let value = eval(&expr, frame.clone(), ctx)?;
        frame.define(name, value);
    }

    Ok(frame)
}

fn eval_letrec(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    eval_letrec_impl(args, env, ctx, LetrecMode::Parallel)
}

fn eval_letrec_star(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    eval_letrec_impl(args, env, ctx, LetrecMode::Sequential)
}

fn eval_letrec_tail(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    eval_letrec_impl_tail(args, env, ctx, LetrecMode::Parallel)
}

fn eval_letrec_star_tail(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    eval_letrec_impl_tail(args, env, ctx, LetrecMode::Sequential)
}

fn eval_letrec_impl(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
    mode: LetrecMode,
) -> Result<Value, EvalError> {
    let (bindings, frame, body) = prepare_letrec(args, env, mode)?;
    populate_letrec_bindings(mode, &bindings, &frame, ctx)?;
    eval_sequence(body, frame, ctx)
}

fn eval_letrec_impl_tail(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
    mode: LetrecMode,
) -> Result<TailOutcome, EvalError> {
    let (bindings, frame, body) = prepare_letrec(args, env, mode)?;
    populate_letrec_bindings(mode, &bindings, &frame, ctx)?;
    eval_sequence_tco(body, frame, ctx)
}

fn prepare_letrec(
    args: &[Expr],
    env: EnvRef,
    mode: LetrecMode,
) -> Result<LetrecSetup<'_>, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: format!("{} requires bindings and a body", mode.form_name()),
        });
    }

    let bindings = parse_bindings(&args[0])?;
    let frame = Env::new(Some(env));
    for (name, _) in &bindings {
        frame.define(name.clone(), Value::Void);
    }

    Ok((bindings, frame, &args[1..]))
}

fn populate_letrec_bindings(
    mode: LetrecMode,
    bindings: &[(String, Expr)],
    frame: &EnvRef,
    ctx: &EvalContext,
) -> Result<(), EvalError> {
    match mode {
        LetrecMode::Sequential => {
            for (name, expr) in bindings {
                let value = eval(expr, frame.clone(), ctx)?;
                set_existing_binding(frame, name, value, "letrec* placeholder should exist");
            }
        }
        LetrecMode::Parallel => {
            let values = bindings
                .iter()
                .map(|(_, expr)| eval(expr, frame.clone(), ctx))
                .collect::<Result<Vec<_>, _>>()?;
            for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
                set_existing_binding(frame, name, value, "letrec placeholder should exist");
            }
        }
    }

    Ok(())
}

fn eval_do(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let (bindings, test_expr, result_exprs, frame) = prepare_do(args, env, ctx)?;

    loop {
        if eval(test_expr, frame.clone(), ctx)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(result_exprs, frame, ctx)
            };
        }

        eval_sequence(&args[2..], frame.clone(), ctx)?;
        apply_do_updates(&bindings, &frame, ctx)?;
    }
}

fn eval_do_tail(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    let (bindings, test_expr, result_exprs, frame) = prepare_do(args, env, ctx)?;

    loop {
        if eval(test_expr, frame.clone(), ctx)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(TailOutcome::Value(Value::Void))
            } else {
                eval_sequence_tco(result_exprs, frame, ctx)
            };
        }

        eval_sequence(&args[2..], frame.clone(), ctx)?;
        apply_do_updates(&bindings, &frame, ctx)?;
    }
}

fn prepare_do<'a>(
    args: &'a [Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<DoSetup<'a>, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "do requires bindings and a termination clause".to_string(),
        });
    }

    let bindings = parse_do_bindings(&args[0])?;
    let (test_expr, result_exprs) = parse_do_termination_clause(&args[1])?;
    let frame = initialize_do_frame(&bindings, env, ctx)?;
    Ok((bindings, test_expr, result_exprs, frame))
}

fn parse_do_termination_clause(expr: &Expr) -> Result<(&Expr, &[Expr]), EvalError> {
    let ExprKind::List(test_clause) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "do termination clause must be a list".to_string(),
        });
    };

    test_clause
        .split_first()
        .ok_or_else(|| EvalError::InvalidSyntax {
            message: "do termination clause cannot be empty".to_string(),
        })
}

fn initialize_do_frame(
    bindings: &[DoBinding],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<EnvRef, EvalError> {
    let init_values = bindings
        .iter()
        .map(|binding| eval(&binding.init, env.clone(), ctx))
        .collect::<Result<Vec<_>, _>>()?;

    let frame = Env::new(Some(env));
    for (binding, value) in bindings.iter().zip(init_values.into_iter()) {
        frame.define(binding.name.clone(), value);
    }

    Ok(frame)
}

fn apply_do_updates(
    bindings: &[DoBinding],
    frame: &EnvRef,
    ctx: &EvalContext,
) -> Result<(), EvalError> {
    let updates = bindings
        .iter()
        .map(|binding| do_update_value(binding, frame, ctx))
        .collect::<Result<Vec<_>, _>>()?;

    for (binding, value) in bindings.iter().zip(updates.into_iter()) {
        set_existing_binding(
            frame,
            &binding.name,
            value,
            "do binding should remain present",
        );
    }

    Ok(())
}

fn do_update_value(
    binding: &DoBinding,
    frame: &EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match &binding.step {
        Some(step) => eval(step, frame.clone(), ctx),
        None => Ok(frame
            .lookup(&binding.name)
            .expect("do binding should remain present")),
    }
}

fn set_existing_binding(frame: &EnvRef, name: &str, value: Value, context: &str) {
    assert!(frame.set(name, value), "{context}: missing {name}");
}

fn require_body(form_name: &str, body: &[Expr]) -> Result<(), EvalError> {
    if body.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: format!("{form_name} requires a body"),
        });
    }

    Ok(())
}

fn parse_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let bindings must be a list".to_string(),
        });
    };

    bindings.iter().map(parse_binding).collect()
}

fn parse_binding(binding: &Expr) -> Result<(String, Expr), EvalError> {
    let ExprKind::List(items) = &binding.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let bindings must be pairs".to_string(),
        });
    };
    if items.len() != 2 {
        return Err(EvalError::InvalidSyntax {
            message: "let bindings must contain exactly 2 items".to_string(),
        });
    }

    let ExprKind::Symbol(name) = &items[0].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let binding names must be symbols".to_string(),
        });
    };
    Ok((name.clone(), items[1].clone()))
}

fn parse_do_bindings(expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "do bindings must be a list".to_string(),
        });
    };

    bindings.iter().map(parse_do_binding).collect()
}

fn parse_do_binding(binding: &Expr) -> Result<DoBinding, EvalError> {
    let ExprKind::List(items) = &binding.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "do bindings must be lists".to_string(),
        });
    };
    if !(2..=3).contains(&items.len()) {
        return Err(EvalError::InvalidSyntax {
            message: "do bindings must contain 2 or 3 items".to_string(),
        });
    }

    let ExprKind::Symbol(name) = &items[0].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "do binding names must be symbols".to_string(),
        });
    };
    Ok(DoBinding {
        name: name.clone(),
        init: items[1].clone(),
        step: items.get(2).cloned(),
    })
}

fn eval_binding_values(
    bindings: &[(String, Expr)],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| eval(expr, env.clone(), ctx))
        .collect()
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<(String, usize), EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "record constructor specification must be a list".to_string(),
        });
    };

    let (name_expr, params) = items
        .split_first()
        .ok_or_else(|| EvalError::InvalidSyntax {
            message: "record constructor specification cannot be empty".to_string(),
        })?;
    let Some(name) = expr_plain_symbol_name(name_expr) else {
        return Err(EvalError::InvalidSyntax {
            message: "record constructor name must be a symbol".to_string(),
        });
    };

    for param in params {
        if expr_plain_symbol_name(param).is_none() {
            return Err(EvalError::InvalidSyntax {
                message: "record constructor parameters must be symbols".to_string(),
            });
        }
    }

    Ok((name.to_string(), params.len()))
}

fn parse_record_field_spec(expr: &Expr) -> Result<String, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "record field specification must be a list".to_string(),
        });
    };
    if items.len() != 2 {
        return Err(EvalError::InvalidSyntax {
            message: "record field specification must contain a field name and accessor"
                .to_string(),
        });
    }

    let Some(_field_name) = expr_plain_symbol_name(&items[0]) else {
        return Err(EvalError::InvalidSyntax {
            message: "record field name must be a symbol".to_string(),
        });
    };
    let Some(accessor_name) = expr_plain_symbol_name(&items[1]) else {
        return Err(EvalError::InvalidSyntax {
            message: "record accessor name must be a symbol".to_string(),
        });
    };

    Ok(accessor_name.to_string())
}

fn parse_param_list(expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match &expr.kind {
        ExprKind::List(items) => parse_param_slice(items),
        ExprKind::Symbol(name) => Ok((Vec::new(), Some(name.clone()))),
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::CapturedSymbol(_, _) => Err(EvalError::InvalidSyntax {
            message: "lambda parameters must be a list or symbol".to_string(),
        }),
    }
}

fn parse_param_slice(items: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::with_capacity(items.len());
    let mut index = 0;
    while let Some(item) = items.get(index) {
        let ExprKind::Symbol(name) = &item.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "parameter names must be symbols".to_string(),
            });
        };

        if name == "." {
            let Some(rest_expr) = items.get(index + 1) else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter dot must be followed by a name".to_string(),
                });
            };
            let ExprKind::Symbol(rest_name) = &rest_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter name must be a symbol".to_string(),
                });
            };
            if index + 2 != items.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter must be the final parameter".to_string(),
                });
            }
            return Ok((params, Some(rest_name.clone())));
        }

        params.push(name.clone());
        index += 1;
    }
    Ok((params, None))
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(value) | ExprKind::CapturedSymbol(value, _) => {
            Ok(Value::Symbol(value.clone()))
        }
        ExprKind::List(items) => items
            .iter()
            .map(quote_expr)
            .collect::<Result<Vec<_>, _>>()
            .map(list_from_values),
    }
}
