use std::rc::Rc;

use super::{
    attach_call_position, expand_macro_call, expr_plain_symbol_name, expr_symbol_name,
    list_from_values, lookup_symbol_value, lookup_syntax, make_string, Continuation,
    ContinuationFrame, ControlProc, DynamicWindContext, DynamicWindRef, Env, EnvRef, EvalContext,
    EvalError, Expr, ExprKind, SourcePos, Value,
};

enum MachineState {
    Eval(Expr, EnvRef, Vec<ContinuationFrame>),
    Return(Value, Vec<ContinuationFrame>),
    Raise(usize, String, Vec<ContinuationFrame>),
}

pub(super) fn program_uses_first_class_continuations(exprs: &[Expr]) -> bool {
    exprs.iter().any(expr_uses_first_class_continuations)
}

fn expr_uses_first_class_continuations(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            matches!(name.as_str(), "call/cc" | "call-with-current-continuation")
        }
        ExprKind::List(items) => items.iter().any(expr_uses_first_class_continuations),
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::String(_) => {
            false
        }
    }
}

pub(super) fn eval_program_with_continuations(
    exprs: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let mut state = start_sequence(exprs.to_vec(), env, Vec::new())?;

    loop {
        state = match state {
            MachineState::Eval(expr, env, frames) => eval_expr(expr, env, frames, ctx)?,
            MachineState::Return(value, mut frames) => match frames.pop() {
                Some(frame) => continue_with_frame(frame, value, frames, ctx)?,
                None => return Ok(value),
            },
            MachineState::Raise(id, value, frames) => handle_raise(id, value, frames, ctx)?,
        };
    }
}

fn start_sequence(
    exprs: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = exprs.split_first() else {
        return Ok(MachineState::Return(Value::Void, frames));
    };

    if !rest.is_empty() {
        frames.push(ContinuationFrame::Sequence {
            rest: rest.to_vec(),
            env: env.clone(),
        });
    }

    Ok(MachineState::Eval(first.clone(), env, frames))
}

fn eval_expr(
    expr: Expr,
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    match expr.kind {
        ExprKind::Number(value) => Ok(MachineState::Return(Value::Number(value), frames)),
        ExprKind::Boolean(value) => Ok(MachineState::Return(Value::Boolean(value), frames)),
        ExprKind::Char(value) => Ok(MachineState::Return(Value::Char(value), frames)),
        ExprKind::String(value) => Ok(MachineState::Return(make_string(value), frames)),
        ExprKind::Symbol(name) => lookup_symbol_value(&name, &env, expr.pos)
            .map(|value| MachineState::Return(value, frames)),
        ExprKind::CapturedSymbol(name, captured_env) => {
            lookup_symbol_value(&name, &captured_env, expr.pos)
                .map(|value| MachineState::Return(value, frames))
        }
        ExprKind::List(items) => eval_list(expr.pos, items, env, frames, ctx),
    }
}

fn eval_list(
    pos: SourcePos,
    items: Vec<Expr>,
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::NotAProcedure {
            found: "()".to_string(),
        }
        .with_position(pos));
    };

    if let Some(name) = expr_symbol_name(head) {
        if matches!(
            name,
            "define" | "set!" | "if" | "quote" | "lambda" | "begin" | "cond" | "let" | "and" | "or"
        ) {
            return eval_special_form_cont(name, tail, env.clone(), frames)
                .map_err(|err| err.with_position(head.pos));
        }
    }

    if let Some(syntax) = lookup_syntax(head, &env) {
        let expanded = expand_macro_call(pos, &items, syntax, ctx)?;
        return Ok(MachineState::Eval(expanded, env, frames));
    }

    let mut next_frames = frames;
    next_frames.push(ContinuationFrame::ApplicationOperator {
        args: tail.to_vec(),
        env: env.clone(),
        pos: head.pos,
    });
    Ok(MachineState::Eval(head.clone(), env, next_frames))
}

fn eval_special_form_cont(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    match name {
        "define" => eval_define(args, env, frames),
        "set!" => eval_set(args, env, frames),
        "if" => eval_if(args, env, frames),
        "quote" => eval_quote(args, frames),
        "lambda" => eval_lambda(args, env, frames),
        "begin" => start_sequence(args.to_vec(), env, frames),
        "cond" => start_cond(args.to_vec(), env, frames),
        "let" => eval_let(args, env, frames),
        "and" => eval_and(args, env, frames),
        "or" => eval_or(args, env, frames),
        _ => unreachable!("checked before dispatch"),
    }
}

fn eval_define(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
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

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::DefineValue {
                name: name.clone(),
                env: env.clone(),
            });
            Ok(MachineState::Eval(args[1].clone(), env, next_frames))
        }
        ExprKind::List(signature) => {
            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(EvalError::InvalidSyntax {
                    message: "define requires a binding name".to_string(),
                });
            };

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
            let closure = Value::Closure(Rc::new(super::Closure::new_single(
                params,
                rest_param,
                args[1..].to_vec(),
                env.clone(),
            )));
            env.define(name.clone(), closure);
            Ok(MachineState::Return(Value::Void, frames))
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

fn eval_set(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
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

    let mut next_frames = frames;
    next_frames.push(ContinuationFrame::SetValue {
        name,
        target_env,
        pos: args[0].pos,
    });
    Ok(MachineState::Eval(args[1].clone(), env, next_frames))
}

fn eval_if(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        });
    }

    let mut next_frames = frames;
    next_frames.push(ContinuationFrame::If {
        consequent: args[1].clone(),
        alternate: args.get(2).cloned(),
        env: env.clone(),
    });
    Ok(MachineState::Eval(args[0].clone(), env, next_frames))
}

fn eval_quote(args: &[Expr], frames: Vec<ContinuationFrame>) -> Result<MachineState, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    quote_expr(&args[0]).map(|value| MachineState::Return(value, frames))
}

fn eval_lambda(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let (params, rest_param) = parse_param_list(&args[0])?;
    Ok(MachineState::Return(
        Value::Closure(Rc::new(super::Closure::new_single(
            params,
            rest_param,
            args[1..].to_vec(),
            env,
        ))),
        frames,
    ))
}

fn eval_let(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    let Some(first) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let requires bindings".to_string(),
        });
    };

    match &first.kind {
        ExprKind::Symbol(_) => Err(EvalError::InvalidSyntax {
            message: "named let is not supported with continuations yet".to_string(),
        }),
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::CapturedSymbol(_, _)
        | ExprKind::List(_) => start_let(parse_bindings(first)?, args[1..].to_vec(), env, frames),
    }
}

fn eval_and(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Ok(MachineState::Return(Value::Boolean(true), frames));
    };

    let mut next_frames = frames;
    if !rest.is_empty() {
        next_frames.push(ContinuationFrame::And {
            rest: rest.to_vec(),
            env: env.clone(),
        });
    }
    Ok(MachineState::Eval(first.clone(), env, next_frames))
}

fn eval_or(
    args: &[Expr],
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Ok(MachineState::Return(Value::Boolean(false), frames));
    };

    let mut next_frames = frames;
    if !rest.is_empty() {
        next_frames.push(ContinuationFrame::Or {
            rest: rest.to_vec(),
            env: env.clone(),
        });
    }
    Ok(MachineState::Eval(first.clone(), env, next_frames))
}

fn continue_with_frame(
    frame: ContinuationFrame,
    value: Value,
    frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    match frame {
        ContinuationFrame::Sequence { rest, env } => start_sequence(rest, env, frames),
        ContinuationFrame::If {
            consequent,
            alternate,
            env,
        } => {
            if value.is_truthy() {
                Ok(MachineState::Eval(consequent, env, frames))
            } else if let Some(alternate) = alternate {
                Ok(MachineState::Eval(alternate, env, frames))
            } else {
                Ok(MachineState::Return(Value::Void, frames))
            }
        }
        ContinuationFrame::DefineValue { name, env } => {
            env.define(name, value);
            Ok(MachineState::Return(Value::Void, frames))
        }
        ContinuationFrame::SetValue {
            name,
            target_env,
            pos,
        } => {
            if target_env.set(&name, value) {
                Ok(MachineState::Return(Value::Void, frames))
            } else {
                Err(EvalError::UnboundVariable { name }.with_position(pos))
            }
        }
        ContinuationFrame::ApplicationOperator { args, env, pos } => {
            let Some((last, rest)) = args.split_last() else {
                return apply_value(value, Vec::new(), Some(pos), frames, ctx);
            };

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::ApplicationArgument {
                procedure: value,
                evaluated: Vec::new(),
                remaining: rest.to_vec(),
                env: env.clone(),
                pos,
            });
            Ok(MachineState::Eval(last.clone(), env, next_frames))
        }
        ContinuationFrame::ApplicationArgument {
            procedure,
            mut evaluated,
            remaining,
            env,
            pos,
        } => {
            evaluated.push(value);
            let Some((next, rest)) = remaining.split_last() else {
                evaluated.reverse();
                return apply_value(procedure, evaluated, Some(pos), frames, ctx);
            };

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::ApplicationArgument {
                procedure,
                evaluated,
                remaining: rest.to_vec(),
                env: env.clone(),
                pos,
            });
            Ok(MachineState::Eval(next.clone(), env, next_frames))
        }
        ContinuationFrame::CondTest {
            body,
            remaining,
            env,
        } => {
            if value.is_truthy() {
                if body.is_empty() {
                    Ok(MachineState::Return(value, frames))
                } else {
                    start_sequence(body, env, frames)
                }
            } else {
                start_cond(remaining, env, frames)
            }
        }
        ContinuationFrame::LetBinding {
            current_name,
            mut evaluated,
            pending,
            body,
            env,
        } => {
            evaluated.push((current_name, value));
            let Some((next_binding, rest)) = pending.split_first() else {
                let frame = Env::new(Some(env));
                for (name, value) in evaluated {
                    frame.define(name, value);
                }
                return start_sequence(body, frame, frames);
            };
            let (next_name, next_expr) = next_binding;

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::LetBinding {
                current_name: next_name.clone(),
                evaluated,
                pending: rest.to_vec(),
                body,
                env: env.clone(),
            });
            Ok(MachineState::Eval(next_expr.clone(), env, next_frames))
        }
        ContinuationFrame::And { rest, env } => {
            if !value.is_truthy() {
                Ok(MachineState::Return(value, frames))
            } else {
                let Some((first, tail)) = rest.split_first() else {
                    return Ok(MachineState::Return(value, frames));
                };
                let mut next_frames = frames;
                if !tail.is_empty() {
                    next_frames.push(ContinuationFrame::And {
                        rest: tail.to_vec(),
                        env: env.clone(),
                    });
                }
                Ok(MachineState::Eval(first.clone(), env, next_frames))
            }
        }
        ContinuationFrame::Or { rest, env } => {
            if value.is_truthy() {
                Ok(MachineState::Return(value, frames))
            } else {
                let Some((first, tail)) = rest.split_first() else {
                    return Ok(MachineState::Return(Value::Boolean(false), frames));
                };
                let mut next_frames = frames;
                if !tail.is_empty() {
                    next_frames.push(ContinuationFrame::Or {
                        rest: tail.to_vec(),
                        env: env.clone(),
                    });
                }
                Ok(MachineState::Eval(first.clone(), env, next_frames))
            }
        }
        ContinuationFrame::ExceptionHandler { .. } => Ok(MachineState::Return(value, frames)),
        ContinuationFrame::DynamicWindStart { context } => {
            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::DynamicWindMarker {
                context: context.clone(),
            });
            next_frames.push(ContinuationFrame::DynamicWindBody {
                context: context.clone(),
            });
            apply_value(
                context.body_thunk.clone(),
                Vec::new(),
                None,
                next_frames,
                ctx,
            )
        }
        ContinuationFrame::DynamicWindBody { context } => {
            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::DynamicWindCleanup {
                context: context.clone(),
                result: value,
            });
            apply_value(
                context.out_thunk.clone(),
                Vec::new(),
                None,
                next_frames,
                ctx,
            )
        }
        ContinuationFrame::DynamicWindCleanup { context, result } => {
            let mut next_frames = frames;
            let Some(ContinuationFrame::DynamicWindMarker {
                context: marker_context,
            }) = next_frames.pop()
            else {
                return Err(EvalError::InvalidSyntax {
                    message: "internal dynamic-wind stack mismatch".to_string(),
                });
            };

            if !Rc::ptr_eq(&marker_context, &context) {
                return Err(EvalError::InvalidSyntax {
                    message: "internal dynamic-wind marker mismatch".to_string(),
                });
            }

            Ok(MachineState::Return(result, next_frames))
        }
        ContinuationFrame::DynamicWindTransition {
            remaining,
            final_value,
            target_frames,
        } => {
            let Some((next, rest)) = remaining.split_first() else {
                return Ok(MachineState::Return(final_value, target_frames));
            };

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::DynamicWindTransition {
                remaining: rest.to_vec(),
                final_value,
                target_frames,
            });
            apply_value(next.clone(), Vec::new(), None, next_frames, ctx)
        }
        ContinuationFrame::TransitionApply {
            remaining,
            procedure,
            args,
            pos,
            target_frames,
        } => {
            let Some((next, rest)) = remaining.split_first() else {
                return apply_value(procedure, args, pos, target_frames, ctx);
            };

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::TransitionApply {
                remaining: rest.to_vec(),
                procedure,
                args,
                pos,
                target_frames,
            });
            apply_value(next.clone(), Vec::new(), None, next_frames, ctx)
        }
        ContinuationFrame::TransitionRaise {
            remaining,
            id,
            value,
        } => {
            let Some((next, rest)) = remaining.split_first() else {
                return Err(EvalError::Raised { id, value });
            };

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::TransitionRaise {
                remaining: rest.to_vec(),
                id,
                value,
            });
            apply_value(next.clone(), Vec::new(), None, next_frames, ctx)
        }
        ContinuationFrame::DynamicWindMarker { .. } => Err(EvalError::InvalidSyntax {
            message: "internal dynamic-wind marker reached unexpectedly".to_string(),
        }),
    }
}

fn start_cond(
    clauses: Vec<Expr>,
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    let Some((clause, rest)) = clauses.split_first() else {
        return Ok(MachineState::Return(Value::Void, frames));
    };

    let ExprKind::List(items) = &clause.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "cond clauses must be lists".to_string(),
        });
    };

    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "cond clauses cannot be empty".to_string(),
        });
    };

    if expr_symbol_name(test).is_some_and(|name| name == "else") {
        if !rest.is_empty() {
            return Err(EvalError::InvalidSyntax {
                message: "cond else clause must be last".to_string(),
            });
        }
        return start_sequence(body.to_vec(), env, frames);
    }

    let mut next_frames = frames;
    next_frames.push(ContinuationFrame::CondTest {
        body: body.to_vec(),
        remaining: rest.to_vec(),
        env: env.clone(),
    });
    Ok(MachineState::Eval(test.clone(), env, next_frames))
}

fn start_let(
    bindings: Vec<(String, Expr)>,
    body: Vec<Expr>,
    env: EnvRef,
    frames: Vec<ContinuationFrame>,
) -> Result<MachineState, EvalError> {
    require_body("let", &body)?;

    let Some((first, rest)) = bindings.split_first() else {
        return start_sequence(body, Env::new(Some(env)), frames);
    };

    let mut next_frames = frames;
    next_frames.push(ContinuationFrame::LetBinding {
        current_name: first.0.clone(),
        evaluated: Vec::new(),
        pending: rest.to_vec(),
        body,
        env: env.clone(),
    });
    Ok(MachineState::Eval(first.1.clone(), env, next_frames))
}

fn apply_value(
    procedure: Value,
    args: Vec<Value>,
    pos: Option<SourcePos>,
    frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    match procedure {
        Value::ControlProc(ControlProc::DynamicWind) => {
            if args.len() != 3 {
                return Err(attach_call_position(
                    EvalError::WrongArgCount {
                        name: "dynamic-wind",
                        expected: "exactly 3",
                        got: args.len(),
                    },
                    pos,
                ));
            }

            let context = Rc::new(DynamicWindContext {
                in_thunk: args[0].clone(),
                body_thunk: args[1].clone(),
                out_thunk: args[2].clone(),
            });

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::DynamicWindStart { context });
            apply_value(args[0].clone(), Vec::new(), pos, next_frames, ctx)
        }
        Value::ControlProc(ControlProc::Raise) => {
            if args.len() != 1 {
                return Err(attach_call_position(
                    EvalError::WrongArgCount {
                        name: "raise",
                        expected: "exactly 1",
                        got: args.len(),
                    },
                    pos,
                ));
            }

            let EvalError::Raised { id, value } = ctx.raise(args[0].clone()) else {
                unreachable!("EvalContext::raise must create a raised exception");
            };
            Ok(MachineState::Raise(id, value, frames))
        }
        Value::ControlProc(ControlProc::WithExceptionHandler) => {
            if args.len() != 2 {
                return Err(attach_call_position(
                    EvalError::WrongArgCount {
                        name: "with-exception-handler",
                        expected: "exactly 2",
                        got: args.len(),
                    },
                    pos,
                ));
            }

            let mut next_frames = frames;
            next_frames.push(ContinuationFrame::ExceptionHandler {
                handler: args[0].clone(),
                pos,
            });
            apply_value(args[1].clone(), Vec::new(), pos, next_frames, ctx)
        }
        Value::ControlProc(ControlProc::CallCc) => {
            if args.len() != 1 {
                return Err(attach_call_position(
                    EvalError::WrongArgCount {
                        name: "call/cc",
                        expected: "exactly 1",
                        got: args.len(),
                    },
                    pos,
                ));
            }

            apply_value(
                args[0].clone(),
                vec![Value::Continuation(Rc::new(Continuation {
                    frames: frames.clone(),
                }))],
                pos,
                frames,
                ctx,
            )
        }
        Value::NativeProc { func, .. } => func(&args, ctx)
            .map(|value| MachineState::Return(value, frames))
            .map_err(|err| attach_call_position(err, pos)),
        Value::RecordProc(procedure) => procedure
            .call(&args)
            .map(|value| MachineState::Return(value, frames))
            .map_err(|err| attach_call_position(err, pos)),
        Value::Closure(closure) => {
            let Some(clause) = closure.matching_clause(args.len()) else {
                return Err(attach_call_position(
                    closure.wrong_arg_count(args.len()),
                    pos,
                ));
            };

            let frame = clause.bind_frame(&args, closure.env.clone());
            start_sequence(clause.body.clone(), frame, frames)
        }
        Value::Continuation(continuation) => {
            if args.len() != 1 {
                return Err(attach_call_position(
                    EvalError::WrongArgCountDynamic {
                        name: "continuation".to_string(),
                        expected: "exactly 1".to_string(),
                        got: args.len(),
                    },
                    pos,
                ));
            }

            start_dynamic_wind_transition(args[0].clone(), frames, continuation.frames.clone(), ctx)
        }
        other => Err(attach_call_position(
            EvalError::NotAProcedure {
                found: other.render(),
            },
            pos,
        )),
    }
}

fn handle_raise(
    id: usize,
    value: String,
    frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    if let Some(index) = frames.iter().rposition(|frame| {
        matches!(frame, ContinuationFrame::ExceptionHandler { .. })
    }) {
        let ContinuationFrame::ExceptionHandler { handler, pos } = frames[index].clone() else {
            unreachable!("matched above");
        };
        let target_frames = frames[..index].to_vec();
        let Some(exception) = ctx.take_exception(id) else {
            return Err(EvalError::InvalidSyntax {
                message: "internal missing exception payload".to_string(),
            });
        };

        return start_transition_apply(
            frames,
            target_frames,
            handler,
            vec![exception],
            pos,
            ctx,
        );
    }

    start_transition_raise(frames, id, value, ctx)
}

fn start_dynamic_wind_transition(
    final_value: Value,
    current_frames: Vec<ContinuationFrame>,
    target_frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    let steps = dynamic_wind_steps(&current_frames, &target_frames);
    start_transition_steps(steps, final_value, target_frames, ctx)
}

fn start_transition_steps(
    steps: Vec<Value>,
    final_value: Value,
    target_frames: Vec<ContinuationFrame>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = steps.split_first() else {
        return Ok(MachineState::Return(final_value, target_frames));
    };

    let frames = vec![ContinuationFrame::DynamicWindTransition {
        remaining: rest.to_vec(),
        final_value,
        target_frames,
    }];
    apply_value(first.clone(), Vec::new(), None, frames, ctx)
}

fn start_transition_apply(
    current_frames: Vec<ContinuationFrame>,
    target_frames: Vec<ContinuationFrame>,
    procedure: Value,
    args: Vec<Value>,
    pos: Option<SourcePos>,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    let steps = dynamic_wind_steps(&current_frames, &target_frames);
    let Some((first, rest)) = steps.split_first() else {
        return apply_value(procedure, args, pos, target_frames, ctx);
    };

    let frames = vec![ContinuationFrame::TransitionApply {
        remaining: rest.to_vec(),
        procedure,
        args,
        pos,
        target_frames,
    }];
    apply_value(first.clone(), Vec::new(), None, frames, ctx)
}

fn start_transition_raise(
    current_frames: Vec<ContinuationFrame>,
    id: usize,
    value: String,
    ctx: &EvalContext,
) -> Result<MachineState, EvalError> {
    let steps = dynamic_wind_steps(&current_frames, &[]);
    let Some((first, rest)) = steps.split_first() else {
        return Err(EvalError::Raised { id, value });
    };

    let frames = vec![ContinuationFrame::TransitionRaise {
        remaining: rest.to_vec(),
        id,
        value,
    }];
    apply_value(first.clone(), Vec::new(), None, frames, ctx)
}

fn active_dynamic_winders(frames: &[ContinuationFrame]) -> Vec<DynamicWindRef> {
    frames
        .iter()
        .filter_map(|frame| match frame {
            ContinuationFrame::DynamicWindMarker { context } => Some(context.clone()),
            _ => None,
        })
        .collect()
}

fn dynamic_wind_steps(
    current_frames: &[ContinuationFrame],
    target_frames: &[ContinuationFrame],
) -> Vec<Value> {
    let current_winders = active_dynamic_winders(current_frames);
    let target_winders = active_dynamic_winders(target_frames);

    let shared_prefix = current_winders
        .iter()
        .zip(target_winders.iter())
        .take_while(|(current, target)| Rc::ptr_eq(current, target))
        .count();

    let mut steps = current_winders[shared_prefix..]
        .iter()
        .rev()
        .map(|context| context.out_thunk.clone())
        .collect::<Vec<_>>();
    steps.extend(
        target_winders[shared_prefix..]
            .iter()
            .map(|context| context.in_thunk.clone()),
    );
    steps
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

    let Some(name) = expr_plain_symbol_name(&items[0]) else {
        return Err(EvalError::InvalidSyntax {
            message: "let binding names must be symbols".to_string(),
        });
    };

    Ok((name.to_string(), items[1].clone()))
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
