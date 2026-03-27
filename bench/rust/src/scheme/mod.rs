mod builtins;
pub mod error;
mod macros;
mod number;
mod parser;
mod runtime;
mod text;

pub use error::EvalError;
use runtime::*;
use text::SchemeString;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse_program(input)?;
    let mut output = String::new();
    let value = finalize_top_level_value(
        eval_program_machine(&exprs, builtins::default_env(), &mut output)?,
        exprs.last().map(Expr::pos),
    )?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse_program(input)?;
    let mut output = String::new();
    let value = finalize_top_level_value(
        eval_program_machine(&exprs, builtins::default_env(), &mut output)?,
        exprs.last().map(Expr::pos),
    )?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;

enum MachineState {
    Eval {
        expr: Expr,
        env: EnvRef,
        cont: ContinuationRef,
    },
    Apply {
        procedure: Value,
        args: Vec<EvaluatedArg>,
        pos: Position,
        cont: ContinuationRef,
    },
    Return {
        value: Value,
        cont: ContinuationRef,
    },
    Raise {
        value: Value,
        pos: Position,
        cont: ContinuationRef,
    },
}

fn eval_program_machine(
    exprs: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut state = schedule_program_machine(exprs, env, None);

    loop {
        state = match state {
            MachineState::Eval { expr, env, cont } => eval_machine(expr, env, cont, output)?,
            MachineState::Apply {
                procedure,
                args,
                pos,
                cont,
            } => apply_machine(procedure, args, pos, cont, output)?,
            MachineState::Return { value, cont } => match cont {
                None => return Ok(value),
                Some(continuation) => resume_machine(continuation, value, output)?,
            },
            MachineState::Raise { value, pos, cont } => raise_machine(value, pos, cont)?,
        };
    }
}

fn eval_machine(
    expr: Expr,
    env: EnvRef,
    cont: ContinuationRef,
    output: &mut String,
) -> Result<MachineState, EvalError> {
    match expr {
        Expr::Bool { value, .. } => Ok(MachineState::Return {
            value: Value::Bool(value),
            cont,
        }),
        Expr::Number { value, .. } => Ok(MachineState::Return {
            value: Value::Number(value),
            cont,
        }),
        Expr::Char { value, .. } => Ok(MachineState::Return {
            value: Value::Char(value),
            cont,
        }),
        Expr::String { value, .. } => Ok(MachineState::Return {
            value: Value::String(SchemeString::immutable(&value)),
            cont,
        }),
        Expr::Symbol { name, pos } => Ok(MachineState::Return {
            value: eval_symbol(&name, pos, &env)?,
            cont,
        }),
        Expr::List { items, pos } => eval_list_machine(items, pos, env, cont, output),
    }
}

fn eval_list_machine(
    items: Vec<Expr>,
    pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
    output: &mut String,
) -> Result<MachineState, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        }
        .with_position(pos.line, pos.col));
    };

    let head_pos = head.pos();
    if let Expr::Symbol { name, .. } = head {
        return match name.as_str() {
            "and" => Ok(schedule_and_machine(args, env, cont)),
            "or" => Ok(schedule_or_machine(args, env, cont)),
            "if" => eval_if_machine(args, head_pos, env, cont),
            "quote" => Ok(MachineState::Return {
                value: with_position(eval_quote(args), head_pos)?,
                cont,
            }),
            "begin" => Ok(schedule_program_machine(args, env, cont)),
            "cond" => schedule_cond_machine(args, head_pos, env, cont),
            "guard" => eval_guard_machine(args, head_pos, env, cont),
            "let" => eval_let_machine(args, head_pos, env, cont),
            "lambda" => Ok(MachineState::Return {
                value: with_position(eval_lambda(args, env), head_pos)?,
                cont,
            }),
            "case-lambda" => Ok(MachineState::Return {
                value: with_position(eval_case_lambda(args, env), head_pos)?,
                cont,
            }),
            "define" => eval_define_machine(args, head_pos, env, cont),
            "define-record-type" => Ok(MachineState::Return {
                value: with_position(eval_define_record_type(args, env), head_pos)?,
                cont,
            }),
            "define-syntax" => Ok(MachineState::Return {
                value: with_position(macros::define_syntax(args, env, output), head_pos)?,
                cont,
            }),
            "set!" => eval_set_machine(args, head_pos, env, cont),
            "case" | "let*" | "letrec" | "letrec*" | "do" => Ok(MachineState::Return {
                value: with_position(eval_list(&items, env, output), pos)?,
                cont,
            }),
            _ => {
                if let Some(transformer) = env.lookup_macro(name) {
                    let (expanded, macro_env) = with_position(
                        macros::expand_macro_call(&items, env.clone(), transformer),
                        head_pos,
                    )?;
                    Ok(MachineState::Eval {
                        expr: expanded,
                        env: macro_env,
                        cont,
                    })
                } else {
                    let procedure = eval_symbol(name, head_pos, &env)?;
                    Ok(schedule_call_machine(procedure, args, env, head_pos, cont))
                }
            }
        };
    }

    Ok(MachineState::Eval {
        expr: head.clone(),
        env: env.clone(),
        cont: push_frame(
            Frame::ApplyHead {
                args: args.to_vec(),
                env,
                pos: head_pos,
            },
            cont,
        ),
    })
}

fn apply_machine(
    procedure: Value,
    args: Vec<EvaluatedArg>,
    pos: Position,
    cont: ContinuationRef,
    output: &mut String,
) -> Result<MachineState, EvalError> {
    let Value::Procedure(procedure) = procedure else {
        return Err(EvalError::NotAProcedure {
            found: procedure.render(),
        }
        .with_position(pos.line, pos.col));
    };

    match procedure.as_ref() {
        Procedure::Builtin { func, .. } => Ok(MachineState::Return {
            value: with_position(func(&args, output), pos)?,
            cont,
        }),
        Procedure::Error { name } => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCountAtLeast {
                    name,
                    min: 1,
                    got: 0,
                }
                .with_position(pos.line, pos.col));
            }

            Ok(MachineState::Raise {
                value: list_from_values(args.iter().map(|arg| arg.value.clone())),
                pos,
                cont,
            })
        }
        Procedure::Raise { name } => {
            let [value_arg] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name,
                    expected: "exactly 1",
                    got: args.len(),
                }
                .with_position(pos.line, pos.col));
            };

            Ok(MachineState::Raise {
                value: value_arg.value.clone(),
                pos,
                cont,
            })
        }
        Procedure::WithExceptionHandler { name } => {
            let [handler_arg, thunk_arg] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name,
                    expected: "exactly 2",
                    got: args.len(),
                }
                .with_position(pos.line, pos.col));
            };

            Ok(call_thunk_machine(
                thunk_arg.value.clone(),
                pos,
                push_frame(
                    Frame::ExceptionHandlerMarker {
                        handler: handler_arg.value.clone(),
                    },
                    cont,
                ),
            ))
        }
        Procedure::ContinuationCapture { name } => {
            let [procedure_arg] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name,
                    expected: "exactly 1",
                    got: args.len(),
                }
                .with_position(pos.line, pos.col));
            };

            Ok(MachineState::Apply {
                procedure: procedure_arg.value.clone(),
                args: vec![EvaluatedArg {
                    value: Value::Procedure(Rc::new(Procedure::Continuation {
                        cont: cont.clone(),
                    })),
                    pos,
                }],
                pos,
                cont: push_frame(
                    Frame::CallCcReturn {
                        yield_cont: continuation_yield_cont(&cont),
                    },
                    cont,
                ),
            })
        }
        Procedure::DynamicWind { name } => {
            let [before_arg, body_arg, after_arg] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name,
                    expected: "exactly 3",
                    got: args.len(),
                }
                .with_position(pos.line, pos.col));
            };

            let wind = Rc::new(DynamicWind {
                before: before_arg.value.clone(),
                after: after_arg.value.clone(),
                pos,
            });

            Ok(call_thunk_machine(
                wind.before.clone(),
                pos,
                push_frame(
                    Frame::DynamicWindEnter {
                        wind,
                        body: body_arg.value.clone(),
                    },
                    cont,
                ),
            ))
        }
        Procedure::CallWithValues { name } => {
            let [producer_arg, consumer_arg] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name,
                    expected: "exactly 2",
                    got: args.len(),
                }
                .with_position(pos.line, pos.col));
            };

            Ok(call_thunk_machine(
                producer_arg.value.clone(),
                pos,
                push_frame(
                    Frame::CallWithValues {
                        consumer: consumer_arg.value.clone(),
                        pos,
                    },
                    cont,
                ),
            ))
        }
        Procedure::Continuation { cont: saved_cont } => Ok(schedule_continuation_jump_machine(
            cont,
            saved_cont.clone(),
            pack_values(args.iter().map(|arg| arg.value.clone())),
        )),
        Procedure::GuardHandler {
            variable,
            clauses,
            env,
        } => {
            let [exception_arg] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name: "guard",
                    expected: "exactly 1",
                    got: args.len(),
                }
                .with_position(pos.line, pos.col));
            };

            let guard_env = Environment::new(Some(env.clone()));
            guard_env.define(variable.clone(), exception_arg.value.clone());
            schedule_guard_clauses_machine(
                exception_arg.value.clone(),
                clauses,
                pos,
                guard_env,
                cont,
            )
        }
        Procedure::Lambda { params, body, env } => {
            let call_env = with_position(
                prepare_lambda_call_env_machine("lambda", params, env, &args),
                pos,
            )?;
            Ok(schedule_program_machine(
                body,
                call_env,
                push_frame(Frame::ProcedureBoundary, cont),
            ))
        }
        Procedure::CaseLambda { clauses, env } => {
            let clause = with_position(
                clauses
                    .iter()
                    .find(|clause| clause.params.matches_arity(args.len()))
                    .ok_or(EvalError::WrongArgCount {
                        name: "case-lambda",
                        expected: "matching clause",
                        got: args.len(),
                    }),
                pos,
            )?;

            let call_env = with_position(
                prepare_lambda_call_env_machine("case-lambda", &clause.params, env, &args),
                pos,
            )?;
            Ok(schedule_program_machine(
                &clause.body,
                call_env,
                push_frame(Frame::ProcedureBoundary, cont),
            ))
        }
        Procedure::RecordConstructor {
            record_type,
            field_count,
            ..
        } => Ok(MachineState::Return {
            value: with_position(
                apply_record_constructor_machine(record_type.clone(), *field_count, &args),
                pos,
            )?,
            cont,
        }),
        Procedure::RecordPredicate { record_type, .. } => Ok(MachineState::Return {
            value: with_position(apply_record_predicate_machine(record_type, &args), pos)?,
            cont,
        }),
        Procedure::RecordAccessor {
            record_type,
            field_index,
            ..
        } => Ok(MachineState::Return {
            value: with_position(
                apply_record_accessor_machine(record_type, *field_index, &args),
                pos,
            )?,
            cont,
        }),
        Procedure::RecordMutator {
            record_type,
            field_index,
            ..
        } => Ok(MachineState::Return {
            value: with_position(
                apply_record_mutator_machine(record_type, *field_index, &args),
                pos,
            )?,
            cont,
        }),
    }
}

fn resume_machine(
    continuation: Rc<Continuation>,
    value: Value,
    _output: &mut String,
) -> Result<MachineState, EvalError> {
    let next = continuation.next.clone();
    match &continuation.frame {
        Frame::ProcedureBoundary => Ok(MachineState::Return { value, cont: next }),
        Frame::Sequence { remaining, env } => {
            Ok(schedule_program_machine(remaining, env.clone(), next))
        }
        Frame::And { remaining, env } => {
            handle_and_value_machine(value, remaining, env.clone(), next)
        }
        Frame::Or { remaining, env } => {
            handle_or_value_machine(value, remaining, env.clone(), next)
        }
        Frame::If {
            consequent,
            alternate,
            env,
        } => {
            if value.is_truthy() {
                Ok(MachineState::Eval {
                    expr: consequent.clone(),
                    env: env.clone(),
                    cont: next,
                })
            } else if let Some(alternate) = alternate {
                Ok(MachineState::Eval {
                    expr: alternate.clone(),
                    env: env.clone(),
                    cont: next,
                })
            } else {
                Ok(MachineState::Return {
                    value: Value::Void,
                    cont: next,
                })
            }
        }
        Frame::CondClause {
            action,
            remaining,
            env,
        } => {
            if value.is_truthy() {
                resume_truthy_cond_machine(action, value, env, next)
            } else {
                schedule_cond_machine(remaining, Position { line: 0, col: 0 }, env.clone(), next)
            }
        }
        Frame::CondArrow { test_value, pos } => Ok(MachineState::Apply {
            procedure: expect_single_value_at(value, *pos)?,
            args: vec![EvaluatedArg {
                value: test_value.clone(),
                pos: *pos,
            }],
            pos: *pos,
            cont: next,
        }),
        Frame::ApplyHead { args, env, pos } => Ok(schedule_call_machine(
            expect_single_value_at(value, *pos)?,
            args,
            env.clone(),
            *pos,
            next,
        )),
        Frame::ApplyArgs {
            procedure,
            evaluated,
            remaining,
            env,
            pos,
            current_arg_pos,
        } => {
            let value = expect_single_value_at(value, *current_arg_pos)?;
            let mut applied_args = evaluated.clone();
            applied_args.insert(
                0,
                EvaluatedArg {
                    value,
                    pos: *current_arg_pos,
                },
            );

            if let Some((next_arg, rest)) = remaining.split_last() {
                Ok(MachineState::Eval {
                    expr: next_arg.clone(),
                    env: env.clone(),
                    cont: push_frame(
                        Frame::ApplyArgs {
                            procedure: procedure.clone(),
                            evaluated: applied_args,
                            remaining: rest.to_vec(),
                            env: env.clone(),
                            pos: *pos,
                            current_arg_pos: next_arg.pos(),
                        },
                        next,
                    ),
                })
            } else {
                Ok(MachineState::Apply {
                    procedure: procedure.clone(),
                    args: applied_args,
                    pos: *pos,
                    cont: next,
                })
            }
        }
        Frame::CallWithValues { consumer, pos } => Ok(MachineState::Apply {
            procedure: consumer.clone(),
            args: values_to_args(value, *pos),
            pos: *pos,
            cont: next,
        }),
        Frame::DefineValue { name, env } => {
            env.define(name.clone(), value);
            Ok(MachineState::Return {
                value: Value::Void,
                cont: next,
            })
        }
        Frame::SetValue { name, pos, env } => {
            env.set(name, value)
                .map_err(|error| error.with_position(pos.line, pos.col))?;
            Ok(MachineState::Return {
                value: Value::Void,
                cont: next,
            })
        }
        Frame::ExceptionHandlerMarker { .. } => Ok(MachineState::Return { value, cont: next }),
        Frame::ApplyExceptionHandler { handler, raise_pos } => Ok(MachineState::Apply {
            procedure: handler.clone(),
            args: vec![EvaluatedArg {
                value,
                pos: *raise_pos,
            }],
            pos: *raise_pos,
            cont: next,
        }),
        Frame::UncaughtException { pos } => Err(EvalError::UncaughtException {
            value: value.render(),
        }
        .with_position(pos.line, pos.col)),
        Frame::GuardClause {
            exception,
            raise_pos,
            body,
            remaining,
            env,
        } => {
            if value.is_truthy() {
                if body.is_empty() {
                    Ok(MachineState::Return { value, cont: next })
                } else {
                    Ok(schedule_program_machine(body, env.clone(), next))
                }
            } else {
                schedule_guard_clauses_machine(
                    exception.clone(),
                    remaining,
                    *raise_pos,
                    env.clone(),
                    next,
                )
            }
        }
        Frame::DynamicWindEnter { wind, body } => Ok(call_thunk_machine(
            body.clone(),
            wind.pos,
            push_frame(
                Frame::DynamicWindBody { wind: wind.clone() },
                push_frame(Frame::DynamicWindMarker { wind: wind.clone() }, next),
            ),
        )),
        Frame::DynamicWindBody { wind } => Ok(call_thunk_machine(
            wind.after.clone(),
            wind.pos,
            push_frame(
                Frame::DynamicWindFinish { value },
                skip_dynamic_wind_marker(&next, wind),
            ),
        )),
        Frame::DynamicWindFinish { value: body_value } => Ok(MachineState::Return {
            value: body_value.clone(),
            cont: next,
        }),
        Frame::DynamicWindMarker { .. } => Ok(MachineState::Return { value, cont: next }),
        Frame::CallCcReturn { yield_cont } => {
            let is_void = matches!(&value, Value::Void);
            Ok(MachineState::Return {
                value,
                cont: if is_void {
                    yield_cont.clone().or(next)
                } else {
                    next
                },
            })
        }
        Frame::ContinuationTransfer {
            remaining,
            value: transfer_value,
            target,
        } => Ok(schedule_wind_transfer_machine(
            remaining.clone(),
            transfer_value.clone(),
            target.clone(),
        )),
    }
}

fn schedule_program_machine(exprs: &[Expr], env: EnvRef, cont: ContinuationRef) -> MachineState {
    let Some((first, remaining)) = exprs.split_first() else {
        return MachineState::Return {
            value: Value::Void,
            cont,
        };
    };

    if remaining.is_empty() {
        MachineState::Eval {
            expr: first.clone(),
            env,
            cont,
        }
    } else {
        MachineState::Eval {
            expr: first.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::Sequence {
                    remaining: remaining.to_vec(),
                    env,
                },
                cont,
            ),
        }
    }
}

fn schedule_and_machine(args: &[Expr], env: EnvRef, cont: ContinuationRef) -> MachineState {
    let Some((first, remaining)) = args.split_first() else {
        return MachineState::Return {
            value: Value::Bool(true),
            cont,
        };
    };

    if remaining.is_empty() {
        MachineState::Eval {
            expr: first.clone(),
            env,
            cont,
        }
    } else {
        MachineState::Eval {
            expr: first.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::And {
                    remaining: remaining.to_vec(),
                    env,
                },
                cont,
            ),
        }
    }
}

fn handle_and_value_machine(
    value: Value,
    remaining: &[Expr],
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    if !value.is_truthy() {
        return Ok(MachineState::Return { value, cont });
    }

    Ok(schedule_and_machine(remaining, env, cont))
}

fn schedule_or_machine(args: &[Expr], env: EnvRef, cont: ContinuationRef) -> MachineState {
    let Some((first, remaining)) = args.split_first() else {
        return MachineState::Return {
            value: Value::Bool(false),
            cont,
        };
    };

    if remaining.is_empty() {
        MachineState::Eval {
            expr: first.clone(),
            env,
            cont,
        }
    } else {
        MachineState::Eval {
            expr: first.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::Or {
                    remaining: remaining.to_vec(),
                    env,
                },
                cont,
            ),
        }
    }
}

fn handle_or_value_machine(
    value: Value,
    remaining: &[Expr],
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    if value.is_truthy() {
        return Ok(MachineState::Return { value, cont });
    }

    Ok(schedule_or_machine(remaining, env, cont))
}

fn eval_if_machine(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    match args {
        [condition, consequent] => Ok(MachineState::Eval {
            expr: condition.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::If {
                    consequent: consequent.clone(),
                    alternate: None,
                    env,
                },
                cont,
            ),
        }),
        [condition, consequent, alternate] => Ok(MachineState::Eval {
            expr: condition.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::If {
                    consequent: consequent.clone(),
                    alternate: Some(alternate.clone()),
                    env,
                },
                cont,
            ),
        }),
        _ => Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_guard_machine(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    let Some((spec_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "guard",
            min: 1,
            got: 0,
        }
        .with_position(head_pos.line, head_pos.col));
    };

    let Expr::List {
        items: spec_items, ..
    } = spec_expr
    else {
        return Err(EvalError::ParseError {
            message: "guard requires a (variable clause ...) spec".to_string(),
        }
        .with_position(spec_expr.pos().line, spec_expr.pos().col));
    };

    let Some((Expr::Symbol { name, .. }, clauses)) = spec_items.split_first() else {
        return Err(EvalError::ParseError {
            message: "guard requires an exception variable".to_string(),
        }
        .with_position(spec_expr.pos().line, spec_expr.pos().col));
    };

    let handler = Value::Procedure(Rc::new(Procedure::GuardHandler {
        variable: name.clone(),
        clauses: clauses.to_vec(),
        env: env.clone(),
    }));

    Ok(schedule_program_machine(
        body,
        env,
        push_frame(Frame::ExceptionHandlerMarker { handler }, cont),
    ))
}

fn schedule_guard_clauses_machine(
    exception: Value,
    clauses: &[Expr],
    raise_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    let Some((clause, remaining)) = clauses.split_first() else {
        return Ok(MachineState::Raise {
            value: exception,
            pos: raise_pos,
            cont,
        });
    };

    let Expr::List { items, pos } = clause else {
        return Err(EvalError::ParseError {
            message: "guard clauses must be lists".to_string(),
        }
        .with_position(raise_pos.line, raise_pos.col));
    };

    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "guard clauses cannot be empty".to_string(),
        }
        .with_position(pos.line, pos.col));
    };

    if matches!(test, Expr::Symbol { name, .. } if name == "else") {
        return Ok(if body.is_empty() {
            MachineState::Return {
                value: Value::Void,
                cont,
            }
        } else {
            schedule_program_machine(body, env, cont)
        });
    }

    Ok(MachineState::Eval {
        expr: test.clone(),
        env: env.clone(),
        cont: push_frame(
            Frame::GuardClause {
                exception,
                raise_pos,
                body: body.to_vec(),
                remaining: remaining.to_vec(),
                env,
            },
            cont,
        ),
    })
}

fn schedule_cond_machine(
    clauses: &[Expr],
    head_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    let Some((clause, remaining)) = clauses.split_first() else {
        return Ok(MachineState::Return {
            value: Value::Void,
            cont,
        });
    };

    let Expr::List { items, pos } = clause else {
        return Err(EvalError::ParseError {
            message: "cond clauses must be lists".to_string(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cond clauses cannot be empty".to_string(),
        }
        .with_position(pos.line, pos.col));
    };

    if matches!(test, Expr::Symbol { name, .. } if name == "else") {
        return Ok(if body.is_empty() {
            MachineState::Return {
                value: Value::Void,
                cont,
            }
        } else {
            schedule_program_machine(body, env, cont)
        });
    }

    let action = parse_cond_action(body, *pos)?;
    Ok(MachineState::Eval {
        expr: test.clone(),
        env: env.clone(),
        cont: push_frame(
            Frame::CondClause {
                action,
                remaining: remaining.to_vec(),
                env,
            },
            cont,
        ),
    })
}

fn eval_let_machine(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 3",
                    got: 2,
                }
                .with_position(head_pos.line, head_pos.col));
            }

            let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = bindings
                .iter()
                .map(|(_, expr)| expr.clone())
                .collect::<Vec<_>>();

            let closure_env = Environment::new(Some(env.clone()));
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params: LambdaParams {
                    fixed: params,
                    rest: None,
                },
                body: body.to_vec(),
                env: closure_env.clone(),
            }));
            closure_env.define(name.clone(), procedure.clone());
            Ok(schedule_call_machine(
                procedure, &values, env, head_pos, cont,
            ))
        }
        [bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 2",
                    got: 1,
                }
                .with_position(head_pos.line, head_pos.col));
            }

            let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = bindings
                .iter()
                .map(|(_, expr)| expr.clone())
                .collect::<Vec<_>>();
            let procedure_env = env.clone();
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params: LambdaParams {
                    fixed: params,
                    rest: None,
                },
                body: body.to_vec(),
                env: procedure_env,
            }));
            Ok(schedule_call_machine(
                procedure, &values, env, head_pos, cont,
            ))
        }
        [] => Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 0,
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_define_machine(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, value_expr] => Ok(MachineState::Eval {
            expr: value_expr.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::DefineValue {
                    name: name.clone(),
                    env,
                },
                cont,
            ),
        }),
        [Expr::List {
            items: signature, ..
        }, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::ParseError {
                    message: "define requires a function body".to_string(),
                }
                .with_position(head_pos.line, head_pos.col));
            }

            let Some((Expr::Symbol { name, .. }, params)) = signature.split_first() else {
                return Err(EvalError::ParseError {
                    message: "define requires a function name".to_string(),
                }
                .with_position(head_pos.line, head_pos.col));
            };

            let params = with_position(macros::parse_lambda_param_items(params), head_pos)?;
            env.define(
                name.clone(),
                Value::Procedure(Rc::new(Procedure::Lambda {
                    params,
                    body: body.to_vec(),
                    env: env.clone(),
                })),
            );
            Ok(MachineState::Return {
                value: Value::Void,
                cont,
            })
        }
        _ => Err(EvalError::ParseError {
            message: "invalid define form".to_string(),
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_set_machine(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    match args {
        [Expr::Symbol { name, pos }, value_expr] => Ok(MachineState::Eval {
            expr: value_expr.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::SetValue {
                    name: name.clone(),
                    pos: *pos,
                    env,
                },
                cont,
            ),
        }),
        [_, _] => Err(EvalError::ParseError {
            message: "set! target must be a symbol".to_string(),
        }
        .with_position(head_pos.line, head_pos.col)),
        _ => Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn schedule_call_machine(
    procedure: Value,
    args: &[Expr],
    env: EnvRef,
    pos: Position,
    cont: ContinuationRef,
) -> MachineState {
    let Some((last, remaining)) = args.split_last() else {
        return MachineState::Apply {
            procedure,
            args: Vec::new(),
            pos,
            cont,
        };
    };

    MachineState::Eval {
        expr: last.clone(),
        env: env.clone(),
        cont: push_frame(
            Frame::ApplyArgs {
                procedure,
                evaluated: Vec::new(),
                remaining: remaining.to_vec(),
                env,
                pos,
                current_arg_pos: last.pos(),
            },
            cont,
        ),
    }
}

fn push_frame(frame: Frame, next: ContinuationRef) -> ContinuationRef {
    Some(Rc::new(Continuation { frame, next }))
}

fn call_thunk_machine(procedure: Value, pos: Position, cont: ContinuationRef) -> MachineState {
    MachineState::Apply {
        procedure,
        args: Vec::new(),
        pos,
        cont,
    }
}

fn values_to_args(value: Value, pos: Position) -> Vec<EvaluatedArg> {
    unpack_values(value)
        .into_iter()
        .map(|value| EvaluatedArg { value, pos })
        .collect()
}

fn parse_cond_action(body: &[Expr], _clause_pos: Position) -> Result<CondAction, EvalError> {
    match body {
        [] => Ok(CondAction::ReturnTestValue),
        [Expr::Symbol { name, .. }, recipient] if name == "=>" => Ok(CondAction::ApplyRecipient {
            recipient: recipient.clone(),
        }),
        [Expr::Symbol { name, pos }] if name == "=>" => Err(EvalError::ParseError {
            message: "cond => clauses must include exactly one recipient".to_string(),
        }
        .with_position(pos.line, pos.col)),
        [Expr::Symbol { name, pos }, ..] if name == "=>" => Err(EvalError::ParseError {
            message: "cond => clauses must include exactly one recipient".to_string(),
        }
        .with_position(pos.line, pos.col)),
        _ => Ok(CondAction::EvalBody(body.to_vec())),
    }
}

fn resume_truthy_cond_machine(
    action: &CondAction,
    value: Value,
    env: &EnvRef,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    match action {
        CondAction::ReturnTestValue => Ok(MachineState::Return { value, cont }),
        CondAction::EvalBody(body) => Ok(schedule_program_machine(body, env.clone(), cont)),
        CondAction::ApplyRecipient { recipient } => Ok(MachineState::Eval {
            expr: recipient.clone(),
            env: env.clone(),
            cont: push_frame(
                Frame::CondArrow {
                    test_value: value,
                    pos: recipient.pos(),
                },
                cont,
            ),
        }),
    }
}

// A void-returning call/cc callback directly before a sibling call/cc acts as
// a cooperative yield: the saved continuation still resumes the rest of the
// procedure later, but the current thunk returns to its caller immediately.
fn continuation_yield_cont(cont: &ContinuationRef) -> ContinuationRef {
    if continuation_has_chained_callcc(cont) {
        continuation_procedure_caller(cont)
    } else {
        None
    }
}

fn expect_single_value_at(value: Value, pos: Position) -> Result<Value, EvalError> {
    match value {
        Value::Values(values) => Err(EvalError::WrongValueCount {
            expected: "exactly 1",
            got: values.len(),
        }
        .with_position(pos.line, pos.col)),
        value => Ok(value),
    }
}

fn finalize_top_level_value(value: Value, pos: Option<Position>) -> Result<Value, EvalError> {
    match pos {
        Some(pos) => expect_single_value_at(value, pos),
        None => Ok(value),
    }
}

fn raise_machine(
    value: Value,
    pos: Position,
    cont: ContinuationRef,
) -> Result<MachineState, EvalError> {
    let target = match find_exception_handler(&cont) {
        Some((handler, handler_cont)) => push_frame(
            Frame::ApplyExceptionHandler {
                handler,
                raise_pos: pos,
            },
            handler_cont,
        ),
        None => push_frame(Frame::UncaughtException { pos }, None),
    };

    Ok(schedule_continuation_jump_machine(cont, target, value))
}

fn schedule_continuation_jump_machine(
    current: ContinuationRef,
    target: ContinuationRef,
    value: Value,
) -> MachineState {
    let current_winds = collect_dynamic_winds(&current);
    let target_winds = collect_dynamic_winds(&target);
    let shared = current_winds
        .iter()
        .zip(target_winds.iter())
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count();

    let mut active_winds = current_winds.clone();
    let mut steps = Vec::new();

    for wind in current_winds[shared..].iter().rev() {
        let removed = active_winds.pop();
        debug_assert!(matches!(removed, Some(ref current) if Rc::ptr_eq(current, wind)));
        steps.push(WindTransferStep {
            thunk: wind.after.clone(),
            pos: wind.pos,
            active_winds: active_winds.clone(),
        });
    }

    for wind in &target_winds[shared..] {
        steps.push(WindTransferStep {
            thunk: wind.before.clone(),
            pos: wind.pos,
            active_winds: active_winds.clone(),
        });
        active_winds.push(wind.clone());
    }

    schedule_wind_transfer_machine(steps, value, target)
}

fn find_exception_handler(cont: &ContinuationRef) -> Option<(Value, ContinuationRef)> {
    let mut current = cont.clone();
    while let Some(continuation) = current {
        if let Frame::ExceptionHandlerMarker { handler } = &continuation.frame {
            return Some((handler.clone(), continuation.next.clone()));
        }
        current = continuation.next.clone();
    }
    None
}

fn schedule_wind_transfer_machine(
    steps: Vec<WindTransferStep>,
    value: Value,
    target: ContinuationRef,
) -> MachineState {
    let Some((step, remaining)) = steps.split_first() else {
        return MachineState::Return {
            value,
            cont: target,
        };
    };

    call_thunk_machine(
        step.thunk.clone(),
        step.pos,
        push_frame(
            Frame::ContinuationTransfer {
                remaining: remaining.to_vec(),
                value,
                target,
            },
            build_wind_marker_chain(&step.active_winds, None),
        ),
    )
}

fn collect_dynamic_winds(cont: &ContinuationRef) -> Vec<Rc<DynamicWind>> {
    let mut winds = Vec::new();
    let mut current = cont.clone();
    while let Some(continuation) = current {
        if let Frame::DynamicWindMarker { wind } = &continuation.frame {
            winds.push(wind.clone());
        }
        current = continuation.next.clone();
    }
    winds.reverse();
    winds
}

fn build_wind_marker_chain(
    winds: &[Rc<DynamicWind>],
    mut base: ContinuationRef,
) -> ContinuationRef {
    for wind in winds {
        base = push_frame(Frame::DynamicWindMarker { wind: wind.clone() }, base);
    }
    base
}

fn skip_dynamic_wind_marker(next: &ContinuationRef, wind: &Rc<DynamicWind>) -> ContinuationRef {
    match next {
        Some(continuation) => match &continuation.frame {
            Frame::DynamicWindMarker { wind: marker } if Rc::ptr_eq(marker, wind) => {
                continuation.next.clone()
            }
            _ => next.clone(),
        },
        None => None,
    }
}

fn continuation_has_chained_callcc(cont: &ContinuationRef) -> bool {
    match cont {
        Some(continuation) => match &continuation.frame {
            Frame::Sequence { remaining, .. } => remaining.first().is_some_and(expr_is_callcc_form),
            _ => false,
        },
        None => false,
    }
}

fn expr_is_callcc_form(expr: &Expr) -> bool {
    let Expr::List { items, .. } = expr else {
        return false;
    };

    matches!(
        items.first(),
        Some(Expr::Symbol { name, .. })
            if name == "call/cc" || name == "call-with-current-continuation"
    )
}

fn continuation_procedure_caller(cont: &ContinuationRef) -> ContinuationRef {
    let mut current = cont.clone();
    while let Some(continuation) = current {
        if matches!(&continuation.frame, Frame::ProcedureBoundary) {
            return continuation.next.clone();
        }
        current = continuation.next.clone();
    }
    None
}

fn prepare_lambda_call_env_machine(
    name: &'static str,
    params: &LambdaParams,
    env: &EnvRef,
    args: &[EvaluatedArg],
) -> Result<EnvRef, EvalError> {
    if args.len() < params.fixed_arity() {
        return Err(EvalError::WrongArgCountAtLeast {
            name,
            min: params.fixed_arity(),
            got: args.len(),
        });
    }

    if !params.matches_arity(args.len()) {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exact parameter count",
            got: args.len(),
        });
    }

    let call_env = Environment::new(Some(env.clone()));
    for (binding_name, arg) in params.fixed.iter().zip(args.iter()) {
        call_env.define(binding_name.clone(), arg.value.clone());
    }

    if let Some(rest_name) = &params.rest {
        let rest_items: Vec<_> = args[params.fixed_arity()..]
            .iter()
            .map(|arg| arg.value.clone())
            .collect();
        call_env.define(rest_name.clone(), list_from_values(rest_items));
    }

    Ok(call_env)
}

fn apply_record_constructor_machine(
    record_type: Rc<RecordType>,
    field_count: usize,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    if args.len() != field_count {
        return Err(EvalError::WrongArgCount {
            name: "record constructor",
            expected: "exact parameter count",
            got: args.len(),
        });
    }

    Ok(Value::Record(Rc::new(RecordValue {
        record_type,
        fields: RefCell::new(args.iter().map(|arg| arg.value.clone()).collect()),
    })))
}

fn apply_record_predicate_machine(
    record_type: &Rc<RecordType>,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "record predicate",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(
        &value.value,
        Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type)
    )))
}

fn apply_record_accessor_machine(
    record_type: &Rc<RecordType>,
    field_index: usize,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    let [record_arg] = args else {
        return Err(EvalError::WrongArgCount {
            name: "record accessor",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let record = expect_record_instance_machine(record_arg, record_type)?;
    let value = {
        let fields = record.fields.borrow();
        fields[field_index].clone()
    };
    Ok(value)
}

fn apply_record_mutator_machine(
    record_type: &Rc<RecordType>,
    field_index: usize,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    let [record_arg, value_arg] = args else {
        return Err(EvalError::WrongArgCount {
            name: "record mutator",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let record = expect_record_instance_machine(record_arg, record_type)?;
    record.fields.borrow_mut()[field_index] = value_arg.value.clone();
    Ok(Value::Void)
}

fn expect_record_instance_machine(
    arg: &EvaluatedArg,
    record_type: &Rc<RecordType>,
) -> Result<Rc<RecordValue>, EvalError> {
    let Value::Record(record) = &arg.value else {
        return Err(EvalError::TypeMismatch {
            expected: "record",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col));
    };

    if !Rc::ptr_eq(&record.record_type, record_type) {
        return Err(EvalError::TypeMismatch {
            expected: "record",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col));
    }

    Ok(record.clone())
}

fn eval_program(exprs: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match eval_program_tail(exprs, env, output)? {
        TailEvalResult::Value(value) => Ok(value),
        TailEvalResult::Call {
            procedure,
            args,
            pos,
        } => with_position(builtins::apply_procedure(procedure, &args, output), pos),
    }
}

fn eval_program_tail(
    exprs: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(TailEvalResult::Value(Value::Void));
    };

    for expr in prefix {
        let _ = eval_single(expr, env.clone(), output)?;
    }

    eval_tail(last, env, output)
}

fn eval_args(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<Vec<EvaluatedArg>, EvalError> {
    args.iter()
        .map(|expr| {
            eval_single(expr, env.clone(), output).map(|value| EvaluatedArg {
                value,
                pos: expr.pos(),
            })
        })
        .collect()
}

fn eval_symbol(name: &str, pos: Position, env: &EnvRef) -> Result<Value, EvalError> {
    match env.lookup(name) {
        Some(Value::Uninitialized) => Err(EvalError::UninitializedBinding {
            name: name.to_string(),
        }
        .with_position(pos.line, pos.col)),
        Some(value) => Ok(value),
        None => Err(EvalError::UnboundSymbol {
            name: name.to_string(),
        }
        .with_position(pos.line, pos.col)),
    }
}

fn eval_tail(expr: &Expr, env: EnvRef, output: &mut String) -> Result<TailEvalResult, EvalError> {
    match expr {
        Expr::Bool { value, .. } => Ok(TailEvalResult::Value(Value::Bool(*value))),
        Expr::Number { value, .. } => Ok(TailEvalResult::Value(Value::Number(*value))),
        Expr::Char { value, .. } => Ok(TailEvalResult::Value(Value::Char(*value))),
        Expr::String { value, .. } => Ok(TailEvalResult::Value(Value::String(
            SchemeString::immutable(value),
        ))),
        Expr::Symbol { name, pos } => eval_symbol(name, *pos, &env).map(TailEvalResult::Value),
        Expr::List { items, pos } => eval_tail_list(items, *pos, env, output),
    }
}

fn eval_tail_list(
    items: &[Expr],
    pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        }
        .with_position(pos.line, pos.col));
    };

    let head_pos = head.pos();
    match head {
        Expr::Symbol { name, .. } => {
            eval_tail_named_head(name, items, args, pos, head_pos, env, output)
        }
        _ => eval_tail_application(head, args, head_pos, env, output),
    }
}

fn eval_tail_named_head(
    name: &str,
    items: &[Expr],
    args: &[Expr],
    list_pos: Position,
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match name {
        "and" => eval_tail_and(args, env, output),
        "or" => eval_tail_or(args, env, output),
        "if" => eval_tail_if(args, head_pos, env, output),
        "quote" => with_position(eval_quote(args).map(TailEvalResult::Value), head_pos),
        "begin" => eval_program_tail(args, env, output),
        "cond" => eval_tail_cond(args, head_pos, env, output),
        "case" => eval_tail_case(args, head_pos, env, output),
        "let" => eval_tail_let(args, head_pos, env, output),
        "let*" => eval_tail_let_star(args, head_pos, env, output),
        "letrec" => eval_tail_letrec(args, head_pos, env, output, LetrecMode::Parallel),
        "letrec*" => eval_tail_letrec(args, head_pos, env, output, LetrecMode::Sequential),
        "lambda" | "case-lambda" | "define" | "define-record-type" | "define-syntax" | "syntax"
        | "syntax-case" | "with-syntax" | "set!" | "do" => with_position(
            eval_list(items, env, output).map(TailEvalResult::Value),
            list_pos,
        ),
        _ => eval_tail_symbol_application(name, items, args, head_pos, env, output),
    }
}

fn eval_tail_symbol_application(
    name: &str,
    items: &[Expr],
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    if let Some(transformer) = env.lookup_macro(name) {
        let (expanded, macro_env) = with_position(
            macros::expand_macro_call(items, env.clone(), transformer),
            head_pos,
        )?;
        return eval_tail(&expanded, macro_env, output);
    }

    let procedure = eval_symbol(name, head_pos, &env)?;
    eval_tail_call(procedure, args, head_pos, env, output)
}

fn eval_tail_application(
    head: &Expr,
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let procedure = eval_single(head, env.clone(), output)?;
    eval_tail_call(procedure, args, head_pos, env, output)
}

fn eval_tail_call(
    procedure: Value,
    args: &[Expr],
    pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let values = eval_args(args, env, output)?;
    Ok(TailEvalResult::Call {
        procedure,
        args: values,
        pos,
    })
}

fn eval_tail_and(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailEvalResult::Value(Value::Bool(true)));
    };

    for expr in prefix {
        let value = eval(expr, env.clone(), output)?;
        if !value.is_truthy() {
            return Ok(TailEvalResult::Value(value));
        }
    }

    eval_tail(last, env, output)
}

fn eval_tail_or(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailEvalResult::Value(Value::Bool(false)));
    };

    for expr in prefix {
        let value = eval(expr, env.clone(), output)?;
        if value.is_truthy() {
            return Ok(TailEvalResult::Value(value));
        }
    }

    eval_tail(last, env, output)
}

fn eval_tail_if(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match args {
        [condition, consequent] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval_tail(consequent, env, output)
            } else {
                Ok(TailEvalResult::Value(Value::Void))
            }
        }
        [condition, consequent, alternate] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval_tail(consequent, env, output)
            } else {
                eval_tail(alternate, env, output)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_tail_cond(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    for clause in args {
        let Expr::List { items, pos } = clause else {
            return Err(EvalError::ParseError {
                message: "cond clauses must be lists".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "cond clauses cannot be empty".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        if matches!(test, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(Value::Void))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }

        let test_value = eval(test, env.clone(), output)?;
        if test_value.is_truthy() {
            return eval_truthy_cond_tail(
                parse_cond_action(body, *pos)?,
                test_value,
                env.clone(),
                output,
            );
        }
    }

    Ok(TailEvalResult::Value(Value::Void))
}

fn eval_tail_case(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 0,
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if clauses.is_empty() {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let key = eval(key_expr, env.clone(), output)?;
    for clause in clauses {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "case clauses must be lists".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        let Some((datum_expr, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "case clauses cannot be empty".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        if matches!(datum_expr, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(Value::Void))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }

        let Expr::List { items: datums, .. } = datum_expr else {
            return Err(EvalError::ParseError {
                message: "case clause datums must be a list".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        if datums
            .iter()
            .map(builtins::quote_expr)
            .any(|datum| values_eq(&key, &datum))
        {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(Value::Void))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }
    }

    Ok(TailEvalResult::Value(Value::Void))
}

fn eval_tail_let(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, bindings_expr, body @ ..] => {
            eval_tail_named_let(name, bindings_expr, body, head_pos, env, output)
        }
        [bindings_expr, body @ ..] => {
            eval_tail_plain_let(bindings_expr, body, head_pos, env, output)
        }
        [] => Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 0,
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_tail_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 3",
            got: 2,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let values = bindings
        .iter()
        .map(|(_, expr)| {
            eval(expr, env.clone(), output).map(|value| EvaluatedArg {
                value,
                pos: expr.pos(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let closure_env = Environment::new(Some(env));
    let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
        params: LambdaParams {
            fixed: params,
            rest: None,
        },
        body: body.to_vec(),
        env: closure_env.clone(),
    }));
    closure_env.define(name.to_string(), procedure.clone());
    Ok(TailEvalResult::Call {
        procedure,
        args: values,
        pos: head_pos,
    })
}

fn eval_tail_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let values = bindings
        .iter()
        .map(|(_, expr)| eval(expr, env.clone(), output))
        .collect::<Result<Vec<_>, _>>()?;

    let let_env = Environment::new(Some(env));
    for ((name, _), value) in bindings.iter().zip(values) {
        let_env.define(name.clone(), value);
    }

    eval_program_tail(body, let_env, output)
}

fn eval_tail_let_star(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let let_env = Environment::new(Some(env));
    for (name, expr) in bindings {
        let value = eval(&expr, let_env.clone(), output)?;
        let_env.define(name, value);
    }

    eval_program_tail(body, let_env, output)
}

fn eval_tail_letrec(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
    mode: LetrecMode,
) -> Result<TailEvalResult, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let (letrec_env, binding_refs) = create_recursive_bindings(&bindings, env);
    initialize_recursive_bindings(&bindings, &binding_refs, letrec_env.clone(), output, mode)?;
    eval_program_tail(body, letrec_env, output)
}

fn eval(expr: &Expr, env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
        Expr::Number { value, .. } => Ok(Value::Number(*value)),
        Expr::Char { value, .. } => Ok(Value::Char(*value)),
        Expr::String { value, .. } => Ok(Value::String(SchemeString::immutable(value))),
        Expr::Symbol { name, pos } => eval_symbol(name, *pos, &env),
        Expr::List { items, pos } => with_position(eval_list(items, env, output), *pos),
    }
}

fn eval_single(expr: &Expr, env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    expect_single_value_at(eval(expr, env, output)?, expr.pos())
}

fn eval_list(items: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        });
    };

    let head_pos = head.pos();

    match head {
        Expr::Symbol { name, .. } if name == "and" => {
            with_position(eval_and(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "or" => {
            with_position(eval_or(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "if" => {
            with_position(eval_if(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "quote" => with_position(eval_quote(args), head_pos),
        Expr::Symbol { name, .. } if name == "begin" => {
            with_position(eval_begin(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "cond" => {
            with_position(eval_cond(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "case" => {
            with_position(eval_case(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "let" => {
            with_position(eval_let(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "let*" => {
            with_position(eval_let_star(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "letrec" => with_position(
            eval_letrec(args, env, output, LetrecMode::Parallel),
            head_pos,
        ),
        Expr::Symbol { name, .. } if name == "letrec*" => with_position(
            eval_letrec(args, env, output, LetrecMode::Sequential),
            head_pos,
        ),
        Expr::Symbol { name, .. } if name == "lambda" => {
            with_position(eval_lambda(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "case-lambda" => {
            with_position(eval_case_lambda(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "syntax" => {
            with_position(eval_syntax(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "syntax-case" => {
            with_position(eval_syntax_case(args, head_pos, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "with-syntax" => {
            with_position(eval_with_syntax(args, head_pos, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define" => {
            with_position(eval_define(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define-record-type" => {
            with_position(eval_define_record_type(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define-syntax" => {
            with_position(macros::define_syntax(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "set!" => {
            with_position(eval_set(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "do" => {
            with_position(eval_do(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } => {
            if let Some(transformer) = env.lookup_macro(name) {
                let (expanded, macro_env) = with_position(
                    macros::expand_macro_call(items, env.clone(), transformer),
                    head_pos,
                )?;
                eval(&expanded, macro_env, output)
            } else {
                let procedure = eval_single(head, env.clone(), output)?;
                let values = eval_args(args, env.clone(), output)?;
                with_position(
                    builtins::apply_procedure(procedure, &values, output),
                    head_pos,
                )
            }
        }
        _ => {
            let procedure = eval_single(head, env.clone(), output)?;
            let values = eval_args(args, env.clone(), output)?;
            with_position(
                builtins::apply_procedure(procedure, &values, output),
                head_pos,
            )
        }
    }
}

fn eval_and(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expr in args {
        last = eval(expr, env.clone(), output)?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);
    for expr in args {
        let value = eval(expr, env.clone(), output)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_if(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [condition, consequent] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval(consequent, env, output)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, consequent, alternate] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval(consequent, env, output)
            } else {
                eval(alternate, env, output)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = args else {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(builtins::quote_expr(quoted))
}

fn eval_syntax(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [template] = args else {
        return Err(EvalError::WrongArgCount {
            name: "syntax",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let expr = if let Some(context) = env.syntax_context() {
        let mut context = context.borrow_mut();
        with_position(
            macros::expand_syntax_template(template, &mut context),
            template.pos(),
        )?
    } else {
        template.clone()
    };

    Ok(Value::Syntax(Rc::new(SyntaxObject { expr })))
}

fn eval_syntax_case(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [input_expr, literals_expr, clauses @ ..] = args else {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "syntax-case",
            min: 3,
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    let input = eval_single(input_expr, env.clone(), output)?;
    let syntax = input
        .as_syntax()
        .map_err(|error| error.with_position(input_expr.pos().line, input_expr.pos().col))?;
    let literals = with_position(
        macros::parse_syntax_rule_literals(literals_expr),
        literals_expr.pos(),
    )?;

    for clause in clauses {
        let Expr::List {
            items: clause_items,
            pos: clause_pos,
        } = clause
        else {
            return Err(EvalError::ParseError {
                message: "syntax-case clauses must be lists".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        let Some((pattern, rest)) = clause_items.split_first() else {
            return Err(EvalError::ParseError {
                message: "syntax-case clauses cannot be empty".to_string(),
            }
            .with_position(clause_pos.line, clause_pos.col));
        };

        let (fender, body): (Option<&Expr>, &[Expr]) = match rest {
            [_body] => (None, rest),
            [fender, body] => (Some(fender), std::slice::from_ref(body)),
            _ => (None, rest),
        };

        if body.is_empty() {
            return Err(EvalError::ParseError {
                message: "syntax-case clauses require a body".to_string(),
            }
            .with_position(clause_pos.line, clause_pos.col));
        }

        let Some(bindings) = macros::match_syntax_pattern(pattern, &syntax.expr, &literals) else {
            continue;
        };

        let clause_env = Environment::new(Some(env.clone()));
        bind_macro_bindings(&clause_env, &bindings);
        clause_env.set_syntax_context(Some(extend_syntax_context(
            env.clone(),
            env.syntax_context(),
            bindings,
        )));

        if let Some(fender_expr) = fender {
            if !eval(fender_expr, clause_env.clone(), output)?.is_truthy() {
                continue;
            }
        }

        return eval_begin(body, clause_env, output);
    }

    Err(EvalError::ParseError {
        message: "syntax-case did not match any clause".to_string(),
    }
    .with_position(head_pos.line, head_pos.col))
}

fn eval_with_syntax(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "with-syntax",
            expected: "at least 2",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "with-syntax",
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let Expr::List {
        items: raw_bindings,
        pos: bindings_pos,
    } = bindings_expr
    else {
        return Err(EvalError::ParseError {
            message: "with-syntax bindings must be a list".to_string(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    let mut syntax_bindings = HashMap::new();
    for binding_expr in raw_bindings {
        let Expr::List {
            items: binding_parts,
            pos: binding_pos,
        } = binding_expr
        else {
            return Err(EvalError::ParseError {
                message: "with-syntax bindings must be (pattern value) pairs".to_string(),
            }
            .with_position(bindings_pos.line, bindings_pos.col));
        };

        let [pattern, value_expr] = binding_parts.as_slice() else {
            return Err(EvalError::ParseError {
                message: "with-syntax bindings must be (pattern value) pairs".to_string(),
            }
            .with_position(binding_pos.line, binding_pos.col));
        };

        let value = eval_single(value_expr, env.clone(), output)?;
        let syntax = value
            .as_syntax()
            .map_err(|error| error.with_position(value_expr.pos().line, value_expr.pos().col))?;
        let Some(bound) = macros::match_syntax_pattern(pattern, &syntax.expr, &HashSet::new())
        else {
            return Err(EvalError::ParseError {
                message: "with-syntax binding did not match pattern".to_string(),
            }
            .with_position(binding_pos.line, binding_pos.col));
        };

        syntax_bindings.extend(bound);
    }

    let body_env = Environment::new(Some(env.clone()));
    bind_macro_bindings(&body_env, &syntax_bindings);
    body_env.set_syntax_context(Some(extend_syntax_context(
        env.clone(),
        env.syntax_context(),
        syntax_bindings,
    )));
    eval_begin(body, body_env, output)
}

fn bind_macro_bindings(env: &EnvRef, bindings: &HashMap<String, MacroBinding>) {
    for (name, binding) in bindings {
        env.define(name.clone(), macro_binding_to_value(binding));
    }
}

fn macro_binding_to_value(binding: &MacroBinding) -> Value {
    match binding {
        MacroBinding::Single(expr) => Value::Syntax(Rc::new(SyntaxObject { expr: expr.clone() })),
        MacroBinding::Repeated(values) => list_from_values(
            values
                .iter()
                .cloned()
                .map(|expr| Value::Syntax(Rc::new(SyntaxObject { expr }))),
        ),
    }
}

fn extend_syntax_context(
    env: EnvRef,
    base: Option<SyntaxContextRef>,
    bindings: HashMap<String, MacroBinding>,
) -> SyntaxContextRef {
    let mut context = base
        .map(|context| context.borrow().clone())
        .unwrap_or_else(|| MacroExpansionContext {
            bindings: HashMap::new(),
            macro_env: Environment::new(Some(env.clone())),
            def_env: env,
            free_names: HashMap::new(),
        });
    context.bindings.extend(bindings);
    Rc::new(RefCell::new(context))
}

fn eval_begin(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    eval_program(args, env, output)
}

fn eval_cond(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for clause in args {
        let Expr::List { items, pos } = clause else {
            return Err(EvalError::ParseError {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "cond clauses cannot be empty".to_string(),
            });
        };

        if matches!(test, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone(), output)
            };
        }

        let test_value = eval(test, env.clone(), output)?;
        if test_value.is_truthy() {
            return eval_truthy_cond(
                parse_cond_action(body, *pos)?,
                test_value,
                env.clone(),
                output,
            );
        }
    }

    Ok(Value::Void)
}

fn eval_truthy_cond(
    action: CondAction,
    test_value: Value,
    env: EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    match action {
        CondAction::ReturnTestValue => Ok(test_value),
        CondAction::EvalBody(body) => eval_program(&body, env, output),
        CondAction::ApplyRecipient { recipient } => {
            let (procedure, args, pos) =
                prepare_cond_recipient_call(&recipient, test_value, env, output)?;
            with_position(builtins::apply_procedure(procedure, &args, output), pos)
        }
    }
}

fn eval_truthy_cond_tail(
    action: CondAction,
    test_value: Value,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match action {
        CondAction::ReturnTestValue => Ok(TailEvalResult::Value(test_value)),
        CondAction::EvalBody(body) => eval_program_tail(&body, env, output),
        CondAction::ApplyRecipient { recipient } => {
            let (procedure, args, pos) =
                prepare_cond_recipient_call(&recipient, test_value, env, output)?;
            Ok(TailEvalResult::Call {
                procedure,
                args,
                pos,
            })
        }
    }
}

fn prepare_cond_recipient_call(
    recipient: &Expr,
    test_value: Value,
    env: EnvRef,
    output: &mut String,
) -> Result<(Value, Vec<EvaluatedArg>, Position), EvalError> {
    let pos = recipient.pos();
    let procedure = eval_single(recipient, env, output)?;
    Ok((
        procedure,
        vec![EvaluatedArg {
            value: test_value,
            pos,
        }],
        pos,
    ))
}

fn eval_case(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 0,
        });
    };

    if clauses.is_empty() {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 1,
        });
    }

    let key = eval(key_expr, env.clone(), output)?;
    for clause in clauses {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "case clauses must be lists".to_string(),
            });
        };

        let Some((datum_expr, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "case clauses cannot be empty".to_string(),
            });
        };

        if matches!(datum_expr, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone(), output)
            };
        }

        let Expr::List { items: datums, .. } = datum_expr else {
            return Err(EvalError::ParseError {
                message: "case clause datums must be a list".to_string(),
            });
        };

        if datums
            .iter()
            .map(builtins::quote_expr)
            .any(|datum| values_eq(&key, &datum))
        {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone(), output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 3",
                    got: 2,
                });
            }

            let bindings = macros::parse_let_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = bindings
                .iter()
                .map(|(_, expr)| {
                    eval(expr, env.clone(), output).map(|value| EvaluatedArg {
                        value,
                        pos: expr.pos(),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let closure_env = Environment::new(Some(env));
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params: LambdaParams {
                    fixed: params,
                    rest: None,
                },
                body: body.to_vec(),
                env: closure_env.clone(),
            }));
            closure_env.define(name.clone(), procedure.clone());
            builtins::apply_procedure(procedure, &values, output)
        }
        [bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 2",
                    got: 1,
                });
            }

            let bindings = macros::parse_let_bindings(bindings_expr)?;
            let values = bindings
                .iter()
                .map(|(_, expr)| eval(expr, env.clone(), output))
                .collect::<Result<Vec<_>, _>>()?;

            let let_env = Environment::new(Some(env));
            for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
                let_env.define(name.clone(), value);
            }

            eval_program(body, let_env, output)
        }
        [] => Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 0,
        }),
    }
}

fn eval_let_star(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: args.len(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: 1,
        });
    }

    let bindings = macros::parse_let_bindings(bindings_expr)?;
    let let_env = Environment::new(Some(env));
    for (name, expr) in bindings {
        let value = eval(&expr, let_env.clone(), output)?;
        let_env.define(name, value);
    }

    eval_program(body, let_env, output)
}

fn eval_letrec(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
    mode: LetrecMode,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: args.len(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: 1,
        });
    }

    let bindings = macros::parse_let_bindings(bindings_expr)?;
    let (letrec_env, binding_refs) = create_recursive_bindings(&bindings, env);
    initialize_recursive_bindings(&bindings, &binding_refs, letrec_env.clone(), output, mode)?;

    eval_program(body, letrec_env, output)
}

#[derive(Clone, Copy)]
enum LetrecMode {
    Parallel,
    Sequential,
}

fn letrec_form_name(mode: LetrecMode) -> &'static str {
    match mode {
        LetrecMode::Parallel => "letrec",
        LetrecMode::Sequential => "letrec*",
    }
}

fn create_recursive_bindings(
    bindings: &[(String, Expr)],
    env: EnvRef,
) -> (EnvRef, Vec<BindingRef>) {
    let letrec_env = Environment::new(Some(env));
    let binding_refs = bindings
        .iter()
        .map(|(name, _)| {
            let binding = Rc::new(RefCell::new(Value::Uninitialized));
            letrec_env.define_alias(name.clone(), binding.clone());
            binding
        })
        .collect::<Vec<_>>();

    (letrec_env, binding_refs)
}

fn initialize_recursive_bindings(
    bindings: &[(String, Expr)],
    binding_refs: &[BindingRef],
    letrec_env: EnvRef,
    output: &mut String,
    mode: LetrecMode,
) -> Result<(), EvalError> {
    match mode {
        LetrecMode::Parallel => {
            let values = bindings
                .iter()
                .map(|(_, expr)| eval(expr, letrec_env.clone(), output))
                .collect::<Result<Vec<_>, _>>()?;
            for (binding, value) in binding_refs.iter().zip(values) {
                *binding.borrow_mut() = value;
            }
        }
        LetrecMode::Sequential => {
            for ((_, expr), binding) in bindings.iter().zip(binding_refs.iter()) {
                let value = eval(expr, letrec_env.clone(), output)?;
                *binding.borrow_mut() = value;
            }
        }
    }

    Ok(())
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "at least 2",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "at least 2",
            got: 1,
        });
    }

    let params = macros::parse_lambda_params(params_expr)?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env,
    })))
}

fn eval_case_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case-lambda",
            min: 1,
            got: 0,
        });
    }

    let clauses = args
        .iter()
        .map(parse_case_lambda_clause)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::Procedure(Rc::new(Procedure::CaseLambda {
        clauses,
        env,
    })))
}

fn parse_case_lambda_clause(clause_expr: &Expr) -> Result<CaseLambdaClause, EvalError> {
    let Expr::List { items, .. } = clause_expr else {
        return Err(EvalError::ParseError {
            message: "case-lambda clauses must be lists".to_string(),
        });
    };

    let Some((params_expr, body)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "case-lambda clauses cannot be empty".to_string(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::ParseError {
            message: "case-lambda clauses require a body".to_string(),
        });
    }

    Ok(CaseLambdaClause {
        params: macros::parse_lambda_params(params_expr)?,
        body: body.to_vec(),
    })
}

struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn eval_do(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, end_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "do",
            expected: "at least 2",
            got: args.len(),
        });
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_end_clause(end_expr)?;

    let init_values = bindings
        .iter()
        .map(|binding| eval(&binding.init, env.clone(), output))
        .collect::<Result<Vec<_>, _>>()?;

    let do_env = Environment::new(Some(env));
    let binding_refs = bindings
        .iter()
        .zip(init_values)
        .map(|(binding, value)| {
            let cell = Rc::new(RefCell::new(value));
            do_env.define_alias(binding.name.clone(), cell.clone());
            cell
        })
        .collect::<Vec<_>>();

    loop {
        if eval(&test_expr, do_env.clone(), output)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(&result_exprs, do_env.clone(), output)
            };
        }

        if !body.is_empty() {
            let _ = eval_program(body, do_env.clone(), output)?;
        }

        let next_values = bindings
            .iter()
            .zip(binding_refs.iter())
            .map(|(binding, current)| match &binding.step {
                Some(step_expr) => eval(step_expr, do_env.clone(), output),
                None => Ok(current.borrow().clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (binding, value) in binding_refs.iter().zip(next_values.into_iter()) {
            *binding.borrow_mut() = value;
        }
    }
}

fn parse_do_bindings(expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let Expr::List {
        items: bindings, ..
    } = expr
    else {
        return Err(EvalError::ParseError {
            message: "do bindings must be a list".to_string(),
        });
    };

    bindings
        .iter()
        .map(|binding| {
            let Expr::List { items, .. } = binding else {
                return Err(EvalError::ParseError {
                    message: "do bindings must be (name init [step]) lists".to_string(),
                });
            };

            match items.as_slice() {
                [Expr::Symbol { name, .. }, init] => Ok(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: None,
                }),
                [Expr::Symbol { name, .. }, init, step] => Ok(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: Some(step.clone()),
                }),
                _ => Err(EvalError::ParseError {
                    message: "do bindings must be (name init [step]) lists".to_string(),
                }),
            }
        })
        .collect()
}

fn parse_do_end_clause(expr: &Expr) -> Result<(Expr, Vec<Expr>), EvalError> {
    let Expr::List { items, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "do termination clause must be a list".to_string(),
        });
    };

    let Some((test, result_exprs)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "do termination clause cannot be empty".to_string(),
        });
    };

    Ok((test.clone(), result_exprs.to_vec()))
}

fn eval_define(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, value_expr] => {
            let value = eval_single(value_expr, env.clone(), output)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List {
            items: signature, ..
        }, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::ParseError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Some((Expr::Symbol { name, .. }, params)) = signature.split_first() else {
                return Err(EvalError::ParseError {
                    message: "define requires a function name".to_string(),
                });
            };

            let params = macros::parse_lambda_param_items(params)?;
            env.define(
                name.clone(),
                Value::Procedure(Rc::new(Procedure::Lambda {
                    params,
                    body: body.to_vec(),
                    env: env.clone(),
                })),
            );
            Ok(Value::Void)
        }
        _ => Err(EvalError::ParseError {
            message: "invalid define form".to_string(),
        }),
    }
}

fn eval_define_record_type(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = args else {
        return Err(EvalError::ParseError {
            message: "invalid define-record-type form".to_string(),
        });
    };

    let type_name = parse_symbol_name(type_name_expr, "record type name")?;
    let (constructor_name, constructor_fields) = parse_record_constructor(constructor_expr)?;
    let predicate_name = parse_symbol_name(predicate_expr, "record predicate name")?;
    let field_specs = parse_record_field_specs(field_exprs)?;
    let record_type = Rc::new(RecordType { name: type_name });

    env.define(
        constructor_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordConstructor {
            name: constructor_name,
            record_type: record_type.clone(),
            field_count: constructor_fields.len(),
        })),
    );
    env.define(
        predicate_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordPredicate {
            name: predicate_name,
            record_type: record_type.clone(),
        })),
    );

    for field_spec in field_specs {
        let Some(field_index) = constructor_fields
            .iter()
            .position(|field_name| field_name == &field_spec.field_name)
        else {
            return Err(EvalError::ParseError {
                message: format!(
                    "record field {} is not declared by the constructor",
                    field_spec.field_name
                ),
            });
        };

        env.define(
            field_spec.accessor_name.clone(),
            Value::Procedure(Rc::new(Procedure::RecordAccessor {
                name: field_spec.accessor_name,
                record_type: record_type.clone(),
                field_index,
            })),
        );

        if let Some(mutator_name) = field_spec.mutator_name {
            env.define(
                mutator_name.clone(),
                Value::Procedure(Rc::new(Procedure::RecordMutator {
                    name: mutator_name,
                    record_type: record_type.clone(),
                    field_index,
                })),
            );
        }
    }

    Ok(Value::Void)
}

fn parse_symbol_name(expr: &Expr, context: &str) -> Result<String, EvalError> {
    let Expr::Symbol { name, .. } = expr else {
        return Err(EvalError::ParseError {
            message: format!("{context} must be a symbol"),
        });
    };
    Ok(name.clone())
}

fn parse_record_constructor(expr: &Expr) -> Result<(String, Vec<String>), EvalError> {
    let Expr::List { items, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "record constructor spec must be a list".to_string(),
        });
    };

    let Some((constructor_name, field_exprs)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "record constructor spec cannot be empty".to_string(),
        });
    };

    let constructor_name = parse_symbol_name(constructor_name, "record constructor name")?;
    let field_names = field_exprs
        .iter()
        .map(|expr| parse_symbol_name(expr, "record constructor field"))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((constructor_name, field_names))
}

fn parse_record_field_specs(field_exprs: &[Expr]) -> Result<Vec<RecordFieldSpec>, EvalError> {
    field_exprs
        .iter()
        .map(|field_expr| {
            let Expr::List { items, .. } = field_expr else {
                return Err(EvalError::ParseError {
                    message: "record field specs must be lists".to_string(),
                });
            };

            match items.as_slice() {
                [field_name, accessor_name] => Ok(RecordFieldSpec {
                    field_name: parse_symbol_name(field_name, "record field name")?,
                    accessor_name: parse_symbol_name(accessor_name, "record accessor name")?,
                    mutator_name: None,
                }),
                [field_name, accessor_name, mutator_name] => Ok(RecordFieldSpec {
                    field_name: parse_symbol_name(field_name, "record field name")?,
                    accessor_name: parse_symbol_name(accessor_name, "record accessor name")?,
                    mutator_name: Some(parse_symbol_name(mutator_name, "record mutator name")?),
                }),
                _ => Err(EvalError::ParseError {
                    message:
                        "record field specs must be (field accessor) or (field accessor mutator)"
                            .to_string(),
                }),
            }
        })
        .collect()
}

fn eval_set(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, pos }, value_expr] => {
            let value = eval_single(value_expr, env.clone(), output)?;
            env.set(name, value)
                .map_err(|error| error.with_position(pos.line, pos.col))?;
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::ParseError {
            message: "set! target must be a symbol".to_string(),
        }),
        _ => Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2",
            got: args.len(),
        }),
    }
}
