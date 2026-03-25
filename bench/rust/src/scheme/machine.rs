use super::builtins;
use super::macros::expand_macro_call;
use super::records::{
    apply_record_accessor, apply_record_constructor, apply_record_predicate,
    eval_define_record_type,
};
use super::value_ops::eqv_value;
use super::{
    bind_lambda_call, env_define, env_lookup_macro, env_set, eval_case_lambda, eval_define_syntax,
    eval_lambda, eval_quote, eval_syntax, eval_syntax_case, eval_with_syntax, lookup_symbol,
    make_immutable_string_value, make_lambda, parse_define_signature, parse_do_binding,
    parse_value_binding, quote_expr, select_case_lambda_clause, syntax_error, wrong_arg_count, Env,
    EnvRef, EvalContext, EvalError, Expr, ExprKind, LambdaParams, Procedure, SourcePos, Value,
    START_POS,
};
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub(super) struct Continuation {
    frames: Vec<Frame>,
    winds: Vec<WindRef>,
}

pub(super) type WindRef = Rc<DynamicWind>;

#[derive(Debug, Clone)]
pub(super) struct DynamicWind {
    in_thunk: Value,
    out_thunk: Value,
}

#[derive(Debug, Clone)]
enum MachineState {
    Eval { expr: Expr, env: EnvRef },
    Value(Value),
}

#[derive(Debug, Clone)]
enum Frame {
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        name: String,
        env: EnvRef,
        pos: SourcePos,
    },
    If {
        when_true: Expr,
        when_false: Option<Expr>,
        env: EnvRef,
    },
    ApplyOperator {
        arg_exprs: Vec<Expr>,
        env: EnvRef,
        pos: SourcePos,
    },
    ApplyArgument {
        operator: Value,
        evaluated_args: Vec<Value>,
        remaining_arg_exprs: Vec<Expr>,
        env: EnvRef,
        pos: SourcePos,
    },
    And {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Or {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Cond {
        body: Vec<Expr>,
        remaining_clauses: Vec<Expr>,
        env: EnvRef,
        clause_pos: SourcePos,
    },
    Case {
        clauses: Vec<Expr>,
        env: EnvRef,
    },
    Let {
        current_name: String,
        remaining_bindings: Vec<(String, Expr)>,
        evaluated_bindings: Vec<(String, Value)>,
        body: Vec<Expr>,
        env: EnvRef,
        pos: SourcePos,
    },
    NamedLet {
        lambda: Value,
        remaining_inits: Vec<Expr>,
        evaluated_args: Vec<Value>,
        env: EnvRef,
        pos: SourcePos,
    },
    DynamicWindAfterIn {
        wind: WindRef,
        body_thunk: Value,
        pos: SourcePos,
    },
    DynamicWindAfterBody {
        wind: WindRef,
        pos: SourcePos,
    },
    DynamicWindAfterOut {
        result: Value,
    },
    CallWithValues {
        consumer: Value,
        pos: SourcePos,
    },
    ExceptionHandler {
        handler: Value,
        winds: Vec<WindRef>,
    },
    Guard {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
        winds: Vec<WindRef>,
    },
    RaiseHandlerReturned {
        pos: SourcePos,
    },
    RaiseInvokeHandler {
        handler: Value,
        pos: SourcePos,
    },
    GuardHandle {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
        raise_pos: SourcePos,
    },
    GuardClause {
        exception: Value,
        body: Vec<Expr>,
        remaining_clauses: Vec<Expr>,
        env: EnvRef,
        clause_pos: SourcePos,
        raise_pos: SourcePos,
    },
    WindTransitionAfterOut {
        transition: WindTransition,
    },
    WindTransitionAfterIn {
        wind: WindRef,
        transition: WindTransition,
    },
    Letrec {
        current_index: usize,
        names: Vec<String>,
        value_exprs: Vec<Expr>,
        letrec_env: EnvRef,
        values: Vec<Value>,
        sequential: bool,
        body: Vec<Expr>,
        pos: SourcePos,
    },
}

#[derive(Debug, Clone)]
struct WindTransition {
    exiting: Vec<WindRef>,
    entering: Vec<WindRef>,
    target_frames: Vec<Frame>,
    target_winds: Vec<WindRef>,
    value: Value,
    pos: SourcePos,
}

enum ExceptionHandlerTarget {
    Handler {
        handler: Value,
        frames: Vec<Frame>,
        winds: Vec<WindRef>,
    },
    Guard {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
        frames: Vec<Frame>,
        winds: Vec<WindRef>,
    },
}

impl fmt::Debug for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Continuation(..)")
    }
}

pub(super) fn eval_sequence(
    exprs: &[Expr],
    env: &EnvRef,
    empty_pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let mut stack = Vec::new();
    let state = start_sequence_state(exprs, Rc::clone(env), empty_pos, &mut stack)?;
    run_machine(state, stack, context)
}

pub(super) fn eval_expr(
    expr: &Expr,
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    run_machine(
        MachineState::Eval {
            expr: expr.clone(),
            env: Rc::clone(env),
        },
        Vec::new(),
        context,
    )
}

pub(super) fn apply_value(
    operator: Value,
    args: Vec<Value>,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let mut stack = Vec::new();
    let state = apply_machine(operator, args, pos, &mut stack, context)?;
    run_machine(state, stack, context)
}

fn run_machine(
    mut state: MachineState,
    mut stack: Vec<Frame>,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    loop {
        match state {
            MachineState::Eval { expr, env } => {
                state = eval_machine_expr(expr, env, &mut stack, context)?;
            }
            MachineState::Value(value) => {
                let Some(frame) = stack.pop() else {
                    return Ok(value);
                };
                state = resume_frame(frame, value, &mut stack, context)?;
            }
        }
    }
}

fn start_sequence_state(
    exprs: &[Expr],
    env: EnvRef,
    empty_pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = exprs.split_first() else {
        return Err(syntax_error(empty_pos, "empty input"));
    };

    if !rest.is_empty() {
        stack.push(Frame::Sequence {
            remaining: rest.to_vec(),
            env: Rc::clone(&env),
        });
    }

    Ok(MachineState::Eval {
        expr: first.clone(),
        env,
    })
}

fn continue_sequence_state(
    remaining: Vec<Expr>,
    env: EnvRef,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let mut iter = remaining.into_iter();
    let first = iter
        .next()
        .ok_or_else(|| syntax_error(START_POS, "internal error: empty sequence"))?;
    let rest = iter.collect::<Vec<_>>();

    if !rest.is_empty() {
        stack.push(Frame::Sequence {
            remaining: rest,
            env: Rc::clone(&env),
        });
    }

    Ok(MachineState::Eval { expr: first, env })
}

fn eval_machine_expr(
    expr: Expr,
    env: EnvRef,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    match expr.kind {
        ExprKind::Number(value) => Ok(MachineState::Value(Value::Number(value))),
        ExprKind::Boolean(value) => Ok(MachineState::Value(Value::Boolean(value))),
        ExprKind::Character(value) => Ok(MachineState::Value(Value::Character(value))),
        ExprKind::String(value) => Ok(MachineState::Value(make_immutable_string_value(value))),
        ExprKind::Symbol(name) => lookup_symbol(&env, &name, expr.pos).map(MachineState::Value),
        ExprKind::List(items) => eval_machine_list(items, env, expr.pos, stack, context),
    }
}

fn eval_machine_list(
    items: Vec<Expr>,
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    if items.is_empty() {
        return Err(syntax_error(pos, "cannot evaluate empty list"));
    }

    if let Some(name) = items[0].symbol_name() {
        let form_pos = items[0].pos;
        match name {
            "begin" => return start_sequence_state(&items[1..], env, form_pos, stack),
            "case" => return start_case_state(&items[1..], env, form_pos, stack),
            "case-lambda" => {
                return eval_case_lambda(&items[1..], &env, form_pos).map(MachineState::Value);
            }
            "cond" => return start_cond_state(&items[1..], env, stack),
            "define" => return start_define_state(&items[1..], env, form_pos, stack),
            "define-record-type" => {
                return eval_define_record_type(&items[1..], &env, form_pos)
                    .map(MachineState::Value);
            }
            "define-syntax" => {
                return eval_define_syntax(&items[1..], &env, form_pos, context)
                    .map(MachineState::Value);
            }
            "do" => {
                let expanded = expand_do(&items[1..], form_pos)?;
                return Ok(MachineState::Eval {
                    expr: expanded,
                    env,
                });
            }
            "guard" => return start_guard_state(&items[1..], env, form_pos, stack, context),
            "if" => return start_if_state(&items[1..], env, form_pos, stack),
            "let" => return start_let_state(&items[1..], env, form_pos, stack, context),
            "let*" => {
                let expanded = expand_let_star(&items[1..], form_pos)?;
                return Ok(MachineState::Eval {
                    expr: expanded,
                    env,
                });
            }
            "letrec" => return start_letrec_state(&items[1..], env, form_pos, false, stack),
            "letrec*" => return start_letrec_state(&items[1..], env, form_pos, true, stack),
            "quote" => return eval_quote(&items[1..], form_pos).map(MachineState::Value),
            "set!" => return start_set_state(&items[1..], env, form_pos, stack),
            "syntax" => {
                return eval_syntax(&items[1..], &env, form_pos, context).map(MachineState::Value)
            }
            "syntax-case" => {
                return eval_syntax_case(&items[1..], &env, form_pos, context)
                    .map(MachineState::Value)
            }
            "lambda" => {
                return eval_lambda(None, &items[1..], &env, form_pos).map(MachineState::Value);
            }
            "with-syntax" => {
                return eval_with_syntax(&items[1..], &env, form_pos, context)
                    .map(MachineState::Value)
            }
            "and" => return start_and_state(&items[1..], env, stack),
            "or" => return start_or_state(&items[1..], env, stack),
            _ => {}
        }

        if let Some(transformer) = env_lookup_macro(&env, name) {
            let expanded = expand_macro_call(&transformer, &items, pos, &env, context)?;
            return Ok(MachineState::Eval {
                expr: expanded,
                env,
            });
        }
    }

    start_application_state(items, env, pos, stack)
}

fn resume_frame(
    frame: Frame,
    value: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    match frame {
        Frame::Sequence { remaining, env } => continue_sequence_state(remaining, env, stack),
        Frame::DefineValue { name, env } => {
            env_define(&env, name, value);
            Ok(MachineState::Value(Value::Void))
        }
        Frame::SetValue { name, env, pos } => handle_set_frame(name, env, pos, value),
        Frame::If {
            when_true,
            when_false,
            env,
        } => handle_if_frame(when_true, when_false, env, value),
        Frame::ApplyOperator {
            arg_exprs,
            env,
            pos,
        } => resume_apply_operator(arg_exprs, env, pos, value, stack, context),
        Frame::ApplyArgument {
            operator,
            evaluated_args,
            remaining_arg_exprs,
            env,
            pos,
        } => resume_apply_argument(
            operator,
            evaluated_args,
            remaining_arg_exprs,
            env,
            pos,
            value,
            stack,
            context,
        ),
        Frame::And { remaining, env } => {
            resume_short_circuit_frame(remaining, env, value, stack, true)
        }
        Frame::Or { remaining, env } => {
            resume_short_circuit_frame(remaining, env, value, stack, false)
        }
        Frame::Cond {
            body,
            remaining_clauses,
            env,
            clause_pos,
        } => resume_cond_frame(body, remaining_clauses, env, clause_pos, value, stack),
        Frame::Case { clauses, env } => finish_case(value, clauses, env, stack),
        Frame::Let {
            current_name,
            remaining_bindings,
            evaluated_bindings,
            body,
            env,
            pos,
        } => resume_let_frame(
            current_name,
            remaining_bindings,
            evaluated_bindings,
            body,
            env,
            pos,
            value,
            stack,
        ),
        Frame::NamedLet {
            lambda,
            remaining_inits,
            evaluated_args,
            env,
            pos,
        } => resume_named_let_frame(
            lambda,
            remaining_inits,
            evaluated_args,
            env,
            pos,
            value,
            stack,
            context,
        ),
        Frame::DynamicWindAfterIn {
            wind,
            body_thunk,
            pos,
        } => resume_dynamic_wind_after_in(wind, body_thunk, pos, stack, context),
        Frame::DynamicWindAfterBody { wind, pos } => {
            resume_dynamic_wind_after_body(wind, pos, value, stack, context)
        }
        Frame::DynamicWindAfterOut { result } => Ok(MachineState::Value(result)),
        Frame::CallWithValues { consumer, pos } => {
            resume_call_with_values(consumer, pos, value, stack, context)
        }
        Frame::ExceptionHandler { .. } | Frame::Guard { .. } => Ok(MachineState::Value(value)),
        Frame::RaiseHandlerReturned { pos } => Err(EvalError::ExceptionHandlerReturned { pos }),
        Frame::RaiseInvokeHandler { handler, pos } => {
            apply_machine(handler, vec![value], pos, stack, context)
        }
        Frame::GuardHandle {
            variable,
            clauses,
            env,
            raise_pos,
        } => resume_guard_handle_frame(variable, clauses, env, raise_pos, value, stack, context),
        Frame::GuardClause {
            exception,
            body,
            remaining_clauses,
            env,
            clause_pos,
            raise_pos,
        } => resume_guard_clause_frame(
            exception,
            body,
            remaining_clauses,
            env,
            clause_pos,
            raise_pos,
            value,
            stack,
            context,
        ),
        Frame::WindTransitionAfterOut { transition } => {
            continue_wind_transition(transition, stack, context)
        }
        Frame::WindTransitionAfterIn { wind, transition } => {
            context.active_winds.push(wind);
            continue_wind_transition(transition, stack, context)
        }
        Frame::Letrec {
            current_index,
            names,
            value_exprs,
            letrec_env,
            values,
            sequential,
            body,
            pos,
        } => resume_letrec_frame(
            current_index,
            names,
            value_exprs,
            letrec_env,
            values,
            sequential,
            body,
            pos,
            value,
            stack,
        ),
    }
}

fn handle_set_frame(
    name: String,
    env: EnvRef,
    pos: SourcePos,
    value: Value,
) -> Result<MachineState, EvalError> {
    if env_set(&env, &name, value) {
        Ok(MachineState::Value(Value::Void))
    } else {
        Err(EvalError::UnboundVariable { pos, name })
    }
}

fn handle_if_frame(
    when_true: Expr,
    when_false: Option<Expr>,
    env: EnvRef,
    value: Value,
) -> Result<MachineState, EvalError> {
    if value.is_truthy() {
        return Ok(MachineState::Eval {
            expr: when_true,
            env,
        });
    }

    if let Some(when_false) = when_false {
        Ok(MachineState::Eval {
            expr: when_false,
            env,
        })
    } else {
        Ok(MachineState::Value(Value::Void))
    }
}

fn resume_apply_operator(
    arg_exprs: Vec<Expr>,
    env: EnvRef,
    pos: SourcePos,
    operator: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    if arg_exprs.is_empty() {
        return apply_machine(operator, Vec::new(), pos, stack, context);
    }

    let mut arg_exprs = arg_exprs;
    let next = arg_exprs.pop().expect("checked for empty arg list");
    stack.push(Frame::ApplyArgument {
        operator,
        evaluated_args: Vec::new(),
        remaining_arg_exprs: arg_exprs,
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval { expr: next, env })
}

#[allow(clippy::too_many_arguments)]
fn resume_apply_argument(
    operator: Value,
    mut evaluated_args: Vec<Value>,
    remaining_arg_exprs: Vec<Expr>,
    env: EnvRef,
    pos: SourcePos,
    value: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    evaluated_args.insert(0, value);
    if remaining_arg_exprs.is_empty() {
        return apply_machine(operator, evaluated_args, pos, stack, context);
    }

    let mut remaining_arg_exprs = remaining_arg_exprs;
    let next = remaining_arg_exprs
        .pop()
        .expect("checked for empty arg list");
    stack.push(Frame::ApplyArgument {
        operator,
        evaluated_args,
        remaining_arg_exprs,
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval { expr: next, env })
}

fn resume_short_circuit_frame(
    remaining: Vec<Expr>,
    env: EnvRef,
    value: Value,
    stack: &mut Vec<Frame>,
    truthy_value_finishes: bool,
) -> Result<MachineState, EvalError> {
    let should_finish = if truthy_value_finishes {
        !value.is_truthy()
    } else {
        value.is_truthy()
    };

    if should_finish || remaining.is_empty() {
        return Ok(MachineState::Value(value));
    }

    let mut iter = remaining.into_iter();
    let first = iter.next().expect("checked for empty short-circuit tail");
    let rest = iter.collect::<Vec<_>>();
    if !rest.is_empty() {
        stack.push(if truthy_value_finishes {
            Frame::And {
                remaining: rest,
                env: Rc::clone(&env),
            }
        } else {
            Frame::Or {
                remaining: rest,
                env: Rc::clone(&env),
            }
        });
    }

    Ok(MachineState::Eval { expr: first, env })
}

fn resume_cond_frame(
    body: Vec<Expr>,
    remaining_clauses: Vec<Expr>,
    env: EnvRef,
    clause_pos: SourcePos,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    if value.is_truthy() {
        return if body.is_empty() {
            Ok(MachineState::Value(value))
        } else {
            start_sequence_state(&body, env, clause_pos, stack)
        };
    }

    start_cond_owned(remaining_clauses, env, stack)
}

#[allow(clippy::too_many_arguments)]
fn resume_let_frame(
    current_name: String,
    remaining_bindings: Vec<(String, Expr)>,
    mut evaluated_bindings: Vec<(String, Value)>,
    body: Vec<Expr>,
    env: EnvRef,
    pos: SourcePos,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    evaluated_bindings.push((current_name, value));
    if remaining_bindings.is_empty() {
        let let_env = Env::new_child(&env);
        for (name, value) in evaluated_bindings {
            env_define(&let_env, name, value);
        }
        return start_sequence_state(&body, let_env, pos, stack);
    }

    let mut iter = remaining_bindings.into_iter();
    let (next_name, next_expr) = iter.next().expect("checked for empty bindings");
    stack.push(Frame::Let {
        current_name: next_name,
        remaining_bindings: iter.collect(),
        evaluated_bindings,
        body,
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval {
        expr: next_expr,
        env,
    })
}

#[allow(clippy::too_many_arguments)]
fn resume_named_let_frame(
    lambda: Value,
    remaining_inits: Vec<Expr>,
    mut evaluated_args: Vec<Value>,
    env: EnvRef,
    pos: SourcePos,
    value: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    evaluated_args.push(value);
    if remaining_inits.is_empty() {
        return apply_machine(lambda, evaluated_args, pos, stack, context);
    }

    let mut iter = remaining_inits.into_iter();
    let first = iter.next().expect("checked for empty named let tail");
    stack.push(Frame::NamedLet {
        lambda,
        remaining_inits: iter.collect(),
        evaluated_args,
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval { expr: first, env })
}

fn resume_dynamic_wind_after_in(
    wind: WindRef,
    body_thunk: Value,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    context.active_winds.push(Rc::clone(&wind));
    stack.push(Frame::DynamicWindAfterBody { wind, pos });
    apply_machine(body_thunk, Vec::new(), pos, stack, context)
}

fn resume_dynamic_wind_after_body(
    wind: WindRef,
    pos: SourcePos,
    result: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    pop_active_wind(&mut context.active_winds, &wind);
    stack.push(Frame::DynamicWindAfterOut { result });
    apply_machine(wind.out_thunk.clone(), Vec::new(), pos, stack, context)
}

fn resume_call_with_values(
    consumer: Value,
    pos: SourcePos,
    produced: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    apply_machine(consumer, produced.into_values(), pos, stack, context)
}

fn resume_guard_handle_frame(
    variable: String,
    clauses: Vec<Expr>,
    env: EnvRef,
    raise_pos: SourcePos,
    exception: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let guard_env = Env::new_child(&env);
    env_define(&guard_env, variable, exception.clone());
    start_guard_clauses_owned(exception, clauses, guard_env, raise_pos, stack, context)
}

#[allow(clippy::too_many_arguments)]
fn resume_guard_clause_frame(
    exception: Value,
    body: Vec<Expr>,
    remaining_clauses: Vec<Expr>,
    env: EnvRef,
    clause_pos: SourcePos,
    raise_pos: SourcePos,
    test_value: Value,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    if test_value.is_truthy() {
        return if body.is_empty() {
            Ok(MachineState::Value(test_value))
        } else {
            start_sequence_state(&body, env, clause_pos, stack)
        };
    }

    start_guard_clauses_owned(exception, remaining_clauses, env, raise_pos, stack, context)
}

#[allow(clippy::too_many_arguments)]
fn resume_letrec_frame(
    current_index: usize,
    names: Vec<String>,
    value_exprs: Vec<Expr>,
    letrec_env: EnvRef,
    mut values: Vec<Value>,
    sequential: bool,
    body: Vec<Expr>,
    pos: SourcePos,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    if sequential {
        env_set(&letrec_env, &names[current_index], value);
    } else {
        values.push(value);
    }

    let next_index = current_index + 1;
    if next_index < names.len() {
        let next_expr = value_exprs[next_index].clone();
        stack.push(Frame::Letrec {
            current_index: next_index,
            names,
            value_exprs,
            letrec_env: Rc::clone(&letrec_env),
            values,
            sequential,
            body,
            pos,
        });
        return Ok(MachineState::Eval {
            expr: next_expr,
            env: letrec_env,
        });
    }

    if !sequential {
        for (name, value) in names.into_iter().zip(values.into_iter()) {
            env_set(&letrec_env, &name, value);
        }
    }
    start_sequence_state(&body, letrec_env, pos, stack)
}

fn start_application_state(
    items: Vec<Expr>,
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let mut iter = items.into_iter();
    let operator = iter
        .next()
        .ok_or_else(|| syntax_error(pos, "cannot evaluate empty list"))?;
    stack.push(Frame::ApplyOperator {
        arg_exprs: iter.collect(),
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval {
        expr: operator,
        env,
    })
}

fn start_define_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    match args {
        [signature, body @ ..] if signature.list_items().is_some() => {
            let (name, params) = parse_define_signature(signature)?;
            let lambda = make_lambda(Some(name.clone()), params, body, &env, pos)?;
            env_define(&env, name, lambda);
            Ok(MachineState::Value(Value::Void))
        }
        [name_expr, value_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(pos, "invalid define form"))?
                .to_string();
            stack.push(Frame::DefineValue {
                name,
                env: Rc::clone(&env),
            });
            Ok(MachineState::Eval {
                expr: value_expr.clone(),
                env,
            })
        }
        _ => Err(syntax_error(pos, "invalid define form")),
    }
}

fn start_set_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    match args {
        [name_expr, value_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "set! target must be a symbol"))?
                .to_string();
            stack.push(Frame::SetValue {
                name,
                env: Rc::clone(&env),
                pos: name_expr.pos,
            });
            Ok(MachineState::Eval {
                expr: value_expr.clone(),
                env,
            })
        }
        _ => Err(wrong_arg_count(
            pos,
            "set!",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn start_if_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    match args {
        [condition, when_true] => {
            stack.push(Frame::If {
                when_true: when_true.clone(),
                when_false: None,
                env: Rc::clone(&env),
            });
            Ok(MachineState::Eval {
                expr: condition.clone(),
                env,
            })
        }
        [condition, when_true, when_false] => {
            stack.push(Frame::If {
                when_true: when_true.clone(),
                when_false: Some(when_false.clone()),
                env: Rc::clone(&env),
            });
            Ok(MachineState::Eval {
                expr: condition.clone(),
                env,
            })
        }
        _ => Err(wrong_arg_count(pos, "if", "2 or 3 arguments", args.len())),
    }
}

fn start_and_state(
    args: &[Expr],
    env: EnvRef,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Ok(MachineState::Value(Value::Boolean(true)));
    };

    if !rest.is_empty() {
        stack.push(Frame::And {
            remaining: rest.to_vec(),
            env: Rc::clone(&env),
        });
    }

    Ok(MachineState::Eval {
        expr: first.clone(),
        env,
    })
}

fn start_or_state(
    args: &[Expr],
    env: EnvRef,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Ok(MachineState::Value(Value::Boolean(false)));
    };

    if !rest.is_empty() {
        stack.push(Frame::Or {
            remaining: rest.to_vec(),
            env: Rc::clone(&env),
        });
    }

    Ok(MachineState::Eval {
        expr: first.clone(),
        env,
    })
}

fn start_cond_state(
    clauses: &[Expr],
    env: EnvRef,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    start_cond_owned(clauses.to_vec(), env, stack)
}

fn start_cond_owned(
    clauses: Vec<Expr>,
    env: EnvRef,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let Some((clause, remaining)) = clauses.split_first() else {
        return Ok(MachineState::Value(Value::Void));
    };

    let items = clause
        .list_items()
        .ok_or_else(|| syntax_error(clause.pos, "cond clauses must be lists"))?;
    let (test, body) = items
        .split_first()
        .ok_or_else(|| syntax_error(clause.pos, "cond clause cannot be empty"))?;

    if test.symbol_name() == Some("else") {
        if !remaining.is_empty() {
            return Err(syntax_error(test.pos, "else clause must be last"));
        }

        return if body.is_empty() {
            Ok(MachineState::Value(Value::Void))
        } else {
            start_sequence_state(body, env, clause.pos, stack)
        };
    }

    stack.push(Frame::Cond {
        body: body.to_vec(),
        remaining_clauses: remaining.to_vec(),
        env: Rc::clone(&env),
        clause_pos: clause.pos,
    });
    Ok(MachineState::Eval {
        expr: test.clone(),
        env,
    })
}

fn start_guard_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let (spec, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "guard requires a variable and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "guard requires a body"));
    }

    let spec_items = spec
        .list_items()
        .ok_or_else(|| syntax_error(spec.pos, "guard variable and clauses must be a list"))?;
    let (variable_expr, clauses) = spec_items
        .split_first()
        .ok_or_else(|| syntax_error(spec.pos, "guard requires a variable"))?;
    let variable = variable_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(variable_expr.pos, "guard variable must be a symbol"))?
        .to_string();

    stack.push(Frame::Guard {
        variable,
        clauses: clauses.to_vec(),
        env: Rc::clone(&env),
        winds: context.active_winds.clone(),
    });
    start_sequence_state(body, env, pos, stack)
}

fn start_guard_clauses_owned(
    exception: Value,
    clauses: Vec<Expr>,
    env: EnvRef,
    raise_pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let Some((clause, remaining)) = clauses.split_first() else {
        return start_raise_state(exception, raise_pos, stack, context);
    };

    let items = clause
        .list_items()
        .ok_or_else(|| syntax_error(clause.pos, "guard clauses must be lists"))?;
    let (test, body) = items
        .split_first()
        .ok_or_else(|| syntax_error(clause.pos, "guard clause cannot be empty"))?;

    if test.symbol_name() == Some("else") {
        if !remaining.is_empty() {
            return Err(syntax_error(test.pos, "else clause must be last"));
        }

        return if body.is_empty() {
            Ok(MachineState::Value(Value::Void))
        } else {
            start_sequence_state(body, env, clause.pos, stack)
        };
    }

    stack.push(Frame::GuardClause {
        exception,
        body: body.to_vec(),
        remaining_clauses: remaining.to_vec(),
        env: Rc::clone(&env),
        clause_pos: clause.pos,
        raise_pos,
    });
    Ok(MachineState::Eval {
        expr: test.clone(),
        env,
    })
}

fn start_case_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    let (key_expr, clauses) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "case requires a key and at least one clause"))?;

    if clauses.is_empty() {
        return Err(syntax_error(pos, "case requires at least one clause"));
    }

    stack.push(Frame::Case {
        clauses: clauses.to_vec(),
        env: Rc::clone(&env),
    });
    Ok(MachineState::Eval {
        expr: key_expr.clone(),
        env,
    })
}

fn finish_case(
    key: Value,
    clauses: Vec<Expr>,
    env: EnvRef,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
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
                Ok(MachineState::Value(Value::Void))
            } else {
                start_sequence_state(body, env, clause.pos, stack)
            };
        }

        let datums = head
            .list_items()
            .ok_or_else(|| syntax_error(head.pos, "case datums must be a list"))?;

        for datum in datums {
            if eqv_value(&key, &quote_expr(datum)?) {
                return if body.is_empty() {
                    Ok(MachineState::Value(Value::Void))
                } else {
                    start_sequence_state(body, env, clause.pos, stack)
                };
            }
        }
    }

    Ok(MachineState::Value(Value::Void))
}

fn start_let_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let (head, rest) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "let requires bindings and a body"))?;

    if let Some(bindings) = head.list_items() {
        start_plain_let_state(bindings, rest, env, pos, stack)
    } else if let Some(name) = head.symbol_name() {
        start_named_let_state(name, rest, env, head.pos, stack, context)
    } else {
        Err(syntax_error(head.pos, "invalid let form"))
    }
}

fn start_plain_let_state(
    bindings: &[Expr],
    body: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "let requires a body"));
    }

    let bindings = bindings
        .iter()
        .map(|binding| parse_value_binding(binding, "let"))
        .collect::<Result<Vec<_>, _>>()?;

    if bindings.is_empty() {
        return start_sequence_state(body, Env::new_child(&env), pos, stack);
    }

    let mut iter = bindings.into_iter();
    let (current_name, current_expr) = iter.next().expect("checked for empty bindings");
    stack.push(Frame::Let {
        current_name,
        remaining_bindings: iter.collect(),
        evaluated_bindings: Vec::new(),
        body: body.to_vec(),
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval {
        expr: current_expr,
        env,
    })
}

fn start_named_let_state(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "named let requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "named let requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "let bindings must be a list"))?;
    let bindings = bindings
        .iter()
        .map(|binding| parse_value_binding(binding, "let"))
        .collect::<Result<Vec<_>, _>>()?;

    let (params, init_exprs): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let let_env = Env::new_child(&env);
    let lambda = make_lambda(
        Some(name.to_string()),
        LambdaParams::fixed(params),
        body,
        &let_env,
        pos,
    )?;
    env_define(&let_env, name.to_string(), lambda.clone());

    if init_exprs.is_empty() {
        return apply_machine(lambda, Vec::new(), pos, stack, context);
    }

    let mut iter = init_exprs.into_iter();
    let first = iter.next().expect("checked for empty named let args");
    stack.push(Frame::NamedLet {
        lambda,
        remaining_inits: iter.collect(),
        evaluated_args: Vec::new(),
        env: Rc::clone(&env),
        pos,
    });
    Ok(MachineState::Eval { expr: first, env })
}

fn start_letrec_state(
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    sequential: bool,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, EvalError> {
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

    let letrec_env = Env::new_child(&env);
    for (name, _) in &bindings {
        env_define(&letrec_env, name.clone(), Value::Uninitialized);
    }

    if bindings.is_empty() {
        return start_sequence_state(body, letrec_env, pos, stack);
    }

    let (names, value_exprs): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let first_expr = value_exprs[0].clone();
    stack.push(Frame::Letrec {
        current_index: 0,
        names,
        value_exprs,
        letrec_env: Rc::clone(&letrec_env),
        values: Vec::new(),
        sequential,
        body: body.to_vec(),
        pos,
    });
    Ok(MachineState::Eval {
        expr: first_expr,
        env: letrec_env,
    })
}

fn apply_machine(
    operator: Value,
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    match operator {
        Value::Procedure(Procedure::Builtin("call/cc")) => apply_call_cc(args, pos, stack, context),
        Value::Procedure(Procedure::Builtin("call-with-values")) => {
            apply_call_with_values(args, pos, stack, context)
        }
        Value::Procedure(Procedure::Builtin("dynamic-wind")) => {
            apply_dynamic_wind(args, pos, stack, context)
        }
        Value::Procedure(Procedure::Builtin("raise")) => apply_raise(args, pos, stack, context),
        Value::Procedure(Procedure::Builtin("with-exception-handler")) => {
            apply_with_exception_handler(args, pos, stack, context)
        }
        Value::Procedure(Procedure::Builtin(name)) => {
            builtins::apply_builtin(name, &args, pos, context).map(MachineState::Value)
        }
        Value::Procedure(Procedure::Lambda(lambda)) => {
            let call_env = bind_lambda_call(&lambda, &args, pos)?;
            start_sequence_state(&lambda.body, call_env, pos, stack)
        }
        Value::Procedure(Procedure::CaseLambda(case_lambda)) => {
            let lambda = select_case_lambda_clause(&case_lambda, args.len(), pos)?;
            let call_env = bind_lambda_call(&lambda, &args, pos)?;
            start_sequence_state(&lambda.body, call_env, pos, stack)
        }
        Value::Procedure(Procedure::Continuation(continuation)) => {
            apply_continuation(continuation, args, pos, stack, context)
        }
        Value::Procedure(Procedure::RecordConstructor(record_type)) => {
            apply_record_constructor(record_type, &args, pos).map(MachineState::Value)
        }
        Value::Procedure(Procedure::RecordPredicate(record_type)) => {
            apply_record_predicate(record_type, &args, pos).map(MachineState::Value)
        }
        Value::Procedure(Procedure::RecordAccessor {
            record_type,
            field_index,
            name,
        }) => apply_record_accessor(record_type, field_index, &name, &args, pos)
            .map(MachineState::Value),
        other => Err(EvalError::NotAProcedure {
            pos,
            found: other.render(),
        }),
    }
}

fn apply_call_cc(
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let [procedure] = args.as_slice() else {
        return Err(wrong_arg_count(
            pos,
            "call/cc",
            "exactly 1 argument",
            args.len(),
        ));
    };

    let continuation = Value::Procedure(Procedure::Continuation(Rc::new(Continuation {
        frames: stack.clone(),
        winds: context.active_winds.clone(),
    })));

    apply_machine(procedure.clone(), vec![continuation], pos, stack, context)
}

fn apply_dynamic_wind(
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let [in_thunk, body_thunk, out_thunk] = args.as_slice() else {
        return Err(wrong_arg_count(
            pos,
            "dynamic-wind",
            "exactly 3 arguments",
            args.len(),
        ));
    };

    let wind = Rc::new(DynamicWind {
        in_thunk: in_thunk.clone(),
        out_thunk: out_thunk.clone(),
    });
    stack.push(Frame::DynamicWindAfterIn {
        wind,
        body_thunk: body_thunk.clone(),
        pos,
    });
    apply_machine(in_thunk.clone(), Vec::new(), pos, stack, context)
}

fn apply_call_with_values(
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let [producer, consumer] = args.as_slice() else {
        return Err(wrong_arg_count(
            pos,
            "call-with-values",
            "exactly 2 arguments",
            args.len(),
        ));
    };

    stack.push(Frame::CallWithValues {
        consumer: consumer.clone(),
        pos,
    });
    apply_machine(producer.clone(), Vec::new(), pos, stack, context)
}

fn apply_with_exception_handler(
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let [handler, thunk] = args.as_slice() else {
        return Err(wrong_arg_count(
            pos,
            "with-exception-handler",
            "exactly 2 arguments",
            args.len(),
        ));
    };

    stack.push(Frame::ExceptionHandler {
        handler: handler.clone(),
        winds: context.active_winds.clone(),
    });
    apply_machine(thunk.clone(), Vec::new(), pos, stack, context)
}

fn apply_raise(
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let [exception] = args.as_slice() else {
        return Err(wrong_arg_count(
            pos,
            "raise",
            "exactly 1 argument",
            args.len(),
        ));
    };

    start_raise_state(exception.clone(), pos, stack, context)
}

fn apply_continuation(
    continuation: Rc<Continuation>,
    args: Vec<Value>,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let [value] = args.as_slice() else {
        return Err(wrong_arg_count(
            pos,
            "continuation",
            "exactly 1 argument",
            args.len(),
        ));
    };

    let shared_len = shared_wind_prefix_len(&context.active_winds, &continuation.winds);
    let exiting = context.active_winds[shared_len..].to_vec();
    let entering = continuation.winds[shared_len..]
        .iter()
        .cloned()
        .rev()
        .collect::<Vec<_>>();

    continue_wind_transition(
        WindTransition {
            exiting,
            entering,
            target_frames: continuation.frames.clone(),
            target_winds: continuation.winds.clone(),
            value: value.clone(),
            pos,
        },
        stack,
        context,
    )
}

fn start_raise_state(
    exception: Value,
    pos: SourcePos,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    let Some(target) = find_exception_handler(stack) else {
        return Err(EvalError::UncaughtException {
            pos,
            value: exception.render(),
        });
    };

    let (mut target_frames, target_winds) = match target {
        ExceptionHandlerTarget::Handler {
            handler,
            frames,
            winds,
        } => {
            let mut frames = frames;
            frames.push(Frame::RaiseHandlerReturned { pos });
            frames.push(Frame::RaiseInvokeHandler { handler, pos });
            (frames, winds)
        }
        ExceptionHandlerTarget::Guard {
            variable,
            clauses,
            env,
            frames,
            winds,
        } => {
            let mut frames = frames;
            frames.push(Frame::GuardHandle {
                variable,
                clauses,
                env,
                raise_pos: pos,
            });
            (frames, winds)
        }
    };

    let shared_len = shared_wind_prefix_len(&context.active_winds, &target_winds);
    let exiting = context.active_winds[shared_len..].to_vec();
    let entering = target_winds[shared_len..]
        .iter()
        .cloned()
        .rev()
        .collect::<Vec<_>>();

    continue_wind_transition(
        WindTransition {
            exiting,
            entering,
            target_frames: std::mem::take(&mut target_frames),
            target_winds,
            value: exception,
            pos,
        },
        stack,
        context,
    )
}

fn find_exception_handler(stack: &[Frame]) -> Option<ExceptionHandlerTarget> {
    for (index, frame) in stack.iter().enumerate().rev() {
        match frame {
            Frame::ExceptionHandler { handler, winds } => {
                return Some(ExceptionHandlerTarget::Handler {
                    handler: handler.clone(),
                    frames: stack[..index].to_vec(),
                    winds: winds.clone(),
                });
            }
            Frame::Guard {
                variable,
                clauses,
                env,
                winds,
            } => {
                return Some(ExceptionHandlerTarget::Guard {
                    variable: variable.clone(),
                    clauses: clauses.clone(),
                    env: Rc::clone(env),
                    frames: stack[..index].to_vec(),
                    winds: winds.clone(),
                });
            }
            _ => {}
        }
    }

    None
}

fn continue_wind_transition(
    mut transition: WindTransition,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> Result<MachineState, EvalError> {
    if let Some(wind) = transition.exiting.pop() {
        pop_active_wind(&mut context.active_winds, &wind);
        let pos = transition.pos;
        stack.push(Frame::WindTransitionAfterOut { transition });
        return apply_machine(wind.out_thunk.clone(), Vec::new(), pos, stack, context);
    }

    if let Some(wind) = transition.entering.pop() {
        let pos = transition.pos;
        stack.push(Frame::WindTransitionAfterIn {
            wind: Rc::clone(&wind),
            transition,
        });
        return apply_machine(wind.in_thunk.clone(), Vec::new(), pos, stack, context);
    }

    context.active_winds = transition.target_winds;
    *stack = transition.target_frames;
    Ok(MachineState::Value(transition.value))
}

fn shared_wind_prefix_len(current: &[WindRef], target: &[WindRef]) -> usize {
    current
        .iter()
        .zip(target.iter())
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count()
}

fn pop_active_wind(active_winds: &mut Vec<WindRef>, expected: &WindRef) {
    let current = active_winds
        .pop()
        .expect("dynamic-wind stack must not be empty");
    assert!(
        Rc::ptr_eq(&current, expected),
        "dynamic-wind stack out of sync"
    );
}

fn expand_let_star(args: &[Expr], pos: SourcePos) -> Result<Expr, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "let* requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "let* requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "let* bindings must be a list"))?;

    expand_let_star_bindings(bindings, body, pos)
}

fn expand_let_star_bindings(
    bindings: &[Expr],
    body: &[Expr],
    pos: SourcePos,
) -> Result<Expr, EvalError> {
    let Some((first, rest)) = bindings.split_first() else {
        return Ok(begin_expr(pos, body.to_vec()));
    };

    let tail_expr = if rest.is_empty() {
        begin_expr(pos, body.to_vec())
    } else {
        expand_let_star_bindings(rest, body, pos)?
    };

    Ok(list_expr(
        pos,
        vec![
            symbol_expr(pos, "let"),
            list_expr(first.pos, vec![first.clone()]),
            tail_expr,
        ],
    ))
}

fn expand_do(args: &[Expr], pos: SourcePos) -> Result<Expr, EvalError> {
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

    let named_bindings = bindings
        .iter()
        .map(|(name, init, _)| {
            list_expr(
                pos,
                vec![symbol_expr(bindings_expr.pos, name), init.clone()],
            )
        })
        .collect::<Vec<_>>();
    let step_args = bindings
        .iter()
        .map(|(name, _, step)| {
            step.clone()
                .unwrap_or_else(|| symbol_expr(bindings_expr.pos, name))
        })
        .collect::<Vec<_>>();

    let recursive_call = list_expr(
        pos,
        std::iter::once(symbol_expr(pos, "loop"))
            .chain(step_args)
            .collect(),
    );
    let false_branch = {
        let mut exprs = body.to_vec();
        exprs.push(recursive_call);
        begin_expr(pos, exprs)
    };
    let true_branch = if exit_exprs.is_empty() {
        void_expr(pos)
    } else {
        begin_expr(pos, exit_exprs.to_vec())
    };

    Ok(list_expr(
        pos,
        vec![
            symbol_expr(pos, "let"),
            symbol_expr(pos, "loop"),
            list_expr(bindings_expr.pos, named_bindings),
            if_expr(pos, test_expr.clone(), true_branch, Some(false_branch)),
        ],
    ))
}

fn symbol_expr(pos: SourcePos, name: &str) -> Expr {
    Expr::new(pos, ExprKind::Symbol(name.to_string()))
}

fn boolean_expr(pos: SourcePos, value: bool) -> Expr {
    Expr::new(pos, ExprKind::Boolean(value))
}

fn list_expr(pos: SourcePos, items: Vec<Expr>) -> Expr {
    Expr::new(pos, ExprKind::List(items))
}

fn if_expr(pos: SourcePos, condition: Expr, when_true: Expr, when_false: Option<Expr>) -> Expr {
    let mut items = vec![symbol_expr(pos, "if"), condition, when_true];
    if let Some(when_false) = when_false {
        items.push(when_false);
    }
    list_expr(pos, items)
}

fn void_expr(pos: SourcePos) -> Expr {
    if_expr(
        pos,
        boolean_expr(pos, false),
        boolean_expr(pos, false),
        None,
    )
}

fn begin_expr(pos: SourcePos, exprs: Vec<Expr>) -> Expr {
    match exprs.len() {
        0 => void_expr(pos),
        1 => exprs.into_iter().next().expect("single expression"),
        _ => list_expr(
            pos,
            std::iter::once(symbol_expr(pos, "begin"))
                .chain(exprs)
                .collect(),
        ),
    }
}
