use super::super::text::SchemeString;
use super::super::{
    eval_program_tail, list_from_values, EnvRef, Environment, EvalError, EvaluatedArg, Expr,
    LambdaParams, Position, Procedure, TailEvalResult, Value,
};
use super::{
    apply_record_accessor, apply_record_constructor, apply_record_mutator, apply_record_predicate,
};
use std::rc::Rc;

pub(crate) fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool { value, .. } => Value::Bool(*value),
        Expr::Number { value, .. } => Value::Number(*value),
        Expr::Char { value, .. } => Value::Char(*value),
        Expr::String { value, .. } => Value::String(SchemeString::immutable(value)),
        Expr::Symbol { name, .. } => Value::Symbol(name.clone()),
        Expr::List { items, .. } => list_from_values(items.iter().map(quote_expr)),
    }
}

pub(crate) fn apply_procedure(
    value: Value,
    args: &[EvaluatedArg],
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut current_value = value;
    let mut current_args = args.to_vec();
    let mut call_pos = None;

    loop {
        let procedure = match current_value {
            Value::Procedure(ref procedure) => procedure.clone(),
            _ => {
                return Err(attach_call_position(
                    EvalError::NotAProcedure {
                        found: current_value.render(),
                    },
                    call_pos,
                ));
            }
        };

        match procedure.as_ref() {
            Procedure::Builtin { func, .. } => {
                return with_call_position(func(&current_args, output), call_pos);
            }
            Procedure::Raise { name } => {
                let [value_arg] = current_args.as_slice() else {
                    return with_call_position(
                        Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 1",
                            got: current_args.len(),
                        }),
                        call_pos,
                    );
                };

                return with_call_position(
                    Err(EvalError::UncaughtException {
                        value: value_arg.value.render(),
                    }),
                    call_pos,
                );
            }
            Procedure::WithExceptionHandler { name } => {
                let [_, thunk_arg] = current_args.as_slice() else {
                    return with_call_position(
                        Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 2",
                            got: current_args.len(),
                        }),
                        call_pos,
                    );
                };

                return with_call_position(
                    apply_procedure(thunk_arg.value.clone(), &[], output),
                    call_pos,
                );
            }
            Procedure::ContinuationCapture { name } => {
                let [procedure_arg] = current_args.as_slice() else {
                    return with_call_position(
                        Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 1",
                            got: current_args.len(),
                        }),
                        call_pos,
                    );
                };

                current_value = procedure_arg.value.clone();
                current_args = vec![EvaluatedArg {
                    value: Value::Procedure(Rc::new(Procedure::Continuation { cont: None })),
                    pos: procedure_arg.pos,
                }];
            }
            Procedure::DynamicWind { name } => {
                let [before_arg, body_arg, after_arg] = current_args.as_slice() else {
                    return with_call_position(
                        Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 3",
                            got: current_args.len(),
                        }),
                        call_pos,
                    );
                };

                with_call_position(
                    apply_procedure(before_arg.value.clone(), &[], output),
                    call_pos,
                )?;
                let body_value = with_call_position(
                    apply_procedure(body_arg.value.clone(), &[], output),
                    call_pos,
                )?;
                with_call_position(
                    apply_procedure(after_arg.value.clone(), &[], output),
                    call_pos,
                )?;
                return Ok(body_value);
            }
            Procedure::Continuation { .. } => {
                let [value_arg] = current_args.as_slice() else {
                    return with_call_position(
                        Err(EvalError::WrongArgCount {
                            name: "continuation",
                            expected: "exactly 1",
                            got: current_args.len(),
                        }),
                        call_pos,
                    );
                };

                return Ok(value_arg.value.clone());
            }
            Procedure::Lambda { params, body, env } => {
                let call_env = with_call_position(
                    prepare_lambda_call_env("lambda", params, env, &current_args),
                    call_pos,
                )?;
                match eval_program_tail(body, call_env, output)? {
                    TailEvalResult::Value(value) => return Ok(value),
                    TailEvalResult::Call {
                        procedure,
                        args,
                        pos,
                    } => {
                        current_value = procedure;
                        current_args = args;
                        call_pos = Some(pos);
                    }
                }
            }
            Procedure::CaseLambda { clauses, env } => {
                let clause = with_call_position(
                    clauses
                        .iter()
                        .find(|clause| clause.params.matches_arity(current_args.len()))
                        .ok_or(EvalError::WrongArgCount {
                            name: "case-lambda",
                            expected: "matching clause",
                            got: current_args.len(),
                        }),
                    call_pos,
                )?;

                let call_env = with_call_position(
                    prepare_lambda_call_env("case-lambda", &clause.params, env, &current_args),
                    call_pos,
                )?;

                match eval_program_tail(&clause.body, call_env, output)? {
                    TailEvalResult::Value(value) => return Ok(value),
                    TailEvalResult::Call {
                        procedure,
                        args,
                        pos,
                    } => {
                        current_value = procedure;
                        current_args = args;
                        call_pos = Some(pos);
                    }
                }
            }
            Procedure::GuardHandler { .. } => {
                return with_call_position(
                    Err(EvalError::ParseError {
                        message: "guard handlers require the machine evaluator".to_string(),
                    }),
                    call_pos,
                );
            }
            Procedure::RecordConstructor {
                record_type,
                field_count,
                ..
            } => {
                return with_call_position(
                    apply_record_constructor(record_type.clone(), *field_count, &current_args),
                    call_pos,
                );
            }
            Procedure::RecordPredicate { record_type, .. } => {
                return with_call_position(
                    apply_record_predicate(record_type, &current_args),
                    call_pos,
                );
            }
            Procedure::RecordAccessor {
                record_type,
                field_index,
                ..
            } => {
                return with_call_position(
                    apply_record_accessor(record_type, *field_index, &current_args),
                    call_pos,
                );
            }
            Procedure::RecordMutator {
                record_type,
                field_index,
                ..
            } => {
                return with_call_position(
                    apply_record_mutator(record_type, *field_index, &current_args),
                    call_pos,
                );
            }
        }
    }
}

fn with_call_position<T>(
    result: Result<T, EvalError>,
    pos: Option<Position>,
) -> Result<T, EvalError> {
    match pos {
        Some(pos) => result.map_err(|error| error.with_position(pos.line, pos.col)),
        None => result,
    }
}

fn attach_call_position(error: EvalError, pos: Option<Position>) -> EvalError {
    match pos {
        Some(pos) => error.with_position(pos.line, pos.col),
        None => error,
    }
}

fn prepare_lambda_call_env(
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

pub(super) fn exact_int(value: i64) -> Value {
    Value::Number(super::super::number::Number::exact_int(value))
}
