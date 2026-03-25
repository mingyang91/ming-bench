use std::cell::RefCell;
use std::rc::Rc;

use super::builtins::default_env;
use super::core::{
    list_from_vec, list_to_vec, make_case_lambda, make_lambda, make_record, make_record_accessor,
    make_record_constructor, make_record_predicate, make_record_type, quote_expr, BindingRef,
    CaseLambdaProcedure, DynamicWinder, EnvRef, Environment, Expr, ExprsRef, LambdaProcedure,
    Position, Procedure, RecordAccessorProcedure, RecordConstructorProcedure,
    RecordPredicateProcedure, Runtime, Value, WinderRef,
};
use super::error::EvalError;
use super::macros::{expand_macro_call, parse_macro_definition};

struct ParsedParams {
    params: Vec<String>,
    rest_param: Option<String>,
}

struct RecordConstructorSpec {
    name: String,
    field_names: Vec<String>,
}

struct RecordFieldSpec {
    field_name: String,
    accessor_name: String,
}

struct DoBindingSpec {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

type ExceptionHandlerRef = Rc<dyn std::any::Any>;

#[derive(Clone, Copy)]
enum SpecialForm {
    Define,
    DefineRecordType,
    DefineSyntax,
    Set,
    If,
    Quote,
    Lambda,
    CaseLambda,
    And,
    Or,
    Let,
    LetStar,
    Letrec,
    LetrecStar,
    Begin,
    Cond,
    Case,
    Do,
    Guard,
}

impl SpecialForm {
    fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "define" => Some(Self::Define),
            "define-record-type" => Some(Self::DefineRecordType),
            "define-syntax" => Some(Self::DefineSyntax),
            "set!" => Some(Self::Set),
            "if" => Some(Self::If),
            "quote" => Some(Self::Quote),
            "lambda" => Some(Self::Lambda),
            "case-lambda" => Some(Self::CaseLambda),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "let" => Some(Self::Let),
            "let*" => Some(Self::LetStar),
            "letrec" => Some(Self::Letrec),
            "letrec*" => Some(Self::LetrecStar),
            "begin" => Some(Self::Begin),
            "cond" => Some(Self::Cond),
            "case" => Some(Self::Case),
            "do" => Some(Self::Do),
            "guard" => Some(Self::Guard),
            _ => None,
        }
    }
}

#[derive(Clone)]
struct CapturedContinuation {
    frames: Vec<MachineFrame>,
    winders: Vec<WinderRef>,
    handlers: Vec<ExceptionHandlerRef>,
}

#[derive(Clone)]
struct CapturedExceptionHandler {
    procedure: Value,
    winders: Vec<WinderRef>,
}

#[derive(Clone)]
struct ProducedValues {
    values: Vec<Value>,
}

impl ProducedValues {
    fn single(value: Value) -> Self {
        Self {
            values: vec![value],
        }
    }

    fn into_single(self) -> Result<Value, EvalError> {
        match self.values.len() {
            1 => Ok(self
                .values
                .into_iter()
                .next()
                .expect("single produced value")),
            actual => Err(wrong_value_count("exactly 1", actual)),
        }
    }

    fn into_vec(self) -> Vec<Value> {
        self.values
    }
}

#[derive(Clone)]
struct MapIteration {
    operator: Value,
    lists: Rc<Vec<Vec<Value>>>,
    index: usize,
    results: Vec<Value>,
    for_each: bool,
    pos: Position,
}

#[derive(Clone)]
struct WindTransition {
    exits: Vec<WinderRef>,
    entries: Vec<WinderRef>,
    target_frames: Vec<MachineFrame>,
    target_winders: Vec<WinderRef>,
    target_handlers: Vec<ExceptionHandlerRef>,
    resume: WindResume,
    pos: Option<Position>,
}

#[derive(Clone)]
enum WindResume {
    Values(ProducedValues),
    Apply {
        operator: Value,
        args: Vec<Value>,
        pos: Option<Position>,
    },
}

#[derive(Clone)]
enum MachineFrame {
    Sequence {
        expressions: ExprsRef,
        index: usize,
        env: EnvRef,
    },
    Define {
        name: String,
        env: EnvRef,
    },
    Set {
        name: String,
        env: EnvRef,
        pos: Position,
    },
    If {
        consequent: Rc<Expr>,
        alternate: Option<Rc<Expr>>,
        env: EnvRef,
    },
    ApplyOperator {
        args: ExprsRef,
        env: EnvRef,
        pos: Position,
    },
    ApplyArgument {
        operator: Value,
        args: ExprsRef,
        index: usize,
        values: Vec<Value>,
        env: EnvRef,
        pos: Position,
    },
    LetrecParallel {
        bindings: Rc<Vec<(String, Expr)>>,
        body: ExprsRef,
        local_env: EnvRef,
        cells: Vec<BindingRef>,
        index: usize,
        values: Vec<Value>,
    },
    LetrecSequential {
        bindings: Rc<Vec<(String, Expr)>>,
        body: ExprsRef,
        local_env: EnvRef,
        cells: Vec<BindingRef>,
        index: usize,
    },
    MapContinue(MapIteration),
    DynamicWindEnter {
        winder: WinderRef,
        body: Value,
        pos: Option<Position>,
    },
    DynamicWindExit {
        winder: WinderRef,
        result: Option<ProducedValues>,
        pos: Option<Position>,
    },
    ExceptionHandlerExit {
        handler: ExceptionHandlerRef,
    },
    CallWithValues {
        consumer: Value,
        pos: Option<Position>,
    },
    WindTransition {
        transition: WindTransition,
        activate_winder: Option<WinderRef>,
    },
}

enum MachineControl {
    Expr(Rc<Expr>, EnvRef),
    Values(ProducedValues),
}

fn machine_eval_program(
    expressions: &[Expr],
    runtime: &mut Runtime,
) -> Result<ProducedValues, EvalError> {
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    machine_eval_sequence(expressions, &env, runtime)
}

fn machine_eval_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<ProducedValues, EvalError> {
    let mut frames = Vec::new();
    let control = start_sequence_control(expressions.to_vec().into(), env.clone(), &mut frames);
    run_machine(control, frames, runtime)
}

fn machine_apply_procedure(
    operator: Value,
    args: &[Value],
    runtime: &mut Runtime,
) -> Result<ProducedValues, EvalError> {
    let mut frames = Vec::new();
    let control = apply_machine_value(operator, args.to_vec(), runtime, &mut frames, None)?;
    run_machine(control, frames, runtime)
}

fn run_machine(
    mut control: MachineControl,
    mut frames: Vec<MachineFrame>,
    runtime: &mut Runtime,
) -> Result<ProducedValues, EvalError> {
    loop {
        control = match control {
            MachineControl::Expr(expr, env) => eval_machine_expr(expr, env, runtime, &mut frames)?,
            MachineControl::Values(values) => match frames.pop() {
                Some(frame) => resume_machine_frame(frame, values, runtime, &mut frames)?,
                None => return Ok(values),
            },
        };
    }
}

fn start_sequence_control(
    expressions: ExprsRef,
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> MachineControl {
    continue_sequence_control(expressions, 0, env, frames)
}

fn continue_sequence_control(
    expressions: ExprsRef,
    index: usize,
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> MachineControl {
    if index >= expressions.len() {
        return MachineControl::Values(ProducedValues::single(Value::Void));
    }

    if index + 1 < expressions.len() {
        frames.push(MachineFrame::Sequence {
            expressions: expressions.clone(),
            index: index + 1,
            env: env.clone(),
        });
    }

    MachineControl::Expr(Rc::new(expressions[index].clone()), env)
}

fn eval_machine_expr(
    expr: Rc<Expr>,
    env: EnvRef,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    match expr.as_ref() {
        Expr::Bool(value, _) => Ok(MachineControl::Values(ProducedValues::single(Value::Bool(
            *value,
        )))),
        Expr::Number(value, _) => Ok(MachineControl::Values(ProducedValues::single(
            Value::Number(*value),
        ))),
        Expr::String(value, _) => Ok(MachineControl::Values(ProducedValues::single(
            super::core::make_immutable_string(value.clone()),
        ))),
        Expr::Char(value, _) => Ok(MachineControl::Values(ProducedValues::single(Value::Char(
            *value,
        )))),
        Expr::Symbol(name, pos) => match Environment::lookup(&env, name) {
            Some(Value::Uninitialized) => {
                Err(pos.attach(EvalError::UninitializedBinding { name: name.clone() }))
            }
            Some(value) => Ok(MachineControl::Values(ProducedValues::single(value))),
            None => Err(pos.attach(EvalError::UnboundSymbol { name: name.clone() })),
        },
        Expr::Vector(_, _) => Ok(MachineControl::Values(ProducedValues::single(quote_expr(
            expr.as_ref(),
        )))),
        Expr::List(items, pos) => {
            eval_machine_list(items, *pos, &env, runtime, frames).map_err(|error| pos.attach(error))
        }
    }
}

fn eval_machine_list(
    items: &[Expr],
    pos: Position,
    env: &EnvRef,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    let (head, args) = items.split_first().ok_or_else(empty_list_error)?;

    if let Expr::Symbol(name, _) = head {
        if let Some(special_form) = SpecialForm::from_symbol(name) {
            return eval_machine_special_form(special_form, args, pos, env, runtime, frames);
        }

        if let Some(transformer) = runtime.lookup_macro(name) {
            let (expanded, expansion_env) = expand_macro_call(&transformer, items, env, runtime)?;
            return Ok(MachineControl::Expr(Rc::new(expanded), expansion_env));
        }
    }

    let arg_exprs: ExprsRef = args.to_vec().into();
    frames.push(MachineFrame::ApplyOperator {
        args: arg_exprs,
        env: env.clone(),
        pos,
    });
    Ok(MachineControl::Expr(Rc::new(head.clone()), env.clone()))
}

fn eval_machine_special_form(
    special_form: SpecialForm,
    args: &[Expr],
    pos: Position,
    env: &EnvRef,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    match special_form {
        SpecialForm::Define => eval_machine_define(args, env, runtime, frames),
        SpecialForm::DefineRecordType => eval_define_record_type(args, env)
            .map(|value| MachineControl::Values(ProducedValues::single(value))),
        SpecialForm::DefineSyntax => eval_define_syntax(args, env, runtime)
            .map(|value| MachineControl::Values(ProducedValues::single(value))),
        SpecialForm::Set => eval_machine_set(args, env, runtime, frames),
        SpecialForm::If => eval_machine_if(args, env, frames),
        SpecialForm::Quote => {
            eval_quote(args).map(|value| MachineControl::Values(ProducedValues::single(value)))
        }
        SpecialForm::Lambda => eval_lambda(args, env)
            .map(|value| MachineControl::Values(ProducedValues::single(value))),
        SpecialForm::CaseLambda => eval_case_lambda(args, env)
            .map(|value| MachineControl::Values(ProducedValues::single(value))),
        SpecialForm::And => Ok(MachineControl::Expr(
            Rc::new(expand_and_form(args, pos, runtime)?),
            env.clone(),
        )),
        SpecialForm::Or => Ok(MachineControl::Expr(
            Rc::new(expand_or_form(args, pos, runtime)?),
            env.clone(),
        )),
        SpecialForm::Let => Ok(MachineControl::Expr(
            Rc::new(expand_let_form(args, pos)?),
            env.clone(),
        )),
        SpecialForm::LetStar => Ok(MachineControl::Expr(
            Rc::new(expand_let_star_form(args, pos)?),
            env.clone(),
        )),
        SpecialForm::Letrec => eval_machine_letrec(args, env, frames, false),
        SpecialForm::LetrecStar => eval_machine_letrec(args, env, frames, true),
        SpecialForm::Begin => Ok(start_sequence_control(
            args.to_vec().into(),
            env.clone(),
            frames,
        )),
        SpecialForm::Cond => Ok(MachineControl::Expr(
            Rc::new(expand_cond_form(args, pos, runtime)?),
            env.clone(),
        )),
        SpecialForm::Case => Ok(MachineControl::Expr(
            Rc::new(expand_case_form(args, pos, runtime)?),
            env.clone(),
        )),
        SpecialForm::Do => Ok(MachineControl::Expr(
            Rc::new(expand_do_form(args, pos, runtime)?),
            env.clone(),
        )),
        SpecialForm::Guard => Ok(MachineControl::Expr(
            Rc::new(expand_guard_form(args, pos)?),
            env.clone(),
        )),
    }
}

fn eval_machine_define(
    args: &[Expr],
    env: &EnvRef,
    _runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    let Some((target, rest)) = args.split_first() else {
        return Err(wrong_arg_count("define", "at least 2", 0));
    };

    match target {
        Expr::Symbol(name, _) => {
            let [value_expr] = rest else {
                return Err(wrong_arg_count("define", "exactly 2", args.len()));
            };

            frames.push(MachineFrame::Define {
                name: name.clone(),
                env: env.clone(),
            });
            Ok(MachineControl::Expr(
                Rc::new(value_expr.clone()),
                env.clone(),
            ))
        }
        Expr::List(signature, _) => {
            let value = eval_function_define(signature, rest, env, args.len())?;
            Ok(MachineControl::Values(ProducedValues::single(value)))
        }
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            target,
            "define requires a symbol or function signature",
        )),
    }
}

fn eval_machine_set(
    args: &[Expr],
    env: &EnvRef,
    _runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    let [target, value_expr] = args else {
        return Err(wrong_arg_count("set!", "exactly 2", args.len()));
    };

    let name = match target {
        Expr::Symbol(name, _) => name.clone(),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _)
        | Expr::Vector(_, _) => {
            return Err(positioned_syntax_error(
                target,
                "set! target must be a symbol",
            ));
        }
    };

    frames.push(MachineFrame::Set {
        name,
        env: env.clone(),
        pos: target.pos(),
    });
    Ok(MachineControl::Expr(
        Rc::new(value_expr.clone()),
        env.clone(),
    ))
}

fn eval_machine_if(
    args: &[Expr],
    env: &EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    let (condition, consequent, alternate) = match args {
        [condition, consequent] => (condition, consequent, None),
        [condition, consequent, alternate] => (condition, consequent, Some(alternate)),
        _ => return Err(wrong_arg_count("if", "exactly 2 or 3", args.len())),
    };

    frames.push(MachineFrame::If {
        consequent: Rc::new(consequent.clone()),
        alternate: alternate.cloned().map(Rc::new),
        env: env.clone(),
    });
    Ok(MachineControl::Expr(
        Rc::new(condition.clone()),
        env.clone(),
    ))
}

fn eval_machine_letrec(
    args: &[Expr],
    env: &EnvRef,
    frames: &mut Vec<MachineFrame>,
    sequential: bool,
) -> Result<MachineControl, EvalError> {
    let name = if sequential { "letrec*" } else { "letrec" };
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count(name, "at least 2", 0));
    };

    if body.is_empty() {
        return Err(wrong_arg_count(name, "at least 2", 1));
    }

    let bindings = Rc::new(parse_let_bindings(bindings_expr)?);
    let local_env = Environment::new(Some(env.clone()));
    let cells = create_recursive_bindings(&local_env, bindings.as_ref());
    let body_ref: ExprsRef = body.to_vec().into();

    if bindings.is_empty() {
        return Ok(start_sequence_control(body_ref, local_env, frames));
    }

    if sequential {
        frames.push(MachineFrame::LetrecSequential {
            bindings: bindings.clone(),
            body: body_ref,
            local_env: local_env.clone(),
            cells,
            index: 0,
        });
    } else {
        frames.push(MachineFrame::LetrecParallel {
            bindings: bindings.clone(),
            body: body_ref,
            local_env: local_env.clone(),
            cells,
            index: 0,
            values: Vec::with_capacity(bindings.len()),
        });
    }

    Ok(MachineControl::Expr(
        Rc::new(bindings[0].1.clone()),
        local_env,
    ))
}

fn resume_machine_frame(
    frame: MachineFrame,
    produced: ProducedValues,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    match frame {
        MachineFrame::Sequence {
            expressions,
            index,
            env,
        } => {
            produced.into_single()?;
            Ok(continue_sequence_control(expressions, index, env, frames))
        }
        MachineFrame::Define { name, env } => {
            let value = produced.into_single()?;
            Environment::define(&env, name, value);
            Ok(MachineControl::Values(ProducedValues::single(Value::Void)))
        }
        MachineFrame::Set { name, env, pos } => {
            let value = produced.into_single()?;
            if Environment::set(&env, &name, value) {
                Ok(MachineControl::Values(ProducedValues::single(Value::Void)))
            } else {
                Err(pos.attach(EvalError::UnboundSymbol { name }))
            }
        }
        MachineFrame::If {
            consequent,
            alternate,
            env,
        } => {
            let value = produced.into_single()?;
            if value.is_truthy() {
                Ok(MachineControl::Expr(consequent, env))
            } else if let Some(alternate) = alternate {
                Ok(MachineControl::Expr(alternate, env))
            } else {
                Ok(MachineControl::Values(ProducedValues::single(Value::Void)))
            }
        }
        MachineFrame::ApplyOperator { args, env, pos } => {
            let value = produced.into_single()?;
            if args.is_empty() {
                apply_machine_value(value, Vec::new(), runtime, frames, Some(pos))
            } else {
                let index = args.len() - 1;
                frames.push(MachineFrame::ApplyArgument {
                    operator: value,
                    args: args.clone(),
                    index,
                    values: Vec::with_capacity(args.len()),
                    env: env.clone(),
                    pos,
                });
                Ok(MachineControl::Expr(Rc::new(args[index].clone()), env))
            }
        }
        MachineFrame::ApplyArgument {
            operator,
            args,
            index,
            mut values,
            env,
            pos,
        } => {
            values.push(produced.into_single()?);
            if index > 0 {
                let next_index = index - 1;
                frames.push(MachineFrame::ApplyArgument {
                    operator,
                    args: args.clone(),
                    index: next_index,
                    values,
                    env: env.clone(),
                    pos,
                });
                Ok(MachineControl::Expr(Rc::new(args[next_index].clone()), env))
            } else {
                values.reverse();
                apply_machine_value(operator, values, runtime, frames, Some(pos))
            }
        }
        MachineFrame::LetrecParallel {
            bindings,
            body,
            local_env,
            cells,
            index,
            mut values,
        } => {
            values.push(produced.into_single()?);
            if index + 1 < bindings.len() {
                let next_index = index + 1;
                frames.push(MachineFrame::LetrecParallel {
                    bindings: bindings.clone(),
                    body,
                    local_env: local_env.clone(),
                    cells,
                    index: next_index,
                    values,
                });
                Ok(MachineControl::Expr(
                    Rc::new(bindings[next_index].1.clone()),
                    local_env,
                ))
            } else {
                for (cell, binding_value) in cells.iter().zip(values) {
                    *cell.borrow_mut() = binding_value;
                }
                Ok(start_sequence_control(body, local_env, frames))
            }
        }
        MachineFrame::LetrecSequential {
            bindings,
            body,
            local_env,
            cells,
            index,
        } => {
            *cells[index].borrow_mut() = produced.into_single()?;
            if index + 1 < bindings.len() {
                let next_index = index + 1;
                frames.push(MachineFrame::LetrecSequential {
                    bindings: bindings.clone(),
                    body,
                    local_env: local_env.clone(),
                    cells,
                    index: next_index,
                });
                Ok(MachineControl::Expr(
                    Rc::new(bindings[next_index].1.clone()),
                    local_env,
                ))
            } else {
                Ok(start_sequence_control(body, local_env, frames))
            }
        }
        MachineFrame::MapContinue(mut iteration) => {
            if !iteration.for_each {
                iteration.results.push(produced.into_single()?);
            }

            iteration.index += 1;
            if iteration.index
                < iteration
                    .lists
                    .iter()
                    .map(|list| list.len())
                    .min()
                    .unwrap_or(0)
            {
                apply_map_iteration(iteration, runtime, frames)
            } else if iteration.for_each {
                Ok(MachineControl::Values(ProducedValues::single(Value::Void)))
            } else {
                Ok(MachineControl::Values(ProducedValues::single(
                    list_from_vec(iteration.results),
                )))
            }
        }
        MachineFrame::DynamicWindEnter { winder, body, pos } => {
            runtime.push_winder(winder.clone());
            frames.push(MachineFrame::DynamicWindExit {
                winder,
                result: None,
                pos,
            });
            apply_machine_value(body, Vec::new(), runtime, frames, pos)
        }
        MachineFrame::DynamicWindExit {
            winder,
            result,
            pos,
        } => {
            if let Some(result) = result {
                Ok(MachineControl::Values(result))
            } else {
                pop_expected_winder(runtime, &winder);
                frames.push(MachineFrame::DynamicWindExit {
                    winder: winder.clone(),
                    result: Some(produced),
                    pos,
                });
                apply_machine_value(winder.after.clone(), Vec::new(), runtime, frames, pos)
            }
        }
        MachineFrame::ExceptionHandlerExit { handler } => {
            pop_expected_exception_handler(runtime, &handler);
            Ok(MachineControl::Values(produced))
        }
        MachineFrame::CallWithValues { consumer, pos } => {
            apply_machine_value(consumer, produced.into_vec(), runtime, frames, pos)
        }
        MachineFrame::WindTransition {
            transition,
            activate_winder,
        } => {
            if let Some(winder) = activate_winder {
                runtime.push_winder(winder);
            }
            step_wind_transition(transition, runtime, frames)
        }
    }
}

fn apply_machine_value(
    operator: Value,
    args: Vec<Value>,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let result = match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => {
                apply_machine_builtin(*builtin, args, runtime, frames, pos)
            }
            Procedure::Lambda(lambda) => {
                let call_env = prepare_lambda_call(lambda, &args)?;
                Ok(start_sequence_control(
                    lambda.body.clone(),
                    call_env,
                    frames,
                ))
            }
            Procedure::CaseLambda(case_lambda) => {
                let clause = case_lambda
                    .clauses
                    .iter()
                    .find(|clause| lambda_accepts_arity(clause, args.len()))
                    .ok_or_else(|| EvalError::WrongArgCount {
                        name: case_lambda
                            .name
                            .clone()
                            .unwrap_or_else(|| "case-lambda".into()),
                        expected: case_lambda_expected(case_lambda),
                        actual: args.len(),
                    })?;
                let call_env = prepare_lambda_call(clause, &args)?;
                Ok(start_sequence_control(
                    clause.body.clone(),
                    call_env,
                    frames,
                ))
            }
            Procedure::RecordConstructor(constructor) => {
                apply_record_constructor(constructor, &args)
                    .map(|value| MachineControl::Values(ProducedValues::single(value)))
            }
            Procedure::RecordPredicate(predicate) => apply_record_predicate(predicate, &args)
                .map(|value| MachineControl::Values(ProducedValues::single(value))),
            Procedure::RecordAccessor(accessor) => apply_record_accessor(accessor, &args)
                .map(|value| MachineControl::Values(ProducedValues::single(value))),
            Procedure::Continuation(captured) => {
                apply_captured_continuation(captured.clone(), &args, runtime, frames, pos)
            }
        },
        Value::Bool(_)
        | Value::Number(_)
        | Value::String(_)
        | Value::Symbol(_)
        | Value::Char(_)
        | Value::List(_)
        | Value::Pair(_)
        | Value::Vector(_)
        | Value::Record(_)
        | Value::Uninitialized
        | Value::Void => Err(EvalError::NotAProcedure {
            found: operator.render_for_error(),
        }),
    };

    result.map_err(|error| match pos {
        Some(pos) => pos.attach(error),
        None => error,
    })
}

fn apply_machine_builtin(
    builtin: super::core::BuiltinProcedure,
    args: Vec<Value>,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    match builtin.name {
        "dynamic-wind" => apply_dynamic_wind_builtin(&args, runtime, frames, pos),
        "error" => apply_error_builtin(&args, runtime, frames, pos),
        "raise" => apply_raise_builtin(&args, runtime, frames, pos),
        "with-exception-handler" => {
            apply_with_exception_handler_builtin(&args, runtime, frames, pos)
        }
        "call/cc" | "call-with-current-continuation" => {
            apply_call_cc_builtin(&args, runtime, frames, pos)
        }
        "values" => Ok(MachineControl::Values(ProducedValues { values: args })),
        "call-with-values" => apply_call_with_values_builtin(&args, runtime, frames, pos),
        "apply" => apply_apply_builtin(&args, runtime, frames, pos),
        "map" => apply_map_builtin(&args, runtime, frames, pos, false),
        "for-each" => apply_map_builtin(&args, runtime, frames, pos, true),
        _ => (builtin.func)(&args, runtime)
            .map(|value| MachineControl::Values(ProducedValues::single(value))),
    }
}

fn apply_dynamic_wind_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let [before, body, after] = args else {
        return Err(wrong_arg_count("dynamic-wind", "exactly 3", args.len()));
    };

    let winder = Rc::new(DynamicWinder {
        before: before.clone(),
        after: after.clone(),
    });

    frames.push(MachineFrame::DynamicWindEnter {
        winder,
        body: body.clone(),
        pos,
    });
    apply_machine_value(before.clone(), Vec::new(), runtime, frames, pos)
}

fn apply_raise_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let [value] = args else {
        return Err(wrong_arg_count("raise", "exactly 1", args.len()));
    };

    raise_machine_value(value.clone(), runtime, frames, pos)
}

fn apply_error_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    raise_machine_value(error_value(args), runtime, frames, pos)
}

fn error_value(args: &[Value]) -> Value {
    if args.is_empty() {
        Value::Symbol("error".into())
    } else {
        list_from_vec(args.to_vec())
    }
}

fn raise_machine_value(
    value: Value,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let current_handlers = runtime.exception_handlers();
    let Some(handler_ref) = current_handlers.last().cloned() else {
        return Err(EvalError::UncaughtException {
            value: value.render_for_error(),
        });
    };

    let handler = Rc::downcast::<CapturedExceptionHandler>(handler_ref.clone()).map_err(|_| {
        EvalError::SyntaxError {
            message: "internal error: invalid exception handler payload".into(),
        }
    })?;
    let target_frames = frames_before_exception_handler(frames, &handler_ref).ok_or_else(|| {
        EvalError::SyntaxError {
            message: "internal error: exception handler frame missing".into(),
        }
    })?;
    let current_winders = runtime.winders();
    let target_winders = handler.winders.clone();
    let shared_prefix = common_winder_prefix_len(&current_winders, &target_winders);
    let mut target_handlers = current_handlers;
    let _ = target_handlers.pop();

    step_wind_transition(
        WindTransition {
            exits: current_winders[shared_prefix..].to_vec(),
            entries: target_winders[shared_prefix..]
                .iter()
                .rev()
                .cloned()
                .collect(),
            target_frames,
            target_winders,
            target_handlers,
            resume: WindResume::Apply {
                operator: handler.procedure.clone(),
                args: vec![value],
                pos,
            },
            pos,
        },
        runtime,
        frames,
    )
}

fn apply_with_exception_handler_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let [handler, thunk] = args else {
        return Err(wrong_arg_count(
            "with-exception-handler",
            "exactly 2",
            args.len(),
        ));
    };

    let captured: ExceptionHandlerRef = Rc::new(CapturedExceptionHandler {
        procedure: handler.clone(),
        winders: runtime.winders(),
    });

    runtime.push_exception_handler(captured.clone());
    frames.push(MachineFrame::ExceptionHandlerExit { handler: captured });
    apply_machine_value(thunk.clone(), Vec::new(), runtime, frames, pos)
}

fn apply_call_cc_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let [procedure] = args else {
        return Err(wrong_arg_count("call/cc", "exactly 1", args.len()));
    };

    let continuation = make_continuation_value(frames, runtime);
    apply_machine_value(procedure.clone(), vec![continuation], runtime, frames, pos)
}

fn apply_call_with_values_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let [producer, consumer] = args else {
        return Err(wrong_arg_count("call-with-values", "exactly 2", args.len()));
    };

    frames.push(MachineFrame::CallWithValues {
        consumer: consumer.clone(),
        pos,
    });
    apply_machine_value(producer.clone(), Vec::new(), runtime, frames, pos)
}

fn apply_apply_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let Some((operator, arg_parts)) = args.split_first() else {
        return Err(wrong_arg_count("apply", "at least 2", 0));
    };

    let Some((list_arg, prefix_args)) = arg_parts.split_last() else {
        return Err(wrong_arg_count("apply", "at least 2", 1));
    };

    let mut applied_args = prefix_args.to_vec();
    applied_args.extend(expect_list_argument(list_arg, "list")?);
    apply_machine_value(operator.clone(), applied_args, runtime, frames, pos)
}

fn apply_map_builtin(
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
    for_each: bool,
) -> Result<MachineControl, EvalError> {
    let Some((operator, list_args)) = args.split_first() else {
        let name = if for_each { "for-each" } else { "map" };
        return Err(wrong_arg_count(name, "at least 2", 0));
    };

    if list_args.is_empty() {
        let name = if for_each { "for-each" } else { "map" };
        return Err(wrong_arg_count(name, "at least 2", 1));
    }

    let lists = Rc::new(
        list_args
            .iter()
            .map(|list| expect_list_argument(list, "list"))
            .collect::<Result<Vec<_>, _>>()?,
    );
    let len = lists.iter().map(|list| list.len()).min().unwrap_or(0);

    if len == 0 {
        return Ok(MachineControl::Values(ProducedValues::single(
            if for_each {
                Value::Void
            } else {
                list_from_vec(Vec::new())
            },
        )));
    }

    let call_pos = pos.unwrap_or(Position { line: 1, col: 1 });
    apply_map_iteration(
        MapIteration {
            operator: operator.clone(),
            lists,
            index: 0,
            results: Vec::with_capacity(len),
            for_each,
            pos: call_pos,
        },
        runtime,
        frames,
    )
}

fn apply_map_iteration(
    iteration: MapIteration,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    let call_args = iteration
        .lists
        .iter()
        .map(|list| list[iteration.index].clone())
        .collect::<Vec<_>>();
    let operator = iteration.operator.clone();
    let pos = iteration.pos;

    frames.push(MachineFrame::MapContinue(iteration));

    apply_machine_value(operator, call_args, runtime, frames, Some(pos))
}

fn make_continuation_value(frames: &[MachineFrame], runtime: &Runtime) -> Value {
    Value::Procedure(Rc::new(Procedure::Continuation(Rc::new(
        CapturedContinuation {
            frames: frames.to_vec(),
            winders: runtime.winders(),
            handlers: runtime.exception_handlers(),
        },
    ))))
}

fn apply_captured_continuation(
    captured: Rc<dyn std::any::Any>,
    args: &[Value],
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
    pos: Option<Position>,
) -> Result<MachineControl, EvalError> {
    let captured =
        Rc::downcast::<CapturedContinuation>(captured).map_err(|_| EvalError::SyntaxError {
            message: "internal error: invalid continuation payload".into(),
        })?;
    let current_winders = runtime.winders();
    let target_winders = captured.winders.clone();
    let shared_prefix = common_winder_prefix_len(&current_winders, &target_winders);

    step_wind_transition(
        WindTransition {
            exits: current_winders[shared_prefix..].to_vec(),
            entries: target_winders[shared_prefix..]
                .iter()
                .rev()
                .cloned()
                .collect(),
            target_frames: captured.frames.clone(),
            target_winders,
            target_handlers: captured.handlers.clone(),
            resume: WindResume::Values(ProducedValues {
                values: args.to_vec(),
            }),
            pos,
        },
        runtime,
        frames,
    )
}

fn common_winder_prefix_len(current: &[WinderRef], target: &[WinderRef]) -> usize {
    current
        .iter()
        .zip(target.iter())
        .take_while(|(current, target)| Rc::ptr_eq(current, target))
        .count()
}

fn pop_expected_winder(runtime: &mut Runtime, expected: &WinderRef) {
    let popped = runtime.pop_winder();
    debug_assert!(
        popped
            .as_ref()
            .map(|actual| Rc::ptr_eq(actual, expected))
            .unwrap_or(false),
        "dynamic-wind stack out of sync"
    );
}

fn pop_expected_exception_handler(runtime: &mut Runtime, expected: &ExceptionHandlerRef) {
    let popped = runtime.pop_exception_handler();
    debug_assert!(
        popped
            .as_ref()
            .map(|actual| Rc::ptr_eq(actual, expected))
            .unwrap_or(false),
        "exception handler stack out of sync"
    );
}

fn frames_before_exception_handler(
    frames: &[MachineFrame],
    target: &ExceptionHandlerRef,
) -> Option<Vec<MachineFrame>> {
    frames
        .iter()
        .rposition(|frame| {
            matches!(
                frame,
                MachineFrame::ExceptionHandlerExit { handler } if Rc::ptr_eq(handler, target)
            )
        })
        .map(|index| frames[..index].to_vec())
}

fn step_wind_transition(
    mut transition: WindTransition,
    runtime: &mut Runtime,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineControl, EvalError> {
    if let Some(winder) = transition.exits.pop() {
        let pos = transition.pos;
        pop_expected_winder(runtime, &winder);
        frames.push(MachineFrame::WindTransition {
            transition,
            activate_winder: None,
        });
        return apply_machine_value(winder.after.clone(), Vec::new(), runtime, frames, pos);
    }

    if let Some(winder) = transition.entries.pop() {
        let pos = transition.pos;
        frames.push(MachineFrame::WindTransition {
            transition,
            activate_winder: Some(winder.clone()),
        });
        return apply_machine_value(winder.before.clone(), Vec::new(), runtime, frames, pos);
    }

    runtime.replace_winders(transition.target_winders);
    runtime.replace_exception_handlers(transition.target_handlers);
    *frames = transition.target_frames;
    match transition.resume {
        WindResume::Values(values) => Ok(MachineControl::Values(values)),
        WindResume::Apply {
            operator,
            args,
            pos,
        } => apply_machine_value(operator, args, runtime, frames, pos),
    }
}

fn expect_list_argument(value: &Value, expected: &str) -> Result<Vec<Value>, EvalError> {
    list_to_vec(value).ok_or_else(|| EvalError::TypeMismatch {
        expected: expected.into(),
        found: value.type_name().into(),
    })
}

fn expand_and_form(args: &[Expr], pos: Position, runtime: &mut Runtime) -> Result<Expr, EvalError> {
    match args {
        [] => Ok(Expr::Bool(true, pos)),
        [expr] => Ok(expr.clone()),
        [first, rest @ ..] => {
            let temp = runtime.fresh_symbol("and");
            let temp_expr = symbol_expr(&temp, pos);
            Ok(build_single_binding_let(
                &temp,
                first.clone(),
                build_if_expr(
                    temp_expr.clone(),
                    expand_and_form(rest, pos, runtime)?,
                    temp_expr,
                    pos,
                ),
                pos,
            ))
        }
    }
}

fn expand_or_form(args: &[Expr], pos: Position, runtime: &mut Runtime) -> Result<Expr, EvalError> {
    match args {
        [] => Ok(Expr::Bool(false, pos)),
        [expr] => Ok(expr.clone()),
        [first, rest @ ..] => {
            let temp = runtime.fresh_symbol("or");
            let temp_expr = symbol_expr(&temp, pos);
            Ok(build_single_binding_let(
                &temp,
                first.clone(),
                build_if_expr(
                    temp_expr.clone(),
                    temp_expr,
                    expand_or_form(rest, pos, runtime)?,
                    pos,
                ),
                pos,
            ))
        }
    }
}

fn expand_let_form(args: &[Expr], pos: Position) -> Result<Expr, EvalError> {
    let Some((head, tail)) = args.split_first() else {
        return Err(wrong_arg_count("let", "at least 2", 0));
    };

    match head {
        Expr::Symbol(name, _) => {
            let Some((bindings_expr, body)) = tail.split_first() else {
                return Err(wrong_arg_count("let", "at least 3", args.len()));
            };

            if body.is_empty() {
                return Err(wrong_arg_count("let", "at least 3", args.len()));
            }

            let bindings = parse_let_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(binding_name, _)| binding_name.clone())
                .collect::<Vec<_>>();
            let lambda_expr = build_lambda_expr(&params, body, pos);
            let binding_expr = build_binding_expr(name, lambda_expr, pos);
            let call_expr = build_application_expr(
                symbol_expr(name, pos),
                bindings
                    .iter()
                    .map(|(_, value_expr)| value_expr.clone())
                    .collect::<Vec<_>>(),
                pos,
            );
            Ok(Expr::List(
                vec![
                    symbol_expr("letrec", pos),
                    Expr::List(vec![binding_expr], pos),
                    call_expr,
                ],
                pos,
            ))
        }
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _)
        | Expr::Vector(_, _) => {
            let bindings = parse_let_bindings(head)?;
            if tail.is_empty() {
                return Err(wrong_arg_count("let", "at least 2", 1));
            }
            Ok(build_plain_let_expr(&bindings, tail, pos))
        }
    }
}

fn expand_let_star_form(args: &[Expr], pos: Position) -> Result<Expr, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("let*", "at least 2", 0));
    };

    if body.is_empty() {
        return Err(wrong_arg_count("let*", "at least 2", 1));
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    Ok(build_let_star_expr(&bindings, body, pos))
}

fn expand_cond_form(
    clauses: &[Expr],
    pos: Position,
    runtime: &mut Runtime,
) -> Result<Expr, EvalError> {
    expand_cond_clauses(clauses, pos, runtime)
}

fn expand_cond_clauses(
    clauses: &[Expr],
    pos: Position,
    runtime: &mut Runtime,
) -> Result<Expr, EvalError> {
    let Some((clause, rest)) = clauses.split_first() else {
        return Ok(build_begin_expr(&[], pos));
    };

    match parse_cond_clause(clause)? {
        CondClause::Else { body } => expand_else_cond_clause(clause, body, rest),
        CondClause::TestOnly { test } => {
            let rest_expr = expand_cond_clauses(rest, pos, runtime)?;
            Ok(expand_test_only_cond_clause(
                test,
                rest_expr,
                clause.pos(),
                runtime,
            ))
        }
        CondClause::Arrow { test, receiver } => {
            let rest_expr = expand_cond_clauses(rest, pos, runtime)?;
            Ok(expand_arrow_cond_clause(
                test,
                receiver,
                rest_expr,
                clause.pos(),
                runtime,
            ))
        }
        CondClause::Body { test, body } => {
            let rest_expr = expand_cond_clauses(rest, pos, runtime)?;
            Ok(build_if_expr(
                test.clone(),
                build_begin_expr(body, clause.pos()),
                rest_expr,
                clause.pos(),
            ))
        }
    }
}

fn expand_else_cond_clause(clause: &Expr, body: &[Expr], rest: &[Expr]) -> Result<Expr, EvalError> {
    if !rest.is_empty() {
        return Err(positioned_syntax_error(
            clause,
            "cond else clause must be last",
        ));
    }

    Ok(if body.is_empty() {
        Expr::Bool(true, clause.pos())
    } else {
        build_begin_expr(body, clause.pos())
    })
}

fn expand_test_only_cond_clause(
    test: &Expr,
    rest_expr: Expr,
    pos: Position,
    runtime: &mut Runtime,
) -> Expr {
    let temp = runtime.fresh_symbol("cond");
    let temp_expr = symbol_expr(&temp, pos);
    build_single_binding_let(
        &temp,
        test.clone(),
        build_if_expr(temp_expr.clone(), temp_expr, rest_expr, pos),
        pos,
    )
}

fn expand_arrow_cond_clause(
    test: &Expr,
    receiver: &Expr,
    rest_expr: Expr,
    pos: Position,
    runtime: &mut Runtime,
) -> Expr {
    let temp = runtime.fresh_symbol("cond");
    let temp_expr = symbol_expr(&temp, pos);
    build_single_binding_let(
        &temp,
        test.clone(),
        build_if_expr(
            temp_expr.clone(),
            build_application_expr(receiver.clone(), vec![temp_expr], pos),
            rest_expr,
            pos,
        ),
        pos,
    )
}

fn expand_case_form(
    args: &[Expr],
    pos: Position,
    runtime: &mut Runtime,
) -> Result<Expr, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(wrong_arg_count("case", "at least 2", 0));
    };

    if clauses.is_empty() {
        return Err(wrong_arg_count("case", "at least 2", 1));
    }

    let key_name = runtime.fresh_symbol("case_key");
    let key_ref = symbol_expr(&key_name, pos);
    let mut cond_items = vec![symbol_expr("cond", pos)];

    for (index, clause) in clauses.iter().enumerate() {
        let parts = case_clause_parts(clause)?;
        if is_else_clause(parts) {
            validate_case_else_clause(clause, parts, index, clauses.len())?;
            cond_items.push(Expr::List(parts.to_vec(), clause.pos()));
            continue;
        }

        ensure_case_clause_has_body(clause, parts, "case clause must have a body")?;
        let datums = case_clause_datums(&parts[0])?;
        let comparisons = datums
            .iter()
            .map(|datum| {
                build_application_expr(
                    symbol_expr("eqv?", clause.pos()),
                    vec![
                        key_ref.clone(),
                        build_quote_form_expr(datum.clone(), datum.pos()),
                    ],
                    clause.pos(),
                )
            })
            .collect::<Vec<_>>();
        let test_expr = if comparisons.len() == 1 {
            comparisons.into_iter().next().expect("single comparison")
        } else {
            let mut items = vec![symbol_expr("or", clause.pos())];
            items.extend(comparisons);
            Expr::List(items, clause.pos())
        };
        let mut clause_items = Vec::with_capacity(parts.len());
        clause_items.push(test_expr);
        clause_items.extend(parts[1..].iter().cloned());
        cond_items.push(Expr::List(clause_items, clause.pos()));
    }

    Ok(build_single_binding_let(
        &key_name,
        key_expr.clone(),
        Expr::List(cond_items, pos),
        pos,
    ))
}

fn expand_do_form(args: &[Expr], pos: Position, runtime: &mut Runtime) -> Result<Expr, EvalError> {
    let Some((bindings_expr, tail)) = args.split_first() else {
        return Err(wrong_arg_count("do", "at least 2", 0));
    };

    let Some((test_clause, body)) = tail.split_first() else {
        return Err(wrong_arg_count("do", "at least 2", 1));
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let test_parts = do_termination_clause_parts(test_clause)?;
    let (test_expr, result_exprs) = test_parts
        .split_first()
        .expect("do termination clause must contain a test expression");
    let loop_name = runtime.fresh_symbol("do_loop");

    let binding_exprs = bindings
        .iter()
        .map(|binding| build_binding_expr(&binding.name, binding.init.clone(), pos))
        .collect::<Vec<_>>();
    let loop_call = build_application_expr(
        symbol_expr(&loop_name, pos),
        bindings
            .iter()
            .map(|binding| {
                binding
                    .step
                    .clone()
                    .unwrap_or_else(|| symbol_expr(&binding.name, pos))
            })
            .collect::<Vec<_>>(),
        pos,
    );

    let mut alternate_body = body.to_vec();
    alternate_body.push(loop_call);

    Ok(Expr::List(
        vec![
            symbol_expr("let", pos),
            symbol_expr(&loop_name, pos),
            Expr::List(binding_exprs, pos),
            build_if_expr(
                test_expr.clone(),
                build_begin_expr(result_exprs, pos),
                build_begin_expr(&alternate_body, pos),
                pos,
            ),
        ],
        pos,
    ))
}

fn expand_guard_form(args: &[Expr], pos: Position) -> Result<Expr, EvalError> {
    let Some((spec, body)) = args.split_first() else {
        return Err(wrong_arg_count("guard", "at least 2", 0));
    };

    if body.is_empty() {
        return Err(wrong_arg_count("guard", "at least 2", 1));
    }

    let (name, clauses) = parse_guard_spec(spec)?;
    let mut cond_items = Vec::with_capacity(clauses.len() + 2);
    cond_items.push(symbol_expr("cond", pos));
    cond_items.extend(clauses.iter().cloned());

    if !guard_has_else_clause(clauses)? {
        cond_items.push(Expr::List(
            vec![
                symbol_expr("else", pos),
                build_application_expr(
                    symbol_expr("raise", pos),
                    vec![symbol_expr(&name, pos)],
                    pos,
                ),
            ],
            pos,
        ));
    }

    let handler_body = Expr::List(cond_items, pos);
    let handler = build_lambda_expr(std::slice::from_ref(&name), &[handler_body], pos);
    let thunk = build_lambda_expr(&[], body, pos);

    Ok(build_application_expr(
        symbol_expr("with-exception-handler", pos),
        vec![handler, thunk],
        pos,
    ))
}

fn build_plain_let_expr(bindings: &[(String, Expr)], body: &[Expr], pos: Position) -> Expr {
    let lambda_expr = build_lambda_expr(
        &bindings
            .iter()
            .map(|(binding_name, _)| binding_name.clone())
            .collect::<Vec<_>>(),
        body,
        pos,
    );
    build_application_expr(
        lambda_expr,
        bindings
            .iter()
            .map(|(_, value_expr)| value_expr.clone())
            .collect::<Vec<_>>(),
        pos,
    )
}

fn build_let_star_expr(bindings: &[(String, Expr)], body: &[Expr], pos: Position) -> Expr {
    if let Some((first, rest)) = bindings.split_first() {
        Expr::List(
            vec![
                symbol_expr("let", pos),
                Expr::List(
                    vec![build_binding_expr(&first.0, first.1.clone(), pos)],
                    pos,
                ),
                build_let_star_expr(rest, body, pos),
            ],
            pos,
        )
    } else {
        Expr::List(
            {
                let mut items = Vec::with_capacity(body.len() + 2);
                items.push(symbol_expr("let", pos));
                items.push(Expr::List(Vec::new(), pos));
                items.extend(body.iter().cloned());
                items
            },
            pos,
        )
    }
}

fn build_lambda_expr(params: &[String], body: &[Expr], pos: Position) -> Expr {
    let mut items = Vec::with_capacity(body.len() + 2);
    items.push(symbol_expr("lambda", pos));
    items.push(Expr::List(
        params
            .iter()
            .map(|param| symbol_expr(param, pos))
            .collect::<Vec<_>>(),
        pos,
    ));
    items.extend(body.iter().cloned());
    Expr::List(items, pos)
}

fn build_single_binding_let(name: &str, value_expr: Expr, body_expr: Expr, pos: Position) -> Expr {
    Expr::List(
        vec![
            symbol_expr("let", pos),
            Expr::List(vec![build_binding_expr(name, value_expr, pos)], pos),
            body_expr,
        ],
        pos,
    )
}

fn build_binding_expr(name: &str, value_expr: Expr, pos: Position) -> Expr {
    Expr::List(vec![symbol_expr(name, pos), value_expr], pos)
}

fn build_application_expr(operator: Expr, args: Vec<Expr>, pos: Position) -> Expr {
    let mut items = Vec::with_capacity(args.len() + 1);
    items.push(operator);
    items.extend(args);
    Expr::List(items, pos)
}

fn build_if_expr(condition: Expr, consequent: Expr, alternate: Expr, pos: Position) -> Expr {
    Expr::List(
        vec![symbol_expr("if", pos), condition, consequent, alternate],
        pos,
    )
}

fn build_begin_expr(body: &[Expr], pos: Position) -> Expr {
    if let [single] = body {
        single.clone()
    } else {
        let mut items = Vec::with_capacity(body.len() + 1);
        items.push(symbol_expr("begin", pos));
        items.extend(body.iter().cloned());
        Expr::List(items, pos)
    }
}

fn build_quote_form_expr(datum: Expr, pos: Position) -> Expr {
    Expr::List(vec![symbol_expr("quote", pos), datum], pos)
}

fn symbol_expr(name: &str, pos: Position) -> Expr {
    Expr::Symbol(name.into(), pos)
}

pub(crate) fn eval_program(
    expressions: &[Expr],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    machine_eval_program(expressions, runtime).and_then(ProducedValues::into_single)
}

fn eval_define_record_type(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = args else {
        return Err(wrong_arg_count(
            "define-record-type",
            "at least 3",
            args.len(),
        ));
    };

    let type_name = expect_symbol_expr(type_name_expr, "record type name")?;
    let constructor = parse_record_constructor_spec(constructor_expr)?;
    let predicate_name = expect_symbol_expr(predicate_expr, "record predicate")?;
    let fields = field_exprs
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;

    if constructor.field_names.len() != fields.len() {
        return Err(positioned_syntax_error(
            constructor_expr,
            "record constructor fields must match accessor fields",
        ));
    }

    if constructor
        .field_names
        .iter()
        .zip(fields.iter())
        .any(|(constructor_field, field)| constructor_field != &field.field_name)
    {
        return Err(positioned_syntax_error(
            constructor_expr,
            "record constructor fields must match accessor fields",
        ));
    }

    let record_type = make_record_type(type_name, constructor.field_names.clone());
    Environment::define(
        env,
        constructor.name.clone(),
        make_record_constructor(constructor.name.clone(), &record_type),
    );
    Environment::define(
        env,
        predicate_name.clone(),
        make_record_predicate(predicate_name, &record_type),
    );

    for (field_index, field) in fields.iter().enumerate() {
        Environment::define(
            env,
            field.accessor_name.clone(),
            make_record_accessor(field.accessor_name.clone(), &record_type, field_index),
        );
    }

    Ok(Value::Void)
}

fn eval_define_syntax(
    args: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    let [target, transformer_expr] = args else {
        return Err(wrong_arg_count("define-syntax", "exactly 2", args.len()));
    };

    let name = expect_symbol_expr(target, "define-syntax name")?;
    let transformer = parse_macro_definition(&name, transformer_expr, env, runtime)?;
    runtime.define_macro(name, transformer);
    Ok(Value::Void)
}

fn eval_function_define(
    signature: &[Expr],
    rest: &[Expr],
    env: &EnvRef,
    actual: usize,
) -> Result<Value, EvalError> {
    let Some((name_expr, params_exprs)) = signature.split_first() else {
        return Err(syntax_error("define requires a function name"));
    };

    if rest.is_empty() {
        return Err(wrong_arg_count("define", "at least 2", actual));
    }

    let name = expect_symbol_expr(name_expr, "define function name")?;
    let params = parse_params(params_exprs)?;
    let lambda = make_lambda(
        Some(name.clone()),
        params.params,
        params.rest_param,
        rest.to_vec(),
        env,
    );
    Environment::define(env, name, lambda);
    Ok(Value::Void)
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(wrong_arg_count("quote", "exactly 1", args.len()));
    };

    Ok(quote_expr(expr))
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("lambda", "at least 2", 0));
    };

    if body.is_empty() {
        return Err(wrong_arg_count("lambda", "at least 2", 1));
    }

    let params = parse_formals(params_expr)?;

    Ok(make_lambda(
        None,
        params.params,
        params.rest_param,
        body.to_vec(),
        env,
    ))
}

fn eval_case_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(wrong_arg_count("case-lambda", "at least 1", 0));
    }

    let clauses = args
        .iter()
        .map(|clause| parse_case_lambda_clause(clause, env))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(make_case_lambda(None, clauses))
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => {
            return Err(positioned_syntax_error(
                bindings_expr,
                "let bindings must be a list",
            ));
        }
    };

    bindings
        .iter()
        .map(parse_let_binding)
        .collect::<Result<Vec<_>, _>>()
}

fn parse_let_binding(binding: &Expr) -> Result<(String, Expr), EvalError> {
    match binding {
        Expr::List(parts, _) if parts.len() == 2 => Ok((
            expect_symbol_expr(&parts[0], "let binding name")?,
            parts[1].clone(),
        )),
        Expr::List(_, _) => Err(positioned_syntax_error(
            binding,
            "let bindings must contain exactly a name and value",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            binding,
            "let binding must be a list",
        )),
    }
}

fn create_recursive_bindings(env: &EnvRef, bindings: &[(String, Expr)]) -> Vec<BindingRef> {
    bindings
        .iter()
        .map(|(binding_name, _)| {
            let cell = Rc::new(RefCell::new(Value::Uninitialized));
            Environment::define_cell(env, binding_name.clone(), cell.clone());
            cell
        })
        .collect()
}

fn parse_do_bindings(bindings_expr: &Expr) -> Result<Vec<DoBindingSpec>, EvalError> {
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => {
            return Err(positioned_syntax_error(
                bindings_expr,
                "do bindings must be a list",
            ));
        }
    };

    bindings
        .iter()
        .map(parse_do_binding)
        .collect::<Result<Vec<_>, _>>()
}

fn parse_do_binding(binding: &Expr) -> Result<DoBindingSpec, EvalError> {
    match binding {
        Expr::List(parts, _) if (2..=3).contains(&parts.len()) => Ok(DoBindingSpec {
            name: expect_symbol_expr(&parts[0], "do binding name")?,
            init: parts[1].clone(),
            step: parts.get(2).cloned(),
        }),
        Expr::List(_, _) => Err(positioned_syntax_error(
            binding,
            "do bindings must contain a name, init, and optional step",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            binding,
            "do binding must be a list",
        )),
    }
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<RecordConstructorSpec, EvalError> {
    let parts = match expr {
        Expr::List(parts, _) if !parts.is_empty() => parts,
        Expr::List(_, _) => {
            return Err(positioned_syntax_error(
                expr,
                "record constructor must include a name",
            ));
        }
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => {
            return Err(positioned_syntax_error(
                expr,
                "record constructor must be a list",
            ));
        }
    };

    let name = expect_symbol_expr(&parts[0], "record constructor name")?;
    let field_names = parts[1..]
        .iter()
        .map(|field| expect_symbol_expr(field, "record constructor field"))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(RecordConstructorSpec { name, field_names })
}

fn parse_record_field_spec(expr: &Expr) -> Result<RecordFieldSpec, EvalError> {
    match expr {
        Expr::List(parts, _) if parts.len() == 2 => Ok(RecordFieldSpec {
            field_name: expect_symbol_expr(&parts[0], "record field name")?,
            accessor_name: expect_symbol_expr(&parts[1], "record accessor name")?,
        }),
        Expr::List(_, _) => Err(positioned_syntax_error(
            expr,
            "record field must contain exactly a field name and accessor name",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(expr, "record field must be a list")),
    }
}

fn validate_case_else_clause(
    clause: &Expr,
    parts: &[Expr],
    index: usize,
    clause_count: usize,
) -> Result<(), EvalError> {
    if index + 1 != clause_count {
        return Err(positioned_syntax_error(
            clause,
            "case else clause must be last",
        ));
    }

    ensure_case_clause_has_body(clause, parts, "case else clause must have a body")
}

fn ensure_case_clause_has_body(
    clause: &Expr,
    parts: &[Expr],
    message: &str,
) -> Result<(), EvalError> {
    if parts.len() > 1 {
        return Ok(());
    }

    Err(positioned_syntax_error(clause, message))
}

fn cond_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "cond clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            clause,
            "cond clause must be a list",
        )),
    }
}

enum CondClause<'a> {
    Else { body: &'a [Expr] },
    TestOnly { test: &'a Expr },
    Arrow { test: &'a Expr, receiver: &'a Expr },
    Body { test: &'a Expr, body: &'a [Expr] },
}

fn parse_cond_clause(clause: &Expr) -> Result<CondClause<'_>, EvalError> {
    let parts = cond_clause_parts(clause)?;
    match parts {
        [Expr::Symbol(symbol, _)] if symbol == "else" => Ok(CondClause::Else { body: &[] }),
        [Expr::Symbol(symbol, _), body @ ..] if symbol == "else" => Ok(CondClause::Else { body }),
        [test] => Ok(CondClause::TestOnly { test }),
        [_, Expr::Symbol(symbol, _)] if symbol == "=>" => Err(positioned_syntax_error(
            clause,
            "cond => clause must have a recipient",
        )),
        [test, Expr::Symbol(symbol, _), receiver] if symbol == "=>" => {
            Ok(CondClause::Arrow { test, receiver })
        }
        [_, Expr::Symbol(symbol, _), _, ..] if symbol == "=>" => Err(positioned_syntax_error(
            clause,
            "cond => clause must have exactly one recipient",
        )),
        [test, body @ ..] => Ok(CondClause::Body { test, body }),
        [] => Err(positioned_syntax_error(
            clause,
            "cond clause cannot be empty",
        )),
    }
}

fn case_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "case clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            clause,
            "case clause must be a list",
        )),
    }
}

fn case_clause_datums(expr: &Expr) -> Result<&[Expr], EvalError> {
    match expr {
        Expr::List(datums, _) if !datums.is_empty() => Ok(datums),
        Expr::List(_, _) => Err(positioned_syntax_error(
            expr,
            "case clause must include at least one datum",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            expr,
            "case clause datums must be a list",
        )),
    }
}

fn do_termination_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "do termination clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            clause,
            "do termination clause must be a list",
        )),
    }
}

fn parse_guard_spec(spec: &Expr) -> Result<(String, &[Expr]), EvalError> {
    match spec {
        Expr::List(parts, _) if !parts.is_empty() => Ok((
            expect_symbol_expr(&parts[0], "guard variable")?,
            &parts[1..],
        )),
        Expr::List(_, _) => Err(positioned_syntax_error(
            spec,
            "guard requires a variable name",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(spec, "guard spec must be a list")),
    }
}

fn is_else_clause(parts: &[Expr]) -> bool {
    matches!(&parts[0], Expr::Symbol(symbol, _) if symbol == "else")
}

fn guard_has_else_clause(clauses: &[Expr]) -> Result<bool, EvalError> {
    match clauses.last() {
        Some(clause) => Ok(is_else_clause(cond_clause_parts(clause)?)),
        None => Ok(false),
    }
}

fn expect_symbol_expr(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name, _) => Ok(name.clone()),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            expr,
            format!("{context} must be a symbol"),
        )),
    }
}

fn parse_formals(params_expr: &Expr) -> Result<ParsedParams, EvalError> {
    match params_expr {
        Expr::List(params, _) => parse_params(params),
        Expr::Symbol(name, _) if name != "." => Ok(ParsedParams {
            params: Vec::new(),
            rest_param: Some(name.clone()),
        }),
        Expr::Symbol(_, _)
        | Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Vector(_, _) => Err(positioned_syntax_error(
            params_expr,
            "lambda parameters must be a list or symbol",
        )),
    }
}

fn parse_case_lambda_clause(clause: &Expr, env: &EnvRef) -> Result<LambdaProcedure, EvalError> {
    let parts = match clause {
        Expr::List(parts, _) => parts,
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _)
        | Expr::Vector(_, _) => {
            return Err(positioned_syntax_error(
                clause,
                "case-lambda clause must be a list",
            ));
        }
    };

    let Some((params_expr, body)) = parts.split_first() else {
        return Err(positioned_syntax_error(
            clause,
            "case-lambda clause cannot be empty",
        ));
    };

    if body.is_empty() {
        return Err(positioned_syntax_error(
            clause,
            "case-lambda clause must have a body",
        ));
    }

    let params = parse_formals(params_expr)?;
    Ok(LambdaProcedure {
        name: None,
        params: params.params,
        rest_param: params.rest_param,
        body: body.to_vec().into(),
        env: env.clone(),
    })
}

fn parse_params(params: &[Expr]) -> Result<ParsedParams, EvalError> {
    let mut parsed = ParsedParams {
        params: Vec::new(),
        rest_param: None,
    };
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(symbol, _) if symbol == "." => {
                parsed.rest_param = Some(parse_rest_param(params, index)?);
                return Ok(parsed);
            }
            param => parsed.params.push(expect_param_name(param)?),
        }

        index += 1;
    }

    Ok(parsed)
}

fn parse_rest_param(params: &[Expr], index: usize) -> Result<String, EvalError> {
    let dot = &params[index];
    let Some(rest_expr) = params.get(index + 1) else {
        return Err(positioned_syntax_error(
            dot,
            "parameter list is missing a rest parameter name",
        ));
    };

    if index + 2 != params.len() {
        return Err(positioned_syntax_error(
            dot,
            "parameter list allows only one rest parameter",
        ));
    }

    expect_param_name(rest_expr)
}

fn expect_param_name(expr: &Expr) -> Result<String, EvalError> {
    let name = expect_symbol_expr(expr, "parameter")?;
    if name == "." {
        Err(positioned_syntax_error(
            expr,
            "parameter name cannot be '.'",
        ))
    } else {
        Ok(name)
    }
}

pub(crate) fn apply_procedure(
    operator: Value,
    args: &[Value],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    machine_apply_procedure(operator, args, runtime).and_then(ProducedValues::into_single)
}

fn prepare_lambda_call(lambda: &LambdaProcedure, args: &[Value]) -> Result<EnvRef, EvalError> {
    let required_len = lambda.params.len();
    let rest_param = lambda.rest_param.as_ref();

    if !lambda_accepts_arity(lambda, args.len()) {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            expected: lambda_expected_arity(lambda),
            actual: args.len(),
        });
    }

    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda.params.iter().zip(args.iter()) {
        Environment::define(&call_env, param.clone(), value.clone());
    }

    if let Some(rest_param) = rest_param {
        Environment::define(
            &call_env,
            rest_param.clone(),
            list_from_vec(args[required_len..].to_vec()),
        );
    }

    Ok(call_env)
}

fn lambda_accepts_arity(lambda: &LambdaProcedure, actual: usize) -> bool {
    actual >= lambda.params.len() && (lambda.rest_param.is_some() || actual == lambda.params.len())
}

fn lambda_expected_arity(lambda: &LambdaProcedure) -> String {
    if lambda.rest_param.is_some() {
        format!("at least {}", lambda.params.len())
    } else {
        format!("exactly {}", lambda.params.len())
    }
}

fn case_lambda_expected(case_lambda: &CaseLambdaProcedure) -> String {
    let mut expected = Vec::with_capacity(case_lambda.clauses.len());

    for clause in &case_lambda.clauses {
        let signature = lambda_expected_arity(clause);
        if !expected.contains(&signature) {
            expected.push(signature);
        }
    }

    match expected.len() {
        0 => "no matching clause".into(),
        1 => expected.pop().expect("single expected clause"),
        _ => format!("one of {}", expected.join(", ")),
    }
}

fn apply_record_constructor(
    constructor: &RecordConstructorProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    let field_count = constructor.record_type.as_ref().field_names.len();
    if args.len() != field_count {
        return Err(EvalError::WrongArgCount {
            name: constructor.name.clone(),
            expected: format!("exactly {field_count}"),
            actual: args.len(),
        });
    }

    Ok(make_record(&constructor.record_type, args.to_vec()))
}

fn apply_record_predicate(
    predicate: &RecordPredicateProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: predicate.name.clone(),
            expected: "exactly 1".into(),
            actual: args.len(),
        });
    };

    Ok(Value::Bool(matches!(
        value,
        Value::Record(record)
            if Rc::ptr_eq(&record.as_ref().record_type, &predicate.record_type)
    )))
}

fn apply_record_accessor(
    accessor: &RecordAccessorProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: accessor.name.clone(),
            expected: "exactly 1".into(),
            actual: args.len(),
        });
    };

    match value {
        Value::Record(record)
            if Rc::ptr_eq(&record.as_ref().record_type, &accessor.record_type) =>
        {
            Ok(record.as_ref().fields[accessor.field_index].clone())
        }
        _ => Err(EvalError::TypeMismatch {
            expected: format!("{} record", accessor.record_type.name),
            found: record_found_type(value),
        }),
    }
}

fn record_found_type(value: &Value) -> String {
    match value {
        Value::Record(record) => record.as_ref().record_type.name.clone(),
        _ => value.type_name().into(),
    }
}

fn empty_list_error() -> EvalError {
    syntax_error("cannot evaluate an empty list")
}

fn syntax_error(message: impl Into<String>) -> EvalError {
    EvalError::SyntaxError {
        message: message.into(),
    }
}

fn positioned_syntax_error(expr: &Expr, message: impl Into<String>) -> EvalError {
    expr.pos().attach(syntax_error(message))
}

fn wrong_arg_count(name: &str, expected: impl Into<String>, actual: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        actual,
    }
}

fn wrong_value_count(expected: impl Into<String>, actual: usize) -> EvalError {
    EvalError::WrongValueCount {
        expected: expected.into(),
        actual,
    }
}
