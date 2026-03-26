use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use super::builtins::{apply_builtin, error_exception_value};
use super::error::{EvalError, SourcePos};
use super::evaluator::wrong_arg_count;
use super::model::{
    list_from_values, Builtin, ContinuationJumpData, ContinuationProc, Env, EnvRef, Expr,
    Procedure, RuntimeError, RuntimeResult, Value,
};
use super::records::apply_record_procedure;
use super::step_limit::StepBudgetRef;
use super::{eval_cps, eval_sequence_cps};

type Continuation = ContinuationProc;
type WindFrameRef = Rc<WindFrame>;
type ExceptionHandlerFrameRef = Rc<ExceptionHandlerFrame>;

#[derive(Clone)]
struct WindFrame {
    in_thunk: Value,
    out_thunk: Value,
}

#[derive(Clone)]
struct ExceptionHandlerFrame {
    handler: Value,
    return_k: Continuation,
    winders: Vec<WindFrameRef>,
}

pub(super) type CpsRuntimeRef = Rc<CpsRuntime>;

pub(super) struct CpsRuntime {
    winders: RefCell<Vec<WindFrameRef>>,
    handlers: RefCell<Vec<ExceptionHandlerFrameRef>>,
    steps: StepBudgetRef,
}

impl CpsRuntime {
    pub(super) fn new(steps: StepBudgetRef) -> CpsRuntimeRef {
        Rc::new(Self {
            winders: RefCell::new(Vec::new()),
            handlers: RefCell::new(Vec::new()),
            steps,
        })
    }

    pub(super) fn reset_winders(&self) {
        self.winders.borrow_mut().clear();
    }

    pub(super) fn reset_handlers(&self) {
        self.handlers.borrow_mut().clear();
    }

    pub(super) fn step(&self, position: SourcePos) -> Result<(), EvalError> {
        self.steps.step(position)
    }

    pub(super) fn steps(&self) -> &StepBudgetRef {
        &self.steps
    }

    fn current_winders(&self) -> Vec<WindFrameRef> {
        self.winders.borrow().clone()
    }

    fn push_winder(&self, frame: WindFrameRef) {
        self.winders.borrow_mut().push(frame);
    }

    fn pop_winder(&self, frame: &WindFrameRef) {
        let mut winders = self.winders.borrow_mut();
        let current = winders.pop().expect("dynamic-wind stack is not empty");
        debug_assert!(Rc::ptr_eq(&current, frame));
    }

    fn push_handler(&self, frame: ExceptionHandlerFrameRef) {
        self.handlers.borrow_mut().push(frame);
    }

    fn pop_handler(&self) -> Option<ExceptionHandlerFrameRef> {
        self.handlers.borrow_mut().pop()
    }

    fn pop_handler_if_current(&self, frame: &ExceptionHandlerFrameRef) {
        let mut handlers = self.handlers.borrow_mut();
        if handlers
            .last()
            .is_some_and(|current| Rc::ptr_eq(current, frame))
        {
            handlers.pop();
        }
    }
}

pub(super) fn bounce_continuation(
    continuation: Continuation,
    value: Value,
) -> RuntimeResult {
    Err(RuntimeError::ContinuationJump {
        jump: ContinuationJumpData::trampoline(continuation, value),
    })
}

pub(super) fn bounce_eval(
    expr: Expr,
    env: EnvRef,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    bounce_continuation(
        Rc::new(move |_ignored, output| {
            eval_cps(
                expr.clone(),
                env.clone(),
                output,
                k.clone(),
                runtime.clone(),
            )
        }),
        Value::Void,
    )
}

pub(super) fn bounce_sequence(
    exprs: Vec<Expr>,
    env: EnvRef,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    bounce_continuation(
        Rc::new(move |_ignored, output| {
            eval_sequence_cps(
                exprs.clone(),
                env.clone(),
                output,
                k.clone(),
                runtime.clone(),
            )
        }),
        Value::Void,
    )
}

fn shared_winder_prefix_len(left: &[WindFrameRef], right: &[WindFrameRef]) -> usize {
    let mut prefix_len = 0;
    while prefix_len < left.len()
        && prefix_len < right.len()
        && Rc::ptr_eq(&left[prefix_len], &right[prefix_len])
    {
        prefix_len += 1;
    }
    prefix_len
}

fn capture_continuation(target: Continuation, runtime: CpsRuntimeRef) -> Continuation {
    let target_winders = runtime.current_winders();
    Rc::new(move |value, output| {
        transition_to_winders(
            runtime.clone(),
            target_winders.clone(),
            value,
            output,
            target.clone(),
        )
    })
}

// Continuation jumps must leave and re-enter dynamic extents in order.
fn transition_to_winders(
    runtime: CpsRuntimeRef,
    target_winders: Vec<WindFrameRef>,
    value: Value,
    output: &mut String,
    k: Continuation,
) -> RuntimeResult {
    let current = runtime.current_winders();
    let shared = shared_winder_prefix_len(&current, &target_winders);
    let exiting = current[shared..].iter().cloned().rev().collect::<Vec<_>>();
    let entering = target_winders[shared..].to_vec();
    unwind_then_rewind(runtime, exiting, entering, value, output, k)
}

fn unwind_then_rewind(
    runtime: CpsRuntimeRef,
    exiting: Vec<WindFrameRef>,
    entering: Vec<WindFrameRef>,
    value: Value,
    output: &mut String,
    k: Continuation,
) -> RuntimeResult {
    let Some((frame, rest)) = exiting.split_first() else {
        return rewind_winders(runtime, entering, value, output, k);
    };

    runtime.pop_winder(frame);
    let rest_frames = rest.to_vec();
    let out_thunk = frame.out_thunk.clone();
    let entering_frames = entering.clone();
    let next_k = k.clone();
    let next_runtime = runtime.clone();
    apply_cps(
        out_thunk,
        Vec::new(),
        output,
        Rc::new(move |_ignored, output| {
            unwind_then_rewind(
                next_runtime.clone(),
                rest_frames.clone(),
                entering_frames.clone(),
                value.clone(),
                output,
                next_k.clone(),
            )
        }),
        runtime,
    )
}

fn rewind_winders(
    runtime: CpsRuntimeRef,
    entering: Vec<WindFrameRef>,
    value: Value,
    output: &mut String,
    k: Continuation,
) -> RuntimeResult {
    let Some((frame, rest)) = entering.split_first() else {
        return k(value, output);
    };

    let rest_frames = rest.to_vec();
    let in_thunk = frame.in_thunk.clone();
    let frame_to_push = frame.clone();
    let next_k = k.clone();
    let next_runtime = runtime.clone();
    apply_cps(
        in_thunk,
        Vec::new(),
        output,
        Rc::new(move |_ignored, output| {
            next_runtime.push_winder(frame_to_push.clone());
            rewind_winders(
                next_runtime.clone(),
                rest_frames.clone(),
                value.clone(),
                output,
                next_k.clone(),
            )
        }),
        runtime,
    )
}

pub(super) fn apply_cps(
    callable: Value,
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    match callable {
        Value::Builtin(Builtin::Raise) => apply_raise_cps(args, output, runtime),
        Value::Builtin(Builtin::Error) => apply_error_cps(args, output, runtime),
        Value::Builtin(Builtin::WithExceptionHandler) => {
            apply_with_exception_handler_cps(args, output, k, runtime)
        }
        Value::Builtin(Builtin::CallCc) => match args.as_slice() {
            [procedure] => apply_cps(
                procedure.clone(),
                vec![Value::Continuation(capture_continuation(
                    k.clone(),
                    runtime.clone(),
                ))],
                output,
                k,
                runtime,
            ),
            _ => Err(wrong_arg_count("call/cc", "1", args.len()).into()),
        },
        Value::Builtin(Builtin::DynamicWind) => apply_dynamic_wind_cps(args, output, k, runtime),
        Value::Builtin(Builtin::Values) => k(Value::from_values(args), output),
        Value::Builtin(Builtin::CallWithValues) => {
            apply_call_with_values_cps(args, output, k, runtime)
        }
        Value::Builtin(Builtin::Apply) => apply_apply_cps(args, output, k, runtime),
        Value::Builtin(Builtin::Map) => apply_map_cps(args, output, k, runtime),
        Value::Builtin(Builtin::ForEach) => apply_for_each_cps(args, output, k, runtime),
        Value::Builtin(builtin) => {
            let value = apply_builtin(builtin, &args, output, runtime.steps())?;
            k(value, output)
        }
        Value::Procedure(procedure) => apply_procedure_cps(&procedure, args, output, k, runtime),
        Value::Continuation(continuation) => Err(RuntimeError::ContinuationJump {
            jump: ContinuationJumpData::new(continuation, Value::from_values(args)),
        }),
        Value::RecordProcedure(procedure) => {
            let value = apply_record_procedure(&procedure, &args)?;
            k(value, output)
        }
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }
        .into()),
    }
}

fn apply_call_with_values_cps(
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let [producer, consumer] = args.as_slice() else {
        return Err(wrong_arg_count("call-with-values", "2", args.len()).into());
    };

    let consumer = consumer.clone();
    let consumer_k = k.clone();
    let consumer_runtime = runtime.clone();
    apply_cps(
        producer.clone(),
        Vec::new(),
        output,
        Rc::new(move |produced, output| {
            apply_cps(
                consumer.clone(),
                produced.into_values(),
                output,
                consumer_k.clone(),
                consumer_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn apply_raise_cps(
    args: Vec<Value>,
    output: &mut String,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    match args.as_slice() {
        [value] => raise_cps(value.clone(), output, runtime),
        _ => Err(wrong_arg_count("raise", "1", args.len()).into()),
    }
}

fn apply_error_cps(
    args: Vec<Value>,
    output: &mut String,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    raise_cps(error_exception_value(&args), output, runtime)
}

fn raise_cps(
    value: Value,
    output: &mut String,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let Some(frame) = runtime.pop_handler() else {
        return Err(EvalError::UncaughtException {
            value: value.render(),
        }
        .into());
    };

    let handler = frame.handler.clone();
    let return_k = frame.return_k.clone();
    let target_winders = frame.winders.clone();
    let handler_runtime = runtime.clone();
    transition_to_winders(
        runtime,
        target_winders,
        value,
        output,
        Rc::new(move |exception, output| {
            apply_cps(
                handler.clone(),
                vec![exception],
                output,
                return_k.clone(),
                handler_runtime.clone(),
            )
        }),
    )
}

fn apply_with_exception_handler_cps(
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let [handler, thunk] = args.as_slice() else {
        return Err(wrong_arg_count("with-exception-handler", "2", args.len()).into());
    };

    let frame = Rc::new(ExceptionHandlerFrame {
        handler: handler.clone(),
        return_k: k.clone(),
        winders: runtime.current_winders(),
    });
    runtime.push_handler(frame.clone());

    let frame_for_normal_return = frame.clone();
    let pop_runtime = runtime.clone();
    let normal_k = k.clone();
    let result = apply_cps(
        thunk.clone(),
        Vec::new(),
        output,
        Rc::new(move |value, _output| {
            pop_runtime.pop_handler_if_current(&frame_for_normal_return);
            bounce_continuation(normal_k.clone(), value)
        }),
        runtime.clone(),
    );

    if matches!(
        result,
        Err(RuntimeError::ContinuationJump { ref jump }) if jump.is_trampoline()
    ) {
        return result;
    }

    if result.is_err() {
        runtime.pop_handler_if_current(&frame);
    }

    result
}

fn apply_procedure_cps(
    procedure: &Procedure,
    args: Vec<Value>,
    _output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
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
        )
        .into());
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
    bounce_sequence(clause.body.clone(), call_env, k, runtime)
}

fn apply_apply_cps(
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let [callable, prefix_and_list @ ..] = args.as_slice() else {
        return Err(wrong_arg_count("apply", "at least 2", 0).into());
    };

    if prefix_and_list.is_empty() {
        return Err(wrong_arg_count("apply", "at least 2", 1).into());
    }

    let (list_arg, prefix_args) = prefix_and_list
        .split_last()
        .expect("prefix_and_list is known to be non-empty");
    let list_items = collect_list_cps("apply", list_arg)?;

    let mut applied_args = Vec::with_capacity(prefix_args.len() + list_items.len());
    applied_args.extend(prefix_args.iter().cloned());
    applied_args.extend(list_items);
    apply_cps(callable.clone(), applied_args, output, k, runtime)
}

fn apply_map_cps(
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let [callable, list_args @ ..] = args.as_slice() else {
        return Err(wrong_arg_count("map", "at least 2", 0).into());
    };

    if list_args.is_empty() {
        return Err(wrong_arg_count("map", "at least 2", 1).into());
    }

    let mut lists = Vec::with_capacity(list_args.len());
    for list in list_args {
        lists.push(collect_list_cps("map", list)?);
    }

    apply_map_loop_cps(callable.clone(), lists, 0, Vec::new(), output, k, runtime)
}

fn apply_map_loop_cps(
    callable: Value,
    lists: Vec<Vec<Value>>,
    index: usize,
    results: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let len = lists.iter().map(Vec::len).min().unwrap_or(0);
    if index >= len {
        return k(list_from_values(results), output);
    }

    let mut mapped_args = Vec::with_capacity(lists.len());
    for list in &lists {
        mapped_args.push(list[index].clone());
    }

    let loop_callable = callable.clone();
    let loop_lists = lists.clone();
    let loop_k = k.clone();
    let loop_runtime = runtime.clone();
    apply_cps(
        callable,
        mapped_args,
        output,
        Rc::new(move |value, output| {
            let mut next_results = results.clone();
            next_results.push(value);
            apply_map_loop_cps(
                loop_callable.clone(),
                loop_lists.clone(),
                index + 1,
                next_results,
                output,
                loop_k.clone(),
                loop_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn apply_dynamic_wind_cps(
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let [in_thunk, body_thunk, out_thunk] = args.as_slice() else {
        return Err(wrong_arg_count("dynamic-wind", "3", args.len()).into());
    };

    let frame = Rc::new(WindFrame {
        in_thunk: in_thunk.clone(),
        out_thunk: out_thunk.clone(),
    });
    let body = body_thunk.clone();
    let return_winders = runtime.current_winders();
    let return_k = k.clone();
    let wind_runtime = runtime.clone();
    apply_cps(
        in_thunk.clone(),
        Vec::new(),
        output,
        Rc::new(move |_ignored, output| {
            wind_runtime.push_winder(frame.clone());
            let target_winders = return_winders.clone();
            let target_k = return_k.clone();
            let body_runtime = wind_runtime.clone();
            let transition_runtime = wind_runtime.clone();
            apply_cps(
                body.clone(),
                Vec::new(),
                output,
                Rc::new(move |body_value, output| {
                    transition_to_winders(
                        transition_runtime.clone(),
                        target_winders.clone(),
                        body_value,
                        output,
                        target_k.clone(),
                    )
                }),
                body_runtime,
            )
        }),
        runtime,
    )
}

fn apply_for_each_cps(
    args: Vec<Value>,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let [callable, list_args @ ..] = args.as_slice() else {
        return Err(wrong_arg_count("for-each", "at least 2", 0).into());
    };

    if list_args.is_empty() {
        return Err(wrong_arg_count("for-each", "at least 2", 1).into());
    }

    let mut lists = Vec::with_capacity(list_args.len());
    for list in list_args {
        lists.push(collect_list_cps("for-each", list)?);
    }

    apply_for_each_loop_cps(callable.clone(), lists, 0, output, k, runtime)
}

fn apply_for_each_loop_cps(
    callable: Value,
    lists: Vec<Vec<Value>>,
    index: usize,
    output: &mut String,
    k: Continuation,
    runtime: CpsRuntimeRef,
) -> RuntimeResult {
    let len = lists.iter().map(Vec::len).min().unwrap_or(0);
    if index >= len {
        return k(Value::Void, output);
    }

    let mut call_args = Vec::with_capacity(lists.len());
    for list in &lists {
        call_args.push(list[index].clone());
    }

    let loop_callable = callable.clone();
    let loop_lists = lists.clone();
    let loop_k = k.clone();
    let loop_runtime = runtime.clone();
    apply_cps(
        callable,
        call_args,
        output,
        Rc::new(move |_value, output| {
            apply_for_each_loop_cps(
                loop_callable.clone(),
                loop_lists.clone(),
                index + 1,
                output,
                loop_k.clone(),
                loop_runtime.clone(),
            )
        }),
        runtime,
    )
}

fn collect_list_cps(name: &str, value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut seen = HashSet::new();

    loop {
        match current {
            Value::EmptyList => return Ok(items),
            Value::Pair(pair) => {
                if !seen.insert(pair.id()) {
                    return Err(EvalError::CircularList { name: name.into() });
                }
                items.push(pair.car());
                current = pair.cdr();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    name: name.into(),
                    expected: "list".into(),
                    got: other.type_name().into(),
                });
            }
        }
    }
}
