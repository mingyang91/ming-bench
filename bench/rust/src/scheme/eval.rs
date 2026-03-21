use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::macros;
use crate::scheme::number::{self, Num, num_add, num_div, num_mul, num_neg, num_sub, num_to_f64, num_to_value, value_to_num};
use crate::scheme::value::{Value, list_from_vec, make_pair};

/// A snapshot of a body being evaluated — used for continuation path matching.
pub struct BodyFrame {
    /// Index of the expression currently being evaluated.
    pub current_pos: usize,
    /// Monotonic frame ID, deterministic across replays.
    pub frame_id: u64,
}

/// Identity path of a call/cc site: sequence of (body_pos, frame_id).
/// Two call/cc invocations with the same path are the "same" call/cc.
type CallPath = Vec<(usize, u64)>;

fn make_call_path(body_stack: &[BodyFrame]) -> CallPath {
    body_stack
        .iter()
        .map(|f| (f.current_pos, f.frame_id))
        .collect()
}

/// Shared interpreter state: output buffer + continuation registry.
pub struct InterpState {
    pub output: String,
    pub next_cont_id: u64,
    pub cont_captures: HashMap<u64, usize>,
    /// Call path recorded at each continuation capture.
    pub cont_paths: HashMap<u64, CallPath>,
    /// Targeted resume: (call-path of the target call/cc, value).
    pub resume: Option<(CallPath, Value)>,
    pub current_expr_idx: usize,
    pub gensym_counter: u64,
    /// Stack of body frames currently being evaluated.
    pub body_stack: Vec<BodyFrame>,
    /// Counter for generating unique record type IDs.
    pub next_record_type_id: u64,
    /// Pending hygiene gensym mappings from syntax-case (orig_name → gensym_name).
    pub pending_hygiene: Vec<(String, String)>,
    /// Monotonic counter for deterministic frame IDs.
    pub next_frame_id: u64,
}

impl InterpState {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            next_cont_id: 0,
            cont_captures: HashMap::new(),
            cont_paths: HashMap::new(),
            resume: None,
            current_expr_idx: 0,
            gensym_counter: 0,
            body_stack: Vec::new(),
            next_record_type_id: 0,
            pending_hygiene: Vec::new(),
            next_frame_id: 0,
        }
    }
}

/// Check if the current body_stack path matches the targeted resume.
/// If so, consume and return the value. Otherwise leave resume in place.
fn try_consume_resume(out: &Output) -> Option<Value> {
    let st = out.borrow();
    let (target_path, _) = st.resume.as_ref()?;
    let current_path = make_call_path(&st.body_stack);
    if current_path == *target_path {
        drop(st);
        let (_, value) = out.borrow_mut().resume.take().expect("checked above");
        Some(value)
    } else {
        None
    }
}

pub type Output = Rc<RefCell<InterpState>>;

/// Validate that the number of arguments matches the parameter list.
fn check_arity(
    params_len: usize,
    has_rest: bool,
    got: usize,
) -> Result<(), EvalError> {
    let ok = if has_rest { got >= params_len } else { got == params_len };
    if !ok {
        return Err(EvalError::WrongArgCount {
            expected: params_len,
            got,
        });
    }
    Ok(())
}

/// Package continuation arguments: 1 arg → value, multiple → Values.
fn continuation_value_from_args(args: Vec<Value>) -> Value {
    match args.len() {
        1 => args.into_iter().next().expect("checked length"),
        _ => Value::Values(args),
    }
}

/// Evaluate a single parsed expression in the given environment.
/// Uses a trampoline loop for tail call optimization.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = Rc::clone(env);

    loop {
        match &current_expr {
            Value::Integer(_) | Value::Float(_) | Value::Rational(_, _)
            | Value::Boolean(_) | Value::Str(_) | Value::Char(_)
            | Value::Void | Value::Vector(_) | Value::Pair(_) | Value::Values(_)
            | Value::Record { .. } => {
                return Ok(current_expr)
            }
            Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_)
            | Value::Macro { .. } | Value::TransformerMacro { .. } => {
                return Ok(current_expr)
            }
            Value::Symbol(name) => {
                return current_env
                    .borrow()
                    .get(name)
                    .ok_or_else(|| EvalError::UnboundVariable {
                        name: name.clone(),
                    })
            }
            Value::List(items) => match eval_list_tail(items, &current_env, out)? {
                TailAction::Return(val) => return Ok(val),
                TailAction::TailEval(expr, env) => {
                    current_expr = expr;
                    current_env = env;
                    continue;
                }
            },
        }
    }
}

/// Evaluate a list form, returning a tail action for the trampoline.
fn eval_list_tail(
    items: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if items.is_empty() {
        return Ok(TailAction::Return(Value::List(vec![])));
    }

    if let Value::Symbol(name) = &items[0] {
        match name.as_str() {
            "define" => return eval_define(&items[1..], env, out).map(TailAction::Return),
            "quote" => return eval_quote(&items[1..]).map(TailAction::Return),
            "lambda" => return eval_lambda(&items[1..], env).map(TailAction::Return),
            "set!" => return eval_set(&items[1..], env, out).map(TailAction::Return),
            "string-set!" => return Err(EvalError::ImmutableString),
            "if" => return eval_if_tail(&items[1..], env, out),
            "begin" => return eval_body_tail(&items[1..], env, out),
            "cond" => return eval_cond_tail(&items[1..], env, out),
            "and" => return eval_and_tail(&items[1..], env, out),
            "or" => return eval_or_tail(&items[1..], env, out),
            "let" => return eval_let_tail(&items[1..], env, out),
            "letrec" => return eval_letrec_tail(&items[1..], env, out),
            "letrec*" => return eval_letrec_star_tail(&items[1..], env, out),
            "case" => return eval_case_tail(&items[1..], env, out),
            "call/cc" | "call-with-current-continuation" => {
                return eval_callcc(&items[1..], env, out).map(TailAction::Return)
            }
            "dynamic-wind" => {
                return eval_dynamic_wind(&items[1..], env, out).map(TailAction::Return)
            }
            "define-syntax" => {
                return eval_define_syntax(&items[1..], env, out).map(TailAction::Return)
            }
            "syntax-case" => {
                return eval_syntax_case_tail(&items[1..], env, out)
            }
            "syntax" => {
                return eval_syntax(&items[1..], env, out).map(TailAction::Return)
            }
            "with-syntax" => {
                return eval_with_syntax_tail(&items[1..], env, out)
            }
            "define-record-type" => {
                return eval_define_record_type(&items[1..], env, out)
                    .map(TailAction::Return)
            }
            "guard" => return eval_guard(&items[1..], env, out),
            "raise" => return eval_raise(&items[1..], env, out).map(TailAction::Return),
            "with-exception-handler" => {
                return eval_with_exception_handler(&items[1..], env, out)
                    .map(TailAction::Return)
            }
            s if is_builtin(s) => {
                return eval_builtin(s, &items[1..], env, out).map(TailAction::Return)
            }
            _ => return try_expand_macro_tail(name, items, env, out),
        }
    }

    eval_application_tail(items, env, out)
}

/// If `name` is a macro, expand and return as tail eval; otherwise fall through
/// to normal application.
fn try_expand_macro_tail(
    name: &str,
    items: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    let macro_val = env.borrow().get(name);
    match macro_val {
        Some(Value::Macro {
            ref literals,
            ref rules,
            ref def_env,
        }) => {
            let mut counter = out.borrow().gensym_counter;
            let expansion = macros::expand_macro(literals, rules, def_env, items, &mut counter)?;
            out.borrow_mut().gensym_counter = counter;
            for (gs_name, val) in expansion.hygiene_bindings {
                env.borrow_mut().define(gs_name, val);
            }
            Ok(TailAction::TailEval(expansion.expanded, Rc::clone(env)))
        }
        Some(Value::TransformerMacro { ref transformer }) => {
            let expanded = call_transformer(transformer, items, env, out)?;
            Ok(TailAction::TailEval(expanded, Rc::clone(env)))
        }
        _ => eval_application_tail(items, env, out),
    }
}

/// Call a syntax-case transformer lambda with the macro invocation form.
/// After evaluation, applies hygiene bindings to the invocation env.
fn call_transformer(
    transformer: &Value,
    items: &[Value],
    invocation_env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let Value::Lambda {
        params,
        rest_param: _,
        body,
        closure,
    } = transformer
    else {
        return Err(EvalError::TypeError {
            expected: "lambda".into(),
            got: format!("{transformer}"),
        });
    };
    let form = Value::List(items.to_vec());
    let call_env = Env::extend(closure);
    if let Some(param) = params.first() {
        call_env.borrow_mut().define(param.clone(), form);
    }
    let mut result = Value::Void;
    for expr in body {
        result = eval(expr, &call_env, out)?;
    }

    // Apply hygiene bindings: resolve gensym'd names in the invocation env.
    // Collect first, then define, to avoid RefCell borrow conflicts.
    let pending: Vec<(String, String)> =
        std::mem::take(&mut out.borrow_mut().pending_hygiene);
    let resolved: Vec<(String, Value)> = pending
        .into_iter()
        .filter_map(|(orig, gs)| {
            invocation_env.borrow().get(&orig).map(|val| (gs, val))
        })
        .collect();
    for (gs, val) in resolved {
        invocation_env.borrow_mut().define(gs, val);
    }

    Ok(result)
}

/// Evaluate an `if` form, returning a tail action.
fn eval_if_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    let (cond, then, els) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if: expected 2 or 3 arguments".into(),
            })
        }
    };
    let cond_val = eval(cond, env, out)?;
    if cond_val != Value::Boolean(false) {
        Ok(TailAction::TailEval(then.clone(), Rc::clone(env)))
    } else {
        match els {
            Some(e) => Ok(TailAction::TailEval(e.clone(), Rc::clone(env))),
            None => Ok(TailAction::Return(Value::Void)),
        }
    }
}

/// Evaluate `and` form, returning a tail action for the last expression.
fn eval_and_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.is_empty() {
        return Ok(TailAction::Return(Value::Boolean(true)));
    }
    let (last, rest) = args.split_last().expect("non-empty");
    for arg in rest {
        let val = eval(arg, env, out)?;
        if val == Value::Boolean(false) {
            return Ok(TailAction::Return(Value::Boolean(false)));
        }
    }
    Ok(TailAction::TailEval(last.clone(), Rc::clone(env)))
}

/// Evaluate `or` form, returning a tail action for the last expression.
fn eval_or_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.is_empty() {
        return Ok(TailAction::Return(Value::Boolean(false)));
    }
    let (last, rest) = args.split_last().expect("non-empty");
    for arg in rest {
        let val = eval(arg, env, out)?;
        if val != Value::Boolean(false) {
            return Ok(TailAction::Return(val));
        }
    }
    Ok(TailAction::TailEval(last.clone(), Rc::clone(env)))
}

/// Evaluate `call/cc`: capture the current continuation and call proc with it.
fn eval_callcc(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [proc_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    if let Some(value) = try_consume_resume(out) {
        return Ok(value);
    }
    let proc = eval(proc_arg, env, out)?;
    eval_callcc_core(proc, env, out)
}

/// Core call/cc logic with an already-evaluated procedure.
fn eval_callcc_core(
    proc: Value,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let id = {
        let mut st = out.borrow_mut();
        let id = st.next_cont_id;
        let expr_idx = st.current_expr_idx;
        let path = make_call_path(&st.body_stack);
        st.next_cont_id += 1;
        st.cont_captures.insert(id, expr_idx);
        st.cont_paths.insert(id, path);
        id
    };
    let cont = Value::Continuation(id);
    apply_values(&proc, vec![cont], env, out)
}

/// Evaluate `dynamic-wind`: (dynamic-wind in-thunk body-thunk out-thunk)
fn eval_dynamic_wind(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [in_arg, body_arg, out_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let in_thunk = eval(in_arg, env, out)?;
    let body_thunk = eval(body_arg, env, out)?;
    let out_thunk = eval(out_arg, env, out)?;
    eval_dynamic_wind_core(&in_thunk, &body_thunk, &out_thunk, env, out)
}

/// Core dynamic-wind logic with already-evaluated thunks.
fn eval_dynamic_wind_core(
    in_thunk: &Value,
    body_thunk: &Value,
    out_thunk: &Value,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    // Run in-thunk
    apply_values(in_thunk, vec![], env, out)?;
    // Run body-thunk, catching ContinuationReturn
    let body_result = apply_values(body_thunk, vec![], env, out);
    match body_result {
        Ok(val) => {
            apply_values(out_thunk, vec![], env, out)?;
            Ok(val)
        }
        Err(EvalError::ContinuationReturn { id, value }) => {
            // Run out-thunk even on non-local exit
            apply_values(out_thunk, vec![], env, out)?;
            Err(EvalError::ContinuationReturn { id, value })
        }
        Err(e) => {
            apply_values(out_thunk, vec![], env, out)?;
            Err(e)
        }
    }
}

/// Evaluate `raise`: (raise value)
fn eval_raise(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [val_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let value = eval(val_expr, env, out)?;
    Err(EvalError::RaisedException {
        value: Box::new(value),
    })
}

/// Parse guard arguments into (var_name, clauses, body).
fn parse_guard_args(args: &[Value]) -> Result<(String, Vec<Value>, Vec<Value>), EvalError> {
    let Some((clauses_form, body)) = args.split_first() else {
        return Err(EvalError::Parse {
            message: "guard: expected (var clause ...) body".into(),
        });
    };
    let Value::List(clause_parts) = clauses_form else {
        return Err(EvalError::Parse {
            message: "guard: first arg must be (var clause ...)".into(),
        });
    };
    let Some((var_name_val, clauses)) = clause_parts.split_first() else {
        return Err(EvalError::Parse {
            message: "guard: empty clause list".into(),
        });
    };
    let Value::Symbol(var_name) = var_name_val else {
        return Err(EvalError::Parse {
            message: "guard: variable must be a symbol".into(),
        });
    };
    Ok((var_name.clone(), clauses.to_vec(), body.to_vec()))
}

/// Result of the guard trampoline's inner loop.
enum GuardTrampResult {
    /// The trampoline produced a final result.
    Finished(Result<TailAction, EvalError>),
    /// A nested guard form was encountered; restart with new parameters.
    RestartGuard {
        var_name: String,
        clauses: Vec<Value>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
}

/// If `expr` is a `(guard ...)` form, parse it and return a trampoline result.
fn try_intercept_guard(expr: &Value, env: &Rc<RefCell<Env>>) -> Option<GuardTrampResult> {
    let Value::List(items) = expr else { return None };
    if !is_guard_form(items) { return None; }
    Some(match parse_guard_args(&items[1..]) {
        Ok((v, c, b)) => GuardTrampResult::RestartGuard {
            var_name: v, clauses: c, body: b, env: Rc::clone(env),
        },
        Err(e) => GuardTrampResult::Finished(Err(e)),
    })
}

/// Run a local trampoline for the guard tail expression, catching exceptions.
/// Returns `RestartGuard` when a nested guard form is encountered.
fn run_guard_trampoline(
    mut current_expr: Value,
    mut current_env: Rc<RefCell<Env>>,
    var_name: &str,
    clauses: &[Value],
    guard_env: &Rc<RefCell<Env>>,
    out: &Output,
) -> GuardTrampResult {
    loop {
        if let Some(result) = try_intercept_guard(&current_expr, &current_env) {
            return result;
        }
        let step = match &current_expr {
            Value::List(items) => eval_list_tail(items, &current_env, out),
            Value::Symbol(name) => current_env
                .borrow()
                .get(name)
                .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })
                .map(TailAction::Return),
            _ => Ok(TailAction::Return(current_expr.clone())),
        };
        match step {
            Ok(TailAction::Return(val)) => {
                return GuardTrampResult::Finished(Ok(TailAction::Return(val)));
            }
            Ok(TailAction::TailEval(expr, env)) => {
                current_expr = expr;
                current_env = env;
            }
            Err(EvalError::RaisedException { value }) => {
                return GuardTrampResult::Finished(dispatch_guard_exception(
                    var_name, clauses, *value, guard_env, out,
                ));
            }
            Err(e) => return GuardTrampResult::Finished(Err(e)),
        }
    }
}

/// Evaluate `guard`: (guard (var clause ...) body ...)
/// Each clause is (test expr ...) or (else expr ...).
/// Uses a local trampoline to support TCO when the body tail-calls back
/// into a function that contains guard (e.g., tail-recursive guard loops).
fn eval_guard(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    let (mut var_name, mut clauses, mut body) = parse_guard_args(args)?;
    let mut guard_env = Rc::clone(env);

    loop {
        let tail_expr = match eval_guard_body_prefix(&body, &guard_env, out) {
            Ok(expr) => expr,
            Err(EvalError::RaisedException { value }) => {
                return dispatch_guard_exception(&var_name, &clauses, *value, &guard_env, out);
            }
            Err(e) => return Err(e),
        };
        let Some(tail_expr) = tail_expr else {
            return Ok(TailAction::Return(Value::Void));
        };
        match run_guard_trampoline(tail_expr, Rc::clone(&guard_env), &var_name, &clauses, &guard_env, out) {
            GuardTrampResult::Finished(result) => return result,
            GuardTrampResult::RestartGuard { var_name: v, clauses: c, body: b, env: e } => {
                var_name = v;
                clauses = c;
                body = b;
                guard_env = e;
            }
        }
    }
}

/// Evaluate non-tail body expressions, returning the last (tail) expression unevaluated.
fn eval_guard_body_prefix(
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Option<Value>, EvalError> {
    let Some((last, rest)) = body.split_last() else {
        return Ok(None);
    };
    for expr in rest {
        eval(expr, env, out)?;
    }
    Ok(Some(last.clone()))
}

fn is_guard_form(items: &[Value]) -> bool {
    matches!(items.first(), Some(Value::Symbol(s)) if s == "guard")
}

/// Dispatch a caught exception to guard clauses.
fn dispatch_guard_exception(
    var_name: &str,
    clauses: &[Value],
    value: Value,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    let guard_env = Env::extend(env);
    guard_env
        .borrow_mut()
        .define(var_name.to_string(), value.clone());
    eval_guard_clauses(clauses, value, &guard_env, out)
}

/// Test guard clauses against a raised exception value.
fn eval_guard_clauses(
    clauses: &[Value],
    raised_value: Value,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::Parse {
                message: "guard: expected clause".into(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "guard: empty clause".into(),
            });
        }
        if matches!(&parts[0], Value::Symbol(s) if s == "else") {
            return eval_body_tail(&parts[1..], env, out);
        }
        let test_val = eval(&parts[0], env, out)?;
        if test_val != Value::Boolean(false) {
            return eval_body_tail(&parts[1..], env, out);
        }
    }
    // No clause matched — re-raise
    Err(EvalError::RaisedException {
        value: Box::new(raised_value),
    })
}

/// Evaluate `with-exception-handler`: (with-exception-handler handler thunk)
fn eval_with_exception_handler(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [handler_expr, thunk_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let handler = eval(handler_expr, env, out)?;
    let thunk = eval(thunk_expr, env, out)?;
    let result = apply_values(&thunk, vec![], env, out);
    match result {
        Ok(val) => Ok(val),
        Err(EvalError::RaisedException { value }) => {
            apply_values(&handler, vec![*value], env, out)
        }
        Err(e) => Err(e),
    }
}

/// Apply a procedure to already-evaluated argument values.
fn apply_values(
    proc: &Value,
    args: Vec<Value>,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => {
            check_arity(params.len(), rest_param.is_some(), args.len())?;
            let child = Env::extend(closure);
            for (param, val) in params.iter().zip(&args) {
                child.borrow_mut().define(param.clone(), val.clone());
            }
            if let Some(rest_name) = rest_param {
                let rest_vals = args[params.len()..].to_vec();
                child.borrow_mut().define(rest_name.clone(), list_from_vec(rest_vals));
            }
            eval_body(body, &child, out)
        }
        Value::Builtin(name) => call_builtin_with_values(name, args, env, out),
        Value::Continuation(id) => {
            let value = continuation_value_from_args(args);
            Err(EvalError::ContinuationReturn {
                id: *id,
                value: Box::new(value),
            })
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

/// Evaluate a function application, returning a tail action for lambda calls.
fn eval_application_tail(
    items: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    let proc = eval(&items[0], env, out)?;

    // Handle call/cc as first-class value (e.g., passed to a lambda)
    if matches!(&proc, Value::Builtin(name) if name == "call/cc") {
        let [proc_arg] = &items[1..] else {
            return Err(EvalError::WrongArgCount {
                expected: 1,
                got: items.len() - 1,
            });
        };
        if let Some(value) = try_consume_resume(out) {
            return Ok(TailAction::Return(value));
        }
        let proc_val = eval(proc_arg, env, out)?;
        return eval_callcc_core(proc_val, env, out).map(TailAction::Return);
    }

    let args: Vec<Value> = items[1..]
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;

    match proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => bind_and_tail_call(params, rest_param, body, closure, args, out),
        Value::Builtin(name) => {
            call_builtin_with_values(&name, args, env, out).map(TailAction::Return)
        }
        Value::Continuation(id) => {
            let value = continuation_value_from_args(args);
            Err(EvalError::ContinuationReturn {
                id,
                value: Box::new(value),
            })
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

/// Bind args to params (with optional rest param) and tail-call the body.
fn bind_and_tail_call(
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Value>,
    closure: Rc<RefCell<Env>>,
    args: Vec<Value>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::WrongArgCount {
                expected: params.len(),
                got: args.len(),
            });
        }
    } else if args.len() != params.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        });
    }
    let child = Env::extend(&closure);
    for (param, val) in params.iter().zip(&args) {
        child.borrow_mut().define(param.clone(), val.clone());
    }
    if let Some(rest_name) = rest_param {
        let rest_vals = args[params.len()..].to_vec();
        child.borrow_mut().define(rest_name, list_from_vec(rest_vals));
    }
    eval_body_tail(&body, &child, out)
}

/// Call a builtin with already-evaluated argument values.
fn call_builtin_with_values(
    name: &str,
    values: Vec<Value>,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    if name == "apply" {
        return eval_apply_values(values, env, out);
    }
    if name == "call/cc" || name == "call-with-current-continuation" {
        let [proc] = values.as_slice() else {
            return Err(EvalError::WrongArgCount {
                expected: 1,
                got: values.len(),
            });
        };
        if let Some(value) = try_consume_resume(out) {
            return Ok(value);
        }
        return eval_callcc_core(proc.clone(), env, out);
    }
    if name == "dynamic-wind" {
        let [in_thunk, body_thunk, out_thunk] = values.as_slice() else {
            return Err(EvalError::WrongArgCount {
                expected: 3,
                got: values.len(),
            });
        };
        return eval_dynamic_wind_core(in_thunk, body_thunk, out_thunk, env, out);
    }
    if name == "values" {
        return eval_values_core(values);
    }
    if name == "call-with-values" {
        let [producer, consumer] = values.as_slice() else {
            return Err(EvalError::WrongArgCount {
                expected: 2,
                got: values.len(),
            });
        };
        return eval_call_with_values_core(producer, consumer, env, out);
    }
    let quoted_args: Vec<Value> = values
        .into_iter()
        .map(|v| Value::List(vec![Value::Symbol("quote".into()), v]))
        .collect();
    eval_builtin(name, &quoted_args, env, out)
}

/// Implement `apply`: (apply proc arg1 ... args-list)
fn eval_apply(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let values: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;
    eval_apply_values(values, env, out)
}

fn eval_apply_values(
    args: Vec<Value>,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    }
    let proc = args[0].clone();
    let tail_args = collect_list(&args[args.len() - 1])?;
    let mut combined: Vec<Value> = args[1..args.len() - 1].to_vec();
    combined.extend(tail_args);

    match proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => {
            check_arity(params.len(), rest_param.is_some(), combined.len())?;
            let child = Env::extend(&closure);
            for (param, val) in params.iter().zip(&combined) {
                child.borrow_mut().define(param.clone(), val.clone());
            }
            if let Some(rest_name) = rest_param {
                let rest_vals = combined[params.len()..].to_vec();
                child.borrow_mut().define(rest_name, list_from_vec(rest_vals));
            }
            eval_body(&body, &child, out)
        }
        Value::Builtin(name) => call_builtin_with_values(&name, combined, env, out),
        Value::Continuation(id) => {
            let value = continuation_value_from_args(combined);
            Err(EvalError::ContinuationReturn {
                id,
                value: Box::new(value),
            })
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

/// Result of evaluating a form that may produce a tail call.
enum TailAction {
    Return(Value),
    TailEval(Value, Rc<RefCell<Env>>),
}

/// Evaluate cond clauses, returning a tail action for the matching clause's last expr.
fn eval_cond_tail(
    clauses: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::Parse {
                message: "cond: expected clause".into(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "cond: empty clause".into(),
            });
        }
        if matches!(&parts[0], Value::Symbol(s) if s == "else") {
            return eval_body_tail(&parts[1..], env, out);
        }
        let test_val = eval(&parts[0], env, out)?;
        if test_val != Value::Boolean(false) {
            return eval_body_tail(&parts[1..], env, out);
        }
    }
    Ok(TailAction::Return(Value::Void))
}

/// Evaluate the non-tail prefix of a body sequence for side effects.
fn eval_body_prefix(
    prefix: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<(), EvalError> {
    let frame_idx = out.borrow().body_stack.len();
    {
        let mut st = out.borrow_mut();
        let fid = st.next_frame_id;
        st.next_frame_id += 1;
        st.body_stack.push(BodyFrame {
            current_pos: 0,
            frame_id: fid,
        });
    }
    for (i, expr) in prefix.iter().enumerate() {
        out.borrow_mut().body_stack[frame_idx].current_pos = i;
        eval(expr, env, out)?;
    }
    out.borrow_mut().body_stack.truncate(frame_idx);
    Ok(())
}

/// Evaluate a body sequence, returning a tail action for the last expression.
fn eval_body_tail(
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    let Some((last, rest)) = body.split_last() else {
        return Ok(TailAction::Return(Value::Void));
    };
    if !rest.is_empty() {
        eval_body_prefix(rest, env, out)?;
    }
    Ok(TailAction::TailEval(last.clone(), Rc::clone(env)))
}

/// Parse a single let binding `(name expr)` into its param name and init expression.
fn parse_let_binding(binding: &Value) -> Result<(&str, &Value), EvalError> {
    let Value::List(pair) = binding else {
        return Err(EvalError::Parse {
            message: "let: expected binding pair".into(),
        });
    };
    let [Value::Symbol(param), init_expr] = pair.as_slice() else {
        return Err(EvalError::Parse {
            message: "let: expected (name expr)".into(),
        });
    };
    Ok((param.as_str(), init_expr))
}

/// Evaluate let (regular or named), returning a tail action for the body's last expr.
fn eval_let_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "let: expected bindings and body".into(),
        });
    }

    if let Value::Symbol(name) = &args[0] {
        return eval_named_let_tail(name, &args[1..], env, out);
    }

    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::Parse {
            message: "let: expected binding list".into(),
        });
    };
    let child = Env::extend(env);
    for binding in bindings {
        let (name, expr) = parse_let_binding(binding)?;
        let val = eval(expr, env, out)?;
        child.borrow_mut().define(name.to_string(), val);
    }
    eval_body_tail(&args[1..], &child, out)
}

/// Evaluate named let: `(let name ((var init) ...) body ...)`
fn eval_named_let_tail(
    name: &str,
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "named let: expected bindings and body".into(),
        });
    }
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::Parse {
            message: "named let: expected binding list".into(),
        });
    };
    let mut params = Vec::new();
    let mut init_vals = Vec::new();
    for binding in bindings {
        let (param, init_expr) = parse_let_binding(binding)?;
        params.push(param.to_string());
        init_vals.push(eval(init_expr, env, out)?);
    }
    let body = args[1..].to_vec();
    let child = Env::extend(env);
    let lambda = Value::Lambda {
        params: params.clone(),
        rest_param: None,
        body,
        closure: Rc::clone(&child),
    };
    child.borrow_mut().define(name.to_string(), lambda);
    for (param, val) in params.iter().zip(init_vals) {
        child.borrow_mut().define(param.clone(), val);
    }
    eval_body_tail(&args[1..], &child, out)
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "string->list"
            | "list->string"
            | "char->integer"
            | "integer->char"
            | "map"
            | "apply"
            | "equal?" | "eqv?" | "eq?"
            | "vector" | "make-vector" | "vector-ref" | "vector-set!"
            | "vector-length" | "vector?" | "vector->list" | "list->vector"
            | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "reverse"
            | "list-ref" | "list-tail" | "list?" | "assoc"
            | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string=?" | "string<?" | "string-ci=?"
            | "string-upcase" | "string-downcase"
            | "values" | "call-with-values"
            | "exact?" | "inexact?" | "rational?" | "integer?"
            | "exact->inexact" | "inexact->exact"
            | "numerator" | "denominator"
            | "set-car!" | "set-cdr!" | "cddr" | "cadr" | "caar" | "cdar"
            | "__make-record-internal"
            | "__record-predicate-internal"
            | "__record-accessor-internal"
            | "syntax->datum"
            | "datum->syntax"
    )
}

fn eval_builtin(
    name: &str,
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args, env, out),
        "-" => eval_sub(args, env, out),
        "*" => eval_mul(args, env, out),
        "/" => eval_div(args, env, out),
        "<" => eval_cmp(args, env, out, |a, b| a < b),
        ">" => eval_cmp(args, env, out, |a, b| a > b),
        "=" => eval_cmp(args, env, out, |a, b| a == b),
        "<=" => eval_cmp(args, env, out, |a, b| a <= b),
        ">=" => eval_cmp(args, env, out, |a, b| a >= b),
        "not" => eval_not(args, env, out),
        "cons" => eval_cons(args, env, out),
        "car" => eval_car(args, env, out),
        "cdr" => eval_cdr(args, env, out),
        "null?" => eval_null_pred(args, env, out),
        "list" => eval_list_builtin(args, env, out),
        "length" => eval_length(args, env, out),
        "string?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Str(_))),
        "number?" => eval_type_pred(args, env, out, number::is_number),
        "boolean?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Boolean(_))),
        "pair?" => eval_type_pred(args, env, out, |v| {
            matches!(v, Value::List(items) if !items.is_empty()) || matches!(v, Value::Pair(_))
        }),
        "symbol?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Symbol(_))),
        "char?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Char(_))),
        "display" => eval_display(args, env, out),
        "write" => eval_write(args, env, out),
        "newline" => eval_newline(args, out),
        "string-append" => eval_string_append(args, env, out),
        "string-length" => eval_string_length(args, env, out),
        "substring" => eval_substring(args, env, out),
        "string->number" => eval_string_to_number(args, env, out),
        "number->string" => eval_number_to_string(args, env, out),
        "symbol->string" => eval_symbol_to_string(args, env, out),
        "string->symbol" => eval_string_to_symbol(args, env, out),
        "string-ref" => eval_string_ref(args, env, out),
        "string-copy" => eval_string_copy(args, env, out),
        "string->list" => eval_string_to_list(args, env, out),
        "list->string" => eval_list_to_string(args, env, out),
        "char->integer" => eval_char_to_integer(args, env, out),
        "integer->char" => eval_integer_to_char(args, env, out),
        "map" => eval_map(args, env, out),
        "apply" => eval_apply(args, env, out),
        "equal?" => eval_equal(args, env, out),
        "eqv?" => eval_eqv(args, env, out),
        "eq?" => eval_eqv(args, env, out),
        "vector" => eval_vector_create(args, env, out),
        "make-vector" => eval_make_vector(args, env, out),
        "vector-ref" => eval_vector_ref(args, env, out),
        "vector-set!" => eval_vector_set(args, env, out),
        "vector-length" => eval_vector_length(args, env, out),
        "vector?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Vector(_))),
        "vector->list" => eval_vector_to_list(args, env, out),
        "list->vector" => eval_list_to_vector(args, env, out),
        "abs" => eval_abs(args, env, out),
        "modulo" => eval_modulo(args, env, out),
        "remainder" => eval_remainder(args, env, out),
        "quotient" => eval_quotient(args, env, out),
        "min" => eval_min_max(args, env, out, true),
        "max" => eval_min_max(args, env, out, false),
        "expt" => eval_expt(args, env, out),
        "zero?" => eval_num_pred(args, env, out, |n| n == 0),
        "positive?" => eval_num_pred(args, env, out, |n| n > 0),
        "negative?" => eval_num_pred(args, env, out, |n| n < 0),
        "odd?" => eval_num_pred(args, env, out, |n| n % 2 != 0),
        "even?" => eval_num_pred(args, env, out, |n| n % 2 == 0),
        "reverse" => eval_reverse(args, env, out),
        "list-ref" => eval_list_ref(args, env, out),
        "list-tail" => eval_list_tail_builtin(args, env, out),
        "list?" => eval_list_pred(args, env, out),
        "assoc" => eval_assoc(args, env, out),
        "char-alphabetic?" => eval_char_pred(args, env, out, |c| c.is_alphabetic()),
        "char-numeric?" => eval_char_pred(args, env, out, |c| c.is_ascii_digit()),
        "char-upcase" => eval_char_case(args, env, out, true),
        "char-downcase" => eval_char_case(args, env, out, false),
        "char=?" => eval_char_cmp(args, env, out, |a, b| a == b),
        "char<?" => eval_char_cmp(args, env, out, |a, b| a < b),
        "string=?" => eval_string_cmp(args, env, out, |a, b| a == b),
        "string<?" => eval_string_cmp(args, env, out, |a, b| a < b),
        "string-ci=?" => eval_string_ci_eq(args, env, out),
        "string-upcase" => eval_string_case(args, env, out, true),
        "string-downcase" => eval_string_case(args, env, out, false),
        "values" => eval_values(args, env, out),
        "call-with-values" => eval_call_with_values(args, env, out),
        "exact?" => eval_type_pred(args, env, out, number::is_exact),
        "inexact?" => eval_type_pred(args, env, out, number::is_inexact),
        "rational?" => eval_type_pred(args, env, out, number::is_rational),
        "integer?" => eval_type_pred(args, env, out, number::is_integer),
        "exact->inexact" => eval_exact_to_inexact(args, env, out),
        "inexact->exact" => eval_inexact_to_exact(args, env, out),
        "numerator" => eval_numerator(args, env, out),
        "denominator" => eval_denominator(args, env, out),
        "set-car!" => eval_set_car(args, env, out),
        "set-cdr!" => eval_set_cdr(args, env, out),
        "cddr" => eval_cddr(args, env, out),
        "cadr" => eval_cadr(args, env, out),
        "caar" => eval_caar(args, env, out),
        "cdar" => eval_cdar(args, env, out),
        "__make-record-internal" => eval_make_record_internal(args, env, out),
        "__record-predicate-internal" => eval_record_predicate_internal(args, env, out),
        "__record-accessor-internal" => eval_record_accessor_internal(args, env, out),
        "syntax->datum" => eval_syntax_to_datum(args, env, out),
        "datum->syntax" => eval_datum_to_syntax(args, env, out),
        _ => Err(EvalError::UnknownProcedure {
            name: name.into(),
        }),
    }
}

fn eval_make_record_internal(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    // args: [type_id_literal, type_name_literal, field_name_str..., field_value_expr...]
    // First arg is the type_id (integer literal), second is type_name (string literal)
    // Then pairs of (field_name_str, field_value_expr)
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "__make-record-internal: bad args".into(),
        });
    }
    let type_id = match eval(&args[0], env, out)? {
        Value::Integer(n) => n as u64,
        _ => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: "other".into(),
            })
        }
    };
    let Value::Str(type_name) = eval(&args[1], env, out)? else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: "other".into(),
        });
    };
    let remaining = &args[2..];
    let n_fields = remaining.len() / 2;
    let field_names = &remaining[..n_fields];
    let field_values = &remaining[n_fields..];
    let fields: Vec<(String, Value)> = field_names
        .iter()
        .zip(field_values)
        .map(|(name_expr, val_expr)| {
            let Value::Str(fname) = eval(name_expr, env, out)? else {
                return Err(EvalError::TypeError {
                    expected: "string".into(),
                    got: "other".into(),
                });
            };
            let val = eval(val_expr, env, out)?;
            Ok((fname, val))
        })
        .collect::<Result<_, EvalError>>()?;
    Ok(Value::Record {
        type_id,
        type_name,
        fields,
    })
}

fn eval_record_predicate_internal(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    // args: [type_id_literal, value_expr]
    let expected_id = match eval(&args[0], env, out)? {
        Value::Integer(n) => n as u64,
        _ => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: "other".into(),
            })
        }
    };
    let val = eval(&args[1], env, out)?;
    let result = matches!(&val, Value::Record { type_id, .. } if *type_id == expected_id);
    Ok(Value::Boolean(result))
}

fn eval_record_accessor_internal(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    // args: [type_id_literal, field_name_str, value_expr]
    let _expected_id = match eval(&args[0], env, out)? {
        Value::Integer(n) => n as u64,
        _ => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: "other".into(),
            })
        }
    };
    let Value::Str(field_name) = eval(&args[1], env, out)? else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: "other".into(),
        });
    };
    let record = eval(&args[2], env, out)?;
    let Value::Record { fields, .. } = &record else {
        return Err(EvalError::TypeError {
            expected: "record".into(),
            got: format!("{record}"),
        });
    };
    fields
        .iter()
        .find_map(|(name, val)| (name == &field_name).then(|| val.clone()))
        .ok_or_else(|| EvalError::TypeError {
            expected: format!("field {field_name}"),
            got: "not found".into(),
        })
}

/// `syntax->datum`: extract the datum from a syntax object (identity in our impl).
fn eval_syntax_to_datum(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    eval(expr, env, out)
}

/// `datum->syntax`: create a syntax object from a datum (identity in our impl).
fn eval_datum_to_syntax(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [_context, datum] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    eval(datum, env, out)
}

/// Evaluate a sequence of body expressions, returning the last.
fn eval_body(body: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let frame_idx = out.borrow().body_stack.len();
    {
        let mut st = out.borrow_mut();
        let fid = st.next_frame_id;
        st.next_frame_id += 1;
        st.body_stack.push(BodyFrame {
            current_pos: 0,
            frame_id: fid,
        });
    }
    let mut result = Value::Void;
    for (i, expr) in body.iter().enumerate() {
        out.borrow_mut().body_stack[frame_idx].current_pos = i;
        result = eval(expr, env, out)?;
    }
    out.borrow_mut().body_stack.truncate(frame_idx);
    Ok(result)
}

fn eval_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [Value::Symbol(name), expr] = args else {
        return Err(EvalError::Parse {
            message: "set!: expected (set! <symbol> <expr>)".into(),
        });
    };
    let val = eval(expr, env, out)?;
    if env.borrow_mut().set(name, val) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundVariable { name: name.clone() })
    }
}

/// Parse a parameter list, detecting dot notation for rest parameters.
/// Returns (fixed_params, optional_rest_param).
fn parse_params(param_list: &[Value]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = param_list
        .iter()
        .position(|v| matches!(v, Value::Symbol(s) if s == "."));
    let Some(dot_pos) = dot_pos else {
        let params: Vec<String> = param_list
            .iter()
            .map(|p| match p {
                Value::Symbol(s) => Ok(s.clone()),
                other => Err(EvalError::Parse {
                    message: format!("expected parameter name, got {other}"),
                }),
            })
            .collect::<Result<_, _>>()?;
        return Ok((params, None));
    };
    let fixed: Vec<String> = param_list[..dot_pos]
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::Parse {
                message: format!("expected parameter name, got {other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    let rest_slice = &param_list[dot_pos + 1..];
    let [Value::Symbol(rest_name)] = rest_slice else {
        return Err(EvalError::Parse {
            message: "expected exactly one rest parameter after dot".into(),
        });
    };
    Ok((fixed, Some(rest_name.clone())))
}

// ===== syntax-case support =====

/// Evaluate `(syntax-case <expr> (<literal> ...) <clause> ...)`.
fn eval_syntax_case_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "syntax-case: expected at least expression and literals".into(),
        });
    }
    let input = eval(&args[0], env, out)?;
    let Value::List(lit_list) = &args[1] else {
        return Err(EvalError::Parse {
            message: "syntax-case: expected literal list".into(),
        });
    };
    let literals: Vec<String> = lit_list
        .iter()
        .filter_map(|v| match v {
            Value::Symbol(s) => Some(s.clone()),
            _ => None,
        })
        .collect();
    let clauses = &args[2..];
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::Parse {
                message: "syntax-case: expected clause".into(),
            });
        };
        if parts.len() < 2 || parts.len() > 3 {
            return Err(EvalError::Parse {
                message: "syntax-case: clause must have pattern and output (with optional fender)"
                    .into(),
            });
        }
        let pattern = &parts[0];
        let (fender, output) = if parts.len() == 3 {
            (Some(&parts[1]), &parts[2])
        } else {
            (None, &parts[1])
        };
        let Some(bindings) = macros::syntax_case_match(pattern, &input, &literals) else {
            continue;
        };
        let child = Env::extend(env);
        install_syntax_bindings(&child, &bindings, env);
        let fender_failed = match fender {
            Some(expr) => matches!(eval(expr, &child, out)?, Value::Boolean(false)),
            None => false,
        };
        if fender_failed {
            continue;
        }
        return Ok(TailAction::TailEval(output.clone(), child));
    }
    Err(EvalError::Parse {
        message: "syntax-case: no matching pattern".into(),
    })
}

/// Install syntax-case bindings into an environment, tracking pvar names.
fn install_syntax_bindings(
    child: &Rc<RefCell<Env>>,
    bindings: &macros::Bindings,
    parent_env: &Rc<RefCell<Env>>,
) {
    // Collect existing pvars from parent scope (for nested syntax-case / with-syntax)
    let mut pvar_names: Vec<Value> = parent_env
        .borrow()
        .get("__sc_pvars__")
        .and_then(|v| match v {
            Value::List(items) => Some(items),
            _ => None,
        })
        .unwrap_or_default();
    let mut repeated_names: Vec<Value> = parent_env
        .borrow()
        .get("__sc_repeated__")
        .and_then(|v| match v {
            Value::List(items) => Some(items),
            _ => None,
        })
        .unwrap_or_default();

    for (name, binding) in bindings {
        match binding {
            macros::Binding::Single(val) => {
                child.borrow_mut().define(name.clone(), val.clone());
                pvar_names.push(Value::Symbol(name.clone()));
            }
            macros::Binding::Repeated(vals) => {
                child
                    .borrow_mut()
                    .define(name.clone(), Value::List(vals.clone()));
                pvar_names.push(Value::Symbol(name.clone()));
                repeated_names.push(Value::Symbol(name.clone()));
            }
        }
    }
    child
        .borrow_mut()
        .define("__sc_pvars__".into(), Value::List(pvar_names));
    child
        .borrow_mut()
        .define("__sc_repeated__".into(), Value::List(repeated_names));
}

/// Evaluate `(syntax <template>)` — template expansion with pattern variable substitution.
/// Unlike syntax-rules `instantiate`, hygiene bindings are defined directly in the
/// environment rather than wrapping in `let`, so `define` forms work correctly.
fn eval_syntax(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [template] = args else {
        return Err(EvalError::Parse {
            message: "syntax: expected one argument".into(),
        });
    };
    let bindings = collect_syntax_bindings(env);

    let mut counter = out.borrow().gensym_counter;
    let pattern_vars: std::collections::HashSet<&String> = bindings.keys().collect();
    let mut gensym_map = std::collections::HashMap::new();
    macros::collect_introduced(template, &pattern_vars, &mut gensym_map, &mut counter);
    let expanded = macros::substitute(template, &bindings, &gensym_map)?;
    out.borrow_mut().gensym_counter = counter;

    // Store gensym map in shared state so call_transformer can apply
    // hygiene bindings to the invocation env.
    out.borrow_mut()
        .pending_hygiene
        .extend(gensym_map);

    Ok(expanded)
}

/// Collect pattern variable bindings from the environment's __sc_pvars__/__sc_repeated__.
fn collect_syntax_bindings(env: &Rc<RefCell<Env>>) -> macros::Bindings {
    let pvar_list = env
        .borrow()
        .get("__sc_pvars__")
        .unwrap_or(Value::List(vec![]));
    let repeated_list = env
        .borrow()
        .get("__sc_repeated__")
        .unwrap_or(Value::List(vec![]));

    let pvar_names: Vec<String> = match &pvar_list {
        Value::List(items) => items
            .iter()
            .filter_map(|v| match v {
                Value::Symbol(s) => Some(s.clone()),
                _ => None,
            })
            .collect(),
        _ => vec![],
    };
    let repeated_names: Vec<String> = match &repeated_list {
        Value::List(items) => items
            .iter()
            .filter_map(|v| match v {
                Value::Symbol(s) => Some(s.clone()),
                _ => None,
            })
            .collect(),
        _ => vec![],
    };

    let mut bindings = std::collections::HashMap::new();
    for name in &pvar_names {
        let Some(val) = env.borrow().get(name) else {
            continue;
        };
        let binding = if repeated_names.contains(name) {
            let vals = match val {
                Value::List(items) => items,
                _ => vec![val],
            };
            macros::Binding::Repeated(vals)
        } else {
            macros::Binding::Single(val)
        };
        bindings.insert(name.clone(), binding);
    }
    bindings
}

/// Evaluate `(with-syntax ((<pattern> <expr>) ...) <body> ...)`.
fn eval_with_syntax_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "with-syntax: expected bindings and body".into(),
        });
    }
    let Value::List(binding_forms) = &args[0] else {
        return Err(EvalError::Parse {
            message: "with-syntax: expected binding list".into(),
        });
    };

    let child = Env::extend(env);
    let mut all_bindings = std::collections::HashMap::new();

    for form in binding_forms {
        let Value::List(parts) = form else {
            return Err(EvalError::Parse {
                message: "with-syntax: expected (pattern expr)".into(),
            });
        };
        let [pattern, expr] = parts.as_slice() else {
            return Err(EvalError::Parse {
                message: "with-syntax: expected (pattern expr)".into(),
            });
        };
        let val = eval(expr, env, out)?;
        if let Value::Symbol(name) = pattern {
            all_bindings.insert(name.clone(), macros::Binding::Single(val));
        } else if let Some(bindings) =
            macros::syntax_case_match(pattern, &val, &[])
        {
            all_bindings.extend(bindings);
        }
    }

    install_syntax_bindings(&child, &all_bindings, env);
    eval_body_tail(&args[1..], &child, out)
}

fn eval_define(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match args {
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env, out)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        [Value::List(sig), body @ ..] if !sig.is_empty() => {
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::Parse {
                    message: "define: expected function name".into(),
                });
            };
            let (params, rest_param) = parse_params(&sig[1..])?;
            let lambda = Value::Lambda {
                params,
                rest_param,
                body: body.to_vec(),
                closure: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse {
            message: "define: bad syntax".into(),
        }),
    }
}

fn eval_define_syntax(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [Value::Symbol(name), transformer] = args else {
        return Err(EvalError::Parse {
            message: "define-syntax: expected name and transformer".into(),
        });
    };
    let Value::List(form) = transformer else {
        return Err(EvalError::Parse {
            message: "define-syntax: expected syntax-rules or lambda".into(),
        });
    };
    let macro_val = match form.first() {
        Some(Value::Symbol(s)) if s == "syntax-rules" => {
            parse_syntax_rules(form, env)?
        }
        Some(Value::Symbol(s)) if s == "lambda" => {
            let lambda_val = eval(transformer, env, out)?;
            Value::TransformerMacro {
                transformer: Box::new(lambda_val),
            }
        }
        _ => {
            return Err(EvalError::Parse {
                message: "define-syntax: expected syntax-rules or lambda".into(),
            });
        }
    };
    env.borrow_mut().define(name.clone(), macro_val);
    Ok(Value::Void)
}

fn parse_syntax_rules(
    sr_form: &[Value],
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    let Value::List(lit_list) = &sr_form[1] else {
        return Err(EvalError::Parse {
            message: "syntax-rules: expected literal list".into(),
        });
    };
    let literals: Vec<String> = lit_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse {
                message: "syntax-rules: literals must be symbols".into(),
            }),
        })
        .collect::<Result<_, _>>()?;
    let rules: Vec<(Vec<Value>, Value)> = sr_form[2..]
        .iter()
        .map(|rule| {
            let Value::List(parts) = rule else {
                return Err(EvalError::Parse {
                    message: "syntax-rules: expected (pattern template)".into(),
                });
            };
            let [Value::List(pattern), template] = parts.as_slice() else {
                return Err(EvalError::Parse {
                    message: "syntax-rules: expected (pattern template)".into(),
                });
            };
            Ok((pattern.clone(), template.clone()))
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Macro {
        literals,
        rules,
        def_env: Rc::clone(env),
    })
}

/// Implement R7RS `define-record-type`.
/// Syntax: (define-record-type <name> (<constructor> <field-name> ...) <predicate> (<field-name> <accessor>) ...)
fn eval_define_record_type(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    // args: [<name>, (<constructor> <field-names>...), <predicate>, (<field> <accessor>)...]
    if args.len() < 3 {
        return Err(EvalError::Parse {
            message: "define-record-type: expected at least type name, constructor, and predicate"
                .into(),
        });
    }
    let Value::Symbol(type_name) = &args[0] else {
        return Err(EvalError::Parse {
            message: "define-record-type: expected type name symbol".into(),
        });
    };
    let Value::List(ctor_form) = &args[1] else {
        return Err(EvalError::Parse {
            message: "define-record-type: expected constructor form".into(),
        });
    };
    if ctor_form.is_empty() {
        return Err(EvalError::Parse {
            message: "define-record-type: constructor form must have a name".into(),
        });
    }
    let Value::Symbol(ctor_name) = &ctor_form[0] else {
        return Err(EvalError::Parse {
            message: "define-record-type: constructor name must be a symbol".into(),
        });
    };
    let ctor_fields: Vec<String> = ctor_form[1..]
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse {
                message: "define-record-type: constructor field must be a symbol".into(),
            }),
        })
        .collect::<Result<_, _>>()?;

    let Value::Symbol(pred_name) = &args[2] else {
        return Err(EvalError::Parse {
            message: "define-record-type: predicate must be a symbol".into(),
        });
    };

    // Parse field specs: (<field-name> <accessor-name>)
    let field_specs: Vec<(String, String)> = args[3..]
        .iter()
        .map(|spec| {
            let Value::List(parts) = spec else {
                return Err(EvalError::Parse {
                    message: "define-record-type: field spec must be a list".into(),
                });
            };
            if parts.len() < 2 {
                return Err(EvalError::Parse {
                    message: "define-record-type: field spec must have name and accessor".into(),
                });
            }
            let Value::Symbol(fname) = &parts[0] else {
                return Err(EvalError::Parse {
                    message: "define-record-type: field name must be a symbol".into(),
                });
            };
            let Value::Symbol(accessor) = &parts[1] else {
                return Err(EvalError::Parse {
                    message: "define-record-type: accessor must be a symbol".into(),
                });
            };
            Ok((fname.clone(), accessor.clone()))
        })
        .collect::<Result<_, _>>()?;

    // Allocate unique type ID
    let type_id = {
        let mut st = out.borrow_mut();
        let id = st.next_record_type_id;
        st.next_record_type_id += 1;
        id
    };

    let type_name_owned = type_name.clone();

    // Define constructor as a lambda that calls internal record-creation builtin
    {
        let params = ctor_fields.clone();
        let mut body_list = vec![
            Value::Symbol("__make-record-internal".into()),
            Value::Integer(type_id as i64),
            Value::Str(type_name_owned.clone()),
        ];
        // Add field names as strings
        for fname in &ctor_fields {
            body_list.push(Value::Str(fname.clone()));
        }
        // Add field value references
        for fname in &ctor_fields {
            body_list.push(Value::Symbol(fname.clone()));
        }
        let body = vec![Value::List(body_list)];
        let ctor_val = Value::Lambda {
            params,
            rest_param: None,
            body,
            closure: Rc::clone(env),
        };
        env.borrow_mut().define(ctor_name.clone(), ctor_val);
    }

    // Predicate lambda: (lambda (x) (__record-predicate-internal type_id x))
    {
        let pred_val = Value::Lambda {
            params: vec!["__x".into()],
            rest_param: None,
            body: vec![Value::List(vec![
                Value::Symbol("__record-predicate-internal".into()),
                Value::Integer(type_id as i64),
                Value::Symbol("__x".into()),
            ])],
            closure: Rc::clone(env),
        };
        env.borrow_mut().define(pred_name.clone(), pred_val);
    }

    // Accessor lambdas
    for (field_name, accessor_name) in &field_specs {
        let acc_val = Value::Lambda {
            params: vec!["__x".into()],
            rest_param: None,
            body: vec![Value::List(vec![
                Value::Symbol("__record-accessor-internal".into()),
                Value::Integer(type_id as i64),
                Value::Str(field_name.clone()),
                Value::Symbol("__x".into()),
            ])],
            closure: Rc::clone(env),
        };
        env.borrow_mut()
            .define(accessor_name.clone(), acc_val);
    }

    Ok(Value::Void)
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [datum] = args else {
        return Err(EvalError::Parse {
            message: "quote: expected exactly 1 argument".into(),
        });
    };
    Ok(datum.clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "lambda: expected parameters and body".into(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::Parse {
            message: "lambda: expected parameter list".into(),
        });
    };
    let (params, rest_param) = parse_params(param_list)?;
    Ok(Value::Lambda {
        params,
        rest_param,
        body: args[1..].to_vec(),
        closure: Rc::clone(env),
    })
}

fn eval_not(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_args_as_nums(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Vec<Num>, EvalError> {
    args.iter()
        .map(|a| {
            let val = eval(a, env, out)?;
            value_to_num(&val)
        })
        .collect()
}

fn eval_add(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_nums(args, env, out)?;
    let result = nums.into_iter().fold(Num::Exact(0, 1), num_add);
    Ok(num_to_value(result))
}

fn eval_sub(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_nums(args, env, out)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount { expected: 1, got: 0 }),
        [single] => Ok(num_to_value(num_neg(*single))),
        [first, rest @ ..] => {
            let result = rest.iter().fold(*first, |acc, n| num_sub(acc, *n));
            Ok(num_to_value(result))
        }
    }
}

fn eval_mul(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_nums(args, env, out)?;
    let result = nums.into_iter().fold(Num::Exact(1, 1), num_mul);
    Ok(num_to_value(result))
}

fn eval_div(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_nums(args, env, out)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount { expected: 1, got: 0 }),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(*first, |acc, n| num_div(acc, *n))
            .map(num_to_value),
    }
}

fn eval_cmp(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    cmp: fn(f64, f64) -> bool,
) -> Result<Value, EvalError> {
    let nums = eval_args_as_nums(args, env, out)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    }
    let floats: Vec<f64> = nums.iter().map(num_to_f64).collect();
    let result = floats.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Boolean(result))
}

fn eval_cons(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let head_val = eval(head, env, out)?;
    let tail_val = eval(tail, env, out)?;
    Ok(make_pair(head_val, tail_val))
}

fn eval_car(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    get_car(&val)
}

fn get_car(val: &Value) -> Result<Value, EvalError> {
    match val {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(p) => Ok(p.borrow().0.clone()),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
}

fn get_cdr(val: &Value) -> Result<Value, EvalError> {
    match val {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(p) => Ok(p.borrow().1.clone()),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_cdr(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    get_cdr(&val)
}

fn eval_null_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(
        matches!(val, Value::List(ref items) if items.is_empty()),
    ))
}

fn eval_list_builtin(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::List(vec![]));
    }
    let items: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;
    Ok(list_from_vec(items))
}

fn eval_length(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    let items = collect_list(&val)?;
    Ok(Value::Integer(items.len() as i64))
}

/// Collect elements from a proper list (either Value::List or Pair chain).
fn collect_list(val: &Value) -> Result<Vec<Value>, EvalError> {
    match val {
        Value::List(items) => Ok(items.clone()),
        Value::Pair(_) => collect_pair_list(val),
        _ => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{val}"),
        }),
    }
}

fn collect_pair_list(val: &Value) -> Result<Vec<Value>, EvalError> {
    let mut elems = Vec::new();
    let mut cur = val.clone();
    loop {
        match &cur {
            Value::Pair(p) => {
                let inner = p.borrow();
                let car = inner.0.clone();
                let cdr = inner.1.clone();
                drop(inner);
                elems.push(car);
                cur = cdr;
            }
            Value::List(items) if items.is_empty() => break,
            _ => {
                return Err(EvalError::TypeError {
                    expected: "proper list".into(),
                    got: format!("{val}"),
                });
            }
        }
    }
    Ok(elems)
}

fn eval_reverse(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    let items = collect_list(&val)?;
    let reversed = items
        .into_iter()
        .fold(Value::List(vec![]), |acc, item| make_pair(item, acc));
    Ok(reversed)
}

fn eval_type_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    pred: fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(pred(&val)))
}

// --- L05: Output builtins ---

fn eval_display(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    out.borrow_mut().output.push_str(&val.display_value());
    Ok(Value::Void)
}

fn eval_write(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    out.borrow_mut().output.push_str(&val.to_string());
    Ok(Value::Void)
}

fn eval_newline(args: &[Value], out: &Output) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 0,
            got: args.len(),
        });
    }
    out.borrow_mut().output.push('\n');
    Ok(Value::Void)
}

// --- L05: String/symbol operations ---

fn eval_string_append(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let mut result = String::new();
    for arg in args {
        match eval(arg, env, out)? {
            Value::Str(s) => result.push_str(&s),
            other => {
                return Err(EvalError::TypeError {
                    expected: "string".into(),
                    got: format!("{other}"),
                })
            }
        }
    }
    Ok(Value::Str(result))
}

fn eval_string_length(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_substring(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [s_arg, start_arg, end_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let s = match eval(s_arg, env, out)? {
        Value::Str(s) => s,
        other => {
            return Err(EvalError::TypeError {
                expected: "string".into(),
                got: format!("{other}"),
            })
        }
    };
    let start = match eval(start_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
    };
    let end = match eval(end_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
    };
    Ok(Value::Str(s[start..end].to_string()))
}

fn eval_string_to_number(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_number_to_string(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Integer(n) => Ok(Value::Str(n.to_string())),
        other => Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_symbol_to_string(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Symbol(s) => Ok(Value::Str(s)),
        other => Err(EvalError::TypeError {
            expected: "symbol".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_string_to_symbol(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => Ok(Value::Symbol(s)),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_string_ref(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [s_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let s = match eval(s_arg, env, out)? {
        Value::Str(s) => s,
        other => {
            return Err(EvalError::TypeError {
                expected: "string".into(),
                got: format!("{other}"),
            })
        }
    };
    let idx = match eval(idx_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
    };
    let ch = s
        .chars()
        .nth(idx)
        .ok_or_else(|| EvalError::TypeError {
            expected: format!("index < {}", s.len()),
            got: format!("{idx}"),
        })?;
    Ok(Value::Char(ch))
}

fn eval_string_copy(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => Ok(Value::Str(s)),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_string_to_list(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => Ok(list_from_vec(s.chars().map(Value::Char).collect())),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_list_to_string(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let list_val = eval(arg, env, out)?;
    let items = collect_list(&list_val)?;
    let s: String = items
        .iter()
        .map(|v| match v {
            Value::Char(c) => Ok(*c),
            other => Err(EvalError::TypeError {
                expected: "char".into(),
                got: format!("{other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Str(s))
}

fn eval_char_to_integer(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Char(c) => Ok(Value::Integer(c as i64)),
        other => Err(EvalError::TypeError {
            expected: "char".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_integer_to_char(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Integer(n) => {
            let ch = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
                expected: "valid unicode code point".into(),
                got: format!("{n}"),
            })?;
            Ok(Value::Char(ch))
        }
        other => Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{other}"),
        }),
    }
}

/// Apply a procedure to already-evaluated arguments (no re-evaluation).
fn apply_proc_evaluated(
    proc: &Value,
    eval_args: Vec<Value>,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => {
            check_arity(params.len(), rest_param.is_some(), eval_args.len())?;
            let child = Env::extend(closure);
            for (param, val) in params.iter().zip(&eval_args) {
                child.borrow_mut().define(param.clone(), val.clone());
            }
            if let Some(rest_name) = rest_param {
                let rest_vals = eval_args[params.len()..].to_vec();
                child.borrow_mut().define(rest_name.clone(), list_from_vec(rest_vals));
            }
            eval_body(body, &child, out)
        }
        Value::Builtin(name) => call_builtin_with_values(name, eval_args, env, out),
        Value::Continuation(id) => {
            let value = continuation_value_from_args(eval_args);
            Err(EvalError::ContinuationReturn {
                id: *id,
                value: Box::new(value),
            })
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

fn eval_map(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    }
    let proc = eval(&args[0], env, out)?;
    let lists: Vec<Vec<Value>> = args[1..]
        .iter()
        .map(|a| {
            let val = eval(a, env, out)?;
            collect_list(&val)
        })
        .collect::<Result<_, EvalError>>()?;
    let len = lists[0].len();
    let results: Vec<Value> = (0..len)
        .map(|i| {
            let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
            apply_proc_evaluated(&proc, call_args, env, out)
        })
        .collect::<Result<_, _>>()?;
    Ok(list_from_vec(results))
}

// --- Level 14: letrec, letrec*, case, equal?, eqv?, vectors ---

fn eval_letrec_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "letrec: expected bindings and body".into(),
        });
    }
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::Parse {
            message: "letrec: expected binding list".into(),
        });
    };
    let child = Env::extend(env);
    // First pass: bind all names to Void so they're visible
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for binding in bindings {
        let (name, expr) = parse_let_binding(binding)?;
        child.borrow_mut().define(name.to_string(), Value::Void);
        names.push(name.to_string());
        init_exprs.push(expr.clone());
    }
    // Second pass: evaluate inits in the child env and set values
    for (name, init_expr) in names.iter().zip(&init_exprs) {
        let val = eval(init_expr, &child, out)?;
        child.borrow_mut().define(name.clone(), val);
    }
    eval_body_tail(&args[1..], &child, out)
}

fn eval_letrec_star_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "letrec*: expected bindings and body".into(),
        });
    }
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::Parse {
            message: "letrec*: expected binding list".into(),
        });
    };
    let child = Env::extend(env);
    for binding in bindings {
        let (name, expr) = parse_let_binding(binding)?;
        let val = eval(expr, &child, out)?;
        child.borrow_mut().define(name.to_string(), val);
    }
    eval_body_tail(&args[1..], &child, out)
}

fn eval_case_tail(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse {
            message: "case: expected key and clauses".into(),
        });
    }
    let key = eval(&args[0], env, out)?;
    for clause in &args[1..] {
        let Value::List(parts) = clause else {
            return Err(EvalError::Parse {
                message: "case: expected clause".into(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "case: empty clause".into(),
            });
        }
        if matches!(&parts[0], Value::Symbol(s) if s == "else") {
            return eval_body_tail(&parts[1..], env, out);
        }
        let Value::List(datums) = &parts[0] else {
            return Err(EvalError::Parse {
                message: "case: expected datum list".into(),
            });
        };
        if datums.iter().any(|d| key.eqv(d)) {
            return eval_body_tail(&parts[1..], env, out);
        }
    }
    Ok(TailAction::Return(Value::Void))
}

fn eval_equal(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let va = eval(a, env, out)?;
    let vb = eval(b, env, out)?;
    Ok(Value::Boolean(va.deep_equal(&vb)))
}

fn eval_eqv(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let va = eval(a, env, out)?;
    let vb = eval(b, env, out)?;
    Ok(Value::Boolean(va.eqv(&vb)))
}

fn eval_vector_create(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let items: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;
    Ok(Value::Vector(Rc::new(RefCell::new(items))))
}

fn eval_make_vector(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let (size, fill) = match args {
        [size_arg] => (eval(size_arg, env, out)?, Value::Integer(0)),
        [size_arg, fill_arg] => (eval(size_arg, env, out)?, eval(fill_arg, env, out)?),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 1,
                got: args.len(),
            })
        }
    };
    let Value::Integer(n) = size else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{size}"),
        });
    };
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; n as usize]))))
}

fn eval_vector_ref(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [vec_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let vec_val = eval(vec_arg, env, out)?;
    let idx_val = eval(idx_arg, env, out)?;
    let Value::Vector(v) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{idx_val}"),
        });
    };
    let items = v.borrow();
    items.get(idx as usize).cloned().ok_or_else(|| EvalError::TypeError {
        expected: format!("index in range 0..{}", items.len()),
        got: format!("{idx}"),
    })
}

fn eval_vector_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [vec_arg, idx_arg, val_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let vec_val = eval(vec_arg, env, out)?;
    let idx_val = eval(idx_arg, env, out)?;
    let new_val = eval(val_arg, env, out)?;
    let Value::Vector(v) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{idx_val}"),
        });
    };
    let mut items = v.borrow_mut();
    let i = idx as usize;
    if i >= items.len() {
        return Err(EvalError::TypeError {
            expected: format!("index in range 0..{}", items.len()),
            got: format!("{idx}"),
        });
    }
    items[i] = new_val;
    Ok(Value::Void)
}

fn eval_vector_length(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [vec_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let vec_val = eval(vec_arg, env, out)?;
    let Value::Vector(v) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    let len = v.borrow().len() as i64;
    Ok(Value::Integer(len))
}

fn eval_vector_to_list(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [vec_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let vec_val = eval(vec_arg, env, out)?;
    let Value::Vector(v) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    let items = v.borrow().clone();
    Ok(list_from_vec(items))
}

fn eval_list_to_vector(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [list_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let list_val = eval(list_arg, env, out)?;
    let items = collect_list(&list_val)?;
    let vec = Rc::new(RefCell::new(items));
    Ok(Value::Vector(vec))
}

// --- Level 15: Numeric/Char/String Utilities ---

fn eval_abs(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Integer(n) = eval(arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    Ok(Value::Integer(n.abs()))
}

fn eval_modulo(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Integer(x) = eval(a, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    let Value::Integer(y) = eval(b, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    let r = x % y;
    let result = if r != 0 && (r > 0) != (y > 0) { r + y } else { r };
    Ok(Value::Integer(result))
}

fn eval_remainder(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Integer(x) = eval(a, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    let Value::Integer(y) = eval(b, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    Ok(Value::Integer(x % y))
}

fn eval_quotient(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Integer(x) = eval(a, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    let Value::Integer(y) = eval(b, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    Ok(Value::Integer(x / y))
}

fn eval_min_max(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    is_min: bool,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
    }
    let values: Vec<i64> = args
        .iter()
        .map(|a| match eval(a, env, out)? {
            Value::Integer(n) => Ok(n),
            other => Err(EvalError::TypeError {
                expected: "number".into(),
                got: format!("{other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    let result = if is_min {
        values.iter().copied().min().expect("non-empty")
    } else {
        values.iter().copied().max().expect("non-empty")
    };
    Ok(Value::Integer(result))
}

fn eval_expt(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [base_arg, exp_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Integer(base) = eval(base_arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    let Value::Integer(exp) = eval(exp_arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    Ok(Value::Integer(base.pow(exp as u32)))
}

fn eval_num_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    pred: fn(i64) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Integer(n) = eval(arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    Ok(Value::Boolean(pred(n)))
}

fn eval_list_ref(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [list_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let list_val = eval(list_arg, env, out)?;
    let items = collect_list(&list_val)?;
    let Value::Integer(idx) = eval(idx_arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    items
        .get(idx as usize)
        .cloned()
        .ok_or_else(|| EvalError::Parse {
            message: format!("list-ref: index {idx} out of range"),
        })
}

fn eval_list_tail_builtin(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [list_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let list_val = eval(list_arg, env, out)?;
    let items = collect_list(&list_val)?;
    let Value::Integer(idx) = eval(idx_arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "number".into(), got: "non-number".into() });
    };
    Ok(list_from_vec(items[idx as usize..].to_vec()))
}

fn eval_list_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(is_proper_list(&val)))
}

/// Check if a value is a proper list (ends with nil), with cycle detection.
fn is_proper_list(val: &Value) -> bool {
    match val {
        Value::List(_) => true,
        Value::Pair(_) => is_proper_list_tortoise_hare(val),
        _ => false,
    }
}

/// Tortoise-and-hare cycle detection for pair-chain lists.
fn is_proper_list_tortoise_hare(val: &Value) -> bool {
    let mut slow = val.clone();
    let mut fast = val.clone();
    loop {
        slow = match &slow {
            Value::Pair(p) => p.borrow().1.clone(),
            Value::List(_) => return true,
            _ => return false,
        };
        // Advance fast by two steps (unrolled to avoid nesting)
        fast = match &fast {
            Value::Pair(p) => p.borrow().1.clone(),
            Value::List(_) => return true,
            _ => return false,
        };
        fast = match &fast {
            Value::Pair(p) => p.borrow().1.clone(),
            Value::List(_) => return true,
            _ => return false,
        };
        let is_cycle = matches!(
            (&slow, &fast),
            (Value::Pair(s), Value::Pair(f)) if Rc::ptr_eq(s, f)
        );
        if is_cycle {
            return false;
        }
    }
}

fn eval_assoc(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [key_arg, list_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let key = eval(key_arg, env, out)?;
    let list_val = eval(list_arg, env, out)?;
    let items = collect_list(&list_val)?;
    let found = items.into_iter().find(|item| {
        if let Ok(pair_items) = collect_list(item) {
            !pair_items.is_empty() && pair_items[0].deep_equal(&key)
        } else {
            false
        }
    });
    Ok(found.unwrap_or(Value::Boolean(false)))
}

fn eval_char_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    pred: fn(char) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Char(c) = eval(arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "char".into(), got: "non-char".into() });
    };
    Ok(Value::Boolean(pred(c)))
}

fn eval_char_case(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    upcase: bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Char(c) = eval(arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "char".into(), got: "non-char".into() });
    };
    let result = if upcase {
        c.to_ascii_uppercase()
    } else {
        c.to_ascii_lowercase()
    };
    Ok(Value::Char(result))
}

fn eval_char_cmp(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    cmp: fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Char(ca) = eval(a, env, out)? else {
        return Err(EvalError::TypeError { expected: "char".into(), got: "non-char".into() });
    };
    let Value::Char(cb) = eval(b, env, out)? else {
        return Err(EvalError::TypeError { expected: "char".into(), got: "non-char".into() });
    };
    Ok(Value::Boolean(cmp(ca, cb)))
}

fn eval_string_cmp(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    cmp: fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Str(sa) = eval(a, env, out)? else {
        return Err(EvalError::TypeError { expected: "string".into(), got: "non-string".into() });
    };
    let Value::Str(sb) = eval(b, env, out)? else {
        return Err(EvalError::TypeError { expected: "string".into(), got: "non-string".into() });
    };
    Ok(Value::Boolean(cmp(&sa, &sb)))
}

fn eval_string_ci_eq(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Str(sa) = eval(a, env, out)? else {
        return Err(EvalError::TypeError { expected: "string".into(), got: "non-string".into() });
    };
    let Value::Str(sb) = eval(b, env, out)? else {
        return Err(EvalError::TypeError { expected: "string".into(), got: "non-string".into() });
    };
    Ok(Value::Boolean(sa.to_lowercase() == sb.to_lowercase()))
}

fn eval_string_case(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    upcase: bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Str(s) = eval(arg, env, out)? else {
        return Err(EvalError::TypeError { expected: "string".into(), got: "non-string".into() });
    };
    let result = if upcase { s.to_uppercase() } else { s.to_lowercase() };
    Ok(Value::Str(result))
}

// --- L18: values & call-with-values ---

fn eval_values(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let vals: Vec<Value> = args.iter().map(|a| eval(a, env, out)).collect::<Result<_, _>>()?;
    eval_values_core(vals)
}

fn eval_values_core(vals: Vec<Value>) -> Result<Value, EvalError> {
    match vals.len() {
        1 => Ok(vals.into_iter().next().expect("checked length")),
        _ => Ok(Value::Values(vals)),
    }
}

fn eval_call_with_values(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [producer_expr, consumer_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let producer = eval(producer_expr, env, out)?;
    let consumer = eval(consumer_expr, env, out)?;
    eval_call_with_values_core(&producer, &consumer, env, out)
}

fn eval_call_with_values_core(
    producer: &Value,
    consumer: &Value,
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let produced = apply_values(producer, vec![], env, out)?;
    let args = match produced {
        Value::Values(vals) => vals,
        single => vec![single],
    };
    apply_values(consumer, args, env, out)
}

fn eval_exact_to_inexact(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    number::exact_to_inexact(&val)
}

fn eval_inexact_to_exact(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    number::inexact_to_exact(&val)
}

fn eval_numerator(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    match eval(arg, env, out)? {
        Value::Integer(n) => Ok(Value::Integer(n)),
        Value::Rational(n, _) => Ok(Value::Integer(n)),
        other => Err(EvalError::TypeError {
            expected: "rational".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_denominator(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    match eval(arg, env, out)? {
        Value::Integer(_) => Ok(Value::Integer(1)),
        Value::Rational(_, d) => Ok(Value::Integer(d)),
        other => Err(EvalError::TypeError {
            expected: "rational".into(),
            got: format!("{other}"),
        }),
    }
}

// --- Level 21: Pair Mutation & Cycle Detection ---

fn eval_set_car(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [pair_arg, val_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let pair_val = eval(pair_arg, env, out)?;
    let new_val = eval(val_arg, env, out)?;
    let Value::Pair(p) = pair_val else {
        return Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{pair_val}"),
        });
    };
    p.borrow_mut().0 = new_val;
    Ok(Value::Void)
}

fn eval_set_cdr(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [pair_arg, val_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let pair_val = eval(pair_arg, env, out)?;
    let new_val = eval(val_arg, env, out)?;
    let Value::Pair(p) = pair_val else {
        return Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{pair_val}"),
        });
    };
    p.borrow_mut().1 = new_val;
    Ok(Value::Void)
}

fn eval_cddr(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    let cdr1 = get_cdr(&val)?;
    get_cdr(&cdr1)
}

fn eval_cadr(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    let cdr1 = get_cdr(&val)?;
    get_car(&cdr1)
}

fn eval_caar(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    let car1 = get_car(&val)?;
    get_car(&car1)
}

fn eval_cdar(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    let car1 = get_car(&val)?;
    get_cdr(&car1)
}

/// Seed all builtin procedures into the environment as first-class values.
pub fn seed_builtins(env: &Rc<RefCell<Env>>) {
    let names = [
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "string-copy", "string->list", "list->string",
        "char->integer", "integer->char",
        "map", "apply",
        "equal?", "eqv?", "eq?",
        "vector", "make-vector", "vector-ref", "vector-set!",
        "vector-length", "vector?", "vector->list", "list->vector",
        "call/cc", "call-with-current-continuation",
        "dynamic-wind",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "reverse",
        "list-ref", "list-tail", "list?", "assoc",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?",
        "string-upcase", "string-downcase",
        "values", "call-with-values",
        "exact?", "inexact?", "rational?", "integer?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
        "set-car!", "set-cdr!", "cddr", "cadr", "caar", "cdar",
    ];
    let mut env_ref = env.borrow_mut();
    for name in names {
        env_ref.define(name.to_string(), Value::Builtin(name.to_string()));
    }
}
