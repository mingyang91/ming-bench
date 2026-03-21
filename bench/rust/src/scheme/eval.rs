use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Shared interpreter state: output buffer + continuation registry.
pub struct InterpState {
    pub output: String,
    pub next_cont_id: u64,
    pub cont_captures: HashMap<u64, usize>,
    pub resume: Option<Value>,
    pub current_expr_idx: usize,
}

impl InterpState {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            next_cont_id: 0,
            cont_captures: HashMap::new(),
            resume: None,
            current_expr_idx: 0,
        }
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

/// Evaluate a single parsed expression in the given environment.
/// Uses a trampoline loop for tail call optimization.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = Rc::clone(env);

    loop {
        match &current_expr {
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_)
            | Value::Void => return Ok(current_expr),
            Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_) => {
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
            "call/cc" | "call-with-current-continuation" => {
                return eval_callcc(&items[1..], env, out).map(TailAction::Return)
            }
            s if is_builtin(s) => {
                return eval_builtin(s, &items[1..], env, out).map(TailAction::Return)
            }
            _ => {}
        }
    }

    eval_application_tail(items, env, out)
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
    if let Some(value) = out.borrow_mut().resume.take() {
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
        st.next_cont_id += 1;
        st.cont_captures.insert(id, expr_idx);
        id
    };
    let cont = Value::Continuation(id);
    apply_values(&proc, vec![cont], env, out)
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
                child.borrow_mut().define(rest_name.clone(), Value::List(rest_vals));
            }
            eval_body(body, &child, out)
        }
        Value::Builtin(name) => call_builtin_with_values(name, args, env, out),
        Value::Continuation(id) => {
            let [value] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            Err(EvalError::ContinuationReturn {
                id: *id,
                value: Box::new(value.clone()),
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
        if let Some(value) = out.borrow_mut().resume.take() {
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
            let [value] = args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            Err(EvalError::ContinuationReturn {
                id,
                value: Box::new(value.clone()),
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
        child.borrow_mut().define(rest_name, Value::List(rest_vals));
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
        if let Some(value) = out.borrow_mut().resume.take() {
            return Ok(value);
        }
        return eval_callcc_core(proc.clone(), env, out);
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
    let Value::List(tail_args) = &args[args.len() - 1] else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{}", args[args.len() - 1]),
        });
    };
    let mut combined: Vec<Value> = args[1..args.len() - 1].to_vec();
    combined.extend(tail_args.iter().cloned());

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
                child.borrow_mut().define(rest_name, Value::List(rest_vals));
            }
            eval_body(&body, &child, out)
        }
        Value::Builtin(name) => call_builtin_with_values(&name, combined, env, out),
        Value::Continuation(id) => {
            let [value] = combined.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: combined.len(),
                });
            };
            Err(EvalError::ContinuationReturn {
                id,
                value: Box::new(value.clone()),
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

/// Evaluate a body sequence, returning a tail action for the last expression.
fn eval_body_tail(
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<TailAction, EvalError> {
    match body.split_last() {
        None => Ok(TailAction::Return(Value::Void)),
        Some((last, rest)) => {
            for expr in rest {
                eval(expr, env, out)?;
            }
            Ok(TailAction::TailEval(last.clone(), Rc::clone(env)))
        }
    }
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
    )
}

/// Non-tail call into a procedure (used by map and similar).
fn call_proc(
    proc: &Value,
    args: &[Value],
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
            let eval_args: Vec<Value> = args
                .iter()
                .map(|a| eval(a, env, out))
                .collect::<Result<_, _>>()?;
            check_arity(params.len(), rest_param.is_some(), eval_args.len())?;
            let child = Env::extend(closure);
            for (param, val) in params.iter().zip(&eval_args) {
                child.borrow_mut().define(param.clone(), val.clone());
            }
            if let Some(rest_name) = rest_param {
                let rest_vals = eval_args[params.len()..].to_vec();
                child.borrow_mut().define(rest_name.clone(), Value::List(rest_vals));
            }
            eval_body(body, &child, out)
        }
        Value::Builtin(name) => eval_builtin(name, args, env, out),
        Value::Continuation(id) => {
            let eval_args: Vec<Value> = args
                .iter()
                .map(|a| eval(a, env, out))
                .collect::<Result<_, _>>()?;
            let [value] = eval_args.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: eval_args.len(),
                });
            };
            Err(EvalError::ContinuationReturn {
                id: *id,
                value: Box::new(value.clone()),
            })
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
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
        "number?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Integer(_))),
        "boolean?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Boolean(_))),
        "pair?" => eval_type_pred(args, env, out, |v| {
            matches!(v, Value::List(items) if !items.is_empty())
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
        _ => Err(EvalError::UnknownProcedure {
            name: name.into(),
        }),
    }
}

/// Evaluate a sequence of body expressions, returning the last.
fn eval_body(body: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in body {
        result = eval(expr, env, out)?;
    }
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

fn eval_args_as_integers(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| {
            let val = eval(a, env, out)?;
            match val {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{other}"),
                }),
            }
        })
        .collect()
}

fn eval_add(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn eval_sub(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => Ok(Value::Integer(-single)),
        [first, rest @ ..] => Ok(Value::Integer(rest.iter().fold(*first, |acc, n| acc - n))),
    }
}

fn eval_mul(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn eval_div(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(*first, |acc, n| {
                if *n == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(acc / n)
                }
            })
            .map(Value::Integer),
    }
}

fn eval_cmp(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    }
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
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
    match tail_val {
        Value::List(mut items) => {
            items.insert(0, head_val);
            Ok(Value::List(items))
        }
        _ => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{tail_val}"),
        }),
    }
}

fn eval_car(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::List(items) if !items.is_empty() => Ok(items.into_iter().next().expect("non-empty")),
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
    match eval(arg, env, out)? {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
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
    let items: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;
    Ok(Value::List(items))
}

fn eval_length(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{other}"),
        }),
    }
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
        Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())),
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
    match eval(arg, env, out)? {
        Value::List(items) => {
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
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{other}"),
        }),
    }
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

fn eval_map(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [proc_arg, list_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let proc = eval(proc_arg, env, out)?;
    let list = match eval(list_arg, env, out)? {
        Value::List(items) => items,
        other => {
            return Err(EvalError::TypeError {
                expected: "list".into(),
                got: format!("{other}"),
            })
        }
    };
    let results: Vec<Value> = list
        .iter()
        .map(|item| call_proc(&proc, std::slice::from_ref(item), env, out))
        .collect::<Result<_, _>>()?;
    Ok(Value::List(results))
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
        "call/cc", "call-with-current-continuation",
    ];
    let mut env_ref = env.borrow_mut();
    for name in names {
        env_ref.define(name.to_string(), Value::Builtin(name.to_string()));
    }
}
