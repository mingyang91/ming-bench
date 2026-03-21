use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::{ContinuationData, Value};

/// Check if a name is a builtin procedure.
fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
            | "not"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
            | "char?"
            | "apply"
            | "map"
            | "display"
            | "write"
            | "newline"
            | "string-append"
            | "string-length"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "string->list"
            | "list->string"
            | "char->integer"
            | "integer->char"
            | "call/cc"
            | "call-with-current-continuation"
    )
}

/// Shared evaluation context threaded through all eval calls.
pub struct EvalContext {
    pub output: RefCell<String>,
    /// Pre-loaded return value for call/cc during continuation re-execution.
    pub callcc_override: RefCell<Option<Value>>,
    /// Data from a continuation invocation, read by eval_str after catching ContinuationReturn.
    pub cont_return_data: RefCell<Option<ContReturnData>>,
    /// Index of the top-level expression currently being evaluated.
    pub current_expr_idx: Cell<usize>,
    next_id: Cell<u64>,
}

pub struct ContReturnData {
    pub cont_id: u64,
    pub expr_idx: usize,
    pub value: Value,
}

impl EvalContext {
    pub fn new() -> Self {
        Self {
            output: RefCell::new(String::new()),
            callcc_override: RefCell::new(None),
            cont_return_data: RefCell::new(None),
            current_expr_idx: Cell::new(0),
            next_id: Cell::new(0),
        }
    }

    fn next_cont_id(&self) -> u64 {
        let id = self.next_id.get();
        self.next_id.set(id + 1);
        id
    }
}

/// Trampoline result: either a final value or a pending tail call.
enum Bounce {
    Done(Value),
    TailCall { expr: Value, env: Rc<RefCell<Env>> },
}

/// Evaluate a single expression in the given environment (trampoline loop).
pub fn eval(
    expr: &Value,
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = Rc::clone(env);

    loop {
        match &cur_expr {
            Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Char(_) => {
                return Ok(cur_expr);
            }
            Value::Symbol(name) => {
                return match cur_env.borrow().get(name) {
                    Some(val) => Ok(val),
                    None if is_builtin(name) => Ok(Value::Symbol(name.clone())),
                    None => Err(EvalError::UnboundVariable {
                        name: name.clone(),
                        span,
                    }),
                };
            }
            Value::List(elems) => match eval_list_tco(&elems.clone(), &cur_env, span, ctx)? {
                Bounce::Done(v) => return Ok(v),
                Bounce::TailCall { expr, env } => { cur_expr = expr; cur_env = env; }
            },
            Value::Lambda { .. } | Value::Continuation(_) => return Ok(cur_expr),
            Value::Void => return Ok(Value::Void),
        }
    }
}

fn eval_list_tco(
    elems: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [head, args @ ..] = elems else {
        return Ok(Bounce::Done(Value::List(vec![])));
    };

    if let Value::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and_tco(args, env, span, ctx),
            "or" => return eval_or_tco(args, env, span, ctx),
            "if" => return eval_if_tco(args, env, span, ctx),
            "define" => return eval_define(args, env, span, ctx).map(Bounce::Done),
            "quote" => return eval_quote(args, span).map(Bounce::Done),
            "lambda" => return eval_lambda(args, env, span).map(Bounce::Done),
            "let" => return eval_let_tco(args, env, span, ctx),
            "begin" => return eval_body_tco(args, env, span, ctx),
            "cond" => return eval_cond_tco(args, env, span, ctx),
            "set!" => return eval_set(args, env, span, ctx).map(Bounce::Done),
            "string-set!" => return eval_string_set(args, env, span, ctx).map(Bounce::Done),
            _ => {}
        }
    }

    let proc = eval(head, env, span, ctx)?;
    let evaluated_args: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, span, ctx))
        .collect::<Result<_, _>>()?;
    apply_tco(&proc, &evaluated_args, span, ctx)
}

/// Apply a procedure, returning a Bounce for TCO.
fn apply_tco(
    proc: &Value,
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    match proc {
        Value::Symbol(name) if name == "call/cc" || name == "call-with-current-continuation" => {
            let [lambda] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            eval_callcc(lambda, span, ctx).map(Bounce::Done)
        }
        Value::Symbol(name) => apply_builtin(name, args, span, ctx).map(Bounce::Done),
        Value::Lambda {
            params,
            rest_param,
            body,
            env,
        } => apply_lambda(params, rest_param.as_deref(), body, env, args, span, ctx),
        Value::Continuation(data) => {
            let [value] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            *ctx.cont_return_data.borrow_mut() = Some(ContReturnData {
                cont_id: data.id,
                expr_idx: data.expr_idx,
                value: value.clone(),
            });
            Err(EvalError::ContinuationReturn)
        }
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {other}"),
            span,
        }),
    }
}

/// Evaluate call/cc: capture continuation, call proc with it.
fn eval_callcc(
    proc: &Value,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    // During re-execution, return the pre-loaded value
    if let Some(val) = ctx.callcc_override.borrow_mut().take() {
        return Ok(val);
    }

    let id = ctx.next_cont_id();
    let cont = Value::Continuation(Rc::new(ContinuationData {
        id,
        expr_idx: ctx.current_expr_idx.get(),
    }));

    match apply(proc, &[cont], span, ctx) {
        Ok(val) => Ok(val),
        Err(EvalError::ContinuationReturn) => {
            let is_ours = ctx
                .cont_return_data
                .borrow()
                .as_ref()
                .is_some_and(|d| d.cont_id == id);
            if is_ours {
                let data = ctx.cont_return_data.borrow_mut().take()
                    .expect("cont_return_data should be present");
                Ok(data.value)
            } else {
                // Not our continuation, re-throw
                Err(EvalError::ContinuationReturn)
            }
        }
        Err(e) => Err(e),
    }
}

/// Apply a lambda procedure to arguments, binding params and returning a TCO bounce.
fn apply_lambda(
    params: &[String],
    rest_param: Option<&str>,
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    if let Some(rest_name) = rest_param {
        if args.len() < params.len() {
            return Err(EvalError::WrongArgCount {
                expected: params.len(),
                got: args.len(),
                span,
            });
        }
        let local_env = Env::with_parent(env);
        for (param, arg) in params.iter().zip(args) {
            local_env.borrow_mut().define(param.clone(), arg.clone());
        }
        let rest = Value::List(args[params.len()..].to_vec());
        local_env.borrow_mut().define(rest_name.to_string(), rest);
        eval_body_tco(body, &local_env, span, ctx)
    } else {
        if params.len() != args.len() {
            return Err(EvalError::WrongArgCount {
                expected: params.len(),
                got: args.len(),
                span,
            });
        }
        let local_env = Env::with_parent(env);
        for (param, arg) in params.iter().zip(args) {
            local_env.borrow_mut().define(param.clone(), arg.clone());
        }
        eval_body_tco(body, &local_env, span, ctx)
    }
}

/// Non-TCO apply for use in builtins like `map`.
fn apply(
    proc: &Value,
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match apply_tco(proc, args, span, ctx)? {
        Bounce::Done(v) => Ok(v),
        Bounce::TailCall { expr, env } => eval(&expr, &env, span, ctx),
    }
}

/// Evaluate a body (sequence of expressions) with TCO on the last one.
fn eval_body_tco(
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [init @ .., last] = body else {
        return Ok(Bounce::Done(Value::Void));
    };
    for expr in init {
        eval(expr, env, span, ctx)?;
    }
    Ok(Bounce::TailCall {
        expr: last.clone(),
        env: Rc::clone(env),
    })
}

fn eval_and_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    if args.is_empty() {
        return Ok(Bounce::Done(Value::Boolean(true)));
    }
    let [init @ .., last] = args else {
        unreachable!()
    };
    for arg in init {
        let val = eval(arg, env, span, ctx)?;
        if !is_truthy(&val) {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::TailCall {
        expr: last.clone(),
        env: Rc::clone(env),
    })
}

fn eval_or_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    if args.is_empty() {
        return Ok(Bounce::Done(Value::Boolean(false)));
    }
    let [init @ .., last] = args else {
        unreachable!()
    };
    for arg in init {
        let val = eval(arg, env, span, ctx)?;
        if is_truthy(&val) {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::TailCall {
        expr: last.clone(),
        env: Rc::clone(env),
    })
}

fn eval_if_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let (condition, consequent, alternate) = match args {
        [cond, cons, alt] => (cond, cons, Some(alt)),
        [cond, cons] => (cond, cons, None),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 2,
                got: args.len(),
                span,
            });
        }
    };

    if is_truthy(&eval(condition, env, span, ctx)?) {
        Ok(Bounce::TailCall {
            expr: consequent.clone(),
            env: Rc::clone(env),
        })
    } else if let Some(alt) = alternate {
        Ok(Bounce::TailCall {
            expr: alt.clone(),
            env: Rc::clone(env),
        })
    } else {
        Ok(Bounce::Done(Value::Void))
    }
}

fn eval_define(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match args {
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env, span, ctx)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        [Value::List(sig), body @ ..] if !sig.is_empty() => {
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::TypeError {
                    message: "define: expected function name".into(),
                    span,
                });
            };
            let (params, rest_param) = parse_params(&sig[1..], span)?;
            let lambda = Value::Lambda {
                params,
                rest_param,
                body: body.to_vec(),
                env: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::TypeError {
            message: "define: invalid syntax".into(),
            span,
        }),
    }
}

fn eval_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let [Value::Symbol(name), expr] = args else {
        return Err(EvalError::TypeError {
            message: "set!: expected (set! variable expr)".into(),
            span,
        });
    };
    let val = eval(expr, env, span, ctx)?;
    if !env.borrow_mut().set(name, val) {
        return Err(EvalError::UnboundVariable {
            name: name.clone(),
            span,
        });
    }
    Ok(Value::Void)
}

fn eval_quote(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [datum] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    Ok(datum.clone())
}

/// Parse a parameter list, handling dot notation for rest params.
fn parse_params(
    param_list: &[Value],
    span: Span,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = param_list
        .iter()
        .position(|v| matches!(v, Value::Symbol(s) if s == "."));

    if let Some(pos) = dot_pos {
        if pos + 2 != param_list.len() {
            return Err(EvalError::TypeError {
                message: "invalid dot notation in parameter list".into(),
                span,
            });
        }
        let Value::Symbol(rest_name) = &param_list[pos + 1] else {
            return Err(EvalError::TypeError {
                message: "rest parameter must be a symbol".into(),
                span,
            });
        };
        let params: Vec<String> = param_list[..pos]
            .iter()
            .map(|v| match v {
                Value::Symbol(s) => Ok(s.clone()),
                other => Err(EvalError::TypeError {
                    message: format!("expected parameter name, got {other}"),
                    span,
                }),
            })
            .collect::<Result<_, _>>()?;
        Ok((params, Some(rest_name.clone())))
    } else {
        let params: Vec<String> = param_list
            .iter()
            .map(|v| match v {
                Value::Symbol(s) => Ok(s.clone()),
                other => Err(EvalError::TypeError {
                    message: format!("expected parameter name, got {other}"),
                    span,
                }),
            })
            .collect::<Result<_, _>>()?;
        Ok((params, None))
    }
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let [Value::List(param_list), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "lambda: expected parameter list".into(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "lambda: expected body".into(),
            span,
        });
    }
    let (params, rest_param) = parse_params(param_list, span)?;
    Ok(Value::Lambda {
        params,
        rest_param,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

/// Parse a single `let` binding form `(name expr)`, returning the name and expression.
fn parse_let_binding(binding: &Value, span: Span) -> Result<(&str, &Value), EvalError> {
    let Value::List(pair) = binding else {
        return Err(EvalError::TypeError {
            message: "let: binding must be a list".into(),
            span,
        });
    };
    let [Value::Symbol(param), val_expr] = pair.as_slice() else {
        return Err(EvalError::TypeError {
            message: "let: binding must be (name expr)".into(),
            span,
        });
    };
    Ok((param.as_str(), val_expr))
}

fn eval_let_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    // Named let: (let name ((var init) ...) body ...)
    if let [Value::Symbol(name), Value::List(bindings), body @ ..] = args {
        if body.is_empty() {
            return Err(EvalError::TypeError {
                message: "let: expected body".into(),
                span,
            });
        }
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            let (param, val_expr) = parse_let_binding(binding, span)?;
            params.push(param.to_owned());
            init_vals.push(eval(val_expr, env, span, ctx)?);
        }
        let local_env = Env::with_parent(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body: body.to_vec(),
            env: Rc::clone(&local_env),
        };
        local_env.borrow_mut().define(name.clone(), lambda);
        for (param, val) in params.iter().zip(&init_vals) {
            local_env.borrow_mut().define(param.clone(), val.clone());
        }
        return eval_body_tco(body, &local_env, span, ctx);
    }

    // Regular let: (let ((var init) ...) body ...)
    let [Value::List(bindings), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "let: expected bindings list".into(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "let: expected body".into(),
            span,
        });
    }
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let (name, val_expr) = parse_let_binding(binding, span)?;
        let val = eval(val_expr, env, span, ctx)?;
        local_env.borrow_mut().define(name.to_owned(), val);
    }
    eval_body_tco(body, &local_env, span, ctx)
}

fn eval_cond_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    for clause in args {
        let Value::List(elems) = clause else {
            return Err(EvalError::TypeError {
                message: "cond: clause must be a list".into(),
                span,
            });
        };
        let [test, body @ ..] = elems.as_slice() else {
            return Err(EvalError::TypeError {
                message: "cond: empty clause".into(),
                span,
            });
        };
        if matches!(test, Value::Symbol(s) if s == "else")
            || is_truthy(&eval(test, env, span, ctx)?)
        {
            return eval_body_tco(body, env, span, ctx);
        }
    }
    Ok(Bounce::Done(Value::Void))
}

fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

/// Implement (apply proc arg1 ... args-list)
fn eval_apply(
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
            span,
        });
    }
    let proc = &args[0];
    let prefix = &args[1..args.len() - 1];
    let Value::List(tail_list) = &args[args.len() - 1] else {
        return Err(EvalError::TypeError {
            message: "apply: last argument must be a list".into(),
            span,
        });
    };
    let mut all_args: Vec<Value> = prefix.to_vec();
    all_args.extend(tail_list.iter().cloned());
    apply(proc, &all_args, span, ctx)
}

fn apply_builtin(
    name: &str,
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b), span),
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b), span),
        "-" => eval_sub(args, span),
        "/" => eval_div(args, span),
        "<" => compare_op(args, |a, b| a < b, span),
        ">" => compare_op(args, |a, b| a > b, span),
        "=" => compare_op(args, |a, b| a == b, span),
        "<=" => compare_op(args, |a, b| a <= b, span),
        ">=" => compare_op(args, |a, b| a >= b, span),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Ok(Value::Boolean(!is_truthy(arg)))
        }
        "cons" | "car" | "cdr" | "null?" | "list" | "length" => {
            apply_list_builtin(name, args, span)
        }
        "string?" => Ok(Value::Boolean(matches!(args, [Value::String(_)]))),
        "number?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        "boolean?" => Ok(Value::Boolean(matches!(args, [Value::Boolean(_)]))),
        "pair?" => Ok(Value::Boolean(
            matches!(args, [Value::List(e)] if !e.is_empty()),
        )),
        "symbol?" => Ok(Value::Boolean(matches!(args, [Value::Symbol(_)]))),
        "char?" => Ok(Value::Boolean(matches!(args, [Value::Char(_)]))),
        "apply" => eval_apply(args, span, ctx),
        "map" => {
            let [proc, Value::List(elems)] = args else {
                return Err(EvalError::TypeError {
                    message: "map: expected procedure and list".into(),
                    span,
                });
            };
            let results: Vec<Value> = elems
                .iter()
                .map(|elem| apply(proc, std::slice::from_ref(elem), span, ctx))
                .collect::<Result<_, _>>()?;
            Ok(Value::List(results))
        }
        "display" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            ctx.output.borrow_mut().push_str(&arg.display_str());
            Ok(Value::Void)
        }
        "write" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            ctx.output.borrow_mut().push_str(&arg.to_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    expected: 0,
                    got: args.len(),
                    span,
                });
            }
            ctx.output.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string->list" | "list->string" | "char->integer"
        | "integer->char" => apply_string_builtin(name, args, span),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}

fn require_integer(val: &Value, span: Span) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            message: format!("expected integer, got {other}"),
            span,
        }),
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
    span: Span,
) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(identity, |acc, val| op(acc, require_integer(val, span)?))
        .map(Value::Integer)
}

fn eval_sub(args: &[Value], span: Span) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
            span,
        }),
        [single] => Ok(Value::Integer(-require_integer(single, span)?)),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(require_integer(first, span)?, |acc, val| {
                Ok(acc - require_integer(val, span)?)
            })
            .map(Value::Integer),
    }
}

fn checked_div(a: i64, b: i64, span: Span) -> Result<i64, EvalError> {
    if b == 0 {
        return Err(EvalError::DivisionByZero { span });
    }
    Ok(a / b)
}

fn eval_div(args: &[Value], span: Span) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
            span,
        }),
        [single] => checked_div(1, require_integer(single, span)?, span).map(Value::Integer),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(require_integer(first, span)?, |acc, val| {
                checked_div(acc, require_integer(val, span)?, span)
            })
            .map(Value::Integer),
    }
}

fn compare_op(
    args: &[Value],
    op: impl Fn(i64, i64) -> bool,
    span: Span,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
            span,
        });
    }

    let nums: Vec<i64> = args
        .iter()
        .map(|v| require_integer(v, span))
        .collect::<Result<_, _>>()?;
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}

fn apply_list_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            let [car, cdr] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                    span,
                });
            };
            match cdr {
                Value::List(elems) => {
                    let mut new_list = vec![car.clone()];
                    new_list.extend(elems.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Ok(Value::List(vec![car.clone(), cdr.clone()])),
            }
        }
        "car" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::TypeError {
                    message: format!("car: expected non-empty pair, got {arg}"),
                    span,
                }),
            }
        }
        "cdr" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                _ => Err(EvalError::TypeError {
                    message: format!("cdr: expected non-empty pair, got {arg}"),
                    span,
                }),
            }
        }
        "null?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Ok(Value::Boolean(matches!(
                arg,
                Value::List(elems) if elems.is_empty()
            )))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::TypeError {
                    message: format!("length: expected list, got {arg}"),
                    span,
                }),
            }
        }
        _ => unreachable!("apply_list_builtin called with non-list builtin: {name}"),
    }
}

fn apply_string_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let result: String = args
                .iter()
                .map(|a| match a {
                    Value::String(s) => Ok(s.as_str()),
                    other => Err(EvalError::TypeError {
                        message: format!("string-append: expected string, got {other}"),
                        span,
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?
                .join("");
            Ok(Value::String(result))
        }
        "string-length" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string-length: expected one string argument".into(),
                    span,
                });
            };
            Ok(Value::Integer(s.len() as i64))
        }
        "substring" => {
            let [Value::String(s), start_val, end_val] = args else {
                return Err(EvalError::TypeError {
                    message: "substring: expected (string start end)".into(),
                    span,
                });
            };
            let start = require_integer(start_val, span)? as usize;
            let end = require_integer(end_val, span)? as usize;
            Ok(Value::String(s[start..end].to_string()))
        }
        "string->number" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string->number: expected one string argument".into(),
                    span,
                });
            };
            match s.parse::<i64>() {
                Ok(n) => Ok(Value::Integer(n)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        "number->string" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            let n = require_integer(arg, span)?;
            Ok(Value::String(n.to_string()))
        }
        "symbol->string" => {
            let [Value::Symbol(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "symbol->string: expected one symbol argument".into(),
                    span,
                });
            };
            Ok(Value::String(s.clone()))
        }
        "string->symbol" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string->symbol: expected one string argument".into(),
                    span,
                });
            };
            Ok(Value::Symbol(s.clone()))
        }
        "string-ref" => {
            let [Value::String(s), idx_val] = args else {
                return Err(EvalError::TypeError {
                    message: "string-ref: expected (string index)".into(),
                    span,
                });
            };
            let idx = require_integer(idx_val, span)? as usize;
            let c = s.chars().nth(idx).ok_or_else(|| EvalError::TypeError {
                message: format!(
                    "string-ref: index {idx} out of range for string of length {}",
                    s.len()
                ),
                span,
            })?;
            Ok(Value::Char(c))
        }
        "string-copy" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string-copy: expected one string argument".into(),
                    span,
                });
            };
            Ok(Value::String(s.clone()))
        }
        "string->list" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string->list: expected one string argument".into(),
                    span,
                });
            };
            Ok(Value::List(s.chars().map(Value::Char).collect()))
        }
        "list->string" => {
            let [Value::List(elems)] = args else {
                return Err(EvalError::TypeError {
                    message: "list->string: expected one list argument".into(),
                    span,
                });
            };
            let s: String = elems
                .iter()
                .map(|v| match v {
                    Value::Char(c) => Ok(*c),
                    other => Err(EvalError::TypeError {
                        message: format!("list->string: expected char, got {other}"),
                        span,
                    }),
                })
                .collect::<Result<_, _>>()?;
            Ok(Value::String(s))
        }
        "char->integer" | "integer->char" => apply_char_builtin(name, args, span),
        _ => unreachable!("apply_string_builtin called with non-string builtin: {name}"),
    }
}

fn apply_char_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "char->integer" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError {
                    message: "char->integer: expected one char argument".into(),
                    span,
                });
            };
            Ok(Value::Integer(*c as i64))
        }
        "integer->char" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            let n = require_integer(arg, span)?;
            let c = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
                message: format!("integer->char: invalid code point {n}"),
                span,
            })?;
            Ok(Value::Char(c))
        }
        _ => unreachable!("apply_char_builtin called with non-char builtin: {name}"),
    }
}

fn eval_string_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let [var_expr, idx_expr, char_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
            span,
        });
    };
    let Value::Symbol(var_name) = var_expr else {
        return Err(EvalError::TypeError {
            message: "string-set!: first argument must be a variable name".into(),
            span,
        });
    };
    let idx = require_integer(&eval(idx_expr, env, span, ctx)?, span)? as usize;
    let ch = match eval(char_expr, env, span, ctx)? {
        Value::Char(c) => c,
        other => {
            return Err(EvalError::TypeError {
                message: format!("string-set!: expected char, got {other}"),
                span,
            });
        }
    };
    let current = env.borrow().get(var_name).ok_or_else(|| EvalError::UnboundVariable {
        name: var_name.clone(),
        span,
    })?;
    let Value::String(s) = current else {
        return Err(EvalError::TypeError {
            message: format!("string-set!: expected string, got {current}"),
            span,
        });
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::TypeError {
            message: format!(
                "string-set!: index {idx} out of range for string of length {}",
                chars.len()
            ),
            span,
        });
    }
    chars[idx] = ch;
    let new_string: String = chars.into_iter().collect();
    if !env.borrow_mut().set(var_name, Value::String(new_string)) {
        return Err(EvalError::UnboundVariable {
            name: var_name.clone(),
            span,
        });
    }
    Ok(Value::Void)
}
