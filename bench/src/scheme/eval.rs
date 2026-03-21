use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::{BodyContinuation, ContinuationData, Value};

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
            | "equal?"
            | "eqv?"
            | "eq?"
            | "vector"
            | "make-vector"
            | "vector-ref"
            | "vector-length"
            | "vector?"
            | "vector->list"
            | "list->vector"
            | "abs"
            | "modulo"
            | "remainder"
            | "quotient"
            | "min"
            | "max"
            | "expt"
            | "zero?"
            | "positive?"
            | "negative?"
            | "odd?"
            | "even?"
            | "list-ref"
            | "list-tail"
            | "list?"
            | "assoc"
            | "char-alphabetic?"
            | "char-numeric?"
            | "char-upcase"
            | "char-downcase"
            | "char=?"
            | "char<?"
            | "string=?"
            | "string<?"
            | "string-ci=?"
            | "string-upcase"
            | "string-downcase"
            | "dynamic-wind"
            | "reverse"
            | "with-exception-handler"
            | "raise"
            | "values"
            | "call-with-values"
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
    pub gensym_counter: Cell<u64>,
    /// Current body continuation context — set by eval_body_tco for init expressions.
    body_continuation: RefCell<Option<BodyContinuation>>,
    /// Nesting depth of active dynamic-wind calls.
    wind_depth: Cell<u64>,
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
            gensym_counter: Cell::new(0),
            body_continuation: RefCell::new(None),
            wind_depth: Cell::new(0),
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
            Value::Lambda { .. } | Value::Continuation(_) | Value::Macro { .. }
            | Value::Vector(_) | Value::Pair(..) | Value::Values(_) => {
                return Ok(cur_expr);
            }
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
            "letrec" => return eval_letrec_tco(args, env, span, ctx),
            "letrec*" => return eval_letrec_star_tco(args, env, span, ctx),
            "case" => return eval_case_tco(args, env, span, ctx),
            "vector-set!" => {
                return eval_vector_set(args, env, span, ctx).map(Bounce::Done);
            }
            "define-syntax" => {
                return eval_define_syntax(args, env, span).map(Bounce::Done);
            }
            "guard" => return eval_guard_tco(args, env, span, ctx),
            _ => {}
        }

        // Check if this symbol is bound to a macro (outside match to reduce nesting)
        if let Some(Value::Macro {
            ref name,
            ref keywords,
            ref rules,
            ref def_env,
        }) = env.borrow().get(name)
        {
            let expanded = crate::scheme::macros::expand_macro(
                name, keywords, rules, def_env, elems, span, &ctx.gensym_counter,
            )?;
            return Ok(Bounce::TailCall {
                expr: expanded,
                env: Rc::clone(env),
            });
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
        Value::Symbol(name) if name == "raise" => {
            let [value] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Err(EvalError::RaisedException(value.clone()))
        }
        Value::Symbol(name) if name == "with-exception-handler" => {
            let [handler, thunk] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                    span,
                });
            };
            eval_with_exception_handler(handler, thunk, span, ctx).map(Bounce::Done)
        }
        Value::Symbol(name) if name == "values" => {
            match args {
                [single] => Ok(Bounce::Done(single.clone())),
                _ => Ok(Bounce::Done(Value::Values(args.to_vec()))),
            }
        }
        Value::Symbol(name) if name == "call-with-values" => {
            let [producer, consumer] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                    span,
                });
            };
            let produced = apply(producer, &[], span, ctx)?;
            let consumer_args = match produced {
                Value::Values(vals) => vals,
                other => vec![other],
            };
            apply_tco(consumer, &consumer_args, span, ctx)
        }
        Value::Symbol(name) if name == "dynamic-wind" => {
            let [in_thunk, body_thunk, out_thunk] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 3,
                    got: args.len(),
                    span,
                });
            };
            eval_dynamic_wind(in_thunk, body_thunk, out_thunk, span, ctx).map(Bounce::Done)
        }
        Value::Symbol(name) => apply_builtin(name, args, span, ctx).map(Bounce::Done),
        Value::Lambda {
            params,
            rest_param,
            body,
            env,
        } => apply_lambda(params, rest_param.as_deref(), body, env, args, span, ctx),
        Value::Continuation(data) => apply_continuation(data, args, span, ctx),
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {other}"),
            span,
        }),
    }
}

/// Apply a continuation value to arguments.
fn apply_continuation(
    data: &Rc<ContinuationData>,
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let Some(body_cont) = &data.body_continuation else {
        *ctx.cont_return_data.borrow_mut() = Some(ContReturnData {
            cont_id: data.id,
            expr_idx: data.expr_idx,
            value: value.clone(),
        });
        return Err(EvalError::ContinuationReturn);
    };
    let body = &body_cont.remaining;
    let env = &body_cont.env;
    let [init @ .., last_expr] = body.as_slice() else {
        return Ok(Bounce::Done(value.clone()));
    };
    for expr in init {
        eval(expr, env, span, ctx)?;
    }
    Ok(Bounce::TailCall {
        expr: last_expr.clone(),
        env: Rc::clone(env),
    })
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
    // Inside dynamic-wind, force ContinuationReturn so re-execution goes
    // through dynamic-wind (firing in-thunks on re-entry, out-thunks on exit).
    let body_cont = if ctx.wind_depth.get() > 0 {
        None
    } else {
        ctx.body_continuation.borrow().clone()
    };
    let cont = Value::Continuation(Rc::new(ContinuationData {
        id,
        expr_idx: ctx.current_expr_idx.get(),
        body_continuation: body_cont,
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

/// Evaluate dynamic-wind: run in-thunk, body-thunk, out-thunk.
/// Out-thunk runs even on non-local exit via continuation.
fn eval_dynamic_wind(
    in_thunk: &Value,
    body_thunk: &Value,
    out_thunk: &Value,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    apply(in_thunk, &[], span, ctx)?;
    ctx.wind_depth.set(ctx.wind_depth.get() + 1);
    let body_result = apply(body_thunk, &[], span, ctx);
    ctx.wind_depth.set(ctx.wind_depth.get() - 1);
    apply(out_thunk, &[], span, ctx)?;
    body_result
}

/// Evaluate `with-exception-handler`: install handler, run thunk.
fn eval_with_exception_handler(
    handler: &Value,
    thunk: &Value,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match apply(thunk, &[], span, ctx) {
        Ok(val) => Ok(val),
        Err(EvalError::RaisedException(exn)) => apply(handler, &[exn], span, ctx),
        Err(e) => Err(e),
    }
}

/// Evaluate `guard` special form:
/// (guard (var clause ...) body ...)
/// Evaluate body; if it raises, bind var to the exception and test clauses.
fn eval_guard_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [clauses_form, body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "guard: missing clauses".into(),
            span,
        });
    };
    let Value::List(clause_list) = clauses_form else {
        return Err(EvalError::TypeError {
            message: "guard: clauses must be a list".into(),
            span,
        });
    };
    let [Value::Symbol(var), clauses @ ..] = clause_list.as_slice() else {
        return Err(EvalError::TypeError {
            message: "guard: first element must be a variable name".into(),
            span,
        });
    };

    // Evaluate body; if no exception, return the result
    let body_result = eval_body(body, env, span, ctx);
    let exn = match body_result {
        Ok(val) => return Ok(Bounce::Done(val)),
        Err(EvalError::RaisedException(exn)) => exn,
        Err(e) => return Err(e),
    };

    // Bind the exception value to var
    let guard_env = Env::with_parent(env);
    guard_env.borrow_mut().define(var.clone(), exn.clone());

    // Test clauses like cond
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::TypeError {
                message: "guard: clause must be a list".into(),
                span,
            });
        };
        let [test, body_exprs @ ..] = parts.as_slice() else {
            return Err(EvalError::TypeError {
                message: "guard: empty clause".into(),
                span,
            });
        };
        // else clause
        if matches!(test, Value::Symbol(s) if s == "else") {
            return eval_body_tco(body_exprs, &guard_env, span, ctx);
        }
        let test_val = eval(test, &guard_env, span, ctx)?;
        if !is_truthy(&test_val) {
            continue;
        }
        if body_exprs.is_empty() {
            return Ok(Bounce::Done(test_val));
        }
        return eval_body_tco(body_exprs, &guard_env, span, ctx);
    }

    // No clause matched — re-raise
    Err(EvalError::RaisedException(exn))
}

/// Evaluate a body (sequence), returning the last value (non-TCO version).
fn eval_body(
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match eval_body_tco(body, env, span, ctx)? {
        Bounce::Done(v) => Ok(v),
        Bounce::TailCall { expr, env } => eval(&expr, &env, span, ctx),
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
    for (i, expr) in init.iter().enumerate() {
        let remaining: Vec<Value> = init[i + 1..]
            .iter()
            .chain(std::iter::once(last))
            .cloned()
            .collect();
        let prev = ctx.body_continuation.borrow_mut().replace(BodyContinuation {
            remaining,
            env: Rc::clone(env),
        });
        let result = eval(expr, env, span, ctx);
        *ctx.body_continuation.borrow_mut() = prev;
        result?;
    }
    // Clear body_continuation for the tail expression — it's in tail position,
    // so no remaining body to capture.
    let _prev = ctx.body_continuation.borrow_mut().take();
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

fn eval_define_syntax(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
) -> Result<Value, EvalError> {
    let [Value::Symbol(name), transformer] = args else {
        return Err(EvalError::TypeError {
            message: "define-syntax: expected (define-syntax name transformer)".into(),
            span,
        });
    };
    let Value::List(sr_elems) = transformer else {
        return Err(EvalError::TypeError {
            message: "define-syntax: expected syntax-rules form".into(),
            span,
        });
    };
    let [Value::Symbol(sr_kw), Value::List(keywords_list), rules @ ..] = sr_elems.as_slice()
    else {
        return Err(EvalError::TypeError {
            message: "syntax-rules: expected (syntax-rules (keywords...) rules...)".into(),
            span,
        });
    };
    if sr_kw != "syntax-rules" {
        return Err(EvalError::TypeError {
            message: "define-syntax: expected syntax-rules".into(),
            span,
        });
    }

    let keywords: Vec<String> = keywords_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                message: format!("syntax-rules: keyword must be a symbol, got {other}"),
                span,
            }),
        })
        .collect::<Result<_, _>>()?;

    let parsed_rules: Vec<(Value, Value)> = rules
        .iter()
        .map(|rule| {
            let Value::List(pair) = rule else {
                return Err(EvalError::TypeError {
                    message: "syntax-rules: rule must be a list".into(),
                    span,
                });
            };
            let [pattern, template] = pair.as_slice() else {
                return Err(EvalError::TypeError {
                    message: "syntax-rules: rule must be (pattern template)".into(),
                    span,
                });
            };
            Ok((pattern.clone(), template.clone()))
        })
        .collect::<Result<_, _>>()?;

    let macro_val = Value::Macro {
        name: name.clone(),
        keywords,
        rules: parsed_rules,
        def_env: Rc::clone(env),
    };
    env.borrow_mut().define(name.clone(), macro_val);
    Ok(Value::Void)
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
        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "reverse" => {
            apply_list_builtin(name, args, span)
        }
        "string?" => Ok(Value::Boolean(matches!(args, [Value::String(_)]))),
        "number?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        "boolean?" => Ok(Value::Boolean(matches!(args, [Value::Boolean(_)]))),
        "pair?" => Ok(Value::Boolean(
            matches!(args, [Value::List(e)] if !e.is_empty())
                || matches!(args, [Value::Pair(..)])
        )),
        "symbol?" => Ok(Value::Boolean(matches!(args, [Value::Symbol(_)]))),
        "char?" => Ok(Value::Boolean(matches!(args, [Value::Char(_)]))),
        "apply" => eval_apply(args, span, ctx),
        "map" => apply_map(args, span, ctx),
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
        "equal?" | "eqv?" | "eq?" => apply_equality_builtin(name, args, span),
        "vector" | "make-vector" | "vector-ref" | "vector-length" | "vector?"
        | "vector->list" | "list->vector" => apply_vector_builtin(name, args, span),
        "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" | "zero?"
        | "positive?" | "negative?" | "odd?" | "even?" => {
            apply_numeric_extra(name, args, span)
        }
        "list-ref" | "list-tail" | "list?" | "assoc" => {
            apply_list_extra(name, args, span)
        }
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?"
        | "char<?" => apply_char_builtin(name, args, span),
        "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" => {
            apply_string_builtin(name, args, span)
        }
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
                Value::Pair(..) => Ok(Value::Pair(
                    Box::new(car.clone()),
                    Box::new(cdr.clone()),
                )),
                _ => Ok(Value::Pair(Box::new(car.clone()), Box::new(cdr.clone()))),
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
                Value::Pair(car, _) => Ok(*car.clone()),
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
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
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
        "reverse" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) => {
                    let reversed: Vec<Value> = elems.iter().rev().cloned().collect();
                    Ok(Value::List(reversed))
                }
                _ => Err(EvalError::TypeError {
                    message: format!("reverse: expected list, got {arg}"),
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
        _ => apply_string_builtin_ext(name, args, span),
    }
}

fn apply_string_builtin_ext(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
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
        "string=?" => {
            let [Value::String(a), Value::String(b)] = args else {
                return Err(EvalError::TypeError {
                    message: "string=?: expected two string arguments".into(),
                    span,
                });
            };
            Ok(Value::Boolean(a == b))
        }
        "string<?" => {
            let [Value::String(a), Value::String(b)] = args else {
                return Err(EvalError::TypeError {
                    message: "string<?: expected two string arguments".into(),
                    span,
                });
            };
            Ok(Value::Boolean(a < b))
        }
        "string-ci=?" => {
            let [Value::String(a), Value::String(b)] = args else {
                return Err(EvalError::TypeError {
                    message: "string-ci=?: expected two string arguments".into(),
                    span,
                });
            };
            Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase()))
        }
        "string-upcase" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string-upcase: expected one string argument".into(),
                    span,
                });
            };
            Ok(Value::String(s.to_uppercase()))
        }
        "string-downcase" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError {
                    message: "string-downcase: expected one string argument".into(),
                    span,
                });
            };
            Ok(Value::String(s.to_lowercase()))
        }
        _ => unreachable!("apply_string_builtin_ext called with non-string builtin: {name}"),
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
        "char-alphabetic?" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError {
                    message: "char-alphabetic?: expected one char argument".into(),
                    span,
                });
            };
            Ok(Value::Boolean(c.is_alphabetic()))
        }
        "char-numeric?" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError {
                    message: "char-numeric?: expected one char argument".into(),
                    span,
                });
            };
            Ok(Value::Boolean(c.is_ascii_digit()))
        }
        "char-upcase" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError {
                    message: "char-upcase: expected one char argument".into(),
                    span,
                });
            };
            Ok(Value::Char(c.to_ascii_uppercase()))
        }
        "char-downcase" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError {
                    message: "char-downcase: expected one char argument".into(),
                    span,
                });
            };
            Ok(Value::Char(c.to_ascii_lowercase()))
        }
        "char=?" => {
            let [Value::Char(a), Value::Char(b)] = args else {
                return Err(EvalError::TypeError {
                    message: "char=?: expected two char arguments".into(),
                    span,
                });
            };
            Ok(Value::Boolean(a == b))
        }
        "char<?" => {
            let [Value::Char(a), Value::Char(b)] = args else {
                return Err(EvalError::TypeError {
                    message: "char<?: expected two char arguments".into(),
                    span,
                });
            };
            Ok(Value::Boolean(a < b))
        }
        _ => unreachable!("apply_char_builtin called with non-char builtin: {name}"),
    }
}

// ===== Level 15: Numeric/Char/String Utilities =====

fn apply_map(
    args: &[Value],
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::TypeError {
            message: "map: expected procedure and at least one list".into(),
            span,
        });
    }
    let proc = &args[0];
    if args.len() == 2 {
        let Value::List(elems) = &args[1] else {
            return Err(EvalError::TypeError {
                message: "map: expected list argument".into(),
                span,
            });
        };
        let results: Vec<Value> = elems
            .iter()
            .map(|elem| apply(proc, std::slice::from_ref(elem), span, ctx))
            .collect::<Result<_, _>>()?;
        return Ok(Value::List(results));
    }
    let lists: Vec<&Vec<Value>> = args[1..]
        .iter()
        .map(|a| match a {
            Value::List(elems) => Ok(elems),
            _ => Err(EvalError::TypeError {
                message: "map: expected list argument".into(),
                span,
            }),
        })
        .collect::<Result<_, _>>()?;
    let len = lists[0].len();
    let results: Vec<Value> = (0..len)
        .map(|i| {
            let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
            apply(proc, &call_args, span, ctx)
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::List(results))
}

fn apply_numeric_extra(
    name: &str,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    match name {
        "abs" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Integer(require_integer(arg, span)?.abs()))
        }
        "modulo" => {
            let [a_val, b_val] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
            };
            let a = require_integer(a_val, span)?;
            let b = require_integer(b_val, span)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            let [a_val, b_val] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
            };
            let a = require_integer(a_val, span)?;
            let b = require_integer(b_val, span)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            let [a_val, b_val] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
            };
            let a = require_integer(a_val, span)?;
            let b = require_integer(b_val, span)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0, span });
            }
            let nums: Vec<i64> = args.iter().map(|v| require_integer(v, span)).collect::<Result<_, _>>()?;
            Ok(Value::Integer(*nums.iter().min().expect("non-empty")))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0, span });
            }
            let nums: Vec<i64> = args.iter().map(|v| require_integer(v, span)).collect::<Result<_, _>>()?;
            Ok(Value::Integer(*nums.iter().max().expect("non-empty")))
        }
        "expt" => {
            let [base_val, exp_val] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
            };
            let base = require_integer(base_val, span)?;
            let exp = require_integer(exp_val, span)?;
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(require_integer(arg, span)? == 0))
        }
        "positive?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(require_integer(arg, span)? > 0))
        }
        "negative?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(require_integer(arg, span)? < 0))
        }
        "odd?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(require_integer(arg, span)? % 2 != 0))
        }
        "even?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(require_integer(arg, span)? % 2 == 0))
        }
        _ => unreachable!(),
    }
}

fn apply_list_extra(
    name: &str,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    match name {
        "list-ref" => {
            let [Value::List(elems), idx_val] = args else {
                return Err(EvalError::TypeError {
                    message: "list-ref: expected list and integer".into(),
                    span,
                });
            };
            let idx = require_integer(idx_val, span)? as usize;
            elems.get(idx).cloned().ok_or_else(|| EvalError::TypeError {
                message: format!("list-ref: index {idx} out of range"),
                span,
            })
        }
        "list-tail" => {
            let [Value::List(elems), idx_val] = args else {
                return Err(EvalError::TypeError {
                    message: "list-tail: expected list and integer".into(),
                    span,
                });
            };
            let idx = require_integer(idx_val, span)? as usize;
            if idx > elems.len() {
                return Err(EvalError::TypeError {
                    message: format!("list-tail: index {idx} out of range"),
                    span,
                });
            }
            Ok(Value::List(elems[idx..].to_vec()))
        }
        "list?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            match arg {
                Value::List(_) => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            }
        }
        "assoc" => {
            let [key, Value::List(alist)] = args else {
                return Err(EvalError::TypeError {
                    message: "assoc: expected key and association list".into(),
                    span,
                });
            };
            let found = alist.iter().find(|entry| {
                matches!(entry, Value::List(pair) if !pair.is_empty() && values_equal(&pair[0], key))
            });
            Ok(found.cloned().unwrap_or(Value::Boolean(false)))
        }
        _ => unreachable!(),
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

// ===== Level 14: Deep Equality, Letrec, Case & Vectors =====

/// Deep structural equality for `equal?`.
fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(xs), Value::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys).all(|(a, b)| values_equal(a, b))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        (Value::Vector(xs), Value::Vector(ys)) => {
            let xs = xs.borrow();
            let ys = ys.borrow();
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

/// `eqv?` / `eq?` — shallow identity-like equality.
fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(xs), Value::List(ys)) => xs.is_empty() && ys.is_empty(),
        (Value::Pair(..), Value::Pair(..)) => false,
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn apply_equality_builtin(
    name: &str,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
            span,
        });
    };
    let result = match name {
        "equal?" => values_equal(a, b),
        "eqv?" | "eq?" => values_eqv(a, b),
        _ => unreachable!(),
    };
    Ok(Value::Boolean(result))
}

fn eval_letrec_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [Value::List(bindings), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "letrec: expected bindings list".into(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "letrec: expected body".into(),
            span,
        });
    }
    let local_env = Env::with_parent(env);
    // First pass: bind all names to Void (placeholder)
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for binding in bindings {
        let (name, val_expr) = parse_let_binding(binding, span)?;
        local_env.borrow_mut().define(name.to_owned(), Value::Void);
        names.push(name.to_owned());
        init_exprs.push(val_expr.clone());
    }
    // Second pass: evaluate init exprs in the local env and set bindings
    for (name, init_expr) in names.iter().zip(&init_exprs) {
        let val = eval(init_expr, &local_env, span, ctx)?;
        local_env.borrow_mut().set(name, val);
    }
    eval_body_tco(body, &local_env, span, ctx)
}

fn eval_letrec_star_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [Value::List(bindings), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "letrec*: expected bindings list".into(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "letrec*: expected body".into(),
            span,
        });
    }
    let local_env = Env::with_parent(env);
    // Evaluate sequentially — each binding is visible to the next
    for binding in bindings {
        let (name, val_expr) = parse_let_binding(binding, span)?;
        let val = eval(val_expr, &local_env, span, ctx)?;
        local_env.borrow_mut().define(name.to_owned(), val);
    }
    eval_body_tco(body, &local_env, span, ctx)
}

fn eval_case_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Bounce, EvalError> {
    let [key_expr, clauses @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "case: expected key expression".into(),
            span,
        });
    };
    let key = eval(key_expr, env, span, ctx)?;
    for clause in clauses {
        let Value::List(elems) = clause else {
            return Err(EvalError::TypeError {
                message: "case: clause must be a list".into(),
                span,
            });
        };
        let [datums, body @ ..] = elems.as_slice() else {
            return Err(EvalError::TypeError {
                message: "case: empty clause".into(),
                span,
            });
        };
        // else clause
        if matches!(datums, Value::Symbol(s) if s == "else") {
            return eval_body_tco(body, env, span, ctx);
        }
        // datum list
        let Value::List(datum_list) = datums else {
            return Err(EvalError::TypeError {
                message: "case: expected datum list".into(),
                span,
            });
        };
        if datum_list.iter().any(|d| values_eqv(&key, d)) {
            return eval_body_tco(body, env, span, ctx);
        }
    }
    Ok(Bounce::Done(Value::Void))
}

fn eval_vector_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    span: Span,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let [vec_expr, idx_expr, val_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
            span,
        });
    };
    let vec_val = eval(vec_expr, env, span, ctx)?;
    let idx = require_integer(&eval(idx_expr, env, span, ctx)?, span)? as usize;
    let val = eval(val_expr, env, span, ctx)?;
    let Value::Vector(cells) = vec_val else {
        return Err(EvalError::TypeError {
            message: format!("vector-set!: expected vector, got {vec_val}"),
            span,
        });
    };
    let mut elems = cells.borrow_mut();
    if idx >= elems.len() {
        return Err(EvalError::TypeError {
            message: format!(
                "vector-set!: index {idx} out of range for vector of length {}",
                elems.len()
            ),
            span,
        });
    }
    elems[idx] = val;
    Ok(Value::Void)
}

fn apply_vector_builtin(
    name: &str,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    match name {
        "vector" => Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec())))),
        "make-vector" => {
            let (len, fill) = match args {
                [len_val] => (require_integer(len_val, span)? as usize, Value::Integer(0)),
                [len_val, fill_val] => {
                    (require_integer(len_val, span)? as usize, fill_val.clone())
                }
                _ => {
                    return Err(EvalError::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                        span,
                    });
                }
            };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            let [vec_val, idx_val] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                    span,
                });
            };
            let Value::Vector(cells) = vec_val else {
                return Err(EvalError::TypeError {
                    message: format!("vector-ref: expected vector, got {vec_val}"),
                    span,
                });
            };
            let idx = require_integer(idx_val, span)? as usize;
            let elems = cells.borrow();
            elems.get(idx).cloned().ok_or_else(|| EvalError::TypeError {
                message: format!(
                    "vector-ref: index {idx} out of range for vector of length {}",
                    elems.len()
                ),
                span,
            })
        }
        "vector-length" => {
            let [vec_val] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            let Value::Vector(cells) = vec_val else {
                return Err(EvalError::TypeError {
                    message: format!("vector-length: expected vector, got {vec_val}"),
                    span,
                });
            };
            Ok(Value::Integer(cells.borrow().len() as i64))
        }
        "vector?" => {
            let [val] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Ok(Value::Boolean(matches!(val, Value::Vector(_))))
        }
        "vector->list" => {
            let [vec_val] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            let Value::Vector(cells) = vec_val else {
                return Err(EvalError::TypeError {
                    message: format!("vector->list: expected vector, got {vec_val}"),
                    span,
                });
            };
            Ok(Value::List(cells.borrow().clone()))
        }
        "list->vector" => {
            let [list_val] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            let Value::List(elems) = list_val else {
                return Err(EvalError::TypeError {
                    message: format!("list->vector: expected list, got {list_val}"),
                    span,
                });
            };
            Ok(Value::Vector(Rc::new(RefCell::new(elems.clone()))))
        }
        _ => unreachable!("apply_vector_builtin called with: {name}"),
    }
}
