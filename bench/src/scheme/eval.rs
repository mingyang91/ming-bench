use std::collections::{HashMap, HashSet};

use super::{
    atom_to_value, builtins, env_define, env_get, env_set, macros, quote_expr, DisplayValue, Env,
    EvalError, Expr, Span, Value,
};

/// Information about a captured continuation for re-execution.
pub(crate) struct ContinuationInfo {
    pub expr_index: usize,
    pub cont_id_at_expr_start: u64,
}

/// Evaluation context threaded through the evaluator.
pub(crate) struct EvalCtx {
    pub out: String,
    pub next_cont_id: u64,
    /// Side channel: value from the most recent continuation invocation.
    pub cont_return_value: Option<Value>,
    /// Override: if set, the next call/cc with this ID returns this value immediately.
    pub cont_override: Option<(u64, Value)>,
    /// Current top-level expression index (set by eval_str).
    pub current_expr_index: usize,
    /// All top-level expressions (set by eval_str).
    pub all_exprs: bool,
    /// Registered continuations for re-execution.
    pub cont_registry: HashMap<u64, ContinuationInfo>,
    /// Value of next_cont_id at the start of the current top-level expression.
    pub cont_id_at_expr_start: u64,
    /// Counter for generating unique hygienic macro variable names.
    pub gensym_counter: u64,
    /// Set of continuation IDs whose call/cc is currently on the call stack.
    pub active_continuations: HashSet<u64>,
}

impl EvalCtx {
    pub fn new() -> Self {
        Self {
            out: String::new(),
            next_cont_id: 0,
            cont_return_value: None,
            cont_override: None,
            current_expr_index: 0,
            all_exprs: false,
            cont_registry: HashMap::new(),
            cont_id_at_expr_start: 0,
            gensym_counter: 0,
            active_continuations: HashSet::new(),
        }
    }
}

/// Result of one evaluation step: either a final value or a tail call to continue.
enum Trampoline {
    Done(Value),
    Continue(Expr, Env),
}

/// Extract an unbound variable name from an error (possibly wrapped in AtPosition).
fn as_unbound_variable(err: &EvalError) -> Option<&str> {
    match err {
        EvalError::UnboundVariable { name } => Some(name),
        EvalError::AtPosition { source, .. } => as_unbound_variable(source),
        _ => None,
    }
}

/// Wrap an error with source position, unless it already has one.
fn with_span(span: Span, err: EvalError) -> EvalError {
    match err {
        EvalError::AtPosition { .. } | EvalError::ContinuationReturn { .. } => err,
        _ => EvalError::AtPosition {
            line: span.line,
            col: span.col,
            source: Box::new(err),
        },
    }
}

/// Check if a name is a known builtin (so it can be used as a first-class value).
fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
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
            | "string-length"
            | "string-ref"
            | "string-append"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-copy"
            | "string->list"
            | "list->string"
            | "char->integer"
            | "integer->char"
            | "apply"
            | "map"
            | "call/cc"
            | "call-with-current-continuation"
    )
}

/// Parse parameter list, detecting dot notation for rest params.
fn parse_params(exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    parse_params_from_slice(exprs)
}

/// Parse a slice of param exprs, handling dot notation.
fn parse_params_from_slice(exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = exprs
        .iter()
        .position(|e| matches!(e, Expr::Atom(s, _) if s == "."));
    match dot_pos {
        Some(pos) => {
            let params: Vec<String> = exprs[..pos]
                .iter()
                .map(|e| match e {
                    Expr::Atom(name, _) => Ok(name.clone()),
                    _ => Err(EvalError::Parse {
                        message: "parameter must be a symbol".to_string(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let [Expr::Atom(rest_name, _)] = &exprs[pos + 1..] else {
                return Err(EvalError::Parse {
                    message: "expected exactly one symbol after dot in parameter list".to_string(),
                });
            };
            Ok((params, Some(rest_name.clone())))
        }
        None => {
            let params: Vec<String> = exprs
                .iter()
                .map(|e| match e {
                    Expr::Atom(name, _) => Ok(name.clone()),
                    _ => Err(EvalError::Parse {
                        message: "parameter must be a symbol".to_string(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            Ok((params, None))
        }
    }
}

/// Validate argument count against lambda signature (fixed + optional rest).
fn validate_lambda_args(
    params: &[String],
    rest_param: &Option<String>,
    arg_count: usize,
) -> Result<(), EvalError> {
    if rest_param.is_some() {
        if arg_count < params.len() {
            return Err(EvalError::WrongArgCount {
                expected: params.len(),
                got: arg_count,
            });
        }
    } else if arg_count != params.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: arg_count,
        });
    }
    Ok(())
}

/// Bind fixed params and optional rest param in a local environment.
fn bind_lambda_params(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Value],
    env: &mut Env,
) {
    for (param, arg) in params.iter().zip(args) {
        env_define(env, param.clone(), arg.clone());
    }
    if let Some(rest_name) = rest_param {
        let rest_args = &args[params.len()..];
        let rest_val = if rest_args.is_empty() {
            Value::Nil
        } else {
            Value::List(rest_args.to_vec())
        };
        env_define(env, rest_name.clone(), rest_val);
    }
}

/// Convert a Value to a list of values (for `apply`).
fn value_to_list(val: &Value) -> Result<Vec<Value>, EvalError> {
    match val {
        Value::Nil => Ok(Vec::new()),
        Value::List(items) => Ok(items.clone()),
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(apply proc arg1 ... arg-list)` from already-evaluated arguments.
fn eval_apply_values(
    args: &[Value],
    env: &Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [proc_val, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: 0,
        });
    };
    let [prefix @ .., last] = rest else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: 1,
        });
    };
    let tail_items = value_to_list(last)?;
    let mut all_args: Vec<Value> = prefix.to_vec();
    all_args.extend(tail_items);
    match proc_val {
        Value::Lambda { .. } => apply_lambda_step(proc_val, &all_args, &env.clone(), ctx),
        Value::Continuation { id } => invoke_continuation(*id, &all_args, ctx),
        Value::Symbol(name) if name == "apply" => eval_apply_values(&all_args, env, ctx),
        Value::Symbol(name) if name == "call/cc" || name == "call-with-current-continuation" => {
            eval_callcc_applied(&all_args, ctx)
        }
        Value::Symbol(name) => apply_builtin(name, &all_args).map(Trampoline::Done),
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
    }
}

/// Invoke a continuation value.
///
/// Three cases:
/// 1. Call/cc is on the stack (escape): throw ContinuationReturn, caught by eval_callcc_with_func.
/// 2. Call/cc is NOT on the stack, captured in same top-level expr: return value directly.
///    Re-execution would follow a different path due to mutable state changes.
/// 3. Call/cc is NOT on the stack, captured in different expr: throw for eval_exprs re-execution.
fn invoke_continuation(
    id: u64,
    args: &[Value],
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };

    if ctx.active_continuations.contains(&id) {
        ctx.cont_return_value = Some(arg.clone());
        return Err(EvalError::ContinuationReturn { id });
    }

    let same_expr = ctx.all_exprs
        && ctx
            .cont_registry
            .get(&id)
            .is_some_and(|info| info.expr_index == ctx.current_expr_index);
    if same_expr {
        return Ok(Trampoline::Done(arg.clone()));
    }

    ctx.cont_return_value = Some(arg.clone());
    Err(EvalError::ContinuationReturn { id })
}

/// Evaluate an expression in the given environment (trampoline loop for TCO).
pub fn eval(expr: &Expr, env: &mut Env, ctx: &mut EvalCtx) -> Result<Value, EvalError> {
    let span = expr.span();
    match eval_step(expr, env, ctx).map_err(|e| with_span(span, e))? {
        Trampoline::Done(val) => Ok(val),
        Trampoline::Continue(mut cur_expr, mut cur_env) => loop {
            let span = cur_expr.span();
            match eval_step(&cur_expr, &mut cur_env, ctx).map_err(|e| with_span(span, e))? {
                Trampoline::Done(val) => return Ok(val),
                Trampoline::Continue(next_expr, next_env) => {
                    cur_expr = next_expr;
                    cur_env = next_env;
                }
            }
        },
    }
}

/// One step of evaluation. Returns Continue for tail-position expressions.
fn eval_step(expr: &Expr, env: &mut Env, ctx: &mut EvalCtx) -> Result<Trampoline, EvalError> {
    match expr {
        Expr::Atom(token, _) => {
            if token.starts_with('"')
                || token.parse::<i64>().is_ok()
                || token == "#t"
                || token == "#f"
                || token.starts_with("#\\")
            {
                atom_to_value(token).map(Trampoline::Done)
            } else if let Some(val) = env_get(env, token) {
                Ok(Trampoline::Done(val))
            } else if is_builtin(token) {
                Ok(Trampoline::Done(Value::Symbol(token.clone())))
            } else {
                Err(EvalError::UnboundVariable {
                    name: token.clone(),
                })
            }
        }
        Expr::List(items, _) => eval_list_step(items, env, ctx),
    }
}

/// Evaluate a list expression, returning Continue for tail-position function calls.
fn eval_list_step(
    items: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty list".to_string(),
        });
    };
    // Check for special forms (operator must be an atom)
    if let Expr::Atom(op, _) = operator {
        match op.as_str() {
            "define" => return eval_define(args, env, ctx).map(Trampoline::Done),
            "if" => return eval_if_step(args, env, ctx),
            "quote" => return eval_quote(args).map(Trampoline::Done),
            "and" => return eval_and_step(args, env, ctx),
            "or" => return eval_or_step(args, env, ctx),
            "lambda" => return eval_lambda(args, env).map(Trampoline::Done),
            "let" => return eval_let_step(args, env, ctx),
            "begin" => return eval_body_step(args, env, ctx),
            "cond" => return eval_cond_step(args, env, ctx),
            "display" => return eval_display(args, env, ctx).map(Trampoline::Done),
            "write" => return eval_write(args, env, ctx).map(Trampoline::Done),
            "newline" => return eval_newline(args, &mut ctx.out).map(Trampoline::Done),
            "set!" => return eval_set(args, env, ctx).map(Trampoline::Done),
            "string-set!" => return eval_string_set(args).map(Trampoline::Done),
            "call/cc" | "call-with-current-continuation" => {
                return eval_callcc(args, env, ctx);
            }
            "define-syntax" => {
                return eval_define_syntax(args, env).map(Trampoline::Done);
            }
            _ => {}
        }
    }
    // Check for macro invocation (before evaluating args)
    if let Expr::Atom(op, _) = operator {
        if let Some(Value::Macro {
            rules,
            literals,
            def_env,
        }) = env_get(env, op)
        {
            let (expanded, injections) =
                macros::expand(&rules, &literals, args, &def_env, &mut ctx.gensym_counter)?;
            env.extend(injections);
            return eval_step(&expanded, env, ctx);
        }
    }
    // Higher-order builtins that need function application
    if let Expr::Atom(op, _) = operator {
        if op == "map" && !env.contains_key("map") {
            return eval_builtin_map(args, env, ctx).map(Trampoline::Done);
        }
    }
    // General function application
    let evaluated: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, ctx))
        .collect::<Result<_, _>>()?;
    // Try evaluating operator; fall back to builtin for atoms
    match eval(operator, env, ctx) {
        Ok(func @ Value::Lambda { .. }) => apply_lambda_step(&func, &evaluated, env, ctx),
        Ok(Value::Symbol(ref name)) if name == "apply" => {
            eval_apply_values(&evaluated, env, ctx)
        }
        Ok(Value::Symbol(ref name))
            if name == "call/cc" || name == "call-with-current-continuation" =>
        {
            eval_callcc_applied(&evaluated, ctx)
        }
        Ok(Value::Continuation { id }) => invoke_continuation(id, &evaluated, ctx),
        Ok(Value::Symbol(ref name)) => apply_builtin(name, &evaluated).map(Trampoline::Done),
        Ok(ref other) => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
        Err(ref e) if as_unbound_variable(e).is_some() => {
            let name = as_unbound_variable(e).expect("checked above").to_string();
            apply_builtin(&name, &evaluated).map(Trampoline::Done)
        }
        Err(e) => Err(e),
    }
}

/// Evaluate `(call/cc func-expr)` as a special form.
fn eval_callcc(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [func_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let func = eval(func_expr, env, ctx)?;
    eval_callcc_with_func(&func, ctx)
}

/// Handle call/cc when applied as a first-class value (already-evaluated args).
fn eval_callcc_applied(
    evaluated: &[Value],
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [func_val] = evaluated else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: evaluated.len(),
        });
    };
    eval_callcc_with_func(func_val, ctx)
}

/// Core call/cc logic: create continuation, call func with it, catch escape returns.
fn eval_callcc_with_func(func: &Value, ctx: &mut EvalCtx) -> Result<Trampoline, EvalError> {
    let id = ctx.next_cont_id;
    ctx.next_cont_id += 1;

    // Register continuation for reentrant invocation
    if ctx.all_exprs {
        ctx.cont_registry.insert(
            id,
            ContinuationInfo {
                expr_index: ctx.current_expr_index,
                cont_id_at_expr_start: ctx.cont_id_at_expr_start,
            },
        );
    }

    // Check for override (re-execution path)
    if let Some((override_id, _)) = &ctx.cont_override {
        if *override_id == id {
            let (_, val) = ctx.cont_override.take().expect("checked above");
            return Ok(Trampoline::Done(val));
        }
    }

    let cont_val = Value::Continuation { id };

    // Fully evaluate func(cont) to catch escape continuation returns
    ctx.active_continuations.insert(id);
    let result = apply_lambda(func, &[cont_val], ctx);
    ctx.active_continuations.remove(&id);

    match result {
        Ok(val) => Ok(Trampoline::Done(val)),
        Err(EvalError::ContinuationReturn { id: ret_id }) if ret_id == id => {
            let val = ctx
                .cont_return_value
                .take()
                .expect("set by continuation invoker");
            Ok(Trampoline::Done(val))
        }
        Err(e) => Err(e),
    }
}

/// Set up a lambda's local env and return Continue for its body (tail call).
fn apply_lambda_step(
    func: &Value,
    args: &[Value],
    caller_env: &Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let Value::Lambda {
        name,
        params,
        rest_param,
        body,
        closure_env,
    } = func
    else {
        unreachable!("apply_lambda_step called with non-lambda");
    };
    validate_lambda_args(params, rest_param, args.len())?;
    let mut local_env = caller_env.clone();
    for (k, v) in closure_env {
        local_env.insert(k.clone(), v.clone());
    }
    if let Some(n) = name {
        env_define(&mut local_env, n.clone(), func.clone());
    }
    bind_lambda_params(params, rest_param, args, &mut local_env);
    eval_body_step(body, &mut local_env, ctx)
}

/// Apply a built-in operator.
fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => builtins::apply_add(args),
        "-" => builtins::apply_sub(args),
        "*" => builtins::apply_mul(args),
        "/" => builtins::apply_div(args),
        "<" => builtins::apply_compare(args, |a, b| a < b),
        ">" => builtins::apply_compare(args, |a, b| a > b),
        "=" => builtins::apply_compare(args, |a, b| a == b),
        "<=" => builtins::apply_compare(args, |a, b| a <= b),
        ">=" => builtins::apply_compare(args, |a, b| a >= b),
        "not" => builtins::apply_not(args),
        "equal?" => builtins::apply_equal(args),
        "eq?" => builtins::apply_eq(args),
        "eqv?" => builtins::apply_eq(args),
        "cons" => builtins::apply_cons(args),
        "car" => builtins::apply_car(args),
        "cdr" => builtins::apply_cdr(args),
        "null?" => builtins::apply_null(args),
        "list" => builtins::apply_list(args),
        "length" => builtins::apply_length(args),
        "string?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::String(_))),
        "number?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::Integer(_))),
        "boolean?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::Boolean(_))),
        "pair?" => builtins::apply_type_predicate(args, |v| match v {
            Value::Pair(_, _) => true,
            Value::List(items) => !items.is_empty(),
            _ => false,
        }),
        "symbol?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::Symbol(_))),
        "char?" => builtins::apply_is_char(args),
        "string-length" => builtins::apply_string_length(args),
        "string-ref" => builtins::apply_string_ref(args),
        "string-append" => builtins::apply_string_append(args),
        "substring" => builtins::apply_substring(args),
        "string->number" => builtins::apply_string_to_number(args),
        "number->string" => builtins::apply_number_to_string(args),
        "symbol->string" => builtins::apply_symbol_to_string(args),
        "string->symbol" => builtins::apply_string_to_symbol(args),
        "string-copy" => builtins::apply_string_copy(args),
        "string->list" => builtins::apply_string_to_list(args),
        "list->string" => builtins::apply_list_to_string(args),
        "char->integer" => builtins::apply_char_to_integer(args),
        "integer->char" => builtins::apply_integer_to_char(args),
        _ => Err(EvalError::UnboundVariable {
            name: op.to_string(),
        }),
    }
}

/// Apply a function value (lambda or builtin lookup) — fully resolves (no TCO).
fn apply_func(func: &Value, args: &[Value], ctx: &mut EvalCtx) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { .. } => apply_lambda(func, args, ctx),
        Value::Continuation { id } => match invoke_continuation(*id, args, ctx) {
            Ok(Trampoline::Done(val)) => Ok(val),
            Err(e) => Err(e),
            Ok(Trampoline::Continue(..)) => unreachable!("invoke_continuation never returns Continue"),
        },
        Value::Symbol(name) => apply_builtin(name, args),
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate a sequence of body expressions, returning the last result (no TCO).
fn eval_body(body: &[Expr], env: &mut Env, ctx: &mut EvalCtx) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for expr in body {
        result = eval(expr, env, ctx)?;
    }
    Ok(result)
}

/// Evaluate body expressions: all-but-last fully, last as Continue (TCO).
fn eval_body_step(
    body: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [rest @ .., last] = body else {
        return Ok(Trampoline::Done(Value::Nil));
    };
    for expr in rest {
        eval(expr, env, ctx)?;
    }
    Ok(Trampoline::Continue(last.clone(), env.clone()))
}

/// Apply a lambda closure to arguments (fully resolves — used by map and call/cc).
fn apply_lambda(func: &Value, args: &[Value], ctx: &mut EvalCtx) -> Result<Value, EvalError> {
    let Value::Lambda {
        name,
        params,
        rest_param,
        body,
        closure_env,
    } = func
    else {
        unreachable!("apply_lambda called with non-lambda");
    };
    validate_lambda_args(params, rest_param, args.len())?;
    let mut local_env = closure_env.clone();
    if let Some(n) = name {
        env_define(&mut local_env, n.clone(), func.clone());
    }
    bind_lambda_params(params, rest_param, args, &mut local_env);
    eval_body(body, &mut local_env, ctx)
}

/// Evaluate `(lambda (params...) body...)`.
fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "lambda requires params and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "lambda requires params and body".to_string(),
        });
    }
    let Expr::List(param_exprs, _) = params_expr else {
        return Err(EvalError::Parse {
            message: "lambda params must be a list".to_string(),
        });
    };
    let (params, rest_param) = parse_params(param_exprs)?;
    Ok(Value::Lambda {
        name: None,
        params,
        rest_param,
        body: body.to_vec(),
        closure_env: env.clone(),
    })
}

/// Evaluate `(define name value)` or `(define (name params...) body...)`.
fn eval_define(args: &[Expr], env: &mut Env, ctx: &mut EvalCtx) -> Result<Value, EvalError> {
    let [target, body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "define requires a name and a value".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "define requires a name and a value".to_string(),
        });
    }
    match target {
        Expr::Atom(name, _) => {
            let [value_expr] = body else {
                return Err(EvalError::Parse {
                    message: "define variable form takes exactly one value".to_string(),
                });
            };
            let val = eval(value_expr, env, ctx)?;
            env_define(env, name.clone(), val.clone());
            Ok(val)
        }
        Expr::List(parts, _) => {
            let [Expr::Atom(name, _), param_exprs @ ..] = parts.as_slice() else {
                return Err(EvalError::Parse {
                    message: "define function form requires a name".to_string(),
                });
            };
            let (params, rest_param) = parse_params_from_slice(param_exprs)?;
            let lambda = Value::Lambda {
                name: Some(name.clone()),
                params,
                rest_param,
                body: body.to_vec(),
                closure_env: env.clone(),
            };
            env_define(env, name.clone(), lambda);
            Ok(Value::Nil)
        }
    }
}

/// Evaluate `(if cond then else)` — returns Continue for the chosen branch (TCO).
fn eval_if_step(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let (cond, then_expr, else_expr) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if requires 2 or 3 arguments".to_string(),
            })
        }
    };
    let cond_val = eval(cond, env, ctx)?;
    if cond_val != Value::Boolean(false) {
        Ok(Trampoline::Continue(then_expr.clone(), env.clone()))
    } else if let Some(e) = else_expr {
        Ok(Trampoline::Continue(e.clone(), env.clone()))
    } else {
        Ok(Trampoline::Done(Value::Nil))
    }
}

/// Evaluate `(quote expr)`.
fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::Parse {
            message: "quote requires exactly 1 argument".to_string(),
        });
    };
    quote_expr(expr)
}

/// Short-circuit `and` with TCO: last expression is in tail position.
fn eval_and_step(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [rest @ .., last] = args else {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    };
    for arg in rest {
        let val = eval(arg, env, ctx)?;
        if val == Value::Boolean(false) {
            return Ok(Trampoline::Done(val));
        }
    }
    Ok(Trampoline::Continue(last.clone(), env.clone()))
}

/// Evaluate `(let ((var val) ...) body...)` or named `(let name ((var val) ...) body...)`.
fn eval_let_step(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    // Check for named let: (let name ((var val) ...) body...)
    if let [Expr::Atom(name, _), Expr::List(bindings, _), body @ ..] = args {
        if !body.is_empty() {
            return eval_named_let_step(name, bindings, body, env, ctx);
        }
    }
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "let requires bindings and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "let requires bindings and body".to_string(),
        });
    }
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Parse {
            message: "let bindings must be a list".to_string(),
        });
    };
    let pairs: Vec<(String, Value)> = bindings
        .iter()
        .map(|b| {
            let Expr::List(pair, _) = b else {
                return Err(EvalError::Parse {
                    message: "let binding must be a list".to_string(),
                });
            };
            let [Expr::Atom(name, _), val_expr] = pair.as_slice() else {
                return Err(EvalError::Parse {
                    message: "let binding must be (name value)".to_string(),
                });
            };
            let val = eval(val_expr, env, ctx)?;
            Ok((name.clone(), val))
        })
        .collect::<Result<_, _>>()?;
    let mut local_env = env.clone();
    for (name, val) in pairs {
        env_define(&mut local_env, name, val);
    }
    eval_body_step(body, &mut local_env, ctx)
}

/// Evaluate named let: `(let name ((var val) ...) body...)`.
fn eval_named_let_step(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let mut params = Vec::new();
    let mut init_vals = Vec::new();
    for b in bindings {
        let Expr::List(pair, _) = b else {
            return Err(EvalError::Parse {
                message: "named let binding must be a list".to_string(),
            });
        };
        let [Expr::Atom(param, _), val_expr] = pair.as_slice() else {
            return Err(EvalError::Parse {
                message: "named let binding must be (name value)".to_string(),
            });
        };
        params.push(param.clone());
        init_vals.push(eval(val_expr, env, ctx)?);
    }
    let lambda = Value::Lambda {
        name: Some(name.to_string()),
        params,
        rest_param: None,
        body: body.to_vec(),
        closure_env: env.clone(),
    };
    apply_lambda_step(&lambda, &init_vals, env, ctx)
}

/// Evaluate `(cond (test expr) ... (else expr))` — returns Continue for body (TCO).
fn eval_cond_step(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    for clause in args {
        let Expr::List(parts, _) = clause else {
            return Err(EvalError::Parse {
                message: "cond clause must be a list".to_string(),
            });
        };
        let [test_expr, body @ ..] = parts.as_slice() else {
            return Err(EvalError::Parse {
                message: "cond clause must have a test and body".to_string(),
            });
        };
        if matches!(test_expr, Expr::Atom(s, _) if s == "else") {
            return eval_body_step(body, env, ctx);
        }
        let test_val = eval(test_expr, env, ctx)?;
        if test_val != Value::Boolean(false) {
            return eval_body_step(body, env, ctx);
        }
    }
    Ok(Trampoline::Done(Value::Nil))
}

/// Short-circuit `or` with TCO: last expression is in tail position.
fn eval_or_step(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Trampoline, EvalError> {
    let [rest @ .., last] = args else {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    };
    for arg in rest {
        let val = eval(arg, env, ctx)?;
        if val != Value::Boolean(false) {
            return Ok(Trampoline::Done(val));
        }
    }
    Ok(Trampoline::Continue(last.clone(), env.clone()))
}

/// Evaluate `(display expr)` — prints value without quotes on strings.
fn eval_display(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(expr, env, ctx)?;
    use std::fmt::Write;
    write!(ctx.out, "{}", DisplayValue(&val)).expect("write to String cannot fail");
    Ok(Value::Nil)
}

/// Evaluate `(write expr)` — prints value with quotes on strings.
fn eval_write(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(expr, env, ctx)?;
    use std::fmt::Write;
    write!(ctx.out, "{val}").expect("write to String cannot fail");
    Ok(Value::Nil)
}

/// Evaluate builtin `(map func list)`.
fn eval_builtin_map(
    args: &[Expr],
    env: &mut Env,
    ctx: &mut EvalCtx,
) -> Result<Value, EvalError> {
    let [func_expr, list_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let func = eval(func_expr, env, ctx)?;
    let list_val = eval(list_expr, env, ctx)?;
    let items = match &list_val {
        Value::Nil => return Ok(Value::Nil),
        Value::List(items) => items,
        _ => {
            return Err(EvalError::TypeError {
                expected: "list".to_string(),
                got: format!("{list_val}"),
            })
        }
    };
    let results: Vec<Value> = items
        .iter()
        .map(|item| apply_func(&func, std::slice::from_ref(item), ctx))
        .collect::<Result<_, _>>()?;
    if results.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(results))
    }
}

/// Evaluate `(set! name value)` — mutate an existing binding.
fn eval_set(args: &[Expr], env: &mut Env, ctx: &mut EvalCtx) -> Result<Value, EvalError> {
    let [Expr::Atom(name, _), value_expr] = args else {
        return Err(EvalError::Parse {
            message: "set! requires a variable name and a value".to_string(),
        });
    };
    let val = eval(value_expr, env, ctx)?;
    env_set(env, name, val)?;
    Ok(Value::Nil)
}

/// Evaluate `(string-set! ...)` — strings are immutable in R7RS.
fn eval_string_set(_args: &[Expr]) -> Result<Value, EvalError> {
    Err(EvalError::Immutable {
        message: "strings are immutable".to_string(),
    })
}

/// Evaluate `(define-syntax name (syntax-rules (literals...) (pattern template) ...))`.
fn eval_define_syntax(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let [Expr::Atom(name, _), sr_expr] = args else {
        return Err(EvalError::Parse {
            message: "define-syntax requires a name and syntax-rules".to_string(),
        });
    };
    let Expr::List(sr_items, _) = sr_expr else {
        return Err(EvalError::Parse {
            message: "expected syntax-rules expression".to_string(),
        });
    };
    let [Expr::Atom(kw, _), Expr::List(lit_exprs, _), rules @ ..] = sr_items.as_slice() else {
        return Err(EvalError::Parse {
            message: "malformed syntax-rules".to_string(),
        });
    };
    if kw != "syntax-rules" {
        return Err(EvalError::Parse {
            message: format!("expected syntax-rules, got {kw}"),
        });
    }
    let literals: Vec<String> = lit_exprs
        .iter()
        .map(|e| match e {
            Expr::Atom(n, _) => Ok(n.clone()),
            _ => Err(EvalError::Parse {
                message: "literal must be an identifier".to_string(),
            }),
        })
        .collect::<Result<_, _>>()?;
    let parsed_rules: Vec<(Vec<Expr>, Expr)> = rules
        .iter()
        .map(|rule| {
            let Expr::List(parts, _) = rule else {
                return Err(EvalError::Parse {
                    message: "syntax rule must be a list".to_string(),
                });
            };
            let [Expr::List(pattern, _), template] = parts.as_slice() else {
                return Err(EvalError::Parse {
                    message: "syntax rule must be (pattern template)".to_string(),
                });
            };
            Ok((pattern.clone(), template.clone()))
        })
        .collect::<Result<_, _>>()?;
    let macro_val = Value::Macro {
        rules: parsed_rules,
        literals,
        def_env: env.clone(),
    };
    env_define(env, name.clone(), macro_val);
    Ok(Value::Nil)
}

/// Evaluate `(newline)` — prints a newline character.
fn eval_newline(args: &[Expr], out: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 0,
            got: args.len(),
        });
    }
    out.push('\n');
    Ok(Value::Nil)
}
