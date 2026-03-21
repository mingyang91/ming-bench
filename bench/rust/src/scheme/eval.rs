use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::macros;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Trampoline result: either a final value or a tail-call continuation.
enum Bounce {
    Done(Value),
    Tco(Expr, Env),
}

/// Evaluate a parsed expression in the given environment.
/// Uses a trampoline loop for tail call optimization.
pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let mut cur = expr.clone();
    let mut cur_env = env.clone();

    loop {
        match cur {
            Expr::Integer(n, _) => return Ok(Value::Integer(n)),
            Expr::Boolean(b, _) => return Ok(Value::Boolean(b)),
            Expr::String(ref s, _) => return Ok(Value::String(s.clone())),
            Expr::Char(c, _) => return Ok(Value::Char(c)),
            Expr::Symbol(ref name, span) => return resolve_symbol(name, span, &cur_env),
            Expr::List(ref elems, span) => match eval_list(elems, span, &cur_env)? {
                Bounce::Done(val) => return Ok(val),
                Bounce::Tco(next_expr, next_env) => {
                    cur = next_expr;
                    cur_env = next_env;
                }
            },
        }
    }
}

/// Build an environment with hygiene bindings from macro expansion.
fn make_hygiene_env(env: &Env, hygiene_bindings: Vec<(String, Value)>) -> Env {
    if hygiene_bindings.is_empty() {
        return env.clone();
    }
    let e = Env::extend(env);
    for (name, val) in hygiene_bindings {
        e.define(name, val);
    }
    e
}

/// Resolve a symbol: check environment, then builtins.
fn resolve_symbol(name: &str, span: Span, env: &Env) -> Result<Value, EvalError> {
    if let Some(val) = env.get(name) {
        return Ok(val);
    }
    if is_builtin(name)
        || name == "apply"
        || name == "call/cc"
        || name == "call-with-current-continuation"
    {
        return Ok(Value::Builtin(name.to_string()));
    }
    Err(EvalError::UnboundVariable { name: name.to_string() }.at(span))
}

/// Extract an integer from a Value, or return a TypeError.
fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::TypeError { expected: "integer".into(), got: format!("{v}") }),
    }
}

/// Integer division with zero-check.
fn checked_div(a: i64, b: i64) -> Result<i64, EvalError> {
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(a / b)
}

/// Evaluate a list form (special forms, builtins, or lambda application).
fn eval_list(elems: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let [ref operator, ref args @ ..] = elems else {
        return Err(EvalError::Parse("empty list".into()).at(span));
    };

    // Check for call/cc before special forms
    if let Expr::Symbol(ref op, _) = operator {
        if op == "call/cc" || op == "call-with-current-continuation" {
            return eval_callcc(args, span, env).map(Bounce::Done);
        }
        if let Some(bounce) = eval_special_form(op, args, span, env)? {
            return Ok(bounce);
        }
        if let Some(Value::Macro {
            ref literals,
            ref rules,
            ref def_env,
        }) = env.get(op)
        {
            let expansion =
                macros::expand_macro(literals, rules, def_env, args, span, env)?;
            let eval_env = make_hygiene_env(env, expansion.hygiene_bindings);
            return Ok(Bounce::Tco(expansion.expr, eval_env));
        }
        if is_builtin(op) {
            return eval_builtin(op, args, env)
                .map(Bounce::Done)
                .map_err(|e| e.at(span));
        }
    }

    // Evaluate operator to get a callable value
    let op_val = eval(operator, env)?;
    eval_application(op_val, args, span, env)
}

/// Dispatch special forms. Returns `None` if `op` is not a special form.
fn eval_special_form(
    op: &str,
    args: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Option<Bounce>, EvalError> {
    match op {
        "quote" => eval_quote(args).map(Bounce::Done).map(Some).map_err(|e| e.at(span)),
        "if" => eval_if(args, span, env).map(Some),
        "define" => eval_define(args, span, env).map(Bounce::Done).map(Some),
        "lambda" => eval_lambda(args, env)
            .map(Bounce::Done)
            .map(Some)
            .map_err(|e| e.at(span)),
        "let" => eval_let(args, span, env).map(Some),
        "begin" => eval_begin(args, env).map(Some),
        "cond" => eval_cond(args, span, env).map(Some),
        "and" => eval_and(args, env).map(Some),
        "or" => eval_or(args, env).map(Some),
        "set!" => eval_set(args, span, env).map(Bounce::Done).map(Some),
        "string-set!" => eval_string_set(args, env)
            .map(Bounce::Done)
            .map(Some)
            .map_err(|e| e.at(span)),
        "define-syntax" => eval_define_syntax(args, span, env)
            .map(Bounce::Done)
            .map(Some),
        "letrec" => eval_letrec(args, span, env).map(Some),
        "letrec*" => eval_letrec_star(args, span, env).map(Some),
        "case" => eval_case(args, span, env).map(Some),
        _ => Ok(None),
    }
}

fn eval_if(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let (cond_expr, consequent, alternate) = match args {
        [c, t, f] => (c, t, Some(f)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 3,
                got: args.len(),
            }
            .at(span))
        }
    };
    let cond_val = eval(cond_expr, env)?;
    if cond_val.is_truthy() {
        Ok(Bounce::Tco(consequent.clone(), env.clone()))
    } else if let Some(alt) = alternate {
        Ok(Bounce::Tco(alt.clone(), env.clone()))
    } else {
        Ok(Bounce::Done(Value::Void))
    }
}

fn eval_let(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    // Named let: (let name ((var init) ...) body ...)
    if let [Expr::Symbol(ref name, _), Expr::List(ref bindings, _), ref body @ ..] = args {
        return eval_named_let(name, bindings, body, span, env);
    }
    // Regular let: (let ((var init) ...) body ...)
    let [Expr::List(ref bindings, _), ref body @ ..] = args else {
        return Err(EvalError::Parse("invalid let form".into()).at(span));
    };
    if body.is_empty() {
        return Err(EvalError::Parse("let requires a body".into()).at(span));
    }
    let let_env = Env::extend(env);
    for binding in bindings {
        let (bname, val) = parse_and_eval_binding(binding, span, env)?;
        let_env.define(bname, val);
    }
    eval_body_tco(body, let_env)
}

fn eval_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Bounce, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Parse("named let requires a body".into()).at(span));
    }
    let mut param_names = Vec::new();
    let mut init_vals = Vec::new();
    for binding in bindings {
        let (pname, val) = parse_and_eval_binding(binding, span, env)?;
        param_names.push(pname);
        init_vals.push(val);
    }
    let loop_body = wrap_body(body, span);
    let let_env = Env::extend(env);
    let lambda = Value::Lambda {
        params: param_names.clone(),
        rest_param: None,
        body: loop_body.clone(),
        closure: let_env.clone(),
    };
    let_env.define(name.into(), lambda);
    for (pname, val) in param_names.iter().zip(init_vals) {
        let_env.define(pname.clone(), val);
    }
    Ok(Bounce::Tco(loop_body, let_env))
}

/// Parse a single let binding `(name expr)` and evaluate the init expression.
fn parse_and_eval_binding(
    binding: &Expr,
    span: Span,
    env: &Env,
) -> Result<(String, Value), EvalError> {
    let Expr::List(ref pair, _) = binding else {
        return Err(EvalError::Parse("let binding must be a list".into()).at(span));
    };
    let [Expr::Symbol(ref bname, _), ref val_expr] = pair.as_slice() else {
        return Err(EvalError::Parse("let binding must be (name expr)".into()).at(span));
    };
    let val = eval(val_expr, env)?;
    Ok((bname.clone(), val))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Bounce, EvalError> {
    eval_body_tco(args, env.clone())
}

fn eval_cond(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    for clause in args {
        let Expr::List(ref parts, _) = clause else {
            return Err(EvalError::Parse("cond clause must be a list".into()).at(span));
        };
        let [ref test, ref body @ ..] = parts.as_slice() else {
            return Err(EvalError::Parse("cond clause must have a test".into()).at(span));
        };
        let is_else = matches!(test, Expr::Symbol(s, _) if s == "else");
        if is_else || eval(test, env)?.is_truthy() {
            return eval_body_tco(body, env.clone());
        }
    }
    Ok(Bounce::Done(Value::Void))
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = args.split_last() else {
        return Ok(Bounce::Done(Value::Boolean(true)));
    };
    for expr in rest {
        let val = eval(expr, env)?;
        if !val.is_truthy() {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::Tco(last.clone(), env.clone()))
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = args.split_last() else {
        return Ok(Bounce::Done(Value::Boolean(false)));
    };
    for expr in rest {
        let val = eval(expr, env)?;
        if val.is_truthy() {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::Tco(last.clone(), env.clone()))
}

/// Evaluate a body sequence with TCO on the last expression.
fn eval_body_tco(body: &[Expr], env: Env) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = body.split_last() else {
        return Ok(Bounce::Done(Value::Void));
    };
    for expr in rest {
        eval(expr, &env)?;
    }
    Ok(Bounce::Tco(last.clone(), env))
}

/// Apply a callable value to evaluated arguments.
fn eval_application(
    op_val: Value,
    args: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Bounce, EvalError> {
    match op_val {
        Value::Lambda { .. } => {
            let arg_vals: Vec<Value> =
                args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_lambda_values(op_val, &arg_vals, span)
        }
        Value::Builtin(ref name) if name == "apply" => {
            let arg_vals: Vec<Value> =
                args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            eval_apply(&arg_vals, span, env)
        }
        Value::Builtin(ref name)
            if name == "call/cc" || name == "call-with-current-continuation" =>
        {
            eval_callcc(args, span, env).map(Bounce::Done)
        }
        Value::Builtin(ref name) => {
            let arg_vals: Vec<Value> =
                args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_builtin_values(name, &arg_vals).map(Bounce::Done).map_err(|e| e.at(span))
        }
        Value::Continuation { id, expr_index } => {
            let [ref arg_expr] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .at(span));
            };
            let value = eval(arg_expr, env)?;
            invoke_continuation(id, value, expr_index, env)
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{op_val}"),
        }
        .at(span)),
    }
}

/// Invoke a continuation: escape if active, restart if from different expr, return if same expr.
fn invoke_continuation(
    id: u64,
    value: Value,
    expr_index: usize,
    env: &Env,
) -> Result<Bounce, EvalError> {
    if env.is_callcc_active(id) || expr_index != env.current_expr_index() {
        Err(EvalError::ContinuationReturn {
            cont_id: id,
            value: Box::new(value),
            expr_index,
        })
    } else {
        Ok(Bounce::Done(value))
    }
}

/// Apply a lambda to already-evaluated argument values.
fn apply_lambda_values(
    lambda: Value,
    arg_vals: &[Value],
    span: Span,
) -> Result<Bounce, EvalError> {
    let Value::Lambda {
        params,
        rest_param,
        body,
        closure,
    } = lambda
    else {
        unreachable!("caller ensures lambda");
    };
    let min_params = params.len();
    if rest_param.is_some() {
        if arg_vals.len() < min_params {
            return Err(EvalError::WrongArgCount {
                expected: min_params,
                got: arg_vals.len(),
            }
            .at(span));
        }
    } else if arg_vals.len() != min_params {
        return Err(EvalError::WrongArgCount {
            expected: min_params,
            got: arg_vals.len(),
        }
        .at(span));
    }
    let call_env = Env::extend(&closure);
    for (param, val) in params.iter().zip(arg_vals.iter()) {
        call_env.define(param.clone(), val.clone());
    }
    if let Some(rest_name) = rest_param {
        let rest_vals = arg_vals[min_params..].to_vec();
        call_env.define(rest_name, Value::List(rest_vals));
    }
    Ok(Bounce::Tco(body, call_env))
}

/// Implement (apply proc arg1 ... args-list).
fn eval_apply(args: &[Value], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        }
        .at(span));
    }
    let proc = &args[0];
    let Value::List(ref tail_list) = args[args.len() - 1] else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{}", args[args.len() - 1]),
        }
        .at(span));
    };
    let mut combined: Vec<Value> = args[1..args.len() - 1].to_vec();
    combined.extend(tail_list.iter().cloned());

    match proc {
        Value::Lambda { .. } => apply_lambda_values(proc.clone(), &combined, span),
        Value::Builtin(ref name) if name == "apply" => eval_apply(&combined, span, env),
        Value::Builtin(ref name)
            if name == "call/cc" || name == "call-with-current-continuation" =>
        {
            if combined.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: combined.len(),
                }
                .at(span));
            }
            eval_callcc_with_proc(combined.into_iter().next().expect("len checked"), span, env)
                .map(Bounce::Done)
        }
        Value::Continuation { id, expr_index } => {
            if combined.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: combined.len(),
                }
                .at(span));
            }
            invoke_continuation(
                *id,
                combined.into_iter().next().expect("len checked"),
                *expr_index,
                env,
            )
        }
        Value::Builtin(ref name) => {
            apply_builtin_values(name, &combined).map(Bounce::Done).map_err(|e| e.at(span))
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }
        .at(span)),
    }
}

fn eval_callcc(args: &[Expr], span: Span, env: &Env) -> Result<Value, EvalError> {
    let [ref proc_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        }
        .at(span));
    };
    let proc = eval(proc_expr, env)?;
    eval_callcc_with_proc(proc, span, env)
}

fn eval_callcc_with_proc(proc: Value, span: Span, env: &Env) -> Result<Value, EvalError> {
    let id = env.next_callcc_id();

    if let Some(value) = env.take_pending_cont(id) {
        return Ok(value);
    }

    let expr_index = env.current_expr_index();
    let cont = Value::Continuation { id, expr_index };

    env.activate_callcc(id);
    let result = if matches!(proc, Value::Lambda { .. }) {
        match apply_lambda_values(proc, &[cont], span) {
            Ok(Bounce::Done(val)) => Ok(val),
            Ok(Bounce::Tco(expr, tco_env)) => eval(&expr, &tco_env),
            Err(e) => Err(e),
        }
    } else {
        env.deactivate_callcc(id);
        return Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }
        .at(span));
    };
    env.deactivate_callcc(id);

    match result {
        Err(EvalError::ContinuationReturn {
            cont_id, value, ..
        }) if cont_id == id => Ok(*value),
        other => other,
    }
}

fn wrap_body(body: &[Expr], span: Span) -> Expr {
    if body.len() == 1 {
        body[0].clone()
    } else {
        let mut begin = vec![Expr::Symbol("begin".into(), span)];
        begin.extend(body.iter().cloned());
        Expr::List(begin, span)
    }
}

/// Parse parameter list, handling dot notation for rest params.
/// `(x y . rest)` → (["x", "y"], Some("rest"))
fn parse_params(params: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    // Look for a dot
    let dot_pos = params
        .iter()
        .position(|p| matches!(p, Expr::Symbol(s, _) if s == "."));

    let Some(dot_idx) = dot_pos else {
        // No dot — all regular params
        let names: Vec<String> = params
            .iter()
            .map(|p| match p {
                Expr::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Parse("parameter must be a symbol".into()).at(span)),
            })
            .collect::<Result<_, _>>()?;
        return Ok((names, None));
    };

    // Dot found: params before dot are regular, one param after dot is rest
    let regular = &params[..dot_idx];
    let rest = &params[dot_idx + 1..];
    let [Expr::Symbol(ref rest_name, _)] = rest else {
        return Err(EvalError::Parse("expected exactly one parameter after dot".into()).at(span));
    };
    let names: Vec<String> = regular
        .iter()
        .map(|p| match p {
            Expr::Symbol(s, _) => Ok(s.clone()),
            _ => Err(EvalError::Parse("parameter must be a symbol".into()).at(span)),
        })
        .collect::<Result<_, _>>()?;
    Ok((names, Some(rest_name.clone())))
}

/// Apply arithmetic builtin operations.
fn apply_arithmetic_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => args.iter()
            .try_fold(0i64, |acc, v| Ok(acc + expect_integer(v)?))
            .map(Value::Integer),
        "-" => {
            let [first, rest @ ..] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            let first_val = expect_integer(first)?;
            if rest.is_empty() {
                return Ok(Value::Integer(-first_val));
            }
            rest.iter()
                .try_fold(first_val, |acc, v| Ok(acc - expect_integer(v)?))
                .map(Value::Integer)
        }
        "*" => args.iter()
            .try_fold(1i64, |acc, v| Ok(acc * expect_integer(v)?))
            .map(Value::Integer),
        "/" => {
            let [first, rest @ ..] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            let first_val = expect_integer(first)?;
            rest.iter()
                .try_fold(first_val, |acc, v| checked_div(acc, expect_integer(v)?))
                .map(Value::Integer)
        }
        "abs" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            Ok(Value::Integer(expect_integer(arg)?.abs()))
        }
        "modulo" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let (a, b) = (expect_integer(a)?, expect_integer(b)?);
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let (a, b) = (expect_integer(a)?, expect_integer(b)?);
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let (a, b) = (expect_integer(a)?, expect_integer(b)?);
            checked_div(a, b).map(Value::Integer)
        }
        "min" => {
            let [first, rest @ ..] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            rest.iter().try_fold(expect_integer(first)?, |acc, v| {
                Ok(acc.min(expect_integer(v)?))
            }).map(Value::Integer)
        }
        "max" => {
            let [first, rest @ ..] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            rest.iter().try_fold(expect_integer(first)?, |acc, v| {
                Ok(acc.max(expect_integer(v)?))
            }).map(Value::Integer)
        }
        "expt" => {
            let [base, exp] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let (b, e) = (expect_integer(base)?, expect_integer(exp)?);
            Ok(Value::Integer(b.pow(e as u32)))
        }
        _ => unreachable!("not an arithmetic builtin: {name}"),
    }
}

/// Apply list-related builtin operations.
fn apply_list_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            let [h, t] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            match t {
                Value::List(items) => {
                    let mut new = vec![h.clone()];
                    new.extend(items.iter().cloned());
                    Ok(Value::List(new))
                }
                _ => Ok(Value::Pair(Box::new(h.clone()), Box::new(t.clone()))),
            }
        }
        "car" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            match arg {
                Value::Pair(car, _) => Ok(car.as_ref().clone()),
                Value::List(items) => items.first().cloned().ok_or_else(|| EvalError::TypeError { expected: "pair".into(), got: "()".into() }),
                _ => Err(EvalError::TypeError { expected: "pair".into(), got: format!("{arg}") }),
            }
        }
        "cdr" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            match arg {
                Value::Pair(_, cdr) => Ok(cdr.as_ref().clone()),
                Value::List(items) if items.is_empty() => Err(EvalError::TypeError { expected: "pair".into(), got: "()".into() }),
                Value::List(items) => Ok(Value::List(items[1..].to_vec())),
                _ => Err(EvalError::TypeError { expected: "pair".into(), got: format!("{arg}") }),
            }
        }
        "null?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            Ok(Value::Boolean(matches!(arg, Value::List(l) if l.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let Value::List(items) = arg else {
                return Err(EvalError::TypeError { expected: "list".into(), got: format!("{arg}") });
            };
            Ok(Value::Integer(items.len() as i64))
        }
        "list-ref" => {
            let [list, idx] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let Value::List(items) = list else {
                return Err(EvalError::TypeError { expected: "list".into(), got: format!("{list}") });
            };
            let i = expect_integer(idx)? as usize;
            items.get(i).cloned().ok_or_else(|| EvalError::TypeError {
                expected: "valid list index".into(),
                got: format!("index {i} out of range"),
            })
        }
        "list-tail" => {
            let [list, idx] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let Value::List(items) = list else {
                return Err(EvalError::TypeError { expected: "list".into(), got: format!("{list}") });
            };
            let i = expect_integer(idx)? as usize;
            if i > items.len() {
                return Err(EvalError::TypeError {
                    expected: "valid list index".into(),
                    got: format!("index {i} out of range"),
                });
            }
            Ok(Value::List(items[i..].to_vec()))
        }
        "list?" => Ok(Value::Boolean(matches!(args, [v] if is_proper_list(v)))),
        "assoc" => {
            let [key, alist] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let Value::List(items) = alist else {
                return Err(EvalError::TypeError { expected: "list".into(), got: format!("{alist}") });
            };
            let found = items.iter().find(|entry| {
                matches!(entry, Value::List(pair) if !pair.is_empty() && values_equal(key, &pair[0]))
            });
            Ok(found.cloned().unwrap_or(Value::Boolean(false)))
        }
        _ => unreachable!("not a list builtin: {name}"),
    }
}

/// Apply a builtin procedure to already-evaluated argument values.
fn apply_builtin_values(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "abs" | "modulo" | "remainder" | "quotient"
        | "min" | "max" | "expt" => apply_arithmetic_builtin(name, args),
        "cons" | "car" | "cdr" | "null?" | "list" | "length"
        | "list-ref" | "list-tail" | "list?" | "assoc" => apply_list_builtin(name, args),
        "<" => eval_cmp_values(args, |a, b| a < b),
        ">" => eval_cmp_values(args, |a, b| a > b),
        "=" => eval_cmp_values(args, |a, b| a == b),
        "<=" => eval_cmp_values(args, |a, b| a <= b),
        ">=" => eval_cmp_values(args, |a, b| a >= b),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            Ok(Value::Boolean(!arg.is_truthy()))
        }
        "string?" => Ok(Value::Boolean(matches!(args, [Value::String(_)]))),
        "number?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        "boolean?" => Ok(Value::Boolean(matches!(args, [Value::Boolean(_)]))),
        "pair?" => Ok(Value::Boolean(matches!(args, [ref v] if v.is_pair()))),
        "symbol?" => Ok(Value::Boolean(matches!(args, [Value::Symbol(_)]))),
        "char?" => Ok(Value::Boolean(matches!(args, [Value::Char(_)]))),
        "eq?" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            Ok(Value::Boolean(values_eq(a, b)))
        }
        "eqv?" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            Ok(Value::Boolean(values_eqv(a, b)))
        }
        "equal?" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            Ok(Value::Boolean(values_equal(a, b)))
        }
        "zero?" => Ok(Value::Boolean(matches!(args, [Value::Integer(0)]))),
        "positive?" => Ok(Value::Boolean(matches!(args, [Value::Integer(n)] if *n > 0))),
        "negative?" => Ok(Value::Boolean(matches!(args, [Value::Integer(n)] if *n < 0))),
        "odd?" => Ok(Value::Boolean(matches!(args, [Value::Integer(n)] if n % 2 != 0))),
        "even?" => Ok(Value::Boolean(matches!(args, [Value::Integer(n)] if n % 2 == 0))),
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?" => apply_char_builtin(name, args),
        "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" =>
            apply_string_builtin(name, args),
        "map" => apply_map_builtin(args),
        "vector" => Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(args.to_vec())))),
        "make-vector" => apply_make_vector(args),
        "vector-ref" => apply_vector_ref(args),
        "vector-set!" => apply_vector_set(args),
        "vector-length" => apply_vector_length(args),
        "vector?" => Ok(Value::Boolean(matches!(args, [Value::Vector(_)]))),
        "vector->list" => apply_vector_to_list(args),
        "list->vector" => apply_list_to_vector(args),
        "procedure?" => Ok(Value::Boolean(matches!(args, [Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation { .. }]))),
        "integer?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn call_proc_values(proc: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda { .. } => {
            match apply_lambda_values(proc.clone(), args, Span { line: 0, col: 0 })? {
                Bounce::Done(val) => Ok(val),
                Bounce::Tco(expr, tco_env) => eval(&expr, &tco_env),
            }
        }
        Value::Builtin(ref bname) => apply_builtin_values(bname, args),
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

fn eval_cmp_values(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Integer(a) = left else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: format!("{left}") });
    };
    let Value::Integer(b) = right else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: format!("{right}") });
    };
    Ok(Value::Boolean(cmp(*a, *b)))
}

fn apply_char_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "char-alphabetic?" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError { expected: "char".into(), got: format!("{args:?}") });
            };
            Ok(Value::Boolean(c.is_alphabetic()))
        }
        "char-numeric?" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError { expected: "char".into(), got: format!("{args:?}") });
            };
            Ok(Value::Boolean(c.is_ascii_digit()))
        }
        "char-upcase" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError { expected: "char".into(), got: format!("{args:?}") });
            };
            Ok(Value::Char(c.to_ascii_uppercase()))
        }
        "char-downcase" => {
            let [Value::Char(c)] = args else {
                return Err(EvalError::TypeError { expected: "char".into(), got: format!("{args:?}") });
            };
            Ok(Value::Char(c.to_ascii_lowercase()))
        }
        "char=?" => {
            let [Value::Char(a), Value::Char(b)] = args else {
                return Err(EvalError::TypeError { expected: "char".into(), got: format!("{args:?}") });
            };
            Ok(Value::Boolean(a == b))
        }
        "char<?" => {
            let [Value::Char(a), Value::Char(b)] = args else {
                return Err(EvalError::TypeError { expected: "char".into(), got: format!("{args:?}") });
            };
            Ok(Value::Boolean(a < b))
        }
        _ => unreachable!("not a char builtin: {name}"),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string=?" | "string<?" | "string-ci=?" => {
            let [Value::String(a), Value::String(b)] = args else {
                return Err(EvalError::TypeError { expected: "string".into(), got: format!("{args:?}") });
            };
            let result = match name {
                "string=?" => a == b,
                "string<?" => a < b,
                "string-ci=?" => a.to_lowercase() == b.to_lowercase(),
                _ => unreachable!(),
            };
            Ok(Value::Boolean(result))
        }
        "string-upcase" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError { expected: "string".into(), got: format!("{args:?}") });
            };
            Ok(Value::String(s.to_uppercase()))
        }
        "string-downcase" => {
            let [Value::String(s)] = args else {
                return Err(EvalError::TypeError { expected: "string".into(), got: format!("{args:?}") });
            };
            Ok(Value::String(s.to_lowercase()))
        }
        _ => unreachable!("not a string builtin: {name}"),
    }
}

fn apply_map_builtin(args: &[Value]) -> Result<Value, EvalError> {
    let [proc, list_args @ ..] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    if list_args.is_empty() {
        return Err(EvalError::WrongArgCount { expected: 2, got: 1 });
    }
    let lists: Vec<&Vec<Value>> = list_args
        .iter()
        .map(|v| match v {
            Value::List(items) => Ok(items),
            _ => Err(EvalError::TypeError { expected: "list".into(), got: format!("{v}") }),
        })
        .collect::<Result<_, _>>()?;
    let len = lists[0].len();
    let mut results = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        let result = call_proc_values(proc, &call_args)?;
        results.push(result);
    }
    Ok(Value::List(results))
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
            | "string-ref" | "symbol->string" | "string->symbol"
            | "string-copy"
            | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "eq?" | "eqv?" | "equal?"
            | "abs" | "modulo" | "remainder" | "quotient"
            | "min" | "max" | "expt"
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "list-ref" | "list-tail" | "list?" | "assoc" | "map"
            | "char-alphabetic?" | "char-numeric?"
            | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string=?" | "string<?" | "string-ci=?"
            | "string-upcase" | "string-downcase"
            | "vector" | "make-vector" | "vector-ref" | "vector-set!"
            | "vector-length" | "vector?" | "vector->list" | "list->vector"
            | "procedure?" | "integer?"
    )
}

fn eval_builtin(op: &str, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    match op {
        "+" => eval_arithmetic(args, env, 0, |a, b| Ok(a + b)),
        "-" => eval_minus(args, env),
        "*" => eval_arithmetic(args, env, 1, |a, b| Ok(a * b)),
        "/" => eval_divide(args, env),
        "<" => eval_comparison(args, env, |a, b| a < b),
        ">" => eval_comparison(args, env, |a, b| a > b),
        "=" => eval_comparison(args, env, |a, b| a == b),
        "<=" => eval_comparison(args, env, |a, b| a <= b),
        ">=" => eval_comparison(args, env, |a, b| a >= b),
        "not" => eval_not(args, env),
        "cons" => eval_cons(args, env),
        "car" => eval_car(args, env),
        "cdr" => eval_cdr(args, env),
        "null?" => eval_null(args, env),
        "list" => eval_list_builtin(args, env),
        "length" => eval_length(args, env),
        "string?" => eval_type_pred(args, env, |v| matches!(v, Value::String(_))),
        "number?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(_))),
        "boolean?" => eval_type_pred(args, env, |v| matches!(v, Value::Boolean(_))),
        "pair?" => eval_type_pred(args, env, |v| v.is_pair()),
        "symbol?" => eval_type_pred(args, env, |v| matches!(v, Value::Symbol(_))),
        "char?" => eval_type_pred(args, env, |v| matches!(v, Value::Char(_))),
        "display" => eval_display(args, env),
        "write" => eval_write(args, env),
        "newline" => eval_newline(args, env),
        "string-append" => eval_string_append(args, env),
        "string-length" => eval_string_length(args, env),
        "substring" => eval_substring(args, env),
        "string->number" => eval_string_to_number(args, env),
        "number->string" => eval_number_to_string(args, env),
        "string-ref" => eval_string_ref(args, env),
        "symbol->string" => eval_symbol_to_string(args, env),
        "string->symbol" => eval_string_to_symbol(args, env),
        "string-copy" => eval_string_copy(args, env),
        "string->list" => eval_string_to_list(args, env),
        "list->string" => eval_list_to_string(args, env),
        "char->integer" => eval_char_to_integer(args, env),
        "integer->char" => eval_integer_to_char(args, env),
        "eq?" => eval_eq(args, env),
        "equal?" => eval_equal(args, env),
        "abs" => eval_abs(args, env),
        "modulo" => eval_modulo(args, env),
        "remainder" => eval_remainder(args, env),
        "quotient" => eval_quotient(args, env),
        "min" => eval_min_max(args, env, true),
        "max" => eval_min_max(args, env, false),
        "expt" => eval_expt(args, env),
        "zero?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(0))),
        "positive?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(n) if *n > 0)),
        "negative?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(n) if *n < 0)),
        "odd?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(n) if n % 2 != 0)),
        "even?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(n) if n % 2 == 0)),
        "list-ref" => eval_list_ref(args, env),
        "list-tail" => eval_list_tail(args, env),
        "list?" => eval_type_pred(args, env, is_proper_list),
        "assoc" => eval_assoc(args, env),
        "map" => eval_map(args, env),
        "char-alphabetic?" => eval_char_pred(args, env, |c| c.is_alphabetic()),
        "char-numeric?" => eval_char_pred(args, env, |c| c.is_ascii_digit()),
        "char-upcase" => eval_char_transform(args, env, |c| c.to_ascii_uppercase()),
        "char-downcase" => eval_char_transform(args, env, |c| c.to_ascii_lowercase()),
        "char=?" => eval_char_cmp(args, env, |a, b| a == b),
        "char<?" => eval_char_cmp(args, env, |a, b| a < b),
        "string=?" => eval_string_cmp(args, env, |a, b| a == b),
        "string<?" => eval_string_cmp(args, env, |a, b| a < b),
        "string-ci=?" => eval_string_cmp(args, env, |a, b| {
            a.to_lowercase() == b.to_lowercase()
        }),
        "string-upcase" => eval_string_case(args, env, |s| s.to_uppercase()),
        "string-downcase" => eval_string_case(args, env, |s| s.to_lowercase()),
        "eqv?" => eval_eqv(args, env),
        "vector" => eval_vector_create(args, env),
        "make-vector" => eval_make_vector(args, env),
        "vector-ref" => eval_vector_ref(args, env),
        "vector-set!" => eval_vector_set(args, env),
        "vector-length" => eval_vector_length(args, env),
        "vector?" => eval_type_pred(args, env, |v| matches!(v, Value::Vector(_))),
        "vector->list" => eval_vector_to_list(args, env),
        "list->vector" => eval_list_to_vector(args, env),
        "procedure?" => eval_type_pred(args, env, |v| {
            matches!(v, Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation { .. })
        }),
        "integer?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(_))),
        _ => Err(EvalError::UnboundVariable { name: op.into() }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    expr_to_value(arg)
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::String(s, _) => Ok(Value::String(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
        Expr::Symbol(s, _) => Ok(Value::Symbol(s.clone())),
        Expr::List(items, _) => {
            let vals: Vec<Value> = items.iter().map(expr_to_value).collect::<Result<_, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_define(args: &[Expr], span: Span, env: &Env) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Expr::Symbol(name, _), expr] => {
            let val = eval(expr, env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        [Expr::List(name_and_params, _), body @ ..] if !body.is_empty() => {
            let [Expr::Symbol(name, _), params @ ..] = name_and_params.as_slice() else {
                return Err(EvalError::Parse("invalid define form".into()).at(span));
            };
            let (param_names, rest_param) = parse_params(params, span)?;
            let wrapped_body = wrap_body(body, span);
            let lambda = Value::Lambda {
                params: param_names,
                rest_param,
                body: wrapped_body,
                closure: env.clone(),
            };
            env.define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("invalid define form".into()).at(span)),
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [Expr::List(params, span), body @ ..] = args else {
        // (lambda rest-symbol body) — single symbol means all args go to rest
        let [Expr::Symbol(ref rest_name, span), body @ ..] = args else {
            return Err(EvalError::Parse("invalid lambda form".into()));
        };
        if body.is_empty() {
            return Err(EvalError::Parse("lambda requires a body".into()));
        }
        let wrapped_body = wrap_body(body, *span);
        return Ok(Value::Lambda {
            params: vec![],
            rest_param: Some(rest_name.clone()),
            body: wrapped_body,
            closure: env.clone(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse("lambda requires a body".into()));
    }
    let (param_names, rest_param) = parse_params(params, *span)?;
    let wrapped_body = wrap_body(body, *span);
    Ok(Value::Lambda {
        params: param_names,
        rest_param,
        body: wrapped_body,
        closure: env.clone(),
    })
}

fn eval_define_syntax(args: &[Expr], span: Span, env: &Env) -> Result<Value, EvalError> {
    let [Expr::Symbol(ref name, _), Expr::List(ref sr_form, _)] = args else {
        return Err(EvalError::Parse("invalid define-syntax form".into()).at(span));
    };
    let [Expr::Symbol(ref sr, _), Expr::List(ref lit_exprs, _), ref rule_exprs @ ..] =
        sr_form.as_slice()
    else {
        return Err(EvalError::Parse("expected syntax-rules form".into()).at(span));
    };
    if sr != "syntax-rules" {
        return Err(EvalError::Parse("expected syntax-rules".into()).at(span));
    }
    let literals: Vec<String> = lit_exprs
        .iter()
        .map(|e| match e {
            Expr::Symbol(s, _) => Ok(s.clone()),
            _ => Err(EvalError::Parse("literal must be a symbol".into()).at(span)),
        })
        .collect::<Result<_, _>>()?;
    let rules: Vec<(Vec<Expr>, Expr)> = rule_exprs
        .iter()
        .map(|r| {
            let Expr::List(ref parts, _) = r else {
                return Err(EvalError::Parse("rule must be a list".into()).at(span));
            };
            let [Expr::List(ref pattern, _), ref template] = parts.as_slice() else {
                return Err(
                    EvalError::Parse("rule must be (pattern template)".into()).at(span),
                );
            };
            Ok((pattern.clone(), template.clone()))
        })
        .collect::<Result<_, _>>()?;
    env.define(
        name.clone(),
        Value::Macro {
            literals,
            rules,
            def_env: env.clone(),
        },
    );
    Ok(Value::Void)
}

fn eval_set(args: &[Expr], span: Span, env: &Env) -> Result<Value, EvalError> {
    let [Expr::Symbol(ref name, _), ref val_expr] = args else {
        return Err(EvalError::Parse("set! requires (set! var expr)".into()).at(span));
    };
    let val = eval(val_expr, env)?;
    if !env.set(name, val) {
        return Err(EvalError::UnboundVariable { name: name.clone() }.at(span));
    }
    Ok(Value::Void)
}

fn eval_arithmetic(
    args: &[Expr],
    env: &Env,
    identity: i64,
    op: fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(identity, |acc, arg| {
            let val = eval(arg, env)?;
            let Value::Integer(n) = val else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{val}"),
                });
            };
            op(acc, n)
        })
        .map(Value::Integer)
}

fn eval_minus(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };

    let Value::Integer(first_val) = eval(first, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    if rest.is_empty() {
        return Ok(Value::Integer(-first_val));
    }

    rest.iter()
        .try_fold(first_val, |acc, arg| {
            let Value::Integer(n) = eval(arg, env)? else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: "non-integer".into(),
                });
            };
            Ok(acc - n)
        })
        .map(Value::Integer)
}

fn eval_divide(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };

    let Value::Integer(first_val) = eval(first, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    rest.iter()
        .try_fold(first_val, |acc, arg| {
            let Value::Integer(n) = eval(arg, env)? else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: "non-integer".into(),
                });
            };
            if n == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(acc / n)
        })
        .map(Value::Integer)
}

fn eval_comparison(
    args: &[Expr],
    env: &Env,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };

    let Value::Integer(a) = eval(left, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };
    let Value::Integer(b) = eval(right, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    Ok(Value::Boolean(cmp(a, b)))
}

fn eval_not(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn eval_cons(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let h = eval(head, env)?;
    let t = eval(tail, env)?;
    match t {
        Value::List(mut items) => {
            items.insert(0, h);
            Ok(Value::List(items))
        }
        _ => Ok(Value::Pair(Box::new(h), Box::new(t))),
    }
}

fn eval_car(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    match val {
        Value::Pair(car, _) => Ok(*car),
        Value::List(items) => items.into_iter().next().ok_or_else(|| EvalError::TypeError {
            expected: "pair".into(),
            got: "()".into(),
        }),
        _ => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{val}"),
        }),
    }
}

fn eval_cdr(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    match val {
        Value::Pair(_, cdr) => Ok(*cdr),
        Value::List(items) if items.is_empty() => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: "()".into(),
        }),
        Value::List(items) => Ok(Value::List(items[1..].to_vec())),
        _ => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{val}"),
        }),
    }
}

fn eval_null(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(
        matches!(val, Value::List(ref items) if items.is_empty()),
    ))
}

fn eval_list_builtin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let items: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    Ok(Value::List(items))
}

fn eval_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let Value::List(items) = val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(items.len() as i64))
}

fn eval_type_pred(
    args: &[Expr],
    env: &Env,
    pred: fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(pred(&val)))
}

fn eval_display(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let mut buf = String::new();
    val.display_fmt(&mut buf);
    env.write_output(&buf);
    Ok(Value::Void)
}

fn eval_write(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    env.write_output(&val.to_string());
    Ok(Value::Void)
}

fn eval_newline(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount { expected: 0, got: args.len() });
    }
    env.write_output("\n");
    Ok(Value::Void)
}

fn eval_string_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let result: String = args
        .iter()
        .map(|a| {
            let val = eval(a, env)?;
            let Value::String(s) = val else {
                return Err(EvalError::TypeError {
                    expected: "string".into(),
                    got: format!("{val}"),
                });
            };
            Ok(s)
        })
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(Value::String(result))
}

fn eval_string_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(s.len() as i64))
}

fn eval_substring(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [s_expr, start_expr, end_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    let val = eval(s_expr, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    let Value::Integer(start) = eval(start_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let Value::Integer(end) = eval(end_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let start = start as usize;
    let end = end as usize;
    if start > end || end > s.len() {
        return Err(EvalError::TypeError {
            expected: "valid substring indices".into(),
            got: format!("start={start}, end={end}, len={}", s.len()),
        });
    }
    Ok(Value::String(s[start..end].to_string()))
}

fn eval_string_to_number(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    match s.parse::<i64>() {
        Ok(n) => Ok(Value::Integer(n)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Integer(n) = val else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(n.to_string()))
}

fn eval_string_ref(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [s_expr, idx_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let val = eval(s_expr, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    let Value::Integer(idx) = eval(idx_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let idx = idx as usize;
    s.chars().nth(idx).map(Value::Char).ok_or_else(|| EvalError::TypeError {
        expected: "valid string index".into(),
        got: format!("index {idx} out of range for string of length {}", s.len()),
    })
}

fn eval_symbol_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Symbol(s) = val else {
        return Err(EvalError::TypeError {
            expected: "symbol".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(s))
}

fn eval_string_to_symbol(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Symbol(s))
}

fn eval_string_copy(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(s))
}

// ===== L13: Equality helpers =====

fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Void, Value::Void) => true,
        _ => std::ptr::eq(a as *const _, b as *const _),
    }
}

fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        _ => std::ptr::eq(a as *const _, b as *const _),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        (Value::List(x), Value::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Vector(x), Value::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

fn is_proper_list(v: &Value) -> bool {
    matches!(v, Value::List(_))
}

// ===== L13: Numeric builtins =====

fn eval_eq(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = eval(a_expr, env)?;
    let b = eval(b_expr, env)?;
    Ok(Value::Boolean(values_eq(&a, &b)))
}

fn eval_equal(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = eval(a_expr, env)?;
    let b = eval(b_expr, env)?;
    Ok(Value::Boolean(values_equal(&a, &b)))
}

fn eval_abs(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    Ok(Value::Integer(expect_integer(&val)?.abs()))
}

fn eval_modulo(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = expect_integer(&eval(a_expr, env)?)?;
    let b = expect_integer(&eval(b_expr, env)?)?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(((a % b) + b) % b))
}

fn eval_remainder(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = expect_integer(&eval(a_expr, env)?)?;
    let b = expect_integer(&eval(b_expr, env)?)?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(a % b))
}

fn eval_quotient(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = expect_integer(&eval(a_expr, env)?)?;
    let b = expect_integer(&eval(b_expr, env)?)?;
    checked_div(a, b).map(Value::Integer)
}

fn eval_min_max(args: &[Expr], env: &Env, is_min: bool) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
    };
    let mut result = expect_integer(&eval(first, env)?)?;
    for arg in rest {
        let n = expect_integer(&eval(arg, env)?)?;
        result = if is_min { result.min(n) } else { result.max(n) };
    }
    Ok(Value::Integer(result))
}

fn eval_expt(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [base_expr, exp_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let base = expect_integer(&eval(base_expr, env)?)?;
    let exp = expect_integer(&eval(exp_expr, env)?)?;
    Ok(Value::Integer(base.pow(exp as u32)))
}

// ===== L13: List builtins =====

fn eval_list_ref(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [list_expr, idx_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let list_val = eval(list_expr, env)?;
    let Value::List(items) = list_val else {
        return Err(EvalError::TypeError { expected: "list".into(), got: format!("{list_val}") });
    };
    let i = expect_integer(&eval(idx_expr, env)?)? as usize;
    items.get(i).cloned().ok_or_else(|| EvalError::TypeError {
        expected: "valid list index".into(),
        got: format!("index {i} out of range"),
    })
}

fn eval_list_tail(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [list_expr, idx_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let list_val = eval(list_expr, env)?;
    let Value::List(items) = list_val else {
        return Err(EvalError::TypeError { expected: "list".into(), got: format!("{list_val}") });
    };
    let i = expect_integer(&eval(idx_expr, env)?)? as usize;
    if i > items.len() {
        return Err(EvalError::TypeError {
            expected: "valid list index".into(),
            got: format!("index {i} out of range"),
        });
    }
    Ok(Value::List(items[i..].to_vec()))
}

fn eval_assoc(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [key_expr, alist_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let key = eval(key_expr, env)?;
    let alist = eval(alist_expr, env)?;
    let Value::List(items) = alist else {
        return Err(EvalError::TypeError { expected: "list".into(), got: format!("{alist}") });
    };
    let found = items.iter().find(|entry| {
        matches!(entry, Value::List(pair) if !pair.is_empty() && values_equal(&key, &pair[0]))
    });
    Ok(found.cloned().unwrap_or(Value::Boolean(false)))
}

fn eval_map(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [proc_expr, list_exprs @ ..] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    if list_exprs.is_empty() {
        return Err(EvalError::WrongArgCount { expected: 2, got: 1 });
    }
    let proc = eval(proc_expr, env)?;
    let lists: Vec<Vec<Value>> = list_exprs
        .iter()
        .map(|expr| {
            let val = eval(expr, env)?;
            let Value::List(items) = val else {
                return Err(EvalError::TypeError { expected: "list".into(), got: format!("{val}") });
            };
            Ok(items)
        })
        .collect::<Result<_, _>>()?;

    let len = lists[0].len();
    let mut results = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        let result = match &proc {
            Value::Lambda { .. } => {
                match apply_lambda_values(proc.clone(), &call_args, Span { line: 0, col: 0 })? {
                    Bounce::Done(val) => val,
                    Bounce::Tco(expr, tco_env) => eval(&expr, &tco_env)?,
                }
            }
            Value::Builtin(name) => apply_builtin_values(name, &call_args)?,
            _ => {
                return Err(EvalError::TypeError {
                    expected: "procedure".into(),
                    got: format!("{proc}"),
                })
            }
        };
        results.push(result);
    }
    Ok(Value::List(results))
}

// ===== L13: Char builtins =====

fn eval_char_pred(
    args: &[Expr],
    env: &Env,
    pred: fn(char) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Char(c) = val else {
        return Err(EvalError::TypeError { expected: "char".into(), got: format!("{val}") });
    };
    Ok(Value::Boolean(pred(c)))
}

fn eval_char_transform(
    args: &[Expr],
    env: &Env,
    transform: fn(char) -> char,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Char(c) = val else {
        return Err(EvalError::TypeError { expected: "char".into(), got: format!("{val}") });
    };
    Ok(Value::Char(transform(c)))
}

fn eval_char_cmp(
    args: &[Expr],
    env: &Env,
    cmp: fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = eval(a_expr, env)?;
    let b = eval(b_expr, env)?;
    let Value::Char(ca) = a else {
        return Err(EvalError::TypeError { expected: "char".into(), got: format!("{a}") });
    };
    let Value::Char(cb) = b else {
        return Err(EvalError::TypeError { expected: "char".into(), got: format!("{b}") });
    };
    Ok(Value::Boolean(cmp(ca, cb)))
}

// ===== L13: String builtins =====

fn eval_string_cmp(
    args: &[Expr],
    env: &Env,
    cmp: fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = eval(a_expr, env)?;
    let b = eval(b_expr, env)?;
    let Value::String(sa) = a else {
        return Err(EvalError::TypeError { expected: "string".into(), got: format!("{a}") });
    };
    let Value::String(sb) = b else {
        return Err(EvalError::TypeError { expected: "string".into(), got: format!("{b}") });
    };
    Ok(Value::Boolean(cmp(&sa, &sb)))
}

fn eval_string_case(
    args: &[Expr],
    env: &Env,
    transform: fn(&str) -> String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError { expected: "string".into(), got: format!("{val}") });
    };
    Ok(Value::String(transform(&s)))
}

fn eval_string_set(args: &[Expr], _env: &Env) -> Result<Value, EvalError> {
    let [_, _, _] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    Err(EvalError::TypeError {
        expected: "mutable string".into(),
        got: "strings are immutable".into(),
    })
}

fn eval_string_to_list(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::String(s) = eval(arg, env)? else {
        return Err(EvalError::TypeError { expected: "string".into(), got: "non-string".into() });
    };
    let items: Vec<Value> = s.chars().map(Value::Char).collect();
    Ok(Value::List(items))
}

fn eval_list_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::List(items) = val else {
        return Err(EvalError::TypeError { expected: "list".into(), got: format!("{val}") });
    };
    let s: Result<String, _> = items.iter().map(|v| match v {
        Value::Char(ch) => Ok(*ch),
        other => Err(EvalError::TypeError {
            expected: "char".into(),
            got: format!("{other}"),
        }),
    }).collect();
    Ok(Value::String(s?))
}

fn eval_char_to_integer(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Char(ch) = eval(arg, env)? else {
        return Err(EvalError::TypeError { expected: "char".into(), got: "non-char".into() });
    };
    Ok(Value::Integer(ch as i64))
}

fn eval_integer_to_char(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Integer(n) = eval(arg, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let ch = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
        expected: "valid Unicode code point".into(),
        got: format!("{n}"),
    })?;
    Ok(Value::Char(ch))
}

// ===== L15: letrec, letrec*, case, eqv?, vectors =====

fn eval_letrec(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let [Expr::List(ref bindings, _), ref body @ ..] = args else {
        return Err(EvalError::Parse("invalid letrec form".into()).at(span));
    };
    if body.is_empty() {
        return Err(EvalError::Parse("letrec requires a body".into()).at(span));
    }
    let letrec_env = Env::extend(env);
    // First pass: define all names as Void (placeholder)
    let mut names_and_exprs = Vec::new();
    for binding in bindings {
        let Expr::List(ref pair, _) = binding else {
            return Err(EvalError::Parse("letrec binding must be a list".into()).at(span));
        };
        let [Expr::Symbol(ref bname, _), ref val_expr] = pair.as_slice() else {
            return Err(EvalError::Parse("letrec binding must be (name expr)".into()).at(span));
        };
        letrec_env.define(bname.clone(), Value::Void);
        names_and_exprs.push((bname.clone(), val_expr.clone()));
    }
    // Second pass: evaluate init expressions in the letrec env and set!
    for (name, expr) in &names_and_exprs {
        let val = eval(expr, &letrec_env)?;
        letrec_env.set(name, val);
    }
    eval_body_tco(body, letrec_env)
}

fn eval_letrec_star(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let [Expr::List(ref bindings, _), ref body @ ..] = args else {
        return Err(EvalError::Parse("invalid letrec* form".into()).at(span));
    };
    if body.is_empty() {
        return Err(EvalError::Parse("letrec* requires a body".into()).at(span));
    }
    let letrec_env = Env::extend(env);
    // Define all names as Void first
    let mut names_and_exprs = Vec::new();
    for binding in bindings {
        let Expr::List(ref pair, _) = binding else {
            return Err(EvalError::Parse("letrec* binding must be a list".into()).at(span));
        };
        let [Expr::Symbol(ref bname, _), ref val_expr] = pair.as_slice() else {
            return Err(EvalError::Parse("letrec* binding must be (name expr)".into()).at(span));
        };
        letrec_env.define(bname.clone(), Value::Void);
        names_and_exprs.push((bname.clone(), val_expr.clone()));
    }
    // Evaluate sequentially so earlier bindings are visible to later ones
    for (name, expr) in &names_and_exprs {
        let val = eval(expr, &letrec_env)?;
        letrec_env.set(name, val);
    }
    eval_body_tco(body, letrec_env)
}

fn eval_case(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let [ref key_expr, ref clauses @ ..] = args else {
        return Err(EvalError::Parse("case requires a key expression".into()).at(span));
    };
    let key = eval(key_expr, env)?;
    for clause in clauses {
        let Expr::List(ref parts, _) = clause else {
            return Err(EvalError::Parse("case clause must be a list".into()).at(span));
        };
        let [ref datums_expr, ref body @ ..] = parts.as_slice() else {
            return Err(EvalError::Parse("case clause must have datums and body".into()).at(span));
        };
        // Check for else clause
        if matches!(datums_expr, Expr::Symbol(s, _) if s == "else") {
            return eval_body_tco(body, env.clone());
        }
        let Expr::List(ref datums, _) = datums_expr else {
            return Err(EvalError::Parse("case datums must be a list".into()).at(span));
        };
        let matched = datums.iter().any(|d| {
            expr_to_value(d).is_ok_and(|dv| values_eqv(&key, &dv))
        });
        if matched {
            return eval_body_tco(body, env.clone());
        }
    }
    // No match, no else → void
    Ok(Bounce::Done(Value::Void))
}

fn eval_eqv(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let a = eval(a_expr, env)?;
    let b = eval(b_expr, env)?;
    Ok(Value::Boolean(values_eqv(&a, &b)))
}

fn eval_vector_create(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let items: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(items))))
}

fn eval_make_vector(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let (size, fill) = match args {
        [size_expr] => (eval(size_expr, env)?, Value::Integer(0)),
        [size_expr, fill_expr] => (eval(size_expr, env)?, eval(fill_expr, env)?),
        _ => return Err(EvalError::WrongArgCount { expected: 2, got: args.len() }),
    };
    let n = expect_integer(&size)? as usize;
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(vec![fill; n]))))
}

fn eval_vector_ref(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [vec_expr, idx_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let vec_val = eval(vec_expr, env)?;
    let Value::Vector(ref v) = vec_val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{vec_val}") });
    };
    let idx = expect_integer(&eval(idx_expr, env)?)? as usize;
    let borrowed = v.borrow();
    borrowed.get(idx).cloned().ok_or_else(|| EvalError::TypeError {
        expected: "valid vector index".into(),
        got: format!("index {idx} out of range"),
    })
}

fn eval_vector_set(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [vec_expr, idx_expr, val_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    let vec_val = eval(vec_expr, env)?;
    let Value::Vector(ref v) = vec_val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{vec_val}") });
    };
    let idx = expect_integer(&eval(idx_expr, env)?)? as usize;
    let new_val = eval(val_expr, env)?;
    let mut borrowed = v.borrow_mut();
    if idx >= borrowed.len() {
        return Err(EvalError::TypeError {
            expected: "valid vector index".into(),
            got: format!("index {idx} out of range"),
        });
    }
    borrowed[idx] = new_val;
    Ok(Value::Void)
}

fn eval_vector_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Vector(ref v) = val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{val}") });
    };
    let len = v.borrow().len() as i64;
    Ok(Value::Integer(len))
}

fn eval_vector_to_list(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Vector(ref v) = val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{val}") });
    };
    let items = v.borrow().clone();
    Ok(Value::List(items))
}

fn eval_list_to_vector(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::List(items) = val else {
        return Err(EvalError::TypeError { expected: "list".into(), got: format!("{val}") });
    };
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(items))))
}

// Apply-style vector builtins (for higher-order use)
fn apply_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    let (size, fill) = match args {
        [size] => (size, &Value::Integer(0)),
        [size, fill] => (size, fill),
        _ => return Err(EvalError::WrongArgCount { expected: 2, got: args.len() }),
    };
    let n = expect_integer(size)? as usize;
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(vec![fill.clone(); n]))))
}

fn apply_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [ref vec_val, ref idx_val] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let Value::Vector(ref v) = vec_val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{vec_val}") });
    };
    let idx = expect_integer(idx_val)? as usize;
    let borrowed = v.borrow();
    borrowed.get(idx).cloned().ok_or_else(|| EvalError::TypeError {
        expected: "valid vector index".into(),
        got: format!("index {idx} out of range"),
    })
}

fn apply_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    let [ref vec_val, ref idx_val, ref new_val] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    let Value::Vector(ref v) = vec_val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{vec_val}") });
    };
    let idx = expect_integer(idx_val)? as usize;
    let mut borrowed = v.borrow_mut();
    if idx >= borrowed.len() {
        return Err(EvalError::TypeError {
            expected: "valid vector index".into(),
            got: format!("index {idx} out of range"),
        });
    }
    borrowed[idx] = new_val.clone();
    Ok(Value::Void)
}

fn apply_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    let [ref val] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Vector(ref v) = val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{val}") });
    };
    Ok(Value::Integer(v.borrow().len() as i64))
}

fn apply_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    let [ref val] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::Vector(ref v) = val else {
        return Err(EvalError::TypeError { expected: "vector".into(), got: format!("{val}") });
    };
    let items = v.borrow().clone();
    Ok(Value::List(items))
}

fn apply_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    let [ref val] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let Value::List(ref items) = val else {
        return Err(EvalError::TypeError { expected: "list".into(), got: format!("{val}") });
    };
    let cloned = items.clone();
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(cloned))))
}
