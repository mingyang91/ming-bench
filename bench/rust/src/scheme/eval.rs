use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, EvalErrorKind, Span};
use crate::scheme::macro_expand;
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::{make_rational, ContCtx, ResumeFrame, SyntaxObjectData, Value};

/// Trampoline action for tail-call optimization.
enum TcoAction {
    Result(Value),
    TailCall { expr: Expr, env: Env },
}

/// Evaluate an expression in the given environment (trampoline entry point).
pub fn eval(
    expr: &Expr,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    let mut action = eval_step(expr, env, output, ctx)?;
    loop {
        match action {
            TcoAction::Result(v) => return Ok(v),
            TcoAction::TailCall { expr, env } => {
                action = eval_step(&expr, &env, output, ctx)?;
            }
        }
    }
}

/// Evaluate a sequence of expressions, pushing continuation frames for each.
/// Returns the value of the last expression.
pub fn eval_sequence(
    exprs: &[Expr],
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for (i, expr) in exprs.iter().enumerate() {
        ctx.push_frame(ResumeFrame {
            exprs: exprs[i..].to_vec(),
            env: env.clone(),
        });
        result = eval(expr, env, output, ctx)?;
        ctx.pop_frame();
    }
    Ok(result)
}

/// Single evaluation step — returns either a final value or a tail call to bounce.
fn eval_step(
    expr: &Expr,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(TcoAction::Result(Value::Integer(*n))),
        ExprKind::Float(f) => Ok(TcoAction::Result(Value::Float(*f))),
        ExprKind::Rational(num, den) => Ok(TcoAction::Result(make_rational(*num, *den))),
        ExprKind::Boolean(b) => Ok(TcoAction::Result(Value::Boolean(*b))),
        ExprKind::SchemeString(s) => Ok(TcoAction::Result(Value::SchemeString(s.clone()))),
        ExprKind::Char(c) => Ok(TcoAction::Result(Value::Char(*c))),
        ExprKind::Symbol(name) => match env.lookup(name) {
            Ok(val) => Ok(TcoAction::Result(val)),
            Err(_) if is_builtin(name) => Ok(TcoAction::Result(Value::Builtin(name.clone()))),
            Err(e) => Err(e.with_span(&expr.span)),
        },
        ExprKind::List(elements) => eval_list_step(elements, &expr.span, env, output, ctx),
    }
}

fn eval_list_step(
    elements: &[Expr],
    list_span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if elements.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "empty application".into(),
        }
        .at(list_span));
    }

    // Check for special forms
    if let ExprKind::Symbol(name) = &elements[0].kind {
        let kw_span = &elements[0].span;
        match name.as_str() {
            "if" => return eval_if_step(&elements[1..], kw_span, env, output, ctx),
            "define" => {
                let v = eval_define(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "quote" => {
                let v = eval_quote(&elements[1..], kw_span)?;
                return Ok(TcoAction::Result(v));
            }
            "lambda" => {
                let v = eval_lambda(&elements[1..], kw_span, env)?;
                return Ok(TcoAction::Result(v));
            }
            "and" => return eval_and_step(&elements[1..], env, output, ctx),
            "or" => return eval_or_step(&elements[1..], env, output, ctx),
            "not" => {
                let v = eval_not(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "let" => return eval_let_step(&elements[1..], kw_span, env, output, ctx),
            "begin" => return eval_begin_step(&elements[1..], env, output, ctx),
            "cond" => return eval_cond_step(&elements[1..], kw_span, env, output, ctx),
            "set!" => {
                let v = eval_set(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "string-set!" => {
                return Err(EvalErrorKind::Type {
                    expected: "mutable string".into(),
                    got: "immutable string (R7RS strings are immutable)".into(),
                }
                .at(kw_span));
            }
            "call/cc" | "call-with-current-continuation" => {
                let v = eval_callcc_form(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "define-syntax" => {
                let v = eval_define_syntax(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "letrec" => return eval_letrec_step(&elements[1..], kw_span, env, output, ctx),
            "letrec*" => return eval_letrec_star_step(&elements[1..], kw_span, env, output, ctx),
            "case" => return eval_case_step(&elements[1..], kw_span, env, output, ctx),
            "dynamic-wind" => {
                let v = eval_dynamic_wind(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "raise" => {
                let v = eval_raise(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "guard" => {
                return eval_guard_step(&elements[1..], kw_span, env, output, ctx);
            }
            "with-exception-handler" => {
                let v = eval_with_exception_handler(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "define-record-type" => {
                let v = eval_define_record_type(&elements[1..], kw_span, env, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "syntax-case" => {
                let v = eval_syntax_case(&elements[1..], kw_span, env, output, ctx)?;
                return Ok(TcoAction::Result(v));
            }
            "syntax" => {
                let v = eval_syntax_template(&elements[1..], kw_span, env)?;
                return Ok(TcoAction::Result(v));
            }
            "with-syntax" => {
                return eval_with_syntax_step(&elements[1..], kw_span, env, output, ctx);
            }
            "case-lambda" => {
                let v = eval_case_lambda(&elements[1..], kw_span, env)?;
                return Ok(TcoAction::Result(v));
            }
            _ => {}
        }
    }

    // Check for macros before builtins
    if let ExprKind::Symbol(name) = &elements[0].kind {
        if let Ok(Value::Macro {
            syntax_rules,
            def_env,
        }) = env.lookup(name)
        {
            let hygiene_id = ctx.next_id();
            let (expanded, introduced) = macro_expand::expand_macro(
                &syntax_rules,
                elements,
                hygiene_id,
                list_span,
            )?;
            for (gensym, original) in &introduced {
                if let Ok(val) = def_env.lookup(original) {
                    env.define(gensym.clone(), val);
                }
            }
            return Ok(TcoAction::TailCall {
                expr: expanded,
                env: env.clone(),
            });
        }
        // Check for syntax-case macros
        if let Ok(Value::SyntaxCaseMacro {
            params,
            body,
            def_env,
        }) = env.lookup(name)
        {
            return eval_syntax_case_macro_invocation(
                &SyntaxCaseInvocation {
                    params: &params,
                    body: &body,
                    def_env: &def_env,
                    use_elements: elements,
                    span: list_span,
                    use_env: env,
                },
                output,
                ctx,
            );
        }
    }

    // Check for builtin functions by name before evaluating
    if let ExprKind::Symbol(name) = &elements[0].kind {
        if name == "call-with-values" {
            let args: Vec<Value> = elements[1..]
                .iter()
                .map(|e| eval(e, env, output, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            return apply_step(
                &Value::Builtin("call-with-values".into()),
                &args,
                &elements[0].span,
                output,
                ctx,
            );
        }
        if name == "map" {
            let args: Vec<Value> = elements[1..]
                .iter()
                .map(|e| eval(e, env, output, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let v = eval_map(&args, &elements[0].span, output, ctx)?;
            return Ok(TcoAction::Result(v));
        }
        if is_builtin(name) {
            let args: Vec<Value> = elements[1..]
                .iter()
                .map(|e| eval(e, env, output, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let v = eval_builtin(name, &args, &elements[0].span, output)?;
            return Ok(TcoAction::Result(v));
        }
    }

    // Evaluate operator and arguments
    let op = eval(&elements[0], env, output, ctx)?;
    let args: Vec<Value> = elements[1..]
        .iter()
        .map(|e| eval(e, env, output, ctx))
        .collect::<Result<Vec<_>, _>>()?;

    apply_step(&op, &args, list_span, output, ctx)
}

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
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "pair?"
            | "string?"
            | "number?"
            | "boolean?"
            | "symbol?"
            | "char?"
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
            | "map"
            | "eq?"
            | "equal?"
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
            | "eqv?"
            | "vector"
            | "make-vector"
            | "vector-ref"
            | "vector-set!"
            | "vector-length"
            | "vector?"
            | "vector->list"
            | "list->vector"
            | "reverse"
            | "values"
            | "call-with-values"
            | "exact?"
            | "inexact?"
            | "exact->inexact"
            | "inexact->exact"
            | "numerator"
            | "denominator"
            | "rational?"
            | "integer?"
            | "set-car!"
            | "set-cdr!"
            | "cddr"
            | "cadr"
            | "syntax->datum"
            | "datum->syntax"
            | "procedure?"
            | "procedure-name"
    )
}

fn apply_step(
    op: &Value,
    args: &[Value],
    span: &Span,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    match op {
        Value::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
            ..
        } => {
            match rest_param {
                None => {
                    if args.len() != params.len() {
                        return Err(EvalErrorKind::Arity {
                            name: "#<procedure>".into(),
                            expected: params.len().to_string(),
                            got: args.len(),
                        }
                        .at(span));
                    }
                }
                Some(_) => {
                    if args.len() < params.len() {
                        return Err(EvalErrorKind::Arity {
                            name: "#<procedure>".into(),
                            expected: format!("at least {}", params.len()),
                            got: args.len(),
                        }
                        .at(span));
                    }
                }
            }
            let call_env = Env::with_parent(closure_env);
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.define(param.clone(), arg.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = &args[params.len()..];
                let mut list = Value::Nil;
                for arg in rest_args.iter().rev() {
                    list = Value::pair(arg.clone(), list);
                }
                call_env.define(rest.clone(), list);
            }
            // Evaluate all body expressions except the last
            if body.is_empty() {
                return Ok(TcoAction::Result(Value::Nil));
            }
            for expr in &body[..body.len() - 1] {
                eval(expr, &call_env, output, ctx)?;
            }
            // Tail call: return the last body expression for trampoline
            Ok(TcoAction::TailCall {
                expr: body[body.len() - 1].clone(),
                env: call_env,
            })
        }
        Value::CaseLambda { clauses, env: closure_env, .. } => {
            // Find the matching clause by arity
            for (params, rest_param, body) in clauses {
                match rest_param {
                    None => {
                        if args.len() != params.len() {
                            continue;
                        }
                    }
                    Some(_) => {
                        if args.len() < params.len() {
                            continue;
                        }
                    }
                }
                // Match found — apply like Lambda
                let call_env = Env::with_parent(closure_env);
                for (param, arg) in params.iter().zip(args.iter()) {
                    call_env.define(param.clone(), arg.clone());
                }
                if let Some(rest) = rest_param {
                    let rest_args = &args[params.len()..];
                    let mut list = Value::Nil;
                    for arg in rest_args.iter().rev() {
                        list = Value::pair(arg.clone(), list);
                    }
                    call_env.define(rest.clone(), list);
                }
                if body.is_empty() {
                    return Ok(TcoAction::Result(Value::Nil));
                }
                for expr in &body[..body.len() - 1] {
                    eval(expr, &call_env, output, ctx)?;
                }
                return Ok(TcoAction::TailCall {
                    expr: body[body.len() - 1].clone(),
                    env: call_env,
                });
            }
            // No clause matched
            Err(EvalErrorKind::Arity {
                name: "#<case-lambda>".into(),
                expected: clauses.iter().map(|(p, r, _)| {
                    if r.is_some() {
                        format!("{}+", p.len())
                    } else {
                        p.len().to_string()
                    }
                }).collect::<Vec<_>>().join(" or "),
                got: args.len(),
            }.at(span))
        }
        Value::Builtin(name) => match name.as_str() {
            "apply" => eval_apply(args, span, output, ctx),
            "call/cc" => {
                if args.len() != 1 {
                    return Err(EvalErrorKind::Arity {
                        name: "call/cc".into(),
                        expected: "1".into(),
                        got: args.len(),
                    }
                    .at(span));
                }
                let result = do_callcc(&args[0], span, output, ctx)?;
                Ok(TcoAction::Result(result))
            }
            "call-with-values" => {
                if args.len() != 2 {
                    return Err(EvalErrorKind::Arity {
                        name: "call-with-values".into(),
                        expected: "2".into(),
                        got: args.len(),
                    }
                    .at(span));
                }
                let producer = &args[0];
                let consumer = &args[1];
                // Call the producer with no arguments
                let produced = apply_step(producer, &[], span, output, ctx)?;
                let produced_val = match produced {
                    TcoAction::Result(v) => v,
                    TcoAction::TailCall { expr, env } => eval(&expr, &env, output, ctx)?,
                };
                // Unpack Values into argument list
                let consumer_args = match produced_val {
                    Value::Values(vals) => vals,
                    single => vec![single],
                };
                apply_step(consumer, &consumer_args, span, output, ctx)
            }
            "map" => {
                let v = eval_map(args, span, output, ctx)?;
                Ok(TcoAction::Result(v))
            }
            _ => {
                let v = eval_builtin(name, args, span, output)?;
                Ok(TcoAction::Result(v))
            }
        },
        Value::Continuation {
            id,
            frames,
            capture_span,
        } => {
            if args.is_empty() {
                return Err(EvalErrorKind::Arity {
                    name: "#<continuation>".into(),
                    expected: "1+".into(),
                    got: 0,
                }
                .at(span));
            }
            // Store the value, frames, and capture span for the resume handler to pick up
            let value = if args.len() == 1 {
                args[0].clone()
            } else {
                Value::Values(args.to_vec())
            };
            ctx.pending = Some(value);
            ctx.resume_span = Some(capture_span.clone());
            ctx.resume_frames = Some(frames.clone());
            Err(EvalErrorKind::ContinuationReturn { id: *id }.at(span))
        }
        Value::Macro { .. } | Value::SyntaxCaseMacro { .. } => {
            Err(EvalErrorKind::NotAProcedure {
                value: "#<macro>".into(),
            }
            .at(span))
        }
        Value::RecordConstructor {
            type_id,
            type_name,
            field_names,
        } => {
            if args.len() != field_names.len() {
                return Err(EvalErrorKind::Arity {
                    name: format!("make-{type_name}"),
                    expected: field_names.len().to_string(),
                    got: args.len(),
                }
                .at(span));
            }
            Ok(TcoAction::Result(Value::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields: args.to_vec(),
            }))
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity {
                    name: "#<record-predicate>".into(),
                    expected: "1".into(),
                    got: args.len(),
                }
                .at(span));
            }
            let result = matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id);
            Ok(TcoAction::Result(Value::Boolean(result)))
        }
        Value::RecordAccessor {
            type_id,
            field_index,
        } => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity {
                    name: "#<record-accessor>".into(),
                    expected: "1".into(),
                    got: args.len(),
                }
                .at(span));
            }
            match &args[0] {
                Value::Record {
                    type_id: tid,
                    fields,
                    ..
                } if tid == type_id => Ok(TcoAction::Result(
                    fields
                        .get(*field_index)
                        .expect("record field index out of bounds")
                        .clone(),
                )),
                other => Err(EvalErrorKind::Type {
                    expected: "matching record type".into(),
                    got: other.to_string(),
                }
                .at(span)),
            }
        }
        Value::Record { .. } => Err(EvalErrorKind::NotAProcedure {
            value: op.to_string(),
        }
        .at(span)),
        _ => Err(EvalErrorKind::NotAProcedure {
            value: op.to_string(),
        }
        .at(span)),
    }
}

// ===== call/cc implementation =====

/// Handle (call/cc <func>) as a special form.
fn eval_callcc_form(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "call/cc".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let func = eval(&args[0], env, output, ctx)?;
    do_callcc(&func, span, output, ctx)
}

/// Core call/cc logic shared between the special form and the first-class value.
fn do_callcc(
    func: &Value,
    span: &Span,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    // If we're resuming a saved continuation and this is the matching call/cc site,
    // return the pending value directly instead of re-executing.
    if ctx.pending.is_some() {
        if let Some(ref resume_span) = ctx.resume_span {
            if resume_span == span {
                ctx.resume_span.take();
                return Ok(ctx
                    .pending
                    .take()
                    .expect("pending should be set when resume_span matches"));
            }
        }
    }

    // Normal call/cc: capture current continuation and call the function
    let id = ctx.next_id();
    let frames = ctx.frames.clone();
    let cont = Value::Continuation {
        id,
        frames,
        capture_span: span.clone(),
    };

    match apply_func(func, &[cont], span, output, ctx) {
        Ok(v) => Ok(v),
        Err(e) => {
            if let EvalErrorKind::ContinuationReturn { id: ret_id } = &e.kind {
                if *ret_id == id {
                    // Escape continuation: invoked within call/cc's function.
                    // The value was stored in ctx.pending by apply_step.
                    ctx.resume_span.take();
                    ctx.resume_frames.take();
                    return Ok(ctx
                        .pending
                        .take()
                        .expect("continuation value should be set"));
                }
            }
            Err(e)
        }
    }
}

/// Apply a function to arguments, resolving any tail calls via the trampoline.
fn apply_func(
    func: &Value,
    args: &[Value],
    span: &Span,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    let action = apply_step(func, args, span, output, ctx)?;
    match action {
        TcoAction::Result(v) => Ok(v),
        TcoAction::TailCall { expr, env } => eval(&expr, &env, output, ctx),
    }
}

// ===== dynamic-wind implementation =====

fn eval_dynamic_wind(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalErrorKind::Arity {
            name: "dynamic-wind".into(),
            expected: "3".into(),
            got: args.len(),
        }
        .at(span));
    }
    let in_thunk = eval(&args[0], env, output, ctx)?;
    let body_thunk = eval(&args[1], env, output, ctx)?;
    let out_thunk = eval(&args[2], env, output, ctx)?;

    // Run in-thunk
    apply_func(&in_thunk, &[], span, output, ctx)?;

    // Run body-thunk, catching continuation escapes to run out-thunk
    match apply_func(&body_thunk, &[], span, output, ctx) {
        Ok(body_val) => {
            // Normal exit: run out-thunk, return body value
            apply_func(&out_thunk, &[], span, output, ctx)?;
            Ok(body_val)
        }
        Err(e) => {
            if matches!(&e.kind, EvalErrorKind::ContinuationReturn { .. } | EvalErrorKind::SchemeRaise { .. }) {
                // Non-local exit: run out-thunk before re-throwing
                apply_func(&out_thunk, &[], span, output, ctx)?;
            }
            Err(e)
        }
    }
}

// ===== raise, guard, with-exception-handler =====

fn eval_raise(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "raise".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let value = eval(&args[0], env, output, ctx)?;
    Err(EvalErrorKind::SchemeRaise { value: Box::new(value) }.at(span))
}

fn eval_guard_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    // (guard (var clause ...) body ...)
    // clause = (test expr ...) | (else expr ...)
    if args.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "guard requires at least a clause list".into(),
        }
        .at(span));
    }
    let clauses_expr = &args[0];
    let body = &args[1..];

    let clause_elements = match &clauses_expr.kind {
        ExprKind::List(elems) => elems,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "guard clauses must be a list".into(),
            }
            .at(span));
        }
    };

    if clause_elements.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "guard requires a variable name".into(),
        }
        .at(span));
    }

    let var_name = match &clause_elements[0].kind {
        ExprKind::Symbol(name) => name.clone(),
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "guard variable must be a symbol".into(),
            }
            .at(span));
        }
    };

    let clauses = &clause_elements[1..];

    // Evaluate body with TCO support for the last expression.
    // All but the last body expression are evaluated normally;
    // the last one is evaluated with eval_step so tail calls can escape.
    let body_result = if body.is_empty() {
        Ok(TcoAction::Result(Value::Nil))
    } else {
        let init = &body[..body.len() - 1];
        let last = &body[body.len() - 1];
        let mut r = Ok(());
        for expr in init {
            match eval(expr, env, output, ctx) {
                Ok(_) => {}
                Err(e) => {
                    r = Err(e);
                    break;
                }
            }
        }
        match r {
            Err(e) => Err(e),
            Ok(()) => eval_step(last, env, output, ctx),
        }
    };

    match body_result {
        Ok(action) => Ok(action),
        Err(e) => {
            if let EvalErrorKind::SchemeRaise { value } = e.kind {
                // Bind the exception value to the variable
                let guard_env = Env::with_parent(env);
                guard_env.define(var_name, (*value).clone());

                // Evaluate clauses like cond
                for clause in clauses {
                    let clause_elems = match &clause.kind {
                        ExprKind::List(elems) => elems,
                        _ => {
                            return Err(EvalErrorKind::Parse {
                                message: "guard clause must be a list".into(),
                            }
                            .at(span));
                        }
                    };

                    if clause_elems.is_empty() {
                        continue;
                    }

                    // Check for else clause
                    let is_else = matches!(&clause_elems[0].kind, ExprKind::Symbol(s) if s == "else");
                    if is_else {
                        return eval_body_sequence(&clause_elems[1..], &guard_env, output, ctx)
                            .map(TcoAction::Result);
                    }

                    // Evaluate test
                    let test_val = eval(&clause_elems[0], &guard_env, output, ctx)?;
                    if test_val.is_truthy() {
                        // Evaluate handler expressions
                        if clause_elems.len() == 1 {
                            return Ok(TcoAction::Result(test_val));
                        }
                        let mut result = Value::Nil;
                        for expr in &clause_elems[1..] {
                            result = eval(expr, &guard_env, output, ctx)?;
                        }
                        return Ok(TcoAction::Result(result));
                    }
                }

                // No clause matched, re-raise
                Err(EvalErrorKind::SchemeRaise { value }.at(span))

            } else {
                Err(e)
            }
        }
    }
}

fn eval_body_sequence(
    exprs: &[Expr],
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for expr in exprs {
        result = eval(expr, env, output, ctx)?;
    }
    Ok(result)
}

fn eval_with_exception_handler(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "with-exception-handler".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let handler = eval(&args[0], env, output, ctx)?;
    let thunk = eval(&args[1], env, output, ctx)?;

    // Run the thunk, catch SchemeRaise
    match apply_func(&thunk, &[], span, output, ctx) {
        Ok(val) => Ok(val),
        Err(e) => {
            if let EvalErrorKind::SchemeRaise { value } = e.kind {
                apply_func(&handler, &[*value], span, output, ctx)
            } else {
                Err(e)
            }
        }
    }
}

fn eval_builtin(
    name: &str,
    args: &[Value],
    span: &Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args, span),
        "-" => eval_sub(args, name, span),
        "*" => eval_mul(args, span),
        "/" => eval_div(args, name, span),
        "<" => eval_cmp(args, name, span, |a, b| a < b),
        ">" => eval_cmp(args, name, span, |a, b| a > b),
        "=" => eval_cmp(args, name, span, |a, b| a == b),
        "<=" => eval_cmp(args, name, span, |a, b| a <= b),
        ">=" => eval_cmp(args, name, span, |a, b| a >= b),
        "cons" => eval_cons(args, span),
        "car" => eval_car(args, span),
        "cdr" => eval_cdr(args, span),
        "null?" => eval_null_pred(args, span),
        "list" => eval_list_builtin(args),
        "length" => eval_length(args, span),
        "pair?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Pair(_))) && args.len() == 1,
        )),
        "string?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::SchemeString(_))) && args.len() == 1,
        )),
        "number?" => Ok(Value::Boolean(
            args.len() == 1 && args.first().is_some_and(is_number),
        )),
        "boolean?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Boolean(_))) && args.len() == 1,
        )),
        "symbol?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Symbol(_))) && args.len() == 1,
        )),
        "char?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Char(_))) && args.len() == 1,
        )),
        "display" => eval_display(args, span, output),
        "write" => eval_write(args, span, output),
        "newline" => eval_newline(args, span, output),
        "string-append" => eval_string_append(args, span),
        "string-length" => eval_string_length(args, span),
        "substring" => eval_substring(args, span),
        "string->number" => eval_string_to_number(args, span),
        "number->string" => eval_number_to_string(args, span),
        "symbol->string" => eval_symbol_to_string(args, span),
        "string->symbol" => eval_string_to_symbol(args, span),
        "string-ref" => eval_string_ref(args, span),
        "string-copy" => eval_string_copy(args, span),
        "string->list" => eval_string_to_list(args, span),
        "list->string" => eval_list_to_string(args, span),
        "char->integer" => eval_char_to_integer(args, span),
        "integer->char" => eval_integer_to_char(args, span),
        "abs" => eval_abs(args, span),
        "modulo" => eval_modulo(args, span),
        "remainder" => eval_remainder(args, span),
        "quotient" => eval_quotient(args, span),
        "min" => eval_min_max(args, "min", span, |a, b| a < b),
        "max" => eval_min_max(args, "max", span, |a, b| a > b),
        "expt" => eval_expt(args, span),
        "zero?" => eval_num_pred(args, "zero?", span, |n| n == 0),
        "positive?" => eval_num_pred(args, "positive?", span, |n| n > 0),
        "negative?" => eval_num_pred(args, "negative?", span, |n| n < 0),
        "odd?" => eval_num_pred(args, "odd?", span, |n| n % 2 != 0),
        "even?" => eval_num_pred(args, "even?", span, |n| n % 2 == 0),
        "list-ref" => eval_list_ref(args, span),
        "list-tail" => eval_list_tail(args, span),
        "list?" => eval_list_pred(args, span),
        "assoc" => eval_assoc(args, span),
        "eq?" => eval_eq(args, span),
        "equal?" => eval_equal(args, span),
        "char-alphabetic?" => eval_char_pred(args, "char-alphabetic?", span, |c| c.is_alphabetic()),
        "char-numeric?" => eval_char_pred(args, "char-numeric?", span, |c| c.is_ascii_digit()),
        "char-upcase" => eval_char_case(args, "char-upcase", span, |c| c.to_uppercase().next().unwrap_or(c)),
        "char-downcase" => eval_char_case(args, "char-downcase", span, |c| c.to_lowercase().next().unwrap_or(c)),
        "char=?" => eval_char_cmp(args, "char=?", span, |a, b| a == b),
        "char<?" => eval_char_cmp(args, "char<?", span, |a, b| a < b),
        "string=?" => eval_string_cmp(args, "string=?", span, |a, b| a == b),
        "string<?" => eval_string_cmp(args, "string<?", span, |a, b| a < b),
        "string-ci=?" => eval_string_ci_eq(args, span),
        "string-upcase" => eval_string_case(args, "string-upcase", span, |s| s.to_uppercase()),
        "string-downcase" => eval_string_case(args, "string-downcase", span, |s| s.to_lowercase()),
        "eqv?" => eval_eqv(args, span),
        "vector" => eval_vector_create(args),
        "make-vector" => eval_make_vector(args, span),
        "vector-ref" => eval_vector_ref(args, span),
        "vector-set!" => eval_vector_set(args, span),
        "vector-length" => eval_vector_length(args, span),
        "vector?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Vector(_))) && args.len() == 1,
        )),
        "vector->list" => eval_vector_to_list(args, span),
        "list->vector" => eval_list_to_vector(args, span),
        "reverse" => eval_reverse(args, span),
        "values" => {
            if args.len() == 1 {
                Ok(args[0].clone())
            } else {
                Ok(Value::Values(args.to_vec()))
            }
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "exact?".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "inexact?".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "exact->inexact".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            let f = to_f64(&args[0], span)?;
            Ok(Value::Float(f))
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "inexact->exact".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            match &args[0] {
                Value::Integer(_) => Ok(args[0].clone()),
                Value::Rational(_, _) => Ok(args[0].clone()),
                Value::Float(f) => {
                    let precision = 1_000_000_000_000_000_i64;
                    let num = (*f * precision as f64).round() as i64;
                    Ok(make_rational(num, precision))
                }
                other => Err(EvalErrorKind::Type { expected: "number".into(), got: format!("{other}") }.at(span)),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "numerator".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                other => Err(EvalErrorKind::Type { expected: "rational".into(), got: format!("{other}") }.at(span)),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "denominator".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                other => Err(EvalErrorKind::Type { expected: "rational".into(), got: format!("{other}") }.at(span)),
            }
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "rational?".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "integer?".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "procedure?".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            Ok(Value::Boolean(matches!(&args[0],
                Value::Lambda { .. }
                | Value::CaseLambda { .. }
                | Value::Builtin(_)
                | Value::Continuation { .. }
                | Value::RecordConstructor { .. }
                | Value::RecordPredicate { .. }
                | Value::RecordAccessor { .. }
            )))
        }
        "procedure-name" => {
            if args.len() != 1 {
                return Err(EvalErrorKind::Arity { name: "procedure-name".into(), expected: "1".into(), got: args.len() }.at(span));
            }
            match &args[0] {
                Value::Lambda { name, .. } | Value::CaseLambda { name, .. } => {
                    match name {
                        Some(n) => Ok(Value::Symbol(n.clone())),
                        None => Ok(Value::Boolean(false)),
                    }
                }
                Value::Builtin(n) => Ok(Value::Symbol(n.clone())),
                Value::Continuation { .. } => Ok(Value::Boolean(false)),
                Value::RecordConstructor { type_name, .. } => Ok(Value::Symbol(format!("make-{type_name}"))),
                Value::RecordPredicate { .. } => Ok(Value::Boolean(false)),
                Value::RecordAccessor { .. } => Ok(Value::Boolean(false)),
                _ => Err(EvalErrorKind::Type { expected: "procedure".into(), got: format!("{}", args[0]) }.at(span)),
            }
        }
        "set-car!" => eval_set_car(args, span),
        "set-cdr!" => eval_set_cdr(args, span),
        "cddr" => eval_cddr(args, span),
        "cadr" => eval_cadr(args, span),
        "syntax->datum" => eval_syntax_to_datum(args, span),
        "datum->syntax" => eval_datum_to_syntax(args, span),
        _ => Err(EvalErrorKind::UnboundVariable {
            name: name.to_string(),
        }
        .at(span)),
    }
}

fn eval_if_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalErrorKind::Parse {
            message: "if requires 2 or 3 arguments".into(),
        }
        .at(span));
    }
    let cond = eval(&args[0], env, output, ctx)?;
    if cond.is_truthy() {
        Ok(TcoAction::TailCall {
            expr: args[1].clone(),
            env: env.clone(),
        })
    } else if args.len() == 3 {
        Ok(TcoAction::TailCall {
            expr: args[2].clone(),
            env: env.clone(),
        })
    } else {
        Ok(TcoAction::Result(Value::Nil))
    }
}

fn eval_set(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Parse {
            message: "set! requires exactly 2 arguments".into(),
        }
        .at(span));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(n) => n,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "set!: first argument must be a symbol".into(),
            }
            .at(span))
        }
    };
    let val = eval(&args[1], env, output, ctx)?;
    env.set(name, val).map_err(|e| e.with_span(span))?;
    Ok(Value::Nil)
}

fn eval_define(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "define requires at least 2 arguments".into(),
        }
        .at(span));
    }

    match &args[0].kind {
        // (define x expr)
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalErrorKind::Parse {
                    message: "define requires exactly 2 arguments".into(),
                }
                .at(span));
            }
            let mut val = eval(&args[1], env, output, ctx)?;
            // Propagate the name to anonymous lambdas/case-lambdas
            match &mut val {
                Value::Lambda { name: ref mut n, .. } if n.is_none() => {
                    *n = Some(name.clone());
                }
                Value::CaseLambda { name: ref mut n, .. } if n.is_none() => {
                    *n = Some(name.clone());
                }
                _ => {}
            }
            env.define(name.clone(), val);
            Ok(Value::Nil)
        }
        // (define (f params...) body...)
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalErrorKind::Parse {
                    message: "define: empty signature".into(),
                }
                .at(span));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => {
                    return Err(EvalErrorKind::Parse {
                        message: "define: expected symbol as function name".into(),
                    }
                    .at(span))
                }
            };
            let (params, rest_param) = parse_params(&sig[1..], span)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
                name: Some(name.clone()),
            };
            env.define(name, lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalErrorKind::Parse {
            message: "define: expected symbol or list".into(),
        }
        .at(span)),
    }
}

/// Evaluate (define-record-type <name> (constructor field...) predicate (field accessor)...)
fn eval_define_record_type(
    args: &[Expr],
    span: &Span,
    env: &Env,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    // args: [<name>, (constructor field...), predicate, (field accessor)...]
    if args.len() < 3 {
        return Err(EvalErrorKind::Parse {
            message: "define-record-type requires type name, constructor, predicate, and field specs"
                .into(),
        }
        .at(span));
    }

    // Parse type name (e.g., <point>)
    let type_name = match &args[0].kind {
        ExprKind::Symbol(name) => name.clone(),
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "define-record-type: expected symbol as type name".into(),
            }
            .at(span))
        }
    };

    // Parse constructor: (make-point x y)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Err(EvalErrorKind::Parse {
                    message: "define-record-type: empty constructor spec".into(),
                }
                .at(span));
            }
            let ctor_name = match &elems[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => {
                    return Err(EvalErrorKind::Parse {
                        message: "define-record-type: expected symbol as constructor name".into(),
                    }
                    .at(span))
                }
            };
            let fields: Vec<String> = elems[1..]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(n) => Ok(n.clone()),
                    _ => Err(EvalErrorKind::Parse {
                        message: "define-record-type: expected symbol as field name".into(),
                    }
                    .at(span)),
                })
                .collect::<Result<_, _>>()?;
            (ctor_name, fields)
        }
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "define-record-type: expected list for constructor spec".into(),
            }
            .at(span))
        }
    };

    // Parse predicate name
    let predicate_name = match &args[2].kind {
        ExprKind::Symbol(name) => name.clone(),
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "define-record-type: expected symbol as predicate name".into(),
            }
            .at(span))
        }
    };

    // Parse field specs: (field-name accessor-name)
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for field_spec in &args[3..] {
        match &field_spec.kind {
            ExprKind::List(elems) => {
                if elems.len() != 2 {
                    return Err(EvalErrorKind::Parse {
                        message: "define-record-type: field spec must be (field accessor)".into(),
                    }
                    .at(span));
                }
                let field_name = match &elems[0].kind {
                    ExprKind::Symbol(n) => n.clone(),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "define-record-type: expected symbol in field spec".into(),
                        }
                        .at(span))
                    }
                };
                let accessor_name = match &elems[1].kind {
                    ExprKind::Symbol(n) => n.clone(),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "define-record-type: expected symbol in field spec".into(),
                        }
                        .at(span))
                    }
                };
                field_accessors.push((field_name, accessor_name));
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "define-record-type: expected list for field spec".into(),
                }
                .at(span))
            }
        }
    }

    let type_id = ctx.next_id();

    // Define constructor
    env.define(
        constructor_name,
        Value::RecordConstructor {
            type_id,
            type_name: type_name.clone(),
            field_names: constructor_fields.clone(),
        },
    );

    // Define predicate
    env.define(predicate_name, Value::RecordPredicate { type_id });

    // Define accessors — map field name to its index in the constructor field list
    for (field_name, accessor_name) in &field_accessors {
        let field_index = constructor_fields
            .iter()
            .position(|f| f == field_name)
            .ok_or_else(|| {
                EvalErrorKind::Parse {
                    message: format!(
                        "define-record-type: field '{field_name}' not in constructor"
                    ),
                }
                .at(span)
            })?;
        env.define(
            accessor_name.clone(),
            Value::RecordAccessor {
                type_id,
                field_index,
            },
        );
    }

    Ok(Value::Nil)
}

fn eval_quote(args: &[Expr], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Parse {
            message: "quote requires exactly 1 argument".into(),
        }
        .at(span));
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Rational(num, den) => Ok(make_rational(*num, *den)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::List(elements) => {
            let mut result = Value::Nil;
            for elem in elements.iter().rev() {
                let val = expr_to_value(elem)?;
                result = Value::pair(val, result);
            }
            Ok(result)
        }
    }
}

fn parse_params(
    param_exprs: &[Expr],
    span: &Span,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    // Look for dot notation: (a b . rest)
    let dot_pos = param_exprs
        .iter()
        .position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "."));
    match dot_pos {
        Some(pos) => {
            if pos + 1 != param_exprs.len() - 1 {
                return Err(EvalErrorKind::Parse {
                    message:
                        "improper parameter list: expected exactly one symbol after dot".into(),
                }
                .at(span));
            }
            let params: Vec<String> = param_exprs[..pos]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalErrorKind::Parse {
                        message: "expected symbol as parameter".into(),
                    }
                    .at(span)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let rest = match &param_exprs[pos + 1].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalErrorKind::Parse {
                        message: "expected symbol after dot in parameter list".into(),
                    }
                    .at(span))
                }
            };
            Ok((params, Some(rest)))
        }
        None => {
            let params: Vec<String> = param_exprs
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalErrorKind::Parse {
                        message: "expected symbol as parameter".into(),
                    }
                    .at(span)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok((params, None))
        }
    }
}

fn eval_lambda(args: &[Expr], span: &Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "lambda requires parameters and body".into(),
        }
        .at(span));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
        ExprKind::Symbol(s) => {
            // (lambda args body) — all args collected as rest
            (Vec::new(), Some(s.clone()))
        }
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "lambda: expected parameter list".into(),
            }
            .at(span))
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
        name: None,
    })
}

fn eval_case_lambda(args: &[Expr], span: &Span, env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "case-lambda requires at least one clause".into(),
        }
        .at(span));
    }
    let mut clauses = Vec::new();
    for clause in args {
        match &clause.kind {
            ExprKind::List(elems) => {
                if elems.len() < 2 {
                    return Err(EvalErrorKind::Parse {
                        message: "case-lambda clause requires formals and body".into(),
                    }
                    .at(span));
                }
                let (params, rest_param) = match &elems[0].kind {
                    ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
                    ExprKind::Symbol(s) => (Vec::new(), Some(s.clone())),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "case-lambda: expected parameter list".into(),
                        }
                        .at(span))
                    }
                };
                let body = elems[1..].to_vec();
                clauses.push((params, rest_param, body));
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "case-lambda: expected clause list".into(),
                }
                .at(span))
            }
        }
    }
    Ok(Value::CaseLambda {
        clauses,
        env: env.clone(),
        name: None,
    })
}

fn eval_and_step(
    args: &[Expr],
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.is_empty() {
        return Ok(TcoAction::Result(Value::Boolean(true)));
    }
    // Evaluate all but the last; short-circuit on false
    for arg in &args[..args.len() - 1] {
        let result = eval(arg, env, output, ctx)?;
        if !result.is_truthy() {
            return Ok(TcoAction::Result(result));
        }
    }
    // Last expression is in tail position
    Ok(TcoAction::TailCall {
        expr: args[args.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_or_step(
    args: &[Expr],
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.is_empty() {
        return Ok(TcoAction::Result(Value::Boolean(false)));
    }
    // Evaluate all but the last; short-circuit on true
    for arg in &args[..args.len() - 1] {
        let result = eval(arg, env, output, ctx)?;
        if result.is_truthy() {
            return Ok(TcoAction::Result(result));
        }
    }
    // Last expression is in tail position
    Ok(TcoAction::TailCall {
        expr: args[args.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_not(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "not".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let val = eval(&args[0], env, output, ctx)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn require_integer(v: &Value, span: &Span) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalErrorKind::Type {
            expected: "integer".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

/// Extract (numerator, denominator) from an exact number.
fn to_exact_pair(v: &Value, span: &Span) -> Result<(i64, i64), EvalError> {
    match v {
        Value::Integer(n) => Ok((*n, 1)),
        Value::Rational(n, d) => Ok((*n, *d)),
        other => Err(EvalErrorKind::Type {
            expected: "number".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

/// Convert any numeric value to f64.
fn to_f64(v: &Value, span: &Span) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        other => Err(EvalErrorKind::Type {
            expected: "number".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

/// Check if any argument is inexact (Float).
fn any_inexact(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Float(_)))
}

/// Check if a value is a number.
fn is_number(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

fn require_string<'a>(v: &'a Value, span: &Span) -> Result<&'a str, EvalError> {
    match v {
        Value::SchemeString(s) => Ok(s.as_str()),
        other => Err(EvalErrorKind::Type {
            expected: "string".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_add(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if any_inexact(args) {
        let mut sum = 0.0_f64;
        for arg in args {
            sum += to_f64(arg, span)?;
        }
        return Ok(Value::Float(sum));
    }
    let mut num: i64 = 0;
    let mut den: i64 = 1;
    for arg in args {
        let (an, ad) = to_exact_pair(arg, span)?;
        // num/den + an/ad = (num*ad + an*den) / (den*ad)
        num = num * ad + an * den;
        den *= ad;
    }
    Ok(make_rational(num, den))
}

fn eval_sub(args: &[Value], name: &str, span: &Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .at(span));
    }
    if any_inexact(args) {
        let first = to_f64(&args[0], span)?;
        if args.len() == 1 {
            return Ok(Value::Float(-first));
        }
        let mut result = first;
        for arg in &args[1..] {
            result -= to_f64(arg, span)?;
        }
        return Ok(Value::Float(result));
    }
    let (mut num, mut den) = to_exact_pair(&args[0], span)?;
    if args.len() == 1 {
        return Ok(make_rational(-num, den));
    }
    for arg in &args[1..] {
        let (an, ad) = to_exact_pair(arg, span)?;
        num = num * ad - an * den;
        den *= ad;
    }
    Ok(make_rational(num, den))
}

fn eval_mul(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if any_inexact(args) {
        let mut product = 1.0_f64;
        for arg in args {
            product *= to_f64(arg, span)?;
        }
        return Ok(Value::Float(product));
    }
    let mut num: i64 = 1;
    let mut den: i64 = 1;
    for arg in args {
        let (an, ad) = to_exact_pair(arg, span)?;
        num *= an;
        den *= ad;
    }
    Ok(make_rational(num, den))
}

fn eval_div(args: &[Value], name: &str, span: &Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .at(span));
    }
    if any_inexact(args) {
        let first = to_f64(&args[0], span)?;
        if args.len() == 1 {
            if first == 0.0 {
                return Err(EvalErrorKind::DivisionByZero.at(span));
            }
            return Ok(Value::Float(1.0 / first));
        }
        let mut result = first;
        for arg in &args[1..] {
            let divisor = to_f64(arg, span)?;
            if divisor == 0.0 {
                return Err(EvalErrorKind::DivisionByZero.at(span));
            }
            result /= divisor;
        }
        return Ok(Value::Float(result));
    }
    let (mut num, mut den) = to_exact_pair(&args[0], span)?;
    if args.len() == 1 {
        if num == 0 {
            return Err(EvalErrorKind::DivisionByZero.at(span));
        }
        return Ok(make_rational(den, num));
    }
    for arg in &args[1..] {
        let (an, ad) = to_exact_pair(arg, span)?;
        if an == 0 {
            return Err(EvalErrorKind::DivisionByZero.at(span));
        }
        num *= ad;
        den *= an;
    }
    Ok(make_rational(num, den))
}

fn eval_cmp(
    args: &[Value],
    name: &str,
    span: &Span,
    cmp: fn(f64, f64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let mut prev = to_f64(&args[0], span)?;
    for arg in &args[1..] {
        let curr = to_f64(arg, span)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn eval_cons(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "cons".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    Ok(Value::pair(args[0].clone(), args[1].clone()))
}

fn eval_car(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "car".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(p) => Ok(p.borrow().0.clone()),
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_cdr(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "cdr".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(p) => Ok(p.borrow().1.clone()),
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_set_car(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "set-car!".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(p) => {
            p.borrow_mut().0 = args[1].clone();
            Ok(Value::Nil)
        }
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_set_cdr(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "set-cdr!".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(p) => {
            p.borrow_mut().1 = args[1].clone();
            Ok(Value::Nil)
        }
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_cddr(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "cddr".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(p1) => {
            let cdr1 = p1.borrow().1.clone();
            match &cdr1 {
                Value::Pair(p2) => Ok(p2.borrow().1.clone()),
                other => Err(EvalErrorKind::Type {
                    expected: "pair".into(),
                    got: format!("{other}"),
                }
                .at(span)),
            }
        }
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_cadr(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "cadr".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(p1) => {
            let cdr1 = p1.borrow().1.clone();
            match &cdr1 {
                Value::Pair(p2) => Ok(p2.borrow().0.clone()),
                other => Err(EvalErrorKind::Type {
                    expected: "pair".into(),
                    got: format!("{other}"),
                }
                .at(span)),
            }
        }
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_null_pred(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "null?".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Nil)))
}

fn eval_list_builtin(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for arg in args.iter().rev() {
        result = Value::pair(arg.clone(), result);
    }
    Ok(result)
}

fn value_list_to_vec(val: &Value, span: &Span) -> Result<Vec<Value>, EvalError> {
    let mut result = Vec::new();
    let mut current = val.clone();
    loop {
        match &current {
            Value::Nil => return Ok(result),
            Value::Pair(p) => {
                let pair = p.borrow();
                let car = pair.0.clone();
                let cdr = pair.1.clone();
                drop(pair);
                result.push(car);
                current = cdr;
            }
            other => {
                return Err(EvalErrorKind::Type {
                    expected: "proper list".into(),
                    got: format!("{other}"),
                }
                .at(span))
            }
        }
    }
}

fn eval_apply(
    args: &[Value],
    span: &Span,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Arity {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail_args = value_list_to_vec(last, span)?;
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail_args);
    apply_step(func, &all_args, span, output, ctx)
}

fn eval_length(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "length".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let mut count: i64 = 0;
    let mut current = args[0].clone();
    loop {
        match current {
            Value::Nil => return Ok(Value::Integer(count)),
            Value::Pair(p) => {
                count += 1;
                current = p.borrow().1.clone();
            }
            other => {
                return Err(EvalErrorKind::Type {
                    expected: "proper list".into(),
                    got: format!("{other}"),
                }
                .at(span))
            }
        }
    }
}

fn eval_let_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "let requires bindings and body".into(),
        }
        .at(span));
    }

    // Check for named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalErrorKind::Parse {
                message: "named let requires bindings and body".into(),
            }
            .at(span));
        }
        let bindings_expr = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "named let: expected binding list".into(),
                }
                .at(span))
            }
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings_expr {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    let param = match &pair[0].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => {
                            return Err(EvalErrorKind::Parse {
                                message: "named let: expected symbol in binding".into(),
                            }
                            .at(span))
                        }
                    };
                    let val = eval(&pair[1], env, output, ctx)?;
                    params.push(param);
                    init_vals.push(val);
                }
                _ => {
                    return Err(EvalErrorKind::Parse {
                        message: "named let: invalid binding".into(),
                    }
                    .at(span))
                }
            }
        }
        let body = args[2..].to_vec();
        // Create env with the named function bound
        let let_env = Env::with_parent(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: let_env.clone(),
            name: Some(name.clone()),
        };
        let_env.define(name.clone(), lambda);
        // Bind initial values
        for (param, val) in params.iter().zip(init_vals.iter()) {
            let_env.define(param.clone(), val.clone());
        }
        // Evaluate body in tail position
        let body_exprs = &args[2..];
        if body_exprs.is_empty() {
            return Ok(TcoAction::Result(Value::Nil));
        }
        for expr in &body_exprs[..body_exprs.len() - 1] {
            eval(expr, &let_env, output, ctx)?;
        }
        return Ok(TcoAction::TailCall {
            expr: body_exprs[body_exprs.len() - 1].clone(),
            env: let_env,
        });
    }

    // Regular let
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "let: expected binding list".into(),
            }
            .at(span))
        }
    };
    let let_env = Env::with_parent(env);
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "let: expected symbol in binding".into(),
                        }
                        .at(span))
                    }
                };
                let val = eval(&pair[1], env, output, ctx)?;
                let_env.define(name, val);
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "let: invalid binding".into(),
                }
                .at(span))
            }
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(TcoAction::Result(Value::Nil));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &let_env, output, ctx)?;
    }
    Ok(TcoAction::TailCall {
        expr: body[body.len() - 1].clone(),
        env: let_env,
    })
}

fn eval_begin_step(
    args: &[Expr],
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.is_empty() {
        return Ok(TcoAction::Result(Value::Nil));
    }
    for expr in &args[..args.len() - 1] {
        eval(expr, env, output, ctx)?;
    }
    Ok(TcoAction::TailCall {
        expr: args[args.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_cond_step(
    clauses: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        for expr in &parts[1..parts.len() - 1] {
                            eval(expr, env, output, ctx)?;
                        }
                        return Ok(TcoAction::TailCall {
                            expr: parts[parts.len() - 1].clone(),
                            env: env.clone(),
                        });
                    }
                }
                let test = eval(&parts[0], env, output, ctx)?;
                if test.is_truthy() {
                    for expr in &parts[1..parts.len() - 1] {
                        eval(expr, env, output, ctx)?;
                    }
                    return Ok(TcoAction::TailCall {
                        expr: parts[parts.len() - 1].clone(),
                        env: env.clone(),
                    });
                }
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "cond: invalid clause".into(),
                }
                .at(span))
            }
        }
    }
    Ok(TcoAction::Result(Value::Nil))
}

// ===== L05: I/O builtins =====

fn eval_display(args: &[Value], span: &Span, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "display".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    args[0].display_fmt(output);
    Ok(Value::Nil)
}

fn eval_write(args: &[Value], span: &Span, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "write".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    args[0].write_fmt(output);
    Ok(Value::Nil)
}

fn eval_newline(args: &[Value], span: &Span, output: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: "newline".into(),
            expected: "0".into(),
            got: args.len(),
        }
        .at(span));
    }
    output.push('\n');
    Ok(Value::Nil)
}

// ===== L05: String/Symbol builtins =====

fn eval_string_append(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    let mut result = String::new();
    for arg in args {
        result.push_str(require_string(arg, span)?);
    }
    Ok(Value::SchemeString(result))
}

fn eval_string_length(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string-length".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    Ok(Value::Integer(s.len() as i64))
}

fn eval_substring(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalErrorKind::Arity {
            name: "substring".into(),
            expected: "3".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    let start = require_integer(&args[1], span)? as usize;
    let end = require_integer(&args[2], span)? as usize;
    if start > end || end > s.len() {
        return Err(EvalErrorKind::Type {
            expected: "valid substring indices".into(),
            got: format!("start={start}, end={end}, length={}", s.len()),
        }
        .at(span));
    }
    Ok(Value::SchemeString(s[start..end].to_string()))
}

fn eval_string_to_number(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string->number".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    match s.parse::<i64>() {
        Ok(n) => Ok(Value::Integer(n)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "number->string".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let n = require_integer(&args[0], span)?;
    Ok(Value::SchemeString(n.to_string()))
}

fn eval_symbol_to_string(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "symbol->string".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::SchemeString(s.clone())),
        other => Err(EvalErrorKind::Type {
            expected: "symbol".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_string_to_symbol(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string->symbol".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    Ok(Value::Symbol(s.to_string()))
}

fn eval_string_ref(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "string-ref".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    let idx = require_integer(&args[1], span)? as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalErrorKind::Type {
            expected: "valid string index".into(),
            got: format!("index {idx} for string of length {}", s.len()),
        }
        .at(span)),
    }
}

// ===== L06: Mutable Strings =====

fn eval_string_copy(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string-copy".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    Ok(Value::SchemeString(s.to_string()))
}

// ===== L13: Numeric/Char/String Utilities =====

fn require_char(v: &Value, span: &Span) -> Result<char, EvalError> {
    match v {
        Value::Char(c) => Ok(*c),
        other => Err(EvalErrorKind::Type {
            expected: "char".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_abs(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "abs".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let n = require_integer(&args[0], span)?;
    Ok(Value::Integer(n.abs()))
}

fn eval_modulo(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "modulo".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let a = require_integer(&args[0], span)?;
    let b = require_integer(&args[1], span)?;
    if b == 0 {
        return Err(EvalErrorKind::DivisionByZero.at(span));
    }
    Ok(Value::Integer(((a % b) + b) % b))
}

fn eval_remainder(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "remainder".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let a = require_integer(&args[0], span)?;
    let b = require_integer(&args[1], span)?;
    if b == 0 {
        return Err(EvalErrorKind::DivisionByZero.at(span));
    }
    Ok(Value::Integer(a % b))
}

fn eval_quotient(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "quotient".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let a = require_integer(&args[0], span)?;
    let b = require_integer(&args[1], span)?;
    if b == 0 {
        return Err(EvalErrorKind::DivisionByZero.at(span));
    }
    Ok(Value::Integer(a / b))
}

fn eval_min_max(
    args: &[Value],
    name: &str,
    span: &Span,
    is_better: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .at(span));
    }
    let mut best = require_integer(&args[0], span)?;
    for arg in &args[1..] {
        let n = require_integer(arg, span)?;
        if is_better(n, best) {
            best = n;
        }
    }
    Ok(Value::Integer(best))
}

fn eval_expt(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "expt".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let base = require_integer(&args[0], span)?;
    let exp = require_integer(&args[1], span)?;
    if exp < 0 {
        return Ok(Value::Integer(0)); // integer exponentiation truncates
    }
    Ok(Value::Integer(base.pow(exp as u32)))
}

fn eval_num_pred(
    args: &[Value],
    name: &str,
    span: &Span,
    pred: fn(i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let n = require_integer(&args[0], span)?;
    Ok(Value::Boolean(pred(n)))
}

fn eval_list_ref(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "list-ref".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let idx = require_integer(&args[1], span)? as usize;
    let mut current = args[0].clone();
    for _ in 0..idx {
        let next = match &current {
            Value::Pair(p) => p.borrow().1.clone(),
            _ => {
                return Err(EvalErrorKind::Type {
                    expected: "pair".into(),
                    got: format!("{current}"),
                }
                .at(span))
            }
        };
        current = next;
    }
    match &current {
        Value::Pair(p) => Ok(p.borrow().0.clone()),
        _ => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{current}"),
        }
        .at(span)),
    }
}

fn eval_list_tail(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "list-tail".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let idx = require_integer(&args[1], span)? as usize;
    let mut current = args[0].clone();
    for _ in 0..idx {
        let next = match &current {
            Value::Pair(p) => p.borrow().1.clone(),
            _ => {
                return Err(EvalErrorKind::Type {
                    expected: "pair".into(),
                    got: format!("{current}"),
                }
                .at(span))
            }
        };
        current = next;
    }
    Ok(current)
}

fn eval_list_pred(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "list?".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    // Tortoise-hare cycle detection
    let mut slow = args[0].clone();
    let mut fast = args[0].clone();
    loop {
        // Advance fast by 2 steps
        fast = match fast {
            Value::Nil => return Ok(Value::Boolean(true)),
            Value::Pair(p) => p.borrow().1.clone(),
            _ => return Ok(Value::Boolean(false)),
        };
        fast = match fast {
            Value::Nil => return Ok(Value::Boolean(true)),
            Value::Pair(p) => p.borrow().1.clone(),
            _ => return Ok(Value::Boolean(false)),
        };
        // Advance slow by 1 step
        slow = match slow {
            Value::Pair(p) => p.borrow().1.clone(),
            _ => return Ok(Value::Boolean(false)),
        };
        // Check if they point to the same pair (cycle)
        if let (Value::Pair(sp), Value::Pair(fp)) = (&slow, &fast) {
            if std::rc::Rc::ptr_eq(sp, fp) {
                return Ok(Value::Boolean(false));
            }
        }
    }
}

fn eval_assoc(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "assoc".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let key = &args[0];
    let mut alist = args[1].clone();
    loop {
        match &alist {
            Value::Nil => return Ok(Value::Boolean(false)),
            Value::Pair(p) => {
                let pair = p.borrow();
                let car = pair.0.clone();
                let cdr = pair.1.clone();
                drop(pair);
                if let Value::Pair(entry_p) = &car {
                    if entry_p.borrow().0 == *key {
                        return Ok(car);
                    }
                }
                alist = cdr;
            }
            _ => {
                return Err(EvalErrorKind::Type {
                    expected: "proper list".into(),
                    got: format!("{alist}"),
                }
                .at(span))
            }
        }
    }
}

fn eval_eq(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "eq?".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let result = match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a), Value::Pair(b)) => std::rc::Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => std::rc::Rc::ptr_eq(a, b),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn eval_equal(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "equal?".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    Ok(Value::Boolean(args[0] == args[1]))
}

fn eval_map(
    args: &[Value],
    span: &Span,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Arity {
            name: "map".into(),
            expected: "at least 2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let func = &args[0];
    let mut lists: Vec<Vec<Value>> = Vec::new();
    for arg in &args[1..] {
        lists.push(value_list_to_vec(arg, span)?);
    }
    let len = lists[0].len();
    for list in &lists[1..] {
        if list.len() != len {
            return Err(EvalErrorKind::Type {
                expected: "lists of equal length".into(),
                got: "lists of different lengths".into(),
            }
            .at(span));
        }
    }
    let mut result_vec = Vec::new();
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        let val = apply_func(func, &call_args, span, output, ctx)?;
        result_vec.push(val);
    }
    let mut result = Value::Nil;
    for val in result_vec.into_iter().rev() {
        result = Value::pair(val, result);
    }
    Ok(result)
}

fn eval_char_pred(
    args: &[Value],
    name: &str,
    span: &Span,
    pred: fn(char) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let c = require_char(&args[0], span)?;
    Ok(Value::Boolean(pred(c)))
}

fn eval_char_case(
    args: &[Value],
    name: &str,
    span: &Span,
    convert: fn(char) -> char,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let c = require_char(&args[0], span)?;
    Ok(Value::Char(convert(c)))
}

fn eval_char_cmp(
    args: &[Value],
    name: &str,
    span: &Span,
    cmp: fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let a = require_char(&args[0], span)?;
    let b = require_char(&args[1], span)?;
    Ok(Value::Boolean(cmp(a, b)))
}

fn eval_string_cmp(
    args: &[Value],
    name: &str,
    span: &Span,
    cmp: fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let a = require_string(&args[0], span)?;
    let b = require_string(&args[1], span)?;
    Ok(Value::Boolean(cmp(a, b)))
}

fn eval_string_ci_eq(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "string-ci=?".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let a = require_string(&args[0], span)?;
    let b = require_string(&args[1], span)?;
    Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase()))
}

fn eval_string_case(
    args: &[Value],
    name: &str,
    span: &Span,
    convert: fn(&str) -> String,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    Ok(Value::SchemeString(convert(s)))
}

fn eval_define_syntax(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Parse {
            message: "define-syntax requires a name and a transformer".into(),
        }
        .at(span));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(n) => n.clone(),
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "define-syntax: expected symbol as name".into(),
            }
            .at(span))
        }
    };
    match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            if let ExprKind::Symbol(kw) = &parts[0].kind {
                // (syntax-rules (literals...) rules...)
                if kw == "syntax-rules" {
                    let syntax_rules =
                        macro_expand::parse_syntax_rules(&parts[1..], span)?;
                    env.define(
                        name,
                        Value::Macro {
                            syntax_rules,
                            def_env: env.clone(),
                        },
                    );
                    return Ok(Value::Nil);
                }
                // (lambda (stx) body...) — syntax-case transformer
                if kw == "lambda" {
                    let transformer = eval_lambda(&parts[1..], span, env)?;
                    if let Value::Lambda {
                            params,
                            body,
                            env: lambda_env,
                            ..
                        } = transformer {
                        env.define(
                            name,
                            Value::SyntaxCaseMacro {
                                params,
                                body,
                                def_env: lambda_env,
                            },
                        );
                        return Ok(Value::Nil);
                    }
                }
            }
            // Fall through: try evaluating the transformer expression
            let transformer = eval(&args[1], env, output, ctx)?;
            match transformer {
                Value::Lambda {
                    params,
                    body,
                    env: lambda_env,
                    ..
                } => {
                    env.define(
                        name,
                        Value::SyntaxCaseMacro {
                            params,
                            body,
                            def_env: lambda_env,
                        },
                    );
                    Ok(Value::Nil)
                }
                _ => Err(EvalErrorKind::Parse {
                    message: "define-syntax: expected syntax-rules or lambda transformer".into(),
                }
                .at(span)),
            }
        }
        _ => Err(EvalErrorKind::Parse {
            message: "define-syntax: expected syntax-rules or lambda transformer".into(),
        }
        .at(span)),
    }
}

fn eval_string_to_list(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string->list".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    let mut result = Value::Nil;
    for ch in s.chars().rev() {
        result = Value::pair(Value::Char(ch), result);
    }
    Ok(result)
}

fn eval_list_to_string(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "list->string".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let mut s = String::new();
    let mut current = args[0].clone();
    loop {
        match &current {
            Value::Nil => break,
            Value::Pair(p) => {
                let pair = p.borrow();
                match &pair.0 {
                    Value::Char(c) => s.push(*c),
                    other => {
                        return Err(EvalErrorKind::Type {
                            expected: "char".into(),
                            got: format!("{other}"),
                        }
                        .at(span))
                    }
                }
                let next = pair.1.clone();
                drop(pair);
                current = next;
            }
            other => {
                return Err(EvalErrorKind::Type {
                    expected: "proper list of chars".into(),
                    got: format!("{other}"),
                }
                .at(span))
            }
        }
    }
    Ok(Value::SchemeString(s))
}

fn eval_char_to_integer(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "char->integer".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Integer(*c as i64)),
        other => Err(EvalErrorKind::Type {
            expected: "char".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_integer_to_char(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "integer->char".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let n = require_integer(&args[0], span)?;
    match char::from_u32(n as u32) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalErrorKind::Type {
            expected: "valid Unicode code point".into(),
            got: format!("{n}"),
        }
        .at(span)),
    }
}

// ===== Level 15: letrec, letrec*, case, eqv?, vectors =====

fn eval_letrec_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "letrec requires bindings and body".into(),
        }
        .at(span));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "letrec: expected binding list".into(),
            }
            .at(span))
        }
    };
    let letrec_env = Env::with_parent(env);
    // First pass: bind all names to Nil (placeholder)
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "letrec: expected symbol in binding".into(),
                        }
                        .at(span))
                    }
                };
                letrec_env.define(name.clone(), Value::Nil);
                names.push(name);
                init_exprs.push(&pair[1]);
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "letrec: invalid binding".into(),
                }
                .at(span))
            }
        }
    }
    // Second pass: evaluate all init exprs in letrec_env, then set!
    let vals: Vec<Value> = init_exprs
        .iter()
        .map(|e| eval(e, &letrec_env, output, ctx))
        .collect::<Result<Vec<_>, _>>()?;
    for (name, val) in names.iter().zip(vals) {
        letrec_env.set(name, val).map_err(|e| e.with_span(span))?;
    }
    // Evaluate body
    let body = &args[1..];
    if body.is_empty() {
        return Ok(TcoAction::Result(Value::Nil));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &letrec_env, output, ctx)?;
    }
    Ok(TcoAction::TailCall {
        expr: body[body.len() - 1].clone(),
        env: letrec_env,
    })
}

fn eval_letrec_star_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "letrec* requires bindings and body".into(),
        }
        .at(span));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "letrec*: expected binding list".into(),
            }
            .at(span))
        }
    };
    let letrec_env = Env::with_parent(env);
    // Bind sequentially: each init can see previous bindings
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "letrec*: expected symbol in binding".into(),
                        }
                        .at(span))
                    }
                };
                let val = eval(&pair[1], &letrec_env, output, ctx)?;
                letrec_env.define(name, val);
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "letrec*: invalid binding".into(),
                }
                .at(span))
            }
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(TcoAction::Result(Value::Nil));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &letrec_env, output, ctx)?;
    }
    Ok(TcoAction::TailCall {
        expr: body[body.len() - 1].clone(),
        env: letrec_env,
    })
}

fn eval_case_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "case requires a key expression".into(),
        }
        .at(span));
    }
    let key = eval(&args[0], env, output, ctx)?;
    for clause in &args[1..] {
        let clause_elems = match &clause.kind {
            ExprKind::List(elems) => elems,
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "case: invalid clause".into(),
                }
                .at(span))
            }
        };
        if clause_elems.is_empty() {
            return Err(EvalErrorKind::Parse {
                message: "case: empty clause".into(),
            }
            .at(span));
        }
        // Check for else clause
        if let ExprKind::Symbol(s) = &clause_elems[0].kind {
            if s == "else" {
                let body = &clause_elems[1..];
                if body.is_empty() {
                    return Ok(TcoAction::Result(Value::Nil));
                }
                for expr in &body[..body.len() - 1] {
                    eval(expr, env, output, ctx)?;
                }
                return Ok(TcoAction::TailCall {
                    expr: body[body.len() - 1].clone(),
                    env: env.clone(),
                });
            }
        }
        // Datums list
        let datums = match &clause_elems[0].kind {
            ExprKind::List(d) => d,
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "case: expected datum list".into(),
                }
                .at(span))
            }
        };
        let matched = datums.iter().any(|d| {
            let datum_val = expr_to_datum(d);
            eqv_compare(&key, &datum_val)
        });
        if matched {
            let body = &clause_elems[1..];
            if body.is_empty() {
                return Ok(TcoAction::Result(Value::Nil));
            }
            for expr in &body[..body.len() - 1] {
                eval(expr, env, output, ctx)?;
            }
            return Ok(TcoAction::TailCall {
                expr: body[body.len() - 1].clone(),
                env: env.clone(),
            });
        }
    }
    // No match and no else clause
    Ok(TcoAction::Result(Value::Nil))
}

/// Convert a datum expression to a Value for eqv? comparison in case.
fn expr_to_datum(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(num, den) => make_rational(*num, *den),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::SchemeString(s) => Value::SchemeString(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(elems) if elems.is_empty() => Value::Nil,
        ExprKind::List(_) => Value::Nil, // shouldn't occur for case datums
    }
}

/// eqv? comparison: identity for objects, value equality for atoms.
fn eqv_compare(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

fn eval_eqv(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "eqv?".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    Ok(Value::Boolean(eqv_compare(&args[0], &args[1])))
}

fn eval_vector_create(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(
        args.to_vec(),
    ))))
}

fn eval_make_vector(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalErrorKind::Arity {
            name: "make-vector".into(),
            expected: "1 or 2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let len = require_integer(&args[0], span)? as usize;
    let fill = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::Integer(0)
    };
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(
        vec![fill; len],
    ))))
}

fn eval_vector_ref(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "vector-ref".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Vector(v) => {
            let idx = require_integer(&args[1], span)? as usize;
            let vec = v.borrow();
            if idx >= vec.len() {
                return Err(EvalErrorKind::Type {
                    expected: format!("index < {}", vec.len()),
                    got: format!("{idx}"),
                }
                .at(span));
            }
            Ok(vec[idx].clone())
        }
        other => Err(EvalErrorKind::Type {
            expected: "vector".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_vector_set(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalErrorKind::Arity {
            name: "vector-set!".into(),
            expected: "3".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Vector(v) => {
            let idx = require_integer(&args[1], span)? as usize;
            let mut vec = v.borrow_mut();
            if idx >= vec.len() {
                return Err(EvalErrorKind::Type {
                    expected: format!("index < {}", vec.len()),
                    got: format!("{idx}"),
                }
                .at(span));
            }
            vec[idx] = args[2].clone();
            Ok(Value::Nil)
        }
        other => Err(EvalErrorKind::Type {
            expected: "vector".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_vector_length(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "vector-length".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
        other => Err(EvalErrorKind::Type {
            expected: "vector".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_vector_to_list(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "vector->list".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Vector(v) => {
            let vec = v.borrow();
            let mut result = Value::Nil;
            for elem in vec.iter().rev() {
                result = Value::pair(elem.clone(), result);
            }
            Ok(result)
        }
        other => Err(EvalErrorKind::Type {
            expected: "vector".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_reverse(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "reverse".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let elems = value_list_to_vec(&args[0], span)?;
    let mut result = Value::Nil;
    for elem in elems {
        result = Value::pair(elem, result);
    }
    Ok(result)
}

fn eval_list_to_vector(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "list->vector".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let elems = value_list_to_vec(&args[0], span)?;
    Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(
        elems,
    ))))
}

// ── syntax-case support ──────────────────────────────────────────────

/// Create a simple SyntaxObject value wrapping an Expr (no hygiene data).
fn make_syntax_object(expr: Expr) -> Value {
    Value::SyntaxObject(Box::new(SyntaxObjectData {
        expr,
        introduced: Vec::new(),
        template_env: None,
    }))
}

/// Create a SyntaxObject with hygiene data.
fn make_syntax_object_with_hygiene(
    expr: Expr,
    introduced: Vec<(String, String)>,
    env: Env,
) -> Value {
    Value::SyntaxObject(Box::new(SyntaxObjectData {
        expr,
        introduced,
        template_env: Some(env),
    }))
}

/// Convert a Value to an Expr (for datum->syntax).
fn value_to_expr(val: &Value, span: &Span) -> Expr {
    let kind = match val {
        Value::Integer(n) => ExprKind::Integer(*n),
        Value::Float(f) => ExprKind::Float(*f),
        Value::Rational(n, d) => ExprKind::Rational(*n, *d),
        Value::Boolean(b) => ExprKind::Boolean(*b),
        Value::SchemeString(s) => ExprKind::SchemeString(s.clone()),
        Value::Char(c) => ExprKind::Char(*c),
        Value::Symbol(s) => ExprKind::Symbol(s.clone()),
        Value::Nil => ExprKind::List(vec![]),
        Value::Pair(_) => {
            let mut elems = Vec::new();
            let mut cur = val.clone();
            loop {
                match cur {
                    Value::Pair(p) => {
                        let pair = p.borrow();
                        elems.push(value_to_expr(&pair.0, span));
                        cur = pair.1.clone();
                    }
                    Value::Nil => break,
                    other => {
                        // Improper list — not fully supported but handle gracefully
                        elems.push(value_to_expr(&other, span));
                        break;
                    }
                }
            }
            ExprKind::List(elems)
        }
        Value::SyntaxObject(data) => return data.expr.clone(),
        _ => ExprKind::Symbol(format!("{val}")),
    };
    Expr {
        kind,
        span: span.clone(),
    }
}

/// Convert a syntax-case pattern binding to a Value.
fn syntax_binding_to_value(binding: &SyntaxBinding) -> Value {
    match binding {
        SyntaxBinding::Single(expr) => make_syntax_object(expr.clone()),
        SyntaxBinding::Ellipsis(exprs) => {
            let mut list = Value::Nil;
            for e in exprs.iter().rev() {
                list = Value::pair(make_syntax_object(e.clone()), list);
            }
            list
        }
    }
}

/// Arguments for invoking a syntax-case macro transformer.
struct SyntaxCaseInvocation<'a> {
    params: &'a [String],
    body: &'a [Expr],
    def_env: &'a Env,
    use_elements: &'a [Expr],
    span: &'a Span,
    use_env: &'a Env,
}

/// Invoke a syntax-case macro transformer.
fn eval_syntax_case_macro_invocation(
    inv: &SyntaxCaseInvocation<'_>,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    let SyntaxCaseInvocation { params, body, def_env, use_elements, span, use_env } = *inv;
    // Build a syntax object from the use-site form
    let stx_expr = Expr {
        kind: ExprKind::List(use_elements.to_vec()),
        span: span.clone(),
    };
    let stx_val = make_syntax_object(stx_expr);

    // Call the transformer lambda: bind param, evaluate body
    let call_env = Env::with_parent(def_env);
    if !params.is_empty() {
        call_env.define(params[0].clone(), stx_val);
    }

    // Evaluate the transformer body
    let result = eval_sequence(body, &call_env, output, ctx)?;

    // The result should be a SyntaxObject — extract the Expr and evaluate it
    match result {
        Value::SyntaxObject(data) => {
            let template_env = data.template_env.as_ref().unwrap_or(&call_env);

            // Set up hygiene bindings in the use-site env
            for (gensym, original) in &data.introduced {
                if let Ok(val) = template_env.lookup(original) {
                    use_env.define(gensym.clone(), val);
                }
            }
            Ok(TcoAction::TailCall {
                expr: data.expr.clone(),
                env: use_env.clone(),
            })
        }
        _ => Err(EvalErrorKind::Parse {
            message: "syntax-case transformer must return a syntax object".into(),
        }
        .at(span)),
    }
}

/// Apply hygiene renaming to template-introduced identifiers in an expanded Expr.
/// Pattern variable substitution has already happened (they contain use-site exprs).
/// We only rename identifiers that are NOT keywords and NOT already from the use-site.
/// Evaluate `(syntax-case expr (literals) clause ...)`.
fn eval_syntax_case(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "syntax-case requires an expression, literals, and at least one clause".into(),
        }
        .at(span));
    }

    // Evaluate the expression to get a syntax object
    let stx_val = eval(&args[0], env, output, ctx)?;
    let stx_expr = match &stx_val {
        Value::SyntaxObject(data) => data.expr.clone(),
        _ => value_to_expr(&stx_val, span),
    };

    // Parse literals
    let literals = match &args[1].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalErrorKind::Parse {
                    message: "syntax-case: literals must be symbols".into(),
                }
                .at(span)),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "syntax-case: expected literal list".into(),
            }
            .at(span))
        }
    };

    // Try each clause
    let clauses = &args[2..];
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let pattern = &parts[0];
                let (fender, template_expr) = if parts.len() == 3 {
                    (Some(&parts[1]), &parts[2])
                } else {
                    (None, &parts[1])
                };

                // Try to match the pattern against the syntax object
                let mut bindings: Vec<(String, SyntaxBinding)> = Vec::new();
                if syntax_case_match(pattern, &stx_expr, &literals, &mut bindings) {
                    // Bind pattern variables in a child env
                    let match_env = Env::with_parent(env);
                    for (name, binding) in &bindings {
                        let val = syntax_binding_to_value(binding);
                        match_env.define(name.clone(), val);
                    }
                    // Store binding metadata for the syntax template to use
                    let binding_names: Vec<String> =
                        bindings.iter().map(|(n, _)| n.clone()).collect();
                    let ellipsis_names: Vec<String> = bindings
                        .iter()
                        .filter(|(_, b)| matches!(b, SyntaxBinding::Ellipsis(_)))
                        .map(|(n, _)| n.clone())
                        .collect();
                    match_env.define(
                        "##syntax-case-bindings##".into(),
                        Value::Symbol(binding_names.join(",")),
                    );
                    match_env.define(
                        "##syntax-case-ellipsis##".into(),
                        Value::Symbol(ellipsis_names.join(",")),
                    );

                    // Evaluate fender if present
                    if let Some(fender_expr) = fender {
                        let fender_val = eval(fender_expr, &match_env, output, ctx)?;
                        if !fender_val.is_truthy() {
                            continue;
                        }
                    }

                    // Evaluate the template expression
                    return eval(template_expr, &match_env, output, ctx);
                }
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "syntax-case: invalid clause".into(),
                }
                .at(span))
            }
        }
    }

    Err(EvalErrorKind::Parse {
        message: "syntax-case: no matching clause".into(),
    }
    .at(span))
}

/// A binding from syntax-case pattern matching.
enum SyntaxBinding {
    Single(Expr),
    Ellipsis(Vec<Expr>),
}

/// Match a syntax-case pattern against a syntax object (Expr).
/// The pattern includes the macro name as first element (matched by _).
fn syntax_case_match(
    pattern: &Expr,
    stx: &Expr,
    literals: &[String],
    bindings: &mut Vec<(String, SyntaxBinding)>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(name) if name == "_" => true,
        ExprKind::Symbol(name) if literals.contains(name) => {
            matches!(&stx.kind, ExprKind::Symbol(s) if s == name)
        }
        ExprKind::Symbol(name) => {
            bindings.push((name.clone(), SyntaxBinding::Single(stx.clone())));
            true
        }
        ExprKind::List(pat_elems) => {
            match &stx.kind {
                ExprKind::List(stx_elems) => {
                    syntax_case_match_elements(pat_elems, stx_elems, literals, bindings)
                }
                _ => false,
            }
        }
        _ => pattern.kind == stx.kind,
    }
}

/// Match pattern elements against syntax elements (handling ellipsis).
fn syntax_case_match_elements(
    pat: &[Expr],
    form: &[Expr],
    literals: &[String],
    bindings: &mut Vec<(String, SyntaxBinding)>,
) -> bool {
    // Find ellipsis position
    let ellipsis_pos = pat
        .iter()
        .position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."));

    match ellipsis_pos {
        Some(pos) if pos > 0 => {
            let before_ellipsis = &pat[..pos - 1];
            let ellipsis_var = &pat[pos - 1];
            let after_ellipsis = &pat[pos + 1..];

            if form.len() < before_ellipsis.len() + after_ellipsis.len() {
                return false;
            }

            // Match fixed elements before
            for (p, f) in before_ellipsis.iter().zip(form.iter()) {
                if !syntax_case_match(p, f, literals, bindings) {
                    return false;
                }
            }

            // Match fixed elements after
            let after_start = form.len() - after_ellipsis.len();
            for (p, f) in after_ellipsis.iter().zip(form[after_start..].iter()) {
                if !syntax_case_match(p, f, literals, bindings) {
                    return false;
                }
            }

            // The ellipsis variable matches everything in between
            let ellipsis_forms = &form[before_ellipsis.len()..after_start];
            match &ellipsis_var.kind {
                ExprKind::Symbol(name) if !literals.contains(name) && name != "_" => {
                    bindings.push((
                        name.clone(),
                        SyntaxBinding::Ellipsis(ellipsis_forms.to_vec()),
                    ));
                    true
                }
                _ => false,
            }
        }
        _ => {
            // No ellipsis — exact count
            if pat.len() != form.len() {
                return false;
            }
            for (p, f) in pat.iter().zip(form.iter()) {
                if !syntax_case_match(p, f, literals, bindings) {
                    return false;
                }
            }
            true
        }
    }
}

/// Evaluate `(syntax template)` — construct a syntax object from a template.
/// Pattern variables from syntax-case are substituted; template-introduced
/// identifiers are renamed for hygiene.
fn eval_syntax_template(args: &[Expr], span: &Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Parse {
            message: "syntax requires exactly one argument".into(),
        }
        .at(span));
    }

    // Retrieve binding metadata from the environment
    let binding_names: Vec<String> = match env.lookup("##syntax-case-bindings##") {
        Ok(Value::Symbol(s)) if !s.is_empty() => s.split(',').map(|s| s.to_string()).collect(),
        _ => Vec::new(),
    };
    let ellipsis_names: Vec<String> = match env.lookup("##syntax-case-ellipsis##") {
        Ok(Value::Symbol(s)) if !s.is_empty() => s.split(',').map(|s| s.to_string()).collect(),
        _ => Vec::new(),
    };

    let hygiene_id = match env.lookup("##syntax-hygiene-id##") {
        Ok(Value::Integer(n)) => n as u64,
        _ => 0,
    };

    let mut introduced = Vec::new();
    let result_expr = instantiate_syntax_template(
        &args[0],
        &binding_names,
        &ellipsis_names,
        env,
        span,
        hygiene_id,
        &mut introduced,
    )?;

    Ok(make_syntax_object_with_hygiene(result_expr, introduced, env.clone()))
}

/// Instantiate a syntax template, substituting pattern variables and
/// renaming template-introduced identifiers.
fn instantiate_syntax_template(
    template: &Expr,
    binding_names: &[String],
    ellipsis_names: &[String],
    env: &Env,
    span: &Span,
    hygiene_id: u64,
    introduced: &mut Vec<(String, String)>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if binding_names.contains(name) {
                // Pattern variable — substitute with its binding
                match env.lookup(name) {
                    Ok(Value::SyntaxObject(data)) => Ok(data.expr.clone()),
                    Ok(other) => Ok(value_to_expr(&other, span)),
                    Err(_) => Ok(template.clone()),
                }
            } else if macro_expand::is_keyword(name) || is_builtin(name) || name == "syntax-case" || name == "syntax" || name == "with-syntax" {
                Ok(template.clone())
            } else {
                // Template-introduced identifier: rename for hygiene
                let gensym = format!("{name}##h{hygiene_id}");
                introduced.push((gensym.clone(), name.clone()));
                Ok(Expr {
                    kind: ExprKind::Symbol(gensym),
                    span: template.span.clone(),
                })
            }
        }
        ExprKind::List(elements) => {
            // Don't recurse into quoted forms
            if let Some(first) = elements.first() {
                if matches!(&first.kind, ExprKind::Symbol(s) if s == "quote") {
                    return Ok(template.clone());
                }
            }
            let mut result = Vec::new();
            let mut i = 0;
            while i < elements.len() {
                // Check if next element is `...`
                if i + 1 < elements.len() {
                    if let ExprKind::Symbol(s) = &elements[i + 1].kind {
                        if s == "..." {
                            // Expand the ellipsis
                            let expanded = expand_syntax_ellipsis(
                                &elements[i],
                                binding_names,
                                ellipsis_names,
                                env,
                                span,
                                hygiene_id,
                                introduced,
                            )?;
                            result.extend(expanded);
                            i += 2;
                            continue;
                        }
                    }
                }
                result.push(instantiate_syntax_template(
                    &elements[i],
                    binding_names,
                    ellipsis_names,
                    env,
                    span,
                    hygiene_id,
                    introduced,
                )?);
                i += 1;
            }
            Ok(Expr {
                kind: ExprKind::List(result),
                span: template.span.clone(),
            })
        }
        _ => Ok(template.clone()),
    }
}

/// Expand an ellipsis-preceded template element.
fn expand_syntax_ellipsis(
    template_elem: &Expr,
    binding_names: &[String],
    ellipsis_names: &[String],
    env: &Env,
    span: &Span,
    hygiene_id: u64,
    introduced: &mut Vec<(String, String)>,
) -> Result<Vec<Expr>, EvalError> {
    // Find which ellipsis variable is used in the template element
    let ellipsis_var = find_syntax_ellipsis_var(template_elem, ellipsis_names);

    match ellipsis_var {
        Some(var_name) => {
            // Get the list of syntax objects for this variable
            let list_val = env.lookup(&var_name).unwrap_or(Value::Nil);
            let items = value_list_to_syntax_objects(&list_val);

            let mut result = Vec::new();
            for item in &items {
                // Create a temporary env with this ellipsis var bound to a single item
                let temp_env = Env::with_parent(env);
                temp_env.define(var_name.clone(), item.clone());
                // Copy binding metadata
                if let Ok(v) = env.lookup("##syntax-case-bindings##") {
                    temp_env.define("##syntax-case-bindings##".into(), v);
                }
                if let Ok(v) = env.lookup("##syntax-case-ellipsis##") {
                    temp_env.define("##syntax-case-ellipsis##".into(), v);
                }

                result.push(instantiate_syntax_template(
                    template_elem,
                    binding_names,
                    ellipsis_names,
                    &temp_env,
                    span,
                    hygiene_id,
                    introduced,
                )?);
            }
            Ok(result)
        }
        None => Ok(Vec::new()),
    }
}

/// Find an ellipsis pattern variable used in a template expression.
fn find_syntax_ellipsis_var(template: &Expr, ellipsis_names: &[String]) -> Option<String> {
    match &template.kind {
        ExprKind::Symbol(name) if ellipsis_names.contains(name) => Some(name.clone()),
        ExprKind::List(elements) => {
            for elem in elements {
                if let Some(var) = find_syntax_ellipsis_var(elem, ellipsis_names) {
                    return Some(var);
                }
            }
            None
        }
        _ => None,
    }
}

/// Extract syntax objects from a Value list into a Vec.
fn value_list_to_syntax_objects(val: &Value) -> Vec<Value> {
    let mut result = Vec::new();
    let mut cur = val.clone();
    loop {
        match cur {
            Value::Pair(p) => {
                let pair = p.borrow();
                result.push(pair.0.clone());
                cur = pair.1.clone();
            }
            Value::Nil => break,
            other => {
                result.push(other);
                break;
            }
        }
    }
    result
}

/// Evaluate `(with-syntax ((pattern expr) ...) body ...)`.
fn eval_with_syntax_step(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
    ctx: &mut ContCtx,
) -> Result<TcoAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "with-syntax requires bindings and body".into(),
        }
        .at(span));
    }

    let bindings_expr = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "with-syntax: expected binding list".into(),
            }
            .at(span))
        }
    };

    let ws_env = Env::with_parent(env);

    // Copy syntax-case metadata from parent
    if let Ok(v) = env.lookup("##syntax-case-bindings##") {
        if let Value::Symbol(existing) = &v {
            let mut names: Vec<String> = if existing.is_empty() {
                Vec::new()
            } else {
                existing.split(',').map(|s| s.to_string()).collect()
            };

            // Process each binding
            for binding in bindings_expr {
                match &binding.kind {
                    ExprKind::List(parts) if parts.len() == 2 => {
                        let pat_name = match &parts[0].kind {
                            ExprKind::Symbol(n) => n.clone(),
                            _ => {
                                return Err(EvalErrorKind::Parse {
                                    message: "with-syntax: pattern must be a symbol".into(),
                                }
                                .at(span))
                            }
                        };
                        let val = eval(&parts[1], env, output, ctx)?;
                        ws_env.define(pat_name.clone(), val);
                        if !names.contains(&pat_name) {
                            names.push(pat_name);
                        }
                    }
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "with-syntax: invalid binding".into(),
                        }
                        .at(span))
                    }
                }
            }

            ws_env.define(
                "##syntax-case-bindings##".into(),
                Value::Symbol(names.join(",")),
            );
        }
    } else {
        let mut names = Vec::new();
        for binding in bindings_expr {
            match &binding.kind {
                ExprKind::List(parts) if parts.len() == 2 => {
                    let pat_name = match &parts[0].kind {
                        ExprKind::Symbol(n) => n.clone(),
                        _ => {
                            return Err(EvalErrorKind::Parse {
                                message: "with-syntax: pattern must be a symbol".into(),
                            }
                            .at(span))
                        }
                    };
                    let val = eval(&parts[1], env, output, ctx)?;
                    ws_env.define(pat_name.clone(), val);
                    names.push(pat_name);
                }
                _ => {
                    return Err(EvalErrorKind::Parse {
                        message: "with-syntax: invalid binding".into(),
                    }
                    .at(span))
                }
            }
        }
        ws_env.define(
            "##syntax-case-bindings##".into(),
            Value::Symbol(names.join(",")),
        );
    }

    // Copy ellipsis metadata
    if let Ok(v) = env.lookup("##syntax-case-ellipsis##") {
        ws_env.define("##syntax-case-ellipsis##".into(), v);
    } else {
        ws_env.define("##syntax-case-ellipsis##".into(), Value::Symbol(String::new()));
    }

    // Evaluate body expressions
    let body = &args[1..];
    if body.is_empty() {
        return Ok(TcoAction::Result(Value::Nil));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &ws_env, output, ctx)?;
    }
    Ok(TcoAction::TailCall {
        expr: body[body.len() - 1].clone(),
        env: ws_env,
    })
}

/// Evaluate `(syntax->datum stx)` — convert syntax object to datum.
fn eval_syntax_to_datum(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "syntax->datum".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::SyntaxObject(data) => expr_to_value(&data.expr),
        other => Ok(other.clone()),
    }
}

/// Evaluate `(datum->syntax template-id datum)` — convert datum to syntax object.
fn eval_datum_to_syntax(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "datum->syntax".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    // template-id is args[0] (used for lexical context, mostly ignored here)
    // datum is args[1]
    let expr = value_to_expr(&args[1], span);
    Ok(make_syntax_object(expr))
}
