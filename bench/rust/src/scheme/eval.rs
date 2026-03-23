use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::macros;
use crate::scheme::value::{make_rational, Mutability, RecordOp, Value};

/// Numeric tower: exact integers, exact rationals, inexact floats.
#[derive(Debug, Clone, Copy)]
enum Num {
    Int(i64),
    Rat(i64, i64), // numerator, denominator (simplified, denom > 0)
    Flt(f64),
}

impl Num {
    fn to_f64(self) -> f64 {
        match self {
            Num::Int(n) => n as f64,
            Num::Rat(n, d) => n as f64 / d as f64,
            Num::Flt(f) => f,
        }
    }

    fn to_value(self) -> Value {
        match self {
            Num::Int(n) => Value::int(n),
            Num::Rat(n, d) => make_rational(n, d, crate::scheme::error::Span::default()),
            Num::Flt(f) => Value::float(f),
        }
    }

    fn add(self, other: Num) -> Num {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => simplify_rat(an * bd + bn * ad, ad * bd),
            (Num::Int(a), Num::Rat(bn, bd)) => simplify_rat(a * bd + bn, bd),
            (Num::Rat(an, ad), Num::Int(b)) => simplify_rat(an + b * ad, ad),
            (Num::Flt(a), other) => Num::Flt(a + other.to_f64()),
            (other, Num::Flt(b)) => Num::Flt(other.to_f64() + b),
        }
    }

    fn sub(self, other: Num) -> Num {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => simplify_rat(an * bd - bn * ad, ad * bd),
            (Num::Int(a), Num::Rat(bn, bd)) => simplify_rat(a * bd - bn, bd),
            (Num::Rat(an, ad), Num::Int(b)) => simplify_rat(an - b * ad, ad),
            (Num::Flt(a), other) => Num::Flt(a - other.to_f64()),
            (other, Num::Flt(b)) => Num::Flt(other.to_f64() - b),
        }
    }

    fn mul(self, other: Num) -> Num {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => simplify_rat(an * bn, ad * bd),
            (Num::Int(a), Num::Rat(bn, bd)) => simplify_rat(a * bn, bd),
            (Num::Rat(an, ad), Num::Int(b)) => simplify_rat(an * b, ad),
            (Num::Flt(a), other) => Num::Flt(a * other.to_f64()),
            (other, Num::Flt(b)) => Num::Flt(other.to_f64() * b),
        }
    }

    fn div(self, other: Num) -> Option<Num> {
        match (self, other) {
            (_, Num::Int(0)) => None,
            (_, Num::Rat(0, _)) => None,
            (Num::Int(a), Num::Int(b)) => Some(simplify_rat(a, b)),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => Some(simplify_rat(an * bd, ad * bn)),
            (Num::Int(a), Num::Rat(bn, bd)) => Some(simplify_rat(a * bd, bn)),
            (Num::Rat(an, ad), Num::Int(b)) => Some(simplify_rat(an, ad * b)),
            (Num::Flt(a), other) => {
                let b = other.to_f64();
                if b == 0.0 { None } else { Some(Num::Flt(a / b)) }
            }
            (other, Num::Flt(b)) => {
                if b == 0.0 { None } else { Some(Num::Flt(other.to_f64() / b)) }
            }
        }
    }

    fn cmp_f64(self) -> f64 {
        self.to_f64()
    }
}

fn simplify_rat(numer: i64, denom: i64) -> Num {
    if denom == 0 {
        return Num::Int(0); // should not happen
    }
    let sign = if denom < 0 { -1 } else { 1 };
    let n = numer * sign;
    let d = denom * sign;
    let g = gcd(n.abs(), d.abs());
    let n = n / g;
    let d = d / g;
    if d == 1 {
        Num::Int(n)
    } else {
        Num::Rat(n, d)
    }
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn value_to_num(v: &Value) -> Option<Num> {
    match v {
        Value::Integer(n, _) => Some(Num::Int(*n)),
        Value::Rational(n, d, _) => Some(Num::Rat(*n, *d)),
        Value::Float(f, _) => Some(Num::Flt(*f)),
        _ => None,
    }
}

fn require_nums(args: &[Value]) -> Result<Vec<Num>, EvalError> {
    args.iter()
        .map(|v| value_to_num(v).ok_or_else(|| EvalError::TypeMismatch {
            expected: "number".to_string(),
            got: format!("{v}"),
            span: v.span(),
        }))
        .collect()
}

/// Evaluate non-tail expressions of `and`. Returns `Some(value)` for short-circuit, `None` for tail.
fn eval_and_prefix(exprs: &[Value], env: &Env) -> Result<Option<Value>, EvalError> {
    for expr in exprs {
        let result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

/// Evaluate non-tail expressions of `or`. Returns `Some(value)` for short-circuit, `None` for tail.
fn eval_or_prefix(exprs: &[Value], env: &Env) -> Result<Option<Value>, EvalError> {
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

/// Validate argument count and bind parameters for a closure call.
/// Returns the local environment with all params (and rest param) bound.
fn bind_closure_args(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Value],
    closure_env: &Env,
    span: Span,
) -> Result<Env, EvalError> {
    let min_args = params.len();
    if rest_param.is_some() {
        if args.len() < min_args {
            return Err(EvalError::WrongArgCount {
                expected: format!("at least {min_args}"),
                got: args.len(),
                span,
            });
        }
    } else if args.len() != min_args {
        return Err(EvalError::WrongArgCount {
            expected: min_args.to_string(),
            got: args.len(),
            span,
        });
    }
    let local_env = Env::with_parent(closure_env);
    for (param, arg) in params.iter().zip(args.iter()) {
        local_env.define(param.clone(), arg.clone());
    }
    if let Some(rest) = rest_param {
        let rest_vals = args[min_args..].to_vec();
        local_env.define(rest.clone(), Value::list(rest_vals));
    }
    Ok(local_env)
}

/// Evaluate a sequence of expressions with continuation replay support.
pub(crate) fn eval_body(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut current_exprs = exprs.to_vec();

    'replay: loop {
        env.push_body_frame(current_exprs.clone());

        let mut last = Value::Void;
        let mut i = 0;
        while i < current_exprs.len() {
            env.set_body_index(i);
            match eval(&current_exprs[i], env) {
                Ok(val) => {
                    last = val;
                    i += 1;
                }
                Err(EvalError::ContinuationReturn { id, value }) => {
                    if env.is_callcc_active(id) {
                        // The call/cc handler is still on the stack — propagate
                        env.pop_body_frame();
                        return Err(EvalError::ContinuationReturn { id, value });
                    }
                    // call/cc has returned — try replay at this level
                    if let Some(replay) = env.get_replay_for_env(id, env) {
                        env.pop_body_frame();
                        env.set_pending_return(value);
                        current_exprs = replay.exprs;
                        continue 'replay;
                    }
                    // No replay match — absorb and continue with remaining exprs
                    last = value;
                    i += 1;
                }
                Err(e) => {
                    env.pop_body_frame();
                    return Err(e);
                }
            }
        }

        env.pop_body_frame();
        return Ok(last);
    }
}

/// Handle call/cc: capture continuation and call the provided function with it.
fn handle_callcc(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }

    // During replay, short-circuit: return the pending value
    if let Some(value) = env.take_pending_return() {
        return Ok(value);
    }

    // Capture continuation
    let id = env.capture_continuation();
    let cont_val = Value::Continuation(id);

    // Mark this call/cc as active on the stack
    env.mark_callcc_active(id);

    // Call the function with the continuation
    let func = &args[0];
    let result = match func {
        Value::Closure {
            ref params,
            ref rest_param,
            ref body,
            env: ref closure_env,
        } => {
            let local_env = bind_closure_args(params, rest_param, &[cont_val], closure_env, span)?;
            match eval(body, &local_env) {
                Ok(val) => Ok(val),
                Err(EvalError::ContinuationReturn { id: ret_id, value }) if ret_id == id => {
                    Ok(value)
                }
                Err(e) => Err(e),
            }
        }
        other => Err(EvalError::TypeMismatch {
            expected: "procedure".to_string(),
            got: other.to_string(),
            span: other.span(),
        }),
    };

    env.unmark_callcc_active(id);
    result
}

pub fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match &current_expr {
            Value::Integer(_, _)
            | Value::Rational(_, _, _)
            | Value::Float(_, _)
            | Value::Boolean(_, _)
            | Value::String(_, _, _)
            | Value::Char(_, _)
            | Value::Vector(_, _)
            | Value::Closure { .. }
            | Value::Continuation(_)
            | Value::Macro(_)
            | Value::Values(_)
            | Value::Record { .. }
            | Value::RecordProcedure(_) => return Ok(current_expr),
            Value::Symbol(name, span) => {
                if let Some(val) = current_env.get(name) {
                    return Ok(val);
                }
                return match name.as_str() {
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                    | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                    | "append" | "pair?" | "string?" | "number?" | "boolean?"
                    | "symbol?" | "integer?" | "rational?" | "zero?" | "positive?" | "negative?"
                    | "even?" | "odd?" | "abs" | "min" | "max" | "modulo"
                    | "remainder" | "quotient"
                    | "exact?" | "inexact?" | "exact->inexact" | "inexact->exact"
                    | "numerator" | "denominator"
                    | "display" | "write" | "newline"
                    | "string-append" | "string-length" | "substring"
                    | "string->number" | "number->string"
                    | "symbol->string" | "string->symbol"
                    | "string-ref" | "string-set!" | "string-copy"
                    | "string->list" | "list->string"
                    | "char->integer" | "integer->char"
                    | "char?" | "char-alphabetic?" | "char-numeric?"
                    | "char-upcase" | "char-downcase" | "char=?" | "char<?"
                    | "string=?" | "string<?" | "string-ci=?"
                    | "string-upcase" | "string-downcase"
                    | "expt" | "list-ref" | "list-tail" | "list?"
                    | "assoc" | "map" | "eq?" | "eqv?" | "equal?"
                    | "vector" | "make-vector" | "vector-ref" | "vector-set!"
                    | "vector-length" | "vector?" | "vector->list" | "list->vector"
                    | "apply" | "dynamic-wind" | "reverse"
                    | "call/cc" | "call-with-current-continuation"
                    | "raise" | "with-exception-handler"
                    | "values" | "call-with-values" => Ok(current_expr.clone()),
                    _ => Err(EvalError::UnboundVariable {
                        name: name.clone(),
                        span: *span,
                    }),
                };
            }
            Value::List(elems, span) => {
                let list_span = *span;
                if elems.is_empty() {
                    return Err(EvalError::Parse {
                        message: "empty application".to_string(),
                        span: list_span,
                    });
                }

                let head = &elems[0];

                // Check for special forms — handle tail positions inline
                if let Value::Symbol(name, _) = head {
                    match name.as_str() {
                        "and" => {
                            let exprs = &elems[1..];
                            if exprs.is_empty() { return Ok(Value::bool(true)); }
                            if let Some(v) = eval_and_prefix(&exprs[..exprs.len() - 1], &current_env)? {
                                return Ok(v);
                            }
                            current_expr = exprs[exprs.len() - 1].clone();
                            continue;
                        }
                        "or" => {
                            let exprs = &elems[1..];
                            if exprs.is_empty() { return Ok(Value::bool(false)); }
                            if let Some(v) = eval_or_prefix(&exprs[..exprs.len() - 1], &current_env)? {
                                return Ok(v);
                            }
                            current_expr = exprs[exprs.len() - 1].clone();
                            continue;
                        }
                        "not" => return eval_not(&elems[1..], list_span, &current_env),
                        "if" => {
                            let args = &elems[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "2 or 3".to_string(),
                                    got: args.len(),
                                    span: list_span,
                                });
                            }
                            let cond = eval(&args[0], &current_env)?;
                            if cond.is_truthy() {
                                current_expr = args[1].clone();
                                continue;
                            } else if args.len() == 3 {
                                current_expr = args[2].clone();
                                continue;
                            } else {
                                return Ok(Value::Void);
                            }
                        }
                        "define" => return eval_define(&elems[1..], list_span, &current_env),
                        "set!" => {
                            let args = &elems[1..];
                            if args.len() != 2 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "2".to_string(),
                                    got: args.len(),
                                    span: list_span,
                                });
                            }
                            let Value::Symbol(name, sym_span) = &args[0] else {
                                return Err(EvalError::TypeMismatch {
                                    expected: "symbol".to_string(),
                                    got: args[0].to_string(),
                                    span: args[0].span(),
                                });
                            };
                            let val = eval(&args[1], &current_env)?;
                            if !current_env.set(name, val) {
                                return Err(EvalError::UnboundVariable {
                                    name: name.clone(),
                                    span: *sym_span,
                                });
                            }
                            return Ok(Value::Void);
                        }
                        "quote" => return eval_quote(&elems[1..], list_span),
                        "lambda" => return eval_lambda(&elems[1..], list_span, &current_env),
                        "let" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "at least 2".to_string(),
                                    got: args.len(),
                                    span: list_span,
                                });
                            }
                            // Named let
                            if let Value::Symbol(loop_name, _) = &args[0] {
                                let (new_env, body) = setup_named_let(loop_name, &args[1..], list_span, &current_env)?;
                                current_expr = body;
                                current_env = new_env;
                                continue;
                            }
                            // Regular let
                            let new_env = setup_let(&args[0], list_span, &current_env)?;
                            let body = &args[1..];
                            if body.len() == 1 {
                                current_expr = body[0].clone();
                                current_env = new_env;
                                continue;
                            }
                            return eval_body(body, &new_env);
                        }
                        "let*" => {
                            match eval_let_star(&elems[1..], list_span, &current_env)? {
                                TailAction::Result(v) => return Ok(v),
                                TailAction::TailCall(expr, env) => {
                                    current_expr = expr;
                                    current_env = env;
                                    continue;
                                }
                            }
                        }
                        "letrec" => {
                            match eval_letrec(&elems[1..], list_span, &current_env)? {
                                TailAction::Result(v) => return Ok(v),
                                TailAction::TailCall(expr, env) => {
                                    current_expr = expr;
                                    current_env = env;
                                    continue;
                                }
                            }
                        }
                        "letrec*" => {
                            match eval_letrec_star(&elems[1..], list_span, &current_env)? {
                                TailAction::Result(v) => return Ok(v),
                                TailAction::TailCall(expr, env) => {
                                    current_expr = expr;
                                    current_env = env;
                                    continue;
                                }
                            }
                        }
                        "case" => {
                            match eval_case_tail(&elems[1..], list_span, &current_env)? {
                                TailAction::Result(v) => return Ok(v),
                                TailAction::TailCall(expr, env) => {
                                    current_expr = expr;
                                    current_env = env;
                                    continue;
                                }
                            }
                        }
                        "do" => {
                            return eval_do(&elems[1..], list_span, &current_env);
                        }
                        "begin" => {
                            let exprs = &elems[1..];
                            if exprs.is_empty() {
                                return Ok(Value::Void);
                            }
                            for expr in &exprs[..exprs.len() - 1] {
                                eval(expr, &current_env)?;
                            }
                            current_expr = exprs[exprs.len() - 1].clone();
                            continue;
                        }
                        "cond" => {
                            match eval_cond_tail(&elems[1..], &current_env)? {
                                TailAction::Result(v) => return Ok(v),
                                TailAction::TailCall(expr, env) => {
                                    current_expr = expr;
                                    current_env = env;
                                    continue;
                                }
                            }
                        }
                        "define-syntax" => {
                            return eval_define_syntax(&elems[1..], list_span, &current_env);
                        }
                        "define-record-type" => {
                            return eval_define_record_type(&elems[1..], list_span, &current_env);
                        }
                        "guard" => {
                            return eval_guard(&elems[1..], list_span, &current_env);
                        }
                        _ => {
                            // Check for macro application
                            if let Some(Value::Macro(ref sr)) = current_env.get(name) {
                                current_expr = macros::expand_macro(sr, elems)?;
                                continue;
                            }
                        }
                    }
                }

                // Evaluate all elements (function application)
                let op = eval(head, &current_env)?;
                let args: Vec<Value> = elems[1..]
                    .iter()
                    .map(|e| eval(e, &current_env))
                    .collect::<Result<_, _>>()?;

                // Handle apply: (apply fn prefix... arg-list)
                let (func, call_args) = if matches!(op, Value::Symbol(ref name, _) if name == "apply") {
                    let (func, assembled) = assemble_apply_args(&args, list_span)?;
                    (func, assembled)
                } else {
                    (op, args)
                };

                // Function application (including call/cc and continuations)
                match dispatch_call(func, call_args, list_span, &current_env)? {
                    TailAction::Result(v) => return Ok(v),
                    TailAction::TailCall(expr, env) => {
                        current_expr = expr;
                        current_env = env;
                        continue;
                    }
                }
            }
            Value::Void => return Ok(Value::Void),
        }
    }
}

enum TailAction {
    Result(Value),
    TailCall(Value, Env),
}

/// Set up named let: returns (env, body_expr) for tail-call loop
fn setup_named_let(name: &str, args: &[Value], form_span: Span, env: &Env) -> Result<(Env, Value), EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: 1,
            span: form_span,
        });
    }
    let Value::List(bindings, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for binding in bindings {
        let Value::List(pair, _) = binding else {
            return Err(EvalError::TypeMismatch {
                expected: "binding pair".to_string(),
                got: binding.to_string(),
                span: binding.span(),
            });
        };
        if pair.len() != 2 {
            return Err(EvalError::WrongArgCount {
                expected: "2".to_string(),
                got: pair.len(),
                span: binding.span(),
            });
        }
        let Value::Symbol(var, _) = &pair[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: pair[0].to_string(),
                span: pair[0].span(),
            });
        };
        params.push(var.clone());
        inits.push(eval(&pair[1], env)?);
    }
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::list(
            std::iter::once(Value::symbol("begin".to_string()))
                .chain(args[1..].iter().cloned())
                .collect(),
        )
    };
    let local_env = Env::with_parent(env);
    let closure = Value::Closure {
        params: params.clone(),
        rest_param: None,
        body: Box::new(body.clone()),
        env: local_env.clone(),
    };
    local_env.define(name.to_string(), closure);
    for (param, init) in params.iter().zip(inits.iter()) {
        local_env.define(param.clone(), init.clone());
    }
    Ok((local_env, body))
}

/// Set up regular let bindings, return the new env
fn setup_let(bindings_expr: &Value, _form_span: Span, env: &Env) -> Result<Env, EvalError> {
    let Value::List(bindings, _) = bindings_expr else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: bindings_expr.to_string(),
            span: bindings_expr.span(),
        });
    };
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair, _) = binding else {
            return Err(EvalError::TypeMismatch {
                expected: "binding pair".to_string(),
                got: binding.to_string(),
                span: binding.span(),
            });
        };
        if pair.len() != 2 {
            return Err(EvalError::WrongArgCount {
                expected: "2".to_string(),
                got: pair.len(),
                span: binding.span(),
            });
        }
        let Value::Symbol(var, _) = &pair[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: pair[0].to_string(),
                span: pair[0].span(),
            });
        };
        let val = eval(&pair[1], env)?;
        local_env.define(var.clone(), val);
    }
    Ok(local_env)
}

/// Evaluate cond, returning either a result or a tail-call action for the last expression
fn eval_cond_tail(clauses: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    for clause in clauses {
        let Value::List(parts, _) = clause else {
            return Err(EvalError::TypeMismatch {
                expected: "cond clause".to_string(),
                got: clause.to_string(),
                span: clause.span(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "empty cond clause".to_string(),
                span: clause.span(),
            });
        }
        // else clause
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                for expr in &parts[1..parts.len() - 1] {
                    eval(expr, env)?;
                }
                if parts.len() > 1 {
                    return Ok(TailAction::TailCall(parts[parts.len() - 1].clone(), env.clone()));
                }
                return Ok(TailAction::Result(Value::Void));
            }
        }
        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(TailAction::Result(test));
            }
            for expr in &parts[1..parts.len() - 1] {
                eval(expr, env)?;
            }
            return Ok(TailAction::TailCall(parts[parts.len() - 1].clone(), env.clone()));
        }
    }
    Ok(TailAction::Result(Value::Void))
}

/// Parse a single binding pair `(var init)` from a let/letrec form.
fn parse_binding(binding: &Value) -> Result<(&str, &Value), EvalError> {
    let Value::List(pair, _) = binding else {
        return Err(EvalError::TypeMismatch {
            expected: "binding pair".to_string(),
            got: binding.to_string(),
            span: binding.span(),
        });
    };
    if pair.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: pair.len(),
            span: binding.span(),
        });
    }
    let Value::Symbol(var, _) = &pair[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: pair[0].to_string(),
            span: pair[0].span(),
        });
    };
    Ok((var.as_str(), &pair[1]))
}

/// Require at least 2 args and extract the binding list from the first arg.
fn require_let_args(args: &[Value], form_span: Span) -> Result<&[Value], EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let Value::List(bindings, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    Ok(bindings)
}

/// Evaluate `let*` form, returning a tail action for the body.
fn eval_let_star(args: &[Value], form_span: Span, env: &Env) -> Result<TailAction, EvalError> {
    let bindings = require_let_args(args, form_span)?;
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let (var, init) = parse_binding(binding)?;
        let val = eval(init, &local_env)?;
        local_env.define(var.to_string(), val);
    }
    let body = &args[1..];
    if body.len() == 1 {
        return Ok(TailAction::TailCall(body[0].clone(), local_env));
    }
    Ok(TailAction::Result(eval_body(body, &local_env)?))
}

/// Evaluate `letrec` form, returning a tail action for the body.
fn eval_letrec(args: &[Value], form_span: Span, env: &Env) -> Result<TailAction, EvalError> {
    let bindings = require_let_args(args, form_span)?;
    let local_env = Env::with_parent(env);
    let mut var_names = Vec::new();
    let mut init_exprs = Vec::new();
    for binding in bindings {
        let (var, init) = parse_binding(binding)?;
        var_names.push(var.to_string());
        init_exprs.push(init.clone());
        local_env.define(var.to_string(), Value::Void);
    }
    for (name, init) in var_names.iter().zip(init_exprs.iter()) {
        let val = eval(init, &local_env)?;
        local_env.set(name, val);
    }
    let body = &args[1..];
    if body.len() == 1 {
        return Ok(TailAction::TailCall(body[0].clone(), local_env));
    }
    Ok(TailAction::Result(eval_body(body, &local_env)?))
}

/// Evaluate `letrec*` form, returning a tail action for the body.
fn eval_letrec_star(args: &[Value], form_span: Span, env: &Env) -> Result<TailAction, EvalError> {
    let bindings = require_let_args(args, form_span)?;
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let (var, init) = parse_binding(binding)?;
        let val = eval(init, &local_env)?;
        local_env.define(var.to_string(), val);
    }
    let body = &args[1..];
    if body.len() == 1 {
        return Ok(TailAction::TailCall(body[0].clone(), local_env));
    }
    Ok(TailAction::Result(eval_body(body, &local_env)?))
}

/// Evaluate `case` form, returning a tail action.
fn eval_case_tail(args: &[Value], form_span: Span, env: &Env) -> Result<TailAction, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        });
    }
    let key = eval(&args[0], env)?;
    for clause in &args[1..] {
        let Value::List(parts, _) = clause else {
            return Err(EvalError::TypeMismatch {
                expected: "case clause".to_string(),
                got: clause.to_string(),
                span: clause.span(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "empty case clause".to_string(),
                span: clause.span(),
            });
        }
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                for expr in &parts[1..parts.len() - 1] {
                    eval(expr, env)?;
                }
                if parts.len() > 1 {
                    return Ok(TailAction::TailCall(parts[parts.len() - 1].clone(), env.clone()));
                }
                return Ok(TailAction::Result(Value::Void));
            }
        }
        let Value::List(datums, _) = &parts[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "datum list".to_string(),
                got: parts[0].to_string(),
                span: parts[0].span(),
            });
        };
        let matched = datums.iter().any(|d| eqv_match(&key, d));
        if matched {
            for expr in &parts[1..parts.len() - 1] {
                eval(expr, env)?;
            }
            if parts.len() > 1 {
                return Ok(TailAction::TailCall(parts[parts.len() - 1].clone(), env.clone()));
            }
            return Ok(TailAction::Result(Value::Void));
        }
    }
    Ok(TailAction::Result(Value::Void))
}

/// Assemble args for `(apply fn prefix... arg-list)`.
fn assemble_apply_args(args: &[Value], span: Span) -> Result<(Value, Vec<Value>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span,
        });
    }
    let func = args[0].clone();
    let last = &args[args.len() - 1];
    let Value::List(tail_list, _) = last else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: last.to_string(),
            span: last.span(),
        });
    };
    let mut full_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    full_args.extend(tail_list.iter().cloned());
    Ok((func, full_args))
}

/// Dispatch a function call, returning a tail action for closures.
fn dispatch_call(func: Value, args: Vec<Value>, span: Span, env: &Env) -> Result<TailAction, EvalError> {
    match func {
        Value::Symbol(ref name, _) if name == "call/cc" || name == "call-with-current-continuation" => {
            Ok(TailAction::Result(handle_callcc(&args, span, env)?))
        }
        Value::Symbol(ref name, _) => Ok(TailAction::Result(apply_builtin(name, &args, span, env)?)),
        Value::Closure {
            ref params,
            ref rest_param,
            ref body,
            env: ref closure_env,
        } => {
            let local_env = bind_closure_args(params, rest_param, &args, closure_env, span)?;
            Ok(TailAction::TailCall(*body.clone(), local_env))
        }
        Value::RecordProcedure(ref op) => {
            Ok(TailAction::Result(apply_record_op(op, args, span)?))
        }
        Value::Continuation(id) => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".to_string(),
                    got: args.len(),
                    span,
                });
            }
            Err(EvalError::ContinuationReturn { id, value: args.into_iter().next().expect("checked len") })
        }
        other => Err(EvalError::NotAProcedure {
            value: other.to_string(),
            span: other.span(),
        }),
    }
}

fn eqv_match(key: &Value, datum: &Value) -> bool {
    match (key, datum) {
        (Value::Integer(a, _), Value::Integer(b, _)) => a == b,
        (Value::Boolean(a, _), Value::Boolean(b, _)) => a == b,
        (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
        (Value::Char(a, _), Value::Char(b, _)) => a == b,
        _ => false,
    }
}

fn eval_do(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let Value::List(var_specs, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "variable specs".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let Value::List(test_clause, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "test clause".to_string(),
            got: args[1].to_string(),
            span: args[1].span(),
        });
    };
    if test_clause.is_empty() {
        return Err(EvalError::Parse {
            message: "empty do test clause".to_string(),
            span: args[1].span(),
        });
    }
    let body = &args[2..];

    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Value>> = Vec::new();

    let do_env = Env::with_parent(env);
    for spec in var_specs {
        let Value::List(parts, _) = spec else {
            return Err(EvalError::TypeMismatch {
                expected: "variable spec".to_string(),
                got: spec.to_string(),
                span: spec.span(),
            });
        };
        if parts.len() < 2 || parts.len() > 3 {
            return Err(EvalError::WrongArgCount {
                expected: "2 or 3".to_string(),
                got: parts.len(),
                span: spec.span(),
            });
        }
        let Value::Symbol(var, _) = &parts[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: parts[0].to_string(),
                span: parts[0].span(),
            });
        };
        let init = eval(&parts[1], env)?;
        do_env.define(var.clone(), init);
        var_names.push(var.clone());
        if parts.len() == 3 {
            step_exprs.push(Some(parts[2].clone()));
        } else {
            step_exprs.push(None);
        }
    }

    loop {
        let test_result = eval(&test_clause[0], &do_env)?;
        if test_result.is_truthy() {
            let result_exprs = &test_clause[1..];
            if result_exprs.is_empty() {
                return Ok(Value::Void);
            }
            let mut last = Value::Void;
            for expr in result_exprs {
                last = eval(expr, &do_env)?;
            }
            return Ok(last);
        }

        for expr in body {
            eval(expr, &do_env)?;
        }

        let new_vals: Vec<Option<Value>> = step_exprs
            .iter()
            .map(|step| match step {
                Some(expr) => eval(expr, &do_env).map(Some),
                None => Ok(None),
            })
            .collect::<Result<_, _>>()?;

        for (name, new_val) in var_names.iter().zip(new_vals.into_iter()) {
            if let Some(val) = new_val {
                do_env.set(name, val);
            }
        }
    }
}

fn apply_builtin(name: &str, args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    match name {
        "+" => arith_add(args),
        "-" => arith_sub(args, span),
        "*" => arith_mul(args),
        "/" => arith_div(args, span),
        "<" => cmp_lt(args),
        ">" => cmp_gt(args),
        "=" => cmp_eq(args),
        "<=" => cmp_le(args),
        ">=" => cmp_ge(args),
        "cons" => builtin_cons(args, span),
        "car" => builtin_car(args, span),
        "cdr" => builtin_cdr(args, span),
        "null?" => builtin_null(args, span),
        "list" => builtin_list(args),
        "length" => builtin_length(args, span),
        "append" => builtin_append(args),
        "pair?" => builtin_pair(args, span),
        "string?" => Ok(Value::bool(matches!(args, [Value::String(_, _, _)]))),
        "number?" => Ok(Value::bool(matches!(args, [Value::Integer(_, _)] | [Value::Rational(_, _, _)] | [Value::Float(_, _)]))),
        "integer?" => match args {
            [Value::Integer(_, _)] => Ok(Value::bool(true)),
            [Value::Rational(n, d, _)] => Ok(Value::bool(*n % *d == 0)),
            [Value::Float(_, _)] => Ok(Value::bool(false)),
            _ => Ok(Value::bool(false)),
        },
        "rational?" => Ok(Value::bool(matches!(args, [Value::Integer(_, _)] | [Value::Rational(_, _, _)]))),
        "exact?" => Ok(Value::bool(matches!(args, [Value::Integer(_, _)] | [Value::Rational(_, _, _)]))),
        "inexact?" => Ok(Value::bool(matches!(args, [Value::Float(_, _)]))),
        "exact->inexact" => builtin_exact_to_inexact(args, span),
        "inexact->exact" => builtin_inexact_to_exact(args, span),
        "numerator" => builtin_numerator(args, span),
        "denominator" => builtin_denominator(args, span),
        "boolean?" => Ok(Value::bool(matches!(args, [Value::Boolean(_, _)]))),
        "symbol?" => Ok(Value::bool(matches!(args, [Value::Symbol(_, _)]))),
        "zero?" => match args {
            [v] => {
                let n = value_to_num(v).ok_or_else(|| EvalError::TypeMismatch {
                    expected: "number".to_string(),
                    got: format!("{v}"),
                    span: v.span(),
                })?;
                Ok(Value::bool(n.to_f64() == 0.0))
            }
            _ => Err(EvalError::WrongArgCount {
                expected: "1".to_string(),
                got: args.len(),
                span,
            }),
        },
        "positive?" => match args {
            [v] => {
                let n = value_to_num(v).ok_or_else(|| EvalError::TypeMismatch {
                    expected: "number".to_string(),
                    got: format!("{v}"),
                    span: v.span(),
                })?;
                Ok(Value::bool(n.to_f64() > 0.0))
            }
            _ => Err(EvalError::WrongArgCount {
                expected: "1".to_string(),
                got: args.len(),
                span,
            }),
        },
        "negative?" => match args {
            [v] => {
                let n = value_to_num(v).ok_or_else(|| EvalError::TypeMismatch {
                    expected: "number".to_string(),
                    got: format!("{v}"),
                    span: v.span(),
                })?;
                Ok(Value::bool(n.to_f64() < 0.0))
            }
            _ => Err(EvalError::WrongArgCount {
                expected: "1".to_string(),
                got: args.len(),
                span,
            }),
        },
        "even?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n % 2 == 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "odd?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n % 2 != 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "abs" => match args {
            [Value::Integer(n, _)] => Ok(Value::int(n.abs())),
            [Value::Rational(n, d, _)] => Ok(make_rational(n.abs(), *d, Span::default())),
            [Value::Float(f, _)] => Ok(Value::float(f.abs())),
            _ => Err(EvalError::TypeMismatch {
                expected: "number".to_string(),
                got: args.first().map_or("nothing".to_string(), |v| format!("{v}")),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "min" => arith_min(args, span),
        "max" => arith_max(args, span),
        "modulo" => arith_modulo(args, span),
        "remainder" => arith_remainder(args, span),
        "quotient" => arith_quotient(args, span),
        "display" => builtin_display(args, span, env),
        "write" => builtin_write(args, span, env),
        "newline" => builtin_newline(args, span, env),
        "string-append" => builtin_string_append(args),
        "string-length" => builtin_string_length(args, span),
        "substring" => builtin_substring(args, span),
        "string->number" => builtin_string_to_number(args, span),
        "number->string" => builtin_number_to_string(args, span),
        "symbol->string" => builtin_symbol_to_string(args, span),
        "string->symbol" => builtin_string_to_symbol(args, span),
        "string-ref" => builtin_string_ref(args, span),
        "string-set!" => builtin_string_set(args, span),
        "string-copy" => builtin_string_copy(args, span),
        "string->list" => builtin_string_to_list(args, span),
        "list->string" => builtin_list_to_string(args, span),
        "char->integer" => builtin_char_to_integer(args, span),
        "integer->char" => builtin_integer_to_char(args, span),
        "char?" => Ok(Value::bool(matches!(args, [Value::Char(_, _)]))),
        "expt" => builtin_expt(args, span),
        "list-ref" => builtin_list_ref(args, span),
        "list-tail" => builtin_list_tail(args, span),
        "list?" => builtin_list_pred(args, span),
        "assoc" => builtin_assoc(args, span),
        "map" => builtin_map(args, span, env),
        "eq?" => builtin_eq(args, span),
        "eqv?" => builtin_eqv(args, span),
        "equal?" => builtin_equal(args, span),
        "vector" => Ok(Value::vector(args.to_vec())),
        "make-vector" => builtin_make_vector(args, span),
        "vector-ref" => builtin_vector_ref(args, span),
        "vector-set!" => builtin_vector_set(args, span),
        "vector-length" => builtin_vector_length(args, span),
        "vector?" => Ok(Value::bool(matches!(args, [Value::Vector(_, _)]))),
        "vector->list" => builtin_vector_to_list(args, span),
        "list->vector" => builtin_list_to_vector(args, span),
        "char-alphabetic?" => builtin_char_alphabetic(args, span),
        "char-numeric?" => builtin_char_numeric(args, span),
        "char-upcase" => builtin_char_upcase(args, span),
        "char-downcase" => builtin_char_downcase(args, span),
        "char=?" => builtin_char_eq(args, span),
        "char<?" => builtin_char_lt(args, span),
        "string=?" => builtin_string_eq(args, span),
        "string<?" => builtin_string_lt(args, span),
        "string-ci=?" => builtin_string_ci_eq(args, span),
        "string-upcase" => builtin_string_upcase(args, span),
        "string-downcase" => builtin_string_downcase(args, span),
        "dynamic-wind" => builtin_dynamic_wind(args, span, env),
        "reverse" => builtin_reverse(args, span),
        "raise" => builtin_raise(args, span),
        "with-exception-handler" => builtin_with_exception_handler(args, span, env),
        "values" => builtin_values(args),
        "call-with-values" => builtin_call_with_values(args, span, env),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}


/// Call a zero-argument thunk (closure).
fn call_thunk(thunk: &Value, span: Span, _env: &Env) -> Result<Value, EvalError> {
    match thunk {
        Value::Closure {
            ref params,
            ref rest_param,
            ref body,
            env: ref closure_env,
        } => {
            let local_env = bind_closure_args(params, rest_param, &[], closure_env, span)?;
            eval(body, &local_env)
        }
        other => Err(EvalError::TypeMismatch {
            expected: "procedure".to_string(),
            got: other.to_string(),
            span: other.span(),
        }),
    }
}

fn builtin_values(args: &[Value]) -> Result<Value, EvalError> {
    match args.len() {
        1 => Ok(args[0].clone()),
        _ => Ok(Value::Values(args.to_vec())),
    }
}

fn builtin_call_with_values(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let producer = &args[0];
    let consumer = &args[1];
    let produced = call_thunk(producer, span, env)?;
    let consumer_args = match produced {
        Value::Values(vals) => vals,
        single => vec![single],
    };
    call_with_args(consumer, &consumer_args, span, env)
}

fn call_with_args(func: &Value, args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    match func {
        Value::Closure {
            ref params,
            ref rest_param,
            ref body,
            env: ref closure_env,
        } => {
            let local_env = bind_closure_args(params, rest_param, args, closure_env, span)?;
            eval(body, &local_env)
        }
        Value::Symbol(ref name, _) => apply_builtin(name, args, span, env),
        Value::Continuation(id) => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".to_string(),
                    got: args.len(),
                    span,
                });
            }
            Err(EvalError::ContinuationReturn { id: *id, value: args[0].clone() })
        }
        other => Err(EvalError::NotAProcedure {
            value: other.to_string(),
            span: other.span(),
        }),
    }
}

fn builtin_reverse(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::List(elems, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let reversed: Vec<Value> = elems.iter().rev().cloned().collect();
    Ok(Value::list(reversed))
}

fn builtin_dynamic_wind(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".to_string(),
            got: args.len(),
            span,
        });
    }
    let in_thunk = &args[0];
    let body_thunk = &args[1];
    let out_thunk = &args[2];

    call_thunk(in_thunk, span, env)?;
    let body_result = call_thunk(body_thunk, span, env);
    call_thunk(out_thunk, span, env)?;
    body_result
}

fn eval_define(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name, _) => {
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body)
        Value::List(elems, _) => {
            if elems.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty define function name".to_string(),
                    span: form_span,
                });
            }
            let Value::Symbol(name, _) = &elems[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: elems[0].to_string(),
                    span: elems[0].span(),
                });
            };
            let (params, rest_param) = parse_params(&elems[1..])?;
            let body = if args.len() == 2 {
                args[1].clone()
            } else {
                Value::list(
                    std::iter::once(Value::symbol("begin".to_string()))
                        .chain(args[1..].iter().cloned())
                        .collect(),
                )
            };
            let closure = Value::Closure {
                params,
                rest_param,
                body: Box::new(body),
                env: env.clone(),
            };
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        other => Err(EvalError::TypeMismatch {
            expected: "symbol or list".to_string(),
            got: other.to_string(),
            span: other.span(),
        }),
    }
}

fn eval_define_record_type(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field-names...) predicate (field accessor)...)
    if args.len() < 3 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 3".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let Value::Symbol(type_name, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    // Constructor: (make-foo field1 field2 ...)
    let Value::List(ctor_elems, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "constructor spec".to_string(),
            got: args[1].to_string(),
            span: args[1].span(),
        });
    };
    if ctor_elems.is_empty() {
        return Err(EvalError::Parse {
            message: "empty constructor spec".to_string(),
            span: form_span,
        });
    }
    let Value::Symbol(ctor_name, _) = &ctor_elems[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: ctor_elems[0].to_string(),
            span: ctor_elems[0].span(),
        });
    };
    let ctor_fields: Vec<String> = ctor_elems[1..]
        .iter()
        .map(|v| match v {
            Value::Symbol(s, _) => Ok(s.clone()),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: other.to_string(),
                span: other.span(),
            }),
        })
        .collect::<Result<_, _>>()?;

    // Predicate
    let Value::Symbol(pred_name, _) = &args[2] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: args[2].to_string(),
            span: args[2].span(),
        });
    };

    // Generate a unique type ID
    let type_id = env.capture_continuation(); // reuse the ID counter

    // Field accessors: (field-name accessor-name) ...
    let field_specs = &args[3..];
    // Build a mapping: field-name -> index (based on constructor field order)
    let field_index_map: std::collections::HashMap<&str, usize> = ctor_fields
        .iter()
        .enumerate()
        .map(|(i, name)| (name.as_str(), i))
        .collect();

    // Define constructor
    env.define(
        ctor_name.clone(),
        Value::RecordProcedure(RecordOp::Constructor {
            type_id,
            type_name: type_name.clone(),
            field_count: ctor_fields.len(),
        }),
    );

    // Define predicate
    env.define(
        pred_name.clone(),
        Value::RecordProcedure(RecordOp::Predicate { type_id }),
    );

    // Define accessors
    for spec in field_specs {
        let Value::List(spec_elems, _) = spec else {
            return Err(EvalError::TypeMismatch {
                expected: "field spec (name accessor)".to_string(),
                got: spec.to_string(),
                span: spec.span(),
            });
        };
        if spec_elems.len() < 2 {
            return Err(EvalError::WrongArgCount {
                expected: "at least 2".to_string(),
                got: spec_elems.len(),
                span: spec.span(),
            });
        }
        let Value::Symbol(field_name, _) = &spec_elems[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: spec_elems[0].to_string(),
                span: spec_elems[0].span(),
            });
        };
        let Value::Symbol(accessor_name, _) = &spec_elems[1] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: spec_elems[1].to_string(),
                span: spec_elems[1].span(),
            });
        };
        let Some(&idx) = field_index_map.get(field_name.as_str()) else {
            return Err(EvalError::Parse {
                message: format!("field {field_name} not in constructor"),
                span: spec_elems[0].span(),
            });
        };
        env.define(
            accessor_name.clone(),
            Value::RecordProcedure(RecordOp::Accessor {
                type_id,
                field_index: idx,
                accessor_name: accessor_name.clone(),
            }),
        );
    }

    Ok(Value::Void)
}

fn apply_record_op(op: &RecordOp, args: Vec<Value>, span: Span) -> Result<Value, EvalError> {
    match op {
        RecordOp::Constructor { type_id, type_name, field_count } => {
            if args.len() != *field_count {
                return Err(EvalError::WrongArgCount {
                    expected: field_count.to_string(),
                    got: args.len(),
                    span,
                });
            }
            Ok(Value::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields: args,
            })
        }
        RecordOp::Predicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".to_string(),
                    got: args.len(),
                    span,
                });
            }
            let is_match = matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id);
            Ok(Value::bool(is_match))
        }
        RecordOp::Accessor { type_id, field_index, accessor_name } => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".to_string(),
                    got: args.len(),
                    span,
                });
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if tid == type_id => {
                    Ok(fields[*field_index].clone())
                }
                other => Err(EvalError::TypeMismatch {
                    expected: format!("record for accessor {accessor_name}"),
                    got: other.to_string(),
                    span,
                }),
            }
        }
    }
}

fn eval_define_syntax(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let Value::Symbol(name, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let Value::List(sr_elems, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "syntax-rules form".to_string(),
            got: args[1].to_string(),
            span: args[1].span(),
        });
    };
    if sr_elems.is_empty() {
        return Err(EvalError::Parse {
            message: "empty syntax-rules".to_string(),
            span: form_span,
        });
    }
    let Value::Symbol(sr_keyword, _) = &sr_elems[0] else {
        return Err(EvalError::Parse {
            message: "expected syntax-rules".to_string(),
            span: sr_elems[0].span(),
        });
    };
    if sr_keyword != "syntax-rules" {
        return Err(EvalError::Parse {
            message: format!("expected syntax-rules, got {sr_keyword}"),
            span: sr_elems[0].span(),
        });
    }
    let Value::List(literals_list, _) = &sr_elems[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "literals list".to_string(),
            got: sr_elems[1].to_string(),
            span: sr_elems[1].span(),
        });
    };
    let literals: Vec<String> = literals_list
        .iter()
        .filter_map(|v| {
            if let Value::Symbol(s, _) = v {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    let mut rules = Vec::new();
    for rule in &sr_elems[2..] {
        let Value::List(rule_parts, _) = rule else {
            return Err(EvalError::TypeMismatch {
                expected: "syntax rule".to_string(),
                got: rule.to_string(),
                span: rule.span(),
            });
        };
        if rule_parts.len() != 2 {
            return Err(EvalError::WrongArgCount {
                expected: "2".to_string(),
                got: rule_parts.len(),
                span: rule.span(),
            });
        }
        rules.push((rule_parts[0].clone(), rule_parts[1].clone()));
    }
    let syntax_rules = crate::scheme::value::SyntaxRules {
        literals,
        rules,
        def_env: env.clone(),
    };
    env.define(name.clone(), Value::Macro(syntax_rules));
    Ok(Value::Void)
}

fn eval_quote(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    Ok(args[0].clone())
}

fn parse_params(param_list: &[Value]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_list.len() {
        match &param_list[i] {
            Value::Symbol(s, _) if s == "." => {
                if i + 1 >= param_list.len() {
                    return Err(EvalError::Parse {
                        message: "expected parameter after dot".to_string(),
                        span: param_list[i].span(),
                    });
                }
                let Value::Symbol(rest_name, _) = &param_list[i + 1] else {
                    return Err(EvalError::TypeMismatch {
                        expected: "symbol".to_string(),
                        got: param_list[i + 1].to_string(),
                        span: param_list[i + 1].span(),
                    });
                };
                rest_param = Some(rest_name.clone());
                break;
            }
            Value::Symbol(s, _) => {
                params.push(s.clone());
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: other.to_string(),
                    span: other.span(),
                });
            }
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let Value::List(param_list, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "parameter list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let (params, rest_param) = parse_params(param_list)?;
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::list(
            std::iter::once(Value::symbol("begin".to_string()))
                .chain(args[1..].iter().cloned())
                .collect(),
        )
    };
    Ok(Value::Closure {
        params,
        rest_param,
        body: Box::new(body),
        env: env.clone(),
    })
}

fn require_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n, _) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: format!("{other}"),
                span: other.span(),
            }),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let result = nums.iter().copied().fold(Num::Int(0), Num::add);
    Ok(result.to_value())
}

fn arith_sub(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        });
    }
    if nums.len() == 1 {
        return Ok(Num::Int(0).sub(nums[0]).to_value());
    }
    let result = nums[1..].iter().copied().fold(nums[0], Num::sub);
    Ok(result.to_value())
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let result = nums.iter().copied().fold(Num::Int(1), Num::mul);
    Ok(result.to_value())
}

fn arith_div(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        result = result.div(n).ok_or(EvalError::DivisionByZero { span: form_span })?;
    }
    Ok(result.to_value())
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let fs: Vec<f64> = nums.iter().map(|n| n.cmp_f64()).collect();
    Ok(Value::bool(fs.windows(2).all(|w| w[0] < w[1])))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let fs: Vec<f64> = nums.iter().map(|n| n.cmp_f64()).collect();
    Ok(Value::bool(fs.windows(2).all(|w| w[0] > w[1])))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let fs: Vec<f64> = nums.iter().map(|n| n.cmp_f64()).collect();
    Ok(Value::bool(fs.windows(2).all(|w| (w[0] - w[1]).abs() < f64::EPSILON || w[0] == w[1])))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let fs: Vec<f64> = nums.iter().map(|n| n.cmp_f64()).collect();
    Ok(Value::bool(fs.windows(2).all(|w| w[0] <= w[1])))
}


fn eval_not(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::bool(!val.is_truthy()))
}


fn cmp_ge(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_nums(args)?;
    let fs: Vec<f64> = nums.iter().map(|n| n.cmp_f64()).collect();
    Ok(Value::bool(fs.windows(2).all(|w| w[0] >= w[1])))
}

fn builtin_cons(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[1] {
        Value::List(elems, _) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(elems.iter().cloned());
            Ok(Value::list(new_list))
        }
        _ => {
            // Improper pair — store as 2-element tagged structure for now
            Ok(Value::list(vec![
                args[0].clone(),
                Value::symbol(".".to_string()),
                args[1].clone(),
            ]))
        }
    }
}

fn builtin_car(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        }),
    }
}

fn builtin_cdr(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        Value::List(elems, _) if !elems.is_empty() => {
            // Improper pair (a . b) stored as [a, ".", b] — cdr returns b
            if elems.len() == 3 {
                if let Value::Symbol(s, _) = &elems[1] {
                    if s == "." {
                        return Ok(elems[2].clone());
                    }
                }
            }
            Ok(Value::list(elems[1..].to_vec()))
        }
        _ => Err(EvalError::TypeMismatch {
            expected: "pair".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        }),
    }
}

fn builtin_null(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    Ok(Value::bool(matches!(
        &args[0],
        Value::List(elems, _) if elems.is_empty()
    )))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::list(args.to_vec()))
}

fn builtin_length(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        Value::List(elems, _) => Ok(Value::int(elems.len() as i64)),
        _ => Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        }),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        if i == args.len() - 1 {
            // Last argument can be any value (for improper lists), but for proper lists:
            match arg {
                Value::List(elems, _) => result.extend(elems.iter().cloned()),
                other => {
                    if result.is_empty() {
                        return Ok(other.clone());
                    }
                    result.push(other.clone());
                }
            }
        } else {
            match arg {
                Value::List(elems, _) => result.extend(elems.iter().cloned()),
                _ => {
                    return Err(EvalError::TypeMismatch {
                        expected: "list".to_string(),
                        got: arg.to_string(),
                        span: arg.span(),
                    });
                }
            }
        }
    }
    Ok(Value::list(result))
}

fn builtin_pair(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    Ok(Value::bool(matches!(
        &args[0],
        Value::List(elems, _) if !elems.is_empty()
    )))
}

fn arith_min(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    nums.iter()
        .copied()
        .min()
        .map(Value::int)
        .ok_or(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        })
}

fn arith_max(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    nums.iter()
        .copied()
        .max()
        .map(Value::int)
        .ok_or(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        })
}

fn arith_modulo(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero { span: form_span });
    }
    Ok(Value::int(((nums[0] % nums[1]) + nums[1]) % nums[1]))
}

fn arith_remainder(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero { span: form_span });
    }
    Ok(Value::int(nums[0] % nums[1]))
}

fn arith_quotient(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero { span: form_span });
    }
    Ok(Value::int(nums[0] / nums[1]))
}

fn builtin_exact_to_inexact(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let n = value_to_num(&args[0]).ok_or_else(|| EvalError::TypeMismatch {
        expected: "number".to_string(),
        got: format!("{}", args[0]),
        span: args[0].span(),
    })?;
    Ok(Value::float(n.to_f64()))
}

fn builtin_inexact_to_exact(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Integer(_, _) => Ok(args[0].clone()),
        Value::Rational(_, _, _) => Ok(args[0].clone()),
        Value::Float(f, _) => {
            // Convert float to exact rational using continued fraction approximation
            Ok(float_to_exact(*f))
        }
        other => Err(EvalError::TypeMismatch {
            expected: "number".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn float_to_exact(f: f64) -> Value {
    if f == f.floor() {
        return Value::int(f as i64);
    }
    // Use the standard approach: multiply by power of 2 to find exact fraction
    // For common fractions, continued fraction approach works well
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    let max_denom = 1_000_000_000i64;
    let mut p0: i64 = 0;
    let mut q0: i64 = 1;
    let mut p1: i64 = 1;
    let mut q1: i64 = 0;
    let mut x = f;
    loop {
        let a = x.floor() as i64;
        let p2 = a * p1 + p0;
        let q2 = a * q1 + q0;
        if q2 > max_denom {
            break;
        }
        p0 = p1;
        q0 = q1;
        p1 = p2;
        q1 = q2;
        let remainder = x - a as f64;
        if remainder.abs() < 1e-15 {
            break;
        }
        x = 1.0 / remainder;
        if x > max_denom as f64 {
            break;
        }
    }
    make_rational(sign * p1, q1, Span::default())
}

fn builtin_numerator(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Integer(n, _) => Ok(Value::int(*n)),
        Value::Rational(n, _, _) => Ok(Value::int(*n)),
        other => Err(EvalError::TypeMismatch {
            expected: "rational".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_denominator(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Integer(_, _) => Ok(Value::int(1)),
        Value::Rational(_, d, _) => Ok(Value::int(*d)),
        other => Err(EvalError::TypeMismatch {
            expected: "rational".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_display(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    env.write_output(&args[0].display_string());
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    env.write_output(&args[0].to_string());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "0".to_string(),
            got: args.len(),
            span,
        });
    }
    env.write_output("\n");
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();
    for arg in args {
        match arg {
            Value::String(s, _, _) => result.push_str(&s.borrow()),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "string".to_string(),
                    got: format!("{other}"),
                    span: other.span(),
                });
            }
        }
    }
    Ok(Value::string(result))
}

fn builtin_string_length(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::String(s, _, _) => Ok(Value::int(s.borrow().chars().count() as i64)),
        other => Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_substring(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(start, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let Value::Integer(end, _) = &args[2] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[2]),
            span: args[2].span(),
        });
    };
    let borrowed = s.borrow();
    let chars: Vec<char> = borrowed.chars().collect();
    let start = *start as usize;
    let end = *end as usize;
    let sub: String = chars[start..end].iter().collect();
    Ok(Value::string(sub))
}

fn builtin_string_to_number(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::String(s, _, _) => {
            let s = s.borrow();
            if let Ok(n) = s.parse::<i64>() {
                return Ok(Value::int(n));
            }
            if s.contains('.') {
                if let Ok(f) = s.parse::<f64>() {
                    return Ok(Value::float(f));
                }
            }
            Ok(Value::bool(false))
        }
        other => Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_number_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Integer(n, _) => Ok(Value::string(n.to_string())),
        Value::Rational(n, d, _) => Ok(Value::string(format!("{n}/{d}"))),
        Value::Float(f, _) => Ok(Value::string(format!("{f}"))),
        other => Err(EvalError::TypeMismatch {
            expected: "number".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_symbol_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Symbol(s, _) => Ok(Value::string(s.clone())),
        other => Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_string_to_symbol(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::String(s, _, _) => Ok(Value::symbol(s.borrow().clone())),
        other => Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_string_ref(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let c = s.borrow().chars().nth(*idx as usize).ok_or_else(|| EvalError::TypeMismatch {
        expected: "valid index".to_string(),
        got: format!("index {idx} out of bounds"),
        span,
    })?;
    Ok(Value::Char(c, Span::default()))
}

fn builtin_string_set(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, mutability, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    if *mutability == Mutability::Immutable {
        return Err(EvalError::ImmutableString { span });
    }
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let Value::Char(c, _) = &args[2] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[2]),
            span: args[2].span(),
        });
    };
    let idx = *idx as usize;
    let mut borrowed = s.borrow_mut();
    let byte_offset = borrowed.char_indices().nth(idx).ok_or_else(|| EvalError::TypeMismatch {
        expected: "valid index".to_string(),
        got: format!("index {idx} out of bounds"),
        span,
    })?.0;
    let old_char_len = borrowed[byte_offset..].chars().next().expect("valid char at index").len_utf8();
    let mut buf = std::string::String::with_capacity(borrowed.len() - old_char_len + c.len_utf8());
    buf.push_str(&borrowed[..byte_offset]);
    buf.push(*c);
    buf.push_str(&borrowed[byte_offset + old_char_len..]);
    *borrowed = buf;
    drop(borrowed);
    Ok(Value::Void)
}

fn builtin_string_copy(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::string(s.borrow().clone()))
}

fn builtin_string_to_list(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let chars: Vec<Value> = s.borrow().chars().map(|c| Value::Char(c, Span::default())).collect();
    Ok(Value::list(chars))
}

fn builtin_list_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::List(elems, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let mut result = String::new();
    for elem in elems {
        let Value::Char(c, _) = elem else {
            return Err(EvalError::TypeMismatch {
                expected: "char".to_string(),
                got: format!("{elem}"),
                span: elem.span(),
            });
        };
        result.push(*c);
    }
    Ok(Value::string(result))
}

fn builtin_char_to_integer(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(c, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::int(*c as i64))
}

fn builtin_integer_to_char(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Integer(n, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let c = char::from_u32(*n as u32).ok_or_else(|| EvalError::TypeMismatch {
        expected: "valid Unicode code point".to_string(),
        got: format!("{n}"),
        span,
    })?;
    Ok(Value::Char(c, Span::default()))
}

fn builtin_expt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
            span,
        });
    }
    let base = nums[0];
    let exp = nums[1];
    if exp < 0 {
        return Ok(Value::int(0)); // integer division truncates
    }
    Ok(Value::int(base.pow(exp as u32)))
}

fn builtin_list_ref(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::List(elems, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let idx = *idx as usize;
    elems.get(idx).cloned().ok_or_else(|| EvalError::TypeMismatch {
        expected: "valid index".to_string(),
        got: format!("index {idx} out of bounds"),
        span,
    })
}

fn builtin_list_tail(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::List(elems, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let idx = *idx as usize;
    if idx > elems.len() {
        return Err(EvalError::TypeMismatch {
            expected: "valid index".to_string(),
            got: format!("index {idx} out of bounds"),
            span,
        });
    }
    Ok(Value::list(elems[idx..].to_vec()))
}

fn builtin_list_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let is_list = match &args[0] {
        Value::List(elems, _) => {
            // A proper list has no dot notation
            // Our representation: improper pairs are [a, ".", b]
            !elems.iter().any(|e| matches!(e, Value::Symbol(s, _) if s == "."))
        }
        _ => false,
    };
    Ok(Value::bool(is_list))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x, _), Value::Integer(y, _)) => x == y,
        (Value::Boolean(x, _), Value::Boolean(y, _)) => x == y,
        (Value::String(x, _, _), Value::String(y, _, _)) => *x.borrow() == *y.borrow(),
        (Value::Symbol(x, _), Value::Symbol(y, _)) => x == y,
        (Value::Char(x, _), Value::Char(y, _)) => x == y,
        (Value::List(xs, _), Value::List(ys, _)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Vector(xs, _), Value::Vector(ys, _)) => {
            let xb = xs.borrow();
            let yb = ys.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

fn builtin_assoc(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let key = &args[0];
    let Value::List(alist, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    for entry in alist {
        let Value::List(pair, _) = entry else {
            return Err(EvalError::TypeMismatch {
                expected: "pair".to_string(),
                got: format!("{entry}"),
                span: entry.span(),
            });
        };
        if !pair.is_empty() && values_equal(key, &pair[0]) {
            return Ok(entry.clone());
        }
    }
    Ok(Value::bool(false))
}

fn builtin_map(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span,
        });
    }
    let func = &args[0];
    let lists: Vec<&Vec<Value>> = args[1..]
        .iter()
        .map(|a| match a {
            Value::List(elems, _) => Ok(elems),
            other => Err(EvalError::TypeMismatch {
                expected: "list".to_string(),
                got: format!("{other}"),
                span: other.span(),
            }),
        })
        .collect::<Result<_, _>>()?;

    if lists.is_empty() {
        return Ok(Value::list(Vec::new()));
    }
    let len = lists[0].len();
    let mut results = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        let call_expr = Value::list(
            std::iter::once(func.clone())
                .chain(call_args.into_iter().map(|a| {
                    Value::list(vec![Value::symbol("quote".to_string()), a])
                }))
                .collect(),
        );
        results.push(eval(&call_expr, env)?);
    }
    Ok(Value::list(results))
}

fn builtin_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let result = match (&args[0], &args[1]) {
        (Value::Integer(a, _), Value::Integer(b, _)) => a == b,
        (Value::Rational(an, ad, _), Value::Rational(bn, bd, _)) => an == bn && ad == bd,
        (Value::Float(a, _), Value::Float(b, _)) => a == b,
        (Value::Boolean(a, _), Value::Boolean(b, _)) => a == b,
        (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
        (Value::Char(a, _), Value::Char(b, _)) => a == b,
        (Value::List(a, _), Value::List(b, _)) if a.is_empty() && b.is_empty() => true,
        (Value::Vector(a, _), Value::Vector(b, _)) => Rc::ptr_eq(a, b),
        (Value::String(a, _, _), Value::String(b, _, _)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    };
    Ok(Value::bool(result))
}

fn builtin_eqv(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let result = match (&args[0], &args[1]) {
        (Value::Integer(a, _), Value::Integer(b, _)) => a == b,
        (Value::Rational(an, ad, _), Value::Rational(bn, bd, _)) => an == bn && ad == bd,
        (Value::Float(a, _), Value::Float(b, _)) => a == b,
        (Value::Boolean(a, _), Value::Boolean(b, _)) => a == b,
        (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
        (Value::Char(a, _), Value::Char(b, _)) => a == b,
        (Value::List(a, _), Value::List(b, _)) if a.is_empty() && b.is_empty() => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    };
    Ok(Value::bool(result))
}

fn builtin_equal(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    Ok(Value::bool(values_equal(&args[0], &args[1])))
}

fn builtin_char_alphabetic(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(c, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::bool(c.is_alphabetic()))
}

fn builtin_char_numeric(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(c, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::bool(c.is_ascii_digit()))
}

fn builtin_char_upcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(c, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::Char(c.to_ascii_uppercase(), Span::default()))
}

fn builtin_char_downcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(c, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::Char(c.to_ascii_lowercase(), Span::default()))
}

fn builtin_char_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(a, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Char(b, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    Ok(Value::bool(a == b))
}

fn builtin_char_lt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Char(a, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Char(b, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    Ok(Value::bool(a < b))
}

fn builtin_string_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(a, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::String(b, _, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    Ok(Value::bool(*a.borrow() == *b.borrow()))
}

fn builtin_string_lt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(a, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::String(b, _, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    Ok(Value::bool(*a.borrow() < *b.borrow()))
}

fn builtin_string_ci_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(a, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::String(b, _, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    Ok(Value::bool(a.borrow().to_lowercase() == b.borrow().to_lowercase()))
}

fn builtin_string_upcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::string(s.borrow().to_uppercase()))
}

fn builtin_string_downcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::string(s.borrow().to_lowercase()))
}

fn builtin_make_vector(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::WrongArgCount {
            expected: "1 or 2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Integer(n, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let fill = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::int(0)
    };
    Ok(Value::vector(vec![fill; *n as usize]))
}

fn builtin_vector_ref(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Vector(v, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "vector".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: args[1].to_string(),
            span: args[1].span(),
        });
    };
    let borrowed = v.borrow();
    let idx = *idx as usize;
    borrowed.get(idx).cloned().ok_or_else(|| EvalError::TypeMismatch {
        expected: "valid index".to_string(),
        got: format!("index {idx} out of bounds"),
        span,
    })
}

fn builtin_vector_set(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Vector(v, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "vector".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: args[1].to_string(),
            span: args[1].span(),
        });
    };
    let idx = *idx as usize;
    let mut borrowed = v.borrow_mut();
    if idx >= borrowed.len() {
        return Err(EvalError::TypeMismatch {
            expected: "valid index".to_string(),
            got: format!("index {idx} out of bounds"),
            span,
        });
    }
    borrowed[idx] = args[2].clone();
    drop(borrowed);
    Ok(Value::Void)
}

fn builtin_vector_length(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Vector(v, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "vector".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    Ok(Value::int(v.borrow().len() as i64))
}

fn builtin_vector_to_list(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::Vector(v, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "vector".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    Ok(Value::list(v.borrow().clone()))
}

fn builtin_list_to_vector(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::List(elems, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    Ok(Value::vector(elems.clone()))
}

fn builtin_raise(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    Err(EvalError::SchemeRaise {
        value: args[0].clone(),
    })
}

fn builtin_with_exception_handler(
    args: &[Value],
    span: Span,
    env: &Env,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let handler = &args[0];
    let thunk = &args[1];

    match call_thunk(thunk, span, env) {
        Ok(val) => Ok(val),
        Err(EvalError::SchemeRaise { value }) => {
            // Call the handler with the raised value
            call_with_arg(handler, value, span, env)
        }
        Err(e) => Err(e),
    }
}

/// Call a one-argument procedure with the given value.
fn call_with_arg(func: &Value, arg: Value, span: Span, _env: &Env) -> Result<Value, EvalError> {
    match func {
        Value::Closure {
            ref params,
            ref rest_param,
            ref body,
            env: ref closure_env,
        } => {
            let local_env = bind_closure_args(params, rest_param, &[arg], closure_env, span)?;
            eval(body, &local_env)
        }
        other => Err(EvalError::TypeMismatch {
            expected: "procedure".to_string(),
            got: other.to_string(),
            span: other.span(),
        }),
    }
}

/// Evaluate `(guard (var clause ...) body ...)`.
fn eval_guard(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    // First arg: (var clause1 clause2 ...)
    let Value::List(guard_spec, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "guard clause list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    if guard_spec.is_empty() {
        return Err(EvalError::Parse {
            message: "guard requires a variable".to_string(),
            span: form_span,
        });
    }
    let Value::Symbol(var_name, _) = &guard_spec[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: guard_spec[0].to_string(),
            span: guard_spec[0].span(),
        });
    };
    let clauses = &guard_spec[1..];
    let body = &args[1..];

    // Evaluate body expressions
    let body_result = eval_body(body, env);

    match body_result {
        Ok(val) => Ok(val),
        Err(EvalError::SchemeRaise { value }) => {
            // Bind the raised value to var_name
            let clause_env = Env::with_parent(env);
            clause_env.define(var_name.clone(), value.clone());

            // Try each clause
            for clause in clauses {
                let Value::List(parts, _) = clause else {
                    return Err(EvalError::TypeMismatch {
                        expected: "guard clause".to_string(),
                        got: clause.to_string(),
                        span: clause.span(),
                    });
                };
                if parts.is_empty() {
                    return Err(EvalError::Parse {
                        message: "empty guard clause".to_string(),
                        span: clause.span(),
                    });
                }
                // Check for else clause
                if matches!(&parts[0], Value::Symbol(s, _) if s == "else") {
                    return eval_body(&parts[1..], &clause_env);
                }
                let test = eval(&parts[0], &clause_env)?;
                if test.is_truthy() {
                    if parts.len() == 1 {
                        return Ok(test);
                    }
                    let mut last = Value::Void;
                    for expr in &parts[1..] {
                        last = eval(expr, &clause_env)?;
                    }
                    return Ok(last);
                }
            }
            // No clause matched — re-raise
            Err(EvalError::SchemeRaise { value })
        }
        Err(e) => Err(e),
    }
}
