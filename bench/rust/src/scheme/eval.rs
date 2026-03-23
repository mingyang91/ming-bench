use std::cell::RefCell;
use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::error::{ErrorKind, EvalError, Span};
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::macros;
use crate::scheme::value::{CapturedCont, StringMutability, Value};

// ── continuation frames ──────────────────────────────────────

/// An entry on the dynamic-wind stack, tracking active in/out thunks.
#[derive(Clone)]
struct WindEntry {
    id: u64,
    in_thunk: Value,
    out_thunk: Value,
}

/// An operation to perform during continuation wind transfer.
#[derive(Clone)]
enum WindOp {
    /// Pop wind stack, call out-thunk
    CallOut(Value),
    /// Call in-thunk, then push entry onto wind stack
    CallInAndPush(WindEntry),
}

/// An exception handler entry.
#[derive(Clone)]
enum HandlerEntry {
    /// Installed by `with-exception-handler`.
    WithExceptionHandler { handler: Value },
    /// Installed by `guard`.
    Guard {
        var: String,
        clauses: Vec<Expr>,
        env: Rc<Env>,
        guard_k: Vec<Frame>,
        guard_wind: Vec<WindEntry>,
        guard_handlers: Vec<HandlerEntry>,
    },
}

/// A frame on the explicit continuation stack (CEK machine).
#[derive(Clone)]
enum Frame {
    CallFunc { arg_exprs: Vec<Expr>, env: Rc<Env>, span: Span },
    CallArg { func: Value, done: Vec<Value>, rest: Vec<Expr>, env: Rc<Env>, span: Span },
    Seq { rest: Vec<Expr>, env: Rc<Env> },
    Def { name: String, env: Rc<Env> },
    SetVar { name: String, env: Rc<Env> },
    IfTest { then_br: Expr, else_br: Option<Expr>, env: Rc<Env> },
    AndRest { rest: Vec<Expr>, env: Rc<Env> },
    OrRest { rest: Vec<Expr>, env: Rc<Env> },
    LetInit {
        name: String,
        done_n: Vec<String>,
        done_v: Vec<Value>,
        rest: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    NamedLetInit {
        loop_name: String,
        name: String,
        done_n: Vec<String>,
        done_v: Vec<Value>,
        rest: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    CondTest { clause_body: Vec<Expr>, rest_clauses: Vec<Expr>, env: Rc<Env> },
    CaseKey { clauses: Vec<Expr>, env: Rc<Env> },
    DoInit {
        var_names: Vec<String>,
        done_vals: Vec<Value>,
        rest_inits: Vec<Expr>,
        step_exprs: Vec<Option<Expr>>,
        test: Expr,
        result_exprs: Vec<Expr>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    DoTest {
        vars: Vec<String>,
        step_exprs: Vec<Option<Expr>>,
        test: Expr,
        result_exprs: Vec<Expr>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    DoStep {
        vars: Vec<String>,
        step_exprs: Vec<Option<Expr>>,
        test: Expr,
        result_exprs: Vec<Expr>,
        body: Vec<Expr>,
        done_vals: Vec<Value>,
        rest_indices: Vec<usize>,
        env: Rc<Env>,
    },
    MapStep {
        func: Value,
        lists: Vec<Vec<Value>>,
        index: usize,
        results: Vec<Value>,
        env: Rc<Env>,
        span: Span,
    },
    /// After in-thunk returns: push wind entry, call body-thunk.
    DynamicWindAfterIn {
        body_thunk: Value,
        out_thunk: Value,
        wind_entry: WindEntry,
        env: Rc<Env>,
        span: Span,
    },
    /// After body-thunk returns: pop wind entry, call out-thunk, save body value.
    DynamicWindAfterBody {
        out_thunk: Value,
        env: Rc<Env>,
        span: Span,
    },
    /// After out-thunk returns: return saved body value.
    DynamicWindAfterOut {
        body_val: Value,
    },
    /// Process a sequence of wind operations during continuation transfer.
    WindTransfer {
        ops: Vec<WindOp>,
        final_val: Value,
        env: Rc<Env>,
        span: Span,
    },
    /// Pop exception handler when guard/with-exception-handler body returns normally.
    PopHandler,
    /// After guard wind transfer completes, test guard clauses.
    GuardClauseTest {
        var: String,
        raised_val: Value,
        clauses: Vec<Expr>,
        env: Rc<Env>,
    },
    /// After evaluating a guard clause test, dispatch on result.
    GuardClauseDispatch {
        clause_body: Vec<Expr>,
        rest_clauses: Vec<Expr>,
        raised_val: Value,
        env: Rc<Env>,
    },
}

/// CEK machine state.
enum State {
    Eval(Expr, Rc<Env>),
    Apply(Value, Vec<Value>, Rc<Env>, Span),
    Ret(Value),
}

// ── public entry points ──────────────────────────────────────

/// Evaluate a sequence of top-level expressions, returning the last result.
pub fn eval_top_level(exprs: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let mut k: Vec<Frame> = Vec::new();
    let mut wind: Vec<WindEntry> = Vec::new();
    let mut wind_id: u64 = 0;
    let mut handlers: Vec<HandlerEntry> = Vec::new();
    let mut state = begin_seq(exprs, env, &mut k);
    run_loop(&mut state, &mut k, &mut wind, &mut wind_id, &mut handlers)
}

// ── main loop ────────────────────────────────────────────────

fn run_loop(
    state: &mut State,
    k: &mut Vec<Frame>,
    wind: &mut Vec<WindEntry>,
    wind_id: &mut u64,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<Value, EvalError> {
    loop {
        *state = match std::mem::replace(state, State::Ret(Value::Void)) {
            State::Eval(expr, env) => step_eval(expr, &env, k, wind, handlers)?,
            State::Apply(func, args, env, span) => {
                step_apply(func, args, &env, k, (wind, wind_id), span, handlers)
                    .map_err(|e| e.with_span(span))?
            }
            State::Ret(val) => match k.pop() {
                Some(frame) => step_ret(val, frame, k, wind, handlers)?,
                None => return Ok(val),
            },
        };
    }
}

/// Start evaluating a non-empty sequence of expressions.
fn begin_seq(exprs: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> State {
    debug_assert!(!exprs.is_empty());
    if exprs.len() > 1 {
        k.push(Frame::Seq { rest: exprs[1..].to_vec(), env: Rc::clone(env) });
    }
    State::Eval(exprs[0].clone(), Rc::clone(env))
}

// ── step_eval ────────────────────────────────────────────────

fn step_eval(expr: Expr, env: &Rc<Env>, k: &mut Vec<Frame>, wind: &[WindEntry], handlers: &mut Vec<HandlerEntry>) -> Result<State, EvalError> {
    let span = expr.span;
    let result = match expr.kind {
        ExprKind::Integer(n) => Ok(State::Ret(Value::Integer(n))),
        ExprKind::Boolean(b) => Ok(State::Ret(Value::Boolean(b))),
        ExprKind::Str(s) => Ok(State::Ret(Value::new_str(s))),
        ExprKind::Char(c) => Ok(State::Ret(Value::Char(c))),
        ExprKind::Symbol(name) => env
            .get(&name)
            .map(State::Ret)
            .ok_or_else(|| EvalError::from(ErrorKind::UnboundVariable { name })),
        ExprKind::List(elems) => step_eval_list(elems, env, k, span, wind, handlers),
    };
    result.map_err(|e| e.with_span(span))
}

fn step_eval_list(
    mut elems: Vec<Expr>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    span: Span,
    wind: &[WindEntry],
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    if elems.is_empty() {
        return Ok(State::Ret(Value::List(Vec::new())));
    }
    if let ExprKind::Symbol(ref name) = elems[0].kind {
        match name.as_str() {
            "if" => return step_if(&elems[1..], env, k),
            "define" => return step_define(&elems[1..], env, k),
            "set!" => return step_set_bang(&elems[1..], env, k),
            "quote" => return eval_quote(&elems[1..]).map(State::Ret),
            "lambda" => return eval_lambda(&elems[1..], env).map(State::Ret),
            "begin" => return step_begin(&elems[1..], env, k),
            "and" => return step_and(&elems[1..], env, k),
            "or" => return step_or(&elems[1..], env, k),
            "let" => return step_let(&elems[1..], env, k),
            "cond" => return step_cond(&elems[1..], env, k),
            "case" => return step_case(&elems[1..], env, k),
            "letrec" => return step_letrec(&elems[1..], env, k),
            "letrec*" => return step_letrec_star(&elems[1..], env, k),
            "do" => return step_do(&elems[1..], env, k),
            "define-syntax" => return step_define_syntax(&elems[1..], env),
            "guard" => return step_guard(&elems[1..], env, k, wind, handlers),
            _ => {}
        }
        // Check for macro invocation
        if let Some(Value::SyntaxRules {
            ref literals,
            ref rules,
            ref def_env,
        }) = env.get(name)
        {
            let (expanded, new_env) =
                macros::expand_macro(literals, rules, def_env, &elems[1..], env, span)?;
            return Ok(State::Eval(expanded, new_env));
        }
    }
    // Function call: evaluate function position first, then args.
    let func_expr = elems.remove(0);
    k.push(Frame::CallFunc { arg_exprs: elems, env: Rc::clone(env), span });
    Ok(State::Eval(func_expr, Rc::clone(env)))
}

// ── special forms ────────────────────────────────────────────

fn step_if(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(ErrorKind::BadSyntax {
            form: "if".into(),
            message: "expected 2 or 3 arguments".into(),
        }
        .into());
    }
    k.push(Frame::IfTest {
        then_br: args[1].clone(),
        else_br: args.get(2).cloned(),
        env: Rc::clone(env),
    });
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_define(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "define".into(),
            message: "missing name".into(),
        }
        .into());
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "expected (define name expr)".into(),
                }
                .into());
            }
            k.push(Frame::Def {
                name: name.clone(),
                env: Rc::clone(env),
            });
            Ok(State::Eval(args[1].clone(), Rc::clone(env)))
        }
        ExprKind::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "missing function name".into(),
                }
                .into());
            }
            let name = match &name_and_params[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(ErrorKind::BadSyntax {
                        form: "define".into(),
                        message: "function name must be a symbol".into(),
                    }
                    .into())
                }
            };
            let (params, rest_param) = parse_params(&name_and_params[1..], "define")?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                closure_env: Rc::clone(env),
            };
            env.define(name, lambda);
            Ok(State::Ret(Value::Void))
        }
        _ => Err(ErrorKind::BadSyntax {
            form: "define".into(),
            message: "invalid define syntax".into(),
        }
        .into()),
    }
}

fn step_set_bang(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() != 2 {
        return Err(ErrorKind::BadSyntax {
            form: "set!".into(),
            message: "expected (set! name expr)".into(),
        }
        .into());
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "set!".into(),
                message: "first argument must be a symbol".into(),
            }
            .into())
        }
    };
    k.push(Frame::SetVar {
        name,
        env: Rc::clone(env),
    });
    Ok(State::Eval(args[1].clone(), Rc::clone(env)))
}

fn step_begin(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Ok(State::Ret(Value::Void));
    }
    Ok(begin_seq(args, env, k))
}

fn step_and(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Ok(State::Ret(Value::Boolean(true)));
    }
    if args.len() > 1 {
        k.push(Frame::AndRest {
            rest: args[1..].to_vec(),
            env: Rc::clone(env),
        });
    }
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_or(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Ok(State::Ret(Value::Boolean(false)));
    }
    if args.len() > 1 {
        k.push(Frame::OrRest {
            rest: args[1..].to_vec(),
            env: Rc::clone(env),
        });
    }
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_let(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "missing bindings".into(),
        }
        .into());
    }
    // Named let: (let name ((var init) ...) body...)
    if let ExprKind::Symbol(loop_name) = &args[0].kind {
        if args.len() < 3 {
            return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "expected (let name ((var init) ...) body...)".into(),
            }
            .into());
        }
        let mut bindings = parse_let_bindings(&args[1])?;
        let body = args[2..].to_vec();
        if bindings.is_empty() {
            let func_env = Env::extend(env, vec![loop_name.clone()], vec![Value::Void]);
            let lambda = Value::Lambda {
                params: Vec::new(),
                rest_param: None,
                body: body.clone(),
                closure_env: Rc::clone(&func_env),
            };
            func_env.define(loop_name.clone(), lambda);
            return Ok(begin_seq(&body, &func_env, k));
        }
        let (first_name, first_expr) = bindings.remove(0);
        k.push(Frame::NamedLetInit {
            loop_name: loop_name.clone(),
            name: first_name,
            done_n: Vec::new(),
            done_v: Vec::new(),
            rest: bindings,
            body,
            env: Rc::clone(env),
        });
        return Ok(State::Eval(first_expr, Rc::clone(env)));
    }
    // Regular let
    let mut bindings = parse_let_bindings(&args[0])?;
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "missing body".into(),
        }
        .into());
    }
    let body = args[1..].to_vec();
    if bindings.is_empty() {
        let local_env = Env::extend(env, Vec::new(), Vec::new());
        return Ok(begin_seq(&body, &local_env, k));
    }
    let (first_name, first_expr) = bindings.remove(0);
    k.push(Frame::LetInit {
        name: first_name,
        done_n: Vec::new(),
        done_v: Vec::new(),
        rest: bindings,
        body,
        env: Rc::clone(env),
    });
    Ok(State::Eval(first_expr, Rc::clone(env)))
}

fn step_cond(clauses: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if let Some((i, clause)) = clauses.iter().enumerate().next() {
        match &clause.kind {
            ExprKind::List(elems) if !elems.is_empty() => {
                if let ExprKind::Symbol(s) = &elems[0].kind {
                    if s == "else" {
                        return Ok(begin_seq(&elems[1..], env, k));
                    }
                }
                k.push(Frame::CondTest {
                    clause_body: elems[1..].to_vec(),
                    rest_clauses: clauses[i + 1..].to_vec(),
                    env: Rc::clone(env),
                });
                return Ok(State::Eval(elems[0].clone(), Rc::clone(env)));
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "cond".into(),
                    message: "each clause must be a list".into(),
                }
                .into())
            }
        }
    }
    Ok(State::Ret(Value::Void))
}

fn step_case(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "case".into(),
            message: "missing key expression".into(),
        }
        .into());
    }
    let clauses = args[1..].to_vec();
    k.push(Frame::CaseKey {
        clauses,
        env: Rc::clone(env),
    });
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_letrec(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "letrec".into(),
            message: "expected (letrec ((var init) ...) body...)".into(),
        }
        .into());
    }
    let bindings = parse_let_bindings(&args[0])?;
    let body = args[1..].to_vec();
    let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let voids: Vec<Value> = vec![Value::Void; names.len()];
    let letrec_env = Env::extend(env, names, voids);
    if bindings.is_empty() {
        return Ok(begin_seq(&body, &letrec_env, k));
    }
    // Build a sequence: (set! name1 init1) (set! name2 init2) ... body...
    let mut seq_exprs: Vec<Expr> = Vec::new();
    for (name, init) in &bindings {
        let span = init.span;
        seq_exprs.push(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("set!".into()), span },
                Expr { kind: ExprKind::Symbol(name.clone()), span },
                init.clone(),
            ]),
            span,
        });
    }
    seq_exprs.extend(body);
    Ok(begin_seq(&seq_exprs, &letrec_env, k))
}

fn step_letrec_star(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "letrec*".into(),
            message: "expected (letrec* ((var init) ...) body...)".into(),
        }
        .into());
    }
    let bindings = parse_let_bindings(&args[0])?;
    let body = args[1..].to_vec();
    // Create env with all names bound to Void, then sequentially evaluate and define
    let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let voids: Vec<Value> = vec![Value::Void; names.len()];
    let letrec_env = Env::extend(env, names, voids);
    if bindings.is_empty() {
        return Ok(begin_seq(&body, &letrec_env, k));
    }
    // Use Seq + Def frames to evaluate each init and define it sequentially
    // Build a sequence of define expressions in the letrec_env
    let mut seq_exprs: Vec<Expr> = Vec::new();
    for (name, init) in &bindings {
        // (set! name init)
        let span = init.span;
        seq_exprs.push(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("set!".into()), span },
                Expr { kind: ExprKind::Symbol(name.clone()), span },
                init.clone(),
            ]),
            span,
        });
    }
    // Add body expressions
    seq_exprs.extend(body);
    Ok(begin_seq(&seq_exprs, &letrec_env, k))
}

fn step_do(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "do".into(),
            message: "expected (do ((var init step) ...) (test expr ...) body...)".into(),
        }
        .into());
    }
    let var_clauses = match &args[0].kind {
        ExprKind::List(clauses) => clauses,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "do".into(),
                message: "variable clauses must be a list".into(),
            }
            .into())
        }
    };
    let mut var_names = Vec::new();
    let mut init_exprs = Vec::new();
    let mut step_exprs: Vec<Option<Expr>> = Vec::new();
    for clause in var_clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 && parts.len() <= 3 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: "do".into(),
                            message: "variable name must be a symbol".into(),
                        }
                        .into())
                    }
                };
                var_names.push(name);
                init_exprs.push(parts[1].clone());
                step_exprs.push(parts.get(2).cloned());
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "do".into(),
                    message: "each variable clause must be (var init) or (var init step)"
                        .into(),
                }
                .into())
            }
        }
    }
    let test_clause = match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => parts,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "do".into(),
                message: "test clause must be (test expr ...)".into(),
            }
            .into())
        }
    };
    let test = test_clause[0].clone();
    let result_exprs = test_clause[1..].to_vec();
    let body = args[2..].to_vec();

    if init_exprs.is_empty() {
        let do_env = Env::extend(env, Vec::new(), Vec::new());
        k.push(Frame::DoTest {
            vars: var_names,
            step_exprs,
            test: test.clone(),
            result_exprs,
            body,
            env: Rc::clone(&do_env),
        });
        return Ok(State::Eval(test, Rc::clone(&do_env)));
    }
    let first = init_exprs[0].clone();
    let rest_inits = init_exprs[1..].to_vec();
    k.push(Frame::DoInit {
        var_names,
        done_vals: Vec::new(),
        rest_inits,
        step_exprs,
        test,
        result_exprs,
        body,
        env: Rc::clone(env),
    });
    Ok(State::Eval(first, Rc::clone(env)))
}

fn dispatch_case(
    key: Value,
    clauses: &[Expr],
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
) -> Result<State, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(elems) if elems.len() >= 2 => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &elems[0].kind {
                    if s == "else" {
                        return Ok(begin_seq(&elems[1..], env, k));
                    }
                }
                // Check datum list
                let datums = match &elems[0].kind {
                    ExprKind::List(d) => d,
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: "case".into(),
                            message: "each clause datum must be a list".into(),
                        }
                        .into())
                    }
                };
                for datum in datums {
                    let datum_val = expr_to_value(datum)?;
                    if scheme_eqv(&key, &datum_val) {
                        return Ok(begin_seq(&elems[1..], env, k));
                    }
                }
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "case".into(),
                    message: "invalid case clause".into(),
                }
                .into())
            }
        }
    }
    // No match, no else
    Ok(State::Ret(Value::Void))
}

fn step_define_syntax(args: &[Expr], env: &Rc<Env>) -> Result<State, EvalError> {
    if args.len() != 2 {
        return Err(ErrorKind::BadSyntax {
            form: "define-syntax".into(),
            message: "expected (define-syntax name (syntax-rules ...))".into(),
        }
        .into());
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "define-syntax".into(),
                message: "name must be a symbol".into(),
            }
            .into())
        }
    };
    let val = parse_syntax_rules(&args[1], env)?;
    env.define(name, val);
    Ok(State::Ret(Value::Void))
}

fn parse_syntax_rules(expr: &Expr, env: &Rc<Env>) -> Result<Value, EvalError> {
    let elems = match &expr.kind {
        ExprKind::List(e) => e,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "syntax-rules".into(),
                message: "expected (syntax-rules ...)".into(),
            }
            .into())
        }
    };
    if elems.is_empty() || !matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
        return Err(ErrorKind::BadSyntax {
            form: "define-syntax".into(),
            message: "expected syntax-rules".into(),
        }
        .into());
    }
    if elems.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "syntax-rules".into(),
            message: "missing literals list".into(),
        }
        .into());
    }
    let literals = match &elems[1].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::from(ErrorKind::BadSyntax {
                    form: "syntax-rules".into(),
                    message: "literal must be a symbol".into(),
                })),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "syntax-rules".into(),
                message: "expected literals list".into(),
            }
            .into())
        }
    };
    let mut rules = Vec::new();
    for clause in &elems[2..] {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                let pattern = match &parts[0].kind {
                    ExprKind::List(pat) if !pat.is_empty() => pat[1..].to_vec(),
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: "syntax-rules".into(),
                            message: "pattern must be (name ...)".into(),
                        }
                        .into())
                    }
                };
                rules.push((pattern, parts[1].clone()));
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "syntax-rules".into(),
                    message: "each rule must be (pattern template)".into(),
                }
                .into())
            }
        }
    }
    Ok(Value::SyntaxRules {
        literals,
        rules,
        def_env: Rc::clone(env),
    })
}

// ── guard special form ───────────────────────────────────────

fn step_guard(
    args: &[Expr],
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    wind: &[WindEntry],
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "guard".into(),
            message: "expected (guard (var clause ...) body ...)".into(),
        }
        .into());
    }
    let header = match &args[0].kind {
        ExprKind::List(elems) if elems.len() >= 2 => elems,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "guard".into(),
                message: "expected (guard (var clause ...) body ...)".into(),
            }
            .into())
        }
    };
    let var = match &header[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "guard".into(),
                message: "guard variable must be a symbol".into(),
            }
            .into())
        }
    };
    let clauses = header[1..].to_vec();
    let body = args[1..].to_vec();

    // Save guard continuation state (before pushing PopHandler)
    let guard_k = k.clone();
    let guard_wind = wind.to_vec();
    let guard_handlers = handlers.clone();

    // Push PopHandler so normal body return pops the handler
    k.push(Frame::PopHandler);

    // Install guard handler
    handlers.push(HandlerEntry::Guard {
        var,
        clauses,
        env: Rc::clone(env),
        guard_k,
        guard_wind,
        guard_handlers,
    });

    // Evaluate body
    if body.is_empty() {
        Ok(State::Ret(Value::Void))
    } else {
        Ok(begin_seq(&body, env, k))
    }
}

fn step_guard_clauses(
    raised_val: &Value,
    clauses: &[Expr],
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
) -> Result<State, EvalError> {
    if clauses.is_empty() {
        return Err(ErrorKind::UserRaise {
            value: raised_val.to_display_string(),
        }
        .into());
    }
    match &clauses[0].kind {
        ExprKind::List(elems) if !elems.is_empty() => {
            if let ExprKind::Symbol(s) = &elems[0].kind {
                if s == "else" {
                    return Ok(begin_seq(&elems[1..], env, k));
                }
            }
            k.push(Frame::GuardClauseDispatch {
                clause_body: elems[1..].to_vec(),
                rest_clauses: clauses[1..].to_vec(),
                raised_val: raised_val.clone(),
                env: Rc::clone(env),
            });
            Ok(State::Eval(elems[0].clone(), Rc::clone(env)))
        }
        _ => Err(ErrorKind::BadSyntax {
            form: "guard".into(),
            message: "invalid guard clause".into(),
        }
        .into()),
    }
}

// ── step_ret ─────────────────────────────────────────────────

fn step_ret(val: Value, frame: Frame, k: &mut Vec<Frame>, wind: &mut Vec<WindEntry>, handlers: &mut Vec<HandlerEntry>) -> Result<State, EvalError> {
    match frame {
        Frame::CallFunc { arg_exprs, env, span } => {
            if arg_exprs.is_empty() {
                Ok(State::Apply(val, Vec::new(), env, span))
            } else {
                // Evaluate arguments right-to-left (R7RS allows any order).
                // This ensures continuations captured inside args see
                // unevaluated siblings, enabling reentrant call/cc patterns.
                let mut args_rev = arg_exprs;
                args_rev.reverse();
                let next = args_rev.remove(0);
                k.push(Frame::CallArg {
                    func: val,
                    done: Vec::new(),
                    rest: args_rev,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::CallArg {
            func,
            mut done,
            rest,
            env,
            span,
        } => {
            done.push(val);
            if rest.is_empty() {
                // Arguments were evaluated right-to-left; restore original order.
                done.reverse();
                Ok(State::Apply(func, done, env, span))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::CallArg {
                    func,
                    done,
                    rest,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::Seq { rest, env } => {
            if rest.len() == 1 {
                Ok(State::Eval(
                    rest.into_iter().next().expect("checked len"),
                    env,
                ))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::Seq {
                    rest,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::Def { name, env } => {
            env.define(name, val);
            Ok(State::Ret(Value::Void))
        }
        Frame::SetVar { name, env } => {
            if !env.set(&name, val) {
                return Err(ErrorKind::UnboundVariable { name }.into());
            }
            Ok(State::Ret(Value::Void))
        }
        Frame::IfTest {
            then_br,
            else_br,
            env,
        } => {
            if val.is_truthy() {
                Ok(State::Eval(then_br, env))
            } else if let Some(e) = else_br {
                Ok(State::Eval(e, env))
            } else {
                Ok(State::Ret(Value::Void))
            }
        }
        Frame::AndRest { rest, env } => {
            if !val.is_truthy() {
                Ok(State::Ret(val))
            } else if rest.len() == 1 {
                Ok(State::Eval(
                    rest.into_iter().next().expect("checked len"),
                    env,
                ))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::AndRest {
                    rest,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::OrRest { rest, env } => {
            if val.is_truthy() {
                Ok(State::Ret(val))
            } else if rest.len() == 1 {
                Ok(State::Eval(
                    rest.into_iter().next().expect("checked len"),
                    env,
                ))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::OrRest {
                    rest,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::LetInit {
            name,
            mut done_n,
            mut done_v,
            rest,
            body,
            env,
        } => {
            done_n.push(name);
            done_v.push(val);
            if rest.is_empty() {
                let local_env = Env::extend(&env, done_n, done_v);
                if body.is_empty() {
                    Ok(State::Ret(Value::Void))
                } else {
                    Ok(begin_seq(&body, &local_env, k))
                }
            } else {
                let mut rest = rest;
                let (next_name, next_expr) = rest.remove(0);
                k.push(Frame::LetInit {
                    name: next_name,
                    done_n,
                    done_v,
                    rest,
                    body,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next_expr, env))
            }
        }
        Frame::NamedLetInit {
            loop_name,
            name,
            mut done_n,
            mut done_v,
            rest,
            body,
            env,
        } => {
            done_n.push(name);
            done_v.push(val);
            if rest.is_empty() {
                let params = done_n;
                let func_env =
                    Env::extend(&env, vec![loop_name.clone()], vec![Value::Void]);
                let recursive_lambda = Value::Lambda {
                    params,
                    rest_param: None,
                    body,
                    closure_env: Rc::clone(&func_env),
                };
                func_env.define(loop_name, recursive_lambda.clone());
                Ok(State::Apply(recursive_lambda, done_v, env, Span::default()))
            } else {
                let mut rest = rest;
                let (next_name, next_expr) = rest.remove(0);
                k.push(Frame::NamedLetInit {
                    loop_name,
                    name: next_name,
                    done_n,
                    done_v,
                    rest,
                    body,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next_expr, env))
            }
        }
        Frame::MapStep {
            func,
            lists,
            index,
            mut results,
            env,
            span,
        } => {
            results.push(val);
            let next_index = index + 1;
            if next_index >= lists[0].len() {
                Ok(State::Ret(Value::List(results)))
            } else {
                let next_args: Vec<Value> = lists.iter().map(|l| l[next_index].clone()).collect();
                k.push(Frame::MapStep {
                    func: func.clone(),
                    lists,
                    index: next_index,
                    results,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Apply(func, next_args, env, span))
            }
        }
        Frame::CondTest {
            clause_body,
            rest_clauses,
            env,
        } => {
            if val.is_truthy() {
                if clause_body.is_empty() {
                    Ok(State::Ret(val))
                } else {
                    Ok(begin_seq(&clause_body, &env, k))
                }
            } else {
                step_cond(&rest_clauses, &env, k)
            }
        }
        Frame::CaseKey { clauses, env } => {
            // val is the key; dispatch through clauses using eqv?
            dispatch_case(val, &clauses, &env, k)
        }
        Frame::DoInit { .. }
        | Frame::DoTest { .. }
        | Frame::DoStep { .. } => step_ret_do(val, frame, k),
        Frame::DynamicWindAfterIn { .. }
        | Frame::DynamicWindAfterBody { .. }
        | Frame::DynamicWindAfterOut { .. }
        | Frame::WindTransfer { .. }
        | Frame::PopHandler
        | Frame::GuardClauseTest { .. }
        | Frame::GuardClauseDispatch { .. } => {
            step_ret_wind(val, frame, k, wind, handlers)
        }
    }
}

// ── step_ret_wind (dynamic-wind / guard continuation frames) ─

fn step_ret_wind(
    val: Value,
    frame: Frame,
    k: &mut Vec<Frame>,
    wind: &mut Vec<WindEntry>,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    match frame {
        Frame::DynamicWindAfterIn {
            body_thunk,
            out_thunk,
            wind_entry,
            env,
            span,
        } => {
            // in-thunk finished; push wind entry and call body-thunk
            wind.push(wind_entry);
            k.push(Frame::DynamicWindAfterBody {
                out_thunk,
                env: Rc::clone(&env),
                span,
            });
            Ok(State::Apply(body_thunk, Vec::new(), env, span))
        }
        Frame::DynamicWindAfterBody { out_thunk, env, span } => {
            // body-thunk finished; pop wind entry, save body value, call out-thunk
            wind.pop();
            k.push(Frame::DynamicWindAfterOut { body_val: val });
            Ok(State::Apply(out_thunk, Vec::new(), env, span))
        }
        Frame::DynamicWindAfterOut { body_val } => {
            // out-thunk finished; return saved body value
            Ok(State::Ret(body_val))
        }
        Frame::WindTransfer {
            mut ops,
            final_val,
            env,
            span,
        } => {
            // Process next wind operation (ignore the value from previous thunk call)
            if ops.is_empty() {
                Ok(State::Ret(final_val))
            } else {
                let op = ops.remove(0);
                match op {
                    WindOp::CallOut(thunk) => {
                        // Wind stack was already truncated; call out-thunk
                        k.push(Frame::WindTransfer {
                            ops,
                            final_val,
                            env: Rc::clone(&env),
                            span,
                        });
                        Ok(State::Apply(thunk, Vec::new(), env, span))
                    }
                    WindOp::CallInAndPush(entry) => {
                        let in_thunk = entry.in_thunk.clone();
                        k.push(Frame::WindTransfer {
                            ops,
                            final_val,
                            env: Rc::clone(&env),
                            span,
                        });
                        wind.push(entry);
                        Ok(State::Apply(in_thunk, Vec::new(), env, span))
                    }
                }
            }
        }
        Frame::PopHandler => {
            handlers.pop();
            Ok(State::Ret(val))
        }
        Frame::GuardClauseTest { var, raised_val, clauses, env } => {
            // Ignore val (from wind transfer); test guard clauses
            let guard_env = Env::extend(&env, vec![var], vec![raised_val.clone()]);
            step_guard_clauses(&raised_val, &clauses, &guard_env, k)
        }
        Frame::GuardClauseDispatch { clause_body, rest_clauses, raised_val, env } => {
            if val.is_truthy() {
                if clause_body.is_empty() {
                    Ok(State::Ret(val))
                } else {
                    Ok(begin_seq(&clause_body, &env, k))
                }
            } else {
                step_guard_clauses(&raised_val, &rest_clauses, &env, k)
            }
        }
        _ => unreachable!("step_ret_wind called with non-wind frame"),
    }
}

// ── step_ret_do (do-loop continuation frames) ──────────────

fn step_ret_do(val: Value, frame: Frame, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    match frame {
        Frame::DoInit {
            var_names,
            mut done_vals,
            rest_inits,
            step_exprs,
            test,
            result_exprs,
            body,
            env,
        } => {
            done_vals.push(val);
            if rest_inits.is_empty() {
                let do_env = Env::extend(&env, var_names.clone(), done_vals);
                k.push(Frame::DoTest {
                    vars: var_names,
                    step_exprs,
                    test: test.clone(),
                    result_exprs,
                    body,
                    env: Rc::clone(&do_env),
                });
                Ok(State::Eval(test, Rc::clone(&do_env)))
            } else {
                let mut rest_inits = rest_inits;
                let next = rest_inits.remove(0);
                k.push(Frame::DoInit {
                    var_names,
                    done_vals,
                    rest_inits,
                    step_exprs,
                    test,
                    result_exprs,
                    body,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::DoTest {
            vars,
            step_exprs,
            test,
            result_exprs,
            body,
            env,
        } => {
            if val.is_truthy() {
                if result_exprs.is_empty() {
                    Ok(State::Ret(Value::Void))
                } else {
                    Ok(begin_seq(&result_exprs, &env, k))
                }
            } else {
                let step_indices: Vec<usize> = step_exprs
                    .iter()
                    .enumerate()
                    .filter_map(|(i, s)| s.as_ref().map(|_| i))
                    .collect();
                if step_indices.is_empty() {
                    if !body.is_empty() {
                        k.push(Frame::DoTest {
                            vars,
                            step_exprs,
                            test: test.clone(),
                            result_exprs,
                            body: body.clone(),
                            env: Rc::clone(&env),
                        });
                        let mut all = body;
                        all.push(test);
                        Ok(begin_seq(&all, &env, k))
                    } else {
                        k.push(Frame::DoTest {
                            vars,
                            step_exprs,
                            test: test.clone(),
                            result_exprs,
                            body,
                            env: Rc::clone(&env),
                        });
                        Ok(State::Eval(test, env))
                    }
                } else {
                    let first_idx = step_indices[0];
                    let first_step = step_exprs[first_idx]
                        .clone()
                        .expect("filtered by step_indices");
                    let remaining_indices = step_indices[1..].to_vec();
                    if !body.is_empty() {
                        k.push(Frame::DoStep {
                            vars,
                            step_exprs,
                            test,
                            result_exprs,
                            body: body.clone(),
                            done_vals: Vec::new(),
                            rest_indices: remaining_indices,
                            env: Rc::clone(&env),
                        });
                        let mut all = body;
                        all.push(first_step);
                        Ok(begin_seq(&all, &env, k))
                    } else {
                        k.push(Frame::DoStep {
                            vars,
                            step_exprs,
                            test,
                            result_exprs,
                            body,
                            done_vals: Vec::new(),
                            rest_indices: remaining_indices,
                            env: Rc::clone(&env),
                        });
                        Ok(State::Eval(first_step, env))
                    }
                }
            }
        }
        Frame::DoStep {
            vars,
            step_exprs,
            test,
            result_exprs,
            body,
            mut done_vals,
            rest_indices,
            env,
        } => {
            done_vals.push(val);
            if rest_indices.is_empty() {
                let mut new_vals: Vec<Value> = Vec::with_capacity(vars.len());
                let mut step_val_iter = done_vals.into_iter();
                for (i, var) in vars.iter().enumerate() {
                    if step_exprs[i].is_some() {
                        new_vals.push(
                            step_val_iter.next().expect("step val count matches"),
                        );
                    } else {
                        new_vals.push(
                            env.get(var).expect("do var bound in env"),
                        );
                    }
                }
                let parent = Env::parent_or_self(&env);
                let new_env = Env::extend(
                    &parent,
                    vars.clone(),
                    new_vals,
                );
                k.push(Frame::DoTest {
                    vars,
                    step_exprs,
                    test: test.clone(),
                    result_exprs,
                    body,
                    env: Rc::clone(&new_env),
                });
                Ok(State::Eval(test, new_env))
            } else {
                let mut rest_indices = rest_indices;
                let next_idx = rest_indices.remove(0);
                let next_step = step_exprs[next_idx]
                    .clone()
                    .expect("filtered by step_indices");
                k.push(Frame::DoStep {
                    vars,
                    step_exprs,
                    test,
                    result_exprs,
                    body,
                    done_vals,
                    rest_indices,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next_step, env))
            }
        }
        _ => unreachable!("step_ret_do called with non-Do frame"),
    }
}

// ── step_apply ───────────────────────────────────────────────

fn step_apply(
    func: Value,
    args: Vec<Value>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    wind_state: (&mut Vec<WindEntry>, &mut u64),
    span: Span,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    let (wind, wind_id) = wind_state;
    match func {
        Value::Builtin(ref name) => match name.as_str() {
            "apply" => {
                if args.len() < 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let last = args.pop().expect("checked len >= 2");
                let tail = match last {
                    Value::List(l) => l,
                    other => {
                        return Err(ErrorKind::TypeMismatch {
                            expected: "list".into(),
                            got: other.to_display_string(),
                        }
                        .into())
                    }
                };
                let f = args.remove(0);
                args.extend(tail);
                Ok(State::Apply(f, args, Rc::clone(env), span))
            }
            "map" => {
                if args.len() < 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let func = args.remove(0);
                let mut lists: Vec<Vec<Value>> = Vec::new();
                for arg in args {
                    match arg {
                        Value::List(l) => lists.push(l),
                        other => {
                            return Err(ErrorKind::TypeMismatch {
                                expected: "list".into(),
                                got: other.to_display_string(),
                            }
                            .into())
                        }
                    }
                }
                if lists.is_empty() || lists[0].is_empty() {
                    return Ok(State::Ret(Value::List(Vec::new())));
                }
                let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
                k.push(Frame::MapStep {
                    func: func.clone(),
                    lists,
                    index: 0,
                    results: Vec::new(),
                    env: Rc::clone(env),
                    span,
                });
                Ok(State::Apply(func, first_args, Rc::clone(env), span))
            }
            "call/cc" | "call-with-current-continuation" => {
                if args.len() != 1 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 1,
                        got: args.len(),
                    }
                    .into());
                }
                let proc = args.into_iter().next().expect("checked len");
                let captured = CapturedCont(Rc::new((k.clone(), wind.clone(), handlers.clone())));
                let cont_val = Value::Continuation(captured);
                Ok(State::Apply(proc, vec![cont_val], Rc::clone(env), span))
            }
            "dynamic-wind" => {
                if args.len() != 3 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 3,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let in_thunk = args.remove(0);
                let body_thunk = args.remove(0);
                let out_thunk = args.remove(0);
                *wind_id += 1;
                let entry = WindEntry {
                    id: *wind_id,
                    in_thunk: in_thunk.clone(),
                    out_thunk: out_thunk.clone(),
                };
                k.push(Frame::DynamicWindAfterIn {
                    body_thunk,
                    out_thunk,
                    wind_entry: entry,
                    env: Rc::clone(env),
                    span,
                });
                // Call in-thunk with no args
                Ok(State::Apply(in_thunk, Vec::new(), Rc::clone(env), span))
            }
            "raise" => {
                if args.len() != 1 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 1,
                        got: args.len(),
                    }
                    .into());
                }
                let raised_val = args.into_iter().next().expect("checked len");
                if let Some(handler_entry) = handlers.pop() {
                    match handler_entry {
                        HandlerEntry::WithExceptionHandler { handler } => {
                            Ok(State::Apply(handler, vec![raised_val], Rc::clone(env), span))
                        }
                        HandlerEntry::Guard {
                            var,
                            clauses,
                            env: guard_env,
                            guard_k,
                            guard_wind,
                            guard_handlers,
                        } => {
                            // Compute wind transfer ops
                            let common_len = wind
                                .iter()
                                .zip(guard_wind.iter())
                                .take_while(|(a, b)| a.id == b.id)
                                .count();
                            let mut ops: Vec<WindOp> = Vec::new();
                            for entry in wind[common_len..].iter().rev() {
                                ops.push(WindOp::CallOut(entry.out_thunk.clone()));
                            }
                            for entry in &guard_wind[common_len..] {
                                ops.push(WindOp::CallInAndPush(entry.clone()));
                            }

                            // Restore to guard point
                            *k = guard_k;
                            wind.truncate(common_len);
                            *handlers = guard_handlers;

                            // Push clause test frame (will be reached after wind transfer)
                            k.push(Frame::GuardClauseTest {
                                var,
                                raised_val,
                                clauses,
                                env: guard_env,
                            });

                            if ops.is_empty() {
                                Ok(State::Ret(Value::Void))
                            } else {
                                k.push(Frame::WindTransfer {
                                    ops,
                                    final_val: Value::Void,
                                    env: Rc::clone(env),
                                    span,
                                });
                                Ok(State::Ret(Value::Void))
                            }
                        }
                    }
                } else {
                    Err(ErrorKind::UserRaise {
                        value: raised_val.to_display_string(),
                    }
                    .into())
                }
            }
            "with-exception-handler" => {
                if args.len() != 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let handler = args.remove(0);
                let thunk = args.remove(0);

                // Push PopHandler frame so handler is removed when thunk returns
                k.push(Frame::PopHandler);

                // Install handler
                handlers.push(HandlerEntry::WithExceptionHandler { handler });

                // Call thunk
                Ok(State::Apply(thunk, Vec::new(), Rc::clone(env), span))
            }
            _ => {
                let result = apply_builtin(name, &args, env)?;
                Ok(State::Ret(result))
            }
        },
        Value::Lambda {
            params,
            rest_param,
            body,
            closure_env,
        } => {
            let local_env = bind_args(&params, &rest_param, args, &closure_env)?;
            if body.is_empty() {
                Ok(State::Ret(Value::Void))
            } else {
                Ok(begin_seq(&body, &local_env, k))
            }
        }
        Value::Continuation(captured) => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            let val = args.into_iter().next().expect("checked len");
            let (target_frames, target_wind, target_handlers) = captured
                .0
                .downcast_ref::<(Vec<Frame>, Vec<WindEntry>, Vec<HandlerEntry>)>()
                .expect("continuation frame type");

            // Compute common prefix length between current and target wind stacks
            let common_len = wind
                .iter()
                .zip(target_wind.iter())
                .take_while(|(a, b)| a.id == b.id)
                .count();

            // Build wind operations: unwind current (innermost first), rewind target (outermost first)
            let mut ops: Vec<WindOp> = Vec::new();
            // Unwind: from innermost (end) to common prefix
            for entry in wind[common_len..].iter().rev() {
                ops.push(WindOp::CallOut(entry.out_thunk.clone()));
            }
            // Rewind: from common prefix to innermost
            for entry in &target_wind[common_len..] {
                ops.push(WindOp::CallInAndPush(entry.clone()));
            }

            // Restore target continuation stack and handlers
            *k = target_frames.clone();
            *handlers = target_handlers.clone();
            // Truncate wind to common prefix (unwind ops will pop further as they run)
            wind.truncate(common_len);

            if ops.is_empty() {
                Ok(State::Ret(val))
            } else {
                // Push WindTransfer frame onto the target stack and start processing
                k.push(Frame::WindTransfer {
                    ops,
                    final_val: val,
                    env: Rc::clone(env),
                    span,
                });
                // Kick off by returning a dummy value to trigger WindTransfer processing
                Ok(State::Ret(Value::Void))
            }
        }
        Value::SyntaxRules { .. } => Err(ErrorKind::NotAProcedure {
            value: "#<macro>".into(),
        }
        .into()),
        other => Err(ErrorKind::NotAProcedure {
            value: other.to_display_string(),
        }
        .into()),
    }
}

fn bind_args(
    params: &[String],
    rest_param: &Option<String>,
    args: Vec<Value>,
    closure_env: &Rc<Env>,
) -> Result<Rc<Env>, EvalError> {
    let mut names = params.to_vec();
    let vals = match rest_param {
        Some(rest) => {
            if args.len() < params.len() {
                return Err(ErrorKind::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                }
                .into());
            }
            let mut v = args[..params.len()].to_vec();
            names.push(rest.clone());
            v.push(Value::List(args[params.len()..].to_vec()));
            v
        }
        None => {
            if args.len() != params.len() {
                return Err(ErrorKind::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                }
                .into());
            }
            args
        }
    };
    Ok(Env::extend(closure_env, names, vals))
}

// ── quote / lambda / params (unchanged) ──────────────────────

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(ErrorKind::BadSyntax {
            form: "quote".into(),
            message: "expected 1 argument".into(),
        }
        .into());
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::new_str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Value> = elems
                .iter()
                .map(expr_to_value)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "lambda".into(),
            message: "expected (lambda (params) body...)".into(),
        }
        .into());
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, "lambda")?,
        ExprKind::Symbol(s) => (Vec::new(), Some(s.clone())),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "lambda".into(),
                message: "expected parameter list".into(),
            }
            .into())
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        closure_env: Rc::clone(env),
    })
}

fn parse_params(
    param_exprs: &[Expr],
    form: &str,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() || i + 2 != param_exprs.len() {
                    return Err(ErrorKind::BadSyntax {
                        form: form.into(),
                        message: "expected exactly one parameter after dot".into(),
                    }
                    .into());
                }
                match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(rest) => rest_param = Some(rest.clone()),
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: form.into(),
                            message: "rest parameter must be a symbol".into(),
                        }
                        .into())
                    }
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: form.into(),
                    message: "parameter must be a symbol".into(),
                }
                .into())
            }
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let binding_list = match &expr.kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "bindings must be a list".into(),
            }
            .into())
        }
    };
    let mut result = Vec::new();
    for binding in binding_list {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => match &pair[0].kind {
                ExprKind::Symbol(var) => result.push((var.clone(), pair[1].clone())),
                _ => {
                    return Err(ErrorKind::BadSyntax {
                        form: "let".into(),
                        message: "binding name must be a symbol".into(),
                    }
                    .into())
                }
            },
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "let".into(),
                    message: "each binding must be (var init)".into(),
                }
                .into())
            }
        }
    }
    Ok(result)
}

// ── builtins (unchanged) ─────────────────────────────────────

fn apply_builtin(name: &str, args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?" => {
            apply_numeric_builtin(name, args)
        }
        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "list-ref" | "list-tail" | "list?" | "assoc" => {
            apply_list_builtin(name, args)
        }
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?" | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase" => {
            apply_char_cmp_builtin(name, args)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string-set!" | "string->list" | "list->string"
        | "char->integer" | "integer->char" => apply_string_builtin(name, args),
        "not" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_, _))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(elems) if !elems.is_empty()
            ) || matches!(&args[0], Value::DottedPair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            env.write_output(&args[0].to_display_output());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            env.write_output(&args[0].to_display_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 0, got: args.len() }.into());
            }
            env.write_output("\n");
            Ok(Value::Void)
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(scheme_eq(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(args[0] == args[1]))
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(scheme_eqv(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let len = require_int(&args[0])? as usize;
            let fill = if args.len() == 2 {
                args[1].clone()
            } else {
                Value::Integer(0)
            };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let vec_ref = match &args[0] {
                Value::Vector(v) => v,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "vector".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            let v = vec_ref.borrow();
            if idx >= v.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid vector index".into(),
                    got: format!("index {} for vector of length {}", idx, v.len()),
                }
                .into());
            }
            Ok(v[idx].clone())
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount { expected: 3, got: args.len() }.into());
            }
            let vec_ref = match &args[0] {
                Value::Vector(v) => v,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "vector".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            let mut v = vec_ref.borrow_mut();
            if idx >= v.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid vector index".into(),
                    got: format!("index {} for vector of length {}", idx, v.len()),
                }
                .into());
            }
            v[idx] = args[2].clone();
            Ok(Value::Void)
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "vector".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "vector".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(l) => Ok(Value::Vector(Rc::new(RefCell::new(l.clone())))),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "list".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(l) => {
                    let mut rev = l.clone();
                    rev.reverse();
                    Ok(Value::List(rev))
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "list".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_numeric_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b)),
        "-" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            if args.len() == 1 {
                let n = require_int(&args[0])?;
                return Ok(Value::Integer(-n));
            }
            let first = require_int(&args[0])?;
            let rest_sum: i64 = args[1..]
                .iter()
                .map(require_int)
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .sum();
            Ok(Value::Integer(first - rest_sum))
        }
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b)),
        "/" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let first = require_int(&args[0])?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(ErrorKind::DivisionByZero.into());
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for arg in &args[1..] {
                let n = require_int(arg)?;
                if n == 0 {
                    return Err(ErrorKind::DivisionByZero.into());
                }
                result /= n;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "abs" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Integer(n.abs()))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 {
                return Err(ErrorKind::DivisionByZero.into());
            }
            let r = a % b;
            let result = if r != 0 && (r > 0) != (b > 0) { r + b } else { r };
            Ok(Value::Integer(result))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 {
                return Err(ErrorKind::DivisionByZero.into());
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 {
                return Err(ErrorKind::DivisionByZero.into());
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let mut result = require_int(&args[0])?;
            for arg in &args[1..] {
                let n = require_int(arg)?;
                if n < result {
                    result = n;
                }
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let mut result = require_int(&args[0])?;
            for arg in &args[1..] {
                let n = require_int(arg)?;
                if n > result {
                    result = n;
                }
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let base = require_int(&args[0])?;
            let exp = require_int(&args[1])?;
            if exp < 0 {
                return Err(ErrorKind::TypeMismatch {
                    expected: "non-negative exponent".into(),
                    got: exp.to_string(),
                }
                .into());
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n % 2 == 0))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_list_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                Value::DottedPair(_, _) => Ok(Value::DottedPair(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
                _ => Ok(Value::DottedPair(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::DottedPair(a, _) => Ok(*a.clone()),
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "pair".into(),
                    got: args[0].to_display_string(),
                }
                .into()),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                Value::DottedPair(_, b) => Ok(*b.clone()),
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "pair".into(),
                    got: args[0].to_display_string(),
                }
                .into()),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(elems) if elems.is_empty()
            )))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "list".into(),
                    got: args[0].to_display_string(),
                }
                .into()),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(elems) => result.extend(elems.iter().cloned()),
                    _ => {
                        return Err(ErrorKind::TypeMismatch {
                            expected: "list".into(),
                            got: arg.to_display_string(),
                        }
                        .into())
                    }
                }
            }
            Ok(Value::List(result))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let elems = match &args[0] {
                Value::List(l) => l,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "list".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            if idx >= elems.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid list index".into(),
                    got: format!("index {} for list of length {}", idx, elems.len()),
                }
                .into());
            }
            Ok(elems[idx].clone())
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let elems = match &args[0] {
                Value::List(l) => l,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "list".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            if idx > elems.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid list index".into(),
                    got: format!("index {} for list of length {}", idx, elems.len()),
                }
                .into());
            }
            Ok(Value::List(elems[idx..].to_vec()))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let key = &args[0];
            let alist = match &args[1] {
                Value::List(l) => l,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "list".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            for entry in alist {
                match entry {
                    Value::List(pair) if !pair.is_empty() => {
                        if pair[0] == *key {
                            return Ok(entry.clone());
                        }
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_char_cmp_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Boolean(c.is_alphabetic()))
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Boolean(c.is_ascii_digit()))
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Char(c.to_ascii_uppercase()))
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Char(c.to_ascii_lowercase()))
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_char(&args[0])?;
            let b = require_char(&args[1])?;
            Ok(Value::Boolean(a == b))
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_char(&args[0])?;
            let b = require_char(&args[1])?;
            Ok(Value::Boolean(a < b))
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() == *b.borrow()))
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() < *b.borrow()))
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(
                a.borrow().to_lowercase() == b.borrow().to_lowercase(),
            ))
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let s = require_string(&args[0])?;
            Ok(Value::new_str(s.borrow().to_uppercase()))
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let s = require_string(&args[0])?;
            Ok(Value::new_str(s.borrow().to_lowercase()))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::Str(s, _) => result.push_str(&s.borrow()),
                    other => {
                        return Err(ErrorKind::TypeMismatch {
                            expected: "string".into(),
                            got: other.to_display_string(),
                        }
                        .into())
                    }
                }
            }
            Ok(Value::new_str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Integer(s.borrow().len() as i64)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 3,
                    got: args.len(),
                }
                .into());
            }
            let s_ref = match &args[0] {
                Value::Str(s, _) => s,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "string".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let s = s_ref.borrow();
            let start = require_int(&args[1])? as usize;
            let end = require_int(&args[2])? as usize;
            if start > s.len() || end > s.len() || start > end {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid substring indices".into(),
                    got: format!("start={}, end={}, length={}", start, end, s.len()),
                }
                .into());
            }
            Ok(Value::new_str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => match s.borrow().parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::new_str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::new_str(s.clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "symbol".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Symbol(s.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                }
                .into());
            }
            let s_ref = match &args[0] {
                Value::Str(s, _) => s,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "string".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let s = s_ref.borrow();
            let idx = require_int(&args[1])? as usize;
            if idx >= s.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid string index".into(),
                    got: format!("index {} for string of length {}", idx, s.len()),
                }
                .into());
            }
            Ok(Value::Char(s.as_bytes()[idx] as char))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::new_mutable_str(s.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount { expected: 3, got: args.len() }.into());
            }
            match &args[0] {
                Value::Str(s, StringMutability::Mutable) => {
                    let idx = require_int(&args[1])? as usize;
                    let ch = require_char(&args[2])?;
                    let mut borrowed = s.borrow_mut();
                    if idx >= borrowed.len() {
                        return Err(ErrorKind::TypeMismatch {
                            expected: "valid string index".into(),
                            got: format!("index {} for string of length {}", idx, borrowed.len()),
                        }
                        .into());
                    }
                    // SAFETY: replacing a single byte with a single-byte char (ASCII assumption matching string-ref)
                    // Safe because we work on bytes directly
                    let bytes = unsafe { borrowed.as_bytes_mut() };
                    bytes[idx] = ch as u8;
                    Ok(Value::Void)
                }
                Value::Str(_, StringMutability::Immutable) => {
                    Err(ErrorKind::ImmutableString.into())
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Str(s, _) => {
                    let chars: Vec<Value> = s.borrow().chars().map(Value::Char).collect();
                    Ok(Value::List(chars))
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let chars = match &args[0] {
                Value::List(elems) => elems,
                other => return Err(ErrorKind::TypeMismatch {
                    expected: "list".into(),
                    got: other.to_display_string(),
                }.into()),
            };
            let mut s = String::with_capacity(chars.len());
            for v in chars {
                match v {
                    Value::Char(c) => s.push(*c),
                    other => return Err(ErrorKind::TypeMismatch {
                        expected: "char".into(),
                        got: other.to_display_string(),
                    }.into()),
                }
            }
            Ok(Value::new_str(s))
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "char".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(ErrorKind::TypeMismatch {
                    expected: "valid Unicode code point".into(),
                    got: format!("{}", n),
                }.into()),
            }
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(ErrorKind::TypeMismatch {
            expected: "integer".into(),
            got: other.to_display_string(),
        }
        .into()),
    }
}

fn require_char(val: &Value) -> Result<char, EvalError> {
    match val {
        Value::Char(c) => Ok(*c),
        other => Err(ErrorKind::TypeMismatch {
            expected: "char".into(),
            got: other.to_display_string(),
        }
        .into()),
    }
}

fn require_string(val: &Value) -> Result<&std::rc::Rc<std::cell::RefCell<String>>, EvalError> {
    match val {
        Value::Str(s, _) => Ok(s),
        other => Err(ErrorKind::TypeMismatch {
            expected: "string".into(),
            got: other.to_display_string(),
        }
        .into()),
    }
}

/// Scheme `eq?` — identity comparison (not structural equality).
fn scheme_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Void, Value::Void) => true,
        (Value::Str(x, _), Value::Str(y, _)) => Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        _ => false,
    }
}

/// Scheme `eqv?` — like eq? but compares numbers and characters by value.
fn scheme_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Void, Value::Void) => true,
        (Value::Str(x, _), Value::Str(y, _)) => Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        _ => false,
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    let mut result = identity;
    for arg in args {
        let n = require_int(arg)?;
        result = op(result, n)?;
    }
    Ok(Value::Integer(result))
}

fn compare_nums(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::WrongArgCount {
            expected: 2,
            got: args.len(),
        }
        .into());
    }
    for pair in args.windows(2) {
        let a = require_int(&pair[0])?;
        let b = require_int(&pair[1])?;
        if !cmp(a, b) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}
