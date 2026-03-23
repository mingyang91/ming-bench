use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::error::{ErrorKind, EvalError, Span};
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::macros;
use crate::scheme::value::{CapturedCont, Value};

// ── continuation frames ──────────────────────────────────────

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
    MapStep {
        func: Value,
        lists: Vec<Vec<Value>>,
        index: usize,
        results: Vec<Value>,
        env: Rc<Env>,
        span: Span,
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
    let mut state = begin_seq(exprs, env, &mut k);
    run_loop(&mut state, &mut k)
}

// ── main loop ────────────────────────────────────────────────

fn run_loop(state: &mut State, k: &mut Vec<Frame>) -> Result<Value, EvalError> {
    loop {
        *state = match std::mem::replace(state, State::Ret(Value::Void)) {
            State::Eval(expr, env) => step_eval(expr, &env, k)?,
            State::Apply(func, args, env, span) => {
                step_apply(func, args, &env, k, span).map_err(|e| e.with_span(span))?
            }
            State::Ret(val) => match k.pop() {
                Some(frame) => step_ret(val, frame, k)?,
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

fn step_eval(expr: Expr, env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
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
        ExprKind::List(elems) => step_eval_list(elems, env, k, span),
    };
    result.map_err(|e| e.with_span(span))
}

fn step_eval_list(
    mut elems: Vec<Expr>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    span: Span,
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
            "define-syntax" => return step_define_syntax(&elems[1..], env),
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

// ── step_ret ─────────────────────────────────────────────────

fn step_ret(val: Value, frame: Frame, k: &mut Vec<Frame>) -> Result<State, EvalError> {
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
    }
}

// ── step_apply ───────────────────────────────────────────────

fn step_apply(
    func: Value,
    args: Vec<Value>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    span: Span,
) -> Result<State, EvalError> {
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
                let captured = CapturedCont(Rc::new(k.clone()));
                let cont_val = Value::Continuation(captured);
                Ok(State::Apply(proc, vec![cont_val], Rc::clone(env), span))
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
            let frames = captured
                .0
                .downcast_ref::<Vec<Frame>>()
                .expect("continuation frame type");
            *k = frames.clone();
            Ok(State::Ret(val))
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
        | "string-copy" | "string-set!" => apply_string_builtin(name, args),
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
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
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
                    Value::Str(s) => result.push_str(&s.borrow()),
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
                Value::Str(s) => Ok(Value::Integer(s.borrow().len() as i64)),
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
                Value::Str(s) => s,
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
                Value::Str(s) => match s.borrow().parse::<i64>() {
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
                Value::Str(s) => Ok(Value::Symbol(s.borrow().clone())),
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
                Value::Str(s) => s,
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
                Value::Str(s) => Ok(Value::new_str(s.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 3,
                    got: args.len(),
                }
                .into());
            }
            let s_ref = match &args[0] {
                Value::Str(s) => s,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "string".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            let ch = match &args[2] {
                Value::Char(c) => *c,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "char".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let mut s = s_ref.borrow_mut();
            if idx >= s.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid string index".into(),
                    got: format!("index {} for string of length {}", idx, s.len()),
                }
                .into());
            }
            // SAFETY: we verified idx is in bounds and we're replacing a single ASCII-range byte
            unsafe {
                s.as_bytes_mut()[idx] = ch as u8;
            }
            Ok(Value::Void)
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
        Value::Str(s) => Ok(s),
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
        (Value::Str(x), Value::Str(y)) => std::rc::Rc::ptr_eq(x, y),
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
