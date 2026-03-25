use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;
use crate::scheme::EvalError;
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::{Env, LambdaData, MacroData, Value};

/// Result type for tail-call optimization. In tail position, a lambda call
/// returns `TailCall` instead of recursing, allowing the trampoline to loop.
enum TailResult {
    Value(Value),
    TailCall { lambda: Rc<LambdaData>, args: Vec<Value>, pos: String },
}

// ===== CEK Machine for call/cc support =====
// Uses Vec<Frame> stack for performance; captured continuations clone the vec.

type CapturedKont = Vec<Frame>;
type WindStack = Vec<(u64, Value, Value)>; // (id, in_thunk, out_thunk)

#[derive(Clone)]
enum Frame {
    If { cons: Expr, alt: Option<Expr>, env: Env },
    Seq { rest: Vec<Expr>, env: Env },
    Def { name: String, env: Env },
    Set { name: String, env: Env, pos: String },
    /// Evaluate operator; args are unevaluated arg expressions
    Op { args: Vec<Expr>, env: Env, pos: String },
    /// Evaluating arguments right-to-left; done accumulates in reverse order
    Arg { op: Value, done: Vec<Value>, rest: Vec<Expr>, env: Env, pos: String },
    And { rest: Vec<Expr>, env: Env },
    Or { rest: Vec<Expr>, env: Env },
    LetBind { var: String, rest: Vec<(String, Expr)>, done: Vec<(String, Value)>, body: Vec<Expr>, env: Env, pos: String },
    NamedLetBind { name: String, params: Vec<String>, rest: Vec<(String, Expr)>, done: Vec<Value>, body: Vec<Expr>, env: Env, pos: String },
    ProcCheck,
    NotCheck,
    // dynamic-wind frames
    DynWindAfterIn { body: Value, out: Value, wind_id: u64, in_thunk: Value },
    DynWindAfterBody { out: Value, wind_id: u64 },
    DynWindAfterOut { result: Value },
    DynWindTransition { outs: Vec<Value>, rewind_entries: WindStack, target_k: CapturedKont, target_val: Value, target_winds: WindStack },
    PopExceptionHandler,
    GuardClauses { var: String, clauses: Vec<Expr>, env: Env },
    CallWithValuesConsumer { consumer: Value, pos: String },
}

enum CekState {
    Eval(Expr, Env),
    Ret(Value),
}

/// Convert a float to an exact rational (numerator, denominator).
fn float_to_rational(f: f64) -> (i64, i64) {
    if f == f.floor() {
        return (f as i64, 1);
    }
    // Use continued fraction approximation
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    let mut p0: i64 = 0;
    let mut q0: i64 = 1;
    let mut p1: i64 = 1;
    let mut q1: i64 = 0;
    let mut x = f;
    for _ in 0..64 {
        let a = x.floor() as i64;
        let p2 = a * p1 + p0;
        let q2 = a * q1 + q0;
        p0 = p1; q0 = q1;
        p1 = p2; q1 = q2;
        let approx = p1 as f64 / q1 as f64;
        if (approx - f).abs() < 1e-12 {
            break;
        }
        let rem = x - a as f64;
        if rem.abs() < 1e-15 {
            break;
        }
        x = 1.0 / rem;
    }
    (sign * p1, q1)
}

#[derive(Clone)]
enum ExceptionHandlerEntry {
    Procedure(Value),
    Guard {
        var: String,
        clauses: Vec<Expr>,
        env: Env,
        kstack: CapturedKont,
        winds: WindStack,
    },
}

fn common_wind_prefix_len(a: &WindStack, b: &WindStack) -> usize {
    a.iter().zip(b.iter()).take_while(|(x, y)| x.0 == y.0).count()
}

pub struct Evaluator {
    env: Env,
    output: String,
    gensym_counter: u64,
    record_type_counter: u64,
    // call/cc support
    continuations: HashMap<u64, CapturedKont>,
    next_cont_id: u64,
    pending_continuation: Option<(CapturedKont, Value, WindStack)>,
    // dynamic-wind support
    wind_stack: WindStack,
    next_wind_id: u64,
    cont_winds: HashMap<u64, WindStack>,
    // exception handling
    exception_handlers: Vec<ExceptionHandlerEntry>,
    pending_exception: Option<Value>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            env: Env::new(), output: String::new(),
            gensym_counter: 0, record_type_counter: 0,
            continuations: HashMap::new(), next_cont_id: 0,
            pending_continuation: None,
            wind_stack: Vec::new(), next_wind_id: 0, cont_winds: HashMap::new(),
            exception_handlers: Vec::new(), pending_exception: None,
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        let mut env = std::mem::replace(&mut self.env, Env::new());
        let result = self.eval_in_env(expr, &mut env);
        self.env = env;
        result
    }

    /// Evaluate all expressions in a single CEK run (needed for call/cc to capture
    /// the continuation across multiple top-level expressions).
    pub fn eval_all(&mut self, exprs: &[Expr]) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Void);
        }
        let env = self.env.clone();
        let mut state = CekState::Eval(exprs[0].clone(), env.clone());
        let mut kstack: Vec<Frame> = Vec::new();
        if exprs.len() > 1 {
            kstack.push(Frame::Seq { rest: exprs[1..].to_vec(), env });
        }
        self.run_cek(&mut state, &mut kstack)
    }

    // ===== CEK Machine =====

    fn run_cek(&mut self, state: &mut CekState, kstack: &mut Vec<Frame>) -> Result<Value, EvalError> {
        loop {
            let step_result = self.cek_step(state, kstack);
            match step_result {
                Ok(Some(val)) => return Ok(val),
                Ok(None) => {}
                Err(EvalError::ContinuationInvoked) => {
                    let (captured_k, val, captured_winds) = self.pending_continuation.take().unwrap();
                    let common = common_wind_prefix_len(&self.wind_stack, &captured_winds);
                    let outs: Vec<Value> = self.wind_stack[common..].iter().rev().map(|e| e.2.clone()).collect();
                    let rewind_entries: WindStack = captured_winds[common..].to_vec();
                    if outs.is_empty() && rewind_entries.is_empty() {
                        *kstack = captured_k;
                        self.wind_stack = captured_winds;
                        *state = CekState::Ret(val);
                    } else {
                        self.wind_stack.truncate(common);
                        kstack.clear();
                        kstack.push(Frame::DynWindTransition { outs, rewind_entries, target_k: captured_k, target_val: val, target_winds: captured_winds });
                        *state = CekState::Ret(Value::Void);
                    }
                }
                Err(EvalError::ExceptionRaised) => {
                    let exc = self.pending_exception.take().unwrap();
                    if let Some(handler_entry) = self.exception_handlers.pop() {
                        match handler_entry {
                            ExceptionHandlerEntry::Procedure(handler) => {
                                // Call handler in current dynamic context (no unwinding)
                                self.cek_apply(handler, vec![exc], kstack, state, "raise")?;
                            }
                            ExceptionHandlerEntry::Guard { var, clauses, env, kstack: guard_k, winds: guard_winds } => {
                                // Unwind to guard context, then eval clauses
                                let common = common_wind_prefix_len(&self.wind_stack, &guard_winds);
                                let outs: Vec<Value> = self.wind_stack[common..].iter().rev().map(|e| e.2.clone()).collect();
                                let rewind_entries: WindStack = guard_winds[common..].to_vec();
                                let mut target_k = guard_k;
                                target_k.push(Frame::GuardClauses { var, clauses, env });
                                if outs.is_empty() && rewind_entries.is_empty() {
                                    *kstack = target_k;
                                    self.wind_stack = guard_winds;
                                    *state = CekState::Ret(exc);
                                } else {
                                    self.wind_stack.truncate(common);
                                    kstack.clear();
                                    kstack.push(Frame::DynWindTransition {
                                        outs, rewind_entries,
                                        target_k,
                                        target_val: exc,
                                        target_winds: guard_winds,
                                    });
                                    *state = CekState::Ret(Value::Void);
                                }
                            }
                        }
                    } else {
                        return Err(EvalError::Custom(format!("unhandled exception: {}", exc)));
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }

    fn cek_step(&mut self, state: &mut CekState, kstack: &mut Vec<Frame>) -> Result<Option<Value>, EvalError> {
        match std::mem::replace(state, CekState::Ret(Value::Void)) {
            CekState::Eval(expr, env) => {
                self.cek_eval(expr, env, kstack, state)?;
                Ok(None)
            }
            CekState::Ret(val) => {
                if let Some(frame) = kstack.pop() {
                    self.cek_ret(frame, val, kstack, state)?;
                    Ok(None)
                } else {
                    Ok(Some(val))
                }
            }
        }
    }

    fn cek_eval(&mut self, expr: Expr, env: Env, kstack: &mut Vec<Frame>, s: &mut CekState) -> Result<(), EvalError> {
        let pos = expr.pos_str();
        match &expr.kind {
            ExprKind::Integer(n) => *s = CekState::Ret(Value::Integer(*n)),
            ExprKind::Float(f) => *s = CekState::Ret(Value::Float(*f)),
            ExprKind::Rational(n, d) => *s = CekState::Ret(Value::make_rational(*n, *d)),
            ExprKind::Boolean(b) => *s = CekState::Ret(Value::Boolean(*b)),
            ExprKind::Char(c) => *s = CekState::Ret(Value::Char(*c)),
            ExprKind::Str(st) => *s = CekState::Ret(Value::Str(st.clone(), false)),
            ExprKind::Symbol(name) => {
                if let Some(v) = env.get(name) {
                    *s = CekState::Ret(v);
                } else if Self::is_builtin(name) || name == "call/cc" || name == "call-with-current-continuation" {
                    *s = CekState::Ret(Value::Symbol(name.clone()));
                } else {
                    return Err(EvalError::UnboundVariable(format!("{name} at {pos}")));
                }
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    *s = CekState::Ret(Value::Nil);
                } else {
                    self.cek_eval_list(elems, env, kstack, s, &pos)?;
                }
            }
        }
        Ok(())
    }

    fn cek_eval_list(&mut self, elems: &[Expr], env: Env, k: &mut Vec<Frame>, s: &mut CekState, pos: &str) -> Result<(), EvalError> {
        if let ExprKind::Symbol(name) = &elems[0].kind {
            match name.as_str() {
                "if" => {
                    let args = &elems[1..];
                    if args.len() < 2 || args.len() > 3 {
                        return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {pos}")));
                    }
                    k.push(Frame::If { cons: args[1].clone(), alt: args.get(2).cloned(), env: env.clone() });
                    *s = CekState::Eval(args[0].clone(), env);
                    return Ok(());
                }
                "begin" => return self.cek_begin(&elems[1..], env, k, s),
                "define" => return self.cek_define(&elems[1..], env, k, s, pos),
                "set!" => {
                    let args = &elems[1..];
                    if args.len() != 2 { return Err(EvalError::Arity(format!("set!: expected 2 args at {pos}"))); }
                    let nm = match &args[0].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err(EvalError::Parse(format!("set!: expected symbol at {pos}"))),
                    };
                    k.push(Frame::Set { name: nm, env: env.clone(), pos: pos.to_string() });
                    *s = CekState::Eval(args[1].clone(), env);
                    return Ok(());
                }
                "let" => return self.cek_let(&elems[1..], env, k, s, pos),
                "and" => {
                    let args = &elems[1..];
                    if args.is_empty() { *s = CekState::Ret(Value::Boolean(true)); }
                    else if args.len() == 1 { *s = CekState::Eval(args[0].clone(), env); }
                    else {
                        k.push(Frame::And { rest: args[1..].to_vec(), env: env.clone() });
                        *s = CekState::Eval(args[0].clone(), env);
                    }
                    return Ok(());
                }
                "or" => {
                    let args = &elems[1..];
                    if args.is_empty() { *s = CekState::Ret(Value::Boolean(false)); }
                    else if args.len() == 1 { *s = CekState::Eval(args[0].clone(), env); }
                    else {
                        k.push(Frame::Or { rest: args[1..].to_vec(), env: env.clone() });
                        *s = CekState::Eval(args[0].clone(), env);
                    }
                    return Ok(());
                }
                "quote" => {
                    if elems.len() != 2 { return Err(EvalError::Arity(format!("quote: expected 1 argument at {pos}"))); }
                    *s = CekState::Ret(Self::expr_to_value(&elems[1]));
                    return Ok(());
                }
                "lambda" => { *s = CekState::Ret(self.eval_lambda(&elems[1..], &env, pos)?); return Ok(()); }
                "case-lambda" => { *s = CekState::Ret(self.eval_case_lambda(&elems[1..], &env, pos)?); return Ok(()); }
                "procedure?" => {
                    if elems.len() != 2 { return Err(EvalError::Arity(format!("procedure?: expected 1 arg at {pos}"))); }
                    k.push(Frame::ProcCheck);
                    *s = CekState::Eval(elems[1].clone(), env);
                    return Ok(());
                }
                "not" => {
                    if elems.len() != 2 { return Err(EvalError::Arity(format!("not: expected 1 arg at {pos}"))); }
                    k.push(Frame::NotCheck);
                    *s = CekState::Eval(elems[1].clone(), env);
                    return Ok(());
                }
                "guard" => {
                    // (guard (var clause1 clause2 ...) body ...)
                    if elems.len() < 3 { return Err(EvalError::Arity(format!("guard: expected at least 2 args at {pos}"))); }
                    let var_and_clauses = match &elems[1].kind {
                        ExprKind::List(l) if l.len() >= 1 => l,
                        _ => return Err(EvalError::Parse(format!("guard: expected (var clause ...) at {pos}"))),
                    };
                    let var = match &var_and_clauses[0].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err(EvalError::Parse(format!("guard: expected variable name at {pos}"))),
                    };
                    let clauses = var_and_clauses[1..].to_vec();
                    let body = elems[2..].to_vec();
                    // Save guard context
                    let guard_k = k.clone();
                    let guard_winds = self.wind_stack.clone();
                    // Push exception handler
                    self.exception_handlers.push(ExceptionHandlerEntry::Guard {
                        var, clauses, env: env.clone(),
                        kstack: guard_k,
                        winds: guard_winds,
                    });
                    // Push cleanup frame for normal completion
                    k.push(Frame::PopExceptionHandler);
                    // Eval body
                    self.cek_begin(&body, env, k, s)?;
                    return Ok(());
                }
                "cond" | "case" | "do" | "let*" | "letrec" | "letrec*" | "string-set!"
                | "define-syntax" | "define-record-type" => {
                    let mut env_mut = env;
                    *s = CekState::Ret(self.tree_eval_form(name, elems, &mut env_mut, pos)?);
                    return Ok(());
                }
                "syntax-case" | "syntax" | "with-syntax" => {
                    let mut env_mut = env;
                    *s = CekState::Ret(self.tree_eval_form(name, elems, &mut env_mut, pos)?);
                    return Ok(());
                }
                _ => {
                    if let Some(Value::Macro(macro_data)) = env.get(name) {
                        let (expanded, eval_env) = self.expand_macro_only(&macro_data, elems, &env, pos)?;
                        *s = CekState::Eval(expanded, eval_env);
                        return Ok(());
                    }
                    if let Some(Value::TransformerMacro(transformer, def_env)) = env.get(name) {
                        let (expanded, gensym_bindings) = self.expand_transformer_macro_to_expr(&transformer, &def_env, elems, &env, pos)?;
                        // Inject gensym bindings into the use-site env directly
                        for (gs_name, gs_val) in gensym_bindings {
                            env.define(gs_name, gs_val);
                        }
                        *s = CekState::Eval(expanded, env);
                        return Ok(());
                    }
                }
            }
        }
        // Function call: evaluate operator, then args right-to-left
        k.push(Frame::Op { args: elems[1..].to_vec(), env: env.clone(), pos: pos.to_string() });
        *s = CekState::Eval(elems[0].clone(), env);
        Ok(())
    }

    fn cek_ret(&mut self, frame: Frame, val: Value, k: &mut Vec<Frame>, s: &mut CekState) -> Result<(), EvalError> {
        match frame {
            Frame::If { cons, alt, env } => {
                if val.is_truthy() { *s = CekState::Eval(cons, env); }
                else if let Some(alt) = alt { *s = CekState::Eval(alt, env); }
                else { *s = CekState::Ret(Value::Void); }
            }
            Frame::Seq { rest, env } => { self.cek_begin(&rest, env, k, s)?; }
            Frame::Def { name, env } => { env.define(name, val); *s = CekState::Ret(Value::Void); }
            Frame::Set { name, env, pos } => {
                if !env.set(&name, val) { return Err(EvalError::UnboundVariable(format!("{name} at {pos}"))); }
                *s = CekState::Ret(Value::Void);
            }
            Frame::Op { args, env, pos } => {
                if args.is_empty() {
                    self.cek_apply(val, vec![], k, s, &pos)?;
                } else {
                    let last = args.len() - 1;
                    k.push(Frame::Arg { op: val, done: vec![], rest: args[..last].to_vec(), env: env.clone(), pos });
                    *s = CekState::Eval(args[last].clone(), env);
                }
            }
            Frame::Arg { op, mut done, rest, env, pos } => {
                done.push(val);
                if rest.is_empty() {
                    done.reverse();
                    self.cek_apply(op, done, k, s, &pos)?;
                } else {
                    let last = rest.len() - 1;
                    k.push(Frame::Arg { op, done, rest: rest[..last].to_vec(), env: env.clone(), pos });
                    *s = CekState::Eval(rest[last].clone(), env);
                }
            }
            Frame::And { rest, env } => {
                if !val.is_truthy() { *s = CekState::Ret(val); }
                else if rest.len() == 1 { *s = CekState::Eval(rest[0].clone(), env); }
                else {
                    k.push(Frame::And { rest: rest[1..].to_vec(), env: env.clone() });
                    *s = CekState::Eval(rest[0].clone(), env);
                }
            }
            Frame::Or { rest, env } => {
                if val.is_truthy() { *s = CekState::Ret(val); }
                else if rest.len() == 1 { *s = CekState::Eval(rest[0].clone(), env); }
                else {
                    k.push(Frame::Or { rest: rest[1..].to_vec(), env: env.clone() });
                    *s = CekState::Eval(rest[0].clone(), env);
                }
            }
            Frame::LetBind { var, rest, mut done, body, env, pos } => {
                done.push((var, val));
                if rest.is_empty() {
                    let mut new_env = env.child();
                    for (n, v) in done { new_env.define(n, v); }
                    self.cek_begin(&body, new_env, k, s)?;
                } else {
                    let next = rest[0].clone();
                    k.push(Frame::LetBind { var: next.0, rest: rest[1..].to_vec(), done, body, env: env.clone(), pos });
                    *s = CekState::Eval(next.1, env);
                }
            }
            Frame::NamedLetBind { name, params, rest, mut done, body, env, pos } => {
                done.push(val);
                if rest.is_empty() {
                    let loop_env = env.child();
                    let lambda = Rc::new(LambdaData { params: params.clone(), rest_param: None, body: body.clone(), env: loop_env.clone() });
                    loop_env.define(name.clone(), Value::Lambda(lambda));
                    let rec = Rc::new(LambdaData { params, rest_param: None, body, env: loop_env.clone() });
                    loop_env.define(name, Value::Lambda(rec.clone()));
                    self.cek_apply(Value::Lambda(rec), done, k, s, &pos)?;
                } else {
                    let next = rest[0].clone();
                    k.push(Frame::NamedLetBind { name, params, rest: rest[1..].to_vec(), done, body, env: env.clone(), pos });
                    *s = CekState::Eval(next.1, env);
                }
            }
            Frame::ProcCheck => {
                *s = CekState::Ret(Value::Boolean(matches!(val,
                    Value::Lambda(_) | Value::CaseLambda(_) | Value::Continuation(_) |
                    Value::RecordConstructor(..) | Value::RecordPredicate(_) | Value::RecordAccessor(..)
                )));
            }
            Frame::NotCheck => { *s = CekState::Ret(Value::Boolean(!val.is_truthy())); }
            Frame::DynWindAfterIn { body, out, wind_id, in_thunk } => {
                self.wind_stack.push((wind_id, in_thunk, out.clone()));
                k.push(Frame::DynWindAfterBody { out, wind_id });
                self.cek_apply(body, vec![], k, s, "dynamic-wind")?;
            }
            Frame::DynWindAfterBody { out, wind_id } => {
                // val is the body's return value; pop wind entry, call out-thunk
                self.wind_stack.retain(|e| e.0 != wind_id);
                k.push(Frame::DynWindAfterOut { result: val });
                self.cek_apply(out, vec![], k, s, "dynamic-wind")?;
            }
            Frame::DynWindAfterOut { result } => {
                // out-thunk returned; return the body's value
                *s = CekState::Ret(result);
            }
            Frame::DynWindTransition { mut outs, mut rewind_entries, target_k, target_val, target_winds } => {
                // val is the return of the previous thunk (discarded)
                if !outs.is_empty() {
                    let out_thunk = outs.remove(0);
                    k.push(Frame::DynWindTransition { outs, rewind_entries, target_k, target_val, target_winds });
                    self.cek_apply(out_thunk, vec![], k, s, "dynamic-wind")?;
                } else if !rewind_entries.is_empty() {
                    let entry = rewind_entries.remove(0);
                    let in_thunk = entry.1.clone();
                    self.wind_stack.push(entry);
                    k.push(Frame::DynWindTransition { outs: vec![], rewind_entries, target_k, target_val, target_winds });
                    self.cek_apply(in_thunk, vec![], k, s, "dynamic-wind")?;
                } else {
                    *k = target_k;
                    self.wind_stack = target_winds;
                    *s = CekState::Ret(target_val);
                }
            }
            Frame::PopExceptionHandler => {
                self.exception_handlers.pop();
                *s = CekState::Ret(val);
            }
            Frame::GuardClauses { var, clauses, env } => {
                // val is the exception value
                let clause_env = env.child();
                clause_env.define(var, val.clone());
                let mut clause_env_mut = clause_env;
                for clause in &clauses {
                    match &clause.kind {
                        ExprKind::List(parts) if !parts.is_empty() => {
                            if let ExprKind::Symbol(sym) = &parts[0].kind {
                                if sym == "else" {
                                    if parts.len() == 1 {
                                        *s = CekState::Ret(Value::Void);
                                    } else {
                                        let result = self.eval_begin(&parts[1..], &mut clause_env_mut)?;
                                        *s = CekState::Ret(result);
                                    }
                                    return Ok(());
                                }
                            }
                            let test = self.eval_in_env(&parts[0], &mut clause_env_mut)?;
                            if test.is_truthy() {
                                if parts.len() == 1 {
                                    *s = CekState::Ret(test);
                                } else {
                                    let result = self.eval_begin(&parts[1..], &mut clause_env_mut)?;
                                    *s = CekState::Ret(result);
                                }
                                return Ok(());
                            }
                        }
                        _ => {}
                    }
                }
                // No clause matched — re-raise
                self.pending_exception = Some(val);
                return Err(EvalError::ExceptionRaised);
            }
            Frame::CallWithValuesConsumer { consumer, pos } => {
                // val is the producer's result; unpack Values into args
                let args = match val {
                    Value::Values(vals) => vals,
                    single => vec![single],
                };
                self.cek_apply(consumer, args, k, s, &pos)?;
            }
        }
        Ok(())
    }

    fn cek_apply(&mut self, op: Value, args: Vec<Value>, k: &mut Vec<Frame>, s: &mut CekState, pos: &str) -> Result<(), EvalError> {
        match &op {
            Value::Lambda(data) => {
                let call_env = Self::bind_lambda_args(data, args, pos)?;
                self.cek_begin(&data.body, call_env, k, s)?;
            }
            Value::CaseLambda(clauses) => {
                for clause in clauses {
                    let m = if clause.rest_param.is_some() { args.len() >= clause.params.len() } else { args.len() == clause.params.len() };
                    if m { return self.cek_apply(Value::Lambda(clause.clone()), args, k, s, pos); }
                }
                return Err(EvalError::Arity(format!("case-lambda: no clause for {} args at {pos}", args.len())));
            }
            Value::Continuation(id) => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("continuation: expected 1 arg at {pos}"))); }
                let captured_k = self.continuations.get(id).cloned()
                    .ok_or_else(|| EvalError::Custom(format!("invalid continuation at {pos}")))?;
                let captured_winds = self.cont_winds.get(id).cloned().unwrap_or_default();
                let target_val = args.into_iter().next().unwrap();
                let common = common_wind_prefix_len(&self.wind_stack, &captured_winds);
                let outs: Vec<Value> = self.wind_stack[common..].iter().rev().map(|e| e.2.clone()).collect();
                let rewind_entries: WindStack = captured_winds[common..].to_vec();
                if outs.is_empty() && rewind_entries.is_empty() {
                    *k = captured_k;
                    self.wind_stack = captured_winds;
                    *s = CekState::Ret(target_val);
                } else {
                    self.wind_stack.truncate(common);
                    k.clear();
                    k.push(Frame::DynWindTransition { outs, rewind_entries, target_k: captured_k, target_val, target_winds: captured_winds });
                    *s = CekState::Ret(Value::Void);
                }
            }
            Value::Symbol(name) if name == "call/cc" || name == "call-with-current-continuation" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("call/cc: expected 1 arg at {pos}"))); }
                let id = self.next_cont_id;
                self.next_cont_id += 1;
                self.continuations.insert(id, k.clone());
                self.cont_winds.insert(id, self.wind_stack.clone());
                let cont_val = Value::Continuation(id);
                let func = args.into_iter().next().unwrap();
                self.cek_apply(func, vec![cont_val], k, s, pos)?;
            }
            Value::Symbol(name) if name == "dynamic-wind" => {
                if args.len() != 3 { return Err(EvalError::Arity(format!("dynamic-wind: expected 3 args at {pos}"))); }
                let in_thunk = args[0].clone();
                let body = args[1].clone();
                let out = args[2].clone();
                let wind_id = self.next_wind_id;
                self.next_wind_id += 1;
                k.push(Frame::DynWindAfterIn { body, out, wind_id, in_thunk: in_thunk.clone() });
                self.cek_apply(in_thunk, vec![], k, s, pos)?;
            }
            Value::Symbol(name) if name == "raise" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("raise: expected 1 arg at {pos}"))); }
                self.pending_exception = Some(args.into_iter().next().unwrap());
                return Err(EvalError::ExceptionRaised);
            }
            Value::Symbol(name) if name == "with-exception-handler" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("with-exception-handler: expected 2 args at {pos}"))); }
                let handler = args[0].clone();
                let thunk = args[1].clone();
                self.exception_handlers.push(ExceptionHandlerEntry::Procedure(handler));
                k.push(Frame::PopExceptionHandler);
                self.cek_apply(thunk, vec![], k, s, pos)?;
            }
            Value::Symbol(name) if name == "values" => {
                // (values) => Values([])
                // (values x) => x  (single value is transparent)
                // (values x y ...) => Values([x, y, ...])
                if args.len() == 1 {
                    *s = CekState::Ret(args.into_iter().next().unwrap());
                } else {
                    *s = CekState::Ret(Value::Values(args));
                }
            }
            Value::Symbol(name) if name == "call-with-values" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("call-with-values: expected 2 args at {pos}"))); }
                let producer = args[0].clone();
                let consumer = args[1].clone();
                k.push(Frame::CallWithValuesConsumer { consumer, pos: pos.to_string() });
                self.cek_apply(producer, vec![], k, s, pos)?;
            }
            Value::Symbol(name) if name == "apply" => {
                if args.len() < 2 { return Err(EvalError::Arity(format!("apply: expected at least 2 args at {pos}"))); }
                let proc = args[0].clone();
                let last = &args[args.len() - 1];
                let tail = last.to_vec().ok_or_else(|| EvalError::Type(format!("apply: last arg must be list at {pos}")))?;
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                all_args.extend(tail);
                self.cek_apply(proc, all_args, k, s, pos)?;
            }
            Value::Symbol(name) => {
                *s = CekState::Ret(self.apply_builtin(name, &args, pos)?);
            }
            Value::RecordConstructor(type_id, num_fields) => {
                if args.len() != *num_fields { return Err(EvalError::Arity(format!("record constructor: expected {} args at {pos}", num_fields))); }
                *s = CekState::Ret(Value::Record(*type_id, args));
            }
            Value::RecordPredicate(type_id) => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("record predicate: expected 1 arg at {pos}"))); }
                *s = CekState::Ret(Value::Boolean(matches!(&args[0], Value::Record(tid, _) if tid == type_id)));
            }
            Value::RecordAccessor(type_id, field_idx) => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("record accessor: expected 1 arg at {pos}"))); }
                match &args[0] {
                    Value::Record(tid, fields) if tid == type_id => *s = CekState::Ret(fields[*field_idx].clone()),
                    _ => return Err(EvalError::Type(format!("record accessor: wrong type at {pos}"))),
                }
            }
            _ => return Err(EvalError::Type(format!("not a procedure: {} at {pos}", op))),
        }
        Ok(())
    }

    fn cek_begin(&mut self, exprs: &[Expr], env: Env, k: &mut Vec<Frame>, s: &mut CekState) -> Result<(), EvalError> {
        if exprs.is_empty() { *s = CekState::Ret(Value::Void); }
        else if exprs.len() == 1 { *s = CekState::Eval(exprs[0].clone(), env); }
        else {
            k.push(Frame::Seq { rest: exprs[1..].to_vec(), env: env.clone() });
            *s = CekState::Eval(exprs[0].clone(), env);
        }
        Ok(())
    }

    fn cek_define(&mut self, args: &[Expr], env: Env, k: &mut Vec<Frame>, s: &mut CekState, pos: &str) -> Result<(), EvalError> {
        if args.is_empty() { return Err(EvalError::Parse(format!("define: missing args at {pos}"))); }
        match &args[0].kind {
            ExprKind::Symbol(name) => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("define: expected 2 args at {pos}"))); }
                k.push(Frame::Def { name: name.clone(), env: env.clone() });
                *s = CekState::Eval(args[1].clone(), env);
            }
            ExprKind::List(name_and_params) => {
                if name_and_params.is_empty() { return Err(EvalError::Parse(format!("define: empty name at {pos}"))); }
                let name = match &name_and_params[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define: expected symbol at {pos}"))),
                };
                let (params, rest_param) = Self::parse_params(&name_and_params[1..], "define", pos)?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda(Rc::new(LambdaData { params: params.clone(), rest_param: rest_param.clone(), body: body.clone(), env: env.clone() }));
                env.define(name.clone(), lambda);
                env.define(name, Value::Lambda(Rc::new(LambdaData { params, rest_param, body, env: env.clone() })));
                *s = CekState::Ret(Value::Void);
            }
            _ => return Err(EvalError::Parse(format!("define: expected symbol or list at {pos}"))),
        }
        Ok(())
    }

    fn cek_let(&mut self, args: &[Expr], env: Env, k: &mut Vec<Frame>, s: &mut CekState, pos: &str) -> Result<(), EvalError> {
        if args.is_empty() { return Err(EvalError::Parse(format!("let: missing args at {pos}"))); }
        if let ExprKind::Symbol(name) = &args[0].kind {
            if args.len() < 3 { return Err(EvalError::Parse(format!("named let: missing bindings at {pos}"))); }
            let bindings = match &args[1].kind { ExprKind::List(b) => b, _ => return Err(EvalError::Parse(format!("named let: expected bindings at {pos}"))) };
            let mut params = Vec::new();
            let mut pairs = Vec::new();
            for b in bindings {
                match &b.kind {
                    ExprKind::List(pair) if pair.len() == 2 => {
                        if let ExprKind::Symbol(var) = &pair[0].kind { params.push(var.clone()); pairs.push((var.clone(), pair[1].clone())); }
                        else { return Err(EvalError::Parse(format!("let: expected symbol at {pos}"))); }
                    }
                    _ => return Err(EvalError::Parse(format!("let: expected (var expr) at {pos}"))),
                }
            }
            let body = args[2..].to_vec();
            if pairs.is_empty() {
                let le = env.child();
                let ld = Rc::new(LambdaData { params: params.clone(), rest_param: None, body: body.clone(), env: le.clone() });
                le.define(name.clone(), Value::Lambda(ld));
                let rec = Rc::new(LambdaData { params, rest_param: None, body, env: le.clone() });
                le.define(name.clone(), Value::Lambda(rec.clone()));
                self.cek_apply(Value::Lambda(rec), vec![], k, s, pos)?;
            } else {
                let first = pairs[0].clone();
                k.push(Frame::NamedLetBind { name: name.clone(), params, rest: pairs[1..].to_vec(), done: vec![], body, env: env.clone(), pos: pos.to_string() });
                *s = CekState::Eval(first.1, env);
            }
            return Ok(());
        }
        let bindings = match &args[0].kind { ExprKind::List(b) => b, _ => return Err(EvalError::Parse(format!("let: expected bindings at {pos}"))) };
        let mut pairs = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind { pairs.push((var.clone(), pair[1].clone())); }
                    else { return Err(EvalError::Parse(format!("let: expected symbol at {pos}"))); }
                }
                _ => return Err(EvalError::Parse(format!("let: expected (var expr) at {pos}"))),
            }
        }
        let body = args[1..].to_vec();
        if pairs.is_empty() {
            self.cek_begin(&body, env.child(), k, s)?;
        } else {
            let first = pairs[0].clone();
            k.push(Frame::LetBind { var: first.0, rest: pairs[1..].to_vec(), done: vec![], body, env: env.clone(), pos: pos.to_string() });
            *s = CekState::Eval(first.1, env);
        }
        Ok(())
    }

    fn tree_eval_form(&mut self, name: &str, elems: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        match name {
            "cond" => self.eval_cond(&elems[1..], env, pos),
            "case" => self.eval_case(&elems[1..], env, pos),
            "do" => self.eval_do(&elems[1..], env, pos),
            "let*" => self.eval_let_star(&elems[1..], env, pos),
            "letrec" => self.eval_letrec(&elems[1..], env, pos),
            "letrec*" => self.eval_letrec_star(&elems[1..], env, pos),
            "string-set!" => self.eval_string_set(&elems[1..], env, pos),
            "define-syntax" => self.eval_define_syntax(&elems[1..], env, pos),
            "define-record-type" => self.eval_define_record_type(&elems[1..], env, pos),
            "syntax-case" => self.eval_syntax_case(&elems[1..], env, pos),
            "syntax" => self.eval_syntax_template(&elems[1..], env, pos),
            "with-syntax" => self.eval_with_syntax(&elems[1..], env, pos),
            _ => unreachable!(),
        }
    }

    /// Expand a macro without evaluating the result. Returns (expanded_expr, eval_env).
    fn expand_macro_only(&mut self, macro_data: &Rc<MacroData>, input: &[Expr], env: &Env, pos: &str) -> Result<(Expr, Env), EvalError> {
        let md = Rc::clone(macro_data);
        for (pattern, template) in &md.rules {
            if let Some(bindings) = Self::match_pattern(pattern, input, &md.literals) {
                let pat_vars: HashSet<String> = bindings.keys().cloned().collect();
                let mut gensym_map: HashMap<String, String> = HashMap::new();
                let mut gensym_values: Vec<(String, Value)> = Vec::new();
                self.build_gensym_map(template, &pat_vars, &md.def_env, &mut gensym_map, &mut gensym_values);
                let expanded = Self::expand_template(template, &bindings, &gensym_map);
                let mut eval_env = env.child();
                for (gs, val) in gensym_values {
                    eval_env.define(gs, val);
                }
                return Ok((expanded, eval_env));
            }
        }
        Err(EvalError::Parse(format!("no matching macro pattern at {pos}")))
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    fn is_builtin(name: &str) -> bool {
        matches!(name, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
            | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "append" | "number?" | "boolean?" | "string?" | "pair?" | "symbol?"
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "modulo" | "remainder" | "quotient" | "abs" | "min" | "max" | "expt"
            | "list-ref" | "list-tail" | "list?" | "assoc" | "map"
            | "display" | "write" | "newline" | "char?"
            | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol" | "string-ref"
            | "string-copy" | "apply"
            | "string=?" | "string<?" | "string-ci=?"
            | "string-upcase" | "string-downcase"
            | "eq?" | "equal?" | "eqv?"
            | "exact?" | "inexact?" | "exact->inexact" | "inexact->exact"
            | "numerator" | "denominator" | "integer?" | "rational?"
            | "vector" | "make-vector" | "vector-ref" | "vector-set!"
            | "vector-length" | "vector?" | "vector->list" | "list->vector"
            | "error" | "for-each" | "char->integer"
            | "string->list" | "list->string" | "integer->char"
            | "set-car!" | "set-cdr!" | "reverse" | "member" | "assv" | "procedure?"
            | "gcd" | "lcm" | "truncate" | "round"
            | "make-string" | "string" | "string>?" | "string<=?" | "string>=?"
            | "caar" | "cadr" | "cdar" | "cddr" | "caddr" | "cadddr"
            | "caaar" | "caadr" | "cdaar" | "cdadr" | "cddar" | "cadaar"
            | "cadadr" | "caddar" | "caaddr" | "caaaar" | "caaadr"
            | "caadar" | "cdaaar" | "cdaadr" | "cdadar" | "cdaddr"
            | "cddaar" | "cddadr" | "cdddar" | "cddddr"
            | "cadar"
            | "call/cc" | "call-with-current-continuation"
            | "dynamic-wind"
            | "raise" | "with-exception-handler"
            | "values" | "call-with-values"
            | "syntax->datum" | "datum->syntax")
    }

    fn eval_in_env(&mut self, expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
        let pos = expr.pos_str();
        match &expr.kind {
            ExprKind::Integer(n) => Ok(Value::Integer(*n)),
            ExprKind::Float(f) => Ok(Value::Float(*f)),
            ExprKind::Rational(n, d) => Ok(Value::make_rational(*n, *d)),
            ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            ExprKind::Char(c) => Ok(Value::Char(*c)),
            ExprKind::Str(s) => Ok(Value::Str(s.clone(), false)),  // literals are immutable
            ExprKind::Symbol(s) => {
                if let Some(v) = env.get(s) {
                    return Ok(v);
                }
                if Self::is_builtin(s) {
                    return Ok(Value::Symbol(s.clone()));
                }
                Err(EvalError::UnboundVariable(format!("{s} at {pos}")))
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Ok(Value::Nil);
                }
                self.eval_list(elems, env, &pos)
            }
        }
    }

    fn eval_list(&mut self, elems: &[Expr], env: &mut Env, call_pos: &str) -> Result<Value, EvalError> {
        if let ExprKind::Symbol(name) = &elems[0].kind {
            match name.as_str() {
                "define" => return self.eval_define(&elems[1..], env, call_pos),
                "if" => return self.eval_if(&elems[1..], env, call_pos),
                "quote" => return self.eval_quote(&elems[1..], call_pos),
                "lambda" => return self.eval_lambda(&elems[1..], env, call_pos),
                "let" => return self.eval_let(&elems[1..], env, call_pos),
                "begin" => return self.eval_begin(&elems[1..], env),
                "cond" => return self.eval_cond(&elems[1..], env, call_pos),
                "and" => return self.eval_and(&elems[1..], env),
                "or" => return self.eval_or(&elems[1..], env),
                "set!" => return self.eval_set(&elems[1..], env, call_pos),
                "define-syntax" => return self.eval_define_syntax(&elems[1..], env, call_pos),
                "define-record-type" => return self.eval_define_record_type(&elems[1..], env, call_pos),
                "string-set!" => return self.eval_string_set(&elems[1..], env, call_pos),
                "case-lambda" => return self.eval_case_lambda(&elems[1..], env, call_pos),
                "letrec" => return self.eval_letrec(&elems[1..], env, call_pos),
                "letrec*" => return self.eval_letrec_star(&elems[1..], env, call_pos),
                "case" => return self.eval_case(&elems[1..], env, call_pos),
                "do" => return self.eval_do(&elems[1..], env, call_pos),
                "let*" => return self.eval_let_star(&elems[1..], env, call_pos),
                "procedure?" => {
                    if elems.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "procedure?: expected 1 argument, got {} at {call_pos}",
                            elems.len() - 1
                        )));
                    }
                    let val = self.eval_in_env(&elems[1], env)?;
                    return Ok(Value::Boolean(matches!(val,
                        Value::Lambda(_) | Value::CaseLambda(_) | Value::Continuation(_) |
                        Value::RecordConstructor(..) | Value::RecordPredicate(_) | Value::RecordAccessor(..)
                    )));
                }
                "not" => {
                    if elems.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "not: expected 1 argument, got {} at {call_pos}",
                            elems.len() - 1
                        )));
                    }
                    let val = self.eval_in_env(&elems[1], env)?;
                    return Ok(Value::Boolean(!val.is_truthy()));
                }
                "syntax-case" => return self.eval_syntax_case(&elems[1..], env, call_pos),
                "syntax" => return self.eval_syntax_template(&elems[1..], env, call_pos),
                "with-syntax" => return self.eval_with_syntax(&elems[1..], env, call_pos),
                _ => {
                    if let Some(Value::Macro(macro_data)) = env.get(name) {
                        return self.expand_and_eval_macro(&macro_data, elems, env, call_pos);
                    }
                    if let Some(Value::TransformerMacro(transformer, def_env)) = env.get(name) {
                        return self.expand_and_eval_transformer_macro(&transformer, &def_env, elems, env, call_pos);
                    }
                }
            }
        }

        // Evaluate operator
        let op = self.eval_in_env(&elems[0], env)?;

        // Evaluate arguments
        let args: Vec<Value> = elems[1..]
            .iter()
            .map(|e| self.eval_in_env(e, env))
            .collect::<Result<Vec<_>, _>>()?;

        match &op {
            Value::Lambda(data) => {
                self.call_lambda(data, args, call_pos)
            }
            Value::CaseLambda(clauses) => {
                self.call_case_lambda(&clauses, args, call_pos)
            }
            Value::Continuation(id) => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("continuation: expected 1 argument at {call_pos}")));
                }
                let kont = self.continuations.get(id).cloned()
                    .ok_or_else(|| EvalError::Custom(format!("invalid continuation at {call_pos}")))?;
                let winds = self.cont_winds.get(id).cloned().unwrap_or_default();
                self.pending_continuation = Some((kont, args.into_iter().next().unwrap(), winds));
                return Err(EvalError::ContinuationInvoked);
            }
            Value::Symbol(name) => self.apply_builtin(name, &args, call_pos),
            Value::RecordConstructor(type_id, num_fields) => {
                if args.len() != *num_fields {
                    return Err(EvalError::Arity(format!(
                        "record constructor: expected {} arguments, got {} at {call_pos}",
                        num_fields, args.len()
                    )));
                }
                Ok(Value::Record(*type_id, args))
            }
            Value::RecordPredicate(type_id) => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!(
                        "record predicate: expected 1 argument, got {} at {call_pos}",
                        args.len()
                    )));
                }
                Ok(Value::Boolean(matches!(&args[0], Value::Record(tid, _) if tid == type_id)))
            }
            Value::RecordAccessor(type_id, field_idx) => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!(
                        "record accessor: expected 1 argument, got {} at {call_pos}",
                        args.len()
                    )));
                }
                match &args[0] {
                    Value::Record(tid, fields) if tid == type_id => {
                        Ok(fields[*field_idx].clone())
                    }
                    _ => Err(EvalError::Type(format!("record accessor: wrong record type at {call_pos}"))),
                }
            }
            _ => {
                if let ExprKind::Symbol(name) = &elems[0].kind {
                    return self.apply_builtin(name, &args, call_pos);
                }
                Err(EvalError::Type(format!("not a procedure: {} at {call_pos}", op)))
            }
        }
    }

    fn eval_set(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("set!: expected 2 arguments at {pos}")));
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("set!: expected symbol at {pos}"))),
        };
        let val = self.eval_in_env(&args[1], env)?;
        if !env.set(&name, val) {
            return Err(EvalError::UnboundVariable(format!("{name} at {pos}")));
        }
        Ok(Value::Void)
    }

    fn eval_define(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("define: missing arguments at {pos}")));
        }
        match &args[0].kind {
            ExprKind::Symbol(name) => {
                if args.len() != 2 {
                    return Err(EvalError::Arity(format!("define: expected 2 arguments at {pos}")));
                }
                let val = self.eval_in_env(&args[1], env)?;
                env.define(name.clone(), val);
                Ok(Value::Void)
            }
            ExprKind::List(name_and_params) => {
                if name_and_params.is_empty() {
                    return Err(EvalError::Parse(format!("define: empty name list at {pos}")));
                }
                let name = match &name_and_params[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define: expected symbol as name at {pos}"))),
                };
                let (params, rest_param) = Self::parse_params(&name_and_params[1..], "define", pos)?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda(Rc::new(LambdaData {
                    params: params.clone(),
                    rest_param: rest_param.clone(),
                    body: body.clone(),
                    env: env.clone(),
                }));
                env.define(name.clone(), lambda);
                let recursive_lambda = Value::Lambda(Rc::new(LambdaData {
                    params,
                    rest_param,
                    body,
                    env: env.clone(),
                }));
                env.define(name, recursive_lambda);
                Ok(Value::Void)
            }
            _ => Err(EvalError::Parse(format!("define: expected symbol or list at {pos}"))),
        }
    }

    fn eval_if(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {pos}")));
        }
        let cond = self.eval_in_env(&args[0], env)?;
        if cond.is_truthy() {
            self.eval_in_env(&args[1], env)
        } else if args.len() == 3 {
            self.eval_in_env(&args[2], env)
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_quote(&self, args: &[Expr], pos: &str) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("quote: expected 1 argument at {pos}")));
        }
        Ok(Self::expr_to_value(&args[0]))
    }

    fn expr_to_value(expr: &Expr) -> Value {
        match &expr.kind {
            ExprKind::Integer(n) => Value::Integer(*n),
            ExprKind::Float(f) => Value::Float(*f),
            ExprKind::Rational(n, d) => Value::make_rational(*n, *d),
            ExprKind::Boolean(b) => Value::Boolean(*b),
            ExprKind::Char(c) => Value::Char(*c),
            ExprKind::Str(s) => Value::Str(s.clone(), false),
            ExprKind::Symbol(s) => Value::Symbol(s.clone()),
            ExprKind::List(elems) => {
                Value::from_vec(elems.iter().map(Self::expr_to_value).collect())
            }
        }
    }

    fn parse_params(param_exprs: &[Expr], context: &str, pos: &str) -> Result<(Vec<String>, Option<String>), EvalError> {
        let mut params = Vec::new();
        let mut rest_param = None;
        let mut i = 0;
        while i < param_exprs.len() {
            match &param_exprs[i].kind {
                ExprKind::Symbol(s) if s == "." => {
                    if i + 1 != param_exprs.len() - 1 {
                        return Err(EvalError::Parse(format!("{context}: expected exactly one parameter after '.' at {pos}")));
                    }
                    match &param_exprs[i + 1].kind {
                        ExprKind::Symbol(r) => rest_param = Some(r.clone()),
                        _ => return Err(EvalError::Parse(format!("{context}: expected symbol after '.' at {pos}"))),
                    }
                    break;
                }
                ExprKind::Symbol(s) => params.push(s.clone()),
                _ => return Err(EvalError::Parse(format!("{context}: expected symbol as parameter at {pos}"))),
            }
            i += 1;
        }
        Ok((params, rest_param))
    }

    fn eval_lambda(&self, args: &[Expr], env: &Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!("lambda: expected at least 2 arguments at {pos}")));
        }
        let (params, rest_param) = match &args[0].kind {
            ExprKind::List(param_exprs) => Self::parse_params(param_exprs, "lambda", pos)?,
            _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {pos}"))),
        };
        let body = args[1..].to_vec();
        Ok(Value::Lambda(Rc::new(LambdaData {
            params,
            rest_param,
            body,
            env: env.clone(),
        })))
    }

    fn eval_case_lambda(&self, args: &[Expr], env: &Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("case-lambda: expected at least one clause at {pos}")));
        }
        let mut clauses = Vec::new();
        for clause in args {
            let clause_exprs = match &clause.kind {
                ExprKind::List(elems) => elems,
                _ => return Err(EvalError::Parse(format!("case-lambda: expected clause list at {pos}"))),
            };
            if clause_exprs.len() < 2 {
                return Err(EvalError::Parse(format!("case-lambda: clause must have params and body at {pos}")));
            }
            let (params, rest_param) = match &clause_exprs[0].kind {
                ExprKind::List(param_exprs) => Self::parse_params(param_exprs, "case-lambda", pos)?,
                _ => return Err(EvalError::Parse(format!("case-lambda: expected parameter list at {pos}"))),
            };
            let body = clause_exprs[1..].to_vec();
            clauses.push(Rc::new(LambdaData {
                params,
                rest_param,
                body,
                env: env.clone(),
            }));
        }
        Ok(Value::CaseLambda(clauses))
    }

    fn call_case_lambda(&mut self, clauses: &[Rc<LambdaData>], args: Vec<Value>, pos: &str) -> Result<Value, EvalError> {
        for clause in clauses {
            if let Some(ref _rest) = clause.rest_param {
                if args.len() >= clause.params.len() {
                    return self.call_lambda(clause, args, pos);
                }
            } else if args.len() == clause.params.len() {
                return self.call_lambda(clause, args, pos);
            }
        }
        Err(EvalError::Arity(format!(
            "case-lambda: no matching clause for {} arguments at {pos}",
            args.len()
        )))
    }

    fn eval_let(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("let: missing arguments at {pos}")));
        }
        // Named let: (let name ((var init) ...) body...)
        if let ExprKind::Symbol(name) = &args[0].kind {
            if args.len() < 3 {
                return Err(EvalError::Parse(format!("named let: missing bindings or body at {pos}")));
            }
            let bindings = match &args[1].kind {
                ExprKind::List(b) => b,
                _ => return Err(EvalError::Parse(format!("named let: expected bindings list at {pos}"))),
            };
            let mut params = Vec::new();
            let mut init_vals = Vec::new();
            for binding in bindings {
                match &binding.kind {
                    ExprKind::List(pair) if pair.len() == 2 => {
                        if let ExprKind::Symbol(var) = &pair[0].kind {
                            params.push(var.clone());
                            init_vals.push(self.eval_in_env(&pair[1], env)?);
                        } else {
                            return Err(EvalError::Parse(format!("let: expected symbol in binding at {pos}")));
                        }
                    }
                    _ => return Err(EvalError::Parse(format!("let: expected (var expr) binding at {pos}"))),
                }
            }
            let body = args[2..].to_vec();
            let loop_env = env.child();
            let lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                rest_param: None,
                body: body.clone(),
                env: loop_env.clone(),
            }));
            loop_env.define(name.clone(), lambda);
            let recursive_lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                rest_param: None,
                body: body.clone(),
                env: loop_env.clone(),
            }));
            loop_env.define(name.clone(), recursive_lambda);
            let mut call_env = loop_env;
            call_env.push_frame();
            for (p, v) in params.iter().zip(init_vals.into_iter()) {
                call_env.define(p.clone(), v);
            }
            let mut result = Value::Void;
            for expr in &body {
                result = self.eval_in_env(expr, &mut call_env)?;
            }
            return Ok(result);
        }
        // Regular let
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {pos}"))),
        };
        let mut new_env = env.child();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind {
                        let val = self.eval_in_env(&pair[1], env)?;
                        new_env.define(var.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected symbol in binding at {pos}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: expected (var expr) binding at {pos}"))),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = self.eval_in_env(expr, &mut new_env)?;
        }
        Ok(result)
    }

    fn eval_string_set(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 3 {
            return Err(EvalError::Arity(format!("string-set!: expected 3 arguments at {pos}")));
        }
        let str_val = self.eval_in_env(&args[0], env)?;
        let idx = match self.eval_in_env(&args[1], env)? {
            Value::Integer(n) => n as usize,
            _ => return Err(EvalError::Type(format!("string-set!: expected integer index at {pos}"))),
        };
        let ch = match self.eval_in_env(&args[2], env)? {
            Value::Char(c) => c,
            _ => return Err(EvalError::Type(format!("string-set!: expected char at {pos}"))),
        };
        match &str_val {
            Value::Str(_, false) => {
                return Err(EvalError::Type(format!("string-set!: strings are immutable at {pos}")));
            }
            Value::Str(_, true) => {}
            _ => return Err(EvalError::Type(format!("string-set!: expected string at {pos}"))),
        }
        // Must be a variable — mutate in the environment
        let var_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type(format!("string-set!: first argument must be a variable at {pos}"))),
        };
        if let Some(Value::Str(s, true)) = env.get(&var_name) {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type(format!("string-set!: index out of bounds at {pos}")));
            }
            chars[idx] = ch;
            let new_s: String = chars.into_iter().collect();
            if !env.set(&var_name, Value::Str(new_s, true)) {
                return Err(EvalError::UnboundVariable(format!("{var_name} at {pos}")));
            }
            Ok(Value::Void)
        } else {
            Err(EvalError::Type(format!("string-set!: strings are immutable at {pos}")))
        }
    }

    fn eval_let_star(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("let*: missing arguments at {pos}")));
        }
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let*: expected bindings list at {pos}"))),
        };
        let mut new_env = env.child();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind {
                        let val = self.eval_in_env(&pair[1], &mut new_env)?;
                        new_env.define(var.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let*: expected symbol in binding at {pos}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let*: expected (var expr) binding at {pos}"))),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = self.eval_in_env(expr, &mut new_env)?;
        }
        Ok(result)
    }

    fn eval_letrec(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("letrec: missing arguments at {pos}")));
        }
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("letrec: expected bindings list at {pos}"))),
        };
        let mut new_env = env.child();
        // First define all variables as void
        let mut names = Vec::new();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind {
                        names.push(var.clone());
                        new_env.define(var.clone(), Value::Void);
                    } else {
                        return Err(EvalError::Parse(format!("letrec: expected symbol in binding at {pos}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("letrec: expected (var expr) binding at {pos}"))),
            }
        }
        // Then evaluate init expressions in the new env and set
        for (i, binding) in bindings.iter().enumerate() {
            if let ExprKind::List(pair) = &binding.kind {
                let val = self.eval_in_env(&pair[1], &mut new_env)?;
                new_env.set(&names[i], val);
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = self.eval_in_env(expr, &mut new_env)?;
        }
        Ok(result)
    }

    fn eval_letrec_star(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("letrec*: missing arguments at {pos}")));
        }
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("letrec*: expected bindings list at {pos}"))),
        };
        let mut new_env = env.child();
        // Define all variables as void first
        for binding in bindings {
            if let ExprKind::List(pair) = &binding.kind {
                if let ExprKind::Symbol(var) = &pair[0].kind {
                    new_env.define(var.clone(), Value::Void);
                }
            }
        }
        // Evaluate and set sequentially
        for binding in bindings {
            if let ExprKind::List(pair) = &binding.kind {
                if let ExprKind::Symbol(var) = &pair[0].kind {
                    let val = self.eval_in_env(&pair[1], &mut new_env)?;
                    new_env.set(var, val);
                }
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = self.eval_in_env(expr, &mut new_env)?;
        }
        Ok(result)
    }

    fn eval_case(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("case: missing arguments at {pos}")));
        }
        let key = self.eval_in_env(&args[0], env)?;
        for clause in &args[1..] {
            let parts = match &clause.kind {
                ExprKind::List(e) if !e.is_empty() => e,
                _ => return Err(EvalError::Parse(format!("case: expected clause at {pos}"))),
            };
            // Check for else clause
            if let ExprKind::Symbol(s) = &parts[0].kind {
                if s == "else" {
                    let mut result = Value::Void;
                    for expr in &parts[1..] {
                        result = self.eval_in_env(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            // Check datums: ((datum ...) expr ...)
            let datums = match &parts[0].kind {
                ExprKind::List(d) => d,
                _ => return Err(EvalError::Parse(format!("case: expected datum list at {pos}"))),
            };
            for datum in datums {
                let datum_val = Self::expr_to_value(datum);
                if self.eqv(&key, &datum_val) {
                    let mut result = Value::Void;
                    for expr in &parts[1..] {
                        result = self.eval_in_env(expr, env)?;
                    }
                    return Ok(result);
                }
            }
        }
        Ok(Value::Void)
    }

    fn eqv(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
            (Value::Boolean(x), Value::Boolean(y)) => x == y,
            (Value::Char(x), Value::Char(y)) => x == y,
            (Value::Symbol(x), Value::Symbol(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }

    fn eval_do(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        // (do ((var init step) ...) (test expr ...) body ...)
        if args.len() < 2 {
            return Err(EvalError::Parse(format!("do: expected at least 2 arguments at {pos}")));
        }
        let var_clauses = match &args[0].kind {
            ExprKind::List(v) => v,
            _ => return Err(EvalError::Parse(format!("do: expected variable clauses at {pos}"))),
        };
        let test_clause = match &args[1].kind {
            ExprKind::List(t) if !t.is_empty() => t,
            _ => return Err(EvalError::Parse(format!("do: expected test clause at {pos}"))),
        };
        let body = &args[2..];

        // Parse variable clauses
        let mut var_names = Vec::new();
        let mut step_exprs: Vec<Option<Expr>> = Vec::new();
        let mut do_env = env.child();

        for vc in var_clauses {
            let parts = match &vc.kind {
                ExprKind::List(p) if p.len() >= 2 => p,
                _ => return Err(EvalError::Parse(format!("do: expected (var init [step]) at {pos}"))),
            };
            let var_name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("do: expected symbol at {pos}"))),
            };
            let init_val = self.eval_in_env(&parts[1], env)?;
            do_env.define(var_name.clone(), init_val);
            var_names.push(var_name);
            if parts.len() >= 3 {
                step_exprs.push(Some(parts[2].clone()));
            } else {
                step_exprs.push(None);
            }
        }

        loop {
            // Evaluate test
            let test_result = self.eval_in_env(&test_clause[0], &mut do_env)?;
            if test_result.is_truthy() {
                // Evaluate result expressions
                if test_clause.len() > 1 {
                    let mut result = Value::Void;
                    for expr in &test_clause[1..] {
                        result = self.eval_in_env(expr, &mut do_env)?;
                    }
                    return Ok(result);
                }
                return Ok(Value::Void);
            }

            // Evaluate body
            for expr in body {
                self.eval_in_env(expr, &mut do_env)?;
            }

            // Evaluate step expressions with old values (parallel update)
            let new_vals: Vec<Option<Value>> = step_exprs.iter().map(|step| {
                match step {
                    Some(expr) => Ok(Some(self.eval_in_env(expr, &mut do_env)?)),
                    None => Ok(None),
                }
            }).collect::<Result<Vec<_>, EvalError>>()?;

            // Update variables
            for (i, new_val) in new_vals.into_iter().enumerate() {
                if let Some(val) = new_val {
                    do_env.set(&var_names[i], val);
                }
            }
        }
    }

    fn eval_begin(&mut self, args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expr in args {
            result = self.eval_in_env(expr, env)?;
        }
        Ok(result)
    }

    fn eval_cond(&mut self, clauses: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        for clause in clauses {
            match &clause.kind {
                ExprKind::List(parts) if !parts.is_empty() => {
                    if let ExprKind::Symbol(s) = &parts[0].kind {
                        if s == "else" {
                            let mut result = Value::Void;
                            for expr in &parts[1..] {
                                result = self.eval_in_env(expr, env)?;
                            }
                            return Ok(result);
                        }
                    }
                    let test = self.eval_in_env(&parts[0], env)?;
                    if test.is_truthy() {
                        let mut result = test;
                        for expr in &parts[1..] {
                            result = self.eval_in_env(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                _ => return Err(EvalError::Parse(format!("cond: expected clause at {pos}"))),
            }
        }
        Ok(Value::Void)
    }

    fn bind_lambda_args(data: &LambdaData, args: Vec<Value>, pos: &str) -> Result<Env, EvalError> {
        if let Some(ref rest) = data.rest_param {
            if args.len() < data.params.len() {
                return Err(EvalError::Arity(format!(
                    "lambda: expected at least {} arguments, got {} at {pos}",
                    data.params.len(),
                    args.len()
                )));
            }
            let mut call_env = data.env.child();
            for (p, a) in data.params.iter().zip(args.iter()) {
                call_env.define(p.clone(), a.clone());
            }
            call_env.define(rest.clone(), Value::from_vec(args[data.params.len()..].to_vec()));
            Ok(call_env)
        } else {
            if data.params.len() != args.len() {
                return Err(EvalError::Arity(format!(
                    "lambda: expected {} arguments, got {} at {pos}",
                    data.params.len(),
                    args.len()
                )));
            }
            let mut call_env = data.env.child();
            for (p, a) in data.params.iter().zip(args.into_iter()) {
                call_env.define(p.clone(), a);
            }
            Ok(call_env)
        }
    }

    fn call_lambda(&mut self, data: &Rc<LambdaData>, args: Vec<Value>, pos: &str) -> Result<Value, EvalError> {
        let mut cur_lambda: Rc<LambdaData> = Rc::clone(data);
        let mut cur_args = args;
        let mut cur_pos = pos.to_string();

        loop {
            let mut call_env = Self::bind_lambda_args(&cur_lambda, cur_args, &cur_pos)?;
            let body = &cur_lambda.body;
            // Evaluate all but last body expression
            for expr in &body[..body.len() - 1] {
                self.eval_in_env(expr, &mut call_env)?;
            }
            // Evaluate last expression in tail position
            match self.eval_tail(&body[body.len() - 1], &mut call_env)? {
                TailResult::Value(v) => return Ok(v),
                TailResult::TailCall { lambda, args, pos } => {
                    cur_lambda = lambda;
                    cur_args = args;
                    cur_pos = pos;
                }
            }
        }
    }

    // --- Tail-position evaluation for TCO ---

    /// Evaluate an expression in tail position. Returns TailCall for lambda calls
    /// instead of recursing, enabling the trampoline in call_lambda.
    fn eval_tail(&mut self, expr: &Expr, env: &mut Env) -> Result<TailResult, EvalError> {
        let pos = expr.pos_str();
        match &expr.kind {
            ExprKind::Integer(_) | ExprKind::Float(_) | ExprKind::Rational(_, _)
            | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::Str(_) => {
                Ok(TailResult::Value(self.eval_in_env(expr, env)?))
            }
            ExprKind::Symbol(_) => {
                Ok(TailResult::Value(self.eval_in_env(expr, env)?))
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Ok(TailResult::Value(Value::Nil));
                }
                self.eval_tail_list(elems, env, &pos)
            }
        }
    }

    fn eval_tail_list(&mut self, elems: &[Expr], env: &mut Env, call_pos: &str) -> Result<TailResult, EvalError> {
        if let ExprKind::Symbol(name) = &elems[0].kind {
            match name.as_str() {
                "if" => return self.eval_tail_if(&elems[1..], env, call_pos),
                "begin" => return self.eval_tail_begin(&elems[1..], env),
                "cond" => return self.eval_tail_cond(&elems[1..], env, call_pos),
                "and" => return self.eval_tail_and(&elems[1..], env),
                "or" => return self.eval_tail_or(&elems[1..], env),
                "let" => return self.eval_tail_let(&elems[1..], env, call_pos),
                "let*" => return self.eval_tail_let_star(&elems[1..], env, call_pos),
                "letrec" => return self.eval_tail_letrec(&elems[1..], env, call_pos),
                "letrec*" => return self.eval_tail_letrec_star(&elems[1..], env, call_pos),
                "case" => return self.eval_tail_case(&elems[1..], env, call_pos),
                "do" => return self.eval_tail_do(&elems[1..], env, call_pos),
                // For define, set!, quote, lambda, etc. - not tail-call relevant, eval normally
                "define" | "set!" | "quote" | "lambda" | "define-syntax"
                | "define-record-type" | "string-set!" | "case-lambda"
                | "procedure?" | "not"
                | "syntax-case" | "syntax" | "with-syntax" => {
                    return Ok(TailResult::Value(self.eval_list(elems, env, call_pos)?));
                }
                _ => {
                    if let Some(Value::Macro(macro_data)) = env.get(name) {
                        let expanded = self.expand_and_eval_macro(&macro_data, elems, env, call_pos)?;
                        return Ok(TailResult::Value(expanded));
                    }
                    if let Some(Value::TransformerMacro(transformer, def_env)) = env.get(name) {
                        let expanded = self.expand_and_eval_transformer_macro(&transformer, &def_env, elems, env, call_pos)?;
                        return Ok(TailResult::Value(expanded));
                    }
                }
            }
        }

        // Function call in tail position - evaluate operator and args
        let op = self.eval_in_env(&elems[0], env)?;
        let args: Vec<Value> = elems[1..]
            .iter()
            .map(|e| self.eval_in_env(e, env))
            .collect::<Result<Vec<_>, _>>()?;

        match &op {
            Value::Lambda(data) => {
                Ok(TailResult::TailCall {
                    lambda: Rc::clone(data),
                    args,
                    pos: call_pos.to_string(),
                })
            }
            Value::CaseLambda(clauses) => {
                // Find matching clause and return as TailCall
                for clause in clauses {
                    if let Some(ref _rest) = clause.rest_param {
                        if args.len() >= clause.params.len() {
                            return Ok(TailResult::TailCall {
                                lambda: Rc::clone(clause),
                                args,
                                pos: call_pos.to_string(),
                            });
                        }
                    } else if args.len() == clause.params.len() {
                        return Ok(TailResult::TailCall {
                            lambda: Rc::clone(clause),
                            args,
                            pos: call_pos.to_string(),
                        });
                    }
                }
                Err(EvalError::Arity(format!(
                    "case-lambda: no matching clause for {} arguments at {call_pos}",
                    args.len()
                )))
            }
            Value::Continuation(id) => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("continuation: expected 1 argument at {call_pos}")));
                }
                let kont = self.continuations.get(id).cloned()
                    .ok_or_else(|| EvalError::Custom(format!("invalid continuation at {call_pos}")))?;
                let winds = self.cont_winds.get(id).cloned().unwrap_or_default();
                self.pending_continuation = Some((kont, args.into_iter().next().unwrap(), winds));
                return Err(EvalError::ContinuationInvoked);
            }
            Value::Symbol(name) => {
                Ok(TailResult::Value(self.apply_builtin(name, &args, call_pos)?))
            }
            _ => {
                // Records, etc - evaluate normally
                Ok(TailResult::Value(self.eval_list(elems, env, call_pos)?))
            }
        }
    }

    fn eval_tail_if(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {pos}")));
        }
        let cond = self.eval_in_env(&args[0], env)?;
        if cond.is_truthy() {
            self.eval_tail(&args[1], env)
        } else if args.len() == 3 {
            self.eval_tail(&args[2], env)
        } else {
            Ok(TailResult::Value(Value::Void))
        }
    }

    fn eval_tail_begin(&mut self, args: &[Expr], env: &mut Env) -> Result<TailResult, EvalError> {
        if args.is_empty() {
            return Ok(TailResult::Value(Value::Void));
        }
        for expr in &args[..args.len() - 1] {
            self.eval_in_env(expr, env)?;
        }
        self.eval_tail(&args[args.len() - 1], env)
    }

    fn eval_tail_cond(&mut self, clauses: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        for clause in clauses {
            match &clause.kind {
                ExprKind::List(parts) if !parts.is_empty() => {
                    if let ExprKind::Symbol(s) = &parts[0].kind {
                        if s == "else" {
                            if parts.len() <= 1 {
                                return Ok(TailResult::Value(Value::Void));
                            }
                            for expr in &parts[1..parts.len() - 1] {
                                self.eval_in_env(expr, env)?;
                            }
                            return self.eval_tail(&parts[parts.len() - 1], env);
                        }
                    }
                    let test = self.eval_in_env(&parts[0], env)?;
                    if test.is_truthy() {
                        if parts.len() <= 1 {
                            return Ok(TailResult::Value(test));
                        }
                        for expr in &parts[1..parts.len() - 1] {
                            self.eval_in_env(expr, env)?;
                        }
                        return self.eval_tail(&parts[parts.len() - 1], env);
                    }
                }
                _ => return Err(EvalError::Parse(format!("cond: expected clause at {pos}"))),
            }
        }
        Ok(TailResult::Value(Value::Void))
    }

    fn eval_tail_and(&mut self, exprs: &[Expr], env: &mut Env) -> Result<TailResult, EvalError> {
        if exprs.is_empty() {
            return Ok(TailResult::Value(Value::Boolean(true)));
        }
        for expr in &exprs[..exprs.len() - 1] {
            let result = self.eval_in_env(expr, env)?;
            if !result.is_truthy() {
                return Ok(TailResult::Value(result));
            }
        }
        self.eval_tail(&exprs[exprs.len() - 1], env)
    }

    fn eval_tail_or(&mut self, exprs: &[Expr], env: &mut Env) -> Result<TailResult, EvalError> {
        if exprs.is_empty() {
            return Ok(TailResult::Value(Value::Boolean(false)));
        }
        for expr in &exprs[..exprs.len() - 1] {
            let result = self.eval_in_env(expr, env)?;
            if result.is_truthy() {
                return Ok(TailResult::Value(result));
            }
        }
        self.eval_tail(&exprs[exprs.len() - 1], env)
    }

    fn eval_tail_let(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("let: missing arguments at {pos}")));
        }
        // Named let
        if let ExprKind::Symbol(name) = &args[0].kind {
            if args.len() < 3 {
                return Err(EvalError::Parse(format!("named let: missing bindings or body at {pos}")));
            }
            let bindings = match &args[1].kind {
                ExprKind::List(b) => b,
                _ => return Err(EvalError::Parse(format!("named let: expected bindings list at {pos}"))),
            };
            let mut params = Vec::new();
            let mut init_vals = Vec::new();
            for binding in bindings {
                match &binding.kind {
                    ExprKind::List(pair) if pair.len() == 2 => {
                        if let ExprKind::Symbol(var) = &pair[0].kind {
                            params.push(var.clone());
                            init_vals.push(self.eval_in_env(&pair[1], env)?);
                        } else {
                            return Err(EvalError::Parse(format!("let: expected symbol in binding at {pos}")));
                        }
                    }
                    _ => return Err(EvalError::Parse(format!("let: expected (var expr) binding at {pos}"))),
                }
            }
            let body = args[2..].to_vec();
            let loop_env = env.child();
            let lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                rest_param: None,
                body: body.clone(),
                env: loop_env.clone(),
            }));
            loop_env.define(name.clone(), lambda);
            let recursive_lambda = Rc::new(LambdaData {
                params,
                rest_param: None,
                body,
                env: loop_env.clone(),
            });
            loop_env.define(name.clone(), Value::Lambda(Rc::clone(&recursive_lambda)));
            // Return as tail call so the trampoline handles it
            return Ok(TailResult::TailCall {
                lambda: recursive_lambda,
                args: init_vals,
                pos: pos.to_string(),
            });
        }
        // Regular let - evaluate bindings, then body in tail position
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {pos}"))),
        };
        let mut new_env = env.child();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind {
                        let val = self.eval_in_env(&pair[1], env)?;
                        new_env.define(var.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected symbol in binding at {pos}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: expected (var expr) binding at {pos}"))),
            }
        }
        let body = &args[1..];
        if body.is_empty() {
            return Ok(TailResult::Value(Value::Void));
        }
        for expr in &body[..body.len() - 1] {
            self.eval_in_env(expr, &mut new_env)?;
        }
        self.eval_tail(&body[body.len() - 1], &mut new_env)
    }

    fn eval_tail_let_star(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        // Evaluate normally then tail-eval last body expr
        if args.is_empty() {
            return Err(EvalError::Parse(format!("let*: missing arguments at {pos}")));
        }
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let*: expected bindings list at {pos}"))),
        };
        let mut new_env = env.child();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind {
                        let val = self.eval_in_env(&pair[1], &mut new_env)?;
                        new_env.define(var.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let*: expected symbol in binding at {pos}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let*: expected (var expr) binding at {pos}"))),
            }
        }
        let body = &args[1..];
        if body.is_empty() {
            return Ok(TailResult::Value(Value::Void));
        }
        for expr in &body[..body.len() - 1] {
            self.eval_in_env(expr, &mut new_env)?;
        }
        self.eval_tail(&body[body.len() - 1], &mut new_env)
    }

    fn eval_tail_letrec(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        // Set up letrec env, then tail-eval last body expr
        let val = self.eval_letrec(args, env, pos)?;
        Ok(TailResult::Value(val))
    }

    fn eval_tail_letrec_star(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        let val = self.eval_letrec_star(args, env, pos)?;
        Ok(TailResult::Value(val))
    }

    fn eval_tail_case(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        let val = self.eval_case(args, env, pos)?;
        Ok(TailResult::Value(val))
    }

    fn eval_tail_do(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<TailResult, EvalError> {
        let val = self.eval_do(args, env, pos)?;
        Ok(TailResult::Value(val))
    }

    fn call_proc(&mut self, proc: &Value, args: Vec<Value>, pos: &str) -> Result<Value, EvalError> {
        match proc {
            Value::Lambda(data) => self.call_lambda(data, args, pos),
            Value::CaseLambda(clauses) => self.call_case_lambda(clauses, args, pos),
            Value::Continuation(id) => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("continuation: expected 1 argument at {pos}")));
                }
                let kont = self.continuations.get(id).cloned()
                    .ok_or_else(|| EvalError::Custom(format!("invalid continuation at {pos}")))?;
                let winds = self.cont_winds.get(id).cloned().unwrap_or_default();
                self.pending_continuation = Some((kont, args.into_iter().next().unwrap(), winds));
                Err(EvalError::ContinuationInvoked)
            }
            Value::Symbol(name) => self.apply_builtin(name, &args, pos),
            _ => Err(EvalError::Type(format!("apply: not a procedure: {} at {pos}", proc))),
        }
    }

    fn apply_builtin(&mut self, name: &str, args: &[Value], pos: &str) -> Result<Value, EvalError> {
        match name {
            "+" => {
                let mut sum = Value::Integer(0);
                for arg in args {
                    self.expect_number(arg, "+", pos)?;
                    sum = Self::num_add(&sum, arg);
                }
                Ok(sum)
            }
            "-" => {
                if args.is_empty() {
                    return Err(EvalError::Arity(format!("-: expected at least 1 argument at {pos}")));
                }
                self.expect_number(&args[0], "-", pos)?;
                if args.len() == 1 {
                    return Ok(Self::num_negate(&args[0]));
                }
                let mut result = args[0].clone();
                for arg in &args[1..] {
                    self.expect_number(arg, "-", pos)?;
                    result = Self::num_sub(&result, arg);
                }
                Ok(result)
            }
            "*" => {
                let mut product = Value::Integer(1);
                for arg in args {
                    self.expect_number(arg, "*", pos)?;
                    product = Self::num_mul(&product, arg);
                }
                Ok(product)
            }
            "/" => {
                if args.is_empty() {
                    return Err(EvalError::Arity(format!("/: expected at least 1 argument at {pos}")));
                }
                self.expect_number(&args[0], "/", pos)?;
                if args.len() == 1 {
                    return Self::num_div(&Value::Integer(1), &args[0], pos);
                }
                let mut result = args[0].clone();
                for arg in &args[1..] {
                    self.expect_number(arg, "/", pos)?;
                    result = Self::num_div(&result, arg, pos)?;
                }
                Ok(result)
            }
            "<" => self.compare_numbers_generic(args, "<", pos, |o| o == std::cmp::Ordering::Less),
            ">" => self.compare_numbers_generic(args, ">", pos, |o| o == std::cmp::Ordering::Greater),
            "=" => self.compare_numbers_generic(args, "=", pos, |o| o == std::cmp::Ordering::Equal),
            "<=" => self.compare_numbers_generic(args, "<=", pos, |o| o != std::cmp::Ordering::Greater),
            ">=" => self.compare_numbers_generic(args, ">=", pos, |o| o != std::cmp::Ordering::Less),
            "cons" => {
                if args.len() != 2 {
                    return Err(EvalError::Arity(format!("cons: expected 2 arguments at {pos}")));
                }
                Ok(Value::cons(args[0].clone(), args[1].clone()))
            }
            "car" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("car: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::Pair(cell) => Ok(cell.borrow().0.clone()),
                    _ => Err(EvalError::Type(format!("car: expected pair at {pos}"))),
                }
            }
            "cdr" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("cdr: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::Pair(cell) => Ok(cell.borrow().1.clone()),
                    _ => Err(EvalError::Type(format!("cdr: expected pair at {pos}"))),
                }
            }
            "null?" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("null?: expected 1 argument at {pos}")));
                }
                Ok(Value::Boolean(matches!(&args[0], Value::Nil)))
            }
            "list" => Ok(Value::from_vec(args.to_vec())),
            "length" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("length: expected 1 argument at {pos}")));
                }
                match args[0].to_vec() {
                    Some(v) => Ok(Value::Integer(v.len() as i64)),
                    None => Err(EvalError::Type(format!("length: expected proper list at {pos}"))),
                }
            }
            "append" => {
                if args.is_empty() {
                    return Ok(Value::Nil);
                }
                let mut result = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    if i == args.len() - 1 {
                        // Last argument can be non-list (improper list result)
                        match arg.to_vec() {
                            Some(v) => result.extend(v),
                            None => {
                                // Build pair chain with improper tail
                                let mut tail = arg.clone();
                                for val in result.into_iter().rev() {
                                    tail = Value::cons(val, tail);
                                }
                                return Ok(tail);
                            }
                        }
                    } else {
                        match arg.to_vec() {
                            Some(v) => result.extend(v),
                            None => return Err(EvalError::Type(format!("append: expected list at {pos}"))),
                        }
                    }
                }
                Ok(Value::from_vec(result))
            }
            "set-car!" => {
                if args.len() != 2 {
                    return Err(EvalError::Arity(format!("set-car!: expected 2 arguments at {pos}")));
                }
                match &args[0] {
                    Value::Pair(cell) => {
                        cell.borrow_mut().0 = args[1].clone();
                        Ok(Value::Void)
                    }
                    _ => Err(EvalError::Type(format!("set-car!: expected pair at {pos}"))),
                }
            }
            "set-cdr!" => {
                if args.len() != 2 {
                    return Err(EvalError::Arity(format!("set-cdr!: expected 2 arguments at {pos}")));
                }
                match &args[0] {
                    Value::Pair(cell) => {
                        cell.borrow_mut().1 = args[1].clone();
                        Ok(Value::Void)
                    }
                    _ => Err(EvalError::Type(format!("set-cdr!: expected pair at {pos}"))),
                }
            }
            "number?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("number?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(args[0].is_number()))
            }
            "boolean?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("boolean?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
            }
            "string?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Str(_, _))))
            }
            "pair?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("pair?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Pair(_))))
            }
            "symbol?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("symbol?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
            }
            "zero?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("zero?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(match &args[0] {
                    Value::Integer(0) => true,
                    Value::Float(f) => *f == 0.0,
                    Value::Rational(n, _) => *n == 0,
                    _ => false,
                }))
            }
            "modulo" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("modulo: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], "modulo", pos)?;
                let b = self.expect_integer(&args[1], "modulo", pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                let r = a % b;
                let result = if r != 0 && ((r > 0) != (b > 0)) { r + b } else { r };
                Ok(Value::Integer(result))
            }
            "remainder" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("remainder: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], "remainder", pos)?;
                let b = self.expect_integer(&args[1], "remainder", pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::Integer(a % b))
            }
            "quotient" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("quotient: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], "quotient", pos)?;
                let b = self.expect_integer(&args[1], "quotient", pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::Integer(a / b))
            }
            "abs" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("abs: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "abs", pos)?;
                Ok(Value::Integer(n.abs()))
            }
            "min" => {
                if args.is_empty() { return Err(EvalError::Arity(format!("min: expected at least 1 argument at {pos}"))); }
                let mut result = self.expect_integer(&args[0], "min", pos)?;
                for arg in &args[1..] {
                    let n = self.expect_integer(arg, "min", pos)?;
                    if n < result { result = n; }
                }
                Ok(Value::Integer(result))
            }
            "max" => {
                if args.is_empty() { return Err(EvalError::Arity(format!("max: expected at least 1 argument at {pos}"))); }
                let mut result = self.expect_integer(&args[0], "max", pos)?;
                for arg in &args[1..] {
                    let n = self.expect_integer(arg, "max", pos)?;
                    if n > result { result = n; }
                }
                Ok(Value::Integer(result))
            }
            "expt" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("expt: expected 2 arguments at {pos}"))); }
                let base = self.expect_integer(&args[0], "expt", pos)?;
                let exp = self.expect_integer(&args[1], "expt", pos)?;
                Ok(Value::Integer(base.pow(exp as u32)))
            }
            "positive?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("positive?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "positive?", pos)?;
                Ok(Value::Boolean(n > 0))
            }
            "negative?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("negative?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "negative?", pos)?;
                Ok(Value::Boolean(n < 0))
            }
            "odd?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("odd?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "odd?", pos)?;
                Ok(Value::Boolean(n % 2 != 0))
            }
            "even?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("even?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "even?", pos)?;
                Ok(Value::Boolean(n % 2 == 0))
            }
            "list-ref" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("list-ref: expected 2 arguments at {pos}"))); }
                let idx = self.expect_integer(&args[1], "list-ref", pos)? as usize;
                let mut current = args[0].clone();
                for _ in 0..idx {
                    current = match &current {
                        Value::Pair(cell) => cell.borrow().1.clone(),
                        _ => return Err(EvalError::Type(format!("list-ref: index out of bounds at {pos}"))),
                    };
                }
                match &current {
                    Value::Pair(cell) => Ok(cell.borrow().0.clone()),
                    _ => Err(EvalError::Type(format!("list-ref: index out of bounds at {pos}"))),
                }
            }
            "list-tail" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("list-tail: expected 2 arguments at {pos}"))); }
                let idx = self.expect_integer(&args[1], "list-tail", pos)? as usize;
                let mut current = args[0].clone();
                for _ in 0..idx {
                    current = match &current {
                        Value::Pair(cell) => cell.borrow().1.clone(),
                        _ => return Err(EvalError::Type(format!("list-tail: index out of bounds at {pos}"))),
                    };
                }
                Ok(current)
            }
            "list?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("list?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Nil) || args[0].is_proper_list()))
            }
            "assoc" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("assoc: expected 2 arguments at {pos}"))); }
                let key = &args[0];
                let lst = args[1].to_vec().ok_or_else(|| EvalError::Type(format!("assoc: expected list at {pos}")))?;
                for item in &lst {
                    if let Value::Pair(cell) = item {
                        let borrowed = cell.borrow();
                        if borrowed.0 == *key {
                            return Ok(item.clone());
                        }
                    }
                }
                Ok(Value::Boolean(false))
            }
            "map" => {
                if args.len() < 2 { return Err(EvalError::Arity(format!("map: expected at least 2 arguments at {pos}"))); }
                let proc = args[0].clone();
                let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                    a.to_vec().ok_or_else(|| EvalError::Type(format!("map: expected list at {pos}")))
                }).collect::<Result<Vec<_>, _>>()?;
                let len = lists[0].len();
                let mut result = Vec::with_capacity(len);
                for i in 0..len {
                    let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                    result.push(self.call_proc(&proc, call_args, pos)?);
                }
                Ok(Value::from_vec(result))
            }
            "char-alphabetic?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-alphabetic?: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                    _ => Err(EvalError::Type(format!("char-alphabetic?: expected char at {pos}"))),
                }
            }
            "char-numeric?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-numeric?: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                    _ => Err(EvalError::Type(format!("char-numeric?: expected char at {pos}"))),
                }
            }
            "char-upcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-upcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                    _ => Err(EvalError::Type(format!("char-upcase: expected char at {pos}"))),
                }
            }
            "char-downcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-downcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                    _ => Err(EvalError::Type(format!("char-downcase: expected char at {pos}"))),
                }
            }
            "char=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("char=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                    _ => Err(EvalError::Type(format!("char=?: expected chars at {pos}"))),
                }
            }
            "char<?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("char<?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                    _ => Err(EvalError::Type(format!("char<?: expected chars at {pos}"))),
                }
            }
            "string=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a == b)),
                    _ => Err(EvalError::Type(format!("string=?: expected strings at {pos}"))),
                }
            }
            "string<?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string<?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a < b)),
                    _ => Err(EvalError::Type(format!("string<?: expected strings at {pos}"))),
                }
            }
            "string-ci=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string-ci=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                    _ => Err(EvalError::Type(format!("string-ci=?: expected strings at {pos}"))),
                }
            }
            "string-upcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-upcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => Ok(Value::Str(s.to_uppercase(), true)),
                    _ => Err(EvalError::Type(format!("string-upcase: expected string at {pos}"))),
                }
            }
            "string-downcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-downcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => Ok(Value::Str(s.to_lowercase(), true)),
                    _ => Err(EvalError::Type(format!("string-downcase: expected string at {pos}"))),
                }
            }
            "display" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("display: expected 1 argument at {pos}"))); }
                self.output.push_str(&args[0].to_scheme_display());
                Ok(Value::Void)
            }
            "write" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("write: expected 1 argument at {pos}"))); }
                self.output.push_str(&args[0].to_write_string());
                Ok(Value::Void)
            }
            "newline" => {
                if !args.is_empty() { return Err(EvalError::Arity(format!("newline: expected 0 arguments at {pos}"))); }
                self.output.push('\n');
                Ok(Value::Void)
            }
            "char?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
            }
            "string-append" => {
                let mut result = String::new();
                for arg in args {
                    match arg {
                        Value::Str(s, _) => result.push_str(s),
                        _ => return Err(EvalError::Type(format!("string-append: expected string at {pos}"))),
                    }
                }
                Ok(Value::Str(result, true))
            }
            "string-length" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-length: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => Ok(Value::Integer(s.len() as i64)),
                    _ => Err(EvalError::Type(format!("string-length: expected string at {pos}"))),
                }
            }
            "substring" => {
                if args.len() != 3 { return Err(EvalError::Arity(format!("substring: expected 3 arguments at {pos}"))); }
                let s = match &args[0] {
                    Value::Str(s, _) => s,
                    _ => return Err(EvalError::Type(format!("substring: expected string at {pos}"))),
                };
                let start = self.expect_integer(&args[1], "substring", pos)? as usize;
                let end = self.expect_integer(&args[2], "substring", pos)? as usize;
                Ok(Value::Str(s[start..end].to_string(), true))
            }
            "string->number" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string->number: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => {
                        if let Ok(n) = s.parse::<i64>() {
                            Ok(Value::Integer(n))
                        } else if let Ok(f) = s.parse::<f64>() {
                            Ok(Value::Float(f))
                        } else {
                            Ok(Value::Boolean(false))
                        }
                    },
                    _ => Err(EvalError::Type(format!("string->number: expected string at {pos}"))),
                }
            }
            "number->string" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("number->string: expected 1 argument at {pos}"))); }
                self.expect_number(&args[0], "number->string", pos)?;
                Ok(Value::Str(args[0].to_display_string(), true))
            }
            "symbol->string" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("symbol->string: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Symbol(s) => Ok(Value::Str(s.clone(), false)),  // symbol->string returns immutable
                    _ => Err(EvalError::Type(format!("symbol->string: expected symbol at {pos}"))),
                }
            }
            "string->symbol" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string->symbol: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => Ok(Value::Symbol(s.clone())),
                    _ => Err(EvalError::Type(format!("string->symbol: expected string at {pos}"))),
                }
            }
            "string-copy" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-copy: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => Ok(Value::Str(s.clone(), true)),  // copies are mutable
                    _ => Err(EvalError::Type(format!("string-copy: expected string at {pos}"))),
                }
            }
            "apply" => {
                if args.len() < 2 {
                    return Err(EvalError::Arity(format!("apply: expected at least 2 arguments at {pos}")));
                }
                let proc = args[0].clone();
                let last = &args[args.len() - 1];
                let tail = last.to_vec().ok_or_else(|| EvalError::Type(format!("apply: last argument must be a list at {pos}")))?;
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                all_args.extend(tail);
                self.call_proc(&proc, all_args, pos)
            }
            "string-ref" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string-ref: expected 2 arguments at {pos}"))); }
                let s = match &args[0] {
                    Value::Str(s, _) => s,
                    _ => return Err(EvalError::Type(format!("string-ref: expected string at {pos}"))),
                };
                let idx = self.expect_integer(&args[1], "string-ref", pos)? as usize;
                Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                    EvalError::Type(format!("string-ref: index out of bounds at {pos}"))
                })?))
            }
            "eq?" | "eqv?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("{name}: expected 2 arguments at {pos}"))); }
                let result = match (&args[0], &args[1]) {
                    (Value::Vector(a), Value::Vector(b)) => std::ptr::eq(a.as_ptr(), b.as_ptr()),
                    _ => self.eqv(&args[0], &args[1]),
                };
                Ok(Value::Boolean(result))
            }
            "equal?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("equal?: expected 2 arguments at {pos}"))); }
                Ok(Value::Boolean(args[0] == args[1]))
            }
            "exact?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("exact?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(args[0].is_exact()))
            }
            "inexact?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("inexact?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(args[0].is_inexact()))
            }
            "exact->inexact" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("exact->inexact: expected 1 argument at {pos}"))); }
                self.expect_number(&args[0], "exact->inexact", pos)?;
                Ok(Value::Float(Self::num_to_f64(&args[0])))
            }
            "inexact->exact" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("inexact->exact: expected 1 argument at {pos}"))); }
                self.expect_number(&args[0], "inexact->exact", pos)?;
                match &args[0] {
                    Value::Integer(_) => Ok(args[0].clone()),
                    Value::Rational(_, _) => Ok(args[0].clone()),
                    Value::Float(f) => {
                        // Convert float to exact rational
                        // For simple cases like 0.5, find the fraction
                        let (n, d) = float_to_rational(*f);
                        Ok(Value::make_rational(n, d))
                    }
                    _ => unreachable!(),
                }
            }
            "numerator" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("numerator: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Integer(n) => Ok(Value::Integer(*n)),
                    Value::Rational(n, _) => Ok(Value::Integer(*n)),
                    _ => Err(EvalError::Type(format!("numerator: expected rational at {pos}"))),
                }
            }
            "denominator" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("denominator: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Integer(_) => Ok(Value::Integer(1)),
                    Value::Rational(_, d) => Ok(Value::Integer(*d)),
                    _ => Err(EvalError::Type(format!("denominator: expected rational at {pos}"))),
                }
            }
            "integer?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("integer?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
            }
            "rational?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("rational?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(args[0].is_exact()))
            }
            "vector" => {
                Ok(Value::Vector(Rc::new(std::cell::RefCell::new(args.to_vec()))))
            }
            "make-vector" => {
                if args.is_empty() || args.len() > 2 {
                    return Err(EvalError::Arity(format!("make-vector: expected 1-2 arguments at {pos}")));
                }
                let len = self.expect_integer(&args[0], "make-vector", pos)? as usize;
                let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
                Ok(Value::Vector(Rc::new(std::cell::RefCell::new(vec![fill; len]))))
            }
            "vector-ref" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("vector-ref: expected 2 arguments at {pos}"))); }
                match &args[0] {
                    Value::Vector(v) => {
                        let idx = self.expect_integer(&args[1], "vector-ref", pos)? as usize;
                        let v = v.borrow();
                        v.get(idx).cloned().ok_or_else(|| EvalError::Type(format!("vector-ref: index out of bounds at {pos}")))
                    }
                    _ => Err(EvalError::Type(format!("vector-ref: expected vector at {pos}"))),
                }
            }
            "vector-set!" => {
                if args.len() != 3 { return Err(EvalError::Arity(format!("vector-set!: expected 3 arguments at {pos}"))); }
                match &args[0] {
                    Value::Vector(v) => {
                        let idx = self.expect_integer(&args[1], "vector-set!", pos)? as usize;
                        let mut v = v.borrow_mut();
                        if idx >= v.len() {
                            return Err(EvalError::Type(format!("vector-set!: index out of bounds at {pos}")));
                        }
                        v[idx] = args[2].clone();
                        Ok(Value::Void)
                    }
                    _ => Err(EvalError::Type(format!("vector-set!: expected vector at {pos}"))),
                }
            }
            "vector-length" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("vector-length: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                    _ => Err(EvalError::Type(format!("vector-length: expected vector at {pos}"))),
                }
            }
            "vector?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("vector?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
            }
            "vector->list" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("vector->list: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Vector(v) => Ok(Value::from_vec(v.borrow().clone())),
                    _ => Err(EvalError::Type(format!("vector->list: expected vector at {pos}"))),
                }
            }
            "list->vector" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("list->vector: expected 1 argument at {pos}"))); }
                let v = args[0].to_vec().ok_or_else(|| EvalError::Type(format!("list->vector: expected list at {pos}")))?;
                Ok(Value::Vector(Rc::new(RefCell::new(v))))
            }
            "error" => {
                let msg = args.iter().map(|a| a.to_scheme_display()).collect::<Vec<_>>().join("");
                Err(EvalError::Custom(format!("error: {msg}")))
            }
            "for-each" => {
                if args.len() < 2 { return Err(EvalError::Arity(format!("for-each: expected at least 2 arguments at {pos}"))); }
                let proc = args[0].clone();
                let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                    a.to_vec().ok_or_else(|| EvalError::Type(format!("for-each: expected list at {pos}")))
                }).collect::<Result<Vec<_>, _>>()?;
                let len = lists[0].len();
                for i in 0..len {
                    let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                    self.call_proc(&proc, call_args, pos)?;
                }
                Ok(Value::Void)
            }
            "char->integer" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char->integer: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Integer(*c as i64)),
                    _ => Err(EvalError::Type(format!("char->integer: expected char at {pos}"))),
                }
            }
            "integer->char" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("integer->char: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Integer(n) => {
                        let c = char::from_u32(*n as u32).ok_or_else(|| {
                            EvalError::Type(format!("integer->char: invalid code point {n} at {pos}"))
                        })?;
                        Ok(Value::Char(c))
                    }
                    _ => Err(EvalError::Type(format!("integer->char: expected integer at {pos}"))),
                }
            }
            "string->list" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string->list: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s, _) => Ok(Value::from_vec(s.chars().map(Value::Char).collect())),
                    _ => Err(EvalError::Type(format!("string->list: expected string at {pos}"))),
                }
            }
            "list->string" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("list->string: expected 1 argument at {pos}"))); }
                let elems = args[0].to_vec().ok_or_else(|| EvalError::Type(format!("list->string: expected list at {pos}")))?;
                let s: Result<String, _> = elems.iter().map(|v| match v {
                    Value::Char(c) => Ok(*c),
                    _ => Err(EvalError::Type(format!("list->string: expected char in list at {pos}"))),
                }).collect();
                Ok(Value::Str(s?, true))
            }
            "reverse" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("reverse: expected 1 argument at {pos}"))); }
                let v = args[0].to_vec().ok_or_else(|| EvalError::Type(format!("reverse: expected list at {pos}")))?;
                let mut reversed = v;
                reversed.reverse();
                Ok(Value::from_vec(reversed))
            }
            "member" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("member: expected 2 arguments at {pos}"))); }
                let key = &args[0];
                let mut current = args[1].clone();
                loop {
                    match current {
                        Value::Nil => return Ok(Value::Boolean(false)),
                        Value::Pair(cell) => {
                            let (car, cdr) = {
                                let b = cell.borrow();
                                (b.0.clone(), b.1.clone())
                            };
                            if car == *key {
                                return Ok(Value::Pair(cell));
                            }
                            current = cdr;
                        }
                        _ => return Err(EvalError::Type(format!("member: expected list at {pos}"))),
                    }
                }
            }
            "assv" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("assv: expected 2 arguments at {pos}"))); }
                let key = &args[0];
                let lst = args[1].to_vec().ok_or_else(|| EvalError::Type(format!("assv: expected list at {pos}")))?;
                for item in &lst {
                    if let Value::Pair(cell) = item {
                        let borrowed = cell.borrow();
                        if self.eqv(&borrowed.0, key) {
                            return Ok(item.clone());
                        }
                    }
                }
                Ok(Value::Boolean(false))
            }
            "gcd" => {
                if args.is_empty() { return Ok(Value::Integer(0)); }
                let mut result = self.expect_integer(&args[0], "gcd", pos)?.abs();
                for arg in &args[1..] {
                    let b = self.expect_integer(arg, "gcd", pos)?.abs();
                    let (mut a2, mut b2) = (result, b);
                    while b2 != 0 { let t = b2; b2 = a2 % b2; a2 = t; }
                    result = a2;
                }
                Ok(Value::Integer(result))
            }
            "lcm" => {
                if args.is_empty() { return Ok(Value::Integer(1)); }
                let mut result = self.expect_integer(&args[0], "lcm", pos)?.abs();
                for arg in &args[1..] {
                    let b = self.expect_integer(arg, "lcm", pos)?.abs();
                    if result == 0 && b == 0 { result = 0; }
                    else { result = result / { let (mut a2, mut b2) = (result, b); while b2 != 0 { let t = b2; b2 = a2 % b2; a2 = t; } a2 } * b; }
                }
                Ok(Value::Integer(result))
            }
            "truncate" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("truncate: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Integer(n) => Ok(Value::Integer(*n)),
                    Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                    Value::Rational(n, d) => Ok(Value::Integer(n / d)),
                    _ => Err(EvalError::Type(format!("truncate: expected number at {pos}"))),
                }
            }
            "round" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("round: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Integer(n) => Ok(Value::Integer(*n)),
                    Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                    Value::Rational(n, d) => Ok(Value::Integer(((*n as f64) / (*d as f64)).round() as i64)),
                    _ => Err(EvalError::Type(format!("round: expected number at {pos}"))),
                }
            }
            "make-string" => {
                if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity(format!("make-string: expected 1-2 arguments at {pos}"))); }
                let n = self.expect_integer(&args[0], "make-string", pos)? as usize;
                let ch = if args.len() == 2 {
                    match &args[1] { Value::Char(c) => *c, _ => return Err(EvalError::Type(format!("make-string: expected char at {pos}"))) }
                } else { '\0' };
                Ok(Value::Str(std::iter::repeat(ch).take(n).collect(), true))
            }
            "string" => {
                let s: Result<String, _> = args.iter().map(|v| match v {
                    Value::Char(c) => Ok(*c),
                    _ => Err(EvalError::Type(format!("string: expected char at {pos}"))),
                }).collect();
                Ok(Value::Str(s?, true))
            }
            "string>?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string>?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a > b)),
                    _ => Err(EvalError::Type(format!("string>?: expected strings at {pos}"))),
                }
            }
            "string<=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string<=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a <= b)),
                    _ => Err(EvalError::Type(format!("string<=?: expected strings at {pos}"))),
                }
            }
            "string>=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string>=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(a >= b)),
                    _ => Err(EvalError::Type(format!("string>=?: expected strings at {pos}"))),
                }
            }
            "procedure?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("procedure?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0],
                    Value::Lambda(_) | Value::CaseLambda(_) | Value::Continuation(_) |
                    Value::RecordConstructor(..) | Value::RecordPredicate(_) | Value::RecordAccessor(..)
                )))
            }
            "call/cc" | "call-with-current-continuation" => {
                // Fallback for tree-walker context: escape-only continuation
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("call/cc: expected 1 argument at {pos}")));
                }
                let id = self.next_cont_id;
                self.next_cont_id += 1;
                // Use an empty vec as marker for escape-only continuation
                let marker: CapturedKont = vec![];
                self.continuations.insert(id, marker);
                let cont_val = Value::Continuation(id);
                let func = args[0].clone();
                match self.call_proc(&func, vec![cont_val], pos) {
                    Ok(val) => {
                        self.continuations.remove(&id);
                        Ok(val)
                    }
                    Err(EvalError::ContinuationInvoked) => {
                        if let Some((kont, val, winds)) = self.pending_continuation.take() {
                            if kont.is_empty() && self.continuations.get(&id).map_or(false, |k| k.is_empty()) {
                                self.continuations.remove(&id);
                                Ok(val)
                            } else {
                                self.pending_continuation = Some((kont, val, winds));
                                Err(EvalError::ContinuationInvoked)
                            }
                        } else {
                            Err(EvalError::ContinuationInvoked)
                        }
                    }
                    Err(e) => {
                        self.continuations.remove(&id);
                        Err(e)
                    }
                }
            }
            "syntax->datum" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("syntax->datum: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Syntax(data) => Ok(Self::expr_to_value(&data.0)),
                    _ => Err(EvalError::Type(format!("syntax->datum: expected syntax object at {pos}"))),
                }
            }
            "datum->syntax" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("datum->syntax: expected 2 arguments at {pos}"))); }
                // First arg is template id (syntax context) - we ignore it for hygiene simplicity
                // Second arg is the datum to convert
                let expr = Self::value_to_expr(&args[1])?;
                Ok(Value::Syntax(Box::new((expr, vec![]))))
            }
            _ => {
                // Handle c*r combinations (caar, cadr, cdar, cddr, etc.)
                if name.starts_with('c') && name.ends_with('r') && name.len() >= 3 {
                    let ops = &name[1..name.len()-1];
                    if ops.chars().all(|c| c == 'a' || c == 'd') {
                        if args.len() != 1 { return Err(EvalError::Arity(format!("{name}: expected 1 argument at {pos}"))); }
                        let mut val = args[0].clone();
                        for op in ops.chars().rev() {
                            val = match val {
                                Value::Pair(cell) => {
                                    let borrowed = cell.borrow();
                                    if op == 'a' { borrowed.0.clone() } else { borrowed.1.clone() }
                                }
                                _ => return Err(EvalError::Type(format!("{name}: expected pair at {pos}"))),
                            };
                        }
                        return Ok(val);
                    }
                }
                Err(EvalError::UnboundVariable(format!("{name} at {pos}")))
            }
        }
    }

    fn compare_numbers_generic(
        &self,
        args: &[Value],
        name: &str,
        pos: &str,
        cmp: fn(std::cmp::Ordering) -> bool,
    ) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!(
                "{name}: expected at least 2 arguments at {pos}"
            )));
        }
        self.expect_number(&args[0], name, pos)?;
        for i in 1..args.len() {
            self.expect_number(&args[i], name, pos)?;
            let ord = Self::num_cmp(&args[i - 1], &args[i]);
            if !cmp(ord) {
                return Ok(Value::Boolean(false));
            }
        }
        Ok(Value::Boolean(true))
    }

    fn eval_and(&mut self, exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Boolean(true));
        }
        let mut result = Value::Boolean(true);
        for expr in exprs {
            result = self.eval_in_env(expr, env)?;
            if !result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Boolean(false));
        }
        for expr in exprs {
            let result = self.eval_in_env(expr, env)?;
            if result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(Value::Boolean(false))
    }

    fn expect_integer(&self, val: &Value, context: &str, pos: &str) -> Result<i64, EvalError> {
        match val {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!(
                "{context}: expected number, got {val} at {pos}"
            ))),
        }
    }

    fn expect_number(&self, val: &Value, context: &str, pos: &str) -> Result<(), EvalError> {
        if val.is_number() {
            Ok(())
        } else {
            Err(EvalError::Type(format!(
                "{context}: expected number, got {val} at {pos}"
            )))
        }
    }

    /// Add two numeric values, preserving exactness.
    fn num_add(a: &Value, b: &Value) -> Value {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                Value::make_rational(n1 * d2 + n2 * d1, d1 * d2)
            }
            (Value::Integer(x), Value::Rational(n, d)) | (Value::Rational(n, d), Value::Integer(x)) => {
                Value::make_rational(x * d + n, *d)
            }
            _ => Value::Float(a.to_f64().unwrap() + b.to_f64().unwrap()),
        }
    }

    fn num_sub(a: &Value, b: &Value) -> Value {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                Value::make_rational(n1 * d2 - n2 * d1, d1 * d2)
            }
            (Value::Integer(x), Value::Rational(n, d)) => {
                Value::make_rational(x * d - n, *d)
            }
            (Value::Rational(n, d), Value::Integer(x)) => {
                Value::make_rational(n - x * d, *d)
            }
            _ => Value::Float(a.to_f64().unwrap() - b.to_f64().unwrap()),
        }
    }

    fn num_mul(a: &Value, b: &Value) -> Value {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y),
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                Value::make_rational(n1 * n2, d1 * d2)
            }
            (Value::Integer(x), Value::Rational(n, d)) | (Value::Rational(n, d), Value::Integer(x)) => {
                Value::make_rational(x * n, *d)
            }
            _ => Value::Float(a.to_f64().unwrap() * b.to_f64().unwrap()),
        }
    }

    fn num_div(a: &Value, b: &Value, pos: &str) -> Result<Value, EvalError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if *y == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::make_rational(*x, *y))
            }
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                if *n2 == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::make_rational(n1 * d2, d1 * n2))
            }
            (Value::Integer(x), Value::Rational(n, d)) => {
                if *n == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::make_rational(x * d, *n))
            }
            (Value::Rational(n, d), Value::Integer(y)) => {
                if *y == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::make_rational(*n, d * y))
            }
            _ => {
                let bv = b.to_f64().unwrap();
                if bv == 0.0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::Float(a.to_f64().unwrap() / bv))
            }
        }
    }

    fn num_negate(a: &Value) -> Value {
        match a {
            Value::Integer(n) => Value::Integer(-n),
            Value::Float(f) => Value::Float(-f),
            Value::Rational(n, d) => Value::Rational(-n, *d),
            _ => unreachable!(),
        }
    }

    fn num_to_f64(a: &Value) -> f64 {
        a.to_f64().unwrap()
    }

    fn num_cmp(a: &Value, b: &Value) -> std::cmp::Ordering {
        // Compare exactly when possible
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => x.cmp(y),
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                (n1 * d2).cmp(&(n2 * d1))
            }
            (Value::Integer(x), Value::Rational(n, d)) => {
                (x * d).cmp(n)
            }
            (Value::Rational(n, d), Value::Integer(y)) => {
                n.cmp(&(y * d))
            }
            _ => {
                let af = Self::num_to_f64(a);
                let bf = Self::num_to_f64(b);
                af.partial_cmp(&bf).unwrap_or(std::cmp::Ordering::Equal)
            }
        }
    }

    // ── Macro support ──────────────────────────────────────────────

    fn gensym(&mut self, base: &str) -> String {
        self.gensym_counter += 1;
        format!("__{}_gs{}", base, self.gensym_counter)
    }

    fn is_special_form(name: &str) -> bool {
        matches!(name, "define" | "if" | "quote" | "lambda" | "let" | "let*" | "begin"
            | "cond" | "and" | "or" | "set!" | "string-set!" | "not"
            | "define-syntax" | "syntax-rules" | "case-lambda" | "procedure?"
            | "letrec" | "letrec*" | "case" | "do" | "guard"
            | "syntax-case" | "syntax" | "with-syntax")
    }

    fn eval_define_syntax(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("define-syntax: expected 2 arguments at {pos}")));
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("define-syntax: expected symbol at {pos}"))),
        };
        // Check if RHS is (syntax-rules ...) or a transformer expression (lambda)
        let is_syntax_rules = match &args[1].kind {
            ExprKind::List(elems) => !elems.is_empty() && matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "syntax-rules"),
            _ => false,
        };
        if is_syntax_rules {
            let macro_data = self.parse_syntax_rules(&args[1], env, pos)?;
            env.define(name, Value::Macro(Rc::new(macro_data)));
        } else {
            // Evaluate the transformer expression (typically a lambda)
            let def_env = env.clone();
            let transformer = self.eval_in_env(&args[1], env)?;
            env.define(name, Value::TransformerMacro(Box::new(transformer), def_env));
        }
        Ok(Value::Void)
    }

    fn eval_define_record_type(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        // (define-record-type <name> (constructor field-names...) predicate (field accessor)...)
        if args.len() < 3 {
            return Err(EvalError::Parse(format!("define-record-type: too few arguments at {pos}")));
        }
        // args[0] = type name (ignored, just a tag like <point>)
        // args[1] = (constructor-name field-name ...)
        // args[2] = predicate-name
        // args[3..] = (field-name accessor-name) ...

        let type_id = self.record_type_counter;
        self.record_type_counter += 1;

        // Parse constructor: (make-point x y)
        let ctor_elems = match &args[1].kind {
            ExprKind::List(e) => e,
            _ => return Err(EvalError::Parse(format!("define-record-type: expected constructor list at {pos}"))),
        };
        if ctor_elems.is_empty() {
            return Err(EvalError::Parse(format!("define-record-type: empty constructor at {pos}")));
        }
        let ctor_name = match &ctor_elems[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("define-record-type: expected constructor name at {pos}"))),
        };
        let ctor_fields: Vec<String> = ctor_elems[1..].iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse(format!("define-record-type: expected field name at {pos}"))),
        }).collect::<Result<Vec<_>, _>>()?;
        let num_fields = ctor_fields.len();

        // Parse predicate
        let pred_name = match &args[2].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("define-record-type: expected predicate name at {pos}"))),
        };

        // Parse field accessors
        for field_spec in &args[3..] {
            let spec = match &field_spec.kind {
                ExprKind::List(e) => e,
                _ => return Err(EvalError::Parse(format!("define-record-type: expected field spec at {pos}"))),
            };
            if spec.len() < 2 {
                return Err(EvalError::Parse(format!("define-record-type: field spec too short at {pos}")));
            }
            let field_name = match &spec[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define-record-type: expected field name at {pos}"))),
            };
            let accessor_name = match &spec[1].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define-record-type: expected accessor name at {pos}"))),
            };
            // Find the field index in the constructor field list
            let field_idx = ctor_fields.iter().position(|f| f == &field_name)
                .ok_or_else(|| EvalError::Parse(format!(
                    "define-record-type: field {field_name} not in constructor at {pos}"
                )))?;
            env.define(accessor_name, Value::RecordAccessor(type_id, field_idx));
        }

        // Define constructor and predicate
        env.define(ctor_name, Value::RecordConstructor(type_id, num_fields));
        env.define(pred_name, Value::RecordPredicate(type_id));

        Ok(Value::Void)
    }

    fn parse_syntax_rules(&self, expr: &Expr, env: &Env, pos: &str) -> Result<MacroData, EvalError> {
        let elems = match &expr.kind {
            ExprKind::List(e) => e,
            _ => return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {pos}"))),
        };
        if elems.is_empty() || !matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
            return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {pos}")));
        }
        if elems.len() < 3 {
            return Err(EvalError::Parse(format!("syntax-rules: need literals and rules at {pos}")));
        }
        let literals = match &elems[1].kind {
            ExprKind::List(lits) => lits.iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("syntax-rules: expected symbol in literals at {pos}"))),
            }).collect::<Result<Vec<_>, _>>()?,
            _ => return Err(EvalError::Parse(format!("syntax-rules: expected literals list at {pos}"))),
        };
        let mut rules = Vec::new();
        for rule_expr in &elems[2..] {
            match &rule_expr.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    rules.push((pair[0].clone(), pair[1].clone()));
                }
                _ => return Err(EvalError::Parse(format!("syntax-rules: expected (pattern template) at {pos}"))),
            }
        }
        Ok(MacroData { literals, rules, def_env: env.clone() })
    }

    fn expand_and_eval_macro(
        &mut self,
        macro_data: &Rc<MacroData>,
        input: &[Expr],
        env: &mut Env,
        pos: &str,
    ) -> Result<Value, EvalError> {
        let md = Rc::clone(macro_data);
        for (pattern, template) in &md.rules {
            if let Some(bindings) = Self::match_pattern(pattern, input, &md.literals) {
                let pat_vars: HashSet<String> = bindings.keys().cloned().collect();
                let mut gensym_map: HashMap<String, String> = HashMap::new();
                let mut gensym_values: Vec<(String, Value)> = Vec::new();
                self.build_gensym_map(template, &pat_vars, &md.def_env, &mut gensym_map, &mut gensym_values);
                let expanded = Self::expand_template(template, &bindings, &gensym_map);
                let mut eval_env = env.child();
                for (gs, val) in gensym_values {
                    eval_env.define(gs, val);
                }
                return self.eval_in_env(&expanded, &mut eval_env);
            }
        }
        Err(EvalError::Parse(format!("no matching macro pattern at {pos}")))
    }

    fn match_pattern(
        pattern: &Expr,
        input: &[Expr],
        literals: &[String],
    ) -> Option<HashMap<String, Vec<Expr>>> {
        // pattern is (macro-name p1 p2 ...), input is [macro-name, arg1, arg2, ...]
        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => e,
            _ => return None,
        };
        let pat_tail = &pat_elems[1..];
        let inp_tail = &input[1..];
        let mut bindings: HashMap<String, Vec<Expr>> = HashMap::new();
        let mut pi = 0;
        let mut ii = 0;
        while pi < pat_tail.len() {
            let has_ellipsis = pi + 1 < pat_tail.len()
                && matches!(&pat_tail[pi + 1].kind, ExprKind::Symbol(s) if s == "...");
            if has_ellipsis {
                let var = match &pat_tail[pi].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return None,
                };
                let remaining_after = pat_tail.len() - pi - 2;
                let available = inp_tail.len().checked_sub(ii + remaining_after)?;
                bindings.insert(var, inp_tail[ii..ii + available].to_vec());
                ii += available;
                pi += 2;
            } else {
                if ii >= inp_tail.len() { return None; }
                match &pat_tail[pi].kind {
                    ExprKind::Symbol(s) if literals.contains(s) => {
                        match &inp_tail[ii].kind {
                            ExprKind::Symbol(is) if is == s => {}
                            _ => return None,
                        }
                    }
                    ExprKind::Symbol(s) => {
                        bindings.insert(s.clone(), vec![inp_tail[ii].clone()]);
                    }
                    _ => return None,
                }
                pi += 1;
                ii += 1;
            }
        }
        if ii != inp_tail.len() { return None; }
        Some(bindings)
    }

    fn build_gensym_map(
        &mut self,
        template: &Expr,
        pat_vars: &HashSet<String>,
        def_env: &Env,
        gensym_map: &mut HashMap<String, String>,
        gensym_values: &mut Vec<(String, Value)>,
    ) {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if pat_vars.contains(s) || s == "..." || Self::is_special_form(s)
                    || Self::is_builtin(s) || gensym_map.contains_key(s)
                {
                    return;
                }
                if let Some(val) = def_env.get(s) {
                    if matches!(val, Value::Macro(_)) { return; }
                    let gs = self.gensym(s);
                    gensym_values.push((gs.clone(), val));
                    gensym_map.insert(s.clone(), gs);
                } else {
                    let gs = self.gensym(s);
                    gensym_map.insert(s.clone(), gs);
                }
            }
            ExprKind::List(elems) => {
                for e in elems {
                    self.build_gensym_map(e, pat_vars, def_env, gensym_map, gensym_values);
                }
            }
            _ => {}
        }
    }

    fn expand_template(
        template: &Expr,
        bindings: &HashMap<String, Vec<Expr>>,
        gensym_map: &HashMap<String, String>,
    ) -> Expr {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if let Some(exprs) = bindings.get(s) {
                    if exprs.len() == 1 {
                        return exprs[0].clone();
                    }
                }
                if let Some(gs) = gensym_map.get(s) {
                    return Expr { kind: ExprKind::Symbol(gs.clone()), line: 0, col: 0 };
                }
                template.clone()
            }
            ExprKind::List(elems) => {
                let mut expanded = Vec::new();
                let mut i = 0;
                while i < elems.len() {
                    let has_ellipsis = i + 1 < elems.len()
                        && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...");
                    if has_ellipsis {
                        if let ExprKind::Symbol(s) = &elems[i].kind {
                            if let Some(exprs) = bindings.get(s) {
                                for e in exprs {
                                    expanded.push(e.clone());
                                }
                            }
                        }
                        i += 2;
                    } else {
                        expanded.push(Self::expand_template(&elems[i], bindings, gensym_map));
                        i += 1;
                    }
                }
                Expr { kind: ExprKind::List(expanded), line: template.line, col: template.col }
            }
            _ => template.clone(),
        }
    }

    // ===== syntax-case support =====

    /// Convert Value to Expr (for datum->syntax)
    fn value_to_expr(val: &Value) -> Result<Expr, EvalError> {
        let kind = match val {
            Value::Integer(n) => ExprKind::Integer(*n),
            Value::Float(f) => ExprKind::Float(*f),
            Value::Rational(n, d) => ExprKind::Rational(*n, *d),
            Value::Boolean(b) => ExprKind::Boolean(*b),
            Value::Char(c) => ExprKind::Char(*c),
            Value::Str(s, _) => ExprKind::Str(s.clone()),
            Value::Symbol(s) => ExprKind::Symbol(s.clone()),
            Value::Nil => ExprKind::List(vec![]),
            Value::Pair(_) => {
                let elems = val.to_vec().ok_or_else(|| EvalError::Type("datum->syntax: improper list".into()))?;
                ExprKind::List(elems.iter().map(Self::value_to_expr).collect::<Result<Vec<_>, _>>()?)
            }
            Value::Syntax(data) => return Ok(data.0.clone()),
            _ => return Err(EvalError::Type(format!("datum->syntax: cannot convert {}", val))),
        };
        Ok(Expr { kind, line: 0, col: 0 })
    }

    /// Expand and evaluate a syntax-case transformer macro (tree-walker path)
    fn expand_and_eval_transformer_macro(
        &mut self,
        transformer: &Value,
        def_env: &Env,
        input: &[Expr],
        env: &mut Env,
        pos: &str,
    ) -> Result<Value, EvalError> {
        let (expanded, gensym_bindings) = self.expand_transformer_macro_to_expr(transformer, def_env, input, env, pos)?;
        // Define gensym bindings directly in the use-site env (gensym names are unique)
        for (gs_name, gs_val) in gensym_bindings {
            env.define(gs_name, gs_val);
        }
        self.eval_in_env(&expanded, env)
    }

    /// Expand a syntax-case transformer macro to an Expr and gensym bindings.
    fn expand_transformer_macro_to_expr(
        &mut self,
        transformer: &Value,
        _def_env: &Env,
        input: &[Expr],
        _env: &Env,
        pos: &str,
    ) -> Result<(Expr, Vec<(String, Value)>), EvalError> {
        // Convert input form to a Syntax value
        let input_expr = Expr { kind: ExprKind::List(input.to_vec()), line: 0, col: 0 };
        let stx_val = Value::Syntax(Box::new((input_expr, vec![])));

        // Call the transformer with the syntax object
        let result = self.call_transformer(transformer, stx_val, pos)?;

        // Extract the expanded Expr from the Syntax result
        match result {
            Value::Syntax(data) => Ok(*data),
            _ => Err(EvalError::Type(format!("syntax-case transformer must return syntax object at {pos}"))),
        }
    }

    /// Expand a syntax-case transformer macro without evaluating. Returns (expanded_expr, eval_env).
    fn expand_transformer_macro_only(
        &mut self,
        transformer: &Value,
        def_env: &Env,
        input: &[Expr],
        env: &Env,
        pos: &str,
    ) -> Result<(Expr, Env), EvalError> {
        let (expr, gensym_bindings) = self.expand_transformer_macro_to_expr(transformer, def_env, input, env, pos)?;
        let mut eval_env = env.child();
        for (gs_name, gs_val) in gensym_bindings {
            eval_env.define(gs_name, gs_val);
        }
        Ok((expr, eval_env))
    }

    /// Call a transformer procedure with a syntax argument
    fn call_transformer(&mut self, transformer: &Value, arg: Value, pos: &str) -> Result<Value, EvalError> {
        self.call_proc(transformer, vec![arg], pos)
    }

    /// Evaluate (syntax-case stx-expr (literals) clause ...)
    /// Each clause is (pattern body) or (pattern fender body)
    fn eval_syntax_case(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 3 {
            return Err(EvalError::Parse(format!("syntax-case: expected at least 3 args at {pos}")));
        }
        // Evaluate the syntax expression
        let stx_val = self.eval_in_env(&args[0], env)?;
        let stx_expr = match &stx_val {
            Value::Syntax(data) => data.0.clone(),
            _ => return Err(EvalError::Type(format!("syntax-case: expected syntax object at {pos}"))),
        };

        // Get literals
        let literals = match &args[1].kind {
            ExprKind::List(lits) => lits.iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("syntax-case: expected symbol in literals at {pos}"))),
            }).collect::<Result<Vec<_>, _>>()?,
            _ => return Err(EvalError::Parse(format!("syntax-case: expected literals list at {pos}"))),
        };

        // Get the input as a list of exprs for matching
        let input_elems = match &stx_expr.kind {
            ExprKind::List(e) => e.clone(),
            _ => vec![stx_expr.clone()],
        };

        // Try each clause
        for clause_expr in &args[2..] {
            let clause = match &clause_expr.kind {
                ExprKind::List(c) => c,
                _ => return Err(EvalError::Parse(format!("syntax-case: expected clause at {pos}"))),
            };
            if clause.len() < 2 {
                return Err(EvalError::Parse(format!("syntax-case: clause too short at {pos}")));
            }

            let pattern = &clause[0];
            let (fender, body) = if clause.len() == 3 {
                (Some(&clause[1]), &clause[2])
            } else {
                (None, &clause[1])
            };

            // Try to match pattern against input
            if let Some(bindings) = self.match_syntax_case_pattern(pattern, &input_elems, &literals) {
                // Create new env with pattern variable bindings
                let mut clause_env = env.child();
                for (name, exprs) in &bindings {
                    if exprs.len() == 1 {
                        clause_env.define(name.clone(), Value::Syntax(Box::new((exprs[0].clone(), vec![]))));
                    } else {
                        // Ellipsis binding: store as a list of Syntax values
                        let list_val = Value::from_vec(
                            exprs.iter().map(|e| Value::Syntax(Box::new((e.clone(), vec![])))).collect()
                        );
                        clause_env.define(name.clone(), list_val);
                    }
                }

                // Check fender if present
                if let Some(fender_expr) = fender {
                    let fender_result = self.eval_in_env(fender_expr, &mut clause_env)?;
                    if !fender_result.is_truthy() {
                        continue;
                    }
                }

                // Evaluate body
                return self.eval_in_env(body, &mut clause_env);
            }
        }

        Err(EvalError::Parse(format!("syntax-case: no matching pattern at {pos}")))
    }

    /// Match a syntax-case pattern against input expressions.
    /// Returns bindings: var name → list of matched Exprs (single elem for non-ellipsis).
    fn match_syntax_case_pattern(
        &self,
        pattern: &Expr,
        input: &[Expr],
        literals: &[String],
    ) -> Option<HashMap<String, Vec<Expr>>> {
        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => e,
            _ => return None,
        };

        let mut bindings: HashMap<String, Vec<Expr>> = HashMap::new();
        let mut pi = 0;
        let mut ii = 0;

        while pi < pat_elems.len() {
            let has_ellipsis = pi + 1 < pat_elems.len()
                && matches!(&pat_elems[pi + 1].kind, ExprKind::Symbol(s) if s == "...");

            if has_ellipsis {
                match &pat_elems[pi].kind {
                    ExprKind::Symbol(s) if s == "_" => {
                        // Wildcard with ellipsis: skip remaining
                        let remaining_after = pat_elems.len() - pi - 2;
                        let available = input.len().checked_sub(ii + remaining_after)?;
                        ii += available;
                    }
                    ExprKind::Symbol(s) => {
                        let remaining_after = pat_elems.len() - pi - 2;
                        let available = input.len().checked_sub(ii + remaining_after)?;
                        bindings.insert(s.clone(), input[ii..ii + available].to_vec());
                        ii += available;
                    }
                    _ => return None,
                }
                pi += 2;
            } else {
                if ii >= input.len() { return None; }
                match &pat_elems[pi].kind {
                    ExprKind::Symbol(s) if s == "_" => {
                        // Wildcard: skip one
                    }
                    ExprKind::Symbol(s) if literals.contains(s) => {
                        match &input[ii].kind {
                            ExprKind::Symbol(is) if is == s => {}
                            _ => return None,
                        }
                    }
                    ExprKind::Symbol(s) => {
                        bindings.insert(s.clone(), vec![input[ii].clone()]);
                    }
                    _ => return None,
                }
                pi += 1;
                ii += 1;
            }
        }

        if ii != input.len() { return None; }
        Some(bindings)
    }

    /// Evaluate (syntax template) - construct a syntax object from a template.
    /// Substitutes pattern variables (bound to Syntax values in env) and
    /// applies hygiene (gensyms) for free identifiers.
    fn eval_syntax_template(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("syntax: expected 1 argument at {pos}")));
        }
        let template = &args[0];

        // Collect which symbols in env are bound to Syntax values (pattern variables)
        let mut syntax_vars: HashSet<String> = HashSet::new();
        let mut syntax_list_vars: HashSet<String> = HashSet::new();
        self.collect_syntax_vars(template, env, &mut syntax_vars, &mut syntax_list_vars);

        // Build gensym map for hygiene (free identifiers in template)
        let all_pattern_vars: HashSet<String> = syntax_vars.iter().chain(syntax_list_vars.iter()).cloned().collect();
        let mut gensym_map: HashMap<String, String> = HashMap::new();
        let mut gensym_values: Vec<(String, Value)> = Vec::new();
        self.build_syntax_gensym_map(template, &all_pattern_vars, env, &mut gensym_map, &mut gensym_values);

        // Expand the template
        let expanded = self.expand_syntax_template(template, env, &syntax_vars, &syntax_list_vars, &gensym_map);

        Ok(Value::Syntax(Box::new((expanded, gensym_values))))
    }

    /// Collect syntax variable names from the environment that appear in the template
    fn collect_syntax_vars(
        &self,
        template: &Expr,
        env: &Env,
        syntax_vars: &mut HashSet<String>,
        syntax_list_vars: &mut HashSet<String>,
    ) {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if let Some(val) = env.get(s) {
                    match &val {
                        Value::Syntax(_) => { syntax_vars.insert(s.clone()); }
                        Value::Pair(_) | Value::Nil => {
                            // Check if it's a list of Syntax values
                            if let Some(vec) = val.to_vec() {
                                if !vec.is_empty() && vec.iter().all(|v| matches!(v, Value::Syntax(_))) {
                                    syntax_list_vars.insert(s.clone());
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            ExprKind::List(elems) => {
                for e in elems {
                    self.collect_syntax_vars(e, env, syntax_vars, syntax_list_vars);
                }
            }
            _ => {}
        }
    }

    /// Build gensym map for hygiene in syntax templates (like build_gensym_map for syntax-rules)
    fn build_syntax_gensym_map(
        &mut self,
        template: &Expr,
        pattern_vars: &HashSet<String>,
        env: &Env,
        gensym_map: &mut HashMap<String, String>,
        gensym_values: &mut Vec<(String, Value)>,
    ) {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if pattern_vars.contains(s) || s == "..." || Self::is_special_form(s)
                    || Self::is_builtin(s) || gensym_map.contains_key(s)
                {
                    return;
                }
                if let Some(val) = env.get(s) {
                    if matches!(val, Value::Macro(_) | Value::TransformerMacro(_, _)) { return; }
                    let gs = self.gensym(s);
                    gensym_values.push((gs.clone(), val));
                    gensym_map.insert(s.clone(), gs);
                } else {
                    let gs = self.gensym(s);
                    gensym_map.insert(s.clone(), gs);
                }
            }
            ExprKind::List(elems) => {
                // Skip contents of (quote ...) forms
                if !elems.is_empty() {
                    if let ExprKind::Symbol(s) = &elems[0].kind {
                        if s == "quote" {
                            return;
                        }
                    }
                }
                for e in elems {
                    self.build_syntax_gensym_map(e, pattern_vars, env, gensym_map, gensym_values);
                }
            }
            _ => {}
        }
    }

    /// Expand a syntax template, substituting pattern variables and gensyms
    fn expand_syntax_template(
        &self,
        template: &Expr,
        env: &Env,
        syntax_vars: &HashSet<String>,
        syntax_list_vars: &HashSet<String>,
        gensym_map: &HashMap<String, String>,
    ) -> Expr {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if syntax_vars.contains(s) {
                    if let Some(Value::Syntax(data)) = env.get(s) {
                        return data.0.clone();
                    }
                }
                if let Some(gs) = gensym_map.get(s) {
                    return Expr { kind: ExprKind::Symbol(gs.clone()), line: 0, col: 0 };
                }
                template.clone()
            }
            ExprKind::List(elems) => {
                let mut expanded = Vec::new();
                let mut i = 0;
                while i < elems.len() {
                    let has_ellipsis = i + 1 < elems.len()
                        && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...");
                    if has_ellipsis {
                        if let ExprKind::Symbol(s) = &elems[i].kind {
                            if syntax_list_vars.contains(s) {
                                if let Some(list_val) = env.get(s) {
                                    if let Some(vec) = list_val.to_vec() {
                                        for v in vec {
                                            if let Value::Syntax(data) = v {
                                                expanded.push(data.0.clone());
                                            }
                                        }
                                    }
                                }
                            } else if syntax_vars.contains(s) {
                                // Single syntax var with ellipsis - just expand once
                                if let Some(Value::Syntax(data)) = env.get(s) {
                                    expanded.push(data.0.clone());
                                }
                            }
                        }
                        i += 2;
                    } else {
                        expanded.push(self.expand_syntax_template(&elems[i], env, syntax_vars, syntax_list_vars, gensym_map));
                        i += 1;
                    }
                }
                Expr { kind: ExprKind::List(expanded), line: template.line, col: template.col }
            }
            _ => template.clone(),
        }
    }

    /// Evaluate (with-syntax ((pattern expr) ...) body ...)
    fn eval_with_syntax(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Parse(format!("with-syntax: expected bindings and body at {pos}")));
        }
        let bindings_list = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("with-syntax: expected bindings list at {pos}"))),
        };

        let mut new_env = env.child();

        for binding in bindings_list {
            let pair = match &binding.kind {
                ExprKind::List(p) if p.len() == 2 => p,
                _ => return Err(EvalError::Parse(format!("with-syntax: expected (pattern expr) at {pos}"))),
            };
            let var = match &pair[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("with-syntax: expected symbol pattern at {pos}"))),
            };
            let val = self.eval_in_env(&pair[1], env)?;
            // Ensure the value is a Syntax object
            let syntax_val = match val {
                Value::Syntax(_) => val,
                other => Value::Syntax(Box::new((Self::value_to_expr(&other)?, vec![]))),
            };
            new_env.define(var, syntax_val);
        }

        // Evaluate body in new env
        let mut result = Value::Void;
        for body_expr in &args[1..] {
            result = self.eval_in_env(body_expr, &mut new_env)?;
        }
        Ok(result)
    }
}
