use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, ErrorKind, Span};
use crate::scheme::macro_expand;
use crate::scheme::value::Value;

static NEXT_CONT_ID: AtomicU64 = AtomicU64::new(1);

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "string?", "number?", "boolean?", "pair?", "symbol?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "char?",
    "string-copy",
    "apply",
    "call/cc", "call-with-current-continuation",
];

fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name)
}

/// Whether a frame comes from a lambda body or a let/begin body.
#[derive(Clone, Copy, PartialEq)]
enum FrameKind {
    Lambda,
    LetOrBegin,
}

/// A captured continuation frame — one level of body nesting.
#[derive(Clone)]
struct ContFrame {
    body: Vec<Value>,
    index: usize,
    env: Rc<RefCell<Env>>,
    kind: FrameKind,
}

/// Info stored for each continuation.
struct ContInfo {
    toplevel_idx: usize,
    frames: Vec<ContFrame>,
    /// True when a Lambda frame was stripped during registration.
    /// When true, the innermost frame's current expression is a function
    /// call that eventually reached call/cc — skip it during replay.
    /// When false, call/cc is directly inside the current expression —
    /// re-evaluate it so call/cc can consume the pending value.
    lambda_stripped: bool,
}

/// Continuation context for saved/reentrant continuations.
pub struct CcCtx {
    registry: RefCell<HashMap<u64, ContInfo>>,
    current_idx: Cell<usize>,
    pending: RefCell<Option<Value>>,
    /// Stack of body frames for continuation capture.
    frame_stack: RefCell<Vec<ContFrame>>,
    /// True when we've popped a Lambda frame and are in its tail-call chain.
    in_lambda_tail: Cell<bool>,
}

impl CcCtx {
    pub fn new() -> Self {
        CcCtx {
            registry: RefCell::new(HashMap::new()),
            current_idx: Cell::new(0),
            pending: RefCell::new(None),
            frame_stack: RefCell::new(Vec::new()),
            in_lambda_tail: Cell::new(false),
        }
    }

    fn register(&self, id: u64) {
        let mut frames = self.frame_stack.borrow().clone();
        // lambda_stripped is true if call/cc was reached via a Lambda body
        // (either directly in the body loop, or via a tail-call chain).
        let mut lambda_stripped = self.in_lambda_tail.get();
        // Exclude the topmost lambda-body frame: the continuation resumes
        // from call/cc's return point, not from the lambda's remaining body.
        if let Some(last) = frames.last() {
            if last.kind == FrameKind::Lambda {
                frames.pop();
                lambda_stripped = true;
            }
        }
        self.registry.borrow_mut().insert(id, ContInfo {
            toplevel_idx: self.current_idx.get(),
            frames,
            lambda_stripped,
        });
    }

    fn lookup_idx(&self, id: u64) -> Option<usize> {
        self.registry.borrow().get(&id).map(|info| info.toplevel_idx)
    }

    fn lookup_cont(&self, id: u64) -> Option<(Vec<ContFrame>, bool)> {
        self.registry.borrow().get(&id)
            .map(|info| (info.frames.clone(), info.lambda_stripped))
    }

    fn take_pending(&self) -> Option<Value> {
        self.pending.borrow_mut().take()
    }

    fn set_pending(&self, value: Value) {
        *self.pending.borrow_mut() = Some(value);
    }

    fn set_current_idx(&self, idx: usize) {
        self.current_idx.set(idx);
    }

    fn push_frame(&self, body: Vec<Value>, env: Rc<RefCell<Env>>, kind: FrameKind) {
        self.frame_stack.borrow_mut().push(ContFrame { body, index: 0, env, kind });
    }

    fn set_frame_index(&self, index: usize) {
        if let Some(frame) = self.frame_stack.borrow_mut().last_mut() {
            frame.index = index;
        }
    }

    fn pop_frame(&self) {
        if let Some(frame) = self.frame_stack.borrow_mut().pop() {
            if frame.kind == FrameKind::Lambda {
                self.in_lambda_tail.set(true);
            }
        }
    }
}

/// Result of handling a continuation return at the top level.
enum ToplevelAction {
    /// Advance to next expression with the given value.
    Advance(Value),
    /// Redirect to a different top-level expression index.
    Redirect(usize),
    /// Propagate an error.
    Err(EvalError),
}

/// Handle a ContinuationReturn error from a top-level expression.
fn handle_continuation_return(
    id: u64,
    value: Value,
    current_idx: usize,
    out: &mut String,
    cc: &CcCtx,
) -> ToplevelAction {
    let target_idx = match cc.lookup_idx(id) {
        Some(idx) => idx,
        None => return ToplevelAction::Err(EvalError::continuation_return(id, value)),
    };
    let (frames, lambda_stripped) = cc.lookup_cont(id).unwrap_or((Vec::new(), false));
    if target_idx == current_idx && !frames.is_empty() {
        match replay_frames(&frames, lambda_stripped, value, out, cc) {
            Ok(val) => ToplevelAction::Advance(val),
            Err(e) => {
                if let ErrorKind::ContinuationReturn { id: id2, value: val2 } = e.kind {
                    return handle_continuation_return(id2, *val2, current_idx, out, cc);
                }
                ToplevelAction::Err(e)
            }
        }
    } else {
        // Different expression or no frames — top-level replay
        cc.set_pending(value);
        cc.frame_stack.borrow_mut().clear();
        ToplevelAction::Redirect(target_idx)
    }
}

/// Replay a continuation by evaluating remaining expressions from each captured frame.
/// Frames are ordered outermost (index 0) to innermost (last).
fn replay_frames(
    frames: &[ContFrame],
    lambda_stripped: bool,
    value: Value,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Value, EvalError> {
    let mut result = value.clone();

    // Process from innermost (last) to outermost (first).
    for fi in (0..frames.len()).rev() {
        let frame = &frames[fi];
        let is_innermost = fi == frames.len() - 1;
        // For the innermost frame, if no Lambda frame was stripped, call/cc
        // is directly inside the current expression — re-evaluate it so
        // call/cc can pick up the pending value (e.g. `(set! x (call/cc ...))`).
        // Otherwise, the current expression is a function call — skip it.
        let start = if is_innermost && !lambda_stripped {
            cc.set_pending(value.clone());
            frame.index
        } else {
            frame.index + 1
        };
        let remaining = &frame.body[start..];

        if remaining.is_empty() {
            continue;
        }

        // Restore frame_stack to outer frames so new continuations captured
        // during replay include the correct context.
        *cc.frame_stack.borrow_mut() = frames[..fi].to_vec();

        // Push a frame for this level so new continuations see this body.
        cc.push_frame(frame.body.clone(), Rc::clone(&frame.env), frame.kind);
        for (ei, expr) in remaining.iter().enumerate() {
            let saved_tail = cc.in_lambda_tail.get();
            cc.in_lambda_tail.set(false);
            cc.set_frame_index(start + ei);
            result = eval(expr, &frame.env, out, cc)?;
            cc.in_lambda_tail.set(saved_tail);
        }
        cc.pop_frame();

        // After processing the innermost frame, clear any unconsumed pending
        // value so outer frames' call/ccs don't accidentally pick it up.
        if is_innermost {
            cc.take_pending();
        }
    }

    Ok(result)
}

/// Evaluate a top-level sequence of expressions, handling saved continuations via replay.
pub fn eval_toplevel(
    exprs: &[(Value, Span)],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    let mut i = 0;
    while i < exprs.len() {
        cc.set_current_idx(i);
        match eval(&exprs[i].0, env, out, cc) {
            Ok(val) => {
                last = val;
                i += 1;
            }
            Err(e) => {
                if let ErrorKind::ContinuationReturn { id, value } = e.kind {
                    match handle_continuation_return(id, *value, i, out, cc) {
                        ToplevelAction::Advance(val) => { last = val; i += 1; }
                        ToplevelAction::Redirect(idx) => { i = idx; }
                        ToplevelAction::Err(e2) => return Err(e2),
                    }
                    continue;
                }
                return Err(e.at(exprs[i].1));
            }
        }
    }
    Ok(last)
}


/// Evaluate a Scheme expression in the given environment.
/// Uses a trampoline loop for tail call optimization.
pub fn eval(
    expr: &Value,
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = Rc::clone(env);

    loop {
        match current_expr {
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) => {
                return Ok(current_expr);
            }
            Value::Symbol(ref name) => {
                return match current_env.borrow().get(name) {
                    Ok(val) => Ok(val),
                    Err(_) if is_builtin(name) => Ok(current_expr),
                    Err(e) => Err(e),
                };
            }
            Value::List(ref items) => {
                if items.is_empty() {
                    return Err(EvalError::parse("empty application"));
                }
                let items = items.clone();
                match eval_list_tco(&items, &current_env, out, cc)? {
                    Trampoline::Done(val) => return Ok(val),
                    Trampoline::TailCall { expr, env } => {
                        current_expr = expr;
                        current_env = env;
                        continue;
                    }
                }
            }
            Value::Lambda { .. } => return Ok(current_expr),
            Value::Continuation { .. } => return Ok(current_expr),
            Value::SyntaxRules { .. } => return Ok(current_expr),
            Value::Void => return Ok(Value::Void),
        }
    }
}

/// Result of evaluating in a tail-call-aware context.
enum Trampoline {
    Done(Value),
    TailCall {
        expr: Value,
        env: Rc<RefCell<Env>>,
    },
}

fn eval_list_tco(
    items: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    // Check for special forms first
    if let Value::Symbol(op) = &items[0] {
        match op.as_str() {
            "and" => return eval_and_tco(&items[1..], env, out, cc),
            "or" => return eval_or_tco(&items[1..], env, out, cc),
            "if" => return eval_if_tco(&items[1..], env, out, cc),
            "define" => {
                let val = eval_define(&items[1..], env, out, cc)?;
                return Ok(Trampoline::Done(val));
            }
            "quote" => return Ok(Trampoline::Done(eval_quote(&items[1..])?)),
            "lambda" => return Ok(Trampoline::Done(eval_lambda(&items[1..], env)?)),
            "let" => return eval_let_tco(&items[1..], env, out, cc),
            "begin" => return eval_begin_tco(&items[1..], env, out, cc),
            "cond" => return eval_cond_tco(&items[1..], env, out, cc),
            "set!" => {
                let val = eval_set(&items[1..], env, out, cc)?;
                return Ok(Trampoline::Done(val));
            }
            "string-set!" => {
                let val = eval_string_set(&items[1..], env, out, cc)?;
                return Ok(Trampoline::Done(val));
            }
            "call/cc" | "call-with-current-continuation" => {
                return eval_callcc(&items[1..], env, out, cc);
            }
            "define-syntax" => {
                let val = eval_define_syntax(&items[1..], env)?;
                return Ok(Trampoline::Done(val));
            }
            _ => {
                // Check for macro expansion
                if let Ok(Value::SyntaxRules { ref literals, ref rules, ref def_env, .. }) = env.borrow().get(op) {
                    let expanded = macro_expand::expand_macro(
                        items, literals, rules, def_env,
                    )?;
                    return Ok(Trampoline::TailCall {
                        expr: expanded,
                        env: Rc::clone(env),
                    });
                }
            }
        }
    }

    // Evaluate operator
    let operator = eval(&items[0], env, out, cc)?;

    // Check if operator resolved to call/cc (e.g. passed as value)
    if let Value::Symbol(ref s) = operator {
        if s == "call/cc" || s == "call-with-current-continuation" {
            let args: Vec<Value> = items[1..]
                .iter()
                .map(|a| eval(a, env, out, cc))
                .collect::<Result<Vec<_>, _>>()?;
            return eval_callcc(&args, env, out, cc);
        }
    }

    // Evaluate arguments
    let args: Vec<Value> = items[1..]
        .iter()
        .map(|a| eval(a, env, out, cc))
        .collect::<Result<Vec<_>, _>>()?;

    apply_tco(&operator, &args, out, cc)
}

fn apply_tco(
    operator: &Value,
    args: &[Value],
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    match operator {
        Value::Symbol(ref name) if name == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::arity("apply requires at least 2 arguments"));
            }
            let proc = &args[0];
            let last = &args[args.len() - 1];
            let Value::List(tail_args) = last else {
                return Err(EvalError::type_err(format!(
                    "apply: last argument must be a list, got {last}"
                )));
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail_args.iter().cloned());
            apply_tco(proc, &all_args, out, cc)
        }
        Value::Symbol(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
            // call/cc used via apply or similar
            if args.len() != 1 {
                return Err(EvalError::arity("call/cc requires exactly 1 argument"));
            }
            eval_callcc_with_proc(&args[0], out, cc)
        }
        Value::Symbol(name) => {
            let val = apply_builtin(name, args, out)?;
            Ok(Trampoline::Done(val))
        }
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => {
            if let Some(ref rest_name) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::arity(format!(
                        "expected at least {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local = Env::with_parent(closure);
                for (param, arg) in params.iter().zip(args.iter()) {
                    local.borrow_mut().set(param.clone(), arg.clone());
                }
                let rest = Value::List(args[params.len()..].to_vec());
                local.borrow_mut().set(rest_name.clone(), rest);
                if body.len() > 1 {
                    cc.push_frame(body.clone(), Rc::clone(&local), FrameKind::Lambda);
                    for (idx, expr) in body[..body.len() - 1].iter().enumerate() {
                        let saved_tail = cc.in_lambda_tail.get();
                        cc.in_lambda_tail.set(false);
                        cc.set_frame_index(idx);
                        eval(expr, &local, out, cc)?;
                        cc.in_lambda_tail.set(saved_tail);
                    }
                    cc.pop_frame();
                }
                Ok(Trampoline::TailCall {
                    expr: body[body.len() - 1].clone(),
                    env: local,
                })
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::arity(format!(
                        "expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local = Env::with_parent(closure);
                for (param, arg) in params.iter().zip(args.iter()) {
                    local.borrow_mut().set(param.clone(), arg.clone());
                }
                if body.len() > 1 {
                    cc.push_frame(body.clone(), Rc::clone(&local), FrameKind::Lambda);
                    for (idx, expr) in body[..body.len() - 1].iter().enumerate() {
                        let saved_tail = cc.in_lambda_tail.get();
                        cc.in_lambda_tail.set(false);
                        cc.set_frame_index(idx);
                        eval(expr, &local, out, cc)?;
                        cc.in_lambda_tail.set(saved_tail);
                    }
                    cc.pop_frame();
                }
                Ok(Trampoline::TailCall {
                    expr: body[body.len() - 1].clone(),
                    env: local,
                })
            }
        }
        Value::Continuation { id } => {
            if args.len() != 1 {
                return Err(EvalError::arity("continuation requires exactly 1 argument"));
            }
            Err(EvalError::continuation_return(*id, args[0].clone()))
        }
        _ => Err(EvalError::type_err(format!("not a procedure: {operator}"))),
    }
}

fn apply_builtin(name: &str, args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += require_int(a, "+")?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::arity("- requires at least 1 argument"));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-require_int(&args[0], "-")?));
            }
            let mut result = require_int(&args[0], "-")?;
            for a in &args[1..] {
                result -= require_int(a, "-")?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= require_int(a, "*")?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::arity("/ requires at least 1 argument"));
            }
            let mut result = require_int(&args[0], "/")?;
            for a in &args[1..] {
                let divisor = require_int(a, "/")?;
                if divisor == 0 {
                    return Err(EvalError::div_zero());
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, "<", |a, b| a < b),
        ">" => compare_nums(args, ">", |a, b| a > b),
        "=" => compare_nums(args, "=", |a, b| a == b),
        "<=" => compare_nums(args, "<=", |a, b| a <= b),
        ">=" => compare_nums(args, ">=", |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::arity("not requires exactly 1 argument"));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::arity("cons requires exactly 2 arguments"));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Err(EvalError::type_err(format!(
                    "cons: second argument must be a list, got {}", args[1]
                ))),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::arity("car requires exactly 1 argument"));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::type_err(format!(
                    "car: expected non-empty list, got {}", args[0]
                ))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::arity("cdr requires exactly 1 argument"));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => {
                    Ok(Value::List(items[1..].to_vec()))
                }
                _ => Err(EvalError::type_err(format!(
                    "cdr: expected non-empty list, got {}", args[0]
                ))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("null? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::arity("length requires exactly 1 argument"));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::type_err(format!(
                    "length: expected list, got {}", args[0]
                ))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(items) => result.extend(items.iter().cloned()),
                    _ => {
                        return Err(EvalError::type_err(format!(
                            "append: expected list, got {arg}"
                        )));
                    }
                }
            }
            Ok(Value::List(result))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("number? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("boolean? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("pair? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("symbol? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        // I/O builtins
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::arity("display requires exactly 1 argument"));
            }
            args[0].display_fmt(out);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::arity("write requires exactly 1 argument"));
            }
            args[0].write_fmt(out);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::arity("newline requires 0 arguments"));
            }
            out.push('\n');
            Ok(Value::Void)
        }
        // String builtins
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "char?" | "string-copy" => apply_string_builtin(name, args),
        _ => Err(EvalError::unbound(name)),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::type_err(format!(
                        "string-append: expected string, got {a}"
                    ))),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string-length requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::type_err(format!(
                    "string-length: expected string, got {}", args[0]
                ))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::arity("substring requires exactly 3 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::type_err(format!(
                    "substring: expected string, got {}", args[0]
                ))),
            };
            let start = require_int(&args[1], "substring")? as usize;
            let end = require_int(&args[2], "substring")? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::type_err(format!(
                    "substring: index out of range for string of length {}", s.len()
                )));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string->number requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::type_err(format!(
                    "string->number: expected string, got {}", args[0]
                ))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::arity("number->string requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Str(n.to_string())),
                _ => Err(EvalError::type_err(format!(
                    "number->string: expected number, got {}", args[0]
                ))),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::arity("symbol->string requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::type_err(format!(
                    "symbol->string: expected symbol, got {}", args[0]
                ))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string->symbol requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::type_err(format!(
                    "string->symbol: expected string, got {}", args[0]
                ))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::arity("string-ref requires exactly 2 arguments"));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::type_err(format!(
                    "string-ref: expected string, got {}", args[0]
                ))),
            };
            let idx = require_int(&args[1], "string-ref")? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::type_err(format!(
                    "string-ref: index {idx} out of range for string of length {}", s.len()
                ))),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::arity("char? requires exactly 1 argument"));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::arity("string-copy requires exactly 1 argument"));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::type_err(format!(
                    "string-copy: expected string, got {}", args[0]
                ))),
            }
        }
        _ => Err(EvalError::unbound(name)),
    }
}

/// Evaluate (call/cc proc) where proc is an unevaluated expression.
fn eval_callcc(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::arity("call/cc requires exactly 1 argument"));
    }
    let proc = eval(&args[0], env, out, cc)?;
    eval_callcc_with_proc(&proc, out, cc)
}

/// Core call/cc logic: given an already-evaluated procedure, create a continuation and call it.
fn eval_callcc_with_proc(
    proc: &Value,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    // Check for pending replay value
    if let Some(value) = cc.take_pending() {
        return Ok(Trampoline::Done(value));
    }
    let id = NEXT_CONT_ID.fetch_add(1, Ordering::Relaxed);
    cc.register(id);
    let cont = Value::Continuation { id };
    // Call the procedure with the continuation.
    // Must fully evaluate (no TailCall escape) so we can catch ContinuationReturn
    // from any point in the proc's execution.
    let result = match apply_tco(proc, &[cont], out, cc) {
        Ok(Trampoline::Done(val)) => Ok(val),
        Ok(Trampoline::TailCall { expr, env }) => eval(&expr, &env, out, cc),
        Err(e) => Err(e),
    };
    match result {
        Ok(val) => Ok(Trampoline::Done(val)),
        Err(e) => {
            if let ErrorKind::ContinuationReturn { id: ret_id, .. } = &e.kind {
                if *ret_id == id {
                    if let ErrorKind::ContinuationReturn { value, .. } = e.kind {
                        return Ok(Trampoline::Done(*value));
                    }
                }
            }
            Err(e)
        }
    }
}

fn eval_and_tco(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let result = eval(expr, env, out, cc)?;
        if !result.is_truthy() {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::TailCall {
        expr: exprs[exprs.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_or_tco(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let result = eval(expr, env, out, cc)?;
        if result.is_truthy() {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::TailCall {
        expr: exprs[exprs.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_if_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::arity("if requires 2 or 3 arguments"));
    }
    let cond = eval(&args[0], env, out, cc)?;
    if cond.is_truthy() {
        Ok(Trampoline::TailCall {
            expr: args[1].clone(),
            env: Rc::clone(env),
        })
    } else if args.len() == 3 {
        Ok(Trampoline::TailCall {
            expr: args[2].clone(),
            env: Rc::clone(env),
        })
    } else {
        Ok(Trampoline::Done(Value::Void))
    }
}

fn eval_define(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::arity("define requires at least 2 arguments"));
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::arity("define requires exactly 2 arguments"));
            }
            let val = eval(&args[1], env, out, cc)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::parse("define: empty signature"));
            }
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::type_err("define: expected function name"));
            };
            let (params, rest_param) = parse_params(&sig[1..], "define")?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                closure: Rc::clone(env),
            };
            env.borrow_mut().set(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::type_err("define: expected symbol or list")),
    }
}

/// Parse a parameter list, handling dot notation for rest params.
fn parse_params(param_list: &[Value], context: &str) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = param_list.iter().position(|v| matches!(v, Value::Symbol(s) if s == "."));
    match dot_pos {
        Some(pos) => {
            if pos + 2 != param_list.len() {
                return Err(EvalError::parse(format!("{context}: invalid dot notation")));
            }
            let params: Vec<String> = param_list[..pos]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::type_err(format!("{context}: expected parameter name"))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let Value::Symbol(rest) = &param_list[pos + 1] else {
                return Err(EvalError::type_err(format!("{context}: expected rest parameter name")));
            };
            Ok((params, Some(rest.clone())))
        }
        None => {
            let params: Vec<String> = param_list
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::type_err(format!("{context}: expected parameter name"))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok((params, None))
        }
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::arity("quote requires exactly 1 argument"));
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::arity("lambda requires at least 2 arguments"));
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::type_err("lambda: expected parameter list"));
    };
    let (params, rest_param) = parse_params(param_list, "lambda")?;
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        closure: Rc::clone(env),
    })
}

fn eval_let_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::arity("let requires at least 2 arguments"));
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::arity("named let requires bindings and body"));
        }
        let Value::List(bindings) = &args[1] else {
            return Err(EvalError::type_err("named let: expected bindings list"));
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            let Value::List(pair) = binding else {
                return Err(EvalError::type_err("let: expected binding pair"));
            };
            if pair.len() != 2 {
                return Err(EvalError::arity("let: binding must have 2 elements"));
            }
            let Value::Symbol(param) = &pair[0] else {
                return Err(EvalError::type_err("let: expected variable name"));
            };
            params.push(param.clone());
            init_vals.push(eval(&pair[1], env, out, cc)?);
        }
        let body = args[2..].to_vec();
        let local = Env::with_parent(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            closure: Rc::clone(&local),
        };
        local.borrow_mut().set(name.clone(), lambda);
        let func = local.borrow().get(name).expect("just defined");
        return apply_tco(&func, &init_vals, out, cc);
    }

    // Regular let: (let ((var init) ...) body ...)
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::type_err("let: expected bindings list"));
    };
    let local = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::type_err("let: expected binding pair"));
        };
        if pair.len() != 2 {
            return Err(EvalError::arity("let: binding must have 2 elements"));
        }
        let Value::Symbol(name) = &pair[0] else {
            return Err(EvalError::type_err("let: expected variable name"));
        };
        let val = eval(&pair[1], env, out, cc)?;
        local.borrow_mut().set(name.clone(), val);
    }
    let body = &args[1..];
    if body.len() > 1 {
        cc.push_frame(body.to_vec(), Rc::clone(&local), FrameKind::LetOrBegin);
        for (idx, expr) in body[..body.len() - 1].iter().enumerate() {
            let saved_tail = cc.in_lambda_tail.get();
            cc.in_lambda_tail.set(false);
            cc.set_frame_index(idx);
            eval(expr, &local, out, cc)?;
            cc.in_lambda_tail.set(saved_tail);
        }
        cc.pop_frame();
    }
    Ok(Trampoline::TailCall {
        expr: body[body.len() - 1].clone(),
        env: local,
    })
}

fn eval_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::arity("set! requires exactly 2 arguments"));
    }
    let Value::Symbol(name) = &args[0] else {
        return Err(EvalError::type_err("set!: first argument must be a symbol"));
    };
    let val = eval(&args[1], env, out, cc)?;
    env.borrow_mut().update(name, val)?;
    Ok(Value::Void)
}

fn eval_string_set(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::arity("string-set! requires exactly 3 arguments"));
    }
    let Value::Symbol(var_name) = &args[0] else {
        return Err(EvalError::type_err("string-set!: first argument must be a variable"));
    };
    let idx_val = eval(&args[1], env, out, cc)?;
    let idx = require_int(&idx_val, "string-set!")? as usize;
    let char_val = eval(&args[2], env, out, cc)?;
    let Value::Char(ch) = char_val else {
        return Err(EvalError::type_err(format!(
            "string-set!: expected char, got {char_val}"
        )));
    };
    let current = env.borrow().get(var_name)?;
    let Value::Str(s) = current else {
        return Err(EvalError::type_err(format!(
            "string-set!: expected string, got {current}"
        )));
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::type_err(format!(
            "string-set!: index {idx} out of range for string of length {}", chars.len()
        )));
    }
    chars[idx] = ch;
    let new_str: String = chars.into_iter().collect();
    env.borrow_mut().set(var_name.clone(), Value::Str(new_str));
    Ok(Value::Void)
}

fn eval_begin_tco(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Void));
    }
    if args.len() > 1 {
        cc.push_frame(args.to_vec(), Rc::clone(env), FrameKind::LetOrBegin);
        for (idx, expr) in args[..args.len() - 1].iter().enumerate() {
            let saved_tail = cc.in_lambda_tail.get();
            cc.in_lambda_tail.set(false);
            cc.set_frame_index(idx);
            eval(expr, env, out, cc)?;
            cc.in_lambda_tail.set(saved_tail);
        }
        cc.pop_frame();
    }
    Ok(Trampoline::TailCall {
        expr: args[args.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_cond_tco(
    clauses: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &mut String,
    cc: &CcCtx,
) -> Result<Trampoline, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::type_err("cond: expected clause"));
        };
        if parts.is_empty() {
            return Err(EvalError::arity("cond: empty clause"));
        }
        if let Value::Symbol(s) = &parts[0] {
            if s == "else" {
                if parts.len() == 1 {
                    return Ok(Trampoline::Done(Value::Void));
                }
                for expr in &parts[1..parts.len() - 1] {
                    eval(expr, env, out, cc)?;
                }
                return Ok(Trampoline::TailCall {
                    expr: parts[parts.len() - 1].clone(),
                    env: Rc::clone(env),
                });
            }
        }
        let test = eval(&parts[0], env, out, cc)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(Trampoline::Done(test));
            }
            for expr in &parts[1..parts.len() - 1] {
                eval(expr, env, out, cc)?;
            }
            return Ok(Trampoline::TailCall {
                expr: parts[parts.len() - 1].clone(),
                env: Rc::clone(env),
            });
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn require_int(val: &Value, op: &str) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::type_err(format!("{op} requires integer, got {val}"))),
    }
}

fn compare_nums(
    args: &[Value],
    op: &str,
    cmp: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::arity(format!("{op} requires at least 2 arguments")));
    }
    let first = require_int(&args[0], op)?;
    let second = require_int(&args[1], op)?;
    Ok(Value::Boolean(cmp(first, second)))
}

fn eval_define_syntax(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::arity("define-syntax requires exactly 2 arguments"));
    }
    let Value::Symbol(name) = &args[0] else {
        return Err(EvalError::type_err("define-syntax: expected name"));
    };
    let Value::List(sr_form) = &args[1] else {
        return Err(EvalError::type_err("define-syntax: expected syntax-rules"));
    };
    if sr_form.is_empty() || !matches!(&sr_form[0], Value::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::parse("define-syntax: expected syntax-rules"));
    }
    if sr_form.len() < 3 {
        return Err(EvalError::arity("syntax-rules requires literals and at least one rule"));
    }
    let Value::List(literals_list) = &sr_form[1] else {
        return Err(EvalError::type_err("syntax-rules: expected literals list"));
    };
    let mut literals = Vec::new();
    for lit in literals_list {
        let Value::Symbol(s) = lit else {
            return Err(EvalError::type_err("syntax-rules: literal must be a symbol"));
        };
        literals.push(s.clone());
    }
    let mut rules = Vec::new();
    for rule in &sr_form[2..] {
        let Value::List(pair) = rule else {
            return Err(EvalError::type_err("syntax-rules: expected rule (pattern template)"));
        };
        if pair.len() != 2 {
            return Err(EvalError::arity("syntax-rules: rule must have pattern and template"));
        }
        rules.push((pair[0].clone(), pair[1].clone()));
    }
    env.borrow_mut().set(name.clone(), Value::SyntaxRules {
        literals,
        rules,
        def_env: Rc::clone(env),
    });
    Ok(Value::Void)
}
