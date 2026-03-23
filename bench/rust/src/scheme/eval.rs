use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
    "cons", "car", "cdr", "null?", "list", "length", "append", "reverse",
    "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "string-copy",
    "string->list", "list->string",
    "char->integer", "integer->char",
    "apply", "map",
    "call/cc", "call-with-current-continuation",
    "eq?", "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
    "zero?", "positive?", "negative?", "odd?", "even?",
    "list-ref", "list-tail", "list?", "assoc", "equal?",
    "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
    "char=?", "char<?",
    "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
    "eqv?",
    "vector", "make-vector", "vector-ref", "vector-set!", "vector-length",
    "vector?", "vector->list", "list->vector",
    "dynamic-wind",
];

pub fn default_env() -> Rc<RefCell<Env>> {
    let env = Env::new();
    for &name in BUILTINS {
        env.borrow_mut().define(name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

// ---------------------------------------------------------------------------
// CEK machine with explicit continuation stack
// ---------------------------------------------------------------------------

/// Continuation frame — one "thing left to do" after a value arrives.
#[derive(Debug, Clone)]
enum Frame {
    /// Application: collecting function + arguments left-to-right.
    /// `all_exprs` stores [func_expr, arg1_expr, ...] so continuations can
    /// re-evaluate already-computed siblings when restored.
    App {
        all_exprs: Vec<Value>,
        evaluated: Vec<Value>,
        env: Rc<RefCell<Env>>,
        span: Option<Span>,
    },
    If {
        then_expr: Value,
        else_expr: Option<Value>,
        env: Rc<RefCell<Env>>,
    },
    Seq {
        remaining: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Define {
        name: String,
        env: Rc<RefCell<Env>>,
    },
    Set {
        name: String,
        env: Rc<RefCell<Env>>,
        span: Option<Span>,
    },
    And {
        remaining: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Or {
        remaining: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    CondTest {
        body: Vec<Value>,
        remaining_clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    LetBindings {
        done: Vec<(String, Value)>,
        current_name: String,
        remaining: Vec<(String, Value)>,
        body: Vec<Value>,
        outer_env: Rc<RefCell<Env>>,
    },
    NamedLetBindings {
        name: String,
        params: Vec<String>,
        done: Vec<(String, Value)>,
        current_name: String,
        remaining: Vec<(String, Value)>,
        body: Vec<Value>,
        outer_env: Rc<RefCell<Env>>,
    },
    StringSetIdx {
        var_name: String,
        char_expr: Value,
        env: Rc<RefCell<Env>>,
        span: Option<Span>,
    },
    StringSetChar {
        var_name: String,
        index: i64,
        env: Rc<RefCell<Env>>,
        span: Option<Span>,
    },
    Map {
        func: Value,
        lists: Vec<Vec<Value>>,
        results: Vec<Value>,
        span: Option<Span>,
    },
    LetrecBindings {
        names: Vec<String>,
        done_values: Vec<Value>,
        remaining_exprs: Vec<Value>,
        body: Vec<Value>,
        letrec_env: Rc<RefCell<Env>>,
    },
    CaseKey {
        clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    /// After in-thunk returns: push winder, call body-thunk
    DynamicWindBody {
        in_thunk: Value,
        body_thunk: Value,
        out_thunk: Value,
    },
    /// After body-thunk returns: pop winder, call out-thunk, then return body value
    DynamicWindOut {
        out_thunk: Value,
    },
    /// After out-thunk returns: yield the saved body value
    DynamicWindDone {
        body_value: Value,
    },
    /// Continuation restore: run wind thunks in sequence, then restore
    DynamicWindRestore {
        /// Winder to push after the thunk that just returned (if any)
        pending_push: Option<Winder>,
        /// Remaining (thunk, optional winder to push after call)
        remaining: Vec<(Value, Option<Winder>)>,
        cont_id: usize,
        value: Value,
    },
}

#[derive(Debug, Clone)]
struct Winder {
    id: usize,
    in_thunk: Value,
    out_thunk: Value,
}

enum Control {
    Eval(Value, Rc<RefCell<Env>>),
    Continue(Value),
    Apply(Value, Vec<Value>, Option<Span>),
}

// Persistent (shared) continuation stack — capturing is O(1) via Rc clone.
struct KontNode {
    frame: Frame,
    rest: Option<Rc<KontNode>>,
}

#[derive(Clone)]
struct Kont(Option<Rc<KontNode>>);

impl Kont {
    fn new() -> Self {
        Kont(None)
    }

    fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    fn push(&mut self, frame: Frame) {
        let rest = self.0.take();
        self.0 = Some(Rc::new(KontNode { frame, rest }));
    }

    fn pop(&mut self) -> Option<Frame> {
        let node = self.0.take()?;
        match Rc::try_unwrap(node) {
            Ok(kn) => {
                self.0 = kn.rest;
                Some(kn.frame)
            }
            Err(rc) => {
                self.0 = rc.rest.clone();
                Some(rc.frame.clone())
            }
        }
    }

    fn top_is_app_with_evaluated(&self) -> bool {
        match self.0.as_ref().map(|n| &n.frame) {
            Some(Frame::App { evaluated, .. }) => !evaluated.is_empty(),
            _ => false,
        }
    }
}

struct Machine {
    kont: Kont,
    saved_conts: Vec<Kont>,
    winders: Vec<Winder>,
    saved_winders: Vec<Vec<Winder>>,
    winder_counter: usize,
    output: Rc<RefCell<String>>,
    gensym_counter: usize,
}

impl Machine {
    fn new(output: Rc<RefCell<String>>) -> Self {
        Machine {
            kont: Kont::new(),
            saved_conts: Vec::new(),
            winders: Vec::new(),
            saved_winders: Vec::new(),
            winder_counter: 0,
            output,
            gensym_counter: 0,
        }
    }

    fn run(&mut self, exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Void);
        }
        let mut control = if exprs.len() > 1 {
            self.kont.push(Frame::Seq {
                remaining: exprs[1..].to_vec(),
                env: Rc::clone(env),
            });
            Control::Eval(exprs[0].clone(), Rc::clone(env))
        } else {
            Control::Eval(exprs[0].clone(), Rc::clone(env))
        };

        loop {
            control = match control {
                Control::Eval(expr, env) => self.step_eval(expr, &env)?,
                Control::Continue(val) => {
                    if self.kont.is_empty() {
                        return Ok(val);
                    }
                    self.step_continue(val)?
                }
                Control::Apply(func, args, span) => self.apply_func(func, args, span)?,
            };
        }
    }

    // --- Evaluation step ---

    fn step_eval(&mut self, expr: Value, env: &Rc<RefCell<Env>>) -> Result<Control, EvalError> {
        let span = expr.span();
        match expr {
            Value::Int(_) | Value::Bool(_) | Value::String(_)
            | Value::Char(_) | Value::Builtin(_) | Value::Void
            | Value::Closure { .. } | Value::Pair(_, _) | Value::Continuation(_)
            | Value::SyntaxRules { .. } | Value::Vector(_) => Ok(Control::Continue(expr)),
            Value::Symbol(ref name, _) => env
                .borrow()
                .get(name)
                .map(Control::Continue)
                .map_err(|e| e.at(span)),
            Value::List(elems, list_span) => self.step_eval_list(elems, list_span, env),
        }
    }

    fn step_eval_list(
        &mut self,
        elems: Vec<Value>,
        span: Option<Span>,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if elems.is_empty() {
            return Err(EvalError::Parse { msg: "empty application".into() }.at(span));
        }

        if let Value::Symbol(ref op, _) = elems[0] {
            match op.as_str() {
                "and" => return self.sf_and(&elems[1..], env),
                "or" => return self.sf_or(&elems[1..], env),
                "if" => return self.sf_if(&elems[1..], span, env),
                "define" => return self.sf_define(&elems[1..], span, env),
                "lambda" => return sf_lambda(&elems[1..], span, env),
                "quote" => return sf_quote(&elems[1..], span),
                "let" => return self.sf_let(&elems[1..], span, env),
                "begin" => return self.eval_body_in(&elems[1..], env),
                "cond" => return self.sf_cond(&elems[1..], env),
                "set!" => return self.sf_set(&elems[1..], span, env),
                "string-set!" => return self.sf_string_set(&elems[1..], span, env),
                "define-syntax" => return self.sf_define_syntax(&elems[1..], span, env),
                "letrec" => return self.sf_letrec(&elems[1..], span, env, false),
                "letrec*" => return self.sf_letrec(&elems[1..], span, env, true),
                "case" => return self.sf_case(&elems[1..], span, env),
                "do" => return self.sf_do(&elems[1..], span, env),
                _ => {}
            }

            // Check if head is a macro (syntax-rules)
            // Extract SyntaxRules data before calling expand, to drop the env borrow
            let macro_data = env.borrow().get(op).ok().and_then(|val| {
                if let Value::SyntaxRules { literals, rules, def_env } = val {
                    Some((literals, rules, def_env))
                } else {
                    None
                }
            });
            if let Some((literals, rules, def_env)) = macro_data {
                let expanded = crate::scheme::macros::expand_syntax_rules(
                    &literals, &rules, &elems, &def_env, env,
                    &mut self.gensym_counter,
                )?;
                return Ok(Control::Eval(expanded, Rc::clone(env)));
            }
        }

        // Application: store full expression list, start evaluating func
        let first = elems[0].clone();
        self.kont.push(Frame::App {
            all_exprs: elems,
            evaluated: Vec::new(),
            env: Rc::clone(env),
            span,
        });
        Ok(Control::Eval(first, Rc::clone(env)))
    }

    // --- Continuation step ---

    fn step_continue(&mut self, val: Value) -> Result<Control, EvalError> {
        let frame = self.kont.pop().expect("kont non-empty checked by caller");
        match frame {
            Frame::App { all_exprs, mut evaluated, env, span } => {
                evaluated.push(val);
                if evaluated.len() >= all_exprs.len() {
                    let func = evaluated.remove(0);
                    Ok(Control::Apply(func, evaluated, span))
                } else {
                    let next_expr = all_exprs[evaluated.len()].clone();
                    self.kont.push(Frame::App {
                        all_exprs, evaluated, env: Rc::clone(&env), span,
                    });
                    Ok(Control::Eval(next_expr, env))
                }
            }
            Frame::If { then_expr, else_expr, env } => {
                if !is_false(&val) {
                    Ok(Control::Eval(then_expr, env))
                } else if let Some(e) = else_expr {
                    Ok(Control::Eval(e, env))
                } else {
                    Ok(Control::Continue(Value::Void))
                }
            }
            Frame::Seq { mut remaining, env } => {
                let next = remaining.remove(0);
                if !remaining.is_empty() {
                    self.kont.push(Frame::Seq { remaining, env: Rc::clone(&env) });
                }
                Ok(Control::Eval(next, env))
            }
            Frame::Define { name, env } => {
                env.borrow_mut().define(name, val);
                Ok(Control::Continue(Value::Void))
            }
            Frame::Set { name, env, span } => {
                env.borrow_mut().set(&name, val).map_err(|e| e.at(span))?;
                Ok(Control::Continue(Value::Void))
            }
            Frame::And { mut remaining, env } => {
                if is_false(&val) || remaining.is_empty() {
                    Ok(Control::Continue(val))
                } else {
                    let next = remaining.remove(0);
                    if !remaining.is_empty() {
                        self.kont.push(Frame::And { remaining, env: Rc::clone(&env) });
                    }
                    Ok(Control::Eval(next, env))
                }
            }
            Frame::Or { mut remaining, env } => {
                if !is_false(&val) || remaining.is_empty() {
                    Ok(Control::Continue(val))
                } else {
                    let next = remaining.remove(0);
                    if !remaining.is_empty() {
                        self.kont.push(Frame::Or { remaining, env: Rc::clone(&env) });
                    }
                    Ok(Control::Eval(next, env))
                }
            }
            Frame::CondTest { body, remaining_clauses, env } => {
                if !is_false(&val) {
                    if body.is_empty() {
                        Ok(Control::Continue(val))
                    } else {
                        self.eval_body_in(&body, &env)
                    }
                } else {
                    self.start_cond(&remaining_clauses, &env)
                }
            }
            Frame::LetBindings { mut done, current_name, mut remaining, body, outer_env } => {
                done.push((current_name, val));
                if remaining.is_empty() {
                    let let_env = Env::with_parent(&outer_env);
                    for (name, value) in done {
                        let_env.borrow_mut().define(name, value);
                    }
                    self.eval_body_in(&body, &let_env)
                } else {
                    let (next_name, next_expr) = remaining.remove(0);
                    self.kont.push(Frame::LetBindings {
                        done, current_name: next_name, remaining, body,
                        outer_env: Rc::clone(&outer_env),
                    });
                    Ok(Control::Eval(next_expr, outer_env))
                }
            }
            Frame::NamedLetBindings {
                name, params, mut done, current_name, mut remaining, body, outer_env,
            } => {
                done.push((current_name, val));
                if remaining.is_empty() {
                    let let_env = Env::with_parent(&outer_env);
                    let closure = Value::Closure {
                        params: params.clone(), rest_param: None,
                        body: body.clone(), env: Rc::clone(&let_env),
                    };
                    let_env.borrow_mut().define(name, closure);
                    let call_env = Env::with_parent(&let_env);
                    for (pname, pval) in done {
                        call_env.borrow_mut().define(pname, pval);
                    }
                    self.eval_body_in(&body, &call_env)
                } else {
                    let (next_name, next_expr) = remaining.remove(0);
                    self.kont.push(Frame::NamedLetBindings {
                        name, params, done, current_name: next_name,
                        remaining, body, outer_env: Rc::clone(&outer_env),
                    });
                    Ok(Control::Eval(next_expr, outer_env))
                }
            }
            Frame::StringSetIdx { var_name, char_expr, env, span } => {
                let Value::Int(idx) = val else {
                    return Err(EvalError::TypeMismatch {
                        expected: "integer".into(), got: format!("{val}"),
                    }.at(span));
                };
                self.kont.push(Frame::StringSetChar {
                    var_name, index: idx, env: Rc::clone(&env), span,
                });
                Ok(Control::Eval(char_expr, env))
            }
            Frame::StringSetChar { var_name, index, env, span } => {
                let Value::Char(ch) = val else {
                    return Err(EvalError::TypeMismatch {
                        expected: "char".into(), got: format!("{val}"),
                    }.at(span));
                };
                let current = env.borrow().get(&var_name).map_err(|e| e.at(span))?;
                let Value::String(s) = current else {
                    return Err(EvalError::TypeMismatch {
                        expected: "string".into(), got: format!("{current}"),
                    }.at(span));
                };
                let idx = index as usize;
                let mut chars: Vec<char> = s.chars().collect();
                if idx >= chars.len() {
                    return Err(EvalError::TypeMismatch {
                        expected: format!("index in range 0..{}", chars.len()),
                        got: format!("{idx}"),
                    }.at(span));
                }
                chars[idx] = ch;
                let new_string: String = chars.into_iter().collect();
                env.borrow_mut()
                    .set(&var_name, Value::String(new_string))
                    .map_err(|e| e.at(span))?;
                Ok(Control::Continue(Value::Void))
            }
            Frame::Map { func, lists, mut results, span } => {
                results.push(val);
                if lists[0].is_empty() {
                    Ok(Control::Continue(Value::List(results, None)))
                } else {
                    let mut next_args = Vec::with_capacity(lists.len());
                    let mut remaining = Vec::with_capacity(lists.len());
                    for mut lst in lists {
                        next_args.push(lst.remove(0));
                        remaining.push(lst);
                    }
                    self.kont.push(Frame::Map {
                        func: func.clone(), lists: remaining, results, span,
                    });
                    Ok(Control::Apply(func, next_args, span))
                }
            }
            Frame::LetrecBindings {
                names, mut done_values, mut remaining_exprs, body, letrec_env,
            } => {
                done_values.push(val);
                if remaining_exprs.is_empty() {
                    // All inits evaluated, set bindings in env
                    for (name, value) in names.iter().zip(done_values) {
                        letrec_env.borrow_mut().set(name, value)
                            .expect("letrec binding must exist");
                    }
                    self.eval_body_in(&body, &letrec_env)
                } else {
                    let next_expr = remaining_exprs.remove(0);
                    self.kont.push(Frame::LetrecBindings {
                        names, done_values, remaining_exprs, body,
                        letrec_env: Rc::clone(&letrec_env),
                    });
                    Ok(Control::Eval(next_expr, letrec_env))
                }
            }
            Frame::CaseKey { clauses, env } => {
                self.dispatch_case(val, &clauses, &env)
            }
            Frame::DynamicWindBody { in_thunk, body_thunk, out_thunk } => {
                // in-thunk just returned — push winder, then call body-thunk
                let id = self.winder_counter;
                self.winder_counter += 1;
                self.winders.push(Winder {
                    id,
                    in_thunk,
                    out_thunk: out_thunk.clone(),
                });
                self.kont.push(Frame::DynamicWindOut {
                    out_thunk,
                });
                Ok(Control::Apply(body_thunk, vec![], None))
            }
            Frame::DynamicWindOut { out_thunk } => {
                // body-thunk just returned with `val` — pop winder, call out-thunk
                self.winders.pop();
                self.kont.push(Frame::DynamicWindDone { body_value: val });
                Ok(Control::Apply(out_thunk, vec![], None))
            }
            Frame::DynamicWindDone { body_value } => {
                // out-thunk just returned — yield the saved body value
                Ok(Control::Continue(body_value))
            }
            Frame::DynamicWindRestore { pending_push, mut remaining, cont_id, value } => {
                // A wind thunk just returned — apply pending push if any
                if let Some(w) = pending_push {
                    self.winders.push(w);
                }
                if let Some((thunk, push)) = remaining.first().cloned() {
                    remaining.remove(0);
                    self.kont.push(Frame::DynamicWindRestore {
                        pending_push: push,
                        remaining,
                        cont_id,
                        value,
                    });
                    Ok(Control::Apply(thunk, vec![], None))
                } else {
                    // All wind thunks done — now restore the actual continuation
                    self.kont = self.saved_conts[cont_id].clone();
                    self.restore_continuation(value)
                }
            }
        }
    }

    // --- Function application ---

    fn apply_func(
        &mut self,
        func: Value,
        args: Vec<Value>,
        span: Option<Span>,
    ) -> Result<Control, EvalError> {
        match func {
            Value::Builtin(name) => self.apply_builtin_dispatch(&name, args, span),
            Value::Closure { params, rest_param, body, env } => {
                let param_count = params.len();
                let arg_count = args.len();
                match &rest_param {
                    Some(_) if arg_count < param_count => {
                        return Err(EvalError::WrongArgCount {
                            expected: param_count, got: arg_count,
                        }.at(span));
                    }
                    None if arg_count != param_count => {
                        return Err(EvalError::WrongArgCount {
                            expected: param_count, got: arg_count,
                        }.at(span));
                    }
                    _ => {}
                }
                let call_env = Env::with_parent(&env);
                for (param, arg) in params.iter().zip(args.iter()) {
                    call_env.borrow_mut().define(param.clone(), arg.clone());
                }
                if let Some(rest) = rest_param {
                    let rest_args = Value::List(args[param_count..].to_vec(), None);
                    call_env.borrow_mut().define(rest, rest_args);
                }
                self.eval_body_in(&body, &call_env)
            }
            Value::Continuation(id) => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCount {
                        expected: 1, got: args.len(),
                    }.at(span));
                }
                let val = args.into_iter().next().expect("checked len");
                let target_winders = &self.saved_winders[id];
                // Find common prefix length
                let common = self.winders.iter().zip(target_winders.iter())
                    .take_while(|(a, b)| a.id == b.id)
                    .count();
                let need_unwind = self.winders.len() > common;
                let need_rewind = target_winders.len() > common;
                if !need_unwind && !need_rewind {
                    // No winding needed — direct restore
                    self.kont = self.saved_conts[id].clone();
                    self.restore_continuation(val)
                } else {
                    // Build action list: unwind out-thunks (innermost first),
                    // then rewind in-thunks (outermost first)
                    let mut actions: Vec<(Value, Option<Winder>)> = Vec::new();
                    // Unwind
                    for w in self.winders[common..].iter().rev() {
                        actions.push((w.out_thunk.clone(), None));
                    }
                    // Pop unwound winders
                    self.winders.truncate(common);
                    // Rewind
                    for w in &target_winders[common..] {
                        actions.push((w.in_thunk.clone(), Some(w.clone())));
                    }
                    // Start calling the first thunk
                    let (first_thunk, first_push) = actions.remove(0);
                    self.kont.push(Frame::DynamicWindRestore {
                        pending_push: first_push,
                        remaining: actions,
                        cont_id: id,
                        value: val,
                    });
                    Ok(Control::Apply(first_thunk, vec![], span))
                }
            }
            other => Err(EvalError::NotAProcedure { value: other.to_string() }.at(span)),
        }
    }

    fn apply_builtin_dispatch(
        &mut self,
        name: &str,
        args: Vec<Value>,
        span: Option<Span>,
    ) -> Result<Control, EvalError> {
        match name {
            "call/cc" | "call-with-current-continuation" => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCount {
                        expected: 1, got: args.len(),
                    }.at(span));
                }
                let proc = args.into_iter().next().expect("checked len");
                let id = self.saved_conts.len();
                self.saved_conts.push(self.kont.clone());
                self.saved_winders.push(self.winders.clone());
                let cont = Value::Continuation(id);
                Ok(Control::Apply(proc, vec![cont], span))
            }
            "dynamic-wind" => {
                if args.len() != 3 {
                    return Err(EvalError::WrongArgCount {
                        expected: 3, got: args.len(),
                    }.at(span));
                }
                let mut it = args.into_iter();
                let in_thunk = it.next().expect("checked len");
                let body_thunk = it.next().expect("checked len");
                let out_thunk = it.next().expect("checked len");
                // Push frame to handle what happens after in-thunk returns
                self.kont.push(Frame::DynamicWindBody {
                    in_thunk: in_thunk.clone(),
                    body_thunk,
                    out_thunk,
                });
                // Call in-thunk with no arguments
                Ok(Control::Apply(in_thunk, vec![], span))
            }
            "map" => {
                if args.len() < 2 {
                    return Err(EvalError::WrongArgCount {
                        expected: 2, got: args.len(),
                    }.at(span));
                }
                let func = args[0].clone();
                let mut lists: Vec<Vec<Value>> = Vec::with_capacity(args.len() - 1);
                for arg in &args[1..] {
                    let Value::List(elems, _) = arg else {
                        return Err(EvalError::TypeMismatch {
                            expected: "list".into(), got: format!("{arg}"),
                        }.at(span));
                    };
                    lists.push(elems.clone());
                }
                if lists[0].is_empty() {
                    return Ok(Control::Continue(Value::List(Vec::new(), None)));
                }
                let mut first_args = Vec::with_capacity(lists.len());
                let mut remaining = Vec::with_capacity(lists.len());
                for mut lst in lists {
                    first_args.push(lst.remove(0));
                    remaining.push(lst);
                }
                self.kont.push(Frame::Map {
                    func: func.clone(), lists: remaining,
                    results: Vec::new(), span,
                });
                Ok(Control::Apply(func, first_args, span))
            }
            "apply" => {
                if args.len() < 2 {
                    return Err(EvalError::WrongArgCount {
                        expected: 2, got: args.len(),
                    }.at(span));
                }
                let Value::List(ref tail_args, _) = args[args.len() - 1] else {
                    return Err(EvalError::TypeMismatch {
                        expected: "list".into(),
                        got: format!("{}", args[args.len() - 1]),
                    }.at(span));
                };
                let tail_args = tail_args.clone();
                let new_func = args[0].clone();
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                all_args.extend(tail_args);
                Ok(Control::Apply(new_func, all_args, span))
            }
            _ => {
                let result = apply_builtin(name, &args, &self.output)
                    .map_err(|e| e.at(span))?;
                Ok(Control::Continue(result))
            }
        }
    }

    // --- Continuation restore ---
    //
    // When a saved continuation is restored, the top frame may be an App that
    // had partially-evaluated arguments (e.g., `(+ count (call/cc …))`).
    // Standard CEK would keep the already-evaluated `count=0`, but we
    // re-evaluate all expressions so mutable variables pick up their new values.

    fn restore_continuation(&mut self, val: Value) -> Result<Control, EvalError> {
        if self.kont.top_is_app_with_evaluated() {
            let Frame::App { mut all_exprs, evaluated, env, span } =
                self.kont.pop().expect("checked")
            else {
                unreachable!();
            };
            let callcc_index = evaluated.len();
            all_exprs[callcc_index] = make_literal(val);
            let first = all_exprs[0].clone();
            self.kont.push(Frame::App {
                all_exprs,
                evaluated: Vec::new(),
                env: Rc::clone(&env),
                span,
            });
            Ok(Control::Eval(first, env))
        } else {
            Ok(Control::Continue(val))
        }
    }

    // --- Helpers ---

    fn eval_body_in(&mut self, body: &[Value], env: &Rc<RefCell<Env>>) -> Result<Control, EvalError> {
        if body.is_empty() {
            return Ok(Control::Continue(Value::Void));
        }
        if body.len() > 1 {
            self.kont.push(Frame::Seq {
                remaining: body[1..].to_vec(),
                env: Rc::clone(env),
            });
        }
        Ok(Control::Eval(body[0].clone(), Rc::clone(env)))
    }

    // --- Special forms ---

    fn sf_if(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::Parse {
                msg: "if requires 2 or 3 arguments".into(),
            }.at(span));
        }
        self.kont.push(Frame::If {
            then_expr: args[1].clone(),
            else_expr: args.get(2).cloned(),
            env: Rc::clone(env),
        });
        Ok(Control::Eval(args[0].clone(), Rc::clone(env)))
    }

    fn sf_define(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse { msg: "define requires arguments".into() }.at(span));
        }
        match &args[0] {
            Value::Symbol(name, _) => {
                if args.len() != 2 {
                    return Err(EvalError::Parse {
                        msg: "define requires exactly 2 arguments".into(),
                    }.at(span));
                }
                self.kont.push(Frame::Define { name: name.clone(), env: Rc::clone(env) });
                Ok(Control::Eval(args[1].clone(), Rc::clone(env)))
            }
            Value::List(sig, _) => {
                if sig.is_empty() {
                    return Err(EvalError::Parse {
                        msg: "define: empty signature".into(),
                    }.at(span));
                }
                let Value::Symbol(name, _) = &sig[0] else {
                    return Err(EvalError::Parse {
                        msg: "define: expected function name".into(),
                    }.at(span));
                };
                let (params, rest_param) = parse_params(&sig[1..], span, "define")?;
                let body = args[1..].to_vec();
                if body.is_empty() {
                    return Err(EvalError::Parse {
                        msg: "define: empty body".into(),
                    }.at(span));
                }
                let closure = Value::Closure {
                    params, rest_param, body, env: Rc::clone(env),
                };
                env.borrow_mut().define(name.clone(), closure);
                Ok(Control::Continue(Value::Void))
            }
            Value::Int(_) | Value::Bool(_) | Value::String(_) | Value::Char(_)
            | Value::Builtin(_) | Value::Closure { .. } | Value::Pair(_, _)
            | Value::Continuation(_) | Value::SyntaxRules { .. }
            | Value::Vector(_) | Value::Void => {
                Err(EvalError::Parse {
                    msg: format!("define: expected symbol or list, got {}", args[0]),
                }.at(span))
            }
        }
    }

    fn sf_set(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Parse {
                msg: "set! requires exactly 2 arguments".into(),
            }.at(span));
        }
        let Value::Symbol(name, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "set!: first argument must be a symbol".into(),
            }.at(span));
        };
        self.kont.push(Frame::Set {
            name: name.clone(), env: Rc::clone(env), span,
        });
        Ok(Control::Eval(args[1].clone(), Rc::clone(env)))
    }

    fn sf_and(&mut self, exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Control, EvalError> {
        if exprs.is_empty() {
            return Ok(Control::Continue(Value::Bool(true)));
        }
        if exprs.len() > 1 {
            self.kont.push(Frame::And {
                remaining: exprs[1..].to_vec(), env: Rc::clone(env),
            });
        }
        Ok(Control::Eval(exprs[0].clone(), Rc::clone(env)))
    }

    fn sf_or(&mut self, exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Control, EvalError> {
        if exprs.is_empty() {
            return Ok(Control::Continue(Value::Bool(false)));
        }
        if exprs.len() > 1 {
            self.kont.push(Frame::Or {
                remaining: exprs[1..].to_vec(), env: Rc::clone(env),
            });
        }
        Ok(Control::Eval(exprs[0].clone(), Rc::clone(env)))
    }

    fn sf_cond(&mut self, clauses: &[Value], env: &Rc<RefCell<Env>>) -> Result<Control, EvalError> {
        self.start_cond(clauses, env)
    }

    fn start_cond(
        &mut self, clauses: &[Value], env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if clauses.is_empty() {
            return Ok(Control::Continue(Value::Void));
        }
        let Value::List(parts, _) = &clauses[0] else {
            return Err(EvalError::Parse { msg: "cond: expected clause".into() });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse { msg: "cond: empty clause".into() });
        }
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                return self.eval_body_in(&parts[1..], env);
            }
        }
        self.kont.push(Frame::CondTest {
            body: parts[1..].to_vec(),
            remaining_clauses: clauses[1..].to_vec(),
            env: Rc::clone(env),
        });
        Ok(Control::Eval(parts[0].clone(), Rc::clone(env)))
    }

    fn sf_let(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: "let requires bindings and body".into(),
            }.at(span));
        }
        if let Value::Symbol(name, _) = &args[0] {
            if args.len() < 3 {
                return Err(EvalError::Parse {
                    msg: "named let requires bindings and body".into(),
                }.at(span));
            }
            return self.start_named_let(name, &args[1], &args[2..], env, span);
        }
        self.start_regular_let(args, span, env)
    }

    fn start_regular_let(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        let Value::List(bindings_list, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "let: expected bindings list".into(),
            }.at(span));
        };
        let mut binding_pairs = Vec::new();
        for binding in bindings_list {
            let Value::List(pair, _) = binding else {
                return Err(EvalError::Parse {
                    msg: "let: expected binding pair".into(),
                }.at(span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "let: binding must have 2 elements".into(),
                }.at(span));
            }
            let Value::Symbol(name, _) = &pair[0] else {
                return Err(EvalError::Parse {
                    msg: "let: expected variable name".into(),
                }.at(span));
            };
            binding_pairs.push((name.clone(), pair[1].clone()));
        }
        let body = args[1..].to_vec();
        if binding_pairs.is_empty() {
            let let_env = Env::with_parent(env);
            return self.eval_body_in(&body, &let_env);
        }
        let (first_name, first_expr) = binding_pairs.remove(0);
        self.kont.push(Frame::LetBindings {
            done: Vec::new(), current_name: first_name, remaining: binding_pairs,
            body, outer_env: Rc::clone(env),
        });
        Ok(Control::Eval(first_expr, Rc::clone(env)))
    }

    fn start_named_let(
        &mut self,
        name: &str,
        bindings_val: &Value,
        body_args: &[Value],
        env: &Rc<RefCell<Env>>,
        span: Option<Span>,
    ) -> Result<Control, EvalError> {
        let Value::List(bindings_list, _) = bindings_val else {
            return Err(EvalError::Parse {
                msg: "let: expected bindings list".into(),
            }.at(span));
        };
        let mut params = Vec::new();
        let mut binding_pairs = Vec::new();
        for binding in bindings_list {
            let Value::List(pair, _) = binding else {
                return Err(EvalError::Parse {
                    msg: "let: expected binding pair".into(),
                }.at(span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "let: binding must have 2 elements".into(),
                }.at(span));
            }
            let Value::Symbol(pname, _) = &pair[0] else {
                return Err(EvalError::Parse {
                    msg: "let: expected variable name".into(),
                }.at(span));
            };
            params.push(pname.clone());
            binding_pairs.push((pname.clone(), pair[1].clone()));
        }
        let body = body_args.to_vec();
        if binding_pairs.is_empty() {
            let let_env = Env::with_parent(env);
            let closure = Value::Closure {
                params, rest_param: None, body: body.clone(), env: Rc::clone(&let_env),
            };
            let_env.borrow_mut().define(name.to_string(), closure);
            return self.eval_body_in(&body, &let_env);
        }
        let (first_name, first_expr) = binding_pairs.remove(0);
        self.kont.push(Frame::NamedLetBindings {
            name: name.to_string(), params, done: Vec::new(),
            current_name: first_name, remaining: binding_pairs,
            body, outer_env: Rc::clone(env),
        });
        Ok(Control::Eval(first_expr, Rc::clone(env)))
    }

    fn sf_string_set(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() != 3 {
            return Err(EvalError::WrongArgCount { expected: 3, got: args.len() }.at(span));
        }
        let Value::Symbol(var_name, _) = &args[0] else {
            return Err(EvalError::ImmutableString.at(span));
        };
        self.kont.push(Frame::StringSetIdx {
            var_name: var_name.clone(), char_expr: args[2].clone(),
            env: Rc::clone(env), span,
        });
        Ok(Control::Eval(args[1].clone(), Rc::clone(env)))
    }

    fn sf_letrec(
        &mut self,
        args: &[Value],
        span: Option<Span>,
        env: &Rc<RefCell<Env>>,
        is_star: bool,
    ) -> Result<Control, EvalError> {
        let form_name = if is_star { "letrec*" } else { "letrec" };
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: format!("{form_name} requires bindings and body"),
            }.at(span));
        }
        let Value::List(bindings_list, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: format!("{form_name}: expected bindings list"),
            }.at(span));
        };
        let mut names = Vec::new();
        let mut init_exprs = Vec::new();
        for binding in bindings_list {
            let Value::List(pair, _) = binding else {
                return Err(EvalError::Parse {
                    msg: format!("{form_name}: expected binding pair"),
                }.at(span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Parse {
                    msg: format!("{form_name}: binding must have 2 elements"),
                }.at(span));
            }
            let Value::Symbol(name, _) = &pair[0] else {
                return Err(EvalError::Parse {
                    msg: format!("{form_name}: expected variable name"),
                }.at(span));
            };
            names.push(name.clone());
            init_exprs.push(pair[1].clone());
        }
        let body = args[1..].to_vec();
        let letrec_env = Env::with_parent(env);
        // Pre-define all names as Void so init exprs can reference them
        for name in &names {
            letrec_env.borrow_mut().define(name.clone(), Value::Void);
        }
        if init_exprs.is_empty() {
            return self.eval_body_in(&body, &letrec_env);
        }
        if is_star {
            // letrec*: evaluate sequentially, setting each binding immediately
            return self.eval_letrec_star(names, init_exprs, body, &letrec_env);
        }
        let mut remaining = init_exprs;
        let first_expr = remaining.remove(0);
        self.kont.push(Frame::LetrecBindings {
            names,
            done_values: Vec::new(),
            remaining_exprs: remaining,
            body,
            letrec_env: Rc::clone(&letrec_env),
        });
        Ok(Control::Eval(first_expr, letrec_env))
    }

    fn eval_letrec_star(
        &mut self,
        names: Vec<String>,
        init_exprs: Vec<Value>,
        body: Vec<Value>,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        // Desugar letrec* into: (begin (set! name0 init0) (set! name1 init1) ... body...)
        // All names are already defined as Void in env.
        let mut full_body = Vec::new();
        for (name, expr) in names.into_iter().zip(init_exprs) {
            full_body.push(Value::List(vec![
                Value::Symbol("set!".into(), None),
                Value::Symbol(name, None),
                expr,
            ], None));
        }
        full_body.extend(body);
        self.eval_body_in(&full_body, env)
    }

    fn sf_case(
        &mut self,
        args: &[Value],
        span: Option<Span>,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse {
                msg: "case requires at least a key expression".into(),
            }.at(span));
        }
        let clauses = args[1..].to_vec();
        self.kont.push(Frame::CaseKey {
            clauses,
            env: Rc::clone(env),
        });
        Ok(Control::Eval(args[0].clone(), Rc::clone(env)))
    }

    fn dispatch_case(
        &mut self,
        key: Value,
        clauses: &[Value],
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        for clause in clauses {
            let Value::List(parts, _) = clause else {
                return Err(EvalError::Parse { msg: "case: expected clause".into() });
            };
            if parts.is_empty() {
                return Err(EvalError::Parse { msg: "case: empty clause".into() });
            }
            // Check for else clause
            if let Value::Symbol(s, _) = &parts[0] {
                if s == "else" {
                    return self.eval_body_in(&parts[1..], env);
                }
            }
            // Datum list: ((datum ...) expr ...)
            let Value::List(datums, _) = &parts[0] else {
                return Err(EvalError::Parse { msg: "case: expected datum list".into() });
            };
            for datum in datums {
                if eqv(&key, datum) {
                    return self.eval_body_in(&parts[1..], env);
                }
            }
        }
        // No match, no else — return void
        Ok(Control::Continue(Value::Void))
    }

    fn sf_do(
        &mut self,
        args: &[Value],
        span: Option<Span>,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: "do requires bindings, test, and optional body".into(),
            }.at(span));
        }
        // Parse bindings: ((var init step?) ...)
        let Value::List(bindings_list, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "do: expected bindings list".into(),
            }.at(span));
        };
        let mut loop_params = Vec::new();
        let mut init_exprs = Vec::new();
        let mut step_exprs = Vec::new();

        for binding in bindings_list {
            let Value::List(parts, _) = binding else {
                return Err(EvalError::Parse {
                    msg: "do: expected binding".into(),
                }.at(span));
            };
            if parts.len() < 2 || parts.len() > 3 {
                return Err(EvalError::Parse {
                    msg: "do: binding must have 2 or 3 elements".into(),
                }.at(span));
            }
            let Value::Symbol(name, _) = &parts[0] else {
                return Err(EvalError::Parse {
                    msg: "do: expected variable name".into(),
                }.at(span));
            };
            loop_params.push(name.clone());
            init_exprs.push(parts[1].clone());
            // Step expr: if missing, variable keeps its value (use variable reference)
            if parts.len() == 3 {
                step_exprs.push(parts[2].clone());
            } else {
                step_exprs.push(Value::Symbol(name.clone(), None));
            }
        }

        // Parse test clause: (test expr ...)
        let Value::List(test_clause, _) = &args[1] else {
            return Err(EvalError::Parse {
                msg: "do: expected test clause".into(),
            }.at(span));
        };
        if test_clause.is_empty() {
            return Err(EvalError::Parse {
                msg: "do: empty test clause".into(),
            }.at(span));
        }
        let test_expr = test_clause[0].clone();
        let result_exprs = test_clause[1..].to_vec();
        let do_body = args[2..].to_vec();

        // Desugar into named let:
        // (let __do_loop ((var1 init1) (var2 init2) ...)
        //   (if test
        //     (begin result_exprs...)
        //     (begin body... (__do_loop step1 step2 ...))))
        let loop_name = "__do_loop";

        // Build the recursive call: (__do_loop step1 step2 ...)
        let mut recursive_call = vec![Value::Symbol(loop_name.into(), None)];
        recursive_call.extend(step_exprs);

        // Build the else branch: (begin body... (recursive-call))
        let mut else_body = do_body;
        else_body.push(Value::List(recursive_call, None));

        // Build the if expression
        let if_expr = if result_exprs.is_empty() {
            Value::List(vec![
                Value::Symbol("if".into(), None),
                test_expr,
                Value::Void,
                Value::List(vec![Value::Symbol("begin".into(), None)]
                    .into_iter().chain(else_body).collect(), None),
            ], None)
        } else {
            let then_branch = if result_exprs.len() == 1 {
                result_exprs[0].clone()
            } else {
                let mut begin = vec![Value::Symbol("begin".into(), None)];
                begin.extend(result_exprs);
                Value::List(begin, None)
            };
            Value::List(vec![
                Value::Symbol("if".into(), None),
                test_expr,
                then_branch,
                Value::List(vec![Value::Symbol("begin".into(), None)]
                    .into_iter().chain(else_body).collect(), None),
            ], None)
        };

        // Build named let bindings: ((var1 init1) (var2 init2) ...)
        let let_bindings: Vec<Value> = loop_params.iter().zip(init_exprs)
            .map(|(name, init)| Value::List(vec![
                Value::Symbol(name.clone(), None), init,
            ], None))
            .collect();

        // Build: (let __do_loop ((bindings...)) if_expr)
        let named_let = Value::List(vec![
            Value::Symbol("let".into(), None),
            Value::Symbol(loop_name.into(), None),
            Value::List(let_bindings, None),
            if_expr,
        ], None);

        Ok(Control::Eval(named_let, Rc::clone(env)))
    }

    fn sf_define_syntax(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Parse {
                msg: "define-syntax requires 2 arguments".into(),
            }.at(span));
        }
        let Value::Symbol(name, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "define-syntax: expected name".into(),
            }.at(span));
        };
        let Value::List(sr_elems, _) = &args[1] else {
            return Err(EvalError::Parse {
                msg: "define-syntax: expected syntax-rules expression".into(),
            }.at(span));
        };
        if sr_elems.is_empty() {
            return Err(EvalError::Parse {
                msg: "define-syntax: empty transformer".into(),
            }.at(span));
        }
        let is_syntax_rules = matches!(&sr_elems[0], Value::Symbol(s, _) if s == "syntax-rules");
        if !is_syntax_rules {
            return Err(EvalError::Parse {
                msg: "define-syntax: expected syntax-rules".into(),
            }.at(span));
        }
        if sr_elems.len() < 2 {
            return Err(EvalError::Parse {
                msg: "syntax-rules: expected literals list".into(),
            }.at(span));
        }
        let Value::List(literals_list, _) = &sr_elems[1] else {
            return Err(EvalError::Parse {
                msg: "syntax-rules: expected literals list".into(),
            }.at(span));
        };
        let literals: Vec<String> = literals_list
            .iter()
            .filter_map(|v| {
                if let Value::Symbol(s, _) = v { Some(s.clone()) } else { None }
            })
            .collect();
        let mut rules = Vec::new();
        for clause in &sr_elems[2..] {
            let Value::List(parts, _) = clause else {
                return Err(EvalError::Parse {
                    msg: "syntax-rules: clause must be a list".into(),
                }.at(span));
            };
            if parts.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "syntax-rules: clause must have pattern and template".into(),
                }.at(span));
            }
            rules.push((parts[0].clone(), parts[1].clone()));
        }
        let syntax = Value::SyntaxRules {
            literals,
            rules,
            def_env: Rc::clone(env),
        };
        env.borrow_mut().define(name.clone(), syntax);
        Ok(Control::Continue(Value::Void))
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

pub fn eval_program(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let mut machine = Machine::new(Rc::clone(output));
    machine.run(exprs, env)
}

// ---------------------------------------------------------------------------
// Pure helpers
// ---------------------------------------------------------------------------

/// Wrap a value so it self-evaluates when placed into an expression list.
/// Symbols and Lists would be interpreted as variable lookups / applications,
/// so we wrap them in `(quote ...)`.
fn make_literal(val: Value) -> Value {
    match val {
        Value::Int(_) | Value::Bool(_) | Value::String(_) | Value::Char(_)
        | Value::Builtin(_) | Value::Closure { .. } | Value::Pair(_, _)
        | Value::Continuation(_) | Value::SyntaxRules { .. }
        | Value::Vector(_) | Value::Void => val,
        Value::Symbol(_, _) | Value::List(_, _) => Value::List(
            vec![Value::Symbol("quote".into(), None), val],
            None,
        ),
    }
}

fn sf_lambda(
    args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
) -> Result<Control, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            msg: "lambda requires params and body".into(),
        }.at(span));
    }
    let Value::List(param_list, _) = &args[0] else {
        return Err(EvalError::Parse {
            msg: "lambda: expected parameter list".into(),
        }.at(span));
    };
    let (params, rest_param) = parse_params(param_list, span, "lambda")?;
    let body = args[1..].to_vec();
    Ok(Control::Continue(Value::Closure {
        params, rest_param, body, env: Rc::clone(env),
    }))
}

fn sf_quote(args: &[Value], span: Option<Span>) -> Result<Control, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse {
            msg: "quote requires exactly 1 argument".into(),
        }.at(span));
    }
    Ok(Control::Continue(args[0].clone()))
}

fn parse_params(
    param_list: &[Value],
    span: Option<Span>,
    context: &str,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = param_list.iter().position(|p| matches!(p, Value::Symbol(s, _) if s == "."));
    match dot_pos {
        Some(pos) => {
            if pos + 1 >= param_list.len() || pos + 2 != param_list.len() {
                return Err(EvalError::Parse {
                    msg: format!("{context}: invalid dot notation in parameter list"),
                }.at(span));
            }
            let params: Vec<String> = param_list[..pos]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s, _) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        msg: format!("{context}: expected parameter name, got {other}"),
                    }.at(span)),
                })
                .collect::<Result<_, _>>()?;
            let Value::Symbol(rest, _) = &param_list[pos + 1] else {
                return Err(EvalError::Parse {
                    msg: format!("{context}: expected rest parameter name after dot"),
                }.at(span));
            };
            Ok((params, Some(rest.clone())))
        }
        None => {
            let params: Vec<String> = param_list
                .iter()
                .map(|p| match p {
                    Value::Symbol(s, _) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        msg: format!("{context}: expected parameter name, got {other}"),
                    }.at(span)),
                })
                .collect::<Result<_, _>>()?;
            Ok((params, None))
        }
    }
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Bool(false))
}

fn eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::List(a, _), Value::List(b, _)) => a.is_empty() && b.is_empty(),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| match a {
            Value::Int(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".into(), got: format!("{other}"),
            }),
        })
        .collect()
}

fn display_value(val: &Value) -> String {
    match val {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        Value::Vector(elems) => {
            let borrowed = elems.borrow();
            let mut s = "#(".to_string();
            for (i, elem) in borrowed.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value(elem));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

// ---------------------------------------------------------------------------
// Builtin application (all args already evaluated)
// ---------------------------------------------------------------------------

fn apply_builtin(
    name: &str,
    args: &[Value],
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "abs" | "modulo" | "remainder" | "quotient"
        | "min" | "max" | "expt" | "zero?" | "positive?" | "negative?"
        | "odd?" | "even?" => apply_numeric_builtin(name, args),
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(is_false(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            match &args[1] {
                Value::List(rest, _) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(rest.iter().cloned());
                    Ok(Value::List(new_list, None))
                }
                _ => Ok(Value::Pair(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::Pair(a, _) => Ok(a.as_ref().clone()),
                _ => Err(EvalError::TypeMismatch {
                    expected: "pair".into(), got: format!("{}", args[0]),
                }),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec(), None))
                }
                Value::Pair(_, b) => Ok(b.as_ref().clone()),
                _ => Err(EvalError::TypeMismatch {
                    expected: "pair".into(), got: format!("{}", args[0]),
                }),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::List(v, _) if v.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec(), None)),
        "append" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            match (&args[0], &args[1]) {
                (Value::List(a, _), Value::List(b, _)) => {
                    let mut result = a.clone();
                    result.extend(b.iter().cloned());
                    Ok(Value::List(result, None))
                }
                (Value::List(_, _), other) => Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{other}"),
                }),
                (other, _) => Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{other}"),
                }),
            }
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) => {
                    let mut rev = elems.clone();
                    rev.reverse();
                    Ok(Value::List(rev, None))
                }
                other => Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{other}"),
                }),
            }
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) => Ok(Value::Int(elems.len() as i64)),
                other => Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{other}"),
                }),
            }
        }
        "string?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::String(_)))),
        "number?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::Int(_)))),
        "boolean?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::Bool(_)))),
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::List(v, _) if !v.is_empty()) || matches!(&args[0], Value::Pair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Symbol(_, _))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            output.borrow_mut().push_str(&display_value(&args[0]));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            output.borrow_mut().push_str(&args[0].to_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 0, got: args.len() });
            }
            output.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::List(elems, _) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let idx = *idx as usize;
            elems.get(idx).cloned().ok_or_else(|| EvalError::TypeMismatch {
                expected: format!("index in range 0..{}", elems.len()),
                got: format!("{idx}"),
            })
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::List(elems, _) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let idx = *idx as usize;
            if idx > elems.len() {
                return Err(EvalError::TypeMismatch {
                    expected: format!("index in range 0..={}", elems.len()),
                    got: format!("{idx}"),
                });
            }
            Ok(Value::List(elems[idx..].to_vec(), None))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::List(_, _))))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::List(alist, _) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[1]),
                });
            };
            for entry in alist {
                let Value::List(pair, _) = entry else {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair".into(), got: format!("{entry}"),
                    });
                };
                if !pair.is_empty() && pair[0] == args[0] {
                    return Ok(entry.clone());
                }
            }
            Ok(Value::Bool(false))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let result = match (&args[0], &args[1]) {
                (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Int(a), Value::Int(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::List(a, _), Value::List(b, _)) => a.is_empty() && b.is_empty(),
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                (Value::Void, Value::Void) => true,
                _ => false,
            };
            Ok(Value::Bool(result))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            Ok(Value::Bool(args[0] == args[1]))
        }
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?" | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase" => apply_char_string_cmp_builtin(name, args),
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string->list" | "list->string" => apply_string_builtin(name, args),
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Char(c) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Int(*c as i64))
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            let c = char::from_u32(*n as u32).ok_or_else(|| EvalError::TypeMismatch {
                expected: "valid Unicode code point".into(), got: format!("{n}"),
            })?;
            Ok(Value::Char(c))
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let result = match (&args[0], &args[1]) {
                (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Int(a), Value::Int(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::List(a, _), Value::List(b, _)) => a.is_empty() && b.is_empty(),
                (Value::Void, Value::Void) => true,
                _ => false,
            };
            Ok(Value::Bool(result))
        }
        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
        | "vector?" | "vector->list" | "list->vector" => apply_vector_builtin(name, args),
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn apply_vector_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(size) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Int(0) };
            let elems = vec![fill; *size as usize];
            Ok(Value::Vector(Rc::new(RefCell::new(elems))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::Vector(elems) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "vector".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let borrowed = elems.borrow();
            let idx = *idx as usize;
            borrowed.get(idx).cloned().ok_or_else(|| EvalError::TypeMismatch {
                expected: format!("index in range 0..{}", borrowed.len()),
                got: format!("{idx}"),
            })
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
            }
            let Value::Vector(elems) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "vector".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let idx = *idx as usize;
            let mut borrowed = elems.borrow_mut();
            if idx >= borrowed.len() {
                return Err(EvalError::TypeMismatch {
                    expected: format!("index in range 0..{}", borrowed.len()),
                    got: format!("{idx}"),
                });
            }
            borrowed[idx] = args[2].clone();
            Ok(Value::Void)
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Vector(elems) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "vector".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Int(elems.borrow().len() as i64))
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Vector(elems) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "vector".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::List(elems.borrow().clone(), None))
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::List(elems, _) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Vector(Rc::new(RefCell::new(elems.clone()))))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn apply_numeric_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().sum()))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            if nums.len() == 1 {
                Ok(Value::Int(-nums[0]))
            } else {
                let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
                Ok(Value::Int(result))
            }
        }
        "*" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().product()))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            for &n in &nums[1..] {
                if n == 0 {
                    return Err(EvalError::DivisionByZero);
                }
            }
            let result = nums[1..].iter().fold(nums[0], |acc, n| acc / n);
            Ok(Value::Int(result))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Int(n.abs()))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let nums = require_ints(args)?;
            if nums[1] == 0 { return Err(EvalError::DivisionByZero); }
            let r = nums[0] % nums[1];
            let result = if r != 0 && (r > 0) != (nums[1] > 0) { r + nums[1] } else { r };
            Ok(Value::Int(result))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let nums = require_ints(args)?;
            if nums[1] == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Int(nums[0] % nums[1]))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let nums = require_ints(args)?;
            if nums[1] == 0 { return Err(EvalError::DivisionByZero); }
            let q = nums[0] / nums[1];
            Ok(Value::Int(q))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            Ok(Value::Int(*nums.iter().min().expect("non-empty")))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            Ok(Value::Int(*nums.iter().max().expect("non-empty")))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let nums = require_ints(args)?;
            Ok(Value::Int(nums[0].pow(nums[1] as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(*n == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(*n > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(*n < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(n % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(n) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(n % 2 == 0))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn apply_char_string_cmp_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Char(c) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(c.is_alphabetic()))
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Char(c) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Bool(c.is_ascii_digit()))
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Char(c) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Char(c.to_ascii_uppercase()))
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Char(c) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Char(c.to_ascii_lowercase()))
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::Char(a) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Char(b) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[1]),
                });
            };
            Ok(Value::Bool(a == b))
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::Char(a) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Char(b) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "char".into(), got: format!("{}", args[1]),
                });
            };
            Ok(Value::Bool(a < b))
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::String(a) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            let Value::String(b) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[1]),
                });
            };
            Ok(Value::Bool(a == b))
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::String(a) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            let Value::String(b) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[1]),
                });
            };
            Ok(Value::Bool(a < b))
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::String(a) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            let Value::String(b) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[1]),
                });
            };
            Ok(Value::Bool(a.to_lowercase() == b.to_lowercase()))
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::String(s.to_uppercase()))
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::String(s.to_lowercase()))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::String(s) => result.push_str(s),
                    other => return Err(EvalError::TypeMismatch {
                        expected: "string".into(), got: format!("{other}"),
                    }),
                }
            }
            Ok(Value::String(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => Ok(Value::Int(s.len() as i64)),
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{other}"),
                }),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Int(start) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let Value::Int(end) = &args[2] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[2]),
                });
            };
            let start = *start as usize;
            let end = *end as usize;
            if start > end || end > s.len() {
                return Err(EvalError::TypeMismatch {
                    expected: format!("valid substring indices (0..{})", s.len()),
                    got: format!("{start}..{end}"),
                });
            }
            Ok(Value::String(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Int(n)),
                    Err(_) => Ok(Value::Bool(false)),
                },
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{other}"),
                }),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::String(format!("{n}"))),
                other => Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{other}"),
                }),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Symbol(s, _) => Ok(Value::String(s.clone())),
                other => Err(EvalError::TypeMismatch {
                    expected: "symbol".into(), got: format!("{other}"),
                }),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => Ok(Value::Symbol(s.clone(), None)),
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{other}"),
                }),
            }
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.clone())),
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{other}"),
                }),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let idx = *idx as usize;
            let ch = s.chars().nth(idx).ok_or_else(|| EvalError::TypeMismatch {
                expected: format!("index in range 0..{}", s.len()),
                got: format!("{idx}"),
            })?;
            Ok(Value::Char(ch))
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(), got: format!("{}", args[0]),
                });
            };
            let chars: Vec<Value> = s.chars().map(Value::Char).collect();
            Ok(Value::List(chars, None))
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let elems = match &args[0] {
                Value::List(elems, _) => elems.clone(),
                Value::Pair(_, _) => {
                    let mut elems = Vec::new();
                    let mut cur = args[0].clone();
                    loop {
                        match cur {
                            Value::Pair(car, cdr) => {
                                elems.push(*car);
                                cur = *cdr;
                            }
                            Value::List(ref items, _) if items.is_empty() => break,
                            _ => break,
                        }
                    }
                    elems
                }
                other => return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{other}"),
                }),
            };
            let mut result = String::new();
            for v in &elems {
                let Value::Char(c) = v else {
                    return Err(EvalError::TypeMismatch {
                        expected: "char".into(), got: format!("{v}"),
                    });
                };
                result.push(*c);
            }
            Ok(Value::String(result))
        }
        other => Err(EvalError::UnboundVariable { name: other.into() }),
    }
}

fn compare_nums(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let nums = require_ints(args)?;
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Bool(result))
}
