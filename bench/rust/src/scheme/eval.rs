use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::macros::Binding;
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
    "raise", "with-exception-handler",
    "values", "call-with-values",
    "exact?", "inexact?", "exact->inexact", "inexact->exact",
    "numerator", "denominator", "rational?", "integer?",
    "set-car!", "set-cdr!",
    "for-each", "assq", "assv", "memq", "memv", "member",
    "cadr", "cddr", "caar", "cdar", "caddr", "cdddr", "caaar", "caddar",
    "gcd", "lcm", "error", "procedure?",
    "truncate", "floor", "ceiling", "round",
    "make-string", "string",
    "string>?", "string<=?", "string>=?",
    "syntax->datum", "datum->syntax",
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
    CondArrow {
        test_result: Value,
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
    ForEach {
        func: Value,
        lists: Vec<Vec<Value>>,
        span: Option<Span>,
    },
    LetStarBind {
        name: String,
        remaining: Vec<(String, Value)>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
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
    /// Guard body completed normally — pop handler, yield value
    GuardDone,
    /// with-exception-handler thunk completed normally — pop handler, yield value
    WithExceptionHandlerDone,
    /// Evaluating guard clause tests (cond-like)
    GuardClauseTest {
        exception_value: Value,
        body: Vec<Value>,
        remaining_clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    /// Unwinding dynamic-wind during guard exception handling
    GuardUnwind {
        pending_push: Option<Winder>,
        remaining: Vec<(Value, Option<Winder>)>,
        guard_var: String,
        exception_value: Value,
        guard_clauses: Vec<Value>,
        guard_env: Rc<RefCell<Env>>,
        target_kont: Kont,
        target_winders: Vec<Winder>,
    },
    /// Handler returned from non-continuable raise — error
    RaiseHandlerReturn,
    /// call-with-values: producer returned, now apply consumer
    CallWithValues {
        consumer: Value,
        span: Option<Span>,
    },
    /// Re-evaluate the result of a macro transformer in the use-site env
    MacroExpand {
        use_env: Rc<RefCell<Env>>,
    },
    /// syntax-case: waiting for scrutinee to evaluate
    SyntaxCaseMatch {
        literals: Vec<String>,
        clauses: Vec<Value>, // raw clause forms
        env: Rc<RefCell<Env>>,
    },
    /// cleanup syntax bindings after syntax-case body returns
    SyntaxCaseCleanup,
    /// with-syntax: collecting binding values
    WithSyntaxBind {
        current_name: String,
        remaining: Vec<(String, Value)>, // (name, expr) pairs still to eval
        collected: HashMap<String, Binding>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
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
#[derive(Debug)]
struct KontNode {
    frame: Frame,
    rest: Option<Rc<KontNode>>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone)]
enum ExHandler {
    Guard {
        var: String,
        clauses: Vec<Value>,
        env: Rc<RefCell<Env>>,
        saved_kont: Kont,
        saved_winders: Vec<Winder>,
    },
    Procedure {
        handler: Value,
    },
}

/// Active syntax-case bindings for `(syntax ...)` template expansion.
#[derive(Clone)]
struct SyntaxBindings {
    bindings: HashMap<String, Binding>,
    literals: Vec<String>,
    def_env: Rc<RefCell<Env>>,
    use_env: Rc<RefCell<Env>>,
}

struct Machine {
    kont: Kont,
    saved_conts: Vec<Kont>,
    winders: Vec<Winder>,
    saved_winders: Vec<Vec<Winder>>,
    winder_counter: usize,
    output: Rc<RefCell<String>>,
    gensym_counter: usize,
    record_type_counter: usize,
    exception_handlers: Vec<ExHandler>,
    syntax_bindings_stack: Vec<SyntaxBindings>,
    /// Stack of use-site envs for active macro transformer invocations.
    macro_use_envs: Vec<Rc<RefCell<Env>>>,
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
            record_type_counter: 0,
            exception_handlers: Vec::new(),
            syntax_bindings_stack: Vec::new(),
            macro_use_envs: Vec::new(),
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
            Value::Int(_) | Value::Float(_) | Value::Rational(_, _)
            | Value::Bool(_) | Value::String(_)
            | Value::Char(_) | Value::Builtin(_) | Value::Void
            | Value::Closure { .. } | Value::Pair(_) | Value::Continuation(_)
            | Value::SyntaxRules { .. } | Value::Vector(_)
            | Value::Values(_) | Value::Record { .. }
            | Value::MacroTransformer { .. }
            | Value::CaseLambda { .. } => Ok(Control::Continue(expr)),
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
                "case-lambda" => return sf_case_lambda(&elems[1..], span, env),
                "quote" => return sf_quote(&elems[1..], span),
                "quasiquote" => {
                    if elems.len() != 2 {
                        return Err(EvalError::Parse {
                            msg: "quasiquote requires exactly 1 argument".into(),
                        }.at(span));
                    }
                    let expanded = expand_quasiquote(&elems[1], 0);
                    return Ok(Control::Eval(expanded, Rc::clone(env)));
                }
                "let" => return self.sf_let(&elems[1..], span, env),
                "begin" => return self.eval_body_in(&elems[1..], env),
                "cond" => return self.sf_cond(&elems[1..], env),
                "set!" => return self.sf_set(&elems[1..], span, env),
                "string-set!" => return self.sf_string_set(&elems[1..], span, env),
                "define-syntax" => return self.sf_define_syntax(&elems[1..], span, env),
                "syntax-case" => return self.sf_syntax_case(&elems[1..], span, env),
                "syntax" => return self.sf_syntax(&elems[1..], span, env),
                "with-syntax" => return self.sf_with_syntax(&elems[1..], span, env),
                "let*" => return self.sf_let_star(&elems[1..], span, env),
                "letrec" => return self.sf_letrec(&elems[1..], span, env, false),
                "letrec*" => return self.sf_letrec(&elems[1..], span, env, true),
                "case" => return self.sf_case(&elems[1..], span, env),
                "do" => return self.sf_do(&elems[1..], span, env),
                "guard" => return self.sf_guard(&elems[1..], span, env),
                "when" => {
                    if elems.len() < 3 {
                        return Err(EvalError::Parse {
                            msg: "when requires test and body".into(),
                        }.at(span));
                    }
                    // (when test body ...) => (if test (begin body ...) void)
                    let test = elems[1].clone();
                    let body = elems[2..].to_vec();
                    self.kont.push(Frame::If {
                        then_expr: Value::List(
                            std::iter::once(Value::Symbol("begin".into(), None))
                                .chain(body)
                                .collect(),
                            None,
                        ),
                        else_expr: None,
                        env: Rc::clone(env),
                    });
                    return Ok(Control::Eval(test, Rc::clone(env)));
                }
                "define-record-type" => return self.sf_define_record_type(&elems[1..], span, env),
                _ => {}
            }

            // Check if head is a macro (syntax-rules or syntax-case transformer)
            let macro_val = env.borrow().get(op).ok();
            if let Some(ref val) = macro_val {
                match val {
                    Value::SyntaxRules { literals, rules, def_env } => {
                        let expanded = crate::scheme::macros::expand_syntax_rules(
                            literals, rules, &elems, def_env, env,
                            &mut self.gensym_counter,
                        )?;
                        return Ok(Control::Eval(expanded, Rc::clone(env)));
                    }
                    Value::MacroTransformer { params, body, env: mac_env } => {
                        // Call the transformer with the input form as argument
                        let input_form = Value::List(elems, span);
                        let call_env = Env::with_parent(mac_env);
                        if let Some(param) = params.first() {
                            call_env.borrow_mut().define(param.clone(), input_form);
                        }
                        let body = body.clone();
                        // Track use-site env for hygiene in (syntax ...) expansion
                        self.macro_use_envs.push(Rc::clone(env));
                        // Push frame to re-eval the expanded form in use-site env
                        self.kont.push(Frame::MacroExpand {
                            use_env: Rc::clone(env),
                        });
                        return self.eval_body_in(&body, &call_env);
                    }
                    _ => {}
                }
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
                    } else if body.len() == 2
                        && matches!(&body[0], Value::Symbol(s, _) if s == "=>")
                    {
                        // (cond (test => proc)) — apply proc to test result
                        self.kont.push(Frame::CondArrow { test_result: val });
                        Ok(Control::Eval(body[1].clone(), env))
                    } else {
                        self.eval_body_in(&body, &env)
                    }
                } else {
                    self.start_cond(&remaining_clauses, &env)
                }
            }
            Frame::CondArrow { test_result } => {
                // val is the proc, apply it to test_result
                self.apply_func(val, vec![test_result], None)
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
            frame @ Frame::NamedLetBindings { .. } => {
                self.continue_named_let(val, frame)
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
                apply_string_set_char(val, var_name, index, &env, span)
            }
            Frame::Map { func, lists, mut results, span } => {
                results.push(val);
                if lists[0].is_empty() {
                    Ok(Control::Continue(Value::list_from_vec(results)))
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
            Frame::ForEach { func, lists, span } => {
                // Ignore return value, just proceed to next iteration
                if lists[0].is_empty() {
                    Ok(Control::Continue(Value::Void))
                } else {
                    let mut next_args = Vec::with_capacity(lists.len());
                    let mut remaining = Vec::with_capacity(lists.len());
                    for mut lst in lists {
                        next_args.push(lst.remove(0));
                        remaining.push(lst);
                    }
                    self.kont.push(Frame::ForEach {
                        func: func.clone(), lists: remaining, span,
                    });
                    Ok(Control::Apply(func, next_args, span))
                }
            }
            Frame::LetStarBind { name, mut remaining, body, env } => {
                env.borrow_mut().define(name, val);
                if remaining.is_empty() {
                    self.eval_body_in(&body, &env)
                } else {
                    let (next_name, next_expr) = remaining.remove(0);
                    let eval_env = Rc::clone(&env);
                    self.kont.push(Frame::LetStarBind {
                        name: next_name, remaining, body, env,
                    });
                    Ok(Control::Eval(next_expr, eval_env))
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
            Frame::GuardDone => {
                self.exception_handlers.pop();
                Ok(Control::Continue(val))
            }
            Frame::WithExceptionHandlerDone => {
                self.exception_handlers.pop();
                Ok(Control::Continue(val))
            }
            Frame::GuardClauseTest { exception_value, body, remaining_clauses, env } => {
                if !is_false(&val) {
                    if body.is_empty() {
                        Ok(Control::Continue(val))
                    } else {
                        self.eval_body_in(&body, &env)
                    }
                } else {
                    self.dispatch_guard_clause(exception_value, &remaining_clauses, &env)
                }
            }
            Frame::GuardUnwind {
                pending_push, mut remaining,
                guard_var, exception_value, guard_clauses, guard_env,
                target_kont, target_winders,
            } => {
                if let Some(w) = pending_push {
                    self.winders.push(w);
                }
                if let Some((thunk, push)) = remaining.first().cloned() {
                    remaining.remove(0);
                    self.kont.push(Frame::GuardUnwind {
                        pending_push: push,
                        remaining,
                        guard_var, exception_value, guard_clauses, guard_env,
                        target_kont, target_winders,
                    });
                    Ok(Control::Apply(thunk, vec![], None))
                } else {
                    self.kont = target_kont;
                    self.winders = target_winders;
                    self.start_guard_clauses(guard_var, exception_value, &guard_clauses, &guard_env)
                }
            }
            Frame::RaiseHandlerReturn => {
                Err(EvalError::Raised {
                    value: "handler returned from non-continuable exception".into(),
                })
            }
            Frame::CallWithValues { consumer, span } => {
                let args = match val {
                    Value::Values(vs) => vs,
                    other => vec![other],
                };
                Ok(Control::Apply(consumer, args, span))
            }
            Frame::MacroExpand { use_env } => {
                // val is the expanded form from the macro transformer
                self.macro_use_envs.pop();
                Ok(Control::Eval(val, use_env))
            }
            Frame::SyntaxCaseMatch { literals, clauses, env } => {
                self.handle_syntax_case_match(val, &literals, &clauses, &env)
            }
            Frame::SyntaxCaseCleanup => {
                self.syntax_bindings_stack.pop();
                Ok(Control::Continue(val))
            }
            Frame::WithSyntaxBind { current_name, remaining, collected, body, env } => {
                self.continue_with_syntax_bind(val, current_name, remaining, collected, body, env)
            }
        }
    }

    fn continue_named_let(
        &mut self, val: Value, frame: Frame,
    ) -> Result<Control, EvalError> {
        let Frame::NamedLetBindings {
            name, params, mut done, current_name, mut remaining, body, outer_env,
        } = frame else {
            unreachable!("continue_named_let called with non-NamedLetBindings frame");
        };
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

    fn continue_with_syntax_bind(
        &mut self, val: Value, current_name: String,
        remaining: Vec<(String, Value)>,
        mut collected: HashMap<String, Binding>,
        body: Vec<Value>, env: Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        collected.insert(current_name, Binding::Single(val));
        if remaining.is_empty() {
            let base = self.syntax_bindings_stack.last().cloned();
            let mut merged_bindings = base.as_ref()
                .map(|sb| sb.bindings.clone())
                .unwrap_or_default();
            let literals = base.as_ref()
                .map(|sb| sb.literals.clone())
                .unwrap_or_default();
            let def_env = base.as_ref()
                .map(|sb| Rc::clone(&sb.def_env))
                .unwrap_or_else(|| Rc::clone(&env));
            let use_env = base.as_ref()
                .map(|sb| Rc::clone(&sb.use_env))
                .unwrap_or_else(|| Rc::clone(&env));
            merged_bindings.extend(collected);
            self.syntax_bindings_stack.push(SyntaxBindings {
                bindings: merged_bindings,
                literals,
                def_env,
                use_env,
            });
            self.kont.push(Frame::SyntaxCaseCleanup);
            self.eval_body_in(&body, &env)
        } else {
            let mut rest = remaining;
            let (next_name, next_expr) = rest.remove(0);
            self.kont.push(Frame::WithSyntaxBind {
                current_name: next_name,
                remaining: rest,
                collected,
                body,
                env: Rc::clone(&env),
            });
            Ok(Control::Eval(next_expr, env))
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
            Value::CaseLambda { clauses, env } => {
                let arg_count = args.len();
                let matched = clauses.iter().find(|(params, rest_param, _)| {
                    match rest_param {
                        Some(_) => arg_count >= params.len(),
                        None => arg_count == params.len(),
                    }
                });
                let Some((params, rest_param, body)) = matched else {
                    return Err(EvalError::WrongArgCount {
                        expected: clauses.first().map_or(0, |(p, _, _)| p.len()),
                        got: arg_count,
                    }.at(span));
                };
                let call_env = Env::with_parent(&env);
                for (param, arg) in params.iter().zip(args.iter()) {
                    call_env.borrow_mut().define(param.clone(), arg.clone());
                }
                if let Some(rest) = rest_param {
                    let rest_args = Value::List(args[params.len()..].to_vec(), None);
                    call_env.borrow_mut().define(rest.clone(), rest_args);
                }
                self.eval_body_in(body, &call_env)
            }
            Value::Continuation(id) => {
                let val = match args.len() {
                    0 => Value::Void,
                    1 => args.into_iter().next().expect("checked len"),
                    _ => Value::Values(args),
                };
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
                    let Some(elems) = value_to_vec(arg) else {
                        return Err(EvalError::TypeMismatch {
                            expected: "list".into(), got: format!("{arg}"),
                        }.at(span));
                    };
                    lists.push(elems);
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
            "for-each" => {
                if args.len() < 2 {
                    return Err(EvalError::WrongArgCount {
                        expected: 2, got: args.len(),
                    }.at(span));
                }
                let func = args[0].clone();
                let mut lists: Vec<Vec<Value>> = Vec::with_capacity(args.len() - 1);
                for arg in &args[1..] {
                    let Some(elems) = value_to_vec(arg) else {
                        return Err(EvalError::TypeMismatch {
                            expected: "list".into(), got: format!("{arg}"),
                        }.at(span));
                    };
                    lists.push(elems);
                }
                if lists[0].is_empty() {
                    return Ok(Control::Continue(Value::Void));
                }
                let mut first_args = Vec::with_capacity(lists.len());
                let mut remaining = Vec::with_capacity(lists.len());
                for mut lst in lists {
                    first_args.push(lst.remove(0));
                    remaining.push(lst);
                }
                self.kont.push(Frame::ForEach {
                    func: func.clone(), lists: remaining, span,
                });
                Ok(Control::Apply(func, first_args, span))
            }
            "apply" => {
                if args.len() < 2 {
                    return Err(EvalError::WrongArgCount {
                        expected: 2, got: args.len(),
                    }.at(span));
                }
                let Some(tail_args) = value_to_vec(&args[args.len() - 1]) else {
                    return Err(EvalError::TypeMismatch {
                        expected: "list".into(),
                        got: format!("{}", args[args.len() - 1]),
                    }.at(span));
                };
                let new_func = args[0].clone();
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                all_args.extend(tail_args);
                Ok(Control::Apply(new_func, all_args, span))
            }
            "raise" => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCount {
                        expected: 1, got: args.len(),
                    }.at(span));
                }
                let val = args.into_iter().next().expect("checked len");
                self.handle_raise(val, span)
            }
            "with-exception-handler" => {
                if args.len() != 2 {
                    return Err(EvalError::WrongArgCount {
                        expected: 2, got: args.len(),
                    }.at(span));
                }
                let mut it = args.into_iter();
                let handler = it.next().expect("checked len");
                let thunk = it.next().expect("checked len");
                self.exception_handlers.push(ExHandler::Procedure { handler });
                self.kont.push(Frame::WithExceptionHandlerDone);
                Ok(Control::Apply(thunk, vec![], span))
            }
            "values" => {
                if args.len() == 1 {
                    Ok(Control::Continue(args.into_iter().next().expect("checked len")))
                } else {
                    Ok(Control::Continue(Value::Values(args)))
                }
            }
            "call-with-values" => {
                if args.len() != 2 {
                    return Err(EvalError::WrongArgCount {
                        expected: 2, got: args.len(),
                    }.at(span));
                }
                let mut it = args.into_iter();
                let producer = it.next().expect("checked len");
                let consumer = it.next().expect("checked len");
                self.kont.push(Frame::CallWithValues { consumer, span });
                Ok(Control::Apply(producer, vec![], span))
            }
            _ => {
                if let Some(rest) = name.strip_prefix("__record_ctor_") {
                    let type_id: usize = rest.parse().expect("valid record type id");
                    Ok(Control::Continue(Value::Record { type_id, fields: args }))
                } else if let Some(rest) = name.strip_prefix("__record_pred_") {
                    let type_id: usize = rest.parse().expect("valid record type id");
                    if args.len() != 1 {
                        return Err(EvalError::WrongArgCount {
                            expected: 1, got: args.len(),
                        }.at(span));
                    }
                    let is_match = matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id);
                    Ok(Control::Continue(Value::Bool(is_match)))
                } else if let Some(rest) = name.strip_prefix("__record_acc_") {
                    // Format: __record_acc_{type_id}_{field_idx}
                    let mut parts = rest.splitn(2, '_');
                    let type_id: usize = parts.next().expect("type_id").parse().expect("valid type id");
                    let field_idx: usize = parts.next().expect("field_idx").parse().expect("valid field idx");
                    if args.len() != 1 {
                        return Err(EvalError::WrongArgCount {
                            expected: 1, got: args.len(),
                        }.at(span));
                    }
                    let Value::Record { type_id: tid, ref fields } = args[0] else {
                        return Err(EvalError::TypeMismatch {
                            expected: "record".into(),
                            got: format!("{}", args[0]),
                        }.at(span));
                    };
                    if tid != type_id {
                        return Err(EvalError::TypeMismatch {
                            expected: "matching record type".into(),
                            got: "different record type".into(),
                        }.at(span));
                    }
                    Ok(Control::Continue(fields[field_idx].clone()))
                } else {
                    let result = apply_builtin(name, &args, &self.output)
                        .map_err(|e| e.at(span))?;
                    Ok(Control::Continue(result))
                }
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
            Value::Pair(cell) => {
                // (define (name . rest) body) — function with only rest param
                let (car, cdr) = {
                    let b = cell.borrow();
                    (b.0.clone(), b.1.clone())
                };
                // Extract name and parameters from the pair structure
                let (name, params, rest_param) = extract_define_pair_sig(&car, &cdr, span)?;
                let body = args[1..].to_vec();
                if body.is_empty() {
                    return Err(EvalError::Parse {
                        msg: "define: empty body".into(),
                    }.at(span));
                }
                let closure = Value::Closure {
                    params, rest_param, body, env: Rc::clone(env),
                };
                env.borrow_mut().define(name, closure);
                Ok(Control::Continue(Value::Void))
            }
            Value::Int(_) | Value::Float(_) | Value::Rational(_, _)
            | Value::Bool(_) | Value::String(_) | Value::Char(_)
            | Value::Builtin(_) | Value::Closure { .. }
            | Value::Continuation(_) | Value::SyntaxRules { .. }
            | Value::Vector(_) | Value::Values(_) | Value::Record { .. }
            | Value::MacroTransformer { .. } | Value::CaseLambda { .. } | Value::Void => {
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

    fn sf_let_star(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: "let* requires bindings and body".into(),
            }.at(span));
        }
        let Value::List(bindings_list, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "let*: expected bindings list".into(),
            }.at(span));
        };
        let let_env = Env::with_parent(env);
        let body = args[1..].to_vec();
        if bindings_list.is_empty() {
            return self.eval_body_in(&body, &let_env);
        }
        // Parse all binding pairs
        let mut binding_pairs = Vec::new();
        for binding in bindings_list {
            let Value::List(pair, _) = binding else {
                return Err(EvalError::Parse {
                    msg: "let*: expected binding pair".into(),
                }.at(span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "let*: binding must have 2 elements".into(),
                }.at(span));
            }
            let Value::Symbol(name, _) = &pair[0] else {
                return Err(EvalError::Parse {
                    msg: "let*: expected variable name".into(),
                }.at(span));
            };
            binding_pairs.push((name.clone(), pair[1].clone()));
        }
        // Evaluate sequentially: each binding visible to the next
        let (first_name, first_expr) = binding_pairs.remove(0);
        if binding_pairs.is_empty() {
            // Only one binding: evaluate it, define, then body
            self.kont.push(Frame::LetStarBind {
                name: first_name,
                remaining: Vec::new(),
                body,
                env: let_env,
            });
            Ok(Control::Eval(first_expr, Rc::clone(env)))
        } else {
            self.kont.push(Frame::LetStarBind {
                name: first_name,
                remaining: binding_pairs,
                body,
                env: Rc::clone(&let_env),
            });
            Ok(Control::Eval(first_expr, Rc::clone(env)))
        }
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

        // Check if it's a lambda transformer (syntax-case macro)
        let is_lambda = matches!(&sr_elems[0], Value::Symbol(s, _) if s == "lambda");
        if is_lambda {
            // (define-syntax name (lambda (stx) body ...))
            let lambda_args = &sr_elems[1..];
            if lambda_args.len() < 2 {
                return Err(EvalError::Parse {
                    msg: "define-syntax lambda requires params and body".into(),
                }.at(span));
            }
            let Value::List(param_list, _) = &lambda_args[0] else {
                return Err(EvalError::Parse {
                    msg: "define-syntax lambda: expected parameter list".into(),
                }.at(span));
            };
            let params: Vec<String> = param_list.iter().map(|p| {
                if let Value::Symbol(s, _) = p { Ok(s.clone()) } else {
                    Err(EvalError::Parse {
                        msg: "define-syntax lambda: expected parameter name".into(),
                    }.at(span))
                }
            }).collect::<Result<_, _>>()?;
            let body = lambda_args[1..].to_vec();
            let transformer = Value::MacroTransformer {
                params, body, env: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), transformer);
            return Ok(Control::Continue(Value::Void));
        }

        let is_syntax_rules = matches!(&sr_elems[0], Value::Symbol(s, _) if s == "syntax-rules");
        if !is_syntax_rules {
            return Err(EvalError::Parse {
                msg: "define-syntax: expected syntax-rules or lambda".into(),
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

    // --- syntax-case support ---

    fn sf_syntax_case(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        // (syntax-case expr (literals ...) clause ...)
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: "syntax-case requires expr and literals".into(),
            }.at(span));
        }
        let expr = args[0].clone();
        let Value::List(lit_list, _) = &args[1] else {
            return Err(EvalError::Parse {
                msg: "syntax-case: expected literals list".into(),
            }.at(span));
        };
        let literals: Vec<String> = lit_list.iter().filter_map(|v| {
            if let Value::Symbol(s, _) = v { Some(s.clone()) } else { None }
        }).collect();
        let clauses = args[2..].to_vec();
        self.kont.push(Frame::SyntaxCaseMatch {
            literals,
            clauses,
            env: Rc::clone(env),
        });
        Ok(Control::Eval(expr, Rc::clone(env)))
    }

    fn handle_syntax_case_match(
        &mut self,
        scrutinee: Value,
        literals: &[String],
        clauses: &[Value],
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        let input_elems = scrutinee.to_vec().unwrap_or_else(|| vec![scrutinee.clone()]);

        for clause in clauses {
            let Value::List(parts, _) = clause else {
                return Err(EvalError::Parse {
                    msg: "syntax-case: clause must be a list".into(),
                });
            };
            if parts.len() < 2 {
                return Err(EvalError::Parse {
                    msg: "syntax-case: clause must have pattern and body".into(),
                });
            }
            let pattern = &parts[0];
            // Body is last element, fender is optional (parts[1] if len==3)
            let body = &parts[parts.len() - 1];

            let Value::List(pat_elems, _) = pattern else {
                return Err(EvalError::Parse {
                    msg: "syntax-case: pattern must be a list".into(),
                });
            };

            let mut bindings = HashMap::new();
            if crate::scheme::macros::match_pattern(pat_elems, &input_elems, literals, &mut bindings) {
                // Push syntax bindings for use by (syntax ...) / #'
                self.syntax_bindings_stack.push(SyntaxBindings {
                    bindings,
                    literals: literals.to_vec(),
                    def_env: Rc::clone(env),
                    use_env: Rc::clone(env),
                });
                self.kont.push(Frame::SyntaxCaseCleanup);
                return Ok(Control::Eval(body.clone(), Rc::clone(env)));
            }
        }
        Err(EvalError::Parse {
            msg: "syntax-case: no matching clause".into(),
        })
    }

    fn sf_syntax(
        &mut self, args: &[Value], span: Option<Span>, _env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        // (syntax template) — expand template using current syntax-case bindings
        if args.len() != 1 {
            return Err(EvalError::Parse {
                msg: "syntax requires exactly 1 argument".into(),
            }.at(span));
        }
        let template = &args[0];
        let Some(sb) = self.syntax_bindings_stack.last() else {
            return Err(EvalError::Parse {
                msg: "syntax used outside syntax-case context".into(),
            }.at(span));
        };
        let bindings = sb.bindings.clone();
        let literals = sb.literals.clone();
        let def_env = Rc::clone(&sb.def_env);
        // Use the actual macro use-site env for hygiene injection
        let use_env = self.macro_use_envs.last()
            .map(Rc::clone)
            .unwrap_or_else(|| Rc::clone(&sb.use_env));
        let mut gensyms = HashMap::new();
        let expanded = crate::scheme::macros::expand_template(
            template, &bindings, &literals, &def_env, &use_env,
            &mut self.gensym_counter, &mut gensyms,
        )?;
        Ok(Control::Continue(expanded))
    }

    fn sf_with_syntax(
        &mut self, args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        // (with-syntax ((name expr) ...) body ...)
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: "with-syntax requires bindings and body".into(),
            }.at(span));
        }
        let Value::List(binding_list, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "with-syntax: expected bindings list".into(),
            }.at(span));
        };
        let body = args[1..].to_vec();
        let mut pairs: Vec<(String, Value)> = Vec::new();
        for binding in binding_list {
            let Value::List(parts, _) = binding else {
                return Err(EvalError::Parse {
                    msg: "with-syntax: binding must be a list".into(),
                }.at(span));
            };
            if parts.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "with-syntax: binding must have name and expression".into(),
                }.at(span));
            }
            let Value::Symbol(name, _) = &parts[0] else {
                return Err(EvalError::Parse {
                    msg: "with-syntax: expected binding name".into(),
                }.at(span));
            };
            pairs.push((name.clone(), parts[1].clone()));
        }
        if pairs.is_empty() {
            return self.eval_body_in(&body, env);
        }
        let (first_name, first_expr) = pairs.remove(0);
        self.kont.push(Frame::WithSyntaxBind {
            current_name: first_name,
            remaining: pairs,
            collected: HashMap::new(),
            body,
            env: Rc::clone(env),
        });
        Ok(Control::Eval(first_expr, Rc::clone(env)))
    }

    // --- Record types ---

    fn sf_define_record_type(
        &mut self,
        args: &[Value],
        span: Option<Span>,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
        if args.len() < 3 {
            return Err(EvalError::Parse {
                msg: "define-record-type requires type name, constructor, predicate, and fields".into(),
            }.at(span));
        }
        // Type name (ignored for now, we just need a unique id)
        let Value::Symbol(_, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "define-record-type: expected type name".into(),
            }.at(span));
        };

        // Constructor: (make-foo field1 field2 ...)
        let Value::List(ctor_elems, _) = &args[1] else {
            return Err(EvalError::Parse {
                msg: "define-record-type: expected constructor spec".into(),
            }.at(span));
        };
        if ctor_elems.is_empty() {
            return Err(EvalError::Parse {
                msg: "define-record-type: empty constructor spec".into(),
            }.at(span));
        }
        let Value::Symbol(ctor_name, _) = &ctor_elems[0] else {
            return Err(EvalError::Parse {
                msg: "define-record-type: expected constructor name".into(),
            }.at(span));
        };
        let ctor_fields: Vec<String> = ctor_elems[1..].iter().map(|v| {
            if let Value::Symbol(s, _) = v { s.clone() }
            else { String::new() }
        }).collect();

        // Predicate
        let Value::Symbol(pred_name, _) = &args[2] else {
            return Err(EvalError::Parse {
                msg: "define-record-type: expected predicate name".into(),
            }.at(span));
        };

        // Field accessors: (field accessor) ...
        let mut field_accessors: Vec<(String, String)> = Vec::new();
        for field_spec in &args[3..] {
            let Value::List(parts, _) = field_spec else {
                return Err(EvalError::Parse {
                    msg: "define-record-type: expected field spec (field accessor)".into(),
                }.at(span));
            };
            if parts.len() < 2 {
                return Err(EvalError::Parse {
                    msg: "define-record-type: field spec must have field name and accessor".into(),
                }.at(span));
            }
            let Value::Symbol(field_name, _) = &parts[0] else {
                return Err(EvalError::Parse {
                    msg: "define-record-type: expected field name".into(),
                }.at(span));
            };
            let Value::Symbol(accessor_name, _) = &parts[1] else {
                return Err(EvalError::Parse {
                    msg: "define-record-type: expected accessor name".into(),
                }.at(span));
            };
            field_accessors.push((field_name.clone(), accessor_name.clone()));
        }

        // Allocate a unique type id
        let type_id = self.record_type_counter;
        self.record_type_counter += 1;

        // Build a mapping from field name to index in the ctor_fields order
        let field_count = ctor_fields.len();

        // Define constructor as a closure that creates a Record value
        // We use a special builtin name to dispatch in apply_builtin_dispatch
        let ctor_builtin_name = format!("__record_ctor_{type_id}");
        env.borrow_mut().define(
            ctor_name.clone(),
            Value::Builtin(ctor_builtin_name.clone()),
        );

        // Define predicate
        let pred_builtin_name = format!("__record_pred_{type_id}");
        env.borrow_mut().define(
            pred_name.clone(),
            Value::Builtin(pred_builtin_name.clone()),
        );

        // Define accessors
        for (field_name, accessor_name) in &field_accessors {
            // Find the index of this field in the constructor field list
            let idx = ctor_fields.iter().position(|f| f == field_name).ok_or_else(|| {
                EvalError::Parse {
                    msg: format!("define-record-type: field {field_name} not in constructor"),
                }
            })?;
            let accessor_builtin_name = format!("__record_acc_{type_id}_{idx}");
            env.borrow_mut().define(
                accessor_name.clone(),
                Value::Builtin(accessor_builtin_name),
            );
        }

        // Store record type info for dispatch
        // We use a convention: builtin names starting with __record_ are dispatched specially
        // Store field_count for the constructor
        env.borrow_mut().define(
            format!("__record_meta_{type_id}"),
            Value::Int(field_count as i64),
        );

        Ok(Control::Continue(Value::Void))
    }

    // --- Exception handling ---

    fn sf_guard(
        &mut self,
        args: &[Value],
        span: Option<Span>,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Parse {
                msg: "guard requires variable/clauses and body".into(),
            }.at(span));
        }
        let Value::List(header, _) = &args[0] else {
            return Err(EvalError::Parse {
                msg: "guard: expected (variable clause ...)".into(),
            }.at(span));
        };
        if header.is_empty() {
            return Err(EvalError::Parse {
                msg: "guard: expected variable name".into(),
            }.at(span));
        }
        let Value::Symbol(var, _) = &header[0] else {
            return Err(EvalError::Parse {
                msg: "guard: expected variable name".into(),
            }.at(span));
        };
        let clauses = header[1..].to_vec();
        let body = args[1..].to_vec();

        let saved_kont = self.kont.clone();
        let saved_winders = self.winders.clone();

        self.exception_handlers.push(ExHandler::Guard {
            var: var.clone(),
            clauses,
            env: Rc::clone(env),
            saved_kont,
            saved_winders,
        });

        self.kont.push(Frame::GuardDone);
        self.eval_body_in(&body, env)
    }

    fn handle_raise(
        &mut self,
        val: Value,
        span: Option<Span>,
    ) -> Result<Control, EvalError> {
        let handler = self.exception_handlers.pop();
        match handler {
            None => Err(EvalError::Raised { value: format!("{val}") }),
            Some(ExHandler::Procedure { handler: handler_proc }) => {
                self.kont.push(Frame::RaiseHandlerReturn);
                Ok(Control::Apply(handler_proc, vec![val], span))
            }
            Some(ExHandler::Guard {
                var, clauses, env, saved_kont, saved_winders,
            }) => {
                let common = self.winders.iter().zip(saved_winders.iter())
                    .take_while(|(a, b)| a.id == b.id)
                    .count();
                let need_unwind = self.winders.len() > common;
                let need_rewind = saved_winders.len() > common;

                if !need_unwind && !need_rewind {
                    self.kont = saved_kont;
                    self.winders = saved_winders;
                    self.start_guard_clauses(var, val, &clauses, &env)
                } else {
                    let mut actions: Vec<(Value, Option<Winder>)> = Vec::new();
                    for w in self.winders[common..].iter().rev() {
                        actions.push((w.out_thunk.clone(), None));
                    }
                    self.winders.truncate(common);
                    for w in &saved_winders[common..] {
                        actions.push((w.in_thunk.clone(), Some(w.clone())));
                    }
                    let (first_thunk, first_push) = actions.remove(0);
                    self.kont.push(Frame::GuardUnwind {
                        pending_push: first_push,
                        remaining: actions,
                        guard_var: var,
                        exception_value: val,
                        guard_clauses: clauses,
                        guard_env: env,
                        target_kont: saved_kont,
                        target_winders: saved_winders,
                    });
                    Ok(Control::Apply(first_thunk, vec![], span))
                }
            }
        }
    }

    fn start_guard_clauses(
        &mut self,
        var: String,
        exception_val: Value,
        clauses: &[Value],
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        let guard_env = Env::with_parent(env);
        guard_env.borrow_mut().define(var, exception_val.clone());
        self.dispatch_guard_clause(exception_val, clauses, &guard_env)
    }

    fn dispatch_guard_clause(
        &mut self,
        exception_val: Value,
        clauses: &[Value],
        env: &Rc<RefCell<Env>>,
    ) -> Result<Control, EvalError> {
        if clauses.is_empty() {
            return self.handle_raise(exception_val, None);
        }
        let Value::List(parts, _) = &clauses[0] else {
            return Err(EvalError::Parse { msg: "guard: expected clause".into() });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse { msg: "guard: empty clause".into() });
        }
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                return self.eval_body_in(&parts[1..], env);
            }
        }
        self.kont.push(Frame::GuardClauseTest {
            exception_value: exception_val,
            body: parts[1..].to_vec(),
            remaining_clauses: clauses[1..].to_vec(),
            env: Rc::clone(env),
        });
        Ok(Control::Eval(parts[0].clone(), Rc::clone(env)))
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
        Value::Int(_) | Value::Float(_) | Value::Rational(_, _)
        | Value::Bool(_) | Value::String(_) | Value::Char(_)
        | Value::Builtin(_) | Value::Closure { .. } | Value::Pair(_)
        | Value::Continuation(_) | Value::SyntaxRules { .. }
        | Value::Vector(_) | Value::Values(_) | Value::Record { .. }
        | Value::MacroTransformer { .. } | Value::CaseLambda { .. } | Value::Void => val,
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
    let (params, rest_param) = match &args[0] {
        Value::List(param_list, _) => parse_params(param_list, span, "lambda")?,
        Value::Symbol(name, _) => (vec![], Some(name.clone())),
        Value::Pair(cell) => {
            let (car, cdr) = {
                let b = cell.borrow();
                (b.0.clone(), b.1.clone())
            };
            let (name, mut p, r) = extract_define_pair_sig(&car, &cdr, span)?;
            p.insert(0, name);
            (p, r)
        }
        _ => return Err(EvalError::Parse {
            msg: "lambda: expected parameter list".into(),
        }.at(span)),
    };
    let body = args[1..].to_vec();
    Ok(Control::Continue(Value::Closure {
        params, rest_param, body, env: Rc::clone(env),
    }))
}

fn sf_case_lambda(
    args: &[Value], span: Option<Span>, env: &Rc<RefCell<Env>>,
) -> Result<Control, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse {
            msg: "case-lambda requires at least one clause".into(),
        }.at(span));
    }
    let mut clauses = Vec::new();
    for clause in args {
        let Value::List(elems, _) = clause else {
            return Err(EvalError::Parse {
                msg: "case-lambda: each clause must be a list".into(),
            }.at(span));
        };
        if elems.is_empty() {
            return Err(EvalError::Parse {
                msg: "case-lambda: clause must have params and body".into(),
            }.at(span));
        }
        let (params, rest_param) = match &elems[0] {
            Value::List(param_list, _) => parse_params(param_list, span, "case-lambda")?,
            Value::Symbol(name, _) => (vec![], Some(name.clone())),
            Value::Pair(cell) => {
                let (car, cdr) = {
                    let b = cell.borrow();
                    (b.0.clone(), b.1.clone())
                };
                let (_, p, r) = extract_define_pair_sig(&car, &cdr, span)?;
                // Prepend the car as first param
                let Value::Symbol(first, _) = &car else {
                    return Err(EvalError::Parse {
                        msg: "case-lambda: expected parameter name".into(),
                    }.at(span));
                };
                let mut params = vec![first.clone()];
                params.extend(p);
                (params, r)
            }
            _ => return Err(EvalError::Parse {
                msg: "case-lambda: expected parameter list".into(),
            }.at(span)),
        };
        let body = elems[1..].to_vec();
        clauses.push((params, rest_param, body));
    }
    Ok(Control::Continue(Value::CaseLambda {
        clauses,
        env: Rc::clone(env),
    }))
}

fn is_splicing_unquote(val: &Value) -> bool {
    let Value::List(inner, _) = val else { return false };
    inner.len() == 2 && matches!(&inner[0], Value::Symbol(s, _) if s == "unquote-splicing")
}

/// Expand quasiquote template into evaluable code.
/// `depth` tracks nesting: 0 = outermost quasiquote.
fn expand_quasiquote(template: &Value, depth: usize) -> Value {
    match template {
        Value::List(elems, span) if !elems.is_empty() => {
            // Check for (unquote expr) at this depth
            if let Value::Symbol(s, _) = &elems[0] {
                if s == "unquote" && elems.len() == 2 {
                    if depth == 0 {
                        return elems[1].clone();
                    }
                    // Nested unquote — decrease depth
                    let inner = expand_quasiquote(&elems[1], depth - 1);
                    return Value::List(
                        vec![Value::Symbol("list".into(), None),
                             Value::List(vec![Value::Symbol("quote".into(), None),
                                              Value::Symbol("unquote".into(), None)], None),
                             inner],
                        *span,
                    );
                }
                if s == "quasiquote" && elems.len() == 2 {
                    // Nested quasiquote — increase depth
                    let inner = expand_quasiquote(&elems[1], depth + 1);
                    return Value::List(
                        vec![Value::Symbol("list".into(), None),
                             Value::List(vec![Value::Symbol("quote".into(), None),
                                              Value::Symbol("quasiquote".into(), None)], None),
                             inner],
                        *span,
                    );
                }
            }
            // General list: build with append to handle splicing
            let mut parts = Vec::new();
            for elem in elems {
                if depth == 0 && is_splicing_unquote(elem) {
                    if let Value::List(inner, _) = elem {
                        parts.push(inner[1].clone());
                        continue;
                    }
                }
                let expanded = expand_quasiquote(elem, depth);
                parts.push(Value::List(
                    vec![Value::Symbol("list".into(), None), expanded],
                    None,
                ));
            }
            if parts.len() == 1 {
                parts.into_iter().next().expect("single part")
            } else {
                let mut result = vec![Value::Symbol("append".into(), None)];
                result.extend(parts);
                Value::List(result, *span)
            }
        }
        Value::Pair(cell) => {
            let (car, cdr) = {
                let b = cell.borrow();
                (b.0.clone(), b.1.clone())
            };
            // Check for (unquote expr) as a pair
            if let Value::Symbol(s, _) = &car {
                if s == "unquote"
                    && depth == 0 {
                        if let Value::Pair(inner_cell) = &cdr {
                            let b = inner_cell.borrow();
                            return b.0.clone();
                        }
                    }
            }
            let exp_car = expand_quasiquote(&car, depth);
            let exp_cdr = expand_quasiquote(&cdr, depth);
            Value::List(
                vec![Value::Symbol("cons".into(), None), exp_car, exp_cdr],
                None,
            )
        }
        Value::Vector(elems_cell) => {
            let elems = elems_cell.borrow();
            let list_form = Value::List(elems.to_vec(), None);
            let expanded = expand_quasiquote(&list_form, depth);
            Value::List(
                vec![Value::Symbol("list->vector".into(), None), expanded],
                None,
            )
        }
        // Atoms are self-quoting
        _ => Value::List(
            vec![Value::Symbol("quote".into(), None), template.clone()],
            None,
        ),
    }
}

fn sf_quote(args: &[Value], span: Option<Span>) -> Result<Control, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse {
            msg: "quote requires exactly 1 argument".into(),
        }.at(span));
    }
    Ok(Control::Continue(args[0].clone()))
}

/// Extract function name, params, and rest param from a dotted-pair define signature.
/// Handles (name . rest) and (name p1 p2 ... . rest)
fn extract_define_pair_sig(
    car: &Value, cdr: &Value, span: Option<Span>,
) -> Result<(String, Vec<String>, Option<String>), EvalError> {
    let Value::Symbol(name, _) = car else {
        return Err(EvalError::Parse {
            msg: format!("define: expected function name, got {car}"),
        }.at(span));
    };
    // Walk the cdr to collect params and rest
    let mut params = Vec::new();
    let mut current = cdr.clone();
    loop {
        match current {
            Value::Pair(cell) => {
                let (c, d) = {
                    let b = cell.borrow();
                    (b.0.clone(), b.1.clone())
                };
                let Value::Symbol(param, _) = c else {
                    return Err(EvalError::Parse {
                        msg: format!("define: expected parameter name, got {c}"),
                    }.at(span));
                };
                params.push(param);
                current = d;
            }
            Value::Symbol(rest, _) => {
                return Ok((name.clone(), params, Some(rest)));
            }
            Value::List(elems, _) if elems.is_empty() => {
                return Ok((name.clone(), params, None));
            }
            other => {
                return Err(EvalError::Parse {
                    msg: format!("define: unexpected in parameter list: {other}"),
                }.at(span));
            }
        }
    }
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
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
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
    crate::scheme::value::display_value_inner(val)
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
        | "odd?" | "even?"
        | "truncate" | "floor" | "ceiling" | "round" => apply_numeric_builtin(name, args),
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
        "cons" | "car" | "cdr" | "null?" | "list" | "append" | "reverse" | "length"
        | "set-car!" | "set-cdr!" | "assq" | "assv" | "memq" | "memv" | "member"
        | "cadr" | "cddr" | "caar" | "cdar" | "caddr" | "cdddr" | "caaar" | "caddar" =>
            apply_list_builtin(name, args),
        "string?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::String(_)))),
        "number?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::Int(_) | Value::Float(_) | Value::Rational(_, _)))),
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let result = match &args[0] {
                Value::Int(_) => true,
                Value::Rational(_, _) => false, // already simplified, so d != 1
                Value::Float(f) => *f == f.floor() && f.is_finite(),
                _ => false,
            };
            Ok(Value::Bool(result))
        }
        "rational?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::Int(_) | Value::Rational(_, _)))),
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Int(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Float(*n as f64)),
                Value::Rational(n, d) => Ok(Value::Float(*n as f64 / *d as f64)),
                Value::Float(f) => Ok(Value::Float(*f)),
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Rational(n, d) => Ok(Value::Rational(*n, *d)),
                Value::Float(f) => Ok(float_to_exact(*f)),
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Rational(n, _) => Ok(Value::Int(*n)),
                other => Err(EvalError::TypeMismatch {
                    expected: "rational".into(), got: format!("{other}"),
                }),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(_) => Ok(Value::Int(1)),
                Value::Rational(_, d) => Ok(Value::Int(*d)),
                other => Err(EvalError::TypeMismatch {
                    expected: "rational".into(), got: format!("{other}"),
                }),
            }
        }
        "boolean?" => Ok(Value::Bool(args.len() == 1 && matches!(&args[0], Value::Bool(_)))),
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::List(v, _) if !v.is_empty()) || matches!(&args[0], Value::Pair(_))))
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
        "list-ref" | "list-tail" | "list?" | "assoc" =>
            apply_list_builtin(name, args),
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
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
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
        | "string-upcase" | "string-downcase"
        | "string>?" | "string<=?" | "string>=?" => apply_char_string_cmp_builtin(name, args),
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string->list" | "list->string"
        | "make-string" | "string" => apply_string_builtin(name, args),
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
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                (Value::Void, Value::Void) => true,
                _ => false,
            };
            Ok(Value::Bool(result))
        }
        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
        | "vector?" | "vector->list" | "list->vector" => apply_vector_builtin(name, args),
        "gcd" | "lcm" => apply_gcd_lcm(name, args),
        "error" => {
            if args.is_empty() {
                return Err(EvalError::Raised { value: "error".into() });
            }
            let mut msg = format!("{}", args[0]);
            for arg in &args[1..] {
                msg.push_str(&format!(" {arg}"));
            }
            Err(EvalError::Raised { value: msg })
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let result = matches!(&args[0], Value::Builtin(_) | Value::Closure { .. } | Value::CaseLambda { .. } | Value::Continuation(_));
            Ok(Value::Bool(result))
        }
        "syntax->datum" => {
            // In our implementation, syntax objects ARE datums, so identity
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(args[0].clone())
        }
        "datum->syntax" => {
            // (datum->syntax template-id datum) — returns datum as syntax
            // In our implementation, this is effectively identity
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            Ok(args[1].clone())
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn apply_gcd_lcm(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let a = match &args[0] {
        Value::Int(n) => *n,
        other => return Err(EvalError::TypeMismatch {
            expected: "integer".into(), got: format!("{other}"),
        }),
    };
    let b = match &args[1] {
        Value::Int(n) => *n,
        other => return Err(EvalError::TypeMismatch {
            expected: "integer".into(), got: format!("{other}"),
        }),
    };
    fn gcd_impl(mut a: i64, mut b: i64) -> i64 {
        a = a.abs();
        b = b.abs();
        while b != 0 {
            let t = b;
            b = a % b;
            a = t;
        }
        a
    }
    match name {
        "gcd" => Ok(Value::Int(gcd_impl(a, b))),
        "lcm" => {
            let g = gcd_impl(a, b);
            let result = if g == 0 { 0 } else { (a / g * b).abs() };
            Ok(Value::Int(result))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

/// Convert a value to a Vec if it's a proper list (List or pair chain).
fn value_to_vec(val: &Value) -> Option<Vec<Value>> {
    val.to_vec()
}

fn apply_string_set_char(
    val: Value, var_name: String, index: i64, env: &Rc<RefCell<Env>>, span: Option<Span>,
) -> Result<Control, EvalError> {
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

fn apply_list_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            Ok(Value::cons(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::Pair(cell) => Ok(cell.borrow().0.clone()),
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
                Value::Pair(cell) => Ok(cell.borrow().1.clone()),
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
        "list" => Ok(Value::list_from_vec(args.to_vec())),
        "append" => {
            if args.is_empty() {
                return Ok(Value::List(vec![], None));
            }
            if args.len() == 1 {
                return Ok(args[0].clone());
            }
            // Collect all but the last into a flat vec, then append the last
            let mut result = Vec::new();
            for arg in &args[..args.len() - 1] {
                let Some(elems) = value_to_vec(arg) else {
                    return Err(EvalError::TypeMismatch {
                        expected: "list".into(), got: format!("{arg}"),
                    });
                };
                result.extend(elems);
            }
            let last = &args[args.len() - 1];
            // If last is a proper list, extend; otherwise build improper list
            if let Some(tail_elems) = value_to_vec(last) {
                result.extend(tail_elems);
                Ok(Value::list_from_vec(result))
            } else {
                // Build pair chain ending in the non-list last value
                let mut acc = last.clone();
                for item in result.into_iter().rev() {
                    acc = Value::cons(item, acc);
                }
                Ok(acc)
            }
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Some(elems) = value_to_vec(&args[0]) else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[0]),
                });
            };
            let mut rev = elems;
            rev.reverse();
            Ok(Value::list_from_vec(rev))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Some(elems) = value_to_vec(&args[0]) else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Int(elems.len() as i64))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Some(elems) = value_to_vec(&args[0]) else {
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
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[1]),
                });
            };
            let idx = *idx as usize;
            // Walk the structure directly to preserve pair identity
            let mut cur = args[0].clone();
            for _ in 0..idx {
                cur = match cur {
                    Value::List(elems, _) if !elems.is_empty() => {
                        Value::List(elems[1..].to_vec(), None)
                    }
                    Value::Pair(ref cell) => cell.borrow().1.clone(),
                    _ => return Err(EvalError::TypeMismatch {
                        expected: "list".into(), got: format!("{cur}"),
                    }),
                };
            }
            Ok(cur)
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let is_list = match &args[0] {
                Value::List(_, _) => true,
                Value::Pair(_) => value_to_vec(&args[0]).is_some(),
                _ => false,
            };
            Ok(Value::Bool(is_list))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Some(alist) = value_to_vec(&args[1]) else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[1]),
                });
            };
            for entry in &alist {
                if let Some(key) = entry.car() {
                    if key == args[0] {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Bool(false))
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::Pair(cell) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "pair".into(), got: format!("{}", args[0]),
                });
            };
            cell.borrow_mut().0 = args[1].clone();
            Ok(Value::Void)
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::Pair(cell) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "pair".into(), got: format!("{}", args[0]),
                });
            };
            cell.borrow_mut().1 = args[1].clone();
            Ok(Value::Void)
        }
        "assq" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Some(alist) = value_to_vec(&args[1]) else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[1]),
                });
            };
            for entry in &alist {
                if let Some(key) = entry.car() {
                    let is_eq = match (&args[0], &key) {
                        (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
                        (Value::Bool(a), Value::Bool(b)) => a == b,
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Char(a), Value::Char(b)) => a == b,
                        (Value::List(a, _), Value::List(b, _)) => a.is_empty() && b.is_empty(),
                        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                        (Value::Void, Value::Void) => true,
                        _ => false,
                    };
                    if is_eq {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Bool(false))
        }
        "assv" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Some(alist) = value_to_vec(&args[1]) else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[1]),
                });
            };
            for entry in &alist {
                if let Some(key) = entry.car() {
                    // eqv? semantics
                    let is_eqv = match (&args[0], &key) {
                        (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
                        (Value::Bool(a), Value::Bool(b)) => a == b,
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Float(a), Value::Float(b)) => a == b,
                        (Value::Char(a), Value::Char(b)) => a == b,
                        (Value::List(a, _), Value::List(b, _)) => a.is_empty() && b.is_empty(),
                        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                        (Value::Void, Value::Void) => true,
                        _ => false,
                    };
                    if is_eqv {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Bool(false))
        }
        "memq" | "memv" | "member" | "cadr" | "cddr" | "caar" | "cdar"
        | "caddr" | "cdddr" | "caaar" | "caddar" => {
            apply_list_search_builtin(name, args)
        }
        other => Err(EvalError::UnboundVariable { name: other.into() }),
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
            Ok(Value::list_from_vec(elems.borrow().clone()))
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Some(elems) = value_to_vec(&args[0]) else {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(), got: format!("{}", args[0]),
                });
            };
            Ok(Value::Vector(Rc::new(RefCell::new(elems))))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

// ---------------------------------------------------------------------------
// Numeric tower helpers
// ---------------------------------------------------------------------------

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn make_rational_value(n: i64, d: i64) -> Value {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d.abs();
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Value::Int(n) } else { Value::Rational(n, d) }
}

/// Represents an intermediate numeric value for arithmetic.
#[derive(Debug, Clone, Copy)]
enum NumVal {
    Exact(i64, i64),  // numerator, denominator (not necessarily reduced)
    Inexact(f64),
}

fn value_to_numval(v: &Value) -> Result<NumVal, EvalError> {
    match v {
        Value::Int(n) => Ok(NumVal::Exact(*n, 1)),
        Value::Rational(n, d) => Ok(NumVal::Exact(*n, *d)),
        Value::Float(f) => Ok(NumVal::Inexact(*f)),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(), got: format!("{other}"),
        }),
    }
}

fn numval_to_value(nv: NumVal) -> Value {
    match nv {
        NumVal::Exact(n, d) => make_rational_value(n, d),
        NumVal::Inexact(f) => Value::Float(f),
    }
}

fn numval_to_f64(nv: NumVal) -> f64 {
    match nv {
        NumVal::Exact(n, d) => n as f64 / d as f64,
        NumVal::Inexact(f) => f,
    }
}

fn numval_add(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Exact(an, ad), NumVal::Exact(bn, bd)) => NumVal::Exact(an * bd + bn * ad, ad * bd),
        (a, b) => NumVal::Inexact(numval_to_f64(a) + numval_to_f64(b)),
    }
}

fn numval_sub(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Exact(an, ad), NumVal::Exact(bn, bd)) => NumVal::Exact(an * bd - bn * ad, ad * bd),
        (a, b) => NumVal::Inexact(numval_to_f64(a) - numval_to_f64(b)),
    }
}

fn numval_mul(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Exact(an, ad), NumVal::Exact(bn, bd)) => NumVal::Exact(an * bn, ad * bd),
        (a, b) => NumVal::Inexact(numval_to_f64(a) * numval_to_f64(b)),
    }
}

fn numval_div(a: NumVal, b: NumVal) -> Result<NumVal, EvalError> {
    match (a, b) {
        (NumVal::Exact(an, ad), NumVal::Exact(bn, bd)) => {
            if bn == 0 { return Err(EvalError::DivisionByZero); }
            Ok(NumVal::Exact(an * bd, ad * bn))
        }
        (a, b) => {
            let bv = numval_to_f64(b);
            if bv == 0.0 { return Err(EvalError::DivisionByZero); }
            Ok(NumVal::Inexact(numval_to_f64(a) / bv))
        }
    }
}

fn float_to_exact(f: f64) -> Value {
    if f == (f as i64) as f64 {
        return Value::Int(f as i64);
    }
    let sign: i64 = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    let mut n = f;
    let mut d: i64 = 1;
    for _ in 0..53 {
        if n == n.floor() {
            break;
        }
        n *= 2.0;
        d *= 2;
    }
    let ni = n as i64 * sign;
    let g = gcd(ni.abs(), d);
    make_rational_value(ni / g, d / g)
}

fn require_numvals(args: &[Value]) -> Result<Vec<NumVal>, EvalError> {
    args.iter().map(value_to_numval).collect()
}

fn apply_numeric_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let nums = require_numvals(args)?;
            let result = nums.iter().copied().fold(NumVal::Exact(0, 1), numval_add);
            Ok(numval_to_value(result))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_numvals(args)?;
            if nums.len() == 1 {
                let negated = numval_sub(NumVal::Exact(0, 1), nums[0]);
                Ok(numval_to_value(negated))
            } else {
                let result = nums[1..].iter().copied().fold(nums[0], numval_sub);
                Ok(numval_to_value(result))
            }
        }
        "*" => {
            let nums = require_numvals(args)?;
            let result = nums.iter().copied().fold(NumVal::Exact(1, 1), numval_mul);
            Ok(numval_to_value(result))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_numvals(args)?;
            if nums.len() == 1 {
                return Ok(numval_to_value(numval_div(NumVal::Exact(1, 1), nums[0])?));
            }
            let mut acc = nums[0];
            for &n in &nums[1..] {
                acc = numval_div(acc, n)?;
            }
            Ok(numval_to_value(acc))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(n.abs())),
                Value::Rational(n, d) => Ok(make_rational_value(n.abs(), *d)),
                Value::Float(f) => Ok(Value::Float(f.abs())),
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
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
            Ok(Value::Int(nums[0] / nums[1]))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_numvals(args)?;
            let floats: Vec<f64> = nums.iter().copied().map(numval_to_f64).collect();
            let min_idx = floats.iter().enumerate()
                .min_by(|(_, a), (_, b)| a.partial_cmp(b).expect("non-NaN"))
                .expect("non-empty").0;
            Ok(numval_to_value(nums[min_idx]))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_numvals(args)?;
            let floats: Vec<f64> = nums.iter().copied().map(numval_to_f64).collect();
            let max_idx = floats.iter().enumerate()
                .max_by(|(_, a), (_, b)| a.partial_cmp(b).expect("non-NaN"))
                .expect("non-empty").0;
            Ok(numval_to_value(nums[max_idx]))
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
            let nv = value_to_numval(&args[0])?;
            Ok(Value::Bool(numval_to_f64(nv) == 0.0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let nv = value_to_numval(&args[0])?;
            Ok(Value::Bool(numval_to_f64(nv) > 0.0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let nv = value_to_numval(&args[0])?;
            Ok(Value::Bool(numval_to_f64(nv) < 0.0))
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
        "truncate" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Float(f) => Ok(Value::Float(f.trunc())),
                Value::Rational(n, d) => Ok(Value::Int(n / d)),
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
        }
        "floor" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Float(f) => Ok(Value::Float(f.floor())),
                Value::Rational(n, d) => {
                    let q = n / d;
                    if (n % d != 0) && ((*n < 0) != (*d < 0)) {
                        Ok(Value::Int(q - 1))
                    } else {
                        Ok(Value::Int(q))
                    }
                }
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
        }
        "ceiling" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Float(f) => Ok(Value::Float(f.ceil())),
                Value::Rational(n, d) => {
                    let q = n / d;
                    if n % d != 0 && ((*n > 0) == (*d > 0)) {
                        Ok(Value::Int(q + 1))
                    } else {
                        Ok(Value::Int(q))
                    }
                }
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
        }
        "round" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Float(f) => Ok(Value::Float(f.round())),
                Value::Rational(n, d) => Ok(Value::Int((*n as f64 / *d as f64).round() as i64)),
                other => Err(EvalError::TypeMismatch {
                    expected: "number".into(), got: format!("{other}"),
                }),
            }
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
        "string>?" => {
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
            Ok(Value::Bool(a > b))
        }
        "string<=?" => {
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
            Ok(Value::Bool(a <= b))
        }
        "string>=?" => {
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
            Ok(Value::Bool(a >= b))
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
            Ok(Value::list_from_vec(chars))
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let elems = match &args[0] {
                Value::List(elems, _) => elems.clone(),
                Value::Pair(_) => {
                    args[0].to_vec().unwrap_or_default()
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
        "string" => {
            let mut result = String::new();
            for arg in args {
                let Value::Char(c) = arg else {
                    return Err(EvalError::TypeMismatch {
                        expected: "char".into(), got: format!("{arg}"),
                    });
                };
                result.push(*c);
            }
            Ok(Value::String(result))
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            let Value::Int(len) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(), got: format!("{}", args[0]),
                });
            };
            let ch = if args.len() == 2 {
                let Value::Char(c) = &args[1] else {
                    return Err(EvalError::TypeMismatch {
                        expected: "char".into(), got: format!("{}", args[1]),
                    });
                };
                *c
            } else {
                '\0'
            };
            Ok(Value::String(std::iter::repeat_n(ch, *len as usize).collect()))
        }
        other => Err(EvalError::UnboundVariable { name: other.into() }),
    }
}

fn compare_nums(args: &[Value], cmp: fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let nums: Vec<f64> = args.iter()
        .map(|a| value_to_numval(a).map(numval_to_f64))
        .collect::<Result<_, _>>()?;
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Bool(result))
}

fn is_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Symbol(x, _), Value::Symbol(y, _)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(a, _), Value::List(b, _)) => a.is_empty() && b.is_empty(),
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn is_eq_identity(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Symbol(x, _), Value::Symbol(y, _)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        _ => false,
    }
}

fn apply_list_search_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "memq" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::List(ref elems, _) if elems.is_empty() => return Ok(Value::Bool(false)),
                    Value::List(ref elems, _) => {
                        if is_eq_identity(&args[0], &elems[0]) {
                            return Ok(cur);
                        }
                        cur = Value::List(elems[1..].to_vec(), None);
                    }
                    Value::Pair(ref cell) => {
                        let (car, cdr) = {
                            let b = cell.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        if is_eq_identity(&args[0], &car) {
                            return Ok(cur);
                        }
                        cur = cdr;
                    }
                    _ => return Ok(Value::Bool(false)),
                }
            }
        }
        "memv" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::List(ref elems, _) if elems.is_empty() => return Ok(Value::Bool(false)),
                    Value::List(ref elems, _) => {
                        if is_eqv(&args[0], &elems[0]) {
                            return Ok(cur);
                        }
                        cur = Value::List(elems[1..].to_vec(), None);
                    }
                    Value::Pair(ref cell) => {
                        let (car, cdr) = {
                            let b = cell.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        if is_eqv(&args[0], &car) {
                            return Ok(cur);
                        }
                        cur = cdr;
                    }
                    _ => return Ok(Value::Bool(false)),
                }
            }
        }
        "member" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::List(ref elems, _) if elems.is_empty() => return Ok(Value::Bool(false)),
                    Value::List(ref elems, _) => {
                        if elems[0] == args[0] {
                            return Ok(cur);
                        }
                        cur = Value::List(elems[1..].to_vec(), None);
                    }
                    Value::Pair(ref cell) => {
                        let (car, cdr) = {
                            let b = cell.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        if car == args[0] {
                            return Ok(cur);
                        }
                        cur = cdr;
                    }
                    _ => return Ok(Value::Bool(false)),
                }
            }
        }
        "cadr" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let d = apply_list_builtin("cdr", args)?;
            apply_list_builtin("car", &[d])
        }
        "cddr" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let d = apply_list_builtin("cdr", args)?;
            apply_list_builtin("cdr", &[d])
        }
        "caar" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let a = apply_list_builtin("car", args)?;
            apply_list_builtin("car", &[a])
        }
        "cdar" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let a = apply_list_builtin("car", args)?;
            apply_list_builtin("cdr", &[a])
        }
        "caddr" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let d = apply_list_builtin("cdr", args)?;
            let dd = apply_list_builtin("cdr", &[d])?;
            apply_list_builtin("car", &[dd])
        }
        "cdddr" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let d = apply_list_builtin("cdr", args)?;
            let dd = apply_list_builtin("cdr", &[d])?;
            apply_list_builtin("cdr", &[dd])
        }
        "caaar" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let a = apply_list_builtin("car", args)?;
            let aa = apply_list_builtin("car", &[a])?;
            apply_list_builtin("car", &[aa])
        }
        "caddar" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: 1, got: args.len() }); }
            let a = apply_list_builtin("car", args)?;
            let da = apply_list_builtin("cdr", &[a])?;
            let dda = apply_list_builtin("cdr", &[da])?;
            apply_list_builtin("car", &[dda])
        }
        other => Err(EvalError::UnboundVariable { name: other.into() }),
    }
}
