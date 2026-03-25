pub mod error;
mod builtins;
mod cek;
mod macros;
mod parser;
mod quasiquote;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

// ---- AST (parsed code with source positions) ----

#[derive(Debug, Clone)]
pub(crate) struct Ast {
    pub(crate) kind: AstKind,
    pub(crate) line: usize,
    pub(crate) col: usize,
}

#[derive(Debug, Clone)]
pub(crate) enum AstKind {
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Ast>),
}

// ---- Runtime values ----

type Kont = Vec<Frame>;
pub(crate) type Winders = Vec<Rc<(Value, Value)>>;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    List(Vec<Value>),
    Symbol(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Ast>,
        env: Env,
    },
    Pair(Rc<RefCell<(Value, Value)>>),
    Builtin(fn(&[Value], &mut String) -> Result<Value, EvalError>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Ast, Ast)>,
        def_env: Env,
    },
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    RecordConstructor {
        type_id: u64,
        type_name: String,
        field_names: Vec<String>,
    },
    RecordPredicate {
        type_id: u64,
    },
    RecordAccessor {
        type_id: u64,
        type_name: String,
        field_name: String,
        field_index: usize,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Ast>)>,
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Void,
    CallCC,
    DynamicWind,
    Continuation(Kont, Winders),
    Raise,
    WithExceptionHandler,
    Values(Vec<Value>),
    CallWithValues,
    Syntax {
        ast: Ast,
        renames: Vec<(String, String)>,
        source_env: Option<Env>,
    },
    SyntaxEllipsis(Vec<Ast>),
    SyntaxCaseMacro {
        transformer: Box<Value>,
    },
}

// ---- CEK Machine continuation frames ----

#[derive(Debug, Clone)]
pub(crate) enum Frame {
    If { then_br: Ast, else_br: Option<Ast>, env: Env },
    Seq { remaining: Vec<Ast>, env: Env },
    Define { name: String, env: Env },
    Set { name: String, env: Env },
    And { remaining: Vec<Ast>, env: Env },
    Or { remaining: Vec<Ast>, env: Env },
    EvalFunc { args: Vec<Ast>, env: Env },
    Args { func: Value, done: Vec<Value>, remaining: Vec<Ast>, env: Env },
    CallCC,
    LetBind { name: String, remaining: Vec<(String, Ast)>, values: Vec<(String, Value)>, body: Vec<Ast>, eval_env: Env },
    LetStarBind { name: String, remaining: Vec<(String, Ast)>, body: Vec<Ast>, local_env: Env },
    NamedLetBind { loop_name: String, param: String, remaining: Vec<(String, Ast)>, values: Vec<(String, Value)>, body: Vec<Ast>, eval_env: Env },
    LetrecBind { name: String, remaining: Vec<(String, Ast)>, body: Vec<Ast>, local_env: Env },
    LetrecStarBind { name: String, remaining: Vec<(String, Ast)>, body: Vec<Ast>, local_env: Env },
    CondClause { body: Vec<Ast>, remaining: Vec<Ast>, env: Env },
    CondArrow { test_val: Value },
    CaseKey { clauses: Vec<Ast>, env: Env },
    StringSetIdx { var_name: String, char_expr: Ast, env: Env },
    StringSetChar { var_name: String, idx: usize, env: Env },
    DynWindAfterIn { entry: Rc<(Value, Value)>, body_thunk: Value },
    DynWindAfterBody { out_thunk: Value },
    DynWindAfterOut { result: Value },
    ContinuationWind { out_thunks: Vec<Value>, in_entries: Winders, saved_kont: Kont, saved_winders: Winders, val: Value },
    ExceptionHandler { handler: Value },
    GuardHandler { var: String, clauses: Vec<Ast>, env: Env },
    RaiseUnwind { raised_val: Value },
    CallWithValues { consumer: Value },
    SyntaxCaseMatch { literals: Vec<String>, clauses: Vec<Ast>, env: Env },
    MacroResult { use_env: Env },
    WithSyntaxBind { name: String, remaining: Vec<(String, Ast)>, body: Vec<Ast>, bind_env: Env, eval_env: Env },
}

pub(crate) enum CekState {
    Eval(Ast, Env),
    Apply(Value),
}

// ---- Helpers ----

pub(crate) fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Create a rational or integer value, always simplified.
pub(crate) fn make_rational(n: i64, d: i64) -> Value {
    assert!(d != 0, "division by zero in make_rational");
    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

pub(crate) fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

pub(crate) fn vec_to_list(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

pub(crate) fn value_to_vec(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(items) => Some(items.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = val.clone();
            let mut limit = 10_000_000usize;
            loop {
                match current {
                    Value::List(ref items) if items.is_empty() => return Some(result),
                    Value::List(ref items) => {
                        result.extend(items.iter().cloned());
                        return Some(result);
                    }
                    Value::Pair(ref p) => {
                        limit = limit.checked_sub(1)?;
                        let (car, cdr) = {
                            let b = p.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        result.push(car);
                        current = cdr;
                    }
                    _ => return None,
                }
            }
        }
        _ => None,
    }
}

pub(crate) type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub(crate) struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Environment {
    pub(crate) fn new() -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub(crate) fn with_parent(parent: &Env) -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    /// Look up a name and return it only if it's a macro (Macro or SyntaxCaseMacro).
    /// Avoids cloning non-macro values (e.g. Lambda) which is expensive in hot loops.
    pub(crate) fn get_if_macro(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            match val {
                Value::Macro { .. } | Value::SyntaxCaseMacro { .. } => Some(val.clone()),
                _ => None,
            }
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get_if_macro(name)
        } else {
            None
        }
    }

    pub(crate) fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }

    fn set_existing(&mut self, name: &str, val: Value) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().set_existing(name, val)
        } else {
            false
        }
    }
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    pub(crate) fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    /// Convert any numeric value to f64 for comparison.
    pub(crate) fn as_f64(&self) -> Result<f64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    pub(crate) fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }

    pub(crate) fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))
    }

    pub(crate) fn is_exact(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _))
    }

    pub(crate) fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Float(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{s}\""),
            Value::Char(c) => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{c}"),
            },
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => display_pair(self, false),
            Value::Lambda { .. } | Value::Builtin(_)
            | Value::RecordConstructor { .. } | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. }
            | Value::CaseLambda { .. }
            | Value::CallCC | Value::DynamicWind | Value::Continuation(_, _)
            | Value::Raise | Value::WithExceptionHandler
            | Value::CallWithValues => "#<procedure>".into(),
            Value::Values(_) => "#<values>".into(),
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Macro { .. } | Value::SyntaxCaseMacro { .. } => "#<macro>".into(),
            Value::Syntax { .. } | Value::SyntaxEllipsis(_) => "#<syntax>".into(),
            Value::Record { type_name, fields, .. } => {
                let inner: Vec<String> = fields.iter().map(|(k, v)| format!("{}: {}", k, v.display_value())).collect();
                format!("#<{} {}>", type_name, inner.join(", "))
            }
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — no quotes on strings, no #\ on chars.
    pub(crate) fn display_repr(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_repr()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => display_pair(self, true),
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display_repr()).collect();
                format!("#({})", inner.join(" "))
            }
            _ => self.display_value(),
        }
    }
}

fn display_pair(val: &Value, use_repr: bool) -> String {
    let mut parts = Vec::new();
    let mut current = val.clone();
    let mut seen = std::collections::HashSet::new();
    let mut improper_tail = None;
    loop {
        match current {
            Value::List(ref items) if items.is_empty() => break,
            Value::List(ref items) => {
                for item in items {
                    parts.push(if use_repr { item.display_repr() } else { item.display_value() });
                }
                break;
            }
            Value::Pair(ref p) => {
                let addr = Rc::as_ptr(p) as usize;
                if !seen.insert(addr) {
                    break; // cycle detected
                }
                let (car, cdr) = {
                    let b = p.borrow();
                    (b.0.clone(), b.1.clone())
                };
                parts.push(if use_repr { car.display_repr() } else { car.display_value() });
                current = cdr;
            }
            other => {
                improper_tail = Some(if use_repr { other.display_repr() } else { other.display_value() });
                break;
            }
        }
    }
    if let Some(tail) = improper_tail {
        format!("({} . {})", parts.join(" "), tail)
    } else {
        format!("({})", parts.join(" "))
    }
}

/// Convert an AST node to a runtime Value (for quote).
pub(crate) fn ast_to_value(ast: &Ast) -> Value {
    match &ast.kind {
        AstKind::Integer(n) => Value::Integer(*n),
        AstKind::Rational(n, d) => Value::Rational(*n, *d),
        AstKind::Float(f) => Value::Float(*f),
        AstKind::Boolean(b) => Value::Boolean(*b),
        AstKind::Str(s) => Value::Str(s.clone()),
        AstKind::Char(c) => Value::Char(*c),
        AstKind::Symbol(s) => Value::Symbol(s.clone()),
        AstKind::List(items) => {
            // Check for dotted pair notation: (a b . c) has "." as second-to-last element
            if let Some(dot_pos) = items.iter().position(|it| matches!(&it.kind, AstKind::Symbol(s) if s == ".")) {
                // Dot must be second-to-last, with exactly one element after it
                if dot_pos + 2 == items.len() && dot_pos > 0 {
                    let tail = ast_to_value(&items[dot_pos + 1]);
                    // Build pairs from right to left: (a b . c) = (cons a (cons b c))
                    let mut result = tail;
                    for item in items[..dot_pos].iter().rev() {
                        result = make_pair(ast_to_value(item), result);
                    }
                    return result;
                }
            }
            Value::List(items.iter().map(ast_to_value).collect())
        }
    }
}

pub(crate) fn value_to_ast(val: &Value) -> Result<Ast, EvalError> {
    match val {
        Value::Integer(n) => Ok(Ast { kind: AstKind::Integer(*n), line: 0, col: 0 }),
        Value::Rational(n, d) => Ok(Ast { kind: AstKind::Rational(*n, *d), line: 0, col: 0 }),
        Value::Float(f) => Ok(Ast { kind: AstKind::Float(*f), line: 0, col: 0 }),
        Value::Boolean(b) => Ok(Ast { kind: AstKind::Boolean(*b), line: 0, col: 0 }),
        Value::Str(s) => Ok(Ast { kind: AstKind::Str(s.clone()), line: 0, col: 0 }),
        Value::Char(c) => Ok(Ast { kind: AstKind::Char(*c), line: 0, col: 0 }),
        Value::Symbol(s) => Ok(Ast { kind: AstKind::Symbol(s.clone()), line: 0, col: 0 }),
        Value::List(items) => {
            let ast_items: Result<Vec<Ast>, _> = items.iter().map(value_to_ast).collect();
            Ok(Ast { kind: AstKind::List(ast_items?), line: 0, col: 0 })
        }
        _ => Err(EvalError::Type(format!("cannot convert to syntax: {}", val.display_value()))),
    }
}

use parser::Parser;

// ---- AST construction helpers ----

static DO_COUNTER: AtomicU64 = AtomicU64::new(0);

fn ast_sym(name: &str) -> Ast {
    Ast { kind: AstKind::Symbol(name.to_string()), line: 0, col: 0 }
}

fn ast_list(items: Vec<Ast>) -> Ast {
    Ast { kind: AstKind::List(items), line: 0, col: 0 }
}

// ---- Evaluator (CEK Machine) ----

/// Evaluate an AST node, wrapping any error with source position.
pub(crate) fn eval(ast: &Ast, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut kont: Kont = Vec::new();
    let mut winders: Winders = Vec::new();
    let state = CekState::Eval(ast.clone(), Rc::clone(env));
    cek_run(state, &mut kont, &mut winders, output).map_err(|e| match e {
        EvalError::WithPosition(_, _, _) => e,
        _ => EvalError::WithPosition(Box::new(e), ast.line, ast.col),
    })
}

fn wrap_err(e: EvalError, pos: (usize, usize)) -> EvalError {
    match e {
        EvalError::WithPosition(_, _, _) => e,
        _ if pos.0 == 0 && pos.1 == 0 => e,
        _ => EvalError::WithPosition(Box::new(e), pos.0, pos.1),
    }
}

fn cek_run(mut state: CekState, kont: &mut Kont, winders: &mut Winders, output: &mut String) -> Result<Value, EvalError> {
    let mut last_pos = (0usize, 0usize);
    loop {
        if let CekState::Eval(ref ast, _) = state {
            last_pos = (ast.line, ast.col);
        }
        state = match state {
            CekState::Eval(ast, env) => {
                cek_eval_step(&ast, &env, kont, output).map_err(|e| wrap_err(e, last_pos))?
            }
            CekState::Apply(val) => {
                match kont.pop() {
                    None => return Ok(val),
                    Some(frame) => {
                        cek_apply_frame(frame, val, kont, winders, output).map_err(|e| wrap_err(e, last_pos))?
                    }
                }
            }
        };
    }
}

fn cek_eval_step(ast: &Ast, env: &Env, kont: &mut Kont, output: &mut String) -> Result<CekState, EvalError> {
    match &ast.kind {
        AstKind::Integer(n) => Ok(CekState::Apply(Value::Integer(*n))),
        AstKind::Rational(n, d) => Ok(CekState::Apply(Value::Rational(*n, *d))),
        AstKind::Float(f) => Ok(CekState::Apply(Value::Float(*f))),
        AstKind::Boolean(b) => Ok(CekState::Apply(Value::Boolean(*b))),
        AstKind::Str(s) => Ok(CekState::Apply(Value::Str(s.clone()))),
        AstKind::Char(c) => Ok(CekState::Apply(Value::Char(*c))),
        AstKind::Symbol(name) => {
            let val = env.borrow().get(name)
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()))?;
            Ok(CekState::Apply(val))
        }
        AstKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            cek_eval_list(items, env, kont, output)
        }
    }
}

fn cek_eval_list(items: &[Ast], env: &Env, kont: &mut Kont, _output: &mut String) -> Result<CekState, EvalError> {
    if let AstKind::Symbol(ref op) = items[0].kind {
        match op.as_str() {
            "define" => cek_eval_define(&items[1..], env, kont),
            "if" => {
                let args = &items[1..];
                if args.len() < 2 || args.len() > 3 {
                    return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
                }
                kont.push(Frame::If {
                    then_br: args[1].clone(),
                    else_br: args.get(2).cloned(),
                    env: Rc::clone(env),
                });
                Ok(CekState::Eval(args[0].clone(), Rc::clone(env)))
            }
            "quote" => {
                if items.len() != 2 {
                    return Err(EvalError::Arity("quote requires exactly 1 argument".into()));
                }
                Ok(CekState::Apply(ast_to_value(&items[1])))
            }
            "quasiquote" => {
                if items.len() != 2 {
                    return Err(EvalError::Arity("quasiquote requires exactly 1 argument".into()));
                }
                cek_eval_quasiquote(&items[1], env, kont)
            }
            "lambda" => {
                let val = eval_lambda(&items[1..], env)?;
                Ok(CekState::Apply(val))
            }
            "let" => cek_eval_let(&items[1..], env, kont),
            "begin" => {
                if items.len() == 1 {
                    return Ok(CekState::Apply(Value::Void));
                }
                if items.len() > 2 {
                    kont.push(Frame::Seq {
                        remaining: items[2..].to_vec(),
                        env: Rc::clone(env),
                    });
                }
                Ok(CekState::Eval(items[1].clone(), Rc::clone(env)))
            }
            "cond" => cek_eval_cond(&items[1..], env, kont),
            "and" => {
                if items.len() == 1 {
                    return Ok(CekState::Apply(Value::Boolean(true)));
                }
                if items.len() == 2 {
                    return Ok(CekState::Eval(items[1].clone(), Rc::clone(env)));
                }
                kont.push(Frame::And {
                    remaining: items[2..].to_vec(),
                    env: Rc::clone(env),
                });
                Ok(CekState::Eval(items[1].clone(), Rc::clone(env)))
            }
            "or" => {
                if items.len() == 1 {
                    return Ok(CekState::Apply(Value::Boolean(false)));
                }
                if items.len() == 2 {
                    return Ok(CekState::Eval(items[1].clone(), Rc::clone(env)));
                }
                kont.push(Frame::Or {
                    remaining: items[2..].to_vec(),
                    env: Rc::clone(env),
                });
                Ok(CekState::Eval(items[1].clone(), Rc::clone(env)))
            }
            "set!" => {
                if items.len() != 3 {
                    return Err(EvalError::Arity("set! requires exactly 2 arguments".into()));
                }
                let var_name = match &items[1].kind {
                    AstKind::Symbol(name) => name.clone(),
                    _ => return Err(EvalError::Type("set!: first argument must be a symbol".into())),
                };
                kont.push(Frame::Set { name: var_name, env: Rc::clone(env) });
                Ok(CekState::Eval(items[2].clone(), Rc::clone(env)))
            }
            "string-set!" => cek_eval_string_set(&items[1..], env, kont),
            "define-syntax" => {
                if items.len() != 3 {
                    return Err(EvalError::Arity("define-syntax requires 2 arguments".into()));
                }
                let ds_name = match &items[1].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
                };
                // Check if body is a lambda (syntax-case transformer)
                if let AstKind::List(body_items) = &items[2].kind {
                    if !body_items.is_empty()
                        && matches!(&body_items[0].kind, AstKind::Symbol(s) if s == "lambda") {
                            let proc = eval_lambda(&body_items[1..], env)?;
                            env.borrow_mut().set(ds_name, Value::SyntaxCaseMacro {
                                transformer: Box::new(proc),
                            });
                            return Ok(CekState::Apply(Value::Void));
                        }
                }
                let result = eval_define_syntax(&items[1..], env)?;
                Ok(CekState::Apply(result))
            }
            "syntax-case" => {
                // (syntax-case expr (literals...) clause ...)
                if items.len() < 4 {
                    return Err(EvalError::Arity("syntax-case requires expr, literals, and clauses".into()));
                }
                let literals = match &items[2].kind {
                    AstKind::List(lits) => lits.iter().map(|l| match &l.kind {
                        AstKind::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Type("syntax-case: expected symbol in literals".into())),
                    }).collect::<Result<Vec<_>, _>>()?,
                    _ => return Err(EvalError::Type("syntax-case: expected literals list".into())),
                };
                let clauses = items[3..].to_vec();
                kont.push(Frame::SyntaxCaseMatch { literals, clauses, env: Rc::clone(env) });
                Ok(CekState::Eval(items[1].clone(), Rc::clone(env)))
            }
            "syntax" => {
                if items.len() != 2 {
                    return Err(EvalError::Arity("syntax requires exactly 1 argument".into()));
                }
                let template = &items[1];
                // Simple case: single symbol bound to a Syntax value
                if let AstKind::Symbol(name) = &template.kind {
                    let val = env.borrow().get(name);
                    if let Some(v @ Value::Syntax { .. }) = val {
                        return Ok(CekState::Apply(v));
                    }
                    if let Some(v @ Value::SyntaxEllipsis(_)) = val {
                        return Ok(CekState::Apply(v));
                    }
                }
                // Full template expansion
                let (expanded, renames) = macros::expand_syntax_form(template, env);
                Ok(CekState::Apply(Value::Syntax {
                    ast: expanded,
                    renames,
                    source_env: Some(Rc::clone(env)),
                }))
            }
            "with-syntax" => {
                // (with-syntax ((pat expr) ...) body ...)
                if items.len() < 3 {
                    return Err(EvalError::Arity("with-syntax requires bindings and body".into()));
                }
                let bindings_list = match &items[1].kind {
                    AstKind::List(b) => b,
                    _ => return Err(EvalError::Type("with-syntax: expected bindings list".into())),
                };
                let body = items[2..].to_vec();
                let mut bindings = Vec::new();
                for b in bindings_list {
                    match &b.kind {
                        AstKind::List(pair) if pair.len() == 2 => {
                            let bname = match &pair[0].kind {
                                AstKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Type("with-syntax: expected symbol in binding".into())),
                            };
                            bindings.push((bname, pair[1].clone()));
                        }
                        _ => return Err(EvalError::Type("with-syntax: invalid binding".into())),
                    }
                }
                let child_env = Environment::with_parent(env);
                if bindings.is_empty() {
                    return cek_eval_body(body, child_env, kont);
                }
                let first = bindings.remove(0);
                kont.push(Frame::WithSyntaxBind {
                    name: first.0,
                    remaining: bindings,
                    body,
                    bind_env: child_env,
                    eval_env: Rc::clone(env),
                });
                Ok(CekState::Eval(first.1, Rc::clone(env)))
            }
            "define-record-type" => {
                let result = eval_define_record_type(&items[1..], env)?;
                Ok(CekState::Apply(result))
            }
            "case-lambda" => {
                let val = eval_case_lambda(&items[1..], env)?;
                Ok(CekState::Apply(val))
            }
            "letrec" => cek_eval_letrec(&items[1..], env, kont),
            "letrec*" => cek_eval_letrec_star(&items[1..], env, kont),
            "case" => cek_eval_case(&items[1..], env, kont),
            "do" => cek_eval_do(&items[1..], env),
            "let*" => cek_eval_let_star(&items[1..], env, kont),
            "when" => {
                if items.len() < 3 {
                    return Err(EvalError::Arity("when requires a test and body".into()));
                }
                let mut begin_items = vec![ast_sym("begin")];
                begin_items.extend(items[2..].iter().cloned());
                let body = ast_list(begin_items);
                let desugared = ast_list(vec![ast_sym("if"), items[1].clone(), body]);
                Ok(CekState::Eval(desugared, Rc::clone(env)))
            }
            "unless" => {
                if items.len() < 3 {
                    return Err(EvalError::Arity("unless requires a test and body".into()));
                }
                let mut begin_items = vec![ast_sym("begin")];
                begin_items.extend(items[2..].iter().cloned());
                let body = ast_list(begin_items);
                let desugared = ast_list(vec![
                    ast_sym("if"), items[1].clone(),
                    ast_list(vec![ast_sym("begin")]),
                    body,
                ]);
                Ok(CekState::Eval(desugared, Rc::clone(env)))
            }
            "guard" => {
                cek_eval_guard(&items[1..], env, kont)
            }
            "call/cc" | "call-with-current-continuation" => {
                if items.len() != 2 {
                    return Err(EvalError::Arity("call/cc requires exactly 1 argument".into()));
                }
                kont.push(Frame::CallCC);
                Ok(CekState::Eval(items[1].clone(), Rc::clone(env)))
            }
            _ => {
                // Check for macro invocation (get_if_macro avoids cloning non-macro values)
                let mac = env.borrow().get_if_macro(op);
                if let Some(mac) = mac {
                    match mac {
                        Value::Macro { literals, rules, def_env } => {
                            let (expanded, eval_env) = macros::expand_macro_form(&literals, &rules, &def_env, items, env)?;
                            return Ok(CekState::Eval(expanded, eval_env));
                        }
                        Value::SyntaxCaseMacro { transformer, .. } => {
                            let form_ast = ast_list(items.to_vec());
                            let stx = Value::Syntax { ast: form_ast, renames: vec![], source_env: None };
                            kont.push(Frame::MacroResult { use_env: Rc::clone(env) });
                            kont.push(Frame::Args { func: *transformer, done: vec![], remaining: vec![], env: Rc::clone(env) });
                            return Ok(CekState::Apply(stx));
                        }
                        _ => unreachable!(),
                    }
                }
                // Function application
                cek_eval_application(items, env, kont)
            }
        }
    } else {
        cek_eval_application(items, env, kont)
    }
}

fn cek_eval_application(items: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    kont.push(Frame::EvalFunc { args: items[1..].to_vec(), env: Rc::clone(env) });
    Ok(CekState::Eval(items[0].clone(), Rc::clone(env)))
}

fn cek_eval_define(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        AstKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            kont.push(Frame::Define { name: name.clone(), env: Rc::clone(env) });
            Ok(CekState::Eval(args[1].clone(), Rc::clone(env)))
        }
        AstKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                AstKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol for function name".into())),
            };
            let (params, rest_param) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, rest_param, body, env: Rc::clone(env) };
            env.borrow_mut().set(name, lambda);
            Ok(CekState::Apply(Value::Void))
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn cek_eval_let(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    if let AstKind::Symbol(name) = &args[0].kind {
        return cek_eval_named_let(name, &args[1..], env, kont);
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let body = args[1..].to_vec();
    let mut bindings = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let bname = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("let: expected symbol in binding".into())),
                };
                bindings.push((bname, pair[1].clone()));
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    if bindings.is_empty() {
        let local_env = Environment::with_parent(env);
        return cek_eval_body(body, local_env, kont);
    }
    let first = bindings.remove(0);
    kont.push(Frame::LetBind {
        name: first.0,
        remaining: bindings,
        values: Vec::new(),
        body,
        eval_env: Rc::clone(env),
    });
    Ok(CekState::Eval(first.1, Rc::clone(env)))
}

fn cek_eval_named_let(name: &str, args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("named let requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let body = args[1..].to_vec();
    let mut bindings = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let bname = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("let: expected symbol in binding".into())),
                };
                bindings.push((bname, pair[1].clone()));
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    if bindings.is_empty() {
        let local_env = Environment::with_parent(env);
        let lambda = Value::Lambda { params: Vec::new(), rest_param: None, body: body.clone(), env: Rc::clone(&local_env) };
        local_env.borrow_mut().set(name.to_string(), lambda);
        return cek_eval_body(body, local_env, kont);
    }
    let first = bindings.remove(0);
    kont.push(Frame::NamedLetBind {
        loop_name: name.to_string(),
        param: first.0,
        remaining: bindings,
        values: Vec::new(),
        body,
        eval_env: Rc::clone(env),
    });
    Ok(CekState::Eval(first.1, Rc::clone(env)))
}

fn cek_eval_let_star(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let*: expected bindings list".into())),
    };
    let body = args[1..].to_vec();
    let local_env = Environment::with_parent(env);
    let mut bindings = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let bname = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("let*: expected symbol in binding".into())),
                };
                bindings.push((bname, pair[1].clone()));
            }
            _ => return Err(EvalError::Type("let*: invalid binding".into())),
        }
    }
    if bindings.is_empty() {
        return cek_eval_body(body, local_env, kont);
    }
    let first = bindings.remove(0);
    kont.push(Frame::LetStarBind {
        name: first.0,
        remaining: bindings,
        body,
        local_env: local_env.clone(),
    });
    Ok(CekState::Eval(first.1, local_env))
}

fn cek_eval_letrec(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let body = args[1..].to_vec();
    let local_env = Environment::with_parent(env);
    let mut bindings = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("letrec: expected symbol in binding".into())),
                };
                local_env.borrow_mut().set(name.clone(), Value::Void);
                bindings.push((name, pair[1].clone()));
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    if bindings.is_empty() {
        return cek_eval_body(body, local_env, kont);
    }
    let first = bindings.remove(0);
    kont.push(Frame::LetrecBind {
        name: first.0,
        remaining: bindings,
        body,
        local_env: local_env.clone(),
    });
    Ok(CekState::Eval(first.1, local_env))
}

fn cek_eval_letrec_star(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let body = args[1..].to_vec();
    let local_env = Environment::with_parent(env);
    let mut bindings = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("letrec*: expected symbol in binding".into())),
                };
                local_env.borrow_mut().set(name.clone(), Value::Void);
                bindings.push((name, pair[1].clone()));
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    if bindings.is_empty() {
        return cek_eval_body(body, local_env, kont);
    }
    let first = bindings.remove(0);
    kont.push(Frame::LetrecStarBind {
        name: first.0,
        remaining: bindings,
        body,
        local_env: local_env.clone(),
    });
    Ok(CekState::Eval(first.1, local_env))
}

/// Transform quasiquote into an evaluable AST expression, then evaluate it.
fn cek_eval_quasiquote(tmpl: &Ast, env: &Env, _kont: &mut Kont) -> Result<CekState, EvalError> {
    let expanded = quasiquote::qq_expand(tmpl);
    Ok(CekState::Eval(expanded, Rc::clone(env)))
}

pub(crate) fn cek_eval_cond(clauses: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if clauses.is_empty() {
        return Ok(CekState::Apply(Value::Void));
    }
    let clause = &clauses[0];
    let citems = match &clause.kind {
        AstKind::List(citems) if !citems.is_empty() => citems,
        _ => return Err(EvalError::Type("cond: invalid clause".into())),
    };
    let is_else = matches!(&citems[0].kind, AstKind::Symbol(s) if s == "else");
    if is_else {
        if citems.len() < 2 {
            return Err(EvalError::Type("cond: else clause needs a body".into()));
        }
        return cek_eval_body(citems[1..].to_vec(), Rc::clone(env), kont);
    }
    kont.push(Frame::CondClause {
        body: citems[1..].to_vec(),
        remaining: clauses[1..].to_vec(),
        env: Rc::clone(env),
    });
    Ok(CekState::Eval(citems[0].clone(), Rc::clone(env)))
}

fn cek_eval_case(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires at least a key expression".into()));
    }
    kont.push(Frame::CaseKey {
        clauses: args[1..].to_vec(),
        env: Rc::clone(env),
    });
    Ok(CekState::Eval(args[0].clone(), Rc::clone(env)))
}

fn cek_eval_string_set(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires 3 arguments".into()));
    }
    let var_name = match &args[0].kind {
        AstKind::Symbol(name) => name.clone(),
        _ => return Err(EvalError::Type("string-set!: strings are immutable".into())),
    };
    kont.push(Frame::StringSetIdx {
        var_name,
        char_expr: args[2].clone(),
        env: Rc::clone(env),
    });
    Ok(CekState::Eval(args[1].clone(), Rc::clone(env)))
}

fn cek_eval_do(args: &[Ast], env: &Env) -> Result<CekState, EvalError> {
    // Desugar to named-let
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable bindings and test clause".into()));
    }
    let var_specs = match &args[0].kind {
        AstKind::List(v) => v,
        _ => return Err(EvalError::Type("do: expected variable list".into())),
    };
    let test_clause = match &args[1].kind {
        AstKind::List(t) if !t.is_empty() => t,
        _ => return Err(EvalError::Type("do: expected test clause".into())),
    };
    let body = &args[2..];
    let loop_name = format!("__do_{}", DO_COUNTER.fetch_add(1, Ordering::Relaxed));
    let mut params = Vec::new();
    let mut inits = Vec::new();
    let mut steps = Vec::new();
    for spec in var_specs {
        match &spec.kind {
            AstKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("do: expected symbol for variable".into())),
                };
                params.push(name.clone());
                inits.push(parts[1].clone());
                steps.push(if parts.len() >= 3 { parts[2].clone() } else { ast_sym(&name) });
            }
            _ => return Err(EvalError::Type("do: invalid variable spec".into())),
        }
    }
    let bindings: Vec<Ast> = params.iter().zip(inits.iter())
        .map(|(p, i)| ast_list(vec![ast_sym(p), i.clone()]))
        .collect();
    let mut loop_call_items = vec![ast_sym(&loop_name)];
    loop_call_items.extend(steps);
    let loop_call = ast_list(loop_call_items);
    let test_expr = test_clause[0].clone();
    let result_exprs = &test_clause[1..];
    let then_branch = if result_exprs.is_empty() {
        ast_list(vec![ast_sym("begin")])
    } else {
        let mut items = vec![ast_sym("begin")];
        items.extend(result_exprs.iter().cloned());
        ast_list(items)
    };
    let mut else_items = vec![ast_sym("begin")];
    else_items.extend(body.iter().cloned());
    else_items.push(loop_call);
    let else_branch = ast_list(else_items);
    let if_expr = ast_list(vec![ast_sym("if"), test_expr, then_branch, else_branch]);
    let desugared = ast_list(vec![ast_sym("let"), ast_sym(&loop_name), ast_list(bindings), if_expr]);
    Ok(CekState::Eval(desugared, Rc::clone(env)))
}

use cek::{cek_apply_frame, cek_eval_body};

fn cek_eval_guard(args: &[Ast], env: &Env, kont: &mut Kont) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("guard requires a clause spec and body".into()));
    }
    let guard_spec = match &args[0].kind {
        AstKind::List(items) if !items.is_empty() => items,
        _ => return Err(EvalError::Type("guard: expected (var clause ...)".into())),
    };
    let var = match &guard_spec[0].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("guard: expected variable name".into())),
    };
    let clauses = guard_spec[1..].to_vec();
    let body = args[1..].to_vec();
    kont.push(Frame::GuardHandler { var, clauses, env: Rc::clone(env) });
    cek_eval_body(body, Rc::clone(env), kont)
}

pub(crate) fn common_winder_prefix_len(a: &Winders, b: &Winders) -> usize {
    a.iter().zip(b.iter())
        .take_while(|(x, y)| Rc::ptr_eq(x, y))
        .count()
}

// ---- Lambda / Parameter parsing ----

fn eval_lambda(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let (params, rest_param) = match &args[0].kind {
        AstKind::List(ps) => parse_params(ps)?,
        AstKind::Symbol(s) => (vec![], Some(s.clone())),
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: Rc::clone(env),
    })
}

fn eval_case_lambda(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    let mut clauses = Vec::new();
    for clause in args {
        match &clause.kind {
            AstKind::List(items) if items.len() >= 2 => {
                let (params, rest_param) = match &items[0].kind {
                    AstKind::List(ps) => parse_params(ps)?,
                    AstKind::Symbol(s) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Type("case-lambda: expected parameter list".into())),
                };
                let body = items[1..].to_vec();
                clauses.push((params, rest_param, body));
            }
            _ => return Err(EvalError::Type("case-lambda: expected clause (params body ...)".into())),
        }
    }
    Ok(Value::CaseLambda {
        clauses,
        env: Rc::clone(env),
    })
}

/// Parse a parameter list, handling optional dot notation for rest params.
fn parse_params(items: &[Ast]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < items.len() {
        match &items[i].kind {
            AstKind::Symbol(s) if s == "." => {
                if i + 1 != items.len() - 1 {
                    return Err(EvalError::Parse("malformed dotted parameter list".into()));
                }
                rest_param = Some(match &items[i + 1].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("expected symbol after dot in parameters".into())),
                });
                break;
            }
            AstKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Type("expected symbol for parameter".into())),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

// ---- eqv? ----

pub(crate) fn eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

// ---- Macro Support (L10) ----

use macros::eval_define_syntax;

// ---- Record Types (L12) ----

static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

fn eval_define_record_type(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()));
    }
    let _type_name = match &args[0].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected type name symbol".into())),
    };
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // Parse constructor: (constructor-name field-name ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        AstKind::List(items) if !items.is_empty() => {
            let cname = match &items[0].kind {
                AstKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define-record-type: expected constructor name".into())),
            };
            let fields: Vec<String> = items[1..].iter().map(|a| match &a.kind {
                AstKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define-record-type: expected field name in constructor".into())),
            }).collect::<Result<_, _>>()?;
            (cname, fields)
        }
        _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into())),
    };

    // Parse predicate name
    let predicate_name = match &args[2].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into())),
    };

    // Parse field accessors: (field-name accessor-name) ...
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            AstKind::List(items) if items.len() == 2 => {
                let field = match &items[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: expected field name".into())),
                };
                let accessor = match &items[1].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: expected accessor name".into())),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Type("define-record-type: invalid field spec".into())),
        }
    }

    // Register constructor
    env.borrow_mut().set(constructor_name, Value::RecordConstructor {
        type_id,
        type_name: _type_name.clone(),
        field_names: constructor_fields.clone(),
    });

    // Register predicate
    env.borrow_mut().set(predicate_name, Value::RecordPredicate { type_id });

    // Register accessors
    for (field_name, accessor_name) in &field_accessors {
        let field_index = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Type(format!(
                "define-record-type: field {} not in constructor", field_name
            )))?;
        env.borrow_mut().set(accessor_name.clone(), Value::RecordAccessor {
            type_id,
            type_name: _type_name.clone(),
            field_name: accessor_name.clone(),
            field_index,
        });
    }

    Ok(Value::Void)
}

// ---- Apply (fallback for builtins like map/for-each) ----

fn apply(func: &Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(f) => f(args, output),
        Value::Lambda { params, rest_param, body, env } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = Environment::with_parent(env);
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.borrow_mut().set(p.clone(), a.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = args[params.len()..].to_vec();
                local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env, output)?;
            }
            Ok(result)
        }
        Value::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = Environment::with_parent(env);
                    for (p, a) in params.iter().zip(args.iter()) {
                        local_env.borrow_mut().set(p.clone(), a.clone());
                    }
                    if let Some(rest) = rest_param {
                        let rest_args = args[params.len()..].to_vec();
                        local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
                    }
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval(expr, &local_env, output)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments", args.len()
            )))
        }
        Value::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{} constructor expects {} arguments, got {}", type_name, field_names.len(), args.len()
                )));
            }
            let fields: Vec<(String, Value)> = field_names.iter().zip(args.iter())
                .map(|(n, v)| (n.clone(), v.clone()))
                .collect();
            Ok(Value::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields,
            })
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate expects 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)))
        }
        Value::RecordAccessor { type_id, type_name, field_name, field_index } => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor expects 1 argument".into()));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if tid == type_id => {
                    Ok(fields[*field_index].1.clone())
                }
                _ => Err(EvalError::Type(format!(
                    "{}: expected {}", field_name, type_name
                ))),
            }
        }
        Value::CallCC => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires 1 argument".into()));
            }
            // Fallback: create identity continuation
            let cont = Value::Continuation(Vec::new(), Vec::new());
            apply(&args[0], &[cont], output)
        }
        Value::Continuation(saved_kont, saved_winders) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into()));
            }
            if saved_kont.is_empty() {
                Ok(args[0].clone())
            } else {
                cek_run(CekState::Apply(args[0].clone()), &mut saved_kont.clone(), &mut saved_winders.clone(), output)
            }
        }
        Value::CallWithValues => {
            if args.len() != 2 {
                return Err(EvalError::Arity("call-with-values requires exactly 2 arguments".into()));
            }
            let producer_result = apply(&args[0], &[], output)?;
            let consumer_args = match producer_result {
                Value::Values(vs) => vs,
                single => vec![single],
            };
            apply(&args[1], &consumer_args, output)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func.display_value()))),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = builtins::make_global_env();
    let mut output = String::new();
    if exprs.is_empty() {
        return Ok(Value::Void.display_value());
    }
    let mut kont: Kont = Vec::new();
    let mut winders: Winders = Vec::new();
    if exprs.len() > 1 {
        kont.push(Frame::Seq { remaining: exprs[1..].to_vec(), env: Rc::clone(&env) });
    }
    let state = CekState::Eval(exprs[0].clone(), Rc::clone(&env));
    let result = cek_run(state, &mut kont, &mut winders, &mut output)?;
    Ok(result.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = builtins::make_global_env();
    let mut output = String::new();
    if exprs.is_empty() {
        return Ok((Value::Void.display_value(), output));
    }
    let mut kont: Kont = Vec::new();
    let mut winders: Winders = Vec::new();
    if exprs.len() > 1 {
        kont.push(Frame::Seq { remaining: exprs[1..].to_vec(), env: Rc::clone(&env) });
    }
    let state = CekState::Eval(exprs[0].clone(), Rc::clone(&env));
    let result = cek_run(state, &mut kont, &mut winders, &mut output)?;
    Ok((result.display_value(), output))
}
#[cfg(test)]
mod tests;
