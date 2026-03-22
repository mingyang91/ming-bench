use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::Span;
use crate::scheme::macro_expand::SyntaxRulesDef;
use crate::scheme::parser::Expr;

/// A saved evaluation context frame for continuation resumption.
#[derive(Debug, Clone)]
pub struct ResumeFrame {
    pub exprs: Vec<Expr>,
    pub env: Env,
}

/// Evaluation context threaded through the interpreter for continuation support.
pub struct ContCtx {
    /// Stack of resume frames (innermost = last).
    pub frames: Vec<ResumeFrame>,
    /// Pending value for a resuming continuation (consumed by the matching call/cc).
    pub pending: Option<Value>,
    /// Span of the call/cc that originally captured the continuation being resumed.
    /// Used to ensure the pending value is only consumed by the matching call/cc site.
    pub resume_span: Option<Span>,
    /// Frames from the most recently invoked saved continuation.
    pub resume_frames: Option<Vec<ResumeFrame>>,
    next_id: u64,
}

impl Default for ContCtx {
    fn default() -> Self {
        Self::new()
    }
}

impl ContCtx {
    pub fn new() -> Self {
        ContCtx {
            frames: Vec::new(),
            pending: None,
            resume_span: None,
            resume_frames: None,
            next_id: 0,
        }
    }

    pub fn next_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn push_frame(&mut self, frame: ResumeFrame) {
        self.frames.push(frame);
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }
}

/// Compute GCD of two non-negative integers.
fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Construct a rational or integer value, always simplified.
pub fn make_rational(num: i64, den: i64) -> Value {
    let sign = if den < 0 { -1 } else { 1 };
    let num = num * sign;
    let den = den.abs();
    let g = gcd(num.abs(), den);
    let num = num / g;
    let den = den / g;
    if den == 1 {
        Value::Integer(num)
    } else {
        Value::Rational(num, den)
    }
}

/// Data carried by a syntax object.
#[derive(Debug, Clone)]
#[allow(clippy::derived_hash_with_manual_eq)]
pub struct SyntaxObjectData {
    pub expr: Expr,
    pub introduced: Vec<(String, String)>,
    pub template_env: Option<Env>,
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    Char(char),
    Nil,
    Pair(Rc<RefCell<(Value, Value)>>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
        name: Option<String>,
    },
    Builtin(String),
    Continuation {
        id: u64,
        frames: Vec<ResumeFrame>,
        capture_span: Span,
    },
    Macro {
        syntax_rules: SyntaxRulesDef,
        def_env: Env,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    /// Multiple return values from `values`.
    Values(Vec<Value>),
    /// A syntax object wrapping a parsed expression (used by syntax-case).
    /// Carries: Expr, introduced hygiene pairs [(gensym, original)], and
    /// the environment for resolving hygiene bindings.
    SyntaxObject(Box<SyntaxObjectData>),
    /// A syntax-case macro transformer (lambda-based).
    SyntaxCaseMacro {
        params: Vec<String>,
        body: Vec<Expr>,
        def_env: Env,
    },
    /// A case-lambda (multiple-arity procedure).
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>,
        env: Env,
        name: Option<String>,
    },
    /// A record instance.
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<Value>,
    },
    /// Constructor for a record type.
    RecordConstructor {
        type_id: u64,
        type_name: String,
        field_names: Vec<String>,
    },
    /// Predicate for a record type.
    RecordPredicate { type_id: u64 },
    /// Accessor for a record field.
    RecordAccessor {
        type_id: u64,
        field_index: usize,
    },
}

impl Value {
    /// Returns true if the value is truthy (everything except #f).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    /// Construct a mutable pair.
    pub fn pair(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new(RefCell::new((car, cdr))))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::SchemeString(a), Value::SchemeString(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a), Value::Pair(b)) => {
                let ab = a.borrow();
                let bb = b.borrow();
                ab.0 == bb.0 && ab.1 == bb.1
            }
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Continuation { id: a, .. }, Value::Continuation { id: b, .. }) => a == b,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::CaseLambda { .. }, Value::CaseLambda { .. }) => false,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::SyntaxObject(a), Value::SyntaxObject(b)) => a.expr == b.expr,
            (Value::SyntaxCaseMacro { .. }, Value::SyntaxCaseMacro { .. }) => false,
            (
                Value::Record {
                    type_id: a_id,
                    fields: a_fields,
                    ..
                },
                Value::Record {
                    type_id: b_id,
                    fields: b_fields,
                    ..
                },
            ) => a_id == b_id && a_fields == b_fields,
            (
                Value::RecordConstructor { type_id: a, .. },
                Value::RecordConstructor { type_id: b, .. },
            ) => a == b,
            (Value::RecordPredicate { type_id: a }, Value::RecordPredicate { type_id: b }) => {
                a == b
            }
            (
                Value::RecordAccessor {
                    type_id: a_id,
                    field_index: a_idx,
                },
                Value::RecordAccessor {
                    type_id: b_id,
                    field_index: b_idx,
                },
            ) => a_id == b_id && a_idx == b_idx,
            _ => false,
        }
    }
}

impl Value {
    /// Format value for `display` (no quotes on strings).
    pub fn display_fmt(&self, buf: &mut String) {
        match self {
            Value::SchemeString(s) => buf.push_str(s),
            Value::Pair(_) => {
                buf.push('(');
                display_list(buf, self, 0);
            }
            Value::Vector(v) => {
                buf.push_str("#(");
                for (i, elem) in v.borrow().iter().enumerate() {
                    if i > 0 {
                        buf.push(' ');
                    }
                    elem.display_fmt(buf);
                }
                buf.push(')');
            }
            Value::Values(vals) => {
                if let Some(last) = vals.last() {
                    last.display_fmt(buf);
                }
            }
            Value::Builtin(_)
            | Value::Lambda { .. }
            | Value::CaseLambda { .. }
            | Value::Continuation { .. }
            | Value::Macro { .. }
            | Value::SyntaxObject(_)
            | Value::SyntaxCaseMacro { .. }
            | Value::Record { .. }
            | Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => buf.push_str(&self.to_string()),
            other => buf.push_str(&other.to_string()),
        }
    }

    /// Format value for `write` (quotes on strings).
    pub fn write_fmt(&self, buf: &mut String) {
        buf.push_str(&self.to_string());
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(val) => {
                let s = format!("{val}");
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    write!(f, "{s}")
                } else {
                    write!(f, "{s}.0")
                }
            }
            Value::Rational(num, den) => write!(f, "{num}/{den}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::SchemeString(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Nil => write!(f, "()"),
            Value::Pair(_) => {
                write!(f, "(")?;
                write_list(f, self, 0)
            }
            Value::Vector(v) => {
                write!(f, "#(")?;
                for (i, elem) in v.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::CaseLambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Continuation { .. } => write!(f, "#<continuation>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::SyntaxObject(_) => write!(f, "#<syntax>"),
            Value::SyntaxCaseMacro { .. } => write!(f, "#<macro>"),
            Value::Record { type_name, .. } => write!(f, "#<record:{type_name}>"),
            Value::RecordConstructor { type_name, .. } => {
                write!(f, "#<procedure:make-{type_name}>")
            }
            Value::RecordPredicate { .. } => write!(f, "#<procedure>"),
            Value::RecordAccessor { .. } => write!(f, "#<procedure>"),
            Value::Values(vals) => {
                if let Some(last) = vals.last() {
                    write!(f, "{last}")
                } else {
                    write!(f, "")
                }
            }
        }
    }
}

const MAX_LIST_DEPTH: usize = 1000;

fn write_list(f: &mut fmt::Formatter<'_>, val: &Value, depth: usize) -> fmt::Result {
    if depth > MAX_LIST_DEPTH {
        return write!(f, "...)");
    }
    match val {
        Value::Pair(p) => {
            let pair = p.borrow();
            write!(f, "{}", pair.0)?;
            match &pair.1 {
                Value::Nil => write!(f, ")"),
                Value::Pair(_) => {
                    write!(f, " ")?;
                    let cdr = pair.1.clone();
                    drop(pair);
                    write_list(f, &cdr, depth + 1)
                }
                other => write!(f, " . {other})"),
            }
        }
        _ => write!(f, ")"),
    }
}

fn display_list(buf: &mut String, val: &Value, depth: usize) {
    if depth > MAX_LIST_DEPTH {
        buf.push_str("...)");
        return;
    }
    match val {
        Value::Pair(p) => {
            let pair = p.borrow();
            pair.0.display_fmt(buf);
            match &pair.1 {
                Value::Nil => buf.push(')'),
                Value::Pair(_) => {
                    buf.push(' ');
                    let cdr = pair.1.clone();
                    drop(pair);
                    display_list(buf, &cdr, depth + 1);
                }
                other => {
                    buf.push_str(" . ");
                    other.display_fmt(buf);
                    buf.push(')');
                }
            }
        }
        _ => buf.push(')'),
    }
}
