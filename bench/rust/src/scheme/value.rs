use std::any::Any;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

/// Opaque wrapper for a captured continuation stack.
#[derive(Clone)]
pub struct CapturedCont(pub Rc<dyn Any>);

impl fmt::Debug for CapturedCont {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<continuation>")
    }
}

/// Whether a Scheme string can be mutated via `string-set!`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringMutability {
    Immutable,
    Mutable,
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(Rc<RefCell<String>>, StringMutability),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Void,
    Builtin(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        closure_env: Rc<Env>,
    },
    Continuation(CapturedCont),
    /// A fixed-size mutable vector.
    Vector(Rc<RefCell<Vec<Value>>>),
    /// A mutable cons cell with shared identity.
    Pair(Rc<RefCell<(Value, Value)>>),
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Vec<Expr>, Expr)>,
        def_env: Rc<Env>,
    },
    /// A syntax object wrapping a parsed expression (for syntax-case macros).
    SyntaxObject(Expr),
    /// A syntax-case macro transformer (wraps a Lambda).
    SyntaxTransformer(Box<Value>),
    /// Multiple return values from `(values ...)`.
    MultipleValues(Vec<Value>),
    /// A record instance created by `define-record-type`.
    Record {
        type_tag: Rc<()>,
        type_name: String,
        fields: Vec<Value>,
    },
    /// Record constructor procedure.
    RecordConstructor {
        type_tag: Rc<()>,
        type_name: String,
        field_names: Vec<String>,
    },
    /// Record type predicate.
    RecordPredicate {
        type_tag: Rc<()>,
    },
    /// Record field accessor.
    RecordAccessor {
        type_tag: Rc<()>,
        type_name: String,
        field_name: String,
        field_index: usize,
    },
}

impl Value {
    /// Convenience constructor for immutable string values (literals).
    pub fn new_str(s: String) -> Self {
        Value::Str(Rc::new(RefCell::new(s)), StringMutability::Immutable)
    }

    /// Convenience constructor for mutable string values (string-copy results).
    pub fn new_mutable_str(s: String) -> Self {
        Value::Str(Rc::new(RefCell::new(s)), StringMutability::Mutable)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a, _), Value::Str(b, _)) => *a.borrow() == *b.borrow(),
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Void, Value::Void) => true,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => {
                if Rc::ptr_eq(a, b) { return true; }
                let ab = a.borrow();
                let bb = b.borrow();
                ab.0 == bb.0 && ab.1 == bb.1
            }
            (Value::Pair(_), Value::List(_)) | (Value::List(_), Value::Pair(_)) => {
                match (self.to_vec(), other.to_vec()) {
                    (Some(a), Some(b)) => a == b,
                    _ => false,
                }
            }
            (Value::Continuation(_), Value::Continuation(_)) => false,
            (Value::SyntaxRules { .. }, Value::SyntaxRules { .. }) => false,
            (Value::SyntaxObject(_), Value::SyntaxObject(_)) => false,
            (Value::SyntaxTransformer(_), Value::SyntaxTransformer(_)) => false,
            (Value::MultipleValues(a), Value::MultipleValues(b)) => a == b,
            (Value::Record { type_tag: ta, fields: fa, .. }, Value::Record { type_tag: tb, fields: fb, .. }) => {
                Rc::ptr_eq(ta, tb) && fa == fb
            }
            _ => false,
        }
    }
}

impl Value {
    /// Display string for output (write-style: strings get quotes).
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => format_float(*f),
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s, _) => format!("\"{}\"", s.borrow()),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|e| e.to_display_string()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Pair(cell) => display_pair_chain(cell, Value::to_display_string),
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::Continuation(_) => "#<continuation>".into(),
            Value::SyntaxRules { .. } => "#<macro>".into(),
            Value::SyntaxObject(_) => "#<syntax>".into(),
            Value::SyntaxTransformer(_) => "#<macro>".into(),
            Value::MultipleValues(vals) => {
                let inner: Vec<String> = vals.iter().map(|v| v.to_display_string()).collect();
                format!("#<values: {}>", inner.join(" "))
            }
            Value::Record { type_name, .. } => format!("#<record:{}>", type_name),
            Value::RecordConstructor { type_name, .. } => format!("#<procedure:make-{}>", type_name),
            Value::RecordPredicate { .. } => "#<procedure>".into(),
            Value::RecordAccessor { type_name, field_name, .. } => {
                format!("#<procedure:{}-{}>", type_name, field_name)
            }
        }
    }

    /// Display string for `display` — strings without quotes.
    pub fn to_display_output(&self) -> String {
        match self {
            Value::Str(s, _) => s.borrow().clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_output()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|e| e.to_display_output()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Pair(cell) => display_pair_chain(cell, Value::to_display_output),
            Value::Continuation(_) => "#<continuation>".into(),
            Value::SyntaxRules { .. } => "#<macro>".into(),
            Value::SyntaxObject(_) => "#<syntax>".into(),
            Value::SyntaxTransformer(_) => "#<macro>".into(),
            Value::MultipleValues(_) => self.to_display_string(),
            Value::Record { .. } | Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. } | Value::RecordAccessor { .. } => self.to_display_string(),
            other => other.to_display_string(),
        }
    }

    /// Returns true if this value is truthy (everything except #f).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

impl Value {
    /// Create a new mutable cons cell (Pair).
    pub fn make_pair(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new(RefCell::new((car, cdr))))
    }

    /// Create a proper list from a vec of values as a Pair chain ending in empty List.
    pub fn list_from_vec(elems: Vec<Value>) -> Value {
        let mut result = Value::List(Vec::new());
        for elem in elems.into_iter().rev() {
            result = Value::make_pair(elem, result);
        }
        result
    }

    /// Flatten this value to a Vec if it's a proper list (List or Pair chain).
    /// Returns None for improper lists or cycles.
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        match self {
            Value::List(v) => Some(v.clone()),
            Value::Pair(_) => {
                let mut result = Vec::new();
                let mut current = self.clone();
                let mut seen = HashSet::new();
                loop {
                    match current {
                        Value::List(v) => {
                            result.extend(v);
                            return Some(result);
                        }
                        Value::Pair(ref cell) => {
                            let ptr = Rc::as_ptr(cell) as usize;
                            if !seen.insert(ptr) {
                                return None; // cycle
                            }
                            let (car, cdr) = {
                                let b = cell.borrow();
                                (b.0.clone(), b.1.clone())
                            };
                            result.push(car);
                            current = cdr;
                        }
                        _ => return None, // improper
                    }
                }
            }
            _ => None,
        }
    }
}

/// Display a Pair chain with cycle detection.
fn display_pair_chain(cell: &Rc<RefCell<(Value, Value)>>, f: fn(&Value) -> String) -> String {
    let mut parts = Vec::new();
    let mut seen = HashSet::new();
    let mut dot_tail: Option<String> = None;
    let mut cur_cell = Rc::clone(cell);

    loop {
        let ptr = Rc::as_ptr(&cur_cell) as usize;
        if !seen.insert(ptr) {
            break; // cycle detected — stop
        }
        let (car, cdr) = {
            let b = cur_cell.borrow();
            (b.0.clone(), b.1.clone())
        };
        parts.push(f(&car));
        match cdr {
            Value::Pair(next) => {
                cur_cell = next;
            }
            Value::List(v) if v.is_empty() => break,
            Value::List(v) => {
                for elem in &v {
                    parts.push(f(elem));
                }
                break;
            }
            other => {
                dot_tail = Some(f(&other));
                break;
            }
        }
    }

    if let Some(tail) = dot_tail {
        format!("({} . {})", parts.join(" "), tail)
    } else {
        format!("({})", parts.join(" "))
    }
}

/// Format a float for Scheme display. Ensures a decimal point is always present.
fn format_float(f: f64) -> String {
    let s = format!("{}", f);
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{}.0", s)
    }
}
