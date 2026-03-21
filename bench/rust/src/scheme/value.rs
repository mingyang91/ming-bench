use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Char(char),
    Pair(Box<Value>, Box<Value>),
    List(Vec<Value>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Expr,
        closure: Env,
    },
    Builtin(String),
    Continuation { id: u64, expr_index: usize },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Expr>, Expr)>,
        def_env: Env,
    },
    Values(Vec<Value>),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (
                Value::Continuation { id: a, .. },
                Value::Continuation { id: b, .. },
            ) => a == b,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Pair(car, cdr) => write!(f, "({car} . {cdr})"),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => fmt_list(f, items),
            Value::Vector(v) => fmt_vector(f, &v.borrow()),
            Value::Lambda { .. }
            | Value::Builtin(_)
            | Value::Continuation { .. }
            | Value::Macro { .. } => write!(f, "#<procedure>"),
            Value::Values(vals) => fmt_values(f, vals),
            Value::Void => write!(f, ""),
        }
    }
}

fn fmt_values(f: &mut fmt::Formatter<'_>, vals: &[Value]) -> fmt::Result {
    let Some((first, _)) = vals.split_first() else {
        return write!(f, "");
    };
    write!(f, "{first}")
}

fn fmt_vector(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    write!(f, "#(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 { write!(f, " ")?; }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

fn fmt_list(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    write!(f, "(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 { write!(f, " ")?; }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    /// Check if this value is a pair (non-empty list or dotted pair).
    pub fn is_pair(&self) -> bool {
        matches!(self, Value::List(l) if !l.is_empty()) || matches!(self, Value::Pair(_, _))
    }

    /// Format for `display` — strings without quotes, chars as plain characters.
    pub fn display_fmt(&self, buf: &mut String) {
        match self {
            Value::String(s) => buf.push_str(s),
            Value::Char(c) => buf.push(*c),
            Value::Pair(car, cdr) => {
                buf.push('(');
                car.display_fmt(buf);
                buf.push_str(" . ");
                cdr.display_fmt(buf);
                buf.push(')');
            }
            Value::List(items) => Self::display_list(items, buf),
            Value::Vector(v) => Self::display_vector(&v.borrow(), buf),
            Value::Continuation { .. } | Value::Macro { .. } => buf.push_str("#<procedure>"),
            Value::Values(vals) => Self::display_values(vals, buf),
            other => buf.push_str(&other.to_string()),
        }
    }

    fn display_vector(items: &[Value], buf: &mut String) {
        buf.push_str("#(");
        let Some((first, rest)) = items.split_first() else {
            buf.push(')');
            return;
        };
        first.display_fmt(buf);
        for item in rest {
            buf.push(' ');
            item.display_fmt(buf);
        }
        buf.push(')');
    }

    fn display_values(vals: &[Value], buf: &mut String) {
        if let Some((first, _)) = vals.split_first() {
            first.display_fmt(buf);
        }
    }

    fn display_list(items: &[Value], buf: &mut String) {
        buf.push('(');
        let Some((first, rest)) = items.split_first() else {
            buf.push(')');
            return;
        };
        first.display_fmt(buf);
        for item in rest {
            buf.push(' ');
            item.display_fmt(buf);
        }
        buf.push(')');
    }
}
