use std::fmt;
use std::rc::Rc;
use crate::scheme::env::Env;

pub type Pos = (usize, usize);

#[derive(Debug, Clone)]
pub enum ValueKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Rc<Env>,
    },
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        def_env: Rc<Env>,
    },
    Void,
}

#[derive(Debug, Clone)]
pub struct Value {
    pub kind: ValueKind,
    pub pos: Pos,
}

impl Value {
    pub fn new(kind: ValueKind, pos: Pos) -> Self {
        Value { kind, pos }
    }

    pub fn unpos(kind: ValueKind) -> Self {
        Value { kind, pos: (0, 0) }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self.kind, ValueKind::Boolean(false))
    }

    pub fn to_display(&self) -> String {
        match &self.kind {
            ValueKind::Integer(n) => n.to_string(),
            ValueKind::Boolean(true) => "#t".to_string(),
            ValueKind::Boolean(false) => "#f".to_string(),
            ValueKind::Str(s) => format!("\"{}\"", s),
            ValueKind::Symbol(s) => s.clone(),
            ValueKind::Char(c) => format!("#\\{}", match *c {
                ' ' => "space".to_string(),
                '\n' => "newline".to_string(),
                ch => ch.to_string(),
            }),
            ValueKind::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            ValueKind::Lambda { .. } => "#<procedure>".to_string(),
            ValueKind::SyntaxRules { .. } => "#<syntax>".to_string(),
            ValueKind::Void => "".to_string(),
        }
    }

    /// Format for `display` — strings without quotes, chars as raw characters
    pub fn to_display_output(&self) -> String {
        match &self.kind {
            ValueKind::Str(s) => s.clone(),
            ValueKind::Char(c) => c.to_string(),
            ValueKind::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            ValueKind::SyntaxRules { .. } => "#<syntax>".to_string(),
            _ => self.to_display(),
        }
    }

    /// Format for `write` — strings with quotes (same as to_display for most types)
    pub fn to_write_output(&self) -> String {
        self.to_display()
    }

    pub fn as_integer(&self) -> Option<i64> {
        if let ValueKind::Integer(n) = self.kind { Some(n) } else { None }
    }

    pub fn fmt_pos(&self) -> String {
        format!("{}:{}", self.pos.0, self.pos.1)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (ValueKind::Integer(a), ValueKind::Integer(b)) => a == b,
            (ValueKind::Boolean(a), ValueKind::Boolean(b)) => a == b,
            (ValueKind::Str(a), ValueKind::Str(b)) => a == b,
            (ValueKind::Symbol(a), ValueKind::Symbol(b)) => a == b,
            (ValueKind::Char(a), ValueKind::Char(b)) => a == b,
            (ValueKind::List(a), ValueKind::List(b)) => a == b,
            (ValueKind::Void, ValueKind::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display())
    }
}
