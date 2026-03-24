use std::cell::RefCell;
use std::rc::Rc;

use super::environment::EnvRef;
use super::expr::Expr;

#[derive(Clone)]
pub(crate) struct SchemeString {
    chars: Rc<RefCell<Vec<char>>>,
}

impl SchemeString {
    pub(crate) fn new(value: impl AsRef<str>) -> Self {
        Self {
            chars: Rc::new(RefCell::new(value.as_ref().chars().collect())),
        }
    }

    pub(crate) fn copy(&self) -> Self {
        Self {
            chars: Rc::new(RefCell::new(self.chars.borrow().clone())),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.chars.borrow().len()
    }

    pub(crate) fn char_at(&self, index: usize) -> Option<char> {
        self.chars.borrow().get(index).copied()
    }

    pub(crate) fn set_char(&self, index: usize, value: char) -> bool {
        let mut chars = self.chars.borrow_mut();
        if index >= chars.len() {
            return false;
        }
        chars[index] = value;
        true
    }

    pub(crate) fn substring(&self, start: usize, end: usize) -> Self {
        let chars = self.chars.borrow();
        Self {
            chars: Rc::new(RefCell::new(chars[start..end].to_vec())),
        }
    }

    pub(crate) fn as_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }
}

#[derive(Clone)]
pub(crate) struct Closure {
    pub(crate) parameters: Vec<String>,
    pub(crate) body: Vec<Expr>,
    pub(crate) env: EnvRef,
}

#[derive(Clone, Copy)]
pub(crate) enum Builtin {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPredicate,
    List,
    Length,
    Append,
    StringPredicate,
    NumberPredicate,
    BooleanPredicate,
    PairPredicate,
    SymbolPredicate,
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    CharPredicate,
    StringSet,
    StringCopy,
}

impl Builtin {
    pub(crate) const ALL: [Builtin; 35] = [
        Builtin::Add,
        Builtin::Subtract,
        Builtin::Multiply,
        Builtin::Divide,
        Builtin::LessThan,
        Builtin::GreaterThan,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::Not,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::NullPredicate,
        Builtin::List,
        Builtin::Length,
        Builtin::Append,
        Builtin::StringPredicate,
        Builtin::NumberPredicate,
        Builtin::BooleanPredicate,
        Builtin::PairPredicate,
        Builtin::SymbolPredicate,
        Builtin::Display,
        Builtin::Write,
        Builtin::Newline,
        Builtin::StringAppend,
        Builtin::StringLength,
        Builtin::Substring,
        Builtin::StringToNumber,
        Builtin::NumberToString,
        Builtin::SymbolToString,
        Builtin::StringToSymbol,
        Builtin::StringRef,
        Builtin::CharPredicate,
        Builtin::StringSet,
        Builtin::StringCopy,
    ];

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPredicate => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::StringPredicate => "string?",
            Self::NumberPredicate => "number?",
            Self::BooleanPredicate => "boolean?",
            Self::PairPredicate => "pair?",
            Self::SymbolPredicate => "symbol?",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::CharPredicate => "char?",
            Self::StringSet => "string-set!",
            Self::StringCopy => "string-copy",
        }
    }
}

#[derive(Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    String(SchemeString),
    Character(char),
    Symbol(String),
    List(Vec<Value>),
    Void,
    Builtin(Builtin),
    Closure(Closure),
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    pub(crate) fn render(&self) -> String {
        self.render_with_mode(false)
    }

    pub(crate) fn render_for_display(&self) -> String {
        self.render_with_mode(true)
    }

    fn render_with_mode(&self, display_mode: bool) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => {
                if *value {
                    "#t".into()
                } else {
                    "#f".into()
                }
            }
            Self::String(value) => {
                if display_mode {
                    value.as_plain_string()
                } else {
                    escape_string(&value.as_plain_string())
                }
            }
            Self::Character(value) => {
                if display_mode {
                    value.to_string()
                } else {
                    render_character(*value)
                }
            }
            Self::Symbol(name) => name.clone(),
            Self::List(elements) => render_list(elements, display_mode),
            Self::Void => "#<void>".into(),
            Self::Builtin(name) => format!("#<procedure:{}>", name.name()),
            Self::Closure(_) => "#<procedure>".into(),
        }
    }
}

fn escape_string(value: &str) -> String {
    let mut builder = String::with_capacity(value.len() + 2);
    builder.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => builder.push_str("\\\\"),
            '"' => builder.push_str("\\\""),
            '\n' => builder.push_str("\\n"),
            '\r' => builder.push_str("\\r"),
            '\t' => builder.push_str("\\t"),
            _ => builder.push(ch),
        }
    }
    builder.push('"');
    builder
}

fn render_character(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        '\t' => "#\\tab".into(),
        _ => format!("#\\{value}"),
    }
}

fn render_list(elements: &[Value], display_mode: bool) -> String {
    if elements.is_empty() {
        return "()".into();
    }

    let mut builder = String::from("(");
    for (index, element) in elements.iter().enumerate() {
        if index > 0 {
            builder.push(' ');
        }
        if display_mode {
            builder.push_str(&element.render_for_display());
        } else {
            builder.push_str(&element.render());
        }
    }
    builder.push(')');
    builder
}
