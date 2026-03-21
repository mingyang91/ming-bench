use crate::scheme::env::Env;
use crate::scheme::expr::Expr;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    Length,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
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
    CharPred,
}

impl Builtin {
    pub fn bind_all(env: &Env) {
        for (name, builtin) in [
            ("+", Self::Add),
            ("-", Self::Sub),
            ("*", Self::Mul),
            ("/", Self::Div),
            ("<", Self::LessThan),
            (">", Self::GreaterThan),
            ("=", Self::Equal),
            ("<=", Self::LessEqual),
            ("not", Self::Not),
            ("cons", Self::Cons),
            ("car", Self::Car),
            ("cdr", Self::Cdr),
            ("null?", Self::NullPred),
            ("list", Self::List),
            ("length", Self::Length),
            ("string?", Self::StringPred),
            ("number?", Self::NumberPred),
            ("boolean?", Self::BooleanPred),
            ("pair?", Self::PairPred),
            ("symbol?", Self::SymbolPred),
            ("display", Self::Display),
            ("write", Self::Write),
            ("newline", Self::Newline),
            ("string-append", Self::StringAppend),
            ("string-length", Self::StringLength),
            ("substring", Self::Substring),
            ("string->number", Self::StringToNumber),
            ("number->string", Self::NumberToString),
            ("symbol->string", Self::SymbolToString),
            ("string->symbol", Self::StringToSymbol),
            ("string-ref", Self::StringRef),
            ("char?", Self::CharPred),
        ] {
            env.define(name.to_string(), Value::Builtin(builtin));
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    EmptyList,
    Pair(Box<Value>, Box<Value>),
    Builtin(Builtin),
    Closure {
        parameters: Vec<String>,
        body: Rc<[Expr]>,
        env: Env,
    },
    Void,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    pub fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    pub fn render_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    pub(crate) fn from_list(items: Vec<Value>) -> Self {
        items.into_iter().rev().fold(Self::EmptyList, |tail, item| {
            Self::Pair(Box::new(item), Box::new(tail))
        })
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Number(value) => value.to_string(),
            Self::Bool(true) => "#t".to_string(),
            Self::Bool(false) => "#f".to_string(),
            Self::Str(value) => match mode {
                RenderMode::Write => format!("\"{}\"", escape_string(value)),
                RenderMode::Display => value.clone(),
            },
            Self::Char(value) => render_char(*value, mode),
            Self::Symbol(name) => name.clone(),
            Self::EmptyList => "()".to_string(),
            Self::Pair(_, _) => format!("({})", render_pair(self, mode)),
            Self::Builtin(_) | Self::Closure { .. } => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

fn render_pair(pair: &Value, mode: RenderMode) -> String {
    let mut parts = Vec::new();
    let mut current = pair;

    loop {
        match current {
            Value::Pair(car, cdr) => {
                parts.push(car.render_with_mode(mode));
                current = cdr.as_ref();
            }
            Value::EmptyList => return parts.join(" "),
            other => {
                parts.push(".".to_string());
                parts.push(other.render_with_mode(mode));
                return parts.join(" ");
            }
        }
    }
}

fn render_char(value: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => value.to_string(),
        RenderMode::Write => match value {
            ' ' => "#\\space".to_string(),
            '\n' => "#\\newline".to_string(),
            other => format!("#\\{other}"),
        },
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();

    for ch in value.chars() {
        match ch {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }

    escaped
}
