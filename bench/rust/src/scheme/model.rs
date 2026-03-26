use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::error::{EvalError, SourcePos};
use super::number::Number;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Expr {
    Number(Number, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    pub(super) fn pos(&self) -> SourcePos {
        match self {
            Self::Number(_, pos)
            | Self::Boolean(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    ZeroPred,
    PositivePred,
    NegativePred,
    OddPred,
    EvenPred,
    ExactPred,
    InexactPred,
    IntegerPred,
    RationalPred,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
    Less,
    Greater,
    Equal,
    LessEqual,
    EqPred,
    EqualPred,
    Not,
    Display,
    Write,
    Newline,
    Cons,
    Car,
    Cdr,
    Append,
    List,
    Length,
    ListRef,
    ListTail,
    ListPred,
    Assoc,
    Map,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringSet,
    StringCopy,
    NullPred,
    NumberPred,
    StringPred,
    BooleanPred,
    ProcedurePred,
    PairPred,
    SymbolPred,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLess,
    StringEqual,
    StringLess,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Apply,
}

impl Builtin {
    pub(super) fn name(self) -> &'static str {
        match self {
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Mul => "*",
            Builtin::Div => "/",
            Builtin::Abs => "abs",
            Builtin::Modulo => "modulo",
            Builtin::Remainder => "remainder",
            Builtin::Quotient => "quotient",
            Builtin::Min => "min",
            Builtin::Max => "max",
            Builtin::Expt => "expt",
            Builtin::ZeroPred => "zero?",
            Builtin::PositivePred => "positive?",
            Builtin::NegativePred => "negative?",
            Builtin::OddPred => "odd?",
            Builtin::EvenPred => "even?",
            Builtin::ExactPred => "exact?",
            Builtin::InexactPred => "inexact?",
            Builtin::IntegerPred => "integer?",
            Builtin::RationalPred => "rational?",
            Builtin::ExactToInexact => "exact->inexact",
            Builtin::InexactToExact => "inexact->exact",
            Builtin::Numerator => "numerator",
            Builtin::Denominator => "denominator",
            Builtin::Less => "<",
            Builtin::Greater => ">",
            Builtin::Equal => "=",
            Builtin::LessEqual => "<=",
            Builtin::EqPred => "eq?",
            Builtin::EqualPred => "equal?",
            Builtin::Not => "not",
            Builtin::Display => "display",
            Builtin::Write => "write",
            Builtin::Newline => "newline",
            Builtin::Cons => "cons",
            Builtin::Car => "car",
            Builtin::Cdr => "cdr",
            Builtin::Append => "append",
            Builtin::List => "list",
            Builtin::Length => "length",
            Builtin::ListRef => "list-ref",
            Builtin::ListTail => "list-tail",
            Builtin::ListPred => "list?",
            Builtin::Assoc => "assoc",
            Builtin::Map => "map",
            Builtin::StringAppend => "string-append",
            Builtin::StringLength => "string-length",
            Builtin::Substring => "substring",
            Builtin::StringToNumber => "string->number",
            Builtin::NumberToString => "number->string",
            Builtin::SymbolToString => "symbol->string",
            Builtin::StringToSymbol => "string->symbol",
            Builtin::StringRef => "string-ref",
            Builtin::StringSet => "string-set!",
            Builtin::StringCopy => "string-copy",
            Builtin::NullPred => "null?",
            Builtin::NumberPred => "number?",
            Builtin::StringPred => "string?",
            Builtin::BooleanPred => "boolean?",
            Builtin::ProcedurePred => "procedure?",
            Builtin::PairPred => "pair?",
            Builtin::SymbolPred => "symbol?",
            Builtin::CharPred => "char?",
            Builtin::CharAlphabeticPred => "char-alphabetic?",
            Builtin::CharNumericPred => "char-numeric?",
            Builtin::CharUpcase => "char-upcase",
            Builtin::CharDowncase => "char-downcase",
            Builtin::CharEqual => "char=?",
            Builtin::CharLess => "char<?",
            Builtin::StringEqual => "string=?",
            Builtin::StringLess => "string<?",
            Builtin::StringCiEqual => "string-ci=?",
            Builtin::StringUpcase => "string-upcase",
            Builtin::StringDowncase => "string-downcase",
            Builtin::Apply => "apply",
        }
    }
}

#[derive(Clone)]
pub(super) struct SchemeString {
    chars: Rc<RefCell<Vec<char>>>,
    mutable: bool,
}

impl SchemeString {
    fn from_owned(value: String, mutable: bool) -> Self {
        Self {
            chars: Rc::new(RefCell::new(value.chars().collect())),
            mutable,
        }
    }

    pub(super) fn literal(value: &str) -> Self {
        Self::from_owned(value.into(), false)
    }

    pub(super) fn fresh(value: String) -> Self {
        Self::from_owned(value, true)
    }

    pub(super) fn mutable_copy(&self) -> Self {
        Self {
            chars: Rc::new(RefCell::new(self.chars.borrow().clone())),
            mutable: true,
        }
    }

    pub(super) fn to_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }

    pub(super) fn chars(&self) -> Vec<char> {
        self.chars.borrow().clone()
    }

    pub(super) fn len(&self) -> usize {
        self.chars.borrow().len()
    }

    pub(super) fn get(&self, index: usize) -> Option<char> {
        self.chars.borrow().get(index).copied()
    }

    pub(super) fn set(&self, index: usize, value: char) -> bool {
        let mut chars = self.chars.borrow_mut();
        let Some(slot) = chars.get_mut(index) else {
            return false;
        };
        *slot = value;
        true
    }

    pub(super) fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub(super) fn shares_storage(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.chars, &other.chars)
    }
}

#[derive(Clone)]
pub(super) enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Record(Rc<RecordInstance>),
    RecordProcedure(Rc<RecordProcedure>),
    Void,
}

impl Value {
    pub(super) fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Char(_) => "char",
            Value::List(_) => "list",
            Value::Pair(_, _) => "pair",
            Value::Builtin(_) | Value::Procedure(_) | Value::RecordProcedure(_) => "procedure",
            Value::Record(_) => "record",
            Value::Void => "void",
        }
    }

    pub(super) fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    pub(super) fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    pub(super) fn render_display(&self) -> String {
        render_value(self, RenderMode::Display)
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

pub(super) type EnvRef = Rc<Env>;
pub(super) type ValueCell = Rc<RefCell<Value>>;
pub(super) type MacroRef = Rc<MacroTransformer>;

pub(super) struct Env {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, ValueCell>>,
    macro_bindings: RefCell<HashMap<String, MacroRef>>,
}

impl Env {
    pub(super) fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            macro_bindings: RefCell::new(HashMap::new()),
        })
    }

    pub(super) fn define(&self, name: String, value: Value) {
        self.bindings
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    pub(super) fn define_alias(&self, name: String, cell: ValueCell) {
        self.bindings.borrow_mut().insert(name, cell);
    }

    pub(super) fn define_macro(&self, name: String, transformer: MacroRef) {
        self.macro_bindings.borrow_mut().insert(name, transformer);
    }

    pub(super) fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_cell(name).map(|cell| cell.borrow().clone())
    }

    pub(super) fn lookup_cell(&self, name: &str) -> Option<ValueCell> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_cell(name))
    }

    pub(super) fn lookup_macro(&self, name: &str) -> Option<MacroRef> {
        if let Some(transformer) = self.macro_bindings.borrow().get(name).cloned() {
            return Some(transformer);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_macro(name))
    }

    pub(super) fn set(&self, name: &str, value: Value) -> bool {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            *cell.borrow_mut() = value;
            return true;
        }

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
    }
}

pub(super) struct Procedure {
    pub(super) kind: ProcedureKind,
    pub(super) name: Option<String>,
    pub(super) clauses: Vec<ProcedureClause>,
    pub(super) env: EnvRef,
}

impl Procedure {
    pub(super) fn expected_args(&self) -> String {
        if self.clauses.len() == 1 {
            return self.clauses[0].params.expected_args();
        }

        let mut expected = Vec::with_capacity(self.clauses.len());
        for clause in &self.clauses {
            let arity = clause.params.expected_args();
            if !expected.contains(&arity) {
                expected.push(arity);
            }
        }

        format!("one of {}", expected.join(", "))
    }

    pub(super) fn error_name(&self) -> &str {
        self.name.as_deref().unwrap_or(match self.kind {
            ProcedureKind::Lambda => "lambda",
            ProcedureKind::CaseLambda => "case-lambda",
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ProcedureKind {
    Lambda,
    CaseLambda,
}

#[derive(Clone)]
pub(super) struct ProcedureClause {
    pub(super) params: Params,
    pub(super) body: Vec<Expr>,
}

#[derive(Clone)]
pub(super) struct RecordType {
    pub(super) name: String,
    pub(super) field_count: usize,
}

#[derive(Clone)]
pub(super) struct RecordInstance {
    pub(super) record_type: Rc<RecordType>,
    pub(super) fields: Vec<Value>,
}

#[derive(Clone)]
pub(super) struct RecordProcedure {
    pub(super) name: String,
    pub(super) kind: RecordProcedureKind,
}

#[derive(Clone)]
pub(super) enum RecordProcedureKind {
    Constructor {
        record_type: Rc<RecordType>,
    },
    Predicate {
        record_type: Rc<RecordType>,
    },
    Accessor {
        record_type: Rc<RecordType>,
        field_index: usize,
    },
}

#[derive(Clone)]
pub(super) struct Params {
    pub(super) required: Vec<String>,
    pub(super) rest: Option<String>,
}

impl Params {
    pub(super) fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }

    pub(super) fn expected_args(&self) -> String {
        match self.rest {
            Some(_) => format!("at least {}", self.required.len()),
            None => self.required.len().to_string(),
        }
    }

    pub(super) fn matches_arity(&self, got: usize) -> bool {
        match self.rest {
            Some(_) => got >= self.required.len(),
            None => got == self.required.len(),
        }
    }
}

#[derive(Clone)]
pub(super) struct MacroTransformer {
    pub(super) literals: HashSet<String>,
    pub(super) rules: Vec<SyntaxRule>,
    pub(super) env: EnvRef,
}

#[derive(Clone)]
pub(super) struct SyntaxRule {
    pub(super) pattern: Expr,
    pub(super) template: Expr,
}

#[derive(Default)]
pub(super) struct PatternBindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

impl PatternBindings {
    pub(super) fn bind_single(&mut self, name: &str, expr: &Expr) -> bool {
        if self.repeated.contains_key(name) {
            return false;
        }

        match self.single.get(name) {
            Some(existing) => expr_datum_eq(existing, expr),
            None => {
                self.single.insert(name.into(), expr.clone());
                true
            }
        }
    }

    pub(super) fn bind_repeated(&mut self, name: &str, expr: &Expr) -> bool {
        if self.single.contains_key(name) {
            return false;
        }

        self.repeated
            .entry(name.into())
            .or_default()
            .push(expr.clone());
        true
    }

    pub(super) fn seed_repeated(&mut self, name: &str) {
        self.repeated.entry(name.into()).or_default();
    }

    pub(super) fn substitute(
        &self,
        name: &str,
        repeat_index: Option<usize>,
    ) -> Result<Option<Expr>, EvalError> {
        if let Some(expr) = self.single.get(name) {
            return Ok(Some(expr.clone()));
        }

        if let Some(values) = self.repeated.get(name) {
            let Some(index) = repeat_index else {
                return Err(EvalError::Syntax {
                    message: format!(
                        "template: repeated pattern variable '{name}' used outside ellipsis"
                    ),
                });
            };

            return values
                .get(index)
                .cloned()
                .map(Some)
                .ok_or_else(|| EvalError::Syntax {
                    message: format!("template: ellipsis index out of range for '{name}'"),
                });
        }

        Ok(None)
    }

    pub(super) fn repetition_len(&self, name: &str) -> Option<usize> {
        self.repeated.get(name).map(Vec::len)
    }
}

pub(super) struct MacroExpansion {
    pub(super) expr: Expr,
    pub(super) value_aliases: Vec<(String, ValueCell)>,
    pub(super) macro_aliases: Vec<(String, MacroRef)>,
}

pub(super) struct ExpansionState {
    pub(super) bindings: PatternBindings,
    pub(super) definition_env: EnvRef,
    pub(super) alias_names: HashMap<String, String>,
    pub(super) value_aliases: Vec<(String, ValueCell)>,
    pub(super) macro_aliases: Vec<(String, MacroRef)>,
}

impl ExpansionState {
    pub(super) fn new(bindings: PatternBindings, definition_env: &EnvRef) -> Self {
        Self {
            bindings,
            definition_env: definition_env.clone(),
            alias_names: HashMap::new(),
            value_aliases: Vec::new(),
            macro_aliases: Vec::new(),
        }
    }
}

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(super) fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name, _) if name == "...")
}

pub(super) fn expr_datum_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Number(a, _), Expr::Number(b, _)) => a == b,
        (Expr::Boolean(a, _), Expr::Boolean(b, _)) => a == b,
        (Expr::String(a, _), Expr::String(b, _)) => a == b,
        (Expr::Char(a, _), Expr::Char(b, _)) => a == b,
        (Expr::Symbol(a, _), Expr::Symbol(b, _)) => a == b,
        (Expr::List(a, _), Expr::List(b, _)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| expr_datum_eq(a, b))
        }
        _ => false,
    }
}

pub(super) fn is_core_syntax(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-syntax"
            | "define-record-type"
            | "set!"
            | "if"
            | "quote"
            | "lambda"
            | "case-lambda"
            | "and"
            | "or"
            | "begin"
            | "cond"
            | "let"
    )
}

pub(super) fn fresh_identifier(base: &str) -> String {
    format!(
        "__ming_macro_{}_{}__",
        base,
        GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed)
    )
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    match value {
        Value::Number(value) => value.render(),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::String(value) => match mode {
            RenderMode::Write => format!("\"{}\"", escape_string(&value.to_plain_string())),
            RenderMode::Display => value.to_plain_string(),
        },
        Value::Symbol(value) => value.clone(),
        Value::Char(ch) => render_char(*ch, mode),
        Value::List(items) => render_list(items, mode),
        Value::Pair(head, tail) => render_pair(head, tail, mode),
        Value::Builtin(_) | Value::Procedure(_) | Value::RecordProcedure(_) => {
            "#<procedure>".into()
        }
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Void => String::new(),
    }
}

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let parts: Vec<String> = items
        .iter()
        .map(|value| render_value(value, mode))
        .collect();
    format!("({})", parts.join(" "))
}

fn render_pair(head: &Value, tail: &Value, mode: RenderMode) -> String {
    let mut rendered = String::new();
    rendered.push('(');
    rendered.push_str(&render_value(head, mode));
    render_pair_tail(tail, mode, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_tail(tail: &Value, mode: RenderMode, rendered: &mut String) {
    match tail {
        Value::List(items) => {
            for item in items {
                rendered.push(' ');
                rendered.push_str(&render_value(item, mode));
            }
        }
        Value::Pair(head, next) => {
            rendered.push(' ');
            rendered.push_str(&render_value(head, mode));
            render_pair_tail(next, mode, rendered);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&render_value(other, mode));
        }
    }
}

fn render_char(ch: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => ch.to_string(),
        RenderMode::Write => match ch {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            other => format!("#\\{other}"),
        },
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();

    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }

    escaped
}
