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
    Gcd,
    Lcm,
    Min,
    Max,
    Expt,
    Truncate,
    Round,
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
    GreaterEqual,
    EqPred,
    EqvPred,
    EqualPred,
    Not,
    Display,
    Write,
    Newline,
    Cons,
    Car,
    Cdr,
    Cddr,
    SetCar,
    SetCdr,
    Append,
    Reverse,
    List,
    Length,
    ListRef,
    ListTail,
    ListPred,
    Member,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
    Assoc,
    Assv,
    Map,
    ForEach,
    MakeString,
    String,
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
    StringToList,
    ListToString,
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
    CharToInteger,
    IntegerToChar,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLess,
    StringEqual,
    StringLess,
    StringGreater,
    StringLessEqual,
    StringGreaterEqual,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Raise,
    WithExceptionHandler,
    DynamicWind,
    Apply,
    CallCc,
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
            Builtin::Gcd => "gcd",
            Builtin::Lcm => "lcm",
            Builtin::Min => "min",
            Builtin::Max => "max",
            Builtin::Expt => "expt",
            Builtin::Truncate => "truncate",
            Builtin::Round => "round",
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
            Builtin::GreaterEqual => ">=",
            Builtin::EqPred => "eq?",
            Builtin::EqvPred => "eqv?",
            Builtin::EqualPred => "equal?",
            Builtin::Not => "not",
            Builtin::Display => "display",
            Builtin::Write => "write",
            Builtin::Newline => "newline",
            Builtin::Cons => "cons",
            Builtin::Car => "car",
            Builtin::Cdr => "cdr",
            Builtin::Cddr => "cddr",
            Builtin::SetCar => "set-car!",
            Builtin::SetCdr => "set-cdr!",
            Builtin::Append => "append",
            Builtin::Reverse => "reverse",
            Builtin::List => "list",
            Builtin::Length => "length",
            Builtin::ListRef => "list-ref",
            Builtin::ListTail => "list-tail",
            Builtin::ListPred => "list?",
            Builtin::Member => "member",
            Builtin::Vector => "vector",
            Builtin::MakeVector => "make-vector",
            Builtin::VectorRef => "vector-ref",
            Builtin::VectorSet => "vector-set!",
            Builtin::VectorLength => "vector-length",
            Builtin::VectorPred => "vector?",
            Builtin::VectorToList => "vector->list",
            Builtin::ListToVector => "list->vector",
            Builtin::Assoc => "assoc",
            Builtin::Assv => "assv",
            Builtin::Map => "map",
            Builtin::ForEach => "for-each",
            Builtin::MakeString => "make-string",
            Builtin::String => "string",
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
            Builtin::StringToList => "string->list",
            Builtin::ListToString => "list->string",
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
            Builtin::CharToInteger => "char->integer",
            Builtin::IntegerToChar => "integer->char",
            Builtin::CharUpcase => "char-upcase",
            Builtin::CharDowncase => "char-downcase",
            Builtin::CharEqual => "char=?",
            Builtin::CharLess => "char<?",
            Builtin::StringEqual => "string=?",
            Builtin::StringLess => "string<?",
            Builtin::StringGreater => "string>?",
            Builtin::StringLessEqual => "string<=?",
            Builtin::StringGreaterEqual => "string>=?",
            Builtin::StringCiEqual => "string-ci=?",
            Builtin::StringUpcase => "string-upcase",
            Builtin::StringDowncase => "string-downcase",
            Builtin::Raise => "raise",
            Builtin::WithExceptionHandler => "with-exception-handler",
            Builtin::DynamicWind => "dynamic-wind",
            Builtin::Apply => "apply",
            Builtin::CallCc => "call/cc",
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
pub(super) struct SchemeVector {
    items: Rc<RefCell<Vec<Value>>>,
}

impl SchemeVector {
    pub(super) fn new(items: Vec<Value>) -> Self {
        Self {
            items: Rc::new(RefCell::new(items)),
        }
    }

    pub(super) fn len(&self) -> usize {
        self.items.borrow().len()
    }

    pub(super) fn get(&self, index: usize) -> Option<Value> {
        self.items.borrow().get(index).cloned()
    }

    pub(super) fn set(&self, index: usize, value: Value) -> bool {
        let mut items = self.items.borrow_mut();
        let Some(slot) = items.get_mut(index) else {
            return false;
        };
        *slot = value;
        true
    }

    pub(super) fn to_vec(&self) -> Vec<Value> {
        self.items.borrow().clone()
    }

    pub(super) fn shares_storage(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.items, &other.items)
    }

    pub(super) fn id(&self) -> usize {
        Rc::as_ptr(&self.items) as usize
    }
}

#[derive(Clone)]
pub(super) struct SchemePair {
    cell: Rc<RefCell<PairValue>>,
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

impl SchemePair {
    pub(super) fn new(car: Value, cdr: Value) -> Self {
        Self {
            cell: Rc::new(RefCell::new(PairValue { car, cdr })),
        }
    }

    pub(super) fn car(&self) -> Value {
        self.cell.borrow().car.clone()
    }

    pub(super) fn cdr(&self) -> Value {
        self.cell.borrow().cdr.clone()
    }

    pub(super) fn set_car(&self, value: Value) {
        self.cell.borrow_mut().car = value;
    }

    pub(super) fn set_cdr(&self, value: Value) {
        self.cell.borrow_mut().cdr = value;
    }

    pub(super) fn shares_storage(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.cell, &other.cell)
    }

    pub(super) fn id(&self) -> usize {
        Rc::as_ptr(&self.cell) as usize
    }
}

pub(super) type ContinuationProc = Rc<dyn Fn(Value, &mut String) -> Result<Value, EvalError>>;

#[derive(Clone)]
pub(super) enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    EmptyList,
    Pair(SchemePair),
    Vector(SchemeVector),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Continuation(ContinuationProc),
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
            Value::EmptyList => "list",
            Value::Pair(_) => "pair",
            Value::Vector(_) => "vector",
            Value::Builtin(_)
            | Value::Procedure(_)
            | Value::Continuation(_)
            | Value::RecordProcedure(_) => "procedure",
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

pub(super) fn list_from_values<I>(items: I) -> Value
where
    I: IntoIterator<Item = Value>,
{
    let mut items = items.into_iter().collect::<Vec<_>>();
    let mut list = Value::EmptyList;

    while let Some(item) = items.pop() {
        list = Value::Pair(SchemePair::new(item, list));
    }

    list
}

pub(super) fn is_proper_list(value: &Value) -> bool {
    let mut current = value.clone();
    let mut seen = HashSet::new();

    loop {
        match current {
            Value::EmptyList => return true,
            Value::Pair(pair) => {
                if !seen.insert(pair.id()) {
                    return false;
                }
                current = pair.cdr();
            }
            _ => return false,
        }
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
            | "let*"
            | "letrec"
            | "letrec*"
            | "case"
            | "do"
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
    let mut state = RenderState::default();
    render_value_with_state(value, mode, &mut state)
}

#[derive(Default)]
struct RenderState {
    active_pairs: HashSet<usize>,
    active_vectors: HashSet<usize>,
}

fn render_value_with_state(value: &Value, mode: RenderMode, state: &mut RenderState) -> String {
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
        Value::EmptyList => "()".into(),
        Value::Pair(pair) => render_pair(pair, mode, state),
        Value::Vector(vector) => render_vector(vector, mode, state),
        Value::Builtin(_)
        | Value::Procedure(_)
        | Value::Continuation(_)
        | Value::RecordProcedure(_) => "#<procedure>".into(),
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Void => String::new(),
    }
}

fn render_pair(pair: &SchemePair, mode: RenderMode, state: &mut RenderState) -> String {
    if !state.active_pairs.insert(pair.id()) {
        return "#<cycle>".into();
    }

    let mut rendered = String::new();
    rendered.push('(');
    rendered.push_str(&render_value_with_state(&pair.car(), mode, state));
    render_pair_tail(&pair.cdr(), mode, state, &mut rendered);
    rendered.push(')');
    state.active_pairs.remove(&pair.id());
    rendered
}

fn render_pair_tail(
    tail: &Value,
    mode: RenderMode,
    state: &mut RenderState,
    rendered: &mut String,
) {
    match tail {
        Value::EmptyList => {}
        Value::Pair(pair) => {
            if !state.active_pairs.insert(pair.id()) {
                rendered.push_str(" . #<cycle>");
                return;
            }
            rendered.push(' ');
            rendered.push_str(&render_value_with_state(&pair.car(), mode, state));
            render_pair_tail(&pair.cdr(), mode, state, rendered);
            state.active_pairs.remove(&pair.id());
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&render_value_with_state(other, mode, state));
        }
    }
}

fn render_vector(vector: &SchemeVector, mode: RenderMode, state: &mut RenderState) -> String {
    if !state.active_vectors.insert(vector.id()) {
        return "#<cycle>".into();
    }

    let parts: Vec<String> = vector
        .to_vec()
        .iter()
        .map(|value| render_value_with_state(value, mode, state))
        .collect();
    state.active_vectors.remove(&vector.id());
    format!("#({})", parts.join(" "))
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
