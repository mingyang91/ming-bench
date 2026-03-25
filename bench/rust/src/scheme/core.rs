use std::any::Any;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::error::EvalError;
use super::macros::MacroTransformer;
use super::number::Number;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

impl Position {
    pub(crate) fn attach(self, error: EvalError) -> EvalError {
        error.with_position(self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Bool(bool, Position),
    Number(Number, Position),
    String(String, Position),
    Char(char, Position),
    Symbol(String, Position),
    List(Vec<Expr>, Position),
}

impl Expr {
    pub(crate) fn pos(&self) -> Position {
        match self {
            Self::Bool(_, pos)
            | Self::Number(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

pub(crate) type StringRef = Rc<SchemeString>;
pub(crate) type PairRef = Rc<RefCell<PairCell>>;
pub(crate) type VectorRef = Rc<RefCell<Vec<Value>>>;
pub(crate) type RecordTypeRef = Rc<RecordType>;
pub(crate) type RecordRef = Rc<RecordValue>;
pub(crate) type WinderRef = Rc<DynamicWinder>;
pub(crate) type EnvRef = Rc<RefCell<Environment>>;
pub(crate) type BindingRef = Rc<RefCell<Value>>;
pub(crate) type ExprsRef = Rc<[Expr]>;

pub(crate) struct SchemeString {
    text: RefCell<String>,
    mutable: bool,
}

impl SchemeString {
    pub(crate) fn new(text: impl Into<String>, mutable: bool) -> Self {
        Self {
            text: RefCell::new(text.into()),
            mutable,
        }
    }

    pub(crate) fn borrow(&self) -> Ref<'_, String> {
        self.text.borrow()
    }

    pub(crate) fn borrow_mut(&self) -> RefMut<'_, String> {
        self.text.borrow_mut()
    }

    pub(crate) fn is_mutable(&self) -> bool {
        self.mutable
    }
}

pub(crate) struct PairCell {
    pub(crate) car: Value,
    pub(crate) cdr: Value,
}

#[derive(Clone)]
pub(crate) struct RecordType {
    pub(crate) name: String,
    pub(crate) field_names: Vec<String>,
}

#[derive(Clone)]
pub(crate) struct RecordValue {
    pub(crate) record_type: RecordTypeRef,
    pub(crate) fields: Vec<Value>,
}

#[derive(Clone)]
pub(crate) struct DynamicWinder {
    pub(crate) before: Value,
    pub(crate) after: Value,
}

#[derive(Clone)]
pub(crate) enum Value {
    Bool(bool),
    Number(Number),
    String(StringRef),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(PairRef),
    Vector(VectorRef),
    Record(RecordRef),
    Procedure(Rc<Procedure>),
    Uninitialized,
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        value_equal(self, other)
    }
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    pub(crate) fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Number(_) => "number",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
            Self::List(_) => "list",
            Self::Pair(_) => "pair",
            Self::Vector(_) => "vector",
            Self::Record(_) => "record",
            Self::Procedure(_) => "procedure",
            Self::Uninitialized => "uninitialized",
            Self::Void => "void",
        }
    }

    pub(crate) fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    pub(crate) fn render_display(&self) -> String {
        render_value(self, RenderMode::Display)
    }

    pub(crate) fn render_for_error(&self) -> String {
        match self {
            Self::Void => "#<void>".into(),
            Self::Uninitialized => "#<uninitialized>".into(),
            _ => self.render(),
        }
    }
}

#[derive(Clone)]
pub(crate) enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
    CaseLambda(CaseLambdaProcedure),
    RecordConstructor(RecordConstructorProcedure),
    RecordPredicate(RecordPredicateProcedure),
    RecordAccessor(RecordAccessorProcedure),
    Continuation(Rc<dyn Any>),
}

#[derive(Clone, Copy)]
pub(crate) struct BuiltinProcedure {
    pub(crate) name: &'static str,
    pub(crate) func: fn(&[Value], &mut Runtime) -> Result<Value, EvalError>,
}

#[derive(Clone)]
pub(crate) struct LambdaProcedure {
    pub(crate) name: Option<String>,
    pub(crate) params: Vec<String>,
    pub(crate) rest_param: Option<String>,
    pub(crate) body: ExprsRef,
    pub(crate) env: EnvRef,
}

#[derive(Clone)]
pub(crate) struct CaseLambdaProcedure {
    pub(crate) name: Option<String>,
    pub(crate) clauses: Vec<LambdaProcedure>,
}

#[derive(Clone)]
pub(crate) struct RecordConstructorProcedure {
    pub(crate) name: String,
    pub(crate) record_type: RecordTypeRef,
}

#[derive(Clone)]
pub(crate) struct RecordPredicateProcedure {
    pub(crate) name: String,
    pub(crate) record_type: RecordTypeRef,
}

#[derive(Clone)]
pub(crate) struct RecordAccessorProcedure {
    pub(crate) name: String,
    pub(crate) record_type: RecordTypeRef,
    pub(crate) field_index: usize,
}

pub(crate) struct Environment {
    bindings: HashMap<String, BindingRef>,
    parent: Option<EnvRef>,
}

impl Environment {
    pub(crate) fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent,
        }))
    }

    pub(crate) fn define(env: &EnvRef, name: String, value: Value) {
        let mut borrowed = env.borrow_mut();
        if let Some(binding) = borrowed.bindings.get(&name).cloned() {
            drop(borrowed);
            *binding.borrow_mut() = value;
        } else {
            borrowed.bindings.insert(name, Rc::new(RefCell::new(value)));
        }
    }

    pub(crate) fn define_cell(env: &EnvRef, name: String, binding: BindingRef) {
        env.borrow_mut().bindings.insert(name, binding);
    }

    pub(crate) fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        Self::lookup_cell(env, name).map(|binding| binding.borrow().clone())
    }

    pub(crate) fn lookup_cell(env: &EnvRef, name: &str) -> Option<BindingRef> {
        let borrowed = env.borrow();
        if let Some(binding) = borrowed.bindings.get(name).cloned() {
            return Some(binding);
        }

        let parent = borrowed.parent.clone();
        drop(borrowed);
        parent.and_then(|parent| Self::lookup_cell(&parent, name))
    }

    pub(crate) fn set(env: &EnvRef, name: &str, value: Value) -> bool {
        let borrowed = env.borrow();
        if let Some(binding) = borrowed.bindings.get(name).cloned() {
            drop(borrowed);
            *binding.borrow_mut() = value;
            return true;
        }

        let parent = borrowed.parent.clone();
        drop(borrowed);
        parent
            .map(|parent| Self::set(&parent, name, value))
            .unwrap_or(false)
    }
}

#[derive(Default)]
pub(crate) struct Runtime {
    output: String,
    macros: HashMap<String, MacroTransformer>,
    gensym_counter: usize,
    winders: Vec<WinderRef>,
}

impl Runtime {
    pub(crate) fn display(&mut self, value: &Value) {
        self.output.push_str(&value.render_display());
    }

    pub(crate) fn write(&mut self, value: &Value) {
        self.output.push_str(&value.render());
    }

    pub(crate) fn newline(&mut self) {
        self.output.push('\n');
    }

    pub(crate) fn define_macro(&mut self, name: String, transformer: MacroTransformer) {
        self.macros.insert(name, transformer);
    }

    pub(crate) fn lookup_macro(&self, name: &str) -> Option<MacroTransformer> {
        self.macros.get(name).cloned()
    }

    pub(crate) fn macro_names(&self) -> Vec<String> {
        self.macros.keys().cloned().collect()
    }

    pub(crate) fn fresh_symbol(&mut self, hint: &str) -> String {
        let suffix = self.gensym_counter;
        self.gensym_counter += 1;

        let sanitized = hint
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect::<String>();
        let base = if sanitized.is_empty() {
            "tmp"
        } else {
            &sanitized
        };

        format!("__macro_{suffix}_{base}")
    }

    pub(crate) fn winders(&self) -> Vec<WinderRef> {
        self.winders.clone()
    }

    pub(crate) fn push_winder(&mut self, winder: WinderRef) {
        self.winders.push(winder);
    }

    pub(crate) fn pop_winder(&mut self) -> Option<WinderRef> {
        self.winders.pop()
    }

    pub(crate) fn replace_winders(&mut self, winders: Vec<WinderRef>) {
        self.winders = winders;
    }

    pub(crate) fn into_output(self) -> String {
        self.output
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

pub(crate) fn make_string(text: impl Into<String>) -> Value {
    Value::String(Rc::new(SchemeString::new(text, true)))
}

pub(crate) fn make_immutable_string(text: impl Into<String>) -> Value {
    Value::String(Rc::new(SchemeString::new(text, false)))
}

pub(crate) fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairCell { car, cdr })))
}

pub(crate) fn empty_list() -> Value {
    Value::List(Vec::new())
}

pub(crate) fn list_from_vec(mut values: Vec<Value>) -> Value {
    let mut list = empty_list();

    while let Some(value) = values.pop() {
        list = make_pair(value, list);
    }

    list
}

pub(crate) fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    let mut values = Vec::new();
    let mut current = value.clone();
    let mut seen_pairs = HashSet::new();

    loop {
        match current {
            Value::List(items) => {
                if items.is_empty() {
                    return Some(values);
                }

                values.extend(items);
                return Some(values);
            }
            Value::Pair(pair) => {
                let ptr = Rc::as_ptr(&pair) as usize;
                if !seen_pairs.insert(ptr) {
                    return None;
                }

                let borrowed = pair.borrow();
                values.push(borrowed.car.clone());
                current = borrowed.cdr.clone();
            }
            _ => return None,
        }
    }
}

pub(crate) fn is_proper_list(value: &Value) -> bool {
    list_to_vec(value).is_some()
}

pub(crate) fn make_vector(values: Vec<Value>) -> Value {
    Value::Vector(Rc::new(RefCell::new(values)))
}

pub(crate) fn make_lambda(
    name: Option<String>,
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: &EnvRef,
) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params,
        rest_param,
        body: body.into(),
        env: env.clone(),
    })))
}

pub(crate) fn make_case_lambda(name: Option<String>, clauses: Vec<LambdaProcedure>) -> Value {
    Value::Procedure(Rc::new(Procedure::CaseLambda(CaseLambdaProcedure {
        name,
        clauses,
    })))
}

pub(crate) fn make_record_type(name: impl Into<String>, field_names: Vec<String>) -> RecordTypeRef {
    Rc::new(RecordType {
        name: name.into(),
        field_names,
    })
}

pub(crate) fn make_record(record_type: &RecordTypeRef, fields: Vec<Value>) -> Value {
    Value::Record(Rc::new(RecordValue {
        record_type: record_type.clone(),
        fields,
    }))
}

pub(crate) fn make_record_constructor(
    name: impl Into<String>,
    record_type: &RecordTypeRef,
) -> Value {
    Value::Procedure(Rc::new(Procedure::RecordConstructor(
        RecordConstructorProcedure {
            name: name.into(),
            record_type: record_type.clone(),
        },
    )))
}

pub(crate) fn make_record_predicate(name: impl Into<String>, record_type: &RecordTypeRef) -> Value {
    Value::Procedure(Rc::new(Procedure::RecordPredicate(
        RecordPredicateProcedure {
            name: name.into(),
            record_type: record_type.clone(),
        },
    )))
}

pub(crate) fn make_record_accessor(
    name: impl Into<String>,
    record_type: &RecordTypeRef,
    field_index: usize,
) -> Value {
    Value::Procedure(Rc::new(Procedure::RecordAccessor(
        RecordAccessorProcedure {
            name: name.into(),
            record_type: record_type.clone(),
            field_index,
        },
    )))
}

pub(crate) fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool(value, _) => Value::Bool(*value),
        Expr::Number(value, _) => Value::Number(*value),
        Expr::String(value, _) => make_immutable_string(value.clone()),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => list_from_vec(items.iter().map(quote_expr).collect()),
    }
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    let mut seen_pairs = HashSet::new();
    let mut seen_vectors = HashSet::new();
    render_value_inner(value, mode, &mut seen_pairs, &mut seen_vectors)
}

fn render_value_inner(
    value: &Value,
    mode: RenderMode,
    seen_pairs: &mut HashSet<usize>,
    seen_vectors: &mut HashSet<usize>,
) -> String {
    match value {
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::Number(value) => value.render(),
        Value::String(value) => {
            let text = value.borrow();
            match mode {
                RenderMode::Write => render_string(text.as_str()),
                RenderMode::Display => text.as_str().to_string(),
            }
        }
        Value::Symbol(value) => value.clone(),
        Value::Char(value) => match mode {
            RenderMode::Write => render_char(*value),
            RenderMode::Display => value.to_string(),
        },
        Value::List(values) => render_list(values, mode, seen_pairs, seen_vectors),
        Value::Pair(pair) => render_pair(pair, mode, seen_pairs, seen_vectors),
        Value::Vector(vector) => render_vector(vector, mode, seen_pairs, seen_vectors),
        Value::Record(record) => render_record(record),
        Value::Procedure(_) => "#<procedure>".into(),
        Value::Uninitialized => "#<uninitialized>".into(),
        Value::Void => String::new(),
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn render_list(
    values: &[Value],
    mode: RenderMode,
    seen_pairs: &mut HashSet<usize>,
    seen_vectors: &mut HashSet<usize>,
) -> String {
    let mut rendered = String::from("(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value_inner(value, mode, seen_pairs, seen_vectors));
    }

    rendered.push(')');
    rendered
}

fn render_pair(
    pair: &PairRef,
    mode: RenderMode,
    seen_pairs: &mut HashSet<usize>,
    seen_vectors: &mut HashSet<usize>,
) -> String {
    let ptr = Rc::as_ptr(pair) as usize;
    if !seen_pairs.insert(ptr) {
        return "#<circular>".into();
    }

    let (car, mut tail) = {
        let borrowed = pair.borrow();
        (borrowed.car.clone(), borrowed.cdr.clone())
    };
    let mut entered_pairs = vec![ptr];
    let mut rendered = String::from("(");

    rendered.push_str(&render_value_inner(&car, mode, seen_pairs, seen_vectors));

    loop {
        match tail {
            Value::List(values) => {
                for value in values {
                    rendered.push(' ');
                    rendered.push_str(&render_value_inner(&value, mode, seen_pairs, seen_vectors));
                }

                rendered.push(')');
                break;
            }
            Value::Pair(next_pair) => {
                let next_ptr = Rc::as_ptr(&next_pair) as usize;
                if !seen_pairs.insert(next_ptr) {
                    rendered.push_str(" . #<circular>)");
                    break;
                }

                entered_pairs.push(next_ptr);
                let (next_car, next_cdr) = {
                    let borrowed = next_pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                rendered.push(' ');
                rendered.push_str(&render_value_inner(
                    &next_car,
                    mode,
                    seen_pairs,
                    seen_vectors,
                ));
                tail = next_cdr;
            }
            other => {
                rendered.push_str(" . ");
                rendered.push_str(&render_value_inner(&other, mode, seen_pairs, seen_vectors));
                rendered.push(')');
                break;
            }
        }
    }

    for ptr in entered_pairs {
        seen_pairs.remove(&ptr);
    }

    rendered
}

fn render_vector(
    vector: &VectorRef,
    mode: RenderMode,
    seen_pairs: &mut HashSet<usize>,
    seen_vectors: &mut HashSet<usize>,
) -> String {
    let ptr = Rc::as_ptr(vector) as usize;
    if !seen_vectors.insert(ptr) {
        return "#<circular-vector>".into();
    }

    let values = vector.borrow().clone();
    let mut rendered = String::from("#(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value_inner(value, mode, seen_pairs, seen_vectors));
    }

    rendered.push(')');
    seen_vectors.remove(&ptr);
    rendered
}

fn render_record(record: &RecordRef) -> String {
    format!("#<record {}>", record.as_ref().record_type.name)
}

pub(crate) fn value_equal(lhs: &Value, rhs: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    let mut seen_vectors = HashSet::new();
    value_equal_inner(lhs, rhs, &mut seen_pairs, &mut seen_vectors)
}

fn value_equal_inner(
    lhs: &Value,
    rhs: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
    seen_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
    match (lhs, rhs) {
        (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
        (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::String(rhs)) => lhs.borrow().as_str() == rhs.borrow().as_str(),
        (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs == rhs,
        (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
        (Value::List(lhs), Value::List(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs.iter())
                    .all(|(lhs, rhs)| value_equal_inner(lhs, rhs, seen_pairs, seen_vectors))
        }
        (Value::Pair(lhs), Value::Pair(rhs)) => {
            let key = (Rc::as_ptr(lhs) as usize, Rc::as_ptr(rhs) as usize);
            if !seen_pairs.insert(key) {
                return true;
            }

            let (lhs_car, lhs_cdr) = {
                let borrowed = lhs.borrow();
                (borrowed.car.clone(), borrowed.cdr.clone())
            };
            let (rhs_car, rhs_cdr) = {
                let borrowed = rhs.borrow();
                (borrowed.car.clone(), borrowed.cdr.clone())
            };

            let equal = value_equal_inner(&lhs_car, &rhs_car, seen_pairs, seen_vectors)
                && value_equal_inner(&lhs_cdr, &rhs_cdr, seen_pairs, seen_vectors);
            seen_pairs.remove(&key);
            equal
        }
        (Value::Vector(lhs), Value::Vector(rhs)) => {
            let key = (Rc::as_ptr(lhs) as usize, Rc::as_ptr(rhs) as usize);
            if !seen_vectors.insert(key) {
                return true;
            }

            let lhs = lhs.borrow().clone();
            let rhs = rhs.borrow().clone();
            let equal = lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs.iter())
                    .all(|(lhs, rhs)| value_equal_inner(lhs, rhs, seen_pairs, seen_vectors));
            seen_vectors.remove(&key);
            equal
        }
        (Value::Record(lhs), Value::Record(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::Procedure(lhs), Value::Procedure(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}
