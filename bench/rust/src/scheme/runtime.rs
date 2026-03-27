use super::error::EvalError;
use super::number::Number;
use super::text::{escape_string, render_char, SchemeString};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Position {
    pub(super) line: usize,
    pub(super) col: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Expr {
    Bool { value: bool, pos: Position },
    Number { value: Number, pos: Position },
    Char { value: char, pos: Position },
    String { value: String, pos: Position },
    Symbol { name: String, pos: Position },
    List { items: Vec<Expr>, pos: Position },
}

impl Expr {
    pub(super) fn pos(&self) -> Position {
        match self {
            Self::Bool { pos, .. }
            | Self::Number { pos, .. }
            | Self::Char { pos, .. }
            | Self::String { pos, .. }
            | Self::Symbol { pos, .. }
            | Self::List { pos, .. } => *pos,
        }
    }
}

pub(super) type EnvRef = Rc<Environment>;
pub(super) type BindingRef = Rc<RefCell<Value>>;
pub(super) type MacroRef = Rc<MacroTransformer>;
pub(super) type SyntaxRef = Rc<SyntaxObject>;
pub(super) type SyntaxContextRef = Rc<RefCell<MacroExpansionContext>>;
pub(super) type BuiltinFn = fn(&[EvaluatedArg], &mut String) -> Result<Value, EvalError>;

#[derive(Clone, Debug)]
pub(super) struct LambdaParams {
    pub(super) fixed: Vec<String>,
    pub(super) rest: Option<String>,
}

impl LambdaParams {
    pub(super) fn fixed_arity(&self) -> usize {
        self.fixed.len()
    }

    pub(super) fn allows_rest(&self) -> bool {
        self.rest.is_some()
    }

    pub(super) fn matches_arity(&self, arity: usize) -> bool {
        arity >= self.fixed_arity() && (self.allows_rest() || arity == self.fixed_arity())
    }
}

#[derive(Clone)]
pub(super) struct CaseLambdaClause {
    pub(super) params: LambdaParams,
    pub(super) body: Vec<Expr>,
}

#[derive(Clone)]
pub(super) struct EvaluatedArg {
    pub(super) value: Value,
    pub(super) pos: Position,
}

pub(super) type ContinuationRef = Option<Rc<Continuation>>;

#[derive(Clone)]
pub(super) struct Continuation {
    pub(super) frame: Frame,
    pub(super) next: ContinuationRef,
}

#[derive(Clone)]
pub(super) struct DynamicWind {
    pub(super) before: Value,
    pub(super) after: Value,
    pub(super) pos: Position,
}

#[derive(Clone)]
pub(super) struct WindTransferStep {
    pub(super) thunk: Value,
    pub(super) pos: Position,
    pub(super) active_winds: Vec<Rc<DynamicWind>>,
}

#[derive(Clone)]
pub(super) enum CondAction {
    ReturnTestValue,
    EvalBody(Vec<Expr>),
    ApplyRecipient { recipient: Expr },
}

#[derive(Clone)]
pub(super) enum Frame {
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    ProcedureBoundary,
    And {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Or {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    If {
        consequent: Expr,
        alternate: Option<Expr>,
        env: EnvRef,
    },
    CondClause {
        action: CondAction,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    CondArrow {
        test_value: Value,
        pos: Position,
    },
    ApplyHead {
        args: Vec<Expr>,
        env: EnvRef,
        pos: Position,
    },
    ApplyArgs {
        procedure: Value,
        evaluated: Vec<EvaluatedArg>,
        remaining: Vec<Expr>,
        env: EnvRef,
        pos: Position,
        current_arg_pos: Position,
    },
    CallWithValues {
        consumer: Value,
        pos: Position,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        name: String,
        pos: Position,
        env: EnvRef,
    },
    ExceptionHandlerMarker {
        handler: Value,
    },
    ApplyExceptionHandler {
        handler: Value,
        raise_pos: Position,
    },
    UncaughtException {
        pos: Position,
    },
    GuardClause {
        exception: Value,
        raise_pos: Position,
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DynamicWindEnter {
        wind: Rc<DynamicWind>,
        body: Value,
    },
    DynamicWindBody {
        wind: Rc<DynamicWind>,
    },
    DynamicWindFinish {
        value: Value,
    },
    DynamicWindMarker {
        wind: Rc<DynamicWind>,
    },
    CallCcReturn {
        yield_cont: ContinuationRef,
    },
    ContinuationTransfer {
        remaining: Vec<WindTransferStep>,
        value: Value,
        target: ContinuationRef,
    },
}

pub(super) enum TailEvalResult {
    Value(Value),
    Call {
        procedure: Value,
        args: Vec<EvaluatedArg>,
        pos: Position,
    },
}

impl EvaluatedArg {
    pub(super) fn as_int(&self) -> Result<i64, EvalError> {
        self.value
            .as_int()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_number(&self) -> Result<Number, EvalError> {
        self.value
            .as_number()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_list(&self) -> Result<Vec<Value>, EvalError> {
        self.value
            .as_list()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_string(&self) -> Result<SchemeString, EvalError> {
        self.value
            .as_string()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_symbol(&self) -> Result<&str, EvalError> {
        self.value
            .as_symbol()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_char(&self) -> Result<char, EvalError> {
        self.value
            .as_char()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_vector(&self) -> Result<Rc<VectorValue>, EvalError> {
        self.value
            .as_vector()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    pub(super) fn as_syntax(&self) -> Result<SyntaxRef, EvalError> {
        self.value
            .as_syntax()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }
}

#[derive(Clone)]
pub(super) enum Value {
    Bool(bool),
    Number(Number),
    Char(char),
    String(SchemeString),
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<PairValue>),
    Vector(Rc<VectorValue>),
    Record(Rc<RecordValue>),
    Procedure(Rc<Procedure>),
    Syntax(SyntaxRef),
    Values(Vec<Value>),
    Uninitialized,
    Void,
}

#[derive(Clone)]
pub(super) struct SyntaxObject {
    pub(super) expr: Expr,
}

pub(super) struct PairValue {
    pub(super) head: RefCell<Value>,
    pub(super) tail: RefCell<Value>,
}

pub(super) struct VectorValue {
    pub(super) elements: RefCell<Vec<Value>>,
}

pub(super) struct RecordType {
    pub(super) name: String,
}

pub(super) struct RecordValue {
    pub(super) record_type: Rc<RecordType>,
    pub(super) fields: RefCell<Vec<Value>>,
}

#[derive(Clone)]
pub(super) struct RecordFieldSpec {
    pub(super) field_name: String,
    pub(super) accessor_name: String,
    pub(super) mutator_name: Option<String>,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.render())
    }
}

#[derive(Clone)]
pub(super) enum Procedure {
    Builtin {
        name: &'static str,
        func: BuiltinFn,
    },
    Error {
        name: &'static str,
    },
    Raise {
        name: &'static str,
    },
    WithExceptionHandler {
        name: &'static str,
    },
    ContinuationCapture {
        name: &'static str,
    },
    DynamicWind {
        name: &'static str,
    },
    CallWithValues {
        name: &'static str,
    },
    Continuation {
        cont: ContinuationRef,
    },
    GuardHandler {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
    },
    Lambda {
        params: LambdaParams,
        body: Vec<Expr>,
        env: EnvRef,
    },
    CaseLambda {
        clauses: Vec<CaseLambdaClause>,
        env: EnvRef,
    },
    RecordConstructor {
        name: String,
        record_type: Rc<RecordType>,
        field_count: usize,
    },
    RecordPredicate {
        name: String,
        record_type: Rc<RecordType>,
    },
    RecordAccessor {
        name: String,
        record_type: Rc<RecordType>,
        field_index: usize,
    },
    RecordMutator {
        name: String,
        record_type: Rc<RecordType>,
        field_index: usize,
    },
}

#[derive(Clone)]
pub(super) enum MacroTransformer {
    SyntaxRules {
        literals: HashSet<String>,
        rules: Vec<MacroRule>,
        def_env: EnvRef,
    },
    Procedure {
        procedure: Value,
        def_env: EnvRef,
    },
}

#[derive(Clone)]
pub(super) struct MacroRule {
    pub(super) pattern_items: Vec<Expr>,
    pub(super) template: Expr,
}

#[derive(Clone)]
pub(super) enum MacroBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

#[derive(Clone)]
pub(super) struct MacroExpansionContext {
    pub(super) bindings: HashMap<String, MacroBinding>,
    pub(super) macro_env: EnvRef,
    pub(super) def_env: EnvRef,
    pub(super) free_names: HashMap<String, String>,
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Builtin { name, .. } => write!(f, "#<builtin:{name}>"),
            Self::Error { name } => write!(f, "#<builtin:{name}>"),
            Self::Raise { name } => write!(f, "#<builtin:{name}>"),
            Self::WithExceptionHandler { name } => write!(f, "#<builtin:{name}>"),
            Self::ContinuationCapture { name } => write!(f, "#<builtin:{name}>"),
            Self::DynamicWind { name } => write!(f, "#<builtin:{name}>"),
            Self::CallWithValues { name } => write!(f, "#<builtin:{name}>"),
            Self::Continuation { .. } => f.write_str("#<continuation>"),
            Self::GuardHandler { .. } => f.write_str("#<guard-handler>"),
            Self::Lambda { .. } => f.write_str("#<lambda>"),
            Self::CaseLambda { .. } => f.write_str("#<case-lambda>"),
            Self::RecordConstructor { name, .. } => write!(f, "#<record-constructor:{name}>"),
            Self::RecordPredicate { name, .. } => write!(f, "#<record-predicate:{name}>"),
            Self::RecordAccessor { name, .. } => write!(f, "#<record-accessor:{name}>"),
            Self::RecordMutator { name, .. } => write!(f, "#<record-mutator:{name}>"),
        }
    }
}

pub(super) struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, BindingRef>>,
    macros: RefCell<HashMap<String, MacroRef>>,
    syntax_context: RefCell<Option<SyntaxContextRef>>,
}

impl Environment {
    pub(super) fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            syntax_context: RefCell::new(
                parent.as_ref().and_then(|parent| parent.syntax_context()),
            ),
            parent,
            bindings: RefCell::new(HashMap::new()),
            macros: RefCell::new(HashMap::new()),
        })
    }

    pub(super) fn define(&self, name: impl Into<String>, value: Value) {
        let name = name.into();
        let mut bindings = self.bindings.borrow_mut();
        if let Some(binding) = bindings.get(&name) {
            *binding.borrow_mut() = value;
            return;
        }

        bindings.insert(name, Rc::new(RefCell::new(value)));
    }

    pub(super) fn define_alias(&self, name: impl Into<String>, binding: BindingRef) {
        self.bindings.borrow_mut().insert(name.into(), binding);
    }

    pub(super) fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_binding(name)
            .map(|binding| binding.borrow().clone())
    }

    pub(super) fn define_macro(&self, name: impl Into<String>, transformer: MacroRef) {
        self.macros.borrow_mut().insert(name.into(), transformer);
    }

    pub(super) fn define_macro_alias(&self, name: impl Into<String>, transformer: MacroRef) {
        self.macros.borrow_mut().insert(name.into(), transformer);
    }

    pub(super) fn set_syntax_context(&self, context: Option<SyntaxContextRef>) {
        *self.syntax_context.borrow_mut() = context;
    }

    pub(super) fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        let Some(binding) = self.lookup_binding(name) else {
            return Err(EvalError::UnboundSymbol {
                name: name.to_string(),
            });
        };

        *binding.borrow_mut() = value;
        Ok(())
    }

    pub(super) fn lookup_binding(&self, name: &str) -> Option<BindingRef> {
        if let Some(binding) = self.bindings.borrow().get(name).cloned() {
            return Some(binding);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_binding(name))
    }

    pub(super) fn lookup_macro(&self, name: &str) -> Option<MacroRef> {
        if let Some(transformer) = self.macros.borrow().get(name).cloned() {
            return Some(transformer);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_macro(name))
    }

    pub(super) fn syntax_context(&self) -> Option<SyntaxContextRef> {
        self.syntax_context.borrow().clone()
    }
}

impl Value {
    pub(super) fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    pub(super) fn as_int(&self) -> Result<i64, EvalError> {
        match self {
            Self::Number(value) => value.as_exact_i64().ok_or_else(|| EvalError::TypeMismatch {
                expected: "number",
                found: self.render(),
            }),
            _ => Err(EvalError::TypeMismatch {
                expected: "number",
                found: self.render(),
            }),
        }
    }

    pub(super) fn as_number(&self) -> Result<Number, EvalError> {
        match self {
            Self::Number(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                expected: "number",
                found: self.render(),
            }),
        }
    }

    pub(super) fn as_list(&self) -> Result<Vec<Value>, EvalError> {
        collect_proper_list(self).ok_or_else(|| EvalError::TypeMismatch {
            expected: "list",
            found: self.render(),
        })
    }

    pub(super) fn as_string(&self) -> Result<SchemeString, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            _ => Err(EvalError::TypeMismatch {
                expected: "string",
                found: self.render(),
            }),
        }
    }

    pub(super) fn as_symbol(&self) -> Result<&str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            _ => Err(EvalError::TypeMismatch {
                expected: "symbol",
                found: self.render(),
            }),
        }
    }

    pub(super) fn as_char(&self) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                expected: "char",
                found: self.render(),
            }),
        }
    }

    pub(super) fn as_vector(&self) -> Result<Rc<VectorValue>, EvalError> {
        match self {
            Self::Vector(value) => Ok(value.clone()),
            _ => Err(EvalError::TypeMismatch {
                expected: "vector",
                found: self.render(),
            }),
        }
    }

    pub(super) fn as_syntax(&self) -> Result<SyntaxRef, EvalError> {
        match self {
            Self::Syntax(value) => Ok(value.clone()),
            _ => Err(EvalError::TypeMismatch {
                expected: "syntax object",
                found: self.render(),
            }),
        }
    }

    pub(super) fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    pub(super) fn display(&self) -> String {
        render_value(self, RenderMode::Display)
    }
}

pub(super) fn unpack_values(value: Value) -> Vec<Value> {
    match value {
        Value::Values(values) => values,
        value => vec![value],
    }
}

pub(super) fn pack_values(values: impl IntoIterator<Item = Value>) -> Value {
    let mut values = values.into_iter();
    let Some(first) = values.next() else {
        return Value::Values(Vec::new());
    };
    let Some(second) = values.next() else {
        return first;
    };

    let mut packed = vec![first, second];
    packed.extend(values);
    Value::Values(packed)
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    let mut active_pairs = HashSet::new();
    let mut active_vectors = HashSet::new();
    render_value_with_state(value, mode, &mut active_pairs, &mut active_vectors)
}

fn render_value_with_state(
    value: &Value,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    match value {
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::Number(value) => value.render(),
        Value::Char(value) => match mode {
            RenderMode::Write => render_char(*value),
            RenderMode::Display => value.to_string(),
        },
        Value::String(value) => match mode {
            RenderMode::Write => format!("\"{}\"", escape_string(&value.to_plain_string())),
            RenderMode::Display => value.to_plain_string(),
        },
        Value::Symbol(value) => value.clone(),
        Value::List(items) => render_list(items, mode, active_pairs, active_vectors),
        Value::Pair(pair) => {
            let id = pair_id(pair);
            if !active_pairs.insert(id) {
                return "#<circular>".to_string();
            }

            let rendered = render_pair(pair, mode, active_pairs, active_vectors);
            active_pairs.remove(&id);
            rendered
        }
        Value::Vector(vector) => {
            let id = vector_id(vector);
            if !active_vectors.insert(id) {
                return "#<circular>".to_string();
            }

            let rendered = render_vector(vector, mode, active_pairs, active_vectors);
            active_vectors.remove(&id);
            rendered
        }
        Value::Record(record) => format!("#<record:{}>", record.record_type.name),
        Value::Procedure(_) => "#<procedure>".to_string(),
        Value::Syntax(_) => "#<syntax>".to_string(),
        Value::Values(_) => "#<values>".to_string(),
        Value::Uninitialized => "#<uninitialized>".to_string(),
        Value::Void => "#<void>".to_string(),
    }
}

fn render_list(
    items: &[Value],
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let rendered = items
        .iter()
        .map(|value| render_value_with_state(value, mode, active_pairs, active_vectors))
        .collect::<Vec<_>>()
        .join(" ");
    format!("({rendered})")
}

fn render_pair(
    pair: &Rc<PairValue>,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let mut rendered = Vec::new();
    let mut current = pair.clone();
    let mut nested_pair_ids = Vec::new();

    loop {
        rendered.push(render_value_with_state(
            &current.head.borrow(),
            mode,
            active_pairs,
            active_vectors,
        ));
        let tail = current.tail.borrow().clone();

        match tail {
            Value::List(items) => {
                rendered.extend(items.iter().map(|value| {
                    render_value_with_state(value, mode, active_pairs, active_vectors)
                }));
                for id in nested_pair_ids {
                    active_pairs.remove(&id);
                }
                return format!("({})", rendered.join(" "));
            }
            Value::Pair(next) => {
                let id = pair_id(&next);
                if !active_pairs.insert(id) {
                    for id in nested_pair_ids {
                        active_pairs.remove(&id);
                    }
                    return format!("({} . #<circular>)", rendered.join(" "));
                }

                nested_pair_ids.push(id);
                current = next;
            }
            value => {
                let tail_rendered =
                    render_value_with_state(&value, mode, active_pairs, active_vectors);
                for id in nested_pair_ids {
                    active_pairs.remove(&id);
                }
                return format!("({} . {tail_rendered})", rendered.join(" "));
            }
        }
    }
}

fn render_vector(
    vector: &VectorValue,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let rendered = vector
        .elements
        .borrow()
        .iter()
        .map(|value| render_value_with_state(value, mode, active_pairs, active_vectors))
        .collect::<Vec<_>>()
        .join(" ");
    format!("#({rendered})")
}

pub(super) fn values_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            (left.is_empty() && right.is_empty())
                || (left.len() == right.len() && left.as_ptr() == right.as_ptr())
        }
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Syntax(left), Value::Syntax(right)) => Rc::ptr_eq(left, right),
        (Value::Values(left), Value::Values(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| values_eq(left, right))
        }
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

pub(super) fn values_equal(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    let mut seen_vectors = HashSet::new();
    values_equal_with_state(left, right, &mut seen_pairs, &mut seen_vectors)
}

fn values_equal_with_state(
    left: &Value,
    right: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
    seen_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
    match (left, right) {
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left.iter().zip(right.iter()).all(|(left, right)| {
                    values_equal_with_state(left, right, seen_pairs, seen_vectors)
                })
        }
        (Value::List(left), other) => {
            proper_list_equals_value(left, other, seen_pairs, seen_vectors)
        }
        (other, Value::List(right)) => {
            proper_list_equals_value(right, other, seen_pairs, seen_vectors)
        }
        (Value::Pair(left), Value::Pair(right)) => {
            let key = (pair_id(left), pair_id(right));
            if !seen_pairs.insert(key) {
                return true;
            }

            values_equal_with_state(
                &left.head.borrow(),
                &right.head.borrow(),
                seen_pairs,
                seen_vectors,
            ) && values_equal_with_state(
                &left.tail.borrow(),
                &right.tail.borrow(),
                seen_pairs,
                seen_vectors,
            )
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (vector_id(left), vector_id(right));
            if !seen_vectors.insert(key) {
                return true;
            }

            let left_items = left.elements.borrow();
            let right_items = right.elements.borrow();
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left, right)| {
                        values_equal_with_state(left, right, seen_pairs, seen_vectors)
                    })
        }
        (Value::Values(left), Value::Values(right)) => {
            left.len() == right.len()
                && left.iter().zip(right.iter()).all(|(left, right)| {
                    values_equal_with_state(left, right, seen_pairs, seen_vectors)
                })
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Syntax(left), Value::Syntax(right)) => Rc::ptr_eq(left, right),
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn proper_list_equals_value(
    items: &[Value],
    other: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
    seen_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
    let Ok(other_items) = other.as_list() else {
        return false;
    };

    items.len() == other_items.len()
        && items
            .iter()
            .zip(other_items.iter())
            .all(|(left, right)| values_equal_with_state(left, right, seen_pairs, seen_vectors))
}

fn collect_proper_list(value: &Value) -> Option<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut seen_pairs = HashSet::new();

    loop {
        match current {
            Value::List(tail_items) => {
                items.extend(tail_items);
                return Some(items);
            }
            Value::Pair(pair) => {
                if !seen_pairs.insert(pair_id(&pair)) {
                    return None;
                }

                items.push(pair.head.borrow().clone());
                current = pair.tail.borrow().clone();
            }
            _ => return None,
        }
    }
}

fn empty_list() -> Value {
    Value::List(Vec::new())
}

pub(super) fn list_from_values<I>(items: I) -> Value
where
    I: IntoIterator<Item = Value>,
{
    list_from_values_with_tail(items, empty_list())
}

pub(super) fn list_from_values_with_tail<I>(items: I, tail: Value) -> Value
where
    I: IntoIterator<Item = Value>,
{
    let mut tail = tail;
    let mut items = items.into_iter().collect::<Vec<_>>();
    while let Some(head) = items.pop() {
        tail = Value::Pair(Rc::new(PairValue {
            head: RefCell::new(head),
            tail: RefCell::new(tail),
        }));
    }
    tail
}

fn pair_id(pair: &Rc<PairValue>) -> usize {
    Rc::as_ptr(pair) as usize
}

fn vector_id(vector: &Rc<VectorValue>) -> usize {
    Rc::as_ptr(vector) as usize
}

pub(super) fn with_position<T>(
    result: Result<T, EvalError>,
    pos: Position,
) -> Result<T, EvalError> {
    result.map_err(|error| error.with_position(pos.line, pos.col))
}
