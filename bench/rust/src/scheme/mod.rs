use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub mod error;

pub use error::EvalError;

static NEXT_HYGIENE_ID: AtomicUsize = AtomicUsize::new(0);
static NEXT_RECORD_TYPE_ID: AtomicUsize = AtomicUsize::new(0);
static NEXT_CONTINUATION_JUMP_ID: AtomicUsize = AtomicUsize::new(0);

std::thread_local! {
    static CONTINUATION_JUMPS: RefCell<HashMap<usize, PendingContinuationJump>> =
        RefCell::new(HashMap::new());
}

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }

    fn symbol(name: impl Into<String>, pos: SourcePos) -> Self {
        Self::new(ExprKind::Symbol(name.into()), pos)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Number(Number),
    Boolean(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SourcePos {
    offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Number {
    Exact { num: i64, den: i64 },
    Inexact(f64),
}

impl Number {
    fn integer(value: i64) -> Self {
        Self::Exact { num: value, den: 1 }
    }

    fn rational(num: i64, den: i64) -> Self {
        debug_assert_ne!(den, 0, "rational denominator must be non-zero");

        if num == 0 {
            return Self::integer(0);
        }

        let mut num = num;
        let mut den = den;
        if den < 0 {
            num = -num;
            den = -den;
        }

        let gcd = gcd_i64(num, den);
        Self::Exact {
            num: num / gcd,
            den: den / gcd,
        }
    }

    fn is_exact(self) -> bool {
        matches!(self, Self::Exact { .. })
    }

    fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    fn is_integer(self) -> bool {
        match self {
            Self::Exact { den, .. } => den == 1,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    fn is_rational(self) -> bool {
        match self {
            Self::Exact { .. } => true,
            Self::Inexact(value) => value.is_finite(),
        }
    }

    fn as_f64(self) -> f64 {
        match self {
            Self::Exact { num, den } => num as f64 / den as f64,
            Self::Inexact(value) => value,
        }
    }

    fn render(self) -> String {
        match self {
            Self::Exact { num, den: 1 } => num.to_string(),
            Self::Exact { num, den } => format!("{num}/{den}"),
            Self::Inexact(value) => render_inexact(value),
        }
    }

    fn negate(self) -> Self {
        match self {
            Self::Exact { num, den } => Self::rational(-num, den),
            Self::Inexact(value) => Self::Inexact(-value),
        }
    }

    fn abs(self) -> Self {
        match self {
            Self::Exact { num, den } => Self::rational(num.abs(), den),
            Self::Inexact(value) => Self::Inexact(value.abs()),
        }
    }

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Exact {
                    num: left_num,
                    den: left_den,
                },
                Self::Exact {
                    num: right_num,
                    den: right_den,
                },
            ) => Self::rational(
                left_num * right_den + right_num * left_den,
                left_den * right_den,
            ),
            _ => Self::Inexact(self.as_f64() + other.as_f64()),
        }
    }

    fn sub(self, other: Self) -> Self {
        self.add(other.negate())
    }

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Exact {
                    num: left_num,
                    den: left_den,
                },
                Self::Exact {
                    num: right_num,
                    den: right_den,
                },
            ) => Self::rational(left_num * right_num, left_den * right_den),
            _ => Self::Inexact(self.as_f64() * other.as_f64()),
        }
    }

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Exact {
                    num: left_num,
                    den: left_den,
                },
                Self::Exact {
                    num: right_num,
                    den: right_den,
                },
            ) => Self::rational(left_num * right_den, left_den * right_num),
            _ => Self::Inexact(self.as_f64() / other.as_f64()),
        }
    }

    fn is_zero(self) -> bool {
        match self {
            Self::Exact { num, .. } => num == 0,
            Self::Inexact(value) => value == 0.0,
        }
    }

    fn equals(self, other: Self) -> bool {
        match (self, other) {
            (
                Self::Exact {
                    num: left_num,
                    den: left_den,
                },
                Self::Exact {
                    num: right_num,
                    den: right_den,
                },
            ) => {
                (left_num as i128) * (right_den as i128) == (right_num as i128) * (left_den as i128)
            }
            _ => self.as_f64() == other.as_f64(),
        }
    }

    fn less_than(self, other: Self) -> bool {
        match (self, other) {
            (
                Self::Exact {
                    num: left_num,
                    den: left_den,
                },
                Self::Exact {
                    num: right_num,
                    den: right_den,
                },
            ) => {
                (left_num as i128) * (right_den as i128) < (right_num as i128) * (left_den as i128)
            }
            _ => self.as_f64() < other.as_f64(),
        }
    }

    fn less_equal(self, other: Self) -> bool {
        self.less_than(other) || self.equals(other)
    }

    fn greater_than(self, other: Self) -> bool {
        !self.less_equal(other)
    }

    fn exact_parts(self) -> Option<(i64, i64)> {
        match self {
            Self::Exact { num, den } => Some((num, den)),
            Self::Inexact(_) => None,
        }
    }

    fn exact_integer(self) -> Option<i64> {
        match self {
            Self::Exact { num, den: 1 } => Some(num),
            _ => None,
        }
    }
}

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Vector(SchemeVector),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Pair(SchemePair),
    Builtin(Builtin),
    NativeProcedure(NativeProcedure),
    Procedure(Rc<Closure>),
    CaseProcedure(Rc<CaseClosure>),
    Continuation(SchemeContinuation),
    Record(Rc<RecordValue>),
    Uninitialized(String),
    Void,
}

#[derive(Clone)]
struct RecordType {
    id: usize,
    name: String,
    fields: Vec<String>,
}

#[derive(Clone)]
struct RecordValue {
    record_type: Rc<RecordType>,
    fields: Vec<Value>,
}

#[derive(Clone)]
enum NativeProcedure {
    RecordConstructor {
        name: String,
        record_type: Rc<RecordType>,
        constructor_fields: Vec<usize>,
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
}

#[derive(Clone)]
struct SchemeString {
    inner: Rc<RefCell<StringCell>>,
}

#[derive(Clone)]
struct SchemeVector {
    inner: Rc<RefCell<Vec<Value>>>,
}

#[derive(Clone)]
struct SchemePair {
    inner: Rc<RefCell<PairCell>>,
}

type ContinuationFn = dyn Fn(Value) -> Result<Value, EvalError>;
type ContinuationRef = Rc<ContinuationFn>;
type ValuesContinuationFn = dyn Fn(Vec<Value>) -> Result<Value, EvalError>;
type ValuesContinuationRef = Rc<ValuesContinuationFn>;

#[derive(Clone)]
struct SchemeContinuation {
    inner: ContinuationRef,
}

struct PendingContinuationJump {
    continuation: ContinuationRef,
    value: Value,
}

struct StringCell {
    value: String,
    mutable: bool,
}

struct PairCell {
    car: Value,
    cdr: Value,
}

enum StringSetError {
    Immutable,
    IndexOutOfBounds { len: usize },
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn kind(&self) -> &'static str {
        match self {
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Vector(_) => "vector",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Pair(_) => "pair",
            Self::Builtin(_)
            | Self::NativeProcedure(_)
            | Self::Procedure(_)
            | Self::CaseProcedure(_)
            | Self::Continuation(_) => "procedure",
            Self::Record(_) => "record",
            Self::Uninitialized(_) => "uninitialized",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        self.render_with(RenderMode::Write)
    }

    fn render_display(&self) -> String {
        self.render_with(RenderMode::Display)
    }

    fn render_with(&self, mode: RenderMode) -> String {
        let mut active_pairs = HashSet::new();
        render_value(self, mode, &mut active_pairs)
    }
}

fn render_value(value: &Value, mode: RenderMode, active_pairs: &mut HashSet<usize>) -> String {
    match value {
        Value::Number(value) => value.render(),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::String(value) => {
            let value = value.as_string();
            match mode {
                RenderMode::Write => format!("{value:?}"),
                RenderMode::Display => value,
            }
        }
        Value::Vector(value) => render_vector(value, mode, active_pairs),
        Value::Char(value) => render_char(*value, mode),
        Value::Symbol(name) => name.clone(),
        Value::List(items) => render_list(items, mode, active_pairs),
        Value::Pair(pair) => render_pair(pair, mode, active_pairs),
        Value::Builtin(_)
        | Value::NativeProcedure(_)
        | Value::Procedure(_)
        | Value::CaseProcedure(_)
        | Value::Continuation(_) => "#<procedure>".into(),
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Uninitialized(name) => format!("#<uninitialized {name}>"),
        Value::Void => "#<void>".into(),
    }
}

impl SchemeContinuation {
    fn new(inner: ContinuationRef) -> Self {
        Self { inner }
    }

    fn invoke(&self, value: Value) -> Result<Value, EvalError> {
        (self.inner)(value)
    }

    fn id(&self) -> usize {
        Rc::as_ptr(&self.inner) as *const () as usize
    }
}

impl NativeProcedure {
    fn name(&self) -> &str {
        match self {
            Self::RecordConstructor { name, .. }
            | Self::RecordPredicate { name, .. }
            | Self::RecordAccessor { name, .. } => name,
        }
    }
}

impl SchemeString {
    fn new(value: impl Into<String>, mutable: bool) -> Self {
        Self {
            inner: Rc::new(RefCell::new(StringCell {
                value: value.into(),
                mutable,
            })),
        }
    }

    fn immutable(value: impl Into<String>) -> Self {
        Self::new(value, false)
    }

    fn copy(&self) -> Self {
        Self::new(self.as_string(), true)
    }

    fn as_string(&self) -> String {
        self.inner.borrow().value.clone()
    }

    fn set_char(&self, index: usize, ch: char) -> Result<(), StringSetError> {
        let mut string = self.inner.borrow_mut();
        if !string.mutable {
            return Err(StringSetError::Immutable);
        }

        let mut chars: Vec<char> = string.value.chars().collect();
        if index >= chars.len() {
            return Err(StringSetError::IndexOutOfBounds { len: chars.len() });
        }

        chars[index] = ch;
        string.value = chars.into_iter().collect();
        Ok(())
    }
}

impl SchemeVector {
    fn new(items: Vec<Value>) -> Self {
        Self {
            inner: Rc::new(RefCell::new(items)),
        }
    }

    fn len(&self) -> usize {
        self.inner.borrow().len()
    }

    fn items(&self) -> Vec<Value> {
        self.inner.borrow().clone()
    }

    fn get(&self, index: usize) -> Option<Value> {
        self.inner.borrow().get(index).cloned()
    }

    fn set(&self, index: usize, value: Value) -> Result<(), usize> {
        let mut items = self.inner.borrow_mut();
        let len = items.len();
        if index >= len {
            return Err(len);
        }

        items[index] = value;
        Ok(())
    }
}

impl SchemePair {
    fn new(car: Value, cdr: Value) -> Self {
        Self {
            inner: Rc::new(RefCell::new(PairCell { car, cdr })),
        }
    }

    fn car(&self) -> Value {
        self.inner.borrow().car.clone()
    }

    fn cdr(&self) -> Value {
        self.inner.borrow().cdr.clone()
    }

    fn parts(&self) -> (Value, Value) {
        let pair = self.inner.borrow();
        (pair.car.clone(), pair.cdr.clone())
    }

    fn set_car(&self, value: Value) {
        self.inner.borrow_mut().car = value;
    }

    fn set_cdr(&self, value: Value) {
        self.inner.borrow_mut().cdr = value;
    }

    fn id(&self) -> usize {
        Rc::as_ptr(&self.inner) as usize
    }
}

fn render_list(items: &[Value], mode: RenderMode, active_pairs: &mut HashSet<usize>) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value(item, mode, active_pairs));
    }

    rendered.push(')');
    rendered
}

fn render_vector(
    vector: &SchemeVector,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
) -> String {
    let items = vector.items();
    let mut rendered = String::from("#(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value(item, mode, active_pairs));
    }

    rendered.push(')');
    rendered
}

fn render_pair(pair: &SchemePair, mode: RenderMode, active_pairs: &mut HashSet<usize>) -> String {
    let id = pair.id();
    if !active_pairs.insert(id) {
        return "#<cycle>".into();
    }

    let (head, tail) = pair.parts();
    let mut rendered = String::from("(");
    render_pair_contents(&head, &tail, mode, &mut rendered, active_pairs);
    rendered.push(')');
    active_pairs.remove(&id);
    rendered
}

fn render_pair_contents(
    head: &Value,
    tail: &Value,
    mode: RenderMode,
    output: &mut String,
    active_pairs: &mut HashSet<usize>,
) {
    output.push_str(&render_value(head, mode, active_pairs));

    match tail {
        Value::List(items) => {
            for item in items {
                output.push(' ');
                output.push_str(&render_value(item, mode, active_pairs));
            }
        }
        Value::Pair(next_pair) => {
            let id = next_pair.id();
            if !active_pairs.insert(id) {
                output.push_str(" . #<cycle>");
                return;
            }

            let (next_head, next_tail) = next_pair.parts();
            output.push(' ');
            render_pair_contents(&next_head, &next_tail, mode, output, active_pairs);
            active_pairs.remove(&id);
        }
        other => {
            output.push_str(" . ");
            output.push_str(&render_value(other, mode, active_pairs));
        }
    }
}

fn render_char(value: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => value.to_string(),
        RenderMode::Write => match value {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            other => format!("#\\{other}"),
        },
    }
}

fn gcd_i64(left: i64, right: i64) -> i64 {
    let mut left = i128::from(left).abs();
    let mut right = i128::from(right).abs();

    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    left as i64
}

fn render_inexact(value: f64) -> String {
    let rendered = format!("{value:?}");
    if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}

fn parse_number_token(token: &str) -> Option<Number> {
    if let Ok(value) = token.parse::<i64>() {
        return Some(Number::integer(value));
    }

    if let Some(value) = parse_rational_token(token) {
        return Some(value);
    }

    parse_inexact_token(token).map(Number::Inexact)
}

fn parse_rational_token(token: &str) -> Option<Number> {
    let (numerator, denominator) = token.split_once('/')?;
    if numerator.is_empty() || denominator.is_empty() {
        return None;
    }

    let numerator = numerator.parse::<i64>().ok()?;
    let denominator = denominator.parse::<i64>().ok()?;
    if denominator == 0 {
        return None;
    }

    Some(Number::rational(numerator, denominator))
}

fn parse_inexact_token(token: &str) -> Option<f64> {
    if !(token.contains('.') || token.contains('e') || token.contains('E')) {
        return None;
    }

    token.parse::<f64>().ok()
}

fn parse_decimal_as_exact(token: &str) -> Option<Number> {
    let (mantissa, exponent) = if let Some(index) = token.find(|ch| matches!(ch, 'e' | 'E')) {
        (&token[..index], token[index + 1..].parse::<i32>().ok()?)
    } else {
        (token, 0)
    };

    let (negative, mantissa) = if let Some(rest) = mantissa.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = mantissa.strip_prefix('+') {
        (false, rest)
    } else {
        (false, mantissa)
    };

    let (integer_part, fractional_part) = match mantissa.split_once('.') {
        Some((integer_part, fractional_part)) => (integer_part, fractional_part),
        None => (mantissa, ""),
    };

    if (integer_part.is_empty() && fractional_part.is_empty())
        || !integer_part.chars().all(|ch| ch.is_ascii_digit())
        || !fractional_part.chars().all(|ch| ch.is_ascii_digit())
    {
        return None;
    }

    let digits = format!("{integer_part}{fractional_part}");
    if digits.is_empty() {
        return None;
    }

    let mut numerator = digits.parse::<i128>().ok()?;
    if negative {
        numerator = -numerator;
    }

    let scale = fractional_part.len() as i32 - exponent;
    let denominator = if scale > 0 {
        checked_pow10(scale as u32)?
    } else {
        1
    };

    if scale < 0 {
        numerator = numerator.checked_mul(checked_pow10((-scale) as u32)?)?;
    }

    Some(Number::rational(
        i64::try_from(numerator).ok()?,
        i64::try_from(denominator).ok()?,
    ))
}

fn checked_pow10(exponent: u32) -> Option<i128> {
    let mut value = 1_i128;
    for _ in 0..exponent {
        value = value.checked_mul(10)?;
    }
    Some(value)
}

enum ListAccessError {
    Improper { found: String },
    Circular,
}

fn make_proper_list(items: Vec<Value>) -> Value {
    items
        .into_iter()
        .rev()
        .fold(Value::List(Vec::new()), |tail, head| {
            Value::Pair(SchemePair::new(head, tail))
        })
}

fn is_empty_list(value: &Value) -> bool {
    matches!(value, Value::List(items) if items.is_empty())
}

fn collect_list_items(value: &Value) -> Result<Vec<Value>, ListAccessError> {
    match value {
        Value::List(items) => Ok(items.clone()),
        Value::Pair(pair) => {
            let mut items = Vec::new();
            let mut current = Value::Pair(pair.clone());
            let mut seen = HashSet::new();

            loop {
                match current {
                    Value::List(rest) => {
                        items.extend(rest);
                        return Ok(items);
                    }
                    Value::Pair(pair) => {
                        let id = pair.id();
                        if !seen.insert(id) {
                            return Err(ListAccessError::Circular);
                        }

                        let (car, cdr) = pair.parts();
                        items.push(car);
                        current = cdr;
                    }
                    other => {
                        return Err(ListAccessError::Improper {
                            found: other.kind().into(),
                        })
                    }
                }
            }
        }
        other => Err(ListAccessError::Improper {
            found: other.kind().into(),
        }),
    }
}

fn list_tail_value(value: &Value, index: usize) -> Result<Value, ListAccessError> {
    match value {
        Value::List(items) => {
            if index <= items.len() {
                Ok(make_proper_list(items[index..].to_vec()))
            } else {
                Err(ListAccessError::Improper {
                    found: "list".into(),
                })
            }
        }
        Value::Pair(pair) => {
            let mut current = Value::Pair(pair.clone());
            let mut seen = HashSet::new();

            for _ in 0..index {
                match current {
                    Value::Pair(pair) => {
                        let id = pair.id();
                        if !seen.insert(id) {
                            return Err(ListAccessError::Circular);
                        }

                        current = pair.cdr();
                    }
                    Value::List(items) => {
                        return Err(ListAccessError::Improper {
                            found: if items.is_empty() {
                                "list".into()
                            } else {
                                "pair".into()
                            },
                        })
                    }
                    other => {
                        return Err(ListAccessError::Improper {
                            found: other.kind().into(),
                        })
                    }
                }
            }

            match current {
                Value::List(_) | Value::Pair(_) => Ok(current),
                other => Err(ListAccessError::Improper {
                    found: other.kind().into(),
                }),
            }
        }
        other => Err(ListAccessError::Improper {
            found: other.kind().into(),
        }),
    }
}

fn is_proper_list(value: &Value) -> bool {
    collect_list_items(value).is_ok()
}

fn list_access_error(error: ListAccessError, pos: SourcePos) -> EvalError {
    match error {
        ListAccessError::Improper { found } => EvalError::TypeMismatch {
            expected: "list".into(),
            found,
        }
        .with_offset(pos.offset),
        ListAccessError::Circular => EvalError::CyclicList.with_offset(pos.offset),
    }
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    GreaterEqual,
    Equal,
    Eq,
    Eqv,
    EqualDeep,
    LessEqual,
    Not,
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
    Gcd,
    Lcm,
    Truncate,
    Round,
    Cons,
    Car,
    Cdr,
    Caar,
    Cadr,
    Cdar,
    Cddr,
    SetCar,
    SetCdr,
    NullPred,
    List,
    ListRef,
    ListTail,
    ListPred,
    Length,
    Append,
    Reverse,
    Assoc,
    Assv,
    Member,
    Map,
    ForEach,
    StringPred,
    NumberPred,
    ExactPred,
    InexactPred,
    IntegerPred,
    RationalPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    ProcedurePred,
    Display,
    Write,
    Newline,
    StringCopy,
    MakeString,
    String,
    StringAppend,
    StringLength,
    StringSet,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringToList,
    ListToString,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLess,
    CharToInteger,
    IntegerToChar,
    StringEqual,
    StringLess,
    StringGreater,
    StringLessEqual,
    StringGreaterEqual,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
    CallCc,
    Apply,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::GreaterEqual => ">=",
            Self::Equal => "=",
            Self::Eq => "eq?",
            Self::Eqv => "eqv?",
            Self::EqualDeep => "equal?",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Abs => "abs",
            Self::Modulo => "modulo",
            Self::Remainder => "remainder",
            Self::Quotient => "quotient",
            Self::Min => "min",
            Self::Max => "max",
            Self::Expt => "expt",
            Self::ZeroPred => "zero?",
            Self::PositivePred => "positive?",
            Self::NegativePred => "negative?",
            Self::OddPred => "odd?",
            Self::EvenPred => "even?",
            Self::Gcd => "gcd",
            Self::Lcm => "lcm",
            Self::Truncate => "truncate",
            Self::Round => "round",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::Caar => "caar",
            Self::Cadr => "cadr",
            Self::Cdar => "cdar",
            Self::Cddr => "cddr",
            Self::SetCar => "set-car!",
            Self::SetCdr => "set-cdr!",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPred => "list?",
            Self::Length => "length",
            Self::Append => "append",
            Self::Reverse => "reverse",
            Self::Assoc => "assoc",
            Self::Assv => "assv",
            Self::Member => "member",
            Self::Map => "map",
            Self::ForEach => "for-each",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::ExactPred => "exact?",
            Self::InexactPred => "inexact?",
            Self::IntegerPred => "integer?",
            Self::RationalPred => "rational?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
            Self::ProcedurePred => "procedure?",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringCopy => "string-copy",
            Self::MakeString => "make-string",
            Self::String => "string",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::StringSet => "string-set!",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::StringToList => "string->list",
            Self::ListToString => "list->string",
            Self::Vector => "vector",
            Self::MakeVector => "make-vector",
            Self::VectorRef => "vector-ref",
            Self::VectorSet => "vector-set!",
            Self::VectorLength => "vector-length",
            Self::VectorPred => "vector?",
            Self::VectorToList => "vector->list",
            Self::ListToVector => "list->vector",
            Self::CharPred => "char?",
            Self::CharAlphabeticPred => "char-alphabetic?",
            Self::CharNumericPred => "char-numeric?",
            Self::CharUpcase => "char-upcase",
            Self::CharDowncase => "char-downcase",
            Self::CharEqual => "char=?",
            Self::CharLess => "char<?",
            Self::CharToInteger => "char->integer",
            Self::IntegerToChar => "integer->char",
            Self::StringEqual => "string=?",
            Self::StringLess => "string<?",
            Self::StringGreater => "string>?",
            Self::StringLessEqual => "string<=?",
            Self::StringGreaterEqual => "string>=?",
            Self::StringCiEqual => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::ExactToInexact => "exact->inexact",
            Self::InexactToExact => "inexact->exact",
            Self::Numerator => "numerator",
            Self::Denominator => "denominator",
            Self::CallCc => "call/cc",
            Self::Apply => "apply",
        }
    }
}

#[derive(Clone)]
struct ParameterSpec {
    required: Vec<String>,
    rest: Option<String>,
}

struct Closure {
    params: ParameterSpec,
    body: Rc<[Expr]>,
    env: EnvRef,
}

struct CaseClosure {
    clauses: Vec<Rc<Closure>>,
}

type EnvRef = Rc<Env>;

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    macros: RefCell<HashMap<String, Rc<SyntaxRulesMacro>>>,
    parent: Option<EnvRef>,
    output: Rc<RefCell<String>>,
}

impl Env {
    fn new(output: Rc<RefCell<String>>) -> EnvRef {
        let env = Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            macros: RefCell::new(HashMap::new()),
            parent: None,
            output,
        });

        for (name, builtin) in [
            ("+", Builtin::Add),
            ("-", Builtin::Sub),
            ("*", Builtin::Mul),
            ("/", Builtin::Div),
            ("<", Builtin::LessThan),
            (">", Builtin::GreaterThan),
            (">=", Builtin::GreaterEqual),
            ("=", Builtin::Equal),
            ("eq?", Builtin::Eq),
            ("eqv?", Builtin::Eqv),
            ("equal?", Builtin::EqualDeep),
            ("<=", Builtin::LessEqual),
            ("not", Builtin::Not),
            ("abs", Builtin::Abs),
            ("modulo", Builtin::Modulo),
            ("remainder", Builtin::Remainder),
            ("quotient", Builtin::Quotient),
            ("min", Builtin::Min),
            ("max", Builtin::Max),
            ("expt", Builtin::Expt),
            ("zero?", Builtin::ZeroPred),
            ("positive?", Builtin::PositivePred),
            ("negative?", Builtin::NegativePred),
            ("odd?", Builtin::OddPred),
            ("even?", Builtin::EvenPred),
            ("gcd", Builtin::Gcd),
            ("lcm", Builtin::Lcm),
            ("truncate", Builtin::Truncate),
            ("round", Builtin::Round),
            ("cons", Builtin::Cons),
            ("car", Builtin::Car),
            ("cdr", Builtin::Cdr),
            ("caar", Builtin::Caar),
            ("cadr", Builtin::Cadr),
            ("cdar", Builtin::Cdar),
            ("cddr", Builtin::Cddr),
            ("set-car!", Builtin::SetCar),
            ("set-cdr!", Builtin::SetCdr),
            ("null?", Builtin::NullPred),
            ("list", Builtin::List),
            ("list-ref", Builtin::ListRef),
            ("list-tail", Builtin::ListTail),
            ("list?", Builtin::ListPred),
            ("length", Builtin::Length),
            ("append", Builtin::Append),
            ("assoc", Builtin::Assoc),
            ("assv", Builtin::Assv),
            ("member", Builtin::Member),
            ("reverse", Builtin::Reverse),
            ("map", Builtin::Map),
            ("for-each", Builtin::ForEach),
            ("string?", Builtin::StringPred),
            ("number?", Builtin::NumberPred),
            ("exact?", Builtin::ExactPred),
            ("inexact?", Builtin::InexactPred),
            ("integer?", Builtin::IntegerPred),
            ("rational?", Builtin::RationalPred),
            ("boolean?", Builtin::BooleanPred),
            ("pair?", Builtin::PairPred),
            ("symbol?", Builtin::SymbolPred),
            ("procedure?", Builtin::ProcedurePred),
            ("display", Builtin::Display),
            ("write", Builtin::Write),
            ("newline", Builtin::Newline),
            ("string-copy", Builtin::StringCopy),
            ("make-string", Builtin::MakeString),
            ("string", Builtin::String),
            ("string-append", Builtin::StringAppend),
            ("string-length", Builtin::StringLength),
            ("string-set!", Builtin::StringSet),
            ("substring", Builtin::Substring),
            ("string->number", Builtin::StringToNumber),
            ("number->string", Builtin::NumberToString),
            ("symbol->string", Builtin::SymbolToString),
            ("string->symbol", Builtin::StringToSymbol),
            ("string-ref", Builtin::StringRef),
            ("string->list", Builtin::StringToList),
            ("list->string", Builtin::ListToString),
            ("vector", Builtin::Vector),
            ("make-vector", Builtin::MakeVector),
            ("vector-ref", Builtin::VectorRef),
            ("vector-set!", Builtin::VectorSet),
            ("vector-length", Builtin::VectorLength),
            ("vector?", Builtin::VectorPred),
            ("vector->list", Builtin::VectorToList),
            ("list->vector", Builtin::ListToVector),
            ("char?", Builtin::CharPred),
            ("char-alphabetic?", Builtin::CharAlphabeticPred),
            ("char-numeric?", Builtin::CharNumericPred),
            ("char-upcase", Builtin::CharUpcase),
            ("char-downcase", Builtin::CharDowncase),
            ("char=?", Builtin::CharEqual),
            ("char<?", Builtin::CharLess),
            ("char->integer", Builtin::CharToInteger),
            ("integer->char", Builtin::IntegerToChar),
            ("string=?", Builtin::StringEqual),
            ("string<?", Builtin::StringLess),
            ("string>?", Builtin::StringGreater),
            ("string<=?", Builtin::StringLessEqual),
            ("string>=?", Builtin::StringGreaterEqual),
            ("string-ci=?", Builtin::StringCiEqual),
            ("string-upcase", Builtin::StringUpcase),
            ("string-downcase", Builtin::StringDowncase),
            ("exact->inexact", Builtin::ExactToInexact),
            ("inexact->exact", Builtin::InexactToExact),
            ("numerator", Builtin::Numerator),
            ("denominator", Builtin::Denominator),
            ("call/cc", Builtin::CallCc),
            ("call-with-current-continuation", Builtin::CallCc),
            ("apply", Builtin::Apply),
        ] {
            env.define(name.into(), Value::Builtin(builtin));
        }

        env
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            macros: RefCell::new(HashMap::new()),
            parent: Some(parent.clone()),
            output: parent.output.clone(),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn define_macro(&self, name: String, transformer: Rc<SyntaxRulesMacro>) {
        self.macros.borrow_mut().insert(name, transformer);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name) {
            return Some(value.clone());
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }

    fn lookup_macro(&self, name: &str) -> Option<Rc<SyntaxRulesMacro>> {
        if let Some(transformer) = self.macros.borrow().get(name) {
            return Some(transformer.clone());
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_macro(name))
    }

    fn set(&self, name: &str, value: Value) -> bool {
        let mut bindings = self.bindings.borrow_mut();
        if let Some(binding) = bindings.get_mut(name) {
            *binding = value;
            return true;
        }
        drop(bindings);

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
    }

    fn write_output(&self, text: &str) {
        self.output.borrow_mut().push_str(text);
    }
}

#[derive(Clone)]
struct SyntaxRulesMacro {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvRef,
    id: usize,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

struct RecordConstructorSpec {
    name: String,
    fields: Vec<String>,
}

struct RecordFieldSpec {
    name: String,
    accessor_name: String,
}

#[derive(Default)]
struct MatchBindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

struct MacroExpansion {
    expr: Expr,
    aliases: Vec<(String, Value)>,
}

struct ExpandState<'a> {
    transformer: &'a SyntaxRulesMacro,
    bindings: &'a MatchBindings,
    alias_names: HashMap<String, String>,
    aliases: Vec<(String, Value)>,
}

fn next_hygiene_id() -> usize {
    NEXT_HYGIENE_ID.fetch_add(1, Ordering::Relaxed)
}

fn next_record_type_id() -> usize {
    NEXT_RECORD_TYPE_ID.fetch_add(1, Ordering::Relaxed)
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            Err(EvalError::EmptyProgram.with_offset(0))
        } else {
            Ok(expressions)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote(),
            Some(')') => Err(EvalError::UnexpectedCloseParen.with_offset(pos.offset)),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof.with_offset(pos.offset)),
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.next_char();
        Ok(Expr::new(
            ExprKind::List(vec![Expr::symbol("quote", pos), self.parse_expr()?]),
            pos,
        ))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.next_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.next_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof.with_offset(pos.offset)),
            }
        }

        Ok(Expr::new(ExprKind::List(items), pos))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.next_char();
        let mut value = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '"' => return Ok(Expr::new(ExprKind::String(value), pos)),
                '\\' => {
                    let escape_pos = self.current_pos();
                    let escaped = self
                        .next_char()
                        .ok_or_else(|| EvalError::UnexpectedEof.with_offset(escape_pos.offset))?;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => {
                            return Err(EvalError::InvalidEscape { escape: other }
                                .with_offset(escape_pos.offset))
                        }
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof.with_offset(pos.offset))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        let pos = SourcePos { offset: start };

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.next_char();
        }

        let token = &self.input[start..self.pos];

        match token {
            "#t" => Ok(Expr::new(ExprKind::Boolean(true), pos)),
            "#f" => Ok(Expr::new(ExprKind::Boolean(false), pos)),
            _ => {
                if let Some(value) = parse_char_literal(token, pos)? {
                    return Ok(Expr::new(ExprKind::Char(value), pos));
                }

                match parse_number_token(token) {
                    Some(value) => Ok(Expr::new(ExprKind::Number(value), pos)),
                    None => Ok(Expr::symbol(token, pos)),
                }
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.next_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.next_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos { offset: self.pos }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

fn parse_char_literal(token: &str, pos: SourcePos) -> Result<Option<char>, EvalError> {
    let Some(literal) = token.strip_prefix("#\\") else {
        return Ok(None);
    };

    let value = match literal {
        "space" => ' ',
        "newline" => '\n',
        "" => {
            return Err(EvalError::InvalidSyntax {
                message: "invalid character literal".into(),
            }
            .with_offset(pos.offset))
        }
        _ => {
            let mut chars = literal.chars();
            let ch = chars.next().unwrap();
            if chars.next().is_some() {
                return Err(EvalError::InvalidSyntax {
                    message: format!("invalid character literal: {token}"),
                }
                .with_offset(pos.offset));
            }
            ch
        }
    };

    Ok(Some(value))
}

fn eval_program(expressions: &[Expr], output: Rc<RefCell<String>>) -> Result<Value, EvalError> {
    if requires_cps_evaluator(expressions) {
        return eval_program_cps(expressions, output);
    }

    let env = Env::new(output);
    eval_sequence(expressions, &env)
}

fn requires_cps_evaluator(expressions: &[Expr]) -> bool {
    expressions.iter().any(expr_mentions_continuations)
}

fn expr_mentions_continuations(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Symbol(name) => {
            matches!(name.as_str(), "call/cc" | "call-with-current-continuation")
        }
        ExprKind::List(items) => items.iter().any(expr_mentions_continuations),
        _ => false,
    }
}

fn identity_continuation() -> ContinuationRef {
    Rc::new(|value| Ok(value))
}

fn queue_continuation_jump(continuation: ContinuationRef, value: Value) -> EvalError {
    let id = NEXT_CONTINUATION_JUMP_ID.fetch_add(1, Ordering::Relaxed);
    CONTINUATION_JUMPS.with(|jumps| {
        jumps.borrow_mut().insert(
            id,
            PendingContinuationJump {
                continuation,
                value,
            },
        );
    });
    EvalError::InternalContinuationJump { id }
}

fn take_continuation_jump(id: usize) -> Option<PendingContinuationJump> {
    CONTINUATION_JUMPS.with(|jumps| jumps.borrow_mut().remove(&id))
}

fn rc_exprs(expressions: Vec<Expr>) -> Rc<[Expr]> {
    Rc::from(expressions.into_boxed_slice())
}

fn rc_bindings(bindings: Vec<(String, Expr)>) -> Rc<[(String, Expr)]> {
    Rc::from(bindings.into_boxed_slice())
}

fn rc_do_bindings(bindings: Vec<DoBinding>) -> Rc<[DoBinding]> {
    Rc::from(bindings.into_boxed_slice())
}

fn rc_value_lists(lists: Vec<Vec<Value>>) -> Rc<[Vec<Value>]> {
    Rc::from(lists.into_boxed_slice())
}

fn eval_program_cps(expressions: &[Expr], output: Rc<RefCell<String>>) -> Result<Value, EvalError> {
    let env = Env::new(output);
    let mut result = eval_sequence_cps(
        rc_exprs(expressions.to_vec()),
        0,
        env,
        identity_continuation(),
    );

    loop {
        match result {
            Ok(value) => return Ok(value),
            Err(EvalError::InternalContinuationJump { id }) => {
                let PendingContinuationJump {
                    continuation,
                    value,
                } = take_continuation_jump(id)
                    .expect("continuation jump payload should be available");
                result = continuation(value);
            }
            Err(error) => return Err(error),
        }
    }
}

fn eval_sequence_cps(
    expressions: Rc<[Expr]>,
    index: usize,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= expressions.len() {
        return k(Value::Void);
    }

    if index + 1 == expressions.len() {
        return eval_expr_cps(expressions[index].clone(), env, k);
    }

    let next_expressions = expressions.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        expressions[index].clone(),
        env,
        Rc::new(move |_| {
            eval_sequence_cps(
                next_expressions.clone(),
                index + 1,
                next_env.clone(),
                next_k.clone(),
            )
        }),
    )
}

fn eval_expr_cps(expr: Expr, env: EnvRef, k: ContinuationRef) -> Result<Value, EvalError> {
    let pos = expr.pos;

    match expr.kind {
        ExprKind::Number(value) => k(Value::Number(value)),
        ExprKind::Boolean(value) => k(Value::Boolean(value)),
        ExprKind::String(value) => k(Value::String(SchemeString::immutable(value))),
        ExprKind::Char(value) => k(Value::Char(value)),
        ExprKind::Symbol(name) => match env.lookup(&name) {
            Some(Value::Uninitialized(_)) => {
                Err(EvalError::UninitializedBinding { name }.with_offset(pos.offset))
            }
            Some(value) => k(value),
            None => Err(EvalError::UnboundVariable { name }.with_offset(pos.offset)),
        },
        ExprKind::List(items) => eval_list_cps(pos, items, env, k),
    }
}

fn eval_list_cps(
    list_pos: SourcePos,
    items: Vec<Expr>,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::EmptyList.with_offset(list_pos.offset));
    };

    let operator = operator.clone();
    let arguments = arguments.to_vec();

    if let ExprKind::Symbol(name) = &operator.kind {
        match name.as_str() {
            "define" => return eval_define_cps(operator.pos, &arguments, env, k),
            "define-record-type" => {
                return k(eval_define_record_type(operator.pos, &arguments, &env)?)
            }
            "define-syntax" => return k(eval_define_syntax(operator.pos, &arguments, &env)?),
            "set!" => return eval_set_cps(operator.pos, &arguments, env, k),
            "if" => return eval_if_cps(operator.pos, &arguments, env, k),
            "quote" => return k(eval_quote(operator.pos, &arguments)?),
            "lambda" => return k(eval_lambda(operator.pos, &arguments, &env)?),
            "case-lambda" => return k(eval_case_lambda(&arguments, &env)?),
            "and" => return eval_and_cps(&arguments, env, k),
            "or" => return eval_or_cps(&arguments, env, k),
            "begin" => return eval_sequence_cps(rc_exprs(arguments), 0, env, k),
            "let" => return eval_let_cps(operator.pos, &arguments, env, k),
            "let*" => return eval_let_star_cps(operator.pos, &arguments, env, k),
            "letrec" => return eval_letrec_cps(operator.pos, &arguments, env, false, k),
            "letrec*" => return eval_letrec_cps(operator.pos, &arguments, env, true, k),
            "cond" => return eval_cond_cps(&arguments, env, k),
            "case" => return eval_case_cps(operator.pos, &arguments, env, k),
            "do" => return eval_do_cps(operator.pos, &arguments, env, k),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let expansion = expand_macro_invocation(
                transformer.as_ref(),
                &Expr::new(ExprKind::List(items), list_pos),
            )?;
            let macro_env = Env::child(&env);

            for (alias, value) in expansion.aliases {
                macro_env.define(alias, value);
            }

            return eval_expr_cps(expansion.expr, macro_env, k);
        }
    }

    let call_pos = operator.pos;
    eval_application_cps(operator, rc_exprs(arguments), env, call_pos, k)
}

fn eval_application_cps(
    operator: Expr,
    arguments: Rc<[Expr]>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let next_arguments = arguments.clone();
    let next_env = env.clone();
    let next_k = k.clone();

    eval_expr_cps(
        operator,
        env,
        Rc::new(move |procedure| {
            let apply_env = next_env.clone();
            let apply_k = next_k.clone();
            eval_args_cps(
                next_arguments.clone(),
                next_arguments.len(),
                next_env.clone(),
                Vec::new(),
                Rc::new(move |argument_values| {
                    apply_value_cps(
                        procedure.clone(),
                        argument_values,
                        apply_env.clone(),
                        call_pos,
                        apply_k.clone(),
                    )
                }),
            )
        }),
    )
}

fn eval_args_cps(
    arguments: Rc<[Expr]>,
    next_index: usize,
    env: EnvRef,
    evaluated: Vec<Value>,
    k: ValuesContinuationRef,
) -> Result<Value, EvalError> {
    if next_index == 0 {
        return k(evaluated);
    }

    let index = next_index - 1;
    let next_arguments = arguments.clone();
    let next_env = env.clone();
    let next_k = k.clone();

    eval_expr_cps(
        arguments[index].clone(),
        env,
        Rc::new(move |value| {
            let mut next_values = evaluated.clone();
            next_values.insert(0, value);
            eval_args_cps(
                next_arguments.clone(),
                index,
                next_env.clone(),
                next_values,
                next_k.clone(),
            )
        }),
    )
}

fn eval_define_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "define".into(),
            expected: "at least 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    }

    match &arguments[0].kind {
        ExprKind::Symbol(name) => {
            if arguments.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define".into(),
                    expected: "exactly 2".into(),
                    got: arguments.len(),
                }
                .with_offset(pos.offset));
            }

            let define_env = env.clone();
            let define_name = name.clone();
            let define_k = k.clone();
            eval_expr_cps(
                arguments[1].clone(),
                env,
                Rc::new(move |value| {
                    define_env.define(define_name.clone(), value);
                    define_k.clone()(Value::Void)
                }),
            )
        }
        ExprKind::List(signature) => {
            let (name_expr, params) = signature.split_first().ok_or_else(|| {
                EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                }
                .with_offset(arguments[0].pos.offset)
            })?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                }
                .with_offset(name_expr.pos.offset));
            };

            env.define(
                name.clone(),
                Value::Procedure(Rc::new(Closure {
                    params: parse_params(params)?,
                    body: rc_exprs(arguments[1..].to_vec()),
                    env: env.clone(),
                })),
            );
            k(Value::Void)
        }
        _ => Err(EvalError::InvalidSyntax {
            message: "define: expected symbol or function signature".into(),
        }
        .with_offset(arguments[0].pos.offset)),
    }
}

fn eval_set_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let [name_expr, value_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "set!".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "set!: expected symbol".into(),
        }
        .with_offset(name_expr.pos.offset));
    };

    let set_env = env.clone();
    let set_name = name.clone();
    let set_k = k.clone();
    let name_pos = name_expr.pos;
    eval_expr_cps(
        value_expr.clone(),
        env,
        Rc::new(move |value| {
            if set_env.set(&set_name, value) {
                set_k.clone()(Value::Void)
            } else {
                Err(EvalError::UnboundVariable {
                    name: set_name.clone(),
                }
                .with_offset(name_pos.offset))
            }
        }),
    )
}

fn eval_if_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    match arguments {
        [condition, consequent] => {
            let consequent = consequent.clone();
            let branch_env = env.clone();
            let branch_k = k.clone();
            eval_expr_cps(
                condition.clone(),
                env,
                Rc::new(move |value| {
                    if value.is_truthy() {
                        eval_expr_cps(consequent.clone(), branch_env.clone(), branch_k.clone())
                    } else {
                        branch_k.clone()(Value::Boolean(false))
                    }
                }),
            )
        }
        [condition, consequent, alternate] => {
            let consequent = consequent.clone();
            let alternate = alternate.clone();
            let branch_env = env.clone();
            let branch_k = k.clone();
            eval_expr_cps(
                condition.clone(),
                env,
                Rc::new(move |value| {
                    let next = if value.is_truthy() {
                        consequent.clone()
                    } else {
                        alternate.clone()
                    };
                    eval_expr_cps(next, branch_env.clone(), branch_k.clone())
                }),
            )
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset)),
    }
}

fn eval_and_cps(arguments: &[Expr], env: EnvRef, k: ContinuationRef) -> Result<Value, EvalError> {
    eval_and_from_cps(rc_exprs(arguments.to_vec()), 0, env, k)
}

fn eval_and_from_cps(
    arguments: Rc<[Expr]>,
    index: usize,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= arguments.len() {
        return k(Value::Boolean(true));
    }

    let is_last = index + 1 == arguments.len();
    let next_arguments = arguments.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        arguments[index].clone(),
        env,
        Rc::new(move |value| {
            if !value.is_truthy() || is_last {
                next_k.clone()(value)
            } else {
                eval_and_from_cps(
                    next_arguments.clone(),
                    index + 1,
                    next_env.clone(),
                    next_k.clone(),
                )
            }
        }),
    )
}

fn eval_or_cps(arguments: &[Expr], env: EnvRef, k: ContinuationRef) -> Result<Value, EvalError> {
    eval_or_from_cps(rc_exprs(arguments.to_vec()), 0, env, k)
}

fn eval_or_from_cps(
    arguments: Rc<[Expr]>,
    index: usize,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= arguments.len() {
        return k(Value::Boolean(false));
    }

    let is_last = index + 1 == arguments.len();
    let next_arguments = arguments.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        arguments[index].clone(),
        env,
        Rc::new(move |value| {
            if value.is_truthy() || is_last {
                next_k.clone()(value)
            } else {
                eval_or_from_cps(
                    next_arguments.clone(),
                    index + 1,
                    next_env.clone(),
                    next_k.clone(),
                )
            }
        }),
    )
}

fn eval_let_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((first, rest)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let_cps(name.clone(), rest.to_vec(), env, pos, k),
        _ => eval_plain_let_cps(first.clone(), rest.to_vec(), env, k),
    }
}

fn eval_plain_let_cps(
    bindings_expr: Expr,
    body: Vec<Expr>,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(bindings_expr.pos.offset));
    }

    let bindings = rc_bindings(parse_bindings(&bindings_expr, "let")?);
    let body = rc_exprs(body);
    let let_env = env.clone();
    let let_k = k.clone();
    eval_binding_values_cps(
        bindings.clone(),
        0,
        env,
        Vec::new(),
        Rc::new(move |values| {
            let frame_env = Env::child(&let_env);
            for ((name, _), value) in bindings.iter().zip(values) {
                frame_env.define(name.clone(), value);
            }
            eval_sequence_cps(body.clone(), 0, frame_env, let_k.clone())
        }),
    )
}

fn eval_let_star_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let*".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let*".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    let bindings = rc_bindings(parse_bindings(bindings_expr, "let*")?);
    let let_env = Env::child(&env);
    eval_let_star_bindings_cps(bindings, 0, let_env.clone(), rc_exprs(body.to_vec()), k)
}

fn eval_let_star_bindings_cps(
    bindings: Rc<[(String, Expr)]>,
    index: usize,
    env: EnvRef,
    body: Rc<[Expr]>,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= bindings.len() {
        return eval_sequence_cps(body, 0, env, k);
    }

    let (name, value_expr) = &bindings[index];
    let next_name = name.clone();
    let next_expr = value_expr.clone();
    let next_bindings = bindings.clone();
    let next_env = env.clone();
    let next_body = body.clone();
    let next_k = k.clone();
    eval_expr_cps(
        next_expr,
        env.clone(),
        Rc::new(move |value| {
            next_env.define(next_name.clone(), value);
            eval_let_star_bindings_cps(
                next_bindings.clone(),
                index + 1,
                next_env.clone(),
                next_body.clone(),
                next_k.clone(),
            )
        }),
    )
}

fn eval_named_let_cps(
    name: String,
    arguments: Vec<Expr>,
    env: EnvRef,
    pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 2,
        }
        .with_offset(pos.offset));
    }

    let bindings = rc_bindings(parse_bindings(bindings_expr, "let")?);
    let named_env = Env::child(&env);
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect::<Vec<_>>();
    let closure = Rc::new(Closure {
        params: ParameterSpec {
            required: params,
            rest: None,
        },
        body: rc_exprs(body.to_vec()),
        env: named_env.clone(),
    });

    named_env.define(name, Value::Procedure(closure.clone()));

    let call_k = k.clone();
    eval_binding_values_cps(
        bindings,
        0,
        named_env,
        Vec::new(),
        Rc::new(move |values| {
            let frame_env = create_closure_call_env(closure.as_ref(), values, pos)?;
            eval_sequence_cps(closure.body.clone(), 0, frame_env, call_k.clone())
        }),
    )
}

fn eval_letrec_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    sequential: bool,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let form_name = if sequential { "letrec*" } else { "letrec" };
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: form_name.into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: form_name.into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    let bindings = rc_bindings(parse_bindings(bindings_expr, form_name)?);
    let letrec_env = Env::child(&env);
    for (name, _) in bindings.iter() {
        letrec_env.define(name.clone(), Value::Uninitialized(name.clone()));
    }

    let body = rc_exprs(body.to_vec());
    if sequential {
        let done_env = letrec_env.clone();
        let done_k = k.clone();
        eval_letrec_sequential_bindings_cps(
            bindings,
            0,
            letrec_env,
            Rc::new(move |_| eval_sequence_cps(body.clone(), 0, done_env.clone(), done_k.clone())),
        )
    } else {
        let done_env = letrec_env.clone();
        let done_k = k.clone();
        eval_binding_values_cps(
            bindings.clone(),
            0,
            letrec_env,
            Vec::new(),
            Rc::new(move |values| {
                for ((name, _), value) in bindings.iter().zip(values) {
                    let updated = done_env.set(name, value);
                    debug_assert!(updated);
                }
                eval_sequence_cps(body.clone(), 0, done_env.clone(), done_k.clone())
            }),
        )
    }
}

fn eval_letrec_sequential_bindings_cps(
    bindings: Rc<[(String, Expr)]>,
    index: usize,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= bindings.len() {
        return k(Value::Void);
    }

    let (name, expression) = &bindings[index];
    let next_name = name.clone();
    let next_expr = expression.clone();
    let next_bindings = bindings.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        next_expr,
        env,
        Rc::new(move |value| {
            let updated = next_env.set(&next_name, value);
            debug_assert!(updated);
            eval_letrec_sequential_bindings_cps(
                next_bindings.clone(),
                index + 1,
                next_env.clone(),
                next_k.clone(),
            )
        }),
    )
}

fn eval_binding_values_cps(
    bindings: Rc<[(String, Expr)]>,
    index: usize,
    env: EnvRef,
    values: Vec<Value>,
    k: ValuesContinuationRef,
) -> Result<Value, EvalError> {
    if index >= bindings.len() {
        return k(values);
    }

    let value_expr = bindings[index].1.clone();
    let next_bindings = bindings.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        value_expr,
        env,
        Rc::new(move |value| {
            let mut next_values = values.clone();
            next_values.push(value);
            eval_binding_values_cps(
                next_bindings.clone(),
                index + 1,
                next_env.clone(),
                next_values,
                next_k.clone(),
            )
        }),
    )
}

fn eval_cond_cps(arguments: &[Expr], env: EnvRef, k: ContinuationRef) -> Result<Value, EvalError> {
    eval_cond_clause_cps(rc_exprs(arguments.to_vec()), 0, env, k)
}

fn eval_cond_clause_cps(
    clauses: Rc<[Expr]>,
    index: usize,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= clauses.len() {
        return k(Value::Void);
    }

    let clause = clauses[index].clone();
    let ExprKind::List(items) = clause.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "cond: clauses must be lists".into(),
        }
        .with_offset(clause.pos.offset));
    };

    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "cond: clauses cannot be empty".into(),
        }
        .with_offset(clause.pos.offset));
    };

    if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
        if index + 1 != clauses.len() {
            return Err(EvalError::InvalidSyntax {
                message: "cond: else clause must be last".into(),
            }
            .with_offset(test.pos.offset));
        }

        return if body.is_empty() {
            k(Value::Void)
        } else {
            eval_sequence_cps(rc_exprs(body.to_vec()), 0, env, k)
        };
    }

    let body = rc_exprs(body.to_vec());
    let next_clauses = clauses.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        test.clone(),
        env,
        Rc::new(move |value| {
            if value.is_truthy() {
                if body.is_empty() {
                    next_k.clone()(value)
                } else {
                    eval_sequence_cps(body.clone(), 0, next_env.clone(), next_k.clone())
                }
            } else {
                eval_cond_clause_cps(
                    next_clauses.clone(),
                    index + 1,
                    next_env.clone(),
                    next_k.clone(),
                )
            }
        }),
    )
}

fn eval_case_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    if clauses.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    let clauses = rc_exprs(clauses.to_vec());
    let branch_env = env.clone();
    let branch_k = k.clone();
    eval_expr_cps(
        key_expr.clone(),
        env,
        Rc::new(move |key| {
            eval_case_clause_cps(
                key,
                clauses.clone(),
                0,
                branch_env.clone(),
                branch_k.clone(),
            )
        }),
    )
}

fn eval_case_clause_cps(
    key: Value,
    clauses: Rc<[Expr]>,
    index: usize,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    if index >= clauses.len() {
        return k(Value::Boolean(false));
    }

    let clause = clauses[index].clone();
    let ExprKind::List(items) = clause.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "case: clauses must be lists".into(),
        }
        .with_offset(clause.pos.offset));
    };

    let Some((datum_expr, body)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "case: clauses cannot be empty".into(),
        }
        .with_offset(clause.pos.offset));
    };

    if matches!(&datum_expr.kind, ExprKind::Symbol(name) if name == "else") {
        if index + 1 != clauses.len() {
            return Err(EvalError::InvalidSyntax {
                message: "case: else clause must be last".into(),
            }
            .with_offset(datum_expr.pos.offset));
        }

        return if body.is_empty() {
            k(Value::Void)
        } else {
            eval_sequence_cps(rc_exprs(body.to_vec()), 0, env, k)
        };
    }

    let ExprKind::List(datums) = &datum_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "case: expected datum list or else".into(),
        }
        .with_offset(datum_expr.pos.offset));
    };

    if datums
        .iter()
        .map(quote_expr)
        .any(|datum| value_eqv(&key, &datum))
    {
        if body.is_empty() {
            k(Value::Void)
        } else {
            eval_sequence_cps(rc_exprs(body.to_vec()), 0, env, k)
        }
    } else {
        eval_case_clause_cps(key, clauses, index + 1, env, k)
    }
}

fn eval_do_cps(
    pos: SourcePos,
    arguments: &[Expr],
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let [bindings_expr, test_expr, body @ ..] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "do".into(),
            expected: "at least 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let bindings = rc_do_bindings(parse_do_bindings(bindings_expr)?);
    let (test, results) = parse_do_test(test_expr)?;
    let loop_env = Env::child(&env);
    let test = test.clone();
    let results = rc_exprs(results);
    let body = rc_exprs(body.to_vec());
    let init_env = loop_env.clone();
    let init_k = k.clone();
    eval_do_init_values_cps(
        bindings.clone(),
        0,
        env,
        Vec::new(),
        Rc::new(move |init_values| {
            for (binding, value) in bindings.iter().zip(init_values) {
                init_env.define(binding.name.clone(), value);
            }

            eval_do_loop_cps(
                pos,
                bindings.clone(),
                test.clone(),
                results.clone(),
                body.clone(),
                init_env.clone(),
                init_k.clone(),
            )
        }),
    )
}

fn eval_do_init_values_cps(
    bindings: Rc<[DoBinding]>,
    index: usize,
    env: EnvRef,
    values: Vec<Value>,
    k: ValuesContinuationRef,
) -> Result<Value, EvalError> {
    if index >= bindings.len() {
        return k(values);
    }

    let init_expr = bindings[index].init.clone();
    let next_bindings = bindings.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        init_expr,
        env,
        Rc::new(move |value| {
            let mut next_values = values.clone();
            next_values.push(value);
            eval_do_init_values_cps(
                next_bindings.clone(),
                index + 1,
                next_env.clone(),
                next_values,
                next_k.clone(),
            )
        }),
    )
}

fn eval_do_loop_cps(
    pos: SourcePos,
    bindings: Rc<[DoBinding]>,
    test: Expr,
    results: Rc<[Expr]>,
    body: Rc<[Expr]>,
    env: EnvRef,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let next_bindings = bindings.clone();
    let next_results = results.clone();
    let next_body = body.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    eval_expr_cps(
        test.clone(),
        env,
        Rc::new(move |value| {
            if value.is_truthy() {
                if next_results.is_empty() {
                    next_k.clone()(Value::Void)
                } else {
                    eval_sequence_cps(next_results.clone(), 0, next_env.clone(), next_k.clone())
                }
            } else {
                let body_env = next_env.clone();
                let step_env = next_env.clone();
                let step_k = next_k.clone();
                let loop_bindings = next_bindings.clone();
                let loop_test = test.clone();
                let loop_results = next_results.clone();
                let loop_body = next_body.clone();
                eval_sequence_cps(
                    next_body.clone(),
                    0,
                    body_env,
                    Rc::new(move |_| {
                        let update_env = step_env.clone();
                        let resume_env = step_env.clone();
                        let resume_k = step_k.clone();
                        let resume_bindings = loop_bindings.clone();
                        let resume_test = loop_test.clone();
                        let resume_results = loop_results.clone();
                        let resume_body = loop_body.clone();
                        eval_do_next_values_cps(
                            pos,
                            loop_bindings.clone(),
                            0,
                            update_env.clone(),
                            Vec::new(),
                            Rc::new(move |next_values| {
                                for (binding, next_value) in resume_bindings.iter().zip(next_values)
                                {
                                    let updated = update_env.set(&binding.name, next_value);
                                    debug_assert!(updated);
                                }

                                eval_do_loop_cps(
                                    pos,
                                    resume_bindings.clone(),
                                    resume_test.clone(),
                                    resume_results.clone(),
                                    resume_body.clone(),
                                    resume_env.clone(),
                                    resume_k.clone(),
                                )
                            }),
                        )
                    }),
                )
            }
        }),
    )
}

fn eval_do_next_values_cps(
    pos: SourcePos,
    bindings: Rc<[DoBinding]>,
    index: usize,
    env: EnvRef,
    values: Vec<Value>,
    k: ValuesContinuationRef,
) -> Result<Value, EvalError> {
    if index >= bindings.len() {
        return k(values);
    }

    let binding = bindings[index].clone();
    let next_bindings = bindings.clone();
    let next_env = env.clone();
    let next_k = k.clone();

    match binding.step {
        Some(step_expr) => eval_expr_cps(
            step_expr,
            env,
            Rc::new(move |value| {
                let mut next_values = values.clone();
                next_values.push(value);
                eval_do_next_values_cps(
                    pos,
                    next_bindings.clone(),
                    index + 1,
                    next_env.clone(),
                    next_values,
                    next_k.clone(),
                )
            }),
        ),
        None => {
            let value = next_env.lookup(&binding.name).ok_or_else(|| {
                EvalError::UnboundVariable {
                    name: binding.name.clone(),
                }
                .with_offset(pos.offset)
            })?;
            let mut next_values = values;
            next_values.push(value);
            eval_do_next_values_cps(pos, next_bindings, index + 1, next_env, next_values, next_k)
        }
    }
}

fn apply_value_cps(
    value: Value,
    argument_values: Vec<Value>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    match value {
        Value::Builtin(Builtin::CallCc) => eval_call_cc_cps(argument_values, env, call_pos, k),
        Value::Builtin(Builtin::Apply) => eval_apply_builtin_cps(argument_values, env, call_pos, k),
        Value::Builtin(Builtin::Map) => eval_map_cps(argument_values, env, call_pos, k),
        Value::Builtin(Builtin::ForEach) => eval_for_each_cps(argument_values, env, call_pos, k),
        Value::Builtin(builtin) => k(eval_builtin_from_values(
            builtin,
            argument_values,
            &env,
            call_pos,
        )?),
        Value::NativeProcedure(procedure) => k(apply_native_procedure_values(
            &procedure,
            argument_values,
            call_pos,
        )?),
        Value::Procedure(closure) => {
            let call_env = create_closure_call_env(closure.as_ref(), argument_values, call_pos)?;
            eval_sequence_cps(closure.body.clone(), 0, call_env, k)
        }
        Value::CaseProcedure(closure) => {
            let clause =
                select_case_lambda_clause(closure.as_ref(), argument_values.len(), call_pos)?;
            let call_env = create_closure_call_env(clause.as_ref(), argument_values, call_pos)?;
            eval_sequence_cps(clause.body.clone(), 0, call_env, k)
        }
        Value::Continuation(continuation) => {
            let [value] = argument_values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name: "procedure".into(),
                    expected: "exactly 1".into(),
                    got: argument_values.len(),
                }
                .with_offset(call_pos.offset));
            };
            Err(queue_continuation_jump(
                continuation.inner.clone(),
                value.clone(),
            ))
        }
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }
        .with_offset(call_pos.offset)),
    }
}

fn eval_call_cc_cps(
    argument_values: Vec<Value>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let [procedure] = argument_values.as_slice() else {
        return Err(EvalError::WrongArgCount {
            name: "call/cc".into(),
            expected: "exactly 1".into(),
            got: argument_values.len(),
        }
        .with_offset(call_pos.offset));
    };

    apply_value_cps(
        procedure.clone(),
        vec![Value::Continuation(SchemeContinuation::new(k.clone()))],
        env,
        call_pos,
        k,
    )
}

fn eval_apply_builtin_cps(
    argument_values: Vec<Value>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((procedure, rest_arguments)) = argument_values.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if rest_arguments.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let (list_value, prefix_values) = rest_arguments
        .split_last()
        .expect("rest arguments are known to be non-empty");
    let mut values = prefix_values.to_vec();
    values.extend(
        collect_list_items(list_value).map_err(|error| list_access_error(error, call_pos))?,
    );

    apply_value_cps(procedure.clone(), values, env, call_pos, k)
}

fn eval_map_cps(
    argument_values: Vec<Value>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((procedure, list_values)) = argument_values.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if list_values.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let lists = list_values
        .iter()
        .map(|value| collect_list_items(value).map_err(|error| list_access_error(error, call_pos)))
        .collect::<Result<Vec<_>, _>>()?;

    let len = lists.first().map(Vec::len).unwrap_or(0);
    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            message: "map: all lists must have the same length".into(),
        }
        .with_offset(call_pos.offset));
    }

    eval_map_rows_cps(
        procedure.clone(),
        rc_value_lists(lists),
        0,
        Vec::new(),
        env,
        call_pos,
        k,
    )
}

fn eval_map_rows_cps(
    procedure: Value,
    lists: Rc<[Vec<Value>]>,
    index: usize,
    results: Vec<Value>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let len = lists.first().map(Vec::len).unwrap_or(0);
    if index >= len {
        return k(make_proper_list(results));
    }

    let row = lists
        .iter()
        .map(|list| list[index].clone())
        .collect::<Vec<_>>();
    let next_lists = lists.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    apply_value_cps(
        procedure.clone(),
        row,
        env,
        call_pos,
        Rc::new(move |value| {
            let mut next_results = results.clone();
            next_results.push(value);
            eval_map_rows_cps(
                procedure.clone(),
                next_lists.clone(),
                index + 1,
                next_results,
                next_env.clone(),
                call_pos,
                next_k.clone(),
            )
        }),
    )
}

fn eval_for_each_cps(
    argument_values: Vec<Value>,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let Some((procedure, list_values)) = argument_values.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "for-each".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if list_values.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "for-each".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let lists = list_values
        .iter()
        .map(|value| collect_list_items(value).map_err(|error| list_access_error(error, call_pos)))
        .collect::<Result<Vec<_>, _>>()?;

    let len = lists.first().map(Vec::len).unwrap_or(0);
    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            message: "for-each: all lists must have the same length".into(),
        }
        .with_offset(call_pos.offset));
    }

    eval_for_each_rows_cps(
        procedure.clone(),
        rc_value_lists(lists),
        0,
        env,
        call_pos,
        k,
    )
}

fn eval_for_each_rows_cps(
    procedure: Value,
    lists: Rc<[Vec<Value>]>,
    index: usize,
    env: EnvRef,
    call_pos: SourcePos,
    k: ContinuationRef,
) -> Result<Value, EvalError> {
    let len = lists.first().map(Vec::len).unwrap_or(0);
    if index >= len {
        return k(Value::Void);
    }

    let row = lists
        .iter()
        .map(|list| list[index].clone())
        .collect::<Vec<_>>();
    let next_lists = lists.clone();
    let next_env = env.clone();
    let next_k = k.clone();
    apply_value_cps(
        procedure.clone(),
        row,
        env,
        call_pos,
        Rc::new(move |_| {
            eval_for_each_rows_cps(
                procedure.clone(),
                next_lists.clone(),
                index + 1,
                next_env.clone(),
                call_pos,
                next_k.clone(),
            )
        }),
    )
}

fn eval_builtin_from_values(
    builtin: Builtin,
    argument_values: Vec<Value>,
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    debug_assert!(!matches!(
        builtin,
        Builtin::CallCc | Builtin::Apply | Builtin::Map | Builtin::ForEach
    ));

    let apply_env = Env::child(env);
    let mut arguments = Vec::with_capacity(argument_values.len());
    for (index, argument_value) in argument_values.into_iter().enumerate() {
        let name = format!("__cps_arg_{}_{}", call_pos.offset, index);
        apply_env.define(name.clone(), argument_value);
        arguments.push(Expr::symbol(name, call_pos));
    }

    eval_builtin(builtin, &arguments, &apply_env, call_pos)
}

enum TailTarget<'a> {
    Expr(&'a Expr),
    Sequence(&'a [Expr]),
    OwnedExpr(Expr),
    OwnedSequence(Rc<[Expr]>),
}

enum TailControl<'a> {
    Return(Value),
    Continue { target: TailTarget<'a>, env: EnvRef },
}

fn borrowed_expr_target<'a>(expr: &'a Expr) -> TailTarget<'a> {
    TailTarget::Expr(expr)
}

fn borrowed_sequence_target<'a>(expressions: &'a [Expr]) -> TailTarget<'a> {
    TailTarget::Sequence(expressions)
}

fn owned_expr_target<'a>(expr: &'a Expr) -> TailTarget<'a> {
    TailTarget::OwnedExpr(expr.clone())
}

fn owned_sequence_target<'a>(expressions: &'a [Expr]) -> TailTarget<'a> {
    TailTarget::OwnedSequence(Rc::from(expressions.to_vec()))
}

fn into_owned_target(target: TailTarget<'_>) -> TailTarget<'static> {
    match target {
        TailTarget::Expr(expr) => TailTarget::OwnedExpr(expr.clone()),
        TailTarget::Sequence(expressions) => {
            TailTarget::OwnedSequence(Rc::from(expressions.to_vec()))
        }
        TailTarget::OwnedExpr(expr) => TailTarget::OwnedExpr(expr),
        TailTarget::OwnedSequence(expressions) => TailTarget::OwnedSequence(expressions),
    }
}

fn into_owned_control(control: TailControl<'_>) -> TailControl<'static> {
    match control {
        TailControl::Return(value) => TailControl::Return(value),
        TailControl::Continue { target, env } => TailControl::Continue {
            target: into_owned_target(target),
            env,
        },
    }
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_tail_target(TailTarget::Sequence(expressions), env)
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    eval_tail_target(TailTarget::Expr(expr), env)
}

fn eval_tail_target<'a>(mut target: TailTarget<'a>, env: &EnvRef) -> Result<Value, EvalError> {
    let mut env = env.clone();

    loop {
        let control = match target {
            TailTarget::Expr(expr) => {
                eval_expr_control(expr, &env, borrowed_expr_target, borrowed_sequence_target)?
            }
            TailTarget::Sequence(expressions) => {
                eval_sequence_control(expressions, &env, borrowed_expr_target)?
            }
            TailTarget::OwnedExpr(expr) => into_owned_control(eval_expr_control(
                &expr,
                &env,
                owned_expr_target,
                owned_sequence_target,
            )?),
            TailTarget::OwnedSequence(expressions) => {
                eval_owned_sequence_control(expressions, &env)?
            }
        };

        match control {
            TailControl::Return(value) => return Ok(value),
            TailControl::Continue {
                target: next_target,
                env: next_env,
            } => {
                target = next_target;
                env = next_env;
            }
        }
    }
}

fn eval_sequence_control<'a, F>(
    expressions: &'a [Expr],
    env: &EnvRef,
    make_expr_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Fn(&'a Expr) -> TailTarget<'a>,
{
    let Some((last, initial)) = expressions.split_last() else {
        return Ok(TailControl::Return(Value::Void));
    };

    for expression in initial {
        eval_expr(expression, env)?;
    }

    Ok(TailControl::Continue {
        target: make_expr_target(last),
        env: env.clone(),
    })
}

fn eval_owned_sequence_control(
    expressions: Rc<[Expr]>,
    env: &EnvRef,
) -> Result<TailControl<'static>, EvalError> {
    let slice = expressions.as_ref();
    let Some((last, initial)) = slice.split_last() else {
        return Ok(TailControl::Return(Value::Void));
    };

    for expression in initial {
        eval_expr(expression, env)?;
    }

    Ok(TailControl::Continue {
        target: TailTarget::OwnedExpr(last.clone()),
        env: env.clone(),
    })
}

fn eval_expr_control<'a, FExpr, FSeq>(
    expr: &'a Expr,
    env: &EnvRef,
    make_expr_target: FExpr,
    make_sequence_target: FSeq,
) -> Result<TailControl<'a>, EvalError>
where
    FExpr: Copy + Fn(&'a Expr) -> TailTarget<'a>,
    FSeq: Copy + Fn(&'a [Expr]) -> TailTarget<'a>,
{
    match &expr.kind {
        ExprKind::Number(value) => Ok(TailControl::Return(Value::Number(*value))),
        ExprKind::Boolean(value) => Ok(TailControl::Return(Value::Boolean(*value))),
        ExprKind::String(value) => Ok(TailControl::Return(Value::String(SchemeString::immutable(
            value.clone(),
        )))),
        ExprKind::Char(value) => Ok(TailControl::Return(Value::Char(*value))),
        ExprKind::Symbol(name) => match env.lookup(name) {
            Some(Value::Uninitialized(_)) => {
                Err(EvalError::UninitializedBinding { name: name.clone() }
                    .with_offset(expr.pos.offset))
            }
            Some(value) => Ok(TailControl::Return(value)),
            None => {
                Err(EvalError::UnboundVariable { name: name.clone() }.with_offset(expr.pos.offset))
            }
        },
        ExprKind::List(items) => {
            eval_list_control(expr.pos, items, env, make_expr_target, make_sequence_target)
        }
    }
}

fn eval_application(list_pos: SourcePos, items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_tail_target(
        TailTarget::OwnedExpr(Expr::new(ExprKind::List(items.to_vec()), list_pos)),
        env,
    )
}

fn eval_list_control<'a, FExpr, FSeq>(
    list_pos: SourcePos,
    items: &'a [Expr],
    env: &EnvRef,
    make_expr_target: FExpr,
    make_sequence_target: FSeq,
) -> Result<TailControl<'a>, EvalError>
where
    FExpr: Copy + Fn(&'a Expr) -> TailTarget<'a>,
    FSeq: Copy + Fn(&'a [Expr]) -> TailTarget<'a>,
{
    let (operator, arguments) = items
        .split_first()
        .ok_or_else(|| EvalError::EmptyList.with_offset(list_pos.offset))?;

    if let ExprKind::Symbol(name) = &operator.kind {
        match name.as_str() {
            "define" => {
                return Ok(TailControl::Return(eval_define(
                    operator.pos,
                    arguments,
                    env,
                )?))
            }
            "define-record-type" => {
                return Ok(TailControl::Return(eval_define_record_type(
                    operator.pos,
                    arguments,
                    env,
                )?))
            }
            "define-syntax" => {
                return Ok(TailControl::Return(eval_define_syntax(
                    operator.pos,
                    arguments,
                    env,
                )?))
            }
            "set!" => return Ok(TailControl::Return(eval_set(operator.pos, arguments, env)?)),
            "if" => return eval_if_control(operator.pos, arguments, env, make_expr_target),
            "quote" => return Ok(TailControl::Return(eval_quote(operator.pos, arguments)?)),
            "lambda" => {
                return Ok(TailControl::Return(eval_lambda(
                    operator.pos,
                    arguments,
                    env,
                )?))
            }
            "case-lambda" => return Ok(TailControl::Return(eval_case_lambda(arguments, env)?)),
            "and" => return eval_and_control(arguments, env, make_expr_target),
            "or" => return eval_or_control(arguments, env, make_expr_target),
            "begin" => {
                return Ok(TailControl::Continue {
                    target: make_sequence_target(arguments),
                    env: env.clone(),
                })
            }
            "let" => return eval_let_control(operator.pos, arguments, env, make_sequence_target),
            "let*" => {
                return eval_let_star_control(operator.pos, arguments, env, make_sequence_target)
            }
            "letrec" => {
                return Ok(TailControl::Return(eval_letrec(
                    operator.pos,
                    arguments,
                    env,
                    false,
                )?))
            }
            "letrec*" => {
                return Ok(TailControl::Return(eval_letrec(
                    operator.pos,
                    arguments,
                    env,
                    true,
                )?))
            }
            "cond" => return eval_cond_control(arguments, env, make_sequence_target),
            "case" => {
                return Ok(TailControl::Return(eval_case(
                    operator.pos,
                    arguments,
                    env,
                )?))
            }
            "do" => return Ok(TailControl::Return(eval_do(operator.pos, arguments, env)?)),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let invocation = Expr::new(ExprKind::List(items.to_vec()), list_pos);
            let expansion = expand_macro_invocation(transformer.as_ref(), &invocation)?;
            let macro_env = Env::child(env);

            for (alias, value) in expansion.aliases {
                macro_env.define(alias, value);
            }

            return Ok(TailControl::Continue {
                target: TailTarget::OwnedExpr(expansion.expr),
                env: macro_env,
            });
        }
    }

    let procedure = eval_expr(operator, env)?;
    eval_tail_application(procedure, arguments, env, operator.pos)
}

fn eval_tail_application<'a>(
    value: Value,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<TailControl<'a>, EvalError> {
    match value {
        Value::Builtin(builtin) => Ok(TailControl::Return(eval_builtin(
            builtin, arguments, env, call_pos,
        )?)),
        Value::NativeProcedure(procedure) => Ok(TailControl::Return(apply_native_procedure(
            &procedure, arguments, env, call_pos,
        )?)),
        Value::Procedure(closure) => {
            let argument_values = eval_args(arguments, env)?;
            let call_env = create_closure_call_env(closure.as_ref(), argument_values, call_pos)?;
            Ok(TailControl::Continue {
                target: TailTarget::OwnedSequence(closure.body.clone()),
                env: call_env,
            })
        }
        Value::CaseProcedure(closure) => {
            let argument_values = eval_args(arguments, env)?;
            let clause =
                select_case_lambda_clause(closure.as_ref(), argument_values.len(), call_pos)?;
            let call_env = create_closure_call_env(clause.as_ref(), argument_values, call_pos)?;
            Ok(TailControl::Continue {
                target: TailTarget::OwnedSequence(clause.body.clone()),
                env: call_env,
            })
        }
        Value::Continuation(continuation) => {
            let argument_values = eval_args(arguments, env)?;
            let [value] = argument_values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name: "procedure".into(),
                    expected: "exactly 1".into(),
                    got: argument_values.len(),
                }
                .with_offset(call_pos.offset));
            };

            Err(queue_continuation_jump(
                continuation.inner.clone(),
                value.clone(),
            ))
        }
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }
        .with_offset(call_pos.offset)),
    }
}

fn eval_if_control<'a, F>(
    pos: SourcePos,
    arguments: &'a [Expr],
    env: &EnvRef,
    make_expr_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a Expr) -> TailTarget<'a>,
{
    match arguments {
        [condition, consequent] => {
            if eval_expr(condition, env)?.is_truthy() {
                Ok(TailControl::Continue {
                    target: make_expr_target(consequent),
                    env: env.clone(),
                })
            } else {
                Ok(TailControl::Return(Value::Boolean(false)))
            }
        }
        [condition, consequent, alternate] => {
            let next = if eval_expr(condition, env)?.is_truthy() {
                consequent
            } else {
                alternate
            };

            Ok(TailControl::Continue {
                target: make_expr_target(next),
                env: env.clone(),
            })
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset)),
    }
}

fn eval_and_control<'a, F>(
    arguments: &'a [Expr],
    env: &EnvRef,
    make_expr_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a Expr) -> TailTarget<'a>,
{
    let Some((last, initial)) = arguments.split_last() else {
        return Ok(TailControl::Return(Value::Boolean(true)));
    };

    for argument in initial {
        let value = eval_expr(argument, env)?;
        if !value.is_truthy() {
            return Ok(TailControl::Return(value));
        }
    }

    Ok(TailControl::Continue {
        target: make_expr_target(last),
        env: env.clone(),
    })
}

fn eval_or_control<'a, F>(
    arguments: &'a [Expr],
    env: &EnvRef,
    make_expr_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a Expr) -> TailTarget<'a>,
{
    let Some((last, initial)) = arguments.split_last() else {
        return Ok(TailControl::Return(Value::Boolean(false)));
    };

    for argument in initial {
        let value = eval_expr(argument, env)?;
        if value.is_truthy() {
            return Ok(TailControl::Return(value));
        }
    }

    Ok(TailControl::Continue {
        target: make_expr_target(last),
        env: env.clone(),
    })
}

fn eval_let_control<'a, F>(
    pos: SourcePos,
    arguments: &'a [Expr],
    env: &EnvRef,
    make_sequence_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a [Expr]) -> TailTarget<'a>,
{
    let Some((first, rest)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let_control(name, rest, env, pos),
        _ => eval_plain_let_control(first, rest, env, make_sequence_target),
    }
}

fn eval_plain_let_control<'a, F>(
    bindings_expr: &'a Expr,
    body: &'a [Expr],
    env: &EnvRef,
    make_sequence_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a [Expr]) -> TailTarget<'a>,
{
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(bindings_expr.pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let let_env = Env::child(env);

    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    Ok(TailControl::Continue {
        target: make_sequence_target(body),
        env: let_env,
    })
}

fn eval_let_star_control<'a, F>(
    pos: SourcePos,
    arguments: &'a [Expr],
    env: &EnvRef,
    make_sequence_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a [Expr]) -> TailTarget<'a>,
{
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let*".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let*".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, "let*")?;
    let let_env = Env::child(env);

    for (name, value_expr) in bindings {
        let value = eval_expr(&value_expr, &let_env)?;
        let_env.define(name, value);
    }

    Ok(TailControl::Continue {
        target: make_sequence_target(body),
        env: let_env,
    })
}

fn eval_named_let_control(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<TailControl<'static>, EvalError> {
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 2,
        }
        .with_offset(pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let named_env = Env::child(env);
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let closure = Rc::new(Closure {
        params: ParameterSpec {
            required: params,
            rest: None,
        },
        body: Rc::from(body.to_vec()),
        env: named_env.clone(),
    });

    named_env.define(name.into(), Value::Procedure(closure.clone()));

    let values = eval_binding_values(&bindings, &named_env)?;
    let call_env = create_closure_call_env(closure.as_ref(), values, pos)?;
    Ok(TailControl::Continue {
        target: TailTarget::OwnedSequence(closure.body.clone()),
        env: call_env,
    })
}

fn eval_cond_control<'a, F>(
    arguments: &'a [Expr],
    env: &EnvRef,
    make_sequence_target: F,
) -> Result<TailControl<'a>, EvalError>
where
    F: Copy + Fn(&'a [Expr]) -> TailTarget<'a>,
{
    for (index, clause) in arguments.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "cond: clauses must be lists".into(),
            }
            .with_offset(clause.pos.offset));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "cond: clauses cannot be empty".into(),
            }
            .with_offset(clause.pos.offset));
        };

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != arguments.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond: else clause must be last".into(),
                }
                .with_offset(test.pos.offset));
            }

            return if body.is_empty() {
                Ok(TailControl::Return(Value::Void))
            } else {
                Ok(TailControl::Continue {
                    target: make_sequence_target(body),
                    env: env.clone(),
                })
            };
        }

        let value = eval_expr(test, env)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(TailControl::Return(value))
            } else {
                Ok(TailControl::Continue {
                    target: make_sequence_target(body),
                    env: env.clone(),
                })
            };
        }
    }

    Ok(TailControl::Return(Value::Void))
}

fn apply_value(
    value: Value,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match value {
        Value::Builtin(builtin) => eval_builtin(builtin, arguments, env, call_pos),
        Value::NativeProcedure(procedure) => {
            apply_native_procedure(&procedure, arguments, env, call_pos)
        }
        Value::Procedure(closure) => apply_closure(closure, arguments, env, call_pos),
        Value::CaseProcedure(closure) => apply_case_closure(closure, arguments, env, call_pos),
        Value::Continuation(continuation) => {
            let argument_values = eval_args(arguments, env)?;
            let [value] = argument_values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name: "procedure".into(),
                    expected: "exactly 1".into(),
                    got: argument_values.len(),
                }
                .with_offset(call_pos.offset));
            };

            Err(queue_continuation_jump(
                continuation.inner.clone(),
                value.clone(),
            ))
        }
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }
        .with_offset(call_pos.offset)),
    }
}

fn apply_native_procedure(
    procedure: &NativeProcedure,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let argument_values = eval_args(arguments, env)?;
    apply_native_procedure_values(procedure, argument_values, call_pos)
}

fn apply_native_procedure_values(
    procedure: &NativeProcedure,
    argument_values: Vec<Value>,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match procedure {
        NativeProcedure::RecordConstructor {
            name,
            record_type,
            constructor_fields,
        } => {
            if argument_values.len() != constructor_fields.len() {
                return Err(EvalError::WrongArgCount {
                    name: name.clone(),
                    expected: format!("exactly {}", constructor_fields.len()),
                    got: argument_values.len(),
                }
                .with_offset(call_pos.offset));
            }

            let mut fields = vec![Value::Void; record_type.fields.len()];
            for (value, index) in argument_values
                .into_iter()
                .zip(constructor_fields.iter().copied())
            {
                fields[index] = value;
            }

            Ok(Value::Record(Rc::new(RecordValue {
                record_type: record_type.clone(),
                fields,
            })))
        }
        NativeProcedure::RecordPredicate { name, record_type } => {
            let [value] = argument_values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name: name.clone(),
                    expected: "exactly 1".into(),
                    got: argument_values.len(),
                }
                .with_offset(call_pos.offset));
            };

            Ok(Value::Boolean(matches!(
                value,
                Value::Record(record) if record.record_type.id == record_type.id
            )))
        }
        NativeProcedure::RecordAccessor {
            name,
            record_type,
            field_index,
        } => {
            let [value] = argument_values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    name: name.clone(),
                    expected: "exactly 1".into(),
                    got: argument_values.len(),
                }
                .with_offset(call_pos.offset));
            };

            match value {
                Value::Record(record) if record.record_type.id == record_type.id => {
                    Ok(record.fields[*field_index].clone())
                }
                other => Err(EvalError::TypeMismatch {
                    expected: format!("{} record", record_type.name),
                    found: other.kind().into(),
                }
                .with_offset(call_pos.offset)),
            }
        }
    }
}

fn eval_builtin(
    builtin: Builtin,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(arguments, env),
        Builtin::Sub => eval_sub(arguments, env, call_pos),
        Builtin::Mul => eval_mul(arguments, env),
        Builtin::Div => eval_div(arguments, env, call_pos),
        Builtin::LessThan => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                left.less_than(right)
            })
        }
        Builtin::GreaterThan => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                left.greater_than(right)
            })
        }
        Builtin::GreaterEqual => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                !left.less_than(right)
            })
        }
        Builtin::Equal => eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
            left.equals(right)
        }),
        Builtin::Eq => eval_eq(arguments, env, call_pos),
        Builtin::Eqv => eval_eqv(arguments, env, call_pos),
        Builtin::EqualDeep => eval_equal(arguments, env, call_pos),
        Builtin::LessEqual => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                left.less_equal(right)
            })
        }
        Builtin::Not => eval_not(arguments, env, call_pos),
        Builtin::Abs => eval_abs(arguments, env, call_pos),
        Builtin::Modulo => eval_modulo(arguments, env, call_pos),
        Builtin::Remainder => eval_remainder(arguments, env, call_pos),
        Builtin::Quotient => eval_quotient(arguments, env, call_pos),
        Builtin::Min => eval_min(arguments, env, call_pos),
        Builtin::Max => eval_max(arguments, env, call_pos),
        Builtin::Expt => eval_expt(arguments, env, call_pos),
        Builtin::ZeroPred => {
            eval_number_predicate(arguments, env, "zero?", call_pos, |value| value.is_zero())
        }
        Builtin::PositivePred => {
            eval_number_predicate(arguments, env, "positive?", call_pos, |value| {
                value.greater_than(Number::integer(0))
            })
        }
        Builtin::NegativePred => {
            eval_number_predicate(arguments, env, "negative?", call_pos, |value| {
                value.less_than(Number::integer(0))
            })
        }
        Builtin::OddPred => {
            eval_integer_number_predicate(arguments, env, "odd?", call_pos, |value| value % 2 != 0)
        }
        Builtin::EvenPred => {
            eval_integer_number_predicate(arguments, env, "even?", call_pos, |value| value % 2 == 0)
        }
        Builtin::Gcd => eval_gcd(arguments, env, call_pos),
        Builtin::Lcm => eval_lcm(arguments, env, call_pos),
        Builtin::Truncate => eval_truncate(arguments, env, call_pos),
        Builtin::Round => eval_round(arguments, env, call_pos),
        Builtin::Cons => eval_cons(arguments, env, call_pos),
        Builtin::Car => eval_car(arguments, env, call_pos),
        Builtin::Cdr => eval_cdr(arguments, env, call_pos),
        Builtin::Caar => eval_cxr(arguments, env, call_pos, "aa", "caar"),
        Builtin::Cadr => eval_cxr(arguments, env, call_pos, "ad", "cadr"),
        Builtin::Cdar => eval_cxr(arguments, env, call_pos, "da", "cdar"),
        Builtin::Cddr => eval_cxr(arguments, env, call_pos, "dd", "cddr"),
        Builtin::SetCar => eval_set_car(arguments, env, call_pos),
        Builtin::SetCdr => eval_set_cdr(arguments, env, call_pos),
        Builtin::NullPred => eval_null_pred(arguments, env, call_pos),
        Builtin::List => eval_list_builtin(arguments, env),
        Builtin::ListRef => eval_list_ref(arguments, env, call_pos),
        Builtin::ListTail => eval_list_tail(arguments, env, call_pos),
        Builtin::ListPred => eval_type_predicate(arguments, env, "list?", call_pos, is_proper_list),
        Builtin::Length => eval_length(arguments, env, call_pos),
        Builtin::Append => eval_append(arguments, env),
        Builtin::Reverse => eval_reverse(arguments, env, call_pos),
        Builtin::Assoc => eval_assoc(arguments, env, call_pos),
        Builtin::Assv => eval_assv(arguments, env, call_pos),
        Builtin::Member => eval_member(arguments, env, call_pos),
        Builtin::Map => eval_map(arguments, env, call_pos),
        Builtin::ForEach => eval_for_each(arguments, env, call_pos),
        Builtin::StringPred => eval_type_predicate(arguments, env, "string?", call_pos, |value| {
            matches!(value, Value::String(_))
        }),
        Builtin::NumberPred => eval_type_predicate(arguments, env, "number?", call_pos, |value| {
            matches!(value, Value::Number(_))
        }),
        Builtin::ExactPred => {
            eval_number_property(arguments, env, "exact?", call_pos, |value| value.is_exact())
        }
        Builtin::InexactPred => {
            eval_number_property(arguments, env, "inexact?", call_pos, |value| {
                value.is_inexact()
            })
        }
        Builtin::IntegerPred => {
            eval_number_property(arguments, env, "integer?", call_pos, |value| {
                value.is_integer()
            })
        }
        Builtin::RationalPred => {
            eval_number_property(arguments, env, "rational?", call_pos, |value| {
                value.is_rational()
            })
        }
        Builtin::BooleanPred => {
            eval_type_predicate(arguments, env, "boolean?", call_pos, |value| {
                matches!(value, Value::Boolean(_))
            })
        }
        Builtin::PairPred => eval_type_predicate(arguments, env, "pair?", call_pos, is_pair),
        Builtin::SymbolPred => eval_type_predicate(arguments, env, "symbol?", call_pos, |value| {
            matches!(value, Value::Symbol(_))
        }),
        Builtin::ProcedurePred => {
            eval_type_predicate(arguments, env, "procedure?", call_pos, is_callable)
        }
        Builtin::Display => eval_display(arguments, env, call_pos),
        Builtin::Write => eval_write(arguments, env, call_pos),
        Builtin::Newline => eval_newline(arguments, env, call_pos),
        Builtin::StringCopy => eval_string_copy(arguments, env, call_pos),
        Builtin::MakeString => eval_make_string(arguments, env, call_pos),
        Builtin::String => eval_string_builtin(arguments, env),
        Builtin::StringAppend => eval_string_append(arguments, env),
        Builtin::StringLength => eval_string_length(arguments, env, call_pos),
        Builtin::StringSet => eval_string_set(arguments, env, call_pos),
        Builtin::Substring => eval_substring(arguments, env, call_pos),
        Builtin::StringToNumber => eval_string_to_number(arguments, env, call_pos),
        Builtin::NumberToString => eval_number_to_string(arguments, env, call_pos),
        Builtin::SymbolToString => eval_symbol_to_string(arguments, env, call_pos),
        Builtin::StringToSymbol => eval_string_to_symbol(arguments, env, call_pos),
        Builtin::StringRef => eval_string_ref(arguments, env, call_pos),
        Builtin::StringToList => eval_string_to_list(arguments, env, call_pos),
        Builtin::ListToString => eval_list_to_string(arguments, env, call_pos),
        Builtin::Vector => eval_vector(arguments, env),
        Builtin::MakeVector => eval_make_vector(arguments, env, call_pos),
        Builtin::VectorRef => eval_vector_ref(arguments, env, call_pos),
        Builtin::VectorSet => eval_vector_set(arguments, env, call_pos),
        Builtin::VectorLength => eval_vector_length(arguments, env, call_pos),
        Builtin::VectorPred => eval_type_predicate(arguments, env, "vector?", call_pos, |value| {
            matches!(value, Value::Vector(_))
        }),
        Builtin::VectorToList => eval_vector_to_list(arguments, env, call_pos),
        Builtin::ListToVector => eval_list_to_vector(arguments, env, call_pos),
        Builtin::CharPred => eval_type_predicate(arguments, env, "char?", call_pos, |value| {
            matches!(value, Value::Char(_))
        }),
        Builtin::CharAlphabeticPred => {
            eval_char_predicate(arguments, env, "char-alphabetic?", call_pos, |ch| {
                ch.is_alphabetic()
            })
        }
        Builtin::CharNumericPred => {
            eval_char_predicate(arguments, env, "char-numeric?", call_pos, |ch| {
                ch.is_numeric()
            })
        }
        Builtin::CharUpcase => eval_char_transform(arguments, env, "char-upcase", call_pos, |ch| {
            ch.to_uppercase().next().unwrap_or(ch)
        }),
        Builtin::CharDowncase => {
            eval_char_transform(arguments, env, "char-downcase", call_pos, |ch| {
                ch.to_lowercase().next().unwrap_or(ch)
            })
        }
        Builtin::CharEqual => {
            eval_char_compare("char=?", arguments, env, call_pos, |left, right| {
                left == right
            })
        }
        Builtin::CharLess => {
            eval_char_compare("char<?", arguments, env, call_pos, |left, right| {
                left < right
            })
        }
        Builtin::CharToInteger => eval_char_to_integer(arguments, env, call_pos),
        Builtin::IntegerToChar => eval_integer_to_char(arguments, env, call_pos),
        Builtin::StringEqual => {
            eval_string_compare("string=?", arguments, env, call_pos, |left, right| {
                left == right
            })
        }
        Builtin::StringLess => {
            eval_string_compare("string<?", arguments, env, call_pos, |left, right| {
                left < right
            })
        }
        Builtin::StringGreater => {
            eval_string_compare("string>?", arguments, env, call_pos, |left, right| {
                left > right
            })
        }
        Builtin::StringLessEqual => {
            eval_string_compare("string<=?", arguments, env, call_pos, |left, right| {
                left <= right
            })
        }
        Builtin::StringGreaterEqual => {
            eval_string_compare("string>=?", arguments, env, call_pos, |left, right| {
                left >= right
            })
        }
        Builtin::StringCiEqual => {
            eval_string_compare("string-ci=?", arguments, env, call_pos, |left, right| {
                left.to_lowercase() == right.to_lowercase()
            })
        }
        Builtin::StringUpcase => {
            eval_string_transform(arguments, env, "string-upcase", call_pos, |value| {
                value.chars().flat_map(|ch| ch.to_uppercase()).collect()
            })
        }
        Builtin::StringDowncase => {
            eval_string_transform(arguments, env, "string-downcase", call_pos, |value| {
                value.chars().flat_map(|ch| ch.to_lowercase()).collect()
            })
        }
        Builtin::ExactToInexact => eval_exact_to_inexact(arguments, env, call_pos),
        Builtin::InexactToExact => eval_inexact_to_exact(arguments, env, call_pos),
        Builtin::Numerator => eval_numerator(arguments, env, call_pos),
        Builtin::Denominator => eval_denominator(arguments, env, call_pos),
        Builtin::CallCc => Err(EvalError::InvalidArgument {
            message: "call/cc requires continuation-aware evaluation".into(),
        }
        .with_offset(call_pos.offset)),
        Builtin::Apply => eval_apply_builtin(arguments, env, call_pos),
    }
}

fn apply_closure(
    closure: Rc<Closure>,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let argument_values = eval_args(arguments, env)?;
    apply_closure_values(closure, argument_values, call_pos)
}

fn apply_case_closure(
    closure: Rc<CaseClosure>,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let argument_values = eval_args(arguments, env)?;
    apply_case_closure_values(closure, argument_values, call_pos)
}

fn apply_closure_values(
    closure: Rc<Closure>,
    argument_values: Vec<Value>,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let call_env = create_closure_call_env(closure.as_ref(), argument_values, call_pos)?;
    eval_sequence(closure.body.as_ref(), &call_env)
}

fn apply_case_closure_values(
    closure: Rc<CaseClosure>,
    argument_values: Vec<Value>,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let clause = select_case_lambda_clause(closure.as_ref(), argument_values.len(), call_pos)?;
    let call_env = create_closure_call_env(clause.as_ref(), argument_values, call_pos)?;
    eval_sequence(clause.body.as_ref(), &call_env)
}

fn create_closure_call_env(
    closure: &Closure,
    argument_values: Vec<Value>,
    call_pos: SourcePos,
) -> Result<EnvRef, EvalError> {
    let required = closure.params.required.len();
    let has_rest = closure.params.rest.is_some();

    if (!has_rest && argument_values.len() != required)
        || (has_rest && argument_values.len() < required)
    {
        return Err(EvalError::WrongArgCount {
            name: "procedure".into(),
            expected: if has_rest {
                format!("at least {required}")
            } else {
                format!("exactly {required}")
            },
            got: argument_values.len(),
        }
        .with_offset(call_pos.offset));
    }

    let call_env = Env::child(&closure.env);
    let mut argument_values = argument_values.into_iter();

    for param in &closure.params.required {
        let value = argument_values
            .next()
            .expect("required argument count already validated");
        call_env.define(param.clone(), value);
    }

    if let Some(rest_param) = &closure.params.rest {
        call_env.define(
            rest_param.clone(),
            make_proper_list(argument_values.collect()),
        );
    }

    Ok(call_env)
}

fn select_case_lambda_clause(
    closure: &CaseClosure,
    argument_count: usize,
    call_pos: SourcePos,
) -> Result<Rc<Closure>, EvalError> {
    closure
        .clauses
        .iter()
        .find(|clause| parameter_spec_accepts(&clause.params, argument_count))
        .cloned()
        .ok_or_else(|| {
            EvalError::WrongArgCount {
                name: "procedure".into(),
                expected: format_case_lambda_arity(closure),
                got: argument_count,
            }
            .with_offset(call_pos.offset)
        })
}

fn eval_args(arguments: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_expr(argument, env))
        .collect()
}

fn eval_define(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "define".into(),
            expected: "at least 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    }

    match &arguments[0].kind {
        ExprKind::Symbol(name) => {
            if arguments.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define".into(),
                    expected: "exactly 2".into(),
                    got: arguments.len(),
                }
                .with_offset(pos.offset));
            }

            let value = eval_expr(&arguments[1], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let (name_expr, params) = signature.split_first().ok_or_else(|| {
                EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                }
                .with_offset(arguments[0].pos.offset)
            })?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                }
                .with_offset(name_expr.pos.offset));
            };

            let closure = Value::Procedure(Rc::new(Closure {
                params: parse_params(params)?,
                body: Rc::from(arguments[1..].to_vec()),
                env: env.clone(),
            }));

            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidSyntax {
            message: "define: expected symbol or function signature".into(),
        }
        .with_offset(arguments[0].pos.offset)),
    }
}

fn eval_define_record_type(
    pos: SourcePos,
    arguments: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "define-record-type".into(),
            expected: "at least 3".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let ExprKind::Symbol(type_name) = &type_name_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type: expected type name".into(),
        }
        .with_offset(type_name_expr.pos.offset));
    };

    let constructor = parse_record_constructor_spec(constructor_expr)?;

    let ExprKind::Symbol(predicate_name) = &predicate_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type: expected predicate name".into(),
        }
        .with_offset(predicate_expr.pos.offset));
    };

    let fields = parse_record_field_specs(field_exprs)?;
    let mut field_indices = HashMap::with_capacity(fields.len());
    for (index, field) in fields.iter().enumerate() {
        if field_indices.insert(field.name.clone(), index).is_some() {
            return Err(EvalError::InvalidSyntax {
                message: format!("define-record-type: duplicate field `{}`", field.name),
            }
            .with_offset(pos.offset));
        }
    }

    let mut constructor_fields = Vec::with_capacity(constructor.fields.len());
    for field_name in &constructor.fields {
        let Some(index) = field_indices.get(field_name).copied() else {
            return Err(EvalError::InvalidSyntax {
                message: format!(
                    "define-record-type: constructor field `{field_name}` is not declared"
                ),
            }
            .with_offset(constructor_expr.pos.offset));
        };
        constructor_fields.push(index);
    }

    let record_type = Rc::new(RecordType {
        id: next_record_type_id(),
        name: type_name.clone(),
        fields: fields.iter().map(|field| field.name.clone()).collect(),
    });

    env.define(
        constructor.name.clone(),
        Value::NativeProcedure(NativeProcedure::RecordConstructor {
            name: constructor.name,
            record_type: record_type.clone(),
            constructor_fields,
        }),
    );
    env.define(
        predicate_name.clone(),
        Value::NativeProcedure(NativeProcedure::RecordPredicate {
            name: predicate_name.clone(),
            record_type: record_type.clone(),
        }),
    );

    for (index, field) in fields.into_iter().enumerate() {
        env.define(
            field.accessor_name.clone(),
            Value::NativeProcedure(NativeProcedure::RecordAccessor {
                name: field.accessor_name,
                record_type: record_type.clone(),
                field_index: index,
            }),
        );
    }

    Ok(Value::Void)
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<RecordConstructorSpec, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type: expected constructor spec".into(),
        }
        .with_offset(expr.pos.offset));
    };

    let Some((name_expr, field_exprs)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type: expected constructor spec".into(),
        }
        .with_offset(expr.pos.offset));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-record-type: constructor name must be a symbol".into(),
        }
        .with_offset(name_expr.pos.offset));
    };

    let mut fields = Vec::with_capacity(field_exprs.len());
    let mut seen = HashSet::new();
    for field_expr in field_exprs {
        let ExprKind::Symbol(field_name) = &field_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "define-record-type: constructor fields must be symbols".into(),
            }
            .with_offset(field_expr.pos.offset));
        };

        if !seen.insert(field_name.clone()) {
            return Err(EvalError::InvalidSyntax {
                message: format!("define-record-type: duplicate constructor field `{field_name}`"),
            }
            .with_offset(field_expr.pos.offset));
        }

        fields.push(field_name.clone());
    }

    Ok(RecordConstructorSpec {
        name: name.clone(),
        fields,
    })
}

fn parse_record_field_specs(field_exprs: &[Expr]) -> Result<Vec<RecordFieldSpec>, EvalError> {
    let mut fields = Vec::with_capacity(field_exprs.len());
    let mut seen_accessors = HashSet::new();

    for field_expr in field_exprs {
        let ExprKind::List(parts) = &field_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "define-record-type: field specs must be lists".into(),
            }
            .with_offset(field_expr.pos.offset));
        };

        let ([field_name_expr, accessor_name_expr] | [field_name_expr, accessor_name_expr, _]) =
            parts.as_slice()
        else {
            return Err(EvalError::InvalidSyntax {
                message:
                    "define-record-type: each field spec must be (field accessor) or (field accessor mutator)"
                        .into(),
            }
            .with_offset(field_expr.pos.offset));
        };

        let ExprKind::Symbol(field_name) = &field_name_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "define-record-type: field names must be symbols".into(),
            }
            .with_offset(field_name_expr.pos.offset));
        };

        let ExprKind::Symbol(accessor_name) = &accessor_name_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "define-record-type: accessor names must be symbols".into(),
            }
            .with_offset(accessor_name_expr.pos.offset));
        };

        if !seen_accessors.insert(accessor_name.clone()) {
            return Err(EvalError::InvalidSyntax {
                message: format!("define-record-type: duplicate accessor `{accessor_name}`"),
            }
            .with_offset(accessor_name_expr.pos.offset));
        }

        fields.push(RecordFieldSpec {
            name: field_name.clone(),
            accessor_name: accessor_name.clone(),
        });
    }

    Ok(fields)
}

fn eval_define_syntax(
    pos: SourcePos,
    arguments: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    let [name_expr, transformer_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "define-syntax".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax: expected macro name".into(),
        }
        .with_offset(name_expr.pos.offset));
    };

    let transformer = parse_syntax_rules(name, transformer_expr, env)?;
    env.define_macro(name.clone(), transformer);
    Ok(Value::Void)
}

fn parse_syntax_rules(
    name: &str,
    transformer_expr: &Expr,
    env: &EnvRef,
) -> Result<Rc<SyntaxRulesMacro>, EvalError> {
    let ExprKind::List(items) = &transformer_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax: expected syntax-rules transformer".into(),
        }
        .with_offset(transformer_expr.pos.offset));
    };

    let Some((keyword, rest)) = items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax: expected syntax-rules transformer".into(),
        }
        .with_offset(transformer_expr.pos.offset));
    };

    if !matches!(&keyword.kind, ExprKind::Symbol(value) if value == "syntax-rules") {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax: expected syntax-rules transformer".into(),
        }
        .with_offset(keyword.pos.offset));
    }

    let Some((literals_expr, rule_exprs)) = rest.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules: expected literal identifiers and at least one rule".into(),
        }
        .with_offset(transformer_expr.pos.offset));
    };

    let ExprKind::List(literal_exprs) = &literals_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules: expected literal identifier list".into(),
        }
        .with_offset(literals_expr.pos.offset));
    };

    if rule_exprs.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules: expected at least one rule".into(),
        }
        .with_offset(transformer_expr.pos.offset));
    }

    let mut literals = HashSet::new();
    for literal in literal_exprs {
        let ExprKind::Symbol(value) = &literal.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules: literal identifiers must be symbols".into(),
            }
            .with_offset(literal.pos.offset));
        };

        literals.insert(value.clone());
    }

    let mut rules = Vec::with_capacity(rule_exprs.len());
    for rule_expr in rule_exprs {
        let ExprKind::List(rule_parts) = &rule_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules: each rule must be a (pattern template) list".into(),
            }
            .with_offset(rule_expr.pos.offset));
        };

        let [pattern, template] = rule_parts.as_slice() else {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules: each rule must contain exactly a pattern and template"
                    .into(),
            }
            .with_offset(rule_expr.pos.offset));
        };

        rules.push(MacroRule {
            pattern: pattern.clone(),
            template: template.clone(),
        });
    }

    Ok(Rc::new(SyntaxRulesMacro {
        name: name.into(),
        literals,
        rules,
        env: env.clone(),
        id: next_hygiene_id(),
    }))
}

fn expand_macro_invocation(
    transformer: &SyntaxRulesMacro,
    invocation: &Expr,
) -> Result<MacroExpansion, EvalError> {
    for rule in &transformer.rules {
        let mut bindings = MatchBindings::default();

        if match_pattern(&rule.pattern, invocation, transformer, &mut bindings)? {
            let mut state = ExpandState {
                transformer,
                bindings: &bindings,
                alias_names: HashMap::new(),
                aliases: Vec::new(),
            };
            let expr = expand_template(&rule.template, &mut state, &HashMap::new(), None)?;

            return Ok(MacroExpansion {
                expr,
                aliases: state.aliases,
            });
        }
    }

    Err(EvalError::InvalidSyntax {
        message: format!("{}: no matching syntax-rules pattern", transformer.name),
    }
    .with_offset(invocation.pos.offset))
}

fn match_pattern(
    pattern: &Expr,
    target: &Expr,
    transformer: &SyntaxRulesMacro,
    bindings: &mut MatchBindings,
) -> Result<bool, EvalError> {
    match (&pattern.kind, &target.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => Ok(left == right),
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => Ok(left == right),
        (ExprKind::String(left), ExprKind::String(right)) => Ok(left == right),
        (ExprKind::Char(left), ExprKind::Char(right)) => Ok(left == right),
        (ExprKind::Symbol(name), _) => {
            match_pattern_symbol(name, pattern.pos, target, transformer, bindings)
        }
        (ExprKind::List(patterns), ExprKind::List(targets)) => {
            match_pattern_list(patterns, targets, transformer, bindings)
        }
        _ => Ok(false),
    }
}

fn match_pattern_symbol(
    name: &str,
    pos: SourcePos,
    target: &Expr,
    transformer: &SyntaxRulesMacro,
    bindings: &mut MatchBindings,
) -> Result<bool, EvalError> {
    if name == "..." {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules: unexpected ellipsis in pattern".into(),
        }
        .with_offset(pos.offset));
    }

    if name == transformer.name || transformer.literals.contains(name) {
        return Ok(matches!(&target.kind, ExprKind::Symbol(target_name) if target_name == name));
    }

    if let Some(existing) = bindings.single.get(name) {
        return Ok(existing == target);
    }

    if let Some(existing) = bindings.repeated.get(name) {
        return Ok(existing.as_slice() == [target.clone()]);
    }

    bindings.single.insert(name.into(), target.clone());
    Ok(true)
}

fn match_pattern_list(
    patterns: &[Expr],
    targets: &[Expr],
    transformer: &SyntaxRulesMacro,
    bindings: &mut MatchBindings,
) -> Result<bool, EvalError> {
    if let Some(index) = ellipsis_index(patterns) {
        let repeated_pattern = &patterns[index];
        let suffix = &patterns[index + 2..];

        if targets.len() < index + suffix.len() {
            return Ok(false);
        }

        for (pattern, target) in patterns[..index].iter().zip(&targets[..index]) {
            if !match_pattern(pattern, target, transformer, bindings)? {
                return Ok(false);
            }
        }

        let repeated_targets = &targets[index..targets.len() - suffix.len()];
        if !match_repeated_pattern(repeated_pattern, repeated_targets, transformer, bindings)? {
            return Ok(false);
        }

        for (pattern, target) in suffix.iter().zip(&targets[targets.len() - suffix.len()..]) {
            if !match_pattern(pattern, target, transformer, bindings)? {
                return Ok(false);
            }
        }

        Ok(true)
    } else {
        if patterns.len() != targets.len() {
            return Ok(false);
        }

        for (pattern, target) in patterns.iter().zip(targets) {
            if !match_pattern(pattern, target, transformer, bindings)? {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

fn match_repeated_pattern(
    pattern: &Expr,
    targets: &[Expr],
    transformer: &SyntaxRulesMacro,
    bindings: &mut MatchBindings,
) -> Result<bool, EvalError> {
    match &pattern.kind {
        ExprKind::Symbol(name)
            if name != "..."
                && name.as_str() != transformer.name
                && !transformer.literals.contains(name) =>
        {
            if let Some(existing) = bindings.repeated.get(name) {
                return Ok(existing.as_slice() == targets);
            }

            if let Some(existing) = bindings.single.get(name) {
                return Ok(targets.len() == 1 && existing == &targets[0]);
            }

            bindings.repeated.insert(name.clone(), targets.to_vec());
            Ok(true)
        }
        _ => {
            for target in targets {
                if !match_pattern(pattern, target, transformer, bindings)? {
                    return Ok(false);
                }
            }

            Ok(true)
        }
    }
}

fn ellipsis_index(items: &[Expr]) -> Option<usize> {
    items
        .windows(2)
        .position(|window| matches!(&window[1].kind, ExprKind::Symbol(name) if name == "..."))
}

fn expand_template(
    template: &Expr,
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::String(_) | ExprKind::Char(_) => {
            Ok(template.clone())
        }
        ExprKind::Symbol(name) => {
            expand_template_symbol(name, template.pos, state, bound_renames, repetition_index)
        }
        ExprKind::List(items) => {
            expand_template_list(template.pos, items, state, bound_renames, repetition_index)
        }
    }
}

fn expand_template_symbol(
    name: &str,
    pos: SourcePos,
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if name == "..." {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules: unexpected ellipsis in template".into(),
        }
        .with_offset(pos.offset));
    }

    if let Some(captured) = state.bindings.single.get(name) {
        return Ok(captured.clone());
    }

    if let Some(captured) = state.bindings.repeated.get(name) {
        let Some(index) = repetition_index else {
            return Err(EvalError::InvalidSyntax {
                message: format!(
                    "syntax-rules: repeated pattern variable `{name}` used without ellipsis"
                ),
            }
            .with_offset(pos.offset));
        };

        return captured.get(index).cloned().ok_or_else(|| {
            EvalError::InvalidSyntax {
                message: format!("syntax-rules: invalid ellipsis expansion for `{name}`"),
            }
            .with_offset(pos.offset)
        });
    }

    if let Some(renamed) = bound_renames.get(name) {
        return Ok(Expr::symbol(renamed.clone(), pos));
    }

    if is_reserved_template_identifier(name) {
        return Ok(Expr::symbol(name, pos));
    }

    if let Some(alias) = state.alias_names.get(name) {
        return Ok(Expr::symbol(alias.clone(), pos));
    }

    if let Some(value) = state.transformer.env.lookup(name) {
        let alias = fresh_macro_identifier(state.transformer.id);
        state.alias_names.insert(name.into(), alias.clone());
        state.aliases.push((alias.clone(), value));
        return Ok(Expr::symbol(alias, pos));
    }

    Ok(Expr::symbol(name, pos))
}

fn expand_template_list(
    pos: SourcePos,
    items: &[Expr],
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(ExprKind::Symbol(name)) = items.first().map(|expr| &expr.kind) {
        match name.as_str() {
            "let" => {
                return expand_let_template(pos, items, state, bound_renames, repetition_index)
            }
            "lambda" => {
                return expand_lambda_template(pos, items, state, bound_renames, repetition_index)
            }
            "case-lambda" => {
                return expand_case_lambda_template(
                    pos,
                    items,
                    state,
                    bound_renames,
                    repetition_index,
                )
            }
            _ => {}
        }
    }

    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if index + 1 < items.len()
            && matches!(&items[index + 1].kind, ExprKind::Symbol(name) if name == "...")
        {
            let repeat_len = template_repetition_len(&items[index], state.bindings)?.ok_or_else(
                || EvalError::InvalidSyntax {
                    message: "syntax-rules: ellipsis template must contain a repeated pattern variable"
                        .into(),
                }
                .with_offset(items[index].pos.offset),
            )?;

            for repeat_index in 0..repeat_len {
                expanded.push(expand_template(
                    &items[index],
                    state,
                    bound_renames,
                    Some(repeat_index),
                )?);
            }

            index += 2;
            continue;
        }

        expanded.push(expand_template(
            &items[index],
            state,
            bound_renames,
            repetition_index,
        )?);
        index += 1;
    }

    Ok(Expr::new(ExprKind::List(expanded), pos))
}

fn expand_let_template(
    pos: SourcePos,
    items: &[Expr],
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
    }

    let mut expanded = vec![Expr::symbol("let", items[0].pos)];
    let mut body_scope = bound_renames.clone();
    let mut binding_index = 1;

    if matches!(&items[1].kind, ExprKind::Symbol(_)) && items.len() >= 4 {
        let loop_name =
            expand_binding_identifier(&items[1], state, &mut body_scope, repetition_index)?;
        expanded.push(loop_name);
        binding_index = 2;
    }

    let bindings_expr = &items[binding_index];
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
    };

    let mut expanded_bindings = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(parts) = &binding.kind else {
            return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
        };

        if parts.len() != 2 {
            return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
        }

        let init = expand_template(&parts[1], state, bound_renames, repetition_index)?;
        let name = expand_binding_identifier(&parts[0], state, &mut body_scope, repetition_index)?;
        expanded_bindings.push(Expr::new(ExprKind::List(vec![name, init]), binding.pos));
    }

    expanded.push(Expr::new(
        ExprKind::List(expanded_bindings),
        bindings_expr.pos,
    ));

    for body_expr in &items[binding_index + 1..] {
        expanded.push(expand_template(
            body_expr,
            state,
            &body_scope,
            repetition_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded), pos))
}

fn expand_lambda_template(
    pos: SourcePos,
    items: &[Expr],
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some((_, rest)) = items.split_first() else {
        return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
    };
    let Some((params_expr, body)) = rest.split_first() else {
        return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
    };

    let mut body_scope = bound_renames.clone();
    let params = expand_lambda_params(params_expr, state, &mut body_scope, repetition_index)?;
    let mut expanded = vec![Expr::symbol("lambda", items[0].pos), params];

    for body_expr in body {
        expanded.push(expand_template(
            body_expr,
            state,
            &body_scope,
            repetition_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded), pos))
}

fn expand_case_lambda_template(
    pos: SourcePos,
    items: &[Expr],
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some((_, clause_exprs)) = items.split_first() else {
        return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
    };

    let mut expanded = vec![Expr::symbol("case-lambda", items[0].pos)];
    for clause_expr in clause_exprs {
        let ExprKind::List(clause_items) = &clause_expr.kind else {
            return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
        };

        let Some((params_expr, body)) = clause_items.split_first() else {
            return expand_plain_template_list(pos, items, state, bound_renames, repetition_index);
        };

        let mut body_scope = bound_renames.clone();
        let params = expand_lambda_params(params_expr, state, &mut body_scope, repetition_index)?;
        let mut expanded_clause = vec![params];

        for body_expr in body {
            expanded_clause.push(expand_template(
                body_expr,
                state,
                &body_scope,
                repetition_index,
            )?);
        }

        expanded.push(Expr::new(ExprKind::List(expanded_clause), clause_expr.pos));
    }

    Ok(Expr::new(ExprKind::List(expanded), pos))
}

fn expand_lambda_params(
    expr: &Expr,
    state: &mut ExpandState<'_>,
    bound_renames: &mut HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &expr.kind {
        ExprKind::Symbol(_) => {
            expand_binding_identifier(expr, state, bound_renames, repetition_index)
        }
        ExprKind::List(items) => {
            let mut expanded = Vec::with_capacity(items.len());
            for item in items {
                if matches!(&item.kind, ExprKind::Symbol(name) if name == ".") {
                    expanded.push(item.clone());
                    continue;
                }

                expanded.push(expand_binding_identifier(
                    item,
                    state,
                    bound_renames,
                    repetition_index,
                )?);
            }

            Ok(Expr::new(ExprKind::List(expanded), expr.pos))
        }
        _ => expand_template(expr, state, bound_renames, repetition_index),
    }
}

fn expand_binding_identifier(
    expr: &Expr,
    state: &mut ExpandState<'_>,
    bound_renames: &mut HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let ExprKind::Symbol(name) = &expr.kind else {
        return expand_template(expr, state, bound_renames, repetition_index);
    };

    if let Some(captured) = state.bindings.single.get(name) {
        return Ok(captured.clone());
    }

    if let Some(captured) = state.bindings.repeated.get(name) {
        let Some(index) = repetition_index else {
            return Err(EvalError::InvalidSyntax {
                message: format!(
                    "syntax-rules: repeated pattern variable `{name}` used without ellipsis"
                ),
            }
            .with_offset(expr.pos.offset));
        };

        return captured.get(index).cloned().ok_or_else(|| {
            EvalError::InvalidSyntax {
                message: format!("syntax-rules: invalid ellipsis expansion for `{name}`"),
            }
            .with_offset(expr.pos.offset)
        });
    }

    let renamed = fresh_macro_identifier(state.transformer.id);
    bound_renames.insert(name.clone(), renamed.clone());
    Ok(Expr::symbol(renamed, expr.pos))
}

fn expand_plain_template_list(
    pos: SourcePos,
    items: &[Expr],
    state: &mut ExpandState<'_>,
    bound_renames: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let mut expanded = Vec::with_capacity(items.len());

    for item in items {
        expanded.push(expand_template(
            item,
            state,
            bound_renames,
            repetition_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded), pos))
}

fn template_repetition_len(
    expr: &Expr,
    bindings: &MatchBindings,
) -> Result<Option<usize>, EvalError> {
    let mut len = None;
    collect_template_repetition_len(expr, bindings, &mut len)?;
    Ok(len)
}

fn collect_template_repetition_len(
    expr: &Expr,
    bindings: &MatchBindings,
    len: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &expr.kind {
        ExprKind::Symbol(name) => {
            if let Some(values) = bindings.repeated.get(name) {
                match len {
                    Some(existing) if *existing != values.len() => {
                        return Err(EvalError::InvalidSyntax {
                            message: "syntax-rules: repeated template variables must have matching lengths"
                                .into(),
                        }
                        .with_offset(expr.pos.offset))
                    }
                    Some(_) => {}
                    None => *len = Some(values.len()),
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_template_repetition_len(item, bindings, len)?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn fresh_macro_identifier(macro_id: usize) -> String {
    format!("__macro_{macro_id}_{}", next_hygiene_id())
}

fn is_reserved_template_identifier(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-record-type"
            | "define-syntax"
            | "set!"
            | "if"
            | "quote"
            | "lambda"
            | "case-lambda"
            | "and"
            | "or"
            | "begin"
            | "let"
            | "let*"
            | "letrec"
            | "letrec*"
            | "cond"
            | "case"
            | "do"
            | "else"
            | "syntax-rules"
            | "."
    )
}

fn eval_set(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [name_expr, value_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "set!".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "set!: expected symbol".into(),
        }
        .with_offset(name_expr.pos.offset));
    };

    let value = eval_expr(value_expr, env)?;
    if env.set(name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundVariable { name: name.clone() }.with_offset(name_expr.pos.offset))
    }
}

fn eval_if(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match arguments {
        [condition, consequent] => {
            if eval_expr(condition, env)?.is_truthy() {
                eval_expr(consequent, env)
            } else {
                Ok(Value::Boolean(false))
            }
        }
        [condition, consequent, alternate] => {
            if eval_expr(condition, env)?.is_truthy() {
                eval_expr(consequent, env)
            } else {
                eval_expr(alternate, env)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset)),
    }
}

fn eval_quote(pos: SourcePos, arguments: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    Ok(quote_expr(quoted))
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Number(value) => Value::Number(*value),
        ExprKind::Boolean(value) => Value::Boolean(*value),
        ExprKind::String(value) => Value::String(SchemeString::immutable(value.clone())),
        ExprKind::Char(value) => Value::Char(*value),
        ExprKind::Symbol(name) => Value::Symbol(name.clone()),
        ExprKind::List(items) => make_proper_list(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (param_list, body) = arguments.split_first().ok_or_else(|| {
        EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset)
    })?;

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    Ok(Value::Procedure(Rc::new(Closure {
        params: parse_param_list(param_list)?,
        body: Rc::from(body.to_vec()),
        env: env.clone(),
    })))
}

fn eval_case_lambda(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut clauses = Vec::with_capacity(arguments.len());

    for clause_expr in arguments {
        let ExprKind::List(items) = &clause_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "case-lambda: clauses must be lists".into(),
            }
            .with_offset(clause_expr.pos.offset));
        };

        let Some((params_expr, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "case-lambda: each clause must include parameters and body".into(),
            }
            .with_offset(clause_expr.pos.offset));
        };

        if body.is_empty() {
            return Err(EvalError::InvalidSyntax {
                message: "case-lambda: each clause must include parameters and body".into(),
            }
            .with_offset(clause_expr.pos.offset));
        }

        clauses.push(Rc::new(Closure {
            params: parse_param_list(params_expr)?,
            body: Rc::from(body.to_vec()),
            env: env.clone(),
        }));
    }

    Ok(Value::CaseProcedure(Rc::new(CaseClosure { clauses })))
}

fn eval_begin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(arguments, env)
}

fn eval_let(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((first, rest)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let(name, rest, env, pos),
        _ => eval_plain_let(first, rest, env),
    }
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(bindings_expr.pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let let_env = Env::child(env);

    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env)
}

fn eval_named_let(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 2,
        }
        .with_offset(pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let named_env = Env::child(env);
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let closure = Rc::new(Closure {
        params: ParameterSpec {
            required: params,
            rest: None,
        },
        body: Rc::from(body.to_vec()),
        env: named_env.clone(),
    });

    named_env.define(name.into(), Value::Procedure(closure.clone()));

    let values = eval_binding_values(&bindings, &named_env)?;
    apply_closure_values(closure, values, pos)
}

fn eval_letrec(
    pos: SourcePos,
    arguments: &[Expr],
    env: &EnvRef,
    sequential: bool,
) -> Result<Value, EvalError> {
    let form_name = if sequential { "letrec*" } else { "letrec" };
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: form_name.into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: form_name.into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, form_name)?;
    let letrec_env = Env::child(env);

    for (name, _) in &bindings {
        letrec_env.define(name.clone(), Value::Uninitialized(name.clone()));
    }

    if sequential {
        for (name, expression) in &bindings {
            let value = eval_expr(expression, &letrec_env)?;
            let updated = letrec_env.set(name, value);
            debug_assert!(updated);
        }
    } else {
        let values = bindings
            .iter()
            .map(|(_, expression)| eval_expr(expression, &letrec_env))
            .collect::<Result<Vec<_>, _>>()?;

        for ((name, _), value) in bindings.iter().zip(values) {
            let updated = letrec_env.set(name, value);
            debug_assert!(updated);
        }
    }

    eval_sequence(body, &letrec_env)
}

fn eval_case(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    if clauses.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    }

    let key = eval_expr(key_expr, env)?;

    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "case: clauses must be lists".into(),
            }
            .with_offset(clause.pos.offset));
        };

        let Some((datum_expr, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "case: clauses cannot be empty".into(),
            }
            .with_offset(clause.pos.offset));
        };

        if matches!(&datum_expr.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "case: else clause must be last".into(),
                }
                .with_offset(datum_expr.pos.offset));
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env)
            };
        }

        let ExprKind::List(datums) = &datum_expr.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "case: expected datum list or else".into(),
            }
            .with_offset(datum_expr.pos.offset));
        };

        if datums
            .iter()
            .map(quote_expr)
            .any(|datum| value_eqv(&key, &datum))
        {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    Ok(Value::Boolean(false))
}

#[derive(Clone)]
struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn eval_do(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [bindings_expr, test_expr, body @ ..] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "do".into(),
            expected: "at least 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test, results) = parse_do_test(test_expr)?;
    let init_values = bindings
        .iter()
        .map(|binding| eval_expr(&binding.init, env))
        .collect::<Result<Vec<_>, _>>()?;
    let loop_env = Env::child(env);

    for (binding, value) in bindings.iter().zip(init_values) {
        loop_env.define(binding.name.clone(), value);
    }

    loop {
        if eval_expr(&test, &loop_env)?.is_truthy() {
            return if results.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(&results, &loop_env)
            };
        }

        for expr in body {
            eval_expr(expr, &loop_env)?;
        }

        let next_values = bindings
            .iter()
            .map(|binding| match &binding.step {
                Some(step) => eval_expr(step, &loop_env),
                None => loop_env.lookup(&binding.name).ok_or_else(|| {
                    EvalError::UnboundVariable {
                        name: binding.name.clone(),
                    }
                    .with_offset(pos.offset)
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (binding, value) in bindings.iter().zip(next_values) {
            let updated = loop_env.set(&binding.name, value);
            debug_assert!(updated);
        }
    }
}

fn parse_do_bindings(bindings_expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "do: expected binding list".into(),
        }
        .with_offset(bindings_expr.pos.offset));
    };

    bindings
        .iter()
        .map(|binding| {
            let ExprKind::List(parts) = &binding.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "do: each binding must be a list".into(),
                }
                .with_offset(binding.pos.offset));
            };

            let ([name_expr, init_expr] | [name_expr, init_expr, _]) = parts.as_slice() else {
                return Err(EvalError::InvalidSyntax {
                    message: "do: each binding must be (name init) or (name init step)".into(),
                }
                .with_offset(binding.pos.offset));
            };

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "do: binding name must be a symbol".into(),
                }
                .with_offset(name_expr.pos.offset));
            };

            Ok(DoBinding {
                name: name.clone(),
                init: init_expr.clone(),
                step: parts.get(2).cloned(),
            })
        })
        .collect()
}

fn parse_do_test(test_expr: &Expr) -> Result<(Expr, Vec<Expr>), EvalError> {
    let ExprKind::List(parts) = &test_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "do: expected termination clause".into(),
        }
        .with_offset(test_expr.pos.offset));
    };

    let Some((test, results)) = parts.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "do: termination clause cannot be empty".into(),
        }
        .with_offset(test_expr.pos.offset));
    };

    Ok((test.clone(), results.to_vec()))
}

fn parse_bindings(bindings_expr: &Expr, form_name: &str) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: format!("{form_name}: expected binding list"),
        }
        .with_offset(bindings_expr.pos.offset));
    };

    bindings
        .iter()
        .map(|binding| match &binding.kind {
            ExprKind::List(parts) if parts.len() == 2 => match &parts[0].kind {
                ExprKind::Symbol(name) => Ok((name.clone(), parts[1].clone())),
                _ => Err(EvalError::InvalidSyntax {
                    message: format!("{form_name}: binding name must be a symbol"),
                }
                .with_offset(parts[0].pos.offset)),
            },
            _ => Err(EvalError::InvalidSyntax {
                message: format!("{form_name}: each binding must have exactly 2 elements"),
            }
            .with_offset(binding.pos.offset)),
        })
        .collect()
}

fn eval_binding_values(bindings: &[(String, Expr)], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expression)| eval_expr(expression, env))
        .collect()
}

fn eval_cond(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in arguments.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "cond: clauses must be lists".into(),
            }
            .with_offset(clause.pos.offset));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "cond: clauses cannot be empty".into(),
            }
            .with_offset(clause.pos.offset));
        };

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != arguments.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond: else clause must be last".into(),
                }
                .with_offset(test.pos.offset));
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env)
            };
        }

        let value = eval_expr(test, env)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn parse_param_list(expr: &Expr) -> Result<ParameterSpec, EvalError> {
    match &expr.kind {
        ExprKind::List(params) => parse_params(params),
        ExprKind::Symbol(name) => Ok(ParameterSpec {
            required: Vec::new(),
            rest: Some(name.clone()),
        }),
        _ => Err(EvalError::InvalidSyntax {
            message: "lambda: expected parameter list".into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn parse_params(params: &[Expr]) -> Result<ParameterSpec, EvalError> {
    let mut required = Vec::new();
    let mut index = 0;

    while index < params.len() {
        match &params[index].kind {
            ExprKind::Symbol(name) if name == "." => {
                if index + 2 != params.len() {
                    return Err(EvalError::InvalidSyntax {
                        message: "lambda: invalid parameter list".into(),
                    }
                    .with_offset(params[index].pos.offset));
                }

                let rest_param = match &params[index + 1].kind {
                    ExprKind::Symbol(name) if name != "." => name.clone(),
                    _ => {
                        return Err(EvalError::InvalidSyntax {
                            message: "lambda: parameters must be symbols".into(),
                        }
                        .with_offset(params[index + 1].pos.offset))
                    }
                };

                return Ok(ParameterSpec {
                    required,
                    rest: Some(rest_param),
                });
            }
            ExprKind::Symbol(name) => required.push(name.clone()),
            _ => {
                return Err(EvalError::InvalidSyntax {
                    message: "lambda: parameters must be symbols".into(),
                }
                .with_offset(params[index].pos.offset))
            }
        }

        index += 1;
    }

    Ok(ParameterSpec {
        required,
        rest: None,
    })
}

fn parameter_spec_accepts(params: &ParameterSpec, argument_count: usize) -> bool {
    if params.rest.is_some() {
        argument_count >= params.required.len()
    } else {
        argument_count == params.required.len()
    }
}

fn format_case_lambda_arity(closure: &CaseClosure) -> String {
    let mut parts = Vec::<String>::new();

    for clause in &closure.clauses {
        let description = if clause.params.rest.is_some() {
            format!("at least {}", clause.params.required.len())
        } else {
            format!("exactly {}", clause.params.required.len())
        };

        if !parts.contains(&description) {
            parts.push(description);
        }
    }

    if parts.is_empty() {
        "no matching clauses".into()
    } else {
        parts.join(" or ")
    }
}

fn eval_apply_builtin(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let Some((procedure_expr, rest_arguments)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if rest_arguments.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let procedure = eval_expr(procedure_expr, env)?;
    let (list_expr, prefix_exprs) = rest_arguments
        .split_last()
        .expect("rest arguments are known to be non-empty");
    let mut values = eval_args(prefix_exprs, env)?;

    let rest_values = eval_expr(list_expr, env)?;
    values.extend(
        collect_list_items(&rest_values)
            .map_err(|error| list_access_error(error, list_expr.pos))?,
    );

    apply_value_with_values(procedure, values, env, procedure_expr.pos)
}

fn apply_value_with_values(
    value: Value,
    argument_values: Vec<Value>,
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match value {
        Value::Builtin(_)
        | Value::NativeProcedure(_)
        | Value::Procedure(_)
        | Value::CaseProcedure(_)
        | Value::Continuation(_) => {
            let apply_env = Env::child(env);
            let procedure_name = "__apply_procedure".to_string();
            apply_env.define(procedure_name.clone(), value);

            let mut application = Vec::with_capacity(argument_values.len() + 1);
            application.push(Expr::symbol(procedure_name, call_pos));

            for (index, argument) in argument_values.into_iter().enumerate() {
                let arg_name = format!("__apply_arg_{index}");
                apply_env.define(arg_name.clone(), argument);
                application.push(Expr::symbol(arg_name, call_pos));
            }

            eval_application(call_pos, &application, &apply_env)
        }
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }
        .with_offset(call_pos.offset)),
    }
}

fn eval_add(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Number(
        numbers
            .into_iter()
            .fold(Number::integer(0), |acc, value| acc.add(value)),
    ))
}

fn eval_eq(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [left_expr, right_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "eq?".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let left = eval_expr(left_expr, env)?;
    let right = eval_expr(right_expr, env)?;
    Ok(Value::Boolean(value_eqv(&left, &right)))
}

fn eval_eqv(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [left_expr, right_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "eqv?".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let left = eval_expr(left_expr, env)?;
    let right = eval_expr(right_expr, env)?;
    Ok(Value::Boolean(value_eqv(&left, &right)))
}

fn eval_equal(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [left_expr, right_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "equal?".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let left = eval_expr(left_expr, env)?;
    let right = eval_expr(right_expr, env)?;
    Ok(Value::Boolean(value_equal(&left, &right)))
}

fn eval_sub(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .with_offset(call_pos.offset)),
        [value] => Ok(Value::Number(value.negate())),
        [first, rest @ ..] => Ok(Value::Number(
            rest.iter()
                .copied()
                .fold(*first, |acc, value| acc.sub(value)),
        )),
    }
}

fn eval_mul(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Number(
        numbers
            .into_iter()
            .fold(Number::integer(1), |acc, value| acc.mul(value)),
    ))
}

fn eval_div(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let Some((first_expr, rest)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let mut total = eval_number(first_expr, env)?;

    for divisor_expr in rest {
        let divisor = eval_number(divisor_expr, env)?;
        if divisor.is_zero() {
            return Err(EvalError::DivisionByZero.with_offset(divisor_expr.pos.offset));
        }
        total = total.div(divisor);
    }

    Ok(Value::Number(total))
}

fn eval_compare(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    predicate: impl Fn(Number, Number) -> bool,
) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

    if numbers.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        }
        .with_offset(call_pos.offset));
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn eval_not(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    if arguments.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    }

    let value = eval_expr(&arguments[0], env)?;
    Ok(Value::Boolean(!value.is_truthy()))
}

fn eval_abs(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "abs".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Number(eval_number(expr, env)?.abs()))
}

fn eval_modulo(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_division_operands("modulo", arguments, env, call_pos)?;
    let remainder = dividend.checked_rem(divisor).ok_or_else(|| {
        EvalError::InvalidArgument {
            message: "modulo: integer overflow".into(),
        }
        .with_offset(call_pos.offset)
    })?;
    let value = if remainder != 0 && (remainder > 0) != (divisor > 0) {
        remainder + divisor
    } else {
        remainder
    };
    Ok(Value::Number(Number::integer(value)))
}

fn eval_remainder(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_division_operands("remainder", arguments, env, call_pos)?;
    let value = dividend.checked_rem(divisor).ok_or_else(|| {
        EvalError::InvalidArgument {
            message: "remainder: integer overflow".into(),
        }
        .with_offset(call_pos.offset)
    })?;
    Ok(Value::Number(Number::integer(value)))
}

fn eval_quotient(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_division_operands("quotient", arguments, env, call_pos)?;
    let value = dividend.checked_div(divisor).ok_or_else(|| {
        EvalError::InvalidArgument {
            message: "quotient: integer overflow".into(),
        }
        .with_offset(call_pos.offset)
    })?;
    Ok(Value::Number(Number::integer(value)))
}

fn eval_min(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let mut numbers = eval_number_args(arguments, env)?.into_iter();
    let first = numbers.next().ok_or_else(|| {
        EvalError::WrongArgCount {
            name: "min".into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .with_offset(call_pos.offset)
    })?;
    let value = numbers.fold(
        first,
        |acc, number| {
            if number.less_than(acc) {
                number
            } else {
                acc
            }
        },
    );
    Ok(Value::Number(value))
}

fn eval_max(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let mut numbers = eval_number_args(arguments, env)?.into_iter();
    let first = numbers.next().ok_or_else(|| {
        EvalError::WrongArgCount {
            name: "max".into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .with_offset(call_pos.offset)
    })?;
    let value = numbers.fold(first, |acc, number| {
        if number.greater_than(acc) {
            number
        } else {
            acc
        }
    });
    Ok(Value::Number(value))
}

fn eval_expt(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [base_expr, exponent_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "expt".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let base = eval_exact_integer(base_expr, env)?;
    let exponent = eval_exact_integer(exponent_expr, env)?;
    if exponent < 0 {
        return Err(EvalError::InvalidArgument {
            message: "expt: exponent must be non-negative".into(),
        }
        .with_offset(exponent_expr.pos.offset));
    }

    let value = base.checked_pow(exponent as u32).ok_or_else(|| {
        EvalError::InvalidArgument {
            message: "expt: integer overflow".into(),
        }
        .with_offset(call_pos.offset)
    })?;
    Ok(Value::Number(Number::integer(value)))
}

fn eval_gcd(arguments: &[Expr], env: &EnvRef, _call_pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = 0_i64;
    for argument in arguments {
        result = gcd_i64(result, eval_exact_integer(argument, env)?);
    }

    Ok(Value::Number(Number::integer(result.abs())))
}

fn eval_lcm(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = 1_i64;
    for argument in arguments {
        let value = eval_exact_integer(argument, env)?;
        if result == 0 || value == 0 {
            result = 0;
            continue;
        }

        let gcd = gcd_i64(result, value);
        let scaled = i128::from(result / gcd) * i128::from(value);
        let scaled = scaled.abs();
        result = i64::try_from(scaled).map_err(|_| {
            EvalError::InvalidArgument {
                message: "lcm: integer overflow".into(),
            }
            .with_offset(call_pos.offset)
        })?;
    }

    Ok(Value::Number(Number::integer(result)))
}

fn eval_truncate(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "truncate".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = match eval_number(expr, env)? {
        Number::Exact { num, den } => Value::Number(Number::integer(num / den)),
        Number::Inexact(value) => Value::Number(Number::Inexact(value.trunc())),
    };

    Ok(value)
}

fn eval_round(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "round".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = match eval_number(expr, env)? {
        Number::Exact { num, den } => {
            let quotient = num / den;
            let remainder = num % den;
            let twice_remainder = i128::from(remainder.abs()) * 2;
            let denominator = i128::from(den.abs());
            let rounded = if twice_remainder < denominator {
                quotient
            } else if twice_remainder > denominator {
                quotient + num.signum()
            } else if quotient % 2 == 0 {
                quotient
            } else {
                quotient + num.signum()
            };
            Value::Number(Number::integer(rounded))
        }
        Number::Inexact(value) => Value::Number(Number::Inexact(value.round())),
    };

    Ok(value)
}

fn eval_number_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(Number) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Boolean(predicate(eval_number(expr, env)?)))
}

fn eval_integer_number_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(i64) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Boolean(predicate(eval_exact_integer(expr, env)?)))
}

fn eval_number_property(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(Number) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Boolean(predicate(eval_number(expr, env)?)))
}

fn eval_exact_to_inexact(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "exact->inexact".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Number(Number::Inexact(
        eval_number(expr, env)?.as_f64(),
    )))
}

fn eval_inexact_to_exact(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "inexact->exact".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let number = eval_number(expr, env)?;
    match number {
        exact @ Number::Exact { .. } => Ok(Value::Number(exact)),
        Number::Inexact(value) => parse_decimal_as_exact(&render_inexact(value))
            .map(Value::Number)
            .ok_or_else(|| {
                EvalError::InvalidArgument {
                    message: "inexact->exact: unsupported inexact value".into(),
                }
                .with_offset(expr.pos.offset)
            }),
    }
}

fn eval_numerator(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "numerator".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let number = eval_number(expr, env)?;
    let (numerator, _) = number.exact_parts().ok_or_else(|| {
        EvalError::InvalidArgument {
            message: "numerator: expected exact number".into(),
        }
        .with_offset(expr.pos.offset)
    })?;
    Ok(Value::Number(Number::integer(numerator)))
}

fn eval_denominator(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "denominator".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let number = eval_number(expr, env)?;
    let (_, denominator) = number.exact_parts().ok_or_else(|| {
        EvalError::InvalidArgument {
            message: "denominator: expected exact number".into(),
        }
        .with_offset(expr.pos.offset)
    })?;
    Ok(Value::Number(Number::integer(denominator)))
}

fn eval_cons(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [head_expr, tail_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let head = eval_expr(head_expr, env)?;
    let tail = eval_expr(tail_expr, env)?;

    Ok(Value::Pair(SchemePair::new(head, tail)))
}

fn eval_car(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "car".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(pair) => Ok(pair.car()),
        Value::List(_) => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        }
        .with_offset(list_expr.pos.offset)),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: other.kind().into(),
        }
        .with_offset(list_expr.pos.offset)),
    }
}

fn eval_cdr(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "cdr".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) if !items.is_empty() => Ok(make_proper_list(items[1..].to_vec())),
        Value::Pair(pair) => Ok(pair.cdr()),
        Value::List(_) => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        }
        .with_offset(list_expr.pos.offset)),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: other.kind().into(),
        }
        .with_offset(list_expr.pos.offset)),
    }
}

fn eval_cxr(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    pattern: &str,
    name: &str,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let mut value = eval_expr(expr, env)?;
    for step in pattern.chars() {
        value = match step {
            'a' => match value {
                Value::List(items) if !items.is_empty() => items[0].clone(),
                Value::Pair(pair) => pair.car(),
                Value::List(_) => {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair".into(),
                        found: "list".into(),
                    }
                    .with_offset(expr.pos.offset))
                }
                other => {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair".into(),
                        found: other.kind().into(),
                    }
                    .with_offset(expr.pos.offset))
                }
            },
            'd' => match value {
                Value::List(items) if !items.is_empty() => make_proper_list(items[1..].to_vec()),
                Value::Pair(pair) => pair.cdr(),
                Value::List(_) => {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair".into(),
                        found: "list".into(),
                    }
                    .with_offset(expr.pos.offset))
                }
                other => {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair".into(),
                        found: other.kind().into(),
                    }
                    .with_offset(expr.pos.offset))
                }
            },
            _ => unreachable!("invalid cxr pattern"),
        };
    }

    Ok(value)
}

fn eval_set_car(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [pair_expr, value_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "set-car!".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let pair = eval_expr(pair_expr, env)?;
    let value = eval_expr(value_expr, env)?;

    match pair {
        Value::Pair(pair) => {
            pair.set_car(value);
            Ok(Value::Void)
        }
        Value::List(_) => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        }
        .with_offset(pair_expr.pos.offset)),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: other.kind().into(),
        }
        .with_offset(pair_expr.pos.offset)),
    }
}

fn eval_set_cdr(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [pair_expr, value_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "set-cdr!".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let pair = eval_expr(pair_expr, env)?;
    let value = eval_expr(value_expr, env)?;

    match pair {
        Value::Pair(pair) => {
            pair.set_cdr(value);
            Ok(Value::Void)
        }
        Value::List(_) => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        }
        .with_offset(pair_expr.pos.offset)),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: other.kind().into(),
        }
        .with_offset(pair_expr.pos.offset)),
    }
}

fn eval_null_pred(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "null?".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(list_expr, env)?;
    Ok(Value::Boolean(is_empty_list(&value)))
}

fn eval_list_builtin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    Ok(make_proper_list(eval_args(arguments, env)?))
}

fn eval_list_ref(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "list-ref".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let items = eval_list_items(list_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let len = items.len();
    if index >= len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(items[index].clone())
}

fn eval_list_tail(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "list-tail".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let index = eval_index(index_expr, env)?;
    let value = eval_expr(list_expr, env)?;
    let items =
        collect_list_items(&value).map_err(|error| list_access_error(error, list_expr.pos))?;
    let len = items.len();
    if index > len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    list_tail_value(&value, index).map_err(|error| match error {
        ListAccessError::Improper { .. } => {
            EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset)
        }
        ListAccessError::Circular => EvalError::CyclicList.with_offset(list_expr.pos.offset),
    })
}

fn eval_length(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(list_expr, env)?;
    let items =
        collect_list_items(&value).map_err(|error| list_access_error(error, list_expr.pos))?;
    Ok(Value::Number(Number::integer(items.len() as i64)))
}

fn eval_append(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        items.extend(
            collect_list_items(&value).map_err(|error| list_access_error(error, argument.pos))?,
        );
    }

    Ok(make_proper_list(items))
}

fn eval_reverse(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "reverse".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let mut items = eval_list_items(list_expr, env)?;
    items.reverse();
    Ok(make_proper_list(items))
}

fn eval_assoc(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    eval_assoc_like(arguments, env, call_pos, "assoc", value_equal)
}

fn eval_assv(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    eval_assoc_like(arguments, env, call_pos, "assv", value_eqv)
}

fn eval_assoc_like(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    name: &str,
    predicate: fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let [key_expr, alist_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let key = eval_expr(key_expr, env)?;
    let entries = eval_list_items(alist_expr, env)?;

    for entry in entries {
        if let Some(entry_key) = pair_head(&entry) {
            if predicate(&key, &entry_key) {
                return Ok(entry);
            }
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_member(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [key_expr, list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "member".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let key = eval_expr(key_expr, env)?;
    let mut current = eval_expr(list_expr, env)?;
    let mut seen = HashSet::new();

    loop {
        match current.clone() {
            Value::Pair(pair) => {
                let id = pair.id();
                if !seen.insert(id) {
                    return Err(EvalError::CyclicList.with_offset(list_expr.pos.offset));
                }

                let car = pair.car();
                if value_equal(&key, &car) {
                    return Ok(current);
                }

                current = pair.cdr();
            }
            Value::List(items) => {
                for (index, item) in items.iter().enumerate() {
                    if value_equal(&key, item) {
                        return Ok(make_proper_list(items[index..].to_vec()));
                    }
                }

                return Ok(Value::Boolean(false));
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    found: other.kind().into(),
                }
                .with_offset(list_expr.pos.offset))
            }
        }
    }
}

fn eval_map(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let Some((procedure_expr, list_exprs)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if list_exprs.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let procedure = eval_expr(procedure_expr, env)?;
    let lists: Vec<Vec<Value>> = list_exprs
        .iter()
        .map(|expr| eval_list_items(expr, env))
        .collect::<Result<_, _>>()?;

    let len = lists.first().map(Vec::len).unwrap_or(0);
    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            message: "map: all lists must have the same length".into(),
        }
        .with_offset(call_pos.offset));
    }

    let mut results = Vec::with_capacity(len);
    for index in 0..len {
        let row = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_value_with_values(
            procedure.clone(),
            row,
            env,
            procedure_expr.pos,
        )?);
    }

    Ok(make_proper_list(results))
}

fn eval_for_each(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let Some((procedure_expr, list_exprs)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "for-each".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if list_exprs.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "for-each".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let procedure = eval_expr(procedure_expr, env)?;
    let lists: Vec<Vec<Value>> = list_exprs
        .iter()
        .map(|expr| eval_list_items(expr, env))
        .collect::<Result<_, _>>()?;

    let len = lists.first().map(Vec::len).unwrap_or(0);
    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            message: "for-each: all lists must have the same length".into(),
        }
        .with_offset(call_pos.offset));
    }

    for index in 0..len {
        let row = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        apply_value_with_values(procedure.clone(), row, env, procedure_expr.pos)?;
    }

    Ok(Value::Void)
}

fn eval_display(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "display".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(expr, env)?;
    env.write_output(&value.render_display());
    Ok(Value::Void)
}

fn eval_write(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "write".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(expr, env)?;
    env.write_output(&value.render());
    Ok(Value::Void)
}

fn eval_newline(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    if !arguments.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline".into(),
            expected: "exactly 0".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    }

    env.write_output("\n");
    Ok(Value::Void)
}

fn eval_make_string(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let ([len_expr] | [len_expr, _]) = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "make-string".into(),
            expected: "1 or 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let len = eval_index(len_expr, env)?;
    let fill = match arguments.get(1) {
        Some(expr) => eval_char(expr, env)?,
        None => ' ',
    };

    Ok(Value::String(SchemeString::immutable(
        std::iter::repeat_n(fill, len).collect::<String>(),
    )))
}

fn eval_string_builtin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let chars = arguments
        .iter()
        .map(|argument| eval_char(argument, env))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::String(SchemeString::immutable(
        chars.into_iter().collect::<String>(),
    )))
}

fn eval_string_append(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut combined = String::new();

    for argument in arguments {
        combined.push_str(&eval_string(argument, env)?);
    }

    Ok(Value::String(SchemeString::immutable(combined)))
}

fn eval_string_length(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-length".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Number(Number::integer(
        eval_string(expr, env)?.chars().count() as i64,
    )))
}

fn eval_string_copy(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-copy".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(eval_string_value(expr, env)?.copy()))
}

fn eval_string_set(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr, index_expr, char_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-set!".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let string = eval_string_value(string_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let ch = eval_char(char_expr, env)?;

    match string.set_char(index, ch) {
        Ok(()) => Ok(Value::Void),
        Err(StringSetError::Immutable) => {
            Err(EvalError::ImmutableString.with_offset(string_expr.pos.offset))
        }
        Err(StringSetError::IndexOutOfBounds { len }) => {
            Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset))
        }
    }
}

fn eval_substring(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr, start_expr, end_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "substring".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_string(string_expr, env)?;
    let start = eval_index(start_expr, env)?;
    let end = eval_index(end_expr, env)?;
    let chars: Vec<char> = value.chars().collect();
    let len = chars.len();

    if start > len {
        return Err(
            EvalError::IndexOutOfBounds { index: start, len }.with_offset(start_expr.pos.offset)
        );
    }

    if end > len {
        return Err(
            EvalError::IndexOutOfBounds { index: end, len }.with_offset(end_expr.pos.offset)
        );
    }

    if start > end {
        return Err(EvalError::InvalidRange { start, end, len }.with_offset(start_expr.pos.offset));
    }

    Ok(Value::String(SchemeString::immutable(
        chars[start..end].iter().collect::<String>(),
    )))
}

fn eval_string_to_number(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string->number".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match parse_number_token(&eval_string(expr, env)?) {
        Some(value) => Ok(Value::Number(value)),
        None => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "number->string".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(SchemeString::immutable(
        eval_number(expr, env)?.render(),
    )))
}

fn eval_symbol_to_string(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(SchemeString::immutable(eval_symbol(
        expr, env,
    )?)))
}

fn eval_string_to_symbol(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Symbol(eval_string(expr, env)?))
}

fn eval_string_ref(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-ref".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_string(string_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let chars: Vec<char> = value.chars().collect();
    let len = chars.len();

    if index >= len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(Value::Char(chars[index]))
}

fn eval_string_to_list(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string->list".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(make_proper_list(
        eval_string(string_expr, env)?
            .chars()
            .map(Value::Char)
            .collect(),
    ))
}

fn eval_list_to_string(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "list->string".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let chars = eval_list_items(list_expr, env)?
        .into_iter()
        .map(|value| match value {
            Value::Char(ch) => Ok(ch),
            other => Err(EvalError::TypeMismatch {
                expected: "char".into(),
                found: other.kind().into(),
            }
            .with_offset(list_expr.pos.offset)),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::String(SchemeString::immutable(
        chars.into_iter().collect::<String>(),
    )))
}

fn eval_vector(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    Ok(Value::Vector(SchemeVector::new(eval_args(arguments, env)?)))
}

fn eval_make_vector(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let ([len_expr] | [len_expr, _]) = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "make-vector".into(),
            expected: "1 or 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let len = eval_index(len_expr, env)?;
    let fill = match arguments.get(1) {
        Some(expr) => eval_expr(expr, env)?,
        None => Value::Void,
    };

    Ok(Value::Vector(SchemeVector::new(vec![fill; len])))
}

fn eval_vector_ref(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [vector_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "vector-ref".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let vector = eval_vector_value(vector_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let len = vector.len();
    if index >= len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(vector
        .get(index)
        .expect("vector index already validated against length"))
}

fn eval_vector_set(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [vector_expr, index_expr, value_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "vector-set!".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let vector = eval_vector_value(vector_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let value = eval_expr(value_expr, env)?;

    match vector.set(index, value) {
        Ok(()) => Ok(Value::Void),
        Err(len) => {
            Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset))
        }
    }
}

fn eval_vector_length(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [vector_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "vector-length".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Number(Number::integer(
        eval_vector_value(vector_expr, env)?.len() as i64,
    )))
}

fn eval_vector_to_list(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [vector_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "vector->list".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(make_proper_list(
        eval_vector_value(vector_expr, env)?.items(),
    ))
}

fn eval_list_to_vector(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "list->vector".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Vector(SchemeVector::new(eval_list_items(
        list_expr, env,
    )?)))
}

fn eval_vector_value(expr: &Expr, env: &EnvRef) -> Result<SchemeVector, EvalError> {
    match eval_expr(expr, env)? {
        Value::Vector(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "vector".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_char_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(char) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Boolean(predicate(eval_char(expr, env)?)))
}

fn eval_char_transform(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    transform: impl Fn(char) -> char,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Char(transform(eval_char(expr, env)?)))
}

fn eval_char_compare(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let chars = arguments
        .iter()
        .map(|argument| eval_char(argument, env))
        .collect::<Result<Vec<_>, _>>()?;

    if chars.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: chars.len(),
        }
        .with_offset(call_pos.offset));
    }

    Ok(Value::Boolean(
        chars.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn eval_char_to_integer(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "char->integer".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Number(Number::integer(i64::from(
        eval_char(expr, env)? as u32,
    ))))
}

fn eval_integer_to_char(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "integer->char".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let code = eval_exact_integer(expr, env)?;
    let value = if code < 0 {
        None
    } else {
        char::from_u32(code as u32)
    };

    match value {
        Some(ch) => Ok(Value::Char(ch)),
        None => Err(EvalError::InvalidArgument {
            message: format!("integer->char: invalid Unicode scalar value {code}"),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_string_compare(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let strings = arguments
        .iter()
        .map(|argument| eval_string(argument, env))
        .collect::<Result<Vec<_>, _>>()?;

    if strings.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: strings.len(),
        }
        .with_offset(call_pos.offset));
    }

    Ok(Value::Boolean(
        strings.windows(2).all(|pair| predicate(&pair[0], &pair[1])),
    ))
}

fn eval_string_transform(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    transform: impl Fn(String) -> String,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(SchemeString::immutable(transform(
        eval_string(expr, env)?,
    ))))
}

fn eval_type_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(expr, env)?;
    Ok(Value::Boolean(predicate(&value)))
}

fn is_callable(value: &Value) -> bool {
    matches!(
        value,
        Value::Builtin(_)
            | Value::NativeProcedure(_)
            | Value::Procedure(_)
            | Value::CaseProcedure(_)
            | Value::Continuation(_)
    )
}

fn eval_and(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(false);

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_number_args(arguments: &[Expr], env: &EnvRef) -> Result<Vec<Number>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_number(argument, env))
        .collect()
}

fn eval_division_operands(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<(i64, i64), EvalError> {
    let [dividend_expr, divisor_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let dividend = eval_exact_integer(dividend_expr, env)?;
    let divisor = eval_exact_integer(divisor_expr, env)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero.with_offset(divisor_expr.pos.offset));
    }

    Ok((dividend, divisor))
}

fn eval_number(expr: &Expr, env: &EnvRef) -> Result<Number, EvalError> {
    match eval_expr(expr, env)? {
        Value::Number(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_exact_integer(expr: &Expr, env: &EnvRef) -> Result<i64, EvalError> {
    let value = eval_number(expr, env)?;
    value.exact_integer().ok_or_else(|| {
        EvalError::TypeMismatch {
            expected: "integer".into(),
            found: "number".into(),
        }
        .with_offset(expr.pos.offset)
    })
}

fn eval_string(expr: &Expr, env: &EnvRef) -> Result<String, EvalError> {
    match eval_expr(expr, env)? {
        Value::String(value) => Ok(value.as_string()),
        other => Err(EvalError::TypeMismatch {
            expected: "string".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_string_value(expr: &Expr, env: &EnvRef) -> Result<SchemeString, EvalError> {
    match eval_expr(expr, env)? {
        Value::String(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "string".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_symbol(expr: &Expr, env: &EnvRef) -> Result<String, EvalError> {
    match eval_expr(expr, env)? {
        Value::Symbol(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "symbol".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_char(expr: &Expr, env: &EnvRef) -> Result<char, EvalError> {
    match eval_expr(expr, env)? {
        Value::Char(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "char".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_index(expr: &Expr, env: &EnvRef) -> Result<usize, EvalError> {
    let index = eval_exact_integer(expr, env)?;
    if index < 0 {
        Err(EvalError::NegativeIndex { index }.with_offset(expr.pos.offset))
    } else {
        Ok(index as usize)
    }
}

fn eval_list_items(expr: &Expr, env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    let value = eval_expr(expr, env)?;
    collect_list_items(&value).map_err(|error| list_access_error(error, expr.pos))
}

fn is_pair(value: &Value) -> bool {
    matches!(value, Value::List(items) if !items.is_empty()) || matches!(value, Value::Pair(_))
}

fn pair_head(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) if !items.is_empty() => Some(items[0].clone()),
        Value::Pair(pair) => Some(pair.car()),
        _ => None,
    }
}

fn value_eqv(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    value_eqv_inner(left, right, &mut seen_pairs)
}

fn value_equal(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    value_equal_inner(left, right, &mut seen_pairs)
}

fn value_eqv_inner(left: &Value, right: &Value, seen_pairs: &mut HashSet<(usize, usize)>) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.equals(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.as_string() == right.as_string(),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(&left.inner, &right.inner),
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left_item, right_item)| {
                        value_eqv_inner(left_item, right_item, seen_pairs)
                    })
        }
        (Value::Pair(left_pair), Value::Pair(right_pair)) => {
            let ids = (left_pair.id(), right_pair.id());
            if !seen_pairs.insert(ids) {
                return true;
            }

            let (left_head, left_tail) = left_pair.parts();
            let (right_head, right_tail) = right_pair.parts();
            value_eqv_inner(&left_head, &right_head, seen_pairs)
                && value_eqv_inner(&left_tail, &right_tail, seen_pairs)
        }
        (Value::List(_), Value::Pair(_)) | (Value::Pair(_), Value::List(_)) => {
            match (collect_list_items(left), collect_list_items(right)) {
                (Ok(left_items), Ok(right_items)) => {
                    left_items.len() == right_items.len()
                        && left_items.iter().zip(right_items.iter()).all(
                            |(left_item, right_item)| {
                                value_eqv_inner(left_item, right_item, seen_pairs)
                            },
                        )
                }
                _ => false,
            }
        }
        (Value::Builtin(left), Value::Builtin(right)) => left.name() == right.name(),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => {
            native_procedure_equal(left, right)
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::CaseProcedure(left), Value::CaseProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => left.id() == right.id(),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Uninitialized(left), Value::Uninitialized(right)) => left == right,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn value_equal_inner(
    left: &Value,
    right: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.equals(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.as_string() == right.as_string(),
        (Value::Vector(left), Value::Vector(right)) => {
            let left_items = left.items();
            let right_items = right.items();
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left_item, right_item)| {
                        value_equal_inner(left_item, right_item, seen_pairs)
                    })
        }
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left_item, right_item)| {
                        value_equal_inner(left_item, right_item, seen_pairs)
                    })
        }
        (Value::Pair(left_pair), Value::Pair(right_pair)) => {
            let ids = (left_pair.id(), right_pair.id());
            if !seen_pairs.insert(ids) {
                return true;
            }

            let (left_head, left_tail) = left_pair.parts();
            let (right_head, right_tail) = right_pair.parts();
            value_equal_inner(&left_head, &right_head, seen_pairs)
                && value_equal_inner(&left_tail, &right_tail, seen_pairs)
        }
        (Value::List(_), Value::Pair(_)) | (Value::Pair(_), Value::List(_)) => {
            match (collect_list_items(left), collect_list_items(right)) {
                (Ok(left_items), Ok(right_items)) => {
                    left_items.len() == right_items.len()
                        && left_items.iter().zip(right_items.iter()).all(
                            |(left_item, right_item)| {
                                value_equal_inner(left_item, right_item, seen_pairs)
                            },
                        )
                }
                _ => false,
            }
        }
        (Value::Builtin(left), Value::Builtin(right)) => left.name() == right.name(),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => {
            native_procedure_equal(left, right)
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::CaseProcedure(left), Value::CaseProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => left.id() == right.id(),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Uninitialized(left), Value::Uninitialized(right)) => left == right,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn native_procedure_equal(left: &NativeProcedure, right: &NativeProcedure) -> bool {
    left.name() == right.name()
        && match (left, right) {
            (
                NativeProcedure::RecordConstructor {
                    record_type: left_type,
                    constructor_fields: left_fields,
                    ..
                },
                NativeProcedure::RecordConstructor {
                    record_type: right_type,
                    constructor_fields: right_fields,
                    ..
                },
            ) => left_type.id == right_type.id && left_fields == right_fields,
            (
                NativeProcedure::RecordPredicate {
                    record_type: left_type,
                    ..
                },
                NativeProcedure::RecordPredicate {
                    record_type: right_type,
                    ..
                },
            ) => left_type.id == right_type.id,
            (
                NativeProcedure::RecordAccessor {
                    record_type: left_type,
                    field_index: left_index,
                    ..
                },
                NativeProcedure::RecordAccessor {
                    record_type: right_type,
                    field_index: right_index,
                    ..
                },
            ) => left_type.id == right_type.id && left_index == right_index,
            _ => false,
        }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = Parser::new(input)
        .parse_program()
        .map_err(|err| err.resolve_positions(input))?;
    let output = Rc::new(RefCell::new(String::new()));
    let value = eval_program(&expressions, output).map_err(|err| err.resolve_positions(input))?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let expressions = Parser::new(input)
        .parse_program()
        .map_err(|err| err.resolve_positions(input))?;
    let output = Rc::new(RefCell::new(String::new()));
    let value =
        eval_program(&expressions, output.clone()).map_err(|err| err.resolve_positions(input))?;
    let captured_output = output.borrow().clone();
    Ok((value.render(), captured_output))
}

#[cfg(test)]
mod tests;
