pub mod error;

use std::{
    cell::{Cell, RefCell},
    cmp::Ordering,
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub use error::EvalError;
use error::SourcePos;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Number(Number, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Rational {
    fn from_integer(value: i64) -> Self {
        Self {
            numerator: value,
            denominator: 1,
        }
    }

    fn new(numerator: i64, denominator: i64) -> Self {
        Self::from_i128(numerator as i128, denominator as i128)
            .expect("rational literal should fit in i64")
    }

    fn from_i128(numerator: i128, denominator: i128) -> Option<Self> {
        if denominator == 0 {
            return None;
        }

        let mut numerator = numerator;
        let mut denominator = denominator;
        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        let gcd = gcd_i128(numerator, denominator);
        numerator /= gcd;
        denominator /= gcd;

        Some(Self {
            numerator: checked_i64(numerator)?,
            denominator: checked_i64(denominator)?,
        })
    }

    fn add(self, other: Self) -> Option<Self> {
        Self::from_i128(
            self.numerator as i128 * other.denominator as i128
                + other.numerator as i128 * self.denominator as i128,
            self.denominator as i128 * other.denominator as i128,
        )
    }

    fn sub(self, other: Self) -> Option<Self> {
        Self::from_i128(
            self.numerator as i128 * other.denominator as i128
                - other.numerator as i128 * self.denominator as i128,
            self.denominator as i128 * other.denominator as i128,
        )
    }

    fn mul(self, other: Self) -> Option<Self> {
        Self::from_i128(
            self.numerator as i128 * other.numerator as i128,
            self.denominator as i128 * other.denominator as i128,
        )
    }

    fn div(self, other: Self) -> Option<Self> {
        Self::from_i128(
            self.numerator as i128 * other.denominator as i128,
            self.denominator as i128 * other.numerator as i128,
        )
    }

    fn abs(self) -> Option<Self> {
        Self::from_i128((self.numerator as i128).abs(), self.denominator as i128)
    }

    fn cmp(self, other: Self) -> Ordering {
        (self.numerator as i128 * other.denominator as i128)
            .cmp(&(other.numerator as i128 * self.denominator as i128))
    }

    fn is_zero(self) -> bool {
        self.numerator == 0
    }

    fn to_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Number {
    Integer(i64),
    Rational(Rational),
    Inexact(f64),
}

impl Number {
    fn from_rational(rational: Rational) -> Self {
        if rational.denominator == 1 {
            Self::Integer(rational.numerator)
        } else {
            Self::Rational(rational)
        }
    }

    fn rational(numerator: i64, denominator: i64) -> Self {
        Self::from_rational(Rational::new(numerator, denominator))
    }

    fn rational_i128(numerator: i128, denominator: i128) -> Option<Self> {
        Rational::from_i128(numerator, denominator).map(Self::from_rational)
    }

    fn render(self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Rational(rational) => format!("{}/{}", rational.numerator, rational.denominator),
            Self::Inexact(value) => render_inexact(value),
        }
    }

    fn is_exact(self) -> bool {
        !matches!(self, Self::Inexact(_))
    }

    fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    fn is_integer(self) -> bool {
        match self {
            Self::Integer(_) => true,
            Self::Rational(rational) => rational.denominator == 1,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    fn is_rational(self) -> bool {
        self.is_exact()
    }

    fn as_exact_rational(self) -> Option<Rational> {
        match self {
            Self::Integer(value) => Some(Rational::from_integer(value)),
            Self::Rational(rational) => Some(rational),
            Self::Inexact(_) => None,
        }
    }

    fn exactified(self) -> Option<Self> {
        match self {
            Self::Inexact(value) => number_from_inexact_decimal(value),
            exact => Some(exact),
        }
    }

    fn comparable_rational(self) -> Option<Rational> {
        self.exactified()?.as_exact_rational()
    }

    fn to_f64(self) -> f64 {
        match self {
            Self::Integer(value) => value as f64,
            Self::Rational(rational) => rational.to_f64(),
            Self::Inexact(value) => value,
        }
    }

    fn is_zero(self) -> bool {
        match self {
            Self::Integer(value) => value == 0,
            Self::Rational(rational) => rational.is_zero(),
            Self::Inexact(value) => value == 0.0,
        }
    }

    fn is_positive(self) -> bool {
        match self {
            Self::Integer(value) => value > 0,
            Self::Rational(rational) => rational.numerator > 0,
            Self::Inexact(value) => value > 0.0,
        }
    }

    fn is_negative(self) -> bool {
        match self {
            Self::Integer(value) => value < 0,
            Self::Rational(rational) => rational.numerator < 0,
            Self::Inexact(value) => value < 0.0,
        }
    }
}

#[derive(Clone)]
struct SchemeString {
    value: Rc<RefCell<String>>,
    mutable: bool,
}

impl SchemeString {
    fn new(value: impl Into<String>) -> Self {
        Self::new_immutable(value)
    }

    fn new_immutable(value: impl Into<String>) -> Self {
        Self {
            value: Rc::new(RefCell::new(value.into())),
            mutable: false,
        }
    }

    fn new_mutable(value: impl Into<String>) -> Self {
        Self {
            value: Rc::new(RefCell::new(value.into())),
            mutable: true,
        }
    }

    fn contents(&self) -> String {
        self.value.borrow().clone()
    }

    fn len_chars(&self) -> usize {
        self.value.borrow().chars().count()
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.value.borrow().chars().nth(index)
    }

    fn is_mutable(&self) -> bool {
        self.mutable
    }

    fn copy_string(&self) -> Self {
        Self::new_mutable(self.contents())
    }

    fn set_char(&self, index: usize, ch: char) -> bool {
        let mut value = self.value.borrow_mut();
        let Some((start, end)) = char_range(&value, index) else {
            return false;
        };
        let mut encoded = [0_u8; 4];
        value.replace_range(start..end, ch.encode_utf8(&mut encoded));
        true
    }
}

#[derive(Clone)]
struct SchemeVector {
    values: Rc<RefCell<Vec<Value>>>,
}

impl SchemeVector {
    fn new(values: Vec<Value>) -> Self {
        Self {
            values: Rc::new(RefCell::new(values)),
        }
    }

    fn len(&self) -> usize {
        self.values.borrow().len()
    }

    fn contents(&self) -> Vec<Value> {
        self.values.borrow().clone()
    }

    fn get(&self, index: usize) -> Option<Value> {
        self.values.borrow().get(index).cloned()
    }

    fn set(&self, index: usize, value: Value) -> bool {
        let mut values = self.values.borrow_mut();
        let Some(slot) = values.get_mut(index) else {
            return false;
        };
        *slot = value;
        true
    }
}

#[derive(Clone)]
struct PairCell {
    car: RefCell<Value>,
    cdr: RefCell<Value>,
}

impl PairCell {
    fn new(car: Value, cdr: Value) -> Self {
        Self {
            car: RefCell::new(car),
            cdr: RefCell::new(cdr),
        }
    }

    fn car(&self) -> Value {
        self.car.borrow().clone()
    }

    fn cdr(&self) -> Value {
        self.cdr.borrow().clone()
    }

    fn set_car(&self, value: Value) {
        *self.car.borrow_mut() = value;
    }

    fn set_cdr(&self, value: Value) {
        *self.cdr.borrow_mut() = value;
    }
}

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    EmptyList,
    Pair(Rc<PairCell>),
    Vector(SchemeVector),
    Record(Rc<RecordInstance>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn as_number(&self, name: &str) -> Result<Number, EvalError> {
        match self {
            Self::Number(number) => Ok(*number),
            _ => Err(EvalError::ExpectedNumber {
                name: name.to_owned(),
            }),
        }
    }

    fn as_integer(&self, name: &str) -> Result<i64, EvalError> {
        match self {
            Self::Number(number) if number.is_exact() && number.is_integer() => Ok(number
                .as_exact_rational()
                .expect("exact integers should be rational")
                .numerator),
            _ => Err(EvalError::ExpectedNumber {
                name: name.to_owned(),
            }),
        }
    }

    fn render(&self) -> String {
        let mut active_pairs = HashSet::new();
        self.render_with_cycles(&mut active_pairs)
    }

    fn render_with_cycles(&self, active_pairs: &mut HashSet<usize>) -> String {
        match self {
            Self::Number(number) => number.render(),
            Self::Boolean(true) => "#t".to_owned(),
            Self::Boolean(false) => "#f".to_owned(),
            Self::String(value) => render_string(&value.contents()),
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value),
            Self::EmptyList => "()".to_owned(),
            Self::Pair(pair) => render_pair(pair, active_pairs),
            Self::Vector(values) => render_vector(values, active_pairs),
            Self::Record(record) => format!("#<record {}>", record.record_type.name),
            Self::Procedure(_) => "#<procedure>".to_owned(),
            Self::Void => "#<void>".to_owned(),
        }
    }

    fn render_display(&self) -> String {
        match self {
            Self::String(value) => value.contents(),
            Self::Char(value) => value.to_string(),
            _ => self.render(),
        }
    }
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    EqPred,
    EqvPred,
    EqualPred,
    LessThan,
    GreaterThan,
    Equal,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Not,
    Cons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
    Append,
    List,
    Length,
    NullPred,
    PairPred,
    SymbolPred,
    StringPred,
    NumberPred,
    ExactPred,
    InexactPred,
    IntegerPred,
    RationalPred,
    BooleanPred,
    ProcedurePred,
    Display,
    Write,
    Newline,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringCopy,
    StringSet,
    StringToList,
    ListToString,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLessThan,
    CharToInteger,
    IntegerToChar,
    CallCc,
    Apply,
    Map,
    ForEach,
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
    ListRef,
    ListTail,
    ListPred,
    Assoc,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
    StringEqual,
    StringLessThan,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
}

#[derive(Clone)]
enum Procedure {
    Builtin(Builtin),
    Lambda(Lambda),
    CaseLambda(CaseLambda),
    RecordConstructor(RecordConstructor),
    RecordPredicate(RecordPredicate),
    RecordAccessor(RecordAccessor),
    Continuation(CapturedContinuation),
}

#[derive(Clone)]
struct Lambda {
    name: Option<String>,
    params: LambdaParams,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct CaseLambda {
    name: Option<String>,
    clauses: Vec<Lambda>,
}

#[derive(Clone)]
struct LambdaParams {
    fixed: Vec<String>,
    rest: Option<String>,
}

#[derive(Clone)]
struct RecordType {
    name: String,
}

#[derive(Clone)]
struct RecordInstance {
    record_type: Rc<RecordType>,
    fields: Vec<Value>,
}

#[derive(Clone)]
struct RecordConstructor {
    name: String,
    record_type: Rc<RecordType>,
    field_count: usize,
}

#[derive(Clone)]
struct RecordPredicate {
    name: String,
    record_type: Rc<RecordType>,
}

#[derive(Clone)]
struct RecordAccessor {
    name: String,
    record_type: Rc<RecordType>,
    field_index: usize,
}

impl LambdaParams {
    fn expected_args(&self) -> String {
        match &self.rest {
            Some(_) => format!("at least {} arguments", self.fixed.len()),
            None => format!("exactly {} arguments", self.fixed.len()),
        }
    }

    fn matches_arity(&self, count: usize) -> bool {
        count >= self.fixed.len() && (self.rest.is_some() || count == self.fixed.len())
    }
}

impl CaseLambda {
    fn expected_args(&self) -> String {
        if self.clauses.is_empty() {
            "no supported arities".to_owned()
        } else {
            self.clauses
                .iter()
                .map(|clause| clause.params.expected_args())
                .collect::<Vec<_>>()
                .join(" or ")
        }
    }
}

#[derive(Clone)]
struct MacroDef {
    keyword: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvRef,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
enum PatternBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

type EnvRef = Rc<Environment>;

enum TailOutcome {
    Value(Value),
    Expr(Expr, EnvRef),
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
    macros: RefCell<HashMap<String, Rc<MacroDef>>>,
    output: Rc<RefCell<String>>,
    gensym_counter: Rc<Cell<usize>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        let output = parent.as_ref().map_or_else(
            || Rc::new(RefCell::new(String::new())),
            |env| env.output.clone(),
        );
        let gensym_counter = parent
            .as_ref()
            .map_or_else(|| Rc::new(Cell::new(0)), |env| env.gensym_counter.clone());

        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            macros: RefCell::new(HashMap::new()),
            output,
            gensym_counter,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }

    fn define_macro(&self, name: impl Into<String>, value: Rc<MacroDef>) {
        self.macros.borrow_mut().insert(name.into(), value);
    }

    fn lookup_macro(&self, name: &str) -> Option<Rc<MacroDef>> {
        if let Some(value) = self.macros.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_macro(name))
    }

    fn set(&self, name: &str, value: Value) -> bool {
        {
            let mut bindings = self.bindings.borrow_mut();
            if let Some(slot) = bindings.get_mut(name) {
                *slot = value;
                return true;
            }
        }

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
    }

    fn fresh_symbol(&self, hint: &str) -> String {
        let next = self.gensym_counter.get() + 1;
        self.gensym_counter.set(next);

        let mut sanitized = hint
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect::<String>();
        if sanitized.is_empty() {
            sanitized = "id".to_owned();
        }

        format!("__ming_{sanitized}_{next}")
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            _ => rendered.push(ch),
        }
    }

    rendered.push('"');
    rendered
}

fn render_pair(pair: &Rc<PairCell>, active_pairs: &mut HashSet<usize>) -> String {
    let mut rendered = String::from("(");
    render_pair_contents(pair, &mut rendered, active_pairs);
    rendered.push(')');
    rendered
}

fn render_pair_contents(pair: &Rc<PairCell>, rendered: &mut String, active_pairs: &mut HashSet<usize>) {
    let id = pair_id(pair);
    if !active_pairs.insert(id) {
        rendered.push_str("#<cycle>");
        return;
    }

    rendered.push_str(&pair.car().render_with_cycles(active_pairs));

    match pair.cdr() {
        Value::EmptyList => {}
        Value::Pair(next_pair) => {
            rendered.push(' ');
            render_pair_contents(&next_pair, rendered, active_pairs);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&other.render_with_cycles(active_pairs));
        }
    }

    active_pairs.remove(&id);
}

fn render_vector(values: &SchemeVector, active_pairs: &mut HashSet<usize>) -> String {
    let mut rendered = String::from("#(");

    for (index, value) in values.contents().iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&value.render_with_cycles(active_pairs));
    }

    rendered.push(')');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_owned(),
        '\n' => "#\\newline".to_owned(),
        _ => format!("#\\{value}"),
    }
}

fn render_inexact(value: f64) -> String {
    let mut rendered = value.to_string();
    if !rendered.contains('.') && !rendered.contains('e') && !rendered.contains('E') {
        rendered.push_str(".0");
    }
    rendered
}

fn empty_list() -> Value {
    Value::EmptyList
}

fn cons_value(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(PairCell::new(car, cdr)))
}

fn list_from_vec(items: Vec<Value>) -> Value {
    items
        .into_iter()
        .rev()
        .fold(empty_list(), |tail, item| cons_value(item, tail))
}

fn pair_id(pair: &Rc<PairCell>) -> usize {
    Rc::as_ptr(pair) as usize
}

fn collect_proper_list(value: &Value) -> Option<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut seen = HashSet::new();

    loop {
        match current {
            Value::EmptyList => return Some(items),
            Value::Pair(pair) => {
                let id = pair_id(&pair);
                if !seen.insert(id) {
                    return None;
                }
                items.push(pair.car());
                current = pair.cdr();
            }
            _ => return None,
        }
    }
}

fn is_proper_list(value: &Value) -> bool {
    collect_proper_list(value).is_some()
}

fn expect_pair(name: &str, value: &Value) -> Result<Rc<PairCell>, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        _ => Err(EvalError::ExpectedPair {
            name: name.to_owned(),
        }),
    }
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    left = left.abs();
    right = right.abs();

    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    if left == 0 {
        1
    } else {
        left
    }
}

fn checked_i64(value: i128) -> Option<i64> {
    i64::try_from(value).ok()
}

fn pow10_i128(exponent: u32) -> Option<i128> {
    let mut value = 1_i128;
    for _ in 0..exponent {
        value = value.checked_mul(10)?;
    }
    Some(value)
}

fn char_range(value: &str, index: usize) -> Option<(usize, usize)> {
    let mut indices = value.char_indices();
    let (start, _) = indices.nth(index)?;
    let end = indices.next().map_or(value.len(), |(end, _)| end);
    Some((start, end))
}

fn parse_char_literal(atom: &str) -> Option<char> {
    let suffix = atom.strip_prefix("#\\")?;

    match suffix {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = suffix.chars();
            let ch = chars.next()?;
            if chars.next().is_some() {
                None
            } else {
                Some(ch)
            }
        }
    }
}

fn parse_rational_literal(atom: &str) -> Option<Number> {
    let (numerator, denominator) = atom.split_once('/')?;
    if numerator.is_empty() || denominator.is_empty() || denominator.contains('/') {
        return None;
    }

    let numerator = numerator.parse::<i64>().ok()?;
    let denominator = denominator.parse::<i64>().ok()?;
    if denominator == 0 {
        return None;
    }

    Some(Number::rational(numerator, denominator))
}

fn parse_number_literal(atom: &str) -> Option<Number> {
    if let Ok(value) = atom.parse::<i64>() {
        return Some(Number::Integer(value));
    }

    if let Some(value) = parse_rational_literal(atom) {
        return Some(value);
    }

    if atom.contains('.') || atom.contains('e') || atom.contains('E') {
        return atom.parse::<f64>().ok().map(Number::Inexact);
    }

    None
}

fn parse_exact_decimal_literal(atom: &str) -> Option<Number> {
    let mut parts = atom.split(|ch| ch == 'e' || ch == 'E');
    let mantissa = parts.next()?;
    let exponent = match parts.next() {
        Some(value) => {
            if parts.next().is_some() {
                return None;
            }
            value.parse::<i32>().ok()?
        }
        None => 0,
    };

    let (sign, mantissa) = match mantissa.chars().next()? {
        '+' => (1_i128, &mantissa[1..]),
        '-' => (-1_i128, &mantissa[1..]),
        _ => (1_i128, mantissa),
    };

    let (whole, fractional) = match mantissa.split_once('.') {
        Some((whole, fractional)) if !fractional.contains('.') => (whole, fractional),
        Some(_) => return None,
        None => (mantissa, ""),
    };

    if whole.is_empty() && fractional.is_empty() {
        return None;
    }

    if !whole.chars().all(|ch| ch.is_ascii_digit())
        || !fractional.chars().all(|ch| ch.is_ascii_digit())
    {
        return None;
    }

    let digits = format!("{whole}{fractional}");
    if digits.is_empty() {
        return None;
    }

    let mut numerator = sign.checked_mul(digits.parse::<i128>().ok()?)?;
    let scale = fractional.len() as i32 - exponent;

    if scale <= 0 {
        numerator = numerator.checked_mul(pow10_i128((-scale) as u32)?)?;
        Number::rational_i128(numerator, 1)
    } else {
        Number::rational_i128(numerator, pow10_i128(scale as u32)?)
    }
}

fn number_from_inexact_decimal(value: f64) -> Option<Number> {
    parse_exact_decimal_literal(&render_inexact(value))
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while self.peek_char().is_some() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(EvalError::EmptyInput.with_position(self.current_pos()))
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let start = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(start),
            Some(')') => Err(self.parse_error("unexpected ')'")),
            Some('\'') => self.parse_quote(start),
            Some('"') => self.parse_string(start),
            Some(_) => self.parse_atom(start),
            None => Err(self.parse_error("unexpected end of input")),
        }
    }

    fn parse_list(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    return Ok(Expr::List(items, start));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.parse_error("unterminated list")),
            }
        }
    }

    fn parse_string(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value, start)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| self.parse_error("unterminated string"))?;
                    let decoded = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    value.push(decoded);
                }
                other => value.push(other),
            }
        }

        Err(self.parse_error("unterminated string"))
    }

    fn parse_quote(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let expr = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".to_owned(), start), expr],
            start,
        ))
    }

    fn parse_atom(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        let start_index = self.pos;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.advance_char();
        }

        let atom = &self.input[start_index..self.pos];

        if atom.is_empty() {
            return Err(EvalError::Parse("expected expression".to_owned()).with_position(start));
        }

        if atom == "#t" {
            return Ok(Expr::Boolean(true, start));
        }

        if atom == "#f" {
            return Ok(Expr::Boolean(false, start));
        }

        if let Some(value) = parse_char_literal(atom) {
            return Ok(Expr::Char(value, start));
        }

        if let Some(value) = parse_number_literal(atom) {
            return Ok(Expr::Number(value, start));
        }

        Ok(Expr::Symbol(atom.to_owned(), start))
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.advance_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.advance_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.peek_char() {
            Some(ch) if ch == expected => {
                self.advance_char();
                Ok(())
            }
            Some(ch) => Err(self.parse_error(format!("expected '{expected}', found '{ch}'"))),
            None => Err(self.parse_error(format!("expected '{expected}'"))),
        }
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            col: self.col,
        }
    }

    fn parse_error(&self, message: impl Into<String>) -> EvalError {
        EvalError::Parse(message.into()).with_position(self.current_pos())
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }
}

fn root_env() -> EnvRef {
    let env = Environment::new(None);

    for (name, builtin) in [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("eq?", Builtin::EqPred),
        ("eqv?", Builtin::EqvPred),
        ("equal?", Builtin::EqualPred),
        ("<", Builtin::LessThan),
        (">", Builtin::GreaterThan),
        ("=", Builtin::Equal),
        ("<=", Builtin::LessThanOrEqual),
        (">=", Builtin::GreaterThanOrEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("set-car!", Builtin::SetCar),
        ("set-cdr!", Builtin::SetCdr),
        ("append", Builtin::Append),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("null?", Builtin::NullPred),
        ("pair?", Builtin::PairPred),
        ("list?", Builtin::ListPred),
        ("symbol?", Builtin::SymbolPred),
        ("string?", Builtin::StringPred),
        ("number?", Builtin::NumberPred),
        ("exact?", Builtin::ExactPred),
        ("inexact?", Builtin::InexactPred),
        ("integer?", Builtin::IntegerPred),
        ("rational?", Builtin::RationalPred),
        ("boolean?", Builtin::BooleanPred),
        ("procedure?", Builtin::ProcedurePred),
        ("display", Builtin::Display),
        ("write", Builtin::Write),
        ("newline", Builtin::Newline),
        ("exact->inexact", Builtin::ExactToInexact),
        ("inexact->exact", Builtin::InexactToExact),
        ("numerator", Builtin::Numerator),
        ("denominator", Builtin::Denominator),
        ("string-append", Builtin::StringAppend),
        ("string-length", Builtin::StringLength),
        ("substring", Builtin::Substring),
        ("string->number", Builtin::StringToNumber),
        ("number->string", Builtin::NumberToString),
        ("symbol->string", Builtin::SymbolToString),
        ("string->symbol", Builtin::StringToSymbol),
        ("string-ref", Builtin::StringRef),
        ("string=?", Builtin::StringEqual),
        ("string<?", Builtin::StringLessThan),
        ("string-ci=?", Builtin::StringCiEqual),
        ("string-upcase", Builtin::StringUpcase),
        ("string-downcase", Builtin::StringDowncase),
        ("string-copy", Builtin::StringCopy),
        ("string-set!", Builtin::StringSet),
        ("string->list", Builtin::StringToList),
        ("list->string", Builtin::ListToString),
        ("char?", Builtin::CharPred),
        ("char-alphabetic?", Builtin::CharAlphabeticPred),
        ("char-numeric?", Builtin::CharNumericPred),
        ("char-upcase", Builtin::CharUpcase),
        ("char-downcase", Builtin::CharDowncase),
        ("char=?", Builtin::CharEqual),
        ("char<?", Builtin::CharLessThan),
        ("char->integer", Builtin::CharToInteger),
        ("integer->char", Builtin::IntegerToChar),
        ("call/cc", Builtin::CallCc),
        (
            "call-with-current-continuation",
            Builtin::CallCc,
        ),
        ("apply", Builtin::Apply),
        ("map", Builtin::Map),
        ("for-each", Builtin::ForEach),
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
        ("list-ref", Builtin::ListRef),
        ("list-tail", Builtin::ListTail),
        ("assoc", Builtin::Assoc),
        ("vector", Builtin::Vector),
        ("make-vector", Builtin::MakeVector),
        ("vector-ref", Builtin::VectorRef),
        ("vector-set!", Builtin::VectorSet),
        ("vector-length", Builtin::VectorLength),
        ("vector?", Builtin::VectorPred),
        ("vector->list", Builtin::VectorToList),
        ("list->vector", Builtin::ListToVector),
    ] {
        env.define(name, Value::Procedure(Rc::new(Procedure::Builtin(builtin))));
    }

    install_prelude(&env);
    env
}

const STANDARD_PRELUDE: &str = r#"
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (reverse xs)
  (let loop ((rest xs) (acc '()))
    (if (null? rest)
        acc
        (loop (cdr rest) (cons (car rest) acc)))))

(define (member obj lst)
  (cond ((null? lst) #f)
        ((equal? obj (car lst)) lst)
        (else (member obj (cdr lst)))))

(define (assv key alist)
  (cond ((null? alist) #f)
        ((eqv? key (caar alist)) (car alist))
        (else (assv key (cdr alist)))))

(define (gcd . nums)
  (define (gcd2 a b)
    (let ((a (abs a)) (b (abs b)))
      (if (= b 0)
          a
          (gcd2 b (modulo a b)))))
  (if (null? nums)
      0
      (let loop ((acc (car nums)) (rest (cdr nums)))
        (if (null? rest)
            (abs acc)
            (loop (gcd2 acc (car rest)) (cdr rest))))))

(define (lcm . nums)
  (define (lcm2 a b)
    (if (or (= a 0) (= b 0))
        0
        (quotient (abs (* a b)) (gcd a b))))
  (if (null? nums)
      1
      (let loop ((acc (car nums)) (rest (cdr nums)))
        (if (null? rest)
            (abs acc)
            (loop (lcm2 acc (car rest)) (cdr rest))))))

(define (truncate x) x)
(define (round x) x)

(define (make-string k . maybe-fill)
  (let ((fill (if (null? maybe-fill) #\space (car maybe-fill))))
    (let loop ((n k) (acc '()))
      (if (= n 0)
          (list->string acc)
          (loop (- n 1) (cons fill acc))))))

(define (string . chars) (list->string chars))
(define (string>? a b) (string<? b a))
(define (string<=? a b) (not (string<? b a)))
(define (string>=? a b) (not (string<? a b)))
"#;

fn install_prelude(env: &EnvRef) {
    let mut parser = Parser::new(STANDARD_PRELUDE);
    let exprs = parser
        .parse_program()
        .expect("standard prelude must parse");
    let _ = eval_sequence_with_continuations(&exprs, env)
        .expect("standard prelude must evaluate");
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    let pos = expr.pos();

    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(SchemeString::new(value.clone()))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundSymbol(name.clone()).with_position(pos)),
        Expr::List(items, _) => eval_list(items, pos, env).map_err(|err| err.with_position(pos)),
    }
}

fn eval_list(items: &[Expr], list_pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or(EvalError::InvalidApplication)?;

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(args, env),
            "define-record-type" => return eval_define_record_type(args, env),
            "define-syntax" => return eval_define_syntax(args, env),
            "set!" => return eval_set(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "lambda" => return eval_lambda(args, env),
            "case-lambda" => return eval_case_lambda(args, env),
            "begin" => return eval_begin(args, env),
            "cond" => return eval_cond(args, env),
            "case" => return eval_case(args, env),
            "let" => return eval_let(args, env),
            "let*" => return eval_let_star(args, env),
            "letrec" => return eval_letrec(args, env),
            "letrec*" => return eval_letrec_star(args, env),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "do" => return eval_do(args, env),
            _ => {}
        }

        if let Some(macro_def) = env.lookup_macro(name) {
            let expanded = expand_macro_use(&macro_def, &Expr::List(items.to_vec(), list_pos))?;
            return eval_expr(&expanded, env);
        }
    }

    let operator = eval_expr(head, env)?;
    apply(operator, args, env)
}

fn eval_expr_tco(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        let pos = current_expr.pos();

        match &current_expr {
            Expr::Number(value, _) => return Ok(Value::Number(*value)),
            Expr::Boolean(value, _) => return Ok(Value::Boolean(*value)),
            Expr::String(value, _) => {
                return Ok(Value::String(SchemeString::new(value.clone())));
            }
            Expr::Char(value, _) => return Ok(Value::Char(*value)),
            Expr::Symbol(name, _) => {
                return current_env
                    .lookup(name)
                    .ok_or_else(|| EvalError::UnboundSymbol(name.clone()).with_position(pos));
            }
            Expr::List(items, _) => {
                match eval_tail_list(items, pos, &current_env)
                    .map_err(|err| err.with_position(pos))?
                {
                    TailOutcome::Value(value) => return Ok(value),
                    TailOutcome::Expr(expr, env) => {
                        current_expr = expr;
                        current_env = env;
                    }
                }
            }
        }
    }
}

fn eval_tail_list(items: &[Expr], list_pos: SourcePos, env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let (head, args) = items.split_first().ok_or(EvalError::InvalidApplication)?;

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(args, env).map(TailOutcome::Value),
            "define-record-type" => return eval_define_record_type(args, env).map(TailOutcome::Value),
            "define-syntax" => return eval_define_syntax(args, env).map(TailOutcome::Value),
            "set!" => return eval_set(args, env).map(TailOutcome::Value),
            "if" => return eval_tail_if(args, env),
            "quote" => return eval_quote(args).map(TailOutcome::Value),
            "lambda" => return eval_lambda(args, env).map(TailOutcome::Value),
            "case-lambda" => return eval_case_lambda(args, env).map(TailOutcome::Value),
            "begin" => return tail_sequence(args, env),
            "cond" => return eval_tail_cond(args, env),
            "case" => return eval_tail_case(args, env),
            "let" => return eval_tail_let(args, env),
            "let*" => return eval_tail_let_star(args, env),
            "letrec" => return eval_tail_letrec(args, env),
            "letrec*" => return eval_tail_letrec_star(args, env),
            "and" => return eval_tail_and(args, env),
            "or" => return eval_tail_or(args, env),
            "do" => return eval_do(args, env).map(TailOutcome::Value),
            _ => {}
        }

        if let Some(macro_def) = env.lookup_macro(name) {
            let expanded = expand_macro_use(&macro_def, &Expr::List(items.to_vec(), list_pos))?;
            return Ok(TailOutcome::Expr(expanded, env.clone()));
        }
    }

    let operator = eval_expr(head, env)?;
    let values = eval_args(args, env)?;
    tail_apply_values(operator, &values, env)
}

fn apply(operator: Value, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let values = eval_args(args, env)?;
    apply_values(operator, &values, env)
}

fn apply_values(operator: Value, args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => apply_builtin(*builtin, args, env),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args),
            Procedure::CaseLambda(case_lambda) => apply_case_lambda(case_lambda, args),
            Procedure::RecordConstructor(constructor) => {
                apply_record_constructor(constructor, args)
            }
            Procedure::RecordPredicate(predicate) => apply_record_predicate(predicate, args),
            Procedure::RecordAccessor(accessor) => apply_record_accessor(accessor, args),
            Procedure::Continuation(_) => Err(continuation_context_error()),
        },
        _ => Err(EvalError::InvalidApplication),
    }
}

fn tail_apply_values(operator: Value, args: &[Value], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(Builtin::Apply) => eval_tail_apply_builtin(args, env),
            Procedure::Builtin(builtin) => apply_builtin(*builtin, args, env).map(TailOutcome::Value),
            Procedure::Lambda(lambda) => {
                let call_env = prepare_lambda_call(
                    lambda,
                    args,
                    lambda.name.as_deref().unwrap_or("lambda"),
                )?;
                tail_sequence(&lambda.body, &call_env)
            }
            Procedure::CaseLambda(case_lambda) => {
                let clause = select_case_lambda_clause(case_lambda, args)?;
                let call_env = prepare_lambda_call(
                    clause,
                    args,
                    case_lambda.name.as_deref().unwrap_or("case-lambda"),
                )?;
                tail_sequence(&clause.body, &call_env)
            }
            Procedure::RecordConstructor(constructor) => {
                apply_record_constructor(constructor, args).map(TailOutcome::Value)
            }
            Procedure::RecordPredicate(predicate) => {
                apply_record_predicate(predicate, args).map(TailOutcome::Value)
            }
            Procedure::RecordAccessor(accessor) => {
                apply_record_accessor(accessor, args).map(TailOutcome::Value)
            }
            Procedure::Continuation(_) => Err(continuation_context_error()),
        },
        _ => Err(EvalError::InvalidApplication),
    }
}

fn eval_tail_apply_builtin(args: &[Value], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "apply".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let operator = args[0].clone();
    let mut applied_args = args[1..args.len() - 1].to_vec();
    let tail = expect_list("apply", &args[args.len() - 1])?;
    applied_args.extend(tail);

    tail_apply_values(operator, &applied_args, env)
}

fn apply_builtin(builtin: Builtin, values: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(values),
        Builtin::Sub => eval_sub(values),
        Builtin::Mul => eval_mul(values),
        Builtin::Div => eval_div(values),
        Builtin::EqPred => eval_eq_pred(values),
        Builtin::EqvPred => eval_eqv_pred(values),
        Builtin::EqualPred => eval_equal_pred(values),
        Builtin::LessThan => eval_compare("<", values, |ordering| ordering == Ordering::Less),
        Builtin::GreaterThan => eval_compare(">", values, |ordering| ordering == Ordering::Greater),
        Builtin::Equal => eval_compare("=", values, |ordering| ordering == Ordering::Equal),
        Builtin::LessThanOrEqual => {
            eval_compare("<=", values, |ordering| ordering != Ordering::Greater)
        }
        Builtin::GreaterThanOrEqual => {
            eval_compare(">=", values, |ordering| ordering != Ordering::Less)
        }
        Builtin::Not => eval_not(values),
        Builtin::Cons => eval_cons(values),
        Builtin::Car => eval_car(values),
        Builtin::Cdr => eval_cdr(values),
        Builtin::SetCar => eval_set_car(values),
        Builtin::SetCdr => eval_set_cdr(values),
        Builtin::Append => eval_append(values),
        Builtin::List => eval_list_builtin(values),
        Builtin::Length => eval_length(values),
        Builtin::NullPred => eval_null_pred(values),
        Builtin::PairPred => eval_pair_pred(values),
        Builtin::ListPred => eval_list_pred(values),
        Builtin::SymbolPred => eval_symbol_pred(values),
        Builtin::StringPred => eval_string_pred(values),
        Builtin::NumberPred => eval_number_pred(values),
        Builtin::ExactPred => eval_exact_pred(values),
        Builtin::InexactPred => eval_inexact_pred(values),
        Builtin::IntegerPred => eval_integer_type_pred(values),
        Builtin::RationalPred => eval_rational_pred(values),
        Builtin::BooleanPred => eval_boolean_pred(values),
        Builtin::ProcedurePred => eval_procedure_pred(values),
        Builtin::Display => eval_display(values, env),
        Builtin::Write => eval_write(values, env),
        Builtin::Newline => eval_newline(values, env),
        Builtin::ExactToInexact => eval_exact_to_inexact(values),
        Builtin::InexactToExact => eval_inexact_to_exact(values),
        Builtin::Numerator => eval_numerator(values),
        Builtin::Denominator => eval_denominator(values),
        Builtin::StringAppend => eval_string_append(values),
        Builtin::StringLength => eval_string_length(values),
        Builtin::Substring => eval_substring(values),
        Builtin::StringToNumber => eval_string_to_number(values),
        Builtin::NumberToString => eval_number_to_string(values),
        Builtin::SymbolToString => eval_symbol_to_string(values),
        Builtin::StringToSymbol => eval_string_to_symbol(values),
        Builtin::StringRef => eval_string_ref(values),
        Builtin::StringEqual => {
            eval_string_compare("string=?", values, |left, right| left == right)
        }
        Builtin::StringLessThan => {
            eval_string_compare("string<?", values, |left, right| left < right)
        }
        Builtin::StringCiEqual => eval_string_compare("string-ci=?", values, |left, right| {
            left.to_lowercase() == right.to_lowercase()
        }),
        Builtin::StringUpcase => eval_string_upcase(values),
        Builtin::StringDowncase => eval_string_downcase(values),
        Builtin::StringCopy => eval_string_copy(values),
        Builtin::StringSet => eval_string_set(values),
        Builtin::StringToList => eval_string_to_list(values),
        Builtin::ListToString => eval_list_to_string(values),
        Builtin::CharPred => eval_char_pred(values),
        Builtin::CharAlphabeticPred => eval_char_alphabetic_pred(values),
        Builtin::CharNumericPred => eval_char_numeric_pred(values),
        Builtin::CharUpcase => eval_char_upcase(values),
        Builtin::CharDowncase => eval_char_downcase(values),
        Builtin::CharEqual => eval_char_compare("char=?", values, |left, right| left == right),
        Builtin::CharLessThan => eval_char_compare("char<?", values, |left, right| left < right),
        Builtin::CharToInteger => eval_char_to_integer(values),
        Builtin::IntegerToChar => eval_integer_to_char(values),
        Builtin::CallCc => Err(EvalError::InvalidArgument {
            name: "call/cc".to_owned(),
            message: "internal continuation requires continuation-aware evaluation".to_owned(),
        }),
        Builtin::Apply => eval_apply_builtin(values, env),
        Builtin::Map => eval_map_builtin(values, env),
        Builtin::ForEach => eval_for_each_builtin(values, env),
        Builtin::Abs => eval_abs(values),
        Builtin::Modulo => eval_modulo(values),
        Builtin::Remainder => eval_remainder(values),
        Builtin::Quotient => eval_quotient(values),
        Builtin::Min => eval_min(values),
        Builtin::Max => eval_max(values),
        Builtin::Expt => eval_expt(values),
        Builtin::ZeroPred => eval_number_predicate(values, "zero?", Number::is_zero),
        Builtin::PositivePred => eval_number_predicate(values, "positive?", Number::is_positive),
        Builtin::NegativePred => eval_number_predicate(values, "negative?", Number::is_negative),
        Builtin::OddPred => eval_integer_predicate(values, "odd?", |value| value % 2 != 0),
        Builtin::EvenPred => eval_integer_predicate(values, "even?", |value| value % 2 == 0),
        Builtin::ListRef => eval_list_ref(values),
        Builtin::ListTail => eval_list_tail(values),
        Builtin::Assoc => eval_assoc(values),
        Builtin::Vector => eval_vector(values),
        Builtin::MakeVector => eval_make_vector(values),
        Builtin::VectorRef => eval_vector_ref(values),
        Builtin::VectorSet => eval_vector_set(values),
        Builtin::VectorLength => eval_vector_length(values),
        Builtin::VectorPred => eval_vector_pred(values),
        Builtin::VectorToList => eval_vector_to_list(values),
        Builtin::ListToVector => eval_list_to_vector(values),
    }
}

fn prepare_lambda_call(lambda: &Lambda, args: &[Value], name: &str) -> Result<EnvRef, EvalError> {
    if !lambda.params.matches_arity(args.len()) {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: lambda.params.expected_args(),
            got: args.len(),
        });
    }

    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda
        .params
        .fixed
        .iter()
        .cloned()
        .zip(args.iter().take(lambda.params.fixed.len()).cloned())
    {
        call_env.define(param, value);
    }

    if let Some(rest) = &lambda.params.rest {
        call_env.define(
            rest.clone(),
            list_from_vec(args[lambda.params.fixed.len()..].to_vec()),
        );
    }

    Ok(call_env)
}

fn select_case_lambda_clause<'a>(
    case_lambda: &'a CaseLambda,
    args: &[Value],
) -> Result<&'a Lambda, EvalError> {
    case_lambda
        .clauses
        .iter()
        .find(|clause| clause.params.matches_arity(args.len()))
        .ok_or_else(|| EvalError::WrongArgCount {
            name: case_lambda
                .name
                .clone()
                .unwrap_or_else(|| "case-lambda".to_owned()),
            expected: case_lambda.expected_args(),
            got: args.len(),
        })
}

fn apply_lambda(lambda: &Lambda, args: &[Value]) -> Result<Value, EvalError> {
    let call_env = prepare_lambda_call(lambda, args, lambda.name.as_deref().unwrap_or("lambda"))?;
    eval_sequence(&lambda.body, &call_env)
}

fn apply_case_lambda(case_lambda: &CaseLambda, args: &[Value]) -> Result<Value, EvalError> {
    let clause = select_case_lambda_clause(case_lambda, args)?;
    let call_env = prepare_lambda_call(
        clause,
        args,
        case_lambda.name.as_deref().unwrap_or("case-lambda"),
    )?;

    eval_sequence(&clause.body, &call_env)
}

fn apply_record_constructor(
    constructor: &RecordConstructor,
    args: &[Value],
) -> Result<Value, EvalError> {
    if args.len() != constructor.field_count {
        return Err(EvalError::WrongArgCount {
            name: constructor.name.clone(),
            expected: format!("exactly {} arguments", constructor.field_count),
            got: args.len(),
        });
    }

    Ok(Value::Record(Rc::new(RecordInstance {
        record_type: constructor.record_type.clone(),
        fields: args.to_vec(),
    })))
}

fn apply_record_predicate(predicate: &RecordPredicate, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: predicate.name.clone(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(matches!(
        &args[0],
        Value::Record(record) if Rc::ptr_eq(&record.record_type, &predicate.record_type)
    )))
}

fn apply_record_accessor(accessor: &RecordAccessor, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: accessor.name.clone(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let record = expect_record(&accessor.name, &args[0], &accessor.record_type)?;
    Ok(record.fields[accessor.field_index].clone())
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (target, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("define requires a target".to_owned()))?;

    match target {
        Expr::Symbol(name, _) => {
            if body.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define".to_owned(),
                    expected: "exactly 2 forms".to_owned(),
                    got: args.len(),
                });
            }

            let value = eval_expr(&body[0], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature, _) => {
            let (name_expr, params_exprs) = signature
                .split_first()
                .ok_or_else(|| EvalError::Parse("define requires a function name".to_owned()))?;
            let name = expect_symbol(name_expr, "define function name")?;

            if body.is_empty() {
                return Err(EvalError::Parse(
                    "define requires a function body".to_owned(),
                ));
            }

            let params = parse_params(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env: env.clone(),
            })));

            env.define(name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse(
            "define target must be a symbol".to_owned(),
        )),
    }
}

fn eval_define_record_type(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::Parse(
            "define-record-type requires a name, constructor, and predicate".to_owned(),
        ));
    }

    let type_name = expect_symbol(&args[0], "define-record-type name")?;
    let constructor_spec = match &args[1] {
        Expr::List(items, _) => items,
        _ => {
            return Err(EvalError::Parse(
                "define-record-type constructor must be a list".to_owned(),
            ));
        }
    };
    let (constructor_name_expr, constructor_fields) =
        constructor_spec.split_first().ok_or_else(|| {
            EvalError::Parse("define-record-type constructor must include a name".to_owned())
        })?;
    let constructor_name =
        expect_symbol(constructor_name_expr, "define-record-type constructor name")?;
    let constructor_fields = constructor_fields
        .iter()
        .map(|expr| expect_symbol(expr, "define-record-type constructor field"))
        .collect::<Result<Vec<_>, _>>()?;
    let predicate_name = expect_symbol(&args[2], "define-record-type predicate name")?;
    let field_specs = args[3..]
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;

    if constructor_fields.len() != field_specs.len()
        || !constructor_fields
            .iter()
            .zip(field_specs.iter())
            .all(|(field, (expected, _))| field == expected)
    {
        return Err(EvalError::Parse(
            "define-record-type constructor fields must match field definitions".to_owned(),
        ));
    }

    let record_type = Rc::new(RecordType { name: type_name });

    env.define(
        constructor_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordConstructor(RecordConstructor {
            name: constructor_name,
            record_type: record_type.clone(),
            field_count: field_specs.len(),
        }))),
    );
    env.define(
        predicate_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordPredicate(RecordPredicate {
            name: predicate_name,
            record_type: record_type.clone(),
        }))),
    );

    for (field_index, (_, accessor_name)) in field_specs.into_iter().enumerate() {
        env.define(
            accessor_name.clone(),
            Value::Procedure(Rc::new(Procedure::RecordAccessor(RecordAccessor {
                name: accessor_name,
                record_type: record_type.clone(),
                field_index,
            }))),
        );
    }

    Ok(Value::Void)
}

fn parse_record_field_spec(expr: &Expr) -> Result<(String, String), EvalError> {
    match expr {
        Expr::List(items, _) if items.len() == 2 => Ok((
            expect_symbol(&items[0], "define-record-type field name")?,
            expect_symbol(&items[1], "define-record-type accessor name")?,
        )),
        Expr::List(_, _) => Err(EvalError::Parse(
            "define-record-type field specs must contain a field name and accessor".to_owned(),
        )),
        _ => Err(EvalError::Parse(
            "define-record-type field specs must be lists".to_owned(),
        )),
    }
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "define-syntax".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let name = expect_symbol(&args[0], "define-syntax name")?;
    let transformer = parse_syntax_rules(&name, &args[1], env)?;
    env.define_macro(name, Rc::new(transformer));
    Ok(Value::Void)
}

fn parse_syntax_rules(keyword: &str, expr: &Expr, env: &EnvRef) -> Result<MacroDef, EvalError> {
    let items = match expr {
        Expr::List(items, _) => items,
        _ => {
            return Err(EvalError::Parse(
                "define-syntax requires a syntax-rules transformer".to_owned(),
            ));
        }
    };

    let (head, tail) = items
        .split_first()
        .ok_or_else(|| EvalError::Parse("syntax-rules form cannot be empty".to_owned()))?;

    match head {
        Expr::Symbol(name, _) if name == "syntax-rules" => {}
        _ => {
            return Err(EvalError::Parse(
                "define-syntax requires syntax-rules".to_owned(),
            ));
        }
    }

    let (literals_expr, rules_exprs) = tail
        .split_first()
        .ok_or_else(|| EvalError::Parse("syntax-rules requires a literals list".to_owned()))?;

    let literal_items = match literals_expr {
        Expr::List(items, _) => items,
        _ => {
            return Err(EvalError::Parse(
                "syntax-rules literals must be a list".to_owned(),
            ));
        }
    };

    let mut literals = HashSet::new();
    for literal in literal_items {
        literals.insert(expect_symbol(literal, "syntax-rules literal")?);
    }

    if rules_exprs.is_empty() {
        return Err(EvalError::Parse(
            "syntax-rules requires at least one rule".to_owned(),
        ));
    }

    let mut rules = Vec::with_capacity(rules_exprs.len());
    for rule in rules_exprs {
        match rule {
            Expr::List(items, _) if items.len() == 2 => rules.push(MacroRule {
                pattern: items[0].clone(),
                template: items[1].clone(),
            }),
            Expr::List(_, _) => {
                return Err(EvalError::Parse(
                    "syntax-rules rules must contain a pattern and template".to_owned(),
                ));
            }
            _ => {
                return Err(EvalError::Parse(
                    "syntax-rules rules must be lists".to_owned(),
                ))
            }
        }
    }

    Ok(MacroDef {
        keyword: keyword.to_owned(),
        literals,
        rules,
        env: env.clone(),
    })
}

fn expand_macro_use(macro_def: &MacroDef, expr: &Expr) -> Result<Expr, EvalError> {
    for rule in &macro_def.rules {
        let mut bindings = HashMap::new();
        if match_pattern(&rule.pattern, expr, macro_def, &mut bindings) {
            let mut renamed = HashMap::new();
            return expand_template(&rule.template, macro_def, &bindings, &mut renamed, None);
        }
    }

    Err(EvalError::Parse(format!(
        "no matching syntax-rules clause for {}",
        macro_def.keyword
    )))
}

fn match_pattern(
    pattern: &Expr,
    expr: &Expr,
    macro_def: &MacroDef,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Number(value, _) => matches!(expr, Expr::Number(other, _) if other == value),
        Expr::Boolean(value, _) => matches!(expr, Expr::Boolean(other, _) if other == value),
        Expr::String(value, _) => matches!(expr, Expr::String(other, _) if other == value),
        Expr::Char(value, _) => matches!(expr, Expr::Char(other, _) if other == value),
        Expr::Symbol(name, _) => match_symbol_pattern(name, expr, macro_def, bindings),
        Expr::List(patterns, _) => match expr {
            Expr::List(exprs, _) => match_list_pattern(patterns, exprs, macro_def, bindings),
            _ => false,
        },
    }
}

fn match_symbol_pattern(
    name: &str,
    expr: &Expr,
    macro_def: &MacroDef,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    if name == "..." {
        return false;
    }

    if name == macro_def.keyword || macro_def.literals.contains(name) {
        return matches!(expr, Expr::Symbol(other, _) if other == name);
    }

    match bindings.get(name) {
        None => {
            bindings.insert(name.to_owned(), PatternBinding::Single(expr.clone()));
            true
        }
        Some(PatternBinding::Single(existing)) => expr_syntax_eq(existing, expr),
        Some(PatternBinding::Repeated(_)) => false,
    }
}

fn match_list_pattern(
    patterns: &[Expr],
    exprs: &[Expr],
    macro_def: &MacroDef,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    let mut pattern_index = 0;
    let mut expr_index = 0;

    while pattern_index < patterns.len() {
        if is_ellipsis(patterns.get(pattern_index + 1)) {
            let min_suffix_len = min_pattern_list_len(&patterns[pattern_index + 2..]);
            if exprs.len() < expr_index + min_suffix_len {
                return false;
            }

            let repeat_count = exprs.len() - expr_index - min_suffix_len;
            let mut repeated_vars = HashSet::new();
            collect_pattern_variables(&patterns[pattern_index], macro_def, &mut repeated_vars);
            for name in repeated_vars {
                match bindings.get(&name) {
                    None => {
                        bindings.insert(name, PatternBinding::Repeated(Vec::new()));
                    }
                    Some(PatternBinding::Repeated(_)) => {}
                    Some(PatternBinding::Single(_)) => return false,
                }
            }

            for expr in &exprs[expr_index..expr_index + repeat_count] {
                let mut repeated_bindings = HashMap::new();
                if !match_pattern(
                    &patterns[pattern_index],
                    expr,
                    macro_def,
                    &mut repeated_bindings,
                ) {
                    return false;
                }
                if !merge_repeated_bindings(bindings, repeated_bindings) {
                    return false;
                }
            }

            pattern_index += 2;
            expr_index += repeat_count;
            continue;
        }

        if expr_index >= exprs.len()
            || !match_pattern(
                &patterns[pattern_index],
                &exprs[expr_index],
                macro_def,
                bindings,
            )
        {
            return false;
        }

        pattern_index += 1;
        expr_index += 1;
    }

    expr_index == exprs.len()
}

fn merge_repeated_bindings(
    bindings: &mut HashMap<String, PatternBinding>,
    repeated_bindings: HashMap<String, PatternBinding>,
) -> bool {
    for (name, binding) in repeated_bindings {
        let PatternBinding::Single(expr) = binding else {
            return false;
        };

        match bindings.get_mut(&name) {
            None => {
                bindings.insert(name, PatternBinding::Repeated(vec![expr]));
            }
            Some(PatternBinding::Repeated(values)) => values.push(expr),
            Some(PatternBinding::Single(_)) => return false,
        }
    }

    true
}

fn min_pattern_list_len(patterns: &[Expr]) -> usize {
    let mut len = 0;
    let mut index = 0;

    while index < patterns.len() {
        if is_ellipsis(patterns.get(index + 1)) {
            index += 2;
        } else {
            len += 1;
            index += 1;
        }
    }

    len
}

fn collect_pattern_variables(pattern: &Expr, macro_def: &MacroDef, vars: &mut HashSet<String>) {
    match pattern {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
        Expr::Symbol(name, _) => {
            if name != "..." && name != &macro_def.keyword && !macro_def.literals.contains(name) {
                vars.insert(name.clone());
            }
        }
        Expr::List(items, _) => {
            for item in items {
                collect_pattern_variables(item, macro_def, vars);
            }
        }
    }
}

fn expand_template(
    template: &Expr,
    macro_def: &MacroDef,
    bindings: &HashMap<String, PatternBinding>,
    renamed: &mut HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) => {
            expand_template_symbol(name, *pos, macro_def, bindings, renamed, repeat_index)
        }
        Expr::List(items, pos) => {
            if is_quote_form(items) {
                return Ok(template.clone());
            }

            let mut expanded = Vec::new();
            let mut index = 0;

            while index < items.len() {
                if is_ellipsis(items.get(index + 1)) {
                    let repeat_count = repeated_template_len(&items[index], bindings)?;
                    for item_index in 0..repeat_count {
                        expanded.push(expand_template(
                            &items[index],
                            macro_def,
                            bindings,
                            renamed,
                            Some(item_index),
                        )?);
                    }
                    index += 2;
                    continue;
                }

                expanded.push(expand_template(
                    &items[index],
                    macro_def,
                    bindings,
                    renamed,
                    repeat_index,
                )?);
                index += 1;
            }

            Ok(Expr::List(expanded, *pos))
        }
    }
}

fn expand_template_symbol(
    name: &str,
    pos: SourcePos,
    macro_def: &MacroDef,
    bindings: &HashMap<String, PatternBinding>,
    renamed: &mut HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if name == "..." {
        return Err(EvalError::Parse(
            "unexpected ellipsis in syntax-rules template".to_owned(),
        ));
    }

    if let Some(binding) = bindings.get(name) {
        return match binding {
            PatternBinding::Single(expr) => Ok(expr.clone()),
            PatternBinding::Repeated(values) => values
                .get(repeat_index.ok_or_else(|| {
                    EvalError::Parse(format!(
                        "pattern variable {name} used outside of an ellipsis context"
                    ))
                })?)
                .cloned()
                .ok_or_else(|| {
                    EvalError::Parse(format!(
                        "pattern variable {name} repetition index out of bounds"
                    ))
                }),
        };
    }

    if is_special_form_name(name) || macro_def.env.lookup_macro(name).is_some() {
        return Ok(Expr::Symbol(name.to_owned(), pos));
    }

    let alias = if let Some(existing) = renamed.get(name) {
        existing.clone()
    } else {
        let alias = macro_def.env.fresh_symbol(name);
        if let Some(value) = macro_def.env.lookup(name) {
            macro_def.env.define(alias.clone(), value);
        }
        renamed.insert(name.to_owned(), alias.clone());
        alias
    };

    Ok(Expr::Symbol(alias, pos))
}

fn repeated_template_len(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
) -> Result<usize, EvalError> {
    let mut len = None;
    collect_repeated_template_len(template, bindings, &mut len)?;
    len.ok_or_else(|| {
        EvalError::Parse("ellipsis template must reference a repeated pattern variable".to_owned())
    })
}

fn collect_repeated_template_len(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    len: &mut Option<usize>,
) -> Result<(), EvalError> {
    match template {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => Ok(()),
        Expr::Symbol(name, _) => {
            if let Some(PatternBinding::Repeated(values)) = bindings.get(name) {
                match len {
                    Some(expected) if *expected != values.len() => Err(EvalError::Parse(
                        "mismatched ellipsis lengths in syntax-rules template".to_owned(),
                    )),
                    Some(_) => Ok(()),
                    None => {
                        *len = Some(values.len());
                        Ok(())
                    }
                }
            } else {
                Ok(())
            }
        }
        Expr::List(items, _) => {
            if is_quote_form(items) {
                return Ok(());
            }

            for item in items {
                collect_repeated_template_len(item, bindings, len)?;
            }
            Ok(())
        }
    }
}

fn is_ellipsis(expr: Option<&Expr>) -> bool {
    matches!(expr, Some(Expr::Symbol(name, _)) if name == "...")
}

fn is_quote_form(items: &[Expr]) -> bool {
    matches!(items, [Expr::Symbol(name, _), _] if name == "quote")
}

fn is_special_form_name(name: &str) -> bool {
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
            | "begin"
            | "cond"
            | "case"
            | "let"
            | "let*"
            | "letrec"
            | "letrec*"
            | "and"
            | "or"
            | "do"
            | "syntax-rules"
    )
}

fn expr_syntax_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Number(left, _), Expr::Number(right, _)) => left == right,
        (Expr::Boolean(left, _), Expr::Boolean(right, _)) => left == right,
        (Expr::String(left, _), Expr::String(right, _)) => left == right,
        (Expr::Char(left, _), Expr::Char(right, _)) => left == right,
        (Expr::Symbol(left, _), Expr::Symbol(right, _)) => left == right,
        (Expr::List(left, _), Expr::List(right, _)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_syntax_eq(left, right))
        }
        _ => false,
    }
}

fn eval_set(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let name = expect_symbol(&args[0], "set! target")?;
    let value = eval_expr(&args[1], env)?;

    if env.set(&name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundSymbol(name))
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            name: "if".to_owned(),
            expected: "2 or 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env)?;

    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else if let Some(alternate) = args.get(2) {
        eval_expr(alternate, env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_tail_if(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            name: "if".to_owned(),
            expected: "2 or 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env)?;

    if condition.is_truthy() {
        Ok(TailOutcome::Expr(args[1].clone(), env.clone()))
    } else if let Some(alternate) = args.get(2) {
        Ok(TailOutcome::Expr(alternate.clone(), env.clone()))
    } else {
        Ok(TailOutcome::Value(Value::Void))
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value, _) => Value::Number(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(SchemeString::new(value.clone())),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => list_from_vec(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (params_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("lambda requires parameters".to_owned()))?;

    if body.is_empty() {
        return Err(EvalError::Parse("lambda requires a body".to_owned()));
    }

    let params = parse_formals_expr(params_expr, "lambda")?;

    Ok(Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
        name: None,
        params,
        body: body.to_vec(),
        env: env.clone(),
    }))))
}

fn eval_case_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut clauses = Vec::with_capacity(args.len());

    for clause in args {
        let items = match clause {
            Expr::List(items, _) => items,
            _ => {
                return Err(EvalError::Parse(
                    "case-lambda clauses must be lists".to_owned(),
                ))
            }
        };
        let (params_expr, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("case-lambda clause cannot be empty".to_owned()))?;

        if body.is_empty() {
            return Err(EvalError::Parse(
                "case-lambda clause requires a body".to_owned(),
            ));
        }

        clauses.push(Lambda {
            name: None,
            params: parse_formals_expr(params_expr, "case-lambda")?,
            body: body.to_vec(),
            env: env.clone(),
        });
    }

    Ok(Value::Procedure(Rc::new(Procedure::CaseLambda(
        CaseLambda {
            name: None,
            clauses,
        },
    ))))
}

fn tail_sequence(exprs: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(TailOutcome::Value(Value::Void));
    };

    for expr in prefix {
        let _ = eval_expr(expr, env)?;
    }

    Ok(TailOutcome::Expr(last.clone(), env.clone()))
}

fn eval_begin(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_cond(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let items = match clause {
            Expr::List(items, _) => items,
            _ => {
                return Err(EvalError::Parse("cond clauses must be lists".to_owned()));
            }
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("cond clause cannot be empty".to_owned()))?;

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::Parse("cond else clause must be last".to_owned()));
            }
            return eval_sequence(body, env);
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

fn eval_tail_cond(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let items = match clause {
            Expr::List(items, _) => items,
            _ => return Err(EvalError::Parse("cond clauses must be lists".to_owned())),
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("cond clause cannot be empty".to_owned()))?;

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::Parse("cond else clause must be last".to_owned()));
            }
            return tail_sequence(body, env);
        }

        let value = eval_expr(test, env)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(TailOutcome::Value(value))
            } else {
                tail_sequence(body, env)
            };
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_case(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (key_expr, clauses) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("case requires a key expression".to_owned()))?;
    let key = eval_expr(key_expr, env)?;

    for (index, clause) in clauses.iter().enumerate() {
        let items = match clause {
            Expr::List(items, _) => items,
            _ => return Err(EvalError::Parse("case clauses must be lists".to_owned())),
        };
        let (datums_expr, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("case clause cannot be empty".to_owned()))?;

        if matches!(datums_expr, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Parse("case else clause must be last".to_owned()));
            }
            return eval_sequence(body, env);
        }

        let datums = match datums_expr {
            Expr::List(datums, _) => datums,
            _ => {
                return Err(EvalError::Parse(
                    "case datum lists must be lists".to_owned(),
                ))
            }
        };

        if datums
            .iter()
            .any(|datum| values_eqv(&key, &quote_expr(datum)))
        {
            return eval_sequence(body, env);
        }
    }

    Ok(Value::Void)
}

fn eval_tail_case(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let (key_expr, clauses) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("case requires a key expression".to_owned()))?;
    let key = eval_expr(key_expr, env)?;

    for (index, clause) in clauses.iter().enumerate() {
        let items = match clause {
            Expr::List(items, _) => items,
            _ => return Err(EvalError::Parse("case clauses must be lists".to_owned())),
        };
        let (datums_expr, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("case clause cannot be empty".to_owned()))?;

        if matches!(datums_expr, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Parse("case else clause must be last".to_owned()));
            }
            return tail_sequence(body, env);
        }

        let datums = match datums_expr {
            Expr::List(datums, _) => datums,
            _ => {
                return Err(EvalError::Parse(
                    "case datum lists must be lists".to_owned(),
                ))
            }
        };

        if datums
            .iter()
            .any(|datum| values_eqv(&key, &quote_expr(datum)))
        {
            return tail_sequence(body, env);
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_let(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (head, tail) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;

    match head {
        Expr::List(bindings, _) => {
            if tail.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }
            eval_regular_let(bindings, tail, env)
        }
        Expr::Symbol(name, _) => {
            let (bindings_expr, body) = tail
                .split_first()
                .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;
            if body.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }
            let bindings = match bindings_expr {
                Expr::List(bindings, _) => bindings,
                _ => {
                    return Err(EvalError::Parse("let bindings must be a list".to_owned()));
                }
            };
            eval_named_let(name, bindings, body, env)
        }
        _ => Err(EvalError::Parse("let requires bindings".to_owned())),
    }
}

fn eval_tail_let(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let (head, tail) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;

    match head {
        Expr::List(bindings, _) => {
            if tail.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }
            eval_tail_regular_let(bindings, tail, env)
        }
        Expr::Symbol(name, _) => {
            let (bindings_expr, body) = tail
                .split_first()
                .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;
            if body.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }
            let bindings = match bindings_expr {
                Expr::List(bindings, _) => bindings,
                _ => return Err(EvalError::Parse("let bindings must be a list".to_owned())),
            };
            eval_tail_named_let(name, bindings, body, env)
        }
        _ => Err(EvalError::Parse("let requires bindings".to_owned())),
    }
}

fn eval_regular_let(bindings: &[Expr], body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let scope = Environment::new(Some(env.clone()));

    for ((name, _), value) in bindings.into_iter().zip(values) {
        scope.define(name, value);
    }

    eval_sequence(body, &scope)
}

fn eval_tail_regular_let(
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
) -> Result<TailOutcome, EvalError> {
    let bindings = parse_bindings(bindings, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let scope = Environment::new(Some(env.clone()));

    for ((name, _), value) in bindings.into_iter().zip(values) {
        scope.define(name, value);
    }

    tail_sequence(body, &scope)
}

fn eval_let_star(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("let* requires bindings".to_owned()))?;
    if body.is_empty() {
        return Err(EvalError::Parse("let* requires a body".to_owned()));
    }

    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => return Err(EvalError::Parse("let* bindings must be a list".to_owned())),
    };
    let bindings = parse_bindings(bindings, "let*")?;
    let scope = Environment::new(Some(env.clone()));

    for (name, expr) in bindings {
        let value = eval_expr(&expr, &scope)?;
        scope.define(name, value);
    }

    eval_sequence(body, &scope)
}

fn eval_tail_let_star(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("let* requires bindings".to_owned()))?;
    if body.is_empty() {
        return Err(EvalError::Parse("let* requires a body".to_owned()));
    }

    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => return Err(EvalError::Parse("let* bindings must be a list".to_owned())),
    };
    let bindings = parse_bindings(bindings, "let*")?;
    let scope = Environment::new(Some(env.clone()));

    for (name, expr) in bindings {
        let value = eval_expr(&expr, &scope)?;
        scope.define(name, value);
    }

    tail_sequence(body, &scope)
}

fn eval_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let params = LambdaParams {
        fixed: params,
        rest: None,
    };
    let recursive_env = Environment::new(Some(env.clone()));

    recursive_env.define(
        name.to_owned(),
        Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
            name: Some(name.to_owned()),
            params: params.clone(),
            body: body.to_vec(),
            env: recursive_env.clone(),
        }))),
    );

    let call_env = Environment::new(Some(recursive_env));
    for (param, value) in params.fixed.into_iter().zip(values) {
        call_env.define(param, value);
    }

    eval_sequence(body, &call_env)
}

fn eval_tail_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
) -> Result<TailOutcome, EvalError> {
    let bindings = parse_bindings(bindings, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let params = LambdaParams {
        fixed: params,
        rest: None,
    };
    let recursive_env = Environment::new(Some(env.clone()));

    recursive_env.define(
        name.to_owned(),
        Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
            name: Some(name.to_owned()),
            params: params.clone(),
            body: body.to_vec(),
            env: recursive_env.clone(),
        }))),
    );

    let call_env = Environment::new(Some(recursive_env));
    for (param, value) in params.fixed.into_iter().zip(values) {
        call_env.define(param, value);
    }

    tail_sequence(body, &call_env)
}

fn eval_letrec(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("letrec requires bindings".to_owned()))?;
    if body.is_empty() {
        return Err(EvalError::Parse("letrec requires a body".to_owned()));
    }
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => {
            return Err(EvalError::Parse(
                "letrec bindings must be a list".to_owned(),
            ))
        }
    };
    let bindings = parse_bindings(bindings, "letrec")?;
    let scope = Environment::new(Some(env.clone()));

    for (name, _) in &bindings {
        scope.define(name.clone(), Value::Void);
    }

    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, &scope))
        .collect::<Result<Vec<_>, _>>()?;

    for ((name, _), value) in bindings.iter().zip(values) {
        let _ = scope.set(name, value);
    }

    eval_sequence(body, &scope)
}

fn eval_tail_letrec(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("letrec requires bindings".to_owned()))?;
    if body.is_empty() {
        return Err(EvalError::Parse("letrec requires a body".to_owned()));
    }
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => {
            return Err(EvalError::Parse(
                "letrec bindings must be a list".to_owned(),
            ))
        }
    };
    let bindings = parse_bindings(bindings, "letrec")?;
    let scope = Environment::new(Some(env.clone()));

    for (name, _) in &bindings {
        scope.define(name.clone(), Value::Void);
    }

    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, &scope))
        .collect::<Result<Vec<_>, _>>()?;

    for ((name, _), value) in bindings.iter().zip(values) {
        let _ = scope.set(name, value);
    }

    tail_sequence(body, &scope)
}

fn eval_letrec_star(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("letrec* requires bindings".to_owned()))?;
    if body.is_empty() {
        return Err(EvalError::Parse("letrec* requires a body".to_owned()));
    }
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => {
            return Err(EvalError::Parse(
                "letrec* bindings must be a list".to_owned(),
            ))
        }
    };
    let bindings = parse_bindings(bindings, "letrec*")?;
    let scope = Environment::new(Some(env.clone()));

    for (name, expr) in bindings {
        scope.define(name.clone(), Value::Void);
        let value = eval_expr(&expr, &scope)?;
        let _ = scope.set(&name, value);
    }

    eval_sequence(body, &scope)
}

fn eval_tail_letrec_star(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("letrec* requires bindings".to_owned()))?;
    if body.is_empty() {
        return Err(EvalError::Parse("letrec* requires a body".to_owned()));
    }
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => {
            return Err(EvalError::Parse(
                "letrec* bindings must be a list".to_owned(),
            ))
        }
    };
    let bindings = parse_bindings(bindings, "letrec*")?;
    let scope = Environment::new(Some(env.clone()));

    for (name, expr) in bindings {
        scope.define(name.clone(), Value::Void);
        let value = eval_expr(&expr, &scope)?;
        let _ = scope.set(&name, value);
    }

    tail_sequence(body, &scope)
}

#[derive(Clone)]
struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn eval_do(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (bindings_expr, tail) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("do requires bindings".to_owned()))?;
    let (test_clause, body) = tail
        .split_first()
        .ok_or_else(|| EvalError::Parse("do requires a test clause".to_owned()))?;

    let bindings = match bindings_expr {
        Expr::List(bindings, _) => parse_do_bindings(bindings)?,
        _ => return Err(EvalError::Parse("do bindings must be a list".to_owned())),
    };

    let test_items = match test_clause {
        Expr::List(items, _) => items,
        _ => return Err(EvalError::Parse("do test clause must be a list".to_owned())),
    };
    let (test_expr, results) = test_items
        .split_first()
        .ok_or_else(|| EvalError::Parse("do test clause cannot be empty".to_owned()))?;

    let init_values = bindings
        .iter()
        .map(|binding| eval_expr(&binding.init, env))
        .collect::<Result<Vec<_>, _>>()?;
    let scope = Environment::new(Some(env.clone()));
    for (binding, value) in bindings.iter().zip(init_values) {
        scope.define(binding.name.clone(), value);
    }

    loop {
        if eval_expr(test_expr, &scope)?.is_truthy() {
            return eval_sequence(results, &scope);
        }

        let _ = eval_sequence(body, &scope)?;

        let next_values = bindings
            .iter()
            .map(|binding| {
                if let Some(step) = &binding.step {
                    eval_expr(step, &scope)
                } else {
                    scope
                        .lookup(&binding.name)
                        .ok_or_else(|| EvalError::UnboundSymbol(binding.name.clone()))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (binding, value) in bindings.iter().zip(next_values) {
            let _ = scope.set(&binding.name, value);
        }
    }
}

fn parse_do_bindings(bindings: &[Expr]) -> Result<Vec<DoBinding>, EvalError> {
    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items, _) if items.len() == 2 || items.len() == 3 => Ok(DoBinding {
                name: expect_symbol(&items[0], "do binding name")?,
                init: items[1].clone(),
                step: items.get(2).cloned(),
            }),
            Expr::List(_, _) => Err(EvalError::Parse(
                "do bindings must contain 2 or 3 forms".to_owned(),
            )),
            _ => Err(EvalError::Parse("do bindings must be lists".to_owned())),
        })
        .collect()
}

fn parse_params(params: &[Expr], form: &str) -> Result<LambdaParams, EvalError> {
    let mut fixed = Vec::new();
    let mut rest = None;
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                if rest.is_some() || index + 1 >= params.len() || index + 2 != params.len() {
                    return Err(EvalError::Parse(format!(
                        "{form} parameters use invalid dotted form"
                    )));
                }
                rest = Some(expect_symbol(
                    &params[index + 1],
                    &format!("{form} rest parameter"),
                )?);
                break;
            }
            expr => fixed.push(expect_symbol(expr, &format!("{form} parameter"))?),
        }
        index += 1;
    }

    Ok(LambdaParams { fixed, rest })
}

fn parse_formals_expr(params_expr: &Expr, form: &str) -> Result<LambdaParams, EvalError> {
    match params_expr {
        Expr::List(items, _) => parse_params(items, form),
        Expr::Symbol(name, _) => Ok(LambdaParams {
            fixed: Vec::new(),
            rest: Some(name.clone()),
        }),
        _ => Err(EvalError::Parse(format!(
            "{form} parameters must be a list or symbol"
        ))),
    }
}

fn parse_bindings(bindings: &[Expr], form: &str) -> Result<Vec<(String, Expr)>, EvalError> {
    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items, _) if items.len() == 2 => Ok((
                expect_symbol(&items[0], &format!("{form} binding name"))?,
                items[1].clone(),
            )),
            Expr::List(_, _) => Err(EvalError::Parse(format!(
                "{form} bindings must contain exactly 2 forms"
            ))),
            _ => Err(EvalError::Parse(format!("{form} bindings must be lists"))),
        })
        .collect()
}

fn eval_binding_values(bindings: &[(String, Expr)], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env))
        .collect()
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name, _) => Ok(name.clone()),
        _ => Err(EvalError::Parse(format!("{context} must be a symbol"))),
    }
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(Value::Void);
    };

    for expr in prefix {
        let _ = eval_expr(expr, env)?;
    }

    eval_expr_tco(last, env)
}

fn eval_args(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval_expr(expr, env)).collect()
}

fn eval_apply_builtin(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "apply".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let operator = args[0].clone();
    let mut applied_args = args[1..args.len() - 1].to_vec();
    let tail = expect_list("apply", &args[args.len() - 1])?;
    applied_args.extend(tail);

    apply_values(operator, &applied_args, env)
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("+", args)?;

    if values.iter().any(|value| value.is_inexact()) {
        return Ok(Value::Number(Number::Inexact(
            values.iter().map(|value| value.to_f64()).sum(),
        )));
    }

    let mut sum = Rational::from_integer(0);
    for value in values {
        sum = sum
            .add(
                value
                    .as_exact_rational()
                    .expect("exact numbers should be rational"),
            )
            .ok_or_else(|| numeric_overflow("+"))?;
    }

    Ok(Value::Number(Number::from_rational(sum)))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("-", args)?;

    match values.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        }),
        [value] if value.is_inexact() => Ok(Value::Number(Number::Inexact(-value.to_f64()))),
        [value] => {
            let rational = value
                .as_exact_rational()
                .expect("exact numbers should be rational");
            let negated =
                Number::rational_i128(-(rational.numerator as i128), rational.denominator as i128)
                    .ok_or_else(|| numeric_overflow("-"))?;
            Ok(Value::Number(negated))
        }
        [first, rest @ ..] if values.iter().any(|value| value.is_inexact()) => {
            let result = rest
                .iter()
                .fold(first.to_f64(), |acc, value| acc - value.to_f64());
            Ok(Value::Number(Number::Inexact(result)))
        }
        [first, rest @ ..] => {
            let mut result = first
                .as_exact_rational()
                .expect("exact numbers should be rational");
            for value in rest {
                result = result
                    .sub(
                        value
                            .as_exact_rational()
                            .expect("exact numbers should be rational"),
                    )
                    .ok_or_else(|| numeric_overflow("-"))?;
            }
            Ok(Value::Number(Number::from_rational(result)))
        }
    }
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("*", args)?;

    if values.iter().any(|value| value.is_inexact()) {
        return Ok(Value::Number(Number::Inexact(
            values.iter().fold(1.0, |acc, value| acc * value.to_f64()),
        )));
    }

    let mut product = Rational::from_integer(1);
    for value in values {
        product = product
            .mul(
                value
                    .as_exact_rational()
                    .expect("exact numbers should be rational"),
            )
            .ok_or_else(|| numeric_overflow("*"))?;
    }

    Ok(Value::Number(Number::from_rational(product)))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("/", args)?;

    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "/".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: 0,
        })?;

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: 1,
        });
    }

    if values.iter().any(|value| value.is_inexact()) {
        let mut quotient = first.to_f64();
        for value in rest {
            if value.is_zero() {
                return Err(EvalError::DivisionByZero);
            }
            quotient /= value.to_f64();
        }
        return Ok(Value::Number(Number::Inexact(quotient)));
    }

    let mut quotient = first
        .as_exact_rational()
        .expect("exact numbers should be rational");
    for value in rest {
        let divisor = value
            .as_exact_rational()
            .expect("exact numbers should be rational");
        if divisor.is_zero() {
            return Err(EvalError::DivisionByZero);
        }
        quotient = quotient.div(divisor).ok_or_else(|| numeric_overflow("/"))?;
    }

    Ok(Value::Number(Number::from_rational(quotient)))
}

fn eval_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(Ordering) -> bool,
{
    let values = eval_number_args(name, args)?;

    for window in values.windows(2) {
        if !predicate(compare_numbers(name, window[0], window[1])?) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "cons".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok(cons_value(args[0].clone(), args[1].clone()))
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "car".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(expect_pair("car", &args[0])?.car())
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "cdr".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(expect_pair("cdr", &args[0])?.cdr())
}

fn eval_set_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set-car!".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    expect_pair("set-car!", &args[0])?.set_car(args[1].clone());
    Ok(Value::Void)
}

fn eval_set_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set-cdr!".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    expect_pair("set-cdr!", &args[0])?.set_cdr(args[1].clone());
    Ok(Value::Void)
}

fn eval_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut combined = Vec::new();

    for value in args {
        combined.extend(expect_list("append", value)?);
    }

    Ok(list_from_vec(combined))
}

fn eval_list_builtin(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_from_vec(args.to_vec()))
}

fn eval_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "length".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_list("length", &args[0])?;
    Ok(Value::Number(Number::Integer(items.len() as i64)))
}

fn eval_null_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "null?".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(matches!(&args[0], Value::EmptyList)))
}

fn eval_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "pair?", |value| matches!(value, Value::Pair(_)))
}

fn eval_list_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "list?", is_proper_list)
}

fn eval_symbol_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "symbol?", |value| matches!(value, Value::Symbol(_)))
}

fn eval_string_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "string?", |value| matches!(value, Value::String(_)))
}

fn eval_number_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_number_type_predicate(args, "number?", |_| true)
}

fn eval_exact_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_number_type_predicate(args, "exact?", Number::is_exact)
}

fn eval_inexact_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_number_type_predicate(args, "inexact?", Number::is_inexact)
}

fn eval_integer_type_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_number_type_predicate(args, "integer?", Number::is_integer)
}

fn eval_rational_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_number_type_predicate(args, "rational?", Number::is_rational)
}

fn eval_boolean_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "boolean?", |value| matches!(value, Value::Boolean(_)))
}

fn eval_procedure_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "procedure?", |value| {
        matches!(value, Value::Procedure(_))
    })
}

fn eval_display(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "display".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    env.output.borrow_mut().push_str(&args[0].render_display());
    Ok(Value::Void)
}

fn eval_write(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "write".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    env.output.borrow_mut().push_str(&args[0].render());
    Ok(Value::Void)
}

fn eval_newline(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline".to_owned(),
            expected: "exactly 0 arguments".to_owned(),
            got: args.len(),
        });
    }

    env.output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn eval_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut combined = String::new();

    for value in args {
        combined.push_str(&expect_string("string-append", value)?.contents());
    }

    Ok(Value::String(SchemeString::new(combined)))
}

fn eval_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-length".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let value = expect_string("string-length", &args[0])?;
    Ok(Value::Number(Number::Integer(value.len_chars() as i64)))
}

fn eval_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "substring".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let string = expect_string("substring", &args[0])?;
    let start = args[1].as_integer("substring")?;
    let end = args[2].as_integer("substring")?;
    let chars = string.contents().chars().collect::<Vec<_>>();
    let len = chars.len();

    if start < 0 || end < start || end as usize > len {
        return Err(EvalError::InvalidRange {
            name: "substring".to_owned(),
            start,
            end,
            len,
        });
    }

    Ok(Value::String(SchemeString::new(
        chars[start as usize..end as usize]
            .iter()
            .collect::<String>(),
    )))
}

fn eval_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->number".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let value = expect_string("string->number", &args[0])?;
    Ok(match parse_number_literal(&value.contents()) {
        Some(number) => Value::Number(number),
        None => Value::Boolean(false),
    })
}

fn eval_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "number->string".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        args[0].as_number("number->string")?.render(),
    )))
}

fn eval_exact_to_inexact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "exact->inexact".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let number = args[0].as_number("exact->inexact")?;
    Ok(Value::Number(match number {
        Number::Inexact(_) => number,
        exact => Number::Inexact(exact.to_f64()),
    }))
}

fn eval_inexact_to_exact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "inexact->exact".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let number = args[0].as_number("inexact->exact")?;
    let exact = number
        .exactified()
        .ok_or_else(|| EvalError::InvalidArgument {
            name: "inexact->exact".to_owned(),
            message: "cannot convert to an exact number".to_owned(),
        })?;
    Ok(Value::Number(exact))
}

fn eval_numerator(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "numerator".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let number = args[0]
        .as_number("numerator")?
        .exactified()
        .ok_or_else(|| EvalError::InvalidArgument {
            name: "numerator".to_owned(),
            message: "cannot extract a numerator".to_owned(),
        })?;
    let rational = number
        .as_exact_rational()
        .expect("exactified numbers should be rational");
    Ok(Value::Number(Number::Integer(rational.numerator)))
}

fn eval_denominator(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "denominator".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let number = args[0]
        .as_number("denominator")?
        .exactified()
        .ok_or_else(|| EvalError::InvalidArgument {
            name: "denominator".to_owned(),
            message: "cannot extract a denominator".to_owned(),
        })?;
    let rational = number
        .as_exact_rational()
        .expect("exactified numbers should be rational");
    Ok(Value::Number(Number::Integer(rational.denominator)))
}

fn eval_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        expect_symbol_value("symbol->string", &args[0])?.to_owned(),
    )))
}

fn eval_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Symbol(
        expect_string("string->symbol", &args[0])?.contents(),
    ))
}

fn eval_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "string-ref".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let string = expect_string("string-ref", &args[0])?;
    let index = args[1].as_integer("string-ref")?;
    let len = string.len_chars();

    if index < 0 {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref".to_owned(),
            index,
            len,
        });
    }

    let Some(ch) = string.char_at(index as usize) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref".to_owned(),
            index,
            len,
        });
    };

    Ok(Value::Char(ch))
}

fn eval_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-copy".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(
        expect_string("string-copy", &args[0])?.copy_string(),
    ))
}

fn eval_string_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "string-set!".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let string = expect_string("string-set!", &args[0])?;
    let index = args[1].as_integer("string-set!")?;
    let ch = expect_char("string-set!", &args[2])?;

    if !string.is_mutable() {
        return Err(EvalError::ImmutableString {
            name: "string-set!".to_owned(),
        });
    }

    let len = string.len_chars();
    if index < 0 || !string.set_char(index as usize, ch) {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-set!".to_owned(),
            index,
            len,
        });
    }

    Ok(Value::Void)
}

fn eval_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->list".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(list_from_vec(
        expect_string("string->list", &args[0])?
            .contents()
            .chars()
            .map(Value::Char)
            .collect(),
    ))
}

fn eval_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "list->string".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let chars = expect_list("list->string", &args[0])?
        .iter()
        .map(|value| expect_char("list->string", value))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::String(SchemeString::new(
        chars.into_iter().collect::<String>(),
    )))
}

fn eval_char_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "char?", |value| matches!(value, Value::Char(_)))
}

fn eval_char_alphabetic_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_char_predicate(args, "char-alphabetic?", |value| value.is_alphabetic())
}

fn eval_char_numeric_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_char_predicate(args, "char-numeric?", |value| value.is_numeric())
}

fn eval_char_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-upcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let ch = expect_char("char-upcase", &args[0])?;
    Ok(Value::Char(ch.to_uppercase().next().unwrap_or(ch)))
}

fn eval_char_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-downcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let ch = expect_char("char-downcase", &args[0])?;
    Ok(Value::Char(ch.to_lowercase().next().unwrap_or(ch)))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_tail_and(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Boolean(true)));
    };

    for expr in prefix {
        let value = eval_expr(expr, env)?;
        if !value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Expr(last.clone(), env.clone()))
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_tail_or(args: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Boolean(false)));
    };

    for expr in prefix {
        let value = eval_expr(expr, env)?;
        if value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Expr(last.clone(), env.clone()))
}

fn eval_eq_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "eq?".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(values_eq(&args[0], &args[1])))
}

fn eval_eqv_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "eqv?".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(values_eqv(&args[0], &args[1])))
}

fn eval_equal_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "equal?".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(values_equal(&args[0], &args[1])))
}

fn eval_abs(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "abs".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let number = args[0].as_number("abs")?;
    let result = match number {
        Number::Inexact(value) => Number::Inexact(value.abs()),
        exact => Number::from_rational(
            exact
                .as_exact_rational()
                .expect("exact numbers should be rational")
                .abs()
                .ok_or_else(|| numeric_overflow("abs"))?,
        ),
    };
    Ok(Value::Number(result))
}

fn eval_modulo(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_integer_pair("modulo", args)?;

    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let mut result = dividend % divisor;
    if result != 0 && ((result > 0) != (divisor > 0)) {
        result += divisor;
    }

    Ok(Value::Number(Number::Integer(result)))
}

fn eval_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_integer_pair("remainder", args)?;

    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(Value::Number(Number::Integer(dividend % divisor)))
}

fn eval_quotient(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_integer_pair("quotient", args)?;

    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(Value::Number(Number::Integer(dividend / divisor)))
}

fn eval_min(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("min", args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "min".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        })?;

    let mut best = *first;
    for value in rest {
        if compare_numbers("min", *value, best)? == Ordering::Less {
            best = *value;
        }
    }

    if values.iter().any(|value| value.is_inexact()) {
        best = Number::Inexact(best.to_f64());
    }

    Ok(Value::Number(best))
}

fn eval_max(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("max", args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "max".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        })?;

    let mut best = *first;
    for value in rest {
        if compare_numbers("max", *value, best)? == Ordering::Greater {
            best = *value;
        }
    }

    if values.iter().any(|value| value.is_inexact()) {
        best = Number::Inexact(best.to_f64());
    }

    Ok(Value::Number(best))
}

fn eval_expt(args: &[Value]) -> Result<Value, EvalError> {
    let (base, exponent) = eval_integer_pair("expt", args)?;

    if exponent < 0 {
        return Err(EvalError::InvalidArgument {
            name: "expt".to_owned(),
            message: "exponent must be non-negative".to_owned(),
        });
    }

    Ok(Value::Number(Number::Integer(base.pow(exponent as u32))))
}

fn eval_integer_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(args[0].as_integer(name)?)))
}

fn eval_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "list-ref".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_list("list-ref", &args[0])?;
    let index = args[1].as_integer("list-ref")?;

    if index < 0 || index as usize >= items.len() {
        return Err(EvalError::IndexOutOfBounds {
            name: "list-ref".to_owned(),
            index,
            len: items.len(),
        });
    }

    Ok(items[index as usize].clone())
}

fn eval_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "list-tail".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_list("list-tail", &args[0])?;
    let index = args[1].as_integer("list-tail")?;

    if index < 0 || index as usize > items.len() {
        return Err(EvalError::IndexOutOfBounds {
            name: "list-tail".to_owned(),
            index,
            len: items.len(),
        });
    }

    Ok(list_from_vec(items[index as usize..].to_vec()))
}

fn eval_assoc(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "assoc".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let key = &args[0];
    let entries = expect_list("assoc", &args[1])?;

    for entry in entries {
        let candidate = match &entry {
            Value::Pair(pair) => pair.car(),
            _ => {
                return Err(EvalError::ExpectedPair {
                    name: "assoc".to_owned(),
                });
            }
        };

        if values_equal(&candidate, key) {
            return Ok(entry.clone());
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(SchemeVector::new(args.to_vec())))
}

fn eval_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::WrongArgCount {
            name: "make-vector".to_owned(),
            expected: "1 or 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let len = args[0].as_integer("make-vector")?;
    if len < 0 {
        return Err(EvalError::InvalidArgument {
            name: "make-vector".to_owned(),
            message: "length must be non-negative".to_owned(),
        });
    }

    let fill = args.get(1).cloned().unwrap_or(Value::Void);
    Ok(Value::Vector(SchemeVector::new(vec![fill; len as usize])))
}

fn eval_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "vector-ref".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let vector = expect_vector("vector-ref", &args[0])?;
    let index = args[1].as_integer("vector-ref")?;
    let len = vector.len();

    if index < 0 {
        return Err(EvalError::IndexOutOfBounds {
            name: "vector-ref".to_owned(),
            index,
            len,
        });
    }

    vector
        .get(index as usize)
        .ok_or_else(|| EvalError::IndexOutOfBounds {
            name: "vector-ref".to_owned(),
            index,
            len,
        })
}

fn eval_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "vector-set!".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let vector = expect_vector("vector-set!", &args[0])?;
    let index = args[1].as_integer("vector-set!")?;
    let len = vector.len();

    if index < 0 || !vector.set(index as usize, args[2].clone()) {
        return Err(EvalError::IndexOutOfBounds {
            name: "vector-set!".to_owned(),
            index,
            len,
        });
    }

    Ok(Value::Void)
}

fn eval_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "vector-length".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Number(Number::Integer(
        expect_vector("vector-length", &args[0])?.len() as i64,
    )))
}

fn eval_vector_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "vector?", |value| matches!(value, Value::Vector(_)))
}

fn eval_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "vector->list".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(list_from_vec(expect_vector("vector->list", &args[0])?.contents()))
}

fn eval_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "list->vector".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Vector(SchemeVector::new(expect_list(
        "list->vector",
        &args[0],
    )?)))
}

fn eval_map_builtin(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let operator = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(|value| expect_list("map", value))
        .collect::<Result<Vec<_>, _>>()?;
    let len = lists.first().map_or(0, |list| list.len());

    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            name: "map".to_owned(),
            message: "list arguments must have the same length".to_owned(),
        });
    }

    let mut results = Vec::with_capacity(len);
    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_values(operator.clone(), &call_args, env)?);
    }

    Ok(list_from_vec(results))
}

fn eval_for_each_builtin(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "for-each".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let operator = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(|value| expect_list("for-each", value))
        .collect::<Result<Vec<_>, _>>()?;
    let len = lists.first().map_or(0, |list| list.len());

    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            name: "for-each".to_owned(),
            message: "list arguments must have the same length".to_owned(),
        });
    }

    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        let _ = apply_values(operator.clone(), &call_args, env)?;
    }

    Ok(Value::Void)
}

fn eval_number_args(name: &str, args: &[Value]) -> Result<Vec<Number>, EvalError> {
    args.iter().map(|value| value.as_number(name)).collect()
}

fn eval_integer_args(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|value| value.as_integer(name)).collect()
}

fn eval_integer_pair(name: &str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok((args[0].as_integer(name)?, args[1].as_integer(name)?))
}

fn compare_numbers(name: &str, left: Number, right: Number) -> Result<Ordering, EvalError> {
    if let (Some(left), Some(right)) = (left.comparable_rational(), right.comparable_rational()) {
        return Ok(left.cmp(right));
    }

    left.to_f64()
        .partial_cmp(&right.to_f64())
        .ok_or_else(|| EvalError::InvalidArgument {
            name: name.to_owned(),
            message: "cannot compare NaN".to_owned(),
        })
}

fn numbers_equal(left: Number, right: Number) -> bool {
    match (left.comparable_rational(), right.comparable_rational()) {
        (Some(left), Some(right)) => left == right,
        _ => left.to_f64() == right.to_f64(),
    }
}

fn numeric_overflow(name: &str) -> EvalError {
    EvalError::InvalidArgument {
        name: name.to_owned(),
        message: "numeric overflow".to_owned(),
    }
}

fn eval_string_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let values = args
        .iter()
        .map(|value| expect_string(name, value).map(|string| string.contents()))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        values
            .windows(2)
            .all(|window| predicate(&window[0], &window[1])),
    ))
}

fn eval_string_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-upcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        expect_string("string-upcase", &args[0])?
            .contents()
            .to_uppercase(),
    )))
}

fn eval_string_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-downcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        expect_string("string-downcase", &args[0])?
            .contents()
            .to_lowercase(),
    )))
}

fn eval_char_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let values = args
        .iter()
        .map(|value| expect_char(name, value))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        values
            .windows(2)
            .all(|window| predicate(window[0], window[1])),
    ))
}

fn eval_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char->integer".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Number(Number::Integer(
        expect_char("char->integer", &args[0])? as i64,
    )))
}

fn eval_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "integer->char".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let code = args[0].as_integer("integer->char")?;
    let ch = u32::try_from(code)
        .ok()
        .and_then(char::from_u32)
        .ok_or_else(|| EvalError::InvalidCharacterCode {
            name: "integer->char".to_owned(),
            code,
        })?;

    Ok(Value::Char(ch))
}

fn eval_char_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(char) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(expect_char(name, &args[0])?)))
}

fn eval_number_type_predicate<F>(
    args: &[Value],
    name: &str,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(Number) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(match &args[0] {
        Value::Number(number) => predicate(*number),
        _ => false,
    }))
}

fn eval_number_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(Number) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(args[0].as_number(name)?)))
}

fn eval_type_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
}

fn expect_string<'a>(name: &str, value: &'a Value) -> Result<&'a SchemeString, EvalError> {
    match value {
        Value::String(value) => Ok(value),
        _ => Err(EvalError::ExpectedString {
            name: name.to_owned(),
        }),
    }
}

fn expect_char(name: &str, value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(value) => Ok(*value),
        _ => Err(EvalError::ExpectedChar {
            name: name.to_owned(),
        }),
    }
}

fn expect_symbol_value<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(value) => Ok(value),
        _ => Err(EvalError::ExpectedSymbol {
            name: name.to_owned(),
        }),
    }
}

fn expect_list(name: &str, value: &Value) -> Result<Vec<Value>, EvalError> {
    collect_proper_list(value).ok_or_else(|| EvalError::ExpectedList {
        name: name.to_owned(),
    })
}

fn expect_vector<'a>(name: &str, value: &'a Value) -> Result<&'a SchemeVector, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector),
        _ => Err(EvalError::ExpectedVector {
            name: name.to_owned(),
        }),
    }
}

fn expect_record<'a>(
    name: &str,
    value: &'a Value,
    record_type: &Rc<RecordType>,
) -> Result<&'a RecordInstance, EvalError> {
    match value {
        Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type) => Ok(record),
        _ => Err(EvalError::ExpectedRecord {
            name: name.to_owned(),
            record_type: record_type.name.clone(),
        }),
    }
}

fn values_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => numbers_equal(*left, *right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.value, &right.value),
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(&left.values, &right.values),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn values_eqv(left: &Value, right: &Value) -> bool {
    values_eq(left, right)
}

fn values_equal(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    let mut seen_vectors = HashSet::new();
    values_equal_inner(left, right, &mut seen_pairs, &mut seen_vectors)
}

fn values_equal_inner(
    left: &Value,
    right: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
    seen_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => numbers_equal(*left, *right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.contents() == right.contents(),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_pair), Value::Pair(right_pair)) => {
            let key = (pair_id(left_pair), pair_id(right_pair));
            if !seen_pairs.insert(key) {
                return true;
            }

            values_equal_inner(&left_pair.car(), &right_pair.car(), seen_pairs, seen_vectors)
                && values_equal_inner(&left_pair.cdr(), &right_pair.cdr(), seen_pairs, seen_vectors)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (
                Rc::as_ptr(&left.values) as usize,
                Rc::as_ptr(&right.values) as usize,
            );
            if !seen_vectors.insert(key) {
                return true;
            }

            let left = left.contents();
            let right = right.contents();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| values_equal_inner(left, right, seen_pairs, seen_vectors))
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

#[derive(Clone)]
enum MachineState {
    Expr(Expr, EnvRef),
    Value(Value),
}

#[derive(Clone)]
enum EvalCont {
    Done,
    Frame(EvalFrame, Rc<EvalCont>),
}

type EvalContRef = Rc<EvalCont>;

#[derive(Clone)]
struct CapturedContinuation {
    cont: EvalContRef,
}

#[derive(Clone)]
enum ArgOperand {
    Expr(Expr),
    Value(Value),
}

#[derive(Clone)]
enum EvalFrame {
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        name: String,
        env: EnvRef,
        pos: SourcePos,
    },
    If {
        consequent: Expr,
        alternate: Option<Expr>,
        env: EnvRef,
    },
    ApplyOperator {
        operator_expr: Expr,
        args: Vec<ArgOperand>,
        env: EnvRef,
        pos: SourcePos,
    },
    ApplyArgs {
        operator: Value,
        operator_expr: Expr,
        prefix_operands: Vec<ArgOperand>,
        current_operand: Expr,
        evaluated: Vec<Value>,
        remaining: Vec<ArgOperand>,
        env: EnvRef,
        pos: SourcePos,
    },
    ReplayApplication {
        operator_expr: Expr,
        prefix_operands: Vec<ArgOperand>,
        suffix_operands: Vec<ArgOperand>,
        env: EnvRef,
        pos: SourcePos,
    },
}

fn done_cont() -> EvalContRef {
    Rc::new(EvalCont::Done)
}

fn push_cont(frame: EvalFrame, next: EvalContRef) -> EvalContRef {
    Rc::new(EvalCont::Frame(frame, next))
}

fn continuation_context_error() -> EvalError {
    EvalError::InvalidArgument {
        name: "continuation".to_owned(),
        message: "internal continuation cannot be applied in this context".to_owned(),
    }
}

fn capture_continuation(cont: &EvalContRef) -> EvalContRef {
    match cont.as_ref() {
        EvalCont::Done => done_cont(),
        EvalCont::Frame(frame, next) => push_cont(
            match frame {
                EvalFrame::Sequence { .. }
                | EvalFrame::DefineValue { .. }
                | EvalFrame::SetValue { .. }
                | EvalFrame::If { .. }
                | EvalFrame::ApplyOperator { .. }
                | EvalFrame::ReplayApplication { .. } => frame.clone(),
                EvalFrame::ApplyArgs {
                    operator_expr,
                    prefix_operands,
                    remaining,
                    env,
                    pos,
                    ..
                } => EvalFrame::ReplayApplication {
                    operator_expr: operator_expr.clone(),
                    prefix_operands: prefix_operands.clone(),
                    suffix_operands: remaining.clone(),
                    env: env.clone(),
                    pos: *pos,
                },
            },
            capture_continuation(next),
        ),
    }
}

fn symbol_expr(name: impl Into<String>, pos: SourcePos) -> Expr {
    Expr::Symbol(name.into(), pos)
}

fn list_expr(items: Vec<Expr>, pos: SourcePos) -> Expr {
    Expr::List(items, pos)
}

fn begin_expr(exprs: &[Expr], pos: SourcePos) -> Expr {
    if exprs.len() == 1 {
        exprs[0].clone()
    } else {
        let mut items = vec![symbol_expr("begin", pos)];
        items.extend(exprs.iter().cloned());
        list_expr(items, pos)
    }
}

fn enter_sequence(exprs: &[Expr], env: &EnvRef, cont: EvalContRef) -> (MachineState, EvalContRef) {
    match exprs.split_first() {
        None => (MachineState::Value(Value::Void), cont),
        Some((first, rest)) => {
            let cont = if rest.is_empty() {
                cont
            } else {
                push_cont(
                    EvalFrame::Sequence {
                        remaining: rest.to_vec(),
                        env: env.clone(),
                    },
                    cont,
                )
            };
            (MachineState::Expr(first.clone(), env.clone()), cont)
        }
    }
}

fn expand_let_expr(args: &[Expr], pos: SourcePos) -> Result<Expr, EvalError> {
    let (head, tail) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;

    match head {
        Expr::List(bindings, _) => {
            if tail.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }

            let bindings = parse_bindings(bindings, "let")?;
            let mut lambda_items = vec![
                symbol_expr("lambda", pos),
                list_expr(
                    bindings
                        .iter()
                        .map(|(name, _)| symbol_expr(name.clone(), pos))
                        .collect(),
                    pos,
                ),
            ];
            lambda_items.extend(tail.iter().cloned());

            let mut call_items = vec![list_expr(lambda_items, pos)];
            call_items.extend(bindings.into_iter().map(|(_, expr)| expr));
            Ok(list_expr(call_items, pos))
        }
        Expr::Symbol(name, _) => {
            let (bindings_expr, body) = tail
                .split_first()
                .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;
            if body.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }

            let bindings = match bindings_expr {
                Expr::List(bindings, _) => parse_bindings(bindings, "let")?,
                _ => {
                    return Err(EvalError::Parse("let bindings must be a list".to_owned()));
                }
            };

            let mut signature_items = vec![symbol_expr(name.clone(), pos)];
            signature_items.extend(
                bindings
                    .iter()
                    .map(|(binding, _)| symbol_expr(binding.clone(), pos)),
            );

            let mut define_items = vec![
                symbol_expr("define", pos),
                list_expr(signature_items, pos),
            ];
            define_items.extend(body.iter().cloned());

            let mut invoke_items = vec![symbol_expr(name.clone(), pos)];
            invoke_items.extend(bindings.into_iter().map(|(_, expr)| expr));

            let inner_lambda = list_expr(
                vec![
                    symbol_expr("lambda", pos),
                    list_expr(Vec::new(), pos),
                    list_expr(define_items, pos),
                    list_expr(invoke_items, pos),
                ],
                pos,
            );

            Ok(list_expr(vec![inner_lambda], pos))
        }
        _ => Err(EvalError::Parse("let requires bindings".to_owned())),
    }
}

fn expand_cond_expr(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Expr, EvalError> {
    fn expand_cond_clauses(clauses: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Expr, EvalError> {
        let Some((clause, rest)) = clauses.split_first() else {
            return Ok(begin_expr(&[], pos));
        };

        let items = match clause {
            Expr::List(items, _) => items,
            _ => return Err(EvalError::Parse("cond clauses must be lists".to_owned())),
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("cond clause cannot be empty".to_owned()))?;

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if !rest.is_empty() {
                return Err(EvalError::Parse("cond else clause must be last".to_owned()));
            }
            return Ok(begin_expr(body, pos));
        }

        let alternate = expand_cond_clauses(rest, env, pos)?;
        if body.is_empty() {
            let temp = env.fresh_symbol("cond");
            let temp_symbol = symbol_expr(temp.clone(), pos);
            let lambda = list_expr(
                vec![
                    symbol_expr("lambda", pos),
                    list_expr(vec![symbol_expr(temp, pos)], pos),
                    list_expr(
                        vec![
                            symbol_expr("if", pos),
                            temp_symbol.clone(),
                            temp_symbol,
                            alternate,
                        ],
                        pos,
                    ),
                ],
                pos,
            );

            Ok(list_expr(vec![lambda, test.clone()], pos))
        } else {
            Ok(list_expr(
                vec![
                    symbol_expr("if", pos),
                    test.clone(),
                    begin_expr(body, pos),
                    alternate,
                ],
                pos,
            ))
        }
    }

    expand_cond_clauses(args, env, pos)
}

fn machine_start_define(
    args: &[Expr],
    env: &EnvRef,
    cont: EvalContRef,
) -> Result<(MachineState, EvalContRef), EvalError> {
    let (target, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("define requires a target".to_owned()))?;

    match target {
        Expr::Symbol(name, _) => {
            if body.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define".to_owned(),
                    expected: "exactly 2 forms".to_owned(),
                    got: args.len(),
                });
            }

            Ok((
                MachineState::Expr(body[0].clone(), env.clone()),
                push_cont(
                    EvalFrame::DefineValue {
                        name: name.clone(),
                        env: env.clone(),
                    },
                    cont,
                ),
            ))
        }
        Expr::List(signature, _) => {
            let (name_expr, params_exprs) = signature
                .split_first()
                .ok_or_else(|| EvalError::Parse("define requires a function name".to_owned()))?;
            let name = expect_symbol(name_expr, "define function name")?;

            if body.is_empty() {
                return Err(EvalError::Parse(
                    "define requires a function body".to_owned(),
                ));
            }

            let params = parse_params(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env: env.clone(),
            })));

            env.define(name, procedure);
            Ok((MachineState::Value(Value::Void), cont))
        }
        _ => Err(EvalError::Parse(
            "define target must be a symbol".to_owned(),
        )),
    }
}

fn machine_start_set(
    args: &[Expr],
    env: &EnvRef,
    cont: EvalContRef,
    pos: SourcePos,
) -> Result<(MachineState, EvalContRef), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let name = expect_symbol(&args[0], "set! target")?;
    Ok((
        MachineState::Expr(args[1].clone(), env.clone()),
        push_cont(
            EvalFrame::SetValue {
                name,
                env: env.clone(),
                pos,
            },
            cont,
        ),
    ))
}

fn machine_start_if(
    args: &[Expr],
    env: &EnvRef,
    cont: EvalContRef,
) -> Result<(MachineState, EvalContRef), EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            name: "if".to_owned(),
            expected: "2 or 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok((
        MachineState::Expr(args[0].clone(), env.clone()),
        push_cont(
            EvalFrame::If {
                consequent: args[1].clone(),
                alternate: args.get(2).cloned(),
                env: env.clone(),
            },
            cont,
        ),
    ))
}

fn machine_enter_list(
    items: Vec<Expr>,
    pos: SourcePos,
    env: EnvRef,
    cont: EvalContRef,
) -> Result<(MachineState, EvalContRef), EvalError> {
    let (head, args) = items
        .split_first()
        .ok_or_else(|| EvalError::InvalidApplication.with_position(pos))?;

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return machine_start_define(args, &env, cont).map_err(|err| err.with_position(pos)),
            "define-record-type" => {
                return Ok((
                    MachineState::Value(
                        eval_define_record_type(args, &env)
                            .map_err(|err| err.with_position(pos))?,
                    ),
                    cont,
                ))
            }
            "define-syntax" => {
                return Ok((
                    MachineState::Value(
                        eval_define_syntax(args, &env)
                            .map_err(|err| err.with_position(pos))?,
                    ),
                    cont,
                ))
            }
            "set!" => return machine_start_set(args, &env, cont, pos).map_err(|err| err.with_position(pos)),
            "if" => return machine_start_if(args, &env, cont).map_err(|err| err.with_position(pos)),
            "quote" => {
                return Ok((
                    MachineState::Value(eval_quote(args).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "lambda" => {
                return Ok((
                    MachineState::Value(eval_lambda(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "case-lambda" => {
                return Ok((
                    MachineState::Value(
                        eval_case_lambda(args, &env).map_err(|err| err.with_position(pos))?,
                    ),
                    cont,
                ))
            }
            "begin" => return Ok(enter_sequence(args, &env, cont)),
            "cond" => {
                let expanded = expand_cond_expr(args, &env, pos).map_err(|err| err.with_position(pos))?;
                return Ok((MachineState::Expr(expanded, env), cont));
            }
            "let" => {
                let expanded = expand_let_expr(args, pos).map_err(|err| err.with_position(pos))?;
                return Ok((MachineState::Expr(expanded, env), cont));
            }
            "case" => {
                return Ok((
                    MachineState::Value(eval_case(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "let*" => {
                return Ok((
                    MachineState::Value(eval_let_star(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "letrec" => {
                return Ok((
                    MachineState::Value(eval_letrec(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "letrec*" => {
                return Ok((
                    MachineState::Value(
                        eval_letrec_star(args, &env).map_err(|err| err.with_position(pos))?,
                    ),
                    cont,
                ))
            }
            "and" => {
                return Ok((
                    MachineState::Value(eval_and(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "or" => {
                return Ok((
                    MachineState::Value(eval_or(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            "do" => {
                return Ok((
                    MachineState::Value(eval_do(args, &env).map_err(|err| err.with_position(pos))?),
                    cont,
                ))
            }
            _ => {}
        }

        if let Some(macro_def) = env.lookup_macro(name) {
            let expanded = expand_macro_use(&macro_def, &Expr::List(items.clone(), pos))
                .map_err(|err| err.with_position(pos))?;
            return Ok((MachineState::Expr(expanded, env), cont));
        }
    }

    Ok((
        MachineState::Expr(head.clone(), env.clone()),
        push_cont(
            EvalFrame::ApplyOperator {
                operator_expr: head.clone(),
                args: args.iter().cloned().map(ArgOperand::Expr).collect(),
                env,
                pos,
            },
            cont,
        ),
    ))
}

fn machine_apply(
    operator: Value,
    args: Vec<Value>,
    env: &EnvRef,
    cont: EvalContRef,
) -> Result<(MachineState, EvalContRef), EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(Builtin::CallCc) => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCount {
                        name: "call/cc".to_owned(),
                        expected: "exactly 1 argument".to_owned(),
                        got: args.len(),
                    });
                }

                let continuation = Value::Procedure(Rc::new(Procedure::Continuation(
                    CapturedContinuation {
                        cont: capture_continuation(&cont),
                    },
                )));
                machine_apply(args[0].clone(), vec![continuation], env, cont)
            }
            Procedure::Builtin(Builtin::Apply) => {
                if args.len() < 2 {
                    return Err(EvalError::WrongArgCount {
                        name: "apply".to_owned(),
                        expected: "at least 2 arguments".to_owned(),
                        got: args.len(),
                    });
                }

                let operator = args[0].clone();
                let mut applied_args = args[1..args.len() - 1].to_vec();
                let tail = expect_list("apply", &args[args.len() - 1])?;
                applied_args.extend(tail);
                machine_apply(operator, applied_args, env, cont)
            }
            Procedure::Builtin(builtin) => Ok((
                MachineState::Value(apply_builtin(*builtin, &args, env)?),
                cont,
            )),
            Procedure::Lambda(lambda) => {
                let call_env = prepare_lambda_call(
                    lambda,
                    &args,
                    lambda.name.as_deref().unwrap_or("lambda"),
                )?;
                Ok(enter_sequence(&lambda.body, &call_env, cont))
            }
            Procedure::CaseLambda(case_lambda) => {
                let clause = select_case_lambda_clause(case_lambda, &args)?;
                let call_env = prepare_lambda_call(
                    clause,
                    &args,
                    case_lambda.name.as_deref().unwrap_or("case-lambda"),
                )?;
                Ok(enter_sequence(&clause.body, &call_env, cont))
            }
            Procedure::RecordConstructor(constructor) => Ok((
                MachineState::Value(apply_record_constructor(constructor, &args)?),
                cont,
            )),
            Procedure::RecordPredicate(predicate) => Ok((
                MachineState::Value(apply_record_predicate(predicate, &args)?),
                cont,
            )),
            Procedure::RecordAccessor(accessor) => Ok((
                MachineState::Value(apply_record_accessor(accessor, &args)?),
                cont,
            )),
            Procedure::Continuation(captured) => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCount {
                        name: "continuation".to_owned(),
                        expected: "exactly 1 argument".to_owned(),
                        got: args.len(),
                    });
                }

                Ok((MachineState::Value(args[0].clone()), captured.cont.clone()))
            }
        },
        _ => Err(EvalError::InvalidApplication),
    }
}

fn advance_apply_args(
    operator: Value,
    operator_expr: Expr,
    mut prefix_operands: Vec<ArgOperand>,
    mut evaluated: Vec<Value>,
    remaining: Vec<ArgOperand>,
    env: EnvRef,
    pos: SourcePos,
    next: EvalContRef,
) -> Result<(MachineState, EvalContRef), EvalError> {
    let mut remaining_iter = remaining.into_iter();

    while let Some(operand) = remaining_iter.next() {
        match operand {
            ArgOperand::Value(value) => {
                prefix_operands.push(ArgOperand::Value(value.clone()));
                evaluated.push(value);
            }
            ArgOperand::Expr(expr) => {
                return Ok((
                    MachineState::Expr(expr.clone(), env.clone()),
                    push_cont(
                        EvalFrame::ApplyArgs {
                            operator,
                            operator_expr,
                            prefix_operands,
                            current_operand: expr,
                            evaluated,
                            remaining: remaining_iter.collect(),
                            env,
                            pos,
                        },
                        next,
                    ),
                ));
            }
        }
    }

    machine_apply(operator, evaluated, &env, next).map_err(|err| err.with_position(pos))
}

fn run_with_continuations(initial: MachineState, initial_cont: EvalContRef) -> Result<Value, EvalError> {
    let mut state = initial;
    let mut cont = initial_cont;

    loop {
        match state {
            MachineState::Expr(expr, env) => match expr {
                Expr::Number(value, _) => state = MachineState::Value(Value::Number(value)),
                Expr::Boolean(value, _) => state = MachineState::Value(Value::Boolean(value)),
                Expr::String(value, _) => {
                    state = MachineState::Value(Value::String(SchemeString::new(value)))
                }
                Expr::Char(value, _) => state = MachineState::Value(Value::Char(value)),
                Expr::Symbol(name, pos) => {
                    let value = env
                        .lookup(&name)
                        .ok_or_else(|| EvalError::UnboundSymbol(name).with_position(pos))?;
                    state = MachineState::Value(value);
                }
                Expr::List(items, pos) => {
                    let (next_state, next_cont) = machine_enter_list(items, pos, env, cont)?;
                    state = next_state;
                    cont = next_cont;
                }
            },
            MachineState::Value(value) => {
                let (frame, next) = match cont.as_ref() {
                    EvalCont::Done => return Ok(value),
                    EvalCont::Frame(frame, next) => (frame.clone(), next.clone()),
                };

                match frame {
                    EvalFrame::Sequence { remaining, env } => {
                        let (next_state, next_cont) = enter_sequence(&remaining, &env, next);
                        state = next_state;
                        cont = next_cont;
                    }
                    EvalFrame::DefineValue { name, env } => {
                        env.define(name, value);
                        state = MachineState::Value(Value::Void);
                        cont = next;
                    }
                    EvalFrame::SetValue { name, env, pos } => {
                        if env.set(&name, value) {
                            state = MachineState::Value(Value::Void);
                            cont = next;
                        } else {
                            return Err(EvalError::UnboundSymbol(name).with_position(pos));
                        }
                    }
                    EvalFrame::If {
                        consequent,
                        alternate,
                        env,
                    } => {
                        if value.is_truthy() {
                            state = MachineState::Expr(consequent, env);
                            cont = next;
                        } else if let Some(alternate) = alternate {
                            state = MachineState::Expr(alternate, env);
                            cont = next;
                        } else {
                            state = MachineState::Value(Value::Void);
                            cont = next;
                        }
                    }
                    EvalFrame::ApplyOperator {
                        operator_expr,
                        args,
                        env,
                        pos,
                    } => {
                        let (next_state, next_cont) = advance_apply_args(
                            value,
                            operator_expr,
                            Vec::new(),
                            Vec::new(),
                            args,
                            env,
                            pos,
                            next,
                        )?;
                        state = next_state;
                        cont = next_cont;
                    }
                    EvalFrame::ApplyArgs {
                        operator,
                        operator_expr,
                        mut prefix_operands,
                        current_operand,
                        mut evaluated,
                        remaining,
                        env,
                        pos,
                    } => {
                        prefix_operands.push(ArgOperand::Expr(current_operand));
                        evaluated.push(value);
                        let (next_state, next_cont) = advance_apply_args(
                            operator,
                            operator_expr,
                            prefix_operands,
                            evaluated,
                            remaining,
                            env,
                            pos,
                            next,
                        )?;
                        state = next_state;
                        cont = next_cont;
                    }
                    EvalFrame::ReplayApplication {
                        operator_expr,
                        mut prefix_operands,
                        suffix_operands,
                        env,
                        pos,
                    } => {
                        prefix_operands.push(ArgOperand::Value(value));
                        prefix_operands.extend(suffix_operands);
                        state = MachineState::Expr(operator_expr.clone(), env.clone());
                        cont = push_cont(
                            EvalFrame::ApplyOperator {
                                operator_expr,
                                args: prefix_operands,
                                env,
                                pos,
                            },
                            next,
                        );
                    }
                }
            }
        }
    }
}

fn eval_sequence_with_continuations(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (initial, cont) = enter_sequence(exprs, env, done_cont());
    run_with_continuations(initial, cont)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let env = root_env();
    let last = eval_sequence_with_continuations(&exprs, &env)
        .map_err(|err| err.with_position(SourcePos { line: 1, col: 1 }))?;
    let output = env.output.borrow().clone();
    Ok((last, output))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (last, _) = eval_program(input)?;
    Ok(last.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (last, output) = eval_program(input)?;
    Ok((last.render(), output))
}

#[cfg(test)]
mod tests;
