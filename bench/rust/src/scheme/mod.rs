use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub mod error;

pub use error::EvalError;

static NEXT_HYGIENE_ID: AtomicUsize = AtomicUsize::new(0);
static NEXT_RECORD_TYPE_ID: AtomicUsize = AtomicUsize::new(0);

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
    Pair(Box<Value>, Box<Value>),
    Builtin(Builtin),
    NativeProcedure(NativeProcedure),
    Procedure(Rc<Closure>),
    CaseProcedure(Rc<CaseClosure>),
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

struct StringCell {
    value: String,
    mutable: bool,
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
            Self::Pair(_, _) => "pair",
            Self::Builtin(_)
            | Self::NativeProcedure(_)
            | Self::Procedure(_)
            | Self::CaseProcedure(_) => "procedure",
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
        match self {
            Self::Number(value) => value.render(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => {
                let value = value.as_string();
                match mode {
                    RenderMode::Write => format!("{value:?}"),
                    RenderMode::Display => value,
                }
            }
            Self::Vector(value) => render_vector(value, mode),
            Self::Char(value) => render_char(*value, mode),
            Self::Symbol(name) => name.clone(),
            Self::List(items) => render_list(items, mode),
            Self::Pair(head, tail) => render_pair(head, tail, mode),
            Self::Builtin(_)
            | Self::NativeProcedure(_)
            | Self::Procedure(_)
            | Self::CaseProcedure(_) => "#<procedure>".into(),
            Self::Record(record) => format!("#<record {}>", record.record_type.name),
            Self::Uninitialized(name) => format!("#<uninitialized {name}>"),
            Self::Void => "#<void>".into(),
        }
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

    fn mutable_copy(&self) -> Self {
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

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render_with(mode));
    }

    rendered.push(')');
    rendered
}

fn render_vector(vector: &SchemeVector, mode: RenderMode) -> String {
    let items = vector.items();
    let mut rendered = String::from("#(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render_with(mode));
    }

    rendered.push(')');
    rendered
}

fn render_pair(head: &Value, tail: &Value, mode: RenderMode) -> String {
    let mut rendered = String::from("(");
    render_pair_contents(head, tail, mode, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_contents(head: &Value, tail: &Value, mode: RenderMode, output: &mut String) {
    output.push_str(&head.render_with(mode));

    match tail {
        Value::List(items) => {
            for item in items {
                output.push(' ');
                output.push_str(&item.render_with(mode));
            }
        }
        Value::Pair(next_head, next_tail) => {
            output.push(' ');
            render_pair_contents(next_head, next_tail, mode, output);
        }
        other => {
            output.push_str(" . ");
            output.push_str(&other.render_with(mode));
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

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
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
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    ListRef,
    ListTail,
    ListPred,
    Length,
    Append,
    Assoc,
    Map,
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
    StringAppend,
    StringLength,
    StringSet,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
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
    StringEqual,
    StringLess,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
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
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPred => "list?",
            Self::Length => "length",
            Self::Append => "append",
            Self::Assoc => "assoc",
            Self::Map => "map",
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
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::StringSet => "string-set!",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
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
            Self::StringEqual => "string=?",
            Self::StringLess => "string<?",
            Self::StringCiEqual => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::ExactToInexact => "exact->inexact",
            Self::InexactToExact => "inexact->exact",
            Self::Numerator => "numerator",
            Self::Denominator => "denominator",
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
    body: Vec<Expr>,
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
            ("cons", Builtin::Cons),
            ("car", Builtin::Car),
            ("cdr", Builtin::Cdr),
            ("null?", Builtin::NullPred),
            ("list", Builtin::List),
            ("list-ref", Builtin::ListRef),
            ("list-tail", Builtin::ListTail),
            ("list?", Builtin::ListPred),
            ("length", Builtin::Length),
            ("append", Builtin::Append),
            ("assoc", Builtin::Assoc),
            ("map", Builtin::Map),
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
            ("string-append", Builtin::StringAppend),
            ("string-length", Builtin::StringLength),
            ("string-set!", Builtin::StringSet),
            ("substring", Builtin::Substring),
            ("string->number", Builtin::StringToNumber),
            ("number->string", Builtin::NumberToString),
            ("symbol->string", Builtin::SymbolToString),
            ("string->symbol", Builtin::StringToSymbol),
            ("string-ref", Builtin::StringRef),
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
            ("string=?", Builtin::StringEqual),
            ("string<?", Builtin::StringLess),
            ("string-ci=?", Builtin::StringCiEqual),
            ("string-upcase", Builtin::StringUpcase),
            ("string-downcase", Builtin::StringDowncase),
            ("exact->inexact", Builtin::ExactToInexact),
            ("inexact->exact", Builtin::InexactToExact),
            ("numerator", Builtin::Numerator),
            ("denominator", Builtin::Denominator),
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
    let env = Env::new(output);
    eval_sequence(expressions, &env)
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Void;

    for expression in expressions {
        last_value = eval_expr(expression, env)?;
    }

    Ok(last_value)
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::String(value) => Ok(Value::String(SchemeString::immutable(value.clone()))),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::Symbol(name) => match env.lookup(name) {
            Some(Value::Uninitialized(_)) => {
                Err(EvalError::UninitializedBinding { name: name.clone() }
                    .with_offset(expr.pos.offset))
            }
            Some(value) => Ok(value),
            None => {
                Err(EvalError::UnboundVariable { name: name.clone() }.with_offset(expr.pos.offset))
            }
        },
        ExprKind::List(items) => eval_application(expr.pos, items, env),
    }
}

fn eval_application(list_pos: SourcePos, items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (operator, arguments) = items
        .split_first()
        .ok_or_else(|| EvalError::EmptyList.with_offset(list_pos.offset))?;

    if let ExprKind::Symbol(name) = &operator.kind {
        match name.as_str() {
            "define" => return eval_define(operator.pos, arguments, env),
            "define-record-type" => return eval_define_record_type(operator.pos, arguments, env),
            "define-syntax" => return eval_define_syntax(operator.pos, arguments, env),
            "set!" => return eval_set(operator.pos, arguments, env),
            "if" => return eval_if(operator.pos, arguments, env),
            "quote" => return eval_quote(operator.pos, arguments),
            "lambda" => return eval_lambda(operator.pos, arguments, env),
            "case-lambda" => return eval_case_lambda(arguments, env),
            "and" => return eval_and(arguments, env),
            "or" => return eval_or(arguments, env),
            "begin" => return eval_begin(arguments, env),
            "let" => return eval_let(operator.pos, arguments, env),
            "letrec" => return eval_letrec(operator.pos, arguments, env, false),
            "letrec*" => return eval_letrec(operator.pos, arguments, env, true),
            "cond" => return eval_cond(arguments, env),
            "case" => return eval_case(operator.pos, arguments, env),
            "do" => return eval_do(operator.pos, arguments, env),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let invocation = Expr::new(ExprKind::List(items.to_vec()), list_pos);
            let expansion = expand_macro_invocation(transformer.as_ref(), &invocation)?;
            let macro_env = Env::child(env);

            for (alias, value) in expansion.aliases {
                macro_env.define(alias, value);
            }

            return eval_expr(&expansion.expr, &macro_env);
        }
    }

    let procedure = eval_expr(operator, env)?;
    apply_value(procedure, arguments, env, operator.pos)
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
        Builtin::Cons => eval_cons(arguments, env, call_pos),
        Builtin::Car => eval_car(arguments, env, call_pos),
        Builtin::Cdr => eval_cdr(arguments, env, call_pos),
        Builtin::NullPred => eval_null_pred(arguments, env, call_pos),
        Builtin::List => eval_list_builtin(arguments, env),
        Builtin::ListRef => eval_list_ref(arguments, env, call_pos),
        Builtin::ListTail => eval_list_tail(arguments, env, call_pos),
        Builtin::ListPred => eval_type_predicate(arguments, env, "list?", call_pos, |value| {
            matches!(value, Value::List(_))
        }),
        Builtin::Length => eval_length(arguments, env, call_pos),
        Builtin::Append => eval_append(arguments, env),
        Builtin::Assoc => eval_assoc(arguments, env, call_pos),
        Builtin::Map => eval_map(arguments, env, call_pos),
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
        Builtin::StringAppend => eval_string_append(arguments, env),
        Builtin::StringLength => eval_string_length(arguments, env, call_pos),
        Builtin::StringSet => eval_string_set(arguments, env, call_pos),
        Builtin::Substring => eval_substring(arguments, env, call_pos),
        Builtin::StringToNumber => eval_string_to_number(arguments, env, call_pos),
        Builtin::NumberToString => eval_number_to_string(arguments, env, call_pos),
        Builtin::SymbolToString => eval_symbol_to_string(arguments, env, call_pos),
        Builtin::StringToSymbol => eval_string_to_symbol(arguments, env, call_pos),
        Builtin::StringRef => eval_string_ref(arguments, env, call_pos),
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
        call_env.define(rest_param.clone(), Value::List(argument_values.collect()));
    }

    eval_sequence(&closure.body, &call_env)
}

fn apply_case_closure_values(
    closure: Rc<CaseClosure>,
    argument_values: Vec<Value>,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let argument_count = argument_values.len();
    let Some(clause) = closure
        .clauses
        .iter()
        .find(|clause| parameter_spec_accepts(&clause.params, argument_count))
    else {
        return Err(EvalError::WrongArgCount {
            name: "procedure".into(),
            expected: format_case_lambda_arity(&closure),
            got: argument_count,
        }
        .with_offset(call_pos.offset));
    };

    apply_closure_values(clause.clone(), argument_values, call_pos)
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
                body: arguments[1..].to_vec(),
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
        ExprKind::List(items) => Value::List(items.iter().map(quote_expr).collect()),
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
        body: body.to_vec(),
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
            body: body.to_vec(),
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
        body: body.to_vec(),
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

    match eval_expr(list_expr, env)? {
        Value::List(mut rest_values) => values.append(&mut rest_values),
        other => {
            return Err(EvalError::TypeMismatch {
                expected: "list".into(),
                found: other.kind().into(),
            }
            .with_offset(list_expr.pos.offset))
        }
    }

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
        | Value::CaseProcedure(_) => {
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

    match tail {
        Value::List(mut items) => {
            items.insert(0, head);
            Ok(Value::List(items))
        }
        other => Ok(Value::Pair(Box::new(head), Box::new(other))),
    }
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
        Value::Pair(head, _) => Ok(*head),
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
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(_, tail) => Ok(*tail),
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
    Ok(Value::Boolean(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_list_builtin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    Ok(Value::List(eval_args(arguments, env)?))
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

    let items = eval_list_items(list_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let len = items.len();
    if index > len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(Value::List(items[index..].to_vec()))
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

    match eval_expr(list_expr, env)? {
        Value::List(items) => Ok(Value::Number(Number::integer(items.len() as i64))),
        other => Err(EvalError::TypeMismatch {
            expected: "list".into(),
            found: other.kind().into(),
        }
        .with_offset(list_expr.pos.offset)),
    }
}

fn eval_append(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        match value {
            Value::List(mut list_items) => items.append(&mut list_items),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    found: other.kind().into(),
                }
                .with_offset(argument.pos.offset));
            }
        }
    }

    Ok(Value::List(items))
}

fn eval_assoc(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [key_expr, alist_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "assoc".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let key = eval_expr(key_expr, env)?;
    let entries = eval_list_items(alist_expr, env)?;

    for entry in entries {
        if let Some(entry_key) = pair_head(&entry) {
            if value_equal(&key, entry_key) {
                return Ok(entry);
            }
        }
    }

    Ok(Value::Boolean(false))
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

    Ok(Value::List(results))
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

    Ok(Value::String(eval_string_value(expr, env)?.mutable_copy()))
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

    Ok(Value::List(eval_vector_value(vector_expr, env)?.items()))
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
    match eval_expr(expr, env)? {
        Value::List(items) => Ok(items),
        other => Err(EvalError::TypeMismatch {
            expected: "list".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn is_pair(value: &Value) -> bool {
    matches!(value, Value::List(items) if !items.is_empty()) || matches!(value, Value::Pair(_, _))
}

fn pair_head(value: &Value) -> Option<&Value> {
    match value {
        Value::List(items) if !items.is_empty() => Some(&items[0]),
        Value::Pair(head, _) => Some(head.as_ref()),
        _ => None,
    }
}

fn value_eqv(left: &Value, right: &Value) -> bool {
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
                    .all(|(left_item, right_item)| value_eqv(left_item, right_item))
        }
        (Value::Pair(left_head, left_tail), Value::Pair(right_head, right_tail)) => {
            value_eqv(left_head, right_head) && value_eqv(left_tail, right_tail)
        }
        (Value::Builtin(left), Value::Builtin(right)) => left.name() == right.name(),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => {
            native_procedure_equal(left, right)
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::CaseProcedure(left), Value::CaseProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Uninitialized(left), Value::Uninitialized(right)) => left == right,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn value_equal(left: &Value, right: &Value) -> bool {
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
                    .all(|(left_item, right_item)| value_equal(left_item, right_item))
        }
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left_item, right_item)| value_equal(left_item, right_item))
        }
        (Value::Pair(left_head, left_tail), Value::Pair(right_head, right_tail)) => {
            value_equal(left_head, right_head) && value_equal(left_tail, right_tail)
        }
        (Value::Builtin(left), Value::Builtin(right)) => left.name() == right.name(),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => {
            native_procedure_equal(left, right)
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::CaseProcedure(left), Value::CaseProcedure(right)) => Rc::ptr_eq(left, right),
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
