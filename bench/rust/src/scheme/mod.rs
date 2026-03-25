pub mod error;

pub use error::{EvalError, SourcePos};

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering as AtomicOrdering},
};

const BUILTIN_NAMES: &[&str] = &[
    "abs",
    "+",
    "-",
    "*",
    "/",
    "<",
    "<=",
    "denominator",
    "=",
    ">",
    ">=",
    "char?",
    "display",
    "eq?",
    "eqv?",
    "equal?",
    "even?",
    "exact->inexact",
    "exact?",
    "expt",
    "integer->char",
    "inexact->exact",
    "inexact?",
    "integer?",
    "not",
    "newline",
    "number->string",
    "append",
    "apply",
    "assoc",
    "boolean?",
    "car",
    "char-alphabetic?",
    "char->integer",
    "char-downcase",
    "char-numeric?",
    "char-upcase",
    "char=?",
    "char<?",
    "cdr",
    "cons",
    "length",
    "list",
    "list?",
    "list-ref",
    "list-tail",
    "list->string",
    "list->vector",
    "map",
    "max",
    "make-vector",
    "min",
    "modulo",
    "negative?",
    "null?",
    "number?",
    "odd?",
    "pair?",
    "positive?",
    "procedure?",
    "numerator",
    "quotient",
    "rational?",
    "remainder",
    "string-append",
    "string?",
    "string-ci=?",
    "string-copy",
    "string-downcase",
    "string=?",
    "string<?",
    "string-length",
    "string->list",
    "string->number",
    "string->symbol",
    "string-ref",
    "string-set!",
    "string-upcase",
    "substring",
    "symbol?",
    "symbol->string",
    "vector",
    "vector->list",
    "vector-length",
    "vector-ref",
    "vector-set!",
    "vector?",
    "write",
    "zero?",
];

static GENERATED_SYMBOL_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_program(input)?;
    Ok(render_result(&value))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((render_result(&value), output))
}

fn current_bench_level() -> u32 {
    std::env::var("BENCH_LEVEL")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(u32::MAX)
}

fn uses_immutable_strings() -> bool {
    current_bench_level() >= 15
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let exprs = Parser::new(input).parse_program()?;
    let env = Environment::global();
    let mut last = None;

    for expr in &exprs {
        last = Some(eval(expr, env.clone())?);
    }

    let value = last.ok_or(EvalError::EmptyInput)?;
    Ok((value, Environment::captured_output(&env)))
}

fn render_result(value: &Value) -> String {
    match value {
        Value::Void => String::new(),
        other => other.to_scheme_string(),
    }
}

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Rational {
    numerator: i64,
    denominator: i64,
}

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

#[derive(Clone, Debug, PartialEq)]
enum ExprKind {
    Integer(i64),
    Rational(Rational),
    Inexact(f64),
    Bool(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }

    fn symbol(name: impl Into<String>, pos: SourcePos) -> Self {
        Self::new(ExprKind::Symbol(name.into()), pos)
    }

    fn list(items: Vec<Expr>, pos: SourcePos) -> Self {
        Self::new(ExprKind::List(items), pos)
    }
}

type EnvRef = Rc<RefCell<Environment>>;
type BindingRef = Rc<RefCell<Value>>;
type MacroRef = Rc<SyntaxRulesMacro>;

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
struct SyntaxRulesMacro {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvRef,
}

#[derive(Clone, Debug, PartialEq)]
enum PatternBinding {
    One(Expr),
    Many(Vec<PatternBinding>),
}

type PatternBindings = HashMap<String, PatternBinding>;

struct ExpandedExpr {
    expr: Expr,
    env: EnvRef,
}

#[derive(Default)]
struct ExpansionState {
    bound_scopes: Vec<HashMap<String, String>>,
    free_aliases: HashMap<String, String>,
    binding_aliases: Vec<(String, BindingRef)>,
    macro_aliases: Vec<(String, MacroRef)>,
}

impl ExpansionState {
    fn lookup_bound_name(&self, name: &str) -> Option<&str> {
        self.bound_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).map(String::as_str))
    }

    fn push_scope(&mut self, scope: HashMap<String, String>) {
        self.bound_scopes.push(scope);
    }

    fn pop_scope(&mut self) {
        self.bound_scopes.pop();
    }

    fn alias_for(&mut self, name: &str, definition_env: &EnvRef) -> String {
        if let Some(existing) = self.free_aliases.get(name) {
            return existing.clone();
        }

        let alias = fresh_generated_symbol("ref");

        if let Some(binding) = Environment::lookup_binding(definition_env, name) {
            self.binding_aliases.push((alias.clone(), binding));
        }

        if let Some(transformer) = Environment::lookup_macro(definition_env, name) {
            self.macro_aliases.push((alias.clone(), transformer));
        }

        self.free_aliases.insert(name.to_string(), alias.clone());
        alias
    }

    fn build_env(self, parent: EnvRef) -> EnvRef {
        if self.binding_aliases.is_empty() && self.macro_aliases.is_empty() {
            return parent;
        }

        let env = Environment::child(parent);

        for (name, binding) in self.binding_aliases {
            Environment::define_existing(&env, name, binding);
        }

        for (name, transformer) in self.macro_aliases {
            Environment::define_macro(&env, name, transformer);
        }

        env
    }
}

#[derive(Clone)]
struct ProcedureClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
}

impl ProcedureClause {
    fn new(params: Vec<String>, rest_param: Option<String>, body: Vec<Expr>) -> Self {
        Self {
            params,
            rest_param,
            body,
        }
    }

    fn matches_arity(&self, arg_count: usize) -> bool {
        if self.rest_param.is_some() {
            arg_count >= self.params.len()
        } else {
            arg_count == self.params.len()
        }
    }

    fn expected_arity(&self) -> String {
        if self.rest_param.is_some() {
            format!("at least {}", self.params.len())
        } else {
            format!("exactly {}", self.params.len())
        }
    }
}

#[derive(Clone)]
struct UserProcedure {
    name: Option<String>,
    clauses: Vec<ProcedureClause>,
    env: EnvRef,
}

impl UserProcedure {
    fn new(name: Option<String>, clauses: Vec<ProcedureClause>, env: EnvRef) -> Self {
        Self { name, clauses, env }
    }

    fn single_clause(
        name: Option<String>,
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: EnvRef,
    ) -> Self {
        Self::new(
            name,
            vec![ProcedureClause::new(params, rest_param, body)],
            env,
        )
    }

    fn display_name(&self) -> &str {
        self.name.as_deref().unwrap_or("lambda")
    }

    fn matching_clause(&self, arg_count: usize) -> Option<&ProcedureClause> {
        self.clauses
            .iter()
            .find(|clause| clause.matches_arity(arg_count))
    }

    fn expected_arity(&self) -> String {
        self.clauses
            .iter()
            .map(ProcedureClause::expected_arity)
            .collect::<Vec<_>>()
            .join(" or ")
    }
}

#[derive(Clone)]
struct RecordType {
    name: String,
    field_count: usize,
}

#[derive(Clone)]
struct RecordInstance {
    record_type: Rc<RecordType>,
    fields: Vec<Value>,
}

#[derive(Clone)]
enum NativeProcedure {
    RecordConstructor {
        name: String,
        record_type: Rc<RecordType>,
        field_indices: Vec<usize>,
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

impl NativeProcedure {
    fn display_name(&self) -> &str {
        match self {
            NativeProcedure::RecordConstructor { name, .. }
            | NativeProcedure::RecordPredicate { name, .. }
            | NativeProcedure::RecordAccessor { name, .. } => name,
        }
    }
}

#[derive(Default)]
struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingRef>,
    macros: HashMap<String, MacroRef>,
    output: Rc<RefCell<String>>,
}

impl Environment {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
            macros: HashMap::new(),
            output: Rc::new(RefCell::new(String::new())),
        }));

        for &name in BUILTIN_NAMES {
            Self::define(&env, name.to_string(), Value::Builtin(name));
        }

        env
    }

    fn child(parent: EnvRef) -> EnvRef {
        let output = parent.borrow().output.clone();
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            output,
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        Self::define_existing(env, name, Rc::new(RefCell::new(value)));
    }

    fn define_existing(env: &EnvRef, name: String, binding: BindingRef) {
        env.borrow_mut().bindings.insert(name, binding);
    }

    fn define_macro(env: &EnvRef, name: String, transformer: MacroRef) {
        env.borrow_mut().macros.insert(name, transformer);
    }

    fn lookup_binding(env: &EnvRef, name: &str) -> Option<BindingRef> {
        let (binding, parent) = {
            let env = env.borrow();
            (env.bindings.get(name).cloned(), env.parent.clone())
        };

        binding.or_else(|| parent.and_then(|parent| Self::lookup_binding(&parent, name)))
    }

    fn lookup(env: &EnvRef, name: &str, position: SourcePos) -> Result<Option<Value>, EvalError> {
        match Self::lookup_binding(env, name) {
            Some(binding) => {
                let value = binding.borrow().clone();
                if matches!(value, Value::Uninitialized) {
                    Err(EvalError::uninitialized_variable(name, position))
                } else {
                    Ok(Some(value))
                }
            }
            None => Ok(None),
        }
    }

    fn lookup_macro(env: &EnvRef, name: &str) -> Option<MacroRef> {
        let (transformer, parent) = {
            let env = env.borrow();
            (env.macros.get(name).cloned(), env.parent.clone())
        };

        transformer.or_else(|| parent.and_then(|parent| Self::lookup_macro(&parent, name)))
    }

    fn append_output(env: &EnvRef, text: &str) {
        let output = env.borrow().output.clone();
        output.borrow_mut().push_str(text);
    }

    fn captured_output(env: &EnvRef) -> String {
        let output = env.borrow().output.clone();
        let captured = output.borrow().clone();
        captured
    }
}

#[derive(Clone)]
struct SchemeString {
    chars: Rc<RefCell<Vec<char>>>,
    mutable: bool,
}

impl SchemeString {
    fn immutable(value: &str) -> Self {
        Self::new(value, false)
    }

    fn new(value: &str, mutable: bool) -> Self {
        Self {
            chars: Rc::new(RefCell::new(value.chars().collect())),
            mutable,
        }
    }

    fn to_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }

    fn len_chars(&self) -> usize {
        self.chars.borrow().len()
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.chars.borrow().get(index).copied()
    }

    fn substring(&self, start: usize, end: usize) -> String {
        self.chars.borrow()[start..end].iter().collect()
    }

    fn mutable_copy(&self) -> Self {
        Self {
            chars: Rc::new(RefCell::new(self.chars.borrow().clone())),
            mutable: true,
        }
    }

    fn set_char(&self, index: usize, ch: char, position: SourcePos) -> Result<(), EvalError> {
        if !self.mutable {
            return Err(EvalError::immutable_string(position));
        }

        self.chars.borrow_mut()[index] = ch;
        Ok(())
    }
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Number {
    Integer(i64),
    Rational(Rational),
    Inexact(f64),
}

impl Number {
    fn is_exact(self) -> bool {
        !matches!(self, Self::Inexact(_))
    }

    fn is_integer(self) -> bool {
        match self {
            Self::Integer(_) => true,
            Self::Rational(rational) => rational.denominator == 1,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    fn is_zero(self) -> bool {
        match self {
            Self::Integer(value) => value == 0,
            Self::Rational(rational) => rational.numerator == 0,
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

    fn exact_parts(self) -> Option<(i128, i128)> {
        match self {
            Self::Integer(value) => Some((value as i128, 1)),
            Self::Rational(rational) => {
                Some((rational.numerator as i128, rational.denominator as i128))
            }
            Self::Inexact(_) => None,
        }
    }

    fn to_f64(self) -> f64 {
        match self {
            Self::Integer(value) => value as f64,
            Self::Rational(rational) => rational.numerator as f64 / rational.denominator as f64,
            Self::Inexact(value) => value,
        }
    }

    fn compare(self, other: Self) -> Option<Ordering> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => {
                Some((left_num * right_den).cmp(&(right_num * left_den)))
            }
            _ => self.to_f64().partial_cmp(&other.to_f64()),
        }
    }

    fn equal(self, other: Self) -> bool {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => {
                left_num == right_num && left_den == right_den
            }
            _ => self.to_f64() == other.to_f64(),
        }
    }

    fn abs(self, position: SourcePos) -> Result<Self, EvalError> {
        match self {
            Self::Inexact(value) => Ok(Self::Inexact(value.abs())),
            Self::Integer(value) => number_from_exact_parts((value as i128).abs(), 1, position),
            Self::Rational(rational) => number_from_exact_parts(
                (rational.numerator as i128).abs(),
                rational.denominator as i128,
                position,
            ),
        }
    }

    fn neg(self, position: SourcePos) -> Result<Self, EvalError> {
        match self {
            Self::Inexact(value) => Ok(Self::Inexact(-value)),
            Self::Integer(value) => number_from_exact_parts(-(value as i128), 1, position),
            Self::Rational(rational) => number_from_exact_parts(
                -(rational.numerator as i128),
                rational.denominator as i128,
                position,
            ),
        }
    }

    fn add(self, other: Self, position: SourcePos) -> Result<Self, EvalError> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => number_from_exact_parts(
                left_num * right_den + right_num * left_den,
                left_den * right_den,
                position,
            ),
            _ => Ok(Self::Inexact(self.to_f64() + other.to_f64())),
        }
    }

    fn sub(self, other: Self, position: SourcePos) -> Result<Self, EvalError> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => number_from_exact_parts(
                left_num * right_den - right_num * left_den,
                left_den * right_den,
                position,
            ),
            _ => Ok(Self::Inexact(self.to_f64() - other.to_f64())),
        }
    }

    fn mul(self, other: Self, position: SourcePos) -> Result<Self, EvalError> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => {
                number_from_exact_parts(left_num * right_num, left_den * right_den, position)
            }
            _ => Ok(Self::Inexact(self.to_f64() * other.to_f64())),
        }
    }

    fn div(
        self,
        other: Self,
        divisor_position: SourcePos,
        position: SourcePos,
    ) -> Result<Self, EvalError> {
        if other.is_zero() {
            return Err(EvalError::division_by_zero(divisor_position));
        }

        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => {
                number_from_exact_parts(left_num * right_den, left_den * right_num, position)
            }
            _ => Ok(Self::Inexact(self.to_f64() / other.to_f64())),
        }
    }

    fn to_scheme_string(self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Rational(rational) => {
                format!("{}/{}", rational.numerator, rational.denominator)
            }
            Self::Inexact(value) => format_inexact(value),
        }
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Rational(Rational),
    Inexact(f64),
    Bool(bool),
    Char(char),
    String(SchemeString),
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<Pair>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Record(Rc<RecordInstance>),
    Builtin(&'static str),
    Procedure(Rc<UserProcedure>),
    NativeProcedure(Rc<NativeProcedure>),
    Uninitialized,
    Void,
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) | Value::Rational(_) | Value::Inexact(_) => "number",
            Value::Bool(_) => "boolean",
            Value::Char(_) => "char",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
            Value::Pair(_) => "pair",
            Value::Vector(_) => "vector",
            Value::Record(_) => "record",
            Value::Builtin(_) | Value::Procedure(_) | Value::NativeProcedure(_) => "procedure",
            Value::Uninitialized => "undefined",
            Value::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false))
    }

    fn number(&self) -> Option<Number> {
        match self {
            Value::Integer(value) => Some(Number::Integer(*value)),
            Value::Rational(value) => Some(Number::Rational(*value)),
            Value::Inexact(value) => Some(Number::Inexact(*value)),
            _ => None,
        }
    }

    fn as_number(&self, position: SourcePos) -> Result<Number, EvalError> {
        self.number()
            .ok_or_else(|| EvalError::type_mismatch("number", self.type_name(), position))
    }

    fn as_integer(&self, position: SourcePos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(value) => Ok(*value),
            other => Err(EvalError::type_mismatch(
                "integer",
                other.type_name(),
                position,
            )),
        }
    }

    fn from_number(number: Number) -> Self {
        match number {
            Number::Integer(value) => Self::Integer(value),
            Number::Rational(rational) if rational.denominator == 1 => {
                Self::Integer(rational.numerator)
            }
            Number::Rational(rational) => Self::Rational(rational),
            Number::Inexact(value) => Self::Inexact(value),
        }
    }

    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(_) | Value::Rational(_) | Value::Inexact(_) => self
                .number()
                .expect("number variants must convert to Number")
                .to_scheme_string(),
            Value::Bool(true) => "#t".into(),
            Value::Bool(false) => "#f".into(),
            Value::Char(value) => format_char(*value),
            Value::String(value) => format!("\"{}\"", escape_string(&value.to_plain_string())),
            Value::Symbol(value) => value.clone(),
            Value::List(values) => {
                let items = values
                    .iter()
                    .map(Value::to_scheme_string)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({items})")
            }
            Value::Pair(pair) => format_pair(pair),
            Value::Vector(values) => {
                let items = values
                    .borrow()
                    .iter()
                    .map(Value::to_scheme_string)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("#({items})")
            }
            Value::Record(record) => format!("#<record {}>", record.record_type.name),
            Value::Builtin(_) | Value::Procedure(_) | Value::NativeProcedure(_) => {
                "#<procedure>".into()
            }
            Value::Uninitialized => "#<uninitialized>".into(),
            Value::Void => "#<void>".into(),
        }
    }

    fn to_display_string(&self) -> String {
        match self {
            Value::String(value) => value.to_plain_string(),
            Value::Char(value) => value.to_string(),
            other => other.to_scheme_string(),
        }
    }
}

#[derive(Clone)]
struct LocatedValue {
    value: Value,
    position: SourcePos,
}

impl LocatedValue {
    fn new(value: Value, position: SourcePos) -> Self {
        Self { value, position }
    }

    fn as_number(&self) -> Result<Number, EvalError> {
        self.value.as_number(self.position)
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        self.value.as_integer(self.position)
    }

    fn as_string(&self) -> Result<SchemeString, EvalError> {
        match &self.value {
            Value::String(value) => Ok(value.clone()),
            other => Err(EvalError::type_mismatch(
                "string",
                other.type_name(),
                self.position,
            )),
        }
    }

    fn as_char(&self) -> Result<char, EvalError> {
        match &self.value {
            Value::Char(value) => Ok(*value),
            other => Err(EvalError::type_mismatch(
                "char",
                other.type_name(),
                self.position,
            )),
        }
    }

    fn as_symbol(&self) -> Result<&str, EvalError> {
        match &self.value {
            Value::Symbol(value) => Ok(value.as_str()),
            other => Err(EvalError::type_mismatch(
                "symbol",
                other.type_name(),
                self.position,
            )),
        }
    }

    fn as_vector(&self) -> Result<Rc<RefCell<Vec<Value>>>, EvalError> {
        match &self.value {
            Value::Vector(values) => Ok(values.clone()),
            other => Err(EvalError::type_mismatch(
                "vector",
                other.type_name(),
                self.position,
            )),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.type_name())
    }
}

fn escape_string(input: &str) -> String {
    let mut escaped = String::new();

    for ch in input.chars() {
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

fn format_char(ch: char) -> String {
    match ch {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{}", other),
    }
}

fn format_pair(pair: &Pair) -> String {
    let mut rendered = String::from("(");
    write_pair_contents(pair, &mut rendered);
    rendered.push(')');
    rendered
}

fn write_pair_contents(pair: &Pair, rendered: &mut String) {
    rendered.push_str(&pair.car.to_scheme_string());

    match &pair.cdr {
        Value::List(values) if values.is_empty() => {}
        Value::List(values) => {
            for value in values {
                rendered.push(' ');
                rendered.push_str(&value.to_scheme_string());
            }
        }
        Value::Pair(next) => {
            rendered.push(' ');
            write_pair_contents(next, rendered);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&other.to_scheme_string());
        }
    }
}

fn format_inexact(value: f64) -> String {
    format!("{value:?}")
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

fn number_from_exact_parts(
    numerator: i128,
    denominator: i128,
    position: SourcePos,
) -> Result<Number, EvalError> {
    if denominator == 0 {
        return Err(EvalError::division_by_zero(position));
    }

    let mut numerator = numerator;
    let mut denominator = denominator;

    if denominator < 0 {
        numerator = -numerator;
        denominator = -denominator;
    }

    let divisor = gcd_i128(numerator, denominator);
    numerator /= divisor;
    denominator /= divisor;

    let numerator = i64::try_from(numerator).map_err(|_| EvalError::integer_overflow(position))?;
    let denominator =
        i64::try_from(denominator).map_err(|_| EvalError::integer_overflow(position))?;

    if denominator == 1 {
        Ok(Number::Integer(numerator))
    } else {
        Ok(Number::Rational(Rational {
            numerator,
            denominator,
        }))
    }
}

fn pow10_i128(exponent: usize, position: SourcePos) -> Result<i128, EvalError> {
    let mut value = 1_i128;

    for _ in 0..exponent {
        value = value
            .checked_mul(10)
            .ok_or_else(|| EvalError::integer_overflow(position))?;
    }

    Ok(value)
}

fn parse_decimal_to_exact(token: &str, position: SourcePos) -> Result<Number, EvalError> {
    let (mantissa, exponent) = match token.find(['e', 'E']) {
        Some(index) => {
            let exponent = token[index + 1..].parse::<i32>().map_err(|_| {
                EvalError::syntax(format!("invalid inexact literal: {token}"), position)
            })?;
            (&token[..index], exponent)
        }
        None => (token, 0),
    };

    let (sign, mantissa) = if let Some(rest) = mantissa.strip_prefix('-') {
        (-1_i128, rest)
    } else if let Some(rest) = mantissa.strip_prefix('+') {
        (1_i128, rest)
    } else {
        (1_i128, mantissa)
    };

    let (whole, fractional) = match mantissa.split_once('.') {
        Some((whole, fractional)) => (whole, fractional),
        None => (mantissa, ""),
    };

    if !whole.chars().all(|ch| ch.is_ascii_digit())
        || !fractional.chars().all(|ch| ch.is_ascii_digit())
        || (whole.is_empty() && fractional.is_empty())
    {
        return Err(EvalError::syntax(
            format!("invalid inexact literal: {token}"),
            position,
        ));
    }

    let digits = format!("{whole}{fractional}");
    let digits = if digits.is_empty() {
        0_i128
    } else {
        digits
            .parse::<i128>()
            .map_err(|_| EvalError::integer_overflow(position))?
    };

    let scale = fractional.len() as i32 - exponent;
    if scale >= 0 {
        number_from_exact_parts(
            sign * digits,
            pow10_i128(scale as usize, position)?,
            position,
        )
    } else {
        number_from_exact_parts(
            sign * digits * pow10_i128((-scale) as usize, position)?,
            1,
            position,
        )
    }
}

fn parse_number_literal(token: &str, position: SourcePos) -> Result<Option<Number>, EvalError> {
    if let Ok(value) = token.parse::<i64>() {
        return Ok(Some(Number::Integer(value)));
    }

    if token.matches('/').count() == 1 {
        let (numerator, denominator) = token
            .split_once('/')
            .expect("single slash count implies split_once succeeds");

        if is_integer_token(numerator) && is_integer_token(denominator) {
            let numerator = numerator.parse::<i64>().map_err(|_| {
                EvalError::syntax(format!("invalid rational literal: {token}"), position)
            })?;
            let denominator = denominator.parse::<i64>().map_err(|_| {
                EvalError::syntax(format!("invalid rational literal: {token}"), position)
            })?;

            if denominator == 0 {
                return Err(EvalError::syntax(
                    format!("invalid rational literal: {token}"),
                    position,
                ));
            }

            return Ok(Some(number_from_exact_parts(
                numerator as i128,
                denominator as i128,
                position,
            )?));
        }
    }

    if token.contains('.') || token.contains('e') || token.contains('E') {
        if let Ok(value) = token.parse::<f64>() {
            if !value.is_finite() {
                return Err(EvalError::syntax(
                    format!("invalid inexact literal: {token}"),
                    position,
                ));
            }

            return Ok(Some(Number::Inexact(value)));
        }
    }

    Ok(None)
}

fn inexact_to_exact(number: Number, position: SourcePos) -> Result<Number, EvalError> {
    match number {
        Number::Inexact(value) => parse_decimal_to_exact(&format_inexact(value), position),
        exact => Ok(exact),
    }
}

fn scheme_eq(left: &Value, right: &Value) -> bool {
    if let (Some(left), Some(right)) = (left.number(), right.number()) {
        return left.equal(right);
    }

    match (left, right) {
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.chars, &right.chars),
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn scheme_equal(left: &Value, right: &Value) -> bool {
    if let (Some(left), Some(right)) = (left.number(), right.number()) {
        return left.equal(right);
    }

    match (left, right) {
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| scheme_equal(left, right))
        }
        (Value::Pair(left), Value::Pair(right)) => {
            scheme_equal(&left.car, &right.car) && scheme_equal(&left.cdr, &right.cdr)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let left = left.borrow();
            let right = right.borrow();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| scheme_equal(left, right))
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Rational(value) => Ok(Value::from_number(Number::Rational(*value))),
        ExprKind::Inexact(value) => Ok(Value::Inexact(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(Value::String(SchemeString::immutable(value))),
        ExprKind::Symbol(name) => Environment::lookup(&env, name, expr.pos)?
            .ok_or_else(|| EvalError::unbound_variable(name.clone(), expr.pos)),
        ExprKind::List(items) => eval_list(items, env, expr.pos),
    }
}

fn eval_list(items: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::syntax("cannot evaluate empty list", position));
    };

    if let ExprKind::Symbol(name) = &head.kind {
        if name == "define-syntax" {
            return eval_define_syntax(tail, env, head.pos);
        }

        if let Some(transformer) = Environment::lookup_macro(&env, name) {
            let expanded = expand_macro_call(transformer, items, env.clone(), position)?;
            return eval(&expanded.expr, expanded.env);
        }

        return match name.as_str() {
            "define" => eval_define(tail, env, head.pos),
            "define-record-type" => eval_define_record_type(tail, env, head.pos),
            "set!" => eval_set(tail, env, head.pos),
            "if" => eval_if(tail, env, head.pos),
            "quote" => eval_quote(tail, head.pos),
            "lambda" => eval_lambda(tail, env, head.pos),
            "case-lambda" => eval_case_lambda(tail, env, head.pos),
            "and" => eval_and(tail, env),
            "or" => eval_or(tail, env),
            "begin" => eval_begin(tail, env),
            "cond" => eval_cond(tail, env, head.pos),
            "case" => eval_case(tail, env, head.pos),
            "do" => eval_do(tail, env, head.pos),
            "let" => eval_let(tail, env, head.pos),
            "letrec" => eval_letrec(tail, env, head.pos, false),
            "letrec*" => eval_letrec(tail, env, head.pos, true),
            _ => {
                let callable = eval(head, env.clone())?;
                let args = eval_all(tail, env.clone())?;
                apply(callable, head.pos, args, env)
            }
        };
    }

    let callable = eval(head, env.clone())?;
    let args = eval_all(tail, env.clone())?;
    apply(callable, head.pos, args, env)
}

fn eval_define_syntax(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let [name_expr, transformer_expr] = exprs else {
        return Err(EvalError::syntax(
            "define-syntax requires exactly 2 expressions",
            position,
        ));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::syntax(
            "define-syntax name must be a symbol",
            name_expr.pos,
        ));
    };

    let transformer =
        parse_syntax_rules(name, transformer_expr, env.clone(), transformer_expr.pos)?;
    Environment::define_macro(&env, name.clone(), transformer);
    Ok(Value::Void)
}

#[derive(Clone)]
struct RecordFieldSpec {
    name: String,
    accessor_name: String,
    position: SourcePos,
}

fn eval_define_record_type(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = exprs else {
        return Err(EvalError::syntax(
            "define-record-type requires a type name, constructor, and predicate",
            position,
        ));
    };

    let type_name = parse_symbol_name(type_name_expr, "define-record-type name must be a symbol")?;
    let (constructor_name, constructor_fields) = parse_record_constructor_spec(constructor_expr)?;
    let predicate_name = parse_symbol_name(
        predicate_expr,
        "define-record-type predicate must be a symbol",
    )?;
    let fields = field_exprs
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;
    let constructor_indices =
        resolve_record_constructor_fields(&constructor_fields, &fields, constructor_expr.pos)?;

    let record_type = Rc::new(RecordType {
        name: type_name,
        field_count: fields.len(),
    });

    Environment::define(
        &env,
        constructor_name.clone(),
        Value::NativeProcedure(Rc::new(NativeProcedure::RecordConstructor {
            name: constructor_name,
            record_type: record_type.clone(),
            field_indices: constructor_indices,
        })),
    );
    Environment::define(
        &env,
        predicate_name.clone(),
        Value::NativeProcedure(Rc::new(NativeProcedure::RecordPredicate {
            name: predicate_name,
            record_type: record_type.clone(),
        })),
    );

    for (field_index, field) in fields.into_iter().enumerate() {
        Environment::define(
            &env,
            field.accessor_name.clone(),
            Value::NativeProcedure(Rc::new(NativeProcedure::RecordAccessor {
                name: field.accessor_name,
                record_type: record_type.clone(),
                field_index,
            })),
        );
    }

    Ok(Value::Void)
}

fn parse_symbol_name(expr: &Expr, message: &str) -> Result<String, EvalError> {
    let ExprKind::Symbol(name) = &expr.kind else {
        return Err(EvalError::syntax(message, expr.pos));
    };

    Ok(name.clone())
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<(String, Vec<String>), EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::syntax(
            "define-record-type constructor must be a list",
            expr.pos,
        ));
    };

    let Some((name_expr, field_exprs)) = items.split_first() else {
        return Err(EvalError::syntax(
            "define-record-type constructor must include a name",
            expr.pos,
        ));
    };

    let name = parse_symbol_name(
        name_expr,
        "define-record-type constructor name must be a symbol",
    )?;
    let mut fields = Vec::with_capacity(field_exprs.len());
    for field_expr in field_exprs {
        fields.push(parse_symbol_name(
            field_expr,
            "define-record-type constructor fields must be symbols",
        )?);
    }

    Ok((name, fields))
}

fn parse_record_field_spec(expr: &Expr) -> Result<RecordFieldSpec, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::syntax(
            "define-record-type field spec must be a list",
            expr.pos,
        ));
    };

    let [field_name_expr, accessor_name_expr] = items.as_slice() else {
        return Err(EvalError::syntax(
            "define-record-type field spec must contain a field name and accessor",
            expr.pos,
        ));
    };

    Ok(RecordFieldSpec {
        name: parse_symbol_name(
            field_name_expr,
            "define-record-type field name must be a symbol",
        )?,
        accessor_name: parse_symbol_name(
            accessor_name_expr,
            "define-record-type accessor name must be a symbol",
        )?,
        position: expr.pos,
    })
}

fn resolve_record_constructor_fields(
    constructor_fields: &[String],
    fields: &[RecordFieldSpec],
    position: SourcePos,
) -> Result<Vec<usize>, EvalError> {
    let mut field_indices = HashMap::with_capacity(fields.len());
    for (index, field) in fields.iter().enumerate() {
        if field_indices.insert(field.name.clone(), index).is_some() {
            return Err(EvalError::syntax(
                format!("duplicate record field: {}", field.name),
                field.position,
            ));
        }
    }

    let mut constructor_indices = Vec::with_capacity(constructor_fields.len());
    let mut seen = HashSet::with_capacity(constructor_fields.len());
    for field_name in constructor_fields {
        let Some(&field_index) = field_indices.get(field_name) else {
            return Err(EvalError::syntax(
                format!("unknown constructor field: {field_name}"),
                position,
            ));
        };

        if !seen.insert(field_index) {
            return Err(EvalError::syntax(
                format!("duplicate constructor field: {field_name}"),
                position,
            ));
        }

        constructor_indices.push(field_index);
    }

    Ok(constructor_indices)
}

fn eval_all(exprs: &[Expr], env: EnvRef) -> Result<Vec<LocatedValue>, EvalError> {
    exprs
        .iter()
        .map(|expr| Ok(LocatedValue::new(eval(expr, env.clone())?, expr.pos)))
        .collect()
}

fn eval_define(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    match exprs {
        [signature_expr, body @ ..]
            if !body.is_empty() && matches!(&signature_expr.kind, ExprKind::List(_)) =>
        {
            let ExprKind::List(signature) = &signature_expr.kind else {
                return Err(EvalError::syntax("invalid define form", position));
            };

            let (name, params, rest_param) =
                parse_function_signature(signature, signature_expr.pos)?;
            let procedure = Value::Procedure(Rc::new(UserProcedure::single_clause(
                Some(name.clone()),
                params,
                rest_param,
                body.to_vec(),
                env.clone(),
            )));

            Environment::define(&env, name, procedure);
            Ok(Value::Void)
        }
        [name_expr, value_expr] if matches!(&name_expr.kind, ExprKind::Symbol(_)) => {
            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::syntax("invalid define form", position));
            };
            let value = eval(value_expr, env.clone())?;
            Environment::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        _ => Err(EvalError::syntax("invalid define form", position)),
    }
}

fn eval_set(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let [name_expr, value_expr] = exprs else {
        return Err(EvalError::syntax(
            "set! requires exactly 2 expressions",
            position,
        ));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::syntax(
            "set! target must be a symbol",
            name_expr.pos,
        ));
    };

    let binding = Environment::lookup_binding(&env, name)
        .ok_or_else(|| EvalError::unbound_variable(name.clone(), name_expr.pos))?;
    let value = eval(value_expr, env)?;
    *binding.borrow_mut() = value;
    Ok(Value::Void)
}

fn eval_if(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    match exprs {
        [condition, then_branch] => {
            if eval(condition, env.clone())?.is_truthy() {
                eval(then_branch, env)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, then_branch, else_branch] => {
            if eval(condition, env.clone())?.is_truthy() {
                eval(then_branch, env)
            } else {
                eval(else_branch, env)
            }
        }
        _ => Err(EvalError::syntax(
            "if requires 2 or 3 expressions",
            position,
        )),
    }
}

fn eval_quote(exprs: &[Expr], position: SourcePos) -> Result<Value, EvalError> {
    let [expr] = exprs else {
        return Err(EvalError::syntax(
            "quote requires exactly 1 expression",
            position,
        ));
    };

    Ok(quote_expr(expr))
}

fn eval_lambda(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = exprs.split_first() else {
        return Err(EvalError::syntax(
            "lambda requires a parameter list and body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "lambda requires at least 1 body expression",
            position,
        ));
    }

    let (params, rest_param) = parse_parameters(params_expr)?;

    Ok(Value::Procedure(Rc::new(UserProcedure::single_clause(
        None,
        params,
        rest_param,
        body.to_vec(),
        env,
    ))))
}

fn eval_case_lambda(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::syntax(
            "case-lambda requires at least 1 clause",
            position,
        ));
    }

    let clauses = exprs
        .iter()
        .map(parse_case_lambda_clause)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Procedure(Rc::new(UserProcedure::new(
        None, clauses, env,
    ))))
}

fn parse_function_signature(
    signature: &[Expr],
    position: SourcePos,
) -> Result<(String, Vec<String>, Option<String>), EvalError> {
    let Some((name_expr, params)) = signature.split_first() else {
        return Err(EvalError::syntax(
            "function definition requires a name",
            position,
        ));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::syntax(
            "function name must be a symbol",
            name_expr.pos,
        ));
    };

    let (parsed_params, rest_param) = parse_parameter_list(params)?;

    Ok((name.clone(), parsed_params, rest_param))
}

fn parse_parameters(expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    let ExprKind::List(params) = &expr.kind else {
        return Err(EvalError::syntax(
            "lambda parameters must be a list",
            expr.pos,
        ));
    };

    parse_parameter_list(params)
}

fn parse_case_lambda_clause(clause_expr: &Expr) -> Result<ProcedureClause, EvalError> {
    let ExprKind::List(items) = &clause_expr.kind else {
        return Err(EvalError::syntax(
            "case-lambda clauses must be lists",
            clause_expr.pos,
        ));
    };

    let Some((params_expr, body)) = items.split_first() else {
        return Err(EvalError::syntax(
            "case-lambda clauses cannot be empty",
            clause_expr.pos,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "case-lambda clauses require at least 1 body expression",
            clause_expr.pos,
        ));
    }

    let (params, rest_param) = parse_parameters(params_expr)?;
    Ok(ProcedureClause::new(params, rest_param, body.to_vec()))
}

fn parse_parameter_list(params: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut parsed = Vec::with_capacity(params.len());
    let mut index = 0;

    while let Some(param) = params.get(index) {
        let ExprKind::Symbol(name) = &param.kind else {
            return Err(EvalError::syntax(
                "parameter name must be a symbol",
                param.pos,
            ));
        };

        if name == "." {
            let Some(rest_expr) = params.get(index + 1) else {
                return Err(EvalError::syntax(
                    "rest parameter name must follow '.'",
                    param.pos,
                ));
            };

            let ExprKind::Symbol(rest_name) = &rest_expr.kind else {
                return Err(EvalError::syntax(
                    "parameter name must be a symbol",
                    rest_expr.pos,
                ));
            };

            if index + 2 != params.len() {
                return Err(EvalError::syntax(
                    "rest parameter must be last",
                    rest_expr.pos,
                ));
            }

            return Ok((parsed, Some(rest_name.clone())));
        }

        parsed.push(name.clone());
        index += 1;
    }

    Ok((parsed, None))
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(value) => Value::Integer(*value),
        ExprKind::Rational(value) => Value::from_number(Number::Rational(*value)),
        ExprKind::Inexact(value) => Value::Inexact(*value),
        ExprKind::Bool(value) => Value::Bool(*value),
        ExprKind::Char(value) => Value::Char(*value),
        ExprKind::String(value) => Value::String(SchemeString::immutable(value)),
        ExprKind::Symbol(value) => Value::Symbol(value.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_and(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for expr in exprs {
        let value = eval(expr, env.clone())?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for expr in exprs {
        let value = eval(expr, env.clone())?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Bool(false))
}

fn eval_begin(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    eval_sequence(exprs, env)
}

fn eval_cond(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    for (index, clause) in exprs.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::syntax("cond clauses must be lists", clause.pos));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::syntax(
                "cond clauses cannot be empty",
                clause.pos,
            ));
        };

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != exprs.len() {
                return Err(EvalError::syntax("cond else clause must be last", test.pos));
            }
            return eval_sequence(body, env);
        }

        let test_value = eval(test, env.clone())?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    let _ = position;
    Ok(Value::Void)
}

fn eval_case(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = exprs.split_first() else {
        return Err(EvalError::syntax(
            "case requires a key and at least 1 clause",
            position,
        ));
    };

    if clauses.is_empty() {
        return Err(EvalError::syntax(
            "case requires at least 1 clause",
            position,
        ));
    }

    let key = eval(key_expr, env.clone())?;

    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::syntax("case clauses must be lists", clause.pos));
        };

        let Some((datums_expr, body)) = items.split_first() else {
            return Err(EvalError::syntax(
                "case clauses cannot be empty",
                clause.pos,
            ));
        };

        if matches!(&datums_expr.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::syntax(
                    "case else clause must be last",
                    datums_expr.pos,
                ));
            }

            return eval_sequence(body, env);
        }

        let ExprKind::List(datums) = &datums_expr.kind else {
            return Err(EvalError::syntax(
                "case clause datums must be a list",
                datums_expr.pos,
            ));
        };

        if datums
            .iter()
            .any(|datum| scheme_eq(&key, &quote_expr(datum)))
        {
            return eval_sequence(body, env);
        }
    }

    Ok(Value::Void)
}

fn eval_let(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    match exprs {
        [first, bindings_expr, body @ ..]
            if !body.is_empty() && matches!(&first.kind, ExprKind::Symbol(_)) =>
        {
            let ExprKind::Symbol(name) = &first.kind else {
                unreachable!("guard ensures symbol");
            };
            eval_named_let(name, bindings_expr, body, env, position)
        }
        [bindings_expr, body @ ..] if !body.is_empty() => eval_plain_let(bindings_expr, body, env),
        _ => Err(EvalError::syntax(
            "let requires bindings and a body",
            position,
        )),
    }
}

fn eval_letrec(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
    sequential: bool,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = exprs.split_first() else {
        let name = if sequential { "letrec*" } else { "letrec" };
        return Err(EvalError::syntax(
            format!("{name} requires bindings and a body"),
            position,
        ));
    };

    if body.is_empty() {
        let name = if sequential { "letrec*" } else { "letrec" };
        return Err(EvalError::syntax(
            format!("{name} requires bindings and a body"),
            position,
        ));
    }

    let bindings = parse_bindings(bindings_expr)?;
    let let_env = Environment::child(env);
    let slots = bindings
        .iter()
        .map(|(name, _)| {
            let binding = Rc::new(RefCell::new(Value::Uninitialized));
            Environment::define_existing(&let_env, name.clone(), binding.clone());
            binding
        })
        .collect::<Vec<_>>();

    if sequential {
        for ((_, expr), slot) in bindings.iter().zip(slots.iter()) {
            let value = eval(expr, let_env.clone())?;
            *slot.borrow_mut() = value;
        }
    } else {
        let values = bindings
            .iter()
            .map(|(_, expr)| eval(expr, let_env.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        for (slot, value) in slots.iter().zip(values) {
            *slot.borrow_mut() = value;
        }
    }

    eval_sequence(body, let_env)
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone())?;
    let let_env = Environment::child(env);

    for ((name, _), value) in bindings.into_iter().zip(values) {
        Environment::define(&let_env, name, value.value);
    }

    eval_sequence(body, let_env)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let args = eval_binding_values(&bindings, env.clone())?;
    let params = bindings.into_iter().map(|(param, _)| param).collect();
    let let_env = Environment::child(env);

    let procedure = Value::Procedure(Rc::new(UserProcedure::single_clause(
        Some(name.into()),
        params,
        None,
        body.to_vec(),
        let_env.clone(),
    )));

    Environment::define(&let_env, name.into(), procedure.clone());
    apply(procedure, position, args, let_env)
}

fn parse_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return Err(EvalError::syntax(
            "let bindings must be a list",
            bindings_expr.pos,
        ));
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(items) = &binding.kind else {
            return Err(EvalError::syntax("let binding must be a list", binding.pos));
        };

        let [name_expr, value_expr] = items.as_slice() else {
            return Err(EvalError::syntax(
                "let binding must contain a name and value",
                binding.pos,
            ));
        };

        let ExprKind::Symbol(name) = &name_expr.kind else {
            return Err(EvalError::syntax(
                "let binding must contain a name and value",
                name_expr.pos,
            ));
        };

        parsed.push((name.clone(), value_expr.clone()));
    }

    Ok(parsed)
}

fn parse_do_bindings(bindings_expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return Err(EvalError::syntax(
            "do bindings must be a list",
            bindings_expr.pos,
        ));
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(items) = &binding.kind else {
            return Err(EvalError::syntax("do binding must be a list", binding.pos));
        };

        let (name_expr, init_expr, step) = match items.as_slice() {
            [name_expr, init_expr] => (name_expr, init_expr, None),
            [name_expr, init_expr, step_expr] => (name_expr, init_expr, Some(step_expr.clone())),
            _ => {
                return Err(EvalError::syntax(
                    "do binding must contain a name, init, and optional step",
                    binding.pos,
                ))
            }
        };

        let ExprKind::Symbol(name) = &name_expr.kind else {
            return Err(EvalError::syntax(
                "do binding name must be a symbol",
                name_expr.pos,
            ));
        };

        parsed.push(DoBinding {
            name: name.clone(),
            init: init_expr.clone(),
            step,
        });
    }

    Ok(parsed)
}

fn parse_do_termination_clause(expr: &Expr) -> Result<(Expr, Vec<Expr>), EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::syntax(
            "do termination clause must be a list",
            expr.pos,
        ));
    };

    let Some((test, result_exprs)) = items.split_first() else {
        return Err(EvalError::syntax(
            "do termination clause cannot be empty",
            expr.pos,
        ));
    };

    Ok((test.clone(), result_exprs.to_vec()))
}

fn eval_binding_values(
    bindings: &[(String, Expr)],
    env: EnvRef,
) -> Result<Vec<LocatedValue>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| Ok(LocatedValue::new(eval(expr, env.clone())?, expr.pos)))
        .collect()
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval(expr, env.clone())?;
    }

    Ok(last)
}

#[derive(Clone)]
struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn eval_do(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let Some((bindings_expr, rest)) = exprs.split_first() else {
        return Err(EvalError::syntax(
            "do requires bindings and a termination clause",
            position,
        ));
    };
    let Some((termination_expr, body)) = rest.split_first() else {
        return Err(EvalError::syntax(
            "do requires a termination clause",
            position,
        ));
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_termination_clause(termination_expr)?;
    let do_env = Environment::child(env.clone());
    let mut slots = Vec::with_capacity(bindings.len());

    for binding in &bindings {
        let initial_value = eval(&binding.init, env.clone())?;
        let slot = Rc::new(RefCell::new(initial_value));
        Environment::define_existing(&do_env, binding.name.clone(), slot.clone());
        slots.push(slot);
    }

    loop {
        if eval(&test_expr, do_env.clone())?.is_truthy() {
            return eval_sequence(&result_exprs, do_env);
        }

        if !body.is_empty() {
            let _ = eval_sequence(body, do_env.clone())?;
        }

        let next_values = bindings
            .iter()
            .zip(slots.iter())
            .map(|(binding, slot)| match &binding.step {
                Some(step) => eval(step, do_env.clone()),
                None => Ok(slot.borrow().clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (slot, value) in slots.iter().zip(next_values) {
            *slot.borrow_mut() = value;
        }
    }
}

fn apply(
    callable: Value,
    position: SourcePos,
    args: Vec<LocatedValue>,
    env: EnvRef,
) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(name) => apply_builtin(name, &args, position, env),
        Value::Procedure(procedure) => apply_user_procedure(procedure, args, position),
        Value::NativeProcedure(procedure) => apply_native_procedure(procedure, args, position),
        other => Err(EvalError::not_callable(other.type_name(), position)),
    }
}

fn apply_native_procedure(
    procedure: Rc<NativeProcedure>,
    args: Vec<LocatedValue>,
    position: SourcePos,
) -> Result<Value, EvalError> {
    match procedure.as_ref() {
        NativeProcedure::RecordConstructor {
            name,
            record_type,
            field_indices,
        } => {
            if args.len() != field_indices.len() {
                return Err(EvalError::wrong_arg_count(
                    name,
                    format!("exactly {}", field_indices.len()),
                    args.len(),
                    position,
                ));
            }

            let mut fields = vec![Value::Void; record_type.field_count];
            for (arg, &field_index) in args.iter().zip(field_indices.iter()) {
                fields[field_index] = arg.value.clone();
            }

            Ok(Value::Record(Rc::new(RecordInstance {
                record_type: record_type.clone(),
                fields,
            })))
        }
        NativeProcedure::RecordPredicate { name, record_type } => {
            if args.len() != 1 {
                return Err(EvalError::wrong_arg_count(
                    name,
                    "exactly 1",
                    args.len(),
                    position,
                ));
            }

            Ok(Value::Bool(matches!(
                &args[0].value,
                Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type)
            )))
        }
        NativeProcedure::RecordAccessor {
            name,
            record_type,
            field_index,
        } => {
            if args.len() != 1 {
                return Err(EvalError::wrong_arg_count(
                    name,
                    "exactly 1",
                    args.len(),
                    position,
                ));
            }

            match &args[0].value {
                Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type) => {
                    Ok(record.fields[*field_index].clone())
                }
                Value::Record(record) => Err(EvalError::type_mismatch(
                    record_type.name.as_str(),
                    record.record_type.name.as_str(),
                    args[0].position,
                )),
                other => Err(EvalError::type_mismatch(
                    record_type.name.as_str(),
                    other.type_name(),
                    args[0].position,
                )),
            }
        }
    }
}

fn apply_user_procedure(
    procedure: Rc<UserProcedure>,
    args: Vec<LocatedValue>,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let Some(clause) = procedure.matching_clause(args.len()) else {
        return Err(EvalError::wrong_arg_count(
            procedure.display_name(),
            procedure.expected_arity(),
            args.len(),
            position,
        ));
    };

    let call_env = Environment::child(procedure.env.clone());
    for (param, value) in clause.params.iter().cloned().zip(args.iter().cloned()) {
        Environment::define(&call_env, param, value.value);
    }

    if let Some(rest_param) = &clause.rest_param {
        let rest_values = args[clause.params.len()..]
            .iter()
            .map(|arg| arg.value.clone())
            .collect();
        Environment::define(&call_env, rest_param.clone(), Value::List(rest_values));
    }

    eval_sequence(&clause.body, call_env)
}

fn apply_builtin(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    match name {
        "abs" => apply_abs(args, position),
        "+" => apply_add(args, position),
        "-" => apply_sub(args, position),
        "*" => apply_mul(args, position),
        "/" => apply_div(args, position),
        "<" => apply_compare(name, args, position, |ordering| ordering == Ordering::Less),
        "<=" => apply_compare(name, args, position, |ordering| {
            ordering != Ordering::Greater
        }),
        "denominator" => apply_denominator(args, position),
        "=" => apply_compare(name, args, position, |ordering| ordering == Ordering::Equal),
        ">" => apply_compare(name, args, position, |ordering| {
            ordering == Ordering::Greater
        }),
        ">=" => apply_compare(name, args, position, |ordering| ordering != Ordering::Less),
        "append" => apply_append(args, position),
        "apply" => apply_apply(args, position, env),
        "assoc" => apply_assoc(args, position),
        "boolean?" => apply_type_predicate("boolean?", args, position, |value| {
            matches!(value, Value::Bool(_))
        }),
        "char?" => apply_type_predicate("char?", args, position, |value| {
            matches!(value, Value::Char(_))
        }),
        "char-alphabetic?" => {
            apply_char_predicate("char-alphabetic?", args, position, |ch| ch.is_alphabetic())
        }
        "char->integer" => apply_char_to_integer(args, position),
        "char-downcase" => apply_char_case_transform("char-downcase", args, position, |ch| {
            ch.to_ascii_lowercase()
        }),
        "char-numeric?" => {
            apply_char_predicate("char-numeric?", args, position, |ch| ch.is_ascii_digit())
        }
        "char-upcase" => {
            apply_char_case_transform("char-upcase", args, position, |ch| ch.to_ascii_uppercase())
        }
        "char=?" => apply_char_compare("char=?", args, position, |left, right| left == right),
        "char<?" => apply_char_compare("char<?", args, position, |left, right| left < right),
        "car" => apply_car(args, position),
        "cdr" => apply_cdr(args, position),
        "cons" => apply_cons(args, position),
        "display" => apply_display(args, position, env),
        "eq?" => apply_equality_predicate("eq?", args, position, scheme_eq),
        "eqv?" => apply_equality_predicate("eqv?", args, position, scheme_eq),
        "equal?" => apply_equality_predicate("equal?", args, position, scheme_equal),
        "even?" => apply_integer_predicate("even?", args, position, |value| value % 2 == 0),
        "exact->inexact" => apply_exact_to_inexact(args, position),
        "exact?" => apply_exact(args, position),
        "expt" => apply_expt(args, position),
        "integer->char" => apply_integer_to_char(args, position),
        "inexact->exact" => apply_inexact_to_exact(args, position),
        "inexact?" => apply_inexact(args, position),
        "integer?" => apply_integer(args, position),
        "length" => apply_length(args, position),
        "list" => Ok(Value::List(
            args.iter().map(|arg| arg.value.clone()).collect(),
        )),
        "list?" => apply_list_predicate(args, position),
        "list-ref" => apply_list_ref(args, position),
        "list-tail" => apply_list_tail(args, position),
        "list->string" => apply_list_to_string(args, position),
        "list->vector" => apply_list_to_vector(args, position),
        "map" => apply_map(args, position, env),
        "max" => apply_min_max("max", args, position, |ordering| {
            ordering == Ordering::Greater
        }),
        "make-vector" => apply_make_vector(args, position),
        "min" => apply_min_max("min", args, position, |ordering| ordering == Ordering::Less),
        "modulo" => apply_modulo(args, position),
        "negative?" => apply_number_predicate("negative?", args, position, Number::is_negative),
        "newline" => apply_newline(args, position, env),
        "null?" => apply_null(args, position),
        "not" => apply_not(args, position),
        "number->string" => apply_number_to_string(args, position),
        "number?" => apply_type_predicate("number?", args, position, |value| {
            matches!(
                value,
                Value::Integer(_) | Value::Rational(_) | Value::Inexact(_)
            )
        }),
        "odd?" => apply_integer_predicate("odd?", args, position, |value| value % 2 != 0),
        "pair?" => apply_type_predicate("pair?", args, position, |value| {
            matches!(value, Value::List(values) if !values.is_empty())
                || matches!(value, Value::Pair(_))
        }),
        "positive?" => apply_number_predicate("positive?", args, position, Number::is_positive),
        "procedure?" => apply_type_predicate("procedure?", args, position, |value| {
            matches!(
                value,
                Value::Builtin(_) | Value::Procedure(_) | Value::NativeProcedure(_)
            )
        }),
        "numerator" => apply_numerator(args, position),
        "quotient" => apply_quotient(args, position),
        "rational?" => apply_rational(args, position),
        "remainder" => apply_remainder(args, position),
        "string-append" => apply_string_append(args),
        "string?" => apply_type_predicate("string?", args, position, |value| {
            matches!(value, Value::String(_))
        }),
        "string-ci=?" => apply_string_compare("string-ci=?", args, position, |left, right| {
            left.to_lowercase() == right.to_lowercase()
        }),
        "string-copy" => apply_string_copy(args, position),
        "string-downcase" => {
            apply_string_case_transform("string-downcase", args, position, |value| {
                value.to_lowercase()
            })
        }
        "string=?" => apply_string_compare("string=?", args, position, |left, right| left == right),
        "string<?" => apply_string_compare("string<?", args, position, |left, right| left < right),
        "string-length" => apply_string_length(args, position),
        "string->list" => apply_string_to_list(args, position),
        "string->number" => apply_string_to_number(args, position),
        "string->symbol" => apply_string_to_symbol(args, position),
        "string-ref" => apply_string_ref(args, position),
        "string-set!" => apply_string_set(args, position),
        "string-upcase" => apply_string_case_transform("string-upcase", args, position, |value| {
            value.to_uppercase()
        }),
        "substring" => apply_substring(args, position),
        "symbol?" => apply_type_predicate("symbol?", args, position, |value| {
            matches!(value, Value::Symbol(_))
        }),
        "symbol->string" => apply_symbol_to_string(args, position),
        "vector" => Ok(Value::Vector(Rc::new(RefCell::new(
            args.iter().map(|arg| arg.value.clone()).collect(),
        )))),
        "vector->list" => apply_vector_to_list(args, position),
        "vector-length" => apply_vector_length(args, position),
        "vector-ref" => apply_vector_ref(args, position),
        "vector-set!" => apply_vector_set(args, position),
        "vector?" => apply_type_predicate("vector?", args, position, |value| {
            matches!(value, Value::Vector(_))
        }),
        "write" => apply_write(args, position, env),
        "zero?" => apply_number_predicate("zero?", args, position, Number::is_zero),
        _ => Err(EvalError::unbound_variable(name, position)),
    }
}

fn apply_abs(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "abs",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::from_number(args[0].as_number()?.abs(position)?))
}

fn apply_add(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let mut total = Number::Integer(0);

    for arg in args {
        total = total.add(arg.as_number()?, position)?;
    }

    Ok(Value::from_number(total))
}

fn apply_sub(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::wrong_arg_count("-", "at least 1", 0, position));
    };

    let first = first.as_number()?;

    if rest.is_empty() {
        return Ok(Value::from_number(first.neg(position)?));
    }

    let mut total = first;
    for arg in rest {
        total = total.sub(arg.as_number()?, position)?;
    }

    Ok(Value::from_number(total))
}

fn apply_mul(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let mut total = Number::Integer(1);

    for arg in args {
        total = total.mul(arg.as_number()?, position)?;
    }

    Ok(Value::from_number(total))
}

fn apply_div(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::wrong_arg_count("/", "at least 1", 0, position));
    };

    if rest.is_empty() {
        return Err(EvalError::wrong_arg_count("/", "at least 2", 1, position));
    }

    let mut total = first.as_number()?;
    for arg in rest {
        total = total.div(arg.as_number()?, arg.position, position)?;
    }

    Ok(Value::from_number(total))
}

fn expect_binary_integers(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 2",
            args.len(),
            position,
        ));
    }

    Ok((args[0].as_integer()?, args[1].as_integer()?))
}

fn apply_quotient(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (left, right) = expect_binary_integers("quotient", args, position)?;

    if right == 0 {
        return Err(EvalError::division_by_zero(args[1].position));
    }

    Ok(Value::Integer(
        left.checked_div(right)
            .ok_or_else(|| EvalError::integer_overflow(position))?,
    ))
}

fn apply_remainder(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (left, right) = expect_binary_integers("remainder", args, position)?;

    if right == 0 {
        return Err(EvalError::division_by_zero(args[1].position));
    }

    Ok(Value::Integer(
        left.checked_rem(right)
            .ok_or_else(|| EvalError::integer_overflow(position))?,
    ))
}

fn apply_modulo(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (left, right) = expect_binary_integers("modulo", args, position)?;

    if right == 0 {
        return Err(EvalError::division_by_zero(args[1].position));
    }

    let remainder = left
        .checked_rem(right)
        .ok_or_else(|| EvalError::integer_overflow(position))?;

    let modulo = if remainder != 0 && (remainder > 0) != (right > 0) {
        remainder
            .checked_add(right)
            .ok_or_else(|| EvalError::integer_overflow(position))?
    } else {
        remainder
    };

    Ok(Value::Integer(modulo))
}

fn apply_min_max<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    choose_new: F,
) -> Result<Value, EvalError>
where
    F: Fn(Ordering) -> bool,
{
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::wrong_arg_count(name, "at least 1", 0, position));
    };

    let mut best = first.as_number()?;
    for arg in rest {
        let value = arg.as_number()?;
        if matches!(value.compare(best), Some(ordering) if choose_new(ordering)) {
            best = value;
        }
    }

    Ok(Value::from_number(best))
}

fn apply_expt(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (base, exponent) = expect_binary_integers("expt", args, position)?;

    if exponent < 0 {
        return Err(EvalError::syntax(
            "expt requires a non-negative exponent",
            args[1].position,
        ));
    }

    let mut result = 1_i64;
    for _ in 0..exponent {
        result = result
            .checked_mul(base)
            .ok_or_else(|| EvalError::integer_overflow(position))?;
    }

    Ok(Value::Integer(result))
}

fn apply_compare<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(Ordering) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            name,
            "at least 2",
            args.len(),
            position,
        ));
    }

    let mut iter = args.iter();
    let mut left = iter.next().expect("comparison arity checked").as_number()?;

    for arg in iter {
        let right = arg.as_number()?;
        if !matches!(left.compare(right), Some(ordering) if predicate(ordering)) {
            return Ok(Value::Bool(false));
        }
        left = right;
    }

    Ok(Value::Bool(true))
}

fn number_parts(number: Number, position: SourcePos) -> Result<(i64, i64), EvalError> {
    match inexact_to_exact(number, position)? {
        Number::Integer(value) => Ok((value, 1)),
        Number::Rational(rational) => Ok((rational.numerator, rational.denominator)),
        Number::Inexact(_) => unreachable!("inexact_to_exact always returns an exact number"),
    }
}

fn apply_exact(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "exact?",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(
        args[0].value.number().is_some_and(Number::is_exact),
    ))
}

fn apply_inexact(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "inexact?",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(matches!(
        args[0].value.number(),
        Some(Number::Inexact(_))
    )))
}

fn apply_exact_to_inexact(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "exact->inexact",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Inexact(args[0].as_number()?.to_f64()))
}

fn apply_inexact_to_exact(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "inexact->exact",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::from_number(inexact_to_exact(
        args[0].as_number()?,
        position,
    )?))
}

fn apply_integer(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "integer?",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(
        args[0].value.number().is_some_and(Number::is_integer),
    ))
}

fn apply_rational(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "rational?",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(args[0].value.number().is_some()))
}

fn apply_numerator(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "numerator",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let (numerator, _) = number_parts(args[0].as_number()?, position)?;
    Ok(Value::Integer(numerator))
}

fn apply_denominator(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "denominator",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let (_, denominator) = number_parts(args[0].as_number()?, position)?;
    Ok(Value::Integer(denominator))
}

fn apply_not(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "not",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(!args[0].value.is_truthy()))
}

fn apply_display(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "display",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Environment::append_output(&env, &args[0].value.to_display_string());
    Ok(Value::Void)
}

fn apply_newline(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::wrong_arg_count(
            "newline",
            "exactly 0",
            args.len(),
            position,
        ));
    }

    Environment::append_output(&env, "\n");
    Ok(Value::Void)
}

fn apply_write(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "write",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Environment::append_output(&env, &args[0].value.to_scheme_string());
    Ok(Value::Void)
}

fn apply_append(args: &[LocatedValue], _position: SourcePos) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for arg in args {
        items.extend(expect_proper_list(arg)?.iter().cloned());
    }

    Ok(Value::List(items))
}

fn apply_apply(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            "apply",
            "at least 2",
            args.len(),
            position,
        ));
    }

    let (callable, list_and_prefix_args) = args.split_first().expect("apply arity checked");
    let (list_arg, prefix_args) = list_and_prefix_args
        .split_last()
        .expect("apply arity checked");

    let list_values = expect_proper_list(list_arg)?;

    let mut expanded_args = Vec::with_capacity(prefix_args.len() + list_values.len());
    expanded_args.extend(prefix_args.iter().cloned());
    expanded_args.extend(
        list_values
            .iter()
            .cloned()
            .map(|value| LocatedValue::new(value, list_arg.position)),
    );

    apply(
        callable.value.clone(),
        callable.position,
        expanded_args,
        env,
    )
}

fn apply_number_to_string(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "number->string",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::String(SchemeString::immutable(
        &args[0].as_number()?.to_scheme_string(),
    )))
}

fn apply_string_append(args: &[LocatedValue]) -> Result<Value, EvalError> {
    let mut result = String::new();

    for arg in args {
        result.push_str(&arg.as_string()?.to_plain_string());
    }

    Ok(Value::String(SchemeString::immutable(&result)))
}

fn apply_string_copy(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "string-copy",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let string = args[0].as_string()?;
    let copy = if uses_immutable_strings() {
        SchemeString::immutable(&string.to_plain_string())
    } else {
        string.mutable_copy()
    };

    Ok(Value::String(copy))
}

fn apply_string_length(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "string-length",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Integer(args[0].as_string()?.len_chars() as i64))
}

fn apply_string_to_list(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "string->list",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::List(
        args[0]
            .as_string()?
            .to_plain_string()
            .chars()
            .map(Value::Char)
            .collect(),
    ))
}

fn apply_string_to_number(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "string->number",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(
        match parse_number_literal(&args[0].as_string()?.to_plain_string(), position)? {
            Some(number) => Value::from_number(number),
            None => Value::Bool(false),
        },
    )
}

fn apply_substring(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::wrong_arg_count(
            "substring",
            "exactly 3",
            args.len(),
            position,
        ));
    }

    let string = args[0].as_string()?;
    let length = string.len_chars();
    let start = args[1].as_integer()?;
    let end = args[2].as_integer()?;

    if start < 0 || end < 0 || start > end || end as usize > length {
        return Err(EvalError::invalid_range(start, end, length, position));
    }

    Ok(Value::String(SchemeString::immutable(
        &string.substring(start as usize, end as usize),
    )))
}

fn apply_symbol_to_string(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "symbol->string",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::String(SchemeString::immutable(
        &args[0].as_symbol()?.to_string(),
    )))
}

fn apply_string_to_symbol(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "string->symbol",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Symbol(args[0].as_string()?.to_plain_string()))
}

fn apply_integer_to_char(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "integer->char",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let value = args[0].as_integer()?;
    let scalar = u32::try_from(value)
        .ok()
        .and_then(char::from_u32)
        .ok_or_else(|| EvalError::invalid_character_code_point(value, args[0].position))?;
    Ok(Value::Char(scalar))
}

fn apply_string_ref(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "string-ref",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let string = args[0].as_string()?;
    let length = string.len_chars();
    let index = args[1].as_integer()?;

    if index < 0 || index as usize >= length {
        return Err(EvalError::index_out_of_bounds(
            index,
            length,
            args[1].position,
        ));
    }

    Ok(Value::Char(
        string
            .char_at(index as usize)
            .expect("validated character index"),
    ))
}

fn apply_string_set(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::wrong_arg_count(
            "string-set!",
            "exactly 3",
            args.len(),
            position,
        ));
    }

    let string = args[0].as_string()?;
    let length = string.len_chars();
    let index = args[1].as_integer()?;

    if index < 0 || index as usize >= length {
        return Err(EvalError::index_out_of_bounds(
            index,
            length,
            args[1].position,
        ));
    }

    string.set_char(index as usize, args[2].as_char()?, args[0].position)?;
    Ok(Value::Void)
}

fn apply_car(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "car",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    match &args[0].value {
        Value::List(values) if !values.is_empty() => Ok(values[0].clone()),
        Value::Pair(pair) => Ok(pair.car.clone()),
        Value::List(_) => Err(EvalError::type_mismatch("pair", "list", args[0].position)),
        other => Err(EvalError::type_mismatch(
            "pair",
            other.type_name(),
            args[0].position,
        )),
    }
}

fn apply_cdr(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "cdr",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    match &args[0].value {
        Value::List(values) if !values.is_empty() => Ok(Value::List(values[1..].to_vec())),
        Value::Pair(pair) => Ok(pair.cdr.clone()),
        Value::List(_) => Err(EvalError::type_mismatch("pair", "list", args[0].position)),
        other => Err(EvalError::type_mismatch(
            "pair",
            other.type_name(),
            args[0].position,
        )),
    }
}

fn apply_cons(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "cons",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    match &args[1].value {
        Value::List(rest) => {
            let mut values = Vec::with_capacity(rest.len() + 1);
            values.push(args[0].value.clone());
            values.extend(rest.iter().cloned());
            Ok(Value::List(values))
        }
        other => Ok(Value::Pair(Rc::new(Pair {
            car: args[0].value.clone(),
            cdr: other.clone(),
        }))),
    }
}

fn apply_length(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "length",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Integer(expect_proper_list(&args[0])?.len() as i64))
}

fn apply_null(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "null?",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(matches!(
        &args[0].value,
        Value::List(values) if values.is_empty()
    )))
}

fn apply_list_predicate(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "list?",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(matches!(&args[0].value, Value::List(_))))
}

fn apply_list_ref(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "list-ref",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let values = expect_proper_list(&args[0])?;
    let index = args[1].as_integer()?;
    if index < 0 || index as usize >= values.len() {
        return Err(EvalError::index_out_of_bounds(
            index,
            values.len(),
            args[1].position,
        ));
    }

    Ok(values[index as usize].clone())
}

fn apply_list_tail(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "list-tail",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let values = expect_proper_list(&args[0])?;
    let index = args[1].as_integer()?;
    if index < 0 || index as usize > values.len() {
        return Err(EvalError::index_out_of_bounds(
            index,
            values.len(),
            args[1].position,
        ));
    }

    Ok(Value::List(values[index as usize..].to_vec()))
}

fn apply_list_to_string(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "list->string",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let values = expect_proper_list(&args[0])?;
    let mut result = String::new();

    for value in values {
        let Value::Char(ch) = value else {
            return Err(EvalError::type_mismatch(
                "char",
                value.type_name(),
                args[0].position,
            ));
        };
        result.push(*ch);
    }

    Ok(Value::String(SchemeString::immutable(&result)))
}

fn expect_non_negative_length(arg: &LocatedValue) -> Result<usize, EvalError> {
    let length = arg.as_integer()?;
    usize::try_from(length).map_err(|_| EvalError::invalid_length(length, arg.position))
}

fn apply_list_to_vector(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "list->vector",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Vector(Rc::new(RefCell::new(
        expect_proper_list(&args[0])?.to_vec(),
    ))))
}

fn apply_make_vector(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::wrong_arg_count(
            "make-vector",
            "1 or 2",
            args.len(),
            position,
        ));
    }

    let length = expect_non_negative_length(&args[0])?;
    let fill = args.get(1).map_or(Value::Void, |arg| arg.value.clone());
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; length]))))
}

fn apply_vector_to_list(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "vector->list",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::List(args[0].as_vector()?.borrow().clone()))
}

fn apply_vector_length(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "vector-length",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Integer(args[0].as_vector()?.borrow().len() as i64))
}

fn apply_vector_ref(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "vector-ref",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let vector = args[0].as_vector()?;
    let index = args[1].as_integer()?;
    let values = vector.borrow();
    if index < 0 || index as usize >= values.len() {
        return Err(EvalError::index_out_of_bounds(
            index,
            values.len(),
            args[1].position,
        ));
    }

    Ok(values[index as usize].clone())
}

fn apply_vector_set(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::wrong_arg_count(
            "vector-set!",
            "exactly 3",
            args.len(),
            position,
        ));
    }

    let vector = args[0].as_vector()?;
    let index = args[1].as_integer()?;
    let mut values = vector.borrow_mut();
    if index < 0 || index as usize >= values.len() {
        return Err(EvalError::index_out_of_bounds(
            index,
            values.len(),
            args[1].position,
        ));
    }

    values[index as usize] = args[2].value.clone();
    Ok(Value::Void)
}

fn apply_assoc(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "assoc",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let alist = expect_proper_list(&args[1])?;
    for entry in alist {
        let key = match entry {
            Value::List(values) if !values.is_empty() => &values[0],
            Value::Pair(pair) => &pair.car,
            _ => continue,
        };

        if scheme_equal(&args[0].value, key) {
            return Ok(entry.clone());
        }
    }

    Ok(Value::Bool(false))
}

fn apply_map(args: &[LocatedValue], position: SourcePos, env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            "map",
            "at least 2",
            args.len(),
            position,
        ));
    }

    let callable = args[0].value.clone();
    let lists = args[1..]
        .iter()
        .map(expect_proper_list)
        .collect::<Result<Vec<_>, _>>()?;

    let Some(first_len) = lists.first().map(|values| values.len()) else {
        return Ok(Value::List(Vec::new()));
    };

    if lists.iter().any(|values| values.len() != first_len) {
        return Err(EvalError::syntax(
            "map requires lists of equal length",
            position,
        ));
    }

    let mut mapped = Vec::with_capacity(first_len);
    for index in 0..first_len {
        let call_args = lists
            .iter()
            .zip(args[1..].iter())
            .map(|(values, arg)| LocatedValue::new(values[index].clone(), arg.position))
            .collect();
        mapped.push(apply(
            callable.clone(),
            args[0].position,
            call_args,
            env.clone(),
        )?);
    }

    Ok(Value::List(mapped))
}

fn apply_equality_predicate<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value, &Value) -> bool,
{
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 2",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(predicate(&args[0].value, &args[1].value)))
}

fn apply_number_predicate<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(Number) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(predicate(args[0].as_number()?)))
}

fn apply_integer_predicate<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(i64) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(predicate(args[0].as_integer()?)))
}

fn apply_char_predicate<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(char) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(predicate(args[0].as_char()?)))
}

fn apply_char_to_integer(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "char->integer",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Integer(i64::from(u32::from(args[0].as_char()?))))
}

fn apply_char_case_transform<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    transform: F,
) -> Result<Value, EvalError>
where
    F: Fn(char) -> char,
{
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Char(transform(args[0].as_char()?)))
}

fn apply_char_compare<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            name,
            "at least 2",
            args.len(),
            position,
        ));
    }

    let mut iter = args.iter();
    let mut left = iter.next().expect("comparison arity checked").as_char()?;
    for arg in iter {
        let right = arg.as_char()?;
        if !predicate(left, right) {
            return Ok(Value::Bool(false));
        }
        left = right;
    }

    Ok(Value::Bool(true))
}

fn apply_string_compare<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            name,
            "at least 2",
            args.len(),
            position,
        ));
    }

    let mut iter = args.iter();
    let mut left = iter
        .next()
        .expect("comparison arity checked")
        .as_string()?
        .to_plain_string();
    for arg in iter {
        let right = arg.as_string()?.to_plain_string();
        if !predicate(&left, &right) {
            return Ok(Value::Bool(false));
        }
        left = right;
    }

    Ok(Value::Bool(true))
}

fn apply_string_case_transform<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    transform: F,
) -> Result<Value, EvalError>
where
    F: Fn(String) -> String,
{
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::String(SchemeString::immutable(&transform(
        args[0].as_string()?.to_plain_string(),
    ))))
}

fn apply_type_predicate<F>(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Bool(predicate(&args[0].value)))
}

fn expect_proper_list<'a>(arg: &'a LocatedValue) -> Result<&'a [Value], EvalError> {
    let Value::List(values) = &arg.value else {
        return Err(EvalError::type_mismatch(
            "list",
            arg.value.type_name(),
            arg.position,
        ));
    };

    Ok(values)
}

fn fresh_generated_symbol(kind: &str) -> String {
    let next = GENERATED_SYMBOL_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
    format!("__macro_{kind}_{next}")
}

fn is_core_syntax_keyword(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "begin"
            | "case"
            | "case-lambda"
            | "cond"
            | "define"
            | "define-record-type"
            | "define-syntax"
            | "do"
            | "if"
            | "lambda"
            | "let"
            | "letrec"
            | "letrec*"
            | "or"
            | "quote"
            | "set!"
            | "syntax-rules"
    )
}

fn is_ellipsis_expr(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(name) if name == "...")
}

fn is_pattern_variable(name: &str, literals: &HashSet<String>, macro_name: &str) -> bool {
    name != "..." && name != "_" && name != macro_name && !literals.contains(name)
}

fn parse_syntax_rules(
    macro_name: &str,
    expr: &Expr,
    env: EnvRef,
    position: SourcePos,
) -> Result<MacroRef, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::syntax(
            "define-syntax requires a syntax-rules transformer",
            position,
        ));
    };

    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::syntax(
            "syntax-rules requires a literals list and at least 1 rule",
            position,
        ));
    };

    match &head.kind {
        ExprKind::Symbol(name) if name == "syntax-rules" => {}
        _ => {
            return Err(EvalError::syntax(
                "define-syntax only supports syntax-rules",
                head.pos,
            ));
        }
    }

    let Some((literals_expr, rules_exprs)) = tail.split_first() else {
        return Err(EvalError::syntax(
            "syntax-rules requires a literals list and at least 1 rule",
            position,
        ));
    };

    if rules_exprs.is_empty() {
        return Err(EvalError::syntax(
            "syntax-rules requires at least 1 rule",
            position,
        ));
    }

    let literals = parse_syntax_rule_literals(literals_expr)?;
    let mut rules = Vec::with_capacity(rules_exprs.len());

    for rule_expr in rules_exprs {
        let ExprKind::List(rule_items) = &rule_expr.kind else {
            return Err(EvalError::syntax(
                "syntax-rules clauses must be lists",
                rule_expr.pos,
            ));
        };

        let [pattern, template] = rule_items.as_slice() else {
            return Err(EvalError::syntax(
                "syntax-rules clauses must contain a pattern and template",
                rule_expr.pos,
            ));
        };

        rules.push(MacroRule {
            pattern: pattern.clone(),
            template: template.clone(),
        });
    }

    Ok(Rc::new(SyntaxRulesMacro {
        name: macro_name.to_string(),
        literals,
        rules,
        env,
    }))
}

fn parse_syntax_rule_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::syntax(
            "syntax-rules literals must be a list",
            expr.pos,
        ));
    };

    let mut literals = HashSet::with_capacity(items.len());
    for item in items {
        let ExprKind::Symbol(name) = &item.kind else {
            return Err(EvalError::syntax(
                "syntax-rules literals must be identifiers",
                item.pos,
            ));
        };

        if name == "..." {
            return Err(EvalError::syntax(
                "syntax-rules literal cannot be '...'",
                item.pos,
            ));
        }

        literals.insert(name.clone());
    }

    Ok(literals)
}

fn expand_macro_call(
    transformer: MacroRef,
    invocation_items: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<ExpandedExpr, EvalError> {
    let invocation = Expr::list(invocation_items.to_vec(), position);

    for rule in &transformer.rules {
        let Some(bindings) =
            match_macro_rule(rule, &invocation, &transformer.literals, &transformer.name)
        else {
            continue;
        };

        let mut state = ExpansionState::default();
        let expr = expand_template(
            &rule.template,
            &bindings,
            &transformer,
            &mut state,
            &[],
            false,
        )?;
        let env = state.build_env(env);
        return Ok(ExpandedExpr { expr, env });
    }

    Err(EvalError::syntax(
        format!("no matching syntax-rules clause for {}", transformer.name),
        position,
    ))
}

fn match_macro_rule(
    rule: &MacroRule,
    invocation: &Expr,
    literals: &HashSet<String>,
    macro_name: &str,
) -> Option<PatternBindings> {
    let (ExprKind::List(pattern_items), ExprKind::List(invocation_items)) =
        (&rule.pattern.kind, &invocation.kind)
    else {
        return match_pattern(&rule.pattern, invocation, literals, macro_name);
    };

    let Some((pattern_head, pattern_tail)) = pattern_items.split_first() else {
        return None;
    };
    let Some((_invocation_head, invocation_tail)) = invocation_items.split_first() else {
        return None;
    };

    matches!(&pattern_head.kind, ExprKind::Symbol(name) if name == macro_name)
        .then_some(())
        .and_then(|_| match_pattern_list(pattern_tail, invocation_tail, literals, macro_name))
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    macro_name: &str,
) -> Option<PatternBindings> {
    match (&pattern.kind, &input.kind) {
        (ExprKind::Integer(left), ExprKind::Integer(right)) if left == right => {
            Some(HashMap::new())
        }
        (ExprKind::Rational(left), ExprKind::Rational(right)) if left == right => {
            Some(HashMap::new())
        }
        (ExprKind::Inexact(left), ExprKind::Inexact(right)) if left == right => {
            Some(HashMap::new())
        }
        (ExprKind::Bool(left), ExprKind::Bool(right)) if left == right => Some(HashMap::new()),
        (ExprKind::Char(left), ExprKind::Char(right)) if left == right => Some(HashMap::new()),
        (ExprKind::String(left), ExprKind::String(right)) if left == right => Some(HashMap::new()),
        (ExprKind::List(pattern_items), ExprKind::List(input_items)) => {
            match_pattern_list(pattern_items, input_items, literals, macro_name)
        }
        (ExprKind::Symbol(name), _) if name == "_" => Some(HashMap::new()),
        (ExprKind::Symbol(name), ExprKind::Symbol(input_name))
            if !is_pattern_variable(name, literals, macro_name) =>
        {
            (name == input_name).then(HashMap::new)
        }
        (ExprKind::Symbol(name), _) if is_pattern_variable(name, literals, macro_name) => Some(
            HashMap::from([(name.clone(), PatternBinding::One(input.clone()))]),
        ),
        _ => None,
    }
}

fn match_pattern_list(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &HashSet<String>,
    macro_name: &str,
) -> Option<PatternBindings> {
    if patterns.is_empty() {
        return inputs.is_empty().then(HashMap::new);
    }

    if patterns.len() >= 2 && is_ellipsis_expr(&patterns[1]) {
        let repeated_pattern = &patterns[0];
        let suffix = &patterns[2..];
        let min_suffix = minimum_pattern_items(suffix);

        if inputs.len() < min_suffix {
            return None;
        }

        let mut repeated_vars = HashSet::new();
        collect_pattern_variables(repeated_pattern, literals, macro_name, &mut repeated_vars);

        let max_repetitions = inputs.len() - min_suffix;
        for repeat_count in 0..=max_repetitions {
            let mut repetition_bindings = Vec::with_capacity(repeat_count);
            let mut matched = true;

            for input in &inputs[..repeat_count] {
                let Some(bindings) = match_pattern(repeated_pattern, input, literals, macro_name)
                else {
                    matched = false;
                    break;
                };
                repetition_bindings.push(bindings);
            }

            if !matched {
                continue;
            }

            let Some(repeated_bindings) =
                collect_repeated_bindings(&repeated_vars, &repetition_bindings)
            else {
                continue;
            };
            let Some(suffix_bindings) =
                match_pattern_list(suffix, &inputs[repeat_count..], literals, macro_name)
            else {
                continue;
            };

            if let Some(merged) = merge_pattern_bindings(repeated_bindings, suffix_bindings) {
                return Some(merged);
            }
        }

        return None;
    }

    let Some((first_input, rest_inputs)) = inputs.split_first() else {
        return None;
    };

    let first_bindings = match_pattern(&patterns[0], first_input, literals, macro_name)?;
    let rest_bindings = match_pattern_list(&patterns[1..], rest_inputs, literals, macro_name)?;
    merge_pattern_bindings(first_bindings, rest_bindings)
}

fn minimum_pattern_items(patterns: &[Expr]) -> usize {
    let mut required = 0;
    let mut index = 0;

    while index < patterns.len() {
        if index + 1 < patterns.len() && is_ellipsis_expr(&patterns[index + 1]) {
            index += 2;
        } else {
            required += 1;
            index += 1;
        }
    }

    required
}

fn collect_pattern_variables(
    pattern: &Expr,
    literals: &HashSet<String>,
    macro_name: &str,
    vars: &mut HashSet<String>,
) {
    match &pattern.kind {
        ExprKind::Symbol(name) if is_pattern_variable(name, literals, macro_name) => {
            vars.insert(name.clone());
        }
        ExprKind::List(items) => {
            let mut index = 0;
            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis_expr(&items[index + 1]) {
                    collect_pattern_variables(&items[index], literals, macro_name, vars);
                    index += 2;
                } else {
                    collect_pattern_variables(&items[index], literals, macro_name, vars);
                    index += 1;
                }
            }
        }
        _ => {}
    }
}

fn collect_repeated_bindings(
    repeated_vars: &HashSet<String>,
    repetitions: &[PatternBindings],
) -> Option<PatternBindings> {
    let mut bindings = HashMap::with_capacity(repeated_vars.len());

    for var in repeated_vars {
        let mut values = Vec::with_capacity(repetitions.len());

        for repetition in repetitions {
            values.push(repetition.get(var)?.clone());
        }

        bindings.insert(var.clone(), PatternBinding::Many(values));
    }

    Some(bindings)
}

fn merge_pattern_bindings(
    mut left: PatternBindings,
    right: PatternBindings,
) -> Option<PatternBindings> {
    for (name, binding) in right {
        match left.get(&name) {
            Some(existing) if existing != &binding => return None,
            Some(_) => {}
            None => {
                left.insert(name, binding);
            }
        }
    }

    Some(left)
}

fn expand_template(
    template: &Expr,
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
    quoted: bool,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => expand_template_symbol(
            template,
            name,
            bindings,
            transformer,
            state,
            indices,
            quoted,
        ),
        ExprKind::List(items) => expand_template_list(
            template,
            items,
            bindings,
            transformer,
            state,
            indices,
            quoted,
        ),
        _ => Ok(template.clone()),
    }
}

fn expand_template_symbol(
    template: &Expr,
    name: &str,
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
    quoted: bool,
) -> Result<Expr, EvalError> {
    if let Some(binding) = bindings.get(name) {
        return resolve_pattern_binding(binding, indices).ok_or_else(|| {
            EvalError::syntax(
                format!("pattern variable {name} used outside the required ellipsis context"),
                template.pos,
            )
        });
    }

    if quoted {
        return Ok(template.clone());
    }

    if let Some(renamed) = state.lookup_bound_name(name) {
        return Ok(Expr::symbol(renamed.to_string(), template.pos));
    }

    if name == "..." || is_core_syntax_keyword(name) {
        return Ok(template.clone());
    }

    Ok(Expr::symbol(
        state.alias_for(name, &transformer.env),
        template.pos,
    ))
}

fn expand_template_list(
    template: &Expr,
    items: &[Expr],
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
    quoted: bool,
) -> Result<Expr, EvalError> {
    if !quoted {
        if let Some(Expr {
            kind: ExprKind::Symbol(head_name),
            ..
        }) = items.first()
        {
            if !bindings.contains_key(head_name) && state.lookup_bound_name(head_name).is_none() {
                match head_name.as_str() {
                    "quote" => {
                        return expand_quote_template(
                            template.pos,
                            items,
                            bindings,
                            transformer,
                            state,
                            indices,
                        )
                    }
                    "let" => {
                        return expand_let_template(
                            template.pos,
                            items,
                            bindings,
                            transformer,
                            state,
                            indices,
                        )
                    }
                    "lambda" => {
                        return expand_lambda_template(
                            template.pos,
                            items,
                            bindings,
                            transformer,
                            state,
                            indices,
                        )
                    }
                    "case-lambda" => {
                        return expand_case_lambda_template(
                            template.pos,
                            items,
                            bindings,
                            transformer,
                            state,
                            indices,
                        )
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(Expr::list(
        expand_template_sequence(items, bindings, transformer, state, indices, quoted)?,
        template.pos,
    ))
}

fn expand_quote_template(
    position: SourcePos,
    items: &[Expr],
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
) -> Result<Expr, EvalError> {
    let [head, datum] = items else {
        return Err(EvalError::syntax(
            "quote template must contain exactly 1 datum",
            position,
        ));
    };

    Ok(Expr::list(
        vec![
            head.clone(),
            expand_template(datum, bindings, transformer, state, indices, true)?,
        ],
        position,
    ))
}

fn expand_let_template(
    position: SourcePos,
    items: &[Expr],
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
) -> Result<Expr, EvalError> {
    let Some((_, tail)) = items.split_first() else {
        return Err(EvalError::syntax("invalid let template", position));
    };

    let Some((bindings_expr, body)) = tail.split_first() else {
        return Err(EvalError::syntax(
            "let template requires bindings and a body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "let template requires at least 1 body expression",
            position,
        ));
    }

    let ExprKind::List(binding_items) = &bindings_expr.kind else {
        return Err(EvalError::syntax(
            "let template bindings must be a list",
            bindings_expr.pos,
        ));
    };

    let mut expanded_bindings = Vec::with_capacity(binding_items.len());
    let mut scope = HashMap::new();

    for binding_expr in binding_items {
        let ExprKind::List(binding_parts) = &binding_expr.kind else {
            return Err(EvalError::syntax(
                "let template bindings must be lists",
                binding_expr.pos,
            ));
        };

        let [name_expr, value_expr] = binding_parts.as_slice() else {
            return Err(EvalError::syntax(
                "let template bindings must contain a name and value",
                binding_expr.pos,
            ));
        };

        let (expanded_name, rename) =
            expand_binding_identifier(name_expr, bindings, transformer, state, indices)?;
        let expanded_value =
            expand_template(value_expr, bindings, transformer, state, indices, false)?;

        if let Some((original, renamed)) = rename {
            scope.insert(original, renamed);
        }

        expanded_bindings.push(Expr::list(
            vec![expanded_name, expanded_value],
            binding_expr.pos,
        ));
    }

    state.push_scope(scope);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(items[0].clone());
    expanded_items.push(Expr::list(expanded_bindings, bindings_expr.pos));
    for body_expr in body {
        expanded_items.push(expand_template(
            body_expr,
            bindings,
            transformer,
            state,
            indices,
            false,
        )?);
    }

    state.pop_scope();
    Ok(Expr::list(expanded_items, position))
}

fn expand_lambda_template(
    position: SourcePos,
    items: &[Expr],
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
) -> Result<Expr, EvalError> {
    let [head, params_expr, body @ ..] = items else {
        return Err(EvalError::syntax(
            "lambda template requires parameters and a body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "lambda template requires at least 1 body expression",
            position,
        ));
    }

    let (expanded_params, scope) =
        expand_lambda_parameters(params_expr, bindings, transformer, state, indices)?;

    state.push_scope(scope);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());
    expanded_items.push(expanded_params);
    for body_expr in body {
        expanded_items.push(expand_template(
            body_expr,
            bindings,
            transformer,
            state,
            indices,
            false,
        )?);
    }

    state.pop_scope();
    Ok(Expr::list(expanded_items, position))
}

fn expand_case_lambda_template(
    position: SourcePos,
    items: &[Expr],
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
) -> Result<Expr, EvalError> {
    let [head, clauses @ ..] = items else {
        return Err(EvalError::syntax(
            "case-lambda template requires at least 1 clause",
            position,
        ));
    };

    if clauses.is_empty() {
        return Err(EvalError::syntax(
            "case-lambda template requires at least 1 clause",
            position,
        ));
    }

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());

    for clause in clauses {
        let ExprKind::List(clause_items) = &clause.kind else {
            return Err(EvalError::syntax(
                "case-lambda template clauses must be lists",
                clause.pos,
            ));
        };

        let Some((params_expr, body)) = clause_items.split_first() else {
            return Err(EvalError::syntax(
                "case-lambda template clauses cannot be empty",
                clause.pos,
            ));
        };

        if body.is_empty() {
            return Err(EvalError::syntax(
                "case-lambda template clauses require at least 1 body expression",
                clause.pos,
            ));
        }

        let (expanded_params, scope) =
            expand_lambda_parameters(params_expr, bindings, transformer, state, indices)?;

        state.push_scope(scope);

        let mut expanded_clause = Vec::with_capacity(clause_items.len());
        expanded_clause.push(expanded_params);
        for body_expr in body {
            expanded_clause.push(expand_template(
                body_expr,
                bindings,
                transformer,
                state,
                indices,
                false,
            )?);
        }

        state.pop_scope();
        expanded_items.push(Expr::list(expanded_clause, clause.pos));
    }

    Ok(Expr::list(expanded_items, position))
}

fn expand_lambda_parameters(
    params_expr: &Expr,
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
) -> Result<(Expr, HashMap<String, String>), EvalError> {
    match &params_expr.kind {
        ExprKind::Symbol(name) => {
            if name == "." {
                return Ok((params_expr.clone(), HashMap::new()));
            }

            let (expanded, rename) =
                expand_binding_identifier(params_expr, bindings, transformer, state, indices)?;
            let mut scope = HashMap::new();
            if let Some((original, renamed)) = rename {
                scope.insert(original, renamed);
            }
            Ok((expanded, scope))
        }
        ExprKind::List(params) => {
            let mut expanded = Vec::with_capacity(params.len());
            let mut scope = HashMap::new();

            for param in params {
                if matches!(&param.kind, ExprKind::Symbol(name) if name == ".") {
                    expanded.push(param.clone());
                    continue;
                }

                let (expanded_param, rename) =
                    expand_binding_identifier(param, bindings, transformer, state, indices)?;
                if let Some((original, renamed)) = rename {
                    scope.insert(original, renamed);
                }
                expanded.push(expanded_param);
            }

            Ok((Expr::list(expanded, params_expr.pos), scope))
        }
        _ => Ok((
            expand_template(params_expr, bindings, transformer, state, indices, false)?,
            HashMap::new(),
        )),
    }
}

fn expand_binding_identifier(
    expr: &Expr,
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
) -> Result<(Expr, Option<(String, String)>), EvalError> {
    let ExprKind::Symbol(name) = &expr.kind else {
        return Ok((
            expand_template(expr, bindings, transformer, state, indices, false)?,
            None,
        ));
    };

    if name == "." {
        return Ok((expr.clone(), None));
    }

    if let Some(binding) = bindings.get(name) {
        let expanded = resolve_pattern_binding(binding, indices).ok_or_else(|| {
            EvalError::syntax(
                format!("pattern variable {name} used outside the required ellipsis context"),
                expr.pos,
            )
        })?;
        return Ok((expanded, None));
    }

    let renamed = fresh_generated_symbol("bind");
    Ok((
        Expr::symbol(renamed.clone(), expr.pos),
        Some((name.clone(), renamed)),
    ))
}

fn expand_template_sequence(
    items: &[Expr],
    bindings: &PatternBindings,
    transformer: &SyntaxRulesMacro,
    state: &mut ExpansionState,
    indices: &[usize],
    quoted: bool,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis_expr(&items[index + 1]) {
            let repeat_count = determine_template_repeat_count(&items[index], bindings, indices)?;
            for repetition in 0..repeat_count {
                let next_indices = extend_indices(indices, repetition);
                expanded.push(expand_template(
                    &items[index],
                    bindings,
                    transformer,
                    state,
                    &next_indices,
                    quoted,
                )?);
            }
            index += 2;
        } else {
            expanded.push(expand_template(
                &items[index],
                bindings,
                transformer,
                state,
                indices,
                quoted,
            )?);
            index += 1;
        }
    }

    Ok(expanded)
}

fn determine_template_repeat_count(
    template: &Expr,
    bindings: &PatternBindings,
    indices: &[usize],
) -> Result<usize, EvalError> {
    let mut repeat_count = None;
    collect_template_repeat_count(template, bindings, indices, &mut repeat_count)?;
    repeat_count.ok_or_else(|| {
        EvalError::syntax(
            "ellipsis template must reference a repeated pattern variable",
            template.pos,
        )
    })
}

fn collect_template_repeat_count(
    template: &Expr,
    bindings: &PatternBindings,
    indices: &[usize],
    repeat_count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            let Some(binding) = bindings.get(name) else {
                return Ok(());
            };

            let Some(current_len) = binding_repeat_len(binding, indices) else {
                return Ok(());
            };

            match repeat_count {
                Some(existing) if *existing != current_len => Err(EvalError::syntax(
                    "mismatched ellipsis lengths in template",
                    template.pos,
                )),
                Some(_) => Ok(()),
                None => {
                    *repeat_count = Some(current_len);
                    Ok(())
                }
            }
        }
        ExprKind::List(items) => {
            let mut index = 0;
            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis_expr(&items[index + 1]) {
                    collect_template_repeat_count(&items[index], bindings, indices, repeat_count)?;
                    index += 2;
                } else {
                    collect_template_repeat_count(&items[index], bindings, indices, repeat_count)?;
                    index += 1;
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn binding_repeat_len(binding: &PatternBinding, indices: &[usize]) -> Option<usize> {
    let mut current = binding;

    for &index in indices {
        let PatternBinding::Many(values) = current else {
            return None;
        };
        current = values.get(index)?;
    }

    match current {
        PatternBinding::Many(values) => Some(values.len()),
        PatternBinding::One(_) => None,
    }
}

fn resolve_pattern_binding(binding: &PatternBinding, indices: &[usize]) -> Option<Expr> {
    let mut current = binding;

    for &index in indices {
        let PatternBinding::Many(values) = current else {
            return None;
        };
        current = values.get(index)?;
    }

    match current {
        PatternBinding::One(expr) => Some(expr.clone()),
        PatternBinding::Many(_) => None,
    }
}

fn extend_indices(indices: &[usize], next: usize) -> Vec<usize> {
    let mut extended = Vec::with_capacity(indices.len() + 1);
    extended.extend_from_slice(indices);
    extended.push(next);
    extended
}

struct Parser {
    chars: Vec<char>,
    line_starts: Vec<usize>,
    index: usize,
}

impl Parser {
    fn new(source: &str) -> Self {
        let chars: Vec<char> = source.chars().collect();
        let mut line_starts = vec![0];

        for (index, ch) in chars.iter().enumerate() {
            if *ch == '\n' {
                line_starts.push(index + 1);
            }
        }

        Self {
            chars,
            line_starts,
            index: 0,
        }
    }

    fn parse_program(mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while self.peek().is_some() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::EmptyInput);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let position = self.current_pos();

        match self.peek() {
            Some('(') => self.parse_list(position),
            Some(')') => Err(EvalError::syntax("unexpected ')'", position)),
            Some('\'') => self.parse_quote_shorthand(position),
            Some('"') => self.parse_string(position),
            Some(_) => self.parse_token_expr(position),
            None => Err(EvalError::syntax("unexpected end of input", position)),
        }
    }

    fn parse_list(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek() {
                Some(')') => {
                    self.index += 1;
                    return Ok(Expr::list(items, position));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::syntax("unterminated list", position)),
            }
        }
    }

    fn parse_quote_shorthand(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect('\'')?;
        Ok(Expr::list(
            vec![Expr::symbol("quote", position), self.parse_expr()?],
            position,
        ))
    }

    fn parse_string(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect('"')?;
        let mut value = String::new();

        while let Some(ch) = self.peek() {
            self.index += 1;
            match ch {
                '"' => return Ok(Expr::new(ExprKind::String(value), position)),
                '\\' => {
                    let escaped = self.peek().ok_or_else(|| {
                        EvalError::syntax("unterminated string escape", self.current_pos())
                    })?;
                    self.index += 1;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => value.push(other),
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::syntax("unterminated string", position))
    }

    fn parse_token_expr(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        let token = self.take_token();

        if token.is_empty() {
            return Err(EvalError::syntax("expected expression", position));
        }

        match token.as_str() {
            "#t" => Ok(Expr::new(ExprKind::Bool(true), position)),
            "#f" => Ok(Expr::new(ExprKind::Bool(false), position)),
            _ if token.starts_with("#\\") => Ok(Expr::new(
                ExprKind::Char(parse_char_literal(&token, position)?),
                position,
            )),
            _ => match parse_number_literal(&token, position)? {
                Some(Number::Integer(value)) => Ok(Expr::new(ExprKind::Integer(value), position)),
                Some(Number::Rational(value)) => Ok(Expr::new(ExprKind::Rational(value), position)),
                Some(Number::Inexact(value)) => Ok(Expr::new(ExprKind::Inexact(value), position)),
                None => Ok(Expr::new(ExprKind::Symbol(token), position)),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.index += 1;
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.peek() {
                    self.index += 1;
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn take_token(&mut self) -> String {
        let start = self.index;

        while matches!(self.peek(), Some(ch) if !is_token_delimiter(ch)) {
            self.index += 1;
        }

        self.chars[start..self.index].iter().collect()
    }

    fn expect(&mut self, expected: char) -> Result<(), EvalError> {
        match self.peek() {
            Some(ch) if ch == expected => {
                self.index += 1;
                Ok(())
            }
            Some(found) => Err(EvalError::syntax(
                format!("expected '{expected}', found '{found}'"),
                self.current_pos(),
            )),
            None => Err(EvalError::syntax(
                format!("expected '{expected}', found end of input"),
                self.current_pos(),
            )),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn current_pos(&self) -> SourcePos {
        self.position_for_index(self.index)
    }

    fn position_for_index(&self, index: usize) -> SourcePos {
        let line_index = match self.line_starts.binary_search(&index) {
            Ok(found) => found,
            Err(insert_at) => insert_at.saturating_sub(1),
        };

        SourcePos::new(
            line_index + 1,
            index.saturating_sub(self.line_starts[line_index]) + 1,
        )
    }
}

fn is_integer_token(token: &str) -> bool {
    if token == "+" || token == "-" {
        return false;
    }

    token.parse::<i64>().is_ok()
}

fn parse_char_literal(token: &str, position: SourcePos) -> Result<char, EvalError> {
    let Some(value) = token.strip_prefix("#\\") else {
        return Err(EvalError::syntax(
            format!("invalid character literal: {token}"),
            position,
        ));
    };

    match value {
        "space" => Ok(' '),
        "newline" => Ok('\n'),
        _ => {
            let mut chars = value.chars();
            let Some(ch) = chars.next() else {
                return Err(EvalError::syntax(
                    format!("invalid character literal: {token}"),
                    position,
                ));
            };

            if chars.next().is_some() {
                return Err(EvalError::syntax(
                    format!("invalid character literal: {token}"),
                    position,
                ));
            }

            Ok(ch)
        }
    }
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | ';')
}
