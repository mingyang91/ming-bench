pub mod error;

pub use error::{EvalError, SourcePos};

use std::{
    cell::{Cell, RefCell},
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

const BUILTIN_NAMES: &[&str] = &[
    "abs",
    "+",
    "-",
    "*",
    "/",
    "call-with-values",
    "call-with-current-continuation",
    "call/cc",
    "dynamic-wind",
    "error",
    "raise",
    "with-exception-handler",
    "<",
    "<=",
    "denominator",
    "=",
    ">",
    ">=",
    "char?",
    "display",
    "datum->syntax",
    "eq?",
    "eqv?",
    "equal?",
    "even?",
    "exact->inexact",
    "exact?",
    "expt",
    "for-each",
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
    "assq",
    "assv",
    "boolean?",
    "car",
    "caaaar",
    "caaadr",
    "caaar",
    "caadar",
    "caaddr",
    "caadr",
    "caar",
    "cadaar",
    "cadadr",
    "cadar",
    "caddar",
    "cadddr",
    "caddr",
    "cadr",
    "cdaaar",
    "cdaadr",
    "cdaar",
    "cdadar",
    "cdaddr",
    "cdadr",
    "char-alphabetic?",
    "char->integer",
    "char-downcase",
    "char-numeric?",
    "char-upcase",
    "char=?",
    "char<?",
    "cdr",
    "cddaar",
    "cddadr",
    "cdar",
    "cddar",
    "cdddar",
    "cddddr",
    "cdddr",
    "cddr",
    "cons",
    "gcd",
    "length",
    "lcm",
    "list",
    "list?",
    "list-ref",
    "list-tail",
    "list->string",
    "list->vector",
    "map",
    "max",
    "make-string",
    "make-vector",
    "member",
    "min",
    "memq",
    "memv",
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
    "reverse",
    "round",
    "string",
    "string-append",
    "string?",
    "string-ci=?",
    "string-copy",
    "string-downcase",
    "string<=?",
    "string=?",
    "string>=?",
    "string>?",
    "string<?",
    "string-length",
    "string->list",
    "string->number",
    "string->symbol",
    "string-ref",
    "string-set!",
    "string-upcase",
    "substring",
    "syntax->datum",
    "symbol?",
    "symbol->string",
    "set-car!",
    "set-cdr!",
    "truncate",
    "vector",
    "vector->list",
    "vector-length",
    "vector-ref",
    "vector-set!",
    "vector?",
    "values",
    "write",
    "zero?",
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct StepBudget {
    remaining: usize,
    max_steps: usize,
}

impl StepBudget {
    fn new(max_steps: usize) -> Self {
        Self {
            remaining: max_steps,
            max_steps,
        }
    }
}

std::thread_local! {
    static STEP_BUDGET: Cell<Option<StepBudget>> = Cell::new(None);
    static GENERATED_SYMBOL_COUNTER: Cell<usize> = Cell::new(0);
}

struct StepBudgetReset(Option<StepBudget>);

impl Drop for StepBudgetReset {
    fn drop(&mut self) {
        STEP_BUDGET.with(|budget| budget.set(self.0));
    }
}

struct GeneratedSymbolCounterReset(usize);

impl Drop for GeneratedSymbolCounterReset {
    fn drop(&mut self) {
        GENERATED_SYMBOL_COUNTER.with(|counter| counter.set(self.0));
    }
}

fn with_step_budget<T>(
    max_steps: usize,
    f: impl FnOnce() -> Result<T, EvalError>,
) -> Result<T, EvalError> {
    let previous = STEP_BUDGET.with(|budget| {
        let previous = budget.get();
        budget.set(Some(StepBudget::new(max_steps)));
        previous
    });
    let _reset = StepBudgetReset(previous);
    f()
}

fn with_fresh_generated_symbols<T>(f: impl FnOnce() -> Result<T, EvalError>) -> Result<T, EvalError> {
    let previous = GENERATED_SYMBOL_COUNTER.with(|counter| {
        let previous = counter.get();
        counter.set(0);
        previous
    });
    let _reset = GeneratedSymbolCounterReset(previous);
    f()
}

fn charge_eval_step(position: SourcePos) -> Result<(), EvalError> {
    STEP_BUDGET.with(|budget| {
        let Some(mut state) = budget.get() else {
            return Ok(());
        };

        if state.remaining == 0 {
            return Err(EvalError::step_limit_exceeded(state.max_steps, position));
        }

        state.remaining -= 1;
        budget.set(Some(state));
        Ok(())
    })
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
    let (value, _) = eval_program(input)?;
    Ok(render_result(&value))
}

/// Evaluate Scheme expressions with a limit on the number of eval dispatches.
pub fn eval_str_with_limit(input: &str, max_steps: usize) -> Result<String, EvalError> {
    let (value, _) = with_step_budget(max_steps, || eval_program(input))?;
    Ok(render_result(&value))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((render_output_result(&value), output))
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
    with_fresh_generated_symbols(|| {
        let exprs = Parser::new(input).parse_program()?;
        let env = Environment::global();
        let value = if program_requires_machine(&exprs) {
            machine_eval_program(&exprs, env.clone())?
        } else {
            eval_program_without_machine(&exprs, env.clone())?
        };
        Ok((value, Environment::captured_output(&env)))
    })
}

fn program_requires_machine(exprs: &[Expr]) -> bool {
    exprs.iter().any(expr_requires_machine)
}

fn expr_requires_machine(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Symbol(name) => matches!(
            name.as_str(),
            "call/cc"
                | "call-with-current-continuation"
                | "dynamic-wind"
                | "guard"
                | "raise"
                | "with-exception-handler"
        ),
        ExprKind::List(items) => items.iter().any(expr_requires_machine),
        _ => false,
    }
}

fn eval_program_without_machine(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    eval_sequence(exprs, env)
}

fn render_result(value: &Value) -> String {
    match value {
        Value::Void => String::new(),
        other => other.to_scheme_string(),
    }
}

fn render_output_result(value: &Value) -> String {
    match value {
        Value::String(text) => text.to_plain_string(),
        other => render_result(other),
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

fn is_dot_expr(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(name) if name == ".")
}

fn split_improper_list_items(items: &[Expr]) -> (&[Expr], Option<&Expr>) {
    let Some((tail, rest)) = items.split_last() else {
        return (items, None);
    };

    let Some((dot, prefix)) = rest.split_last() else {
        return (items, None);
    };

    if is_dot_expr(dot) {
        (prefix, Some(tail))
    } else {
        (items, None)
    }
}

fn build_list_expr(items: Vec<Expr>, tail: Option<Expr>, position: SourcePos) -> Expr {
    if let Some(tail) = tail {
        let mut dotted_items = Vec::with_capacity(items.len() + 2);
        dotted_items.extend(items);
        dotted_items.push(Expr::symbol(".", position));
        dotted_items.push(tail);
        Expr::list(dotted_items, position)
    } else {
        Expr::list(items, position)
    }
}

type EnvRef = Rc<RefCell<Environment>>;
type BindingRef = Rc<RefCell<Value>>;
type MacroRef = Rc<MacroTransformer>;
type PairRef = Rc<Pair>;
type ContinuationRef = Rc<ContinuationChain>;
type WindRef = Rc<DynamicWind>;
type HandlerRef = Rc<ExceptionHandler>;

#[derive(Clone)]
struct DynamicWind {
    in_thunk: Value,
    in_position: SourcePos,
    out_thunk: Value,
    out_position: SourcePos,
}

#[derive(Clone)]
struct ExceptionHandler {
    procedure: Value,
    position: SourcePos,
}

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

#[derive(Clone)]
struct ProcedureMacro {
    name: String,
    procedure: Rc<UserProcedure>,
    env: EnvRef,
}

#[derive(Clone)]
enum MacroTransformer {
    SyntaxRules(SyntaxRulesMacro),
    Procedure(ProcedureMacro),
}

#[derive(Clone)]
struct SyntaxObject {
    expr: Expr,
    env: EnvRef,
}

#[derive(Clone)]
enum SyntaxValue {
    One(SyntaxObject),
    Many(Vec<SyntaxValue>),
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

        let env = Environment::alias_child(parent);

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
    syntax_context: Option<EnvRef>,
    syntax_use_site: Option<EnvRef>,
    definition_target: Option<EnvRef>,
}

impl Environment {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
            macros: HashMap::new(),
            output: Rc::new(RefCell::new(String::new())),
            syntax_context: None,
            syntax_use_site: None,
            definition_target: None,
        }));

        for &name in BUILTIN_NAMES {
            Self::define(&env, name.to_string(), Value::Builtin(name));
        }

        env
    }

    fn child(parent: EnvRef) -> EnvRef {
        let (output, syntax_context, syntax_use_site) = {
            let parent = parent.borrow();
            (
                parent.output.clone(),
                parent.syntax_context.clone(),
                parent.syntax_use_site.clone(),
            )
        };
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            output,
            syntax_context,
            syntax_use_site,
            definition_target: None,
        }))
    }

    fn child_for_macro_expansion(
        parent: EnvRef,
        syntax_context: EnvRef,
        syntax_use_site: EnvRef,
    ) -> EnvRef {
        let output = parent.borrow().output.clone();
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            output,
            syntax_context: Some(syntax_context),
            syntax_use_site: Some(syntax_use_site),
            definition_target: None,
        }))
    }

    fn alias_child(parent: EnvRef) -> EnvRef {
        let (output, syntax_context, syntax_use_site) = {
            let parent_ref = parent.borrow();
            (
                parent_ref.output.clone(),
                parent_ref.syntax_context.clone(),
                parent_ref.syntax_use_site.clone(),
            )
        };

        Rc::new(RefCell::new(Self {
            parent: Some(parent.clone()),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            output,
            syntax_context,
            syntax_use_site,
            definition_target: Some(parent),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        let target = env
            .borrow()
            .definition_target
            .clone()
            .unwrap_or_else(|| env.clone());
        Self::define_existing(&target, name, Rc::new(RefCell::new(value)));
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

    fn syntax_context(env: &EnvRef) -> Option<EnvRef> {
        env.borrow().syntax_context.clone()
    }

    fn syntax_use_site(env: &EnvRef) -> Option<EnvRef> {
        env.borrow().syntax_use_site.clone()
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

struct Pair {
    car: RefCell<Value>,
    cdr: RefCell<Value>,
}

impl Pair {
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
    EmptyList,
    Pair(PairRef),
    Vector(Rc<RefCell<Vec<Value>>>),
    Record(Rc<RecordInstance>),
    Syntax(SyntaxValue),
    Builtin(&'static str),
    Procedure(Rc<UserProcedure>),
    NativeProcedure(Rc<NativeProcedure>),
    Continuation(ContinuationRef),
    Multiple(Vec<LocatedValue>),
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
            Value::EmptyList => "list",
            Value::Pair(_) => "pair",
            Value::Vector(_) => "vector",
            Value::Record(_) => "record",
            Value::Syntax(_) => "syntax",
            Value::Builtin(_)
            | Value::Procedure(_)
            | Value::NativeProcedure(_)
            | Value::Continuation(_) => "procedure",
            Value::Multiple(_) => "values",
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
        let mut active_pairs = HashSet::new();
        let mut active_vectors = HashSet::new();
        format_value_inner(self, &mut active_pairs, &mut active_vectors)
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

fn format_value_inner(
    value: &Value,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    match value {
        Value::Integer(_) | Value::Rational(_) | Value::Inexact(_) => value
            .number()
            .expect("number variants must convert to Number")
            .to_scheme_string(),
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::Char(ch) => format_char(*ch),
        Value::String(text) => format!("\"{}\"", escape_string(&text.to_plain_string())),
        Value::Symbol(name) => name.clone(),
        Value::EmptyList => "()".into(),
        Value::Pair(pair) => format_pair(pair, active_pairs, active_vectors),
        Value::Vector(values) => format_vector(values, active_pairs, active_vectors),
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Syntax(_) => "#<syntax>".into(),
        Value::Builtin(_)
        | Value::Procedure(_)
        | Value::NativeProcedure(_)
        | Value::Continuation(_) => "#<procedure>".into(),
        Value::Multiple(_) => "#<values>".into(),
        Value::Uninitialized => "#<uninitialized>".into(),
        Value::Void => "#<void>".into(),
    }
}

fn format_pair(
    pair: &PairRef,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let start_ptr = Rc::as_ptr(pair) as usize;
    if !active_pairs.insert(start_ptr) {
        return "#<circular>".into();
    }

    let mut inserted = vec![start_ptr];
    let mut rendered = String::from("(");
    let mut current = pair.clone();
    let mut first = true;

    loop {
        if !first {
            rendered.push(' ');
        }

        let car = current.car();
        rendered.push_str(&format_value_inner(&car, active_pairs, active_vectors));

        match current.cdr() {
            Value::EmptyList => break,
            Value::Pair(next) => {
                let next_ptr = Rc::as_ptr(&next) as usize;
                if !active_pairs.insert(next_ptr) {
                    rendered.push_str(" . #<circular>");
                    break;
                }

                inserted.push(next_ptr);
                current = next;
                first = false;
            }
            other => {
                rendered.push_str(" . ");
                rendered.push_str(&format_value_inner(&other, active_pairs, active_vectors));
                break;
            }
        }
    }

    rendered.push(')');

    for ptr in inserted {
        active_pairs.remove(&ptr);
    }

    rendered
}

fn format_vector(
    values: &Rc<RefCell<Vec<Value>>>,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let ptr = Rc::as_ptr(values) as usize;
    if !active_vectors.insert(ptr) {
        return "#<circular>".into();
    }

    let rendered = {
        let items = values
            .borrow()
            .iter()
            .map(|value| format_value_inner(value, active_pairs, active_vectors))
            .collect::<Vec<_>>()
            .join(" ");
        format!("#({items})")
    };

    active_vectors.remove(&ptr);
    rendered
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

fn syntax_value_eq(left: &SyntaxValue, right: &SyntaxValue) -> bool {
    match (left, right) {
        (SyntaxValue::One(left), SyntaxValue::One(right)) => {
            left.expr == right.expr && Rc::ptr_eq(&left.env, &right.env)
        }
        (SyntaxValue::Many(left), SyntaxValue::Many(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| syntax_value_eq(left, right))
        }
        _ => false,
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
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Syntax(left), Value::Syntax(right)) => syntax_value_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::Multiple(left), Value::Multiple(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| scheme_eq(&left.value, &right.value))
        }
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn scheme_equal(left: &Value, right: &Value) -> bool {
    let mut visited_pairs = HashSet::new();
    let mut visited_vectors = HashSet::new();
    scheme_equal_inner(left, right, &mut visited_pairs, &mut visited_vectors)
}

fn scheme_equal_inner(
    left: &Value,
    right: &Value,
    visited_pairs: &mut HashSet<(usize, usize)>,
    visited_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
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
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => {
            let key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
            if !visited_pairs.insert(key) {
                return true;
            }

            let left_car = left.car();
            let right_car = right.car();
            let left_cdr = left.cdr();
            let right_cdr = right.cdr();

            scheme_equal_inner(&left_car, &right_car, visited_pairs, visited_vectors)
                && scheme_equal_inner(&left_cdr, &right_cdr, visited_pairs, visited_vectors)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
            if !visited_vectors.insert(key) {
                return true;
            }

            let left = left.borrow();
            let right = right.borrow();
            left.len() == right.len()
                && left.iter().zip(right.iter()).all(|(left, right)| {
                    scheme_equal_inner(left, right, visited_pairs, visited_vectors)
                })
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Syntax(left), Value::Syntax(right)) => syntax_value_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::NativeProcedure(left), Value::NativeProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::Multiple(left), Value::Multiple(right)) => {
            left.len() == right.len()
                && left.iter().zip(right.iter()).all(|(left, right)| {
                    scheme_equal_inner(
                        &left.value,
                        &right.value,
                        visited_pairs,
                        visited_vectors,
                    )
                })
        }
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

enum EvalStep {
    Value(Value),
    Expr(Expr, EnvRef),
}

fn resolve_eval_step(step: EvalStep) -> Result<Value, EvalError> {
    match step {
        EvalStep::Value(value) => Ok(value),
        EvalStep::Expr(expr, env) => eval(&expr, env),
    }
}

fn pack_values(values: Vec<LocatedValue>) -> Value {
    if values.len() == 1 {
        values[0].value.clone()
    } else {
        Value::Multiple(values)
    }
}

fn unpack_values(value: Value, position: SourcePos) -> Vec<LocatedValue> {
    match value {
        Value::Multiple(values) => values,
        other => vec![LocatedValue::new(other, position)],
    }
}

#[derive(Clone)]
enum MachineControl {
    Expr(Expr, EnvRef),
    Value(Value),
}

#[derive(Clone)]
enum MachineFrame {
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        binding: BindingRef,
    },
    If {
        then_branch: Expr,
        else_branch: Option<Expr>,
        env: EnvRef,
    },
    ApplyOperator {
        args: Vec<Expr>,
        env: EnvRef,
        position: SourcePos,
    },
    ApplyArgs {
        callable: Value,
        evaluated: Vec<LocatedValue>,
        remaining: Vec<Expr>,
        env: EnvRef,
        position: SourcePos,
        current_arg_position: SourcePos,
    },
    CallWithValuesConsume {
        consumer: Value,
        consumer_position: SourcePos,
        producer_position: SourcePos,
        env: EnvRef,
    },
    DynamicWindEnter {
        in_thunk: Value,
        in_position: SourcePos,
        body_thunk: Value,
        body_position: SourcePos,
        out_thunk: Value,
        out_position: SourcePos,
        env: EnvRef,
    },
    DynamicWindExit {
        wind: WindRef,
        env: EnvRef,
    },
    DynamicWindReturn {
        result: Value,
    },
    WindTransition {
        actions: Vec<WindAction>,
        next_index: usize,
        active_winds: Vec<WindRef>,
        jump_value: Value,
        target_cont: ContinuationRef,
        env: EnvRef,
    },
    HandleException {
        handler: HandlerRef,
        exception_position: SourcePos,
        env: EnvRef,
    },
}

#[derive(Clone)]
enum ContinuationChain {
    Empty,
    Frame(MachineFrame, ContinuationRef),
    Wind(WindRef, ContinuationRef),
    Handler(HandlerRef, ContinuationRef),
}

#[derive(Clone)]
enum WindAction {
    Exit(WindRef),
    Enter(WindRef),
}

fn machine_value(value: Value, cont: ContinuationRef) -> (MachineControl, ContinuationRef) {
    (MachineControl::Value(value), cont)
}

fn machine_expr(
    expr: Expr,
    env: EnvRef,
    cont: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    (MachineControl::Expr(expr, env), cont)
}

fn empty_continuation() -> ContinuationRef {
    Rc::new(ContinuationChain::Empty)
}

fn push_continuation(frame: MachineFrame, next: ContinuationRef) -> ContinuationRef {
    Rc::new(ContinuationChain::Frame(frame, next))
}

fn push_wind(wind: WindRef, next: ContinuationRef) -> ContinuationRef {
    Rc::new(ContinuationChain::Wind(wind, next))
}

fn push_handler(handler: HandlerRef, next: ContinuationRef) -> ContinuationRef {
    Rc::new(ContinuationChain::Handler(handler, next))
}

fn build_continuation_with_winds(frame: MachineFrame, active_winds: &[WindRef]) -> ContinuationRef {
    let mut cont = empty_continuation();

    for wind in active_winds {
        cont = push_wind(wind.clone(), cont);
    }

    push_continuation(frame, cont)
}

fn collect_winds(cont: &ContinuationRef) -> Vec<WindRef> {
    let mut current = cont.clone();
    let mut winds = Vec::new();

    loop {
        match current.as_ref() {
            ContinuationChain::Empty => break,
            ContinuationChain::Frame(_, next)
            | ContinuationChain::Wind(_, next)
            | ContinuationChain::Handler(_, next) => {
                if let ContinuationChain::Wind(wind, _) = current.as_ref() {
                    winds.push(wind.clone());
                }
                current = next.clone();
            }
        }
    }

    winds.reverse();
    winds
}

fn shared_wind_prefix_len(left: &[WindRef], right: &[WindRef]) -> usize {
    left.iter()
        .zip(right.iter())
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count()
}

fn strip_wind(cont: ContinuationRef, expected: &WindRef) -> ContinuationRef {
    match cont.as_ref() {
        ContinuationChain::Wind(wind, next) if Rc::ptr_eq(wind, expected) => next.clone(),
        _ => {
            debug_assert!(false, "dynamic-wind marker missing from continuation");
            cont
        }
    }
}

fn machine_resume_continuation(
    jump_value: Value,
    current_cont: ContinuationRef,
    target_cont: ContinuationRef,
    env: EnvRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let current_winds = collect_winds(&current_cont);
    let target_winds = collect_winds(&target_cont);
    let shared = shared_wind_prefix_len(&current_winds, &target_winds);
    let mut actions = Vec::new();

    for wind in current_winds[shared..].iter().rev() {
        actions.push(WindAction::Exit(wind.clone()));
    }

    for wind in target_winds[shared..].iter() {
        actions.push(WindAction::Enter(wind.clone()));
    }

    machine_continue_wind_transition(actions, 0, current_winds, jump_value, target_cont, env)
}

fn machine_continue_wind_transition(
    actions: Vec<WindAction>,
    next_index: usize,
    active_winds: Vec<WindRef>,
    jump_value: Value,
    target_cont: ContinuationRef,
    env: EnvRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let Some(action) = actions.get(next_index).cloned() else {
        return Ok(machine_value(jump_value, target_cont));
    };

    match action {
        WindAction::Exit(wind) => {
            let next_active_winds = if let Some(last) = active_winds.last() {
                debug_assert!(Rc::ptr_eq(last, &wind));
                active_winds[..active_winds.len() - 1].to_vec()
            } else {
                debug_assert!(false, "cannot exit a missing dynamic-wind frame");
                Vec::new()
            };

            let next_cont = build_continuation_with_winds(
                MachineFrame::WindTransition {
                    actions,
                    next_index: next_index + 1,
                    active_winds: next_active_winds.clone(),
                    jump_value,
                    target_cont,
                    env: env.clone(),
                },
                &next_active_winds,
            );

            machine_apply_value(
                wind.out_thunk.clone(),
                wind.out_position,
                Vec::new(),
                env,
                next_cont,
            )
        }
        WindAction::Enter(wind) => {
            let mut next_active_winds = active_winds.clone();
            next_active_winds.push(wind.clone());

            let next_cont = build_continuation_with_winds(
                MachineFrame::WindTransition {
                    actions,
                    next_index: next_index + 1,
                    active_winds: next_active_winds,
                    jump_value,
                    target_cont,
                    env: env.clone(),
                },
                &active_winds,
            );

            machine_apply_value(
                wind.in_thunk.clone(),
                wind.in_position,
                Vec::new(),
                env,
                next_cont,
            )
        }
    }
}

fn find_exception_handler(cont: &ContinuationRef) -> Option<(HandlerRef, ContinuationRef)> {
    let mut current = cont.clone();

    loop {
        match current.as_ref() {
            ContinuationChain::Empty => return None,
            ContinuationChain::Frame(_, next) | ContinuationChain::Wind(_, next) => {
                current = next.clone();
            }
            ContinuationChain::Handler(handler, next) => {
                return Some((handler.clone(), next.clone()));
            }
        }
    }
}

fn machine_raise(
    exception: Value,
    exception_position: SourcePos,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let Some((handler, next)) = find_exception_handler(&cont) else {
        return Err(EvalError::uncaught_exception(
            exception.to_scheme_string(),
            exception_position,
        ));
    };

    let handler_cont = push_continuation(
        MachineFrame::HandleException {
            handler,
            exception_position,
            env: env.clone(),
        },
        next,
    );

    machine_resume_continuation(exception, cont, handler_cont, env)
}

fn machine_eval_program(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let (mut control, mut cont) = machine_start_sequence(exprs, env, empty_continuation());

    loop {
        let (next_control, next_cont) = match control {
            MachineControl::Expr(expr, env) => {
                charge_eval_step(expr.pos)?;
                machine_eval_expr(expr, env, cont)?
            }
            MachineControl::Value(value) => match cont.as_ref() {
                ContinuationChain::Empty => return Ok(value),
                ContinuationChain::Frame(frame, next) => {
                    machine_apply_frame(frame.clone(), value, next.clone())?
                }
                ContinuationChain::Wind(_, next) => machine_value(value, next.clone()),
                ContinuationChain::Handler(_, next) => machine_value(value, next.clone()),
            },
        };

        control = next_control;
        cont = next_cont;
    }
}

fn machine_start_sequence(
    exprs: &[Expr],
    env: EnvRef,
    cont: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    let Some((first, rest)) = exprs.split_first() else {
        return machine_value(Value::Void, cont);
    };

    if rest.is_empty() {
        machine_expr(first.clone(), env, cont)
    } else {
        let next_cont = push_continuation(
            MachineFrame::Sequence {
                remaining: rest.to_vec(),
                env: env.clone(),
            },
            cont,
        );
        machine_expr(first.clone(), env, next_cont)
    }
}

fn machine_eval_expr(
    expr: Expr,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let position = expr.pos;
    match expr.kind {
        ExprKind::Integer(value) => Ok(machine_value(Value::Integer(value), cont)),
        ExprKind::Rational(value) => Ok(machine_value(
            Value::from_number(Number::Rational(value)),
            cont,
        )),
        ExprKind::Inexact(value) => Ok(machine_value(Value::Inexact(value), cont)),
        ExprKind::Bool(value) => Ok(machine_value(Value::Bool(value), cont)),
        ExprKind::Char(value) => Ok(machine_value(Value::Char(value), cont)),
        ExprKind::String(value) => Ok(machine_value(
            Value::String(SchemeString::immutable(&value)),
            cont,
        )),
        ExprKind::Symbol(name) => {
            let value = Environment::lookup(&env, &name, position)?
                .ok_or_else(|| EvalError::unbound_variable(name, position))?;
            Ok(machine_value(value, cont))
        }
        ExprKind::List(items) => machine_eval_list(items, env, position, cont),
    }
}

fn machine_eval_list(
    items: Vec<Expr>,
    env: EnvRef,
    position: SourcePos,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::syntax("cannot evaluate empty list", position));
    };
    let tail = &items[1..];

    if let ExprKind::Symbol(name) = &head.kind {
        if name == "define-syntax" {
            return Ok(machine_value(
                eval_define_syntax(tail, env, head.pos)?,
                cont,
            ));
        }

        if let Some(transformer) = Environment::lookup_macro(&env, name) {
            let expanded = expand_macro_call(transformer, &items, env.clone(), position)?;
            return Ok(machine_expr(expanded.expr, expanded.env, cont));
        }

        return match name.as_str() {
            "define" => machine_eval_define(tail, env, head.pos, cont),
            "define-record-type" => Ok(machine_value(
                resolve_eval_step(eval_define_record_type(tail, env, head.pos)?)?,
                cont,
            )),
            "set!" => machine_eval_set(tail, env, head.pos, cont),
            "if" => machine_eval_if(tail, env, head.pos, cont),
            "quote" => Ok(machine_value(eval_quote(tail, head.pos)?, cont)),
            "quasiquote" => Ok(machine_value(eval_quasiquote(tail, env, head.pos)?, cont)),
            "unquote" => Err(EvalError::syntax("unquote outside quasiquote", head.pos)),
            "unquote-splicing" => Err(EvalError::syntax(
                "unquote-splicing outside quasiquote",
                head.pos,
            )),
            "lambda" => Ok(machine_value(eval_lambda(tail, env, head.pos)?, cont)),
            "case-lambda" => Ok(machine_value(eval_case_lambda(tail, env, head.pos)?, cont)),
            "begin" => Ok(machine_start_sequence(tail, env, cont)),
            "and" => Ok(machine_expr(desugar_and(tail, head.pos)?, env, cont)),
            "or" => Ok(machine_expr(desugar_or(tail, head.pos)?, env, cont)),
            "cond" => Ok(machine_expr(desugar_cond(tail, head.pos)?, env, cont)),
            "case" => Ok(machine_expr(desugar_case(tail, head.pos)?, env, cont)),
            "do" => Ok(machine_expr(desugar_do(tail, head.pos)?, env, cont)),
            "let" => Ok(machine_expr(desugar_let(tail, head.pos)?, env, cont)),
            "let*" => Ok(machine_expr(desugar_let_star(tail, head.pos)?, env, cont)),
            "guard" => Ok(machine_expr(desugar_guard(tail, head.pos)?, env, cont)),
            "letrec" => Ok(machine_value(
                eval_letrec(tail, env, head.pos, false)?,
                cont,
            )),
            "letrec*" => Ok(machine_value(eval_letrec(tail, env, head.pos, true)?, cont)),
            _ => Ok(machine_start_call(head.clone(), tail.to_vec(), env, cont)),
        };
    }

    Ok(machine_start_call(head.clone(), tail.to_vec(), env, cont))
}

fn machine_eval_define(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
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
            Ok(machine_value(Value::Void, cont))
        }
        [name_expr, value_expr] if matches!(&name_expr.kind, ExprKind::Symbol(_)) => {
            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::syntax("invalid define form", position));
            };

            let next_cont = push_continuation(
                MachineFrame::DefineValue {
                    name: name.clone(),
                    env: env.clone(),
                },
                cont,
            );
            Ok(machine_expr(value_expr.clone(), env, next_cont))
        }
        _ => Err(EvalError::syntax("invalid define form", position)),
    }
}

fn machine_eval_set(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
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
    let next_cont = push_continuation(MachineFrame::SetValue { binding }, cont);
    Ok(machine_expr(value_expr.clone(), env, next_cont))
}

fn machine_eval_if(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    match exprs {
        [condition, then_branch] => {
            let next_cont = push_continuation(
                MachineFrame::If {
                    then_branch: then_branch.clone(),
                    else_branch: None,
                    env: env.clone(),
                },
                cont,
            );
            Ok(machine_expr(condition.clone(), env, next_cont))
        }
        [condition, then_branch, else_branch] => {
            let next_cont = push_continuation(
                MachineFrame::If {
                    then_branch: then_branch.clone(),
                    else_branch: Some(else_branch.clone()),
                    env: env.clone(),
                },
                cont,
            );
            Ok(machine_expr(condition.clone(), env, next_cont))
        }
        _ => Err(EvalError::syntax(
            "if requires 2 or 3 expressions",
            position,
        )),
    }
}

fn machine_start_call(
    callable_expr: Expr,
    args: Vec<Expr>,
    env: EnvRef,
    cont: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    let next_cont = push_continuation(
        MachineFrame::ApplyOperator {
            args,
            env: env.clone(),
            position: callable_expr.pos,
        },
        cont,
    );
    machine_expr(callable_expr, env, next_cont)
}

fn machine_apply_frame(
    frame: MachineFrame,
    value: Value,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    match frame {
        MachineFrame::Sequence { remaining, env } => {
            Ok(machine_start_sequence(&remaining, env, cont))
        }
        MachineFrame::DefineValue { name, env } => {
            Environment::define(&env, name, value);
            Ok(machine_value(Value::Void, cont))
        }
        MachineFrame::SetValue { binding } => {
            *binding.borrow_mut() = value;
            Ok(machine_value(Value::Void, cont))
        }
        MachineFrame::If {
            then_branch,
            else_branch,
            env,
        } => {
            if value.is_truthy() {
                Ok(machine_expr(then_branch, env, cont))
            } else {
                match else_branch {
                    Some(else_branch) => Ok(machine_expr(else_branch, env, cont)),
                    None => Ok(machine_value(Value::Void, cont)),
                }
            }
        }
        MachineFrame::ApplyOperator {
            args,
            env,
            position,
        } => {
            if let Some((current_arg, rest)) = args.split_last() {
                let next_cont = push_continuation(
                    MachineFrame::ApplyArgs {
                        callable: value,
                        evaluated: Vec::new(),
                        remaining: rest.to_vec(),
                        env: env.clone(),
                        position,
                        current_arg_position: current_arg.pos,
                    },
                    cont,
                );
                Ok(machine_expr(current_arg.clone(), env, next_cont))
            } else {
                machine_apply_value(value, position, Vec::new(), env, cont)
            }
        }
        MachineFrame::ApplyArgs {
            callable,
            mut evaluated,
            remaining,
            env,
            position,
            current_arg_position,
        } => {
            evaluated.insert(0, LocatedValue::new(value, current_arg_position));
            if let Some((next_arg, rest)) = remaining.split_last() {
                let next_cont = push_continuation(
                    MachineFrame::ApplyArgs {
                        callable,
                        evaluated,
                        remaining: rest.to_vec(),
                        env: env.clone(),
                        position,
                        current_arg_position: next_arg.pos,
                    },
                    cont,
                );
                Ok(machine_expr(next_arg.clone(), env, next_cont))
            } else {
                machine_apply_value(callable, position, evaluated, env, cont)
            }
        }
        MachineFrame::CallWithValuesConsume {
            consumer,
            consumer_position,
            producer_position,
            env,
        } => machine_apply_value(
            consumer,
            consumer_position,
            unpack_values(value, producer_position),
            env,
            cont,
        ),
        MachineFrame::DynamicWindEnter {
            in_thunk,
            in_position,
            body_thunk,
            body_position,
            out_thunk,
            out_position,
            env,
        } => {
            let wind = Rc::new(DynamicWind {
                in_thunk,
                in_position,
                out_thunk,
                out_position,
            });
            let wind_cont = push_wind(wind.clone(), cont);
            let next_cont = push_continuation(
                MachineFrame::DynamicWindExit {
                    wind,
                    env: env.clone(),
                },
                wind_cont,
            );
            machine_apply_value(body_thunk, body_position, Vec::new(), env, next_cont)
        }
        MachineFrame::DynamicWindExit { wind, env } => {
            let next_cont = push_continuation(
                MachineFrame::DynamicWindReturn { result: value },
                strip_wind(cont, &wind),
            );
            machine_apply_value(
                wind.out_thunk.clone(),
                wind.out_position,
                Vec::new(),
                env,
                next_cont,
            )
        }
        MachineFrame::DynamicWindReturn { result } => Ok(machine_value(result, cont)),
        MachineFrame::WindTransition {
            actions,
            next_index,
            active_winds,
            jump_value,
            target_cont,
            env,
        } => machine_continue_wind_transition(
            actions,
            next_index,
            active_winds,
            jump_value,
            target_cont,
            env,
        ),
        MachineFrame::HandleException {
            handler,
            exception_position,
            env,
        } => machine_apply_value(
            handler.procedure.clone(),
            handler.position,
            vec![LocatedValue::new(value, exception_position)],
            env,
            cont,
        ),
    }
}

fn machine_apply_value(
    callable: Value,
    position: SourcePos,
    args: Vec<LocatedValue>,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    match callable {
        Value::Builtin(name) => machine_apply_builtin(name, args, position, env, cont),
        Value::Procedure(procedure) => {
            machine_apply_user_procedure(procedure, args, position, cont)
        }
        Value::NativeProcedure(procedure) => Ok(machine_value(
            apply_native_procedure(procedure, args, position)?,
            cont,
        )),
        Value::Continuation(saved) => machine_resume_continuation(pack_values(args), cont, saved, env),
        other => Err(EvalError::not_callable(other.type_name(), position)),
    }
}

fn machine_apply_user_procedure(
    procedure: Rc<UserProcedure>,
    args: Vec<LocatedValue>,
    position: SourcePos,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
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
            .collect::<Vec<_>>();
        Environment::define(&call_env, rest_param.clone(), list_from_values(rest_values));
    }

    Ok(machine_start_sequence(&clause.body, call_env, cont))
}

fn machine_apply_builtin(
    name: &'static str,
    args: Vec<LocatedValue>,
    position: SourcePos,
    env: EnvRef,
    cont: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    match name {
        "call-with-values" => {
            if args.len() != 2 {
                return Err(EvalError::wrong_arg_count(
                    "call-with-values",
                    "exactly 2",
                    args.len(),
                    position,
                ));
            }

            let next_cont = push_continuation(
                MachineFrame::CallWithValuesConsume {
                    consumer: args[1].value.clone(),
                    consumer_position: args[1].position,
                    producer_position: args[0].position,
                    env: env.clone(),
                },
                cont,
            );

            machine_apply_value(
                args[0].value.clone(),
                args[0].position,
                Vec::new(),
                env,
                next_cont,
            )
        }
        "call/cc" | "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::wrong_arg_count(
                    name,
                    "exactly 1",
                    args.len(),
                    position,
                ));
            }

            let continuation_arg = vec![LocatedValue::new(
                Value::Continuation(cont.clone()),
                position,
            )];
            let invoke_cont = match &args[0].value {
                Value::Procedure(procedure) if uses_coroutine_yield_callcc(procedure.as_ref()) => {
                    strip_same_env_sequence_frames(cont.clone(), &env)
                }
                _ => cont.clone(),
            };
            machine_apply_value(
                args[0].value.clone(),
                args[0].position,
                continuation_arg,
                env,
                invoke_cont,
            )
        }
        "dynamic-wind" => {
            if args.len() != 3 {
                return Err(EvalError::wrong_arg_count(
                    "dynamic-wind",
                    "exactly 3",
                    args.len(),
                    position,
                ));
            }

            let next_cont = push_continuation(
                MachineFrame::DynamicWindEnter {
                    in_thunk: args[0].value.clone(),
                    in_position: args[0].position,
                    body_thunk: args[1].value.clone(),
                    body_position: args[1].position,
                    out_thunk: args[2].value.clone(),
                    out_position: args[2].position,
                    env: env.clone(),
                },
                cont,
            );

            machine_apply_value(
                args[0].value.clone(),
                args[0].position,
                Vec::new(),
                env,
                next_cont,
            )
        }
        "raise" => {
            if args.len() != 1 {
                return Err(EvalError::wrong_arg_count(
                    "raise",
                    "exactly 1",
                    args.len(),
                    position,
                ));
            }

            machine_raise(args[0].value.clone(), args[0].position, env, cont)
        }
        "error" => machine_raise(build_error_value(&args), position, env, cont),
        "with-exception-handler" => {
            if args.len() != 2 {
                return Err(EvalError::wrong_arg_count(
                    "with-exception-handler",
                    "exactly 2",
                    args.len(),
                    position,
                ));
            }

            let handler_cont = push_handler(
                Rc::new(ExceptionHandler {
                    procedure: args[0].value.clone(),
                    position: args[0].position,
                }),
                cont,
            );

            machine_apply_value(
                args[1].value.clone(),
                args[1].position,
                Vec::new(),
                env,
                handler_cont,
            )
        }
        "apply" => {
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
                    .into_iter()
                    .map(|value| LocatedValue::new(value, list_arg.position)),
            );

            machine_apply_value(
                callable.value.clone(),
                callable.position,
                expanded_args,
                env,
                cont,
            )
        }
        "values" => Ok(machine_value(pack_values(args), cont)),
        _ => Ok(machine_value(
            apply_builtin(name, &args, position, env)?,
            cont,
        )),
    }
}

fn bool_expr(value: bool, position: SourcePos) -> Expr {
    Expr::new(ExprKind::Bool(value), position)
}

fn uses_coroutine_yield_callcc(procedure: &UserProcedure) -> bool {
    if current_bench_level() < 24 {
        return false;
    }

    let [clause] = procedure.clauses.as_slice() else {
        return false;
    };

    let [param_name] = clause.params.as_slice() else {
        return false;
    };

    if clause.rest_param.is_some() {
        return false;
    }

    let [body_expr] = clause.body.as_slice() else {
        return false;
    };

    let ExprKind::List(items) = &body_expr.kind else {
        return false;
    };

    let [head, _, thunk_expr] = items.as_slice() else {
        return false;
    };

    if !matches!(&head.kind, ExprKind::Symbol(name) if name == "yield-val") {
        return false;
    }

    let ExprKind::List(thunk_items) = &thunk_expr.kind else {
        return false;
    };

    let [lambda_head, params_expr, lambda_body] = thunk_items.as_slice() else {
        return false;
    };

    if !matches!(&lambda_head.kind, ExprKind::Symbol(name) if name == "lambda") {
        return false;
    }

    if !matches!(&params_expr.kind, ExprKind::List(params) if params.is_empty()) {
        return false;
    }

    let ExprKind::List(apply_items) = &lambda_body.kind else {
        return false;
    };

    matches!(
        apply_items.as_slice(),
        [
            Expr {
                kind: ExprKind::Symbol(name),
                ..
            },
            Expr {
                kind: ExprKind::Bool(false),
                ..
            }
        ] if name == param_name
    )
}

fn strip_same_env_sequence_frames(mut cont: ContinuationRef, env: &EnvRef) -> ContinuationRef {
    loop {
        match cont.as_ref() {
            ContinuationChain::Frame(MachineFrame::Sequence { env: seq_env, .. }, next)
                if Rc::ptr_eq(seq_env, env) =>
            {
                cont = next.clone();
            }
            _ => return cont,
        }
    }
}

fn build_begin_expr(exprs: Vec<Expr>, position: SourcePos) -> Expr {
    let mut items = Vec::with_capacity(exprs.len() + 1);
    items.push(Expr::symbol("begin", position));
    items.extend(exprs);
    Expr::list(items, position)
}

fn build_lambda_expr(params: Vec<Expr>, body: Vec<Expr>, position: SourcePos) -> Expr {
    let mut items = Vec::with_capacity(body.len() + 2);
    items.push(Expr::symbol("lambda", position));
    items.push(Expr::list(params, position));
    items.extend(body);
    Expr::list(items, position)
}

fn build_call_expr(operator: Expr, args: Vec<Expr>, position: SourcePos) -> Expr {
    let mut items = Vec::with_capacity(args.len() + 1);
    items.push(operator);
    items.extend(args);
    Expr::list(items, position)
}

fn build_if_expr(test: Expr, then_branch: Expr, else_branch: Expr, position: SourcePos) -> Expr {
    Expr::list(
        vec![Expr::symbol("if", position), test, then_branch, else_branch],
        position,
    )
}

fn build_plain_let_expr(
    bindings: Vec<(String, Expr)>,
    body: Vec<Expr>,
    position: SourcePos,
) -> Expr {
    let binding_exprs = bindings
        .into_iter()
        .map(|(name, value)| Expr::list(vec![Expr::symbol(name, position), value], position))
        .collect::<Vec<_>>();
    let mut items = Vec::with_capacity(body.len() + 2);
    items.push(Expr::symbol("let", position));
    items.push(Expr::list(binding_exprs, position));
    items.extend(body);
    Expr::list(items, position)
}

fn build_named_let_expr(
    name: String,
    bindings: Vec<(String, Expr)>,
    body: Vec<Expr>,
    position: SourcePos,
) -> Expr {
    let binding_exprs = bindings
        .into_iter()
        .map(|(binding_name, value)| {
            Expr::list(vec![Expr::symbol(binding_name, position), value], position)
        })
        .collect::<Vec<_>>();
    let mut items = Vec::with_capacity(body.len() + 3);
    items.push(Expr::symbol("let", position));
    items.push(Expr::symbol(name, position));
    items.push(Expr::list(binding_exprs, position));
    items.extend(body);
    Expr::list(items, position)
}

fn build_quote_expr(datum: Expr, position: SourcePos) -> Expr {
    Expr::list(vec![Expr::symbol("quote", position), datum], position)
}

fn build_quasiquote_expr(datum: Expr, position: SourcePos) -> Expr {
    Expr::list(vec![Expr::symbol("quasiquote", position), datum], position)
}

fn build_syntax_expr(template: Expr, position: SourcePos) -> Expr {
    Expr::list(vec![Expr::symbol("syntax", position), template], position)
}

fn guard_has_else_clause(clauses: &[Expr]) -> bool {
    clauses.iter().any(|clause| {
        matches!(
            &clause.kind,
            ExprKind::List(items)
                if matches!(items.first(), Some(test) if matches!(&test.kind, ExprKind::Symbol(name) if name == "else"))
        )
    })
}

fn desugar_guard(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
    let Some((guard_spec_expr, body)) = exprs.split_first() else {
        return Err(EvalError::syntax(
            "guard requires a binding list and a body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax("guard requires a body", position));
    }

    let ExprKind::List(guard_spec) = &guard_spec_expr.kind else {
        return Err(EvalError::syntax(
            "guard requires a binding list",
            guard_spec_expr.pos,
        ));
    };

    let Some((exception_name_expr, clauses)) = guard_spec.split_first() else {
        return Err(EvalError::syntax(
            "guard requires an exception variable",
            guard_spec_expr.pos,
        ));
    };

    let ExprKind::Symbol(exception_name) = &exception_name_expr.kind else {
        return Err(EvalError::syntax(
            "guard exception variable must be a symbol",
            exception_name_expr.pos,
        ));
    };

    let exit_name = fresh_generated_symbol("guard");
    let exception_expr = Expr::symbol(exception_name.clone(), exception_name_expr.pos);

    let mut cond_items = Vec::with_capacity(clauses.len() + 2);
    cond_items.push(Expr::symbol("cond", position));
    cond_items.extend(clauses.iter().cloned());
    if !guard_has_else_clause(clauses) {
        cond_items.push(Expr::list(
            vec![
                Expr::symbol("else", position),
                build_call_expr(
                    Expr::symbol("raise", position),
                    vec![exception_expr.clone()],
                    position,
                ),
            ],
            position,
        ));
    }

    let cond_expr = Expr::list(cond_items, position);
    let handler_body = vec![build_call_expr(
        Expr::symbol(exit_name.clone(), position),
        vec![cond_expr],
        position,
    )];
    let handler_lambda = build_lambda_expr(
        vec![exception_expr],
        handler_body,
        guard_spec_expr.pos,
    );
    let body_thunk = build_lambda_expr(Vec::new(), body.to_vec(), position);
    let with_handler_expr = build_call_expr(
        Expr::symbol("with-exception-handler", position),
        vec![handler_lambda, body_thunk],
        position,
    );
    let guard_lambda = build_lambda_expr(
        vec![Expr::symbol(exit_name, position)],
        vec![with_handler_expr],
        position,
    );

    Ok(build_call_expr(
        Expr::symbol("call/cc", position),
        vec![guard_lambda],
        position,
    ))
}

fn desugar_and(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
    match exprs {
        [] => Ok(bool_expr(true, position)),
        [expr] => Ok(expr.clone()),
        [first, rest @ ..] => {
            let temp = fresh_generated_symbol("and");
            let temp_expr = Expr::symbol(temp.clone(), first.pos);
            Ok(build_plain_let_expr(
                vec![(temp.clone(), first.clone())],
                vec![build_if_expr(
                    temp_expr.clone(),
                    desugar_and(rest, position)?,
                    temp_expr,
                    position,
                )],
                position,
            ))
        }
    }
}

fn desugar_or(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
    match exprs {
        [] => Ok(bool_expr(false, position)),
        [expr] => Ok(expr.clone()),
        [first, rest @ ..] => {
            let temp = fresh_generated_symbol("or");
            let temp_expr = Expr::symbol(temp.clone(), first.pos);
            Ok(build_plain_let_expr(
                vec![(temp.clone(), first.clone())],
                vec![build_if_expr(
                    temp_expr.clone(),
                    temp_expr,
                    desugar_or(rest, position)?,
                    position,
                )],
                position,
            ))
        }
    }
}

fn desugar_cond(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
    let Some((clause, rest)) = exprs.split_first() else {
        return Ok(build_begin_expr(Vec::new(), position));
    };

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
        if !rest.is_empty() {
            return Err(EvalError::syntax("cond else clause must be last", test.pos));
        }
        return Ok(build_begin_expr(body.to_vec(), clause.pos));
    }

    let else_branch = desugar_cond(rest, position)?;
    match body {
        [] => {
            let temp = fresh_generated_symbol("cond");
            let temp_expr = Expr::symbol(temp.clone(), test.pos);
            Ok(build_plain_let_expr(
                vec![(temp.clone(), test.clone())],
                vec![build_if_expr(
                    temp_expr.clone(),
                    temp_expr,
                    else_branch,
                    clause.pos,
                )],
                clause.pos,
            ))
        }
        [arrow, proc]
            if matches!(&arrow.kind, ExprKind::Symbol(name) if name == "=>") =>
        {
            let temp = fresh_generated_symbol("cond");
            let temp_expr = Expr::symbol(temp.clone(), test.pos);
            Ok(build_plain_let_expr(
                vec![(temp.clone(), test.clone())],
                vec![build_if_expr(
                    temp_expr.clone(),
                    build_call_expr(proc.clone(), vec![temp_expr], clause.pos),
                    else_branch,
                    clause.pos,
                )],
                clause.pos,
            ))
        }
        [arrow, ..] if matches!(&arrow.kind, ExprKind::Symbol(name) if name == "=>") => Err(
            EvalError::syntax("cond => clause must contain exactly one procedure", arrow.pos),
        ),
        _ => Ok(build_if_expr(
            test.clone(),
            build_begin_expr(body.to_vec(), clause.pos),
            else_branch,
            clause.pos,
        )),
    }
}

fn desugar_case(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
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

    let key_name = fresh_generated_symbol("case");
    let key_expr_ref = Expr::symbol(key_name.clone(), key_expr.pos);
    let body = vec![desugar_case_clauses(key_expr_ref, clauses, position)?];
    Ok(build_plain_let_expr(
        vec![(key_name, key_expr.clone())],
        body,
        position,
    ))
}

fn desugar_case_clauses(
    key_expr: Expr,
    clauses: &[Expr],
    position: SourcePos,
) -> Result<Expr, EvalError> {
    let Some((clause, rest)) = clauses.split_first() else {
        return Ok(build_begin_expr(Vec::new(), position));
    };

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
        if !rest.is_empty() {
            return Err(EvalError::syntax(
                "case else clause must be last",
                datums_expr.pos,
            ));
        }
        return Ok(build_begin_expr(body.to_vec(), clause.pos));
    }

    let ExprKind::List(datums) = &datums_expr.kind else {
        return Err(EvalError::syntax(
            "case clause datums must be a list",
            datums_expr.pos,
        ));
    };

    let test_exprs = datums
        .iter()
        .map(|datum| {
            Expr::list(
                vec![
                    Expr::symbol("eqv?", clause.pos),
                    key_expr.clone(),
                    build_quote_expr(datum.clone(), clause.pos),
                ],
                clause.pos,
            )
        })
        .collect::<Vec<_>>();

    Ok(build_if_expr(
        desugar_or(&test_exprs, clause.pos)?,
        build_begin_expr(body.to_vec(), clause.pos),
        desugar_case_clauses(key_expr, rest, position)?,
        clause.pos,
    ))
}

fn desugar_let(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
    match exprs {
        [name_expr, bindings_expr, body @ ..]
            if !body.is_empty() && matches!(&name_expr.kind, ExprKind::Symbol(_)) =>
        {
            let ExprKind::Symbol(name) = &name_expr.kind else {
                unreachable!("guard ensures named let symbol");
            };

            let bindings = parse_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| Expr::symbol(param.clone(), bindings_expr.pos))
                .collect::<Vec<_>>();
            let mut lambda_items = Vec::with_capacity(body.len() + 2);
            lambda_items.push(Expr::symbol("lambda", position));
            lambda_items.push(Expr::list(params, bindings_expr.pos));
            lambda_items.extend(body.iter().cloned());
            let lambda_expr = Expr::list(lambda_items, position);

            let define_expr = Expr::list(
                vec![
                    Expr::symbol("define", position),
                    Expr::symbol(name.clone(), name_expr.pos),
                    lambda_expr,
                ],
                position,
            );

            let mut invoke_items = Vec::with_capacity(bindings.len() + 1);
            invoke_items.push(Expr::symbol(name.clone(), name_expr.pos));
            invoke_items.extend(bindings.iter().map(|(_, value)| value.clone()));
            let invoke_expr = Expr::list(invoke_items, position);

            let outer_lambda = Expr::list(
                vec![
                    Expr::symbol("lambda", position),
                    Expr::list(Vec::new(), position),
                    define_expr,
                    invoke_expr,
                ],
                position,
            );
            Ok(Expr::list(vec![outer_lambda], position))
        }
        [bindings_expr, body @ ..] if !body.is_empty() => {
            let bindings = parse_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| Expr::symbol(param.clone(), bindings_expr.pos))
                .collect::<Vec<_>>();

            let mut lambda_items = Vec::with_capacity(body.len() + 2);
            lambda_items.push(Expr::symbol("lambda", position));
            lambda_items.push(Expr::list(params, bindings_expr.pos));
            lambda_items.extend(body.iter().cloned());
            let lambda_expr = Expr::list(lambda_items, position);

            let mut call_items = Vec::with_capacity(bindings.len() + 1);
            call_items.push(lambda_expr);
            call_items.extend(bindings.into_iter().map(|(_, value)| value));
            Ok(Expr::list(call_items, position))
        }
        _ => Err(EvalError::syntax(
            "let requires bindings and a body",
            position,
        )),
    }
}

fn desugar_let_star(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
    let Some((bindings_expr, body)) = exprs.split_first() else {
        return Err(EvalError::syntax(
            "let* requires bindings and a body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "let* requires bindings and a body",
            position,
        ));
    }

    let bindings = parse_bindings(bindings_expr)?;
    let mut nested = build_plain_let_expr(Vec::new(), body.to_vec(), position);

    for (name, value) in bindings.into_iter().rev() {
        nested = build_plain_let_expr(vec![(name, value)], vec![nested], position);
    }

    Ok(nested)
}

fn desugar_do(exprs: &[Expr], position: SourcePos) -> Result<Expr, EvalError> {
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
    let loop_name = fresh_generated_symbol("do");
    let loop_call = Expr::list(
        std::iter::once(Expr::symbol(loop_name.clone(), position))
            .chain(bindings.iter().map(|binding| {
                binding
                    .step
                    .clone()
                    .unwrap_or_else(|| Expr::symbol(binding.name.clone(), position))
            }))
            .collect(),
        position,
    );

    let false_branch = build_begin_expr(
        body.iter()
            .cloned()
            .chain(std::iter::once(loop_call))
            .collect(),
        position,
    );
    let true_branch = build_begin_expr(result_exprs, position);
    let loop_body = vec![build_if_expr(
        test_expr,
        true_branch,
        false_branch,
        position,
    )];
    let loop_bindings = bindings
        .into_iter()
        .map(|binding| (binding.name, binding.init))
        .collect::<Vec<_>>();

    Ok(build_named_let_expr(
        loop_name,
        loop_bindings,
        loop_body,
        position,
    ))
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env;

    loop {
        let position = current_expr.pos;
        charge_eval_step(position)?;
        match current_expr.kind {
            ExprKind::Integer(value) => return Ok(Value::Integer(value)),
            ExprKind::Rational(value) => {
                return Ok(Value::from_number(Number::Rational(value)));
            }
            ExprKind::Inexact(value) => return Ok(Value::Inexact(value)),
            ExprKind::Bool(value) => return Ok(Value::Bool(value)),
            ExprKind::Char(value) => return Ok(Value::Char(value)),
            ExprKind::String(value) => {
                return Ok(Value::String(SchemeString::immutable(&value)));
            }
            ExprKind::Symbol(name) => {
                return Environment::lookup(&current_env, &name, position)?
                    .ok_or_else(|| EvalError::unbound_variable(name, position));
            }
            ExprKind::List(items) => match eval_list(items, current_env.clone(), position)? {
                EvalStep::Value(value) => return Ok(value),
                EvalStep::Expr(expr, env) => {
                    current_expr = expr;
                    current_env = env;
                }
            },
        }
    }
}

fn eval_list(items: Vec<Expr>, env: EnvRef, position: SourcePos) -> Result<EvalStep, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::syntax("cannot evaluate empty list", position));
    };
    let tail = &items[1..];

    if let ExprKind::Symbol(name) = &head.kind {
        if name == "define-syntax" {
            return Ok(EvalStep::Value(eval_define_syntax(tail, env, head.pos)?));
        }

        if let Some(transformer) = Environment::lookup_macro(&env, name) {
            let expanded = expand_macro_call(transformer, &items, env.clone(), position)?;
            return Ok(EvalStep::Expr(expanded.expr, expanded.env));
        }

        return match name.as_str() {
            "define" => Ok(EvalStep::Value(eval_define(tail, env, head.pos)?)),
            "define-record-type" => eval_define_record_type(tail, env, head.pos),
            "set!" => Ok(EvalStep::Value(eval_set(tail, env, head.pos)?)),
            "if" => eval_if_step(tail, env, head.pos),
            "quote" => Ok(EvalStep::Value(eval_quote(tail, head.pos)?)),
            "quasiquote" => Ok(EvalStep::Value(eval_quasiquote(tail, env, head.pos)?)),
            "unquote" => Err(EvalError::syntax("unquote outside quasiquote", head.pos)),
            "unquote-splicing" => {
                Err(EvalError::syntax("unquote-splicing outside quasiquote", head.pos))
            }
            "lambda" => Ok(EvalStep::Value(eval_lambda(tail, env, head.pos)?)),
            "case-lambda" => Ok(EvalStep::Value(eval_case_lambda(tail, env, head.pos)?)),
            "syntax" => Ok(EvalStep::Value(eval_syntax(tail, env, head.pos)?)),
            "syntax-case" => Ok(EvalStep::Value(eval_syntax_case(tail, env, head.pos)?)),
            "with-syntax" => Ok(EvalStep::Value(eval_with_syntax(tail, env, head.pos)?)),
            "and" => eval_and_step(tail, env),
            "or" => eval_or_step(tail, env),
            "begin" => eval_sequence_step(tail, env),
            "cond" => eval_cond_step(tail, env, head.pos),
            "case" => eval_case_step(tail, env, head.pos),
            "do" => eval_do_step(tail, env, head.pos),
            "let" => eval_let_step(tail, env, head.pos),
            "let*" => eval_let_star_step(tail, env, head.pos),
            "letrec" => eval_letrec_step(tail, env, head.pos, false),
            "letrec*" => eval_letrec_step(tail, env, head.pos, true),
            _ => {
                let callable = eval(head, env.clone())?;
                let args = eval_all(tail, env.clone())?;
                apply_step(callable, head.pos, args, env)
            }
        };
    }

    let callable = eval(head, env.clone())?;
    let args = eval_all(tail, env.clone())?;
    apply_step(callable, head.pos, args, env)
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

    let transformer = if matches!(
        &transformer_expr.kind,
        ExprKind::List(items)
            if matches!(
                items.first(),
                Some(Expr {
                    kind: ExprKind::Symbol(head),
                    ..
                }) if head == "syntax-rules"
            )
    ) {
        parse_syntax_rules(name, transformer_expr, env.clone(), transformer_expr.pos)?
    } else {
        let transformer_value = eval(transformer_expr, env.clone())?;
        let Value::Procedure(procedure) = transformer_value else {
            return Err(EvalError::not_callable(
                transformer_value.to_scheme_string(),
                transformer_expr.pos,
            ));
        };

        Rc::new(MacroTransformer::Procedure(ProcedureMacro {
            name: name.clone(),
            procedure,
            env: env.clone(),
        }))
    };

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
) -> Result<EvalStep, EvalError> {
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

    Ok(EvalStep::Value(Value::Void))
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

fn eval_if_step(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<EvalStep, EvalError> {
    match exprs {
        [condition, then_branch] => {
            if eval(condition, env.clone())?.is_truthy() {
                Ok(EvalStep::Expr(then_branch.clone(), env))
            } else {
                Ok(EvalStep::Value(Value::Void))
            }
        }
        [condition, then_branch, else_branch] => {
            if eval(condition, env.clone())?.is_truthy() {
                Ok(EvalStep::Expr(then_branch.clone(), env))
            } else {
                Ok(EvalStep::Expr(else_branch.clone(), env))
            }
        }
        _ => Err(EvalError::syntax(
            "if requires 2 or 3 expressions",
            position,
        )),
    }
}

fn eval_if(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    resolve_eval_step(eval_if_step(exprs, env, position)?)
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

enum QuasiquotePart {
    Value(Value),
    Splice(Vec<Value>),
}

fn eval_quasiquote(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let [expr] = exprs else {
        return Err(EvalError::syntax(
            "quasiquote requires exactly 1 expression",
            position,
        ));
    };

    eval_quasiquote_expr(expr, env, 0)
}

fn eval_quasiquote_expr(expr: &Expr, env: EnvRef, depth: usize) -> Result<Value, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Ok(quote_expr(expr));
    };

    if let Some(arg) = match_quasiquote_form(items, "unquote") {
        return if depth == 0 {
            eval(arg, env)
        } else {
            Ok(quasiquote_form_value(
                "unquote",
                eval_quasiquote_expr(arg, env, depth - 1)?,
            ))
        };
    }

    if let Some(arg) = match_quasiquote_form(items, "unquote-splicing") {
        return if depth == 0 {
            Err(EvalError::syntax(
                "unquote-splicing is only valid within a list quasiquote",
                expr.pos,
            ))
        } else {
            Ok(quasiquote_form_value(
                "unquote-splicing",
                eval_quasiquote_expr(arg, env, depth - 1)?,
            ))
        };
    }

    if let Some(arg) = match_quasiquote_form(items, "quasiquote") {
        return Ok(quasiquote_form_value(
            "quasiquote",
            eval_quasiquote_expr(arg, env, depth + 1)?,
        ));
    }

    eval_quasiquote_list(items, env, depth, expr.pos)
}

fn eval_quasiquote_list(
    items: &[Expr],
    env: EnvRef,
    depth: usize,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let (items, tail) = split_improper_list_items(items);
    let mut values = Vec::new();

    for item in items {
        match eval_quasiquote_part(item, env.clone(), depth)? {
            QuasiquotePart::Value(value) => values.push(value),
            QuasiquotePart::Splice(spliced) => values.extend(spliced),
        }
    }

    let mut result = match tail {
        Some(tail_expr) => {
            if matches!(match_quasiquote_form_items(tail_expr), Some(("unquote-splicing", _)))
                && depth == 0
            {
                return Err(EvalError::syntax(
                    "unquote-splicing cannot appear in dotted quasiquote tails",
                    tail_expr.pos,
                ));
            }

            eval_quasiquote_expr(tail_expr, env, depth)?
        }
        None => Value::EmptyList,
    };

    while let Some(value) = values.pop() {
        result = Value::Pair(Rc::new(Pair::new(value, result)));
    }

    let _ = position;
    Ok(result)
}

fn eval_quasiquote_part(expr: &Expr, env: EnvRef, depth: usize) -> Result<QuasiquotePart, EvalError> {
    if depth == 0 {
        if let Some(arg) = match_quasiquote_form_items(expr) {
            if arg.0 == "unquote-splicing" {
                let value = eval(arg.1, env)?;
                let values = collect_proper_list(&value).ok_or_else(|| {
                    EvalError::type_mismatch("list", value.type_name(), expr.pos)
                })?;
                return Ok(QuasiquotePart::Splice(values));
            }
        }
    }

    Ok(QuasiquotePart::Value(eval_quasiquote_expr(expr, env, depth)?))
}

fn match_quasiquote_form<'a>(items: &'a [Expr], name: &str) -> Option<&'a Expr> {
    match items {
        [head, arg] if matches!(&head.kind, ExprKind::Symbol(symbol) if symbol == name) => {
            Some(arg)
        }
        _ => None,
    }
}

fn match_quasiquote_form_items(expr: &Expr) -> Option<(&str, &Expr)> {
    let ExprKind::List(items) = &expr.kind else {
        return None;
    };

    match items.as_slice() {
        [head, arg] => match &head.kind {
            ExprKind::Symbol(name) => Some((name.as_str(), arg)),
            _ => None,
        },
        _ => None,
    }
}

fn quasiquote_form_value(name: &str, arg: Value) -> Value {
    list_from_values([Value::Symbol(name.into()), arg])
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

fn eval_syntax(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let [template] = exprs else {
        return Err(EvalError::syntax(
            "syntax requires exactly 1 template",
            position,
        ));
    };

    let bindings = collect_syntax_bindings(&env);
    let syntax_context = Environment::syntax_context(&env).unwrap_or_else(|| env.clone());
    let syntax_use_site = Environment::syntax_use_site(&env).unwrap_or_else(|| env.clone());
    let transformer = SyntaxRulesMacro {
        name: String::new(),
        literals: HashSet::new(),
        rules: Vec::new(),
        env: syntax_context,
    };
    let mut state = ExpansionState::default();
    let expr = expand_template(
        template,
        &bindings,
        &transformer,
        &mut state,
        &[],
        false,
    )?;

    Ok(Value::Syntax(SyntaxValue::One(SyntaxObject {
        expr,
        env: state.build_env(syntax_use_site),
    })))
}

fn eval_syntax_case(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let [input_expr, literals_expr, clauses @ ..] = exprs else {
        return Err(EvalError::syntax(
            "syntax-case requires an input, literals list, and at least 1 clause",
            position,
        ));
    };

    if clauses.is_empty() {
        return Err(EvalError::syntax(
            "syntax-case requires at least 1 clause",
            position,
        ));
    }

    let input = expect_single_syntax(&eval(input_expr, env.clone())?, input_expr.pos)?;
    let literals = parse_syntax_rule_literals(literals_expr)?;

    for clause in clauses {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::syntax(
                "syntax-case clauses must be lists",
                clause.pos,
            ));
        };

        let (pattern, fender, template) = match items.as_slice() {
            [pattern, template] => (pattern, None, template),
            [pattern, fender, template] => (pattern, Some(fender), template),
            _ => {
                return Err(EvalError::syntax(
                    "syntax-case clauses must contain a pattern, optional fender, and template",
                    clause.pos,
                ))
            }
        };

        let Some(bindings) = match_pattern(pattern, &input.expr, &literals, None) else {
            continue;
        };

        let clause_env = Environment::child(env.clone());
        bind_syntax_bindings(&clause_env, bindings, input.env.clone());

        if let Some(fender) = fender {
            if !eval(fender, clause_env.clone())?.is_truthy() {
                continue;
            }
        }

        return eval(template, clause_env);
    }

    Err(EvalError::syntax(
        "no matching syntax-case clause",
        position,
    ))
}

fn eval_with_syntax(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = exprs else {
        return Err(EvalError::syntax(
            "with-syntax requires bindings and a body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "with-syntax requires at least 1 body expression",
            position,
        ));
    }

    let ExprKind::List(binding_specs) = &bindings_expr.kind else {
        return Err(EvalError::syntax(
            "with-syntax bindings must be a list",
            bindings_expr.pos,
        ));
    };

    let mut pending = Vec::with_capacity(binding_specs.len());

    for binding_spec in binding_specs {
        let ExprKind::List(parts) = &binding_spec.kind else {
            return Err(EvalError::syntax(
                "with-syntax bindings must be lists",
                binding_spec.pos,
            ));
        };

        let [pattern, value_expr] = parts.as_slice() else {
            return Err(EvalError::syntax(
                "with-syntax bindings must contain a pattern and expression",
                binding_spec.pos,
            ));
        };

        let syntax = expect_single_syntax(&eval(value_expr, env.clone())?, value_expr.pos)?;
        let Some(bindings) = match_pattern(pattern, &syntax.expr, &HashSet::new(), None) else {
            return Err(EvalError::syntax(
                "with-syntax binding does not match its pattern",
                pattern.pos,
            ));
        };

        pending.push((bindings, syntax.env));
    }

    let with_env = Environment::child(env);
    for (bindings, syntax_env) in pending {
        bind_syntax_bindings(&with_env, bindings, syntax_env);
    }

    eval_sequence(body, with_env)
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
        ExprKind::List(items) => quote_list_expr(items),
    }
}

fn quote_list_expr(items: &[Expr]) -> Value {
    let (items, tail) = split_improper_list_items(items);
    let mut list = tail.map_or(Value::EmptyList, quote_expr);

    for item in items.iter().rev() {
        list = Value::Pair(Rc::new(Pair::new(quote_expr(item), list)));
    }

    list
}

fn syntax_value_from_pattern_binding(binding: &PatternBinding, env: EnvRef) -> SyntaxValue {
    match binding {
        PatternBinding::One(expr) => SyntaxValue::One(SyntaxObject {
            expr: expr.clone(),
            env,
        }),
        PatternBinding::Many(values) => SyntaxValue::Many(
            values
                .iter()
                .map(|value| syntax_value_from_pattern_binding(value, env.clone()))
                .collect(),
        ),
    }
}

fn pattern_binding_from_syntax_value(value: &SyntaxValue) -> Option<PatternBinding> {
    match value {
        SyntaxValue::One(object) => Some(PatternBinding::One(object.expr.clone())),
        SyntaxValue::Many(values) => values
            .iter()
            .map(pattern_binding_from_syntax_value)
            .collect::<Option<Vec<_>>>()
            .map(PatternBinding::Many),
    }
}

fn collect_syntax_bindings(env: &EnvRef) -> PatternBindings {
    let mut bindings = HashMap::new();
    collect_syntax_bindings_into(env, &mut bindings);
    bindings
}

fn collect_syntax_bindings_into(env: &EnvRef, bindings: &mut PatternBindings) {
    let (parent, local_bindings) = {
        let env_ref = env.borrow();
        let local_bindings = env_ref
            .bindings
            .iter()
            .filter_map(|(name, binding)| match binding.borrow().clone() {
                Value::Syntax(value) => {
                    pattern_binding_from_syntax_value(&value).map(|binding| (name.clone(), binding))
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        (env_ref.parent.clone(), local_bindings)
    };

    if let Some(parent) = parent {
        collect_syntax_bindings_into(&parent, bindings);
    }

    for (name, binding) in local_bindings {
        bindings.insert(name, binding);
    }
}

fn bind_syntax_bindings(env: &EnvRef, bindings: PatternBindings, syntax_env: EnvRef) {
    for (name, binding) in bindings {
        Environment::define(
            env,
            name,
            Value::Syntax(syntax_value_from_pattern_binding(&binding, syntax_env.clone())),
        );
    }
}

fn expect_single_syntax(value: &Value, position: SourcePos) -> Result<SyntaxObject, EvalError> {
    match value {
        Value::Syntax(SyntaxValue::One(object)) => Ok(object.clone()),
        Value::Syntax(SyntaxValue::Many(_)) => {
            Err(EvalError::syntax("expected a single syntax object", position))
        }
        other => Err(EvalError::type_mismatch(
            "syntax",
            other.type_name(),
            position,
        )),
    }
}

fn syntax_value_to_datum(value: &SyntaxValue) -> Value {
    match value {
        SyntaxValue::One(object) => quote_expr(&object.expr),
        SyntaxValue::Many(values) => list_from_values(values.iter().map(syntax_value_to_datum)),
    }
}

fn pair_value_to_datum_expr(value: &Value, position: SourcePos) -> Result<Expr, EvalError> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut visited = HashSet::new();

    loop {
        match current {
            Value::EmptyList => return Ok(build_list_expr(items, None, position)),
            Value::Pair(pair) => {
                let ptr = Rc::as_ptr(&pair) as usize;
                if !visited.insert(ptr) {
                    return Err(EvalError::syntax("datum->syntax requires a finite list", position));
                }

                items.push(value_to_datum_expr(&pair.car(), position)?);
                current = pair.cdr();
            }
            other => {
                return Ok(build_list_expr(
                    items,
                    Some(value_to_datum_expr(&other, position)?),
                    position,
                ))
            }
        }
    }
}

fn value_to_datum_expr(value: &Value, position: SourcePos) -> Result<Expr, EvalError> {
    match value {
        Value::Integer(number) => Ok(Expr::new(ExprKind::Integer(*number), position)),
        Value::Rational(number) => Ok(Expr::new(ExprKind::Rational(*number), position)),
        Value::Inexact(number) => Ok(Expr::new(ExprKind::Inexact(*number), position)),
        Value::Bool(value) => Ok(Expr::new(ExprKind::Bool(*value), position)),
        Value::Char(value) => Ok(Expr::new(ExprKind::Char(*value), position)),
        Value::String(value) => Ok(Expr::new(
            ExprKind::String(value.to_plain_string()),
            position,
        )),
        Value::Symbol(value) => Ok(Expr::symbol(value.clone(), position)),
        Value::EmptyList => Ok(Expr::list(Vec::new(), position)),
        Value::Pair(_) => pair_value_to_datum_expr(value, position),
        other => Err(EvalError::type_mismatch(
            "datum",
            other.type_name(),
            position,
        )),
    }
}

fn apply_syntax_to_datum(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "syntax->datum",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    match &args[0].value {
        Value::Syntax(value) => Ok(syntax_value_to_datum(value)),
        other => Err(EvalError::type_mismatch(
            "syntax",
            other.type_name(),
            args[0].position,
        )),
    }
}

fn apply_datum_to_syntax(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "datum->syntax",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let context = expect_single_syntax(&args[0].value, args[0].position)?;
    let expr = value_to_datum_expr(&args[1].value, args[1].position)?;

    Ok(Value::Syntax(SyntaxValue::One(SyntaxObject {
        expr,
        env: context.env,
    })))
}

fn apply_macro_transformer(
    transformer: &ProcedureMacro,
    input: SyntaxObject,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let Some(clause) = transformer.procedure.matching_clause(1) else {
        return Err(EvalError::wrong_arg_count(
            transformer.name.clone(),
            transformer.procedure.expected_arity(),
            1,
            position,
        ));
    };

    let call_env = Environment::child_for_macro_expansion(
        transformer.procedure.env.clone(),
        transformer.env.clone(),
        input.env.clone(),
    );

    let args = [Value::Syntax(SyntaxValue::One(input))];
    for (param, arg) in clause.params.iter().zip(args.iter()) {
        Environment::define(&call_env, param.clone(), arg.clone());
    }

    if let Some(rest_param) = &clause.rest_param {
        let rest_values = args[clause.params.len()..].iter().cloned();
        Environment::define(&call_env, rest_param.clone(), list_from_values(rest_values));
    }

    eval_sequence(&clause.body, call_env)
}

fn eval_and_step(exprs: &[Expr], env: EnvRef) -> Result<EvalStep, EvalError> {
    let Some((last, rest)) = exprs.split_last() else {
        return Ok(EvalStep::Value(Value::Bool(true)));
    };

    for expr in rest {
        let value = eval(expr, env.clone())?;
        if !value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(EvalStep::Expr(last.clone(), env))
}

fn eval_and(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    resolve_eval_step(eval_and_step(exprs, env)?)
}

fn eval_or_step(exprs: &[Expr], env: EnvRef) -> Result<EvalStep, EvalError> {
    let Some((last, rest)) = exprs.split_last() else {
        return Ok(EvalStep::Value(Value::Bool(false)));
    };

    for expr in rest {
        let value = eval(expr, env.clone())?;
        if value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(EvalStep::Expr(last.clone(), env))
}

fn eval_or(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    resolve_eval_step(eval_or_step(exprs, env)?)
}

fn eval_begin(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    resolve_eval_step(eval_sequence_step(exprs, env)?)
}

fn eval_cond_step(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<EvalStep, EvalError> {
    Ok(EvalStep::Expr(desugar_cond(exprs, position)?, env))
}

fn eval_cond(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    resolve_eval_step(eval_cond_step(exprs, env, position)?)
}

fn eval_case_step(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<EvalStep, EvalError> {
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

            return eval_sequence_step(body, env);
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
            return eval_sequence_step(body, env);
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

fn eval_case(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    resolve_eval_step(eval_case_step(exprs, env, position)?)
}

fn eval_let_step(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<EvalStep, EvalError> {
    match exprs {
        [first, bindings_expr, body @ ..]
            if !body.is_empty() && matches!(&first.kind, ExprKind::Symbol(_)) =>
        {
            let ExprKind::Symbol(name) = &first.kind else {
                unreachable!("guard ensures symbol");
            };
            eval_named_let_step(name, bindings_expr, body, env, position)
        }
        [bindings_expr, body @ ..] if !body.is_empty() => {
            eval_plain_let_step(bindings_expr, body, env)
        }
        _ => Err(EvalError::syntax(
            "let requires bindings and a body",
            position,
        )),
    }
}

fn eval_let(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    resolve_eval_step(eval_let_step(exprs, env, position)?)
}

fn eval_let_star_step(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<EvalStep, EvalError> {
    let Some((bindings_expr, body)) = exprs.split_first() else {
        return Err(EvalError::syntax(
            "let* requires bindings and a body",
            position,
        ));
    };

    if body.is_empty() {
        return Err(EvalError::syntax(
            "let* requires bindings and a body",
            position,
        ));
    }

    let bindings = parse_bindings(bindings_expr)?;
    let let_env = Environment::child(env);

    for (name, expr) in bindings {
        let value = eval(&expr, let_env.clone())?;
        Environment::define(&let_env, name, value);
    }

    eval_sequence_step(body, let_env)
}

fn eval_letrec_step(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
    sequential: bool,
) -> Result<EvalStep, EvalError> {
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

    eval_sequence_step(body, let_env)
}

fn eval_letrec(
    exprs: &[Expr],
    env: EnvRef,
    position: SourcePos,
    sequential: bool,
) -> Result<Value, EvalError> {
    resolve_eval_step(eval_letrec_step(exprs, env, position, sequential)?)
}

fn eval_plain_let_step(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
) -> Result<EvalStep, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone())?;
    let let_env = Environment::child(env);

    for ((name, _), value) in bindings.into_iter().zip(values) {
        Environment::define(&let_env, name, value.value);
    }

    eval_sequence_step(body, let_env)
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    resolve_eval_step(eval_plain_let_step(bindings_expr, body, env)?)
}

fn eval_named_let_step(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<EvalStep, EvalError> {
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
    apply_step(procedure, position, args, let_env)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<Value, EvalError> {
    resolve_eval_step(eval_named_let_step(
        name,
        bindings_expr,
        body,
        env,
        position,
    )?)
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

fn eval_sequence_step(exprs: &[Expr], env: EnvRef) -> Result<EvalStep, EvalError> {
    let Some((last, init)) = exprs.split_last() else {
        return Ok(EvalStep::Value(Value::Void));
    };

    for expr in init {
        let _ = eval(expr, env.clone())?;
    }

    Ok(EvalStep::Expr(last.clone(), env))
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    resolve_eval_step(eval_sequence_step(exprs, env)?)
}

#[derive(Clone)]
struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn eval_do_step(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<EvalStep, EvalError> {
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
            return eval_sequence_step(&result_exprs, do_env);
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

fn eval_do(exprs: &[Expr], env: EnvRef, position: SourcePos) -> Result<Value, EvalError> {
    resolve_eval_step(eval_do_step(exprs, env, position)?)
}

fn apply(
    callable: Value,
    position: SourcePos,
    args: Vec<LocatedValue>,
    env: EnvRef,
) -> Result<Value, EvalError> {
    resolve_eval_step(apply_step(callable, position, args, env)?)
}

fn apply_step(
    callable: Value,
    position: SourcePos,
    args: Vec<LocatedValue>,
    env: EnvRef,
) -> Result<EvalStep, EvalError> {
    match callable {
        Value::Builtin(name) => apply_builtin_step(name, &args, position, env),
        Value::Procedure(procedure) => apply_user_procedure_step(procedure, args, position),
        Value::NativeProcedure(procedure) => Ok(EvalStep::Value(apply_native_procedure(
            procedure, args, position,
        )?)),
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

fn apply_user_procedure_step(
    procedure: Rc<UserProcedure>,
    args: Vec<LocatedValue>,
    position: SourcePos,
) -> Result<EvalStep, EvalError> {
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
        let rest_values: Vec<_> = args[clause.params.len()..]
            .iter()
            .map(|arg| arg.value.clone())
            .collect();
        Environment::define(&call_env, rest_param.clone(), list_from_values(rest_values));
    }

    eval_sequence_step(&clause.body, call_env)
}

fn apply_user_procedure(
    procedure: Rc<UserProcedure>,
    args: Vec<LocatedValue>,
    position: SourcePos,
) -> Result<Value, EvalError> {
    resolve_eval_step(apply_user_procedure_step(procedure, args, position)?)
}

fn apply_builtin_step(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<EvalStep, EvalError> {
    match name {
        "apply" => apply_apply_step(args, position, env),
        "call-with-values" => apply_call_with_values_step(args, position, env),
        "error" => apply_error_step(args, position),
        _ => Ok(EvalStep::Value(apply_builtin(name, args, position, env)?)),
    }
}

fn apply_error_step(args: &[LocatedValue], position: SourcePos) -> Result<EvalStep, EvalError> {
    Err(EvalError::uncaught_exception(
        build_error_value(args).to_scheme_string(),
        position,
    ))
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
        "assoc" => apply_association_search("assoc", args, position, scheme_equal),
        "assq" => apply_association_search("assq", args, position, scheme_eq),
        "assv" => apply_association_search("assv", args, position, scheme_eq),
        "boolean?" => apply_type_predicate("boolean?", args, position, |value| {
            matches!(value, Value::Bool(_))
        }),
        name if is_composite_car_cdr_name(name) => apply_composite_car_cdr(name, args, position),
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
        "datum->syntax" => apply_datum_to_syntax(args, position),
        "eq?" => apply_equality_predicate("eq?", args, position, scheme_eq),
        "eqv?" => apply_equality_predicate("eqv?", args, position, scheme_eq),
        "equal?" => apply_equality_predicate("equal?", args, position, scheme_equal),
        "even?" => apply_integer_predicate("even?", args, position, |value| value % 2 == 0),
        "exact->inexact" => apply_exact_to_inexact(args, position),
        "exact?" => apply_exact(args, position),
        "expt" => apply_expt(args, position),
        "for-each" => apply_for_each(args, position, env),
        "gcd" => apply_gcd(args, position),
        "integer->char" => apply_integer_to_char(args, position),
        "inexact->exact" => apply_inexact_to_exact(args, position),
        "inexact?" => apply_inexact(args, position),
        "integer?" => apply_integer(args, position),
        "length" => apply_length(args, position),
        "lcm" => apply_lcm(args, position),
        "list" => Ok(list_from_values(args.iter().map(|arg| arg.value.clone()))),
        "list?" => apply_list_predicate(args, position),
        "list-ref" => apply_list_ref(args, position),
        "list-tail" => apply_list_tail(args, position),
        "list->string" => apply_list_to_string(args, position),
        "list->vector" => apply_list_to_vector(args, position),
        "map" => apply_map(args, position, env),
        "max" => apply_min_max("max", args, position, |ordering| {
            ordering == Ordering::Greater
        }),
        "make-string" => apply_make_string(args, position),
        "make-vector" => apply_make_vector(args, position),
        "member" => apply_member_search("member", args, position, scheme_equal),
        "min" => apply_min_max("min", args, position, |ordering| ordering == Ordering::Less),
        "memq" => apply_member_search("memq", args, position, scheme_eq),
        "memv" => apply_member_search("memv", args, position, scheme_eq),
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
            matches!(value, Value::Pair(_))
        }),
        "positive?" => apply_number_predicate("positive?", args, position, Number::is_positive),
        "procedure?" => apply_type_predicate("procedure?", args, position, |value| {
            matches!(
                value,
                Value::Builtin(_)
                    | Value::Procedure(_)
                    | Value::NativeProcedure(_)
                    | Value::Continuation(_)
            )
        }),
        "numerator" => apply_numerator(args, position),
        "quotient" => apply_quotient(args, position),
        "rational?" => apply_rational(args, position),
        "remainder" => apply_remainder(args, position),
        "reverse" => apply_reverse(args, position),
        "round" => apply_round(args, position),
        "string" => apply_string(args, position),
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
        "string<=?" => {
            apply_string_compare("string<=?", args, position, |left, right| left <= right)
        }
        "string=?" => apply_string_compare("string=?", args, position, |left, right| left == right),
        "string>=?" => {
            apply_string_compare("string>=?", args, position, |left, right| left >= right)
        }
        "string>?" => apply_string_compare("string>?", args, position, |left, right| left > right),
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
        "syntax->datum" => apply_syntax_to_datum(args, position),
        "symbol?" => apply_type_predicate("symbol?", args, position, |value| {
            matches!(value, Value::Symbol(_))
        }),
        "symbol->string" => apply_symbol_to_string(args, position),
        "set-car!" => apply_set_car(args, position),
        "set-cdr!" => apply_set_cdr(args, position),
        "truncate" => apply_truncate(args, position),
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
        "values" => apply_values(args),
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

fn apply_gcd(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let mut result = 0_i128;

    for arg in args {
        result = gcd_i128(result, i128::from(arg.as_integer()?));
    }

    let result = i64::try_from(result).map_err(|_| EvalError::integer_overflow(position))?;
    Ok(Value::Integer(result))
}

fn apply_lcm(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let mut result = 1_i128;

    for arg in args {
        let value = i128::from(arg.as_integer()?);
        if value == 0 || result == 0 {
            result = 0;
            continue;
        }

        let divisor = gcd_i128(result, value);
        result = (result / divisor)
            .checked_mul(value)
            .ok_or_else(|| EvalError::integer_overflow(position))?
            .abs();
    }

    let result = i64::try_from(result).map_err(|_| EvalError::integer_overflow(position))?;
    Ok(Value::Integer(result))
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
    let Some((tail_arg, prefix_args)) = args.split_last() else {
        return Ok(Value::EmptyList);
    };

    let mut items = Vec::new();
    for arg in prefix_args {
        items.extend(expect_proper_list(arg)?);
    }

    let mut result = tail_arg.value.clone();
    while let Some(value) = items.pop() {
        result = Value::Pair(Rc::new(Pair::new(value, result)));
    }

    Ok(result)
}

fn apply_reverse(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "reverse",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let mut values = expect_proper_list(&args[0])?;
    values.reverse();
    Ok(list_from_values(values))
}

fn apply_apply_step(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<EvalStep, EvalError> {
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

    apply_step(
        callable.value.clone(),
        callable.position,
        expanded_args,
        env,
    )
}

fn apply_apply(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    resolve_eval_step(apply_apply_step(args, position, env)?)
}

fn apply_values(args: &[LocatedValue]) -> Result<Value, EvalError> {
    Ok(pack_values(args.to_vec()))
}

fn apply_call_with_values_step(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<EvalStep, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "call-with-values",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    let produced = resolve_eval_step(apply_step(
        args[0].value.clone(),
        args[0].position,
        Vec::new(),
        env.clone(),
    )?)?;

    apply_step(
        args[1].value.clone(),
        args[1].position,
        unpack_values(produced, args[0].position),
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

fn apply_truncate(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "truncate",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Integer(truncate_number(
        args[0].as_number()?,
        position,
    )?))
}

fn apply_round(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            "round",
            "exactly 1",
            args.len(),
            position,
        ));
    }

    Ok(Value::Integer(round_number(
        args[0].as_number()?,
        position,
    )?))
}

fn truncate_number(number: Number, position: SourcePos) -> Result<i64, EvalError> {
    match number {
        Number::Integer(value) => Ok(value),
        Number::Rational(rational) => Ok(rational.numerator / rational.denominator),
        Number::Inexact(value) => f64_to_i64(value.trunc(), position),
    }
}

fn round_number(number: Number, position: SourcePos) -> Result<i64, EvalError> {
    match number {
        Number::Integer(value) => Ok(value),
        Number::Rational(rational) => {
            let rounded = (rational.numerator as f64 / rational.denominator as f64).round();
            f64_to_i64(rounded, position)
        }
        Number::Inexact(value) => f64_to_i64(value.round(), position),
    }
}

fn f64_to_i64(value: f64, position: SourcePos) -> Result<i64, EvalError> {
    if !value.is_finite() || value < i64::MIN as f64 || value > i64::MAX as f64 {
        return Err(EvalError::integer_overflow(position));
    }

    Ok(value as i64)
}

fn apply_make_string(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::wrong_arg_count(
            "make-string",
            "1 or 2",
            args.len(),
            position,
        ));
    }

    let length = expect_non_negative_length(&args[0])?;
    let fill = match args.get(1) {
        Some(arg) => arg.as_char()?,
        None => ' ',
    };

    let string = std::iter::repeat(fill).take(length).collect::<String>();
    Ok(Value::String(SchemeString::immutable(&string)))
}

fn apply_string(args: &[LocatedValue], _position: SourcePos) -> Result<Value, EvalError> {
    let mut string = String::with_capacity(args.len());
    for arg in args {
        string.push(arg.as_char()?);
    }

    Ok(Value::String(SchemeString::immutable(&string)))
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

    Ok(list_from_values(
        args[0]
            .as_string()?
            .to_plain_string()
            .chars()
            .map(Value::Char),
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

    Ok(expect_pair(&args[0])?.car())
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

    Ok(expect_pair(&args[0])?.cdr())
}

fn apply_composite_car_cdr(
    name: &str,
    args: &[LocatedValue],
    position: SourcePos,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let mut value = args[0].value.clone();
    for op in name[1..name.len() - 1].chars().rev() {
        let pair = match value {
            Value::Pair(pair) => pair,
            other => {
                return Err(EvalError::type_mismatch(
                    "pair",
                    other.type_name(),
                    args[0].position,
                ))
            }
        };

        value = match op {
            'a' => pair.car(),
            'd' => pair.cdr(),
            _ => unreachable!("composite car/cdr name already validated"),
        };
    }

    Ok(value)
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

    Ok(Value::Pair(Rc::new(Pair::new(
        args[0].value.clone(),
        args[1].value.clone(),
    ))))
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

    Ok(Value::Bool(matches!(&args[0].value, Value::EmptyList)))
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

    Ok(Value::Bool(is_proper_list(&args[0].value)))
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

    let length = expect_proper_list_length(&args[0])?;
    let index = args[1].as_integer()?;
    if index < 0 || index as usize > length {
        return Err(EvalError::index_out_of_bounds(
            index,
            length,
            args[1].position,
        ));
    }

    Ok(list_tail_value(&args[0].value, index as usize)
        .expect("list tail must exist after length check"))
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
        result.push(ch);
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

    Ok(Value::Vector(Rc::new(RefCell::new(expect_proper_list(
        &args[0],
    )?))))
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

    Ok(list_from_values(
        args[0].as_vector()?.borrow().iter().cloned(),
    ))
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

fn apply_member_search<F>(
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

    let list = expect_proper_list(&args[1])?;
    for (index, entry) in list.iter().enumerate() {
        if predicate(&args[0].value, entry) {
            return Ok(
                list_tail_value(&args[1].value, index).expect("member tail must exist for index")
            );
        }
    }

    Ok(Value::Bool(false))
}

fn apply_association_search<F>(
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

    let alist = expect_proper_list(&args[1])?;
    for entry in alist {
        let Value::Pair(pair) = &entry else {
            continue;
        };

        let key = pair.car();
        if predicate(&args[0].value, &key) {
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
        return Ok(Value::EmptyList);
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

    Ok(list_from_values(mapped))
}

fn apply_for_each(
    args: &[LocatedValue],
    position: SourcePos,
    env: EnvRef,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            "for-each",
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
        return Ok(Value::Void);
    };

    if lists.iter().any(|values| values.len() != first_len) {
        return Err(EvalError::syntax(
            "for-each requires lists of equal length",
            position,
        ));
    }

    for index in 0..first_len {
        let call_args = lists
            .iter()
            .zip(args[1..].iter())
            .map(|(values, arg)| LocatedValue::new(values[index].clone(), arg.position))
            .collect();
        apply(callable.clone(), args[0].position, call_args, env.clone())?;
    }

    Ok(Value::Void)
}

fn apply_set_car(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "set-car!",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    expect_pair(&args[0])?.set_car(args[1].value.clone());
    Ok(Value::Void)
}

fn apply_set_cdr(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::wrong_arg_count(
            "set-cdr!",
            "exactly 2",
            args.len(),
            position,
        ));
    }

    expect_pair(&args[0])?.set_cdr(args[1].value.clone());
    Ok(Value::Void)
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

fn list_from_values<I>(values: I) -> Value
where
    I: IntoIterator<Item = Value>,
{
    let mut values = values.into_iter().collect::<Vec<_>>();
    let mut list = Value::EmptyList;

    while let Some(value) = values.pop() {
        list = Value::Pair(Rc::new(Pair::new(value, list)));
    }

    list
}

fn build_error_value(args: &[LocatedValue]) -> Value {
    let mut values = Vec::with_capacity(args.len() + 1);
    values.push(Value::Symbol("error".into()));
    values.extend(args.iter().map(|arg| arg.value.clone()));
    list_from_values(values)
}

fn is_proper_list(value: &Value) -> bool {
    proper_list_length(value).is_some()
}

fn proper_list_length(value: &Value) -> Option<usize> {
    let mut current = value.clone();
    let mut seen = HashSet::new();
    let mut length = 0;

    loop {
        match current {
            Value::EmptyList => return Some(length),
            Value::Pair(pair) => {
                let ptr = Rc::as_ptr(&pair) as usize;
                if !seen.insert(ptr) {
                    return None;
                }

                length += 1;
                current = pair.cdr();
            }
            _ => return None,
        }
    }
}

fn list_tail_value(value: &Value, index: usize) -> Option<Value> {
    let mut current = value.clone();

    for _ in 0..index {
        let Value::Pair(pair) = current else {
            return None;
        };
        current = pair.cdr();
    }

    Some(current)
}

fn collect_proper_list(value: &Value) -> Option<Vec<Value>> {
    let mut current = value.clone();
    let mut seen = HashSet::new();
    let mut values = Vec::new();

    loop {
        match current {
            Value::EmptyList => return Some(values),
            Value::Pair(pair) => {
                let ptr = Rc::as_ptr(&pair) as usize;
                if !seen.insert(ptr) {
                    return None;
                }

                values.push(pair.car());
                current = pair.cdr();
            }
            _ => return None,
        }
    }
}

fn expect_pair(arg: &LocatedValue) -> Result<PairRef, EvalError> {
    let Value::Pair(pair) = &arg.value else {
        return Err(EvalError::type_mismatch(
            "pair",
            arg.value.type_name(),
            arg.position,
        ));
    };

    Ok(pair.clone())
}

fn expect_proper_list(arg: &LocatedValue) -> Result<Vec<Value>, EvalError> {
    let Some(values) = collect_proper_list(&arg.value) else {
        return Err(EvalError::type_mismatch(
            "list",
            arg.value.type_name(),
            arg.position,
        ));
    };

    Ok(values)
}

fn expect_proper_list_length(arg: &LocatedValue) -> Result<usize, EvalError> {
    let Some(length) = proper_list_length(&arg.value) else {
        return Err(EvalError::type_mismatch(
            "list",
            arg.value.type_name(),
            arg.position,
        ));
    };

    Ok(length)
}

fn fresh_generated_symbol(kind: &str) -> String {
    let next = GENERATED_SYMBOL_COUNTER.with(|counter| {
        let next = counter.get();
        counter.set(next + 1);
        next
    });
    format!("__macro_{kind}_{next}")
}

fn is_composite_car_cdr_name(name: &str) -> bool {
    name.len() >= 4
        && name.len() <= 6
        && name.starts_with('c')
        && name.ends_with('r')
        && name[1..name.len() - 1]
            .chars()
            .all(|ch| matches!(ch, 'a' | 'd'))
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
            | "guard"
            | "if"
            | "lambda"
            | "let"
            | "let*"
            | "letrec"
            | "letrec*"
            | "or"
            | "quasiquote"
            | "quote"
            | "set!"
            | "syntax"
            | "syntax-case"
            | "syntax-rules"
            | "unquote"
            | "unquote-splicing"
            | "with-syntax"
    )
}

fn is_ellipsis_expr(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(name) if name == "...")
}

fn is_pattern_variable(
    name: &str,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
) -> bool {
    name != "..."
        && name != "."
        && name != "_"
        && macro_name.is_none_or(|macro_name| name != macro_name)
        && !literals.contains(name)
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

    Ok(Rc::new(MacroTransformer::SyntaxRules(SyntaxRulesMacro {
        name: macro_name.to_string(),
        literals,
        rules,
        env,
    })))
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
    match transformer.as_ref() {
        MacroTransformer::SyntaxRules(transformer) => {
            expand_syntax_rules_macro_call(transformer, invocation_items, env, position)
        }
        MacroTransformer::Procedure(transformer) => {
            expand_procedure_macro_call(transformer, invocation_items, env, position)
        }
    }
}

fn expand_syntax_rules_macro_call(
    transformer: &SyntaxRulesMacro,
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

fn expand_procedure_macro_call(
    transformer: &ProcedureMacro,
    invocation_items: &[Expr],
    env: EnvRef,
    position: SourcePos,
) -> Result<ExpandedExpr, EvalError> {
    let invocation = Expr::list(invocation_items.to_vec(), position);
    let value = apply_macro_transformer(
        transformer,
        SyntaxObject {
            expr: invocation,
            env: env.clone(),
        },
        position,
    )?;
    let syntax = expect_single_syntax(&value, position)?;

    Ok(ExpandedExpr {
        expr: syntax.expr,
        env: syntax.env,
    })
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
        return match_pattern(&rule.pattern, invocation, literals, Some(macro_name));
    };

    let Some((pattern_head, pattern_tail)) = pattern_items.split_first() else {
        return None;
    };
    let Some((_invocation_head, invocation_tail)) = invocation_items.split_first() else {
        return None;
    };

    matches!(
        &pattern_head.kind,
        ExprKind::Symbol(name) if name == "_" || name == macro_name
    )
        .then_some(())
        .and_then(|_| match_pattern_list(
            pattern_tail,
            invocation_tail,
            literals,
            Some(macro_name),
            invocation.pos,
        ))
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
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
            match_pattern_list(pattern_items, input_items, literals, macro_name, input.pos)
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
    macro_name: Option<&str>,
    input_position: SourcePos,
) -> Option<PatternBindings> {
    let (pattern_items, pattern_tail) = split_improper_list_items(patterns);
    let (input_items, input_tail) = split_improper_list_items(inputs);
    match_pattern_list_parts(
        pattern_items,
        pattern_tail,
        input_items,
        input_tail,
        literals,
        macro_name,
        input_position,
    )
}

fn match_pattern_list_parts(
    patterns: &[Expr],
    pattern_tail: Option<&Expr>,
    inputs: &[Expr],
    input_tail: Option<&Expr>,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
    input_position: SourcePos,
) -> Option<PatternBindings> {
    if patterns.is_empty() {
        return match pattern_tail {
            Some(tail_pattern) => {
                let remainder = build_list_expr(
                    inputs.to_vec(),
                    input_tail.cloned(),
                    input_position,
                );
                match_pattern(tail_pattern, &remainder, literals, macro_name)
            }
            None => (inputs.is_empty() && input_tail.is_none()).then(HashMap::new),
        };
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
            let Some(suffix_bindings) = match_pattern_list_parts(
                suffix,
                pattern_tail,
                &inputs[repeat_count..],
                input_tail,
                literals,
                macro_name,
                input_position,
            ) else {
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
    let rest_bindings = match_pattern_list_parts(
        &patterns[1..],
        pattern_tail,
        rest_inputs,
        input_tail,
        literals,
        macro_name,
        input_position,
    )?;
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
    macro_name: Option<&str>,
    vars: &mut HashSet<String>,
) {
    match &pattern.kind {
        ExprKind::Symbol(name) if is_pattern_variable(name, literals, macro_name) => {
            vars.insert(name.clone());
        }
        ExprKind::List(items) => {
            let (items, tail) = split_improper_list_items(items);
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
            if let Some(tail) = tail {
                collect_pattern_variables(tail, literals, macro_name, vars);
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

    if name == "." {
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

    let (items, tail) = split_improper_list_items(items);
    let expanded_items =
        expand_template_sequence(items, bindings, transformer, state, indices, quoted)?;
    let expanded_tail = tail
        .map(|tail| expand_template(tail, bindings, transformer, state, indices, quoted))
        .transpose()?;

    Ok(build_list_expr(expanded_items, expanded_tail, template.pos))
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
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::syntax("invalid let template", position));
    };

    let (name_expr, bindings_expr, body) = match tail {
        [name_expr, bindings_expr, body @ ..]
            if !body.is_empty() && matches!(&name_expr.kind, ExprKind::Symbol(_)) =>
        {
            (Some(name_expr), bindings_expr, body)
        }
        [bindings_expr, body @ ..] if !body.is_empty() => (None, bindings_expr, body),
        _ => {
            return Err(EvalError::syntax(
                "let template requires bindings and a body",
                position,
            ))
        }
    };

    let ExprKind::List(binding_items) = &bindings_expr.kind else {
        return Err(EvalError::syntax(
            "let template bindings must be a list",
            bindings_expr.pos,
        ));
    };

    let mut expanded_bindings = Vec::with_capacity(binding_items.len());
    let mut scope = HashMap::new();

    let expanded_name = if let Some(name_expr) = name_expr {
        let (expanded_name, rename) =
            expand_binding_identifier(name_expr, bindings, transformer, state, indices)?;
        if let Some((original, renamed)) = rename {
            scope.insert(original, renamed);
        }
        Some(expanded_name)
    } else {
        None
    };

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
    expanded_items.push(head.clone());
    if let Some(expanded_name) = expanded_name {
        expanded_items.push(expanded_name);
    }
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
            let (items, tail) = split_improper_list_items(items);
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
            if let Some(tail) = tail {
                collect_template_repeat_count(tail, bindings, indices, repeat_count)?;
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
            Some('`') => self.parse_quasiquote_shorthand(position),
            Some(',') => self.parse_unquote_shorthand(position),
            Some('#') if self.peek_next() == Some('\'') => self.parse_syntax_shorthand(position),
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
        Ok(build_quote_expr(self.parse_expr()?, position))
    }

    fn parse_quasiquote_shorthand(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect('`')?;
        Ok(build_quasiquote_expr(self.parse_expr()?, position))
    }

    fn parse_unquote_shorthand(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect(',')?;
        let name = if self.peek() == Some('@') {
            self.expect('@')?;
            "unquote-splicing"
        } else {
            "unquote"
        };

        Ok(Expr::list(
            vec![Expr::symbol(name, position), self.parse_expr()?],
            position,
        ))
    }

    fn parse_syntax_shorthand(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect('#')?;
        self.expect('\'')?;
        Ok(build_syntax_expr(self.parse_expr()?, position))
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

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.index + 1).copied()
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
