mod builtins;
pub mod error;
mod macros;
mod number;
mod parser;
mod record;
mod render;
mod special_forms;

pub use error::{EvalError, SourcePos};
use macros::{MacroEnvRef, MacroEnvironment};
use number::Number;
use parser::Parser;
use record::{
    define_record_type as eval_define_record_type, render_record, NativeProcedure, RecordRef,
};
use render::{
    render_char, render_display_improper_list, render_display_list, render_improper_list,
    render_list, render_string, render_vector,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
enum Expr {
    Number(Number, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    CapturedSymbol(String, BindingRef, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn position(&self) -> SourcePos {
        match self {
            Self::Number(_, position)
            | Self::Boolean(_, position)
            | Self::String(_, position)
            | Self::Char(_, position)
            | Self::Symbol(_, position)
            | Self::CapturedSymbol(_, _, position)
            | Self::List(_, position) => *position,
        }
    }
}

type EnvRef = Rc<RefCell<Environment>>;
type OutputRef = Rc<RefCell<String>>;
type BindingRef = Rc<RefCell<Value>>;

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(String),
    MutableString(Rc<RefCell<Vec<char>>>),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    ImproperList(Vec<Value>, Box<Value>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Procedure(Rc<Procedure>),
    NativeProcedure(Rc<NativeProcedure>),
    Builtin(Builtin),
    Record(RecordRef),
    Uninitialized,
    Void,
}

struct Procedure {
    clauses: Vec<ProcedureClause>,
    env: EnvRef,
    macro_env: MacroEnvRef,
}

struct ProcedureClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Rc<Expr>,
}

#[derive(Clone)]
struct Builtin {
    kind: BuiltinKind,
    output: OutputRef,
}

#[derive(Clone, Copy)]
enum BuiltinKind {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    Length,
    Append,
    Apply,
    EqPred,
    EqvPred,
    EqualPred,
    Map,
    StringPred,
    NumberPred,
    IntegerPred,
    RationalPred,
    ExactPred,
    InexactPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    ProcedurePred,
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringCopy,
    StringSet,
    StringToList,
    ListToString,
    CharPred,
    CharToInteger,
    IntegerToChar,
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
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLessThan,
    StringEqual,
    StringLessThan,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingRef>,
}

#[derive(Clone)]
struct OwnedExprRef {
    root: Rc<Expr>,
    path: Vec<usize>,
}

enum EvalTarget<'a> {
    BorrowedExpr(&'a Expr),
    BorrowedSequence(&'a [Expr]),
    OwnedExpr(OwnedExprRef),
}

enum EvalStep<'a> {
    Value(Value),
    Tail {
        target: EvalTarget<'a>,
        env: EnvRef,
        macro_env: MacroEnvRef,
    },
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Number(number) if number.as_exact_integer().is_some() => "integer",
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) | Self::MutableString(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
            Self::List(_) => "list",
            Self::ImproperList(_, _) => "pair",
            Self::Vector(_) => "vector",
            Self::Procedure(_) | Self::NativeProcedure(_) | Self::Builtin(_) => "procedure",
            Self::Record(_) => "record",
            Self::Uninitialized => "uninitialized",
            Self::Void => "void",
        }
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Self::Number(number) => number.as_exact_integer().ok_or(EvalError::TypeMismatch {
                expected: "integer",
                found: self.type_name().into(),
            }),
            other => Err(EvalError::TypeMismatch {
                expected: "integer",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_number(&self) -> Result<Number, EvalError> {
        match self {
            Self::Number(number) => Ok(*number),
            other => Err(EvalError::TypeMismatch {
                expected: "number",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_string(&self) -> Result<String, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            Self::MutableString(value) => Ok(value.borrow().iter().collect()),
            other => Err(EvalError::TypeMismatch {
                expected: "string",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_symbol(&self) -> Result<&str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_char(&self) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            other => Err(EvalError::TypeMismatch {
                expected: "character",
                found: other.type_name().into(),
            }),
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Number(number) => number.render(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => render_string(value),
            Self::MutableString(value) => render_string(&value.borrow().iter().collect::<String>()),
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value),
            Self::List(items) => render_list(items),
            Self::ImproperList(items, tail) => render_improper_list(items, tail),
            Self::Vector(items) => render_vector(&items.borrow()),
            Self::Procedure(_) | Self::NativeProcedure(_) | Self::Builtin(_) => {
                "#<procedure>".into()
            }
            Self::Record(record) => render_record(record),
            Self::Uninitialized => "#<uninitialized>".into(),
            Self::Void => "#<void>".into(),
        }
    }

    fn render_display(&self) -> String {
        match self {
            Self::String(value) => value.clone(),
            Self::MutableString(value) => value.borrow().iter().collect(),
            Self::Char(value) => value.to_string(),
            Self::List(items) => render_display_list(items),
            Self::ImproperList(items, tail) => render_display_improper_list(items, tail),
            Self::Vector(items) => render_vector(&items.borrow()),
            _ => self.render(),
        }
    }
}

impl Builtin {
    fn new(kind: BuiltinKind, output: OutputRef) -> Self {
        Self { kind, output }
    }

    fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        builtins::apply_builtin(self.kind, args, &self.output)
    }
}

impl BuiltinKind {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThanOrEqual => ">=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::Apply => "apply",
            Self::EqPred => "eq?",
            Self::EqvPred => "eqv?",
            Self::EqualPred => "equal?",
            Self::Map => "map",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::IntegerPred => "integer?",
            Self::RationalPred => "rational?",
            Self::ExactPred => "exact?",
            Self::InexactPred => "inexact?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
            Self::ProcedurePred => "procedure?",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::ExactToInexact => "exact->inexact",
            Self::InexactToExact => "inexact->exact",
            Self::Numerator => "numerator",
            Self::Denominator => "denominator",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::StringToList => "string->list",
            Self::ListToString => "list->string",
            Self::CharPred => "char?",
            Self::CharToInteger => "char->integer",
            Self::IntegerToChar => "integer->char",
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
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPred => "list?",
            Self::Assoc => "assoc",
            Self::CharAlphabeticPred => "char-alphabetic?",
            Self::CharNumericPred => "char-numeric?",
            Self::CharUpcase => "char-upcase",
            Self::CharDowncase => "char-downcase",
            Self::CharEqual => "char=?",
            Self::CharLessThan => "char<?",
            Self::StringEqual => "string=?",
            Self::StringLessThan => "string<?",
            Self::StringCiEqual => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::Vector => "vector",
            Self::MakeVector => "make-vector",
            Self::VectorRef => "vector-ref",
            Self::VectorSet => "vector-set!",
            Self::VectorLength => "vector-length",
            Self::VectorPred => "vector?",
            Self::VectorToList => "vector->list",
            Self::ListToVector => "list->vector",
        }
    }
}

impl ProcedureClause {
    fn new(params: Vec<String>, rest_param: Option<String>, body: Vec<Expr>) -> Self {
        Self {
            params,
            rest_param,
            body: wrap_procedure_body(body),
        }
    }

    fn matches_arity(&self, arg_count: usize) -> bool {
        arg_count >= self.params.len()
            && (self.rest_param.is_some() || arg_count == self.params.len())
    }

    fn expected_arity(&self) -> String {
        if self.rest_param.is_some() {
            format!("at least {} argument(s)", self.params.len())
        } else {
            format!("exactly {} argument(s)", self.params.len())
        }
    }
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
        }))
    }
}

impl OwnedExprRef {
    fn new(root: Rc<Expr>) -> Self {
        Self {
            root,
            path: Vec::new(),
        }
    }

    fn current(&self) -> &Expr {
        let mut expr = self.root.as_ref();
        for &index in &self.path {
            let Expr::List(items, _) = expr else {
                unreachable!("owned expression paths must stay within list expressions");
            };
            expr = items
                .get(index)
                .expect("owned expression path index must be valid");
        }
        expr
    }

    fn child(&self, index: usize) -> Self {
        let mut path = self.path.clone();
        path.push(index);
        Self {
            root: Rc::clone(&self.root),
            path,
        }
    }
}

fn wrap_procedure_body(body: Vec<Expr>) -> Rc<Expr> {
    match body.len() {
        0 => unreachable!("procedure bodies must not be empty"),
        1 => Rc::new(body.into_iter().next().expect("body length checked")),
        _ => {
            let position = body[0].position();
            let mut items = Vec::with_capacity(body.len() + 1);
            items.push(Expr::Symbol("begin".into(), position));
            items.extend(body);
            Rc::new(Expr::List(items, position))
        }
    }
}

fn make_procedure(clauses: Vec<ProcedureClause>, env: &EnvRef, macro_env: &MacroEnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        clauses,
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }))
}

fn single_clause_procedure(
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Value {
    make_procedure(
        vec![ProcedureClause::new(params, rest_param, body)],
        env,
        macro_env,
    )
}

fn apply_callable(callable: Value, args: &[Value]) -> Result<Value, EvalError> {
    special_forms::apply_callable(callable, args)
}

fn parse_required_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    special_forms::parse_required_param_names(items)
}

fn values_eq(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Number(lhs), Value::Number(rhs)) => lhs.equals(*rhs).unwrap_or(false),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::MutableString(rhs))
        | (Value::MutableString(rhs), Value::String(lhs)) => {
            lhs.chars().eq(rhs.borrow().iter().copied())
        }
        (Value::MutableString(lhs), Value::MutableString(rhs)) => *lhs.borrow() == *rhs.borrow(),
        (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs == rhs,
        (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
        (Value::Vector(lhs), Value::Vector(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::List(lhs), Value::List(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs.iter())
                    .all(|(lhs, rhs)| values_eq(lhs, rhs))
        }
        (Value::ImproperList(lhs_items, lhs_tail), Value::ImproperList(rhs_items, rhs_tail)) => {
            lhs_items.len() == rhs_items.len()
                && lhs_items
                    .iter()
                    .zip(rhs_items.iter())
                    .all(|(lhs, rhs)| values_eq(lhs, rhs))
                && values_eq(lhs_tail, rhs_tail)
        }
        (Value::Record(lhs), Value::Record(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn values_eqv(lhs: &Value, rhs: &Value) -> bool {
    values_eq(lhs, rhs)
}

fn values_equal(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Vector(lhs), Value::Vector(rhs)) => {
            let lhs = lhs.borrow();
            let rhs = rhs.borrow();
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs.iter())
                    .all(|(lhs, rhs)| values_equal(lhs, rhs))
        }
        _ => values_eq(lhs, rhs),
    }
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut()
        .bindings
        .insert(name, Rc::new(RefCell::new(value)));
}

fn env_lookup_binding(env: &EnvRef, name: &str) -> Option<BindingRef> {
    let (binding, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    binding.or_else(|| parent.and_then(|parent| env_lookup_binding(&parent, name)))
}

fn env_lookup(env: &EnvRef, name: &str) -> Option<Value> {
    env_lookup_binding(env, name).map(|binding| binding.borrow().clone())
}

fn env_set(env: &EnvRef, name: &str, value: Value) -> bool {
    let (binding, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    if let Some(binding) = binding {
        *binding.borrow_mut() = value;
        true
    } else if let Some(parent) = parent {
        env_set(&parent, name, value)
    } else {
        false
    }
}

fn tail_borrowed_expr<'a>(expr: &'a Expr, env: &EnvRef, macro_env: &MacroEnvRef) -> EvalStep<'a> {
    EvalStep::Tail {
        target: EvalTarget::BorrowedExpr(expr),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }
}

fn tail_borrowed_sequence<'a>(
    exprs: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> EvalStep<'a> {
    EvalStep::Tail {
        target: EvalTarget::BorrowedSequence(exprs),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }
}

fn tail_owned_expr<'a>(expr: OwnedExprRef, env: &EnvRef, macro_env: &MacroEnvRef) -> EvalStep<'a> {
    EvalStep::Tail {
        target: EvalTarget::OwnedExpr(expr),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }
}

fn eval_target<'a>(
    mut target: EvalTarget<'a>,
    mut env: EnvRef,
    mut macro_env: MacroEnvRef,
) -> Result<Value, EvalError> {
    loop {
        let step = match &target {
            EvalTarget::BorrowedExpr(expr) => eval_expr_step(expr, &env, &macro_env)?,
            EvalTarget::BorrowedSequence(exprs) => eval_sequence_step(exprs, &env, &macro_env)?,
            EvalTarget::OwnedExpr(expr) => eval_owned_expr_step(expr, &env, &macro_env)?,
        };

        match step {
            EvalStep::Value(value) => return Ok(value),
            EvalStep::Tail {
                target: next_target,
                env: next_env,
                macro_env: next_macro_env,
            } => {
                target = next_target;
                env = next_env;
                macro_env = next_macro_env;
            }
        }
    }
}

fn eval_program(exprs: &[Expr], output: OutputRef) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = builtins::default_env(output);
    let macro_env = MacroEnvironment::new(None);
    eval_sequence(exprs, &env, &macro_env)
}

fn eval_sequence(
    exprs: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    eval_target(
        EvalTarget::BorrowedSequence(exprs),
        Rc::clone(env),
        Rc::clone(macro_env),
    )
}

fn eval_expr(expr: &Expr, env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    eval_target(
        EvalTarget::BorrowedExpr(expr),
        Rc::clone(env),
        Rc::clone(macro_env),
    )
}

fn eval_sequence_step<'a>(
    exprs: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<EvalStep<'a>, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(EvalStep::Value(Value::Void));
    };

    for expr in prefix {
        eval_expr(expr, env, macro_env)?;
    }

    Ok(tail_borrowed_expr(last, env, macro_env))
}

fn eval_expr_step<'a>(
    expr: &'a Expr,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<EvalStep<'a>, EvalError> {
    match expr {
        Expr::Number(value, _) => Ok(EvalStep::Value(Value::Number(*value))),
        Expr::Boolean(value, _) => Ok(EvalStep::Value(Value::Boolean(*value))),
        Expr::String(value, _) => Ok(EvalStep::Value(Value::String(value.clone()))),
        Expr::Char(value, _) => Ok(EvalStep::Value(Value::Char(*value))),
        Expr::Symbol(name, position) => {
            let value = env_lookup(env, name).ok_or_else(|| {
                EvalError::UnboundVariable { name: name.clone() }.with_position(*position)
            })?;
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }.with_position(*position))
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::CapturedSymbol(name, binding, position) => {
            let value = binding.borrow().clone();
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }.with_position(*position))
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::List(items, position) => eval_borrowed_list_step(items, *position, env, macro_env)
            .map_err(|err| err.with_position(*position)),
    }
}

fn eval_owned_expr_step<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<EvalStep<'a>, EvalError> {
    match expr.current() {
        Expr::Number(value, _) => Ok(EvalStep::Value(Value::Number(*value))),
        Expr::Boolean(value, _) => Ok(EvalStep::Value(Value::Boolean(*value))),
        Expr::String(value, _) => Ok(EvalStep::Value(Value::String(value.clone()))),
        Expr::Char(value, _) => Ok(EvalStep::Value(Value::Char(*value))),
        Expr::Symbol(name, position) => {
            let value = env_lookup(env, name).ok_or_else(|| {
                EvalError::UnboundVariable { name: name.clone() }.with_position(*position)
            })?;
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }.with_position(*position))
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::CapturedSymbol(name, binding, position) => {
            let value = binding.borrow().clone();
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }.with_position(*position))
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::List(items, position) => {
            eval_owned_list_step(expr, items, *position, env, macro_env)
                .map_err(|err| err.with_position(*position))
        }
    }
}

fn eval_borrowed_list_step<'a>(
    items: &'a [Expr],
    position: SourcePos,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<EvalStep<'a>, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        });
    };

    let head_position = head.position();

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => {
                return special_forms::eval_define(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "define-syntax" => {
                return macros::eval_define_syntax(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "if" => {
                return special_forms::eval_if(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "quote" => {
                return special_forms::eval_quote(&items[1..])
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "lambda" => {
                return special_forms::eval_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "case-lambda" => {
                return special_forms::eval_case_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "define-record-type" => {
                return eval_define_record_type(&items[1..], env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "set!" => {
                return special_forms::eval_set(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "and" => {
                return special_forms::eval_and(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "or" => {
                return special_forms::eval_or(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "begin" => {
                return special_forms::eval_begin(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "cond" => {
                return special_forms::eval_cond(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "let" => {
                return special_forms::eval_let(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "letrec" => {
                return special_forms::eval_letrec(&items[1..], env, macro_env, false)
                    .map_err(|err| err.with_position(head_position))
            }
            "letrec*" => {
                return special_forms::eval_letrec(&items[1..], env, macro_env, true)
                    .map_err(|err| err.with_position(head_position))
            }
            "case" => {
                return special_forms::eval_case(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "do" => {
                return special_forms::eval_do(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            _ => {}
        }
    }

    if let Some(expanded) = macros::expand_macro_call(items, position, macro_env)
        .map_err(|err| err.with_position(head_position))?
    {
        return eval_expr(&expanded, env, macro_env).map(EvalStep::Value);
    }

    let callable = match head {
        Expr::Symbol(name, position) => env_lookup(env, name).ok_or_else(|| {
            EvalError::UnknownOperator { name: name.clone() }.with_position(*position)
        })?,
        _ => eval_expr(head, env, macro_env)?,
    };

    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for expr in &items[1..] {
        args.push(eval_expr(expr, env, macro_env)?);
    }

    special_forms::apply_callable_result(callable, &args)
        .map_err(|err| err.with_position(head_position))
}

fn eval_owned_list_step<'a>(
    expr: &OwnedExprRef,
    items: &[Expr],
    position: SourcePos,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<EvalStep<'a>, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        });
    };

    let head_position = head.position();

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => {
                return special_forms::eval_define(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "define-syntax" => {
                return macros::eval_define_syntax(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "if" => {
                return special_forms::eval_owned_if(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "quote" => {
                return special_forms::eval_quote(&items[1..])
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "lambda" => {
                return special_forms::eval_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "case-lambda" => {
                return special_forms::eval_case_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "define-record-type" => {
                return eval_define_record_type(&items[1..], env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "set!" => {
                return special_forms::eval_set(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            "and" => {
                return special_forms::eval_owned_and(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "or" => {
                return special_forms::eval_owned_or(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "begin" => {
                return special_forms::eval_owned_begin(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "cond" => {
                return special_forms::eval_owned_cond(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "let" => {
                return special_forms::eval_owned_let(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "letrec" => {
                return special_forms::eval_owned_letrec(expr, env, macro_env, false)
                    .map_err(|err| err.with_position(head_position))
            }
            "letrec*" => {
                return special_forms::eval_owned_letrec(expr, env, macro_env, true)
                    .map_err(|err| err.with_position(head_position))
            }
            "case" => {
                return special_forms::eval_owned_case(expr, env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "do" => {
                return special_forms::eval_do(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position))
            }
            _ => {}
        }
    }

    if let Some(expanded) = macros::expand_macro_call(items, position, macro_env)
        .map_err(|err| err.with_position(head_position))?
    {
        return eval_expr(&expanded, env, macro_env).map(EvalStep::Value);
    }

    let callable = match head {
        Expr::Symbol(name, position) => env_lookup(env, name).ok_or_else(|| {
            EvalError::UnknownOperator { name: name.clone() }.with_position(*position)
        })?,
        _ => eval_expr(head, env, macro_env)?,
    };

    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for expr in &items[1..] {
        args.push(eval_expr(expr, env, macro_env)?);
    }

    special_forms::apply_callable_result(callable, &args)
        .map_err(|err| err.with_position(head_position))
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
    let (value, _) = eval_input(input)?;
    Ok(value.render())
}

fn eval_input(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let output = Rc::new(RefCell::new(String::new()));
    let value = eval_program(&exprs, Rc::clone(&output))?;
    let rendered_output = output.borrow().clone();
    Ok((value, rendered_output))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_input(input)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;
