mod builtins;
pub mod error;
mod macros;
mod number;
mod parser;
mod record;
mod render;

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
    body: Vec<Expr>,
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
    CharPred,
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
            Self::CharPred => "char?",
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
            body,
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
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env, macro_env)?;
    }

    Ok(last)
}

fn eval_expr(expr: &Expr, env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, position) => {
            let value = env_lookup(env, name).ok_or_else(|| {
                EvalError::UnboundVariable { name: name.clone() }.with_position(*position)
            })?;
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }.with_position(*position))
            } else {
                Ok(value)
            }
        }
        Expr::CapturedSymbol(name, binding, position) => {
            let value = binding.borrow().clone();
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }.with_position(*position))
            } else {
                Ok(value)
            }
        }
        Expr::List(items, position) => {
            eval_list(items, *position, env, macro_env).map_err(|err| err.with_position(*position))
        }
    }
}

fn eval_list(
    items: &[Expr],
    position: SourcePos,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        });
    };

    let head_position = head.position();

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => {
                return eval_define(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "define-syntax" => {
                return macros::eval_define_syntax(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "if" => {
                return eval_if(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "quote" => {
                return eval_quote(&items[1..]).map_err(|err| err.with_position(head_position))
            }
            "lambda" => {
                return eval_lambda(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "case-lambda" => {
                return eval_case_lambda(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "define-record-type" => {
                return eval_define_record_type(&items[1..], env)
                    .map_err(|err| err.with_position(head_position))
            }
            "set!" => {
                return eval_set(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "and" => {
                return eval_and(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "or" => {
                return eval_or(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "begin" => {
                return eval_begin(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "cond" => {
                return eval_cond(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "let" => {
                return eval_let(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "letrec" => {
                return eval_letrec(&items[1..], env, macro_env, false)
                    .map_err(|err| err.with_position(head_position))
            }
            "letrec*" => {
                return eval_letrec(&items[1..], env, macro_env, true)
                    .map_err(|err| err.with_position(head_position))
            }
            "case" => {
                return eval_case(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            "do" => {
                return eval_do(&items[1..], env, macro_env)
                    .map_err(|err| err.with_position(head_position))
            }
            _ => {}
        }
    }

    if let Some(expanded) = macros::expand_macro_call(items, position, macro_env)
        .map_err(|err| err.with_position(head_position))?
    {
        return eval_expr(&expanded, env, macro_env);
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

    apply_callable(callable, &args).map_err(|err| err.with_position(head_position))
}

fn eval_define(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    if let [Expr::Symbol(name, _), value_expr] = args {
        let value = eval_expr(value_expr, env, macro_env)?;
        env_define(env, name.clone(), value);
        return Ok(Value::Void);
    }

    if let Some((Expr::List(signature, _), body)) = args.split_first() {
        let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "invalid function definition".into(),
            });
        };

        if body.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "function definition requires a body".into(),
            });
        }

        let (params, rest_param) = parse_param_list_items(params)?;
        let procedure = single_clause_procedure(params, rest_param, body.to_vec(), env, macro_env);

        env_define(env, name.clone(), procedure);
        return Ok(Value::Void);
    }

    Err(EvalError::SyntaxError {
        message: "invalid define".into(),
    })
}

fn eval_if(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    match args {
        [condition, consequent] => {
            if eval_expr(condition, env, macro_env)?.is_truthy() {
                eval_expr(consequent, env, macro_env)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, consequent, alternate] => {
            if eval_expr(condition, env, macro_env)?.is_truthy() {
                eval_expr(consequent, env, macro_env)
            } else {
                eval_expr(alternate, env, macro_env)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3 arguments".into(),
            got: args.len(),
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [datum] => quote_to_value(datum),
        _ => Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn quote_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => Ok(Value::Symbol(name.clone())),
        Expr::CapturedSymbol(name, _, _) => Ok(Value::Symbol(name.clone())),
        Expr::List(items, _) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_to_value(item)?);
            }
            Ok(Value::List(values))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a parameter list and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a body".into(),
        });
    }

    let (params, rest_param) = parse_param_list(params_expr)?;
    Ok(single_clause_procedure(
        params,
        rest_param,
        body.to_vec(),
        env,
        macro_env,
    ))
}

fn eval_case_lambda(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "case-lambda requires at least one clause".into(),
        });
    }

    let mut clauses = Vec::with_capacity(args.len());
    for clause in args {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clauses must be lists".into(),
            });
        };

        let Some((params_expr, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clause cannot be empty".into(),
            });
        };

        if body.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clause requires a body".into(),
            });
        }

        let (params, rest_param) = parse_param_list(params_expr)?;
        clauses.push(ProcedureClause::new(params, rest_param, body.to_vec()));
    }

    Ok(make_procedure(clauses, env, macro_env))
}

fn eval_set(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, position), value_expr] => {
            let value = eval_expr(value_expr, env, macro_env)?;
            if env_set(env, name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() }.with_position(*position))
            }
        }
        [Expr::CapturedSymbol(_, binding, _), value_expr] => {
            let value = eval_expr(value_expr, env, macro_env)?;
            *binding.borrow_mut() = value;
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::SyntaxError {
            message: "set! target must be a symbol".into(),
        }),
        _ => Err(EvalError::WrongArgCount {
            name: "set!".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        }),
    }
}

fn parse_param_list(params_expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match params_expr {
        Expr::List(items, _) => parse_param_list_items(items),
        Expr::Symbol(name, _) if name != "." => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(EvalError::SyntaxError {
            message: "parameter list must be a symbol or list of symbols".into(),
        }),
    }
}

fn parse_param_list_items(items: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_index = items
        .iter()
        .position(|item| matches!(item, Expr::Symbol(name, _) if name == "."));

    let Some(dot_index) = dot_index else {
        return Ok((parse_required_param_names(items)?, None));
    };

    if items[dot_index + 1..]
        .iter()
        .any(|item| matches!(item, Expr::Symbol(name, _) if name == "."))
        || dot_index + 2 != items.len()
    {
        return Err(EvalError::SyntaxError {
            message: "invalid dotted parameter list".into(),
        });
    }

    let params = parse_required_param_names(&items[..dot_index])?;
    let rest_param = parse_param_name(&items[dot_index + 1])?;
    Ok((params, Some(rest_param)))
}

fn parse_required_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        params.push(parse_param_name(item)?);
    }
    Ok(params)
}

fn parse_param_name(item: &Expr) -> Result<String, EvalError> {
    match item {
        Expr::Symbol(name, _) if name != "." => Ok(name.clone()),
        _ => Err(EvalError::SyntaxError {
            message: "parameter names must be symbols".into(),
        }),
    }
}

fn apply_callable(callable: Value, args: &[Value]) -> Result<Value, EvalError> {
    match callable {
        Value::Procedure(procedure) => apply_procedure(&procedure, args),
        Value::NativeProcedure(procedure) => procedure.apply(args),
        Value::Builtin(builtin) => builtin.apply(args),
        other => Err(EvalError::NotCallable {
            found: other.type_name().into(),
        }),
    }
}

fn apply_procedure(procedure: &Procedure, args: &[Value]) -> Result<Value, EvalError> {
    let clause = procedure
        .clauses
        .iter()
        .find(|clause| clause.matches_arity(args.len()))
        .ok_or_else(|| EvalError::WrongArgCount {
            name: if procedure.clauses.len() > 1 {
                "case-lambda".into()
            } else {
                "lambda".into()
            },
            expected: procedure_expected_arity(procedure),
            got: args.len(),
        })?;

    apply_procedure_clause(clause, &procedure.env, &procedure.macro_env, args)
}

fn procedure_expected_arity(procedure: &Procedure) -> String {
    if let [clause] = procedure.clauses.as_slice() {
        return clause.expected_arity();
    }

    let mut arities = Vec::with_capacity(procedure.clauses.len());
    for clause in &procedure.clauses {
        let expected = clause.expected_arity();
        if !arities.contains(&expected) {
            arities.push(expected);
        }
    }

    format!("one of {}", arities.join(", "))
}

fn apply_procedure_clause(
    clause: &ProcedureClause,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    args: &[Value],
) -> Result<Value, EvalError> {
    let required = clause.params.len();
    let call_env = Environment::new(Some(Rc::clone(env)));
    for (param, arg) in clause.params.iter().zip(args.iter().take(required)) {
        env_define(&call_env, param.clone(), arg.clone());
    }
    if let Some(rest_param) = &clause.rest_param {
        env_define(
            &call_env,
            rest_param.clone(),
            Value::List(args[required..].to_vec()),
        );
    }

    let call_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));
    eval_sequence(&clause.body, &call_env, &call_macro_env)
}

fn eval_and(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env, macro_env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env, macro_env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env, macro_env)
}

fn eval_cond(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "cond clauses must be lists".into(),
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "cond clause cannot be empty".into(),
            });
        };

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                });
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, macro_env)
            };
        }

        let test_value = eval_expr(test, env, macro_env)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, macro_env)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::List(bindings, _), body @ ..] => eval_plain_let(bindings, body, env, macro_env),
        [Expr::Symbol(name, _), Expr::List(bindings, _), body @ ..] => {
            eval_named_let(name, bindings, body, env, macro_env)
        }
        _ => Err(EvalError::SyntaxError {
            message: "invalid let".into(),
        }),
    }
}

fn eval_letrec(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    sequential: bool,
) -> Result<Value, EvalError> {
    let [Expr::List(bindings, _), body @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: if sequential {
                "invalid letrec*".into()
            } else {
                "invalid letrec".into()
            },
        });
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: if sequential {
                "letrec* requires a body".into()
            } else {
                "letrec requires a body".into()
            },
        });
    }

    let bindings = parse_binding_exprs(bindings, if sequential { "letrec*" } else { "letrec" })?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    let let_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for (name, _) in &bindings {
        env_define(&let_env, name.clone(), Value::Uninitialized);
    }

    if sequential {
        for (name, value_expr) in &bindings {
            let value = eval_expr(value_expr, &let_env, &let_macro_env)?;
            env_set(&let_env, name, value);
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for (_, value_expr) in &bindings {
            values.push(eval_expr(value_expr, &let_env, &let_macro_env)?);
        }

        for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
            env_set(&let_env, name, value);
        }
    }

    eval_sequence(body, &let_env, &let_macro_env)
}

fn eval_case(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        });
    };

    let key = eval_expr(key_expr, env, macro_env)?;

    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must be lists".into(),
            });
        };

        let Some((datums, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "case clause cannot be empty".into(),
            });
        };

        if matches!(datums, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                });
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, macro_env)
            };
        }

        let Expr::List(datums, _) = datums else {
            return Err(EvalError::SyntaxError {
                message: "case clause datums must be in a list".into(),
            });
        };

        for datum in datums {
            let datum = quote_to_value(datum)?;
            if values_eqv(&key, &datum) {
                return if body.is_empty() {
                    Ok(Value::Void)
                } else {
                    eval_sequence(body, env, macro_env)
                };
            }
        }
    }

    Ok(Value::Void)
}

fn eval_do(args: &[Expr], env: &EnvRef, macro_env: &MacroEnvRef) -> Result<Value, EvalError> {
    let [Expr::List(bindings, _), Expr::List(test_clause, _), body @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: "invalid do".into(),
        });
    };

    let Some((test_expr, result_exprs)) = test_clause.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "do test clause cannot be empty".into(),
        });
    };

    let bindings = parse_do_bindings(bindings)?;
    let do_env = Environment::new(Some(Rc::clone(env)));
    let do_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));

    for binding in &bindings {
        let value = eval_expr(binding.init_expr, env, macro_env)?;
        env_define(&do_env, binding.name.clone(), value);
    }

    loop {
        if eval_expr(test_expr, &do_env, &do_macro_env)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(result_exprs, &do_env, &do_macro_env)
            };
        }

        if !body.is_empty() {
            eval_sequence(body, &do_env, &do_macro_env)?;
        }

        let mut updates = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let value = if let Some(step_expr) = binding.step_expr {
                eval_expr(step_expr, &do_env, &do_macro_env)?
            } else {
                env_lookup(&do_env, &binding.name).ok_or_else(|| EvalError::UnboundVariable {
                    name: binding.name.clone(),
                })?
            };
            updates.push((binding.name.clone(), value));
        }

        for (name, value) in updates {
            env_set(&do_env, &name, value);
        }
    }
}

fn eval_plain_let(
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings, env, macro_env)?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    for (name, value) in bindings {
        env_define(&let_env, name, value);
    }

    let let_macro_env = MacroEnvironment::new(Some(Rc::clone(macro_env)));
    eval_sequence(body, &let_env, &let_macro_env)
}

fn eval_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings, env, macro_env)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let args = bindings
        .into_iter()
        .map(|(_, value)| value)
        .collect::<Vec<_>>();

    let let_env = Environment::new(Some(Rc::clone(env)));
    let procedure = single_clause_procedure(params, None, body.to_vec(), &let_env, macro_env);
    env_define(&let_env, name.to_string(), procedure.clone());

    apply_callable(procedure, &args)
}

fn parse_let_bindings(
    bindings: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Vec<(String, Value)>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: "let bindings must be lists".into(),
            });
        };

        let [Expr::Symbol(name, _), value_expr] = items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: "let bindings must be (name value) pairs".into(),
            });
        };

        parsed.push((name.clone(), eval_expr(value_expr, env, macro_env)?));
    }

    Ok(parsed)
}

fn parse_binding_exprs<'a>(
    bindings: &'a [Expr],
    form_name: &str,
) -> Result<Vec<(String, &'a Expr)>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: format!("{form_name} bindings must be lists"),
            });
        };

        let [Expr::Symbol(name, _), value_expr] = items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: format!("{form_name} bindings must be (name value) pairs"),
            });
        };

        parsed.push((name.clone(), value_expr));
    }

    Ok(parsed)
}

struct DoBinding<'a> {
    name: String,
    init_expr: &'a Expr,
    step_expr: Option<&'a Expr>,
}

fn parse_do_bindings(bindings: &[Expr]) -> Result<Vec<DoBinding<'_>>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: "do bindings must be lists".into(),
            });
        };

        match items.as_slice() {
            [Expr::Symbol(name, _), init_expr] => parsed.push(DoBinding {
                name: name.clone(),
                init_expr,
                step_expr: None,
            }),
            [Expr::Symbol(name, _), init_expr, step_expr] => parsed.push(DoBinding {
                name: name.clone(),
                init_expr,
                step_expr: Some(step_expr),
            }),
            _ => {
                return Err(EvalError::SyntaxError {
                    message: "do bindings must be (name init [step])".into(),
                });
            }
        }
    }

    Ok(parsed)
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
