mod builtin_helpers;
mod builtins;
pub mod error;
mod helpers;
mod macros;
mod number;
mod parser;
mod value_ops;

pub use error::{EvalError, SourcePos};

use self::builtins::builtin_name;
use self::helpers::{
    invalid_argument, make_immutable_string_value, make_string_value, make_vector_value,
    number_error, syntax_error, type_mismatch, wrong_arg_count,
};
use self::macros::{expand_macro_call, parse_syntax_rules};
use self::number::{parse_number_literal, Number, NumberError};
use self::value_ops::{
    eqv_value, render_char, render_list, render_pair, render_string, render_vector, value_type_name,
};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

const START_POS: SourcePos = SourcePos::new(1, 1);

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(pos: SourcePos, kind: ExprKind) -> Self {
        Self { kind, pos }
    }

    fn list_items(&self) -> Option<&[Expr]> {
        match &self.kind {
            ExprKind::List(items) => Some(items),
            _ => None,
        }
    }

    fn symbol_name(&self) -> Option<&str> {
        match &self.kind {
            ExprKind::Symbol(name) => Some(name.as_str()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(StringRef),
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<PairValue>),
    Vector(VectorRef),
    Record(Rc<RecordValue>),
    Procedure(Procedure),
    Void,
    Uninitialized,
}

#[derive(Debug, Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Debug, Clone)]
enum Procedure {
    Builtin(&'static str),
    Lambda(Rc<Lambda>),
    CaseLambda(Rc<CaseLambda>),
    RecordConstructor(Rc<RecordType>),
    RecordPredicate(Rc<RecordType>),
    RecordAccessor {
        record_type: Rc<RecordType>,
        field_index: usize,
        name: String,
    },
}

#[derive(Debug, Clone)]
struct LambdaParams {
    required: Vec<String>,
    rest: Option<String>,
}

#[derive(Debug)]
struct Lambda {
    name: Option<String>,
    params: LambdaParams,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Debug)]
struct CaseLambda {
    clauses: Vec<Rc<Lambda>>,
}

#[derive(Debug)]
struct RecordType {
    type_name: String,
    constructor_name: String,
    field_count: usize,
}

#[derive(Debug)]
struct RecordValue {
    record_type: Rc<RecordType>,
    fields: Vec<Value>,
}

#[derive(Debug)]
struct StringValue {
    value: RefCell<String>,
    mutable: bool,
}

type EnvRef = Rc<RefCell<Env>>;
type EnvWeak = Weak<RefCell<Env>>;
type VectorRef = Rc<RefCell<Vec<Value>>>;
type BindingRef = Rc<RefCell<Value>>;
type MacroRef = Rc<MacroTransformer>;

#[derive(Debug, Clone)]
struct StringRef(Rc<StringValue>);

#[derive(Debug, Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Debug, Clone)]
struct MacroTransformer {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvWeak,
}

#[derive(Debug, Clone)]
enum PatternBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

#[derive(Debug, Default)]
struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingRef>,
    syntax_bindings: HashMap<String, MacroRef>,
}

#[derive(Debug, Default)]
struct EvalContext {
    output: String,
    next_fresh: usize,
}

impl LambdaParams {
    fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }

    fn matches_arg_count(&self, arg_count: usize) -> bool {
        match self.rest {
            Some(_) => arg_count >= self.required.len(),
            None => arg_count == self.required.len(),
        }
    }

    fn expected_arg_count(&self) -> String {
        let required_len = self.required.len();
        match self.rest {
            Some(_) => format!("at least {required_len} arguments"),
            None => format!("exactly {required_len} arguments"),
        }
    }
}

impl StringRef {
    fn new(value: impl Into<String>, mutable: bool) -> Self {
        Self(Rc::new(StringValue {
            value: RefCell::new(value.into()),
            mutable,
        }))
    }

    fn borrow(&self) -> Ref<'_, String> {
        self.0.value.borrow()
    }

    fn borrow_mut(&self) -> RefMut<'_, String> {
        self.0.value.borrow_mut()
    }

    fn is_mutable(&self) -> bool {
        self.0.mutable
    }

    fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::Character(_) => "char",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(items) if items.is_empty() => "null",
            Self::List(_) => "pair",
            Self::Pair(_) => "pair",
            Self::Vector(_) => "vector",
            Self::Record(_) => "record",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
            Self::Uninitialized => "undefined",
        }
    }

    fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    fn render_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Number(value) => value.render(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::Character(value) => render_char(*value, mode),
            Self::String(value) => {
                let value = value.borrow();
                match mode {
                    RenderMode::Display => value.clone(),
                    RenderMode::Write => render_string(&value),
                }
            }
            Self::Symbol(value) => value.clone(),
            Self::List(items) => render_list(items, mode),
            Self::Pair(pair) => render_pair(&pair.car, &pair.cdr, mode),
            Self::Vector(values) => render_vector(&values.borrow(), mode),
            Self::Record(record) => format!("#<record {}>", record.record_type.type_name),
            Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
            Self::Uninitialized => "#<undefined>".into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum RenderMode {
    Display,
    Write,
}

impl Env {
    fn new_root() -> EnvRef {
        Rc::new(RefCell::new(Self::default()))
    }

    fn new_child(parent: &EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(Rc::clone(parent)),
            bindings: HashMap::new(),
            syntax_bindings: HashMap::new(),
        }))
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
    let exprs = parser::parse_program(input)?;
    let env = Env::new_root();
    let mut context = EvalContext::default();
    let result = eval_sequence(&exprs, &env, START_POS, &mut context)?;
    Ok(result.render())
}

fn eval_sequence(
    exprs: &[Expr],
    env: &EnvRef,
    empty_pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let mut last = None;

    for expr in exprs {
        last = Some(eval_expr(expr, env, context)?);
    }

    last.ok_or_else(|| syntax_error(empty_pos, "empty input"))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse_program(input)?;
    let env = Env::new_root();
    let mut context = EvalContext::default();
    let result = eval_sequence(&exprs, &env, START_POS, &mut context)?;
    Ok((result.render(), context.output))
}

fn eval_expr(expr: &Expr, env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Character(value) => Ok(Value::Character(*value)),
        ExprKind::String(value) => Ok(make_immutable_string_value(value.clone())),
        ExprKind::Symbol(name) => lookup_symbol(env, name, expr.pos),
        ExprKind::List(items) => eval_list(items, env, expr.pos, context),
    }
}

fn lookup_symbol(env: &EnvRef, name: &str, pos: SourcePos) -> Result<Value, EvalError> {
    if let Some(cell) = env_lookup_cell(env, name) {
        let value = cell.borrow().clone();
        return if matches!(value, Value::Uninitialized) {
            Err(EvalError::UninitializedBinding {
                pos,
                name: name.to_string(),
            })
        } else {
            Ok(value)
        };
    }

    if let Some(name) = builtin_name(name) {
        return Ok(Value::Procedure(Procedure::Builtin(name)));
    }

    Err(EvalError::UnboundVariable {
        pos,
        name: name.to_string(),
    })
}

fn env_lookup_cell(env: &EnvRef, name: &str) -> Option<BindingRef> {
    let mut current = Some(Rc::clone(env));

    while let Some(scope) = current {
        let (value, parent) = {
            let scope = scope.borrow();
            (
                scope.bindings.get(name).cloned(),
                scope.parent.as_ref().map(Rc::clone),
            )
        };

        if let Some(value) = value {
            return Some(value);
        }

        current = parent;
    }

    None
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    let mut scope = env.borrow_mut();

    if let Some(cell) = scope.bindings.get(&name) {
        *cell.borrow_mut() = value;
    } else {
        scope.bindings.insert(name, Rc::new(RefCell::new(value)));
    }
}

fn env_define_alias(env: &EnvRef, name: String, cell: BindingRef) {
    env.borrow_mut().bindings.insert(name, cell);
}

fn env_lookup_macro(env: &EnvRef, name: &str) -> Option<MacroRef> {
    let mut current = Some(Rc::clone(env));

    while let Some(scope) = current {
        let (value, parent) = {
            let scope = scope.borrow();
            (
                scope.syntax_bindings.get(name).cloned(),
                scope.parent.as_ref().map(Rc::clone),
            )
        };

        if let Some(value) = value {
            return Some(value);
        }

        current = parent;
    }

    None
}

fn env_define_macro(env: &EnvRef, name: String, transformer: MacroRef) {
    env.borrow_mut().syntax_bindings.insert(name, transformer);
}

fn env_set(env: &EnvRef, name: &str, value: Value) -> bool {
    if let Some(cell) = env_lookup_cell(env, name) {
        *cell.borrow_mut() = value;
        true
    } else {
        false
    }
}

fn eval_list(
    items: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(syntax_error(pos, "cannot evaluate empty list"));
    }

    if let Some(name) = items[0].symbol_name() {
        let form_pos = items[0].pos;
        match name {
            "begin" => return eval_begin(&items[1..], env, form_pos, context),
            "case" => return eval_case(&items[1..], env, form_pos, context),
            "case-lambda" => return eval_case_lambda(&items[1..], env, form_pos),
            "cond" => return eval_cond(&items[1..], env, context),
            "define" => return eval_define(&items[1..], env, form_pos, context),
            "define-record-type" => return eval_define_record_type(&items[1..], env, form_pos),
            "define-syntax" => return eval_define_syntax(&items[1..], env, form_pos),
            "do" => return eval_do(&items[1..], env, form_pos, context),
            "if" => return eval_if(&items[1..], env, form_pos, context),
            "let" => return eval_let(&items[1..], env, form_pos, context),
            "letrec" => return eval_letrec(&items[1..], env, form_pos, context, false),
            "letrec*" => return eval_letrec(&items[1..], env, form_pos, context, true),
            "quote" => return eval_quote(&items[1..], form_pos),
            "set!" => return eval_set(&items[1..], env, form_pos, context),
            "lambda" => return eval_lambda(None, &items[1..], env, form_pos),
            "and" => return eval_and(&items[1..], env, context),
            "or" => return eval_or(&items[1..], env, context),
            _ => {}
        }

        if let Some(transformer) = env_lookup_macro(env, name) {
            let expanded = expand_macro_call(&transformer, items, pos, env, context)?;
            return eval_expr(&expanded, env, context);
        }
    }

    let operator = eval_expr(&items[0], env, context)?;
    let args = items[1..]
        .iter()
        .map(|expr| eval_expr(expr, env, context))
        .collect::<Result<Vec<_>, _>>()?;

    apply(operator, &args, items[0].pos, context)
}

fn eval_define(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match args {
        [signature, body @ ..] if signature.list_items().is_some() => {
            let (name, params) = parse_define_signature(signature)?;
            let lambda = make_lambda(Some(name.clone()), params, body, env, pos)?;
            env_define(env, name, lambda);
            Ok(Value::Void)
        }
        [name_expr, value_expr] => {
            if let Some(name) = name_expr.symbol_name() {
                let value = eval_expr(value_expr, env, context)?;
                env_define(env, name.to_string(), value);
                Ok(Value::Void)
            } else {
                Err(syntax_error(pos, "invalid define form"))
            }
        }
        _ => Err(syntax_error(pos, "invalid define form")),
    }
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [name_expr, transformer_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "macro name must be a symbol"))?
                .to_string();
            let transformer = parse_syntax_rules(&name, transformer_expr, env)?;
            env_define_macro(env, name, transformer);
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count(
            pos,
            "define-syntax",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn eval_define_record_type(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let (type_name_expr, constructor_expr, predicate_expr, field_exprs) = match args {
        [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] => (
            type_name_expr,
            constructor_expr,
            predicate_expr,
            field_exprs,
        ),
        _ => return Err(syntax_error(pos, "invalid define-record-type form")),
    };

    let type_name = type_name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(type_name_expr.pos, "record type name must be a symbol"))?
        .to_string();
    let (constructor_name, constructor_field_count) = parse_record_constructor(constructor_expr)?;
    let predicate_name = predicate_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(predicate_expr.pos, "record predicate name must be a symbol"))?
        .to_string();
    let accessor_names = field_exprs
        .iter()
        .map(parse_record_field)
        .collect::<Result<Vec<_>, _>>()?;

    if constructor_field_count != accessor_names.len() {
        return Err(syntax_error(
            constructor_expr.pos,
            "record constructor arity must match field count",
        ));
    }

    let record_type = Rc::new(RecordType {
        type_name,
        constructor_name: constructor_name.clone(),
        field_count: accessor_names.len(),
    });

    env_define(
        env,
        constructor_name,
        Value::Procedure(Procedure::RecordConstructor(Rc::clone(&record_type))),
    );
    env_define(
        env,
        predicate_name,
        Value::Procedure(Procedure::RecordPredicate(Rc::clone(&record_type))),
    );

    for (field_index, accessor_name) in accessor_names.into_iter().enumerate() {
        env_define(
            env,
            accessor_name.clone(),
            Value::Procedure(Procedure::RecordAccessor {
                record_type: Rc::clone(&record_type),
                field_index,
                name: accessor_name,
            }),
        );
    }

    Ok(Value::Void)
}

fn eval_begin(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    eval_sequence(args, env, pos, context)
}

fn eval_set(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match args {
        [name_expr, value_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "set! target must be a symbol"))?;
            let value = eval_expr(value_expr, env, context)?;

            if env_set(env, name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable {
                    pos: name_expr.pos,
                    name: name.to_string(),
                })
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "set!",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn parse_define_signature(signature: &Expr) -> Result<(String, LambdaParams), EvalError> {
    let items = signature
        .list_items()
        .ok_or_else(|| syntax_error(signature.pos, "invalid define form"))?;

    let (name_expr, params) = items
        .split_first()
        .ok_or_else(|| syntax_error(signature.pos, "invalid define form"))?;

    let name = name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(name_expr.pos, "function name must be a symbol"))?
        .to_string();

    let params = parse_params(params)?;
    Ok((name, params))
}

fn parse_params(params: &[Expr]) -> Result<LambdaParams, EvalError> {
    let mut required = Vec::new();
    let mut rest = None;
    let mut iter = params.iter().peekable();

    while let Some(expr) = iter.next() {
        match expr.symbol_name() {
            Some(".") => {
                let rest_expr = iter
                    .next()
                    .ok_or_else(|| syntax_error(expr.pos, "rest parameter requires a name"))?;
                let rest_name = rest_expr
                    .symbol_name()
                    .filter(|name| *name != ".")
                    .ok_or_else(|| {
                        syntax_error(rest_expr.pos, "parameter name must be a symbol")
                    })?;

                if iter.next().is_some() {
                    return Err(syntax_error(expr.pos, "rest parameter must be last"));
                }

                rest = Some(rest_name.to_string());
                break;
            }
            Some(name) => required.push(name.to_string()),
            None => return Err(syntax_error(expr.pos, "parameter name must be a symbol")),
        }
    }

    Ok(LambdaParams { required, rest })
}

fn parse_record_constructor(constructor: &Expr) -> Result<(String, usize), EvalError> {
    let items = constructor
        .list_items()
        .ok_or_else(|| syntax_error(constructor.pos, "record constructor must be a list"))?;
    let (name_expr, params) = items
        .split_first()
        .ok_or_else(|| syntax_error(constructor.pos, "record constructor cannot be empty"))?;
    let name = name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(name_expr.pos, "record constructor name must be a symbol"))?
        .to_string();

    for param in params {
        if param.symbol_name().is_none() {
            return Err(syntax_error(
                param.pos,
                "record constructor parameter must be a symbol",
            ));
        }
    }

    Ok((name, params.len()))
}

fn parse_record_field(field: &Expr) -> Result<String, EvalError> {
    let items = field
        .list_items()
        .ok_or_else(|| syntax_error(field.pos, "record field must be a list"))?;

    match items {
        [name_expr, accessor_expr] => {
            if name_expr.symbol_name().is_none() {
                return Err(syntax_error(
                    name_expr.pos,
                    "record field name must be a symbol",
                ));
            }

            accessor_expr
                .symbol_name()
                .ok_or_else(|| {
                    syntax_error(accessor_expr.pos, "record accessor name must be a symbol")
                })
                .map(str::to_string)
        }
        _ => Err(syntax_error(
            field.pos,
            "record field must contain a name and accessor",
        )),
    }
}

fn eval_cond(
    clauses: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let items = clause
            .list_items()
            .ok_or_else(|| syntax_error(clause.pos, "cond clauses must be lists"))?;

        let (test, body) = items
            .split_first()
            .ok_or_else(|| syntax_error(clause.pos, "cond clause cannot be empty"))?;

        if test.symbol_name() == Some("else") {
            if index + 1 != clauses.len() {
                return Err(syntax_error(test.pos, "else clause must be last"));
            }
            return eval_sequence(body, env, clause.pos, context);
        }

        let test_value = eval_expr(test, env, context)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, clause.pos, context)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_case(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (key_expr, clauses) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "case requires a key and at least one clause"))?;

    if clauses.is_empty() {
        return Err(syntax_error(pos, "case requires at least one clause"));
    }

    let key = eval_expr(key_expr, env, context)?;

    for (index, clause) in clauses.iter().enumerate() {
        let items = clause
            .list_items()
            .ok_or_else(|| syntax_error(clause.pos, "case clauses must be lists"))?;
        let (head, body) = items
            .split_first()
            .ok_or_else(|| syntax_error(clause.pos, "case clause cannot be empty"))?;

        if head.symbol_name() == Some("else") {
            if index + 1 != clauses.len() {
                return Err(syntax_error(head.pos, "else clause must be last"));
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, clause.pos, context)
            };
        }

        let datums = head
            .list_items()
            .ok_or_else(|| syntax_error(head.pos, "case datums must be a list"))?;

        for datum in datums {
            if eqv_value(&key, &quote_expr(datum)?) {
                return if body.is_empty() {
                    Ok(Value::Void)
                } else {
                    eval_sequence(body, env, clause.pos, context)
                };
            }
        }
    }

    Ok(Value::Void)
}

fn eval_if(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match args {
        [condition, when_true] => {
            if eval_expr(condition, env, context)?.is_truthy() {
                eval_expr(when_true, env, context)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, when_true, when_false] => {
            if eval_expr(condition, env, context)?.is_truthy() {
                eval_expr(when_true, env, context)
            } else {
                eval_expr(when_false, env, context)
            }
        }
        _ => Err(wrong_arg_count(pos, "if", "2 or 3 arguments", args.len())),
    }
}

fn eval_do(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (bindings_expr, rest) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "do requires bindings and a test clause"))?;
    let (test_clause_expr, body) = rest
        .split_first()
        .ok_or_else(|| syntax_error(pos, "do requires a test clause"))?;

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "do bindings must be a list"))?;
    let test_clause = test_clause_expr
        .list_items()
        .ok_or_else(|| syntax_error(test_clause_expr.pos, "do test clause must be a list"))?;
    let (test_expr, exit_exprs) = test_clause
        .split_first()
        .ok_or_else(|| syntax_error(test_clause_expr.pos, "do test clause cannot be empty"))?;

    let bindings = bindings
        .iter()
        .map(parse_do_binding)
        .collect::<Result<Vec<_>, _>>()?;
    let init_values = bindings
        .iter()
        .map(|(_, init, _)| eval_expr(init, env, context))
        .collect::<Result<Vec<_>, _>>()?;

    let do_env = Env::new_child(env);
    let mut cells = Vec::with_capacity(bindings.len());

    for ((name, _, _), value) in bindings.iter().zip(init_values.into_iter()) {
        env_define(&do_env, name.clone(), value);
        let cell = env_lookup_cell(&do_env, name).expect("binding defined in current scope");
        cells.push(cell);
    }

    loop {
        if eval_expr(test_expr, &do_env, context)?.is_truthy() {
            return if exit_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(exit_exprs, &do_env, test_clause_expr.pos, context)
            };
        }

        if !body.is_empty() {
            let _ = eval_sequence(body, &do_env, pos, context)?;
        }

        let next_values = bindings
            .iter()
            .zip(cells.iter())
            .map(|((_, _, step), cell)| match step {
                Some(step) => eval_expr(step, &do_env, context),
                None => Ok(cell.borrow().clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (cell, value) in cells.iter().zip(next_values.into_iter()) {
            *cell.borrow_mut() = value;
        }
    }
}

fn eval_let(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (head, rest) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "let requires bindings and a body"))?;

    if let Some(bindings) = head.list_items() {
        eval_plain_let(bindings, rest, env, pos, context)
    } else if let Some(name) = head.symbol_name() {
        eval_named_let(name, rest, env, head.pos, context)
    } else {
        Err(syntax_error(head.pos, "invalid let form"))
    }
}

fn eval_plain_let(
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "let requires a body"));
    }

    let bindings = eval_bindings(bindings, env, context)?;
    let let_env = Env::new_child(env);

    for (name, value) in bindings {
        env_define(&let_env, name, value);
    }

    eval_sequence(body, &let_env, pos, context)
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "named let requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "named let requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "let bindings must be a list"))?;
    let bindings = eval_bindings(bindings, env, context)?;

    let (params, values): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let let_env = Env::new_child(env);
    let lambda = make_lambda(
        Some(name.to_string()),
        LambdaParams::fixed(params),
        body,
        &let_env,
        pos,
    )?;
    env_define(&let_env, name.to_string(), lambda.clone());
    apply(lambda, &values, pos, context)
}

fn eval_letrec(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
    sequential: bool,
) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "letrec requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "letrec requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "letrec bindings must be a list"))?;
    let bindings = bindings
        .iter()
        .map(|binding| parse_value_binding(binding, "letrec"))
        .collect::<Result<Vec<_>, _>>()?;

    let letrec_env = Env::new_child(env);
    for (name, _) in &bindings {
        env_define(&letrec_env, name.clone(), Value::Uninitialized);
    }

    if sequential {
        for (name, value_expr) in &bindings {
            let value = eval_expr(value_expr, &letrec_env, context)?;
            env_set(&letrec_env, name, value);
        }
    } else {
        let values = bindings
            .iter()
            .map(|(_, value_expr)| eval_expr(value_expr, &letrec_env, context))
            .collect::<Result<Vec<_>, _>>()?;

        for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
            env_set(&letrec_env, name, value);
        }
    }

    eval_sequence(body, &letrec_env, pos, context)
}

fn eval_bindings(
    bindings: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Vec<(String, Value)>, EvalError> {
    bindings
        .iter()
        .map(|binding| {
            let items = binding
                .list_items()
                .ok_or_else(|| syntax_error(binding.pos, "let bindings must be lists"))?;

            match items {
                [name_expr, value_expr] => {
                    let name = name_expr
                        .symbol_name()
                        .ok_or_else(|| {
                            syntax_error(
                                binding.pos,
                                "each let binding must contain a name and value",
                            )
                        })?
                        .to_string();
                    let value = eval_expr(value_expr, env, context)?;
                    Ok((name, value))
                }
                _ => Err(syntax_error(
                    binding.pos,
                    "each let binding must contain a name and value",
                )),
            }
        })
        .collect()
}

fn parse_value_binding(binding: &Expr, form_name: &str) -> Result<(String, Expr), EvalError> {
    let items = binding
        .list_items()
        .ok_or_else(|| syntax_error(binding.pos, format!("{form_name} bindings must be lists")))?;

    match items {
        [name_expr, value_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "binding name must be a symbol"))?
                .to_string();
            Ok((name, value_expr.clone()))
        }
        _ => Err(syntax_error(
            binding.pos,
            format!("each {form_name} binding must contain a name and value"),
        )),
    }
}

fn parse_do_binding(binding: &Expr) -> Result<(String, Expr, Option<Expr>), EvalError> {
    let items = binding
        .list_items()
        .ok_or_else(|| syntax_error(binding.pos, "do bindings must be lists"))?;

    match items {
        [name_expr, init_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "do binding name must be a symbol"))?
                .to_string();
            Ok((name, init_expr.clone(), None))
        }
        [name_expr, init_expr, step_expr] => {
            let name = name_expr
                .symbol_name()
                .ok_or_else(|| syntax_error(name_expr.pos, "do binding name must be a symbol"))?
                .to_string();
            Ok((name, init_expr.clone(), Some(step_expr.clone())))
        }
        _ => Err(syntax_error(
            binding.pos,
            "each do binding must contain a name, init, and optional step",
        )),
    }
}

fn eval_quote(args: &[Expr], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [expr] => quote_expr(expr),
        _ => Err(wrong_arg_count(
            pos,
            "quote",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Character(value) => Ok(Value::Character(*value)),
        ExprKind::String(value) => Ok(make_immutable_string_value(value.clone())),
        ExprKind::Symbol(value) => Ok(Value::Symbol(value.clone())),
        ExprKind::List(items) => items
            .iter()
            .map(quote_expr)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::List),
    }
}

fn eval_lambda(
    name: Option<String>,
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let (params_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "lambda requires parameters and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    let params = params_expr
        .list_items()
        .ok_or_else(|| syntax_error(params_expr.pos, "lambda parameters must be a list"))?;
    let params = parse_params(params)?;

    make_lambda(name, params, body, env, pos)
}

fn eval_case_lambda(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(syntax_error(
            pos,
            "case-lambda requires at least one clause",
        ));
    }

    let clauses = args
        .iter()
        .map(|clause| parse_case_lambda_clause(clause, env))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Procedure(Procedure::CaseLambda(Rc::new(
        CaseLambda { clauses },
    ))))
}

fn parse_case_lambda_clause(clause: &Expr, env: &EnvRef) -> Result<Rc<Lambda>, EvalError> {
    let items = clause
        .list_items()
        .ok_or_else(|| syntax_error(clause.pos, "case-lambda clauses must be lists"))?;
    let (params_expr, body) = items
        .split_first()
        .ok_or_else(|| syntax_error(clause.pos, "case-lambda clause cannot be empty"))?;

    if body.is_empty() {
        return Err(syntax_error(
            clause.pos,
            "case-lambda clause requires a body",
        ));
    }

    let params = params_expr.list_items().ok_or_else(|| {
        syntax_error(
            params_expr.pos,
            "case-lambda clause parameters must be a list",
        )
    })?;
    let params = parse_params(params)?;

    Ok(Rc::new(Lambda {
        name: None,
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    }))
}

fn make_lambda(
    name: Option<String>,
    params: LambdaParams,
    body: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    Ok(Value::Procedure(Procedure::Lambda(Rc::new(Lambda {
        name,
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    }))))
}

fn eval_and(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env, context)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env, context)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn apply(
    operator: Value,
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(Procedure::Builtin(name)) => {
            builtins::apply_builtin(name, args, pos, context)
        }
        Value::Procedure(Procedure::Lambda(lambda)) => apply_lambda(lambda, args, pos, context),
        Value::Procedure(Procedure::CaseLambda(case_lambda)) => {
            apply_case_lambda(case_lambda, args, pos, context)
        }
        Value::Procedure(Procedure::RecordConstructor(record_type)) => {
            apply_record_constructor(record_type, args, pos)
        }
        Value::Procedure(Procedure::RecordPredicate(record_type)) => {
            apply_record_predicate(record_type, args, pos)
        }
        Value::Procedure(Procedure::RecordAccessor {
            record_type,
            field_index,
            name,
        }) => apply_record_accessor(record_type, field_index, &name, args, pos),
        other => Err(EvalError::NotAProcedure {
            pos,
            found: other.render(),
        }),
    }
}

fn apply_lambda(
    lambda: Rc<Lambda>,
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    if !lambda.params.matches_arg_count(args.len()) {
        return Err(wrong_arg_count(
            pos,
            lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            lambda.params.expected_arg_count(),
            args.len(),
        ));
    }

    let required_len = lambda.params.required.len();
    let call_env = Env::new_child(&lambda.env);
    for (param, value) in lambda.params.required.iter().zip(args.iter()) {
        env_define(&call_env, param.clone(), value.clone());
    }

    if let Some(rest) = &lambda.params.rest {
        env_define(
            &call_env,
            rest.clone(),
            Value::List(args[required_len..].to_vec()),
        );
    }

    eval_sequence(&lambda.body, &call_env, pos, context)
}

fn apply_case_lambda(
    case_lambda: Rc<CaseLambda>,
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    if let Some(clause) = case_lambda
        .clauses
        .iter()
        .find(|clause| clause.params.matches_arg_count(args.len()))
    {
        return apply_lambda(Rc::clone(clause), args, pos, context);
    }

    let expected = case_lambda
        .clauses
        .iter()
        .map(|clause| clause.params.expected_arg_count())
        .collect::<Vec<_>>()
        .join(" or ");

    Err(wrong_arg_count(
        pos,
        "case-lambda",
        format!("one of {expected}"),
        args.len(),
    ))
}

fn apply_record_constructor(
    record_type: Rc<RecordType>,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if args.len() != record_type.field_count {
        return Err(wrong_arg_count(
            pos,
            record_type.constructor_name.clone(),
            format!("exactly {} arguments", record_type.field_count),
            args.len(),
        ));
    }

    Ok(Value::Record(Rc::new(RecordValue {
        record_type,
        fields: args.to_vec(),
    })))
}

fn apply_record_predicate(
    record_type: Rc<RecordType>,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    match args {
        [Value::Record(record)] => Ok(Value::Boolean(Rc::ptr_eq(
            &record.record_type,
            &record_type,
        ))),
        [_] => Ok(Value::Boolean(false)),
        _ => Err(wrong_arg_count(
            pos,
            "record predicate",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn apply_record_accessor(
    record_type: Rc<RecordType>,
    field_index: usize,
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    match args {
        [Value::Record(record)] if Rc::ptr_eq(&record.record_type, &record_type) => {
            Ok(record.fields[field_index].clone())
        }
        [other] => Err(type_mismatch(
            pos,
            name,
            record_type.type_name.clone(),
            value_type_name(other),
        )),
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}

#[cfg(test)]
mod tests;
