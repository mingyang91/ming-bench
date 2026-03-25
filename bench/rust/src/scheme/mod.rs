pub mod error;

pub use error::{EvalError, SourcePos};

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

const BUILTIN_NAMES: &[&str] = &[
    "abs",
    "+",
    "-",
    "*",
    "/",
    "<",
    "<=",
    "=",
    ">",
    ">=",
    "char?",
    "display",
    "eq?",
    "equal?",
    "even?",
    "expt",
    "not",
    "newline",
    "number->string",
    "append",
    "apply",
    "assoc",
    "boolean?",
    "car",
    "char-alphabetic?",
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
    "map",
    "max",
    "min",
    "modulo",
    "negative?",
    "null?",
    "number?",
    "odd?",
    "pair?",
    "positive?",
    "quotient",
    "remainder",
    "string-append",
    "string?",
    "string-ci=?",
    "string-copy",
    "string-downcase",
    "string=?",
    "string<?",
    "string-length",
    "string->number",
    "string->symbol",
    "string-ref",
    "string-set!",
    "string-upcase",
    "substring",
    "symbol?",
    "symbol->string",
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

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

#[derive(Clone, Debug, PartialEq)]
enum ExprKind {
    Integer(i64),
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
struct UserProcedure {
    name: Option<String>,
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

impl UserProcedure {
    fn display_name(&self) -> &str {
        self.name.as_deref().unwrap_or("lambda")
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

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        Self::lookup_binding(env, name).map(|binding| binding.borrow().clone())
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

#[derive(Clone)]
enum Value {
    Integer(i64),
    Bool(bool),
    Char(char),
    String(SchemeString),
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<Pair>),
    Builtin(&'static str),
    Procedure(Rc<UserProcedure>),
    Void,
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Bool(_) => "boolean",
            Value::Char(_) => "char",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
            Value::Pair(_) => "pair",
            Value::Builtin(_) | Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false))
    }

    fn as_number(&self, position: SourcePos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(value) => Ok(*value),
            other => Err(EvalError::type_mismatch(
                "number",
                other.type_name(),
                position,
            )),
        }
    }

    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
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
            Value::Builtin(_) | Value::Procedure(_) => "#<procedure>".into(),
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

    fn as_number(&self) -> Result<i64, EvalError> {
        self.value.as_number(self.position)
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

fn scheme_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.chars, &right.chars),
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn scheme_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
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
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(Value::String(SchemeString::immutable(value))),
        ExprKind::Symbol(name) => Environment::lookup(&env, name)
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
            "set!" => eval_set(tail, env, head.pos),
            "if" => eval_if(tail, env, head.pos),
            "quote" => eval_quote(tail, head.pos),
            "lambda" => eval_lambda(tail, env, head.pos),
            "and" => eval_and(tail, env),
            "or" => eval_or(tail, env),
            "begin" => eval_begin(tail, env),
            "cond" => eval_cond(tail, env, head.pos),
            "let" => eval_let(tail, env, head.pos),
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
            let procedure = Value::Procedure(Rc::new(UserProcedure {
                name: Some(name.clone()),
                params,
                rest_param,
                body: body.to_vec(),
                env: env.clone(),
            }));

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
    let [condition, then_branch, else_branch] = exprs else {
        return Err(EvalError::syntax(
            "if requires exactly 3 expressions",
            position,
        ));
    };

    if eval(condition, env.clone())?.is_truthy() {
        eval(then_branch, env)
    } else {
        eval(else_branch, env)
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

    Ok(Value::Procedure(Rc::new(UserProcedure {
        name: None,
        params,
        rest_param,
        body: body.to_vec(),
        env,
    })))
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

    let procedure = Value::Procedure(Rc::new(UserProcedure {
        name: Some(name.into()),
        params,
        rest_param: None,
        body: body.to_vec(),
        env: let_env.clone(),
    }));

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

fn apply(
    callable: Value,
    position: SourcePos,
    args: Vec<LocatedValue>,
    env: EnvRef,
) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(name) => apply_builtin(name, &args, position, env),
        Value::Procedure(procedure) => apply_user_procedure(procedure, args, position),
        other => Err(EvalError::not_callable(other.type_name(), position)),
    }
}

fn apply_user_procedure(
    procedure: Rc<UserProcedure>,
    args: Vec<LocatedValue>,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let required = procedure.params.len();
    let invalid_arity = if procedure.rest_param.is_some() {
        args.len() < required
    } else {
        args.len() != required
    };

    if invalid_arity {
        return Err(EvalError::wrong_arg_count(
            procedure.display_name(),
            if procedure.rest_param.is_some() {
                format!("at least {required}")
            } else {
                format!("exactly {required}")
            },
            args.len(),
            position,
        ));
    }

    let call_env = Environment::child(procedure.env.clone());
    for (param, value) in procedure.params.iter().cloned().zip(args.iter().cloned()) {
        Environment::define(&call_env, param, value.value);
    }

    if let Some(rest_param) = &procedure.rest_param {
        let rest_values = args[required..]
            .iter()
            .map(|arg| arg.value.clone())
            .collect();
        Environment::define(&call_env, rest_param.clone(), Value::List(rest_values));
    }

    eval_sequence(&procedure.body, call_env)
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
        "<" => apply_compare(name, args, position, |left, right| left < right),
        "<=" => apply_compare(name, args, position, |left, right| left <= right),
        "=" => apply_compare(name, args, position, |left, right| left == right),
        ">" => apply_compare(name, args, position, |left, right| left > right),
        ">=" => apply_compare(name, args, position, |left, right| left >= right),
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
        "equal?" => apply_equality_predicate("equal?", args, position, scheme_equal),
        "even?" => apply_number_predicate("even?", args, position, |value| value % 2 == 0),
        "expt" => apply_expt(args, position),
        "length" => apply_length(args, position),
        "list" => Ok(Value::List(
            args.iter().map(|arg| arg.value.clone()).collect(),
        )),
        "list?" => apply_list_predicate(args, position),
        "list-ref" => apply_list_ref(args, position),
        "list-tail" => apply_list_tail(args, position),
        "map" => apply_map(args, position, env),
        "max" => apply_min_max("max", args, position, |left, right| left > right),
        "min" => apply_min_max("min", args, position, |left, right| left < right),
        "modulo" => apply_modulo(args, position),
        "negative?" => apply_number_predicate("negative?", args, position, |value| value < 0),
        "newline" => apply_newline(args, position, env),
        "null?" => apply_null(args, position),
        "not" => apply_not(args, position),
        "number->string" => apply_number_to_string(args, position),
        "number?" => apply_type_predicate("number?", args, position, |value| {
            matches!(value, Value::Integer(_))
        }),
        "odd?" => apply_number_predicate("odd?", args, position, |value| value % 2 != 0),
        "pair?" => apply_type_predicate("pair?", args, position, |value| {
            matches!(value, Value::List(values) if !values.is_empty())
                || matches!(value, Value::Pair(_))
        }),
        "positive?" => apply_number_predicate("positive?", args, position, |value| value > 0),
        "quotient" => apply_quotient(args, position),
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
        "write" => apply_write(args, position, env),
        "zero?" => apply_number_predicate("zero?", args, position, |value| value == 0),
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

    Ok(Value::Integer(
        args[0]
            .as_number()?
            .checked_abs()
            .ok_or_else(|| EvalError::integer_overflow(position))?,
    ))
}

fn apply_add(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let mut total = 0_i64;

    for arg in args {
        total = total
            .checked_add(arg.as_number()?)
            .ok_or_else(|| EvalError::integer_overflow(position))?;
    }

    Ok(Value::Integer(total))
}

fn apply_sub(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::wrong_arg_count("-", "at least 1", 0, position));
    };

    let first = first.as_number()?;

    if rest.is_empty() {
        return Ok(Value::Integer(
            first
                .checked_neg()
                .ok_or_else(|| EvalError::integer_overflow(position))?,
        ));
    }

    let mut total = first;
    for arg in rest {
        total = total
            .checked_sub(arg.as_number()?)
            .ok_or_else(|| EvalError::integer_overflow(position))?;
    }

    Ok(Value::Integer(total))
}

fn apply_mul(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let mut total = 1_i64;

    for arg in args {
        total = total
            .checked_mul(arg.as_number()?)
            .ok_or_else(|| EvalError::integer_overflow(position))?;
    }

    Ok(Value::Integer(total))
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
        let divisor = arg.as_number()?;
        if divisor == 0 {
            return Err(EvalError::division_by_zero(arg.position));
        }
        total = total
            .checked_div(divisor)
            .ok_or_else(|| EvalError::integer_overflow(position))?;
    }

    Ok(Value::Integer(total))
}

fn expect_binary_numbers(
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

    Ok((args[0].as_number()?, args[1].as_number()?))
}

fn apply_quotient(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (left, right) = expect_binary_numbers("quotient", args, position)?;

    if right == 0 {
        return Err(EvalError::division_by_zero(args[1].position));
    }

    Ok(Value::Integer(
        left.checked_div(right)
            .ok_or_else(|| EvalError::integer_overflow(position))?,
    ))
}

fn apply_remainder(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (left, right) = expect_binary_numbers("remainder", args, position)?;

    if right == 0 {
        return Err(EvalError::division_by_zero(args[1].position));
    }

    Ok(Value::Integer(
        left.checked_rem(right)
            .ok_or_else(|| EvalError::integer_overflow(position))?,
    ))
}

fn apply_modulo(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (left, right) = expect_binary_numbers("modulo", args, position)?;

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
    F: Fn(i64, i64) -> bool,
{
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::wrong_arg_count(name, "at least 1", 0, position));
    };

    let mut best = first.as_number()?;
    for arg in rest {
        let value = arg.as_number()?;
        if choose_new(value, best) {
            best = value;
        }
    }

    Ok(Value::Integer(best))
}

fn apply_expt(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let (base, exponent) = expect_binary_numbers("expt", args, position)?;

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
    F: Fn(i64, i64) -> bool,
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
        if !predicate(left, right) {
            return Ok(Value::Bool(false));
        }
        left = right;
    }

    Ok(Value::Bool(true))
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
        &args[0].as_number()?.to_string(),
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

    Ok(Value::String(args[0].as_string()?.mutable_copy()))
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
        match args[0].as_string()?.to_plain_string().parse::<i64>() {
            Ok(value) => Value::Integer(value),
            Err(_) => Value::Bool(false),
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
    let start = args[1].as_number()?;
    let end = args[2].as_number()?;

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
    let index = args[1].as_number()?;

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
    let index = args[1].as_number()?;

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
    let index = args[1].as_number()?;
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
    let index = args[1].as_number()?;
    if index < 0 || index as usize > values.len() {
        return Err(EvalError::index_out_of_bounds(
            index,
            values.len(),
            args[1].position,
        ));
    }

    Ok(Value::List(values[index as usize..].to_vec()))
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

    Ok(Value::Bool(predicate(args[0].as_number()?)))
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
    let next = GENERATED_SYMBOL_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__macro_{kind}_{next}")
}

fn is_core_syntax_keyword(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "begin"
            | "cond"
            | "define"
            | "define-syntax"
            | "if"
            | "lambda"
            | "let"
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
            _ if is_integer_token(&token) => {
                let value = token.parse().map_err(|_| {
                    EvalError::syntax(format!("invalid integer literal: {token}"), position)
                })?;
                Ok(Expr::new(ExprKind::Integer(value), position))
            }
            _ => Ok(Expr::new(ExprKind::Symbol(token), position)),
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
