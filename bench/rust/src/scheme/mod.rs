pub mod error;
mod parser;

pub use error::{EvalError, SourcePos};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const START_POS: SourcePos = SourcePos::new(1, 1);

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
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
    Integer(i64),
    Boolean(bool),
    Character(char),
    String(StringRef),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Procedure),
    Void,
}

#[derive(Debug, Clone)]
enum Procedure {
    Builtin(&'static str),
    Lambda(Rc<Lambda>),
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

type EnvRef = Rc<RefCell<Env>>;
type StringRef = Rc<RefCell<String>>;

#[derive(Debug, Default)]
struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

#[derive(Debug, Default)]
struct EvalContext {
    output: String,
}

impl LambdaParams {
    fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::Character(_) => "char",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(items) if items.is_empty() => "null",
            Self::List(_) => "pair",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
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
            Self::Integer(value) => value.to_string(),
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
            Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
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
        }))
    }
}

fn syntax_error(pos: SourcePos, message: impl Into<String>) -> EvalError {
    EvalError::Syntax {
        pos,
        message: message.into(),
    }
}

fn wrong_arg_count(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    got: usize,
) -> EvalError {
    EvalError::WrongArgCount {
        pos,
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn type_mismatch(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    found: impl Into<String>,
) -> EvalError {
    EvalError::TypeMismatch {
        pos,
        name: name.into(),
        expected: expected.into(),
        found: found.into(),
    }
}

fn invalid_argument(
    pos: SourcePos,
    name: impl Into<String>,
    message: impl Into<String>,
) -> EvalError {
    EvalError::InvalidArgument {
        pos,
        name: name.into(),
        message: message.into(),
    }
}

fn make_string_value(value: impl Into<String>) -> Value {
    Value::String(Rc::new(RefCell::new(value.into())))
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
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Character(value) => Ok(Value::Character(*value)),
        ExprKind::String(value) => Ok(make_string_value(value.clone())),
        ExprKind::Symbol(name) => lookup_symbol(env, name, expr.pos),
        ExprKind::List(items) => eval_list(items, env, expr.pos, context),
    }
}

fn lookup_symbol(env: &EnvRef, name: &str, pos: SourcePos) -> Result<Value, EvalError> {
    if let Some(value) = env_lookup(env, name) {
        return Ok(value);
    }

    if let Some(name) = builtin_name(name) {
        return Ok(Value::Procedure(Procedure::Builtin(name)));
    }

    Err(EvalError::UnboundVariable {
        pos,
        name: name.to_string(),
    })
}

fn env_lookup(env: &EnvRef, name: &str) -> Option<Value> {
    let mut current = Some(Rc::clone(env));

    while let Some(scope) = current {
        let (value, parent) = {
            let scope = scope.borrow();
            (
                scope.bindings.get(name).cloned(),
                scope.parent.as_ref().map(Rc::clone),
            )
        };

        if value.is_some() {
            return value;
        }

        current = parent;
    }

    None
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut().bindings.insert(name, value);
}

fn env_set(env: &EnvRef, name: &str, value: Value) -> bool {
    let mut current = Some(Rc::clone(env));
    let mut value = Some(value);

    while let Some(scope) = current {
        let parent = {
            let mut scope = scope.borrow_mut();
            if let Some(slot) = scope.bindings.get_mut(name) {
                *slot = value
                    .take()
                    .expect("environment update value should only be consumed once");
                return true;
            }
            scope.parent.as_ref().map(Rc::clone)
        };

        current = parent;
    }

    false
}

fn builtin_name(name: &str) -> Option<&'static str> {
    match name {
        "+" => Some("+"),
        "-" => Some("-"),
        "*" => Some("*"),
        "/" => Some("/"),
        "<" => Some("<"),
        ">" => Some(">"),
        "=" => Some("="),
        "<=" => Some("<="),
        "apply" => Some("apply"),
        "append" => Some("append"),
        "boolean?" => Some("boolean?"),
        "char?" => Some("char?"),
        "car" => Some("car"),
        "cdr" => Some("cdr"),
        "cons" => Some("cons"),
        "display" => Some("display"),
        "length" => Some("length"),
        "list" => Some("list"),
        "newline" => Some("newline"),
        "number->string" => Some("number->string"),
        "not" => Some("not"),
        "null?" => Some("null?"),
        "number?" => Some("number?"),
        "pair?" => Some("pair?"),
        "string->number" => Some("string->number"),
        "string->symbol" => Some("string->symbol"),
        "string-append" => Some("string-append"),
        "string-copy" => Some("string-copy"),
        "string-length" => Some("string-length"),
        "string-ref" => Some("string-ref"),
        "string-set!" => Some("string-set!"),
        "string?" => Some("string?"),
        "substring" => Some("substring"),
        "symbol->string" => Some("symbol->string"),
        "symbol?" => Some("symbol?"),
        "write" => Some("write"),
        _ => None,
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
            "cond" => return eval_cond(&items[1..], env, context),
            "define" => return eval_define(&items[1..], env, form_pos, context),
            "if" => return eval_if(&items[1..], env, form_pos, context),
            "let" => return eval_let(&items[1..], env, form_pos, context),
            "quote" => return eval_quote(&items[1..], form_pos),
            "set!" => return eval_set(&items[1..], env, form_pos, context),
            "lambda" => return eval_lambda(None, &items[1..], env, form_pos),
            "and" => return eval_and(&items[1..], env, context),
            "or" => return eval_or(&items[1..], env, context),
            _ => {}
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

fn eval_if(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match args {
        [condition, when_true, when_false] => {
            if eval_expr(condition, env, context)?.is_truthy() {
                eval_expr(when_true, env, context)
            } else {
                eval_expr(when_false, env, context)
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "if",
            "exactly 3 arguments",
            args.len(),
        )),
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
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Character(value) => Ok(Value::Character(*value)),
        ExprKind::String(value) => Ok(make_string_value(value.clone())),
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
        Value::Procedure(Procedure::Builtin(name)) => apply_builtin(name, args, pos, context),
        Value::Procedure(Procedure::Lambda(lambda)) => apply_lambda(lambda, args, pos, context),
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
    let required_len = lambda.params.required.len();
    let wrong_arity = match lambda.params.rest {
        Some(_) => args.len() < required_len,
        None => args.len() != required_len,
    };

    if wrong_arity {
        let expected = match lambda.params.rest {
            Some(_) => format!("at least {required_len} arguments"),
            None => format!("exactly {required_len} arguments"),
        };

        return Err(wrong_arg_count(
            pos,
            lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            expected,
            args.len(),
        ));
    }

    let call_env = Env::new_child(&lambda.env);
    for (param, value) in lambda.params.required.iter().zip(args.iter()) {
        env_define(&call_env, param.clone(), value.clone());
    }

    if let Some(rest) = &lambda.params.rest {
        env_define(&call_env, rest.clone(), Value::List(args[required_len..].to_vec()));
    }

    eval_sequence(&lambda.body, &call_env, pos, context)
}

fn apply_builtin(
    name: &str,
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match name {
        "+" => add(args, pos),
        "-" => subtract(args, pos),
        "*" => multiply(args, pos),
        "/" => divide(args, pos),
        "<" => compare(name, args, pos, |left, right| left < right),
        ">" => compare(name, args, pos, |left, right| left > right),
        "=" => compare(name, args, pos, |left, right| left == right),
        "<=" => compare(name, args, pos, |left, right| left <= right),
        "apply" => builtin_apply(args, pos, context),
        "append" => append(args, pos),
        "boolean?" => predicate(args, "boolean?", pos, |value| {
            matches!(value, Value::Boolean(_))
        }),
        "char?" => predicate(args, "char?", pos, |value| {
            matches!(value, Value::Character(_))
        }),
        "car" => car(args, pos),
        "cdr" => cdr(args, pos),
        "cons" => cons(args, pos),
        "display" => display(args, pos, context),
        "length" => length(args, pos),
        "list" => Ok(Value::List(args.to_vec())),
        "newline" => newline(args, pos, context),
        "number->string" => number_to_string(args, pos),
        "not" => builtin_not(args, pos),
        "null?" => predicate(
            args,
            "null?",
            pos,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "number?" => predicate(args, "number?", pos, |value| {
            matches!(value, Value::Integer(_))
        }),
        "pair?" => predicate(
            args,
            "pair?",
            pos,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        "string->number" => string_to_number(args, pos),
        "string->symbol" => string_to_symbol(args, pos),
        "string-append" => string_append(args, pos),
        "string-copy" => string_copy(args, pos),
        "string-length" => string_length(args, pos),
        "string-ref" => string_ref(args, pos),
        "string-set!" => string_set(args, pos),
        "string?" => predicate(args, "string?", pos, |value| {
            matches!(value, Value::String(_))
        }),
        "substring" => substring(args, pos),
        "symbol->string" => symbol_to_string(args, pos),
        "symbol?" => predicate(args, "symbol?", pos, |value| {
            matches!(value, Value::Symbol(_))
        }),
        "write" => write(args, pos, context),
        _ => unreachable!("unsupported builtin: {name}"),
    }
}

fn add(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("+", args, pos)?;
    Ok(Value::Integer(values.into_iter().sum()))
}

fn subtract(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("-", args, pos)?;
    match values.as_slice() {
        [] => Err(wrong_arg_count(pos, "-", "at least 1 argument", 0)),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn multiply(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("*", args, pos)?;
    Ok(Value::Integer(values.into_iter().product()))
}

fn divide(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("/", args, pos)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| wrong_arg_count(pos, "/", "at least 2 arguments", 0))?;

    if rest.is_empty() {
        return Err(wrong_arg_count(pos, "/", "at least 2 arguments", 1));
    }

    let mut total = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero { pos });
        }
        total /= value;
    }

    Ok(Value::Integer(total))
}

fn compare<F>(name: &str, args: &[Value], pos: SourcePos, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = expect_numbers(name, args, pos)?;
    if values.len() < 2 {
        return Err(wrong_arg_count(
            pos,
            name,
            "at least 2 arguments",
            values.len(),
        ));
    }

    let result = values.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(result))
}

fn builtin_not(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(wrong_arg_count(
            pos,
            "not",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn cons(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [head, Value::List(tail)] => {
            let mut items = Vec::with_capacity(tail.len() + 1);
            items.push(head.clone());
            items.extend(tail.iter().cloned());
            Ok(Value::List(items))
        }
        [_, other] => Err(type_mismatch(pos, "cons", "list", other.type_name())),
        _ => Err(wrong_arg_count(
            pos,
            "cons",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let items = expect_non_empty_list("car", args, pos)?;
    Ok(items[0].clone())
}

fn cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let items = expect_non_empty_list("cdr", args, pos)?;
    Ok(Value::List(items[1..].to_vec()))
}

fn append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for value in args {
        let list = expect_list("append", value, pos)?;
        items.extend(list.iter().cloned());
    }

    Ok(Value::List(items))
}

fn builtin_apply(
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (operator, rest) = args
        .split_first()
        .ok_or_else(|| wrong_arg_count(pos, "apply", "at least 2 arguments", 0))?;
    let (list_arg, prefix_args) = rest
        .split_last()
        .ok_or_else(|| wrong_arg_count(pos, "apply", "at least 2 arguments", 1))?;

    let list_args = expect_list("apply", list_arg, pos)?;
    let mut expanded_args = Vec::with_capacity(prefix_args.len() + list_args.len());
    expanded_args.extend(prefix_args.iter().cloned());
    expanded_args.extend(list_args.iter().cloned());

    apply(operator.clone(), &expanded_args, pos, context)
}

fn length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Integer(
            expect_list("length", value, pos)?.len() as i64
        )),
        _ => Err(wrong_arg_count(
            pos,
            "length",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn display(args: &[Value], pos: SourcePos, context: &mut EvalContext) -> Result<Value, EvalError> {
    match args {
        [value] => {
            context.output.push_str(&value.render_display());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count(
            pos,
            "display",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn write(args: &[Value], pos: SourcePos, context: &mut EvalContext) -> Result<Value, EvalError> {
    match args {
        [value] => {
            context.output.push_str(&value.render());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count(
            pos,
            "write",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn newline(args: &[Value], pos: SourcePos, context: &mut EvalContext) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(wrong_arg_count(
            pos,
            "newline",
            "exactly 0 arguments",
            args.len(),
        ));
    }

    context.output.push('\n');
    Ok(Value::Void)
}

fn string_append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        let value = expect_string("string-append", value, pos)?;
        result.push_str(&value.borrow());
    }

    Ok(make_string_value(result))
}

fn string_copy(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string-copy", value, pos)?;
            let copy = value.borrow().clone();
            Ok(make_string_value(copy))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-copy",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string-length", value, pos)?;
            let len = value.borrow().chars().count() as i64;
            Ok(Value::Integer(len))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-length",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn substring(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [string, start, end] => {
            let string = expect_string("substring", string, pos)?;
            let start = expect_non_negative_integer("substring", start, pos, "start index")?;
            let end = expect_non_negative_integer("substring", end, pos, "end index")?;
            let string = string.borrow();
            let len = string.chars().count();

            if start > end {
                return Err(invalid_argument(
                    pos,
                    "substring",
                    format!("start index {start} cannot exceed end index {end}"),
                ));
            }

            if end > len {
                return Err(invalid_argument(
                    pos,
                    "substring",
                    format!("end index {end} out of range for string of length {len}"),
                ));
            }

            let start_byte = byte_index_for_char(&string, start).unwrap_or(string.len());
            let end_byte = byte_index_for_char(&string, end).unwrap_or(string.len());
            Ok(make_string_value(string[start_byte..end_byte].to_string()))
        }
        _ => Err(wrong_arg_count(
            pos,
            "substring",
            "exactly 3 arguments",
            args.len(),
        )),
    }
}

fn string_to_number(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let string = expect_string("string->number", value, pos)?;
            let parsed = string.borrow().parse::<i64>();
            Ok(match parsed {
                Ok(number) => Value::Integer(number),
                Err(_) => Value::Boolean(false),
            })
        }
        _ => Err(wrong_arg_count(
            pos,
            "string->number",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn number_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [Value::Integer(number)] => Ok(make_string_value(number.to_string())),
        [other] => Err(type_mismatch(
            pos,
            "number->string",
            "number",
            other.type_name(),
        )),
        _ => Err(wrong_arg_count(
            pos,
            "number->string",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn symbol_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(make_string_value(
            expect_symbol("symbol->string", value, pos)?.to_string(),
        )),
        _ => Err(wrong_arg_count(
            pos,
            "symbol->string",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_to_symbol(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string->symbol", value, pos)?;
            let symbol = value.borrow().clone();
            Ok(Value::Symbol(symbol))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string->symbol",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [string, index] => {
            let string = expect_string("string-ref", string, pos)?;
            let index = expect_non_negative_integer("string-ref", index, pos, "index")?;
            let string = string.borrow();
            let len = string.chars().count();

            match string.chars().nth(index) {
                Some(ch) => Ok(Value::Character(ch)),
                None => Err(invalid_argument(
                    pos,
                    "string-ref",
                    format!("index {index} out of range for string of length {len}"),
                )),
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-ref",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn string_set(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [string, index, Value::Character(ch)] => {
            let string = expect_string("string-set!", string, pos)?;
            let index = expect_non_negative_integer("string-set!", index, pos, "index")?;
            let mut string = string.borrow_mut();
            let len = string.chars().count();

            if index >= len {
                return Err(invalid_argument(
                    pos,
                    "string-set!",
                    format!("index {index} out of range for string of length {len}"),
                ));
            }

            let start = byte_index_for_char(&string, index)
                .expect("valid character index must have a byte offset");
            let end = byte_index_for_char(&string, index + 1)
                .expect("valid character index must have an end byte offset");
            let replacement = ch.to_string();
            string.replace_range(start..end, &replacement);
            Ok(Value::Void)
        }
        [_, _, other] => Err(type_mismatch(pos, "string-set!", "char", other.type_name())),
        _ => Err(wrong_arg_count(
            pos,
            "string-set!",
            "exactly 3 arguments",
            args.len(),
        )),
    }
}

fn predicate<F>(args: &[Value], name: &str, pos: SourcePos, test: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(test(value))),
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}

fn expect_numbers(name: &str, args: &[Value], pos: SourcePos) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(type_mismatch(pos, name, "number", other.type_name())),
        })
        .collect()
}

fn expect_string(name: &str, value: &Value, pos: SourcePos) -> Result<StringRef, EvalError> {
    match value {
        Value::String(value) => Ok(Rc::clone(value)),
        other => Err(type_mismatch(pos, name, "string", other.type_name())),
    }
}

fn expect_symbol<'a>(name: &str, value: &'a Value, pos: SourcePos) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(value) => Ok(value),
        other => Err(type_mismatch(pos, name, "symbol", other.type_name())),
    }
}

fn expect_non_negative_integer(
    name: &str,
    value: &Value,
    pos: SourcePos,
    label: &str,
) -> Result<usize, EvalError> {
    match value {
        Value::Integer(number) if *number >= 0 => Ok(*number as usize),
        Value::Integer(number) => Err(invalid_argument(
            pos,
            name,
            format!("{label} must be non-negative, got {number}"),
        )),
        other => Err(type_mismatch(pos, name, "number", other.type_name())),
    }
}

fn expect_list<'a>(name: &str, value: &'a Value, pos: SourcePos) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(type_mismatch(pos, name, "list", other.type_name())),
    }
}

fn expect_non_empty_list<'a>(
    name: &str,
    args: &'a [Value],
    pos: SourcePos,
) -> Result<&'a [Value], EvalError> {
    match args {
        [value] => match value {
            Value::List(items) if !items.is_empty() => Ok(items),
            other => Err(type_mismatch(pos, name, "pair", other.type_name())),
        },
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}

fn render_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 2);
    out.push('"');

    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }

    out.push('"');
    out
}

fn render_char(ch: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => ch.to_string(),
        RenderMode::Write => match ch {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            _ => format!("#\\{ch}"),
        },
    }
}

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let mut out = String::from("(");

    for (index, value) in items.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&value.render_with_mode(mode));
    }

    out.push(')');
    out
}

fn byte_index_for_char(input: &str, char_index: usize) -> Option<usize> {
    if char_index == input.chars().count() {
        Some(input.len())
    } else {
        input
            .char_indices()
            .nth(char_index)
            .map(|(byte_index, _)| byte_index)
    }
}

#[cfg(test)]
mod tests;
