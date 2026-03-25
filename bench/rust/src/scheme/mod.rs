pub mod error;

pub use error::{EvalError, SourcePos};

use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

const BUILTIN_NAMES: &[&str] = &[
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
    "not",
    "newline",
    "number->string",
    "append",
    "apply",
    "boolean?",
    "car",
    "cdr",
    "cons",
    "length",
    "list",
    "null?",
    "number?",
    "pair?",
    "string-append",
    "string?",
    "string-copy",
    "string-length",
    "string->number",
    "string->symbol",
    "string-ref",
    "string-set!",
    "substring",
    "symbol?",
    "symbol->string",
    "write",
];

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
    output: Rc<RefCell<String>>,
}

impl Environment {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
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
            output,
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut()
            .bindings
            .insert(name, Rc::new(RefCell::new(value)));
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
enum Value {
    Integer(i64),
    Bool(bool),
    Char(char),
    String(SchemeString),
    Symbol(String),
    List(Vec<Value>),
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
            other => Err(EvalError::type_mismatch("number", other.type_name(), position)),
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
            other => Err(EvalError::type_mismatch("char", other.type_name(), self.position)),
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

    match &head.kind {
        ExprKind::Symbol(name) if name == "define" => eval_define(tail, env, head.pos),
        ExprKind::Symbol(name) if name == "set!" => eval_set(tail, env, head.pos),
        ExprKind::Symbol(name) if name == "if" => eval_if(tail, env, head.pos),
        ExprKind::Symbol(name) if name == "quote" => eval_quote(tail, head.pos),
        ExprKind::Symbol(name) if name == "lambda" => eval_lambda(tail, env, head.pos),
        ExprKind::Symbol(name) if name == "and" => eval_and(tail, env),
        ExprKind::Symbol(name) if name == "or" => eval_or(tail, env),
        ExprKind::Symbol(name) if name == "begin" => eval_begin(tail, env),
        ExprKind::Symbol(name) if name == "cond" => eval_cond(tail, env, head.pos),
        ExprKind::Symbol(name) if name == "let" => eval_let(tail, env, head.pos),
        _ => {
            let callable = eval(head, env.clone())?;
            let args = eval_all(tail, env.clone())?;
            apply(callable, head.pos, args, env)
        }
    }
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
        return Err(EvalError::syntax("set! target must be a symbol", name_expr.pos));
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
            return Err(EvalError::syntax(
                "cond clauses must be lists",
                clause.pos,
            ));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::syntax(
                "cond clauses cannot be empty",
                clause.pos,
            ));
        };

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != exprs.len() {
                return Err(EvalError::syntax(
                    "cond else clause must be last",
                    test.pos,
                ));
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
        _ => Err(EvalError::syntax("let requires bindings and a body", position)),
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
        "boolean?" => apply_type_predicate("boolean?", args, position, |value| matches!(value, Value::Bool(_))),
        "char?" => apply_type_predicate("char?", args, position, |value| matches!(value, Value::Char(_))),
        "car" => apply_car(args, position),
        "cdr" => apply_cdr(args, position),
        "cons" => apply_cons(args, position),
        "display" => apply_display(args, position, env),
        "length" => apply_length(args, position),
        "list" => Ok(Value::List(args.iter().map(|arg| arg.value.clone()).collect())),
        "newline" => apply_newline(args, position, env),
        "null?" => apply_null(args, position),
        "not" => apply_not(args, position),
        "number->string" => apply_number_to_string(args, position),
        "number?" => apply_type_predicate("number?", args, position, |value| matches!(value, Value::Integer(_))),
        "pair?" => apply_type_predicate("pair?", args, position, |value| {
            matches!(value, Value::List(values) if !values.is_empty())
        }),
        "string-append" => apply_string_append(args),
        "string?" => apply_type_predicate("string?", args, position, |value| matches!(value, Value::String(_))),
        "string-copy" => apply_string_copy(args, position),
        "string-length" => apply_string_length(args, position),
        "string->number" => apply_string_to_number(args, position),
        "string->symbol" => apply_string_to_symbol(args, position),
        "string-ref" => apply_string_ref(args, position),
        "string-set!" => apply_string_set(args, position),
        "substring" => apply_substring(args, position),
        "symbol?" => apply_type_predicate("symbol?", args, position, |value| matches!(value, Value::Symbol(_))),
        "symbol->string" => apply_symbol_to_string(args, position),
        "write" => apply_write(args, position, env),
        _ => Err(EvalError::unbound_variable(name, position)),
    }
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

fn apply_display(args: &[LocatedValue], position: SourcePos, env: EnvRef) -> Result<Value, EvalError> {
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

fn apply_newline(args: &[LocatedValue], position: SourcePos, env: EnvRef) -> Result<Value, EvalError> {
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

fn apply_write(args: &[LocatedValue], position: SourcePos, env: EnvRef) -> Result<Value, EvalError> {
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
        let Value::List(values) = &arg.value else {
            return Err(EvalError::type_mismatch(
                "list",
                arg.value.type_name(),
                arg.position,
            ));
        };

        items.extend(values.iter().cloned());
    }

    Ok(Value::List(items))
}

fn apply_apply(args: &[LocatedValue], position: SourcePos, env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::wrong_arg_count(
            "apply",
            "at least 2",
            args.len(),
            position,
        ));
    }

    let (callable, list_and_prefix_args) = args
        .split_first()
        .expect("apply arity checked");
    let (list_arg, prefix_args) = list_and_prefix_args
        .split_last()
        .expect("apply arity checked");

    let Value::List(list_values) = &list_arg.value else {
        return Err(EvalError::type_mismatch(
            "list",
            list_arg.value.type_name(),
            list_arg.position,
        ));
    };

    let mut expanded_args = Vec::with_capacity(prefix_args.len() + list_values.len());
    expanded_args.extend(prefix_args.iter().cloned());
    expanded_args.extend(
        list_values
            .iter()
            .cloned()
            .map(|value| LocatedValue::new(value, list_arg.position)),
    );

    apply(callable.value.clone(), callable.position, expanded_args, env)
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

    Ok(match args[0].as_string()?.to_plain_string().parse::<i64>() {
        Ok(value) => Value::Integer(value),
        Err(_) => Value::Bool(false),
    })
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
        return Err(EvalError::index_out_of_bounds(index, length, args[1].position));
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
        return Err(EvalError::index_out_of_bounds(index, length, args[1].position));
    }

    string.set_char(index as usize, args[2].as_char()?, args[0].position)?;
    Ok(Value::Void)
}

fn apply_car(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let values = expect_non_empty_list(args, "car", position)?;
    Ok(values[0].clone())
}

fn apply_cdr(args: &[LocatedValue], position: SourcePos) -> Result<Value, EvalError> {
    let values = expect_non_empty_list(args, "cdr", position)?;
    Ok(Value::List(values[1..].to_vec()))
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

    let Value::List(rest) = &args[1].value else {
        return Err(EvalError::type_mismatch(
            "list",
            args[1].value.type_name(),
            args[1].position,
        ));
    };

    let mut values = Vec::with_capacity(rest.len() + 1);
    values.push(args[0].value.clone());
    values.extend(rest.iter().cloned());
    Ok(Value::List(values))
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

    let Value::List(values) = &args[0].value else {
        return Err(EvalError::type_mismatch(
            "list",
            args[0].value.type_name(),
            args[0].position,
        ));
    };

    Ok(Value::Integer(values.len() as i64))
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

fn expect_non_empty_list<'a>(
    args: &'a [LocatedValue],
    name: &str,
    position: SourcePos,
) -> Result<&'a [Value], EvalError> {
    if args.len() != 1 {
        return Err(EvalError::wrong_arg_count(
            name,
            "exactly 1",
            args.len(),
            position,
        ));
    }

    let Value::List(values) = &args[0].value else {
        return Err(EvalError::type_mismatch(
            "list",
            args[0].value.type_name(),
            args[0].position,
        ));
    };

    if values.is_empty() {
        return Err(EvalError::type_mismatch("pair", "list", args[0].position));
    }

    Ok(values)
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
