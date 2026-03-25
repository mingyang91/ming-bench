pub mod error;

pub use error::EvalError;

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
    "not",
    "append",
    "boolean?",
    "car",
    "cdr",
    "cons",
    "length",
    "list",
    "null?",
    "number?",
    "pair?",
    "string?",
    "symbol?",
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
    let exprs = Parser::new(input).parse_program()?;
    let env = Environment::global();
    let mut last = None;

    for expr in &exprs {
        last = Some(eval(expr, env.clone())?);
    }

    let value = last.ok_or(EvalError::EmptyInput)?;
    Ok(match value {
        Value::Void => String::new(),
        other => other.to_scheme_string(),
    })
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

type EnvRef = Rc<RefCell<Environment>>;

#[derive(Clone)]
struct UserProcedure {
    name: Option<String>,
    params: Vec<String>,
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
    bindings: HashMap<String, Value>,
}

impl Environment {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self::default()));

        for &name in BUILTIN_NAMES {
            Self::define(&env, name.to_string(), Value::Builtin(name));
        }

        env
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        let parent = {
            let env = env.borrow();

            if let Some(value) = env.bindings.get(name) {
                return Some(value.clone());
            }

            env.parent.clone()
        };

        parent.and_then(|parent| Self::lookup(&parent, name))
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Bool(bool),
    String(String),
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

    fn as_number(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(value) => Ok(*value),
            other => Err(EvalError::TypeMismatch {
                expected: "number".into(),
                got: other.type_name().into(),
            }),
        }
    }

    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Bool(true) => "#t".into(),
            Value::Bool(false) => "#f".into(),
            Value::String(value) => format!("\"{}\"", escape_string(value)),
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

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Environment::lookup(&env, name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    match head {
        Expr::Symbol(name) if name == "define" => eval_define(tail, env),
        Expr::Symbol(name) if name == "if" => eval_if(tail, env),
        Expr::Symbol(name) if name == "quote" => eval_quote(tail),
        Expr::Symbol(name) if name == "lambda" => eval_lambda(tail, env),
        Expr::Symbol(name) if name == "and" => eval_and(tail, env),
        Expr::Symbol(name) if name == "or" => eval_or(tail, env),
        Expr::Symbol(name) if name == "begin" => eval_begin(tail, env),
        Expr::Symbol(name) if name == "cond" => eval_cond(tail, env),
        Expr::Symbol(name) if name == "let" => eval_let(tail, env),
        _ => {
            let callable = eval(head, env.clone())?;
            let args = eval_all(tail, env)?;
            apply(callable, args)
        }
    }
}

fn eval_all(exprs: &[Expr], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    exprs.iter().map(|expr| eval(expr, env.clone())).collect()
}

fn eval_define(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match exprs {
        [Expr::Symbol(name), value_expr] => {
            let value = eval(value_expr, env.clone())?;
            Environment::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] if !body.is_empty() => {
            let (name, params) = parse_function_signature(signature)?;
            let procedure = Value::Procedure(Rc::new(UserProcedure {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env: env.clone(),
            }));

            Environment::define(&env, name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "invalid define form".into(),
        }),
    }
}

fn eval_if(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [condition, then_branch, else_branch] = exprs else {
        return Err(EvalError::Syntax {
            message: "if requires exactly 3 expressions".into(),
        });
    };

    if eval(condition, env.clone())?.is_truthy() {
        eval(then_branch, env)
    } else {
        eval(else_branch, env)
    }
}

fn eval_quote(exprs: &[Expr]) -> Result<Value, EvalError> {
    let [expr] = exprs else {
        return Err(EvalError::Syntax {
            message: "quote requires exactly 1 expression".into(),
        });
    };

    Ok(quote_expr(expr))
}

fn eval_lambda(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = exprs.split_first() else {
        return Err(EvalError::Syntax {
            message: "lambda requires a parameter list and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda requires at least 1 body expression".into(),
        });
    }

    Ok(Value::Procedure(Rc::new(UserProcedure {
        name: None,
        params: parse_parameters(params_expr)?,
        body: body.to_vec(),
        env,
    })))
}

fn parse_function_signature(signature: &[Expr]) -> Result<(String, Vec<String>), EvalError> {
    let Some((name_expr, params)) = signature.split_first() else {
        return Err(EvalError::Syntax {
            message: "function definition requires a name".into(),
        });
    };

    let Expr::Symbol(name) = name_expr else {
        return Err(EvalError::Syntax {
            message: "function name must be a symbol".into(),
        });
    };

    let mut parsed_params = Vec::with_capacity(params.len());
    for param in params {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::Syntax {
                message: "parameter name must be a symbol".into(),
            });
        };
        parsed_params.push(name.clone());
    }

    Ok((name.clone(), parsed_params))
}

fn parse_parameters(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::Syntax {
            message: "lambda parameters must be a list".into(),
        });
    };

    let mut parsed = Vec::with_capacity(params.len());
    for param in params {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::Syntax {
                message: "parameter name must be a symbol".into(),
            });
        };
        parsed.push(name.clone());
    }

    Ok(parsed)
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
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

fn eval_cond(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in exprs.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::Syntax {
                message: "cond clauses must be lists".into(),
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax {
                message: "cond clauses cannot be empty".into(),
            });
        };

        if matches!(test, Expr::Symbol(name) if name == "else") {
            if index + 1 != exprs.len() {
                return Err(EvalError::Syntax {
                    message: "cond else clause must be last".into(),
                });
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

    Ok(Value::Void)
}

fn eval_let(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match exprs {
        [Expr::Symbol(name), bindings_expr, body @ ..] if !body.is_empty() => {
            eval_named_let(name, bindings_expr, body, env)
        }
        [bindings_expr, body @ ..] if !body.is_empty() => eval_plain_let(bindings_expr, body, env),
        _ => Err(EvalError::Syntax {
            message: "let requires bindings and a body".into(),
        }),
    }
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone())?;
    let let_env = Environment::child(env);

    for ((name, _), value) in bindings.into_iter().zip(values) {
        Environment::define(&let_env, name, value);
    }

    eval_sequence(body, let_env)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings_expr)?;
    let args = eval_binding_values(&bindings, env.clone())?;
    let params = bindings.into_iter().map(|(param, _)| param).collect();
    let let_env = Environment::child(env);

    let procedure = Value::Procedure(Rc::new(UserProcedure {
        name: Some(name.into()),
        params,
        body: body.to_vec(),
        env: let_env.clone(),
    }));

    Environment::define(&let_env, name.into(), procedure.clone());
    apply(procedure, args)
}

fn parse_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "let bindings must be a list".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(items) = binding else {
            return Err(EvalError::Syntax {
                message: "let binding must be a list".into(),
            });
        };

        let [Expr::Symbol(name), value_expr] = items.as_slice() else {
            return Err(EvalError::Syntax {
                message: "let binding must contain a name and value".into(),
            });
        };

        parsed.push((name.clone(), value_expr.clone()));
    }

    Ok(parsed)
}

fn eval_binding_values(bindings: &[(String, Expr)], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| eval(expr, env.clone()))
        .collect()
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval(expr, env.clone())?;
    }

    Ok(last)
}

fn apply(callable: Value, args: Vec<Value>) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(name) => apply_builtin(name, &args),
        Value::Procedure(procedure) => apply_user_procedure(procedure, args),
        other => Err(EvalError::NotCallable {
            found: other.type_name().into(),
        }),
    }
}

fn apply_user_procedure(
    procedure: Rc<UserProcedure>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        return Err(EvalError::WrongArgCount {
            name: procedure.display_name().into(),
            expected: format!("exactly {}", procedure.params.len()),
            got: args.len(),
        });
    }

    let call_env = Environment::child(procedure.env.clone());
    for (param, value) in procedure.params.iter().cloned().zip(args) {
        Environment::define(&call_env, param, value);
    }

    eval_sequence(&procedure.body, call_env)
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => apply_add(args),
        "-" => apply_sub(args),
        "*" => apply_mul(args),
        "/" => apply_div(args),
        "<" => apply_compare(name, args, |left, right| left < right),
        "<=" => apply_compare(name, args, |left, right| left <= right),
        "=" => apply_compare(name, args, |left, right| left == right),
        ">" => apply_compare(name, args, |left, right| left > right),
        ">=" => apply_compare(name, args, |left, right| left >= right),
        "append" => apply_append(args),
        "boolean?" => apply_type_predicate("boolean?", args, |value| matches!(value, Value::Bool(_))),
        "car" => apply_car(args),
        "cdr" => apply_cdr(args),
        "cons" => apply_cons(args),
        "length" => apply_length(args),
        "list" => Ok(Value::List(args.to_vec())),
        "null?" => apply_null(args),
        "not" => apply_not(args),
        "number?" => apply_type_predicate("number?", args, |value| matches!(value, Value::Integer(_))),
        "pair?" => apply_type_predicate("pair?", args, |value| {
            matches!(value, Value::List(values) if !values.is_empty())
        }),
        "string?" => apply_type_predicate("string?", args, |value| matches!(value, Value::String(_))),
        "symbol?" => apply_type_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_))),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn apply_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;

    for arg in args {
        total = total
            .checked_add(arg.as_number()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_sub(args: &[Value]) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        });
    };

    let first = first.as_number()?;

    if rest.is_empty() {
        return Ok(Value::Integer(
            first.checked_neg().ok_or(EvalError::IntegerOverflow)?,
        ));
    }

    let mut total = first;
    for arg in rest {
        total = total
            .checked_sub(arg.as_number()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i64;

    for arg in args {
        total = total
            .checked_mul(arg.as_number()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_div(args: &[Value]) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 1".into(),
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 1,
        });
    }

    let mut total = first.as_number()?;
    for arg in rest {
        let divisor = arg.as_number()?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total = total
            .checked_div(divisor)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
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

fn apply_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    Ok(Value::Bool(!args[0].is_truthy()))
}

fn apply_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for arg in args {
        let Value::List(values) = arg else {
            return Err(EvalError::TypeMismatch {
                expected: "list".into(),
                got: arg.type_name().into(),
            });
        };

        items.extend(values.iter().cloned());
    }

    Ok(Value::List(items))
}

fn apply_car(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_non_empty_list(args, "car")?;
    Ok(values[0].clone())
}

fn apply_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_non_empty_list(args, "cdr")?;
    Ok(Value::List(values[1..].to_vec()))
}

fn apply_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2".into(),
            got: args.len(),
        });
    }

    let Value::List(rest) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".into(),
            got: args[1].type_name().into(),
        });
    };

    let mut values = Vec::with_capacity(rest.len() + 1);
    values.push(args[0].clone());
    values.extend(rest.iter().cloned());
    Ok(Value::List(values))
}

fn apply_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    let Value::List(values) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".into(),
            got: args[0].type_name().into(),
        });
    };

    Ok(Value::Integer(values.len() as i64))
}

fn apply_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "null?".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    Ok(Value::Bool(matches!(&args[0], Value::List(values) if values.is_empty())))
}

fn apply_type_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    Ok(Value::Bool(predicate(&args[0])))
}

fn expect_non_empty_list<'a>(args: &'a [Value], name: &str) -> Result<&'a [Value], EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    let Value::List(values) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "list".into(),
            got: args[0].type_name().into(),
        });
    };

    if values.is_empty() {
        return Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            got: "list".into(),
        });
    }

    Ok(values)
}

struct Parser<'a> {
    chars: Vec<char>,
    index: usize,
    _source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            _source: source,
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

        match self.peek() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::Syntax {
                message: "unexpected ')'".into(),
            }),
            Some('\'') => self.parse_quote_shorthand(),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_token_expr(),
            None => Err(EvalError::Syntax {
                message: "unexpected end of input".into(),
            }),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek() {
                Some(')') => {
                    self.index += 1;
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::Syntax {
                        message: "unterminated list".into(),
                    });
                }
            }
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.expect('\'')?;
        Ok(Expr::List(vec![
            Expr::Symbol("quote".into()),
            self.parse_expr()?,
        ]))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect('"')?;
        let mut value = String::new();

        while let Some(ch) = self.peek() {
            self.index += 1;
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.peek().ok_or(EvalError::Syntax {
                        message: "unterminated string escape".into(),
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

        Err(EvalError::Syntax {
            message: "unterminated string".into(),
        })
    }

    fn parse_token_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self.take_token();

        if token.is_empty() {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            });
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ if is_integer_token(&token) => {
                let value = token.parse().map_err(|_| EvalError::Syntax {
                    message: format!("invalid integer literal: {token}"),
                })?;
                Ok(Expr::Integer(value))
            }
            _ => Ok(Expr::Symbol(token)),
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
            Some(found) => Err(EvalError::Syntax {
                message: format!("expected '{expected}', found '{found}'"),
            }),
            None => Err(EvalError::Syntax {
                message: format!("expected '{expected}', found end of input"),
            }),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }
}

fn is_integer_token(token: &str) -> bool {
    if token == "+" || token == "-" {
        return false;
    }

    token.parse::<i64>().is_ok()
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | ';')
}
