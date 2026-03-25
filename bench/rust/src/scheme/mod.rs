pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Bool(bool, Position),
    Int(i64, Position),
    String(String, Position),
    Symbol(String, Position),
    List(Vec<Expr>, Position),
}

impl Expr {
    fn pos(&self) -> Position {
        match self {
            Self::Bool(_, pos)
            | Self::Int(_, pos)
            | Self::String(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone)]
enum Value {
    Bool(bool),
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Int(_) => "number",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
            Self::Int(value) => value.to_string(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::List(values) => render_list(values),
            Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => String::new(),
        }
    }

    fn render_for_error(&self) -> String {
        match self {
            Self::Void => "#<void>".into(),
            _ => self.render(),
        }
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
}

#[derive(Clone, Copy)]
struct BuiltinProcedure {
    name: &'static str,
    func: fn(&[Value]) -> Result<Value, EvalError>,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<RefCell<Environment>>;

struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent,
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        let mut current = Some(env.clone());

        while let Some(frame) = current {
            let (value, parent) = {
                let borrowed = frame.borrow();
                (
                    borrowed.bindings.get(name).cloned(),
                    borrowed.parent.clone(),
                )
            };

            if value.is_some() {
                return value;
            }

            current = parent;
        }

        None
    }
}

struct Parser<'a> {
    input: &'a str,
    cursor: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            cursor: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            Err(EvalError::EmptyInput)
        } else {
            Ok(expressions)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_position();

        match self.peek_char() {
            Some('(') => self.parse_list(pos),
            Some('\'') => self.parse_quote_shorthand(pos),
            Some('"') => self.parse_string(pos),
            Some(')') => Err(with_pos(
                EvalError::SyntaxError {
                    message: "unexpected ')'".into(),
                },
                pos,
            )),
            Some(_) => self.parse_atom(pos),
            None => Err(with_pos(
                EvalError::SyntaxError {
                    message: "unexpected end of input".into(),
                },
                pos,
            )),
        }
    }

    fn parse_list(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List(items, pos));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(with_pos(
                        EvalError::SyntaxError {
                            message: "unterminated list".into(),
                        },
                        self.current_position(),
                    ));
                }
            }
        }
    }

    fn parse_quote_shorthand(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), pos), quoted],
            pos,
        ))
    }

    fn parse_string(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(value, pos)),
                Some('\\') => match self.bump_char() {
                    Some('"') => value.push('"'),
                    Some('\\') => value.push('\\'),
                    Some('n') => value.push('\n'),
                    Some('r') => value.push('\r'),
                    Some('t') => value.push('\t'),
                    Some(other) => {
                        return Err(with_pos(
                            EvalError::SyntaxError {
                                message: format!("unsupported string escape: \\{other}"),
                            },
                            self.current_position(),
                        ));
                    }
                    None => {
                        return Err(with_pos(
                            EvalError::SyntaxError {
                                message: "unterminated string escape".into(),
                            },
                            self.current_position(),
                        ));
                    }
                },
                Some(ch) => value.push(ch),
                None => {
                    return Err(with_pos(
                        EvalError::SyntaxError {
                            message: "unterminated string".into(),
                        },
                        self.current_position(),
                    ));
                }
            }
        }
    }

    fn parse_atom(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let mut token = String::new();

        while let Some(ch) = self.peek_char() {
            if is_delimiter(ch) {
                break;
            }

            token.push(ch);
            self.bump_char();
        }

        if token.is_empty() {
            return Err(with_pos(
                EvalError::SyntaxError {
                    message: "expected expression".into(),
                },
                pos,
            ));
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true, pos)),
            "#f" => Ok(Expr::Bool(false, pos)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Int(value, pos)),
                Err(_) => Ok(Expr::Symbol(token, pos)),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.bump_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        let pos = self.current_position();
        match self.bump_char() {
            Some(actual) if actual == expected => Ok(()),
            Some(actual) => Err(with_pos(
                EvalError::SyntaxError {
                    message: format!("expected '{expected}', found '{actual}'"),
                },
                pos,
            )),
            None => Err(with_pos(
                EvalError::SyntaxError {
                    message: format!("expected '{expected}', found end of input"),
                },
                pos,
            )),
        }
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.cursor..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.cursor += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.cursor >= self.input.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

fn with_pos(error: EvalError, pos: Position) -> EvalError {
    error.with_position(pos.line, pos.col)
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

fn render_list(values: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&value.render());
    }

    rendered.push(')');
    rendered
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);

    for builtin in [
        BuiltinProcedure {
            name: "+",
            func: builtin_add,
        },
        BuiltinProcedure {
            name: "-",
            func: builtin_sub,
        },
        BuiltinProcedure {
            name: "*",
            func: builtin_mul,
        },
        BuiltinProcedure {
            name: "/",
            func: builtin_div,
        },
        BuiltinProcedure {
            name: "<",
            func: builtin_lt,
        },
        BuiltinProcedure {
            name: ">",
            func: builtin_gt,
        },
        BuiltinProcedure {
            name: "=",
            func: builtin_eq,
        },
        BuiltinProcedure {
            name: "<=",
            func: builtin_le,
        },
        BuiltinProcedure {
            name: "not",
            func: builtin_not,
        },
        BuiltinProcedure {
            name: "cons",
            func: builtin_cons,
        },
        BuiltinProcedure {
            name: "car",
            func: builtin_car,
        },
        BuiltinProcedure {
            name: "cdr",
            func: builtin_cdr,
        },
        BuiltinProcedure {
            name: "null?",
            func: builtin_null,
        },
        BuiltinProcedure {
            name: "list",
            func: builtin_list,
        },
        BuiltinProcedure {
            name: "length",
            func: builtin_length,
        },
        BuiltinProcedure {
            name: "append",
            func: builtin_append,
        },
        BuiltinProcedure {
            name: "string?",
            func: builtin_is_string,
        },
        BuiltinProcedure {
            name: "number?",
            func: builtin_is_number,
        },
        BuiltinProcedure {
            name: "boolean?",
            func: builtin_is_boolean,
        },
        BuiltinProcedure {
            name: "pair?",
            func: builtin_is_pair,
        },
        BuiltinProcedure {
            name: "symbol?",
            func: builtin_is_symbol,
        },
    ] {
        Environment::define(
            &env,
            builtin.name.to_string(),
            Value::Procedure(Rc::new(Procedure::Builtin(builtin))),
        );
    }

    env
}

fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    eval_sequence(expressions, &env)
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expression in expressions {
        last = eval_expr(expression, env)?;
    }

    Ok(last)
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value, _) => Ok(Value::Bool(*value)),
        Expr::Int(value, _) => Ok(Value::Int(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Symbol(name, pos) => Environment::lookup(env, name)
            .ok_or_else(|| with_pos(EvalError::UnboundSymbol { name: name.clone() }, *pos)),
        Expr::List(items, pos) => eval_list(items, env).map_err(|error| with_pos(error, *pos)),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or_else(|| EvalError::SyntaxError {
        message: "cannot evaluate an empty list".into(),
    })?;

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "lambda" => return eval_lambda(args, env),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "let" => return eval_let(args, env),
            "begin" => return eval_begin(args, env),
            "cond" => return eval_cond(args, env),
            _ => {}
        }
    }

    let operator = eval_expr(head, env)?;
    let values = args
        .iter()
        .map(|arg| eval_expr(arg, env))
        .collect::<Result<Vec<_>, _>>()?;
    apply_procedure(operator, &values)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (target, rest) = args.split_first().ok_or_else(|| EvalError::WrongArgCount {
        name: "define".into(),
        expected: "at least 2".into(),
        actual: 0,
    })?;

    match target {
        Expr::Symbol(name, _) => {
            if rest.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define".into(),
                    expected: "exactly 2".into(),
                    actual: args.len(),
                });
            }

            let value = eval_expr(&rest[0], env)?;
            Environment::define(env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature, _) => {
            let (name_expr, params_exprs) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::SyntaxError {
                        message: "define requires a function name".into(),
                    })?;

            if rest.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "define".into(),
                    expected: "at least 2".into(),
                    actual: args.len(),
                });
            }

            let name = expect_symbol_expr(name_expr, "define function name")?;
            let params = parse_params(params_exprs)?;
            let lambda = make_lambda(Some(name.clone()), params, rest.to_vec(), env);
            Environment::define(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(with_pos(
            EvalError::SyntaxError {
                message: "define requires a symbol or function signature".into(),
            },
            target.pos(),
        )),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "exactly 3".into(),
            actual: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env)?;
    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else {
        eval_expr(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1".into(),
            actual: args.len(),
        });
    }

    Ok(quote_expr(&args[0]))
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            actual: args.len(),
        });
    }

    let params = match &args[0] {
        Expr::List(params, _) => parse_params(params)?,
        _ => {
            return Err(with_pos(
                EvalError::SyntaxError {
                    message: "lambda parameters must be a list".into(),
                },
                args[0].pos(),
            ));
        }
    };

    Ok(make_lambda(None, params, args[1..].to_vec(), env))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for arg in args {
        let value = eval_expr(arg, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);

    for arg in args {
        let value = eval_expr(arg, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_begin(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_let(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            actual: 0,
        });
    };

    match head {
        Expr::Symbol(name, _) => {
            let Some((bindings_expr, body)) = tail.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "let".into(),
                    expected: "at least 3".into(),
                    actual: args.len(),
                });
            };
            let bindings = parse_let_bindings(bindings_expr)?;
            eval_named_let(name, &bindings, body, env)
        }
        _ => {
            let bindings = parse_let_bindings(head)?;
            eval_plain_let(&bindings, tail, env)
        }
    }
}

fn eval_plain_let(
    bindings: &[(String, Expr)],
    body: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            actual: 1,
        });
    }

    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env))
        .collect::<Result<Vec<_>, _>>()?;
    let local_env = Environment::new(Some(env.clone()));

    for ((name, _), value) in bindings.iter().zip(values) {
        Environment::define(&local_env, name.clone(), value);
    }

    eval_sequence(body, &local_env)
}

fn eval_named_let(
    name: &str,
    bindings: &[(String, Expr)],
    body: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            actual: 2,
        });
    }

    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env))
        .collect::<Result<Vec<_>, _>>()?;
    let params = bindings
        .iter()
        .map(|(binding_name, _)| binding_name.clone())
        .collect();
    let recursive_env = Environment::new(Some(env.clone()));
    let procedure = make_lambda(
        Some(name.to_string()),
        params,
        body.to_vec(),
        &recursive_env,
    );

    Environment::define(&recursive_env, name.to_string(), procedure.clone());
    apply_procedure(procedure, &values)
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        _ => {
            return Err(with_pos(
                EvalError::SyntaxError {
                    message: "let bindings must be a list".into(),
                },
                bindings_expr.pos(),
            ));
        }
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(parts, _) if parts.len() == 2 => Ok((
                expect_symbol_expr(&parts[0], "let binding name")?,
                parts[1].clone(),
            )),
            Expr::List(_, _) => Err(with_pos(
                EvalError::SyntaxError {
                    message: "let bindings must contain exactly a name and value".into(),
                },
                binding.pos(),
            )),
            _ => Err(with_pos(
                EvalError::SyntaxError {
                    message: "let binding must be a list".into(),
                },
                binding.pos(),
            )),
        })
        .collect()
}

fn eval_cond(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for clause in args {
        let parts = match clause {
            Expr::List(parts, _) if !parts.is_empty() => parts,
            Expr::List(_, _) => {
                return Err(with_pos(
                    EvalError::SyntaxError {
                        message: "cond clause cannot be empty".into(),
                    },
                    clause.pos(),
                ));
            }
            _ => {
                return Err(with_pos(
                    EvalError::SyntaxError {
                        message: "cond clause must be a list".into(),
                    },
                    clause.pos(),
                ));
            }
        };

        if matches!(&parts[0], Expr::Symbol(symbol, _) if symbol == "else") {
            return eval_cond_clause_body(&parts[1..], env, Value::Bool(true));
        }

        let test_value = eval_expr(&parts[0], env)?;
        if test_value.is_truthy() {
            return eval_cond_clause_body(&parts[1..], env, test_value);
        }
    }

    Ok(Value::Void)
}

fn eval_cond_clause_body(args: &[Expr], env: &EnvRef, fallback: Value) -> Result<Value, EvalError> {
    if args.is_empty() {
        Ok(fallback)
    } else {
        eval_sequence(args, env)
    }
}

fn make_lambda(name: Option<String>, params: Vec<String>, body: Vec<Expr>, env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params,
        body,
        env: env.clone(),
    })))
}

fn parse_params(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|param| expect_symbol_expr(param, "parameter"))
        .collect()
}

fn expect_symbol_expr(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name, _) => Ok(name.clone()),
        _ => Err(with_pos(
            EvalError::SyntaxError {
                message: format!("{context} must be a symbol"),
            },
            expr.pos(),
        )),
    }
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool(value, _) => Value::Bool(*value),
        Expr::Int(value, _) => Value::Int(*value),
        Expr::String(value, _) => Value::String(value.clone()),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn expect_list<'a>(value: &'a Value, expected: &str) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(values) => Ok(values),
        other => Err(EvalError::TypeMismatch {
            expected: expected.into(),
            found: other.type_name().into(),
        }),
    }
}

fn expect_single_arg<'a>(name: &str, args: &'a [Value]) -> Result<&'a Value, EvalError> {
    match args {
        [value] => Ok(value),
        _ => Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            actual: args.len(),
        }),
    }
}

fn apply_procedure(operator: Value, args: &[Value]) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => (builtin.func)(args),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args),
        },
        other => Err(EvalError::NotAProcedure {
            found: other.render_for_error(),
        }),
    }
}

fn apply_lambda(lambda: &LambdaProcedure, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            expected: format!("exactly {}", lambda.params.len()),
            actual: args.len(),
        });
    }

    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda.params.iter().zip(args.iter()) {
        Environment::define(&call_env, param.clone(), value.clone());
    }

    eval_sequence(&lambda.body, &call_env)
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;

    for arg in args {
        total += expect_number(arg)?;
    }

    Ok(Value::Int(total))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("-", args, 1)?;

    let result = if values.len() == 1 {
        -values[0]
    } else {
        let (first, rest) = values.split_first().expect("length checked");
        rest.iter().fold(*first, |acc, value| acc - value)
    };

    Ok(Value::Int(result))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1_i64;

    for arg in args {
        product *= expect_number(arg)?;
    }

    Ok(Value::Int(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_number_args("/", args, 2)?;
    let (first, rest) = values.split_first().expect("length checked");
    let mut result = *first;

    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        if result % value != 0 {
            return Err(EvalError::NonIntegerDivision);
        }
        result /= value;
    }

    Ok(Value::Int(result))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare("<", args, |lhs, rhs| lhs < rhs)
}

fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare(">", args, |lhs, rhs| lhs > rhs)
}

fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare("=", args, |lhs, rhs| lhs == rhs)
}

fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare("<=", args, |lhs, rhs| lhs <= rhs)
}

fn builtin_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = eval_number_args(name, args, 2)?;

    for pair in values.windows(2) {
        if !compare(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(!expect_single_arg("not", args)?.is_truthy()))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2".into(),
            actual: args.len(),
        });
    }

    let mut values = Vec::new();
    values.push(args[0].clone());
    values.extend(expect_list(&args[1], "list")?.iter().cloned());
    Ok(Value::List(values))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_list(expect_single_arg("car", args)?, "pair")?;
    values
        .first()
        .cloned()
        .ok_or_else(|| EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        })
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_list(expect_single_arg("cdr", args)?, "pair")?;
    if values.is_empty() {
        return Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        });
    }

    Ok(Value::List(values[1..].to_vec()))
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("null?", args)?,
        Value::List(values) if values.is_empty()
    )))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_list(expect_single_arg("length", args)?, "list")?;
    Ok(Value::Int(values.len() as i64))
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut combined = Vec::new();

    for arg in args {
        combined.extend(expect_list(arg, "list")?.iter().cloned());
    }

    Ok(Value::List(combined))
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("string?", args)?,
        Value::String(_)
    )))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("number?", args)?,
        Value::Int(_)
    )))
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("boolean?", args)?,
        Value::Bool(_)
    )))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("pair?", args)?,
        Value::List(values) if !values.is_empty()
    )))
}

fn builtin_is_symbol(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("symbol?", args)?,
        Value::Symbol(_)
    )))
}

fn eval_number_args(name: &str, args: &[Value], min: usize) -> Result<Vec<i64>, EvalError> {
    if args.len() < min {
        return Err(EvalError::WrongArgCount {
            name: name.to_string(),
            expected: format!("at least {min}"),
            actual: args.len(),
        });
    }

    args.iter().map(expect_number).collect()
}

fn expect_number(value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(),
            found: other.type_name().into(),
        }),
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
    let mut parser = Parser::new(input);
    let expressions = parser.parse_program()?;
    let result = eval_program(&expressions)?;
    Ok(result.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_str(input).map(|result| (result, String::new()))
}

#[cfg(test)]
mod tests;
