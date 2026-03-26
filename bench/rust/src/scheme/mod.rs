use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }

    fn symbol(name: impl Into<String>, pos: SourcePos) -> Self {
        Self::new(ExprKind::Symbol(name.into()), pos)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SourcePos {
    offset: usize,
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<Closure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn kind(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => format!("{value:?}"),
            Self::Symbol(name) => name.clone(),
            Self::List(items) => render_list(items),
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

fn render_list(items: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render());
    }

    rendered.push(')');
    rendered
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    Length,
    Append,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
        }
    }
}

struct Closure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<Env>;

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

impl Env {
    fn new() -> EnvRef {
        let env = Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        });

        for (name, builtin) in [
            ("+", Builtin::Add),
            ("-", Builtin::Sub),
            ("*", Builtin::Mul),
            ("/", Builtin::Div),
            ("<", Builtin::LessThan),
            (">", Builtin::GreaterThan),
            ("=", Builtin::Equal),
            ("<=", Builtin::LessEqual),
            ("not", Builtin::Not),
            ("cons", Builtin::Cons),
            ("car", Builtin::Car),
            ("cdr", Builtin::Cdr),
            ("null?", Builtin::NullPred),
            ("list", Builtin::List),
            ("length", Builtin::Length),
            ("append", Builtin::Append),
            ("string?", Builtin::StringPred),
            ("number?", Builtin::NumberPred),
            ("boolean?", Builtin::BooleanPred),
            ("pair?", Builtin::PairPred),
            ("symbol?", Builtin::SymbolPred),
        ] {
            env.define(name.into(), Value::Builtin(builtin));
        }

        env
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent.clone()),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name) {
            return Some(value.clone());
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            Err(EvalError::EmptyProgram.with_offset(0))
        } else {
            Ok(expressions)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote(),
            Some(')') => Err(EvalError::UnexpectedCloseParen.with_offset(pos.offset)),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof.with_offset(pos.offset)),
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.next_char();
        Ok(Expr::new(ExprKind::List(vec![
            Expr::symbol("quote", pos),
            self.parse_expr()?,
        ]), pos))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.next_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.next_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof.with_offset(pos.offset)),
            }
        }

        Ok(Expr::new(ExprKind::List(items), pos))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.next_char();
        let mut value = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '"' => return Ok(Expr::new(ExprKind::String(value), pos)),
                '\\' => {
                    let escape_pos = self.current_pos();
                    let escaped = self
                        .next_char()
                        .ok_or_else(|| EvalError::UnexpectedEof.with_offset(escape_pos.offset))?;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => {
                            return Err(
                                EvalError::InvalidEscape { escape: other }
                                    .with_offset(escape_pos.offset),
                            )
                        }
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof.with_offset(pos.offset))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        let pos = SourcePos { offset: start };

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.next_char();
        }

        let token = &self.input[start..self.pos];

        match token {
            "#t" => Ok(Expr::new(ExprKind::Boolean(true), pos)),
            "#f" => Ok(Expr::new(ExprKind::Boolean(false), pos)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::new(ExprKind::Integer(value), pos)),
                Err(_) => Ok(Expr::symbol(token, pos)),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.next_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.next_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos { offset: self.pos }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    let env = Env::new();
    eval_sequence(expressions, &env)
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Void;

    for expression in expressions {
        last_value = eval_expr(expression, env)?;
    }

    Ok(last_value)
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }.with_offset(expr.pos.offset)),
        ExprKind::List(items) => eval_application(expr.pos, items, env),
    }
}

fn eval_application(list_pos: SourcePos, items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (operator, arguments) = items
        .split_first()
        .ok_or_else(|| EvalError::EmptyList.with_offset(list_pos.offset))?;

    if let ExprKind::Symbol(name) = &operator.kind {
        match name.as_str() {
            "define" => return eval_define(operator.pos, arguments, env),
            "if" => return eval_if(operator.pos, arguments, env),
            "quote" => return eval_quote(operator.pos, arguments),
            "lambda" => return eval_lambda(operator.pos, arguments, env),
            "and" => return eval_and(arguments, env),
            "or" => return eval_or(arguments, env),
            "begin" => return eval_begin(arguments, env),
            "let" => return eval_let(operator.pos, arguments, env),
            "cond" => return eval_cond(arguments, env),
            _ => {}
        }
    }

    let procedure = eval_expr(operator, env)?;
    apply_value(procedure, arguments, env, operator.pos)
}

fn apply_value(
    value: Value,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match value {
        Value::Builtin(builtin) => eval_builtin(builtin, arguments, env, call_pos),
        Value::Procedure(closure) => apply_closure(closure, arguments, env, call_pos),
        other => Err(
            EvalError::NotAProcedure {
                found: other.kind().into(),
            }
            .with_offset(call_pos.offset),
        ),
    }
}

fn eval_builtin(
    builtin: Builtin,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(arguments, env),
        Builtin::Sub => eval_sub(arguments, env, call_pos),
        Builtin::Mul => eval_mul(arguments, env),
        Builtin::Div => eval_div(arguments, env, call_pos),
        Builtin::LessThan => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| left < right)
        }
        Builtin::GreaterThan => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| left > right)
        }
        Builtin::Equal => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| left == right)
        }
        Builtin::LessEqual => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| left <= right)
        }
        Builtin::Not => eval_not(arguments, env, call_pos),
        Builtin::Cons => eval_cons(arguments, env, call_pos),
        Builtin::Car => eval_car(arguments, env, call_pos),
        Builtin::Cdr => eval_cdr(arguments, env, call_pos),
        Builtin::NullPred => eval_null_pred(arguments, env, call_pos),
        Builtin::List => eval_list_builtin(arguments, env),
        Builtin::Length => eval_length(arguments, env, call_pos),
        Builtin::Append => eval_append(arguments, env),
        Builtin::StringPred => eval_type_predicate(arguments, env, "string?", call_pos, |value| {
            matches!(value, Value::String(_))
        }),
        Builtin::NumberPred => eval_type_predicate(arguments, env, "number?", call_pos, |value| {
            matches!(value, Value::Integer(_))
        }),
        Builtin::BooleanPred => {
            eval_type_predicate(arguments, env, "boolean?", call_pos, |value| {
                matches!(value, Value::Boolean(_))
            })
        }
        Builtin::PairPred => eval_type_predicate(
            arguments,
            env,
            "pair?",
            call_pos,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        Builtin::SymbolPred => {
            eval_type_predicate(arguments, env, "symbol?", call_pos, |value| {
                matches!(value, Value::Symbol(_))
            })
        }
    }
}

fn apply_closure(
    closure: Rc<Closure>,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let argument_values = eval_args(arguments, env)?;
    apply_closure_values(closure, argument_values, call_pos)
}

fn apply_closure_values(
    closure: Rc<Closure>,
    argument_values: Vec<Value>,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    if argument_values.len() != closure.params.len() {
        return Err(
            EvalError::WrongArgCount {
                name: "procedure".into(),
                expected: format!("exactly {}", closure.params.len()),
                got: argument_values.len(),
            }
            .with_offset(call_pos.offset),
        );
    }

    let call_env = Env::child(&closure.env);

    for (param, value) in closure.params.iter().cloned().zip(argument_values) {
        call_env.define(param, value);
    }

    eval_sequence(&closure.body, &call_env)
}

fn eval_args(arguments: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_expr(argument, env))
        .collect()
}

fn eval_define(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(
            EvalError::WrongArgCount {
                name: "define".into(),
                expected: "at least 2".into(),
                got: arguments.len(),
            }
            .with_offset(pos.offset),
        );
    }

    match &arguments[0].kind {
        ExprKind::Symbol(name) => {
            if arguments.len() != 2 {
                return Err(
                    EvalError::WrongArgCount {
                        name: "define".into(),
                        expected: "exactly 2".into(),
                        got: arguments.len(),
                    }
                    .with_offset(pos.offset),
                );
            }

            let value = eval_expr(&arguments[1], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let (name_expr, params) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: "define: expected function name".into(),
                    }
                    .with_offset(arguments[0].pos.offset))?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(
                    EvalError::InvalidSyntax {
                        message: "define: expected function name".into(),
                    }
                    .with_offset(name_expr.pos.offset),
                );
            };

            let closure = Value::Procedure(Rc::new(Closure {
                params: parse_params(params)?,
                body: arguments[1..].to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(
            EvalError::InvalidSyntax {
                message: "define: expected symbol or function signature".into(),
            }
            .with_offset(arguments[0].pos.offset),
        ),
    }
}

fn eval_if(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "if".into(),
                expected: "exactly 3".into(),
                got: arguments.len(),
            }
            .with_offset(pos.offset),
        );
    };

    if eval_expr(condition, env)?.is_truthy() {
        eval_expr(consequent, env)
    } else {
        eval_expr(alternate, env)
    }
}

fn eval_quote(pos: SourcePos, arguments: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "quote".into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(pos.offset),
        );
    };

    Ok(quote_expr(quoted))
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(value) => Value::Integer(*value),
        ExprKind::Boolean(value) => Value::Boolean(*value),
        ExprKind::String(value) => Value::String(value.clone()),
        ExprKind::Symbol(name) => Value::Symbol(name.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (param_list, body) =
        arguments.split_first().ok_or_else(|| {
            EvalError::WrongArgCount {
                name: "lambda".into(),
                expected: "at least 2".into(),
                got: 0,
            }
            .with_offset(pos.offset)
        })?;

    if body.is_empty() {
        return Err(
            EvalError::WrongArgCount {
                name: "lambda".into(),
                expected: "at least 2".into(),
                got: 1,
            }
            .with_offset(pos.offset),
        );
    }

    Ok(Value::Procedure(Rc::new(Closure {
        params: parse_param_list(param_list)?,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn eval_begin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(arguments, env)
}

fn eval_let(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((first, rest)) = arguments.split_first() else {
        return Err(
            EvalError::WrongArgCount {
                name: "let".into(),
                expected: "at least 2".into(),
                got: 0,
            }
            .with_offset(pos.offset),
        );
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let(name, rest, env, pos),
        _ => eval_plain_let(first, rest, env),
    }
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(
            EvalError::WrongArgCount {
                name: "let".into(),
                expected: "at least 2".into(),
                got: 1,
            }
            .with_offset(bindings_expr.pos.offset),
        );
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let let_env = Env::child(env);

    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env)
}

fn eval_named_let(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = arguments.split_first() else {
        return Err(
            EvalError::WrongArgCount {
                name: "let".into(),
                expected: "at least 3".into(),
                got: 1,
            }
            .with_offset(pos.offset),
        );
    };

    if body.is_empty() {
        return Err(
            EvalError::WrongArgCount {
                name: "let".into(),
                expected: "at least 3".into(),
                got: 2,
            }
            .with_offset(pos.offset),
        );
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let named_env = Env::child(env);
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let closure = Rc::new(Closure {
        params,
        body: body.to_vec(),
        env: named_env.clone(),
    });

    named_env.define(name.into(), Value::Procedure(closure.clone()));

    let values = eval_binding_values(&bindings, &named_env)?;
    apply_closure_values(closure, values, pos)
}

fn parse_bindings(bindings_expr: &Expr, form_name: &str) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return Err(
            EvalError::InvalidSyntax {
                message: format!("{form_name}: expected binding list"),
            }
            .with_offset(bindings_expr.pos.offset),
        );
    };

    bindings
        .iter()
        .map(|binding| match &binding.kind {
            ExprKind::List(parts) if parts.len() == 2 => match &parts[0].kind {
                ExprKind::Symbol(name) => Ok((name.clone(), parts[1].clone())),
                _ => Err(
                    EvalError::InvalidSyntax {
                        message: format!("{form_name}: binding name must be a symbol"),
                    }
                    .with_offset(parts[0].pos.offset),
                ),
            },
            _ => Err(
                EvalError::InvalidSyntax {
                    message: format!("{form_name}: each binding must have exactly 2 elements"),
                }
                .with_offset(binding.pos.offset),
            ),
        })
        .collect()
}

fn eval_binding_values(bindings: &[(String, Expr)], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expression)| eval_expr(expression, env))
        .collect()
}

fn eval_cond(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in arguments.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(
                EvalError::InvalidSyntax {
                    message: "cond: clauses must be lists".into(),
                }
                .with_offset(clause.pos.offset),
            );
        };

        let Some((test, body)) = items.split_first() else {
            return Err(
                EvalError::InvalidSyntax {
                    message: "cond: clauses cannot be empty".into(),
                }
                .with_offset(clause.pos.offset),
            );
        };

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != arguments.len() {
                return Err(
                    EvalError::InvalidSyntax {
                        message: "cond: else clause must be last".into(),
                    }
                    .with_offset(test.pos.offset),
                );
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env)
            };
        }

        let value = eval_expr(test, env)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn parse_param_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let ExprKind::List(params) = &expr.kind else {
        return Err(
            EvalError::InvalidSyntax {
                message: "lambda: expected parameter list".into(),
            }
            .with_offset(expr.pos.offset),
        );
    };

    parse_params(params)
}

fn parse_params(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|param| match &param.kind {
            ExprKind::Symbol(name) => Ok(name.clone()),
            _ => Err(
                EvalError::InvalidSyntax {
                    message: "lambda: parameters must be symbols".into(),
                }
                .with_offset(param.pos.offset),
            ),
        })
        .collect()
}

fn eval_add(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Integer(numbers.into_iter().sum()))
}

fn eval_sub(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

    match numbers.as_slice() {
        [] => Err(
            EvalError::WrongArgCount {
                name: "-".into(),
                expected: "at least 1".into(),
                got: 0,
            }
            .with_offset(call_pos.offset),
        ),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_mul(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Integer(
        numbers.into_iter().fold(1_i64, |acc, value| acc * value),
    ))
}

fn eval_div(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let Some((first_expr, rest)) = arguments.split_first() else {
        return Err(
            EvalError::WrongArgCount {
                name: "/".into(),
                expected: "at least 2".into(),
                got: 0,
            }
            .with_offset(call_pos.offset),
        );
    };

    if rest.is_empty() {
        return Err(
            EvalError::WrongArgCount {
                name: "/".into(),
                expected: "at least 2".into(),
                got: 1,
            }
            .with_offset(call_pos.offset),
        );
    }

    let mut total = eval_number(first_expr, env)?;

    for divisor_expr in rest {
        let divisor = eval_number(divisor_expr, env)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero.with_offset(divisor_expr.pos.offset));
        }
        total /= divisor;
    }

    Ok(Value::Integer(total))
}

fn eval_compare(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

    if numbers.len() < 2 {
        return Err(
            EvalError::WrongArgCount {
                name: name.into(),
                expected: "at least 2".into(),
                got: numbers.len(),
            }
            .with_offset(call_pos.offset),
        );
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn eval_not(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    if arguments.len() != 1 {
        return Err(
            EvalError::WrongArgCount {
                name: "not".into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    }

    let value = eval_expr(&arguments[0], env)?;
    Ok(Value::Boolean(!value.is_truthy()))
}

fn eval_cons(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [head_expr, tail_expr] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "cons".into(),
                expected: "exactly 2".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    };

    let head = eval_expr(head_expr, env)?;
    let tail = eval_expr(tail_expr, env)?;

    match tail {
        Value::List(mut items) => {
            items.insert(0, head);
            Ok(Value::List(items))
        }
        other => Err(
            EvalError::TypeMismatch {
                expected: "list".into(),
                found: other.kind().into(),
            }
            .with_offset(tail_expr.pos.offset),
        ),
    }
}

fn eval_car(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "car".into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::List(_) => Err(
            EvalError::TypeMismatch {
                expected: "pair".into(),
                found: "list".into(),
            }
            .with_offset(list_expr.pos.offset),
        ),
        other => Err(
            EvalError::TypeMismatch {
                expected: "pair".into(),
                found: other.kind().into(),
            }
            .with_offset(list_expr.pos.offset),
        ),
    }
}

fn eval_cdr(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "cdr".into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::List(_) => Err(
            EvalError::TypeMismatch {
                expected: "pair".into(),
                found: "list".into(),
            }
            .with_offset(list_expr.pos.offset),
        ),
        other => Err(
            EvalError::TypeMismatch {
                expected: "pair".into(),
                found: other.kind().into(),
            }
            .with_offset(list_expr.pos.offset),
        ),
    }
}

fn eval_null_pred(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "null?".into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    };

    let value = eval_expr(list_expr, env)?;
    Ok(Value::Boolean(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_list_builtin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    Ok(Value::List(eval_args(arguments, env)?))
}

fn eval_length(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: "length".into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        other => Err(
            EvalError::TypeMismatch {
                expected: "list".into(),
                found: other.kind().into(),
            }
            .with_offset(list_expr.pos.offset),
        ),
    }
}

fn eval_append(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        match value {
            Value::List(mut list_items) => items.append(&mut list_items),
            other => {
                return Err(
                    EvalError::TypeMismatch {
                        expected: "list".into(),
                        found: other.kind().into(),
                    }
                    .with_offset(argument.pos.offset),
                );
            }
        }
    }

    Ok(Value::List(items))
}

fn eval_type_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(
            EvalError::WrongArgCount {
                name: name.into(),
                expected: "exactly 1".into(),
                got: arguments.len(),
            }
            .with_offset(call_pos.offset),
        );
    };

    let value = eval_expr(expr, env)?;
    Ok(Value::Boolean(predicate(&value)))
}

fn eval_and(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(false);

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_number_args(arguments: &[Expr], env: &EnvRef) -> Result<Vec<i64>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_number(argument, env))
        .collect()
}

fn eval_number(expr: &Expr, env: &EnvRef) -> Result<i64, EvalError> {
    match eval_expr(expr, env)? {
        Value::Integer(value) => Ok(value),
        other => Err(
            EvalError::TypeMismatch {
                expected: "number".into(),
                found: other.kind().into(),
            }
            .with_offset(expr.pos.offset),
        ),
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
    let expressions = Parser::new(input)
        .parse_program()
        .map_err(|err| err.resolve_positions(input))?;
    let value = eval_program(&expressions).map_err(|err| err.resolve_positions(input))?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;
