pub mod error;

pub use error::EvalError;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct PositionedExpr {
    expr: Expr,
    position: Position,
}

#[derive(Debug, Clone)]
struct SchemeString {
    inner: Rc<SchemeStringInner>,
}

#[derive(Debug)]
struct SchemeStringInner {
    chars: RefCell<Vec<char>>,
    mutable: bool,
}

impl SchemeString {
    fn new_immutable(value: impl AsRef<str>) -> Self {
        Self::from_chars(value.as_ref().chars().collect(), false)
    }

    fn new_mutable(value: impl AsRef<str>) -> Self {
        Self::from_chars(value.as_ref().chars().collect(), true)
    }

    fn from_chars(chars: Vec<char>, mutable: bool) -> Self {
        Self {
            inner: Rc::new(SchemeStringInner {
                chars: RefCell::new(chars),
                mutable,
            }),
        }
    }

    fn copy_mutable(&self) -> Self {
        Self::from_chars(self.chars(), true)
    }

    fn chars(&self) -> Vec<char> {
        self.inner.chars.borrow().clone()
    }

    fn len(&self) -> usize {
        self.inner.chars.borrow().len()
    }

    fn get(&self, index: usize) -> char {
        self.inner.chars.borrow()[index]
    }

    fn set(&self, index: usize, value: char, name: &'static str) -> Result<(), EvalError> {
        if !self.inner.mutable {
            return Err(EvalError::ImmutableString { name });
        }

        self.inner.chars.borrow_mut()[index] = value;
        Ok(())
    }

    fn to_plain_string(&self) -> String {
        self.inner.chars.borrow().iter().collect()
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Debug, Clone)]
enum Procedure {
    Builtin {
        name: &'static str,
    },
    Lambda {
        params: LambdaParams,
        body: Vec<Expr>,
        env: EnvRef,
    },
}

#[derive(Debug, Clone)]
struct LambdaParams {
    required: Vec<String>,
    rest: Option<String>,
}

impl LambdaParams {
    fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }
}

type EnvRef = Rc<Env>;

#[derive(Debug)]
struct Env {
    values: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

#[derive(Debug, Default)]
struct EvalContext {
    output: String,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            values: RefCell::new(HashMap::new()),
            parent,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.values.borrow_mut().insert(name.into(), value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.values
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }

    fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        {
            let mut values = self.values.borrow_mut();
            if let Some(slot) = values.get_mut(name) {
                *slot = value;
                return Ok(());
            }
        }

        if let Some(parent) = &self.parent {
            parent.set(name, value)
        } else {
            Err(EvalError::UnboundVariable {
                name: name.to_string(),
            })
        }
    }
}

impl Procedure {
    fn call(&self, args: Vec<Value>, context: &mut EvalContext) -> Result<Value, EvalError> {
        match self {
            Self::Builtin { name } => apply_builtin(name, &args, context),
            Self::Lambda { params, body, env } => {
                if args.len() < params.required.len()
                    || (params.rest.is_none() && args.len() != params.required.len())
                {
                    return Err(EvalError::WrongArgCount {
                        name: "lambda",
                        expected: if params.rest.is_some() {
                            "at least the declared number of required arguments"
                        } else {
                            "exactly the declared number of arguments"
                        },
                        got: args.len(),
                    });
                }

                let call_env = Env::new(Some(env.clone()));
                let mut args = args.into_iter();

                for param in &params.required {
                    let value = args.next().expect("arity checked before binding lambda args");
                    call_env.define(param.clone(), value);
                }

                if let Some(rest) = &params.rest {
                    call_env.define(rest.clone(), Value::List(args.collect()));
                }

                eval_sequence(body, &call_env, context)
            }
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
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
            Self::List(_) => "list",
            Self::Pair(_, _) => "pair",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => {
                let escaped = value
                    .to_plain_string()
                    .chars()
                    .flat_map(|ch| match ch {
                        '\\' => ['\\', '\\'].into_iter().collect::<Vec<_>>(),
                        '"' => ['\\', '"'].into_iter().collect::<Vec<_>>(),
                        '\n' => ['\\', 'n'].into_iter().collect::<Vec<_>>(),
                        '\t' => ['\\', 't'].into_iter().collect::<Vec<_>>(),
                        other => [other].into_iter().collect::<Vec<_>>(),
                    })
                    .collect::<String>();

                format!("\"{escaped}\"")
            }
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value),
            Self::List(items) => {
                let rendered = items
                    .iter()
                    .map(Value::render)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({rendered})")
            }
            Self::Pair(car, cdr) => render_pair(car, cdr, false),
            Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }

    fn display_render(&self) -> String {
        match self {
            Self::String(value) => value.to_plain_string(),
            Self::Char(value) => value.to_string(),
            Self::List(items) => {
                let rendered = items
                    .iter()
                    .map(Value::display_render)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({rendered})")
            }
            Self::Pair(car, cdr) => render_pair(car, cdr, true),
            _ => self.render(),
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<PositionedExpr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            let position = self.current_position();
            let expr = self.parse_expr()?;
            expressions.push(PositionedExpr { expr, position });
            self.skip_ignored();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        let Some(ch) = self.peek_char() else {
            return Err(self.error(EvalError::UnexpectedEof));
        };

        match ch {
            '(' => self.parse_list(),
            '\'' => self.parse_quote_shorthand(),
            '"' => self.parse_string(),
            '#' => self.parse_hash_literal(),
            ')' => Err(self.error(EvalError::UnexpectedToken { token: ")".into() })),
            '-' if self
                .peek_second_char()
                .is_some_and(|next| next.is_ascii_digit()) =>
            {
                self.parse_number()
            }
            ch if ch.is_ascii_digit() => self.parse_number(),
            _ => self.parse_symbol(),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), quoted]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error(EvalError::UnexpectedEof)),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self
                        .bump_char()
                        .ok_or_else(|| self.error(EvalError::UnterminatedString))?;
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(self.error(EvalError::UnterminatedString))
    }

    fn parse_hash_literal(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('#')?;
        match self.bump_char() {
            Some('t') => Ok(Expr::Boolean(true)),
            Some('f') => Ok(Expr::Boolean(false)),
            Some('\\') => self.parse_character_literal(),
            Some(other) => Err(self.error(EvalError::InvalidBoolean {
                literal: format!("#{other}"),
            })),
            None => Err(self.error(EvalError::UnexpectedEof)),
        }
    }

    fn parse_character_literal(&mut self) -> Result<Expr, EvalError> {
        let Some(first) = self.peek_char() else {
            return Err(self.error(EvalError::InvalidCharacter {
                literal: "#\\".into(),
            }));
        };

        if first.is_whitespace() {
            return Err(self.error(EvalError::InvalidCharacter {
                literal: "#\\".into(),
            }));
        }

        if first.is_ascii_alphabetic() {
            let start = self.offset;
            while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
                self.bump_char();
            }

            let literal = &self.input[start..self.offset];
            let value = match literal {
                "space" => ' ',
                "newline" => '\n',
                _ if literal.chars().count() == 1 => literal.chars().next().unwrap(),
                _ => {
                    return Err(self.error(EvalError::InvalidCharacter {
                        literal: format!("#\\{literal}"),
                    }));
                }
            };

            Ok(Expr::Char(value))
        } else {
            Ok(Expr::Char(self.bump_char().unwrap()))
        }
    }

    fn parse_number(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;
        if self.peek_char() == Some('-') {
            self.bump_char();
        }

        let mut saw_digit = false;
        while self.peek_char().is_some_and(|ch| ch.is_ascii_digit()) {
            saw_digit = true;
            self.bump_char();
        }

        let literal = &self.input[start..self.offset];
        if !saw_digit {
            return Err(self.error(EvalError::InvalidNumber {
                literal: literal.into(),
            }));
        }

        literal.parse::<i64>().map(Expr::Integer).map_err(|_| {
            self.error(EvalError::InvalidNumber {
                literal: literal.into(),
            })
        })
    }

    fn parse_symbol(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
            self.bump_char();
        }

        if start == self.offset {
            let token = self
                .peek_char()
                .map(|ch| ch.to_string())
                .unwrap_or_else(|| "<eof>".into());
            return Err(self.error(EvalError::UnexpectedToken { token }));
        }

        Ok(Expr::Symbol(self.input[start..self.offset].into()))
    }

    fn skip_ignored(&mut self) {
        loop {
            while self.peek_char().is_some_and(char::is_whitespace) {
                self.bump_char();
            }

            if self.peek_char() != Some(';') {
                return;
            }

            while let Some(ch) = self.bump_char() {
                if ch == '\n' {
                    break;
                }
            }
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(self.error(EvalError::UnexpectedToken {
                token: ch.to_string(),
            })),
            None => Err(self.error(EvalError::UnexpectedEof)),
        }
    }

    fn bump_char(&mut self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        let ch = chars.next()?;
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        chars.next()?;
        chars.next()
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn error(&self, error: EvalError) -> EvalError {
        let position = self.current_position();
        error.with_position(position.line, position.column)
    }
}

fn eval_expr_in_env(
    expr: &Expr,
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::Char(value) => Ok(Value::Char(*value)),
        Expr::String(value) => Ok(Value::String(SchemeString::new_immutable(value))),
        Expr::Symbol(name) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_application(items, env, context),
    }
}

fn eval_application(
    items: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::NotAProcedure);
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(tail, env, context),
            "begin" => return eval_begin(tail, env, context),
            "cond" => return eval_cond(tail, env, context),
            "define" => return eval_define(tail, env, context),
            "if" => return eval_if(tail, env, context),
            "lambda" => return eval_lambda(tail, env),
            "let" => return eval_let(tail, env, context),
            "or" => return eval_or(tail, env, context),
            "quote" => return eval_quote(tail),
            "set!" => return eval_set(tail, env, context),
            _ => {}
        }
    }

    let procedure = eval_expr_in_env(head, env, context)?;
    let args = tail
        .iter()
        .map(|expr| eval_expr_in_env(expr, env, context))
        .collect::<Result<Vec<_>, EvalError>>()?;

    apply_procedure(procedure, args, context)
}

fn eval_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expression in expressions {
        last = eval_expr_in_env(expression, env, context)?;
    }

    Ok(last)
}

fn eval_and(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval_expr_in_env(arg, env, context)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_or(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr_in_env(arg, env, context)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, context)
}

fn eval_cond(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected clauses to be lists",
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected each clause to contain a test",
            });
        };

        if matches!(test, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must be last",
                });
            }

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must contain a body",
                });
            }

            return eval_sequence(body, env, context);
        }

        let value = eval_expr_in_env(test, env, context)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, context)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_define(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    let Some((target, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "define",
            expected: "a binding target and value",
            got: 0,
        });
    };

    match target {
        Expr::Symbol(name) => {
            if rest.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let value = eval_expr_in_env(&rest[0], env, context)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected a function name",
                });
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected a function name",
                });
            };

            if rest.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected at least one body expression",
                });
            }

            let params = parse_param_names(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params,
                body: rest.to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidForm {
            name: "define",
            message: "expected a symbol or function signature",
        }),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "exactly 3 arguments",
            got: args.len(),
        });
    }

    if eval_expr_in_env(&args[0], env, context)?.is_truthy() {
        eval_expr_in_env(&args[1], env, context)
    } else {
        eval_expr_in_env(&args[2], env, context)
    }
}

fn eval_set(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2 arguments",
            got: args.len(),
        });
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::InvalidForm {
            name: "set!",
            message: "expected a symbol as the binding target",
        });
    };

    let value = eval_expr_in_env(&args[1], env, context)?;
    env.set(name, value)?;
    Ok(Value::Void)
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "a parameter list and body",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "lambda",
            message: "expected at least one body expression",
        });
    }

    let params = parse_params(params_expr, "lambda")?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn eval_let(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "bindings and at least one body expression",
            got: 0,
        });
    };

    match first {
        Expr::List(_) => {
            if rest.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected at least one body expression",
                });
            }

            let bindings = parse_let_bindings(first, "let")?;
            let values = bindings
                .iter()
                .map(|(_, expr)| eval_expr_in_env(expr, env, context))
                .collect::<Result<Vec<_>, EvalError>>()?;

            let let_env = Env::new(Some(env.clone()));
            for ((name, _), value) in bindings.into_iter().zip(values) {
                let_env.define(name, value);
            }

            eval_sequence(rest, &let_env, context)
        }
        Expr::Symbol(name) => {
            let Some((bindings_expr, body)) = rest.split_first() else {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected bindings and at least one body expression",
                });
            };

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected at least one body expression",
                });
            }

            let bindings = parse_let_bindings(bindings_expr, "let")?;
            let args = bindings
                .iter()
                .map(|(_, expr)| eval_expr_in_env(expr, env, context))
                .collect::<Result<Vec<_>, EvalError>>()?;
            let params = bindings
                .into_iter()
                .map(|(binding_name, _)| binding_name)
                .collect();

            let let_env = Env::new(Some(env.clone()));
            let_env.define(name.clone(), Value::Void);

            let procedure = Rc::new(Procedure::Lambda {
                params: LambdaParams::fixed(params),
                body: body.to_vec(),
                env: let_env.clone(),
            });
            let value = Value::Procedure(procedure.clone());
            let_env.define(name.clone(), value.clone());

            apply_procedure(value, args, context)
        }
        _ => Err(EvalError::InvalidForm {
            name: "let",
            message: "expected a binding list or let name",
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(quote_expr(&args[0]))
}

fn parse_params(expr: &Expr, name: &'static str) -> Result<LambdaParams, EvalError> {
    match expr {
        Expr::List(items) => parse_param_names(items, name),
        Expr::Symbol(symbol) => Ok(LambdaParams {
            required: Vec::new(),
            rest: Some(symbol.clone()),
        }),
        _ => Err(EvalError::InvalidForm {
            name,
            message: "expected a parameter list",
        }),
    }
}

fn parse_let_bindings(expr: &Expr, name: &'static str) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::InvalidForm {
            name,
            message: "expected a binding list",
        });
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) if items.len() == 2 => match (&items[0], &items[1]) {
                (Expr::Symbol(symbol), value) => Ok((symbol.clone(), value.clone())),
                _ => Err(EvalError::InvalidForm {
                    name,
                    message: "expected binding names to be symbols",
                }),
            },
            _ => Err(EvalError::InvalidForm {
                name,
                message: "expected each binding to have a name and value",
            }),
        })
        .collect()
}

fn parse_param_names(items: &[Expr], name: &'static str) -> Result<LambdaParams, EvalError> {
    let mut required = Vec::new();
    let mut iter = items.iter();

    while let Some(item) = iter.next() {
        match item {
            Expr::Symbol(symbol) if symbol == "." => {
                let Some(Expr::Symbol(rest)) = iter.next() else {
                    return Err(EvalError::InvalidForm {
                        name,
                        message: "expected a rest parameter name after .",
                    });
                };

                if iter.next().is_some() {
                    return Err(EvalError::InvalidForm {
                        name,
                        message: "expected . to appear before the final parameter name only",
                    });
                }

                return Ok(LambdaParams {
                    required,
                    rest: Some(rest.clone()),
                });
            }
            Expr::Symbol(symbol) => required.push(symbol.clone()),
            _ => {
                return Err(EvalError::InvalidForm {
                    name,
                    message: "expected parameter names to be symbols",
                });
            }
        }
    }

    Ok(LambdaParams::fixed(required))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::Char(value) => Value::Char(*value),
        Expr::String(value) => Value::String(SchemeString::new_immutable(value)),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn render_pair(car: &Value, cdr: &Value, display: bool) -> String {
    let mut rendered = vec![if display {
        car.display_render()
    } else {
        car.render()
    }];
    let mut tail = cdr;

    loop {
        match tail {
            Value::List(items) => {
                rendered.extend(items.iter().map(|item| {
                    if display {
                        item.display_render()
                    } else {
                        item.render()
                    }
                }));
                return format!("({})", rendered.join(" "));
            }
            Value::Pair(next_car, next_cdr) => {
                rendered.push(if display {
                    next_car.display_render()
                } else {
                    next_car.render()
                });
                tail = next_cdr;
            }
            other => {
                let tail_rendered = if display {
                    other.display_render()
                } else {
                    other.render()
                };
                return format!("({} . {tail_rendered})", rendered.join(" "));
            }
        }
    }
}

fn root_env() -> EnvRef {
    let env = Env::new(None);
    for name in [
        "+",
        "-",
        "*",
        "/",
        "<",
        "<=",
        "=",
        ">",
        ">=",
        "abs",
        "apply",
        "append",
        "assoc",
        "boolean?",
        "char?",
        "char-alphabetic?",
        "char-downcase",
        "char-numeric?",
        "char-upcase",
        "char<?",
        "char=?",
        "car",
        "cdr",
        "cons",
        "display",
        "eq?",
        "equal?",
        "even?",
        "expt",
        "length",
        "list",
        "list-ref",
        "list-tail",
        "list?",
        "map",
        "max",
        "min",
        "modulo",
        "negative?",
        "newline",
        "not",
        "number->string",
        "null?",
        "number?",
        "odd?",
        "pair?",
        "positive?",
        "quotient",
        "remainder",
        "string-ci=?",
        "string-downcase",
        "string<?",
        "string=?",
        "string->number",
        "string->symbol",
        "string-append",
        "string-copy",
        "string-length",
        "string-ref",
        "string-set!",
        "string-upcase",
        "string?",
        "substring",
        "symbol?",
        "symbol->string",
        "write",
        "zero?",
    ] {
        env.define(name, Value::Procedure(Rc::new(Procedure::Builtin { name })));
    }

    env
}

fn apply_procedure(
    procedure: Value,
    args: Vec<Value>,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match procedure {
        Value::Procedure(procedure) => procedure.call(args, context),
        _ => Err(EvalError::NotAProcedure),
    }
}

fn apply_builtin(
    name: &'static str,
    args: &[Value],
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let numbers = extract_numbers("+", args)?;
            Ok(Value::Integer(numbers.iter().sum()))
        }
        "abs" => {
            let number = expect_number_arg("abs", args)?;
            number
                .checked_abs()
                .map(Value::Integer)
                .ok_or(EvalError::InvalidArgument {
                    name: "abs",
                    message: "overflow",
                })
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::WrongArgCount {
                    name: "apply",
                    expected: "at least 2 arguments",
                    got: args.len(),
                });
            }

            let mut applied_args = args[1..args.len() - 1].to_vec();
            let tail = args.last().expect("apply arity checked before reading tail");

            match tail {
                Value::List(items) => applied_args.extend(items.iter().cloned()),
                other => {
                    return Err(EvalError::ExpectedList {
                        name: "apply",
                        found: other.type_name(),
                    });
                }
            }

            apply_procedure(args[0].clone(), applied_args, context)
        }
        "append" => {
            let mut values = Vec::new();
            for arg in args {
                match arg {
                    Value::List(items) => values.extend(items.iter().cloned()),
                    other => {
                        return Err(EvalError::ExpectedList {
                            name: "append",
                            found: other.type_name(),
                        });
                    }
                }
            }

            Ok(Value::List(values))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "assoc",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let alist = expect_list("assoc", &args[1])?;
            for entry in alist {
                let Some(key) = pair_first(entry) else {
                    return Err(EvalError::ExpectedPair {
                        name: "assoc",
                        found: entry.type_name(),
                    });
                };

                if equal_values(&args[0], key) {
                    return Ok(entry.clone());
                }
            }

            Ok(Value::Boolean(false))
        }
        "*" => {
            let numbers = extract_numbers("*", args)?;
            Ok(Value::Integer(numbers.iter().product()))
        }
        "-" => {
            let numbers = extract_numbers("-", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "-",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            let value = if rest.is_empty() {
                -*first
            } else {
                rest.iter().fold(*first, |acc, value| acc - value)
            };

            Ok(Value::Integer(value))
        }
        "/" => {
            let numbers = extract_numbers("/", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            if rest.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 2 arguments",
                    got: 1,
                });
            }

            let mut result = *first;
            for value in rest {
                if *value == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= value;
            }

            Ok(Value::Integer(result))
        }
        "<" => compare_numbers("<", args, |left, right| left < right),
        "<=" => compare_numbers("<=", args, |left, right| left <= right),
        "=" => compare_numbers("=", args, |left, right| left == right),
        ">" => compare_numbers(">", args, |left, right| left > right),
        ">=" => compare_numbers(">=", args, |left, right| left >= right),
        "boolean?" => {
            predicate_builtin("boolean?", args, |value| matches!(value, Value::Boolean(_)))
        }
        "char?" => predicate_builtin("char?", args, |value| matches!(value, Value::Char(_))),
        "char-alphabetic?" => predicate_builtin("char-alphabetic?", args, |value| {
            matches!(value, Value::Char(ch) if ch.is_alphabetic())
        }),
        "char-downcase" => {
            let ch = expect_char_arg("char-downcase", args)?;
            Ok(Value::Char(ch.to_ascii_lowercase()))
        }
        "char-numeric?" => predicate_builtin("char-numeric?", args, |value| {
            matches!(value, Value::Char(ch) if ch.is_numeric())
        }),
        "char-upcase" => {
            let ch = expect_char_arg("char-upcase", args)?;
            Ok(Value::Char(ch.to_ascii_uppercase()))
        }
        "char<?" => compare_characters("char<?", args, |left, right| left < right),
        "char=?" => compare_characters("char=?", args, |left, right| left == right),
        "car" => {
            let value = expect_pair_value("car", args)?;
            Ok(match value {
                Value::List(items) => items[0].clone(),
                Value::Pair(car, _) => (**car).clone(),
                _ => unreachable!("expect_pair_value only returns pairs"),
            })
        }
        "cdr" => {
            let value = expect_pair_value("cdr", args)?;
            Ok(match value {
                Value::List(items) => Value::List(items[1..].to_vec()),
                Value::Pair(_, cdr) => (**cdr).clone(),
                _ => unreachable!("expect_pair_value only returns pairs"),
            })
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "cons",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            match &args[1] {
                Value::List(rest) => {
                    let mut values = Vec::with_capacity(rest.len() + 1);
                    values.push(args[0].clone());
                    values.extend(rest.iter().cloned());
                    Ok(Value::List(values))
                }
                other => Ok(Value::Pair(
                    Box::new(args[0].clone()),
                    Box::new(other.clone()),
                )),
            }
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "display",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            context.output.push_str(&args[0].display_render());
            Ok(Value::Void)
        }
        "length" => {
            let list = expect_list_arg("length", args)?;
            Ok(Value::Integer(list.len() as i64))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "list-ref",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let list = expect_list("list-ref", &args[0])?;
            let index = expect_index("list-ref", &args[1], list.len())?;
            Ok(list[index].clone())
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "list-tail",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let list = expect_list("list-tail", &args[0])?;
            let index = expect_index_inclusive_end("list-tail", &args[1], list.len())?;
            Ok(Value::List(list[index..].to_vec()))
        }
        "list?" => predicate_builtin("list?", args, |value| matches!(value, Value::List(_))),
        "map" => apply_map(args, context),
        "max" => {
            let numbers = extract_numbers("max", args)?;
            let Some(maximum) = numbers.into_iter().max() else {
                return Err(EvalError::WrongArgCount {
                    name: "max",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            Ok(Value::Integer(maximum))
        }
        "min" => {
            let numbers = extract_numbers("min", args)?;
            let Some(minimum) = numbers.into_iter().min() else {
                return Err(EvalError::WrongArgCount {
                    name: "min",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            Ok(Value::Integer(minimum))
        }
        "modulo" => {
            let (left, right) = expect_two_numbers("modulo", args)?;
            if right == 0 {
                return Err(EvalError::DivisionByZero);
            }

            let mut remainder = left % right;
            if remainder != 0 && (remainder > 0) != (right > 0) {
                remainder += right;
            }

            Ok(Value::Integer(remainder))
        }
        "negative?" => {
            let number = expect_number_arg("negative?", args)?;
            Ok(Value::Boolean(number < 0))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "newline",
                    expected: "exactly 0 arguments",
                    got: args.len(),
                });
            }

            context.output.push('\n');
            Ok(Value::Void)
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "not",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "number->string" => {
            let number = expect_number_arg("number->string", args)?;
            Ok(Value::String(SchemeString::new_mutable(number.to_string())))
        }
        "null?" => predicate_builtin(
            "null?",
            args,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "number?" => predicate_builtin("number?", args, |value| matches!(value, Value::Integer(_))),
        "odd?" => {
            let number = expect_number_arg("odd?", args)?;
            Ok(Value::Boolean(number.rem_euclid(2) == 1))
        }
        "pair?" => predicate_builtin(
            "pair?",
            args,
            |value| match value {
                Value::Pair(_, _) => true,
                Value::List(items) => !items.is_empty(),
                _ => false,
            },
        ),
        "positive?" => {
            let number = expect_number_arg("positive?", args)?;
            Ok(Value::Boolean(number > 0))
        }
        "quotient" => {
            let (left, right) = expect_two_numbers("quotient", args)?;
            if right == 0 {
                return Err(EvalError::DivisionByZero);
            }

            Ok(Value::Integer(left / right))
        }
        "remainder" => {
            let (left, right) = expect_two_numbers("remainder", args)?;
            if right == 0 {
                return Err(EvalError::DivisionByZero);
            }

            Ok(Value::Integer(left % right))
        }
        "string->number" => {
            let value = expect_string_arg("string->number", args)?;
            match value.to_plain_string().parse::<i64>() {
                Ok(number) => Ok(Value::Integer(number)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        "string->symbol" => {
            let value = expect_string_arg("string->symbol", args)?;
            Ok(Value::Symbol(value.to_plain_string()))
        }
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                result.push_str(&expect_string("string-append", arg)?.to_plain_string());
            }
            Ok(Value::String(SchemeString::new_mutable(result)))
        }
        "string-copy" => {
            let value = expect_string_arg("string-copy", args)?;
            Ok(Value::String(value.copy_mutable()))
        }
        "string-ci=?" => compare_strings("string-ci=?", args, |left, right| {
            left.to_lowercase() == right.to_lowercase()
        }),
        "string-downcase" => {
            let value = expect_string_arg("string-downcase", args)?;
            Ok(Value::String(SchemeString::new_mutable(
                value.to_plain_string().to_lowercase(),
            )))
        }
        "string=?" => compare_strings("string=?", args, |left, right| left == right),
        "string-length" => {
            let value = expect_string_arg("string-length", args)?;
            Ok(Value::Integer(value.len() as i64))
        }
        "string<?" => compare_strings("string<?", args, |left, right| left < right),
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "string-ref",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let string = expect_string("string-ref", &args[0])?;
            let index = expect_index("string-ref", &args[1], string.len())?;
            Ok(Value::Char(string.get(index)))
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount {
                    name: "string-set!",
                    expected: "exactly 3 arguments",
                    got: args.len(),
                });
            }

            let string = expect_string("string-set!", &args[0])?;
            let index = expect_index("string-set!", &args[1], string.len())?;
            let value = expect_char("string-set!", &args[2])?;
            string.set(index, value, "string-set!")?;
            Ok(Value::Void)
        }
        "string-upcase" => {
            let value = expect_string_arg("string-upcase", args)?;
            Ok(Value::String(SchemeString::new_mutable(
                value.to_plain_string().to_uppercase(),
            )))
        }
        "string?" => predicate_builtin("string?", args, |value| matches!(value, Value::String(_))),
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount {
                    name: "substring",
                    expected: "exactly 3 arguments",
                    got: args.len(),
                });
            }

            let chars = expect_string("substring", &args[0])?.chars();
            let start = expect_index_inclusive_end("substring", &args[1], chars.len())?;
            let end = expect_index_inclusive_end("substring", &args[2], chars.len())?;

            if start > end {
                return Err(EvalError::InvalidRange {
                    name: "substring",
                    start: start as i64,
                    end: end as i64,
                });
            }

            Ok(Value::String(SchemeString::new_mutable(
                chars[start..end].iter().collect::<String>(),
            )))
        }
        "symbol?" => predicate_builtin("symbol?", args, |value| matches!(value, Value::Symbol(_))),
        "symbol->string" => {
            let value = expect_symbol_arg("symbol->string", args)?;
            Ok(Value::String(SchemeString::new_mutable(value)))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "eq?",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(eq_values(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "equal?",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(equal_values(&args[0], &args[1])))
        }
        "even?" => {
            let number = expect_number_arg("even?", args)?;
            Ok(Value::Boolean(number.rem_euclid(2) == 0))
        }
        "expt" => {
            let (base, exponent) = expect_two_numbers("expt", args)?;
            if exponent < 0 {
                return Err(EvalError::InvalidArgument {
                    name: "expt",
                    message: "expected a non-negative exponent",
                });
            }

            base.checked_pow(exponent as u32)
                .map(Value::Integer)
                .ok_or(EvalError::InvalidArgument {
                    name: "expt",
                    message: "overflow",
                })
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "write",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            context.output.push_str(&args[0].render());
            Ok(Value::Void)
        }
        "zero?" => {
            let number = expect_number_arg("zero?", args)?;
            Ok(Value::Boolean(number == 0))
        }
        _ => Err(EvalError::UnknownProcedure {
            name: name.to_string(),
        }),
    }
}

fn compare_numbers(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = extract_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn compare_characters(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let chars = args
        .iter()
        .map(|value| expect_char(name, value))
        .collect::<Result<Vec<_>, EvalError>>()?;

    if chars.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: chars.len(),
        });
    }

    Ok(Value::Boolean(
        chars.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn compare_strings(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let strings = args
        .iter()
        .map(|value| expect_string(name, value).map(|string| string.to_plain_string()))
        .collect::<Result<Vec<_>, EvalError>>()?;

    if strings.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: strings.len(),
        });
    }

    Ok(Value::Boolean(
        strings.windows(2).all(|pair| predicate(&pair[0], &pair[1])),
    ))
}

fn extract_numbers(name: &'static str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            }),
        })
        .collect()
}

fn expect_two_numbers(name: &'static str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 2 arguments",
            got: args.len(),
        });
    }

    let left = match args[0] {
        Value::Integer(number) => number,
        ref other => {
            return Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            });
        }
    };
    let right = match args[1] {
        Value::Integer(number) => number,
        ref other => {
            return Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            });
        }
    };

    Ok((left, right))
}

fn expect_number_arg(name: &'static str, args: &[Value]) -> Result<i64, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    match args[0] {
        Value::Integer(number) => Ok(number),
        ref other => Err(EvalError::ExpectedNumber {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_string(name: &'static str, value: &Value) -> Result<SchemeString, EvalError> {
    match value {
        Value::String(string) => Ok(string.clone()),
        other => Err(EvalError::ExpectedString {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_string_arg(name: &'static str, args: &[Value]) -> Result<SchemeString, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_string(name, &args[0])
}

fn expect_char(name: &'static str, value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(EvalError::ExpectedCharacter {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_char_arg(name: &'static str, args: &[Value]) -> Result<char, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_char(name, &args[0])
}

fn expect_symbol_arg<'a>(name: &'static str, args: &'a [Value]) -> Result<&'a str, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    match &args[0] {
        Value::Symbol(symbol) => Ok(symbol),
        other => Err(EvalError::ExpectedSymbol {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_index(name: &'static str, value: &Value, len: usize) -> Result<usize, EvalError> {
    let index = match value {
        Value::Integer(index) => *index,
        other => {
            return Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            });
        }
    };

    if index < 0 || index as usize >= len {
        return Err(EvalError::IndexOutOfBounds { name, index });
    }

    Ok(index as usize)
}

fn expect_index_inclusive_end(
    name: &'static str,
    value: &Value,
    len: usize,
) -> Result<usize, EvalError> {
    let index = match value {
        Value::Integer(index) => *index,
        other => {
            return Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            });
        }
    };

    if index < 0 || index as usize > len {
        return Err(EvalError::IndexOutOfBounds { name, index });
    }

    Ok(index as usize)
}

fn predicate_builtin(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
}

fn expect_list<'a>(name: &'static str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(EvalError::ExpectedList {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_list_arg<'a>(name: &'static str, args: &'a [Value]) -> Result<&'a [Value], EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_list(name, &args[0])
}

fn expect_pair_value<'a>(name: &'static str, args: &'a [Value]) -> Result<&'a Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    match &args[0] {
        Value::List(items) if items.is_empty() => Err(EvalError::ExpectedPair {
            name,
            found: "list",
        }),
        value @ Value::List(_) | value @ Value::Pair(_, _) => Ok(value),
        other => Err(EvalError::ExpectedPair {
            name,
            found: other.type_name(),
        }),
    }
}

fn pair_first(value: &Value) -> Option<&Value> {
    match value {
        Value::List(items) => items.first(),
        Value::Pair(car, _) => Some(car),
        _ => None,
    }
}

fn eq_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.inner, &right.inner),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::List(left), Value::List(right)) => left.is_empty() && right.is_empty(),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.to_plain_string() == right.to_plain_string(),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left_item, right_item)| equal_values(left_item, right_item))
        }
        (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
            equal_values(left_car, right_car) && equal_values(left_cdr, right_cdr)
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn apply_map(args: &[Value], context: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "a procedure and at least one list",
            got: args.len(),
        });
    }

    if !matches!(args[0], Value::Procedure(_)) {
        return Err(EvalError::NotAProcedure);
    }

    let lists = args[1..]
        .iter()
        .map(|value| expect_list("map", value))
        .collect::<Result<Vec<_>, EvalError>>()?;
    let length = lists.iter().map(|list| list.len()).min().unwrap_or(0);

    let mut results = Vec::with_capacity(length);
    for index in 0..length {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_procedure(args[0].clone(), call_args, context)?);
    }

    Ok(Value::List(results))
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    if program.is_empty() {
        return Err(EvalError::EmptyInput.with_position(1, 1));
    }

    let env = root_env();
    let mut context = EvalContext::default();
    let mut last_value = Value::Void;

    for expression in &program {
        last_value = eval_expr_in_env(&expression.expr, &env, &mut context).map_err(|error| {
            error.with_position(expression.position.line, expression.position.column)
        })?;
    }

    Ok((last_value, context.output))
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
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'' | '"')
}

#[cfg(test)]
mod tests;
