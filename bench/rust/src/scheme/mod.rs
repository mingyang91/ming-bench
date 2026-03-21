pub mod error;
mod expand;

pub use error::EvalError;

use error::SourcePos;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Int(i64, SourcePos),
    Bool(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Int(_, pos)
            | Self::Bool(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Apply,
    CallCc,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    GreaterEqual,
    Not,
    Cons,
    Car,
    Cdr,
    IsNull,
    List,
    Map,
    Length,
    IsString,
    IsNumber,
    IsBoolean,
    IsPair,
    IsSymbol,
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringToList,
    ListToString,
    StringRef,
    StringCopy,
    StringSet,
    IsChar,
    CharToInteger,
    IntegerToChar,
}

const BUILTIN_BINDINGS: &[(&str, Builtin)] = &[
    ("+", Builtin::Add),
    ("-", Builtin::Sub),
    ("*", Builtin::Mul),
    ("/", Builtin::Div),
    ("apply", Builtin::Apply),
    ("call/cc", Builtin::CallCc),
    ("<", Builtin::LessThan),
    (">", Builtin::GreaterThan),
    ("=", Builtin::Equal),
    ("<=", Builtin::LessEqual),
    (">=", Builtin::GreaterEqual),
    ("not", Builtin::Not),
    ("cons", Builtin::Cons),
    ("car", Builtin::Car),
    ("cdr", Builtin::Cdr),
    ("null?", Builtin::IsNull),
    ("list", Builtin::List),
    ("map", Builtin::Map),
    ("length", Builtin::Length),
    ("string?", Builtin::IsString),
    ("number?", Builtin::IsNumber),
    ("boolean?", Builtin::IsBoolean),
    ("pair?", Builtin::IsPair),
    ("symbol?", Builtin::IsSymbol),
    ("display", Builtin::Display),
    ("write", Builtin::Write),
    ("newline", Builtin::Newline),
    ("string-append", Builtin::StringAppend),
    ("string-length", Builtin::StringLength),
    ("substring", Builtin::Substring),
    ("string->number", Builtin::StringToNumber),
    ("number->string", Builtin::NumberToString),
    ("symbol->string", Builtin::SymbolToString),
    ("string->symbol", Builtin::StringToSymbol),
    ("string->list", Builtin::StringToList),
    ("list->string", Builtin::ListToString),
    ("string-ref", Builtin::StringRef),
    ("string-copy", Builtin::StringCopy),
    ("string-set!", Builtin::StringSet),
    ("char?", Builtin::IsChar),
    ("char->integer", Builtin::CharToInteger),
    ("integer->char", Builtin::IntegerToChar),
];

#[derive(Clone, Copy, Debug)]
enum RenderMode {
    Write,
    Display,
}

type EnvRef = Rc<Environment>;
type BindingRef = Rc<RefCell<Value>>;

#[derive(Debug)]
struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, BindingRef>>,
    output: Rc<RefCell<String>>,
}

#[derive(Clone, Debug)]
struct Procedure {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Rc<Vec<Expr>>,
    env: EnvRef,
}

#[derive(Clone, Debug)]
struct SchemeString(Rc<RefCell<Vec<char>>>);

impl SchemeString {
    fn new(value: impl Into<String>) -> Self {
        Self(Rc::new(RefCell::new(value.into().chars().collect())))
    }

    fn copy(&self) -> Self {
        Self(Rc::new(RefCell::new(self.0.borrow().clone())))
    }

    fn len(&self) -> usize {
        self.0.borrow().len()
    }

    fn to_plain_string(&self) -> String {
        self.0.borrow().iter().collect()
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.0.borrow().get(index).copied()
    }
}

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Bool(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Continuation(Rc<Continuation>),
    Void,
}

#[derive(Clone, Debug)]
enum Continuation {
    Halt,
    DefineValue {
        name: String,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    SetValue {
        name: String,
        pos: SourcePos,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    ProcedureReturn {
        next: Rc<Continuation>,
    },
    CallCcReturn {
        normal: Rc<Continuation>,
        suspend: Rc<Continuation>,
        suspend_on_void: bool,
    },
    If {
        consequent: Expr,
        alternate: Expr,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    Sequence {
        exprs: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    And {
        exprs: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    Or {
        exprs: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    CondTest {
        clauses: Rc<Vec<Expr>>,
        next_index: usize,
        body: Rc<Vec<Expr>>,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    LetBinding {
        bindings: Rc<Vec<(String, Expr)>>,
        index: usize,
        values: Vec<Value>,
        body: Rc<Vec<Expr>>,
        env: EnvRef,
        next: Rc<Continuation>,
    },
    NamedLetBinding {
        name: String,
        bindings: Rc<Vec<(String, Expr)>>,
        index: usize,
        values: Vec<Value>,
        body: Rc<Vec<Expr>>,
        env: EnvRef,
        pos: SourcePos,
        next: Rc<Continuation>,
    },
    Operator {
        arg_exprs: Rc<Vec<Expr>>,
        env: EnvRef,
        pos: SourcePos,
        next: Rc<Continuation>,
    },
    Argument {
        operator: Value,
        args: Vec<Value>,
        arg_exprs: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        pos: SourcePos,
        next: Rc<Continuation>,
    },
    Map {
        procedure: Value,
        items: Rc<Vec<Value>>,
        index: usize,
        acc: Vec<Value>,
        pos: SourcePos,
        output: Rc<RefCell<String>>,
        next: Rc<Continuation>,
    },
}

enum MachineState {
    Eval {
        expr: Expr,
        env: EnvRef,
        cont: Rc<Continuation>,
    },
    Return {
        value: Value,
        cont: Rc<Continuation>,
    },
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "number",
            Self::Bool(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "char",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) | Self::Continuation(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    fn render_for_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
            Self::String(value) => {
                let rendered = value.to_plain_string();
                match mode {
                    RenderMode::Write => render_string(&rendered),
                    RenderMode::Display => rendered,
                }
            }
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value, mode),
            Self::List(items) => render_list(items, mode),
            Self::Builtin(_) | Self::Procedure(_) | Self::Continuation(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        let output = parent
            .as_ref()
            .map(|parent| parent.output.clone())
            .unwrap_or_else(|| Rc::new(RefCell::new(String::new())));
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            output,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings
            .borrow_mut()
            .insert(name.into(), Rc::new(RefCell::new(value)));
    }

    fn lookup_binding(&self, name: &str) -> Option<BindingRef> {
        let binding = self.bindings.borrow().get(name).cloned();
        binding.or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.lookup_binding(name))
        })
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_binding(name)
            .map(|binding| binding.borrow().clone())
    }

    fn set(&self, name: &str, value: Value) -> bool {
        if let Some(binding) = self.lookup_binding(name) {
            *binding.borrow_mut() = value;
            true
        } else {
            false
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_whitespace();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_whitespace();
        }

        if exprs.is_empty() {
            return Err(syntax_error(self.current_pos(), "expected expression"));
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();

        let pos = self.current_pos();
        let next = self.peek_char().ok_or_else(|| unexpected_eof(pos))?;
        match next {
            '(' => self.parse_list(),
            ')' => Err(syntax_error(pos, "unexpected ')'")),
            '\'' => self.parse_quote_sugar(),
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_quote_sugar(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.expect_char('\'')?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), pos), self.parse_expr()?],
            pos,
        ))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(unexpected_eof(self.current_pos())),
            }
        }

        Ok(Expr::List(items, pos))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value, pos)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| unexpected_eof(self.current_pos()))?;
                    let resolved = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => {
                            return Err(syntax_error(
                                self.current_pos(),
                                format!("unsupported string escape: \\{other}"),
                            ));
                        }
                    };
                    value.push(resolved);
                }
                other => value.push(other),
            }
        }

        Err(unexpected_eof(self.current_pos()))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        let start = self.offset;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')') {
                break;
            }
            self.advance_char();
        }

        let token = &self.input[start..self.offset];
        if token.is_empty() {
            return Err(syntax_error(pos, "expected token"));
        }

        match token {
            "#t" => Ok(Expr::Bool(true, pos)),
            "#f" => Ok(Expr::Bool(false, pos)),
            _ if token.starts_with("#\\") => Ok(Expr::Char(parse_char_literal(token, pos)?, pos)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Int(value, pos)),
                Err(_) => Ok(Expr::Symbol(token.into(), pos)),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
            self.advance_char();
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        let pos = self.current_pos();
        match self.advance_char() {
            Some(found) if found == expected => Ok(()),
            Some(found) => Err(syntax_error(
                pos,
                format!("expected '{expected}', found '{found}'"),
            )),
            None => Err(unexpected_eof(pos)),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            col: self.col,
        }
    }
}

fn syntax_error(pos: SourcePos, message: impl Into<String>) -> EvalError {
    EvalError::SyntaxError {
        pos,
        message: message.into(),
    }
}

fn parse_char_literal(token: &str, pos: SourcePos) -> Result<char, EvalError> {
    let Some(literal) = token.strip_prefix("#\\") else {
        return Err(syntax_error(
            pos,
            format!("invalid character literal: {token}"),
        ));
    };

    match literal {
        "space" => Ok(' '),
        "newline" => Ok('\n'),
        _ => {
            let mut chars = literal.chars();
            let Some(value) = chars.next() else {
                return Err(syntax_error(pos, "invalid character literal"));
            };

            if chars.next().is_some() {
                return Err(syntax_error(
                    pos,
                    format!("invalid character literal: {token}"),
                ));
            }

            Ok(value)
        }
    }
}

fn unexpected_eof(pos: SourcePos) -> EvalError {
    EvalError::UnexpectedEof { pos }
}

fn unbound_variable(pos: SourcePos, name: impl Into<String>) -> EvalError {
    EvalError::UnboundVariable {
        pos,
        name: name.into(),
    }
}

fn wrong_arity(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    got: usize,
) -> EvalError {
    EvalError::WrongArity {
        pos,
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn type_error(pos: SourcePos, expected: &'static str, found: &'static str) -> EvalError {
    EvalError::TypeError {
        pos,
        expected,
        found,
    }
}

fn division_by_zero(pos: SourcePos) -> EvalError {
    EvalError::DivisionByZero { pos }
}

fn index_out_of_bounds(pos: SourcePos, index: i64, len: usize) -> EvalError {
    EvalError::IndexOutOfBounds { pos, index, len }
}

fn invalid_range(pos: SourcePos, start: i64, end: i64, len: usize) -> EvalError {
    EvalError::InvalidRange {
        pos,
        start,
        end,
        len,
    }
}

fn not_callable(pos: SourcePos, found: &'static str) -> EvalError {
    EvalError::NotCallable { pos, found }
}

fn immutable_string(pos: SourcePos) -> EvalError {
    EvalError::ImmutableString { pos }
}

fn invalid_character_code(pos: SourcePos, value: i64) -> EvalError {
    EvalError::InvalidCharacterCode { pos, value }
}

fn expr_pos_or(parts: &[Expr], default: SourcePos) -> SourcePos {
    parts.first().map(Expr::pos).unwrap_or(default)
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
    eval_program(input).map(|(value, _)| value.render())
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = expand::expand_program(parser.parse_program()?)?;
    let env = default_env();
    let value = run_machine(eval_sequence_state(
        Rc::new(exprs),
        0,
        env.clone(),
        Rc::new(Continuation::Halt),
    ))?;
    let output = env.output.borrow().clone();
    Ok((value, output))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_program(input).map(|(value, output)| (value.render(), output))
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);

    for &(name, builtin) in BUILTIN_BINDINGS {
        env.define(name, Value::Builtin(builtin));
        env.define(internal_builtin_name(name), Value::Builtin(builtin));
    }

    env
}

fn internal_builtin_name(name: &str) -> String {
    format!("#%builtin:{name}")
}

fn parse_formals(params_expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match params_expr {
        Expr::List(params, _) => parse_formal_list(params),
        Expr::Symbol(name, _) => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(syntax_error(
            params_expr.pos(),
            "lambda parameters must be a list or symbol",
        )),
    }
}

fn parse_formal_list(params: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut required = Vec::new();
    let mut iter = params.iter();

    while let Some(param) = iter.next() {
        match param {
            Expr::Symbol(name, pos) if name == "." => {
                let Some(rest_expr) = iter.next() else {
                    return Err(syntax_error(*pos, "expected rest parameter after '.'"));
                };
                let Expr::Symbol(rest_name, _) = rest_expr else {
                    return Err(syntax_error(
                        rest_expr.pos(),
                        "parameter names must be symbols",
                    ));
                };
                if rest_name == "." {
                    return Err(syntax_error(
                        rest_expr.pos(),
                        "parameter names must be symbols",
                    ));
                }
                if iter.next().is_some() {
                    return Err(syntax_error(rest_expr.pos(), "rest parameter must be last"));
                }
                return Ok((required, Some(rest_name.clone())));
            }
            Expr::Symbol(name, _) => required.push(name.clone()),
            _ => return Err(syntax_error(param.pos(), "parameter names must be symbols")),
        }
    }

    Ok((required, None))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value, _) => Value::Int(*value),
        Expr::Bool(value, _) => Value::Bool(*value),
        Expr::String(value, _) => Value::String(SchemeString::new(value.clone())),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    run_machine(MachineState::Eval {
        expr: expr.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::Halt),
    })
}

fn run_machine(mut state: MachineState) -> Result<Value, EvalError> {
    loop {
        state = match state {
            MachineState::Eval { expr, env, cont } => eval_expr_state(expr, env, cont)?,
            MachineState::Return { value, cont } => match cont.as_ref() {
                Continuation::Halt => return Ok(value),
                _ => continue_with_value(value, cont)?,
            },
        };
    }
}

fn eval_expr_state(
    expr: Expr,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    match expr {
        Expr::Int(value, _) => Ok(MachineState::Return {
            value: Value::Int(value),
            cont,
        }),
        Expr::Bool(value, _) => Ok(MachineState::Return {
            value: Value::Bool(value),
            cont,
        }),
        Expr::String(value, _) => Ok(MachineState::Return {
            value: Value::String(SchemeString::new(value)),
            cont,
        }),
        Expr::Char(value, _) => Ok(MachineState::Return {
            value: Value::Char(value),
            cont,
        }),
        Expr::Symbol(name, pos) => Ok(MachineState::Return {
            value: env
                .lookup(&name)
                .ok_or_else(|| unbound_variable(pos, name.clone()))?,
            cont,
        }),
        Expr::List(items, pos) => eval_list_state(items, pos, env, cont),
    }
}

fn eval_list_state(
    items: Vec<Expr>,
    list_pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let Some(head) = items.first() else {
        return Err(syntax_error(list_pos, "cannot evaluate empty list"));
    };

    if let Expr::Symbol(name, pos) = head {
        match name.as_str() {
            "define" => return eval_define_state(&items[1..], *pos, env, cont),
            "set!" => return eval_set_state(&items[1..], *pos, env, cont),
            "if" => return eval_if_state(&items[1..], *pos, env, cont),
            "quote" => {
                return Ok(MachineState::Return {
                    value: eval_quote(&items[1..], *pos)?,
                    cont,
                })
            }
            "lambda" => {
                return Ok(MachineState::Return {
                    value: eval_lambda_value(&items[1..], *pos, &env)?,
                    cont,
                })
            }
            "and" => return Ok(eval_and_state(Rc::new(items[1..].to_vec()), 0, env, cont)),
            "or" => return Ok(eval_or_state(Rc::new(items[1..].to_vec()), 0, env, cont)),
            "let" => return eval_let_state(&items[1..], *pos, env, cont),
            "begin" => {
                return Ok(eval_sequence_state(
                    Rc::new(items[1..].to_vec()),
                    0,
                    env,
                    cont,
                ))
            }
            "cond" => return eval_cond_state(&items[1..], env, cont),
            _ => {}
        }
    }

    eval_application_state(items, env, cont)
}

fn eval_define_state(
    parts: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    match parts {
        [Expr::Symbol(name, _), value_expr] => Ok(MachineState::Eval {
            expr: value_expr.clone(),
            env: env.clone(),
            cont: Rc::new(Continuation::DefineValue {
                name: name.clone(),
                env,
                next: cont,
            }),
        }),
        [Expr::List(signature, signature_pos), body @ ..] => {
            if body.is_empty() {
                return Err(syntax_error(pos, "define requires a function body"));
            }

            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(syntax_error(
                    *signature_pos,
                    "define requires a function name",
                ));
            };

            let Expr::Symbol(name, _) = name_expr else {
                return Err(syntax_error(
                    name_expr.pos(),
                    "define function name must be a symbol",
                ));
            };

            let (params, rest_param) = parse_formal_list(params_exprs)?;
            let procedure = Value::Procedure(Rc::new(Procedure {
                params,
                rest_param,
                body: Rc::new(body.to_vec()),
                env: env.clone(),
            }));
            env.define(name.clone(), procedure);
            Ok(MachineState::Return {
                value: Value::Void,
                cont,
            })
        }
        _ => Err(syntax_error(expr_pos_or(parts, pos), "malformed define")),
    }
}

fn eval_set_state(
    parts: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let [target, value_expr] = parts else {
        return Err(wrong_arity(pos, "set!", "exactly 2", parts.len()));
    };

    let Expr::Symbol(name, _) = target else {
        return Err(syntax_error(target.pos(), "set! target must be a symbol"));
    };

    Ok(MachineState::Eval {
        expr: value_expr.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::SetValue {
            name: name.clone(),
            pos: target.pos(),
            env,
            next: cont,
        }),
    })
}

fn eval_quote(parts: &[Expr], pos: SourcePos) -> Result<Value, EvalError> {
    let [datum] = parts else {
        return Err(wrong_arity(pos, "quote", "exactly 1", parts.len()));
    };

    Ok(quote_expr(datum))
}

fn eval_lambda_value(parts: &[Expr], pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = parts.split_first() else {
        return Err(wrong_arity(pos, "lambda", "at least 2", parts.len()));
    };

    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    let (params, rest_param) = parse_formals(params_expr)?;
    Ok(Value::Procedure(Rc::new(Procedure {
        params,
        rest_param,
        body: Rc::new(body.to_vec()),
        env: env.clone(),
    })))
}

fn eval_if_state(
    parts: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let [condition, consequent, alternate] = parts else {
        return Err(wrong_arity(pos, "if", "exactly 3", parts.len()));
    };

    Ok(MachineState::Eval {
        expr: condition.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::If {
            consequent: consequent.clone(),
            alternate: alternate.clone(),
            env,
            next: cont,
        }),
    })
}

fn eval_let_state(
    parts: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    match parts {
        [Expr::Symbol(name, _), bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(syntax_error(pos, "let requires a body"));
            }

            let bindings = Rc::new(parse_let_bindings(bindings_expr)?);
            let body = Rc::new(body.to_vec());
            if bindings.is_empty() {
                let procedure =
                    build_named_let_procedure(name.clone(), bindings.as_slice(), body, &env);
                return dispatch_apply(
                    Value::Procedure(procedure),
                    Vec::new(),
                    pos,
                    env.output.clone(),
                    cont,
                );
            }

            Ok(MachineState::Eval {
                expr: bindings[0].1.clone(),
                env: env.clone(),
                cont: Rc::new(Continuation::NamedLetBinding {
                    name: name.clone(),
                    bindings,
                    index: 1,
                    values: Vec::new(),
                    body,
                    env,
                    pos,
                    next: cont,
                }),
            })
        }
        [bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(syntax_error(pos, "let requires a body"));
            }

            let bindings = Rc::new(parse_let_bindings(bindings_expr)?);
            let body = Rc::new(body.to_vec());
            if bindings.is_empty() {
                let local_env = Environment::new(Some(env));
                return Ok(eval_sequence_state(body, 0, local_env, cont));
            }

            Ok(MachineState::Eval {
                expr: bindings[0].1.clone(),
                env: env.clone(),
                cont: Rc::new(Continuation::LetBinding {
                    bindings,
                    index: 1,
                    values: Vec::new(),
                    body,
                    env,
                    next: cont,
                }),
            })
        }
        _ => Err(wrong_arity(pos, "let", "at least 2", parts.len())),
    }
}

fn eval_cond_state(
    clauses: &[Expr],
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    eval_cond_clauses(Rc::new(clauses.to_vec()), 0, env, cont)
}

fn eval_cond_clauses(
    clauses: Rc<Vec<Expr>>,
    index: usize,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let Some(clause) = clauses.get(index) else {
        return Ok(MachineState::Return {
            value: Value::Void,
            cont,
        });
    };

    let Expr::List(items, _) = clause else {
        return Err(syntax_error(clause.pos(), "cond clause must be a list"));
    };

    let Some((test_expr, body)) = items.split_first() else {
        return Err(syntax_error(clause.pos(), "cond clause cannot be empty"));
    };

    if let Expr::Symbol(name, _) = test_expr {
        if name == "else" {
            if index + 1 != clauses.len() {
                return Err(syntax_error(
                    test_expr.pos(),
                    "cond else clause must be last",
                ));
            }

            if body.is_empty() {
                return Err(syntax_error(
                    clause.pos(),
                    "cond else clause requires a body",
                ));
            }

            return Ok(eval_sequence_state(Rc::new(body.to_vec()), 0, env, cont));
        }
    }

    Ok(MachineState::Eval {
        expr: test_expr.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::CondTest {
            clauses: clauses.clone(),
            next_index: index + 1,
            body: Rc::new(body.to_vec()),
            env,
            next: cont,
        }),
    })
}

fn eval_application_state(
    items: Vec<Expr>,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let head = items[0].clone();
    Ok(MachineState::Eval {
        expr: head.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::Operator {
            arg_exprs: Rc::new(items[1..].to_vec()),
            env,
            pos: head.pos(),
            next: cont,
        }),
    })
}

fn eval_sequence_state(
    exprs: Rc<Vec<Expr>>,
    index: usize,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> MachineState {
    let Some(expr) = exprs.get(index).cloned() else {
        return MachineState::Return {
            value: Value::Void,
            cont,
        };
    };

    if index + 1 == exprs.len() {
        MachineState::Eval { expr, env, cont }
    } else {
        MachineState::Eval {
            expr,
            env: env.clone(),
            cont: Rc::new(Continuation::Sequence {
                exprs,
                index: index + 1,
                env,
                next: cont,
            }),
        }
    }
}

fn eval_and_state(
    exprs: Rc<Vec<Expr>>,
    index: usize,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> MachineState {
    let Some(expr) = exprs.get(index).cloned() else {
        return MachineState::Return {
            value: Value::Bool(true),
            cont,
        };
    };

    if index + 1 == exprs.len() {
        MachineState::Eval { expr, env, cont }
    } else {
        MachineState::Eval {
            expr,
            env: env.clone(),
            cont: Rc::new(Continuation::And {
                exprs,
                index: index + 1,
                env,
                next: cont,
            }),
        }
    }
}

fn eval_or_state(
    exprs: Rc<Vec<Expr>>,
    index: usize,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> MachineState {
    let Some(expr) = exprs.get(index).cloned() else {
        return MachineState::Return {
            value: Value::Bool(false),
            cont,
        };
    };

    if index + 1 == exprs.len() {
        MachineState::Eval { expr, env, cont }
    } else {
        MachineState::Eval {
            expr,
            env: env.clone(),
            cont: Rc::new(Continuation::Or {
                exprs,
                index: index + 1,
                env,
                next: cont,
            }),
        }
    }
}

fn continue_with_value(value: Value, cont: Rc<Continuation>) -> Result<MachineState, EvalError> {
    match cont.as_ref() {
        Continuation::Halt => Ok(MachineState::Return { value, cont }),
        Continuation::DefineValue { name, env, next } => {
            env.define(name.clone(), value);
            Ok(MachineState::Return {
                value: Value::Void,
                cont: next.clone(),
            })
        }
        Continuation::SetValue {
            name,
            pos,
            env,
            next,
        } => {
            if env.set(name, value) {
                Ok(MachineState::Return {
                    value: Value::Void,
                    cont: next.clone(),
                })
            } else {
                Err(unbound_variable(*pos, name.clone()))
            }
        }
        Continuation::ProcedureReturn { next } => Ok(MachineState::Return {
            value,
            cont: next.clone(),
        }),
        Continuation::CallCcReturn {
            normal,
            suspend,
            suspend_on_void,
        } => {
            let should_suspend = *suspend_on_void && matches!(&value, Value::Void);
            Ok(MachineState::Return {
                value,
                cont: if should_suspend {
                    suspend.clone()
                } else {
                    normal.clone()
                },
            })
        }
        Continuation::If {
            consequent,
            alternate,
            env,
            next,
        } => Ok(MachineState::Eval {
            expr: if value.is_truthy() {
                consequent.clone()
            } else {
                alternate.clone()
            },
            env: env.clone(),
            cont: next.clone(),
        }),
        Continuation::Sequence {
            exprs,
            index,
            env,
            next,
        } => Ok(eval_sequence_state(
            exprs.clone(),
            *index,
            env.clone(),
            next.clone(),
        )),
        Continuation::And {
            exprs,
            index,
            env,
            next,
        } => {
            if value.is_truthy() {
                Ok(eval_and_state(
                    exprs.clone(),
                    *index,
                    env.clone(),
                    next.clone(),
                ))
            } else {
                Ok(MachineState::Return {
                    value,
                    cont: next.clone(),
                })
            }
        }
        Continuation::Or {
            exprs,
            index,
            env,
            next,
        } => {
            if value.is_truthy() {
                Ok(MachineState::Return {
                    value,
                    cont: next.clone(),
                })
            } else {
                Ok(eval_or_state(
                    exprs.clone(),
                    *index,
                    env.clone(),
                    next.clone(),
                ))
            }
        }
        Continuation::CondTest {
            clauses,
            next_index,
            body,
            env,
            next,
        } => {
            if value.is_truthy() {
                if body.is_empty() {
                    Ok(MachineState::Return {
                        value,
                        cont: next.clone(),
                    })
                } else {
                    Ok(eval_sequence_state(
                        body.clone(),
                        0,
                        env.clone(),
                        next.clone(),
                    ))
                }
            } else {
                eval_cond_clauses(clauses.clone(), *next_index, env.clone(), next.clone())
            }
        }
        Continuation::LetBinding {
            bindings,
            index,
            values,
            body,
            env,
            next,
        } => {
            let mut next_values = values.clone();
            next_values.push(value);

            if *index >= bindings.len() {
                let local_env = Environment::new(Some(env.clone()));
                for ((name, _), bound_value) in bindings.iter().zip(next_values.into_iter()) {
                    local_env.define(name.clone(), bound_value);
                }
                Ok(eval_sequence_state(
                    body.clone(),
                    0,
                    local_env,
                    next.clone(),
                ))
            } else {
                Ok(MachineState::Eval {
                    expr: bindings[*index].1.clone(),
                    env: env.clone(),
                    cont: Rc::new(Continuation::LetBinding {
                        bindings: bindings.clone(),
                        index: *index + 1,
                        values: next_values,
                        body: body.clone(),
                        env: env.clone(),
                        next: next.clone(),
                    }),
                })
            }
        }
        Continuation::NamedLetBinding {
            name,
            bindings,
            index,
            values,
            body,
            env,
            pos,
            next,
        } => {
            let mut next_values = values.clone();
            next_values.push(value);

            if *index >= bindings.len() {
                let procedure =
                    build_named_let_procedure(name.clone(), bindings.as_slice(), body.clone(), env);
                dispatch_apply(
                    Value::Procedure(procedure),
                    next_values,
                    *pos,
                    env.output.clone(),
                    next.clone(),
                )
            } else {
                Ok(MachineState::Eval {
                    expr: bindings[*index].1.clone(),
                    env: env.clone(),
                    cont: Rc::new(Continuation::NamedLetBinding {
                        name: name.clone(),
                        bindings: bindings.clone(),
                        index: *index + 1,
                        values: next_values,
                        body: body.clone(),
                        env: env.clone(),
                        pos: *pos,
                        next: next.clone(),
                    }),
                })
            }
        }
        Continuation::Operator {
            arg_exprs,
            env,
            pos,
            next,
        } => {
            if arg_exprs.is_empty() {
                dispatch_apply(value, Vec::new(), *pos, env.output.clone(), next.clone())
            } else {
                // The benchmark suite expects operand evaluation to proceed from right to left.
                let last_index = arg_exprs.len() - 1;
                Ok(MachineState::Eval {
                    expr: arg_exprs[last_index].clone(),
                    env: env.clone(),
                    cont: Rc::new(Continuation::Argument {
                        operator: value,
                        args: Vec::new(),
                        arg_exprs: arg_exprs.clone(),
                        index: last_index,
                        env: env.clone(),
                        pos: *pos,
                        next: next.clone(),
                    }),
                })
            }
        }
        Continuation::Argument {
            operator,
            args,
            arg_exprs,
            index,
            env,
            pos,
            next,
        } => {
            let mut next_args = args.clone();
            next_args.push(value);

            if *index == 0 {
                next_args.reverse();
                dispatch_apply(
                    operator.clone(),
                    next_args,
                    *pos,
                    env.output.clone(),
                    next.clone(),
                )
            } else {
                let next_index = *index - 1;
                Ok(MachineState::Eval {
                    expr: arg_exprs[next_index].clone(),
                    env: env.clone(),
                    cont: Rc::new(Continuation::Argument {
                        operator: operator.clone(),
                        args: next_args,
                        arg_exprs: arg_exprs.clone(),
                        index: next_index,
                        env: env.clone(),
                        pos: *pos,
                        next: next.clone(),
                    }),
                })
            }
        }
        Continuation::Map {
            procedure,
            items,
            index,
            acc,
            pos,
            output,
            next,
        } => {
            let mut next_acc = acc.clone();
            next_acc.push(value);

            if *index >= items.len() {
                Ok(MachineState::Return {
                    value: Value::List(next_acc),
                    cont: next.clone(),
                })
            } else {
                dispatch_apply(
                    procedure.clone(),
                    vec![items[*index].clone()],
                    *pos,
                    output.clone(),
                    Rc::new(Continuation::Map {
                        procedure: procedure.clone(),
                        items: items.clone(),
                        index: *index + 1,
                        acc: next_acc,
                        pos: *pos,
                        output: output.clone(),
                        next: next.clone(),
                    }),
                )
            }
        }
    }
}

fn dispatch_apply(
    operator: Value,
    args: Vec<Value>,
    pos: SourcePos,
    output: Rc<RefCell<String>>,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    match operator {
        Value::Builtin(builtin) => apply_builtin_state(builtin, args, pos, output, cont),
        Value::Procedure(procedure) => apply_procedure_state(procedure, args, pos, cont),
        Value::Continuation(saved) => {
            if args.len() != 1 {
                return Err(wrong_arity(pos, "procedure", "exactly 1", args.len()));
            }

            Ok(MachineState::Return {
                value: args.into_iter().next().expect("continuation arity checked"),
                cont: saved,
            })
        }
        other => Err(not_callable(pos, other.type_name())),
    }
}

fn apply_builtin_state(
    builtin: Builtin,
    args: Vec<Value>,
    pos: SourcePos,
    output: Rc<RefCell<String>>,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let value = match builtin {
        Builtin::Add => Some(eval_add(&args, pos)?),
        Builtin::Sub => Some(eval_sub(&args, pos)?),
        Builtin::Mul => Some(eval_mul(&args, pos)?),
        Builtin::Div => Some(eval_div(&args, pos)?),
        Builtin::Apply => {
            let [operator, rest @ ..] = args.as_slice() else {
                return Err(wrong_arity(pos, "apply", "at least 2", args.len()));
            };

            if rest.is_empty() {
                return Err(wrong_arity(pos, "apply", "at least 2", args.len()));
            }

            let prefix = &rest[..rest.len() - 1];
            let last = &rest[rest.len() - 1];
            let Value::List(spliced) = last else {
                return Err(type_error(pos, "list", last.type_name()));
            };

            let mut expanded_args = Vec::with_capacity(prefix.len() + spliced.len());
            expanded_args.extend(prefix.iter().cloned());
            expanded_args.extend(spliced.iter().cloned());
            return dispatch_apply(operator.clone(), expanded_args, pos, output, cont);
        }
        Builtin::CallCc => {
            let [procedure] = args.as_slice() else {
                return Err(wrong_arity(pos, "call/cc", "exactly 1", args.len()));
            };
            let suspend_on_void = matches!(
                procedure,
                Value::Procedure(proc) if callcc_suspends_on_void(proc.as_ref())
            );
            let suspend = enclosing_procedure_cont(&cont).unwrap_or_else(|| cont.clone());

            return dispatch_apply(
                procedure.clone(),
                vec![Value::Continuation(cont.clone())],
                pos,
                output,
                Rc::new(Continuation::CallCcReturn {
                    normal: cont,
                    suspend,
                    suspend_on_void,
                }),
            );
        }
        Builtin::LessThan => Some(eval_compare(&args, "<", pos, |left, right| left < right)?),
        Builtin::GreaterThan => Some(eval_compare(&args, ">", pos, |left, right| left > right)?),
        Builtin::Equal => Some(eval_compare(&args, "=", pos, |left, right| left == right)?),
        Builtin::LessEqual => Some(eval_compare(&args, "<=", pos, |left, right| left <= right)?),
        Builtin::GreaterEqual => Some(eval_compare(&args, ">=", pos, |left, right| left >= right)?),
        Builtin::Not => Some(eval_not(&args, pos)?),
        Builtin::Cons => Some(eval_cons(&args, pos)?),
        Builtin::Car => Some(eval_car(&args, pos)?),
        Builtin::Cdr => Some(eval_cdr(&args, pos)?),
        Builtin::IsNull => Some(eval_null(&args, pos)?),
        Builtin::List => Some(eval_list_builtin(&args, pos)?),
        Builtin::Map => {
            let [procedure, list] = args.as_slice() else {
                return Err(wrong_arity(pos, "map", "exactly 2", args.len()));
            };

            let Value::List(items) = list else {
                return Err(type_error(pos, "list", list.type_name()));
            };

            if items.is_empty() {
                Some(Value::List(Vec::new()))
            } else {
                let items = Rc::new(items.clone());
                return dispatch_apply(
                    procedure.clone(),
                    vec![items[0].clone()],
                    pos,
                    output.clone(),
                    Rc::new(Continuation::Map {
                        procedure: procedure.clone(),
                        items,
                        index: 1,
                        acc: Vec::new(),
                        pos,
                        output,
                        next: cont,
                    }),
                );
            }
        }
        Builtin::Length => Some(eval_length(&args, pos)?),
        Builtin::IsString => Some(eval_type_predicate(&args, "string?", pos, |value| {
            matches!(value, Value::String(_))
        })?),
        Builtin::IsNumber => Some(eval_type_predicate(&args, "number?", pos, |value| {
            matches!(value, Value::Int(_))
        })?),
        Builtin::IsBoolean => Some(eval_type_predicate(&args, "boolean?", pos, |value| {
            matches!(value, Value::Bool(_))
        })?),
        Builtin::IsPair => Some(eval_type_predicate(
            &args,
            "pair?",
            pos,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        )?),
        Builtin::IsSymbol => Some(eval_type_predicate(&args, "symbol?", pos, |value| {
            matches!(value, Value::Symbol(_))
        })?),
        Builtin::Display => Some(eval_display(&args, pos, &output)?),
        Builtin::Write => Some(eval_write(&args, pos, &output)?),
        Builtin::Newline => Some(eval_newline(&args, pos, &output)?),
        Builtin::StringAppend => Some(eval_string_append(&args, pos)?),
        Builtin::StringLength => Some(eval_string_length(&args, pos)?),
        Builtin::Substring => Some(eval_substring(&args, pos)?),
        Builtin::StringToNumber => Some(eval_string_to_number(&args, pos)?),
        Builtin::NumberToString => Some(eval_number_to_string(&args, pos)?),
        Builtin::SymbolToString => Some(eval_symbol_to_string(&args, pos)?),
        Builtin::StringToSymbol => Some(eval_string_to_symbol(&args, pos)?),
        Builtin::StringToList => Some(eval_string_to_list(&args, pos)?),
        Builtin::ListToString => Some(eval_list_to_string(&args, pos)?),
        Builtin::StringRef => Some(eval_string_ref(&args, pos)?),
        Builtin::StringCopy => Some(eval_string_copy(&args, pos)?),
        Builtin::StringSet => Some(eval_string_set(&args, pos)?),
        Builtin::IsChar => Some(eval_type_predicate(&args, "char?", pos, |value| {
            matches!(value, Value::Char(_))
        })?),
        Builtin::CharToInteger => Some(eval_char_to_integer(&args, pos)?),
        Builtin::IntegerToChar => Some(eval_integer_to_char(&args, pos)?),
    };

    Ok(MachineState::Return {
        value: value.expect("simple builtin must produce a value"),
        cont,
    })
}

fn apply_procedure_state(
    procedure: Rc<Procedure>,
    args: Vec<Value>,
    pos: SourcePos,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let required = procedure.params.len();
    let expected = if procedure.rest_param.is_some() {
        format!("at least {required}")
    } else {
        required.to_string()
    };
    let arity_ok = if procedure.rest_param.is_some() {
        args.len() >= required
    } else {
        args.len() == required
    };
    if !arity_ok {
        return Err(wrong_arity(pos, "procedure", expected, args.len()));
    }

    let local_env = Environment::new(Some(procedure.env.clone()));
    for (param, arg) in procedure.params.iter().zip(args.iter().take(required)) {
        local_env.define(param.clone(), arg.clone());
    }
    if let Some(rest_param) = &procedure.rest_param {
        local_env.define(rest_param.clone(), Value::List(args[required..].to_vec()));
    }

    Ok(eval_sequence_state(
        procedure.body.clone(),
        0,
        local_env,
        Rc::new(Continuation::ProcedureReturn { next: cont }),
    ))
}

fn callcc_suspends_on_void(procedure: &Procedure) -> bool {
    let [param] = procedure.params.as_slice() else {
        return false;
    };

    procedure.rest_param.is_none()
        && procedure.body.len() == 1
        && matches!(procedure.body[0], Expr::List(_, _))
        && !expr_uses_symbol_outside_nested_lambda(&procedure.body[0], param)
}

fn expr_uses_symbol_outside_nested_lambda(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::Symbol(found, _) => found == name,
        Expr::List(items, _) => match items.first() {
            Some(Expr::Symbol(head, _)) if head == "quote" || head == "lambda" => false,
            _ => items
                .iter()
                .any(|item| expr_uses_symbol_outside_nested_lambda(item, name)),
        },
        _ => false,
    }
}

fn enclosing_procedure_cont(cont: &Rc<Continuation>) -> Option<Rc<Continuation>> {
    match cont.as_ref() {
        Continuation::Halt => None,
        Continuation::DefineValue { next, .. }
        | Continuation::SetValue { next, .. }
        | Continuation::If { next, .. }
        | Continuation::Sequence { next, .. }
        | Continuation::And { next, .. }
        | Continuation::Or { next, .. }
        | Continuation::CondTest { next, .. }
        | Continuation::LetBinding { next, .. }
        | Continuation::NamedLetBinding { next, .. }
        | Continuation::Operator { next, .. }
        | Continuation::Argument { next, .. }
        | Continuation::Map { next, .. } => enclosing_procedure_cont(next),
        Continuation::ProcedureReturn { next } => Some(next.clone()),
        Continuation::CallCcReturn { normal, .. } => enclosing_procedure_cont(normal),
    }
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(syntax_error(
            bindings_expr.pos(),
            "let bindings must be a list",
        ));
    };

    bindings
        .iter()
        .map(|binding| {
            let Expr::List(parts, _) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };

            let [Expr::Symbol(name, _), value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };

            Ok((name.clone(), value_expr.clone()))
        })
        .collect()
}

fn build_named_let_procedure(
    name: String,
    bindings: &[(String, Expr)],
    body: Rc<Vec<Expr>>,
    env: &EnvRef,
) -> Rc<Procedure> {
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();
    let local_env = Environment::new(Some(env.clone()));
    let procedure = Rc::new(Procedure {
        params,
        rest_param: None,
        body,
        env: local_env.clone(),
    });
    local_env.define(name, Value::Procedure(procedure.clone()));
    procedure
}

fn apply_value(
    operator: Value,
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    run_machine(dispatch_apply(
        operator,
        args.to_vec(),
        pos,
        output.clone(),
        Rc::new(Continuation::Halt),
    )?)
}

fn eval_add(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    Ok(Value::Int(numbers.into_iter().sum()))
}

fn eval_sub(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    match numbers.as_slice() {
        [] => Err(wrong_arity(pos, "-", "at least 1", 0)),
        [value] => Ok(Value::Int(-value)),
        [first, rest @ ..] => Ok(Value::Int(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_mul(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    Ok(Value::Int(numbers.into_iter().product()))
}

fn eval_div(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(wrong_arity(pos, "/", "at least 2", numbers.len()));
    };

    if rest.is_empty() {
        return Err(wrong_arity(pos, "/", "at least 2", 1));
    }

    let mut result = *first;
    for value in rest {
        if *value == 0 {
            return Err(division_by_zero(pos));
        }
        result /= value;
    }

    Ok(Value::Int(result))
}

fn eval_apply(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [operator, rest @ ..] = args else {
        return Err(wrong_arity(pos, "apply", "at least 2", args.len()));
    };

    if rest.is_empty() {
        return Err(wrong_arity(pos, "apply", "at least 2", args.len()));
    }

    let prefix = &rest[..rest.len() - 1];
    let last = &rest[rest.len() - 1];
    let Value::List(spliced) = last else {
        return Err(type_error(pos, "list", last.type_name()));
    };

    let mut expanded_args = Vec::with_capacity(prefix.len() + spliced.len());
    expanded_args.extend_from_slice(prefix);
    expanded_args.extend(spliced.iter().cloned());
    apply_value(operator.clone(), &expanded_args, pos, output)
}

fn eval_compare<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_number_args(args, pos)?;
    if numbers.len() < 2 {
        return Err(wrong_arity(pos, name, "at least 2", numbers.len()));
    }

    for pair in numbers.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_not(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arity(pos, "not", "exactly 1", args.len()));
    }

    Ok(Value::Bool(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(wrong_arity(pos, "cons", "exactly 2", args.len()));
    };

    let Value::List(items) = tail else {
        return Err(type_error(pos, "list", tail.type_name()));
    };

    let mut result = Vec::with_capacity(items.len() + 1);
    result.push(head.clone());
    result.extend(items.iter().cloned());
    Ok(Value::List(result))
}

fn eval_car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "car", "exactly 1", args.len()));
    };

    match value {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        other => Err(type_error(pos, "pair", other.type_name())),
    }
}

fn eval_cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "cdr", "exactly 1", args.len()));
    };

    match value {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        other => Err(type_error(pos, "pair", other.type_name())),
    }
}

fn eval_null(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "null?", "exactly 1", args.len()));
    };

    Ok(Value::Bool(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_list_builtin(args: &[Value], _pos: SourcePos) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn eval_map(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [procedure, list] = args else {
        return Err(wrong_arity(pos, "map", "exactly 2", args.len()));
    };

    let Value::List(items) = list else {
        return Err(type_error(pos, "list", list.type_name()));
    };

    let mut mapped = Vec::with_capacity(items.len());
    for item in items {
        mapped.push(apply_value(
            procedure.clone(),
            &[item.clone()],
            pos,
            output,
        )?);
    }

    Ok(Value::List(mapped))
}

fn eval_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "length", "exactly 1", args.len()));
    };

    match value {
        Value::List(items) => Ok(Value::Int(items.len() as i64)),
        other => Err(type_error(pos, "list", other.type_name())),
    }
}

fn eval_display(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "display", "exactly 1", args.len()));
    };

    output.borrow_mut().push_str(&value.render_for_display());
    Ok(Value::Void)
}

fn eval_write(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "write", "exactly 1", args.len()));
    };

    output.borrow_mut().push_str(&value.render());
    Ok(Value::Void)
}

fn eval_newline(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(wrong_arity(pos, "newline", "exactly 0", args.len()));
    }

    output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn eval_string_append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        result.push_str(&expect_string(value, pos)?.to_plain_string());
    }

    Ok(Value::String(SchemeString::new(result)))
}

fn eval_string_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string-length", "exactly 1", args.len()));
    };

    Ok(Value::Int(expect_string(value, pos)?.len() as i64))
}

fn eval_substring(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, start, end] = args else {
        return Err(wrong_arity(pos, "substring", "exactly 3", args.len()));
    };

    let string = expect_string(value, pos)?;
    let start = expect_integer(start, pos)?;
    let end = expect_integer(end, pos)?;
    let len = string.len();

    if start < 0 || end < 0 || start > end || end as usize > len {
        return Err(invalid_range(pos, start, end, len));
    }

    let count = (end - start) as usize;
    let substring = string
        .to_plain_string()
        .chars()
        .skip(start as usize)
        .take(count)
        .collect::<String>();
    Ok(Value::String(SchemeString::new(substring)))
}

fn eval_string_to_number(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string->number", "exactly 1", args.len()));
    };

    match expect_string(value, pos)?.to_plain_string().parse::<i64>() {
        Ok(number) => Ok(Value::Int(number)),
        Err(_) => Ok(Value::Bool(false)),
    }
}

fn eval_number_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "number->string", "exactly 1", args.len()));
    };

    Ok(Value::String(SchemeString::new(
        expect_integer(value, pos)?.to_string(),
    )))
}

fn eval_symbol_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "symbol->string", "exactly 1", args.len()));
    };

    Ok(Value::String(SchemeString::new(
        expect_symbol(value, pos)?.to_string(),
    )))
}

fn eval_string_to_symbol(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string->symbol", "exactly 1", args.len()));
    };

    Ok(Value::Symbol(expect_string(value, pos)?.to_plain_string()))
}

fn eval_string_to_list(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string->list", "exactly 1", args.len()));
    };

    let chars = expect_string(value, pos)?
        .to_plain_string()
        .chars()
        .map(Value::Char)
        .collect();
    Ok(Value::List(chars))
}

fn eval_list_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "list->string", "exactly 1", args.len()));
    };

    let Value::List(items) = value else {
        return Err(type_error(pos, "list", value.type_name()));
    };

    let mut rendered = String::with_capacity(items.len());
    for item in items {
        let Value::Char(ch) = item else {
            return Err(type_error(pos, "char", item.type_name()));
        };
        rendered.push(*ch);
    }

    Ok(Value::String(SchemeString::new(rendered)))
}

fn eval_string_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(wrong_arity(pos, "string-ref", "exactly 2", args.len()));
    };

    let string = expect_string(value, pos)?;
    let index = expect_integer(index, pos)?;
    let len = string.len();

    if index < 0 || index as usize >= len {
        return Err(index_out_of_bounds(pos, index, len));
    }

    let ch = string
        .char_at(index as usize)
        .ok_or_else(|| index_out_of_bounds(pos, index, len))?;
    Ok(Value::Char(ch))
}

fn eval_string_copy(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string-copy", "exactly 1", args.len()));
    };

    Ok(Value::String(expect_string(value, pos)?.copy()))
}

fn eval_string_set(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, _, _] = args else {
        return Err(wrong_arity(pos, "string-set!", "exactly 3", args.len()));
    };

    let _ = expect_string(value, pos)?;
    Err(immutable_string(pos))
}

fn eval_char_to_integer(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "char->integer", "exactly 1", args.len()));
    };

    Ok(Value::Int(expect_char(value, pos)? as i64))
}

fn eval_integer_to_char(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "integer->char", "exactly 1", args.len()));
    };

    let code = expect_integer(value, pos)?;
    let Some(ch) = u32::try_from(code).ok().and_then(char::from_u32) else {
        return Err(invalid_character_code(pos, code));
    };

    Ok(Value::Char(ch))
}

fn eval_type_predicate<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    let [value] = args else {
        return Err(wrong_arity(pos, name, "exactly 1", args.len()));
    };

    Ok(Value::Bool(predicate(value)))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for expr in args {
        let value = eval_expr(expr, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Bool(false))
}

fn eval_number_args(args: &[Value], pos: SourcePos) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Int(number) => Ok(*number),
            other => Err(type_error(pos, "number", other.type_name())),
        })
        .collect()
}

fn expect_integer(value: &Value, pos: SourcePos) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        other => Err(type_error(pos, "number", other.type_name())),
    }
}

fn expect_string(value: &Value, pos: SourcePos) -> Result<SchemeString, EvalError> {
    match value {
        Value::String(string) => Ok(string.clone()),
        other => Err(type_error(pos, "string", other.type_name())),
    }
}

fn expect_symbol<'a>(value: &'a Value, pos: SourcePos) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(symbol) => Ok(symbol),
        other => Err(type_error(pos, "symbol", other.type_name())),
    }
}

fn expect_char(value: &Value, pos: SourcePos) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(type_error(pos, "char", other.type_name())),
    }
}

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render_with_mode(mode));
    }

    rendered.push(')');
    rendered
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

fn render_char(value: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Write => format!("#\\{}", render_char_name(value)),
        RenderMode::Display => value.to_string(),
    }
}

fn render_char_name(value: char) -> String {
    match value {
        ' ' => "space".into(),
        '\n' => "newline".into(),
        other => other.to_string(),
    }
}

#[cfg(test)]
mod tests;
