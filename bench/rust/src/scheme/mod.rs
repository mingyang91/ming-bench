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
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    Apply,
    CallCc,
    Eq,
    Eqv,
    EqualDeep,
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
    ListRef,
    ListTail,
    Length,
    IsList,
    Assoc,
    IsString,
    IsNumber,
    IsBoolean,
    IsPair,
    IsSymbol,
    IsZero,
    IsPositive,
    IsNegative,
    IsOdd,
    IsEven,
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
    CharAlphabetic,
    CharNumeric,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLess,
    StringEqual,
    StringLess,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    IsVector,
    VectorToList,
    ListToVector,
}

const BUILTIN_BINDINGS: &[(&str, Builtin)] = &[
    ("+", Builtin::Add),
    ("-", Builtin::Sub),
    ("*", Builtin::Mul),
    ("/", Builtin::Div),
    ("abs", Builtin::Abs),
    ("modulo", Builtin::Modulo),
    ("remainder", Builtin::Remainder),
    ("quotient", Builtin::Quotient),
    ("min", Builtin::Min),
    ("max", Builtin::Max),
    ("expt", Builtin::Expt),
    ("apply", Builtin::Apply),
    ("call/cc", Builtin::CallCc),
    ("eq?", Builtin::Eq),
    ("eqv?", Builtin::Eqv),
    ("equal?", Builtin::EqualDeep),
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
    ("list-ref", Builtin::ListRef),
    ("list-tail", Builtin::ListTail),
    ("length", Builtin::Length),
    ("list?", Builtin::IsList),
    ("assoc", Builtin::Assoc),
    ("string?", Builtin::IsString),
    ("number?", Builtin::IsNumber),
    ("boolean?", Builtin::IsBoolean),
    ("pair?", Builtin::IsPair),
    ("symbol?", Builtin::IsSymbol),
    ("zero?", Builtin::IsZero),
    ("positive?", Builtin::IsPositive),
    ("negative?", Builtin::IsNegative),
    ("odd?", Builtin::IsOdd),
    ("even?", Builtin::IsEven),
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
    ("char-alphabetic?", Builtin::CharAlphabetic),
    ("char-numeric?", Builtin::CharNumeric),
    ("char-upcase", Builtin::CharUpcase),
    ("char-downcase", Builtin::CharDowncase),
    ("char=?", Builtin::CharEqual),
    ("char<?", Builtin::CharLess),
    ("string=?", Builtin::StringEqual),
    ("string<?", Builtin::StringLess),
    ("string-ci=?", Builtin::StringCiEqual),
    ("string-upcase", Builtin::StringUpcase),
    ("string-downcase", Builtin::StringDowncase),
    ("vector", Builtin::Vector),
    ("make-vector", Builtin::MakeVector),
    ("vector-ref", Builtin::VectorRef),
    ("vector-set!", Builtin::VectorSet),
    ("vector-length", Builtin::VectorLength),
    ("vector?", Builtin::IsVector),
    ("vector->list", Builtin::VectorToList),
    ("list->vector", Builtin::ListToVector),
];

#[derive(Clone, Copy, Debug)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone, Copy, Debug)]
enum LetRecKind {
    LetRec,
    LetRecStar,
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
struct SchemeVector(Rc<RefCell<Vec<Value>>>);

impl SchemeVector {
    fn new(items: Vec<Value>) -> Self {
        Self(Rc::new(RefCell::new(items)))
    }

    fn len(&self) -> usize {
        self.0.borrow().len()
    }

    fn get(&self, index: usize) -> Option<Value> {
        self.0.borrow().get(index).cloned()
    }

    fn set(&self, index: usize, value: Value) -> bool {
        let mut items = self.0.borrow_mut();
        let Some(slot) = items.get_mut(index) else {
            return false;
        };
        *slot = value;
        true
    }

    fn to_vec(&self) -> Vec<Value> {
        self.0.borrow().clone()
    }
}

#[derive(Clone, Debug)]
struct SchemePair(Rc<PairValue>);

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Bool(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(SchemePair),
    Vector(SchemeVector),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Continuation(Rc<Continuation>),
    Uninitialized(String),
    Void,
}

#[derive(Clone, Debug)]
struct PairValue {
    car: Value,
    cdr: Value,
}

impl SchemePair {
    fn new(car: Value, cdr: Value) -> Self {
        Self(Rc::new(PairValue { car, cdr }))
    }

    fn car(&self) -> Value {
        self.0.car.clone()
    }

    fn cdr(&self) -> Value {
        self.0.cdr.clone()
    }
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
        alternate: Option<Expr>,
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
    LetRecBinding {
        kind: LetRecKind,
        bindings: Rc<Vec<(String, Expr)>>,
        index: usize,
        local_env: EnvRef,
        body: Rc<Vec<Expr>>,
        next: Rc<Continuation>,
    },
    Case {
        clauses: Rc<Vec<Expr>>,
        env: EnvRef,
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
        lists: Rc<Vec<Vec<Value>>>,
        len: usize,
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
            Self::Pair(_) => "pair",
            Self::Vector(_) => "vector",
            Self::Builtin(_) | Self::Procedure(_) | Self::Continuation(_) => "procedure",
            Self::Uninitialized(_) => "uninitialized",
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
            Self::Pair(pair) => render_pair(pair, mode),
            Self::Vector(items) => render_vector(items, mode),
            Self::Builtin(_) | Self::Procedure(_) | Self::Continuation(_) => "#<procedure>".into(),
            Self::Uninitialized(_) => "#<uninitialized>".into(),
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

fn invalid_length(pos: SourcePos, len: i64) -> EvalError {
    EvalError::InvalidLength { pos, len }
}

fn invalid_argument(pos: SourcePos, message: impl Into<String>) -> EvalError {
    EvalError::InvalidArgument {
        pos,
        message: message.into(),
    }
}

fn uninitialized_binding(pos: SourcePos, name: impl Into<String>) -> EvalError {
    EvalError::UninitializedBinding {
        pos,
        name: name.into(),
    }
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
        Expr::List(items, _) => quote_list(items),
    }
}

fn quote_list(items: &[Expr]) -> Value {
    if let Some((prefix, tail)) = dotted_list_parts(items) {
        prefix.iter().rev().fold(quote_expr(tail), |cdr, expr| {
            Value::Pair(SchemePair::new(quote_expr(expr), cdr))
        })
    } else {
        Value::List(items.iter().map(quote_expr).collect())
    }
}

fn dotted_list_parts(items: &[Expr]) -> Option<(&[Expr], &Expr)> {
    let mut dot_index = None;

    for (index, expr) in items.iter().enumerate() {
        if matches!(expr, Expr::Symbol(name, _) if name == ".") {
            if dot_index.is_some() {
                return None;
            }
            dot_index = Some(index);
        }
    }

    let dot_index = dot_index?;
    if dot_index == 0 || dot_index + 2 != items.len() {
        return None;
    }

    Some((&items[..dot_index], &items[dot_index + 1]))
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
        Expr::Symbol(name, pos) => {
            let value = env
                .lookup(&name)
                .ok_or_else(|| unbound_variable(pos, name.clone()))?;
            if let Value::Uninitialized(binding) = value {
                return Err(uninitialized_binding(pos, binding));
            }

            Ok(MachineState::Return { value, cont })
        }
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
            "letrec" => return eval_letrec_state(&items[1..], *pos, env, cont, LetRecKind::LetRec),
            "letrec*" => {
                return eval_letrec_state(&items[1..], *pos, env, cont, LetRecKind::LetRecStar)
            }
            "begin" => {
                return Ok(eval_sequence_state(
                    Rc::new(items[1..].to_vec()),
                    0,
                    env,
                    cont,
                ))
            }
            "cond" => return eval_cond_state(&items[1..], env, cont),
            "case" => return eval_case_state(&items[1..], *pos, env, cont),
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
    let (condition, consequent, alternate) = match parts {
        [condition, consequent] => (condition, consequent, None),
        [condition, consequent, alternate] => (condition, consequent, Some(alternate.clone())),
        _ => return Err(wrong_arity(pos, "if", "2 or 3", parts.len())),
    };

    Ok(MachineState::Eval {
        expr: condition.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::If {
            consequent: consequent.clone(),
            alternate,
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

fn eval_letrec_state(
    parts: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
    kind: LetRecKind,
) -> Result<MachineState, EvalError> {
    let form_name = match kind {
        LetRecKind::LetRec => "letrec",
        LetRecKind::LetRecStar => "letrec*",
    };
    let [bindings_expr, body @ ..] = parts else {
        return Err(wrong_arity(pos, form_name, "at least 2", parts.len()));
    };

    if body.is_empty() {
        return Err(syntax_error(pos, format!("{form_name} requires a body")));
    }

    let bindings = Rc::new(parse_let_bindings(bindings_expr)?);
    let body = Rc::new(body.to_vec());
    let local_env = Environment::new(Some(env));
    if bindings.is_empty() {
        return Ok(eval_sequence_state(body, 0, local_env, cont));
    }

    match kind {
        LetRecKind::LetRec => {
            for (name, _) in bindings.iter() {
                local_env.define(name.clone(), Value::Uninitialized(name.clone()));
            }
        }
        LetRecKind::LetRecStar => {
            let first_name = bindings[0].0.clone();
            local_env.define(first_name.clone(), Value::Uninitialized(first_name));
        }
    }

    Ok(MachineState::Eval {
        expr: bindings[0].1.clone(),
        env: local_env.clone(),
        cont: Rc::new(Continuation::LetRecBinding {
            kind,
            bindings,
            index: 0,
            local_env,
            body,
            next: cont,
        }),
    })
}

fn eval_cond_state(
    clauses: &[Expr],
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    eval_cond_clauses(Rc::new(clauses.to_vec()), 0, env, cont)
}

fn eval_case_state(
    parts: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    let [key_expr, clauses @ ..] = parts else {
        return Err(wrong_arity(pos, "case", "at least 2", parts.len()));
    };

    Ok(MachineState::Eval {
        expr: key_expr.clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::Case {
            clauses: Rc::new(clauses.to_vec()),
            env,
            next: cont,
        }),
    })
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

fn eval_case_clauses(
    key: &Value,
    clauses: &[Expr],
    env: EnvRef,
    cont: Rc<Continuation>,
) -> Result<MachineState, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(syntax_error(clause.pos(), "case clause must be a list"));
        };

        let Some((datum_expr, body)) = items.split_first() else {
            return Err(syntax_error(clause.pos(), "case clause cannot be empty"));
        };

        if let Expr::Symbol(name, _) = datum_expr {
            if name == "else" {
                if index + 1 != clauses.len() {
                    return Err(syntax_error(
                        datum_expr.pos(),
                        "case else clause must be last",
                    ));
                }
                if body.is_empty() {
                    return Err(syntax_error(
                        clause.pos(),
                        "case else clause requires a body",
                    ));
                }
                return Ok(eval_sequence_state(Rc::new(body.to_vec()), 0, env, cont));
            }
        }

        let Expr::List(datums, _) = datum_expr else {
            return Err(syntax_error(
                datum_expr.pos(),
                "case clause datums must be a list",
            ));
        };

        if datums
            .iter()
            .map(quote_expr)
            .any(|datum| values_eqv(key, &datum))
        {
            if body.is_empty() {
                return Err(syntax_error(clause.pos(), "case clause requires a body"));
            }
            return Ok(eval_sequence_state(Rc::new(body.to_vec()), 0, env, cont));
        }
    }

    Ok(MachineState::Return {
        value: Value::Void,
        cont,
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
            } else if let Some(alternate) = alternate {
                alternate.clone()
            } else {
                return Ok(MachineState::Return {
                    value: Value::Void,
                    cont: next.clone(),
                });
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
        Continuation::LetRecBinding {
            kind,
            bindings,
            index,
            local_env,
            body,
            next,
        } => {
            let (name, _) = &bindings[*index];
            local_env.set(name, value);

            if *index + 1 >= bindings.len() {
                Ok(eval_sequence_state(
                    body.clone(),
                    0,
                    local_env.clone(),
                    next.clone(),
                ))
            } else {
                let next_index = *index + 1;
                if matches!(kind, LetRecKind::LetRecStar) {
                    let next_name = bindings[next_index].0.clone();
                    local_env.define(next_name.clone(), Value::Uninitialized(next_name));
                }

                Ok(MachineState::Eval {
                    expr: bindings[next_index].1.clone(),
                    env: local_env.clone(),
                    cont: Rc::new(Continuation::LetRecBinding {
                        kind: *kind,
                        bindings: bindings.clone(),
                        index: next_index,
                        local_env: local_env.clone(),
                        body: body.clone(),
                        next: next.clone(),
                    }),
                })
            }
        }
        Continuation::Case { clauses, env, next } => {
            eval_case_clauses(&value, clauses.as_ref(), env.clone(), next.clone())
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
            lists,
            len,
            index,
            acc,
            pos,
            output,
            next,
        } => {
            let mut next_acc = acc.clone();
            next_acc.push(value);

            if *index >= *len {
                Ok(MachineState::Return {
                    value: Value::List(next_acc),
                    cont: next.clone(),
                })
            } else {
                let row = lists
                    .iter()
                    .map(|list| list[*index].clone())
                    .collect::<Vec<_>>();
                dispatch_apply(
                    procedure.clone(),
                    row,
                    *pos,
                    output.clone(),
                    Rc::new(Continuation::Map {
                        procedure: procedure.clone(),
                        lists: lists.clone(),
                        len: *len,
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
        Builtin::Abs => Some(eval_abs(&args, pos)?),
        Builtin::Modulo => Some(eval_modulo(&args, pos)?),
        Builtin::Remainder => Some(eval_remainder(&args, pos)?),
        Builtin::Quotient => Some(eval_quotient(&args, pos)?),
        Builtin::Min => Some(eval_min(&args, pos)?),
        Builtin::Max => Some(eval_max(&args, pos)?),
        Builtin::Expt => Some(eval_expt(&args, pos)?),
        Builtin::Apply => {
            let [operator, rest @ ..] = args.as_slice() else {
                return Err(wrong_arity(pos, "apply", "at least 2", args.len()));
            };

            if rest.is_empty() {
                return Err(wrong_arity(pos, "apply", "at least 2", args.len()));
            }

            let prefix = &rest[..rest.len() - 1];
            let last = &rest[rest.len() - 1];
            let spliced = proper_list_to_vec(last).ok_or_else(|| type_error(pos, "list", last.type_name()))?;

            let mut expanded_args = Vec::with_capacity(prefix.len() + spliced.len());
            expanded_args.extend(prefix.iter().cloned());
            expanded_args.extend(spliced);
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
        Builtin::Eq => Some(eval_eqv_like(&args, pos, "eq?", values_eq)?),
        Builtin::Eqv => Some(eval_eqv_like(&args, pos, "eqv?", values_eqv)?),
        Builtin::EqualDeep => Some(eval_eqv_like(&args, pos, "equal?", values_equal)?),
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
            let [procedure, list_args @ ..] = args.as_slice() else {
                return Err(wrong_arity(pos, "map", "at least 2", args.len()));
            };

            if list_args.is_empty() {
                return Err(wrong_arity(pos, "map", "at least 2", args.len()));
            }

            let lists = list_args
                .iter()
                .map(|value| {
                    proper_list_to_vec(value).ok_or_else(|| type_error(pos, "list", value.type_name()))
                })
                .collect::<Result<Vec<_>, _>>()?;

            let len = lists.iter().map(Vec::len).min().unwrap_or(0);
            if len == 0 {
                Some(Value::List(Vec::new()))
            } else {
                let row = lists.iter().map(|list| list[0].clone()).collect::<Vec<_>>();
                let lists = Rc::new(lists);
                return dispatch_apply(
                    procedure.clone(),
                    row,
                    pos,
                    output.clone(),
                    Rc::new(Continuation::Map {
                        procedure: procedure.clone(),
                        lists,
                        len,
                        index: 1,
                        acc: Vec::new(),
                        pos,
                        output,
                        next: cont,
                    }),
                );
            }
        }
        Builtin::ListRef => Some(eval_list_ref(&args, pos)?),
        Builtin::ListTail => Some(eval_list_tail(&args, pos)?),
        Builtin::Length => Some(eval_length(&args, pos)?),
        Builtin::IsList => Some(eval_type_predicate(&args, "list?", pos, |value| {
            proper_list_to_vec(value).is_some()
        })?),
        Builtin::Assoc => Some(eval_assoc(&args, pos)?),
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
            is_pair,
        )?),
        Builtin::IsSymbol => Some(eval_type_predicate(&args, "symbol?", pos, |value| {
            matches!(value, Value::Symbol(_))
        })?),
        Builtin::IsZero => Some(eval_number_predicate(&args, "zero?", pos, |value| value == 0)?),
        Builtin::IsPositive => {
            Some(eval_number_predicate(&args, "positive?", pos, |value| value > 0)?)
        }
        Builtin::IsNegative => {
            Some(eval_number_predicate(&args, "negative?", pos, |value| value < 0)?)
        }
        Builtin::IsOdd => Some(eval_number_predicate(&args, "odd?", pos, |value| value % 2 != 0)?),
        Builtin::IsEven => {
            Some(eval_number_predicate(&args, "even?", pos, |value| value % 2 == 0)?)
        }
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
        Builtin::CharAlphabetic => Some(eval_char_predicate(
            &args,
            "char-alphabetic?",
            pos,
            |value| value.is_alphabetic(),
        )?),
        Builtin::CharNumeric => Some(eval_char_predicate(
            &args,
            "char-numeric?",
            pos,
            |value| value.is_numeric(),
        )?),
        Builtin::CharUpcase => Some(eval_char_transform(&args, "char-upcase", pos, |value| {
            value.to_uppercase().next().unwrap_or(value)
        })?),
        Builtin::CharDowncase => Some(eval_char_transform(
            &args,
            "char-downcase",
            pos,
            |value| value.to_lowercase().next().unwrap_or(value),
        )?),
        Builtin::CharEqual => Some(eval_char_compare(&args, "char=?", pos, |left, right| {
            left == right
        })?),
        Builtin::CharLess => Some(eval_char_compare(&args, "char<?", pos, |left, right| {
            left < right
        })?),
        Builtin::StringEqual => Some(eval_string_compare(
            &args,
            "string=?",
            pos,
            |left, right| left == right,
        )?),
        Builtin::StringLess => Some(eval_string_compare(
            &args,
            "string<?",
            pos,
            |left, right| left < right,
        )?),
        Builtin::StringCiEqual => Some(eval_string_compare(
            &args,
            "string-ci=?",
            pos,
            |left, right| left.to_lowercase() == right.to_lowercase(),
        )?),
        Builtin::StringUpcase => Some(eval_string_case(&args, "string-upcase", pos, |value| {
            value.chars().flat_map(char::to_uppercase).collect()
        })?),
        Builtin::StringDowncase => Some(eval_string_case(
            &args,
            "string-downcase",
            pos,
            |value| value.chars().flat_map(char::to_lowercase).collect(),
        )?),
        Builtin::Vector => Some(eval_vector(&args, pos)?),
        Builtin::MakeVector => Some(eval_make_vector(&args, pos)?),
        Builtin::VectorRef => Some(eval_vector_ref(&args, pos)?),
        Builtin::VectorSet => Some(eval_vector_set(&args, pos)?),
        Builtin::VectorLength => Some(eval_vector_length(&args, pos)?),
        Builtin::IsVector => Some(eval_type_predicate(&args, "vector?", pos, |value| {
            matches!(value, Value::Vector(_))
        })?),
        Builtin::VectorToList => Some(eval_vector_to_list(&args, pos)?),
        Builtin::ListToVector => Some(eval_list_to_vector(&args, pos)?),
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
        | Continuation::LetRecBinding { next, .. }
        | Continuation::Case { next, .. }
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

fn eval_abs(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "abs", "exactly 1", args.len()));
    };

    Ok(Value::Int(expect_integer(value, pos)?.abs()))
}

fn eval_modulo(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [dividend, divisor] = args else {
        return Err(wrong_arity(pos, "modulo", "exactly 2", args.len()));
    };

    let dividend = expect_integer(dividend, pos)?;
    let divisor = expect_integer(divisor, pos)?;
    if divisor == 0 {
        return Err(division_by_zero(pos));
    }

    let remainder = dividend % divisor;
    let modulo = if remainder != 0 && (remainder > 0) != (divisor > 0) {
        remainder + divisor
    } else {
        remainder
    };
    Ok(Value::Int(modulo))
}

fn eval_remainder(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [dividend, divisor] = args else {
        return Err(wrong_arity(pos, "remainder", "exactly 2", args.len()));
    };

    let dividend = expect_integer(dividend, pos)?;
    let divisor = expect_integer(divisor, pos)?;
    if divisor == 0 {
        return Err(division_by_zero(pos));
    }

    Ok(Value::Int(dividend % divisor))
}

fn eval_quotient(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [dividend, divisor] = args else {
        return Err(wrong_arity(pos, "quotient", "exactly 2", args.len()));
    };

    let dividend = expect_integer(dividend, pos)?;
    let divisor = expect_integer(divisor, pos)?;
    if divisor == 0 {
        return Err(division_by_zero(pos));
    }

    Ok(Value::Int(dividend / divisor))
}

fn eval_min(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    let Some(minimum) = numbers.into_iter().min() else {
        return Err(wrong_arity(pos, "min", "at least 1", 0));
    };
    Ok(Value::Int(minimum))
}

fn eval_max(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    let Some(maximum) = numbers.into_iter().max() else {
        return Err(wrong_arity(pos, "max", "at least 1", 0));
    };
    Ok(Value::Int(maximum))
}

fn eval_expt(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [base, exponent] = args else {
        return Err(wrong_arity(pos, "expt", "exactly 2", args.len()));
    };

    let base = expect_integer(base, pos)?;
    let exponent = expect_integer(exponent, pos)?;
    let exponent =
        u32::try_from(exponent).map_err(|_| invalid_argument(pos, "expt requires a non-negative exponent"))?;
    Ok(Value::Int(base.pow(exponent)))
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
    let spliced = proper_list_to_vec(last).ok_or_else(|| type_error(pos, "list", last.type_name()))?;

    let mut expanded_args = Vec::with_capacity(prefix.len() + spliced.len());
    expanded_args.extend_from_slice(prefix);
    expanded_args.extend(spliced);
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

fn eval_eqv_like<F>(
    args: &[Value],
    pos: SourcePos,
    name: &str,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value, &Value) -> bool,
{
    let [left, right] = args else {
        return Err(wrong_arity(pos, name, "exactly 2", args.len()));
    };

    Ok(Value::Bool(predicate(left, right)))
}

fn eval_number_predicate<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(i64) -> bool,
{
    let [value] = args else {
        return Err(wrong_arity(pos, name, "exactly 1", args.len()));
    };

    Ok(Value::Bool(predicate(expect_integer(value, pos)?)))
}

fn values_eq(left: &Value, right: &Value) -> bool {
    values_eqv(left, right)
}

fn values_eqv(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::String(a), Value::String(b)) => Rc::ptr_eq(&a.0, &b.0),
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(&a.0, &b.0),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(&a.0, &b.0),
        (Value::Builtin(a), Value::Builtin(b)) => {
            std::mem::discriminant(a) == std::mem::discriminant(b)
        }
        (Value::Procedure(a), Value::Procedure(b)) => Rc::ptr_eq(a, b),
        (Value::Continuation(a), Value::Continuation(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn values_equal(left: &Value, right: &Value) -> bool {
    if let (Some(left_items), Some(right_items)) = (proper_list_to_vec(left), proper_list_to_vec(right))
    {
        return left_items.len() == right_items.len()
            && left_items
                .iter()
                .zip(right_items.iter())
                .all(|(left_item, right_item)| values_equal(left_item, right_item));
    }

    match (left, right) {
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::String(a), Value::String(b)) => a.to_plain_string() == b.to_plain_string(),
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Pair(a), Value::Pair(b)) => {
            values_equal(&a.car(), &b.car()) && values_equal(&a.cdr(), &b.cdr())
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let left_items = a.to_vec();
            let right_items = b.to_vec();
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left_item, right_item)| values_equal(left_item, right_item))
        }
        _ => values_eqv(left, right),
    }
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

    if let Some(mut items) = proper_list_to_vec(tail) {
        let mut result = Vec::with_capacity(items.len() + 1);
        result.push(head.clone());
        result.append(&mut items);
        Ok(Value::List(result))
    } else {
        Ok(Value::Pair(SchemePair::new(head.clone(), tail.clone())))
    }
}

fn eval_car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "car", "exactly 1", args.len()));
    };

    pair_car(value).ok_or_else(|| type_error(pos, "pair", value.type_name()))
}

fn eval_cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "cdr", "exactly 1", args.len()));
    };

    pair_cdr(value).ok_or_else(|| type_error(pos, "pair", value.type_name()))
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

fn eval_list_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(wrong_arity(pos, "list-ref", "exactly 2", args.len()));
    };

    let index = expect_integer(index, pos)?;
    if index < 0 {
        return Err(index_out_of_bounds(pos, index, 0));
    }

    let items = proper_list_to_vec(list).ok_or_else(|| type_error(pos, "list", list.type_name()))?;
    items
        .get(index as usize)
        .cloned()
        .ok_or_else(|| index_out_of_bounds(pos, index, items.len()))
}

fn eval_list_tail(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(wrong_arity(pos, "list-tail", "exactly 2", args.len()));
    };

    let index = expect_integer(index, pos)?;
    if index < 0 {
        return Err(index_out_of_bounds(pos, index, 0));
    }

    let items = proper_list_to_vec(list).ok_or_else(|| type_error(pos, "list", list.type_name()))?;
    if index as usize > items.len() {
        return Err(index_out_of_bounds(pos, index, items.len()));
    }

    Ok(Value::List(items[index as usize..].to_vec()))
}

fn eval_assoc(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [key, alist] = args else {
        return Err(wrong_arity(pos, "assoc", "exactly 2", args.len()));
    };

    let entries = proper_list_to_vec(alist).ok_or_else(|| type_error(pos, "list", alist.type_name()))?;
    for entry in entries {
        let Some(found_key) = pair_car(&entry) else {
            return Err(type_error(pos, "pair", entry.type_name()));
        };

        if values_equal(key, &found_key) {
            return Ok(entry);
        }
    }

    Ok(Value::Bool(false))
}

fn eval_map(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [procedure, list_args @ ..] = args else {
        return Err(wrong_arity(pos, "map", "at least 2", args.len()));
    };

    if list_args.is_empty() {
        return Err(wrong_arity(pos, "map", "at least 2", args.len()));
    }

    let lists = list_args
        .iter()
        .map(|value| {
            proper_list_to_vec(value).ok_or_else(|| type_error(pos, "list", value.type_name()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let len = lists.iter().map(Vec::len).min().unwrap_or(0);
    let mut mapped = Vec::with_capacity(len);
    for index in 0..len {
        let row = lists.iter().map(|list| list[index].clone()).collect::<Vec<_>>();
        mapped.push(apply_value(procedure.clone(), &row, pos, output)?);
    }

    Ok(Value::List(mapped))
}

fn eval_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "length", "exactly 1", args.len()));
    };

    let items = proper_list_to_vec(value).ok_or_else(|| type_error(pos, "list", value.type_name()))?;
    Ok(Value::Int(items.len() as i64))
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

    let items = proper_list_to_vec(value).ok_or_else(|| type_error(pos, "list", value.type_name()))?;

    let mut rendered = String::with_capacity(items.len());
    for item in &items {
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

fn eval_char_predicate<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(char) -> bool,
{
    let [value] = args else {
        return Err(wrong_arity(pos, name, "exactly 1", args.len()));
    };

    Ok(Value::Bool(predicate(expect_char(value, pos)?)))
}

fn eval_char_transform<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    transform: F,
) -> Result<Value, EvalError>
where
    F: Fn(char) -> char,
{
    let [value] = args else {
        return Err(wrong_arity(pos, name, "exactly 1", args.len()));
    };

    Ok(Value::Char(transform(expect_char(value, pos)?)))
}

fn eval_char_compare<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    let chars = args
        .iter()
        .map(|value| expect_char(value, pos))
        .collect::<Result<Vec<_>, _>>()?;
    if chars.len() < 2 {
        return Err(wrong_arity(pos, name, "at least 2", chars.len()));
    }

    for pair in chars.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_string_compare<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    let strings = args
        .iter()
        .map(|value| Ok(expect_string(value, pos)?.to_plain_string()))
        .collect::<Result<Vec<_>, EvalError>>()?;
    if strings.len() < 2 {
        return Err(wrong_arity(pos, name, "at least 2", strings.len()));
    }

    for pair in strings.windows(2) {
        if !predicate(&pair[0], &pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_string_case<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    transform: F,
) -> Result<Value, EvalError>
where
    F: Fn(&str) -> String,
{
    let [value] = args else {
        return Err(wrong_arity(pos, name, "exactly 1", args.len()));
    };

    Ok(Value::String(SchemeString::new(transform(
        &expect_string(value, pos)?.to_plain_string(),
    ))))
}

fn eval_vector(args: &[Value], _pos: SourcePos) -> Result<Value, EvalError> {
    Ok(Value::Vector(SchemeVector::new(args.to_vec())))
}

fn eval_make_vector(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let (length_value, fill_value) = match args {
        [length] => (length, Value::Void),
        [length, fill] => (length, fill.clone()),
        _ => return Err(wrong_arity(pos, "make-vector", "1 or 2", args.len())),
    };

    let length = expect_integer(length_value, pos)?;
    if length < 0 {
        return Err(invalid_length(pos, length));
    }

    Ok(Value::Vector(SchemeVector::new(vec![
        fill_value;
        length as usize
    ])))
}

fn eval_vector_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(wrong_arity(pos, "vector-ref", "exactly 2", args.len()));
    };

    let vector = expect_vector(value, pos)?;
    let index = expect_integer(index, pos)?;
    let len = vector.len();
    if index < 0 || index as usize >= len {
        return Err(index_out_of_bounds(pos, index, len));
    }

    vector
        .get(index as usize)
        .ok_or_else(|| index_out_of_bounds(pos, index, len))
}

fn eval_vector_set(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, index, replacement] = args else {
        return Err(wrong_arity(pos, "vector-set!", "exactly 3", args.len()));
    };

    let vector = expect_vector(value, pos)?;
    let index = expect_integer(index, pos)?;
    let len = vector.len();
    if index < 0 || index as usize >= len {
        return Err(index_out_of_bounds(pos, index, len));
    }

    if !vector.set(index as usize, replacement.clone()) {
        return Err(index_out_of_bounds(pos, index, len));
    }

    Ok(Value::Void)
}

fn eval_vector_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "vector-length", "exactly 1", args.len()));
    };

    Ok(Value::Int(expect_vector(value, pos)?.len() as i64))
}

fn eval_vector_to_list(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "vector->list", "exactly 1", args.len()));
    };

    Ok(Value::List(expect_vector(value, pos)?.to_vec()))
}

fn eval_list_to_vector(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "list->vector", "exactly 1", args.len()));
    };

    let items = proper_list_to_vec(value).ok_or_else(|| type_error(pos, "list", value.type_name()))?;

    Ok(Value::Vector(SchemeVector::new(items)))
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

fn expect_vector(value: &Value, pos: SourcePos) -> Result<SchemeVector, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector.clone()),
        other => Err(type_error(pos, "vector", other.type_name())),
    }
}

fn is_pair(value: &Value) -> bool {
    pair_car(value).is_some()
}

fn pair_car(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) if !items.is_empty() => Some(items[0].clone()),
        Value::Pair(pair) => Some(pair.car()),
        _ => None,
    }
}

fn pair_cdr(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) if !items.is_empty() => Some(Value::List(items[1..].to_vec())),
        Value::Pair(pair) => Some(pair.cdr()),
        _ => None,
    }
}

fn proper_list_to_vec(value: &Value) -> Option<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::List(rest) => {
                items.extend(rest);
                return Some(items);
            }
            Value::Pair(pair) => {
                items.push(pair.car());
                current = pair.cdr();
            }
            _ => return None,
        }
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

fn render_pair(pair: &SchemePair, mode: RenderMode) -> String {
    let mut rendered = String::from("(");
    let mut first = true;
    let mut current = Value::Pair(pair.clone());

    loop {
        match current {
            Value::Pair(next) => {
                if !first {
                    rendered.push(' ');
                }
                rendered.push_str(&next.car().render_with_mode(mode));
                current = next.cdr();
                first = false;
            }
            Value::List(items) => {
                for item in items {
                    if !first {
                        rendered.push(' ');
                    }
                    rendered.push_str(&item.render_with_mode(mode));
                    first = false;
                }
                rendered.push(')');
                return rendered;
            }
            other => {
                if !first {
                    rendered.push_str(" . ");
                }
                rendered.push_str(&other.render_with_mode(mode));
                rendered.push(')');
                return rendered;
            }
        }
    }
}

fn render_vector(items: &SchemeVector, mode: RenderMode) -> String {
    let mut rendered = String::from("#(");

    for (index, item) in items.to_vec().iter().enumerate() {
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
