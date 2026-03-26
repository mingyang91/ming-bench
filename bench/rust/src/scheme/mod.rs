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
    Char(char),
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
    String(SchemeString),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Builtin(Builtin),
    Procedure(Rc<Closure>),
    Void,
}

#[derive(Clone)]
struct SchemeString {
    inner: Rc<RefCell<StringCell>>,
}

struct StringCell {
    value: String,
    mutable: bool,
}

enum StringSetError {
    Immutable,
    IndexOutOfBounds { len: usize },
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
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
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Pair(_, _) => "pair",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        self.render_with(RenderMode::Write)
    }

    fn render_display(&self) -> String {
        self.render_with(RenderMode::Display)
    }

    fn render_with(&self, mode: RenderMode) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => {
                let value = value.as_string();
                match mode {
                    RenderMode::Write => format!("{value:?}"),
                    RenderMode::Display => value,
                }
            }
            Self::Char(value) => render_char(*value, mode),
            Self::Symbol(name) => name.clone(),
            Self::List(items) => render_list(items, mode),
            Self::Pair(head, tail) => render_pair(head, tail, mode),
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

impl SchemeString {
    fn new(value: impl Into<String>, mutable: bool) -> Self {
        Self {
            inner: Rc::new(RefCell::new(StringCell {
                value: value.into(),
                mutable,
            })),
        }
    }

    fn immutable(value: impl Into<String>) -> Self {
        Self::new(value, false)
    }

    fn mutable_copy(&self) -> Self {
        Self::new(self.as_string(), true)
    }

    fn as_string(&self) -> String {
        self.inner.borrow().value.clone()
    }

    fn set_char(&self, index: usize, ch: char) -> Result<(), StringSetError> {
        let mut string = self.inner.borrow_mut();
        if !string.mutable {
            return Err(StringSetError::Immutable);
        }

        let mut chars: Vec<char> = string.value.chars().collect();
        if index >= chars.len() {
            return Err(StringSetError::IndexOutOfBounds { len: chars.len() });
        }

        chars[index] = ch;
        string.value = chars.into_iter().collect();
        Ok(())
    }
}

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render_with(mode));
    }

    rendered.push(')');
    rendered
}

fn render_pair(head: &Value, tail: &Value, mode: RenderMode) -> String {
    let mut rendered = String::from("(");
    render_pair_contents(head, tail, mode, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_contents(head: &Value, tail: &Value, mode: RenderMode, output: &mut String) {
    output.push_str(&head.render_with(mode));

    match tail {
        Value::List(items) => {
            for item in items {
                output.push(' ');
                output.push_str(&item.render_with(mode));
            }
        }
        Value::Pair(next_head, next_tail) => {
            output.push(' ');
            render_pair_contents(next_head, next_tail, mode, output);
        }
        other => {
            output.push_str(" . ");
            output.push_str(&other.render_with(mode));
        }
    }
}

fn render_char(value: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => value.to_string(),
        RenderMode::Write => match value {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            other => format!("#\\{other}"),
        },
    }
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
    Eq,
    EqualDeep,
    LessEqual,
    Not,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    ZeroPred,
    PositivePred,
    NegativePred,
    OddPred,
    EvenPred,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    ListRef,
    ListTail,
    ListPred,
    Length,
    Append,
    Assoc,
    Map,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    Display,
    Write,
    Newline,
    StringCopy,
    StringAppend,
    StringLength,
    StringSet,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLess,
    StringEqual,
    StringLess,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Apply,
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
            Self::Eq => "eq?",
            Self::EqualDeep => "equal?",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Abs => "abs",
            Self::Modulo => "modulo",
            Self::Remainder => "remainder",
            Self::Quotient => "quotient",
            Self::Min => "min",
            Self::Max => "max",
            Self::Expt => "expt",
            Self::ZeroPred => "zero?",
            Self::PositivePred => "positive?",
            Self::NegativePred => "negative?",
            Self::OddPred => "odd?",
            Self::EvenPred => "even?",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPred => "list?",
            Self::Length => "length",
            Self::Append => "append",
            Self::Assoc => "assoc",
            Self::Map => "map",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringCopy => "string-copy",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::StringSet => "string-set!",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::CharPred => "char?",
            Self::CharAlphabeticPred => "char-alphabetic?",
            Self::CharNumericPred => "char-numeric?",
            Self::CharUpcase => "char-upcase",
            Self::CharDowncase => "char-downcase",
            Self::CharEqual => "char=?",
            Self::CharLess => "char<?",
            Self::StringEqual => "string=?",
            Self::StringLess => "string<?",
            Self::StringCiEqual => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::Apply => "apply",
        }
    }
}

#[derive(Clone)]
struct ParameterSpec {
    required: Vec<String>,
    rest: Option<String>,
}

struct Closure {
    params: ParameterSpec,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<Env>;

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
    output: Rc<RefCell<String>>,
}

impl Env {
    fn new(output: Rc<RefCell<String>>) -> EnvRef {
        let env = Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
            output,
        });

        for (name, builtin) in [
            ("+", Builtin::Add),
            ("-", Builtin::Sub),
            ("*", Builtin::Mul),
            ("/", Builtin::Div),
            ("<", Builtin::LessThan),
            (">", Builtin::GreaterThan),
            ("=", Builtin::Equal),
            ("eq?", Builtin::Eq),
            ("equal?", Builtin::EqualDeep),
            ("<=", Builtin::LessEqual),
            ("not", Builtin::Not),
            ("abs", Builtin::Abs),
            ("modulo", Builtin::Modulo),
            ("remainder", Builtin::Remainder),
            ("quotient", Builtin::Quotient),
            ("min", Builtin::Min),
            ("max", Builtin::Max),
            ("expt", Builtin::Expt),
            ("zero?", Builtin::ZeroPred),
            ("positive?", Builtin::PositivePred),
            ("negative?", Builtin::NegativePred),
            ("odd?", Builtin::OddPred),
            ("even?", Builtin::EvenPred),
            ("cons", Builtin::Cons),
            ("car", Builtin::Car),
            ("cdr", Builtin::Cdr),
            ("null?", Builtin::NullPred),
            ("list", Builtin::List),
            ("list-ref", Builtin::ListRef),
            ("list-tail", Builtin::ListTail),
            ("list?", Builtin::ListPred),
            ("length", Builtin::Length),
            ("append", Builtin::Append),
            ("assoc", Builtin::Assoc),
            ("map", Builtin::Map),
            ("string?", Builtin::StringPred),
            ("number?", Builtin::NumberPred),
            ("boolean?", Builtin::BooleanPred),
            ("pair?", Builtin::PairPred),
            ("symbol?", Builtin::SymbolPred),
            ("display", Builtin::Display),
            ("write", Builtin::Write),
            ("newline", Builtin::Newline),
            ("string-copy", Builtin::StringCopy),
            ("string-append", Builtin::StringAppend),
            ("string-length", Builtin::StringLength),
            ("string-set!", Builtin::StringSet),
            ("substring", Builtin::Substring),
            ("string->number", Builtin::StringToNumber),
            ("number->string", Builtin::NumberToString),
            ("symbol->string", Builtin::SymbolToString),
            ("string->symbol", Builtin::StringToSymbol),
            ("string-ref", Builtin::StringRef),
            ("char?", Builtin::CharPred),
            ("char-alphabetic?", Builtin::CharAlphabeticPred),
            ("char-numeric?", Builtin::CharNumericPred),
            ("char-upcase", Builtin::CharUpcase),
            ("char-downcase", Builtin::CharDowncase),
            ("char=?", Builtin::CharEqual),
            ("char<?", Builtin::CharLess),
            ("string=?", Builtin::StringEqual),
            ("string<?", Builtin::StringLess),
            ("string-ci=?", Builtin::StringCiEqual),
            ("string-upcase", Builtin::StringUpcase),
            ("string-downcase", Builtin::StringDowncase),
            ("apply", Builtin::Apply),
        ] {
            env.define(name.into(), Value::Builtin(builtin));
        }

        env
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent.clone()),
            output: parent.output.clone(),
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

    fn set(&self, name: &str, value: Value) -> bool {
        let mut bindings = self.bindings.borrow_mut();
        if let Some(binding) = bindings.get_mut(name) {
            *binding = value;
            return true;
        }
        drop(bindings);

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
    }

    fn write_output(&self, text: &str) {
        self.output.borrow_mut().push_str(text);
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
        Ok(Expr::new(
            ExprKind::List(vec![Expr::symbol("quote", pos), self.parse_expr()?]),
            pos,
        ))
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
                            return Err(EvalError::InvalidEscape { escape: other }
                                .with_offset(escape_pos.offset))
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
            _ => {
                if let Some(value) = parse_char_literal(token, pos)? {
                    return Ok(Expr::new(ExprKind::Char(value), pos));
                }

                match token.parse::<i64>() {
                    Ok(value) => Ok(Expr::new(ExprKind::Integer(value), pos)),
                    Err(_) => Ok(Expr::symbol(token, pos)),
                }
            }
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

fn parse_char_literal(token: &str, pos: SourcePos) -> Result<Option<char>, EvalError> {
    let Some(literal) = token.strip_prefix("#\\") else {
        return Ok(None);
    };

    let value = match literal {
        "space" => ' ',
        "newline" => '\n',
        "" => {
            return Err(EvalError::InvalidSyntax {
                message: "invalid character literal".into(),
            }
            .with_offset(pos.offset))
        }
        _ => {
            let mut chars = literal.chars();
            let ch = chars.next().unwrap();
            if chars.next().is_some() {
                return Err(EvalError::InvalidSyntax {
                    message: format!("invalid character literal: {token}"),
                }
                .with_offset(pos.offset));
            }
            ch
        }
    };

    Ok(Some(value))
}

fn eval_program(expressions: &[Expr], output: Rc<RefCell<String>>) -> Result<Value, EvalError> {
    let env = Env::new(output);
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
        ExprKind::String(value) => Ok(Value::String(SchemeString::immutable(value.clone()))),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::Symbol(name) => env.lookup(name).ok_or_else(|| {
            EvalError::UnboundVariable { name: name.clone() }.with_offset(expr.pos.offset)
        }),
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
            "set!" => return eval_set(operator.pos, arguments, env),
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
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }
        .with_offset(call_pos.offset)),
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
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                left < right
            })
        }
        Builtin::GreaterThan => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                left > right
            })
        }
        Builtin::Equal => eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
            left == right
        }),
        Builtin::Eq => eval_eq(arguments, env, call_pos),
        Builtin::EqualDeep => eval_equal(arguments, env, call_pos),
        Builtin::LessEqual => {
            eval_compare(builtin.name(), arguments, env, call_pos, |left, right| {
                left <= right
            })
        }
        Builtin::Not => eval_not(arguments, env, call_pos),
        Builtin::Abs => eval_abs(arguments, env, call_pos),
        Builtin::Modulo => eval_modulo(arguments, env, call_pos),
        Builtin::Remainder => eval_remainder(arguments, env, call_pos),
        Builtin::Quotient => eval_quotient(arguments, env, call_pos),
        Builtin::Min => eval_min(arguments, env, call_pos),
        Builtin::Max => eval_max(arguments, env, call_pos),
        Builtin::Expt => eval_expt(arguments, env, call_pos),
        Builtin::ZeroPred => eval_number_predicate(arguments, env, "zero?", call_pos, |value| {
            value == 0
        }),
        Builtin::PositivePred => {
            eval_number_predicate(arguments, env, "positive?", call_pos, |value| value > 0)
        }
        Builtin::NegativePred => {
            eval_number_predicate(arguments, env, "negative?", call_pos, |value| value < 0)
        }
        Builtin::OddPred => eval_number_predicate(arguments, env, "odd?", call_pos, |value| {
            value % 2 != 0
        }),
        Builtin::EvenPred => eval_number_predicate(arguments, env, "even?", call_pos, |value| {
            value % 2 == 0
        }),
        Builtin::Cons => eval_cons(arguments, env, call_pos),
        Builtin::Car => eval_car(arguments, env, call_pos),
        Builtin::Cdr => eval_cdr(arguments, env, call_pos),
        Builtin::NullPred => eval_null_pred(arguments, env, call_pos),
        Builtin::List => eval_list_builtin(arguments, env),
        Builtin::ListRef => eval_list_ref(arguments, env, call_pos),
        Builtin::ListTail => eval_list_tail(arguments, env, call_pos),
        Builtin::ListPred => eval_type_predicate(arguments, env, "list?", call_pos, |value| {
            matches!(value, Value::List(_))
        }),
        Builtin::Length => eval_length(arguments, env, call_pos),
        Builtin::Append => eval_append(arguments, env),
        Builtin::Assoc => eval_assoc(arguments, env, call_pos),
        Builtin::Map => eval_map(arguments, env, call_pos),
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
            is_pair,
        ),
        Builtin::SymbolPred => eval_type_predicate(arguments, env, "symbol?", call_pos, |value| {
            matches!(value, Value::Symbol(_))
        }),
        Builtin::Display => eval_display(arguments, env, call_pos),
        Builtin::Write => eval_write(arguments, env, call_pos),
        Builtin::Newline => eval_newline(arguments, env, call_pos),
        Builtin::StringCopy => eval_string_copy(arguments, env, call_pos),
        Builtin::StringAppend => eval_string_append(arguments, env),
        Builtin::StringLength => eval_string_length(arguments, env, call_pos),
        Builtin::StringSet => eval_string_set(arguments, env, call_pos),
        Builtin::Substring => eval_substring(arguments, env, call_pos),
        Builtin::StringToNumber => eval_string_to_number(arguments, env, call_pos),
        Builtin::NumberToString => eval_number_to_string(arguments, env, call_pos),
        Builtin::SymbolToString => eval_symbol_to_string(arguments, env, call_pos),
        Builtin::StringToSymbol => eval_string_to_symbol(arguments, env, call_pos),
        Builtin::StringRef => eval_string_ref(arguments, env, call_pos),
        Builtin::CharPred => eval_type_predicate(arguments, env, "char?", call_pos, |value| {
            matches!(value, Value::Char(_))
        }),
        Builtin::CharAlphabeticPred => {
            eval_char_predicate(arguments, env, "char-alphabetic?", call_pos, |ch| {
                ch.is_alphabetic()
            })
        }
        Builtin::CharNumericPred => {
            eval_char_predicate(arguments, env, "char-numeric?", call_pos, |ch| ch.is_numeric())
        }
        Builtin::CharUpcase => eval_char_transform(arguments, env, "char-upcase", call_pos, |ch| {
            ch.to_uppercase().next().unwrap_or(ch)
        }),
        Builtin::CharDowncase => {
            eval_char_transform(arguments, env, "char-downcase", call_pos, |ch| {
                ch.to_lowercase().next().unwrap_or(ch)
            })
        }
        Builtin::CharEqual => {
            eval_char_compare("char=?", arguments, env, call_pos, |left, right| left == right)
        }
        Builtin::CharLess => {
            eval_char_compare("char<?", arguments, env, call_pos, |left, right| left < right)
        }
        Builtin::StringEqual => {
            eval_string_compare("string=?", arguments, env, call_pos, |left, right| left == right)
        }
        Builtin::StringLess => {
            eval_string_compare("string<?", arguments, env, call_pos, |left, right| left < right)
        }
        Builtin::StringCiEqual => {
            eval_string_compare("string-ci=?", arguments, env, call_pos, |left, right| {
                left.to_lowercase() == right.to_lowercase()
            })
        }
        Builtin::StringUpcase => {
            eval_string_transform(arguments, env, "string-upcase", call_pos, |value| {
                value.chars().flat_map(|ch| ch.to_uppercase()).collect()
            })
        }
        Builtin::StringDowncase => {
            eval_string_transform(arguments, env, "string-downcase", call_pos, |value| {
                value.chars().flat_map(|ch| ch.to_lowercase()).collect()
            })
        }
        Builtin::Apply => eval_apply_builtin(arguments, env, call_pos),
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
    let required = closure.params.required.len();
    let has_rest = closure.params.rest.is_some();

    if (!has_rest && argument_values.len() != required)
        || (has_rest && argument_values.len() < required)
    {
        return Err(EvalError::WrongArgCount {
            name: "procedure".into(),
            expected: if has_rest {
                format!("at least {required}")
            } else {
                format!("exactly {required}")
            },
            got: argument_values.len(),
        }
        .with_offset(call_pos.offset));
    }

    let call_env = Env::child(&closure.env);
    let mut argument_values = argument_values.into_iter();

    for param in &closure.params.required {
        let value = argument_values
            .next()
            .expect("required argument count already validated");
        call_env.define(param.clone(), value);
    }

    if let Some(rest_param) = &closure.params.rest {
        call_env.define(rest_param.clone(), Value::List(argument_values.collect()));
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
        return Err(EvalError::WrongArgCount {
            name: "define".into(),
            expected: "at least 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    }

    match &arguments[0].kind {
        ExprKind::Symbol(name) => {
            if arguments.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define".into(),
                    expected: "exactly 2".into(),
                    got: arguments.len(),
                }
                .with_offset(pos.offset));
            }

            let value = eval_expr(&arguments[1], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let (name_expr, params) = signature.split_first().ok_or_else(|| {
                EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                }
                .with_offset(arguments[0].pos.offset)
            })?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                }
                .with_offset(name_expr.pos.offset));
            };

            let closure = Value::Procedure(Rc::new(Closure {
                params: parse_params(params)?,
                body: arguments[1..].to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidSyntax {
            message: "define: expected symbol or function signature".into(),
        }
        .with_offset(arguments[0].pos.offset)),
    }
}

fn eval_set(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [name_expr, value_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "set!".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "set!: expected symbol".into(),
        }
        .with_offset(name_expr.pos.offset));
    };

    let value = eval_expr(value_expr, env)?;
    if env.set(name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundVariable { name: name.clone() }.with_offset(name_expr.pos.offset))
    }
}

fn eval_if(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    if eval_expr(condition, env)?.is_truthy() {
        eval_expr(consequent, env)
    } else {
        eval_expr(alternate, env)
    }
}

fn eval_quote(pos: SourcePos, arguments: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(pos.offset));
    };

    Ok(quote_expr(quoted))
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(value) => Value::Integer(*value),
        ExprKind::Boolean(value) => Value::Boolean(*value),
        ExprKind::String(value) => Value::String(SchemeString::immutable(value.clone())),
        ExprKind::Char(value) => Value::Char(*value),
        ExprKind::Symbol(name) => Value::Symbol(name.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(pos: SourcePos, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (param_list, body) = arguments.split_first().ok_or_else(|| {
        EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset)
    })?;

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(pos.offset));
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
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(pos.offset));
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let(name, rest, env, pos),
        _ => eval_plain_let(first, rest, env),
    }
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(bindings_expr.pos.offset));
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
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 1,
        }
        .with_offset(pos.offset));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 3".into(),
            got: 2,
        }
        .with_offset(pos.offset));
    }

    let bindings = parse_bindings(bindings_expr, "let")?;
    let named_env = Env::child(env);
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let closure = Rc::new(Closure {
        params: ParameterSpec {
            required: params,
            rest: None,
        },
        body: body.to_vec(),
        env: named_env.clone(),
    });

    named_env.define(name.into(), Value::Procedure(closure.clone()));

    let values = eval_binding_values(&bindings, &named_env)?;
    apply_closure_values(closure, values, pos)
}

fn parse_bindings(bindings_expr: &Expr, form_name: &str) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: format!("{form_name}: expected binding list"),
        }
        .with_offset(bindings_expr.pos.offset));
    };

    bindings
        .iter()
        .map(|binding| match &binding.kind {
            ExprKind::List(parts) if parts.len() == 2 => match &parts[0].kind {
                ExprKind::Symbol(name) => Ok((name.clone(), parts[1].clone())),
                _ => Err(EvalError::InvalidSyntax {
                    message: format!("{form_name}: binding name must be a symbol"),
                }
                .with_offset(parts[0].pos.offset)),
            },
            _ => Err(EvalError::InvalidSyntax {
                message: format!("{form_name}: each binding must have exactly 2 elements"),
            }
            .with_offset(binding.pos.offset)),
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
            return Err(EvalError::InvalidSyntax {
                message: "cond: clauses must be lists".into(),
            }
            .with_offset(clause.pos.offset));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidSyntax {
                message: "cond: clauses cannot be empty".into(),
            }
            .with_offset(clause.pos.offset));
        };

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != arguments.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond: else clause must be last".into(),
                }
                .with_offset(test.pos.offset));
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

fn parse_param_list(expr: &Expr) -> Result<ParameterSpec, EvalError> {
    match &expr.kind {
        ExprKind::List(params) => parse_params(params),
        ExprKind::Symbol(name) => Ok(ParameterSpec {
            required: Vec::new(),
            rest: Some(name.clone()),
        }),
        _ => Err(EvalError::InvalidSyntax {
            message: "lambda: expected parameter list".into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn parse_params(params: &[Expr]) -> Result<ParameterSpec, EvalError> {
    let mut required = Vec::new();
    let mut index = 0;

    while index < params.len() {
        match &params[index].kind {
            ExprKind::Symbol(name) if name == "." => {
                if index + 2 != params.len() {
                    return Err(EvalError::InvalidSyntax {
                        message: "lambda: invalid parameter list".into(),
                    }
                    .with_offset(params[index].pos.offset));
                }

                let rest_param = match &params[index + 1].kind {
                    ExprKind::Symbol(name) if name != "." => name.clone(),
                    _ => {
                        return Err(EvalError::InvalidSyntax {
                            message: "lambda: parameters must be symbols".into(),
                        }
                        .with_offset(params[index + 1].pos.offset))
                    }
                };

                return Ok(ParameterSpec {
                    required,
                    rest: Some(rest_param),
                });
            }
            ExprKind::Symbol(name) => required.push(name.clone()),
            _ => {
                return Err(EvalError::InvalidSyntax {
                    message: "lambda: parameters must be symbols".into(),
                }
                .with_offset(params[index].pos.offset))
            }
        }

        index += 1;
    }

    Ok(ParameterSpec {
        required,
        rest: None,
    })
}

fn eval_apply_builtin(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let Some((procedure_expr, rest_arguments)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if rest_arguments.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let procedure = eval_expr(procedure_expr, env)?;
    let (list_expr, prefix_exprs) = rest_arguments
        .split_last()
        .expect("rest arguments are known to be non-empty");
    let mut values = eval_args(prefix_exprs, env)?;

    match eval_expr(list_expr, env)? {
        Value::List(mut rest_values) => values.append(&mut rest_values),
        other => {
            return Err(EvalError::TypeMismatch {
                expected: "list".into(),
                found: other.kind().into(),
            }
            .with_offset(list_expr.pos.offset))
        }
    }

    apply_value_with_values(procedure, values, env, procedure_expr.pos)
}

fn apply_value_with_values(
    value: Value,
    argument_values: Vec<Value>,
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    match value {
        Value::Builtin(_) | Value::Procedure(_) => {
            let apply_env = Env::child(env);
            let procedure_name = "__apply_procedure".to_string();
            apply_env.define(procedure_name.clone(), value);

            let mut application = Vec::with_capacity(argument_values.len() + 1);
            application.push(Expr::symbol(procedure_name, call_pos));

            for (index, argument) in argument_values.into_iter().enumerate() {
                let arg_name = format!("__apply_arg_{index}");
                apply_env.define(arg_name.clone(), argument);
                application.push(Expr::symbol(arg_name, call_pos));
            }

            eval_application(call_pos, &application, &apply_env)
        }
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }
        .with_offset(call_pos.offset)),
    }
}

fn eval_add(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Integer(numbers.into_iter().sum()))
}

fn eval_eq(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [left_expr, right_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "eq?".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let left = eval_expr(left_expr, env)?;
    let right = eval_expr(right_expr, env)?;
    Ok(Value::Boolean(value_equal(&left, &right)))
}

fn eval_equal(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [left_expr, right_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "equal?".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let left = eval_expr(left_expr, env)?;
    let right = eval_expr(right_expr, env)?;
    Ok(Value::Boolean(value_equal(&left, &right)))
}

fn eval_sub(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .with_offset(call_pos.offset)),
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
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
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
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        }
        .with_offset(call_pos.offset));
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn eval_not(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    if arguments.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    }

    let value = eval_expr(&arguments[0], env)?;
    Ok(Value::Boolean(!value.is_truthy()))
}

fn eval_abs(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "abs".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_number(expr, env)?;
    let value = value
        .checked_abs()
        .ok_or_else(|| EvalError::InvalidArgument {
            message: "abs: integer overflow".into(),
        }
        .with_offset(expr.pos.offset))?;
    Ok(Value::Integer(value))
}

fn eval_modulo(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_division_operands("modulo", arguments, env, call_pos)?;
    let remainder = dividend
        .checked_rem(divisor)
        .ok_or_else(|| EvalError::InvalidArgument {
            message: "modulo: integer overflow".into(),
        }
        .with_offset(call_pos.offset))?;
    let value = if remainder != 0 && (remainder > 0) != (divisor > 0) {
        remainder + divisor
    } else {
        remainder
    };
    Ok(Value::Integer(value))
}

fn eval_remainder(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_division_operands("remainder", arguments, env, call_pos)?;
    let value = dividend
        .checked_rem(divisor)
        .ok_or_else(|| EvalError::InvalidArgument {
            message: "remainder: integer overflow".into(),
        }
        .with_offset(call_pos.offset))?;
    Ok(Value::Integer(value))
}

fn eval_quotient(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_division_operands("quotient", arguments, env, call_pos)?;
    let value = dividend
        .checked_div(divisor)
        .ok_or_else(|| EvalError::InvalidArgument {
            message: "quotient: integer overflow".into(),
        }
        .with_offset(call_pos.offset))?;
    Ok(Value::Integer(value))
}

fn eval_min(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    let value = numbers.into_iter().min().ok_or_else(|| {
        EvalError::WrongArgCount {
            name: "min".into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .with_offset(call_pos.offset)
    })?;
    Ok(Value::Integer(value))
}

fn eval_max(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    let value = numbers.into_iter().max().ok_or_else(|| {
        EvalError::WrongArgCount {
            name: "max".into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .with_offset(call_pos.offset)
    })?;
    Ok(Value::Integer(value))
}

fn eval_expt(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [base_expr, exponent_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "expt".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let base = eval_number(base_expr, env)?;
    let exponent = eval_number(exponent_expr, env)?;
    if exponent < 0 {
        return Err(EvalError::InvalidArgument {
            message: "expt: exponent must be non-negative".into(),
        }
        .with_offset(exponent_expr.pos.offset));
    }

    let value = base
        .checked_pow(exponent as u32)
        .ok_or_else(|| EvalError::InvalidArgument {
            message: "expt: integer overflow".into(),
        }
        .with_offset(call_pos.offset))?;
    Ok(Value::Integer(value))
}

fn eval_number_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(i64) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Boolean(predicate(eval_number(expr, env)?)))
}

fn eval_cons(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [head_expr, tail_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let head = eval_expr(head_expr, env)?;
    let tail = eval_expr(tail_expr, env)?;

    match tail {
        Value::List(mut items) => {
            items.insert(0, head);
            Ok(Value::List(items))
        }
        other => Ok(Value::Pair(Box::new(head), Box::new(other))),
    }
}

fn eval_car(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "car".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(head, _) => Ok(*head),
        Value::List(_) => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        }
        .with_offset(list_expr.pos.offset)),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: other.kind().into(),
        }
        .with_offset(list_expr.pos.offset)),
    }
}

fn eval_cdr(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "cdr".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(_, tail) => Ok(*tail),
        Value::List(_) => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: "list".into(),
        }
        .with_offset(list_expr.pos.offset)),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".into(),
            found: other.kind().into(),
        }
        .with_offset(list_expr.pos.offset)),
    }
}

fn eval_null_pred(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "null?".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(list_expr, env)?;
    Ok(Value::Boolean(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_list_builtin(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    Ok(Value::List(eval_args(arguments, env)?))
}

fn eval_list_ref(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "list-ref".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let items = eval_list_items(list_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let len = items.len();
    if index >= len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(items[index].clone())
}

fn eval_list_tail(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [list_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "list-tail".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let items = eval_list_items(list_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let len = items.len();
    if index > len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(Value::List(items[index..].to_vec()))
}

fn eval_length(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [list_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match eval_expr(list_expr, env)? {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeMismatch {
            expected: "list".into(),
            found: other.kind().into(),
        }
        .with_offset(list_expr.pos.offset)),
    }
}

fn eval_append(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        match value {
            Value::List(mut list_items) => items.append(&mut list_items),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    found: other.kind().into(),
                }
                .with_offset(argument.pos.offset));
            }
        }
    }

    Ok(Value::List(items))
}

fn eval_assoc(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [key_expr, alist_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "assoc".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let key = eval_expr(key_expr, env)?;
    let entries = eval_list_items(alist_expr, env)?;

    for entry in entries {
        if let Some(entry_key) = pair_head(&entry) {
            if value_equal(&key, entry_key) {
                return Ok(entry);
            }
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_map(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let Some((procedure_expr, list_exprs)) = arguments.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2".into(),
            got: 0,
        }
        .with_offset(call_pos.offset));
    };

    if list_exprs.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2".into(),
            got: 1,
        }
        .with_offset(call_pos.offset));
    }

    let procedure = eval_expr(procedure_expr, env)?;
    let lists: Vec<Vec<Value>> = list_exprs
        .iter()
        .map(|expr| eval_list_items(expr, env))
        .collect::<Result<_, _>>()?;

    let len = lists.first().map(Vec::len).unwrap_or(0);
    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            message: "map: all lists must have the same length".into(),
        }
        .with_offset(call_pos.offset));
    }

    let mut results = Vec::with_capacity(len);
    for index in 0..len {
        let row = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_value_with_values(
            procedure.clone(),
            row,
            env,
            procedure_expr.pos,
        )?);
    }

    Ok(Value::List(results))
}

fn eval_display(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "display".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(expr, env)?;
    env.write_output(&value.render_display());
    Ok(Value::Void)
}

fn eval_write(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "write".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_expr(expr, env)?;
    env.write_output(&value.render());
    Ok(Value::Void)
}

fn eval_newline(arguments: &[Expr], env: &EnvRef, call_pos: SourcePos) -> Result<Value, EvalError> {
    if !arguments.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline".into(),
            expected: "exactly 0".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    }

    env.write_output("\n");
    Ok(Value::Void)
}

fn eval_string_append(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut combined = String::new();

    for argument in arguments {
        combined.push_str(&eval_string(argument, env)?);
    }

    Ok(Value::String(SchemeString::immutable(combined)))
}

fn eval_string_length(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-length".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Integer(
        eval_string(expr, env)?.chars().count() as i64
    ))
}

fn eval_string_copy(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-copy".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(eval_string_value(expr, env)?.mutable_copy()))
}

fn eval_string_set(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr, index_expr, char_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-set!".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let string = eval_string_value(string_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let ch = eval_char(char_expr, env)?;

    match string.set_char(index, ch) {
        Ok(()) => Ok(Value::Void),
        Err(StringSetError::Immutable) => {
            Err(EvalError::ImmutableString.with_offset(string_expr.pos.offset))
        }
        Err(StringSetError::IndexOutOfBounds { len }) => {
            Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset))
        }
    }
}

fn eval_substring(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr, start_expr, end_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "substring".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_string(string_expr, env)?;
    let start = eval_index(start_expr, env)?;
    let end = eval_index(end_expr, env)?;
    let chars: Vec<char> = value.chars().collect();
    let len = chars.len();

    if start > len {
        return Err(
            EvalError::IndexOutOfBounds { index: start, len }.with_offset(start_expr.pos.offset)
        );
    }

    if end > len {
        return Err(
            EvalError::IndexOutOfBounds { index: end, len }.with_offset(end_expr.pos.offset)
        );
    }

    if start > end {
        return Err(EvalError::InvalidRange { start, end, len }.with_offset(start_expr.pos.offset));
    }

    Ok(Value::String(SchemeString::immutable(
        chars[start..end].iter().collect::<String>(),
    )))
}

fn eval_string_to_number(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string->number".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    match eval_string(expr, env)?.parse::<i64>() {
        Ok(value) => Ok(Value::Integer(value)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "number->string".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(SchemeString::immutable(
        eval_number(expr, env)?.to_string(),
    )))
}

fn eval_symbol_to_string(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(SchemeString::immutable(eval_symbol(
        expr, env,
    )?)))
}

fn eval_string_to_symbol(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Symbol(eval_string(expr, env)?))
}

fn eval_string_ref(
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<Value, EvalError> {
    let [string_expr, index_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "string-ref".into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let value = eval_string(string_expr, env)?;
    let index = eval_index(index_expr, env)?;
    let chars: Vec<char> = value.chars().collect();
    let len = chars.len();

    if index >= len {
        return Err(EvalError::IndexOutOfBounds { index, len }.with_offset(index_expr.pos.offset));
    }

    Ok(Value::Char(chars[index]))
}

fn eval_char_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(char) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Boolean(predicate(eval_char(expr, env)?)))
}

fn eval_char_transform(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    transform: impl Fn(char) -> char,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::Char(transform(eval_char(expr, env)?)))
}

fn eval_char_compare(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let chars = arguments
        .iter()
        .map(|argument| eval_char(argument, env))
        .collect::<Result<Vec<_>, _>>()?;

    if chars.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: chars.len(),
        }
        .with_offset(call_pos.offset));
    }

    Ok(Value::Boolean(chars.windows(2).all(|pair| predicate(pair[0], pair[1]))))
}

fn eval_string_compare(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let strings = arguments
        .iter()
        .map(|argument| eval_string(argument, env))
        .collect::<Result<Vec<_>, _>>()?;

    if strings.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: strings.len(),
        }
        .with_offset(call_pos.offset));
    }

    Ok(Value::Boolean(
        strings.windows(2).all(|pair| predicate(&pair[0], &pair[1])),
    ))
}

fn eval_string_transform(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    transform: impl Fn(String) -> String,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    Ok(Value::String(SchemeString::immutable(transform(eval_string(
        expr, env,
    )?))))
}

fn eval_type_predicate(
    arguments: &[Expr],
    env: &EnvRef,
    name: &str,
    call_pos: SourcePos,
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
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

fn eval_division_operands(
    name: &str,
    arguments: &[Expr],
    env: &EnvRef,
    call_pos: SourcePos,
) -> Result<(i64, i64), EvalError> {
    let [dividend_expr, divisor_expr] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 2".into(),
            got: arguments.len(),
        }
        .with_offset(call_pos.offset));
    };

    let dividend = eval_number(dividend_expr, env)?;
    let divisor = eval_number(divisor_expr, env)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero.with_offset(divisor_expr.pos.offset));
    }

    Ok((dividend, divisor))
}

fn eval_number(expr: &Expr, env: &EnvRef) -> Result<i64, EvalError> {
    match eval_expr(expr, env)? {
        Value::Integer(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_string(expr: &Expr, env: &EnvRef) -> Result<String, EvalError> {
    match eval_expr(expr, env)? {
        Value::String(value) => Ok(value.as_string()),
        other => Err(EvalError::TypeMismatch {
            expected: "string".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_string_value(expr: &Expr, env: &EnvRef) -> Result<SchemeString, EvalError> {
    match eval_expr(expr, env)? {
        Value::String(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "string".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_symbol(expr: &Expr, env: &EnvRef) -> Result<String, EvalError> {
    match eval_expr(expr, env)? {
        Value::Symbol(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "symbol".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_char(expr: &Expr, env: &EnvRef) -> Result<char, EvalError> {
    match eval_expr(expr, env)? {
        Value::Char(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "char".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn eval_index(expr: &Expr, env: &EnvRef) -> Result<usize, EvalError> {
    let index = eval_number(expr, env)?;
    if index < 0 {
        Err(EvalError::NegativeIndex { index }.with_offset(expr.pos.offset))
    } else {
        Ok(index as usize)
    }
}

fn eval_list_items(expr: &Expr, env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    match eval_expr(expr, env)? {
        Value::List(items) => Ok(items),
        other => Err(EvalError::TypeMismatch {
            expected: "list".into(),
            found: other.kind().into(),
        }
        .with_offset(expr.pos.offset)),
    }
}

fn is_pair(value: &Value) -> bool {
    matches!(value, Value::List(items) if !items.is_empty()) || matches!(value, Value::Pair(_, _))
}

fn pair_head(value: &Value) -> Option<&Value> {
    match value {
        Value::List(items) if !items.is_empty() => Some(&items[0]),
        Value::Pair(head, _) => Some(head.as_ref()),
        _ => None,
    }
}

fn value_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.as_string() == right.as_string(),
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left_item, right_item)| value_equal(left_item, right_item))
        }
        (Value::Pair(left_head, left_tail), Value::Pair(right_head, right_tail)) => {
            value_equal(left_head, right_head) && value_equal(left_tail, right_tail)
        }
        (Value::Builtin(left), Value::Builtin(right)) => left.name() == right.name(),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
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
    let output = Rc::new(RefCell::new(String::new()));
    let value = eval_program(&expressions, output).map_err(|err| err.resolve_positions(input))?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let expressions = Parser::new(input)
        .parse_program()
        .map_err(|err| err.resolve_positions(input))?;
    let output = Rc::new(RefCell::new(String::new()));
    let value =
        eval_program(&expressions, output.clone()).map_err(|err| err.resolve_positions(input))?;
    let captured_output = output.borrow().clone();
    Ok((value.render(), captured_output))
}

#[cfg(test)]
mod tests;
