pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EvalResult<T> = Result<T, EvalError>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut interpreter = Interpreter::new();
    let value = interpreter.eval_program(input)?;
    Ok(format_value(&value, false))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut interpreter = Interpreter::new();
    let value = interpreter.eval_program(input)?;
    Ok((format_value(&value, false), interpreter.output))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SourcePos {
    line: usize,
    column: usize,
}

#[derive(Clone, Debug)]
enum Expr {
    Number(i64, SourcePos),
    Bool(bool, SourcePos),
    String(String, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Expr::Number(_, pos)
            | Expr::Bool(_, pos)
            | Expr::String(_, pos)
            | Expr::Symbol(_, pos)
            | Expr::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone)]
enum Value {
    Number(i64),
    Bool(bool),
    String(Rc<String>),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Builtin(BuiltinKind),
    Closure(Rc<Closure>),
    Void,
    Uninitialized,
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Clone)]
struct Closure {
    params: ParamSpec,
    body: Vec<Expr>,
    env: Rc<Env>,
}

#[derive(Clone)]
struct ParamSpec {
    required: Vec<String>,
    rest: Option<String>,
}

impl ParamSpec {
    fn matches(&self, arg_count: usize) -> bool {
        match self.rest {
            Some(_) => arg_count >= self.required.len(),
            None => arg_count == self.required.len(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BuiltinKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equals,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    PairPred,
    List,
    Length,
    Append,
    Reverse,
    Map,
    EqPred,
    EqvPred,
    EqualPred,
    ZeroPred,
    NumberPred,
    SymbolPred,
    BooleanPred,
    StringPred,
    ProcedurePred,
    Quotient,
    Remainder,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
    SymbolToString,
    StringToSymbol,
    Display,
    Write,
    Newline,
    Error,
}

impl BuiltinKind {
    fn name(self) -> &'static str {
        match self {
            BuiltinKind::Add => "+",
            BuiltinKind::Subtract => "-",
            BuiltinKind::Multiply => "*",
            BuiltinKind::Divide => "/",
            BuiltinKind::LessThan => "<",
            BuiltinKind::GreaterThan => ">",
            BuiltinKind::Equals => "=",
            BuiltinKind::LessEqual => "<=",
            BuiltinKind::Not => "not",
            BuiltinKind::Cons => "cons",
            BuiltinKind::Car => "car",
            BuiltinKind::Cdr => "cdr",
            BuiltinKind::NullPred => "null?",
            BuiltinKind::PairPred => "pair?",
            BuiltinKind::List => "list",
            BuiltinKind::Length => "length",
            BuiltinKind::Append => "append",
            BuiltinKind::Reverse => "reverse",
            BuiltinKind::Map => "map",
            BuiltinKind::EqPred => "eq?",
            BuiltinKind::EqvPred => "eqv?",
            BuiltinKind::EqualPred => "equal?",
            BuiltinKind::ZeroPred => "zero?",
            BuiltinKind::NumberPred => "number?",
            BuiltinKind::SymbolPred => "symbol?",
            BuiltinKind::BooleanPred => "boolean?",
            BuiltinKind::StringPred => "string?",
            BuiltinKind::ProcedurePred => "procedure?",
            BuiltinKind::Quotient => "quotient",
            BuiltinKind::Remainder => "remainder",
            BuiltinKind::Vector => "vector",
            BuiltinKind::MakeVector => "make-vector",
            BuiltinKind::VectorRef => "vector-ref",
            BuiltinKind::VectorSet => "vector-set!",
            BuiltinKind::VectorLength => "vector-length",
            BuiltinKind::VectorPred => "vector?",
            BuiltinKind::VectorToList => "vector->list",
            BuiltinKind::ListToVector => "list->vector",
            BuiltinKind::SymbolToString => "symbol->string",
            BuiltinKind::StringToSymbol => "string->symbol",
            BuiltinKind::Display => "display",
            BuiltinKind::Write => "write",
            BuiltinKind::Newline => "newline",
            BuiltinKind::Error => "error",
        }
    }
}

struct Env {
    parent: Option<Rc<Env>>,
    bindings: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
}

impl Env {
    fn new(parent: Option<Rc<Env>>) -> Rc<Self> {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) -> Rc<RefCell<Value>> {
        let cell = Rc::new(RefCell::new(value));
        self.bindings.borrow_mut().insert(name.into(), cell.clone());
        cell
    }

    fn define_cell(&self, name: impl Into<String>, cell: Rc<RefCell<Value>>) {
        self.bindings.borrow_mut().insert(name.into(), cell);
    }

    fn lookup(&self, name: &str, pos: SourcePos) -> EvalResult<Value> {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            let value = cell.borrow().clone();
            if matches!(value, Value::Uninitialized) {
                return Err(error(format!("uninitialized variable: {name}"), pos));
            }
            return Ok(value);
        }
        if let Some(parent) = &self.parent {
            return parent.lookup(name, pos);
        }
        Err(error(format!("unbound variable: {name}"), pos))
    }

    fn assign(&self, name: &str, value: Value, pos: SourcePos) -> EvalResult<()> {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            *cell.borrow_mut() = value;
            return Ok(());
        }
        if let Some(parent) = &self.parent {
            return parent.assign(name, value, pos);
        }
        Err(error(format!("unbound variable: {name}"), pos))
    }
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(&mut self) -> EvalResult<Vec<Expr>> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> EvalResult<Expr> {
        self.skip_ignored();
        let pos = self.current_pos();
        match self.peek_char() {
            None => Err(error("unexpected end of input", pos)),
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash_literal(),
            Some(')') => Err(error("unexpected ')'", pos)),
            Some(_) => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> EvalResult<Expr> {
        let pos = self.current_pos();
        self.advance_char();
        let mut elements = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            if self.peek_char() == Some(')') {
                self.advance_char();
                return Ok(Expr::List(elements, pos));
            }
            elements.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Err(error("unterminated list", pos))
    }

    fn parse_quote(&mut self) -> EvalResult<Expr> {
        let pos = self.current_pos();
        self.advance_char();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".to_string(), pos), quoted],
            pos,
        ))
    }

    fn parse_string(&mut self) -> EvalResult<Expr> {
        let pos = self.current_pos();
        self.advance_char();
        let mut value = String::new();
        while let Some(ch) = self.peek_char() {
            match ch {
                '"' => {
                    self.advance_char();
                    return Ok(Expr::String(value, pos));
                }
                '\\' => {
                    self.advance_char();
                    let escaped = match self.advance_char() {
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some(other) => {
                            return Err(error(
                                format!("unsupported string escape: \\{other}"),
                                pos,
                            ))
                        }
                        None => return Err(error("unterminated string escape", pos)),
                    };
                    value.push(escaped);
                }
                other => {
                    self.advance_char();
                    value.push(other);
                }
            }
        }
        Err(error("unterminated string", pos))
    }

    fn parse_hash_literal(&mut self) -> EvalResult<Expr> {
        let pos = self.current_pos();
        self.advance_char();
        match self.advance_char() {
            Some('t') => Ok(Expr::Bool(true, pos)),
            Some('f') => Ok(Expr::Bool(false, pos)),
            Some(other) => Err(error(format!("unknown hash literal '#{other}'"), pos)),
            None => Err(error("incomplete hash literal", pos)),
        }
    }

    fn parse_atom(&mut self) -> EvalResult<Expr> {
        let pos = self.current_pos();
        let token = self.read_token();
        if token.is_empty() {
            return Err(error("unexpected token", pos));
        }
        if looks_like_number(&token) {
            return token
                .parse::<i64>()
                .map(|value| Expr::Number(value, pos))
                .map_err(|_| error(format!("invalid number: {token}"), pos));
        }
        Ok(Expr::Symbol(token, pos))
    }

    fn read_token(&mut self) -> String {
        let start = self.index;
        while let Some(ch) = self.peek_char() {
            if is_delimiter(ch) {
                break;
            }
            self.advance_char();
        }
        self.input[start..self.index].to_string()
    }

    fn skip_ignored(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.advance_char();
                continue;
            }
            if ch == ';' {
                self.skip_comment();
                continue;
            }
            break;
        }
    }

    fn skip_comment(&mut self) {
        while let Some(ch) = self.peek_char() {
            self.advance_char();
            if ch == '\n' {
                break;
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            column: self.column,
        }
    }
}

struct Interpreter {
    output: String,
    global: Rc<Env>,
}

impl Interpreter {
    fn new() -> Self {
        let global = Env::new(None);
        let interpreter = Self {
            output: String::new(),
            global,
        };
        interpreter.install_builtins();
        interpreter
    }

    fn install_builtins(&self) {
        self.install_builtin("+", BuiltinKind::Add);
        self.install_builtin("-", BuiltinKind::Subtract);
        self.install_builtin("*", BuiltinKind::Multiply);
        self.install_builtin("/", BuiltinKind::Divide);
        self.install_builtin("<", BuiltinKind::LessThan);
        self.install_builtin(">", BuiltinKind::GreaterThan);
        self.install_builtin("=", BuiltinKind::Equals);
        self.install_builtin("<=", BuiltinKind::LessEqual);
        self.install_builtin("not", BuiltinKind::Not);
        self.install_builtin("cons", BuiltinKind::Cons);
        self.install_builtin("car", BuiltinKind::Car);
        self.install_builtin("cdr", BuiltinKind::Cdr);
        self.install_builtin("null?", BuiltinKind::NullPred);
        self.install_builtin("pair?", BuiltinKind::PairPred);
        self.install_builtin("list", BuiltinKind::List);
        self.install_builtin("length", BuiltinKind::Length);
        self.install_builtin("append", BuiltinKind::Append);
        self.install_builtin("reverse", BuiltinKind::Reverse);
        self.install_builtin("map", BuiltinKind::Map);
        self.install_builtin("eq?", BuiltinKind::EqPred);
        self.install_builtin("eqv?", BuiltinKind::EqvPred);
        self.install_builtin("equal?", BuiltinKind::EqualPred);
        self.install_builtin("zero?", BuiltinKind::ZeroPred);
        self.install_builtin("number?", BuiltinKind::NumberPred);
        self.install_builtin("symbol?", BuiltinKind::SymbolPred);
        self.install_builtin("boolean?", BuiltinKind::BooleanPred);
        self.install_builtin("string?", BuiltinKind::StringPred);
        self.install_builtin("procedure?", BuiltinKind::ProcedurePred);
        self.install_builtin("quotient", BuiltinKind::Quotient);
        self.install_builtin("remainder", BuiltinKind::Remainder);
        self.install_builtin("vector", BuiltinKind::Vector);
        self.install_builtin("make-vector", BuiltinKind::MakeVector);
        self.install_builtin("vector-ref", BuiltinKind::VectorRef);
        self.install_builtin("vector-set!", BuiltinKind::VectorSet);
        self.install_builtin("vector-length", BuiltinKind::VectorLength);
        self.install_builtin("vector?", BuiltinKind::VectorPred);
        self.install_builtin("vector->list", BuiltinKind::VectorToList);
        self.install_builtin("list->vector", BuiltinKind::ListToVector);
        self.install_builtin("symbol->string", BuiltinKind::SymbolToString);
        self.install_builtin("string->symbol", BuiltinKind::StringToSymbol);
        self.install_builtin("display", BuiltinKind::Display);
        self.install_builtin("write", BuiltinKind::Write);
        self.install_builtin("newline", BuiltinKind::Newline);
        self.install_builtin("error", BuiltinKind::Error);
    }

    fn install_builtin(&self, name: &'static str, kind: BuiltinKind) {
        self.global.define(name, Value::Builtin(kind));
    }

    fn eval_program(&mut self, input: &str) -> EvalResult<Value> {
        let mut parser = Parser::new(input);
        let expressions = parser.parse_program()?;
        if expressions.is_empty() {
            return Err(error("empty input", SourcePos { line: 1, column: 1 }));
        }
        let mut result = Value::Void;
        for expression in &expressions {
            result = self.eval_expr(expression, &self.global.clone())?;
        }
        Ok(result)
    }

    fn eval_expr(&mut self, expr: &Expr, env: &Rc<Env>) -> EvalResult<Value> {
        match expr {
            Expr::Number(value, _) => Ok(Value::Number(*value)),
            Expr::Bool(value, _) => Ok(Value::Bool(*value)),
            Expr::String(value, _) => Ok(Value::String(Rc::new(value.clone()))),
            Expr::Symbol(name, pos) => env.lookup(name, *pos),
            Expr::List(elements, pos) => self.eval_list(elements, *pos, env),
        }
    }

    fn eval_list(&mut self, elements: &[Expr], pos: SourcePos, env: &Rc<Env>) -> EvalResult<Value> {
        if elements.is_empty() {
            return Err(error("cannot evaluate empty list", pos));
        }

        if let Expr::Symbol(symbol, symbol_pos) = &elements[0] {
            let args = &elements[1..];
            match symbol.as_str() {
                "define" => return self.eval_define(args, env, *symbol_pos),
                "set!" => return self.eval_set(args, env, *symbol_pos),
                "if" => return self.eval_if(args, env, *symbol_pos),
                "quote" => return self.eval_quote(args, *symbol_pos),
                "lambda" => return self.eval_lambda(args, env, *symbol_pos),
                "begin" => return self.eval_sequence(args, env),
                "cond" => return self.eval_cond(args, env, *symbol_pos),
                "let" => return self.eval_let(args, env, *symbol_pos),
                "letrec" => return self.eval_letrec(args, env, *symbol_pos, false),
                "letrec*" => return self.eval_letrec(args, env, *symbol_pos, true),
                "and" => return self.eval_and(args, env),
                "or" => return self.eval_or(args, env),
                "case" => return self.eval_case(args, env, *symbol_pos),
                "do" => return self.eval_do(args, env, *symbol_pos),
                _ => {}
            }
        }

        let procedure = self.eval_expr(&elements[0], env)?;
        let mut arguments = Vec::with_capacity(elements.len().saturating_sub(1));
        for arg in &elements[1..] {
            arguments.push(self.eval_expr(arg, env)?);
        }
        self.apply_procedure(procedure, &arguments, pos)
    }

    fn eval_define(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if args.is_empty() {
            return Err(error("'define' expects a target and a value", pos));
        }

        match &args[0] {
            Expr::Symbol(name, _) => {
                if args.len() != 2 {
                    return Err(error("'define' expects exactly 2 arguments", pos));
                }
                let value = self.eval_expr(&args[1], env)?;
                env.define(name.clone(), value);
                Ok(Value::Void)
            }
            Expr::List(signature, signature_pos) => {
                if signature.is_empty() {
                    return Err(error("function definition requires a name", *signature_pos));
                }
                let name = match &signature[0] {
                    Expr::Symbol(name, _) => name.clone(),
                    _ => return Err(error("function definition requires a symbol name", *signature_pos)),
                };
                if args.len() < 2 {
                    return Err(error("function definition requires a body", pos));
                }
                let params = parse_param_list(&signature[1..], *signature_pos)?;
                let closure = Value::Closure(Rc::new(Closure {
                    params,
                    body: args[1..].to_vec(),
                    env: env.clone(),
                }));
                env.define(name, closure);
                Ok(Value::Void)
            }
            other => Err(error(
                "'define' target must be a symbol or parameter list",
                other.pos(),
            )),
        }
    }

    fn eval_set(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if args.len() != 2 {
            return Err(error("'set!' expects exactly 2 arguments", pos));
        }
        let name = match &args[0] {
            Expr::Symbol(name, _) => name.clone(),
            other => return Err(error("'set!' target must be a symbol", other.pos())),
        };
        let value = self.eval_expr(&args[1], env)?;
        env.assign(&name, value, args[0].pos())?;
        Ok(Value::Void)
    }

    fn eval_if(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if !(2..=3).contains(&args.len()) {
            return Err(error("'if' expects 2 or 3 arguments", pos));
        }
        let condition = self.eval_expr(&args[0], env)?;
        if is_truthy(&condition) {
            return self.eval_expr(&args[1], env);
        }
        if args.len() == 3 {
            return self.eval_expr(&args[2], env);
        }
        Ok(Value::Void)
    }

    fn eval_quote(&mut self, args: &[Expr], pos: SourcePos) -> EvalResult<Value> {
        if args.len() != 1 {
            return Err(error("'quote' expects exactly 1 argument", pos));
        }
        quote_to_value(&args[0])
    }

    fn eval_lambda(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if args.len() < 2 {
            return Err(error("'lambda' expects a parameter list and a body", pos));
        }
        let params = parse_params(&args[0])?;
        Ok(Value::Closure(Rc::new(Closure {
            params,
            body: args[1..].to_vec(),
            env: env.clone(),
        })))
    }

    fn eval_cond(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        for (index, clause) in args.iter().enumerate() {
            let parts = match clause {
                Expr::List(parts, _) if !parts.is_empty() => parts,
                _ => return Err(error("'cond' clauses must be non-empty lists", pos)),
            };
            if let Expr::Symbol(name, else_pos) = &parts[0] {
                if name == "else" {
                    if index != args.len() - 1 {
                        return Err(error("'cond' else clause must be last", *else_pos));
                    }
                    return self.eval_sequence(&parts[1..], env);
                }
            }
            let test = self.eval_expr(&parts[0], env)?;
            if is_truthy(&test) {
                if parts.len() == 1 {
                    return Ok(test);
                }
                return self.eval_sequence(&parts[1..], env);
            }
        }
        Ok(Value::Void)
    }

    fn eval_let(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if args.len() < 2 {
            return Err(error("'let' expects bindings and a body", pos));
        }
        let bindings = parse_bindings(&args[0], "'let'")?;
        let let_env = Env::new(Some(env.clone()));
        for binding in &bindings {
            let value = self.eval_expr(&binding.initializer, env)?;
            let_env.define(binding.name.clone(), value);
        }
        self.eval_sequence(&args[1..], &let_env)
    }

    fn eval_letrec(
        &mut self,
        args: &[Expr],
        env: &Rc<Env>,
        pos: SourcePos,
        sequential: bool,
    ) -> EvalResult<Value> {
        if args.len() < 2 {
            return Err(error("'letrec' expects bindings and a body", pos));
        }
        let bindings = parse_bindings(&args[0], if sequential { "'letrec*'" } else { "'letrec'" })?;
        let letrec_env = Env::new(Some(env.clone()));
        let mut cells = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let cell = Rc::new(RefCell::new(Value::Uninitialized));
            letrec_env.define_cell(binding.name.clone(), cell.clone());
            cells.push(cell);
        }

        if sequential {
            for (binding, cell) in bindings.iter().zip(&cells) {
                let value = self.eval_expr(&binding.initializer, &letrec_env)?;
                *cell.borrow_mut() = value;
            }
        } else {
            let mut values = Vec::with_capacity(bindings.len());
            for binding in &bindings {
                values.push(self.eval_expr(&binding.initializer, &letrec_env)?);
            }
            for (cell, value) in cells.into_iter().zip(values) {
                *cell.borrow_mut() = value;
            }
        }

        self.eval_sequence(&args[1..], &letrec_env)
    }

    fn eval_and(&mut self, args: &[Expr], env: &Rc<Env>) -> EvalResult<Value> {
        let mut result = Value::Bool(true);
        for arg in args {
            result = self.eval_expr(arg, env)?;
            if !is_truthy(&result) {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, args: &[Expr], env: &Rc<Env>) -> EvalResult<Value> {
        for arg in args {
            let result = self.eval_expr(arg, env)?;
            if is_truthy(&result) {
                return Ok(result);
            }
        }
        Ok(Value::Bool(false))
    }

    fn eval_case(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if args.len() < 2 {
            return Err(error("'case' expects a key and at least one clause", pos));
        }
        let key = self.eval_expr(&args[0], env)?;
        for (index, clause) in args[1..].iter().enumerate() {
            let parts = match clause {
                Expr::List(parts, _) if !parts.is_empty() => parts,
                _ => return Err(error("'case' clauses must be non-empty lists", pos)),
            };
            if let Expr::Symbol(name, else_pos) = &parts[0] {
                if name == "else" {
                    if index != args.len() - 2 {
                        return Err(error("'case' else clause must be last", *else_pos));
                    }
                    return self.eval_sequence(&parts[1..], env);
                }
            }

            let datums = match &parts[0] {
                Expr::List(datums, _) => datums,
                other => return Err(error("'case' datums must be in a list", other.pos())),
            };
            for datum in datums {
                let quoted = quote_to_value(datum)?;
                if eqv_values(&key, &quoted) {
                    return self.eval_sequence(&parts[1..], env);
                }
            }
        }
        Ok(Value::Void)
    }

    fn eval_do(&mut self, args: &[Expr], env: &Rc<Env>, pos: SourcePos) -> EvalResult<Value> {
        if args.len() < 2 {
            return Err(error("'do' expects bindings and a termination clause", pos));
        }
        let bindings = parse_do_bindings(&args[0])?;
        let (test_expr, result_exprs) = match &args[1] {
            Expr::List(parts, clause_pos) if !parts.is_empty() => {
                (parts[0].clone(), parts[1..].to_vec())
            }
            Expr::List(_, clause_pos) => {
                return Err(error("'do' termination clause must not be empty", *clause_pos))
            }
            other => return Err(error("'do' termination clause must be a list", other.pos())),
        };

        let loop_env = Env::new(Some(env.clone()));
        for binding in &bindings {
            let value = self.eval_expr(&binding.initializer, env)?;
            loop_env.define(binding.name.clone(), value);
        }

        loop {
            let test_value = self.eval_expr(&test_expr, &loop_env)?;
            if is_truthy(&test_value) {
                return self.eval_sequence(&result_exprs, &loop_env);
            }

            self.eval_sequence(&args[2..], &loop_env)?;

            let mut next_values = Vec::with_capacity(bindings.len());
            for binding in &bindings {
                let next = match &binding.step {
                    Some(step) => self.eval_expr(step, &loop_env)?,
                    None => loop_env.lookup(&binding.name, pos)?,
                };
                next_values.push(next);
            }
            for (binding, value) in bindings.iter().zip(next_values) {
                loop_env.assign(&binding.name, value, pos)?;
            }
        }
    }

    fn eval_sequence(&mut self, expressions: &[Expr], env: &Rc<Env>) -> EvalResult<Value> {
        let mut result = Value::Void;
        for expr in expressions {
            result = self.eval_expr(expr, env)?;
        }
        Ok(result)
    }

    fn apply_procedure(
        &mut self,
        procedure: Value,
        args: &[Value],
        pos: SourcePos,
    ) -> EvalResult<Value> {
        match procedure {
            Value::Builtin(kind) => self.apply_builtin(kind, args, pos),
            Value::Closure(closure) => {
                if !closure.params.matches(args.len()) {
                    return Err(error("wrong number of arguments", pos));
                }
                let call_env = Env::new(Some(closure.env.clone()));
                for (name, value) in closure.params.required.iter().zip(args.iter()) {
                    call_env.define(name.clone(), value.clone());
                }
                if let Some(rest) = &closure.params.rest {
                    call_env.define(rest.clone(), build_list(args[closure.params.required.len()..].to_vec()));
                }
                self.eval_sequence(&closure.body, &call_env)
            }
            _ => Err(error("not a procedure", pos)),
        }
    }

    fn apply_builtin(&mut self, kind: BuiltinKind, args: &[Value], pos: SourcePos) -> EvalResult<Value> {
        match kind {
            BuiltinKind::Add => {
                let mut total = 0_i64;
                for arg in args {
                    total += as_number(arg, kind.name(), pos)?;
                }
                Ok(Value::Number(total))
            }
            BuiltinKind::Subtract => {
                if args.is_empty() {
                    return Err(error("'-' expects at least 1 argument", pos));
                }
                let mut value = as_number(&args[0], "-", pos)?;
                if args.len() == 1 {
                    return Ok(Value::Number(-value));
                }
                for arg in &args[1..] {
                    value -= as_number(arg, "-", pos)?;
                }
                Ok(Value::Number(value))
            }
            BuiltinKind::Multiply => {
                let mut total = 1_i64;
                for arg in args {
                    total *= as_number(arg, "*", pos)?;
                }
                Ok(Value::Number(total))
            }
            BuiltinKind::Divide => {
                if args.len() < 2 {
                    return Err(error("'/' expects at least 2 arguments", pos));
                }
                let mut value = as_number(&args[0], "/", pos)?;
                for arg in &args[1..] {
                    let divisor = as_number(arg, "/", pos)?;
                    if divisor == 0 {
                        return Err(error("division by zero", pos));
                    }
                    value /= divisor;
                }
                Ok(Value::Number(value))
            }
            BuiltinKind::LessThan => compare_numbers(args, pos, "<", |a, b| a < b),
            BuiltinKind::GreaterThan => compare_numbers(args, pos, ">", |a, b| a > b),
            BuiltinKind::Equals => compare_numbers(args, pos, "=", |a, b| a == b),
            BuiltinKind::LessEqual => compare_numbers(args, pos, "<=", |a, b| a <= b),
            BuiltinKind::Not => {
                require_exact_arity(args, 1, "not", pos)?;
                Ok(Value::Bool(!is_truthy(&args[0])))
            }
            BuiltinKind::Cons => {
                require_exact_arity(args, 2, "cons", pos)?;
                Ok(make_pair(args[0].clone(), args[1].clone()))
            }
            BuiltinKind::Car => {
                require_exact_arity(args, 1, "car", pos)?;
                Ok(as_pair(&args[0], "car", pos)?.car.clone())
            }
            BuiltinKind::Cdr => {
                require_exact_arity(args, 1, "cdr", pos)?;
                Ok(as_pair(&args[0], "cdr", pos)?.cdr.clone())
            }
            BuiltinKind::NullPred => {
                require_exact_arity(args, 1, "null?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::EmptyList)))
            }
            BuiltinKind::PairPred => {
                require_exact_arity(args, 1, "pair?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::Pair(_))))
            }
            BuiltinKind::List => Ok(build_list(args.to_vec())),
            BuiltinKind::Length => {
                require_exact_arity(args, 1, "length", pos)?;
                Ok(Value::Number(collect_list(&args[0], "length", pos)?.len() as i64))
            }
            BuiltinKind::Append => {
                if args.is_empty() {
                    return Ok(Value::EmptyList);
                }
                let mut result = args[args.len() - 1].clone();
                for arg in args[..args.len() - 1].iter().rev() {
                    result = copy_list_onto(arg, result, "append", pos)?;
                }
                Ok(result)
            }
            BuiltinKind::Reverse => {
                require_exact_arity(args, 1, "reverse", pos)?;
                let mut items = collect_list(&args[0], "reverse", pos)?;
                items.reverse();
                Ok(build_list(items))
            }
            BuiltinKind::Map => self.apply_map(args, pos),
            BuiltinKind::EqPred => {
                require_exact_arity(args, 2, "eq?", pos)?;
                Ok(Value::Bool(eqv_values(&args[0], &args[1])))
            }
            BuiltinKind::EqvPred => {
                require_exact_arity(args, 2, "eqv?", pos)?;
                Ok(Value::Bool(eqv_values(&args[0], &args[1])))
            }
            BuiltinKind::EqualPred => {
                require_exact_arity(args, 2, "equal?", pos)?;
                Ok(Value::Bool(equal_values(&args[0], &args[1])))
            }
            BuiltinKind::ZeroPred => {
                require_exact_arity(args, 1, "zero?", pos)?;
                Ok(Value::Bool(as_number(&args[0], "zero?", pos)? == 0))
            }
            BuiltinKind::NumberPred => {
                require_exact_arity(args, 1, "number?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::Number(_))))
            }
            BuiltinKind::SymbolPred => {
                require_exact_arity(args, 1, "symbol?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::Symbol(_))))
            }
            BuiltinKind::BooleanPred => {
                require_exact_arity(args, 1, "boolean?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::Bool(_))))
            }
            BuiltinKind::StringPred => {
                require_exact_arity(args, 1, "string?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::String(_))))
            }
            BuiltinKind::ProcedurePred => {
                require_exact_arity(args, 1, "procedure?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::Builtin(_) | Value::Closure(_))))
            }
            BuiltinKind::Quotient => {
                require_exact_arity(args, 2, "quotient", pos)?;
                let dividend = as_number(&args[0], "quotient", pos)?;
                let divisor = as_number(&args[1], "quotient", pos)?;
                if divisor == 0 {
                    return Err(error("division by zero", pos));
                }
                Ok(Value::Number(dividend / divisor))
            }
            BuiltinKind::Remainder => {
                require_exact_arity(args, 2, "remainder", pos)?;
                let dividend = as_number(&args[0], "remainder", pos)?;
                let divisor = as_number(&args[1], "remainder", pos)?;
                if divisor == 0 {
                    return Err(error("division by zero", pos));
                }
                Ok(Value::Number(dividend % divisor))
            }
            BuiltinKind::Vector => Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec())))),
            BuiltinKind::MakeVector => {
                if !(1..=2).contains(&args.len()) {
                    return Err(error("'make-vector' expects 1 or 2 arguments", pos));
                }
                let len = as_index(&args[0], "make-vector", pos)?;
                let fill = args.get(1).cloned().unwrap_or(Value::Void);
                Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
            }
            BuiltinKind::VectorRef => {
                require_exact_arity(args, 2, "vector-ref", pos)?;
                let vector = as_vector(&args[0], "vector-ref", pos)?;
                let index = as_index(&args[1], "vector-ref", pos)?;
                let value = {
                    let items = vector.borrow();
                    items.get(index).cloned()
                };
                value.ok_or_else(|| error("'vector-ref' index out of range", pos))
            }
            BuiltinKind::VectorSet => {
                require_exact_arity(args, 3, "vector-set!", pos)?;
                let vector = as_vector(&args[0], "vector-set!", pos)?;
                let index = as_index(&args[1], "vector-set!", pos)?;
                let mut items = vector.borrow_mut();
                if index >= items.len() {
                    return Err(error("'vector-set!' index out of range", pos));
                }
                items[index] = args[2].clone();
                Ok(Value::Void)
            }
            BuiltinKind::VectorLength => {
                require_exact_arity(args, 1, "vector-length", pos)?;
                Ok(Value::Number(as_vector(&args[0], "vector-length", pos)?.borrow().len() as i64))
            }
            BuiltinKind::VectorPred => {
                require_exact_arity(args, 1, "vector?", pos)?;
                Ok(Value::Bool(matches!(args[0], Value::Vector(_))))
            }
            BuiltinKind::VectorToList => {
                require_exact_arity(args, 1, "vector->list", pos)?;
                Ok(build_list(as_vector(&args[0], "vector->list", pos)?.borrow().clone()))
            }
            BuiltinKind::ListToVector => {
                require_exact_arity(args, 1, "list->vector", pos)?;
                Ok(Value::Vector(Rc::new(RefCell::new(collect_list(
                    &args[0],
                    "list->vector",
                    pos,
                )?))))
            }
            BuiltinKind::SymbolToString => {
                require_exact_arity(args, 1, "symbol->string", pos)?;
                Ok(Value::String(Rc::new(as_symbol(&args[0], "symbol->string", pos)?.to_string())))
            }
            BuiltinKind::StringToSymbol => {
                require_exact_arity(args, 1, "string->symbol", pos)?;
                Ok(Value::Symbol(as_string(&args[0], "string->symbol", pos)?.to_string()))
            }
            BuiltinKind::Display => {
                require_exact_arity(args, 1, "display", pos)?;
                self.output.push_str(&format_value(&args[0], true));
                Ok(Value::Void)
            }
            BuiltinKind::Write => {
                require_exact_arity(args, 1, "write", pos)?;
                self.output.push_str(&format_value(&args[0], false));
                Ok(Value::Void)
            }
            BuiltinKind::Newline => {
                require_exact_arity(args, 0, "newline", pos)?;
                self.output.push('\n');
                Ok(Value::Void)
            }
            BuiltinKind::Error => {
                let mut message = String::new();
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        message.push(' ');
                    }
                    message.push_str(&format_value(arg, true));
                }
                if message.is_empty() {
                    message = "error".to_string();
                }
                Err(error(message, pos))
            }
        }
    }

    fn apply_map(&mut self, args: &[Value], pos: SourcePos) -> EvalResult<Value> {
        require_at_least_arity(args, 2, "map", pos)?;
        let procedure = args[0].clone();
        let mut lists = args[1..].to_vec();
        let mut results = Vec::new();

        loop {
            let mut all_empty = true;
            let mut any_empty = false;
            let mut call_args = Vec::with_capacity(lists.len());
            let mut next_lists = Vec::with_capacity(lists.len());

            for list in lists {
                match list {
                    Value::EmptyList => {
                        any_empty = true;
                        next_lists.push(Value::EmptyList);
                    }
                    Value::Pair(pair) => {
                        all_empty = false;
                        call_args.push(pair.car.clone());
                        next_lists.push(pair.cdr.clone());
                    }
                    _ => return Err(error("'map' expects proper list arguments", pos)),
                }
            }

            if all_empty {
                return Ok(build_list(results));
            }
            if any_empty {
                return Err(error("'map' list arguments must have the same length", pos));
            }

            results.push(self.apply_procedure(procedure.clone(), &call_args, pos)?);
            lists = next_lists;
        }
    }
}

#[derive(Clone)]
struct Binding {
    name: String,
    initializer: Expr,
}

#[derive(Clone)]
struct DoBinding {
    name: String,
    initializer: Expr,
    step: Option<Expr>,
}

fn parse_bindings(expr: &Expr, form_name: &str) -> EvalResult<Vec<Binding>> {
    let items = match expr {
        Expr::List(items, _) => items,
        other => return Err(error(format!("{form_name} expects a binding list"), other.pos())),
    };

    let mut bindings = Vec::with_capacity(items.len());
    for item in items {
        let parts = match item {
            Expr::List(parts, _) => parts,
            other => return Err(error(format!("{form_name} bindings must be lists"), other.pos())),
        };
        if parts.len() != 2 {
            return Err(error(
                format!("{form_name} bindings must contain a name and a value"),
                item.pos(),
            ));
        }
        let name = match &parts[0] {
            Expr::Symbol(name, _) => name.clone(),
            other => {
                return Err(error(
                    format!("{form_name} binding names must be symbols"),
                    other.pos(),
                ))
            }
        };
        bindings.push(Binding {
            name,
            initializer: parts[1].clone(),
        });
    }
    Ok(bindings)
}

fn parse_do_bindings(expr: &Expr) -> EvalResult<Vec<DoBinding>> {
    let items = match expr {
        Expr::List(items, _) => items,
        other => return Err(error("'do' expects a binding list", other.pos())),
    };

    let mut bindings = Vec::with_capacity(items.len());
    for item in items {
        let parts = match item {
            Expr::List(parts, _) => parts,
            other => return Err(error("'do' bindings must be lists", other.pos())),
        };
        if !(2..=3).contains(&parts.len()) {
            return Err(error(
                "'do' bindings must contain a name, an init expression, and an optional step",
                item.pos(),
            ));
        }
        let name = match &parts[0] {
            Expr::Symbol(name, _) => name.clone(),
            other => return Err(error("'do' binding names must be symbols", other.pos())),
        };
        bindings.push(DoBinding {
            name,
            initializer: parts[1].clone(),
            step: parts.get(2).cloned(),
        });
    }
    Ok(bindings)
}

fn parse_params(expr: &Expr) -> EvalResult<ParamSpec> {
    match expr {
        Expr::List(items, pos) => parse_param_list(items, *pos),
        Expr::Symbol(name, _) => Ok(ParamSpec {
            required: Vec::new(),
            rest: Some(name.clone()),
        }),
        other => Err(error(
            "'lambda' parameters must be a list or a symbol",
            other.pos(),
        )),
    }
}

fn parse_param_list(items: &[Expr], pos: SourcePos) -> EvalResult<ParamSpec> {
    let mut required = Vec::new();
    let mut rest = None;
    let mut saw_dot = false;

    for (index, item) in items.iter().enumerate() {
        let name = match item {
            Expr::Symbol(name, _) => name,
            other => return Err(error("parameters must be symbols", other.pos())),
        };

        if name == "." {
            if saw_dot || index == items.len() - 1 {
                return Err(error("invalid dotted parameter list", item.pos()));
            }
            saw_dot = true;
            continue;
        }

        if saw_dot {
            if index != items.len() - 1 {
                return Err(error("rest parameter must be last", item.pos()));
            }
            rest = Some(name.clone());
            return Ok(ParamSpec { required, rest });
        }

        required.push(name.clone());
    }

    if saw_dot {
        return Err(error("invalid dotted parameter list", pos));
    }

    Ok(ParamSpec { required, rest })
}

fn compare_numbers(
    args: &[Value],
    pos: SourcePos,
    name: &'static str,
    predicate: impl Fn(i64, i64) -> bool,
) -> EvalResult<Value> {
    require_at_least_arity(args, 2, name, pos)?;
    let mut left = as_number(&args[0], name, pos)?;
    for arg in &args[1..] {
        let right = as_number(arg, name, pos)?;
        if !predicate(left, right) {
            return Ok(Value::Bool(false));
        }
        left = right;
    }
    Ok(Value::Bool(true))
}

fn quote_to_value(expr: &Expr) -> EvalResult<Value> {
    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Bool(value, _) => Ok(Value::Bool(*value)),
        Expr::String(value, _) => Ok(Value::String(Rc::new(value.clone()))),
        Expr::Symbol(value, _) => Ok(Value::Symbol(value.clone())),
        Expr::List(items, _) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_to_value(item)?);
            }
            Ok(build_list(values))
        }
    }
}

fn build_list(values: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;
    for value in values.into_iter().rev() {
        result = make_pair(value, result);
    }
    result
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(PairValue { car, cdr }))
}

fn collect_list(value: &Value, name: &str, pos: SourcePos) -> EvalResult<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();
    loop {
        match current {
            Value::EmptyList => return Ok(items),
            Value::Pair(pair) => {
                items.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            _ => return Err(error(format!("'{name}' expects a proper list"), pos)),
        }
    }
}

fn copy_list_onto(list: &Value, tail: Value, name: &str, pos: SourcePos) -> EvalResult<Value> {
    let mut items = collect_list(list, name, pos)?;
    let mut result = tail;
    while let Some(item) = items.pop() {
        result = make_pair(item, result);
    }
    Ok(result)
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn eqv_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Void, Value::Void) => true,
        (Value::String(a), Value::String(b)) => Rc::ptr_eq(a, b),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Closure(a), Value::Closure(b)) => Rc::ptr_eq(a, b),
        (Value::Builtin(a), Value::Builtin(b)) => a == b,
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    if eqv_values(left, right) {
        return true;
    }
    match (left, right) {
        (Value::String(a), Value::String(b)) => a.as_str() == b.as_str(),
        (Value::Pair(a), Value::Pair(b)) => {
            equal_values(&a.car, &b.car) && equal_values(&a.cdr, &b.cdr)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let left_items = a.borrow();
            let right_items = b.borrow();
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(l, r)| equal_values(l, r))
        }
        _ => false,
    }
}

fn format_value(value: &Value, display_mode: bool) -> String {
    match value {
        Value::Number(number) => number.to_string(),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::String(text) => {
            if display_mode {
                text.as_str().to_string()
            } else {
                format!("\"{}\"", escape_string(text.as_str()))
            }
        }
        Value::Symbol(symbol) => symbol.clone(),
        Value::EmptyList => "()".to_string(),
        Value::Pair(pair) => format_pair(pair, display_mode),
        Value::Vector(elements) => format_vector(elements, display_mode),
        Value::Builtin(_) | Value::Closure(_) => "#<procedure>".to_string(),
        Value::Void => "#<void>".to_string(),
        Value::Uninitialized => "#<uninitialized>".to_string(),
    }
}

fn format_pair(pair: &PairValue, display_mode: bool) -> String {
    let mut result = String::from("(");
    let mut first = true;
    let mut current = Value::Pair(Rc::new(pair.clone()));
    loop {
        match current {
            Value::Pair(next) => {
                if !first {
                    result.push(' ');
                }
                result.push_str(&format_value(&next.car, display_mode));
                current = next.cdr.clone();
                first = false;
            }
            Value::EmptyList => {
                result.push(')');
                return result;
            }
            other => {
                result.push_str(" . ");
                result.push_str(&format_value(&other, display_mode));
                result.push(')');
                return result;
            }
        }
    }
}

fn format_vector(elements: &Rc<RefCell<Vec<Value>>>, display_mode: bool) -> String {
    let items = elements.borrow();
    let mut result = String::from("#(");
    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            result.push(' ');
        }
        result.push_str(&format_value(item, display_mode));
    }
    result.push(')');
    result
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for ch in value.chars() {
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

fn looks_like_number(token: &str) -> bool {
    let bytes = token.as_bytes();
    if bytes.is_empty() {
        return false;
    }
    let start = if bytes[0] == b'+' || bytes[0] == b'-' {
        if bytes.len() == 1 {
            return false;
        }
        1
    } else {
        0
    };
    bytes[start..].iter().all(u8::is_ascii_digit)
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | ';' | '\'')
}

fn require_exact_arity(args: &[Value], expected: usize, name: &str, pos: SourcePos) -> EvalResult<()> {
    if args.len() == expected {
        return Ok(());
    }
    Err(error(
        format!(
            "'{name}' expects exactly {expected} argument{}",
            if expected == 1 { "" } else { "s" }
        ),
        pos,
    ))
}

fn require_at_least_arity(
    args: &[Value],
    expected: usize,
    name: &str,
    pos: SourcePos,
) -> EvalResult<()> {
    if args.len() >= expected {
        return Ok(());
    }
    Err(error(
        format!(
            "'{name}' expects at least {expected} argument{}",
            if expected == 1 { "" } else { "s" }
        ),
        pos,
    ))
}

fn as_number(value: &Value, name: &str, pos: SourcePos) -> EvalResult<i64> {
    match value {
        Value::Number(number) => Ok(*number),
        _ => Err(error(format!("'{name}' expects numeric arguments"), pos)),
    }
}

fn as_index(value: &Value, name: &str, pos: SourcePos) -> EvalResult<usize> {
    let number = as_number(value, name, pos)?;
    if number < 0 {
        return Err(error(
            format!("'{name}' expects a non-negative integer index"),
            pos,
        ));
    }
    Ok(number as usize)
}

fn as_pair<'a>(value: &'a Value, name: &str, pos: SourcePos) -> EvalResult<&'a PairValue> {
    match value {
        Value::Pair(pair) => Ok(pair.as_ref()),
        _ => Err(error(format!("'{name}' expects a pair"), pos)),
    }
}

fn as_vector(
    value: &Value,
    name: &str,
    pos: SourcePos,
) -> EvalResult<Rc<RefCell<Vec<Value>>>> {
    match value {
        Value::Vector(vector) => Ok(vector.clone()),
        _ => Err(error(format!("'{name}' expects a vector"), pos)),
    }
}

fn as_symbol<'a>(value: &'a Value, name: &str, pos: SourcePos) -> EvalResult<&'a str> {
    match value {
        Value::Symbol(symbol) => Ok(symbol),
        _ => Err(error(format!("'{name}' expects a symbol"), pos)),
    }
}

fn as_string<'a>(value: &'a Value, name: &str, pos: SourcePos) -> EvalResult<&'a str> {
    match value {
        Value::String(text) => Ok(text.as_str()),
        _ => Err(error(format!("'{name}' expects string arguments"), pos)),
    }
}

fn error(message: impl Into<String>, pos: SourcePos) -> EvalError {
    EvalError::at(message.into(), pos.line, pos.column)
}

#[cfg(test)]
mod tests;
