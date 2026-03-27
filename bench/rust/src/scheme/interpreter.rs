use crate::scheme::error::EvalError;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type EnvRef = Rc<Environment>;
type SlotRef = Rc<RefCell<Value>>;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Position {
    line: usize,
    column: usize,
}

impl Position {
    const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: Position,
}

impl Expr {
    fn symbol(pos: Position, name: impl Into<String>) -> Self {
        Self {
            kind: ExprKind::Symbol(name.into()),
            pos,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ExprKind {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    Pair(Rc<Pair>),
    EmptyList,
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone)]
enum Procedure {
    Builtin(Builtin),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    CaseLambda {
        clauses: Vec<CaseLambdaClause>,
        env: EnvRef,
    },
    Continuation(Vec<Frame>),
}

#[derive(Clone)]
struct CaseLambdaClause {
    params: Vec<String>,
    body: Vec<Expr>,
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Subtract,
    LessThan,
    NumericEqual,
    NullPredicate,
    Cons,
    Car,
    Cdr,
    List,
    Not,
    ProcedurePredicate,
    StringAppend,
    StringLength,
    CallCc,
}

#[derive(Clone)]
struct SyntaxRulesMacro {
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Default)]
struct PatternBindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

struct Environment {
    parent: Option<EnvRef>,
    values: RefCell<HashMap<String, SlotRef>>,
    macros: RefCell<HashMap<String, Rc<SyntaxRulesMacro>>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            values: RefCell::new(HashMap::new()),
            macros: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.values
            .borrow_mut()
            .insert(name.into(), Rc::new(RefCell::new(value)));
    }

    fn lookup_slot(&self, name: &str) -> Option<SlotRef> {
        if let Some(slot) = self.values.borrow().get(name) {
            return Some(slot.clone());
        }
        self.parent.as_ref().and_then(|parent| parent.lookup_slot(name))
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_slot(name).map(|slot| slot.borrow().clone())
    }

    fn define_macro(&self, name: impl Into<String>, rules: SyntaxRulesMacro) {
        self.macros
            .borrow_mut()
            .insert(name.into(), Rc::new(rules));
    }

    fn lookup_macro(&self, name: &str) -> Option<Rc<SyntaxRulesMacro>> {
        if let Some(rules) = self.macros.borrow().get(name) {
            return Some(rules.clone());
        }
        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_macro(name))
    }
}

#[derive(Clone)]
enum Control {
    Expr(Expr, EnvRef),
    Value(Value),
}

#[derive(Clone)]
enum Frame {
    Sequence {
        rest: Vec<Expr>,
        env: EnvRef,
    },
    DefineVar {
        name: String,
        env: EnvRef,
    },
    SetVar {
        slot: SlotRef,
    },
    If {
        then_expr: Expr,
        else_expr: Option<Expr>,
        env: EnvRef,
    },
    CondTest {
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    And {
        rest: Vec<Expr>,
        env: EnvRef,
    },
    Or {
        rest: Vec<Expr>,
        env: EnvRef,
    },
    Case {
        clauses: Vec<Expr>,
        env: EnvRef,
        pos: Position,
    },
    LetBindings {
        names: Vec<String>,
        remaining_exprs: Vec<Expr>,
        values: Vec<Value>,
        body: Vec<Expr>,
        outer_env: EnvRef,
    },
    ApplyOperator {
        arg_exprs: Vec<Expr>,
        env: EnvRef,
        pos: Position,
    },
    ApplyArgs {
        proc: Value,
        remaining_exprs: Vec<Expr>,
        evaluated: Vec<Value>,
        env: EnvRef,
        pos: Position,
    },
}

#[derive(Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    String(String),
    Atom(String),
}

#[derive(Clone)]
struct Token {
    kind: TokenKind,
    pos: Position,
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        while self.index < self.tokens.len() {
            expressions.push(self.parse_expr()?);
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .cloned()
            .ok_or_else(|| EvalError::new("unexpected end of input"))?;
        self.index += 1;

        match token.kind {
            TokenKind::LParen => self.parse_list(token.pos),
            TokenKind::RParen => Err(EvalError::at(
                token.pos.line,
                token.pos.column,
                "unexpected ')'",
            )),
            TokenKind::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr {
                    pos: token.pos,
                    kind: ExprKind::List(vec![Expr::symbol(token.pos, "quote"), quoted]),
                })
            }
            TokenKind::String(text) => Ok(Expr {
                kind: ExprKind::String(text),
                pos: token.pos,
            }),
            TokenKind::Atom(text) => Ok(parse_atom(token.pos, text)),
        }
    }

    fn parse_list(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let mut elements = Vec::new();
        loop {
            let token = self.tokens.get(self.index).cloned().ok_or_else(|| {
                EvalError::at(pos.line, pos.column, "unterminated list")
            })?;
            match token.kind {
                TokenKind::RParen => {
                    self.index += 1;
                    return Ok(Expr {
                        kind: ExprKind::List(elements),
                        pos,
                    });
                }
                _ => elements.push(self.parse_expr()?),
            }
        }
    }
}

fn parse_atom(pos: Position, text: String) -> Expr {
    if text == "#t" {
        return Expr {
            kind: ExprKind::Bool(true),
            pos,
        };
    }
    if text == "#f" {
        return Expr {
            kind: ExprKind::Bool(false),
            pos,
        };
    }
    if let Some(value) = parse_int_literal(&text) {
        return Expr {
            kind: ExprKind::Int(value),
            pos,
        };
    }
    Expr {
        kind: ExprKind::Symbol(text),
        pos,
    }
}

fn parse_int_literal(text: &str) -> Option<i64> {
    if text.is_empty() {
        return None;
    }

    let digits = if let Some(rest) = text.strip_prefix('+') {
        rest
    } else if let Some(rest) = text.strip_prefix('-') {
        rest
    } else {
        text
    };

    if digits.is_empty() || !digits.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }

    text.parse().ok()
}

fn lex(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut column = 1usize;

    while let Some(ch) = chars.peek().copied() {
        match ch {
            ' ' | '\t' | '\r' => {
                chars.next();
                column += 1;
            }
            '\n' => {
                chars.next();
                line += 1;
                column = 1;
            }
            ';' => {
                chars.next();
                column += 1;
                while let Some(comment_ch) = chars.next() {
                    if comment_ch == '\n' {
                        line += 1;
                        column = 1;
                        break;
                    }
                    column += 1;
                }
            }
            '(' => {
                let pos = Position::new(line, column);
                chars.next();
                column += 1;
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    pos,
                });
            }
            ')' => {
                let pos = Position::new(line, column);
                chars.next();
                column += 1;
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    pos,
                });
            }
            '\'' => {
                let pos = Position::new(line, column);
                chars.next();
                column += 1;
                tokens.push(Token {
                    kind: TokenKind::Quote,
                    pos,
                });
            }
            '"' => {
                let pos = Position::new(line, column);
                chars.next();
                column += 1;

                let mut text = String::new();
                let mut terminated = false;
                while let Some(string_ch) = chars.next() {
                    match string_ch {
                        '"' => {
                            column += 1;
                            terminated = true;
                            break;
                        }
                        '\\' => {
                            column += 1;
                            let escaped = chars.next().ok_or_else(|| {
                                EvalError::at(pos.line, pos.column, "unterminated string")
                            })?;
                            match escaped {
                                '"' => {
                                    column += 1;
                                    text.push('"');
                                }
                                '\\' => {
                                    column += 1;
                                    text.push('\\');
                                }
                                'n' => {
                                    column += 1;
                                    text.push('\n');
                                }
                                't' => {
                                    column += 1;
                                    text.push('\t');
                                }
                                '\n' => {
                                    line += 1;
                                    column = 1;
                                    text.push('\n');
                                }
                                other => {
                                    column += 1;
                                    text.push(other);
                                }
                            }
                        }
                        '\n' => {
                            line += 1;
                            column = 1;
                            text.push('\n');
                        }
                        other => {
                            column += 1;
                            text.push(other);
                        }
                    }
                }

                if !terminated {
                    return Err(EvalError::at(pos.line, pos.column, "unterminated string"));
                }

                tokens.push(Token {
                    kind: TokenKind::String(text),
                    pos,
                });
            }
            _ => {
                let pos = Position::new(line, column);
                let mut text = String::new();
                while let Some(atom_ch) = chars.peek().copied() {
                    if atom_ch.is_whitespace()
                        || atom_ch == '('
                        || atom_ch == ')'
                        || atom_ch == '\''
                        || atom_ch == '"'
                        || atom_ch == ';'
                    {
                        break;
                    }
                    chars.next();
                    column += 1;
                    text.push(atom_ch);
                }

                if text.is_empty() {
                    return Err(EvalError::at(pos.line, pos.column, "invalid token"));
                }

                tokens.push(Token {
                    kind: TokenKind::Atom(text),
                    pos,
                });
            }
        }
    }

    Ok(tokens)
}

pub(crate) fn eval_str(input: &str) -> Result<String, EvalError> {
    let (result, _) = eval_internal(input)?;
    Ok(result)
}

pub(crate) fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_internal(input)
}

fn eval_internal(input: &str) -> Result<(String, String), EvalError> {
    let tokens = lex(input)?;
    let mut parser = Parser::new(tokens);
    let expressions = parser.parse_program()?;
    let value = evaluate(expressions)?;
    Ok((format_value(&value), String::new()))
}

fn evaluate(expressions: Vec<Expr>) -> Result<Value, EvalError> {
    let global = Environment::new(None);
    install_builtins(&global);

    let (mut control, mut stack) = start_sequence(&expressions, global, Vec::new())?;
    loop {
        match control {
            Control::Expr(expr, env) => {
                let (next_control, next_stack) = step_expr(expr, env, stack)?;
                control = next_control;
                stack = next_stack;
            }
            Control::Value(value) => {
                if let Some(frame) = stack.pop() {
                    let (next_control, next_stack) = resume(frame, value, stack)?;
                    control = next_control;
                    stack = next_stack;
                } else {
                    return Ok(value);
                }
            }
        }
    }
}

fn install_builtins(env: &EnvRef) {
    let builtins = [
        ("+", Builtin::Add),
        ("-", Builtin::Subtract),
        ("<", Builtin::LessThan),
        ("=", Builtin::NumericEqual),
        ("null?", Builtin::NullPredicate),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("list", Builtin::List),
        ("not", Builtin::Not),
        ("procedure?", Builtin::ProcedurePredicate),
        ("string-append", Builtin::StringAppend),
        ("string-length", Builtin::StringLength),
        ("call/cc", Builtin::CallCc),
        (
            "call-with-current-continuation",
            Builtin::CallCc,
        ),
    ];

    for (name, builtin) in builtins {
        env.define(name, Value::Procedure(Rc::new(Procedure::Builtin(builtin))));
    }
}

fn start_sequence(
    expressions: &[Expr],
    env: EnvRef,
    mut stack: Vec<Frame>,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if expressions.is_empty() {
        return Ok((Control::Value(Value::Void), stack));
    }

    if expressions.len() > 1 {
        stack.push(Frame::Sequence {
            rest: expressions[1..].to_vec(),
            env: env.clone(),
        });
    }

    Ok((Control::Expr(expressions[0].clone(), env), stack))
}

fn start_cond(
    clauses: &[Expr],
    env: EnvRef,
    stack: Vec<Frame>,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if clauses.is_empty() {
        return Ok((Control::Value(Value::Void), stack));
    }

    let first = &clauses[0];
    let elements = match &first.kind {
        ExprKind::List(elements) if !elements.is_empty() => elements,
        _ => {
            return Err(EvalError::at(
                first.pos.line,
                first.pos.column,
                "cond clauses must be non-empty lists",
            ))
        }
    };

    if let Some(symbol) = symbol_name(&elements[0]) {
        if symbol == "else" {
            if clauses.len() != 1 {
                return Err(EvalError::at(
                    elements[0].pos.line,
                    elements[0].pos.column,
                    "cond else clause must be last",
                ));
            }
            return start_sequence(&elements[1..], env, stack);
        }
    }

    let mut next_stack = stack;
    next_stack.push(Frame::CondTest {
        body: elements[1..].to_vec(),
        remaining: clauses[1..].to_vec(),
        env: env.clone(),
    });
    Ok((Control::Expr(elements[0].clone(), env), next_stack))
}

fn start_and(
    expressions: &[Expr],
    env: EnvRef,
    mut stack: Vec<Frame>,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if expressions.is_empty() {
        return Ok((Control::Value(Value::Bool(true)), stack));
    }

    if expressions.len() == 1 {
        return Ok((Control::Expr(expressions[0].clone(), env), stack));
    }

    stack.push(Frame::And {
        rest: expressions[1..].to_vec(),
        env: env.clone(),
    });
    Ok((Control::Expr(expressions[0].clone(), env), stack))
}

fn start_or(
    expressions: &[Expr],
    env: EnvRef,
    mut stack: Vec<Frame>,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if expressions.is_empty() {
        return Ok((Control::Value(Value::Bool(false)), stack));
    }

    if expressions.len() == 1 {
        return Ok((Control::Expr(expressions[0].clone(), env), stack));
    }

    stack.push(Frame::Or {
        rest: expressions[1..].to_vec(),
        env: env.clone(),
    });
    Ok((Control::Expr(expressions[0].clone(), env), stack))
}

fn start_case(
    key: Expr,
    clauses: &[Expr],
    env: EnvRef,
    mut stack: Vec<Frame>,
    pos: Position,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if clauses.is_empty() {
        return Err(EvalError::at(
            pos.line,
            pos.column,
            "case expects a key and at least 1 clause",
        ));
    }

    stack.push(Frame::Case {
        clauses: clauses.to_vec(),
        env: env.clone(),
        pos,
    });
    Ok((Control::Expr(key, env), stack))
}

fn step_expr(expr: Expr, env: EnvRef, mut stack: Vec<Frame>) -> Result<(Control, Vec<Frame>), EvalError> {
    let pos = expr.pos;
    match expr.kind {
        ExprKind::Int(value) => Ok((Control::Value(Value::Int(value)), stack)),
        ExprKind::Bool(value) => Ok((Control::Value(Value::Bool(value)), stack)),
        ExprKind::String(value) => Ok((Control::Value(Value::String(value)), stack)),
        ExprKind::Symbol(name) => {
            let value = env.lookup(&name).ok_or_else(|| {
                EvalError::at(pos.line, pos.column, format!("unbound variable: {name}"))
            })?;
            Ok((Control::Value(value), stack))
        }
        ExprKind::List(elements) => {
            if elements.is_empty() {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "cannot evaluate empty list",
                ));
            }

            if let Some(form) = symbol_name(&elements[0]) {
                match form {
                    "quote" => {
                        if elements.len() != 2 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "quote expects exactly 1 argument",
                            ));
                        }
                        return Ok((Control::Value(datum_to_value(&elements[1])?), stack));
                    }
                    "if" => {
                        if elements.len() < 3 || elements.len() > 4 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "if expects 2 or 3 arguments",
                            ));
                        }
                        stack.push(Frame::If {
                            then_expr: elements[2].clone(),
                            else_expr: elements.get(3).cloned(),
                            env: env.clone(),
                        });
                        return Ok((Control::Expr(elements[1].clone(), env), stack));
                    }
                    "begin" => {
                        return start_sequence(&elements[1..], env, stack);
                    }
                    "and" => {
                        return start_and(&elements[1..], env, stack);
                    }
                    "or" => {
                        return start_or(&elements[1..], env, stack);
                    }
                    "lambda" => {
                        if elements.len() < 3 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "lambda expects parameters and a body",
                            ));
                        }
                        let params = parse_params(&elements[1])?;
                        let procedure = Procedure::Lambda {
                            params,
                            body: elements[2..].to_vec(),
                            env,
                        };
                        return Ok((
                            Control::Value(Value::Procedure(Rc::new(procedure))),
                            stack,
                        ));
                    }
                    "case-lambda" => {
                        if elements.len() < 2 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "case-lambda expects at least 1 clause",
                            ));
                        }

                        let mut clauses = Vec::with_capacity(elements.len() - 1);
                        for clause in &elements[1..] {
                            let ExprKind::List(parts) = &clause.kind else {
                                return Err(EvalError::at(
                                    clause.pos.line,
                                    clause.pos.column,
                                    "case-lambda clauses must contain parameters and a body",
                                ));
                            };
                            if parts.len() < 2 {
                                return Err(EvalError::at(
                                    clause.pos.line,
                                    clause.pos.column,
                                    "case-lambda clauses must contain parameters and a body",
                                ));
                            }

                            clauses.push(CaseLambdaClause {
                                params: parse_params(&parts[0])?,
                                body: parts[1..].to_vec(),
                            });
                        }

                        let procedure = Procedure::CaseLambda { clauses, env };
                        return Ok((
                            Control::Value(Value::Procedure(Rc::new(procedure))),
                            stack,
                        ));
                    }
                    "define" => {
                        return eval_define(&elements[1..], pos, env, stack);
                    }
                    "define-syntax" => {
                        if elements.len() != 3 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "define-syntax expects exactly 2 arguments",
                            ));
                        }
                        let name = match &elements[1].kind {
                            ExprKind::Symbol(name) => name.clone(),
                            _ => {
                                return Err(EvalError::at(
                                    elements[1].pos.line,
                                    elements[1].pos.column,
                                    "define-syntax requires a symbol name",
                                ))
                            }
                        };
                        let rules = parse_syntax_rules(&elements[2])?;
                        env.define_macro(name, rules);
                        return Ok((Control::Value(Value::Void), stack));
                    }
                    "set!" => {
                        if elements.len() != 3 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "set! expects exactly 2 arguments",
                            ));
                        }
                        let name = match &elements[1].kind {
                            ExprKind::Symbol(name) => name.clone(),
                            _ => {
                                return Err(EvalError::at(
                                    elements[1].pos.line,
                                    elements[1].pos.column,
                                    "set! requires a symbol name",
                                ))
                            }
                        };
                        let slot = env.lookup_slot(&name).ok_or_else(|| {
                            EvalError::at(
                                elements[1].pos.line,
                                elements[1].pos.column,
                                format!("unbound variable: {name}"),
                            )
                        })?;
                        stack.push(Frame::SetVar { slot });
                        return Ok((Control::Expr(elements[2].clone(), env), stack));
                    }
                    "let" => {
                        return eval_let(&elements[1..], pos, env, stack);
                    }
                    "cond" => {
                        return start_cond(&elements[1..], env, stack);
                    }
                    "case" => {
                        if elements.len() < 3 {
                            return Err(EvalError::at(
                                pos.line,
                                pos.column,
                                "case expects a key and at least 1 clause",
                            ));
                        }
                        return start_case(
                            elements[1].clone(),
                            &elements[2..],
                            env,
                            stack,
                            pos,
                        );
                    }
                    _ => {}
                }

                if let Some(rules) = env.lookup_macro(form) {
                    let expanded = rules.expand(&Expr {
                        kind: ExprKind::List(elements),
                        pos,
                    })?;
                    return Ok((Control::Expr(expanded, env), stack));
                }
            }

            stack.push(Frame::ApplyOperator {
                arg_exprs: elements[1..].to_vec(),
                env: env.clone(),
                pos,
            });
            Ok((Control::Expr(elements[0].clone(), env), stack))
        }
    }
}

fn eval_define(
    args: &[Expr],
    pos: Position,
    env: EnvRef,
    mut stack: Vec<Frame>,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::at(
            pos.line,
            pos.column,
            "define expects at least 2 arguments",
        ));
    }

    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "define expects exactly 2 arguments",
                ));
            }
            stack.push(Frame::DefineVar {
                name: name.clone(),
                env: env.clone(),
            });
            Ok((Control::Expr(args[1].clone(), env), stack))
        }
        ExprKind::List(parts) if !parts.is_empty() => {
            let name = match &parts[0].kind {
                ExprKind::Symbol(name) => name.clone(),
                _ => {
                    return Err(EvalError::at(
                        parts[0].pos.line,
                        parts[0].pos.column,
                        "define requires a symbol name",
                    ))
                }
            };
            let params = parse_params_from_slice(&parts[1..])?;
            let procedure = Procedure::Lambda {
                params,
                body: args[1..].to_vec(),
                env: env.clone(),
            };
            env.define(name, Value::Procedure(Rc::new(procedure)));
            Ok((Control::Value(Value::Void), stack))
        }
        ExprKind::List(_) => Err(EvalError::at(
            args[0].pos.line,
            args[0].pos.column,
            "define requires a function name",
        )),
        _ => Err(EvalError::at(
            args[0].pos.line,
            args[0].pos.column,
            "define requires a symbol or parameter list",
        )),
    }
}

fn eval_let(
    args: &[Expr],
    pos: Position,
    env: EnvRef,
    mut stack: Vec<Frame>,
) -> Result<(Control, Vec<Frame>), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::at(
            pos.line,
            pos.column,
            "let expects bindings and a body",
        ));
    }

    let bindings = parse_let_bindings(&args[0])?;
    let body = args[1..].to_vec();

    if bindings.is_empty() {
        let let_env = Environment::new(Some(env));
        return start_sequence(&body, let_env, stack);
    }

    let names = bindings.iter().map(|(name, _)| name.clone()).collect::<Vec<_>>();
    let exprs = bindings.into_iter().map(|(_, expr)| expr).collect::<Vec<_>>();
    stack.push(Frame::LetBindings {
        names,
        remaining_exprs: exprs[1..].to_vec(),
        values: Vec::new(),
        body,
        outer_env: env.clone(),
    });
    Ok((Control::Expr(exprs[0].clone(), env), stack))
}

fn resume(frame: Frame, value: Value, mut stack: Vec<Frame>) -> Result<(Control, Vec<Frame>), EvalError> {
    match frame {
        Frame::Sequence { rest, env } => start_sequence(&rest, env, stack),
        Frame::DefineVar { name, env } => {
            env.define(name, value);
            Ok((Control::Value(Value::Void), stack))
        }
        Frame::SetVar { slot } => {
            *slot.borrow_mut() = value;
            Ok((Control::Value(Value::Void), stack))
        }
        Frame::If {
            then_expr,
            else_expr,
            env,
        } => {
            if is_truthy(&value) {
                Ok((Control::Expr(then_expr, env), stack))
            } else if let Some(else_expr) = else_expr {
                Ok((Control::Expr(else_expr, env), stack))
            } else {
                Ok((Control::Value(Value::Void), stack))
            }
        }
        Frame::CondTest {
            body,
            remaining,
            env,
        } => {
            if is_truthy(&value) {
                if body.is_empty() {
                    Ok((Control::Value(value), stack))
                } else {
                    start_sequence(&body, env, stack)
                }
            } else {
                start_cond(&remaining, env, stack)
            }
        }
        Frame::And { rest, env } => {
            if !is_truthy(&value) || rest.is_empty() {
                Ok((Control::Value(value), stack))
            } else if rest.len() == 1 {
                Ok((Control::Expr(rest[0].clone(), env), stack))
            } else {
                stack.push(Frame::And {
                    rest: rest[1..].to_vec(),
                    env: env.clone(),
                });
                Ok((Control::Expr(rest[0].clone(), env), stack))
            }
        }
        Frame::Or { rest, env } => {
            if is_truthy(&value) || rest.is_empty() {
                Ok((Control::Value(value), stack))
            } else if rest.len() == 1 {
                Ok((Control::Expr(rest[0].clone(), env), stack))
            } else {
                stack.push(Frame::Or {
                    rest: rest[1..].to_vec(),
                    env: env.clone(),
                });
                Ok((Control::Expr(rest[0].clone(), env), stack))
            }
        }
        Frame::Case { clauses, env, pos } => resume_case(value, clauses, env, stack, pos),
        Frame::LetBindings {
            names,
            remaining_exprs,
            mut values,
            body,
            outer_env,
        } => {
            values.push(value);
            if remaining_exprs.is_empty() {
                let let_env = Environment::new(Some(outer_env));
                for (name, value) in names.into_iter().zip(values.into_iter()) {
                    let_env.define(name, value);
                }
                start_sequence(&body, let_env, stack)
            } else {
                stack.push(Frame::LetBindings {
                    names,
                    remaining_exprs: remaining_exprs[1..].to_vec(),
                    values,
                    body,
                    outer_env: outer_env.clone(),
                });
                Ok((Control::Expr(remaining_exprs[0].clone(), outer_env), stack))
            }
        }
        Frame::ApplyOperator { arg_exprs, env, pos } => {
            if arg_exprs.is_empty() {
                apply_procedure(value, Vec::new(), stack, pos)
            } else {
                let split_at = arg_exprs.len() - 1;
                stack.push(Frame::ApplyArgs {
                    proc: value,
                    remaining_exprs: arg_exprs[..split_at].to_vec(),
                    evaluated: Vec::new(),
                    env: env.clone(),
                    pos,
                });
                Ok((Control::Expr(arg_exprs[split_at].clone(), env), stack))
            }
        }
        Frame::ApplyArgs {
            proc,
            remaining_exprs,
            mut evaluated,
            env,
            pos,
        } => {
            evaluated.insert(0, value);
            if remaining_exprs.is_empty() {
                apply_procedure(proc, evaluated, stack, pos)
            } else {
                let split_at = remaining_exprs.len() - 1;
                stack.push(Frame::ApplyArgs {
                    proc,
                    remaining_exprs: remaining_exprs[..split_at].to_vec(),
                    evaluated,
                    env: env.clone(),
                    pos,
                });
                Ok((Control::Expr(remaining_exprs[split_at].clone(), env), stack))
            }
        }
    }
}

fn apply_procedure(
    proc: Value,
    args: Vec<Value>,
    stack: Vec<Frame>,
    pos: Position,
) -> Result<(Control, Vec<Frame>), EvalError> {
    let Value::Procedure(proc_ref) = proc else {
        return Err(EvalError::at(
            pos.line,
            pos.column,
            "attempt to call non-procedure",
        ));
    };

    match proc_ref.as_ref() {
        Procedure::Builtin(builtin) => apply_builtin(*builtin, args, stack, pos),
        Procedure::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    format!(
                        "wrong number of arguments: expected {}, got {}",
                        params.len(),
                        args.len()
                    ),
                ));
            }

            let call_env = Environment::new(Some(env.clone()));
            for (name, value) in params.iter().cloned().zip(args.into_iter()) {
                call_env.define(name, value);
            }
            start_sequence(body, call_env, stack)
        }
        Procedure::CaseLambda { clauses, env } => {
            let clause = clauses.iter().find(|clause| clause.params.len() == args.len());
            let Some(clause) = clause else {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    format!(
                        "wrong number of arguments: no matching case-lambda clause for {} argument(s)",
                        args.len()
                    ),
                ));
            };

            let call_env = Environment::new(Some(env.clone()));
            for (name, value) in clause.params.iter().cloned().zip(args.into_iter()) {
                call_env.define(name, value);
            }
            start_sequence(&clause.body, call_env, stack)
        }
        Procedure::Continuation(saved_stack) => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "continuation expects exactly 1 argument",
                ));
            }
            Ok((Control::Value(args[0].clone()), saved_stack.clone()))
        }
    }
}

fn apply_builtin(
    builtin: Builtin,
    args: Vec<Value>,
    stack: Vec<Frame>,
    pos: Position,
) -> Result<(Control, Vec<Frame>), EvalError> {
    let value = match builtin {
        Builtin::Add => {
            let mut total = 0i64;
            for arg in &args {
                total += expect_int(arg, pos, "+")?;
            }
            Value::Int(total)
        }
        Builtin::Subtract => {
            if args.is_empty() {
                return Err(EvalError::at(pos.line, pos.column, "- expects at least 1 argument"));
            }
            let first = expect_int(&args[0], pos, "-")?;
            let result = if args.len() == 1 {
                -first
            } else {
                let mut running = first;
                for arg in &args[1..] {
                    running -= expect_int(arg, pos, "-")?;
                }
                running
            };
            Value::Int(result)
        }
        Builtin::LessThan => Value::Bool(compare_numbers(&args, pos, "<", |left, right| left < right)?),
        Builtin::NumericEqual => {
            Value::Bool(compare_numbers(&args, pos, "=", |left, right| left == right)?)
        }
        Builtin::NullPredicate => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "null? expects exactly 1 argument",
                ));
            }
            Value::Bool(matches!(args[0], Value::EmptyList))
        }
        Builtin::Cons => {
            if args.len() != 2 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "cons expects exactly 2 arguments",
                ));
            }
            Value::Pair(Rc::new(Pair {
                car: args[0].clone(),
                cdr: args[1].clone(),
            }))
        }
        Builtin::Car => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "car expects exactly 1 argument",
                ));
            }
            expect_pair(&args[0], pos, "car")?.car.clone()
        }
        Builtin::Cdr => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "cdr expects exactly 1 argument",
                ));
            }
            expect_pair(&args[0], pos, "cdr")?.cdr.clone()
        }
        Builtin::List => build_list(args),
        Builtin::Not => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "not expects exactly 1 argument",
                ));
            }
            Value::Bool(!is_truthy(&args[0]))
        }
        Builtin::ProcedurePredicate => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "procedure? expects exactly 1 argument",
                ));
            }
            Value::Bool(matches!(args[0], Value::Procedure(_)))
        }
        Builtin::StringAppend => {
            let total_len = args.iter().try_fold(0usize, |len, arg| {
                Ok::<usize, EvalError>(len + expect_string(arg, pos, "string-append")?.len())
            })?;
            let mut result = String::with_capacity(total_len);
            for arg in &args {
                result.push_str(expect_string(arg, pos, "string-append")?);
            }
            Value::String(result)
        }
        Builtin::StringLength => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "string-length expects exactly 1 argument",
                ));
            }
            Value::Int(expect_string(&args[0], pos, "string-length")?.chars().count() as i64)
        }
        Builtin::CallCc => {
            if args.len() != 1 {
                return Err(EvalError::at(
                    pos.line,
                    pos.column,
                    "call/cc expects exactly 1 argument",
                ));
            }
            let continuation = Value::Procedure(Rc::new(Procedure::Continuation(stack.clone())));
            return apply_procedure(args[0].clone(), vec![continuation], stack, pos);
        }
    };

    Ok((Control::Value(value), stack))
}

fn parse_params(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let ExprKind::List(parts) = &expr.kind else {
        return Err(EvalError::at(
            expr.pos.line,
            expr.pos.column,
            "lambda parameters must be a list",
        ));
    };
    parse_params_from_slice(parts)
}

fn parse_params_from_slice(parts: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(parts.len());
    for part in parts {
        match &part.kind {
            ExprKind::Symbol(name) => params.push(name.clone()),
            _ => {
                return Err(EvalError::at(
                    part.pos.line,
                    part.pos.column,
                    "parameter name must be a symbol",
                ))
            }
        }
    }
    Ok(params)
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::at(
            expr.pos.line,
            expr.pos.column,
            "let bindings must be a list",
        ));
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(parts) = &binding.kind else {
            return Err(EvalError::at(
                binding.pos.line,
                binding.pos.column,
                "let bindings must contain a name and value",
            ));
        };
        if parts.len() != 2 {
            return Err(EvalError::at(
                binding.pos.line,
                binding.pos.column,
                "let bindings must contain a name and value",
            ));
        }

        let name = match &parts[0].kind {
            ExprKind::Symbol(name) => name.clone(),
            _ => {
                return Err(EvalError::at(
                    parts[0].pos.line,
                    parts[0].pos.column,
                    "let binding name must be a symbol",
                ))
            }
        };
        parsed.push((name, parts[1].clone()));
    }
    Ok(parsed)
}

fn parse_syntax_rules(expr: &Expr) -> Result<SyntaxRulesMacro, EvalError> {
    let ExprKind::List(parts) = &expr.kind else {
        return Err(EvalError::at(
            expr.pos.line,
            expr.pos.column,
            "define-syntax requires a syntax-rules transformer",
        ));
    };
    if parts.len() < 3 || symbol_name(&parts[0]) != Some("syntax-rules") {
        return Err(EvalError::at(
            expr.pos.line,
            expr.pos.column,
            "define-syntax requires a syntax-rules transformer",
        ));
    }

    let ExprKind::List(literal_exprs) = &parts[1].kind else {
        return Err(EvalError::at(
            parts[1].pos.line,
            parts[1].pos.column,
            "syntax-rules literals must be a list",
        ));
    };

    let mut literals = HashSet::new();
    for literal in literal_exprs {
        let ExprKind::Symbol(name) = &literal.kind else {
            return Err(EvalError::at(
                literal.pos.line,
                literal.pos.column,
                "syntax-rules literals must be symbols",
            ));
        };
        literals.insert(name.clone());
    }

    let mut rules = Vec::with_capacity(parts.len() - 2);
    for rule_expr in &parts[2..] {
        let ExprKind::List(rule_parts) = &rule_expr.kind else {
            return Err(EvalError::at(
                rule_expr.pos.line,
                rule_expr.pos.column,
                "syntax-rules clauses must contain a pattern and template",
            ));
        };
        if rule_parts.len() != 2 {
            return Err(EvalError::at(
                rule_expr.pos.line,
                rule_expr.pos.column,
                "syntax-rules clauses must contain a pattern and template",
            ));
        }
        rules.push(SyntaxRule {
            pattern: rule_parts[0].clone(),
            template: rule_parts[1].clone(),
        });
    }

    Ok(SyntaxRulesMacro { literals, rules })
}

impl SyntaxRulesMacro {
    fn expand(&self, invocation: &Expr) -> Result<Expr, EvalError> {
        for rule in &self.rules {
            let mut bindings = PatternBindings::default();
            if match_pattern(&rule.pattern, invocation, &self.literals, &mut bindings) {
                return expand_template(&rule.template, &bindings);
            }
        }

        Err(EvalError::at(
            invocation.pos.line,
            invocation.pos.column,
            "no matching syntax-rules clause",
        ))
    }
}

fn datum_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Int(value) => Ok(Value::Int(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => Ok(Value::Symbol(name.clone())),
        ExprKind::List(elements) => {
            let mut result = Value::EmptyList;
            for element in elements.iter().rev() {
                let value = datum_to_value(element)?;
                result = Value::Pair(Rc::new(Pair {
                    car: value,
                    cdr: result,
                }));
            }
            Ok(result)
        }
    }
}

fn build_list(values: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;
    for value in values.into_iter().rev() {
        result = Value::Pair(Rc::new(Pair {
            car: value,
            cdr: result,
        }));
    }
    result
}

fn expect_int(value: &Value, pos: Position, name: &str) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        _ => Err(EvalError::at(
            pos.line,
            pos.column,
            format!("{name} expects numeric arguments"),
        )),
    }
}

fn expect_string<'a>(value: &'a Value, pos: Position, name: &str) -> Result<&'a str, EvalError> {
    match value {
        Value::String(text) => Ok(text),
        _ => Err(EvalError::at(
            pos.line,
            pos.column,
            format!("{name} expects string arguments"),
        )),
    }
}

fn expect_pair<'a>(value: &'a Value, pos: Position, name: &str) -> Result<&'a Pair, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.as_ref()),
        _ => Err(EvalError::at(
            pos.line,
            pos.column,
            format!("{name} expects a pair"),
        )),
    }
}

fn compare_numbers(
    args: &[Value],
    pos: Position,
    name: &str,
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<bool, EvalError> {
    if args.len() < 2 {
        return Ok(true);
    }

    let mut previous = expect_int(&args[0], pos, name)?;
    for arg in &args[1..] {
        let current = expect_int(arg, pos, name)?;
        if !predicate(previous, current) {
            return Ok(false);
        }
        previous = current;
    }
    Ok(true)
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn resume_case(
    key: Value,
    clauses: Vec<Expr>,
    env: EnvRef,
    stack: Vec<Frame>,
    pos: Position,
) -> Result<(Control, Vec<Frame>), EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let ExprKind::List(parts) = &clause.kind else {
            return Err(EvalError::at(
                clause.pos.line,
                clause.pos.column,
                "case clauses must be non-empty lists",
            ));
        };
        if parts.is_empty() {
            return Err(EvalError::at(
                clause.pos.line,
                clause.pos.column,
                "case clauses must be non-empty lists",
            ));
        }

        if symbol_name(&parts[0]) == Some("else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::at(
                    parts[0].pos.line,
                    parts[0].pos.column,
                    "case else clause must be last",
                ));
            }
            return start_sequence(&parts[1..], env, stack);
        }

        let ExprKind::List(datums) = &parts[0].kind else {
            return Err(EvalError::at(
                parts[0].pos.line,
                parts[0].pos.column,
                "case clause datums must be a list",
            ));
        };
        for datum in datums {
            let candidate = datum_to_value(datum)?;
            if values_equal(&key, &candidate) {
                return start_sequence(&parts[1..], env, stack);
            }
        }
    }

    Ok((Control::Value(Value::Void), stack))
}

fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_pair), Value::Pair(right_pair)) => {
            values_equal(&left_pair.car, &right_pair.car)
                && values_equal(&left_pair.cdr, &right_pair.cdr)
        }
        (Value::Procedure(left_proc), Value::Procedure(right_proc)) => Rc::ptr_eq(left_proc, right_proc),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
) -> bool {
    match_pattern_with_mode(pattern, input, literals, bindings, false)
}

fn match_pattern_with_mode(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> bool {
    match (&pattern.kind, &input.kind) {
        (ExprKind::Int(left), ExprKind::Int(right)) => left == right,
        (ExprKind::Bool(left), ExprKind::Bool(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Symbol(name), _) => match name.as_str() {
            "_" => true,
            "..." => false,
            _ if literals.contains(name) => matches!(&input.kind, ExprKind::Symbol(other) if other == name),
            _ if repeated => bind_repeated(name, input.clone(), bindings),
            _ => bind_single(name, input.clone(), bindings),
        },
        (ExprKind::List(patterns), ExprKind::List(inputs)) => {
            match_list_pattern(patterns, inputs, literals, bindings, repeated)
        }
        _ => false,
    }
}

fn match_list_pattern(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> bool {
    if let Some(index) = ellipsis_pattern_index(patterns) {
        let prefix = &patterns[..index];
        let repeated_pattern = &patterns[index];
        let suffix = &patterns[index + 2..];

        if inputs.len() < prefix.len() + suffix.len() {
            return false;
        }

        for (pattern, input) in prefix.iter().zip(&inputs[..prefix.len()]) {
            if !match_pattern_with_mode(pattern, input, literals, bindings, repeated) {
                return false;
            }
        }

        let suffix_start = inputs.len() - suffix.len();
        for (pattern, input) in suffix.iter().zip(&inputs[suffix_start..]) {
            if !match_pattern_with_mode(pattern, input, literals, bindings, repeated) {
                return false;
            }
        }

        let middle = &inputs[prefix.len()..suffix_start];
        if middle.is_empty() {
            record_empty_repeated_bindings(repeated_pattern, literals, bindings);
            return true;
        }

        for input in middle {
            if !match_pattern_with_mode(repeated_pattern, input, literals, bindings, true) {
                return false;
            }
        }
        true
    } else {
        if patterns.len() != inputs.len() {
            return false;
        }

        patterns
            .iter()
            .zip(inputs)
            .all(|(pattern, input)| match_pattern_with_mode(pattern, input, literals, bindings, repeated))
    }
}

fn bind_single(name: &str, input: Expr, bindings: &mut PatternBindings) -> bool {
    if let Some(existing) = bindings.single.get(name) {
        existing == &input
    } else if bindings.repeated.contains_key(name) {
        false
    } else {
        bindings.single.insert(name.to_string(), input);
        true
    }
}

fn bind_repeated(name: &str, input: Expr, bindings: &mut PatternBindings) -> bool {
    if bindings.single.contains_key(name) {
        return false;
    }

    bindings
        .repeated
        .entry(name.to_string())
        .or_default()
        .push(input);
    true
}

fn ellipsis_pattern_index(patterns: &[Expr]) -> Option<usize> {
    patterns
        .windows(2)
        .position(|window| matches!(window, [_, expr] if symbol_name(expr) == Some("...")))
}

fn record_empty_repeated_bindings(
    pattern: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
) {
    let mut names = Vec::new();
    collect_pattern_variables(pattern, literals, &mut names);
    for name in names {
        bindings.repeated.entry(name).or_default();
    }
}

fn collect_pattern_variables(
    pattern: &Expr,
    literals: &HashSet<String>,
    names: &mut Vec<String>,
) {
    match &pattern.kind {
        ExprKind::Symbol(name)
            if name != "_"
                && name != "..."
                && !literals.contains(name)
                && !names.contains(name) =>
        {
            names.push(name.clone());
        }
        ExprKind::List(parts) => {
            for part in parts {
                collect_pattern_variables(part, literals, names);
            }
        }
        _ => {}
    }
}

fn expand_template(template: &Expr, bindings: &PatternBindings) -> Result<Expr, EvalError> {
    expand_template_with_index(template, bindings, None)
}

fn expand_template_with_index(
    template: &Expr,
    bindings: &PatternBindings,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(expr) = bindings.single.get(name) {
                return Ok(expr.clone());
            }
            if let Some(exprs) = bindings.repeated.get(name) {
                let Some(index) = repetition_index else {
                    return Err(EvalError::at(
                        template.pos.line,
                        template.pos.column,
                        format!("template references repeated variable {name} outside ellipsis"),
                    ));
                };
                return exprs.get(index).cloned().ok_or_else(|| {
                    EvalError::at(
                        template.pos.line,
                        template.pos.column,
                        format!("template repetition out of bounds for {name}"),
                    )
                });
            }
            Ok(template.clone())
        }
        ExprKind::List(elements) => {
            let mut expanded = Vec::new();
            let mut index = 0usize;
            while index < elements.len() {
                if index + 1 < elements.len() && symbol_name(&elements[index + 1]) == Some("...") {
                    let repeat_count = template_repeat_count(&elements[index], bindings)?;
                    for repeat_index in 0..repeat_count {
                        expanded.push(expand_template_with_index(
                            &elements[index],
                            bindings,
                            Some(repeat_index),
                        )?);
                    }
                    index += 2;
                } else {
                    expanded.push(expand_template_with_index(
                        &elements[index],
                        bindings,
                        repetition_index,
                    )?);
                    index += 1;
                }
            }
            Ok(Expr {
                kind: ExprKind::List(expanded),
                pos: template.pos,
            })
        }
        _ => Ok(template.clone()),
    }
}

fn template_repeat_count(template: &Expr, bindings: &PatternBindings) -> Result<usize, EvalError> {
    let mut names = Vec::new();
    collect_template_repeated_names(template, bindings, &mut names);

    let mut count = None;
    for name in names {
        let current = bindings.repeated.get(&name).map_or(0, Vec::len);
        match count {
            None => count = Some(current),
            Some(existing) if existing == current => {}
            Some(_) => {
                return Err(EvalError::at(
                    template.pos.line,
                    template.pos.column,
                    "template repetition length mismatch",
                ))
            }
        }
    }

    count.ok_or_else(|| {
        EvalError::at(
            template.pos.line,
            template.pos.column,
            "ellipsis template must reference a repeated pattern variable",
        )
    })
}

fn collect_template_repeated_names(
    template: &Expr,
    bindings: &PatternBindings,
    names: &mut Vec<String>,
) {
    match &template.kind {
        ExprKind::Symbol(name)
            if bindings.repeated.contains_key(name) && !names.contains(name) =>
        {
            names.push(name.clone());
        }
        ExprKind::List(parts) => {
            let mut index = 0usize;
            while index < parts.len() {
                if index + 1 < parts.len() && symbol_name(&parts[index + 1]) == Some("...") {
                    collect_template_repeated_names(&parts[index], bindings, names);
                    index += 2;
                } else {
                    collect_template_repeated_names(&parts[index], bindings, names);
                    index += 1;
                }
            }
        }
        _ => {}
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::String(text) => format_string(text),
        Value::Symbol(name) => name.clone(),
        Value::Pair(pair) => format_pair(pair),
        Value::EmptyList => "()".to_string(),
        Value::Procedure(_) => "#<procedure>".to_string(),
        Value::Void => String::new(),
    }
}

fn format_pair(pair: &Rc<Pair>) -> String {
    let mut out = String::from("(");
    let mut current = Some(pair.clone());
    while let Some(pair) = current {
        out.push_str(&format_value(&pair.car));
        match &pair.cdr {
            Value::EmptyList => {
                out.push(')');
                return out;
            }
            Value::Pair(next) => {
                out.push(' ');
                current = Some(next.clone());
            }
            other => {
                out.push_str(" . ");
                out.push_str(&format_value(other));
                out.push(')');
                return out;
            }
        }
    }
    out.push(')');
    out
}

fn format_string(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out.push('"');
    out
}
