use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::error::{EvalError, SourcePosition};

type EvalResult<T> = Result<T, EvalError>;
type EnvRef = Rc<Env>;
type BuiltinFn = fn(&[Value]) -> EvalResult<Value>;

#[derive(Clone)]
struct Expr {
    kind: ExprKind,
    position: SourcePosition,
}

impl Expr {
    fn symbol(name: impl Into<String>, position: SourcePosition) -> Self {
        Self {
            kind: ExprKind::Symbol(name.into()),
            position,
        }
    }
}

#[derive(Clone)]
enum ExprKind {
    Number(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Number(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Void,
    Builtin(BuiltinProc),
    Lambda(Rc<UserProc>),
    Continuation(Rc<ContinuationValue>),
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Copy)]
struct BuiltinProc {
    name: &'static str,
    func: BuiltinFn,
}

#[derive(Clone)]
struct UserProc {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct ContinuationValue {
    frames: Vec<Frame>,
}

#[derive(Clone)]
struct BindingSpec {
    name: String,
    init: Expr,
}

#[derive(Default)]
struct EvalContext {
    macros: HashMap<String, MacroDef>,
}

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

impl Env {
    fn new() -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        })
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(Rc::clone(parent)),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }

    fn set(&self, name: &str, value: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_owned(), value);
            return true;
        }

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
    }
}

#[derive(Clone)]
struct MacroDef {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
enum PatternCapture {
    Single(Expr),
    Repeat(Vec<Expr>),
}

#[derive(Clone)]
enum Control {
    Eval(Expr, EnvRef),
    Value(Value),
}

#[derive(Clone)]
enum Frame {
    ProcedureBoundary,
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Define {
        name: String,
        env: EnvRef,
    },
    Set {
        name: String,
        env: EnvRef,
    },
    If {
        then_expr: Expr,
        else_expr: Expr,
        env: EnvRef,
    },
    CallOp {
        arg_exprs: Vec<Expr>,
        env: EnvRef,
        position: SourcePosition,
    },
    CallArgs {
        proc: Value,
        pending: Vec<Expr>,
        evaluated: Vec<Value>,
        env: EnvRef,
        position: SourcePosition,
    },
    Let {
        bindings: Vec<BindingSpec>,
        index: usize,
        values: Vec<Value>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    NamedLet {
        name: String,
        bindings: Vec<BindingSpec>,
        index: usize,
        values: Vec<Value>,
        body: Vec<Expr>,
        env: EnvRef,
        position: SourcePosition,
    },
    CallCcReturn {
        void_stack: Option<Vec<Frame>>,
    },
    CallCc {
        position: SourcePosition,
    },
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
        let mut exprs = Vec::new();

        self.skip_ignored();
        while !self.is_at_end() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> EvalResult<Expr> {
        self.skip_ignored();

        if self.is_at_end() {
            return Err(self.error_here("unexpected end of input"));
        }

        let position = self.current_position();
        match self.peek_char().expect("checked is_at_end") {
            '\'' => {
                self.advance_char();
                Ok(Expr {
                    kind: ExprKind::List(vec![Expr::symbol("quote", position), self.parse_expr()?]),
                    position,
                })
            }
            '(' => self.parse_list(),
            ')' => Err(self.error_at("unexpected )", position)),
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> EvalResult<Expr> {
        let position = self.current_position();
        self.advance_char();

        let mut items = Vec::new();
        self.skip_ignored();

        while !self.is_at_end() && self.peek_char() != Some(')') {
            items.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if self.is_at_end() {
            return Err(self.error_at("unterminated list", position));
        }

        self.advance_char();
        Ok(Expr {
            kind: ExprKind::List(items),
            position,
        })
    }

    fn parse_string(&mut self) -> EvalResult<Expr> {
        let position = self.current_position();
        self.advance_char();

        let mut value = String::new();
        while let Some(ch) = self.peek_char() {
            let ch = self.advance_char();
            if ch == '"' {
                return Ok(Expr {
                    kind: ExprKind::String(value),
                    position,
                });
            }

            if ch == '\\' {
                let escaped = self
                    .peek_char()
                    .ok_or_else(|| self.error_at("unterminated string", position))?;
                let escaped = self.advance_char();
                match escaped {
                    '"' => value.push('"'),
                    '\\' => value.push('\\'),
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    other => value.push(other),
                }
            } else {
                value.push(ch);
            }
        }

        Err(self.error_at("unterminated string", position))
    }

    fn parse_atom(&mut self) -> EvalResult<Expr> {
        let position = self.current_position();
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if is_delimiter(ch) {
                break;
            }
            self.advance_char();
        }

        let token = &self.input[start..self.index];
        if token.is_empty() {
            return Err(self.error_at("expected expression", position));
        }

        if token == "#t" {
            return Ok(Expr {
                kind: ExprKind::Boolean(true),
                position,
            });
        }

        if token == "#f" {
            return Ok(Expr {
                kind: ExprKind::Boolean(false),
                position,
            });
        }

        if token.starts_with("#\\") {
            return parse_char_token(token, position);
        }

        if token != "+" && token != "-" {
            if let Ok(value) = token.parse::<i64>() {
                return Ok(Expr {
                    kind: ExprKind::Number(value as f64),
                    position,
                });
            }
        }

        Ok(Expr {
            kind: ExprKind::Symbol(token.to_owned()),
            position,
        })
    }

    fn skip_ignored(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.advance_char();
                continue;
            }

            if ch == ';' {
                while let Some(comment_char) = self.peek_char() {
                    if comment_char == '\n' {
                        break;
                    }
                    self.advance_char();
                }
                continue;
            }

            break;
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn advance_char(&mut self) -> char {
        let ch = self
            .peek_char()
            .expect("advance_char called at end of input");
        self.index += ch.len_utf8();

        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        ch
    }

    fn current_position(&self) -> SourcePosition {
        SourcePosition::new(self.line, self.column)
    }

    fn error_here(&self, message: impl Into<String>) -> EvalError {
        EvalError::new_with_position(message, self.current_position())
    }

    fn error_at(&self, message: impl Into<String>, position: SourcePosition) -> EvalError {
        EvalError::new_with_position(message, position)
    }
}

const BUILTINS: &[BuiltinProc] = &[
    BuiltinProc {
        name: "+",
        func: builtin_add,
    },
    BuiltinProc {
        name: "<",
        func: builtin_lt,
    },
    BuiltinProc {
        name: "car",
        func: builtin_car,
    },
    BuiltinProc {
        name: "cdr",
        func: builtin_cdr,
    },
    BuiltinProc {
        name: "cons",
        func: builtin_cons,
    },
    BuiltinProc {
        name: "length",
        func: builtin_length,
    },
    BuiltinProc {
        name: "list",
        func: builtin_list,
    },
    BuiltinProc {
        name: "null?",
        func: builtin_null_pred,
    },
];

pub(super) fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    if program.is_empty() {
        return Err(EvalError::new_with_position(
            "empty input",
            SourcePosition::new(1, 1),
        ));
    }

    let mut context = EvalContext::default();
    let result = eval_program(&program, &mut context)?;
    Ok((format_value(&result), String::new()))
}

fn create_global_env() -> EnvRef {
    let env = Env::new();
    for builtin in BUILTINS {
        env.define(builtin.name, Value::Builtin(*builtin));
    }
    env
}

fn eval_program(program: &[Expr], context: &mut EvalContext) -> EvalResult<Value> {
    let env = create_global_env();
    let mut stack = Vec::new();
    let mut control = set_sequence_control(program, env, &mut stack);

    loop {
        match control {
            Control::Eval(expr, env) => {
                let position = expr.position;
                control = eval_expr_step(expr, env, &mut stack, context)
                    .map_err(|error| error.with_position(position))?;
            }
            Control::Value(value) => {
                let Some(frame) = stack.pop() else {
                    return Ok(value);
                };
                control = handle_frame(frame, value, &mut stack, context)?;
            }
        }
    }
}

fn set_sequence_control(exprs: &[Expr], env: EnvRef, stack: &mut Vec<Frame>) -> Control {
    if exprs.is_empty() {
        return Control::Value(Value::Void);
    }

    if exprs.len() > 1 {
        stack.push(Frame::Sequence {
            remaining: exprs[1..].to_vec(),
            env: Rc::clone(&env),
        });
    }

    Control::Eval(exprs[0].clone(), env)
}

fn eval_expr_step(
    expr: Expr,
    env: EnvRef,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> EvalResult<Control> {
    match expr.kind {
        ExprKind::Number(value) => Ok(Control::Value(Value::Number(value))),
        ExprKind::Boolean(value) => Ok(Control::Value(Value::Boolean(value))),
        ExprKind::Char(value) => Ok(Control::Value(Value::Char(value))),
        ExprKind::String(value) => Ok(Control::Value(Value::String(value))),
        ExprKind::Symbol(name) => env
            .lookup(&name)
            .map(Control::Value)
            .ok_or_else(|| EvalError::new(format!("unbound symbol: {name}"))),
        ExprKind::List(items) => eval_list_step(expr.position, items, env, stack, context),
    }
}

fn eval_list_step(
    position: SourcePosition,
    items: Vec<Expr>,
    env: EnvRef,
    stack: &mut Vec<Frame>,
    context: &mut EvalContext,
) -> EvalResult<Control> {
    if items.is_empty() {
        return Err(EvalError::new("cannot evaluate empty list"));
    }

    let first = &items[0];
    if let ExprKind::Symbol(name) = &first.kind {
        if name == "define-syntax" {
            return eval_define_syntax_step(&items[1..], context);
        }

        if let Some(transformer) = context.macros.get(name).cloned() {
            let invocation = Expr {
                kind: ExprKind::List(items),
                position,
            };
            let expanded = expand_macro_invocation(&transformer, &invocation, &context.macros)?;
            return Ok(Control::Eval(expanded, env));
        }

        match name.as_str() {
            "begin" => return Ok(set_sequence_control(&items[1..], env, stack)),
            "define" => return eval_define_step(&items[1..], env, stack),
            "if" => return eval_if_step(&items[1..], env, stack),
            "lambda" => return eval_lambda_step(&items[1..], env),
            "let" => return eval_let_step(&items[1..], env, stack),
            "quote" => return eval_quote_step(&items[1..]),
            "set!" => return eval_set_step(&items[1..], env, stack),
            "call/cc" | "call-with-current-continuation" => {
                return eval_call_cc_step(&items[1..], env, stack, position);
            }
            _ => {}
        }
    }

    stack.push(Frame::CallOp {
        arg_exprs: items[1..].to_vec(),
        env: Rc::clone(&env),
        position: first.position,
    });
    Ok(Control::Eval(first.clone(), env))
}

fn eval_define_syntax_step(args: &[Expr], context: &mut EvalContext) -> EvalResult<Control> {
    assert_exact_arity("define-syntax", args.len(), 2)?;

    let name = match &args[0].kind {
        ExprKind::Symbol(name) => name.clone(),
        _ => return Err(EvalError::new("define-syntax expects a symbol name")),
    };

    let transformer = parse_syntax_rules(&name, &args[1])?;
    context.macros.insert(name, transformer);
    Ok(Control::Value(Value::Void))
}

fn eval_define_step(args: &[Expr], env: EnvRef, stack: &mut Vec<Frame>) -> EvalResult<Control> {
    assert_at_least_arity("define", args.len(), 2)?;

    let target = &args[0];
    let body = &args[1..];

    match &target.kind {
        ExprKind::Symbol(name) => {
            assert_exact_arity("define", body.len(), 1)?;
            let value_env = Rc::clone(&env);
            stack.push(Frame::Define {
                name: name.clone(),
                env,
            });
            Ok(Control::Eval(body[0].clone(), value_env))
        }
        ExprKind::List(items) if !items.is_empty() => {
            let name = match &items[0].kind {
                ExprKind::Symbol(name) => name.clone(),
                _ => return Err(EvalError::new("define expects a symbol name")),
            };

            let lambda = Value::Lambda(Rc::new(UserProc {
                name: Some(name.clone()),
                params: parse_params(&items[1..])?,
                body: body.to_vec(),
                env: Rc::clone(&env),
            }));
            env.define(name, lambda);
            Ok(Control::Value(Value::Void))
        }
        _ => Err(EvalError::new("define expects a symbol name")),
    }
}

fn eval_if_step(args: &[Expr], env: EnvRef, stack: &mut Vec<Frame>) -> EvalResult<Control> {
    assert_exact_arity("if", args.len(), 3)?;
    stack.push(Frame::If {
        then_expr: args[1].clone(),
        else_expr: args[2].clone(),
        env: Rc::clone(&env),
    });
    Ok(Control::Eval(args[0].clone(), env))
}

fn eval_lambda_step(args: &[Expr], env: EnvRef) -> EvalResult<Control> {
    assert_at_least_arity("lambda", args.len(), 2)?;

    let params_expr = &args[0];
    let items = match &params_expr.kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::new("lambda expects a parameter list")),
    };

    Ok(Control::Value(Value::Lambda(Rc::new(UserProc {
        name: None,
        params: parse_params(items)?,
        body: args[1..].to_vec(),
        env,
    }))))
}

fn eval_let_step(args: &[Expr], env: EnvRef, stack: &mut Vec<Frame>) -> EvalResult<Control> {
    assert_at_least_arity("let", args.len(), 2)?;

    if let ExprKind::Symbol(name) = &args[0].kind {
        assert_at_least_arity("let", args.len(), 3)?;
        let bindings = parse_bindings(&args[1])?;
        let body = args[2..].to_vec();
        if bindings.is_empty() {
            return apply_named_let(
                name,
                bindings,
                Vec::new(),
                body,
                env,
                args[1].position,
                stack,
            );
        }

        stack.push(Frame::NamedLet {
            name: name.clone(),
            bindings: bindings.clone(),
            index: 0,
            values: Vec::new(),
            body,
            env: Rc::clone(&env),
            position: args[1].position,
        });
        return Ok(Control::Eval(bindings[0].init.clone(), env));
    }

    let bindings = parse_bindings(&args[0])?;
    let body = args[1..].to_vec();
    if bindings.is_empty() {
        let let_env = Env::child(&env);
        return Ok(set_sequence_control(&body, let_env, stack));
    }

    stack.push(Frame::Let {
        bindings: bindings.clone(),
        index: 0,
        values: Vec::new(),
        body,
        env: Rc::clone(&env),
    });
    Ok(Control::Eval(bindings[0].init.clone(), env))
}

fn eval_quote_step(args: &[Expr]) -> EvalResult<Control> {
    assert_exact_arity("quote", args.len(), 1)?;
    Ok(Control::Value(quote_expr(&args[0])?))
}

fn eval_set_step(args: &[Expr], env: EnvRef, stack: &mut Vec<Frame>) -> EvalResult<Control> {
    assert_exact_arity("set!", args.len(), 2)?;

    let name = match &args[0].kind {
        ExprKind::Symbol(name) => name.clone(),
        _ => return Err(EvalError::new("set! expects a symbol name")),
    };

    stack.push(Frame::Set {
        name,
        env: Rc::clone(&env),
    });
    Ok(Control::Eval(args[1].clone(), env))
}

fn eval_call_cc_step(
    args: &[Expr],
    env: EnvRef,
    stack: &mut Vec<Frame>,
    position: SourcePosition,
) -> EvalResult<Control> {
    assert_exact_arity("call/cc", args.len(), 1)?;
    stack.push(Frame::CallCc { position });
    Ok(Control::Eval(args[0].clone(), env))
}

fn handle_frame(
    frame: Frame,
    value: Value,
    stack: &mut Vec<Frame>,
    _context: &mut EvalContext,
) -> EvalResult<Control> {
    match frame {
        Frame::Sequence { remaining, env } => Ok(set_sequence_control(&remaining, env, stack)),
        Frame::ProcedureBoundary => Ok(Control::Value(value)),
        Frame::Define { name, env } => {
            env.define(name, value);
            Ok(Control::Value(Value::Void))
        }
        Frame::Set { name, env } => {
            if env.set(&name, value) {
                Ok(Control::Value(Value::Void))
            } else {
                Err(EvalError::new(format!("unbound symbol: {name}")))
            }
        }
        Frame::If {
            then_expr,
            else_expr,
            env,
        } => {
            if is_truthy(&value) {
                Ok(Control::Eval(then_expr, env))
            } else {
                Ok(Control::Eval(else_expr, env))
            }
        }
        Frame::CallOp {
            arg_exprs,
            env,
            position,
        } => {
            if !is_procedure(&value) {
                return Err(
                    EvalError::new("attempted to call a non-procedure").with_position(position)
                );
            }

            if arg_exprs.is_empty() {
                return apply_procedure(value, Vec::new(), position, stack);
            }

            stack.push(Frame::CallArgs {
                proc: value,
                pending: arg_exprs[1..].to_vec(),
                evaluated: Vec::new(),
                env: Rc::clone(&env),
                position,
            });
            Ok(Control::Eval(arg_exprs[0].clone(), env))
        }
        Frame::CallArgs {
            proc,
            pending,
            mut evaluated,
            env,
            position,
        } => {
            evaluated.push(value);
            if pending.is_empty() {
                return apply_procedure(proc, evaluated, position, stack);
            }

            stack.push(Frame::CallArgs {
                proc,
                pending: pending[1..].to_vec(),
                evaluated,
                env: Rc::clone(&env),
                position,
            });
            Ok(Control::Eval(pending[0].clone(), env))
        }
        Frame::Let {
            bindings,
            index,
            mut values,
            body,
            env,
        } => {
            values.push(value);
            let next_index = index + 1;
            if next_index < bindings.len() {
                stack.push(Frame::Let {
                    bindings: bindings.clone(),
                    index: next_index,
                    values,
                    body,
                    env: Rc::clone(&env),
                });
                return Ok(Control::Eval(bindings[next_index].init.clone(), env));
            }

            let let_env = Env::child(&env);
            for (binding, bound_value) in bindings.iter().zip(values.into_iter()) {
                let_env.define(binding.name.clone(), bound_value);
            }
            Ok(set_sequence_control(&body, let_env, stack))
        }
        Frame::NamedLet {
            name,
            bindings,
            index,
            mut values,
            body,
            env,
            position,
        } => {
            values.push(value);
            let next_index = index + 1;
            if next_index < bindings.len() {
                stack.push(Frame::NamedLet {
                    name,
                    bindings: bindings.clone(),
                    index: next_index,
                    values,
                    body,
                    env: Rc::clone(&env),
                    position,
                });
                return Ok(Control::Eval(bindings[next_index].init.clone(), env));
            }

            apply_named_let(&name, bindings, values, body, env, position, stack)
        }
        Frame::CallCcReturn { void_stack } => {
            if matches!(value, Value::Void) {
                if let Some(void_stack) = void_stack {
                    *stack = void_stack;
                }
            }
            Ok(Control::Value(value))
        }
        Frame::CallCc { position } => {
            if !is_procedure(&value) {
                return Err(EvalError::new("call/cc expects a procedure").with_position(position));
            }

            let void_stack = procedure_return_stack(stack);

            let continuation = Value::Continuation(Rc::new(ContinuationValue {
                frames: stack.clone(),
            }));
            stack.push(Frame::CallCcReturn { void_stack });
            apply_procedure(value, vec![continuation], position, stack)
        }
    }
}

fn apply_named_let(
    name: &str,
    bindings: Vec<BindingSpec>,
    values: Vec<Value>,
    body: Vec<Expr>,
    env: EnvRef,
    position: SourcePosition,
    stack: &mut Vec<Frame>,
) -> EvalResult<Control> {
    let let_env = Env::child(&env);
    let proc = Value::Lambda(Rc::new(UserProc {
        name: Some(name.to_owned()),
        params: bindings
            .iter()
            .map(|binding| binding.name.clone())
            .collect(),
        body,
        env: Rc::clone(&let_env),
    }));
    let_env.define(name.to_owned(), proc.clone());
    apply_procedure(proc, values, position, stack)
}

fn apply_procedure(
    proc: Value,
    args: Vec<Value>,
    position: SourcePosition,
    stack: &mut Vec<Frame>,
) -> EvalResult<Control> {
    let result = match proc {
        Value::Builtin(builtin) => (builtin.func)(&args).map(Control::Value),
        Value::Lambda(user_proc) => {
            assert_exact_arity(
                user_proc.name.as_deref().unwrap_or("lambda"),
                args.len(),
                user_proc.params.len(),
            )?;

            let call_env = Env::child(&user_proc.env);
            for (param, value) in user_proc.params.iter().zip(args.into_iter()) {
                call_env.define(param.clone(), value);
            }
            stack.push(Frame::ProcedureBoundary);
            Ok(set_sequence_control(&user_proc.body, call_env, stack))
        }
        Value::Continuation(continuation) => {
            assert_exact_arity("continuation", args.len(), 1)?;
            *stack = continuation.frames.clone();
            Ok(Control::Value(args[0].clone()))
        }
        _ => Err(EvalError::new("attempted to call a non-procedure")),
    };

    result.map_err(|error| error.with_position(position))
}

fn parse_params(items: &[Expr]) -> EvalResult<Vec<String>> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        match &item.kind {
            ExprKind::Symbol(name) => params.push(name.clone()),
            _ => return Err(EvalError::new("lambda parameters must be symbols")),
        }
    }
    Ok(params)
}

fn procedure_return_stack(stack: &[Frame]) -> Option<Vec<Frame>> {
    let boundary_index = stack
        .iter()
        .rposition(|frame| matches!(frame, Frame::ProcedureBoundary))?;
    Some(stack[..boundary_index].to_vec())
}

fn parse_bindings(expr: &Expr) -> EvalResult<Vec<BindingSpec>> {
    let items = match &expr.kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::new("let expects a binding list")),
    };

    let mut bindings = Vec::with_capacity(items.len());
    for binding_expr in items {
        let binding_items = match &binding_expr.kind {
            ExprKind::List(binding_items) if binding_items.len() == 2 => binding_items,
            _ => return Err(EvalError::new("let bindings must be pairs")),
        };

        let name = match &binding_items[0].kind {
            ExprKind::Symbol(name) => name.clone(),
            _ => return Err(EvalError::new("let bindings must start with a symbol")),
        };

        bindings.push(BindingSpec {
            name,
            init: binding_items[1].clone(),
        });
    }

    Ok(bindings)
}

fn parse_syntax_rules(name: &str, expr: &Expr) -> EvalResult<MacroDef> {
    let items = match &expr.kind {
        ExprKind::List(items) if items.len() >= 3 => items,
        _ => {
            return Err(EvalError::new(
                "define-syntax expects a syntax-rules transformer",
            ))
        }
    };

    match &items[0].kind {
        ExprKind::Symbol(head) if head == "syntax-rules" => {}
        _ => {
            return Err(EvalError::new(
                "define-syntax expects a syntax-rules transformer",
            ))
        }
    }

    let literal_items = match &items[1].kind {
        ExprKind::List(items) => items,
        _ => {
            return Err(EvalError::new(
                "syntax-rules expects a literal identifier list",
            ))
        }
    };

    let mut literals = HashSet::new();
    for literal in literal_items {
        match &literal.kind {
            ExprKind::Symbol(name) => {
                literals.insert(name.clone());
            }
            _ => return Err(EvalError::new("syntax-rules literals must be identifiers")),
        }
    }

    let mut rules = Vec::with_capacity(items.len() - 2);
    for rule_expr in &items[2..] {
        let rule_items = match &rule_expr.kind {
            ExprKind::List(rule_items) if rule_items.len() == 2 => rule_items,
            _ => {
                return Err(EvalError::new(
                    "syntax-rules expects (pattern template) rules",
                ))
            }
        };
        rules.push(MacroRule {
            pattern: rule_items[0].clone(),
            template: rule_items[1].clone(),
        });
    }

    Ok(MacroDef {
        name: name.to_owned(),
        literals,
        rules,
    })
}

fn expand_macro_invocation(
    transformer: &MacroDef,
    invocation: &Expr,
    macros: &HashMap<String, MacroDef>,
) -> EvalResult<Expr> {
    let ExprKind::List(_) = invocation.kind else {
        return Err(EvalError::new("macro invocation must be a list"));
    };

    let mut literals = transformer.literals.clone();
    literals.insert(transformer.name.clone());

    for rule in &transformer.rules {
        let mut captures = HashMap::new();
        if !match_pattern(&rule.pattern, invocation, &literals, &mut captures, false)? {
            continue;
        }

        let expanded = expand_template(&rule.template, &captures, None)?;
        return expand_nested_macros(&expanded, macros);
    }

    Err(EvalError::new(format!(
        "no matching syntax-rules pattern for {}",
        transformer.name
    )))
}

fn expand_nested_macros(expr: &Expr, macros: &HashMap<String, MacroDef>) -> EvalResult<Expr> {
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(name) = &items[0].kind {
                if name == "quote" || name == "define-syntax" {
                    return Ok(expr.clone());
                }

                if let Some(transformer) = macros.get(name) {
                    let expanded = expand_macro_invocation(transformer, expr, macros)?;
                    return expand_nested_macros(&expanded, macros);
                }
            }

            let mut expanded_items = Vec::with_capacity(items.len());
            for item in items {
                expanded_items.push(expand_nested_macros(item, macros)?);
            }
            Ok(Expr {
                kind: ExprKind::List(expanded_items),
                position: expr.position,
            })
        }
        _ => Ok(expr.clone()),
    }
}

fn match_pattern(
    pattern: &Expr,
    expr: &Expr,
    literals: &HashSet<String>,
    captures: &mut HashMap<String, PatternCapture>,
    repeated_context: bool,
) -> EvalResult<bool> {
    match (&pattern.kind, &expr.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => Ok(left == right),
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => Ok(left == right),
        (ExprKind::Char(left), ExprKind::Char(right)) => Ok(left == right),
        (ExprKind::String(left), ExprKind::String(right)) => Ok(left == right),
        (ExprKind::Symbol(name), _) => {
            if name == "_" {
                return Ok(true);
            }
            if name == "..." {
                return Ok(false);
            }
            if literals.contains(name) {
                return Ok(matches!(&expr.kind, ExprKind::Symbol(other) if other == name));
            }

            if repeated_context {
                return Ok(add_repeated_capture(name, expr, captures));
            }

            Ok(add_single_capture(name, expr, captures))
        }
        (ExprKind::List(pattern_items), ExprKind::List(expr_items)) => match_list_pattern(
            pattern_items,
            expr_items,
            literals,
            captures,
            repeated_context,
        ),
        _ => Ok(false),
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    expr_items: &[Expr],
    literals: &HashSet<String>,
    captures: &mut HashMap<String, PatternCapture>,
    repeated_context: bool,
) -> EvalResult<bool> {
    let mut pattern_index = 0;
    let mut expr_index = 0;

    while pattern_index < pattern_items.len() {
        let current_pattern = &pattern_items[pattern_index];
        if is_ellipsis_symbol(current_pattern) {
            return Ok(false);
        }

        if pattern_index + 1 < pattern_items.len()
            && is_ellipsis_symbol(&pattern_items[pattern_index + 1])
        {
            if pattern_index + 2 != pattern_items.len() {
                return Err(EvalError::new(
                    "syntax-rules only supports trailing ellipsis patterns",
                ));
            }

            ensure_repeated_capture_slots(current_pattern, literals, captures)?;
            while expr_index < expr_items.len() {
                if !match_pattern(
                    current_pattern,
                    &expr_items[expr_index],
                    literals,
                    captures,
                    true,
                )? {
                    return Ok(false);
                }
                expr_index += 1;
            }

            return Ok(true);
        }

        if expr_index >= expr_items.len() {
            return Ok(false);
        }

        if !match_pattern(
            current_pattern,
            &expr_items[expr_index],
            literals,
            captures,
            repeated_context,
        )? {
            return Ok(false);
        }

        pattern_index += 1;
        expr_index += 1;
    }

    Ok(expr_index == expr_items.len())
}

fn add_single_capture(
    name: &str,
    expr: &Expr,
    captures: &mut HashMap<String, PatternCapture>,
) -> bool {
    match captures.get(name) {
        None => {
            captures.insert(name.to_owned(), PatternCapture::Single(expr.clone()));
            true
        }
        Some(PatternCapture::Single(existing)) => same_expr(existing, expr),
        Some(PatternCapture::Repeat(_)) => false,
    }
}

fn add_repeated_capture(
    name: &str,
    expr: &Expr,
    captures: &mut HashMap<String, PatternCapture>,
) -> bool {
    match captures.get_mut(name) {
        None => {
            captures.insert(name.to_owned(), PatternCapture::Repeat(vec![expr.clone()]));
            true
        }
        Some(PatternCapture::Repeat(exprs)) => {
            exprs.push(expr.clone());
            true
        }
        Some(PatternCapture::Single(_)) => false,
    }
}

fn ensure_repeated_capture_slots(
    pattern: &Expr,
    literals: &HashSet<String>,
    captures: &mut HashMap<String, PatternCapture>,
) -> EvalResult<()> {
    match &pattern.kind {
        ExprKind::Symbol(name) => {
            if name == "_" || name == "..." || literals.contains(name) {
                return Ok(());
            }

            match captures.get(name) {
                None => {
                    captures.insert(name.clone(), PatternCapture::Repeat(Vec::new()));
                }
                Some(PatternCapture::Repeat(_)) => {}
                Some(PatternCapture::Single(_)) => {
                    return Err(EvalError::new(
                        "syntax-rules pattern variable used inconsistently with ellipsis",
                    ));
                }
            }
            Ok(())
        }
        ExprKind::List(items) => {
            for item in items {
                if !is_ellipsis_symbol(item) {
                    ensure_repeated_capture_slots(item, literals, captures)?;
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn same_expr(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => left == right,
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left_items), ExprKind::List(right_items)) => {
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left_item, right_item)| same_expr(left_item, right_item))
        }
        _ => false,
    }
}

fn expand_template(
    expr: &Expr,
    captures: &HashMap<String, PatternCapture>,
    repetition_index: Option<usize>,
) -> EvalResult<Expr> {
    match &expr.kind {
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::String(_) => {
            Ok(expr.clone())
        }
        ExprKind::Symbol(name) => match captures.get(name) {
            Some(capture) => resolve_capture(capture, repetition_index),
            None => Ok(expr.clone()),
        },
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut index = 0;
            while index < items.len() {
                let item = &items[index];
                if is_ellipsis_symbol(item) {
                    return Err(EvalError::new(
                        "unexpected ellipsis in syntax-rules template",
                    ));
                }

                if index + 1 < items.len() && is_ellipsis_symbol(&items[index + 1]) {
                    let repeat_count = template_repetition_count(item, captures)?;
                    for repeat_index in 0..repeat_count {
                        expanded.push(expand_template(item, captures, Some(repeat_index))?);
                    }
                    index += 2;
                    continue;
                }

                expanded.push(expand_template(item, captures, repetition_index)?);
                index += 1;
            }

            Ok(Expr {
                kind: ExprKind::List(expanded),
                position: expr.position,
            })
        }
    }
}

fn resolve_capture(capture: &PatternCapture, repetition_index: Option<usize>) -> EvalResult<Expr> {
    match capture {
        PatternCapture::Single(expr) => Ok(expr.clone()),
        PatternCapture::Repeat(exprs) => {
            let Some(index) = repetition_index else {
                return Err(EvalError::new(
                    "syntax-rules template expected ellipsis for repeated pattern variable",
                ));
            };
            exprs
                .get(index)
                .cloned()
                .ok_or_else(|| EvalError::new("syntax-rules ellipsis repetition mismatch"))
        }
    }
}

fn template_repetition_count(
    template: &Expr,
    captures: &HashMap<String, PatternCapture>,
) -> EvalResult<usize> {
    let mut names = Vec::new();
    collect_repeated_capture_names(template, captures, &mut names);

    if names.is_empty() {
        return Err(EvalError::new(
            "syntax-rules ellipsis requires a repeated pattern variable",
        ));
    }

    let first_count = repeated_capture_length(
        captures
            .get(&names[0])
            .expect("repeated capture should be present"),
    );
    for name in &names[1..] {
        let count = repeated_capture_length(
            captures
                .get(name)
                .expect("repeated capture should be present"),
        );
        if count != first_count {
            return Err(EvalError::new(
                "syntax-rules ellipsis groups must repeat in lockstep",
            ));
        }
    }

    Ok(first_count)
}

fn collect_repeated_capture_names(
    template: &Expr,
    captures: &HashMap<String, PatternCapture>,
    names: &mut Vec<String>,
) {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if matches!(captures.get(name), Some(PatternCapture::Repeat(_)))
                && !names.iter().any(|existing| existing == name)
            {
                names.push(name.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_repeated_capture_names(item, captures, names);
            }
        }
        _ => {}
    }
}

fn repeated_capture_length(capture: &PatternCapture) -> usize {
    match capture {
        PatternCapture::Repeat(exprs) => exprs.len(),
        PatternCapture::Single(_) => 0,
    }
}

fn is_ellipsis_symbol(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(name) if name == "...")
}

fn quote_expr(expr: &Expr) -> EvalResult<Value> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => Ok(Value::Symbol(name.clone())),
        ExprKind::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_expr(item)?);
            }
            Ok(make_list(values))
        }
    }
}

fn builtin_add(args: &[Value]) -> EvalResult<Value> {
    Ok(Value::Number(sum_numbers("+", args, 0.0)?))
}

fn builtin_lt(args: &[Value]) -> EvalResult<Value> {
    Ok(Value::Boolean(compare_numbers(
        "<",
        args,
        |left, right| left < right,
    )?))
}

fn builtin_car(args: &[Value]) -> EvalResult<Value> {
    assert_exact_arity("car", args.len(), 1)?;
    Ok(expect_pair("car", &args[0])?.car.clone())
}

fn builtin_cdr(args: &[Value]) -> EvalResult<Value> {
    assert_exact_arity("cdr", args.len(), 1)?;
    Ok(expect_pair("cdr", &args[0])?.cdr.clone())
}

fn builtin_cons(args: &[Value]) -> EvalResult<Value> {
    assert_exact_arity("cons", args.len(), 2)?;
    Ok(Value::Pair(Rc::new(PairValue {
        car: args[0].clone(),
        cdr: args[1].clone(),
    })))
}

fn builtin_length(args: &[Value]) -> EvalResult<Value> {
    assert_exact_arity("length", args.len(), 1)?;
    Ok(Value::Number(
        expect_proper_list("length", &args[0])?.len() as f64
    ))
}

fn builtin_list(args: &[Value]) -> EvalResult<Value> {
    Ok(make_list(args.to_vec()))
}

fn builtin_null_pred(args: &[Value]) -> EvalResult<Value> {
    assert_exact_arity("null?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::EmptyList)))
}

fn sum_numbers(name: &str, args: &[Value], initial: f64) -> EvalResult<f64> {
    let numbers = expect_numbers(name, args)?;
    Ok(normalize_number(
        numbers.into_iter().fold(initial, |sum, value| sum + value),
    ))
}

fn compare_numbers(name: &str, args: &[Value], compare: fn(f64, f64) -> bool) -> EvalResult<bool> {
    let numbers = expect_numbers(name, args)?;
    assert_at_least_arity(name, numbers.len(), 2)?;

    for index in 1..numbers.len() {
        if !compare(numbers[index - 1], numbers[index]) {
            return Ok(false);
        }
    }

    Ok(true)
}

fn make_list(items: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;
    for item in items.into_iter().rev() {
        result = Value::Pair(Rc::new(PairValue {
            car: item,
            cdr: result,
        }));
    }
    result
}

fn expect_pair(name: &str, value: &Value) -> EvalResult<Rc<PairValue>> {
    match value {
        Value::Pair(pair) => Ok(Rc::clone(pair)),
        _ => Err(EvalError::new(format!("{name} expects a pair"))),
    }
}

fn expect_proper_list(name: &str, value: &Value) -> EvalResult<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                items.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            Value::EmptyList => return Ok(items),
            _ => return Err(EvalError::new(format!("{name} expects a proper list"))),
        }
    }
}

fn expect_numbers(name: &str, args: &[Value]) -> EvalResult<Vec<f64>> {
    let mut numbers = Vec::with_capacity(args.len());
    for arg in args {
        numbers.push(expect_number_value(name, arg)?);
    }
    Ok(numbers)
}

fn expect_number_value(name: &str, value: &Value) -> EvalResult<f64> {
    match value {
        Value::Number(number) => Ok(*number),
        _ => Err(EvalError::new(format!("{name} expects a number"))),
    }
}

fn assert_exact_arity(name: &str, actual: usize, expected: usize) -> EvalResult<()> {
    if actual != expected {
        return Err(EvalError::new(format!(
            "{name} expects exactly {expected} argument(s)"
        )));
    }
    Ok(())
}

fn assert_at_least_arity(name: &str, actual: usize, minimum: usize) -> EvalResult<()> {
    if actual < minimum {
        return Err(EvalError::new(format!(
            "{name} expects at least {minimum} argument(s)"
        )));
    }
    Ok(())
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Number(number) => format_number(*number),
        Value::Boolean(true) => "#t".to_owned(),
        Value::Boolean(false) => "#f".to_owned(),
        Value::Char(ch) => format_char_literal(*ch),
        Value::String(string) => escape_string(string),
        Value::Symbol(symbol) => symbol.clone(),
        Value::EmptyList => "()".to_owned(),
        Value::Pair(pair) => format!("({})", format_pair_contents(pair)),
        Value::Void => String::new(),
        Value::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
        Value::Lambda(user_proc) => match &user_proc.name {
            Some(name) => format!("#<procedure:{name}>"),
            None => "#<procedure>".to_owned(),
        },
        Value::Continuation(_) => "#<procedure>".to_owned(),
    }
}

fn format_pair_contents(pair: &PairValue) -> String {
    let mut parts = Vec::new();
    let mut current = Value::Pair(Rc::new(PairValue {
        car: pair.car.clone(),
        cdr: pair.cdr.clone(),
    }));

    loop {
        match current {
            Value::Pair(current_pair) => {
                parts.push(format_value(&current_pair.car));
                current = current_pair.cdr.clone();
            }
            Value::EmptyList => return parts.join(" "),
            other => return format!("{} . {}", parts.join(" "), format_value(&other)),
        }
    }
}

fn format_char_literal(value: char) -> String {
    match value {
        ' ' => "#\\space".to_owned(),
        '\n' => "#\\newline".to_owned(),
        other => format!("#\\{other}"),
    }
}

fn format_number(value: f64) -> String {
    let normalized = normalize_number(value);
    if normalized.fract() == 0.0 {
        (normalized as i64).to_string()
    } else {
        normalized.to_string()
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len() + 2);
    escaped.push('"');
    for ch in value.chars() {
        match ch {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            '\r' => escaped.push_str("\\r"),
            other => escaped.push(other),
        }
    }
    escaped.push('"');
    escaped
}

fn parse_char_token(token: &str, position: SourcePosition) -> EvalResult<Expr> {
    let raw_value = &token[2..];
    if raw_value.is_empty() {
        return Err(EvalError::new_with_position(
            "invalid character literal",
            position,
        ));
    }

    let lowered = raw_value.to_ascii_lowercase();
    match lowered.as_str() {
        "space" => {
            return Ok(Expr {
                kind: ExprKind::Char(' '),
                position,
            })
        }
        "newline" => {
            return Ok(Expr {
                kind: ExprKind::Char('\n'),
                position,
            })
        }
        _ => {}
    }

    let mut chars = raw_value.chars();
    let ch = chars
        .next()
        .ok_or_else(|| EvalError::new_with_position("invalid character literal", position))?;
    if chars.next().is_some() {
        return Err(EvalError::new_with_position(
            "invalid character literal",
            position,
        ));
    }

    Ok(Expr {
        kind: ExprKind::Char(ch),
        position,
    })
}

fn normalize_number(value: f64) -> f64 {
    if value == 0.0 {
        0.0
    } else {
        value
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

fn is_procedure(value: &Value) -> bool {
    matches!(
        value,
        Value::Builtin(_) | Value::Lambda(_) | Value::Continuation(_)
    )
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'')
}
