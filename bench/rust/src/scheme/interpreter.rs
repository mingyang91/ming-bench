use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::advanced;
use super::error::{EvalError, SourcePosition};

type EvalResult<T> = Result<T, EvalError>;
type EnvRef = Rc<Env>;
type BuiltinFn = fn(&[Value], &mut EvalContext) -> EvalResult<Value>;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
enum ExprKind {
    Number(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Char(char),
    MutableString(Rc<RefCell<Vec<char>>>),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Void,
    Builtin(BuiltinProc),
    Lambda(Rc<UserProc>),
}

#[derive(Debug, Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Debug, Clone, Copy)]
struct BuiltinProc {
    name: &'static str,
    func: BuiltinFn,
}

#[derive(Debug, Clone)]
struct UserProc {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Debug, Clone)]
struct BindingSpec {
    name: String,
    init: Expr,
}

#[derive(Debug, Default)]
struct EvalContext {
    output: String,
}

#[derive(Debug)]
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
        name: "*",
        func: builtin_mul,
    },
    BuiltinProc {
        name: "-",
        func: builtin_sub,
    },
    BuiltinProc {
        name: "/",
        func: builtin_div,
    },
    BuiltinProc {
        name: "<",
        func: builtin_lt,
    },
    BuiltinProc {
        name: ">",
        func: builtin_gt,
    },
    BuiltinProc {
        name: "=",
        func: builtin_num_eq,
    },
    BuiltinProc {
        name: "<=",
        func: builtin_lte,
    },
    BuiltinProc {
        name: "append",
        func: builtin_append,
    },
    BuiltinProc {
        name: "boolean?",
        func: builtin_boolean_pred,
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
        name: "char?",
        func: builtin_char_pred,
    },
    BuiltinProc {
        name: "cons",
        func: builtin_cons,
    },
    BuiltinProc {
        name: "display",
        func: builtin_display,
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
        name: "newline",
        func: builtin_newline,
    },
    BuiltinProc {
        name: "not",
        func: builtin_not,
    },
    BuiltinProc {
        name: "null?",
        func: builtin_null_pred,
    },
    BuiltinProc {
        name: "number->string",
        func: builtin_number_to_string,
    },
    BuiltinProc {
        name: "number?",
        func: builtin_number_pred,
    },
    BuiltinProc {
        name: "pair?",
        func: builtin_pair_pred,
    },
    BuiltinProc {
        name: "string->number",
        func: builtin_string_to_number,
    },
    BuiltinProc {
        name: "string->symbol",
        func: builtin_string_to_symbol,
    },
    BuiltinProc {
        name: "string-append",
        func: builtin_string_append,
    },
    BuiltinProc {
        name: "string-copy",
        func: builtin_string_copy,
    },
    BuiltinProc {
        name: "string-length",
        func: builtin_string_length,
    },
    BuiltinProc {
        name: "string-ref",
        func: builtin_string_ref,
    },
    BuiltinProc {
        name: "string-set!",
        func: builtin_string_set,
    },
    BuiltinProc {
        name: "string?",
        func: builtin_string_pred,
    },
    BuiltinProc {
        name: "substring",
        func: builtin_substring,
    },
    BuiltinProc {
        name: "symbol->string",
        func: builtin_symbol_to_string,
    },
    BuiltinProc {
        name: "symbol?",
        func: builtin_symbol_pred,
    },
    BuiltinProc {
        name: "write",
        func: builtin_write,
    },
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
    eval_str_with_output(input).map(|(result, _)| result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    if needs_advanced_eval(input) {
        return advanced::eval_str_with_output(input);
    }

    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    if program.is_empty() {
        return Err(EvalError::new_with_position(
            "empty input",
            SourcePosition::new(1, 1),
        ));
    }

    let env = create_global_env();
    let mut context = EvalContext::default();
    let last_value = eval_sequence(&program, &env, &mut context)?;

    Ok((format_value(&last_value), context.output))
}

fn needs_advanced_eval(input: &str) -> bool {
    input.contains("call/cc")
        || input.contains("call-with-current-continuation")
        || input.contains("define-syntax")
}

fn create_global_env() -> EnvRef {
    let env = Env::new();
    for builtin in BUILTINS {
        env.define(builtin.name, Value::Builtin(*builtin));
    }
    env
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval_expr(expr, env, context)?;
    }
    Ok(result)
}

fn eval_expr(expr: &Expr, env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    let result = match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::new(format!("unbound symbol: {name}"))),
        ExprKind::List(items) => eval_list(items, env, context),
    };

    result.map_err(|error| error.with_position(expr.position))
}

fn eval_list(items: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    if items.is_empty() {
        return Err(EvalError::new("cannot evaluate empty list"));
    }

    let first = &items[0];
    if let ExprKind::Symbol(name) = &first.kind {
        match name.as_str() {
            "and" => return eval_and(&items[1..], env, context),
            "begin" => return eval_sequence(&items[1..], env, context),
            "cond" => return eval_cond(&items[1..], env, context),
            "define" => return eval_define(&items[1..], env, context),
            "if" => return eval_if(&items[1..], env, context),
            "lambda" => return eval_lambda(&items[1..], env),
            "let" => return eval_let(&items[1..], env, context),
            "or" => return eval_or(&items[1..], env, context),
            "quote" => return eval_quote(&items[1..]),
            "set!" => return eval_set(&items[1..], env, context),
            _ => {}
        }
    }

    let proc = eval_expr(first, env, context)?;
    if !is_procedure(&proc) {
        return Err(EvalError::new("attempted to call a non-procedure"));
    }

    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for item in &items[1..] {
        args.push(eval_expr(item, env, context)?);
    }

    apply_procedure(&proc, &args, first.position, context)
}

fn eval_and(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval_expr(arg, env, context)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    for arg in args {
        let value = eval_expr(arg, env, context)?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    for (index, clause_expr) in clauses.iter().enumerate() {
        let clause_items = match &clause_expr.kind {
            ExprKind::List(items) if !items.is_empty() => items,
            _ => return Err(EvalError::new("cond expects non-empty clauses")),
        };

        let test_expr = &clause_items[0];
        let body = &clause_items[1..];

        if let ExprKind::Symbol(name) = &test_expr.kind {
            if name == "else" {
                if index != clauses.len() - 1 {
                    return Err(EvalError::new("cond else clause must be last"));
                }
                return eval_sequence(body, env, context);
            }
        }

        let test_value = eval_expr(test_expr, env, context)?;
        if is_truthy(&test_value) {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, context)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_define(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    assert_at_least_arity("define", args.len(), 2)?;

    let target = &args[0];
    let body = &args[1..];

    match &target.kind {
        ExprKind::Symbol(name) => {
            assert_exact_arity("define", body.len(), 1)?;
            let value = eval_expr(&body[0], env, context)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(items) if !items.is_empty() => {
            let name = match &items[0].kind {
                ExprKind::Symbol(name) => name.clone(),
                _ => return Err(EvalError::new("define expects a symbol name")),
            };

            assert_at_least_arity("define", body.len(), 1)?;
            let lambda = Value::Lambda(Rc::new(UserProc {
                name: Some(name.clone()),
                params: parse_params(&items[1..])?,
                body: body.to_vec(),
                env: Rc::clone(env),
            }));
            env.define(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::new("define expects a symbol name")),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("if", args.len(), 3)?;
    if is_truthy(&eval_expr(&args[0], env, context)?) {
        eval_expr(&args[1], env, context)
    } else {
        eval_expr(&args[2], env, context)
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> EvalResult<Value> {
    assert_at_least_arity("lambda", args.len(), 2)?;

    let params_expr = &args[0];
    let items = match &params_expr.kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::new("lambda expects a parameter list")),
    };

    Ok(Value::Lambda(Rc::new(UserProc {
        name: None,
        params: parse_params(items)?,
        body: args[1..].to_vec(),
        env: Rc::clone(env),
    })))
}

fn eval_let(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    assert_at_least_arity("let", args.len(), 2)?;

    if let ExprKind::Symbol(name) = &args[0].kind {
        assert_at_least_arity("let", args.len(), 3)?;
        return eval_named_let(name, &args[1], &args[2..], env, context);
    }

    let bindings = parse_bindings(&args[0])?;
    let body = &args[1..];
    let mut values = Vec::with_capacity(bindings.len());
    for binding in &bindings {
        values.push(eval_expr(&binding.init, env, context)?);
    }

    let let_env = Env::child(env);
    for (binding, value) in bindings.iter().zip(values.into_iter()) {
        let_env.define(binding.name.clone(), value);
    }

    eval_sequence(body, &let_env, context)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let bindings = parse_bindings(bindings_expr)?;
    let mut values = Vec::with_capacity(bindings.len());
    for binding in &bindings {
        values.push(eval_expr(&binding.init, env, context)?);
    }

    let let_env = Env::child(env);
    let proc = Value::Lambda(Rc::new(UserProc {
        name: Some(name.to_owned()),
        params: bindings
            .iter()
            .map(|binding| binding.name.clone())
            .collect(),
        body: body.to_vec(),
        env: Rc::clone(&let_env),
    }));

    let let_env_for_define = Rc::clone(&let_env);
    let_env_for_define.define(name.to_owned(), proc.clone());
    apply_procedure(&proc, &values, bindings_expr.position, context)
}

fn eval_quote(args: &[Expr]) -> EvalResult<Value> {
    assert_exact_arity("quote", args.len(), 1)?;
    quote_expr(&args[0])
}

fn eval_set(args: &[Expr], env: &EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("set!", args.len(), 2)?;

    let name = match &args[0].kind {
        ExprKind::Symbol(name) => name,
        _ => return Err(EvalError::new("set! expects a symbol name")),
    };

    let value = eval_expr(&args[1], env, context)?;
    if env.set(name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::new(format!("unbound symbol: {name}")))
    }
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

fn apply_procedure(
    proc: &Value,
    args: &[Value],
    position: SourcePosition,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let result = match proc {
        Value::Builtin(builtin) => (builtin.func)(args, context),
        Value::Lambda(user_proc) => {
            let name = user_proc.name.as_deref().unwrap_or("lambda");
            assert_exact_arity(name, args.len(), user_proc.params.len())?;

            let call_env = Env::child(&user_proc.env);
            for (param, value) in user_proc.params.iter().zip(args.iter()) {
                call_env.define(param.clone(), value.clone());
            }

            eval_sequence(&user_proc.body, &call_env, context)
        }
        _ => Err(EvalError::new("attempted to call a non-procedure")),
    };

    result.map_err(|error| error.with_position(position))
}

fn builtin_add(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Number(sum_numbers("+", args, 0.0)?))
}

fn builtin_mul(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Number(product_numbers("*", args, 1.0)?))
}

fn builtin_sub(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Number(subtract_numbers(args)?))
}

fn builtin_div(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Number(divide_numbers(args)?))
}

fn builtin_lt(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(compare_numbers(
        "<",
        args,
        |left, right| left < right,
    )?))
}

fn builtin_gt(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(compare_numbers(
        ">",
        args,
        |left, right| left > right,
    )?))
}

fn builtin_num_eq(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(compare_numbers(
        "=",
        args,
        |left, right| left == right,
    )?))
}

fn builtin_lte(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(compare_numbers(
        "<=",
        args,
        |left, right| left <= right,
    )?))
}

fn builtin_append(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    append_values(args)
}

fn builtin_boolean_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate(
        "boolean?", args, is_boolean,
    )?))
}

fn builtin_car(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("car", args.len(), 1)?;
    Ok(expect_pair("car", &args[0])?.car.clone())
}

fn builtin_cdr(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("cdr", args.len(), 1)?;
    Ok(expect_pair("cdr", &args[0])?.cdr.clone())
}

fn builtin_char_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate(
        "char?",
        args,
        is_char_value,
    )?))
}

fn builtin_cons(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("cons", args.len(), 2)?;
    Ok(Value::Pair(Rc::new(PairValue {
        car: args[0].clone(),
        cdr: args[1].clone(),
    })))
}

fn builtin_display(args: &[Value], context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("display", args.len(), 1)?;
    context.output.push_str(&format_display_value(&args[0]));
    Ok(Value::Void)
}

fn builtin_length(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("length", args.len(), 1)?;
    Ok(Value::Number(
        expect_proper_list("length", &args[0])?.len() as f64
    ))
}

fn builtin_list(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(make_list(args.to_vec()))
}

fn builtin_newline(args: &[Value], context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("newline", args.len(), 0)?;
    context.output.push('\n');
    Ok(Value::Void)
}

fn builtin_not(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("not", args.len(), 1)?;
    Ok(Value::Boolean(!is_truthy(&args[0])))
}

fn builtin_null_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate(
        "null?",
        args,
        is_empty_list,
    )?))
}

fn builtin_number_to_string(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("number->string", args.len(), 1)?;
    Ok(make_mutable_string(&format_number(expect_number_value(
        "number->string",
        &args[0],
    )?)))
}

fn builtin_number_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate("number?", args, is_number)?))
}

fn builtin_pair_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate("pair?", args, is_pair)?))
}

fn builtin_string_to_number(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("string->number", args.len(), 1)?;
    Ok(parse_string_number(&expect_string_value(
        "string->number",
        &args[0],
    )?))
}

fn builtin_string_to_symbol(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("string->symbol", args.len(), 1)?;
    Ok(Value::Symbol(expect_string_value(
        "string->symbol",
        &args[0],
    )?))
}

fn builtin_string_append(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    let mut result = String::new();
    for arg in args {
        result.push_str(&expect_string_value("string-append", arg)?);
    }
    Ok(make_mutable_string(&result))
}

fn builtin_string_copy(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("string-copy", args.len(), 1)?;
    Ok(make_mutable_string(&expect_string_value(
        "string-copy",
        &args[0],
    )?))
}

fn builtin_string_length(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("string-length", args.len(), 1)?;
    Ok(Value::Number(
        string_chars(&expect_string_value("string-length", &args[0])?).len() as f64,
    ))
}

fn builtin_string_ref(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("string-ref", args.len(), 2)?;
    let chars = string_chars(&expect_string_value("string-ref", &args[0])?);
    let index = expect_index("string-ref", &args[1])?;
    if index >= chars.len() {
        return Err(EvalError::new("string-ref index out of bounds"));
    }
    Ok(Value::Char(chars[index]))
}

fn builtin_string_set(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("string-set!", args.len(), 3)?;
    let string_value = expect_mutable_string_value("string-set!", &args[0])?;
    let index = expect_index("string-set!", &args[1])?;
    let char_value = expect_char_value("string-set!", &args[2])?;

    let mut chars = string_value.borrow_mut();
    if index >= chars.len() {
        return Err(EvalError::new("string-set! index out of bounds"));
    }

    chars[index] = char_value;
    Ok(Value::Void)
}

fn builtin_string_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate(
        "string?",
        args,
        is_string_value,
    )?))
}

fn builtin_substring(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("substring", args.len(), 3)?;
    let chars = string_chars(&expect_string_value("substring", &args[0])?);
    let start = expect_index("substring", &args[1])?;
    let end = expect_index("substring", &args[2])?;

    if start > end || end > chars.len() {
        return Err(EvalError::new("substring expects valid start/end indices"));
    }

    let substring: String = chars[start..end].iter().collect();
    Ok(make_mutable_string(&substring))
}

fn builtin_symbol_to_string(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("symbol->string", args.len(), 1)?;
    Ok(make_mutable_string(&expect_symbol_value(
        "symbol->string",
        &args[0],
    )?))
}

fn builtin_symbol_pred(args: &[Value], _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(Value::Boolean(unary_predicate(
        "symbol?",
        args,
        is_symbol_value,
    )?))
}

fn builtin_write(args: &[Value], context: &mut EvalContext) -> EvalResult<Value> {
    assert_exact_arity("write", args.len(), 1)?;
    context.output.push_str(&format_value(&args[0]));
    Ok(Value::Void)
}

fn unary_predicate(name: &str, args: &[Value], predicate: fn(&Value) -> bool) -> EvalResult<bool> {
    assert_exact_arity(name, args.len(), 1)?;
    Ok(predicate(&args[0]))
}

fn sum_numbers(name: &str, args: &[Value], initial: f64) -> EvalResult<f64> {
    let numbers = expect_numbers(name, args)?;
    Ok(normalize_number(
        numbers.into_iter().fold(initial, |sum, value| sum + value),
    ))
}

fn product_numbers(name: &str, args: &[Value], initial: f64) -> EvalResult<f64> {
    let numbers = expect_numbers(name, args)?;
    Ok(normalize_number(
        numbers
            .into_iter()
            .fold(initial, |product, value| product * value),
    ))
}

fn subtract_numbers(args: &[Value]) -> EvalResult<f64> {
    let numbers = expect_numbers("-", args)?;
    assert_at_least_arity("-", numbers.len(), 1)?;

    if numbers.len() == 1 {
        return Ok(normalize_number(-numbers[0]));
    }

    let first = numbers[0];
    Ok(normalize_number(
        numbers[1..].iter().fold(first, |acc, value| acc - value),
    ))
}

fn divide_numbers(args: &[Value]) -> EvalResult<f64> {
    let numbers = expect_numbers("/", args)?;
    assert_at_least_arity("/", numbers.len(), 1)?;

    if numbers.len() == 1 {
        if numbers[0] == 0.0 {
            return Err(EvalError::new("division by zero"));
        }
        return Ok(normalize_number(1.0 / numbers[0]));
    }

    let mut result = numbers[0];
    for divisor in &numbers[1..] {
        if *divisor == 0.0 {
            return Err(EvalError::new("division by zero"));
        }
        result /= divisor;
    }

    Ok(normalize_number(result))
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

fn append_values(args: &[Value]) -> EvalResult<Value> {
    if args.is_empty() {
        return Ok(Value::EmptyList);
    }

    let mut result = args.last().cloned().expect("checked non-empty");
    for list in args[..args.len() - 1].iter().rev() {
        result = append_list(list, result)?;
    }
    Ok(result)
}

fn append_list(list: &Value, tail: Value) -> EvalResult<Value> {
    match list {
        Value::EmptyList => Ok(tail),
        Value::Pair(pair) => Ok(Value::Pair(Rc::new(PairValue {
            car: pair.car.clone(),
            cdr: append_list(&pair.cdr, tail)?,
        }))),
        _ => Err(EvalError::new("append expects list arguments")),
    }
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

fn expect_string_value(name: &str, value: &Value) -> EvalResult<String> {
    match value {
        Value::String(string) => Ok(string.clone()),
        Value::MutableString(chars) => Ok(chars.borrow().iter().collect()),
        _ => Err(EvalError::new(format!("{name} expects a string"))),
    }
}

fn expect_mutable_string_value(name: &str, value: &Value) -> EvalResult<Rc<RefCell<Vec<char>>>> {
    match value {
        Value::MutableString(chars) => Ok(Rc::clone(chars)),
        _ => Err(EvalError::new(format!("{name} expects a mutable string"))),
    }
}

fn expect_char_value(name: &str, value: &Value) -> EvalResult<char> {
    match value {
        Value::Char(ch) => Ok(*ch),
        _ => Err(EvalError::new(format!("{name} expects a character"))),
    }
}

fn expect_symbol_value(name: &str, value: &Value) -> EvalResult<String> {
    match value {
        Value::Symbol(symbol) => Ok(symbol.clone()),
        _ => Err(EvalError::new(format!("{name} expects a symbol"))),
    }
}

fn expect_index(name: &str, value: &Value) -> EvalResult<usize> {
    let index = expect_number_value(name, value)?;
    if index < 0.0 || index.fract() != 0.0 {
        return Err(EvalError::new(format!(
            "{name} expects a non-negative integer index"
        )));
    }
    Ok(index as usize)
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
    format_value_with_mode(value, FormatMode::Write)
}

fn format_display_value(value: &Value) -> String {
    format_value_with_mode(value, FormatMode::Display)
}

#[derive(Clone, Copy)]
enum FormatMode {
    Display,
    Write,
}

fn format_value_with_mode(value: &Value, mode: FormatMode) -> String {
    match value {
        Value::Number(number) => format_number(*number),
        Value::Boolean(true) => "#t".to_owned(),
        Value::Boolean(false) => "#f".to_owned(),
        Value::String(string) => match mode {
            FormatMode::Display => string.clone(),
            FormatMode::Write => escape_string(string),
        },
        Value::Char(ch) => match mode {
            FormatMode::Display => ch.to_string(),
            FormatMode::Write => format_char_literal(*ch),
        },
        Value::MutableString(chars) => {
            let contents: String = chars.borrow().iter().collect();
            match mode {
                FormatMode::Display => contents,
                FormatMode::Write => escape_string(&contents),
            }
        }
        Value::Symbol(symbol) => symbol.clone(),
        Value::EmptyList => "()".to_owned(),
        Value::Pair(pair) => format!("({})", format_pair_contents(pair, mode)),
        Value::Void => String::new(),
        Value::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
        Value::Lambda(user_proc) => match &user_proc.name {
            Some(name) => format!("#<procedure:{name}>"),
            None => "#<procedure>".to_owned(),
        },
    }
}

fn format_pair_contents(pair: &PairValue, mode: FormatMode) -> String {
    let mut parts = Vec::new();
    let mut current = Value::Pair(Rc::new(PairValue {
        car: pair.car.clone(),
        cdr: pair.cdr.clone(),
    }));

    loop {
        match current {
            Value::Pair(current_pair) => {
                parts.push(format_value_with_mode(&current_pair.car, mode));
                current = current_pair.cdr.clone();
            }
            Value::EmptyList => return parts.join(" "),
            other => {
                return format!(
                    "{} . {}",
                    parts.join(" "),
                    format_value_with_mode(&other, mode)
                )
            }
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

fn parse_string_number(value: &str) -> Value {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Value::Boolean(false);
    }

    match trimmed.parse::<f64>() {
        Ok(number) if number.is_finite() => Value::Number(normalize_number(number)),
        _ => Value::Boolean(false),
    }
}

fn string_chars(value: &str) -> Vec<char> {
    value.chars().collect()
}

fn make_mutable_string(value: &str) -> Value {
    Value::MutableString(Rc::new(RefCell::new(string_chars(value))))
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

fn is_boolean(value: &Value) -> bool {
    matches!(value, Value::Boolean(_))
}

fn is_number(value: &Value) -> bool {
    matches!(value, Value::Number(_))
}

fn is_procedure(value: &Value) -> bool {
    matches!(value, Value::Builtin(_) | Value::Lambda(_))
}

fn is_pair(value: &Value) -> bool {
    matches!(value, Value::Pair(_))
}

fn is_char_value(value: &Value) -> bool {
    matches!(value, Value::Char(_))
}

fn is_string_value(value: &Value) -> bool {
    matches!(value, Value::String(_) | Value::MutableString(_))
}

fn is_empty_list(value: &Value) -> bool {
    matches!(value, Value::EmptyList)
}

fn is_symbol_value(value: &Value) -> bool {
    matches!(value, Value::Symbol(_))
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'')
}
