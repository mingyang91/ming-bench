use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::scheme::error::{EvalError, EvalResult};
use crate::scheme::parser::{parse_program, Expr, ExprKind, Span};

type BuiltinFn = fn(&mut Evaluator, &[Value], Span) -> EvalResult<Value>;
type CellRef = Rc<RefCell<Value>>;
type EnvRef = Rc<RefCell<Env>>;

pub(crate) struct Evaluator {
    global: EnvRef,
    output: String,
}

struct Env {
    parent: Option<EnvRef>,
    vars: HashMap<String, CellRef>,
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(Rc<RefCell<SchemeString>>),
    Char(char),
    Symbol(String),
    Pair(Rc<Pair>),
    Nil,
    Procedure(Procedure),
    Void,
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProc),
    Lambda(Rc<LambdaProc>),
}

#[derive(Clone, Copy)]
struct BuiltinProc {
    name: &'static str,
    func: BuiltinFn,
}

#[derive(Clone)]
struct LambdaProc {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Debug)]
struct SchemeString {
    text: String,
    mutable: bool,
}

impl Evaluator {
    pub(crate) fn new() -> Self {
        let global = Env::new(None);
        let mut evaluator = Self {
            global: global.clone(),
            output: String::new(),
        };
        evaluator.install_builtins();
        evaluator
    }

    pub(crate) fn eval(&mut self, input: &str) -> EvalResult<(String, String)> {
        let expressions = parse_program(input)?;
        let mut last = Value::Void;
        for expression in &expressions {
            last = self.eval_expr(expression, self.global.clone())?;
        }
        Ok((last.write_repr(), self.output.clone()))
    }

    fn install_builtins(&mut self) {
        let builtins: [(&str, BuiltinFn); 27] = [
            ("+", builtin_add),
            ("-", builtin_sub),
            ("*", builtin_mul),
            ("/", builtin_div),
            ("<", builtin_less_than),
            (">", builtin_greater_than),
            ("=", builtin_num_eq),
            ("<=", builtin_less_equal),
            ("not", builtin_not),
            ("cons", builtin_cons),
            ("car", builtin_car),
            ("cdr", builtin_cdr),
            ("null?", builtin_null_pred),
            ("list", builtin_list),
            ("length", builtin_length),
            ("append", builtin_append),
            ("number?", builtin_number_pred),
            ("boolean?", builtin_boolean_pred),
            ("string?", builtin_string_pred),
            ("pair?", builtin_pair_pred),
            ("symbol?", builtin_symbol_pred),
            ("char?", builtin_char_pred),
            ("display", builtin_display),
            ("write", builtin_write),
            ("newline", builtin_newline),
            ("string-append", builtin_string_append),
            ("string-length", builtin_string_length),
        ];

        for (name, func) in builtins {
            self.define_builtin(name, func);
        }

        for (name, func) in [
            ("substring", builtin_substring as BuiltinFn),
            ("string->number", builtin_string_to_number),
            ("number->string", builtin_number_to_string),
            ("symbol->string", builtin_symbol_to_string),
            ("string->symbol", builtin_string_to_symbol),
            ("string-ref", builtin_string_ref),
            ("string-copy", builtin_string_copy),
            ("string-set!", builtin_string_set),
        ] {
            self.define_builtin(name, func);
        }
    }

    fn define_builtin(&mut self, name: &'static str, func: BuiltinFn) {
        let value = Value::Procedure(Procedure::Builtin(BuiltinProc { name, func }));
        Env::define(&self.global, name.to_string(), value);
    }

    fn eval_expr(&mut self, expr: &Expr, env: EnvRef) -> EvalResult<Value> {
        match &expr.kind {
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::Int(value) => Ok(Value::Int(*value)),
            ExprKind::String(value) => Ok(Value::string(value.clone(), false)),
            ExprKind::Char(value) => Ok(Value::Char(*value)),
            ExprKind::Symbol(name) => Env::lookup(&env, name)
                .map(|cell| cell.borrow().clone())
                .ok_or_else(|| runtime_error(format!("unbound variable: {name}"), expr.span)),
            ExprKind::Quote(inner) => Ok(datum_to_value(inner)),
            ExprKind::List(items) => self.eval_list(items, expr.span, env),
        }
    }

    fn eval_list(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        let Some(first) = items.first() else {
            return Err(runtime_error("cannot evaluate empty list", span));
        };

        if let Some(name) = symbol_name(first) {
            match name {
                "quote" => return self.eval_quote(items, span),
                "if" => return self.eval_if(items, span, env),
                "define" => return self.eval_define(items, span, env),
                "lambda" => return self.eval_lambda(items, span, env),
                "begin" => return self.eval_sequence(&items[1..], env),
                "let" => return self.eval_let(items, span, env),
                "cond" => return self.eval_cond(items, span, env),
                "and" => return self.eval_and(items, env),
                "or" => return self.eval_or(items, env),
                "set!" => return self.eval_set(items, span, env),
                _ => {}
            }
        }

        let procedure = self.eval_expr(first, env.clone())?;
        let mut args = Vec::with_capacity(items.len().saturating_sub(1));
        for item in &items[1..] {
            args.push(self.eval_expr(item, env.clone())?);
        }
        self.apply(procedure, &args, span)
    }

    fn eval_quote(&mut self, items: &[Expr], span: Span) -> EvalResult<Value> {
        if items.len() != 2 {
            return Err(wrong_arg_count("quote", "1", items.len() - 1, span));
        }
        Ok(datum_to_value(&items[1]))
    }

    fn eval_if(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        if !(items.len() == 3 || items.len() == 4) {
            return Err(wrong_arg_count("if", "2 or 3", items.len() - 1, span));
        }
        let condition = self.eval_expr(&items[1], env.clone())?;
        if condition.is_truthy() {
            self.eval_expr(&items[2], env)
        } else if let Some(alternate) = items.get(3) {
            self.eval_expr(alternate, env)
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_define(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        if items.len() < 3 {
            return Err(runtime_error("define expects a binding and body", span));
        }

        match &items[1].kind {
            ExprKind::Symbol(name) => {
                if items.len() != 3 {
                    return Err(runtime_error(
                        "define variable form expects exactly one value expression",
                        span,
                    ));
                }
                let value = self.eval_expr(&items[2], env.clone())?;
                Env::define(&env, name.clone(), value);
            }
            ExprKind::List(signature) => {
                let Some(name_expr) = signature.first() else {
                    return Err(runtime_error(
                        "define function form needs a name",
                        items[1].span,
                    ));
                };
                let name = symbol_name(name_expr).ok_or_else(|| {
                    runtime_error("function name must be a symbol", name_expr.span)
                })?;
                let params = parse_param_list(&signature[1..])?;
                let lambda = Value::Procedure(Procedure::Lambda(Rc::new(LambdaProc {
                    params,
                    body: items[2..].to_vec(),
                    env: env.clone(),
                })));
                Env::define(&env, name.to_string(), lambda);
            }
            _ => {
                return Err(runtime_error(
                    "define expects a symbol or parameter list",
                    items[1].span,
                ))
            }
        }

        Ok(Value::Void)
    }

    fn eval_lambda(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        if items.len() < 3 {
            return Err(runtime_error("lambda expects parameters and body", span));
        }
        let params = parse_params(&items[1])?;
        Ok(Value::Procedure(Procedure::Lambda(Rc::new(LambdaProc {
            params,
            body: items[2..].to_vec(),
            env,
        }))))
    }

    fn eval_let(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        if items.len() < 3 {
            return Err(runtime_error("let expects bindings and body", span));
        }

        if let ExprKind::Symbol(name) = &items[1].kind {
            if items.len() < 4 {
                return Err(runtime_error("named let expects bindings and body", span));
            }
            let bindings = parse_bindings(&items[2])?;
            let mut args = Vec::with_capacity(bindings.len());
            let mut params = Vec::with_capacity(bindings.len());
            for (param, expr) in bindings {
                params.push(param);
                args.push(self.eval_expr(&expr, env.clone())?);
            }

            let recursion_env = Env::new(Some(env));
            let lambda = Value::Procedure(Procedure::Lambda(Rc::new(LambdaProc {
                params,
                body: items[3..].to_vec(),
                env: recursion_env.clone(),
            })));
            Env::define(&recursion_env, name.clone(), lambda.clone());
            return self.apply(lambda, &args, span);
        }

        let bindings = parse_bindings(&items[1])?;
        let child_env = Env::new(Some(env.clone()));
        for (name, value_expr) in bindings {
            let value = self.eval_expr(&value_expr, env.clone())?;
            Env::define(&child_env, name, value);
        }
        self.eval_sequence(&items[2..], child_env)
    }

    fn eval_cond(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        for clause in &items[1..] {
            let ExprKind::List(parts) = &clause.kind else {
                return Err(runtime_error("cond clauses must be lists", clause.span));
            };
            if parts.is_empty() {
                return Err(runtime_error("cond clause cannot be empty", clause.span));
            }
            if symbol_name(&parts[0]) == Some("else") {
                return self.eval_sequence(&parts[1..], env.clone());
            }
            let test = self.eval_expr(&parts[0], env.clone())?;
            if test.is_truthy() {
                if parts.len() == 1 {
                    return Ok(test);
                }
                return self.eval_sequence(&parts[1..], env.clone());
            }
        }
        if items.len() < 2 {
            return Err(runtime_error("cond expects at least one clause", span));
        }
        Ok(Value::Void)
    }

    fn eval_and(&mut self, items: &[Expr], env: EnvRef) -> EvalResult<Value> {
        let mut last = Value::Bool(true);
        for item in &items[1..] {
            let value = self.eval_expr(item, env.clone())?;
            if !value.is_truthy() {
                return Ok(value);
            }
            last = value;
        }
        Ok(last)
    }

    fn eval_or(&mut self, items: &[Expr], env: EnvRef) -> EvalResult<Value> {
        for item in &items[1..] {
            let value = self.eval_expr(item, env.clone())?;
            if value.is_truthy() {
                return Ok(value);
            }
        }
        Ok(Value::Bool(false))
    }

    fn eval_set(&mut self, items: &[Expr], span: Span, env: EnvRef) -> EvalResult<Value> {
        if items.len() != 3 {
            return Err(wrong_arg_count("set!", "2", items.len() - 1, span));
        }
        let ExprKind::Symbol(name) = &items[1].kind else {
            return Err(runtime_error("set! expects a symbol", items[1].span));
        };
        let value = self.eval_expr(&items[2], env.clone())?;
        let Some(cell) = Env::lookup(&env, name) else {
            return Err(runtime_error(
                format!("unbound variable: {name}"),
                items[1].span,
            ));
        };
        *cell.borrow_mut() = value;
        Ok(Value::Void)
    }

    fn eval_sequence(&mut self, expressions: &[Expr], env: EnvRef) -> EvalResult<Value> {
        let mut last = Value::Void;
        for expression in expressions {
            last = self.eval_expr(expression, env.clone())?;
        }
        Ok(last)
    }

    fn apply(&mut self, procedure: Value, args: &[Value], span: Span) -> EvalResult<Value> {
        match procedure {
            Value::Procedure(Procedure::Builtin(builtin)) => (builtin.func)(self, args, span),
            Value::Procedure(Procedure::Lambda(lambda)) => {
                if args.len() != lambda.params.len() {
                    return Err(wrong_arg_count(
                        "lambda",
                        &lambda.params.len().to_string(),
                        args.len(),
                        span,
                    ));
                }
                let call_env = Env::new(Some(lambda.env.clone()));
                for (name, value) in lambda.params.iter().zip(args.iter()) {
                    Env::define(&call_env, name.clone(), value.clone());
                }
                self.eval_sequence(&lambda.body, call_env)
            }
            other => Err(runtime_error(
                format!("attempted to call non-procedure {}", other.type_name()),
                span,
            )),
        }
    }
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            vars: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        let mut frame = env.borrow_mut();
        if let Some(existing) = frame.vars.get(&name) {
            *existing.borrow_mut() = value;
        } else {
            frame.vars.insert(name, Rc::new(RefCell::new(value)));
        }
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<CellRef> {
        let mut current = Some(env.clone());
        while let Some(frame_ref) = current {
            let frame = frame_ref.borrow();
            if let Some(value) = frame.vars.get(name) {
                return Some(value.clone());
            }
            current = frame.parent.clone();
        }
        None
    }
}

impl Value {
    fn string(text: String, mutable: bool) -> Self {
        Self::String(Rc::new(RefCell::new(SchemeString { text, mutable })))
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "number",
            Self::Bool(_) => "boolean",
            Self::String(_) => "string",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::Pair(_) => "pair",
            Self::Nil => "null",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn write_repr(&self) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(value) => {
                if *value {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            Self::String(text) => format!("\"{}\"", escape_string(&text.borrow().text)),
            Self::Char(ch) => format_char(*ch),
            Self::Symbol(name) => name.clone(),
            Self::Pair(_) => format_pair(self),
            Self::Nil => "()".to_string(),
            Self::Procedure(Procedure::Builtin(proc)) => format!("#<procedure:{}>", proc.name),
            Self::Procedure(Procedure::Lambda(_)) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }

    fn display_repr(&self) -> String {
        match self {
            Self::String(text) => text.borrow().text.clone(),
            Self::Char(ch) => ch.to_string(),
            _ => self.write_repr(),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.write_repr())
    }
}

fn format_pair(value: &Value) -> String {
    let mut output = String::from("(");
    let mut current = value.clone();
    let mut first = true;
    loop {
        match current {
            Value::Pair(pair) => {
                if !first {
                    output.push(' ');
                }
                output.push_str(&pair.car.write_repr());
                current = pair.cdr.clone();
                first = false;
            }
            Value::Nil => {
                output.push(')');
                return output;
            }
            other => {
                output.push_str(" . ");
                output.push_str(&other.write_repr());
                output.push(')');
                return output;
            }
        }
    }
}

fn escape_string(text: &str) -> String {
    let mut escaped = String::new();
    for ch in text.chars() {
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

fn format_char(ch: char) -> String {
    match ch {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

fn runtime_error(message: impl Into<String>, span: Span) -> EvalError {
    EvalError::with_position(message, span.line, span.col)
}

fn wrong_arg_count(name: &str, expected: &str, got: usize, span: Span) -> EvalError {
    runtime_error(format!("{name} expected {expected} args, got {got}"), span)
}

fn type_error(expected: &str, actual: &Value, span: Span) -> EvalError {
    runtime_error(
        format!("expected {expected}, got {}", actual.type_name()),
        span,
    )
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn parse_params(expr: &Expr) -> EvalResult<Vec<String>> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(runtime_error("lambda parameters must be a list", expr.span));
    };
    parse_param_list(items)
}

fn parse_param_list(items: &[Expr]) -> EvalResult<Vec<String>> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        let Some(name) = symbol_name(item) else {
            return Err(runtime_error("parameter names must be symbols", item.span));
        };
        params.push(name.to_string());
    }
    Ok(params)
}

fn parse_bindings(expr: &Expr) -> EvalResult<Vec<(String, Expr)>> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(runtime_error("bindings must be a list", expr.span));
    };
    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(parts) = &binding.kind else {
            return Err(runtime_error("binding must be a list", binding.span));
        };
        if parts.len() != 2 {
            return Err(runtime_error(
                "binding must have a name and value expression",
                binding.span,
            ));
        }
        let Some(name) = symbol_name(&parts[0]) else {
            return Err(runtime_error(
                "binding name must be a symbol",
                parts[0].span,
            ));
        };
        parsed.push((name.to_string(), parts[1].clone()));
    }
    Ok(parsed)
}

fn datum_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Bool(value) => Value::Bool(*value),
        ExprKind::Int(value) => Value::Int(*value),
        ExprKind::String(value) => Value::string(value.clone(), false),
        ExprKind::Char(value) => Value::Char(*value),
        ExprKind::Symbol(name) => Value::Symbol(name.clone()),
        ExprKind::List(items) => list_from_vec(items.iter().map(datum_to_value).collect()),
        ExprKind::Quote(inner) => list_from_vec(vec![
            Value::Symbol("quote".to_string()),
            datum_to_value(inner),
        ]),
    }
}

fn list_from_vec(items: Vec<Value>) -> Value {
    items.into_iter().rev().fold(Value::Nil, |tail, car| {
        Value::Pair(Rc::new(Pair { car, cdr: tail }))
    })
}

fn expect_int(value: &Value, span: Span) -> EvalResult<i64> {
    match value {
        Value::Int(number) => Ok(*number),
        _ => Err(type_error("number", value, span)),
    }
}

fn expect_index(value: &Value, span: Span) -> EvalResult<usize> {
    let number = expect_int(value, span)?;
    usize::try_from(number).map_err(|_| runtime_error("index must be non-negative", span))
}

fn expect_string_ref(value: &Value, span: Span) -> EvalResult<Rc<RefCell<SchemeString>>> {
    match value {
        Value::String(text) => Ok(text.clone()),
        _ => Err(type_error("string", value, span)),
    }
}

fn expect_string(value: &Value, span: Span) -> EvalResult<String> {
    Ok(expect_string_ref(value, span)?.borrow().text.clone())
}

fn expect_char(value: &Value, span: Span) -> EvalResult<char> {
    match value {
        Value::Char(ch) => Ok(*ch),
        _ => Err(type_error("char", value, span)),
    }
}

fn expect_pair(value: &Value, span: Span) -> EvalResult<Rc<Pair>> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        _ => Err(type_error("pair", value, span)),
    }
}

fn list_to_vec(value: &Value, span: Span, context: &str) -> EvalResult<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();
    loop {
        match current {
            Value::Nil => return Ok(items),
            Value::Pair(pair) => {
                items.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            _ => {
                return Err(runtime_error(
                    format!("{context} expects a proper list"),
                    span,
                ))
            }
        }
    }
}

fn builtin_add(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    let mut total = 0_i64;
    for arg in args {
        total = total
            .checked_add(expect_int(arg, span)?)
            .ok_or_else(|| runtime_error("integer overflow", span))?;
    }
    Ok(Value::Int(total))
}

fn builtin_sub(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    let Some((first, rest)) = args.split_first() else {
        return Err(wrong_arg_count("-", "at least 1", 0, span));
    };
    let first = expect_int(first, span)?;
    if rest.is_empty() {
        return Ok(Value::Int(-first));
    }
    let mut total = first;
    for arg in rest {
        total = total
            .checked_sub(expect_int(arg, span)?)
            .ok_or_else(|| runtime_error("integer overflow", span))?;
    }
    Ok(Value::Int(total))
}

fn builtin_mul(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    let mut total = 1_i64;
    for arg in args {
        total = total
            .checked_mul(expect_int(arg, span)?)
            .ok_or_else(|| runtime_error("integer overflow", span))?;
    }
    Ok(Value::Int(total))
}

fn builtin_div(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    let Some((first, rest)) = args.split_first() else {
        return Err(wrong_arg_count("/", "at least 1", 0, span));
    };
    if rest.is_empty() {
        return Err(wrong_arg_count("/", "at least 2", 1, span));
    }
    let mut total = expect_int(first, span)?;
    for arg in rest {
        let divisor = expect_int(arg, span)?;
        if divisor == 0 {
            return Err(runtime_error("division by zero", span));
        }
        total /= divisor;
    }
    Ok(Value::Int(total))
}

fn numeric_compare(
    args: &[Value],
    span: Span,
    predicate: impl Fn(i64, i64) -> bool,
) -> EvalResult<Value> {
    if args.len() < 2 {
        return Ok(Value::Bool(true));
    }
    let mut previous = expect_int(&args[0], span)?;
    for arg in &args[1..] {
        let current = expect_int(arg, span)?;
        if !predicate(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }
    Ok(Value::Bool(true))
}

fn builtin_less_than(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    numeric_compare(args, span, |left, right| left < right)
}

fn builtin_greater_than(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    numeric_compare(args, span, |left, right| left > right)
}

fn builtin_num_eq(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    numeric_compare(args, span, |left, right| left == right)
}

fn builtin_less_equal(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    numeric_compare(args, span, |left, right| left <= right)
}

fn builtin_not(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("not", "1", args.len(), span));
    }
    Ok(Value::Bool(!args[0].is_truthy()))
}

fn builtin_cons(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 2 {
        return Err(wrong_arg_count("cons", "2", args.len(), span));
    }
    Ok(Value::Pair(Rc::new(Pair {
        car: args[0].clone(),
        cdr: args[1].clone(),
    })))
}

fn builtin_car(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("car", "1", args.len(), span));
    }
    Ok(expect_pair(&args[0], span)?.car.clone())
}

fn builtin_cdr(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("cdr", "1", args.len(), span));
    }
    Ok(expect_pair(&args[0], span)?.cdr.clone())
}

fn builtin_null_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("null?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::Nil)))
}

fn builtin_list(_: &mut Evaluator, args: &[Value], _: Span) -> EvalResult<Value> {
    Ok(list_from_vec(args.to_vec()))
}

fn builtin_length(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("length", "1", args.len(), span));
    }
    let len = list_to_vec(&args[0], span, "length")?.len();
    Ok(Value::Int(len as i64))
}

fn builtin_append(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    let Some((last, lists)) = args.split_last() else {
        return Ok(Value::Nil);
    };
    let mut result = last.clone();
    for list in lists.iter().rev() {
        for item in list_to_vec(list, span, "append")?.into_iter().rev() {
            result = Value::Pair(Rc::new(Pair {
                car: item,
                cdr: result,
            }));
        }
    }
    Ok(result)
}

fn builtin_number_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("number?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::Int(_))))
}

fn builtin_boolean_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("boolean?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::Bool(_))))
}

fn builtin_string_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("string?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::String(_))))
}

fn builtin_pair_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("pair?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::Pair(_))))
}

fn builtin_symbol_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("symbol?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::Symbol(_))))
}

fn builtin_char_pred(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("char?", "1", args.len(), span));
    }
    Ok(Value::Bool(matches!(args[0], Value::Char(_))))
}

fn builtin_display(evaluator: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("display", "1", args.len(), span));
    }
    evaluator.output.push_str(&args[0].display_repr());
    Ok(Value::Void)
}

fn builtin_write(evaluator: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("write", "1", args.len(), span));
    }
    evaluator.output.push_str(&args[0].write_repr());
    Ok(Value::Void)
}

fn builtin_newline(evaluator: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if !args.is_empty() {
        return Err(wrong_arg_count("newline", "0", args.len(), span));
    }
    evaluator.output.push('\n');
    Ok(Value::Void)
}

fn builtin_string_append(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    let mut combined = String::new();
    for arg in args {
        combined.push_str(&expect_string(arg, span)?);
    }
    Ok(Value::string(combined, false))
}

fn builtin_string_length(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("string-length", "1", args.len(), span));
    }
    let len = expect_string(&args[0], span)?.chars().count() as i64;
    Ok(Value::Int(len))
}

fn builtin_substring(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 3 {
        return Err(wrong_arg_count("substring", "3", args.len(), span));
    }
    let text = expect_string(&args[0], span)?;
    let start = expect_index(&args[1], span)?;
    let end = expect_index(&args[2], span)?;
    let chars: Vec<char> = text.chars().collect();
    if start > end || end > chars.len() {
        return Err(runtime_error("substring indices out of range", span));
    }
    let slice: String = chars[start..end].iter().collect();
    Ok(Value::string(slice, false))
}

fn builtin_string_to_number(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("string->number", "1", args.len(), span));
    }
    let text = expect_string(&args[0], span)?;
    match text.parse::<i64>() {
        Ok(number) => Ok(Value::Int(number)),
        Err(_) => Ok(Value::Bool(false)),
    }
}

fn builtin_number_to_string(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("number->string", "1", args.len(), span));
    }
    Ok(Value::string(
        expect_int(&args[0], span)?.to_string(),
        false,
    ))
}

fn builtin_symbol_to_string(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("symbol->string", "1", args.len(), span));
    }
    match &args[0] {
        Value::Symbol(symbol) => Ok(Value::string(symbol.clone(), false)),
        value => Err(type_error("symbol", value, span)),
    }
}

fn builtin_string_to_symbol(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("string->symbol", "1", args.len(), span));
    }
    Ok(Value::Symbol(expect_string(&args[0], span)?))
}

fn builtin_string_ref(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 2 {
        return Err(wrong_arg_count("string-ref", "2", args.len(), span));
    }
    let text = expect_string(&args[0], span)?;
    let index = expect_index(&args[1], span)?;
    let chars: Vec<char> = text.chars().collect();
    let Some(ch) = chars.get(index) else {
        return Err(runtime_error("string-ref index out of range", span));
    };
    Ok(Value::Char(*ch))
}

fn builtin_string_copy(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 1 {
        return Err(wrong_arg_count("string-copy", "1", args.len(), span));
    }
    Ok(Value::string(expect_string(&args[0], span)?, true))
}

fn builtin_string_set(_: &mut Evaluator, args: &[Value], span: Span) -> EvalResult<Value> {
    if args.len() != 3 {
        return Err(wrong_arg_count("string-set!", "3", args.len(), span));
    }
    let string_ref = expect_string_ref(&args[0], span)?;
    let index = expect_index(&args[1], span)?;
    let ch = expect_char(&args[2], span)?;
    let mut string = string_ref.borrow_mut();
    if !string.mutable {
        return Err(runtime_error("string is immutable", span));
    }
    let mut chars: Vec<char> = string.text.chars().collect();
    let Some(slot) = chars.get_mut(index) else {
        return Err(runtime_error("string-set! index out of range", span));
    };
    *slot = ch;
    string.text = chars.into_iter().collect();
    Ok(Value::Void)
}
