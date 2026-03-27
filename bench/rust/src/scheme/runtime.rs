use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::scheme::error::{EvalError, SourcePos};
use crate::scheme::parser::{parse_program, Expr, ExprKind};

#[derive(Clone)]
enum Value {
    Number(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    Void,
    EmptyList,
    Pair(Rc<Pair>),
    Builtin(BuiltinProc),
    Closure(Rc<Closure>),
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

type BuiltinFn = fn(&[Value], SourcePos) -> Result<Value, EvalError>;

#[derive(Clone, Copy)]
struct BuiltinProc {
    name: &'static str,
    func: BuiltinFn,
}

#[derive(Clone)]
struct Closure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: Env,
}

#[derive(Clone)]
struct LetBinding {
    name: String,
    init: Expr,
}

#[derive(Clone)]
struct Env(Rc<EnvData>);

struct EnvData {
    parent: Option<Env>,
    vars: RefCell<HashMap<String, Value>>,
}

pub fn eval_input(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_program(input)?;
    let env = new_global_env();
    let mut last = Value::Void;

    for expr in &exprs {
        last = eval_expr(expr, &env)?;
    }

    Ok((last.scheme_string(), String::new()))
}

impl Value {
    fn scheme_string(&self) -> String {
        match self {
            Self::Number(number) => number.to_string(),
            Self::Bool(true) => "#t".to_string(),
            Self::Bool(false) => "#f".to_string(),
            Self::String(text) => format!("{text:?}"),
            Self::Symbol(name) => name.clone(),
            Self::Void => String::new(),
            Self::EmptyList => "()".to_string(),
            Self::Pair(pair) => pair.scheme_string(),
            Self::Builtin(proc) => format!("#<procedure:{}>", proc.name),
            Self::Closure(_) => "#<procedure>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }
}

impl Pair {
    fn scheme_string(&self) -> String {
        let mut parts = vec![self.car.scheme_string()];
        let mut tail = self.cdr.clone();

        loop {
            match tail {
                Value::EmptyList => return format!("({})", parts.join(" ")),
                Value::Pair(next) => {
                    parts.push(next.car.scheme_string());
                    tail = next.cdr.clone();
                }
                other => {
                    return format!("({} . {})", parts.join(" "), other.scheme_string());
                }
            }
        }
    }
}

impl Closure {
    fn call(&self, args: &[Value], call_pos: SourcePos) -> Result<Value, EvalError> {
        if args.len() != self.params.len() {
            return Err(EvalError::new(
                format!(
                    "expected {} arguments, got {}",
                    self.params.len(),
                    args.len()
                ),
                call_pos,
            ));
        }

        let call_env = Env::new(Some(self.env.clone()));
        for (name, value) in self.params.iter().zip(args.iter()) {
            call_env.define(name.clone(), value.clone());
        }

        eval_sequence(&self.body, &call_env)
    }
}

impl Env {
    fn new(parent: Option<Env>) -> Self {
        Self(Rc::new(EnvData {
            parent,
            vars: RefCell::new(HashMap::new()),
        }))
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.0.vars.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        let mut current = Some(self.clone());
        while let Some(env) = current {
            if let Some(value) = env.0.vars.borrow().get(name).cloned() {
                return Some(value);
            }
            current = env.0.parent.clone();
        }
        None
    }
}

fn new_global_env() -> Env {
    let env = Env::new(None);

    env.define("+", Value::Builtin(BuiltinProc { name: "+", func: eval_add }));
    env.define("-", Value::Builtin(BuiltinProc { name: "-", func: eval_sub }));
    env.define("*", Value::Builtin(BuiltinProc { name: "*", func: eval_mul }));
    env.define("/", Value::Builtin(BuiltinProc { name: "/", func: eval_div }));
    env.define(
        "<",
        Value::Builtin(BuiltinProc {
            name: "<",
            func: |args, pos| eval_compare(args, "<", pos, |left, right| left < right),
        }),
    );
    env.define(
        ">",
        Value::Builtin(BuiltinProc {
            name: ">",
            func: |args, pos| eval_compare(args, ">", pos, |left, right| left > right),
        }),
    );
    env.define(
        "=",
        Value::Builtin(BuiltinProc {
            name: "=",
            func: |args, pos| eval_compare(args, "=", pos, |left, right| left == right),
        }),
    );
    env.define(
        "<=",
        Value::Builtin(BuiltinProc {
            name: "<=",
            func: |args, pos| eval_compare(args, "<=", pos, |left, right| left <= right),
        }),
    );
    env.define(
        "not",
        Value::Builtin(BuiltinProc {
            name: "not",
            func: eval_not,
        }),
    );
    env.define(
        "cons",
        Value::Builtin(BuiltinProc {
            name: "cons",
            func: eval_cons,
        }),
    );
    env.define(
        "car",
        Value::Builtin(BuiltinProc {
            name: "car",
            func: eval_car,
        }),
    );
    env.define(
        "cdr",
        Value::Builtin(BuiltinProc {
            name: "cdr",
            func: eval_cdr,
        }),
    );
    env.define(
        "append",
        Value::Builtin(BuiltinProc {
            name: "append",
            func: eval_append,
        }),
    );
    env.define(
        "list",
        Value::Builtin(BuiltinProc {
            name: "list",
            func: eval_list_builtin,
        }),
    );
    env.define(
        "length",
        Value::Builtin(BuiltinProc {
            name: "length",
            func: eval_length,
        }),
    );
    env.define(
        "null?",
        Value::Builtin(BuiltinProc {
            name: "null?",
            func: |args, pos| eval_type_predicate(args, "null?", pos, |value| {
                matches!(value, Value::EmptyList)
            }),
        }),
    );
    env.define(
        "number?",
        Value::Builtin(BuiltinProc {
            name: "number?",
            func: |args, pos| eval_type_predicate(args, "number?", pos, |value| {
                matches!(value, Value::Number(_))
            }),
        }),
    );
    env.define(
        "boolean?",
        Value::Builtin(BuiltinProc {
            name: "boolean?",
            func: |args, pos| eval_type_predicate(args, "boolean?", pos, |value| {
                matches!(value, Value::Bool(_))
            }),
        }),
    );
    env.define(
        "string?",
        Value::Builtin(BuiltinProc {
            name: "string?",
            func: |args, pos| eval_type_predicate(args, "string?", pos, |value| {
                matches!(value, Value::String(_))
            }),
        }),
    );
    env.define(
        "pair?",
        Value::Builtin(BuiltinProc {
            name: "pair?",
            func: |args, pos| eval_type_predicate(args, "pair?", pos, |value| {
                matches!(value, Value::Pair(_))
            }),
        }),
    );
    env.define(
        "symbol?",
        Value::Builtin(BuiltinProc {
            name: "symbol?",
            func: |args, pos| eval_type_predicate(args, "symbol?", pos, |value| {
                matches!(value, Value::Symbol(_))
            }),
        }),
    );

    env
}

fn eval_sequence(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval_expr(expr, env)?;
    }
    Ok(last)
}

fn eval_expr(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(number) => Ok(Value::Number(*number)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::String(text) => Ok(Value::String(text.clone())),
        ExprKind::Symbol(name) => env.lookup(name).ok_or_else(|| {
            EvalError::new(format!("unbound variable: {name}"), expr.pos)
        }),
        ExprKind::List(items) => eval_list(items, env, expr.pos),
    }
}

fn eval_list(items: &[Expr], env: &Env, call_pos: SourcePos) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(EvalError::new("cannot evaluate empty list", call_pos));
    }

    if let ExprKind::Symbol(operator) = &items[0].kind {
        match operator.as_str() {
            "and" => return eval_and(&items[1..], env),
            "or" => return eval_or(&items[1..], env),
            "begin" => return eval_sequence(&items[1..], env),
            "if" => return eval_if(&items[1..], env, call_pos),
            "cond" => return eval_cond(&items[1..], env, call_pos),
            "define" => return eval_define(&items[1..], env, call_pos),
            "quote" => return eval_quote(&items[1..], call_pos),
            "let" => return eval_let(&items[1..], env, call_pos),
            "lambda" => return eval_lambda(&items[1..], env, call_pos),
            _ => {}
        }
    }

    let operator = eval_expr(&items[0], env)?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for item in &items[1..] {
        args.push(eval_expr(item, env)?);
    }

    match operator {
        Value::Builtin(proc) => (proc.func)(&args, call_pos),
        Value::Closure(closure) => closure.call(&args, call_pos),
        other => Err(EvalError::new(
            format!("attempt to call non-procedure: {}", other.scheme_string()),
            call_pos,
        )),
    }
}

fn eval_and(items: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Bool(true);
    for item in items {
        let next = eval_expr(item, env)?;
        if !next.is_truthy() {
            return Ok(next);
        }
        result = next;
    }
    Ok(result)
}

fn eval_or(items: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for item in items {
        let next = eval_expr(item, env)?;
        if next.is_truthy() {
            return Ok(next);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_if(parts: &[Expr], env: &Env, call_pos: SourcePos) -> Result<Value, EvalError> {
    if parts.len() != 2 && parts.len() != 3 {
        return Err(EvalError::new("'if' expects 2 or 3 arguments", call_pos));
    }

    let condition = eval_expr(&parts[0], env)?;
    if condition.is_truthy() {
        return eval_expr(&parts[1], env);
    }

    if let Some(else_branch) = parts.get(2) {
        return eval_expr(else_branch, env);
    }

    Ok(Value::Void)
}

fn eval_define(parts: &[Expr], env: &Env, call_pos: SourcePos) -> Result<Value, EvalError> {
    if parts.len() < 2 {
        return Err(EvalError::new(
            "'define' expects at least 2 arguments",
            call_pos,
        ));
    }

    match &parts[0].kind {
        ExprKind::Symbol(name) => {
            if parts.len() != 2 {
                return Err(EvalError::new(
                    "'define' expects exactly 2 arguments for variable definitions",
                    call_pos,
                ));
            }

            let value = eval_expr(&parts[1], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            if signature.is_empty() {
                return Err(EvalError::new("function name is required", parts[0].pos));
            }

            let name = match &signature[0].kind {
                ExprKind::Symbol(name) => name.clone(),
                _ => return Err(EvalError::new("function name must be a symbol", signature[0].pos)),
            };

            let params = parse_params(&signature[1..])?;
            let proc = Value::Closure(Rc::new(Closure {
                params,
                body: parts[1..].to_vec(),
                env: env.clone(),
            }));
            env.define(name, proc);
            Ok(Value::Void)
        }
        _ => Err(EvalError::new("invalid define target", parts[0].pos)),
    }
}

fn eval_quote(parts: &[Expr], call_pos: SourcePos) -> Result<Value, EvalError> {
    if parts.len() != 1 {
        return Err(EvalError::new("'quote' expects exactly 1 argument", call_pos));
    }
    quote_expr(&parts[0])
}

fn eval_lambda(parts: &[Expr], env: &Env, call_pos: SourcePos) -> Result<Value, EvalError> {
    if parts.len() < 2 {
        return Err(EvalError::new(
            "'lambda' expects a parameter list and body",
            call_pos,
        ));
    }

    let ExprKind::List(params_expr) = &parts[0].kind else {
        return Err(EvalError::new(
            "'lambda' parameter list must be a list",
            parts[0].pos,
        ));
    };

    Ok(Value::Closure(Rc::new(Closure {
        params: parse_params(params_expr)?,
        body: parts[1..].to_vec(),
        env: env.clone(),
    })))
}

fn eval_cond(clauses: &[Expr], env: &Env, call_pos: SourcePos) -> Result<Value, EvalError> {
    for (index, clause_expr) in clauses.iter().enumerate() {
        let ExprKind::List(clause) = &clause_expr.kind else {
            return Err(EvalError::new(
                "'cond' clauses must be non-empty lists",
                clause_expr.pos,
            ));
        };

        if clause.is_empty() {
            return Err(EvalError::new(
                "'cond' clauses must be non-empty lists",
                clause_expr.pos,
            ));
        }

        if let ExprKind::Symbol(keyword) = &clause[0].kind {
            if keyword == "else" {
                if index + 1 != clauses.len() {
                    return Err(EvalError::new(
                        "'cond' else clause must be last",
                        clause[0].pos,
                    ));
                }

                if clause.len() == 1 {
                    return Ok(Value::Void);
                }

                return eval_sequence(&clause[1..], env);
            }
        }

        let test = eval_expr(&clause[0], env)?;
        if test.is_truthy() {
            if clause.len() == 1 {
                return Ok(test);
            }
            return eval_sequence(&clause[1..], env);
        }
    }

    let _ = call_pos;
    Ok(Value::Void)
}

fn eval_let(parts: &[Expr], env: &Env, call_pos: SourcePos) -> Result<Value, EvalError> {
    if parts.len() < 2 {
        return Err(EvalError::new("'let' expects bindings and a body", call_pos));
    }

    if let ExprKind::Symbol(name) = &parts[0].kind {
        if parts.len() < 3 {
            return Err(EvalError::new(
                "named 'let' expects bindings and a body",
                call_pos,
            ));
        }

        let ExprKind::List(binding_exprs) = &parts[1].kind else {
            return Err(EvalError::new("'let' bindings must be a list", parts[1].pos));
        };

        let bindings = parse_let_bindings(binding_exprs)?;
        let mut args = Vec::with_capacity(bindings.len());
        let mut params = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            args.push(eval_expr(&binding.init, env)?);
            params.push(binding.name.clone());
        }

        let let_env = Env::new(Some(env.clone()));
        let proc = Rc::new(Closure {
            params,
            body: parts[2..].to_vec(),
            env: let_env.clone(),
        });
        let_env.define(name.clone(), Value::Closure(proc.clone()));
        proc.call(&args, call_pos)
    } else {
        let ExprKind::List(binding_exprs) = &parts[0].kind else {
            return Err(EvalError::new("'let' bindings must be a list", parts[0].pos));
        };

        let bindings = parse_let_bindings(binding_exprs)?;
        let let_env = Env::new(Some(env.clone()));
        for binding in bindings {
            let value = eval_expr(&binding.init, env)?;
            let_env.define(binding.name, value);
        }

        eval_sequence(&parts[1..], &let_env)
    }
}

fn parse_params(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());
    let mut seen = HashSet::with_capacity(items.len());

    for item in items {
        let ExprKind::Symbol(name) = &item.kind else {
            return Err(EvalError::new("parameter name must be a symbol", item.pos));
        };

        if !seen.insert(name.clone()) {
            return Err(EvalError::new(
                format!("duplicate parameter: {name}"),
                item.pos,
            ));
        }

        params.push(name.clone());
    }

    Ok(params)
}

fn parse_let_bindings(items: &[Expr]) -> Result<Vec<LetBinding>, EvalError> {
    let mut bindings = Vec::with_capacity(items.len());
    let mut seen = HashSet::with_capacity(items.len());

    for item in items {
        let ExprKind::List(binding) = &item.kind else {
            return Err(EvalError::new(
                "'let' bindings must be (name value) pairs",
                item.pos,
            ));
        };

        if binding.len() != 2 {
            return Err(EvalError::new(
                "'let' bindings must be (name value) pairs",
                item.pos,
            ));
        }

        let ExprKind::Symbol(name) = &binding[0].kind else {
            return Err(EvalError::new(
                "'let' binding names must be symbols",
                binding[0].pos,
            ));
        };

        if !seen.insert(name.clone()) {
            return Err(EvalError::new(
                format!("duplicate binding: {name}"),
                binding[0].pos,
            ));
        }

        bindings.push(LetBinding {
            name: name.clone(),
            init: binding[1].clone(),
        });
    }

    Ok(bindings)
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(number) => Ok(Value::Number(*number)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::String(text) => Ok(Value::String(text.clone())),
        ExprKind::Symbol(name) => Ok(Value::Symbol(name.clone())),
        ExprKind::List(items) => quote_list(items),
    }
}

fn quote_list(items: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::EmptyList;
    for item in items.iter().rev() {
        result = Value::Pair(Rc::new(Pair {
            car: quote_expr(item)?,
            cdr: result,
        }));
    }
    Ok(result)
}

fn eval_add(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut sum = 0_i64;
    for arg in args {
        sum += expect_number(arg, pos)?;
    }
    Ok(Value::Number(sum))
}

fn eval_sub(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::new("'-' expects at least 1 argument", pos));
    }

    let first = expect_number(&args[0], pos)?;
    if args.len() == 1 {
        return Ok(Value::Number(-first));
    }

    let mut result = first;
    for arg in &args[1..] {
        result -= expect_number(arg, pos)?;
    }
    Ok(Value::Number(result))
}

fn eval_mul(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut product = 1_i64;
    for arg in args {
        product *= expect_number(arg, pos)?;
    }
    Ok(Value::Number(product))
}

fn eval_div(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::new("'/' expects at least 2 arguments", pos));
    }

    let mut result = expect_number(&args[0], pos)?;
    for arg in &args[1..] {
        let divisor = expect_number(arg, pos)?;
        if divisor == 0 {
            return Err(EvalError::new("division by zero", pos));
        }
        result /= divisor;
    }

    Ok(Value::Number(result))
}

fn eval_compare(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    pred: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::new(
            format!("'{name}' expects at least 2 arguments"),
            pos,
        ));
    }

    let mut previous = expect_number(&args[0], pos)?;
    for arg in &args[1..] {
        let current = expect_number(arg, pos)?;
        if !pred(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }

    Ok(Value::Bool(true))
}

fn eval_not(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new("'not' expects exactly 1 argument", pos));
    }

    Ok(Value::Bool(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new("'cons' expects exactly 2 arguments", pos));
    }

    Ok(Value::Pair(Rc::new(Pair {
        car: args[0].clone(),
        cdr: args[1].clone(),
    })))
}

fn eval_car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new("'car' expects exactly 1 argument", pos));
    }

    match &args[0] {
        Value::Pair(pair) => Ok(pair.car.clone()),
        other => Err(EvalError::new(
            format!("'car' expects a pair, got {}", other.scheme_string()),
            pos,
        )),
    }
}

fn eval_cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new("'cdr' expects exactly 1 argument", pos));
    }

    match &args[0] {
        Value::Pair(pair) => Ok(pair.cdr.clone()),
        other => Err(EvalError::new(
            format!("'cdr' expects a pair, got {}", other.scheme_string()),
            pos,
        )),
    }
}

fn eval_append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::EmptyList);
    }

    if args.len() == 1 {
        return Ok(args[0].clone());
    }

    let mut elements = Vec::new();
    for arg in &args[..args.len() - 1] {
        elements.extend(proper_list_elements(arg, pos)?);
    }

    let mut result = args[args.len() - 1].clone();
    for value in elements.into_iter().rev() {
        result = Value::Pair(Rc::new(Pair {
            car: value,
            cdr: result,
        }));
    }

    Ok(result)
}

fn eval_list_builtin(args: &[Value], _pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = Value::EmptyList;
    for value in args.iter().rev() {
        result = Value::Pair(Rc::new(Pair {
            car: value.clone(),
            cdr: result,
        }));
    }
    Ok(result)
}

fn eval_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new("'length' expects exactly 1 argument", pos));
    }

    Ok(Value::Number(proper_list_length(&args[0], pos)? as i64))
}

fn eval_type_predicate(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    pred: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("'{name}' expects exactly 1 argument"),
            pos,
        ));
    }

    Ok(Value::Bool(pred(&args[0])))
}

fn proper_list_length(value: &Value, pos: SourcePos) -> Result<usize, EvalError> {
    Ok(proper_list_elements(value, pos)?.len())
}

fn proper_list_elements(value: &Value, pos: SourcePos) -> Result<Vec<Value>, EvalError> {
    let mut elements = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::EmptyList => return Ok(elements),
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            other => {
                return Err(EvalError::new(
                    format!("expected list, got {}", other.scheme_string()),
                    pos,
                ))
            }
        }
    }
}

fn expect_number(value: &Value, pos: SourcePos) -> Result<i64, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        other => Err(EvalError::new(
            format!("expected number, got {}", other.scheme_string()),
            pos,
        )),
    }
}
