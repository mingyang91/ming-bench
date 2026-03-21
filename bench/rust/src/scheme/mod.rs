pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

thread_local! {
    static CALLCC_PENDING: RefCell<Option<Value>> = RefCell::new(None);
    static CALLCC_TOP_IDX: Cell<usize> = Cell::new(0);
    static CONT_RETURN: RefCell<Option<(usize, Value)>> = RefCell::new(None);
}

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}##{}", base, n)
}

/// Source position (1-indexed line, 0-indexed column).
#[derive(Debug, Clone, Copy)]
struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn fmt(&self) -> String {
        format!("{}:{}", self.line, self.col)
    }
}

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Rc<Vec<String>>,
        rest_param: Option<String>,
        body: Rc<Vec<Expr>>,
        env: Env,
    },
    Builtin(String),
    Continuation(usize),
    Macro {
        literals: Rc<Vec<String>>,
        rules: Rc<Vec<(Expr, Expr)>>,
        def_env: Env,
    },
}

impl Value {
    /// Format for `write` and return values (strings get quotes).
    fn display(&self) -> String {
        self.fmt_value(true)
    }

    /// Format for `display` output (strings without quotes).
    fn display_unquoted(&self) -> String {
        self.fmt_value(false)
    }

    fn fmt_value(&self, quote_strings: bool) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => {
                if quote_strings {
                    format!("\"{}\"", s)
                } else {
                    s.clone()
                }
            }
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::Nil => "()".to_string(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first {
                                out.push(' ');
                            }
                            first = false;
                            out.push_str(&car.fmt_value(quote_strings));
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.fmt_value(quote_strings));
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Lambda { .. } => "<procedure>".to_string(),
            Value::Builtin(name) => format!("<builtin:{}>", name),
            Value::Continuation(_) => "<continuation>".to_string(),
            Value::Macro { .. } => "<macro>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn is_builtin_name(name: &str) -> bool {
        matches!(
            name,
            "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
                | "display" | "write" | "newline"
                | "string-append" | "string-length" | "substring"
                | "string->number" | "number->string" | "symbol->string" | "string->symbol"
                | "string-copy" | "string-ref" | "string-set!"
                | "apply" | "call/cc"
        )
    }

    fn as_integer(&self, pos: Pos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type(format!(
                "expected integer, got {} at {}",
                other.display(),
                pos.fmt()
            ))),
        }
    }
}

/// A parsed S-expression with source position.
#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: Pos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Expr>),
}

// ── Environment ────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Env(Rc<RefCell<EnvInner>>);

#[derive(Debug)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    fn with_parent(parent: &Env) -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(parent.clone()),
        })))
    }

    fn get(&self, name: &str) -> Option<Value> {
        let inner = self.0.borrow();
        if let Some(val) = inner.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = inner.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    /// Mutate an existing binding in the nearest scope that contains it.
    fn set_existing(&self, name: &str, val: Value) -> bool {
        let has_key = self.0.borrow().bindings.contains_key(name);
        if has_key {
            self.0.borrow_mut().bindings.insert(name.to_string(), val);
            true
        } else {
            let parent = self.0.borrow().parent.clone();
            if let Some(parent) = parent {
                parent.set_existing(name, val)
            } else {
                false
            }
        }
    }

    /// Copy all bindings from this env chain into target, skipping any
    /// that already exist in target's chain (preserves lexical scoping).
    fn copy_all_into_if_absent(&self, target: &Env) {
        let inner = self.0.borrow();
        for (k, v) in &inner.bindings {
            if target.get(k).is_none() {
                target.set(k.clone(), v.clone());
            }
        }
        if let Some(ref parent) = inner.parent {
            parent.copy_all_into_if_absent(target);
        }
    }
}

// ── Parser ──────────────────────────────────────────────────────────

struct Parser {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 0,
        }
    }

    fn current_pos(&self) -> Pos {
        Pos {
            line: self.line,
            col: self.col,
        }
    }

    fn advance(&mut self) {
        if self.pos < self.chars.len() {
            if self.chars[self.pos] == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.advance();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.advance();
        }
        c
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        let start = self.current_pos();
        match self.peek() {
            None => Err(EvalError::Parse(format!(
                "unexpected end of input at {}",
                start.fmt()
            ))),
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.next_char();
                let inner = self.parse_expr()?;
                Ok(Expr {
                    kind: ExprKind::List(vec![
                        Expr {
                            kind: ExprKind::Symbol("quote".into()),
                            pos: start,
                        },
                        inner,
                    ]),
                    pos: start,
                })
            }
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => {
                    return Err(EvalError::Parse(format!(
                        "unterminated list at {}",
                        start.fmt()
                    )))
                }
                Some(')') => {
                    self.next_char();
                    return Ok(Expr {
                        kind: ExprKind::List(items),
                        pos: start,
                    });
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume opening "
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => {
                    return Err(EvalError::Parse(format!(
                        "unterminated string at {}",
                        start.fmt()
                    )))
                }
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => {
                        return Err(EvalError::Parse(format!(
                            "unterminated escape at {}",
                            start.fmt()
                        )))
                    }
                },
                Some('"') => {
                    return Ok(Expr {
                        kind: ExprKind::Str(s),
                        pos: start,
                    })
                }
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume '#'
        match self.peek() {
            Some('t') => {
                self.next_char();
                Ok(Expr {
                    kind: ExprKind::Boolean(true),
                    pos: start,
                })
            }
            Some('f') => {
                self.next_char();
                Ok(Expr {
                    kind: ExprKind::Boolean(false),
                    pos: start,
                })
            }
            Some('\\') => {
                self.next_char(); // consume '\'
                match self.next_char() {
                    Some(c) => Ok(Expr {
                        kind: ExprKind::Char(c),
                        pos: start,
                    }),
                    None => Err(EvalError::Parse(format!(
                        "unterminated character literal at {}",
                        start.fmt()
                    ))),
                }
            }
            _ => Err(EvalError::Parse(format!(
                "invalid # literal at {}",
                start.fmt()
            ))),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        let char_start = self.pos;
        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            self.advance();
        }
        let token: String = self.chars[char_start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(EvalError::Parse(format!("empty token at {}", start.fmt())));
        }
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr {
                kind: ExprKind::Integer(n),
                pos: start,
            });
        }
        Ok(Expr {
            kind: ExprKind::Symbol(token),
            pos: start,
        })
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// ── Evaluator ───────────────────────────────────────────────────────

fn eval(expr: &Expr, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    let result = 'tco: loop {
        let pos = cur_expr.pos;
        match cur_expr.kind.clone() {
            ExprKind::Integer(n) => break 'tco Ok(Value::Integer(n)),
            ExprKind::Boolean(b) => break 'tco Ok(Value::Boolean(b)),
            ExprKind::Str(s) => break 'tco Ok(Value::Str(s)),
            ExprKind::Char(c) => break 'tco Ok(Value::Char(c)),
            ExprKind::Symbol(name) => {
                if let Some(val) = cur_env.get(&name) {
                    break 'tco Ok(val);
                } else if Value::is_builtin_name(&name) {
                    break 'tco Ok(Value::Builtin(name));
                } else {
                    break 'tco Err(EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt())));
                }
            }
            ExprKind::List(items) => {
                if items.is_empty() {
                    break 'tco Err(EvalError::Parse(format!(
                        "empty application at {}",
                        pos.fmt()
                    )));
                }
                // Check for special forms
                if let ExprKind::Symbol(ref op) = items[0].kind {
                    match op.as_str() {
                        "if" => {
                            let args = &items[1..];
                            if args.len() < 2 || args.len() > 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "if requires 2 or 3 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let cond = eval(&args[0], &cur_env, out)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                                continue 'tco;
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                                continue 'tco;
                            } else {
                                break 'tco Ok(Value::Nil);
                            }
                        }
                        "define" => {
                            break 'tco eval_define(&items[1..], pos, &cur_env, out);
                        }
                        "quote" => {
                            break 'tco eval_quote(&items[1..], pos);
                        }
                        "lambda" => {
                            break 'tco eval_lambda(&items[1..], pos, &cur_env);
                        }
                        "and" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                break 'tco Ok(Value::Boolean(true));
                            }
                            for a in &args[..args.len() - 1] {
                                let v = eval(a, &cur_env, out)?;
                                if !v.is_truthy() {
                                    break 'tco Ok(v);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue 'tco;
                        }
                        "or" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                break 'tco Ok(Value::Boolean(false));
                            }
                            for a in &args[..args.len() - 1] {
                                let v = eval(a, &cur_env, out)?;
                                if v.is_truthy() {
                                    break 'tco Ok(v);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue 'tco;
                        }
                        "let" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "let requires bindings and body at {}",
                                    pos.fmt()
                                )));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let ExprKind::Symbol(ref loop_name) = args[0].kind {
                                if args.len() < 3 {
                                    break 'tco Err(EvalError::Arity(format!(
                                        "named let requires bindings and body at {}",
                                        pos.fmt()
                                    )));
                                }
                                let bindings_list = match &args[1].kind {
                                    ExprKind::List(bs) => bs,
                                    _ => break 'tco Err(EvalError::Type(format!(
                                        "let: bindings must be a list at {}",
                                        pos.fmt()
                                    ))),
                                };
                                let mut params = Vec::new();
                                let mut init_vals = Vec::new();
                                for b in bindings_list {
                                    match &b.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => break 'tco Err(EvalError::Type(format!(
                                                    "let: binding name must be symbol at {}",
                                                    pair[0].pos.fmt()
                                                ))),
                                            };
                                            let val = eval(&pair[1], &cur_env, out)?;
                                            params.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => break 'tco Err(EvalError::Type(format!(
                                            "let: invalid binding at {}",
                                            b.pos.fmt()
                                        ))),
                                    }
                                }
                                let body = Rc::new(args[2..].to_vec());
                                let new_env = Env::with_parent(&cur_env);
                                for (p, v) in params.iter().zip(init_vals.into_iter()) {
                                    new_env.set(p.clone(), v);
                                }
                                let lambda = Value::Lambda {
                                    params: Rc::new(params),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: cur_env.clone(),
                                };
                                new_env.set(loop_name.clone(), lambda);
                                for e in &body[..body.len() - 1] {
                                    eval(e, &new_env, out)?;
                                }
                                cur_expr = body.last().unwrap().clone();
                                cur_env = new_env;
                                    continue 'tco;
                            }
                            // Regular let
                            let bindings = match &args[0].kind {
                                ExprKind::List(bs) => bs,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "let: bindings must be a list at {}",
                                    pos.fmt()
                                ))),
                            };
                            let local_env = Env::with_parent(&cur_env);
                            for b in bindings {
                                match &b.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => break 'tco Err(EvalError::Type(format!(
                                                "let: binding name must be symbol at {}",
                                                pair[0].pos.fmt()
                                            ))),
                                        };
                                        let val = eval(&pair[1], &cur_env, out)?;
                                        local_env.set(bname, val);
                                    }
                                    _ => break 'tco Err(EvalError::Type(format!(
                                        "let: invalid binding at {}",
                                        b.pos.fmt()
                                    ))),
                                }
                            }
                            let body = &args[1..];
                            for e in &body[..body.len() - 1] {
                                eval(e, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue 'tco;
                        }
                        "begin" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                break 'tco Ok(Value::Nil);
                            }
                            for e in &args[..args.len() - 1] {
                                eval(e, &cur_env, out)?;
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue 'tco;
                        }
                        "cond" => {
                            let clauses = &items[1..];
                            let mut found = false;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let is_else = matches!(
                                            &parts[0].kind,
                                            ExprKind::Symbol(ref s) if s == "else"
                                        );
                                        if is_else
                                            || eval(&parts[0], &cur_env, out)?.is_truthy()
                                        {
                                            for e in &parts[1..parts.len() - 1] {
                                                eval(e, &cur_env, out)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                    }
                                    _ => {
                                        break 'tco Err(EvalError::Type(format!(
                                            "cond: invalid clause at {}",
                                            clause.pos.fmt()
                                        )));
                                    }
                                }
                            }
                            if found {
                                continue 'tco;
                            }
                            break 'tco Ok(Value::Nil);
                        }
                        "set!" => {
                            if items.len() != 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "set! requires 2 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let name = match &items[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "set!: first argument must be a symbol at {}",
                                    pos.fmt()
                                ))),
                            };
                            let val = eval(&items[2], &cur_env, out)?;
                            if !cur_env.set_existing(&name, val) {
                                break 'tco Err(EvalError::UnboundVariable(format!(
                                    "{} at {}",
                                    name,
                                    pos.fmt()
                                )));
                            }
                            break 'tco Ok(Value::Nil);
                        }
                        "string-set!" => {
                            break 'tco eval_string_set(
                                &items[1..],
                                pos,
                                &cur_env,
                                out,
                            );
                        }
                        "call/cc" | "call-with-current-continuation" => {
                            if items.len() != 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "call/cc requires 1 argument at {}",
                                    pos.fmt()
                                )));
                            }
                            break 'tco do_callcc(&items[1], pos, &cur_env, out);
                        }
                        "define-syntax" => {
                            if items.len() != 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "define-syntax requires 2 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let macro_name = match &items[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "define-syntax: name must be a symbol at {}",
                                    pos.fmt()
                                ))),
                            };
                            let sr = match &items[2].kind {
                                ExprKind::List(parts) => parts,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "define-syntax: expected syntax-rules at {}",
                                    pos.fmt()
                                ))),
                            };
                            let literals = Rc::new(match &sr[1].kind {
                                ExprKind::List(lits) => lits.iter().filter_map(|e| {
                                    if let ExprKind::Symbol(s) = &e.kind { Some(s.clone()) } else { None }
                                }).collect(),
                                _ => vec![],
                            });
                            let rules = Rc::new(sr[2..].iter().filter_map(|clause| {
                                if let ExprKind::List(parts) = &clause.kind {
                                    if parts.len() == 2 {
                                        return Some((parts[0].clone(), parts[1].clone()));
                                    }
                                }
                                None
                            }).collect());
                            let macro_val = Value::Macro { literals, rules, def_env: cur_env.clone() };
                            cur_env.set(macro_name, macro_val);
                            break 'tco Ok(Value::Nil);
                        }
                        _ => {
                            if let Some(Value::Macro { ref literals, ref rules, ref def_env }) = cur_env.get(op) {
                                let expanded = expand_macro(&items, literals, rules, def_env, &cur_env, pos)?;
                                cur_expr = expanded;
                                continue 'tco;
                            }
                        }
                    }
                }
                // Function application
                let func_name = match &items[0].kind {
                    ExprKind::Symbol(name) => Some(name.clone()),
                    _ => None,
                };
                let args: Result<Vec<Value>, _> =
                    items[1..].iter().map(|a| eval(a, &cur_env, out)).collect();
                let args = args?;
                if let Some(ref name) = func_name {
                    if name == "apply" {
                        break 'tco call_apply(&args, pos, &cur_env, out);
                    }
                    if let Some(result) = apply_builtin(name, &args, pos, out)? {
                        break 'tco Ok(result);
                    }
                }
                let func = if let Some(ref name) = func_name {
                    if let Some(val) = cur_env.get(name) {
                        val
                    } else if Value::is_builtin_name(name) {
                        Value::Builtin(name.clone())
                    } else {
                        break 'tco Err(EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt())));
                    }
                } else {
                    eval(&items[0], &cur_env, out)?
                };
                match func {
                    Value::Continuation(idx) => {
                        if args.len() != 1 {
                            break 'tco Err(EvalError::Arity(format!(
                                "continuation requires 1 argument at {}",
                                pos.fmt()
                            )));
                        }
                        CONT_RETURN.with(|cr| {
                            *cr.borrow_mut() = Some((idx, args.into_iter().next().unwrap()));
                        });
                        break 'tco Err(EvalError::ContinuationReturn);
                    }
                    Value::Builtin(ref bname) => {
                        if bname == "call/cc" {
                            if args.len() != 1 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "call/cc requires 1 argument at {}",
                                    pos.fmt()
                                )));
                            }
                            let pending = CALLCC_PENDING.with(|p| p.borrow_mut().take());
                            if let Some(val) = pending {
                                break 'tco Ok(val);
                            }
                            let start_idx = CALLCC_TOP_IDX.with(|c| c.get());
                            let k = Value::Continuation(start_idx);
                            let proc = args.into_iter().next().unwrap();
                            break 'tco apply_func(proc, vec![k], pos, &cur_env, out);
                        }
                        if bname == "apply" {
                            break 'tco call_apply(&args, pos, &cur_env, out);
                        }
                        match apply_builtin(bname, &args, pos, out)? {
                            Some(result) => break 'tco Ok(result),
                            None => break 'tco Err(EvalError::Type(format!(
                                "unknown builtin {} at {}",
                                bname,
                                pos.fmt()
                            ))),
                        }
                    }
                    Value::Lambda {
                        params,
                        rest_param,
                        body,
                        env: closure_env,
                    } => {
                        if let Some(ref rp) = rest_param {
                            if args.len() < params.len() {
                                break 'tco Err(EvalError::Arity(format!(
                                    "expected at least {} arguments, got {} at {}",
                                    params.len(),
                                    args.len(),
                                    pos.fmt()
                                )));
                            }
                            let new_env = Env::with_parent(&closure_env);
                            cur_env.copy_all_into_if_absent(&new_env);
                            for (p, a) in params.iter().zip(args.iter()) {
                                new_env.set(p.clone(), a.clone());
                            }
                            // Build rest list from excess args
                            let mut rest = Value::Nil;
                            for a in args[params.len()..].iter().rev() {
                                rest = Value::Pair(Box::new(a.clone()), Box::new(rest));
                            }
                            new_env.set(rp.clone(), rest);
                            if body.is_empty() {
                                break 'tco Ok(Value::Nil);
                            }
                            for e in &body[..body.len() - 1] {
                                eval(e, &new_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = new_env;
                            continue 'tco;
                        } else {
                            if params.len() != args.len() {
                                break 'tco Err(EvalError::Arity(format!(
                                    "expected {} arguments, got {} at {}",
                                    params.len(),
                                    args.len(),
                                    pos.fmt()
                                )));
                            }
                            let new_env = Env::with_parent(&closure_env);
                            cur_env.copy_all_into_if_absent(&new_env);
                            for (p, a) in params.iter().zip(args.into_iter()) {
                                new_env.set(p.clone(), a);
                            }
                            if body.is_empty() {
                                break 'tco Ok(Value::Nil);
                            }
                            for e in &body[..body.len() - 1] {
                                eval(e, &new_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = new_env;
                            continue 'tco;
                        }
                    }
                    _ => {
                        break 'tco Err(EvalError::Type(format!(
                            "not a procedure at {}",
                            pos.fmt()
                        )));
                    }
                }
            }
        }
    };
    result
}

fn eval_define(args: &[Expr], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "define requires arguments at {}",
            pos.fmt()
        )));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "define requires a value at {}",
                    pos.fmt()
                )));
            }
            let val = eval(&args[1], env, out)?;
            env.set(name.clone(), val);
            Ok(Value::Nil)
        }
        ExprKind::List(parts) => {
            if parts.is_empty() {
                return Err(EvalError::Parse(format!(
                    "define: empty name list at {}",
                    pos.fmt()
                )));
            }
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "define: name must be symbol at {}",
                        pos.fmt()
                    )))
                }
            };
            let (params, rest_param) = parse_params(&parts[1..], pos)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params: Rc::new(params),
                rest_param,
                body: Rc::new(body),
                env: env.clone(),
            };
            env.set(name.clone(), lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Type(format!(
            "define: first argument must be symbol or list at {}",
            pos.fmt()
        ))),
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = Value::Pair(Box::new(expr_to_value(item)), Box::new(result));
            }
            result
        }
    }
}

fn eval_quote(args: &[Expr], pos: Pos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!(
            "quote requires 1 argument at {}",
            pos.fmt()
        )));
    }
    Ok(expr_to_value(&args[0]))
}

fn eval_lambda(args: &[Expr], pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "lambda requires params and body at {}",
            pos.fmt()
        )));
    }
    match &args[0].kind {
        ExprKind::List(parts) => {
            let (params, rest_param) = parse_params(parts, pos)?;
            Ok(Value::Lambda {
                params: Rc::new(params),
                rest_param,
                body: Rc::new(args[1..].to_vec()),
                env: env.clone(),
            })
        }
        ExprKind::Symbol(s) => {
            // (lambda args body) — all args collected into rest
            Ok(Value::Lambda {
                params: Rc::new(vec![]),
                rest_param: Some(s.clone()),
                body: Rc::new(args[1..].to_vec()),
                env: env.clone(),
            })
        }
        _ => Err(EvalError::Type(format!(
            "lambda: params must be a list or symbol at {}",
            pos.fmt()
        ))),
    }
}

/// Perform call/cc: check for pending return, otherwise create continuation and call proc.
fn do_callcc(proc_expr: &Expr, pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    let pending = CALLCC_PENDING.with(|p| p.borrow_mut().take());
    if let Some(val) = pending {
        return Ok(val);
    }
    let proc = eval(proc_expr, env, out)?;
    let start_idx = CALLCC_TOP_IDX.with(|c| c.get());
    let k = Value::Continuation(start_idx);
    apply_func(proc, vec![k], pos, env, out)
}

/// Apply a function value to arguments (non-TCO, used by call/cc).
fn apply_func(func: Value, args: Vec<Value>, pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            let new_env = Env::with_parent(&closure_env);
            env.copy_all_into_if_absent(&new_env);
            if let Some(ref rp) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(),
                        args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.set(p.clone(), a.clone());
                }
                let mut rest = Value::Nil;
                for a in args[params.len()..].iter().rev() {
                    rest = Value::Pair(Box::new(a.clone()), Box::new(rest));
                }
                new_env.set(rp.clone(), rest);
            } else {
                if params.len() != args.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {} at {}",
                        params.len(),
                        args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(args.into_iter()) {
                    new_env.set(p.clone(), a);
                }
            }
            if body.is_empty() {
                return Ok(Value::Nil);
            }
            for e in &body[..body.len() - 1] {
                eval(e, &new_env, out)?;
            }
            eval(body.last().unwrap(), &new_env, out)
        }
        Value::Continuation(idx) => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            CONT_RETURN.with(|cr| {
                *cr.borrow_mut() = Some((idx, args.into_iter().next().unwrap()));
            });
            Err(EvalError::ContinuationReturn)
        }
        _ => Err(EvalError::Type(format!(
            "call/cc: argument must be a procedure at {}",
            pos.fmt()
        ))),
    }
}

/// Parse parameter list, handling dot notation for rest params.
/// e.g. [x, y, ., rest] -> (vec!["x", "y"], Some("rest"))
fn parse_params(parts: &[Expr], pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < parts.len() {
        match &parts[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= parts.len() {
                    return Err(EvalError::Parse(format!(
                        "expected rest parameter after . at {}",
                        pos.fmt()
                    )));
                }
                match &parts[i + 1].kind {
                    ExprKind::Symbol(rp) => rest_param = Some(rp.clone()),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "rest parameter must be symbol at {}",
                            parts[i + 1].pos.fmt()
                        )))
                    }
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "parameter must be symbol at {}",
                    parts[i].pos.fmt()
                )))
            }
        }
    }
    Ok((params, rest_param))
}

/// Convert a Value list to a Vec<Value>.
fn value_list_to_vec(val: &Value, pos: Pos) -> Result<Vec<Value>, EvalError> {
    let mut result = Vec::new();
    let mut cur = val;
    loop {
        match cur {
            Value::Nil => return Ok(result),
            Value::Pair(car, cdr) => {
                result.push(*car.clone());
                cur = cdr;
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "apply: last argument must be a proper list at {}",
                    pos.fmt()
                )))
            }
        }
    }
}

/// Implement (apply fn arg1 ... argN list)
fn call_apply(args: &[Value], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "apply requires at least 2 arguments at {}",
            pos.fmt()
        )));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    let tail = value_list_to_vec(last, pos)?;
    call_args.extend(tail);

    match func {
        Value::Continuation(idx) => {
            if call_args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            CONT_RETURN.with(|cr| {
                *cr.borrow_mut() = Some((*idx, call_args.into_iter().next().unwrap()));
            });
            Err(EvalError::ContinuationReturn)
        }
        Value::Builtin(bname) => {
            if bname == "apply" {
                return call_apply(&call_args, pos, env, out);
            }
            match apply_builtin(bname, &call_args, pos, out)? {
                Some(result) => Ok(result),
                None => Err(EvalError::Type(format!(
                    "unknown builtin {} at {}",
                    bname,
                    pos.fmt()
                ))),
            }
        }
        Value::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            let new_env = Env::with_parent(closure_env);
            env.copy_all_into_if_absent(&new_env);
            if let Some(ref rp) = rest_param {
                if call_args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(),
                        call_args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(call_args.iter()) {
                    new_env.set(p.clone(), a.clone());
                }
                let mut rest = Value::Nil;
                for a in call_args[params.len()..].iter().rev() {
                    rest = Value::Pair(Box::new(a.clone()), Box::new(rest));
                }
                new_env.set(rp.clone(), rest);
            } else {
                if params.len() != call_args.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {} at {}",
                        params.len(),
                        call_args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(call_args.into_iter()) {
                    new_env.set(p.clone(), a);
                }
            }
            if body.is_empty() {
                return Ok(Value::Nil);
            }
            for e in &body[..body.len() - 1] {
                eval(e, &new_env, out)?;
            }
            eval(body.last().unwrap(), &new_env, out)
        }
        _ => Err(EvalError::Type(format!(
            "apply: first argument must be a procedure at {}",
            pos.fmt()
        ))),
    }
}

fn eval_string_set(args: &[Expr], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!(
            "string-set! requires 3 arguments at {}",
            pos.fmt()
        )));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(EvalError::Type(format!(
                "string-set!: first argument must be a variable at {}",
                pos.fmt()
            )))
        }
    };
    let idx = eval(&args[1], env, out)?.as_integer(pos)? as usize;
    let ch = match eval(&args[2], env, out)? {
        Value::Char(c) => c,
        _ => {
            return Err(EvalError::Type(format!(
                "string-set!: third argument must be a character at {}",
                pos.fmt()
            )))
        }
    };
    let val = env.get(&name).ok_or_else(|| {
        EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt()))
    })?;
    match val {
        Value::Str(s) => {
            let mut chars: Vec<char> = s.chars().collect();
            chars[idx] = ch;
            let new_s: String = chars.into_iter().collect();
            env.set_existing(&name, Value::Str(new_s));
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Type(format!(
            "string-set!: expected string at {}",
            pos.fmt()
        ))),
    }
}

fn apply_builtin(op: &str, args: &[Value], pos: Pos, out: &mut String) -> Result<Option<Value>, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer(pos)?;
            }
            Ok(Some(Value::Integer(sum)))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "- requires at least 1 argument at {}",
                    pos.fmt()
                )));
            }
            if args.len() == 1 {
                return Ok(Some(Value::Integer(-args[0].as_integer(pos)?)));
            }
            let mut result = args[0].as_integer(pos)?;
            for a in &args[1..] {
                result -= a.as_integer(pos)?;
            }
            Ok(Some(Value::Integer(result)))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= a.as_integer(pos)?;
            }
            Ok(Some(Value::Integer(product)))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "/ requires at least 1 argument at {}",
                    pos.fmt()
                )));
            }
            let mut result = args[0].as_integer(pos)?;
            for a in &args[1..] {
                let divisor = a.as_integer(pos)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
                }
                result /= divisor;
            }
            Ok(Some(Value::Integer(result)))
        }
        "<" => compare_op_val(args, |a, b| a < b, pos),
        ">" => compare_op_val(args, |a, b| a > b, pos),
        "=" => compare_op_val(args, |a, b| a == b, pos),
        "<=" => compare_op_val(args, |a, b| a <= b, pos),
        ">=" => compare_op_val(args, |a, b| a >= b, pos),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "not requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(!args[0].is_truthy())))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "cons requires 2 arguments at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Pair(
                Box::new(args[0].clone()),
                Box::new(args[1].clone()),
            )))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "car requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Pair(car, _) => Ok(Some(*car.clone())),
                _ => Err(EvalError::Type(format!("car: not a pair at {}", pos.fmt()))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "cdr requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Pair(_, cdr) => Ok(Some(*cdr.clone())),
                _ => Err(EvalError::Type(format!("cdr: not a pair at {}", pos.fmt()))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "null? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Nil))))
        }
        "list" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                result = Value::Pair(Box::new(a.clone()), Box::new(result));
            }
            Ok(Some(result))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "length requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(_, cdr) => {
                        count += 1;
                        cur = cdr;
                    }
                    _ => {
                        return Err(EvalError::Type(format!(
                            "length: not a proper list at {}",
                            pos.fmt()
                        )))
                    }
                }
            }
            Ok(Some(Value::Integer(count)))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Str(_)))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "number? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Integer(_)))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "boolean? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Boolean(_)))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "pair? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Pair(_, _)))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "symbol? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Symbol(_)))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "char? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Char(_)))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "display requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            out.push_str(&args[0].display_unquoted());
            Ok(Some(Value::Nil))
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "write requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            out.push_str(&args[0].display());
            Ok(Some(Value::Nil))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "newline requires 0 arguments at {}",
                    pos.fmt()
                )));
            }
            out.push('\n');
            Ok(Some(Value::Nil))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!(
                        "string-append: expected string at {}",
                        pos.fmt()
                    ))),
                }
            }
            Ok(Some(Value::Str(result)))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string-length requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Integer(s.chars().count() as i64))),
                _ => Err(EvalError::Type(format!(
                    "string-length: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!(
                    "substring requires 3 arguments at {}",
                    pos.fmt()
                )));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!(
                    "substring: expected string at {}",
                    pos.fmt()
                ))),
            };
            let start = args[1].as_integer(pos)? as usize;
            let end = args[2].as_integer(pos)? as usize;
            let chars: Vec<char> = s.chars().collect();
            Ok(Some(Value::Str(chars[start..end].iter().collect())))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string->number requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Some(Value::Integer(n))),
                    Err(_) => Ok(Some(Value::Boolean(false))),
                },
                _ => Err(EvalError::Type(format!(
                    "string->number: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "number->string requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            let n = args[0].as_integer(pos)?;
            Ok(Some(Value::Str(n.to_string())))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "symbol->string requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Some(Value::Str(s.clone()))),
                _ => Err(EvalError::Type(format!(
                    "symbol->string: expected symbol at {}",
                    pos.fmt()
                ))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string->symbol requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Symbol(s.clone()))),
                _ => Err(EvalError::Type(format!(
                    "string->symbol: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string-copy requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Str(s.clone()))),
                _ => Err(EvalError::Type(format!(
                    "string-copy: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "string-ref requires 2 arguments at {}",
                    pos.fmt()
                )));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!(
                    "string-ref: expected string at {}",
                    pos.fmt()
                ))),
            };
            let idx = args[1].as_integer(pos)? as usize;
            let chars: Vec<char> = s.chars().collect();
            Ok(Some(Value::Char(chars[idx])))
        }
        _ => Ok(None),
    }
}

fn compare_op_val(
    args: &[Value],
    cmp: impl Fn(i64, i64) -> bool,
    pos: Pos,
) -> Result<Option<Value>, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "comparison requires at least 2 arguments at {}",
            pos.fmt()
        )));
    }
    let mut prev = args[0].as_integer(pos)?;
    for a in &args[1..] {
        let cur = a.as_integer(pos)?;
        if !cmp(prev, cur) {
            return Ok(Some(Value::Boolean(false)));
        }
        prev = cur;
    }
    Ok(Some(Value::Boolean(true)))
}

// ── Hygienic Macros ─────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    List(Vec<Expr>),
}

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "define" | "quote" | "lambda" | "and" | "or"
            | "let" | "begin" | "cond" | "set!" | "call/cc"
            | "call-with-current-continuation" | "string-set!"
            | "define-syntax" | "syntax-rules"
    ) || Value::is_builtin_name(name)
}

fn match_pattern(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
) -> Option<HashMap<String, MacroBinding>> {
    let mut bindings = HashMap::new();
    let mut pi = 0;
    let mut ii = 0;

    while pi < pattern.len() {
        // Check for ellipsis
        if pi + 1 < pattern.len() {
            if let ExprKind::Symbol(ref s) = pattern[pi + 1].kind {
                if s == "..." {
                    let var_name = match &pattern[pi].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return None,
                    };
                    let remaining_pattern = pattern.len() - pi - 2;
                    let available = input.len().checked_sub(ii + remaining_pattern)?;
                    let mut collected = Vec::new();
                    for _ in 0..available {
                        collected.push(input[ii].clone());
                        ii += 1;
                    }
                    bindings.insert(var_name, MacroBinding::List(collected));
                    pi += 2;
                    continue;
                }
            }
        }

        if ii >= input.len() {
            return None;
        }

        match &pattern[pi].kind {
            ExprKind::Symbol(ref s) if literals.contains(s) => {
                match &input[ii].kind {
                    ExprKind::Symbol(ref is) if is == s => {}
                    _ => return None,
                }
            }
            ExprKind::Symbol(ref s) if s == "_" => {}
            ExprKind::Symbol(ref s) => {
                bindings.insert(s.clone(), MacroBinding::Single(input[ii].clone()));
            }
            ExprKind::List(sub_pat) => match &input[ii].kind {
                ExprKind::List(sub_inp) => {
                    let sub = match_pattern(sub_pat, sub_inp, literals)?;
                    bindings.extend(sub);
                }
                _ => return None,
            },
            ExprKind::Integer(n) => match &input[ii].kind {
                ExprKind::Integer(m) if n == m => {}
                _ => return None,
            },
            ExprKind::Boolean(b) => match &input[ii].kind {
                ExprKind::Boolean(b2) if b == b2 => {}
                _ => return None,
            },
            _ => return None,
        }

        pi += 1;
        ii += 1;
    }

    if ii == input.len() {
        Some(bindings)
    } else {
        None
    }
}

fn find_ellipsis_var(expr: &Expr, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match &expr.kind {
        ExprKind::Symbol(ref s) => {
            if let Some(MacroBinding::List(_)) = bindings.get(s) {
                Some(s.clone())
            } else {
                None
            }
        }
        ExprKind::List(items) => {
            for item in items {
                if let Some(v) = find_ellipsis_var(item, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(ref s) => {
            if let Some(MacroBinding::Single(expr)) = bindings.get(s) {
                return expr.clone();
            }
            if let Some(new_name) = renames.get(s) {
                return Expr {
                    kind: ExprKind::Symbol(new_name.clone()),
                    pos: template.pos,
                };
            }
            template.clone()
        }
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() {
                    if let ExprKind::Symbol(ref s) = items[i + 1].kind {
                        if s == "..." {
                            if let Some(var_name) = find_ellipsis_var(&items[i], bindings) {
                                if let Some(MacroBinding::List(ref elems)) = bindings.get(&var_name) {
                                    for elem in elems {
                                        let mut local = bindings.clone();
                                        local.insert(var_name.clone(), MacroBinding::Single(elem.clone()));
                                        expanded.push(expand_template(&items[i], &local, renames));
                                    }
                                }
                            }
                            i += 2;
                            continue;
                        }
                    }
                }
                expanded.push(expand_template(&items[i], bindings, renames));
                i += 1;
            }
            Expr {
                kind: ExprKind::List(expanded),
                pos: template.pos,
            }
        }
        _ => template.clone(),
    }
}

fn collect_free_symbols(
    expr: &Expr,
    pattern_vars: &HashSet<String>,
    result: &mut HashSet<String>,
) {
    match &expr.kind {
        ExprKind::Symbol(ref s) => {
            if s != "..." && !pattern_vars.contains(s) && !is_keyword(s) {
                result.insert(s.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_free_symbols(item, pattern_vars, result);
            }
        }
        _ => {}
    }
}

fn expand_macro(
    input: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    pos: Pos,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        let pat_items = match &pattern.kind {
            ExprKind::List(items) => items,
            _ => continue,
        };
        if let Some(bindings) = match_pattern(&pat_items[1..], &input[1..], literals) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            let mut free_syms = HashSet::new();
            collect_free_symbols(template, &pattern_vars, &mut free_syms);

            let mut renames = HashMap::new();
            for sym in &free_syms {
                renames.insert(sym.clone(), gensym(sym));
            }

            for (orig, renamed) in &renames {
                if let Some(val) = def_env.get(orig) {
                    use_env.set(renamed.clone(), val);
                }
            }

            return Ok(expand_template(template, &bindings, &renames));
        }
    }
    Err(EvalError::Type(format!(
        "no matching macro pattern at {}",
        pos.fmt()
    )))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    let mut out = String::new();
    let mut result = Value::Boolean(false);
    let mut i = 0;
    while i < exprs.len() {
        CALLCC_TOP_IDX.with(|c| c.set(i));
        match eval(&exprs[i], &env, &mut out) {
            Ok(val) => {
                result = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let (start_idx, value) =
                    CONT_RETURN.with(|cr| cr.borrow_mut().take().unwrap());
                CALLCC_PENDING.with(|p| *p.borrow_mut() = Some(value));
                i = start_idx;
            }
            Err(e) => return Err(e),
        }
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    let mut out = String::new();
    let mut result = Value::Boolean(false);
    let mut i = 0;
    while i < exprs.len() {
        CALLCC_TOP_IDX.with(|c| c.set(i));
        match eval(&exprs[i], &env, &mut out) {
            Ok(val) => {
                result = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let (start_idx, value) =
                    CONT_RETURN.with(|cr| cr.borrow_mut().take().unwrap());
                CALLCC_PENDING.with(|p| *p.borrow_mut() = Some(value));
                i = start_idx;
            }
            Err(e) => return Err(e),
        }
    }
    Ok((result.display(), out))
}

#[cfg(test)]
mod tests;
