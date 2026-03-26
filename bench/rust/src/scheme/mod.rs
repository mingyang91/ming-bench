use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

mod builtins;
pub mod error;
mod parser;

use builtins::default_env;
pub use error::EvalError;
use error::SourcePos;
use parser::parse_program;

#[derive(Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    CapturedSymbol(String, EnvRef),
    List(Vec<Expr>),
}

#[derive(Clone)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

type NativeFunc = fn(&[Value], &EvalContext) -> Result<Value, EvalError>;
type EnvRef = Rc<Env>;
type PairRef = Rc<RefCell<PairCell>>;
type StringRef = Rc<RefCell<Vec<char>>>;
type MacroRef = Rc<SyntaxRules>;

struct SyntaxRules {
    name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    env: EnvRef,
}

struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

enum MatchBinding {
    One(Expr),
    Many(Vec<Expr>),
}

struct EvalContext {
    output: RefCell<String>,
    gensym_counter: RefCell<usize>,
}

impl EvalContext {
    fn new() -> Self {
        Self {
            output: RefCell::new(String::new()),
            gensym_counter: RefCell::new(0),
        }
    }

    fn push_output(&self, value: &str) {
        self.output.borrow_mut().push_str(value);
    }

    fn into_output(self) -> String {
        self.output.into_inner()
    }

    fn fresh_name(&self, base: &str) -> String {
        let mut counter = self.gensym_counter.borrow_mut();
        let name = format!("__ming_macro_{}_{}", base, *counter);
        *counter += 1;
        name
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(StringRef),
    Char(char),
    Symbol(String),
    Nil,
    Pair(PairRef),
    NativeProc {
        name: &'static str,
        func: NativeFunc,
    },
    Closure(Rc<Closure>),
    Void,
}

struct PairCell {
    car: Value,
    cdr: Value,
}

struct Closure {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    syntax_bindings: RefCell<HashMap<String, MacroRef>>,
    parent: Option<EnvRef>,
}

fn make_string(value: impl AsRef<str>) -> Value {
    Value::String(Rc::new(RefCell::new(value.as_ref().chars().collect())))
}

fn render_string(value: &StringRef) -> String {
    value.borrow().iter().collect()
}

impl Value {
    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => {
                let escaped = render_string(value)
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t");
                format!("\"{escaped}\"")
            }
            Self::Char(value) => render_char(*value),
            Self::Symbol(value) => value.clone(),
            Self::Nil => "()".to_string(),
            Self::Pair(pair) => render_pair(pair.clone()),
            Self::NativeProc { name, .. } => format!("#<procedure:{name}>"),
            Self::Closure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }

    fn display_render(&self) -> String {
        match self {
            Self::String(value) => render_string(value),
            Self::Char(value) => value.to_string(),
            _ => self.render(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::Nil => "null",
            Self::Pair(_) => "pair",
            Self::NativeProc { .. } | Self::Closure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn as_number(&self, name: &'static str) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name().to_string(),
            }),
        }
    }

    fn as_string(&self, name: &'static str) -> Result<String, EvalError> {
        match self {
            Self::String(value) => Ok(render_string(value)),
            other => Err(EvalError::ExpectedString {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_string_ref(&self, name: &'static str) -> Result<StringRef, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            other => Err(EvalError::ExpectedString {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_char(&self, name: &'static str) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            other => Err(EvalError::ExpectedChar {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_symbol<'a>(&'a self, name: &'static str) -> Result<&'a str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            other => Err(EvalError::ExpectedSymbol {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_pair(&self, name: &'static str) -> Result<PairRef, EvalError> {
        match self {
            Self::Pair(pair) => Ok(pair.clone()),
            other => Err(EvalError::ExpectedPair {
                name,
                found: other.render(),
            }),
        }
    }
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            syntax_bindings: RefCell::new(HashMap::new()),
            parent,
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }

    fn define_syntax(&self, name: String, value: MacroRef) {
        self.syntax_bindings.borrow_mut().insert(name, value);
    }

    fn lookup_syntax(&self, name: &str) -> Option<MacroRef> {
        if let Some(value) = self.syntax_bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_syntax(name))
    }

    fn set(&self, name: &str, value: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            true
        } else {
            self.parent
                .as_ref()
                .is_some_and(|parent| parent.set(name, value))
        }
    }
}

impl Closure {
    fn call(&self, args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
        if args.len() < self.params.len()
            || (self.rest_param.is_none() && args.len() != self.params.len())
        {
            return Err(EvalError::WrongArgCount {
                name: "lambda",
                expected: "the declared arity",
                got: args.len(),
            });
        }

        let frame = Env::new(Some(self.env.clone()));
        for (name, value) in self.params.iter().zip(args.iter()) {
            frame.define(name.clone(), value.clone());
        }

        if let Some(name) = &self.rest_param {
            frame.define(
                name.clone(),
                list_from_values(args[self.params.len()..].to_vec()),
            );
        }

        eval_sequence(&self.body, frame, ctx)
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
    let (result, _) = eval_str_with_output(input)?;
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_program(input)?;

    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    let ctx = EvalContext::new();
    let last = eval_sequence(&exprs, env, &ctx)?;
    Ok((last.render(), ctx.into_output()))
}

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval(expr, env.clone(), ctx)?;
    }
    Ok(last)
}

fn eval(expr: &Expr, env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(name) => lookup_symbol_value(name, &env, expr.pos),
        ExprKind::CapturedSymbol(name, captured_env) => {
            lookup_symbol_value(name, captured_env, expr.pos)
        }
        ExprKind::List(items) => eval_list(expr.pos, items, env, ctx),
    }
}

fn eval_list(
    pos: SourcePos,
    items: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let (head, tail) = items.split_first().ok_or_else(|| {
        EvalError::NotAProcedure {
            found: "()".to_string(),
        }
        .with_position(pos)
    })?;

    if let Some(name) = expr_symbol_name(head) {
        match name {
            "define" => {
                return eval_define(tail, env, ctx).map_err(|err| err.with_position(head.pos))
            }
            "define-syntax" => {
                return eval_define_syntax(tail, env).map_err(|err| err.with_position(head.pos))
            }
            "set!" => return eval_set(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "if" => return eval_if(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "quote" => return eval_quote(tail).map_err(|err| err.with_position(head.pos)),
            "lambda" => return eval_lambda(tail, env).map_err(|err| err.with_position(head.pos)),
            "and" => return eval_and(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "or" => return eval_or(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "begin" => {
                return eval_begin(tail, env, ctx).map_err(|err| err.with_position(head.pos))
            }
            "cond" => return eval_cond(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "let" => return eval_let(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            _ => {}
        }
    }

    if let Some(syntax) = lookup_syntax(head, &env) {
        let expanded = expand_macro_call(pos, items, syntax, ctx)?;
        return eval(&expanded, env, ctx);
    }

    let procedure = eval(head, env.clone(), ctx)?;
    let args = eval_args(tail, env, ctx)?;
    apply_procedure(procedure, &args, ctx).map_err(|err| err.with_position(head.pos))
}

fn eval_define(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(target) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define requires a target".to_string(),
        });
    };

    match &target.kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2",
                    got: args.len(),
                });
            }

            let value = eval(&args[1], env.clone(), ctx)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let (name_expr, params_exprs) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: "define requires a binding name".to_string(),
                    })?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "function name must be a symbol".to_string(),
                });
            };

            if args.len() < 2 {
                return Err(EvalError::InvalidSyntax {
                    message: "function definition requires a body".to_string(),
                });
            }

            let (params, rest_param) = parse_param_slice(params_exprs)?;
            let closure = Value::Closure(Rc::new(Closure {
                params,
                rest_param,
                body: args[1..].to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidSyntax {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn eval_define_syntax(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "define-syntax",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let Some(name) = expr_plain_symbol_name(&args[0]) else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax requires a symbol name".to_string(),
        });
    };

    let rules = parse_syntax_rules(name, &args[1], env.clone())?;
    env.define_syntax(name.to_string(), Rc::new(rules));
    Ok(Value::Void)
}

fn eval_set(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let (name, target_env) = match &args[0].kind {
        ExprKind::Symbol(name) => (name.clone(), env.clone()),
        ExprKind::CapturedSymbol(name, captured_env) => (name.clone(), captured_env.clone()),
        _ => {
            return Err(EvalError::InvalidSyntax {
                message: "set! requires a symbol target".to_string(),
            });
        }
    };

    let value = eval(&args[1], env.clone(), ctx)?;
    if target_env.set(&name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundVariable { name }.with_position(args[0].pos))
    }
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "exactly 3",
            got: args.len(),
        });
    }

    if eval(&args[0], env.clone(), ctx)?.is_truthy() {
        eval(&args[1], env, ctx)
    } else {
        eval(&args[2], env, ctx)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    quote_expr(&args[0])
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let (params, rest_param) = parse_param_list(&args[0])?;
    Ok(Value::Closure(Rc::new(Closure {
        params,
        rest_param,
        body: args[1..].to_vec(),
        env,
    })))
}

fn eval_and(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);
    for expr in args {
        let value = eval(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_cond(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "cond clauses cannot be empty".to_string(),
            })?;

        if expr_symbol_name(test).is_some_and(|name| name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond else clause must be last".to_string(),
                });
            }
            return eval_sequence(body, env, ctx);
        }

        let test_value = eval(test, env.clone(), ctx)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, ctx)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(first) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let requires bindings".to_string(),
        });
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let(name, &args[1..], env, ctx),
        _ => eval_plain_let(first, &args[1..], env, ctx),
    }
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "let requires a body".to_string(),
        });
    }

    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone(), ctx)?;
    let frame = Env::new(Some(env));

    for ((name, _), value) in bindings.into_iter().zip(values.into_iter()) {
        frame.define(name, value);
    }

    eval_sequence(body, frame, ctx)
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "named let requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_bindings(&args[0])?;
    let values = eval_binding_values(&bindings, env.clone(), ctx)?;
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let frame = Env::new(Some(env));
    let closure = Value::Closure(Rc::new(Closure {
        params,
        rest_param: None,
        body: args[1..].to_vec(),
        env: frame.clone(),
    }));

    frame.define(name.to_string(), closure.clone());
    apply_procedure(closure, &values, ctx)
}

fn parse_syntax_rules(name: &str, expr: &Expr, env: EnvRef) -> Result<SyntaxRules, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax requires a syntax-rules transformer".to_string(),
        });
    };

    let Some(head) = items.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules form cannot be empty".to_string(),
        });
    };

    if expr_symbol_name(head) != Some("syntax-rules") {
        return Err(EvalError::InvalidSyntax {
            message: "only syntax-rules transformers are supported".to_string(),
        });
    }

    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules requires literals and at least one rule".to_string(),
        });
    }

    let literals = parse_literal_identifiers(&items[1])?;
    let mut rules = Vec::with_capacity(items.len() - 2);
    for rule in &items[2..] {
        let ExprKind::List(rule_parts) = &rule.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules clauses must be lists".to_string(),
            });
        };

        if rule_parts.len() != 2 {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules clauses must contain a pattern and template".to_string(),
            });
        }

        rules.push(SyntaxRule {
            pattern: rule_parts[0].clone(),
            template: rule_parts[1].clone(),
        });
    }

    Ok(SyntaxRules {
        name: name.to_string(),
        literals,
        rules,
        env,
    })
}

fn parse_literal_identifiers(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules literals must be a list".to_string(),
        });
    };

    let mut literals = HashSet::with_capacity(items.len());
    for item in items {
        let Some(name) = expr_plain_symbol_name(item) else {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules literals must be identifiers".to_string(),
            });
        };
        literals.insert(name.to_string());
    }
    Ok(literals)
}

fn expand_macro_call(
    pos: SourcePos,
    items: &[Expr],
    syntax: MacroRef,
    ctx: &EvalContext,
) -> Result<Expr, EvalError> {
    let call = Expr::new(ExprKind::List(items.to_vec()), pos);

    for rule in &syntax.rules {
        let mut bindings = HashMap::new();
        if match_pattern(
            &rule.pattern,
            &call,
            &syntax.literals,
            &syntax.name,
            &mut bindings,
        )? {
            let renames = HashMap::new();
            return expand_template(&rule.template, &bindings, &syntax.env, &renames, ctx, None);
        }
    }

    Err(EvalError::InvalidSyntax {
        message: format!("{}: no matching syntax-rules pattern", syntax.name),
    })
}

fn match_pattern(
    pattern: &Expr,
    value: &Expr,
    literals: &HashSet<String>,
    macro_name: &str,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match &pattern.kind {
        ExprKind::Integer(expected) => {
            Ok(matches!(&value.kind, ExprKind::Integer(found) if found == expected))
        }
        ExprKind::Boolean(expected) => {
            Ok(matches!(&value.kind, ExprKind::Boolean(found) if found == expected))
        }
        ExprKind::Char(expected) => {
            Ok(matches!(&value.kind, ExprKind::Char(found) if found == expected))
        }
        ExprKind::String(expected) => {
            Ok(matches!(&value.kind, ExprKind::String(found) if found == expected))
        }
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            if name == "_" {
                return Ok(true);
            }

            if name == macro_name || literals.contains(name) {
                return Ok(expr_symbol_name(value).is_some_and(|found| found == name));
            }

            bind_single(name, value, bindings)
        }
        ExprKind::List(pattern_items) => {
            let ExprKind::List(value_items) = &value.kind else {
                return Ok(false);
            };

            if let Some((prefix, repeated)) = pattern_items.split_last_chunk::<2>() {
                if is_ellipsis_expr(&repeated[1]) {
                    if value_items.len() < prefix.len() {
                        return Ok(false);
                    }

                    for (pattern_item, value_item) in prefix.iter().zip(value_items.iter()) {
                        if !match_pattern(pattern_item, value_item, literals, macro_name, bindings)?
                        {
                            return Ok(false);
                        }
                    }

                    return match_repeated_pattern(
                        &repeated[0],
                        &value_items[prefix.len()..],
                        bindings,
                        literals,
                        macro_name,
                    );
                }
            }

            if pattern_items.len() != value_items.len() {
                return Ok(false);
            }

            for (pattern_item, value_item) in pattern_items.iter().zip(value_items.iter()) {
                if !match_pattern(pattern_item, value_item, literals, macro_name, bindings)? {
                    return Ok(false);
                }
            }

            Ok(true)
        }
    }
}

fn match_repeated_pattern(
    pattern: &Expr,
    values: &[Expr],
    bindings: &mut HashMap<String, MatchBinding>,
    literals: &HashSet<String>,
    macro_name: &str,
) -> Result<bool, EvalError> {
    if let Some(name) = expr_symbol_name(pattern) {
        if name == "_" {
            return Ok(true);
        }
        if name == macro_name || literals.contains(name) {
            return Ok(values
                .iter()
                .all(|value| expr_symbol_name(value).is_some_and(|found| found == name)));
        }
        return bind_many(name, values, bindings);
    }

    for value in values {
        if !match_pattern(pattern, value, literals, macro_name, bindings)? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn bind_single(
    name: &str,
    value: &Expr,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match bindings.get(name) {
        None => {
            bindings.insert(name.to_string(), MatchBinding::One(value.clone()));
            Ok(true)
        }
        Some(MatchBinding::One(existing)) => Ok(expr_syntax_eq(existing, value)),
        Some(MatchBinding::Many(_)) => Err(EvalError::InvalidSyntax {
            message: format!("pattern variable {name} used as both repeated and non-repeated"),
        }),
    }
}

fn bind_many(
    name: &str,
    values: &[Expr],
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match bindings.get(name) {
        None => {
            bindings.insert(name.to_string(), MatchBinding::Many(values.to_vec()));
            Ok(true)
        }
        Some(MatchBinding::Many(existing)) => {
            if existing.len() != values.len() {
                return Ok(false);
            }
            Ok(existing
                .iter()
                .zip(values.iter())
                .all(|(left, right)| expr_syntax_eq(left, right)))
        }
        Some(MatchBinding::One(_)) => Err(EvalError::InvalidSyntax {
            message: format!("pattern variable {name} used as both repeated and non-repeated"),
        }),
    }
}

fn expr_syntax_eq(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Integer(left), ExprKind::Integer(right)) => left == right,
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::CapturedSymbol(left, left_env), ExprKind::CapturedSymbol(right, right_env)) => {
            left == right && Rc::ptr_eq(left_env, right_env)
        }
        (ExprKind::Symbol(left), ExprKind::CapturedSymbol(right, _))
        | (ExprKind::CapturedSymbol(left, _), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left), ExprKind::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_syntax_eq(left, right))
        }
        _ => false,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Integer(value) => Ok(Expr::new(ExprKind::Integer(*value), template.pos)),
        ExprKind::Boolean(value) => Ok(Expr::new(ExprKind::Boolean(*value), template.pos)),
        ExprKind::Char(value) => Ok(Expr::new(ExprKind::Char(*value), template.pos)),
        ExprKind::String(value) => Ok(Expr::new(ExprKind::String(value.clone()), template.pos)),
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            if let Some(binding) = bindings.get(name) {
                return match binding {
                    MatchBinding::One(expr) => Ok(expr.clone()),
                    MatchBinding::Many(exprs) => {
                        let Some(index) = repeat_index else {
                            return Err(EvalError::InvalidSyntax {
                                message: format!(
                                    "template uses repeated pattern variable {name} outside ellipsis"
                                ),
                            });
                        };
                        exprs
                            .get(index)
                            .cloned()
                            .ok_or_else(|| EvalError::InvalidSyntax {
                                message: format!(
                                    "template repetition index {index} out of bounds for {name}"
                                ),
                            })
                    }
                };
            }

            if let Some(rename) = renames.get(name) {
                return Ok(Expr::new(ExprKind::Symbol(rename.clone()), template.pos));
            }

            Ok(Expr::new(
                ExprKind::CapturedSymbol(name.clone(), definition_env.clone()),
                template.pos,
            ))
        }
        ExprKind::List(items) => {
            if let Some(head) = items.first() {
                if let Some(name) = expr_plain_symbol_name(head) {
                    if !bindings.contains_key(name) && name == "let" {
                        return expand_let_template(
                            template,
                            items,
                            bindings,
                            definition_env,
                            renames,
                            ctx,
                            repeat_index,
                        );
                    }
                }
            }

            let expanded_items =
                expand_template_items(items, bindings, definition_env, renames, ctx, repeat_index)?;
            Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
        }
    }
}

fn expand_template_items(
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis_expr(&items[index + 1]) {
            let repeat_count = template_repeat_count(&items[index], bindings)?;
            for current in 0..repeat_count {
                expanded.push(expand_template(
                    &items[index],
                    bindings,
                    definition_env,
                    renames,
                    ctx,
                    Some(current),
                )?);
            }
            index += 2;
            continue;
        }

        expanded.push(expand_template(
            &items[index],
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        )?);
        index += 1;
    }

    Ok(expanded)
}

fn expand_let_template(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "let template requires bindings and a body".to_string(),
        });
    }

    let head = expand_template(
        &items[0],
        bindings,
        definition_env,
        renames,
        ctx,
        repeat_index,
    )?;

    let ExprKind::List(binding_items) = &items[1].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let template bindings must be a list".to_string(),
        });
    };

    let mut scoped_renames = renames.clone();
    let mut expanded_bindings = Vec::with_capacity(binding_items.len());
    for binding in binding_items {
        let ExprKind::List(parts) = &binding.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "let template bindings must be pairs".to_string(),
            });
        };

        if parts.len() != 2 {
            return Err(EvalError::InvalidSyntax {
                message: "let template bindings must contain exactly 2 items".to_string(),
            });
        }

        let binding_name = if let Some(name) = expr_plain_symbol_name(&parts[0]) {
            if bindings.contains_key(name) {
                expand_template(
                    &parts[0],
                    bindings,
                    definition_env,
                    renames,
                    ctx,
                    repeat_index,
                )?
            } else {
                let fresh = ctx.fresh_name(name);
                scoped_renames.insert(name.to_string(), fresh.clone());
                Expr::new(ExprKind::Symbol(fresh), parts[0].pos)
            }
        } else {
            return Err(EvalError::InvalidSyntax {
                message: "let template binding names must be identifiers".to_string(),
            });
        };

        let binding_value = expand_template(
            &parts[1],
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        )?;

        expanded_bindings.push(Expr::new(
            ExprKind::List(vec![binding_name, binding_value]),
            binding.pos,
        ));
    }

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head);
    expanded_items.push(Expr::new(ExprKind::List(expanded_bindings), items[1].pos));
    for body in &items[2..] {
        expanded_items.push(expand_template(
            body,
            bindings,
            definition_env,
            &scoped_renames,
            ctx,
            repeat_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
}

fn template_repeat_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
) -> Result<usize, EvalError> {
    let mut count = None;
    collect_template_repeat_count(template, bindings, &mut count)?;
    count.ok_or_else(|| EvalError::InvalidSyntax {
        message: "ellipsis template must reference a repeated pattern variable".to_string(),
    })
}

fn collect_template_repeat_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            if let Some(MatchBinding::Many(values)) = bindings.get(name) {
                match count {
                    None => *count = Some(values.len()),
                    Some(existing) if *existing == values.len() => {}
                    Some(_) => {
                        return Err(EvalError::InvalidSyntax {
                            message: "repeated template variables must have the same length"
                                .to_string(),
                        });
                    }
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                if is_ellipsis_expr(item) {
                    continue;
                }
                collect_template_repeat_count(item, bindings, count)?;
            }
        }
        ExprKind::Integer(_) | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::String(_) => {}
    }

    Ok(())
}

fn parse_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let bindings must be a list".to_string(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(items) = &binding.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "let bindings must be pairs".to_string(),
            });
        };

        if items.len() != 2 {
            return Err(EvalError::InvalidSyntax {
                message: "let bindings must contain exactly 2 items".to_string(),
            });
        }

        let ExprKind::Symbol(name) = &items[0].kind else {
            return Err(EvalError::InvalidSyntax {
                message: "let binding names must be symbols".to_string(),
            });
        };

        parsed.push((name.clone(), items[1].clone()));
    }

    Ok(parsed)
}

fn eval_binding_values(
    bindings: &[(String, Expr)],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(bindings.len());
    for (_, expr) in bindings {
        values.push(eval(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn eval_args(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn apply_procedure(
    procedure: Value,
    args: &[Value],
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match procedure {
        Value::NativeProc { func, .. } => func(args, ctx),
        Value::Closure(closure) => closure.call(args, ctx),
        other => Err(EvalError::NotAProcedure {
            found: other.render(),
        }),
    }
}

fn parse_param_list(expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match &expr.kind {
        ExprKind::List(items) => parse_param_slice(items),
        ExprKind::Symbol(name) => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(EvalError::InvalidSyntax {
            message: "lambda parameters must be a list or symbol".to_string(),
        }),
    }
}

fn parse_param_slice(items: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::with_capacity(items.len());
    let mut index = 0;
    while let Some(item) = items.get(index) {
        let ExprKind::Symbol(name) = &item.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "parameter names must be symbols".to_string(),
            });
        };

        if name == "." {
            let Some(rest_expr) = items.get(index + 1) else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter dot must be followed by a name".to_string(),
                });
            };
            let ExprKind::Symbol(rest_name) = &rest_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter name must be a symbol".to_string(),
                });
            };
            if index + 2 != items.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter must be the final parameter".to_string(),
                });
            }
            return Ok((params, Some(rest_name.clone())));
        }

        params.push(name.clone());
        index += 1;
    }
    Ok((params, None))
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(value) | ExprKind::CapturedSymbol(value, _) => {
            Ok(Value::Symbol(value.clone()))
        }
        ExprKind::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_expr(item)?);
            }
            Ok(list_from_values(values))
        }
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairCell { car, cdr })))
}

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::Nil, |tail, head| make_pair(head, tail))
}

fn list_to_vec(value: &Value, name: &'static str) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut cursor = value.clone();

    loop {
        match cursor {
            Value::Nil => return Ok(items),
            Value::Pair(pair) => {
                let (car, cdr) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                items.push(car);
                cursor = cdr;
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name,
                    found: other.render(),
                });
            }
        }
    }
}

fn render_pair(pair: PairRef) -> String {
    let mut rendered = Vec::new();
    let mut cursor = Value::Pair(pair);

    loop {
        match cursor {
            Value::Pair(pair) => {
                let (car, cdr) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                rendered.push(car.render());
                cursor = cdr;
            }
            Value::Nil => return format!("({})", rendered.join(" ")),
            other => {
                let prefix = rendered.join(" ");
                return format!("({prefix} . {})", other.render());
            }
        }
    }
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        _ => format!("#\\{value}"),
    }
}

fn expr_symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => Some(name.as_str()),
        _ => None,
    }
}

fn expr_plain_symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn is_ellipsis_expr(expr: &Expr) -> bool {
    expr_symbol_name(expr).is_some_and(|name| name == "...")
}

fn lookup_symbol_value(name: &str, env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    env.lookup(name).ok_or_else(|| {
        EvalError::UnboundVariable {
            name: name.to_string(),
        }
        .with_position(pos)
    })
}

fn lookup_syntax(expr: &Expr, env: &EnvRef) -> Option<MacroRef> {
    match &expr.kind {
        ExprKind::Symbol(name) => env.lookup_syntax(name),
        ExprKind::CapturedSymbol(name, captured_env) => captured_env.lookup_syntax(name),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
