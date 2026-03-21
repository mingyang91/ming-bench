pub mod error;

pub use error::EvalError;
use error::SourcePos;

use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut ctx = EvalContext::default();
    Ok(eval_program(input, &mut ctx)?.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut ctx = EvalContext::default();
    let value = eval_program(input, &mut ctx)?;
    Ok((value.to_string(), ctx.output))
}

#[cfg(test)]
mod tests;

#[derive(Debug, Default)]
struct EvalContext {
    output: String,
}

fn eval_program(input: &str, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;

    if program.is_empty() {
        return Err(EvalError::Syntax("expected at least one expression".into())
            .with_position(SourcePos::new(1, 1)));
    }

    let env = Env::global();
    let outcome = eval_tail_sequence(&program, env, ctx)?;
    resolve_tail_outcome(outcome, ctx)
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    pos: SourcePos,
}

impl Token {
    fn new(kind: TokenKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Bool(bool),
    Number(i64),
    Char(char),
    String(String),
    Symbol(String),
}

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Bool(bool),
    Number(i64),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

type StringRef = Rc<RefCell<String>>;

#[derive(Debug, Clone)]
enum Value {
    Bool(bool),
    Number(i64),
    String(StringRef),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<LambdaProcedure>),
    Void,
}

impl Value {
    fn string(value: impl Into<String>) -> Self {
        Self::String(Rc::new(RefCell::new(value.into())))
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Number(_) => "number",
            Self::String(_) => "string",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn display_repr(&self) -> String {
        self.render(RenderMode::Display)
    }

    fn render(&self, mode: RenderMode) -> String {
        match self {
            Self::Bool(true) => "#t".to_string(),
            Self::Bool(false) => "#f".to_string(),
            Self::Number(value) => value.to_string(),
            Self::String(value) => match mode {
                RenderMode::Display => value.borrow().clone(),
                RenderMode::Write => {
                    let value = value.borrow();
                    format!("\"{}\"", escape_string(value.as_str()))
                }
            },
            Self::Char(ch) => match mode {
                RenderMode::Display => ch.to_string(),
                RenderMode::Write => render_char(*ch),
            },
            Self::Symbol(value) => value.clone(),
            Self::List(items) => {
                let rendered = items
                    .iter()
                    .map(|item| item.render(mode))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({rendered})")
            }
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.render(RenderMode::Write))
    }
}

#[derive(Debug, Clone, Copy)]
enum RenderMode {
    Display,
    Write,
}

#[derive(Debug, Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    Null,
    List,
    Length,
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    StringCopy,
    StringSet,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    CharPred,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
}

impl Builtin {
    const ALL: [Self; 34] = [
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::LessThan,
        Self::GreaterThan,
        Self::Equal,
        Self::LessEqual,
        Self::Not,
        Self::Cons,
        Self::Car,
        Self::Cdr,
        Self::Null,
        Self::List,
        Self::Length,
        Self::Display,
        Self::Write,
        Self::Newline,
        Self::StringAppend,
        Self::StringLength,
        Self::StringCopy,
        Self::StringSet,
        Self::Substring,
        Self::StringToNumber,
        Self::NumberToString,
        Self::SymbolToString,
        Self::StringToSymbol,
        Self::StringRef,
        Self::CharPred,
        Self::StringPred,
        Self::NumberPred,
        Self::BooleanPred,
        Self::PairPred,
        Self::SymbolPred,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::Null => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::CharPred => "char?",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
        }
    }
}

type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug)]
struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Env {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }));

        {
            let mut bindings = env.borrow_mut();
            for builtin in Builtin::ALL {
                bindings
                    .bindings
                    .insert(builtin.name().to_string(), Value::Builtin(builtin));
            }
        }

        env
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        let (value, parent) = {
            let env_ref = env.borrow();
            (env_ref.bindings.get(name).cloned(), env_ref.parent.clone())
        };

        value.or_else(|| parent.and_then(|parent| Self::lookup(&parent, name)))
    }
}

#[derive(Debug)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Debug)]
enum TailOutcome {
    Value(Value),
    TailCall {
        procedure: Rc<LambdaProcedure>,
        args: Vec<Value>,
        pos: SourcePos,
    },
}

struct LetForm<'a> {
    name: Option<&'a str>,
    bindings_expr: &'a Expr,
    body: &'a [Expr],
}

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval_expr(expr, env.clone(), ctx)?;
    }
    Ok(last)
}

fn eval_tail_sequence(
    exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(TailOutcome::Value(Value::Void));
    };

    for expr in prefix {
        eval_expr(expr, env.clone(), ctx)?;
    }

    eval_tail_expr(last, env, ctx)
}

fn resolve_tail_outcome(
    mut outcome: TailOutcome,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    loop {
        match outcome {
            TailOutcome::Value(value) => return Ok(value),
            TailOutcome::TailCall {
                procedure,
                args,
                pos,
            } => {
                let local_env =
                    bind_call_env(&procedure, &args).map_err(|err| err.with_position(pos))?;
                outcome = eval_tail_sequence(&procedure.body, local_env, ctx)
                    .map_err(|err| err.with_position(pos))?;
            }
        }
    }
}

fn eval_tail_expr(
    expr: &Expr,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    match &expr.kind {
        ExprKind::Bool(value) => Ok(TailOutcome::Value(Value::Bool(*value))),
        ExprKind::Number(value) => Ok(TailOutcome::Value(Value::Number(*value))),
        ExprKind::Char(value) => Ok(TailOutcome::Value(Value::Char(*value))),
        ExprKind::String(value) => Ok(TailOutcome::Value(Value::string(value.clone()))),
        ExprKind::Symbol(name) => Env::lookup(&env, name)
            .map(TailOutcome::Value)
            .ok_or_else(|| EvalError::UnboundSymbol(name.clone()).with_position(expr.pos)),
        ExprKind::List(items) => {
            eval_tail_list(items, expr.pos, env, ctx).map_err(|err| err.with_position(expr.pos))
        }
    }
}

fn eval_expr(expr: &Expr, env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(Value::string(value.clone())),
        ExprKind::Symbol(name) => Env::lookup(&env, name)
            .ok_or_else(|| EvalError::UnboundSymbol(name.clone()).with_position(expr.pos)),
        ExprKind::List(items) => {
            eval_list(items, expr.pos, env, ctx).map_err(|err| err.with_position(expr.pos))
        }
    }
}

fn eval_list(
    items: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::Syntax("cannot evaluate an empty list".into()).with_position(pos));
    };

    match &head.kind {
        ExprKind::Symbol(name) if name == "and" => eval_and(args, env, ctx),
        ExprKind::Symbol(name) if name == "or" => eval_or(args, env, ctx),
        ExprKind::Symbol(name) if name == "if" => eval_if(args, env, ctx),
        ExprKind::Symbol(name) if name == "let" => eval_let(args, pos, env, ctx),
        ExprKind::Symbol(name) if name == "begin" => eval_begin(args, env, ctx),
        ExprKind::Symbol(name) if name == "cond" => eval_cond(args, env, ctx),
        ExprKind::Symbol(name) if name == "quote" => eval_quote(args),
        ExprKind::Symbol(name) if name == "define" => eval_define(args, env, ctx),
        ExprKind::Symbol(name) if name == "lambda" => eval_lambda(args, env),
        _ => {
            let procedure = eval_expr(head, env.clone(), ctx)?;
            let mut evaluated = Vec::with_capacity(args.len());
            for arg in args {
                evaluated.push(eval_expr(arg, env.clone(), ctx)?);
            }
            apply(procedure, &evaluated, pos, ctx)
        }
    }
}

fn eval_tail_list(
    items: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::Syntax("cannot evaluate an empty list".into()).with_position(pos));
    };

    match &head.kind {
        ExprKind::Symbol(name) if name == "and" => eval_tail_and(args, env, ctx),
        ExprKind::Symbol(name) if name == "or" => eval_tail_or(args, env, ctx),
        ExprKind::Symbol(name) if name == "if" => eval_tail_if(args, env, ctx),
        ExprKind::Symbol(name) if name == "let" => eval_tail_let(args, pos, env, ctx),
        ExprKind::Symbol(name) if name == "begin" => eval_tail_begin(args, env, ctx),
        ExprKind::Symbol(name) if name == "cond" => eval_tail_cond(args, env, ctx),
        ExprKind::Symbol(name) if name == "quote" => eval_quote(args).map(TailOutcome::Value),
        ExprKind::Symbol(name) if name == "define" => {
            eval_define(args, env, ctx).map(TailOutcome::Value)
        }
        ExprKind::Symbol(name) if name == "lambda" => {
            eval_lambda(args, env).map(TailOutcome::Value)
        }
        _ => {
            let procedure = eval_expr(head, env.clone(), ctx)?;
            let mut evaluated = Vec::with_capacity(args.len());
            for arg in args {
                evaluated.push(eval_expr(arg, env.clone(), ctx)?);
            }
            apply_in_tail_position(procedure, evaluated, pos, ctx)
        }
    }
}

fn eval_and(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for arg in args {
        last = eval_expr(arg, env.clone(), ctx)?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr(arg, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(wrong_arg_count("if", "exactly 3", args.len()));
    }

    let condition = eval_expr(&args[0], env.clone(), ctx)?;
    if condition.is_truthy() {
        eval_expr(&args[1], env, ctx)
    } else {
        eval_expr(&args[2], env, ctx)
    }
}

fn eval_tail_and(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Bool(true)));
    };

    for arg in prefix {
        let value = eval_expr(arg, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    eval_tail_expr(last, env, ctx)
}

fn eval_tail_or(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Bool(false)));
    };

    for arg in prefix {
        let value = eval_expr(arg, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    eval_tail_expr(last, env, ctx)
}

fn eval_tail_if(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    if args.len() != 3 {
        return Err(wrong_arg_count("if", "exactly 3", args.len()));
    }

    let condition = eval_expr(&args[0], env.clone(), ctx)?;
    if condition.is_truthy() {
        eval_tail_expr(&args[1], env, ctx)
    } else {
        eval_tail_expr(&args[2], env, ctx)
    }
}

fn eval_let(
    args: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let form = parse_let_form(args)?;
    let (names, values) = eval_let_bindings(form.bindings_expr, env.clone(), ctx)?;

    match form.name {
        Some(name) => {
            let local_env = Env::child(env);
            let procedure = Rc::new(LambdaProcedure {
                name: Some(name.to_string()),
                params: names,
                body: form.body.to_vec(),
                env: local_env.clone(),
            });
            Env::define(
                &local_env,
                name.to_string(),
                Value::Procedure(procedure.clone()),
            );
            resolve_tail_outcome(
                TailOutcome::TailCall {
                    procedure,
                    args: values,
                    pos,
                },
                ctx,
            )
        }
        None => {
            let local_env = bind_names(env, &names, &values);
            eval_sequence(form.body, local_env, ctx)
        }
    }
}

fn eval_tail_let(
    args: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let form = parse_let_form(args)?;
    let (names, values) = eval_let_bindings(form.bindings_expr, env.clone(), ctx)?;

    match form.name {
        Some(name) => {
            let local_env = Env::child(env);
            let procedure = Rc::new(LambdaProcedure {
                name: Some(name.to_string()),
                params: names,
                body: form.body.to_vec(),
                env: local_env.clone(),
            });
            Env::define(
                &local_env,
                name.to_string(),
                Value::Procedure(procedure.clone()),
            );
            Ok(TailOutcome::TailCall {
                procedure,
                args: values,
                pos,
            })
        }
        None => {
            let local_env = bind_names(env, &names, &values);
            eval_tail_sequence(form.body, local_env, ctx)
        }
    }
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_tail_begin(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    eval_tail_sequence(args, env, ctx)
}

fn eval_cond(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::Syntax("cond clauses must be lists".into()));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax("cond clauses cannot be empty".into()));
        };

        match &test.kind {
            ExprKind::Symbol(name) if name == "else" => {
                if index + 1 != args.len() {
                    return Err(EvalError::Syntax("cond else clause must be last".into()));
                }

                if body.is_empty() {
                    return Err(EvalError::Syntax("cond else clause requires a body".into()));
                }

                return eval_sequence(body, env, ctx);
            }
            _ => {
                let result = eval_expr(test, env.clone(), ctx)?;
                if result.is_truthy() {
                    if body.is_empty() {
                        return Ok(result);
                    }
                    return eval_sequence(body, env, ctx);
                }
            }
        }
    }

    Ok(Value::Void)
}

fn eval_tail_cond(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::Syntax("cond clauses must be lists".into()));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax("cond clauses cannot be empty".into()));
        };

        match &test.kind {
            ExprKind::Symbol(name) if name == "else" => {
                if index + 1 != args.len() {
                    return Err(EvalError::Syntax("cond else clause must be last".into()));
                }

                if body.is_empty() {
                    return Err(EvalError::Syntax("cond else clause requires a body".into()));
                }

                return eval_tail_sequence(body, env, ctx);
            }
            _ => {
                let result = eval_expr(test, env.clone(), ctx)?;
                if result.is_truthy() {
                    if body.is_empty() {
                        return Ok(TailOutcome::Value(result));
                    }
                    return eval_tail_sequence(body, env, ctx);
                }
            }
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arg_count("quote", "exactly 1", args.len()));
    }

    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Bool(value) => Value::Bool(*value),
        ExprKind::Number(value) => Value::Number(*value),
        ExprKind::Char(value) => Value::Char(*value),
        ExprKind::String(value) => Value::string(value.clone()),
        ExprKind::Symbol(value) => Value::Symbol(value.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_define(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(wrong_arg_count("define", "at least 2", args.len()));
    }

    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(wrong_arg_count("define", "exactly 2", args.len()));
            }

            let value = eval_expr(&args[1], env.clone(), ctx)?;
            Env::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let Some((name, params)) = signature.split_first() else {
                return Err(EvalError::Syntax("define requires a function name".into()));
            };
            let name = expect_symbol(name, "function name")?;
            let params = parse_parameters(params)?;
            let procedure = Value::Procedure(Rc::new(LambdaProcedure {
                name: Some(name.clone()),
                params,
                body: args[1..].to_vec(),
                env: env.clone(),
            }));

            Env::define(&env, name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax(
            "define requires a symbol or function signature".into(),
        )),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("lambda", "at least 2", args.len()));
    };
    if body.is_empty() {
        return Err(wrong_arg_count("lambda", "at least 2", args.len()));
    }

    let params = parse_parameter_list(params_expr)?;
    Ok(Value::Procedure(Rc::new(LambdaProcedure {
        name: None,
        params,
        body: body.to_vec(),
        env,
    })))
}

fn parse_parameter_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    match &expr.kind {
        ExprKind::List(items) => parse_parameters(items),
        _ => Err(EvalError::Syntax(
            "lambda parameter list must be a list".into(),
        )),
    }
}

fn parse_let_form<'a>(args: &'a [Expr]) -> Result<LetForm<'a>, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(wrong_arg_count("let", "at least 2", args.len()));
    };

    match &first.kind {
        ExprKind::Symbol(name) => {
            let Some((bindings_expr, body)) = rest.split_first() else {
                return Err(wrong_arg_count("let", "at least 3", args.len()));
            };
            if body.is_empty() {
                return Err(wrong_arg_count("let", "at least 3", args.len()));
            }

            Ok(LetForm {
                name: Some(name),
                bindings_expr,
                body,
            })
        }
        _ => {
            if rest.is_empty() {
                return Err(wrong_arg_count("let", "at least 2", args.len()));
            }

            Ok(LetForm {
                name: None,
                bindings_expr: first,
                body: rest,
            })
        }
    }
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::Syntax("let bindings must be a list".into()));
    };

    bindings
        .iter()
        .map(|binding| {
            let ExprKind::List(parts) = &binding.kind else {
                return Err(EvalError::Syntax(
                    "let binding must be a (name expr) pair".into(),
                ));
            };

            match parts.as_slice() {
                [name, value] => Ok((expect_symbol(name, "let binding name")?, value.clone())),
                _ => Err(EvalError::Syntax(
                    "let binding must be a (name expr) pair".into(),
                )),
            }
        })
        .collect()
}

fn eval_let_bindings(
    expr: &Expr,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<(Vec<String>, Vec<Value>), EvalError> {
    let bindings = parse_let_bindings(expr)?;
    let mut names = Vec::with_capacity(bindings.len());
    let mut values = Vec::with_capacity(bindings.len());

    for (name, expr) in bindings {
        names.push(name);
        values.push(eval_expr(&expr, env.clone(), ctx)?);
    }

    Ok((names, values))
}

fn parse_parameters(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    items
        .iter()
        .map(|expr| expect_symbol(expr, "parameter"))
        .collect()
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match &expr.kind {
        ExprKind::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::Syntax(format!("{context} must be a symbol"))),
    }
}

fn apply(
    function: Value,
    args: &[Value],
    pos: SourcePos,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    match function {
        Value::Builtin(builtin) => {
            apply_builtin(builtin, args, ctx).map_err(|err| err.with_position(pos))
        }
        Value::Procedure(procedure) => resolve_tail_outcome(
            TailOutcome::TailCall {
                procedure,
                args: args.to_vec(),
                pos,
            },
            ctx,
        ),
        other => Err(EvalError::NotAProcedure(other.to_string()).with_position(pos)),
    }
}

fn apply_in_tail_position(
    function: Value,
    args: Vec<Value>,
    pos: SourcePos,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    match function {
        Value::Builtin(builtin) => apply_builtin(builtin, &args, ctx)
            .map(TailOutcome::Value)
            .map_err(|err| err.with_position(pos)),
        Value::Procedure(procedure) => Ok(TailOutcome::TailCall {
            procedure,
            args,
            pos,
        }),
        other => Err(EvalError::NotAProcedure(other.to_string()).with_position(pos)),
    }
}

fn bind_call_env(procedure: &Rc<LambdaProcedure>, args: &[Value]) -> Result<EnvRef, EvalError> {
    if args.len() != procedure.params.len() {
        let expected = format!("exactly {}", procedure.params.len());
        let name = procedure.name.as_deref().unwrap_or("lambda");
        return Err(wrong_arg_count(name, &expected, args.len()));
    }

    Ok(bind_names(procedure.env.clone(), &procedure.params, args))
}

fn bind_names(parent: EnvRef, names: &[String], values: &[Value]) -> EnvRef {
    let local_env = Env::child(parent);
    for (name, value) in names.iter().zip(values.iter()) {
        Env::define(&local_env, name.clone(), value.clone());
    }
    local_env
}

fn apply_builtin(
    builtin: Builtin,
    args: &[Value],
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let name = builtin.name();
    match builtin {
        Builtin::Add => {
            let numbers = expect_numbers(name, args)?;
            let sum = numbers
                .iter()
                .try_fold(0_i64, |acc, value| acc.checked_add(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(sum))
        }
        Builtin::Sub => {
            let numbers = expect_numbers(name, args)?;
            match numbers.split_first() {
                None => Err(wrong_arg_count(name, "at least 1", args.len())),
                Some((first, [])) => first
                    .checked_neg()
                    .map(Value::Number)
                    .ok_or(EvalError::IntegerOverflow),
                Some((first, rest)) => {
                    let result = rest
                        .iter()
                        .try_fold(*first, |acc, value| acc.checked_sub(*value))
                        .ok_or(EvalError::IntegerOverflow)?;
                    Ok(Value::Number(result))
                }
            }
        }
        Builtin::Mul => {
            let numbers = expect_numbers(name, args)?;
            let product = numbers
                .iter()
                .try_fold(1_i64, |acc, value| acc.checked_mul(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(product))
        }
        Builtin::Div => {
            let numbers = expect_numbers(name, args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(wrong_arg_count(name, "at least 2", args.len()));
            };
            if rest.is_empty() {
                return Err(wrong_arg_count(name, "at least 2", args.len()));
            }

            let result = rest.iter().try_fold(*first, |acc, value| {
                if *value == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                acc.checked_div(*value).ok_or(EvalError::IntegerOverflow)
            })?;
            Ok(Value::Number(result))
        }
        Builtin::LessThan => compare_numbers(name, args, |left, right| left < right),
        Builtin::GreaterThan => compare_numbers(name, args, |left, right| left > right),
        Builtin::Equal => compare_numbers(name, args, |left, right| left == right),
        Builtin::LessEqual => compare_numbers(name, args, |left, right| left <= right),
        Builtin::Not => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            Ok(Value::Bool(!args[0].is_truthy()))
        }
        Builtin::Cons => {
            if args.len() != 2 {
                return Err(wrong_arg_count(name, "exactly 2", args.len()));
            }

            let list = expect_list_value(name, &args[1])?;
            let mut items = Vec::with_capacity(list.len() + 1);
            items.push(args[0].clone());
            items.extend(list.iter().cloned());
            Ok(Value::List(items))
        }
        Builtin::Car => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let list = expect_list_value(name, &args[0])?;
            list.first()
                .cloned()
                .ok_or_else(|| EvalError::TypeMismatch {
                    expected: "non-empty list for car".into(),
                    found: "empty list".into(),
                })
        }
        Builtin::Cdr => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let list = expect_list_value(name, &args[0])?;
            if list.is_empty() {
                return Err(EvalError::TypeMismatch {
                    expected: "non-empty list for cdr".into(),
                    found: "empty list".into(),
                });
            }
            Ok(Value::List(list[1..].to_vec()))
        }
        Builtin::Null => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            Ok(Value::Bool(
                matches!(&args[0], Value::List(items) if items.is_empty()),
            ))
        }
        Builtin::List => Ok(Value::List(args.to_vec())),
        Builtin::Length => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let list = expect_list_value(name, &args[0])?;
            let length = i64::try_from(list.len()).map_err(|_| EvalError::IntegerOverflow)?;
            Ok(Value::Number(length))
        }
        Builtin::Display => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            ctx.output.push_str(&args[0].display_repr());
            Ok(Value::Void)
        }
        Builtin::Write => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            ctx.output.push_str(&args[0].to_string());
            Ok(Value::Void)
        }
        Builtin::Newline => {
            if !args.is_empty() {
                return Err(wrong_arg_count(name, "exactly 0", args.len()));
            }
            ctx.output.push('\n');
            Ok(Value::Void)
        }
        Builtin::StringAppend => {
            let mut result = String::new();
            for arg in args {
                let string = expect_string_value(name, arg)?;
                result.push_str(string.borrow().as_str());
            }
            Ok(Value::string(result))
        }
        Builtin::StringLength => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let length = i64::try_from(string.borrow().chars().count())
                .map_err(|_| EvalError::IntegerOverflow)?;
            Ok(Value::Number(length))
        }
        Builtin::StringCopy => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let copied = string.borrow().clone();
            Ok(Value::string(copied))
        }
        Builtin::StringSet => {
            if args.len() != 3 {
                return Err(wrong_arg_count(name, "exactly 3", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let index = expect_number_value(name, &args[1])?;
            let ch = expect_char_value(name, &args[2])?;
            let mut chars = {
                let value = string.borrow();
                value.chars().collect::<Vec<_>>()
            };
            let len = chars.len();

            if index < 0 {
                return Err(EvalError::IndexOutOfBounds { index, len });
            }

            let index = usize::try_from(index).map_err(|_| EvalError::IntegerOverflow)?;
            if index >= len {
                return Err(EvalError::IndexOutOfBounds {
                    index: i64::try_from(index).map_err(|_| EvalError::IntegerOverflow)?,
                    len,
                });
            }

            chars[index] = ch;
            *string.borrow_mut() = chars.into_iter().collect();
            Ok(Value::Void)
        }
        Builtin::Substring => {
            if args.len() != 3 {
                return Err(wrong_arg_count(name, "exactly 3", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let start = expect_number_value(name, &args[1])?;
            let end = expect_number_value(name, &args[2])?;
            let value = string.borrow();
            let len = value.chars().count();
            let len_i64 = i64::try_from(len).map_err(|_| EvalError::IntegerOverflow)?;

            if start < 0 || end < start || end > len_i64 {
                return Err(EvalError::InvalidRange { start, end, len });
            }

            let start = usize::try_from(start).map_err(|_| EvalError::IntegerOverflow)?;
            let end = usize::try_from(end).map_err(|_| EvalError::IntegerOverflow)?;
            let result: String = value.chars().skip(start).take(end - start).collect();
            Ok(Value::string(result))
        }
        Builtin::StringToNumber => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let value = string.borrow().clone();
            match value.parse::<i64>() {
                Ok(value) => Ok(Value::Number(value)),
                Err(_) => Ok(Value::Bool(false)),
            }
        }
        Builtin::NumberToString => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let number = expect_number_value(name, &args[0])?;
            Ok(Value::string(number.to_string()))
        }
        Builtin::SymbolToString => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let symbol = expect_symbol_value(name, &args[0])?;
            Ok(Value::string(symbol.to_string()))
        }
        Builtin::StringToSymbol => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let symbol = string.borrow().clone();
            Ok(Value::Symbol(symbol))
        }
        Builtin::StringRef => {
            if args.len() != 2 {
                return Err(wrong_arg_count(name, "exactly 2", args.len()));
            }

            let string = expect_string_value(name, &args[0])?;
            let index = expect_number_value(name, &args[1])?;
            let value = string.borrow();
            let len = value.chars().count();

            if index < 0 {
                return Err(EvalError::IndexOutOfBounds { index, len });
            }

            let index = usize::try_from(index).map_err(|_| EvalError::IntegerOverflow)?;
            let ch = value
                .chars()
                .nth(index)
                .ok_or(EvalError::IndexOutOfBounds {
                    index: i64::try_from(index).map_err(|_| EvalError::IntegerOverflow)?,
                    len,
                })?;
            Ok(Value::Char(ch))
        }
        Builtin::CharPred => unary_predicate(name, args, |value| matches!(value, Value::Char(_))),
        Builtin::StringPred => {
            unary_predicate(name, args, |value| matches!(value, Value::String(_)))
        }
        Builtin::NumberPred => {
            unary_predicate(name, args, |value| matches!(value, Value::Number(_)))
        }
        Builtin::BooleanPred => {
            unary_predicate(name, args, |value| matches!(value, Value::Bool(_)))
        }
        Builtin::PairPred => unary_predicate(
            name,
            args,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        Builtin::SymbolPred => {
            unary_predicate(name, args, |value| matches!(value, Value::Symbol(_)))
        }
    }
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let is_true = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Bool(is_true))
}

fn expect_numbers(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| expect_number_value(name, value))
        .collect()
}

fn expect_list_value<'a>(name: &str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(EvalError::TypeMismatch {
            expected: format!("list for {name}"),
            found: other.type_name().to_string(),
        }),
    }
}

fn unary_predicate(
    name: &str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arg_count(name, "exactly 1", args.len()));
    }

    Ok(Value::Bool(predicate(&args[0])))
}

fn expect_number_value(name: &str, value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        other => Err(EvalError::TypeMismatch {
            expected: format!("number for {name}"),
            found: other.type_name().to_string(),
        }),
    }
}

fn expect_string_value(name: &str, value: &Value) -> Result<StringRef, EvalError> {
    match value {
        Value::String(string) => Ok(string.clone()),
        other => Err(EvalError::TypeMismatch {
            expected: format!("string for {name}"),
            found: other.type_name().to_string(),
        }),
    }
}

fn expect_char_value(name: &str, value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(EvalError::TypeMismatch {
            expected: format!("char for {name}"),
            found: other.type_name().to_string(),
        }),
    }
}

fn expect_symbol_value<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(symbol) => Ok(symbol),
        other => Err(EvalError::TypeMismatch {
            expected: format!("symbol for {name}"),
            found: other.type_name().to_string(),
        }),
    }
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgumentCount {
        name: name.to_string(),
        expected: expected.to_string(),
        got,
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn render_char(ch: char) -> String {
    match ch {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        _ => format!("#\\{ch}"),
    }
}

fn advance_position(pos: &mut SourcePos, ch: char) {
    if ch == '\n' {
        pos.line += 1;
        pos.col = 1;
    } else {
        pos.col += 1;
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let mut index = 0;
    let mut pos = SourcePos::new(1, 1);

    while index < input.len() {
        let ch = input[index..]
            .chars()
            .next()
            .expect("index always points to a valid character boundary");

        match ch {
            c if c.is_whitespace() => {
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            ';' => {
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
                while index < input.len() {
                    let next = input[index..]
                        .chars()
                        .next()
                        .expect("index always points to a valid character boundary");
                    index += next.len_utf8();
                    advance_position(&mut pos, next);
                    if next == '\n' {
                        break;
                    }
                }
            }
            '(' => {
                tokens.push(Token::new(TokenKind::LParen, pos));
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            ')' => {
                tokens.push(Token::new(TokenKind::RParen, pos));
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            '\'' => {
                tokens.push(Token::new(TokenKind::Quote, pos));
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            '"' => {
                let start_pos = pos;
                let (string, next_index, next_pos) = parse_string(input, index, pos)?;
                tokens.push(Token::new(TokenKind::String(string), start_pos));
                index = next_index;
                pos = next_pos;
            }
            _ => {
                let start = index;
                let start_pos = pos;
                while index < input.len() {
                    let next = input[index..]
                        .chars()
                        .next()
                        .expect("index always points to a valid character boundary");
                    if next.is_whitespace()
                        || next == '('
                        || next == ')'
                        || next == '\''
                        || next == ';'
                    {
                        break;
                    }
                    index += next.len_utf8();
                    advance_position(&mut pos, next);
                }

                let atom = &input[start..index];
                tokens.push(Token::new(parse_atom(atom), start_pos));
            }
        }
    }

    Ok(tokens)
}

fn parse_string(
    input: &str,
    start: usize,
    start_pos: SourcePos,
) -> Result<(String, usize, SourcePos), EvalError> {
    let mut result = String::new();
    let mut pos = start_pos;
    let mut index = start + 1;
    advance_position(&mut pos, '"');

    while index < input.len() {
        let ch = input[index..]
            .chars()
            .next()
            .expect("index always points to a valid character boundary");
        index += ch.len_utf8();
        advance_position(&mut pos, ch);

        match ch {
            '"' => return Ok((result, index, pos)),
            '\\' => {
                let escape_pos = pos;
                let escaped = input[index..].chars().next().ok_or_else(|| {
                    EvalError::Syntax("unterminated string literal".into()).with_position(start_pos)
                })?;
                index += escaped.len_utf8();
                advance_position(&mut pos, escaped);
                match escaped {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    _ => {
                        return Err(EvalError::Syntax(format!(
                            "unsupported escape sequence: \\{escaped}"
                        ))
                        .with_position(escape_pos));
                    }
                }
            }
            _ => result.push(ch),
        }
    }

    Err(EvalError::Syntax("unterminated string literal".into()).with_position(start_pos))
}

fn parse_atom(atom: &str) -> TokenKind {
    match atom {
        "#t" => TokenKind::Bool(true),
        "#f" => TokenKind::Bool(false),
        _ => match parse_char_literal(atom) {
            Some(value) => TokenKind::Char(value),
            None => match atom.parse::<i64>() {
                Ok(value) => TokenKind::Number(value),
                Err(_) => TokenKind::Symbol(atom.to_string()),
            },
        },
    }
}

fn parse_char_literal(atom: &str) -> Option<char> {
    let literal = atom.strip_prefix("#\\")?;
    match literal {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = literal.chars();
            let ch = chars.next()?;
            if chars.next().is_none() {
                Some(ch)
            } else {
                None
            }
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    last_pos: SourcePos,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            last_pos: SourcePos::new(1, 1),
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        while self.index < self.tokens.len() {
            expressions.push(self.parse_expr()?);
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self.tokens.get(self.index).cloned().ok_or_else(|| {
            EvalError::Syntax("unexpected end of input".into()).with_position(self.last_pos)
        })?;
        self.index += 1;
        self.last_pos = token.pos;

        match token.kind {
            TokenKind::LParen => {
                let mut items = Vec::new();
                while self.index < self.tokens.len() {
                    if matches!(
                        self.tokens.get(self.index).map(|next| &next.kind),
                        Some(TokenKind::RParen)
                    ) {
                        self.last_pos = self.tokens[self.index].pos;
                        self.index += 1;
                        return Ok(Expr::new(ExprKind::List(items), token.pos));
                    }
                    items.push(self.parse_expr()?);
                }
                Err(EvalError::Syntax("missing ')'".into()).with_position(token.pos))
            }
            TokenKind::RParen => {
                Err(EvalError::Syntax("unexpected ')'".into()).with_position(token.pos))
            }
            TokenKind::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::new(
                    ExprKind::List(vec![
                        Expr::new(ExprKind::Symbol("quote".to_string()), token.pos),
                        quoted,
                    ]),
                    token.pos,
                ))
            }
            TokenKind::Bool(value) => Ok(Expr::new(ExprKind::Bool(value), token.pos)),
            TokenKind::Number(value) => Ok(Expr::new(ExprKind::Number(value), token.pos)),
            TokenKind::Char(value) => Ok(Expr::new(ExprKind::Char(value), token.pos)),
            TokenKind::String(value) => Ok(Expr::new(ExprKind::String(value), token.pos)),
            TokenKind::Symbol(name) => Ok(Expr::new(ExprKind::Symbol(name), token.pos)),
        }
    }
}
