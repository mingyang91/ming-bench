pub mod error;

pub use error::EvalError;
use error::SourcePos;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

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
    macros: HashMap<String, MacroDefinition>,
    gensym_counter: usize,
}

impl EvalContext {
    fn fresh_generated_name(&mut self, hint: &str) -> String {
        self.gensym_counter += 1;
        format!("__macro_{}_{}", self.gensym_counter, hint)
    }
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
    run_machine(&program, env, ctx)
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
type ContinuationRef = Rc<MachineContinuation>;
type BindingRef = Rc<RefCell<Value>>;

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
    Continuation(ContinuationRef),
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
            Self::Builtin(_) | Self::Procedure(_) | Self::Continuation(_) => "procedure",
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
            Self::Builtin(_) | Self::Procedure(_) | Self::Continuation(_) => {
                "#<procedure>".to_string()
            }
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
    Apply,
    CallCc,
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
    const ALL: [Self; 36] = [
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
        Self::Apply,
        Self::CallCc,
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
            Self::Apply => "apply",
            Self::CallCc => "call/cc",
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
    bindings: HashMap<String, BindingRef>,
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
                    .insert(
                        builtin.name().to_string(),
                        Rc::new(RefCell::new(Value::Builtin(builtin))),
                    );
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
        let existing = {
            let env_ref = env.borrow();
            env_ref.bindings.get(&name).cloned()
        };

        match existing {
            Some(binding) => {
                *binding.borrow_mut() = value;
            }
            None => Self::define_alias(env, name, Rc::new(RefCell::new(value))),
        }
    }

    fn define_alias(env: &EnvRef, name: String, binding: BindingRef) {
        env.borrow_mut().bindings.insert(name, binding);
    }

    fn set(env: &EnvRef, name: &str, value: Value) -> Result<(), EvalError> {
        let binding = Self::lookup_cell(env, name)
            .ok_or_else(|| EvalError::UnboundSymbol(name.to_string()))?;
        *binding.borrow_mut() = value;
        Ok(())
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        Self::lookup_cell(env, name).map(|binding| binding.borrow().clone())
    }

    fn lookup_cell(env: &EnvRef, name: &str) -> Option<BindingRef> {
        let (value, parent) = {
            let env_ref = env.borrow();
            (env_ref.bindings.get(name).cloned(), env_ref.parent.clone())
        };

        value.or_else(|| parent.and_then(|parent| Self::lookup_cell(&parent, name)))
    }
}

#[derive(Debug)]
struct LambdaProcedure {
    name: Option<String>,
    params: Parameters,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Debug)]
struct Parameters {
    required: Vec<String>,
    rest: Option<String>,
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

#[derive(Debug, Clone)]
struct MacroDefinition {
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    definition_env: EnvRef,
}

#[derive(Debug, Clone)]
struct MacroRule {
    pattern_args: Vec<Expr>,
    template: Expr,
}

#[derive(Debug, Clone)]
enum MatchBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

#[derive(Debug, Clone)]
struct SyntaxExpr {
    kind: SyntaxExprKind,
    pos: SourcePos,
}

impl SyntaxExpr {
    fn raw(expr: Expr) -> Self {
        Self {
            pos: expr.pos,
            kind: SyntaxExprKind::Raw(expr),
        }
    }

    fn generated(kind: SyntaxExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug, Clone)]
enum SyntaxExprKind {
    Raw(Expr),
    Bool(bool),
    Number(i64),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<SyntaxExpr>),
}

#[derive(Debug, Clone)]
enum MachineLetMode {
    Plain,
    Named(String),
}

#[derive(Debug)]
enum MachineContinuation {
    Halt,
    ProcedureReturn {
        next: ContinuationRef,
    },
    CallCcReturn {
        resume: ContinuationRef,
        suspend: Option<ContinuationRef>,
    },
    Sequence {
        exprs: Rc<[Expr]>,
        index: usize,
        env: EnvRef,
        next: ContinuationRef,
    },
    If {
        consequent: Expr,
        alternate: Expr,
        env: EnvRef,
        next: ContinuationRef,
    },
    And {
        exprs: Rc<[Expr]>,
        index: usize,
        env: EnvRef,
        next: ContinuationRef,
    },
    Or {
        exprs: Rc<[Expr]>,
        index: usize,
        env: EnvRef,
        next: ContinuationRef,
    },
    CallHead {
        args: Rc<[Expr]>,
        env: EnvRef,
        pos: SourcePos,
        next: ContinuationRef,
    },
    CallArg {
        function: Value,
        args: Rc<[Expr]>,
        index: usize,
        evaluated: Vec<Value>,
        env: EnvRef,
        pos: SourcePos,
        next: ContinuationRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
        next: ContinuationRef,
    },
    SetValue {
        name: String,
        env: EnvRef,
        pos: SourcePos,
        next: ContinuationRef,
    },
    LetBinding {
        mode: MachineLetMode,
        names: Rc<[String]>,
        exprs: Rc<[Expr]>,
        index: usize,
        values: Vec<Value>,
        env: EnvRef,
        body: Rc<[Expr]>,
        pos: SourcePos,
        next: ContinuationRef,
    },
    Cond {
        clauses: Rc<[Expr]>,
        index: usize,
        env: EnvRef,
        next: ContinuationRef,
    },
}

#[derive(Debug)]
enum MachineControl {
    Expr(Expr, EnvRef),
    Apply {
        function: Value,
        args: Vec<Value>,
        pos: SourcePos,
    },
    Value(Value),
}

fn run_machine(exprs: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let halt = Rc::new(MachineContinuation::Halt);
    let (mut control, mut cont) = schedule_sequence(exprs, env, halt);

    loop {
        match control {
            MachineControl::Expr(expr, env) => {
                let (next_control, next_cont) = match &expr.kind {
                    ExprKind::Bool(value) => (MachineControl::Value(Value::Bool(*value)), cont),
                    ExprKind::Number(value) => (MachineControl::Value(Value::Number(*value)), cont),
                    ExprKind::Char(value) => (MachineControl::Value(Value::Char(*value)), cont),
                    ExprKind::String(value) => {
                        (MachineControl::Value(Value::string(value.clone())), cont)
                    }
                    ExprKind::Symbol(name) => {
                        let value = Env::lookup(&env, name).ok_or_else(|| {
                            EvalError::UnboundSymbol(name.clone()).with_position(expr.pos)
                        })?;
                        (MachineControl::Value(value), cont)
                    }
                    ExprKind::List(items) => {
                        schedule_list_eval(items, expr.pos, env, cont, ctx)
                            .map_err(|err| err.with_position(expr.pos))?
                    }
                };

                control = next_control;
                cont = next_cont;
            }
            MachineControl::Apply {
                function,
                args,
                pos,
            } => match function {
                Value::Builtin(Builtin::Apply) => {
                    let (function, args) =
                        expand_apply_args(&args).map_err(|err| err.with_position(pos))?;
                    control = MachineControl::Apply {
                        function,
                        args,
                        pos,
                    };
                }
                Value::Builtin(Builtin::CallCc) => {
                    if args.len() != 1 {
                        return Err(wrong_arg_count("call/cc", "exactly 1", args.len())
                            .with_position(pos));
                    }

                    let resume = cont.clone();
                    let suspend = find_enclosing_procedure_caller(&resume);
                    control = MachineControl::Apply {
                        function: args[0].clone(),
                        args: vec![Value::Continuation(resume.clone())],
                        pos,
                    };
                    cont = Rc::new(MachineContinuation::CallCcReturn { resume, suspend });
                }
                Value::Builtin(builtin) => {
                    let value = apply_builtin(builtin, &args, ctx)
                        .map_err(|err| err.with_position(pos))?;
                    control = MachineControl::Value(value);
                }
                Value::Procedure(procedure) => {
                    let local_env =
                        bind_call_env(&procedure, &args).map_err(|err| err.with_position(pos))?;
                    let body_cont = if matches!(
                        cont.as_ref(),
                        MachineContinuation::Halt | MachineContinuation::ProcedureReturn { .. }
                    ) {
                        cont.clone()
                    } else {
                        Rc::new(MachineContinuation::ProcedureReturn { next: cont.clone() })
                    };
                    let (next_control, next_cont) =
                        schedule_sequence(&procedure.body, local_env, body_cont);
                    control = next_control;
                    cont = next_cont;
                }
                Value::Continuation(saved) => {
                    if args.len() != 1 {
                        return Err(wrong_arg_count("continuation", "exactly 1", args.len())
                            .with_position(pos));
                    }

                    control = MachineControl::Value(args[0].clone());
                    cont = saved;
                }
                other => {
                    return Err(EvalError::NotAProcedure(other.to_string()).with_position(pos));
                }
            },
            MachineControl::Value(value) => match cont.as_ref() {
                MachineContinuation::Halt => return Ok(value),
                MachineContinuation::ProcedureReturn { next } => {
                    control = MachineControl::Value(value);
                    cont = next.clone();
                }
                MachineContinuation::CallCcReturn { resume, suspend } => {
                    control = MachineControl::Value(value.clone());
                    cont = if matches!(value, Value::Void) {
                        suspend.clone().unwrap_or_else(|| resume.clone())
                    } else {
                        resume.clone()
                    };
                }
                MachineContinuation::Sequence {
                    exprs,
                    index,
                    env,
                    next,
                } => {
                    let expr = exprs[*index].clone();
                    let next_cont = if *index + 1 < exprs.len() {
                        Rc::new(MachineContinuation::Sequence {
                            exprs: exprs.clone(),
                            index: index + 1,
                            env: env.clone(),
                            next: next.clone(),
                        })
                    } else {
                        next.clone()
                    };

                    control = MachineControl::Expr(expr, env.clone());
                    cont = next_cont;
                }
                MachineContinuation::If {
                    consequent,
                    alternate,
                    env,
                    next,
                } => {
                    let branch = if value.is_truthy() {
                        consequent.clone()
                    } else {
                        alternate.clone()
                    };
                    control = MachineControl::Expr(branch, env.clone());
                    cont = next.clone();
                }
                MachineContinuation::And {
                    exprs,
                    index,
                    env,
                    next,
                } => {
                    if !value.is_truthy() || *index >= exprs.len() {
                        control = MachineControl::Value(value);
                        cont = next.clone();
                    } else {
                        let expr = exprs[*index].clone();
                        let next_cont = if *index + 1 < exprs.len() {
                            Rc::new(MachineContinuation::And {
                                exprs: exprs.clone(),
                                index: index + 1,
                                env: env.clone(),
                                next: next.clone(),
                            })
                        } else {
                            next.clone()
                        };
                        control = MachineControl::Expr(expr, env.clone());
                        cont = next_cont;
                    }
                }
                MachineContinuation::Or {
                    exprs,
                    index,
                    env,
                    next,
                } => {
                    if value.is_truthy() || *index >= exprs.len() {
                        control = MachineControl::Value(value);
                        cont = next.clone();
                    } else {
                        let expr = exprs[*index].clone();
                        let next_cont = if *index + 1 < exprs.len() {
                            Rc::new(MachineContinuation::Or {
                                exprs: exprs.clone(),
                                index: index + 1,
                                env: env.clone(),
                                next: next.clone(),
                            })
                        } else {
                            next.clone()
                        };
                        control = MachineControl::Expr(expr, env.clone());
                        cont = next_cont;
                    }
                }
                MachineContinuation::CallHead {
                    args,
                    env,
                    pos,
                    next,
                } => {
                    if args.is_empty() {
                        control = MachineControl::Apply {
                            function: value,
                            args: Vec::new(),
                            pos: *pos,
                        };
                        cont = next.clone();
                    } else {
                        let last_index = args.len() - 1;
                        control = MachineControl::Expr(args[last_index].clone(), env.clone());
                        cont = Rc::new(MachineContinuation::CallArg {
                            function: value,
                            args: args.clone(),
                            index: last_index,
                            evaluated: Vec::new(),
                            env: env.clone(),
                            pos: *pos,
                            next: next.clone(),
                        });
                    }
                }
                MachineContinuation::CallArg {
                    function,
                    args,
                    index,
                    evaluated,
                    env,
                    pos,
                    next,
                } => {
                    let mut evaluated = evaluated.clone();
                    evaluated.insert(0, value);

                    if *index == 0 {
                        control = MachineControl::Apply {
                            function: function.clone(),
                            args: evaluated,
                            pos: *pos,
                        };
                        cont = next.clone();
                    } else {
                        let next_index = index - 1;
                        control = MachineControl::Expr(args[next_index].clone(), env.clone());
                        cont = Rc::new(MachineContinuation::CallArg {
                            function: function.clone(),
                            args: args.clone(),
                            index: next_index,
                            evaluated,
                            env: env.clone(),
                            pos: *pos,
                            next: next.clone(),
                        });
                    }
                }
                MachineContinuation::DefineValue { name, env, next } => {
                    Env::define(env, name.clone(), value);
                    control = MachineControl::Value(Value::Void);
                    cont = next.clone();
                }
                MachineContinuation::SetValue {
                    name,
                    env,
                    pos,
                    next,
                } => {
                    Env::set(env, name, value).map_err(|err| err.with_position(*pos))?;
                    control = MachineControl::Value(Value::Void);
                    cont = next.clone();
                }
                MachineContinuation::LetBinding {
                    mode,
                    names,
                    exprs,
                    index,
                    values,
                    env,
                    body,
                    pos,
                    next,
                } => {
                    let mut values = values.clone();
                    values.push(value);

                    if *index >= exprs.len() {
                        let (next_control, next_cont) = finish_let(
                            mode.clone(),
                            names.as_ref(),
                            values,
                            env.clone(),
                            body.clone(),
                            *pos,
                            next.clone(),
                        )
                        .map_err(|err| err.with_position(*pos))?;
                        control = next_control;
                        cont = next_cont;
                    } else {
                        control = MachineControl::Expr(exprs[*index].clone(), env.clone());
                        cont = Rc::new(MachineContinuation::LetBinding {
                            mode: mode.clone(),
                            names: names.clone(),
                            exprs: exprs.clone(),
                            index: index + 1,
                            values,
                            env: env.clone(),
                            body: body.clone(),
                            pos: *pos,
                            next: next.clone(),
                        });
                    }
                }
                MachineContinuation::Cond {
                    clauses,
                    index,
                    env,
                    next,
                } => {
                    let clause = &clauses[*index];
                    let ExprKind::List(items) = &clause.kind else {
                        return Err(EvalError::Syntax("cond clauses must be lists".into())
                            .with_position(clause.pos));
                    };
                    let Some((_test, body)) = items.split_first() else {
                        return Err(EvalError::Syntax("cond clauses cannot be empty".into())
                            .with_position(clause.pos));
                    };

                    if value.is_truthy() {
                        if body.is_empty() {
                            control = MachineControl::Value(value);
                            cont = next.clone();
                        } else {
                            let (next_control, next_cont) =
                                schedule_sequence(body, env.clone(), next.clone());
                            control = next_control;
                            cont = next_cont;
                        }
                    } else {
                        let (next_control, next_cont) = schedule_cond(
                            clauses.as_ref(),
                            index + 1,
                            env.clone(),
                            next.clone(),
                        )
                        .map_err(|err| err.with_position(clause.pos))?;
                        control = next_control;
                        cont = next_cont;
                    }
                }
            },
        }
    }
}

fn schedule_sequence(
    exprs: &[Expr],
    env: EnvRef,
    next: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    let Some((first, _rest)) = exprs.split_first() else {
        return (MachineControl::Value(Value::Void), next);
    };

    if exprs.len() == 1 {
        return (MachineControl::Expr(first.clone(), env), next);
    }

    let exprs: Rc<[Expr]> = Rc::from(exprs.to_vec());
    let cont = Rc::new(MachineContinuation::Sequence {
        exprs,
        index: 1,
        env: env.clone(),
        next,
    });
    (MachineControl::Expr(first.clone(), env), cont)
}

fn find_enclosing_procedure_caller(cont: &ContinuationRef) -> Option<ContinuationRef> {
    match cont.as_ref() {
        MachineContinuation::Halt => None,
        MachineContinuation::ProcedureReturn { next } => Some(next.clone()),
        MachineContinuation::CallCcReturn { resume, .. } => {
            find_enclosing_procedure_caller(resume)
        }
        MachineContinuation::Sequence { next, .. }
        | MachineContinuation::If { next, .. }
        | MachineContinuation::And { next, .. }
        | MachineContinuation::Or { next, .. }
        | MachineContinuation::CallHead { next, .. }
        | MachineContinuation::CallArg { next, .. }
        | MachineContinuation::DefineValue { next, .. }
        | MachineContinuation::SetValue { next, .. }
        | MachineContinuation::LetBinding { next, .. }
        | MachineContinuation::Cond { next, .. } => find_enclosing_procedure_caller(next),
    }
}

fn schedule_list_eval(
    items: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    next: ContinuationRef,
    ctx: &mut EvalContext,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::Syntax("cannot evaluate an empty list".into()).with_position(pos));
    };

    if let Some(expanded) = try_expand_macro_invocation(items, pos, env.clone(), ctx)? {
        return Ok((MachineControl::Expr(expanded, env), next));
    }

    match &head.kind {
        ExprKind::Symbol(name) if name == "and" => Ok(schedule_and(args, env, next)),
        ExprKind::Symbol(name) if name == "or" => Ok(schedule_or(args, env, next)),
        ExprKind::Symbol(name) if name == "if" => {
            if args.len() != 3 {
                return Err(wrong_arg_count("if", "exactly 3", args.len()));
            }

            Ok((
                MachineControl::Expr(args[0].clone(), env.clone()),
                Rc::new(MachineContinuation::If {
                    consequent: args[1].clone(),
                    alternate: args[2].clone(),
                    env,
                    next,
                }),
            ))
        }
        ExprKind::Symbol(name) if name == "let" => schedule_let(args, pos, env, next),
        ExprKind::Symbol(name) if name == "begin" => Ok(schedule_sequence(args, env, next)),
        ExprKind::Symbol(name) if name == "cond" => schedule_cond(args, 0, env, next),
        ExprKind::Symbol(name) if name == "quote" => {
            Ok((MachineControl::Value(eval_quote(args)?), next))
        }
        ExprKind::Symbol(name) if name == "define" => schedule_define(args, env, next),
        ExprKind::Symbol(name) if name == "define-syntax" => {
            let (name, definition) = parse_define_syntax(args, env)?;
            ctx.macros.insert(name, definition);
            Ok((MachineControl::Value(Value::Void), next))
        }
        ExprKind::Symbol(name) if name == "set!" => schedule_set(args, pos, env, next),
        ExprKind::Symbol(name) if name == "lambda" => {
            Ok((MachineControl::Value(eval_lambda(args, env)?), next))
        }
        _ => Ok(schedule_application(head, args, env, pos, next)),
    }
}

fn schedule_and(
    args: &[Expr],
    env: EnvRef,
    next: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    let Some((first, _rest)) = args.split_first() else {
        return (MachineControl::Value(Value::Bool(true)), next);
    };

    if args.len() == 1 {
        return (MachineControl::Expr(first.clone(), env), next);
    }

    let exprs: Rc<[Expr]> = Rc::from(args.to_vec());
    let cont = Rc::new(MachineContinuation::And {
        exprs,
        index: 1,
        env: env.clone(),
        next,
    });
    (MachineControl::Expr(first.clone(), env), cont)
}

fn schedule_or(
    args: &[Expr],
    env: EnvRef,
    next: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    let Some((first, _rest)) = args.split_first() else {
        return (MachineControl::Value(Value::Bool(false)), next);
    };

    if args.len() == 1 {
        return (MachineControl::Expr(first.clone(), env), next);
    }

    let exprs: Rc<[Expr]> = Rc::from(args.to_vec());
    let cont = Rc::new(MachineContinuation::Or {
        exprs,
        index: 1,
        env: env.clone(),
        next,
    });
    (MachineControl::Expr(first.clone(), env), cont)
}

fn schedule_application(
    head: &Expr,
    args: &[Expr],
    env: EnvRef,
    pos: SourcePos,
    next: ContinuationRef,
) -> (MachineControl, ContinuationRef) {
    let args: Rc<[Expr]> = Rc::from(args.to_vec());
    let cont = Rc::new(MachineContinuation::CallHead {
        args,
        env: env.clone(),
        pos,
        next,
    });
    (MachineControl::Expr(head.clone(), env), cont)
}

fn schedule_define(
    args: &[Expr],
    env: EnvRef,
    next: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    if args.len() < 2 {
        return Err(wrong_arg_count("define", "at least 2", args.len()));
    }

    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(wrong_arg_count("define", "exactly 2", args.len()));
            }

            Ok((
                MachineControl::Expr(args[1].clone(), env.clone()),
                Rc::new(MachineContinuation::DefineValue {
                    name: name.clone(),
                    env,
                    next,
                }),
            ))
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
            Ok((MachineControl::Value(Value::Void), next))
        }
        _ => Err(EvalError::Syntax(
            "define requires a symbol or function signature".into(),
        )),
    }
}

fn schedule_set(
    args: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    next: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    if args.len() != 2 {
        return Err(wrong_arg_count("set!", "exactly 2", args.len()));
    }

    let name = expect_symbol(&args[0], "set! target")?;
    Ok((
        MachineControl::Expr(args[1].clone(), env.clone()),
        Rc::new(MachineContinuation::SetValue {
            name,
            env,
            pos,
            next,
        }),
    ))
}

fn schedule_let(
    args: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    next: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let form = parse_let_form(args)?;
    let bindings = parse_let_bindings(form.bindings_expr)?;
    let mode = match form.name {
        Some(name) => MachineLetMode::Named(name.to_string()),
        None => MachineLetMode::Plain,
    };
    let names: Vec<String> = bindings.iter().map(|(name, _)| name.clone()).collect();
    let exprs: Vec<Expr> = bindings.into_iter().map(|(_, expr)| expr).collect();
    let body: Rc<[Expr]> = Rc::from(form.body.to_vec());

    if exprs.is_empty() {
        return finish_let(mode, &names, Vec::new(), env, body, pos, next);
    }

    let names: Rc<[String]> = Rc::from(names);
    let exprs: Rc<[Expr]> = Rc::from(exprs);

    Ok((
        MachineControl::Expr(exprs[0].clone(), env.clone()),
        Rc::new(MachineContinuation::LetBinding {
            mode,
            names,
            exprs: exprs.clone(),
            index: 1,
            values: Vec::new(),
            env,
            body,
            pos,
            next,
        }),
    ))
}

fn finish_let(
    mode: MachineLetMode,
    names: &[String],
    values: Vec<Value>,
    env: EnvRef,
    body: Rc<[Expr]>,
    pos: SourcePos,
    next: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    match mode {
        MachineLetMode::Plain => {
            let local_env = bind_names(env, names, &values);
            Ok(schedule_sequence(body.as_ref(), local_env, next))
        }
        MachineLetMode::Named(name) => {
            let local_env = Env::child(env);
            let procedure = Rc::new(LambdaProcedure {
                name: Some(name.clone()),
                params: Parameters {
                    required: names.to_vec(),
                    rest: None,
                },
                body: body.as_ref().to_vec(),
                env: local_env.clone(),
            });
            Env::define(&local_env, name, Value::Procedure(procedure.clone()));
            Ok((
                MachineControl::Apply {
                    function: Value::Procedure(procedure),
                    args: values,
                    pos,
                },
                next,
            ))
        }
    }
}

fn schedule_cond(
    clauses: &[Expr],
    index: usize,
    env: EnvRef,
    next: ContinuationRef,
) -> Result<(MachineControl, ContinuationRef), EvalError> {
    let Some(clause) = clauses.get(index) else {
        return Ok((MachineControl::Value(Value::Void), next));
    };

    let ExprKind::List(items) = &clause.kind else {
        return Err(EvalError::Syntax("cond clauses must be lists".into())
            .with_position(clause.pos));
    };
    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::Syntax("cond clauses cannot be empty".into())
            .with_position(clause.pos));
    };

    match &test.kind {
        ExprKind::Symbol(name) if name == "else" => {
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax("cond else clause must be last".into())
                    .with_position(clause.pos));
            }
            if body.is_empty() {
                return Err(EvalError::Syntax("cond else clause requires a body".into())
                    .with_position(clause.pos));
            }

            Ok(schedule_sequence(body, env, next))
        }
        _ => Ok((
            MachineControl::Expr(test.clone(), env.clone()),
            Rc::new(MachineContinuation::Cond {
                clauses: Rc::from(clauses.to_vec()),
                index,
                env,
                next,
            }),
        )),
    }
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

    if let Some(expanded) = try_expand_macro_invocation(items, pos, env.clone(), ctx)? {
        return eval_expr(&expanded, env, ctx);
    }

    match &head.kind {
        ExprKind::Symbol(name) if name == "and" => eval_and(args, env, ctx),
        ExprKind::Symbol(name) if name == "or" => eval_or(args, env, ctx),
        ExprKind::Symbol(name) if name == "if" => eval_if(args, env, ctx),
        ExprKind::Symbol(name) if name == "let" => eval_let(args, pos, env, ctx),
        ExprKind::Symbol(name) if name == "begin" => eval_begin(args, env, ctx),
        ExprKind::Symbol(name) if name == "cond" => eval_cond(args, env, ctx),
        ExprKind::Symbol(name) if name == "quote" => eval_quote(args),
        ExprKind::Symbol(name) if name == "define" => eval_define(args, env, ctx),
        ExprKind::Symbol(name) if name == "define-syntax" => eval_define_syntax(args, env, ctx),
        ExprKind::Symbol(name) if name == "set!" => eval_set(args, env, ctx),
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

    if let Some(expanded) = try_expand_macro_invocation(items, pos, env.clone(), ctx)? {
        return eval_tail_expr(&expanded, env, ctx);
    }

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
        ExprKind::Symbol(name) if name == "define-syntax" => {
            eval_define_syntax(args, env, ctx).map(TailOutcome::Value)
        }
        ExprKind::Symbol(name) if name == "set!" => {
            eval_set(args, env, ctx).map(TailOutcome::Value)
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
                params: Parameters {
                    required: names,
                    rest: None,
                },
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
                params: Parameters {
                    required: names,
                    rest: None,
                },
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

fn eval_set(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(wrong_arg_count("set!", "exactly 2", args.len()));
    }

    let name = expect_symbol(&args[0], "set! target")?;
    let value = eval_expr(&args[1], env.clone(), ctx)?;
    Env::set(&env, &name, value)?;
    Ok(Value::Void)
}

fn eval_define_syntax(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (name, definition) = parse_define_syntax(args, env)?;
    ctx.macros.insert(name, definition);
    Ok(Value::Void)
}

fn parse_define_syntax(args: &[Expr], env: EnvRef) -> Result<(String, MacroDefinition), EvalError> {
    if args.len() != 2 {
        return Err(wrong_arg_count("define-syntax", "exactly 2", args.len()));
    }

    let name = expect_symbol(&args[0], "macro name")?;
    let ExprKind::List(items) = &args[1].kind else {
        return Err(EvalError::Syntax("define-syntax requires a syntax-rules form".into()));
    };

    let Some((keyword, rest)) = items.split_first() else {
        return Err(EvalError::Syntax("define-syntax requires a syntax-rules form".into()));
    };

    let keyword = expect_symbol(keyword, "syntax-rules keyword")?;
    if keyword != "syntax-rules" {
        return Err(EvalError::Syntax("define-syntax requires syntax-rules".into()));
    }

    let Some((literals_expr, rule_exprs)) = rest.split_first() else {
        return Err(EvalError::Syntax("syntax-rules requires literals and at least one rule".into()));
    };
    if rule_exprs.is_empty() {
        return Err(EvalError::Syntax("syntax-rules requires at least one rule".into()));
    }

    let literals = parse_syntax_rule_literals(literals_expr)?;
    let mut rules = Vec::with_capacity(rule_exprs.len());
    for rule_expr in rule_exprs {
        rules.push(parse_macro_rule(rule_expr, &name)?);
    }

    Ok((
        name,
        MacroDefinition {
            literals,
            rules,
            definition_env: env,
        },
    ))
}

fn parse_syntax_rule_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::Syntax(
            "syntax-rules literals must be a list of identifiers".into(),
        ));
    };

    items
        .iter()
        .map(|item| expect_symbol(item, "syntax-rules literal identifier"))
        .collect()
}

fn parse_macro_rule(expr: &Expr, macro_name: &str) -> Result<MacroRule, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::Syntax("syntax-rules clause must be a (pattern template) pair".into()));
    };

    if items.len() != 2 {
        return Err(EvalError::Syntax("syntax-rules clause must be a (pattern template) pair".into()));
    }

    let ExprKind::List(pattern_items) = &items[0].kind else {
        return Err(EvalError::Syntax("syntax-rules pattern must be a list".into()));
    };
    let Some((head, pattern_args)) = pattern_items.split_first() else {
        return Err(EvalError::Syntax("syntax-rules pattern cannot be empty".into()));
    };

    let head = expect_symbol(head, "syntax-rules pattern head")?;
    if head != macro_name {
        return Err(EvalError::Syntax(
            "syntax-rules pattern must start with the macro name".into(),
        ));
    }

    Ok(MacroRule {
        pattern_args: pattern_args.to_vec(),
        template: items[1].clone(),
    })
}

fn try_expand_macro_invocation(
    items: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Option<Expr>, EvalError> {
    let Some(head) = items.first() else {
        return Ok(None);
    };

    let ExprKind::Symbol(name) = &head.kind else {
        return Ok(None);
    };
    let Some(definition) = ctx.macros.get(name).cloned() else {
        return Ok(None);
    };

    expand_macro_invocation(&definition, &items[1..], pos, env, ctx).map(Some)
}

fn expand_macro_invocation(
    definition: &MacroDefinition,
    args: &[Expr],
    pos: SourcePos,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Expr, EvalError> {
    for rule in &definition.rules {
        let Some(bindings) = match_macro_rule(rule, args, &definition.literals) else {
            continue;
        };

        let syntax = expand_template(&rule.template, &bindings)?;
        return lower_macro_syntax(&syntax, definition.definition_env.clone(), env, ctx)
            .map_err(|err| err.with_position(pos));
    }

    Err(EvalError::Syntax("macro invocation did not match any syntax-rules pattern".into()))
}

fn match_macro_rule(
    rule: &MacroRule,
    args: &[Expr],
    literals: &HashSet<String>,
) -> Option<HashMap<String, MatchBinding>> {
    let mut bindings = HashMap::new();
    if match_pattern_list(&rule.pattern_args, args, literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_pattern_list(
    patterns: &[Expr],
    values: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> bool {
    let Some((first_pattern, rest_patterns)) = patterns.split_first() else {
        return values.is_empty();
    };

    if rest_patterns
        .first()
        .is_some_and(|expr| is_ellipsis_expr(expr))
    {
        let repeated_vars = collect_pattern_variables(first_pattern, literals);

        for repeat_count in 0..=values.len() {
            let mut candidate = bindings.clone();
            if !ensure_repeated_bindings(&mut candidate, &repeated_vars) {
                continue;
            }

            let mut ok = true;
            for value in &values[..repeat_count] {
                let mut occurrence = HashMap::new();
                if !match_pattern_expr(first_pattern, value, literals, &mut occurrence) {
                    ok = false;
                    break;
                }
                if !merge_repeated_bindings(&mut candidate, occurrence) {
                    ok = false;
                    break;
                }
            }

            if ok
                && match_pattern_list(
                    &rest_patterns[1..],
                    &values[repeat_count..],
                    literals,
                    &mut candidate,
                )
            {
                *bindings = candidate;
                return true;
            }
        }

        false
    } else {
        let Some((first_value, rest_values)) = values.split_first() else {
            return false;
        };

        if !match_pattern_expr(first_pattern, first_value, literals, bindings) {
            return false;
        }

        match_pattern_list(rest_patterns, rest_values, literals, bindings)
    }
}

fn match_pattern_expr(
    pattern: &Expr,
    value: &Expr,
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Bool(left) => matches!(&value.kind, ExprKind::Bool(right) if left == right),
        ExprKind::Number(left) => {
            matches!(&value.kind, ExprKind::Number(right) if left == right)
        }
        ExprKind::Char(left) => matches!(&value.kind, ExprKind::Char(right) if left == right),
        ExprKind::String(left) => {
            matches!(&value.kind, ExprKind::String(right) if left == right)
        }
        ExprKind::Symbol(name) => {
            if is_pattern_variable(name, literals) {
                merge_single_binding(bindings, name, value.clone())
            } else {
                matches!(&value.kind, ExprKind::Symbol(other) if name == other)
            }
        }
        ExprKind::List(pattern_items) => {
            let ExprKind::List(value_items) = &value.kind else {
                return false;
            };
            match_pattern_list(pattern_items, value_items, literals, bindings)
        }
    }
}

fn merge_single_binding(
    bindings: &mut HashMap<String, MatchBinding>,
    name: &str,
    expr: Expr,
) -> bool {
    match bindings.get_mut(name) {
        Some(MatchBinding::Single(existing)) => *existing == expr,
        Some(MatchBinding::Repeated(_)) => false,
        None => {
            bindings.insert(name.to_string(), MatchBinding::Single(expr));
            true
        }
    }
}

fn merge_repeated_bindings(
    bindings: &mut HashMap<String, MatchBinding>,
    occurrence: HashMap<String, MatchBinding>,
) -> bool {
    for (name, binding) in occurrence {
        let MatchBinding::Single(expr) = binding else {
            return false;
        };

        match bindings.get_mut(&name) {
            Some(MatchBinding::Repeated(values)) => values.push(expr),
            Some(MatchBinding::Single(_)) => return false,
            None => {
                bindings.insert(name, MatchBinding::Repeated(vec![expr]));
            }
        }
    }

    true
}

fn ensure_repeated_bindings(
    bindings: &mut HashMap<String, MatchBinding>,
    repeated_vars: &[String],
) -> bool {
    for name in repeated_vars {
        match bindings.get(name) {
            Some(MatchBinding::Single(_)) => return false,
            Some(MatchBinding::Repeated(_)) => {}
            None => {
                bindings.insert(name.clone(), MatchBinding::Repeated(Vec::new()));
            }
        }
    }

    true
}

fn collect_pattern_variables(expr: &Expr, literals: &HashSet<String>) -> Vec<String> {
    fn visit(
        expr: &Expr,
        literals: &HashSet<String>,
        names: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match &expr.kind {
            ExprKind::Symbol(name) if is_pattern_variable(name, literals) => {
                if seen.insert(name.clone()) {
                    names.push(name.clone());
                }
            }
            ExprKind::List(items) => {
                for item in items {
                    visit(item, literals, names, seen);
                }
            }
            _ => {}
        }
    }

    let mut names = Vec::new();
    let mut seen = HashSet::new();
    visit(expr, literals, &mut names, &mut seen);
    names
}

fn is_pattern_variable(name: &str, literals: &HashSet<String>) -> bool {
    name != "..." && !literals.contains(name)
}

fn is_ellipsis_expr(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(name) if name == "...")
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
) -> Result<SyntaxExpr, EvalError> {
    expand_template_node(template, bindings, None)
}

fn expand_template_node(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    repetition_index: Option<usize>,
) -> Result<SyntaxExpr, EvalError> {
    match &template.kind {
        ExprKind::Bool(value) => Ok(SyntaxExpr::generated(SyntaxExprKind::Bool(*value), template.pos)),
        ExprKind::Number(value) => {
            Ok(SyntaxExpr::generated(SyntaxExprKind::Number(*value), template.pos))
        }
        ExprKind::Char(value) => Ok(SyntaxExpr::generated(SyntaxExprKind::Char(*value), template.pos)),
        ExprKind::String(value) => Ok(SyntaxExpr::generated(
            SyntaxExprKind::String(value.clone()),
            template.pos,
        )),
        ExprKind::Symbol(name) => match bindings.get(name) {
            Some(MatchBinding::Single(expr)) => Ok(SyntaxExpr::raw(expr.clone())),
            Some(MatchBinding::Repeated(values)) => {
                let Some(index) = repetition_index else {
                    return Err(EvalError::Syntax(
                        "repeated pattern variable must appear under ellipsis in template".into(),
                    ));
                };
                let expr = values.get(index).ok_or_else(|| {
                    EvalError::Syntax("ellipsis repetition index out of bounds during macro expansion".into())
                })?;
                Ok(SyntaxExpr::raw(expr.clone()))
            }
            None => Ok(SyntaxExpr::generated(
                SyntaxExprKind::Symbol(name.clone()),
                template.pos,
            )),
        },
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut index = 0;
            while index < items.len() {
                let item = &items[index];
                if items
                    .get(index + 1)
                    .is_some_and(|expr| is_ellipsis_expr(expr))
                {
                    if repetition_index.is_some() {
                        return Err(EvalError::Syntax(
                            "nested ellipsis in syntax-rules templates is not supported".into(),
                        ));
                    }

                    let count = template_repetition_count(item, bindings)?;
                    for repeat_index in 0..count {
                        expanded.push(expand_template_node(item, bindings, Some(repeat_index))?);
                    }
                    index += 2;
                } else {
                    expanded.push(expand_template_node(item, bindings, repetition_index)?);
                    index += 1;
                }
            }

            Ok(SyntaxExpr::generated(SyntaxExprKind::List(expanded), template.pos))
        }
    }
}

fn template_repetition_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
) -> Result<usize, EvalError> {
    let repeated_vars = collect_template_repeated_variables(template, bindings);
    let Some(first_name) = repeated_vars.first() else {
        return Err(EvalError::Syntax(
            "template ellipsis requires at least one repeated pattern variable".into(),
        ));
    };

    let Some(MatchBinding::Repeated(values)) = bindings.get(first_name) else {
        return Err(EvalError::Syntax(
            "template ellipsis expected a repeated pattern variable".into(),
        ));
    };
    let count = values.len();

    for name in repeated_vars.iter().skip(1) {
        let Some(MatchBinding::Repeated(other_values)) = bindings.get(name) else {
            return Err(EvalError::Syntax(
                "template ellipsis expected a repeated pattern variable".into(),
            ));
        };
        if other_values.len() != count {
            return Err(EvalError::Syntax(
                "template ellipsis variables must repeat the same number of times".into(),
            ));
        }
    }

    Ok(count)
}

fn collect_template_repeated_variables(
    expr: &Expr,
    bindings: &HashMap<String, MatchBinding>,
) -> Vec<String> {
    fn visit(expr: &Expr, bindings: &HashMap<String, MatchBinding>, out: &mut Vec<String>) {
        match &expr.kind {
            ExprKind::Symbol(name) => {
                if matches!(bindings.get(name), Some(MatchBinding::Repeated(_)))
                    && !out.iter().any(|existing| existing == name)
                {
                    out.push(name.clone());
                }
            }
            ExprKind::List(items) => {
                for item in items {
                    visit(item, bindings, out);
                }
            }
            _ => {}
        }
    }

    let mut out = Vec::new();
    visit(expr, bindings, &mut out);
    out
}

struct HygieneState<'a> {
    definition_env: EnvRef,
    use_env: EnvRef,
    ctx: &'a mut EvalContext,
    renamed_bindings: Vec<HashMap<String, String>>,
    captured_identifiers: HashMap<String, String>,
}

fn lower_macro_syntax(
    syntax: &SyntaxExpr,
    definition_env: EnvRef,
    use_env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Expr, EvalError> {
    let mut state = HygieneState {
        definition_env,
        use_env,
        ctx,
        renamed_bindings: Vec::new(),
        captured_identifiers: HashMap::new(),
    };
    lower_macro_syntax_with_state(syntax, false, &mut state)
}

fn lower_macro_syntax_with_state(
    syntax: &SyntaxExpr,
    quote_mode: bool,
    state: &mut HygieneState<'_>,
) -> Result<Expr, EvalError> {
    match &syntax.kind {
        SyntaxExprKind::Raw(expr) => Ok(expr.clone()),
        SyntaxExprKind::Bool(value) => Ok(Expr::new(ExprKind::Bool(*value), syntax.pos)),
        SyntaxExprKind::Number(value) => Ok(Expr::new(ExprKind::Number(*value), syntax.pos)),
        SyntaxExprKind::Char(value) => Ok(Expr::new(ExprKind::Char(*value), syntax.pos)),
        SyntaxExprKind::String(value) => {
            Ok(Expr::new(ExprKind::String(value.clone()), syntax.pos))
        }
        SyntaxExprKind::Symbol(name) => lower_generated_symbol(name, syntax.pos, quote_mode, state),
        SyntaxExprKind::List(items) => lower_generated_list(items, syntax.pos, quote_mode, state),
    }
}

fn lower_generated_symbol(
    name: &str,
    pos: SourcePos,
    quote_mode: bool,
    state: &mut HygieneState<'_>,
) -> Result<Expr, EvalError> {
    if quote_mode || name == "." || is_syntax_keyword(name) {
        return Ok(Expr::new(ExprKind::Symbol(name.to_string()), pos));
    }

    if let Some(renamed) = lookup_renamed_binding(&state.renamed_bindings, name) {
        return Ok(Expr::new(ExprKind::Symbol(renamed), pos));
    }

    if let Some(alias) = state.captured_identifiers.get(name) {
        return Ok(Expr::new(ExprKind::Symbol(alias.clone()), pos));
    }

    if let Some(binding) = Env::lookup_cell(&state.definition_env, name) {
        let alias = state.ctx.fresh_generated_name(name);
        Env::define_alias(&state.use_env, alias.clone(), binding);
        state
            .captured_identifiers
            .insert(name.to_string(), alias.clone());
        return Ok(Expr::new(ExprKind::Symbol(alias), pos));
    }

    Ok(Expr::new(ExprKind::Symbol(name.to_string()), pos))
}

fn lower_generated_list(
    items: &[SyntaxExpr],
    pos: SourcePos,
    quote_mode: bool,
    state: &mut HygieneState<'_>,
) -> Result<Expr, EvalError> {
    if quote_mode {
        let lowered = items
            .iter()
            .map(|item| lower_macro_syntax_with_state(item, true, state))
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(Expr::new(ExprKind::List(lowered), pos));
    }

    match syntax_head_name(items) {
        Some("quote") if items.len() == 2 => {
            let head = lower_macro_syntax_with_state(&items[0], false, state)?;
            let quoted = lower_macro_syntax_with_state(&items[1], true, state)?;
            Ok(Expr::new(ExprKind::List(vec![head, quoted]), pos))
        }
        Some("lambda") if items.len() >= 3 => lower_lambda_form(items, pos, state),
        Some("let") if items.len() >= 3 => lower_let_form(items, pos, state),
        _ => {
            let lowered = items
                .iter()
                .map(|item| lower_macro_syntax_with_state(item, false, state))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::new(ExprKind::List(lowered), pos))
        }
    }
}

fn lower_lambda_form(
    items: &[SyntaxExpr],
    pos: SourcePos,
    state: &mut HygieneState<'_>,
) -> Result<Expr, EvalError> {
    let head = lower_macro_syntax_with_state(&items[0], false, state)?;
    let mut scope = HashMap::new();
    let params = lower_binding_expr(&items[1], state, &mut scope)?;

    state.renamed_bindings.push(scope);
    let mut lowered = vec![head, params];
    for body_expr in &items[2..] {
        lowered.push(lower_macro_syntax_with_state(body_expr, false, state)?);
    }
    state.renamed_bindings.pop();

    Ok(Expr::new(ExprKind::List(lowered), pos))
}

fn lower_let_form(
    items: &[SyntaxExpr],
    pos: SourcePos,
    state: &mut HygieneState<'_>,
) -> Result<Expr, EvalError> {
    let head = lower_macro_syntax_with_state(&items[0], false, state)?;
    let mut lowered = vec![head];
    let mut scope = HashMap::new();
    let mut binding_index = 1;

    if !syntax_is_list(&items[1]) {
        lowered.push(lower_binding_expr(&items[1], state, &mut scope)?);
        binding_index = 2;
    }

    lowered.push(lower_let_bindings(&items[binding_index], state, &mut scope)?);

    state.renamed_bindings.push(scope);
    for body_expr in &items[binding_index + 1..] {
        lowered.push(lower_macro_syntax_with_state(body_expr, false, state)?);
    }
    state.renamed_bindings.pop();

    Ok(Expr::new(ExprKind::List(lowered), pos))
}

fn lower_let_bindings(
    bindings: &SyntaxExpr,
    state: &mut HygieneState<'_>,
    scope: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    let SyntaxExprKind::List(entries) = &bindings.kind else {
        return lower_macro_syntax_with_state(bindings, false, state);
    };

    let mut lowered_entries = Vec::with_capacity(entries.len());
    for entry in entries {
        match &entry.kind {
            SyntaxExprKind::Raw(expr) => lowered_entries.push(expr.clone()),
            SyntaxExprKind::List(parts) if !parts.is_empty() => {
                let mut lowered_parts = Vec::with_capacity(parts.len());
                lowered_parts.push(lower_binding_expr(&parts[0], state, scope)?);
                for part in &parts[1..] {
                    lowered_parts.push(lower_macro_syntax_with_state(part, false, state)?);
                }
                lowered_entries.push(Expr::new(ExprKind::List(lowered_parts), entry.pos));
            }
            _ => lowered_entries.push(lower_macro_syntax_with_state(entry, false, state)?),
        }
    }

    Ok(Expr::new(ExprKind::List(lowered_entries), bindings.pos))
}

fn lower_binding_expr(
    syntax: &SyntaxExpr,
    state: &mut HygieneState<'_>,
    scope: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match &syntax.kind {
        SyntaxExprKind::Raw(expr) => Ok(expr.clone()),
        SyntaxExprKind::Symbol(name) if name == "." => {
            Ok(Expr::new(ExprKind::Symbol(name.clone()), syntax.pos))
        }
        SyntaxExprKind::Symbol(name) => {
            let renamed = bind_generated_name(scope, name, state.ctx);
            Ok(Expr::new(ExprKind::Symbol(renamed), syntax.pos))
        }
        SyntaxExprKind::List(items) => {
            let lowered = items
                .iter()
                .map(|item| lower_binding_expr(item, state, scope))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::new(ExprKind::List(lowered), syntax.pos))
        }
        _ => lower_macro_syntax_with_state(syntax, false, state),
    }
}

fn bind_generated_name(
    scope: &mut HashMap<String, String>,
    name: &str,
    ctx: &mut EvalContext,
) -> String {
    if let Some(existing) = scope.get(name) {
        return existing.clone();
    }

    let renamed = ctx.fresh_generated_name(name);
    scope.insert(name.to_string(), renamed.clone());
    renamed
}

fn lookup_renamed_binding(scopes: &[HashMap<String, String>], name: &str) -> Option<String> {
    scopes
        .iter()
        .rev()
        .find_map(|scope| scope.get(name).cloned())
}

fn syntax_head_name(items: &[SyntaxExpr]) -> Option<&str> {
    let head = items.first()?;
    match &head.kind {
        SyntaxExprKind::Raw(expr) => match &expr.kind {
            ExprKind::Symbol(name) => Some(name.as_str()),
            _ => None,
        },
        SyntaxExprKind::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn syntax_is_list(expr: &SyntaxExpr) -> bool {
    matches!(&expr.kind, SyntaxExprKind::List(_))
        || matches!(&expr.kind, SyntaxExprKind::Raw(raw) if matches!(&raw.kind, ExprKind::List(_)))
}

fn is_syntax_keyword(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "if"
            | "let"
            | "begin"
            | "cond"
            | "quote"
            | "define"
            | "set!"
            | "lambda"
            | "define-syntax"
            | "syntax-rules"
    )
}

fn parse_parameter_list(expr: &Expr) -> Result<Parameters, EvalError> {
    match &expr.kind {
        ExprKind::List(items) => parse_parameters(items),
        ExprKind::Symbol(name) => Ok(Parameters {
            required: Vec::new(),
            rest: Some(parse_parameter_name(name)?),
        }),
        _ => Err(EvalError::Syntax(
            "lambda parameter list must be a list or symbol".into(),
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

fn parse_parameters(items: &[Expr]) -> Result<Parameters, EvalError> {
    let dot_index = items
        .iter()
        .position(|expr| matches!(&expr.kind, ExprKind::Symbol(name) if name == "."));

    match dot_index {
        None => Ok(Parameters {
            required: items
                .iter()
                .map(expect_parameter_symbol)
                .collect::<Result<Vec<_>, _>>()?,
            rest: None,
        }),
        Some(index) => {
            if index + 2 != items.len() {
                return Err(EvalError::Syntax(
                    "parameter list may contain at most one '.' before a final rest parameter"
                        .into(),
                ));
            }

            Ok(Parameters {
                required: items[..index]
                    .iter()
                    .map(expect_parameter_symbol)
                    .collect::<Result<Vec<_>, _>>()?,
                rest: Some(expect_parameter_symbol(&items[index + 1])?),
            })
        }
    }
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match &expr.kind {
        ExprKind::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::Syntax(format!("{context} must be a symbol"))),
    }
}

fn expect_parameter_symbol(expr: &Expr) -> Result<String, EvalError> {
    let name = expect_symbol(expr, "parameter")?;
    parse_parameter_name(&name)
}

fn parse_parameter_name(name: &str) -> Result<String, EvalError> {
    if name == "." {
        return Err(EvalError::Syntax("parameter name must not be '.'".into()));
    }

    Ok(name.to_string())
}

fn apply(
    function: Value,
    args: &[Value],
    pos: SourcePos,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    resolve_tail_outcome(dispatch_call(function, args.to_vec(), pos, ctx)?, ctx)
}

fn apply_in_tail_position(
    function: Value,
    args: Vec<Value>,
    pos: SourcePos,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    dispatch_call(function, args, pos, ctx)
}

fn dispatch_call(
    function: Value,
    args: Vec<Value>,
    pos: SourcePos,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    match function {
        Value::Builtin(builtin) => dispatch_builtin_call(builtin, args, pos, ctx),
        Value::Procedure(procedure) => Ok(TailOutcome::TailCall {
            procedure,
            args,
            pos,
        }),
        other => Err(EvalError::NotAProcedure(other.to_string()).with_position(pos)),
    }
}

fn dispatch_builtin_call(
    builtin: Builtin,
    args: Vec<Value>,
    pos: SourcePos,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    match builtin {
        Builtin::Apply => {
            let (function, applied_args) =
                expand_apply_args(&args).map_err(|err| err.with_position(pos))?;
            dispatch_call(function, applied_args, pos, ctx)
        }
        _ => apply_builtin(builtin, &args, ctx)
            .map(TailOutcome::Value)
            .map_err(|err| err.with_position(pos)),
    }
}

fn bind_call_env(procedure: &Rc<LambdaProcedure>, args: &[Value]) -> Result<EnvRef, EvalError> {
    let required = procedure.params.required.len();
    let name = procedure.name.as_deref().unwrap_or("lambda");

    match procedure.params.rest.as_ref() {
        Some(rest) => {
            if args.len() < required {
                let expected = format!("at least {required}");
                return Err(wrong_arg_count(name, &expected, args.len()));
            }

            let local_env = bind_names(
                procedure.env.clone(),
                &procedure.params.required,
                &args[..required],
            );
            Env::define(
                &local_env,
                rest.clone(),
                Value::List(args[required..].to_vec()),
            );
            Ok(local_env)
        }
        None => {
            if args.len() != required {
                let expected = format!("exactly {required}");
                return Err(wrong_arg_count(name, &expected, args.len()));
            }

            Ok(bind_names(
                procedure.env.clone(),
                &procedure.params.required,
                args,
            ))
        }
    }
}

fn bind_names(parent: EnvRef, names: &[String], values: &[Value]) -> EnvRef {
    let local_env = Env::child(parent);
    for (name, value) in names.iter().zip(values.iter()) {
        Env::define(&local_env, name.clone(), value.clone());
    }
    local_env
}

fn expand_apply_args(args: &[Value]) -> Result<(Value, Vec<Value>), EvalError> {
    if args.len() < 2 {
        return Err(wrong_arg_count("apply", "at least 2", args.len()));
    }

    let function = args[0].clone();
    let tail_args = expect_list_value("apply", &args[args.len() - 1])?;
    let mut applied_args = Vec::with_capacity(args.len() + tail_args.len() - 2);
    applied_args.extend_from_slice(&args[1..args.len() - 1]);
    applied_args.extend_from_slice(tail_args);
    Ok((function, applied_args))
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
        Builtin::Apply => unreachable!("apply is handled by dispatch_builtin_call"),
        Builtin::CallCc => unreachable!("call/cc is handled by the machine runtime"),
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
