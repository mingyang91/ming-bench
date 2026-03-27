pub mod error;

pub use error::EvalError;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
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
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::message("empty input"));
    }

    let env = Env::new(None);
    let result = eval_sequence(&expressions, env)?;
    Ok(render(&result))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;

#[derive(Clone, PartialEq, Eq)]
enum Expr {
    Int(i64),
    Rational(i64, i64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    NumericEq,
    LessEqual,
    Eq,
    Equal,
    Not,
    Map,
    Apply,
    Reverse,
    Cons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
    NullPred,
    List,
    Length,
    Append,
    ZeroPred,
    Remainder,
    StringPred,
    StringAppend,
    NumberToString,
    StringToSymbol,
    SymbolToString,
    StringRef,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    Vector,
    VectorRef,
    Values,
    CallWithValues,
    DynamicWind,
    Raise,
    WithExceptionHandler,
    GuardProtect,
    CallCc,
}

impl Builtin {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "<" => Some(Self::LessThan),
            ">" => Some(Self::GreaterThan),
            "=" => Some(Self::NumericEq),
            "<=" => Some(Self::LessEqual),
            "eq?" => Some(Self::Eq),
            "equal?" => Some(Self::Equal),
            "not" => Some(Self::Not),
            "map" => Some(Self::Map),
            "apply" => Some(Self::Apply),
            "reverse" => Some(Self::Reverse),
            "cons" => Some(Self::Cons),
            "car" => Some(Self::Car),
            "cdr" => Some(Self::Cdr),
            "set-car!" => Some(Self::SetCar),
            "set-cdr!" => Some(Self::SetCdr),
            "null?" => Some(Self::NullPred),
            "list" => Some(Self::List),
            "length" => Some(Self::Length),
            "append" => Some(Self::Append),
            "zero?" => Some(Self::ZeroPred),
            "remainder" => Some(Self::Remainder),
            "string?" => Some(Self::StringPred),
            "string-append" => Some(Self::StringAppend),
            "number->string" => Some(Self::NumberToString),
            "string->symbol" => Some(Self::StringToSymbol),
            "symbol->string" => Some(Self::SymbolToString),
            "string-ref" => Some(Self::StringRef),
            "number?" => Some(Self::NumberPred),
            "boolean?" => Some(Self::BooleanPred),
            "pair?" => Some(Self::PairPred),
            "symbol?" => Some(Self::SymbolPred),
            "vector" => Some(Self::Vector),
            "vector-ref" => Some(Self::VectorRef),
            "values" => Some(Self::Values),
            "call-with-values" => Some(Self::CallWithValues),
            "dynamic-wind" => Some(Self::DynamicWind),
            "raise" => Some(Self::Raise),
            "with-exception-handler" => Some(Self::WithExceptionHandler),
            "__guard-protect" => Some(Self::GuardProtect),
            "call/cc" | "call-with-current-continuation" => Some(Self::CallCc),
            _ => None,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::NumericEq => "=",
            Self::LessEqual => "<=",
            Self::Eq => "eq?",
            Self::Equal => "equal?",
            Self::Not => "not",
            Self::Map => "map",
            Self::Apply => "apply",
            Self::Reverse => "reverse",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::SetCar => "set-car!",
            Self::SetCdr => "set-cdr!",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::ZeroPred => "zero?",
            Self::Remainder => "remainder",
            Self::StringPred => "string?",
            Self::StringAppend => "string-append",
            Self::NumberToString => "number->string",
            Self::StringToSymbol => "string->symbol",
            Self::SymbolToString => "symbol->string",
            Self::StringRef => "string-ref",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
            Self::Vector => "vector",
            Self::VectorRef => "vector-ref",
            Self::Values => "values",
            Self::CallWithValues => "call-with-values",
            Self::DynamicWind => "dynamic-wind",
            Self::Raise => "raise",
            Self::WithExceptionHandler => "with-exception-handler",
            Self::GuardProtect => "__guard-protect",
            Self::CallCc => "call/cc",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SpecialForm {
    Define,
    If,
    Quote,
    Lambda,
    And,
    Or,
    Let,
    Begin,
    Cond,
    Set,
    Letrec,
    Guard,
    Do,
    DefineSyntax,
    DefineRecordType,
}

impl SpecialForm {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "define" => Some(Self::Define),
            "if" => Some(Self::If),
            "quote" => Some(Self::Quote),
            "lambda" => Some(Self::Lambda),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "let" => Some(Self::Let),
            "begin" => Some(Self::Begin),
            "cond" => Some(Self::Cond),
            "set!" => Some(Self::Set),
            "letrec" => Some(Self::Letrec),
            "guard" => Some(Self::Guard),
            "do" => Some(Self::Do),
            "define-syntax" => Some(Self::DefineSyntax),
            "define-record-type" => Some(Self::DefineRecordType),
            _ => None,
        }
    }
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Rational(i64, i64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    EmptyList,
    Pair(Rc<Pair>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Builtin(Builtin),
    Closure(Rc<Closure>),
    Record(Rc<RecordInstance>),
    RecordConstructor(Rc<RecordConstructor>),
    RecordPredicate(Rc<RecordPredicate>),
    RecordAccessor(Rc<RecordAccessor>),
    Continuation(Rc<Continuation>),
    Multi(Vec<Value>),
    Void,
}

struct Pair {
    car: RefCell<Value>,
    cdr: RefCell<Value>,
}

#[derive(Clone)]
struct Closure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct Continuation {
    frames: Vec<MachineFrame>,
}

#[derive(Clone)]
struct DynamicWindContext {
    in_thunk: Value,
    out_thunk: Value,
}

#[derive(Clone)]
struct ExceptionHandlerContext {
    handler: Value,
    outer_frame_len: usize,
    outer_wind_len: usize,
    outer_handler_len: usize,
    allow_return: bool,
}

#[derive(Clone)]
struct RecordTypeDef {
    name: String,
    field_names: Vec<String>,
}

#[derive(Clone)]
struct RecordInstance {
    record_type: Rc<RecordTypeDef>,
    fields: Vec<Value>,
}

#[derive(Clone)]
struct RecordConstructor {
    name: String,
    record_type: Rc<RecordTypeDef>,
    field_order: Vec<usize>,
}

#[derive(Clone)]
struct RecordPredicate {
    name: String,
    record_type: Rc<RecordTypeDef>,
}

#[derive(Clone)]
struct RecordAccessor {
    name: String,
    record_type: Rc<RecordTypeDef>,
    field_index: usize,
}

#[derive(Clone)]
enum MachineFrame {
    ProcedureBoundary,
    CallCcResult,
    CallWithValuesConsumer {
        consumer: Value,
    },
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    IfBranch {
        then_branch: Expr,
        else_branch: Expr,
        env: EnvRef,
    },
    And {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Or {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        name: String,
        env: EnvRef,
    },
    CallHead {
        args: Vec<Expr>,
        env: EnvRef,
    },
    CallArgs {
        procedure: Value,
        evaluated: Vec<Value>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    LetBindings {
        kind: MachineLetKind,
        names: Vec<String>,
        evaluated: Vec<Value>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    CondClause {
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DynamicWindEntered {
        body_thunk: Value,
        wind: Rc<DynamicWindContext>,
    },
    DynamicWindBodyResult {
        wind: Rc<DynamicWindContext>,
    },
    DynamicWindOutResult {
        body_result: Value,
    },
    WithExceptionHandlerResult {
        handler: Rc<ExceptionHandlerContext>,
    },
    ExceptionHandlerReturned,
    ExceptionWindExit {
        remaining: Vec<Rc<DynamicWindContext>>,
        entering: Vec<Rc<DynamicWindContext>>,
        handler: Rc<ExceptionHandlerContext>,
        exception: Value,
    },
    ExceptionWindEnter {
        current: Rc<DynamicWindContext>,
        remaining: Vec<Rc<DynamicWindContext>>,
        handler: Rc<ExceptionHandlerContext>,
        exception: Value,
    },
}

#[derive(Clone)]
enum MachineLetKind {
    Unnamed { body: Vec<Expr> },
    Named { name: String, body: Vec<Expr> },
}

enum MachineState {
    Eval {
        expr: Expr,
        env: EnvRef,
        frames: Vec<MachineFrame>,
    },
    Apply {
        value: Value,
        frames: Vec<MachineFrame>,
    },
}

type EnvRef = Rc<Env>;
type CellRef = Rc<RefCell<Value>>;

struct Env {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, CellRef>>,
    syntax_bindings: RefCell<HashMap<String, SyntaxBinding>>,
}

#[derive(Clone)]
enum SyntaxBinding {
    Macro(Rc<MacroDef>),
    SpecialForm(SpecialForm),
}

#[derive(Clone)]
struct MacroDef {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvRef,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Default)]
struct PatternBindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

impl PatternBindings {
    fn bind_single(&mut self, name: &str, value: Expr) -> bool {
        match self.single.get(name) {
            Some(existing) => existing == &value,
            None => {
                self.single.insert(name.to_string(), value);
                true
            }
        }
    }

    fn bind_repeated(&mut self, name: &str, values: Vec<Expr>) -> bool {
        match self.repeated.get(name) {
            Some(existing) => existing == &values,
            None => {
                self.repeated.insert(name.to_string(), values);
                true
            }
        }
    }
}

struct ExpansionContext {
    def_env: EnvRef,
    introduced: HashMap<String, String>,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            syntax_bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.define_cell(name, Rc::new(RefCell::new(value)));
    }

    fn define_cell(&self, name: impl Into<String>, cell: CellRef) {
        self.bindings.borrow_mut().insert(name.into(), cell);
    }

    fn define_syntax(&self, name: impl Into<String>, binding: SyntaxBinding) {
        self.syntax_bindings
            .borrow_mut()
            .insert(name.into(), binding);
    }

    fn lookup_cell(&self, name: &str) -> Option<CellRef> {
        if let Some(cell) = self.bindings.borrow().get(name) {
            return Some(cell.clone());
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_cell(name))
    }

    fn lookup_syntax(&self, name: &str) -> Option<SyntaxBinding> {
        if let Some(binding) = self.syntax_bindings.borrow().get(name) {
            return Some(binding.clone());
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_syntax(name))
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(cell) = self.lookup_cell(name) {
            return Some(cell.borrow().clone());
        }

        Builtin::from_name(name).map(Value::Builtin)
    }

    fn set(&self, name: &str, value: Value) -> bool {
        if let Some(cell) = self.lookup_cell(name) {
            *cell.borrow_mut() = value;
            true
        } else {
            false
        }
    }
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    run_machine(MachineState::Eval {
        expr: expr.clone(),
        env,
        frames: Vec::new(),
    })
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::message("cannot evaluate empty list"));
    };

    if let Expr::Symbol(symbol) = head {
        if let Some(binding) = resolve_syntax(symbol, &env) {
            return match binding {
                SyntaxBinding::Macro(macro_def) => {
                    let expanded =
                        expand_macro_call(&macro_def, &Expr::List(items.to_vec()), &env)?;
                    eval(&expanded, env)
                }
                SyntaxBinding::SpecialForm(special_form) => {
                    eval_special_form(special_form, tail, env)
                }
            };
        }
    }

    let procedure = eval(head, env.clone())?;
    apply(procedure, tail, env)
}

fn resolve_syntax(name: &str, env: &EnvRef) -> Option<SyntaxBinding> {
    env.lookup_syntax(name)
        .or_else(|| SpecialForm::from_name(name).map(SyntaxBinding::SpecialForm))
}

fn eval_special_form(
    special_form: SpecialForm,
    args: &[Expr],
    env: EnvRef,
) -> Result<Value, EvalError> {
    match special_form {
        SpecialForm::Define => eval_define(args, env),
        SpecialForm::If => eval_if(args, env),
        SpecialForm::Quote => eval_quote(args),
        SpecialForm::Lambda => eval_lambda(args, env),
        SpecialForm::And => eval_and(args, env),
        SpecialForm::Or => eval_or(args, env),
        SpecialForm::Let => eval_let(args, env),
        SpecialForm::Letrec => eval_letrec(args, env),
        SpecialForm::Begin => eval_sequence(args, env),
        SpecialForm::Cond => eval_cond(args, env),
        SpecialForm::Set => eval_set(args, env),
        SpecialForm::Guard => eval(&desugar_guard(args)?, env),
        SpecialForm::Do => eval(&desugar_do(args)?, env),
        SpecialForm::DefineSyntax => eval_define_syntax(args, env),
        SpecialForm::DefineRecordType => eval_define_record_type(args, env),
    }
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), value_expr] => {
            let value = eval(value_expr, env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] if !body.is_empty() => {
            let Some((Expr::Symbol(name), params)) = signature.split_first() else {
                return Err(EvalError::message("invalid define"));
            };

            let closure = Value::Closure(Rc::new(Closure {
                name: Some(name.clone()),
                params: parse_param_names(params)?,
                body: body.to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::message("invalid define")),
    }
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch, else_branch] => {
            if is_truthy(&eval(condition, env.clone())?) {
                eval(then_branch, env)
            } else {
                eval(else_branch, env)
            }
        }
        _ => Err(EvalError::message("if expects exactly 3 arguments")),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote(expr)),
        _ => Err(EvalError::message("quote expects exactly 1 argument")),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [params_expr, body @ ..] if !body.is_empty() => Ok(Value::Closure(Rc::new(Closure {
            name: None,
            params: parse_params_expr(params_expr)?,
            body: body.to_vec(),
            env,
        }))),
        _ => Err(EvalError::message(
            "lambda expects parameters and at least one body expression",
        )),
    }
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Bool(true);
    for expr in args {
        let value = eval(expr, env.clone())?;
        if !is_truthy(&value) {
            return Ok(value);
        }
        last_value = value;
    }
    Ok(last_value)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr, env.clone())?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_let(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), bindings_expr, body @ ..] if !body.is_empty() => {
            let bindings = parse_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = eval_binding_values(&bindings, env.clone())?;
            let closure = Rc::new(Closure {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env,
            });
            call_closure(closure, values)
        }
        [bindings_expr, body @ ..] if !body.is_empty() => {
            let bindings = parse_bindings(bindings_expr)?;
            let values = eval_binding_values(&bindings, env.clone())?;
            let let_env = Env::new(Some(env));
            for ((name, _), value) in bindings.into_iter().zip(values.into_iter()) {
                let_env.define(name, value);
            }
            eval_sequence(body, let_env)
        }
        _ => Err(EvalError::message(
            "let expects bindings and at least one body expression",
        )),
    }
}

fn eval_letrec(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [bindings_expr, body @ ..] if !body.is_empty() => {
            let bindings = parse_bindings(bindings_expr)?;
            let let_env = Env::new(Some(env));
            let mut prepared = Vec::with_capacity(bindings.len());
            for (name, expr) in bindings {
                let cell = Rc::new(RefCell::new(Value::Void));
                let_env.define_cell(name, cell.clone());
                prepared.push((expr, cell));
            }

            for (expr, cell) in prepared {
                *cell.borrow_mut() = eval(&expr, let_env.clone())?;
            }

            eval_sequence(body, let_env)
        }
        _ => Err(EvalError::message(
            "letrec expects bindings and at least one body expression",
        )),
    }
}

fn eval_cond(clauses: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::message("cond clauses must be lists"));
        };

        let Some((test_expr, body)) = items.split_first() else {
            return Err(EvalError::message("cond clauses cannot be empty"));
        };

        if matches!(test_expr, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::message("cond else clause must be last"));
            }
            if body.is_empty() {
                return Err(EvalError::message("cond else clause requires a body"));
            }
            return eval_sequence(body, env.clone());
        }

        let test_value = eval(test_expr, env.clone())?;
        if is_truthy(&test_value) {
            if body.is_empty() {
                return Ok(test_value);
            }
            return eval_sequence(body, env.clone());
        }
    }

    Ok(Value::Void)
}

fn eval_set(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), value_expr] => {
            let value = eval(value_expr, env.clone())?;
            if env.set(name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::message(format!("unbound variable: {name}")))
            }
        }
        _ => Err(EvalError::message("set! expects a variable and a value")),
    }
}

fn eval_define_syntax(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), transformer] => {
            let macro_def = Rc::new(parse_syntax_rules(name.clone(), transformer, env.clone())?);
            env.define_syntax(name.clone(), SyntaxBinding::Macro(macro_def));
            Ok(Value::Void)
        }
        _ => Err(EvalError::message(
            "define-syntax expects a name and a transformer",
        )),
    }
}

fn eval_define_record_type(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [Expr::Symbol(type_name), constructor_expr, Expr::Symbol(predicate_name), field_exprs @ ..] =
        args
    else {
        return Err(EvalError::message("invalid define-record-type"));
    };

    let (constructor_name, constructor_fields) = parse_record_constructor_spec(constructor_expr)?;
    let fields = parse_record_fields(field_exprs)?;

    let field_index_by_name = fields
        .iter()
        .enumerate()
        .map(|(index, (field_name, _))| (field_name.clone(), index))
        .collect::<HashMap<_, _>>();

    let mut field_order = Vec::with_capacity(constructor_fields.len());
    for field_name in constructor_fields {
        let Some(index) = field_index_by_name.get(&field_name) else {
            return Err(EvalError::message(format!(
                "constructor field {field_name} is not declared in {type_name}",
            )));
        };
        field_order.push(*index);
    }

    let record_type = Rc::new(RecordTypeDef {
        name: type_name.clone(),
        field_names: fields
            .iter()
            .map(|(field_name, _)| field_name.clone())
            .collect(),
    });

    env.define(
        constructor_name.clone(),
        Value::RecordConstructor(Rc::new(RecordConstructor {
            name: constructor_name,
            record_type: record_type.clone(),
            field_order,
        })),
    );
    env.define(
        predicate_name.clone(),
        Value::RecordPredicate(Rc::new(RecordPredicate {
            name: predicate_name.clone(),
            record_type: record_type.clone(),
        })),
    );

    for (index, (_, accessor_name)) in fields.into_iter().enumerate() {
        env.define(
            accessor_name.clone(),
            Value::RecordAccessor(Rc::new(RecordAccessor {
                name: accessor_name,
                record_type: record_type.clone(),
                field_index: index,
            })),
        );
    }

    Ok(Value::Void)
}

fn parse_syntax_rules(name: String, expr: &Expr, env: EnvRef) -> Result<MacroDef, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::message(
            "define-syntax transformer must be a syntax-rules form",
        ));
    };

    let Some((Expr::Symbol(keyword), rest)) = items.split_first() else {
        return Err(EvalError::message(
            "define-syntax transformer must be a syntax-rules form",
        ));
    };
    if keyword != "syntax-rules" {
        return Err(EvalError::message(
            "define-syntax transformer must be a syntax-rules form",
        ));
    }

    let Some((literals_expr, rules_exprs)) = rest.split_first() else {
        return Err(EvalError::message(
            "syntax-rules expects literals and at least one rule",
        ));
    };
    if rules_exprs.is_empty() {
        return Err(EvalError::message("syntax-rules expects at least one rule"));
    }

    let literals = parse_syntax_rule_literals(literals_expr)?;
    let rules = rules_exprs
        .iter()
        .map(parse_syntax_rule)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(MacroDef {
        name,
        literals,
        rules,
        env,
    })
}

fn parse_syntax_rule_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::message("syntax-rules literals must be a list"));
    };

    items
        .iter()
        .map(|item| match item {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::message(
                "syntax-rules literals must be identifiers",
            )),
        })
        .collect()
}

fn parse_syntax_rule(expr: &Expr) -> Result<MacroRule, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::message("syntax-rules rules must be lists"));
    };

    match items.as_slice() {
        [pattern, template] => Ok(MacroRule {
            pattern: pattern.clone(),
            template: template.clone(),
        }),
        _ => Err(EvalError::message(
            "syntax-rules rules expect a pattern and a template",
        )),
    }
}

fn expand_macro_call(
    macro_def: &Rc<MacroDef>,
    call_expr: &Expr,
    env: &EnvRef,
) -> Result<Expr, EvalError> {
    for rule in &macro_def.rules {
        let mut bindings = PatternBindings::default();
        if match_pattern(&rule.pattern, call_expr, macro_def, env, &mut bindings) {
            let mut expansion_ctx = ExpansionContext::new(macro_def.env.clone());
            return expand_template(&rule.template, &bindings, &mut expansion_ctx);
        }
    }

    Err(EvalError::message(format!(
        "no matching syntax-rules pattern for {}",
        macro_def.name
    )))
}

fn match_pattern(
    pattern: &Expr,
    expr: &Expr,
    macro_def: &Rc<MacroDef>,
    env: &EnvRef,
    bindings: &mut PatternBindings,
) -> bool {
    match pattern {
        Expr::Int(value) => matches!(expr, Expr::Int(other) if value == other),
        Expr::Rational(numerator, denominator) => {
            matches!(expr, Expr::Rational(other_num, other_den) if numerator == other_num && denominator == other_den)
        }
        Expr::Bool(value) => matches!(expr, Expr::Bool(other) if value == other),
        Expr::String(value) => matches!(expr, Expr::String(other) if value == other),
        Expr::Char(value) => matches!(expr, Expr::Char(other) if value == other),
        Expr::Symbol(name) if name == "_" => true,
        Expr::Symbol(name) if is_literal_pattern_identifier(name, macro_def) => {
            literal_identifier_matches(name, expr, macro_def, env)
        }
        Expr::Symbol(name) => bindings.bind_single(name, expr.clone()),
        Expr::List(pattern_items) => {
            match_list_pattern(pattern_items, expr, macro_def, env, bindings)
        }
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    expr: &Expr,
    macro_def: &Rc<MacroDef>,
    env: &EnvRef,
    bindings: &mut PatternBindings,
) -> bool {
    let Expr::List(expr_items) = expr else {
        return false;
    };

    if pattern_items.len() >= 2 && is_ellipsis(&pattern_items[pattern_items.len() - 1]) {
        let repeat_pattern = &pattern_items[pattern_items.len() - 2];
        let prefix = &pattern_items[..pattern_items.len() - 2];
        if expr_items.len() < prefix.len() {
            return false;
        }

        for (pattern_item, expr_item) in prefix.iter().zip(expr_items.iter()) {
            if !match_pattern(pattern_item, expr_item, macro_def, env, bindings) {
                return false;
            }
        }

        return match repeat_pattern {
            Expr::Symbol(name) if name == "_" => true,
            Expr::Symbol(name) if !is_literal_pattern_identifier(name, macro_def) => {
                bindings.bind_repeated(name, expr_items[prefix.len()..].to_vec())
            }
            _ => false,
        };
    }

    if pattern_items.len() != expr_items.len() {
        return false;
    }

    for (pattern_item, expr_item) in pattern_items.iter().zip(expr_items.iter()) {
        if !match_pattern(pattern_item, expr_item, macro_def, env, bindings) {
            return false;
        }
    }

    true
}

fn is_literal_pattern_identifier(name: &str, macro_def: &MacroDef) -> bool {
    name == macro_def.name || macro_def.literals.contains(name)
}

fn literal_identifier_matches(
    pattern_name: &str,
    expr: &Expr,
    macro_def: &Rc<MacroDef>,
    env: &EnvRef,
) -> bool {
    let Expr::Symbol(subject_name) = expr else {
        return false;
    };

    if subject_name == pattern_name {
        return true;
    }

    let Some(pattern_binding) = resolve_syntax(pattern_name, &macro_def.env) else {
        return false;
    };
    let Some(subject_binding) = resolve_syntax(subject_name, env) else {
        return false;
    };

    syntax_bindings_equal(&pattern_binding, &subject_binding)
}

fn syntax_bindings_equal(left: &SyntaxBinding, right: &SyntaxBinding) -> bool {
    match (left, right) {
        (SyntaxBinding::SpecialForm(left), SyntaxBinding::SpecialForm(right)) => left == right,
        (SyntaxBinding::Macro(left), SyntaxBinding::Macro(right)) => Rc::ptr_eq(left, right),
        _ => false,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &PatternBindings,
    expansion_ctx: &mut ExpansionContext,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Int(_)
        | Expr::Rational(_, _)
        | Expr::Bool(_)
        | Expr::String(_)
        | Expr::Char(_) => Ok(template.clone()),
        Expr::Symbol(name) => {
            if let Some(value) = bindings.single.get(name) {
                Ok(value.clone())
            } else if let Some(values) = bindings.repeated.get(name) {
                match values.as_slice() {
                    [value] => Ok(value.clone()),
                    _ => Err(EvalError::message(format!(
                        "pattern variable {name} must be used with ellipsis",
                    ))),
                }
            } else {
                Ok(Expr::Symbol(expansion_ctx.introduce_identifier(name)))
            }
        }
        Expr::List(items) => expand_template_list(items, bindings, expansion_ctx),
    }
}

fn expand_template_list(
    items: &[Expr],
    bindings: &PatternBindings,
    expansion_ctx: &mut ExpansionContext,
) -> Result<Expr, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0usize;

    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
            let Expr::Symbol(name) = &items[index] else {
                return Err(EvalError::message(
                    "ellipsis templates only support pattern variables",
                ));
            };

            let Some(values) = bindings.repeated.get(name) else {
                return Err(EvalError::message(format!(
                    "pattern variable {name} is not repeated",
                )));
            };
            expanded.extend(values.iter().cloned());
            index += 2;
            continue;
        }

        expanded.push(expand_template(&items[index], bindings, expansion_ctx)?);
        index += 1;
    }

    Ok(Expr::List(expanded))
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(symbol) if symbol == "...")
}

fn apply(procedure: Value, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut winds = Vec::new();
    let mut handlers = Vec::new();
    run_machine(start_call_machine(
        procedure,
        args.to_vec(),
        env,
        Vec::new(),
        &mut winds,
        &mut handlers,
    )?)
}

fn apply_evaluated(procedure: Value, args: Vec<Value>) -> Result<Value, EvalError> {
    let mut winds = Vec::new();
    let mut handlers = Vec::new();
    run_machine(invoke_procedure_machine(
        procedure,
        args,
        Vec::new(),
        &mut winds,
        &mut handlers,
    )?)
}

fn call_closure(closure: Rc<Closure>, args: Vec<Value>) -> Result<Value, EvalError> {
    run_machine(invoke_closure_machine(closure, args, Vec::new())?)
}

fn call_record_constructor(
    constructor: Rc<RecordConstructor>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    if args.len() != constructor.field_order.len() {
        return Err(EvalError::message(format!(
            "{} expects {} argument(s), got {}",
            constructor.name,
            constructor.field_order.len(),
            args.len()
        )));
    }

    let mut fields = vec![Value::Void; constructor.record_type.field_names.len()];
    for (value, field_index) in args
        .into_iter()
        .zip(constructor.field_order.iter().copied())
    {
        fields[field_index] = value;
    }

    Ok(Value::Record(Rc::new(RecordInstance {
        record_type: constructor.record_type.clone(),
        fields,
    })))
}

fn call_record_predicate(
    predicate: Rc<RecordPredicate>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    let actual_len = args.len();
    let [value] = args.as_slice() else {
        return Err(EvalError::message(format!(
            "{} expects 1 argument(s), got {}",
            predicate.name, actual_len
        )));
    };

    let is_match = matches!(
        value,
        Value::Record(record) if Rc::ptr_eq(&record.record_type, &predicate.record_type)
    );
    Ok(Value::Bool(is_match))
}

fn call_record_accessor(
    accessor: Rc<RecordAccessor>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    let actual_len = args.len();
    let [value] = args.as_slice() else {
        return Err(EvalError::message(format!(
            "{} expects 1 argument(s), got {}",
            accessor.name, actual_len
        )));
    };

    match value {
        Value::Record(record) if Rc::ptr_eq(&record.record_type, &accessor.record_type) => {
            Ok(record.fields[accessor.field_index].clone())
        }
        Value::Record(_) => Err(EvalError::message(format!(
            "{} expected {}",
            accessor.name, accessor.record_type.name
        ))),
        other => Err(type_error(&accessor.name, "record", other)),
    }
}

fn apply_map(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::message(format!(
            "map expects at least 2 argument(s), got {}",
            args.len()
        )));
    }

    let procedure = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(|value| collect_list_items("map", value))
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = lists[0].len();
    if lists.iter().any(|items| items.len() != expected_len) {
        return Err(EvalError::message("map expected lists of equal length"));
    }

    let mut results = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        let call_args = lists
            .iter()
            .map(|items| items[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_evaluated(procedure.clone(), call_args)?);
    }

    Ok(make_proper_list(results))
}

fn apply_builtin(builtin: Builtin, args: &[Value]) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Map
        | Builtin::Apply
        | Builtin::CallWithValues
        | Builtin::DynamicWind
        | Builtin::Raise
        | Builtin::WithExceptionHandler
        | Builtin::GuardProtect
        | Builtin::CallCc => {
            Err(EvalError::message(format!(
                "{} requires the machine evaluator",
                builtin.name()
            )))
        }
        Builtin::Add => Ok(number_to_value(add_numbers(&collect_numbers(builtin.name(), args)?))),
        Builtin::Sub => {
            let numbers = collect_numbers(builtin.name(), args)?;
            let numbers = require_min_args(builtin.name(), &numbers, 1)?;
            Ok(number_to_value(sub_numbers(numbers)?))
        }
        Builtin::Mul => Ok(number_to_value(mul_numbers(&collect_numbers(builtin.name(), args)?))),
        Builtin::Div => {
            let numbers = collect_numbers(builtin.name(), args)?;
            let numbers = require_min_args(builtin.name(), &numbers, 2)?;
            Ok(number_to_value(div_numbers(numbers)?))
        }
        Builtin::LessThan => compare_numbers(builtin.name(), args, |left, right| {
            compare_number_values(left, right) < 0
        }),
        Builtin::GreaterThan => compare_numbers(builtin.name(), args, |left, right| {
            compare_number_values(left, right) > 0
        }),
        Builtin::NumericEq => compare_numbers(builtin.name(), args, |left, right| {
            compare_number_values(left, right) == 0
        }),
        Builtin::LessEqual => compare_numbers(builtin.name(), args, |left, right| {
            compare_number_values(left, right) <= 0
        }),
        Builtin::Eq => {
            let [left, right] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            Ok(Value::Bool(is_eq(left, right)))
        }
        Builtin::Equal => {
            let [left, right] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            Ok(Value::Bool(is_equal(left, right)))
        }
        Builtin::Not => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(!is_truthy(value)))
        }
        Builtin::Cons => {
            let [car, cdr] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            Ok(Value::Pair(Rc::new(Pair {
                car: RefCell::new(car.clone()),
                cdr: RefCell::new(cdr.clone()),
            })))
        }
        Builtin::Car => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(expect_pair(builtin.name(), value)?.car.borrow().clone())
        }
        Builtin::Cdr => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(expect_pair(builtin.name(), value)?.cdr.borrow().clone())
        }
        Builtin::SetCar => {
            let [pair_value, new_value] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            *expect_pair(builtin.name(), pair_value)?.car.borrow_mut() = new_value.clone();
            Ok(Value::Void)
        }
        Builtin::SetCdr => {
            let [pair_value, new_value] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            *expect_pair(builtin.name(), pair_value)?.cdr.borrow_mut() = new_value.clone();
            Ok(Value::Void)
        }
        Builtin::NullPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::EmptyList)))
        }
        Builtin::List => Ok(make_proper_list(args.iter().cloned())),
        Builtin::Length => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Int(proper_list_length(builtin.name(), value)? as i64))
        }
        Builtin::Append => append_lists(args),
        Builtin::Reverse => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            let mut items = collect_list_items(builtin.name(), value)?;
            items.reverse();
            Ok(make_proper_list(items))
        }
        Builtin::ZeroPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(number_is_zero(&expect_number(builtin.name(), value)?)))
        }
        Builtin::Remainder => {
            let [left, right] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            let dividend = expect_exact_int(builtin.name(), left)?;
            let divisor = expect_exact_int(builtin.name(), right)?;
            if divisor == 0 {
                return Err(EvalError::message("division by zero"));
            }
            Ok(Value::Int(dividend % divisor))
        }
        Builtin::StringPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::String(_))))
        }
        Builtin::StringAppend => {
            let mut result = String::new();
            for value in args {
                result.push_str(expect_string(builtin.name(), value)?);
            }
            Ok(Value::String(result))
        }
        Builtin::NumberToString => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::String(render_number(&expect_number(builtin.name(), value)?)))
        }
        Builtin::StringToSymbol => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Symbol(expect_string(builtin.name(), value)?.to_string()))
        }
        Builtin::SymbolToString => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::String(expect_symbol(builtin.name(), value)?.to_string()))
        }
        Builtin::StringRef => {
            let [text, index] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            let text = expect_string(builtin.name(), text)?;
            let index = expect_exact_int(builtin.name(), index)?;
            if index < 0 {
                return Err(EvalError::message(format!(
                    "{} index out of range",
                    builtin.name()
                )));
            }
            let Some(ch) = text.chars().nth(index as usize) else {
                return Err(EvalError::message(format!(
                    "{} index out of range",
                    builtin.name()
                )));
            };
            Ok(Value::Char(ch))
        }
        Builtin::NumberPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Int(_) | Value::Rational(_, _))))
        }
        Builtin::BooleanPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Bool(_))))
        }
        Builtin::PairPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Pair(_))))
        }
        Builtin::SymbolPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Symbol(_))))
        }
        Builtin::Vector => Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec())))),
        Builtin::VectorRef => {
            let [vector, index] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            let vector = expect_vector(builtin.name(), vector)?;
            let index = expect_exact_int(builtin.name(), index)?;
            if index < 0 || index as usize >= vector.borrow().len() {
                return Err(EvalError::message(format!(
                    "{} index out of range",
                    builtin.name()
                )));
            }
            let value = vector.borrow()[index as usize].clone();
            Ok(value)
        }
        Builtin::Values => Ok(pack_values(args.to_vec())),
    }
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    run_machine(start_sequence_machine(exprs.to_vec(), env, Vec::new()))
}

fn run_machine(mut state: MachineState) -> Result<Value, EvalError> {
    let mut winds = Vec::new();
    let mut handlers = Vec::new();

    loop {
        state = match state {
            MachineState::Eval { expr, env, frames } => {
                eval_expr_machine(expr, env, frames)?
            }
            MachineState::Apply { value, mut frames } => {
                let Some(frame) = frames.pop() else {
                    return Ok(value);
                };

                match frame {
                    MachineFrame::ProcedureBoundary => MachineState::Apply { value, frames },
                    MachineFrame::CallCcResult => {
                        if matches!(value, Value::Void) && winds.is_empty() && handlers.is_empty() {
                            suspend_current_procedure_machine(value, frames)
                        } else {
                            MachineState::Apply { value, frames }
                        }
                    }
                    MachineFrame::CallWithValuesConsumer { consumer } => {
                        invoke_procedure_machine(
                            consumer,
                            unpack_values(value),
                            frames,
                            &mut winds,
                            &mut handlers,
                        )?
                    }
                    MachineFrame::Sequence { remaining, env } => {
                        start_sequence_machine(remaining, env, frames)
                    }
                    MachineFrame::IfBranch {
                        then_branch,
                        else_branch,
                        env,
                    } => {
                        let branch = if is_truthy(&value) {
                            then_branch
                        } else {
                            else_branch
                        };
                        MachineState::Eval {
                            expr: branch,
                            env,
                            frames,
                        }
                    }
                    MachineFrame::And { remaining, env } => {
                        if !is_truthy(&value) || remaining.is_empty() {
                            MachineState::Apply { value, frames }
                        } else {
                            continue_chain_machine(remaining, env, frames, |remaining, env| {
                                MachineFrame::And { remaining, env }
                            })?
                        }
                    }
                    MachineFrame::Or { remaining, env } => {
                        if is_truthy(&value) || remaining.is_empty() {
                            MachineState::Apply { value, frames }
                        } else {
                            continue_chain_machine(remaining, env, frames, |remaining, env| {
                                MachineFrame::Or { remaining, env }
                            })?
                        }
                    }
                    MachineFrame::DefineValue { name, env } => {
                        env.define(name, require_single_value("define", value)?);
                        MachineState::Apply {
                            value: Value::Void,
                            frames,
                        }
                    }
                    MachineFrame::SetValue { name, env } => {
                        if env.set(&name, require_single_value("set!", value)?) {
                            MachineState::Apply {
                                value: Value::Void,
                                frames,
                            }
                        } else {
                            return Err(EvalError::message(format!("unbound variable: {name}")));
                        }
                    }
                    MachineFrame::CallHead { args, env } => {
                        start_call_machine(
                            value,
                            args,
                            env,
                            frames,
                            &mut winds,
                            &mut handlers,
                        )?
                    }
                    MachineFrame::CallArgs {
                        procedure,
                        mut evaluated,
                        remaining,
                        env,
                    } => {
                        evaluated.push(require_single_value("procedure argument", value)?);
                        if remaining.is_empty() {
                            invoke_procedure_machine(
                                procedure,
                                evaluated,
                                frames,
                                &mut winds,
                                &mut handlers,
                            )?
                        } else {
                            continue_call_args_machine(
                                procedure, evaluated, remaining, env, frames,
                            )?
                        }
                    }
                    MachineFrame::LetBindings {
                        kind,
                        names,
                        mut evaluated,
                        remaining,
                        env,
                    } => {
                        evaluated.push(value);
                        if remaining.is_empty() {
                            finish_let_machine(kind, names, evaluated, env, frames)?
                        } else {
                            continue_let_machine(kind, names, evaluated, remaining, env, frames)?
                        }
                    }
                    MachineFrame::CondClause {
                        body,
                        remaining,
                        env,
                    } => {
                        if is_truthy(&value) {
                            if body.is_empty() {
                                MachineState::Apply { value, frames }
                            } else {
                                start_sequence_machine(body, env, frames)
                            }
                        } else {
                            eval_cond_machine(remaining, env, frames)?
                        }
                    }
                    MachineFrame::DynamicWindEntered { body_thunk, wind } => {
                        activate_wind(&mut winds, wind.clone());
                        frames.push(MachineFrame::DynamicWindBodyResult { wind });
                        invoke_procedure_machine(
                            body_thunk,
                            Vec::new(),
                            frames,
                            &mut winds,
                            &mut handlers,
                        )?
                    }
                    MachineFrame::DynamicWindBodyResult { wind } => {
                        deactivate_wind(&mut winds, &wind);
                        let body_result = value;
                        frames.push(MachineFrame::DynamicWindOutResult { body_result });
                        invoke_procedure_machine(
                            wind.out_thunk.clone(),
                            Vec::new(),
                            frames,
                            &mut winds,
                            &mut handlers,
                        )?
                    }
                    MachineFrame::DynamicWindOutResult { body_result } => MachineState::Apply {
                        value: body_result,
                        frames,
                    },
                    MachineFrame::WithExceptionHandlerResult { handler } => {
                        deactivate_handler(&mut handlers, &handler);
                        MachineState::Apply { value, frames }
                    }
                    MachineFrame::ExceptionHandlerReturned => {
                        return Err(EvalError::message("raise handler returned"));
                    }
                    MachineFrame::ExceptionWindExit {
                        remaining,
                        entering,
                        handler,
                        exception,
                    } => continue_raise_after_exit(
                        remaining,
                        entering,
                        handler,
                        exception,
                        frames,
                        &mut winds,
                        &mut handlers,
                    )?,
                    MachineFrame::ExceptionWindEnter {
                        current,
                        remaining,
                        handler,
                        exception,
                    } => {
                        activate_wind(&mut winds, current);
                        continue_raise_after_enter(
                            remaining,
                            handler,
                            exception,
                            frames,
                            &mut winds,
                            &mut handlers,
                        )?
                    }
                }
            }
        };
    }
}

fn eval_expr_machine(
    expr: Expr,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    match expr {
        Expr::Int(value) => Ok(MachineState::Apply {
            value: Value::Int(value),
            frames,
        }),
        Expr::Rational(numerator, denominator) => Ok(MachineState::Apply {
            value: Value::Rational(numerator, denominator),
            frames,
        }),
        Expr::Bool(value) => Ok(MachineState::Apply {
            value: Value::Bool(value),
            frames,
        }),
        Expr::String(value) => Ok(MachineState::Apply {
            value: Value::String(value),
            frames,
        }),
        Expr::Char(value) => Ok(MachineState::Apply {
            value: Value::Char(value),
            frames,
        }),
        Expr::Symbol(name) => Ok(MachineState::Apply {
            value: env
                .lookup(&name)
                .ok_or_else(|| EvalError::message(format!("unbound variable: {name}")))?,
            frames,
        }),
        Expr::List(items) => {
            let mut iter = items.into_iter();
            let Some(head) = iter.next() else {
                return Err(EvalError::message("cannot evaluate empty list"));
            };
            let tail = iter.collect::<Vec<_>>();

            if let Expr::Symbol(symbol) = &head {
                if let Some(binding) = resolve_syntax(symbol, &env) {
                    return match binding {
                        SyntaxBinding::Macro(macro_def) => {
                            let mut call_items = Vec::with_capacity(1 + tail.len());
                            call_items.push(head);
                            call_items.extend(tail);
                            let expanded =
                                expand_macro_call(&macro_def, &Expr::List(call_items), &env)?;
                            Ok(MachineState::Eval {
                                expr: expanded,
                                env,
                                frames,
                            })
                        }
                        SyntaxBinding::SpecialForm(special_form) => {
                            handle_special_form_machine(special_form, tail, env, frames)
                        }
                    };
                }
            }

            frames.push(MachineFrame::CallHead {
                args: tail,
                env: env.clone(),
            });
            Ok(MachineState::Eval {
                expr: head,
                env,
                frames,
            })
        }
    }
}

fn handle_special_form_machine(
    special_form: SpecialForm,
    args: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    match special_form {
        SpecialForm::Define => match args.as_slice() {
            [Expr::Symbol(name), value_expr] => {
                frames.push(MachineFrame::DefineValue {
                    name: name.clone(),
                    env: env.clone(),
                });
                Ok(MachineState::Eval {
                    expr: value_expr.clone(),
                    env,
                    frames,
                })
            }
            [Expr::List(signature), body @ ..] if !body.is_empty() => {
                let Some((Expr::Symbol(name), params)) = signature.split_first() else {
                    return Err(EvalError::message("invalid define"));
                };

                env.define(
                    name.clone(),
                    Value::Closure(Rc::new(Closure {
                        name: Some(name.clone()),
                        params: parse_param_names(params)?,
                        body: body.to_vec(),
                        env: env.clone(),
                    })),
                );
                Ok(MachineState::Apply {
                    value: Value::Void,
                    frames,
                })
            }
            _ => Err(EvalError::message("invalid define")),
        },
        SpecialForm::If => match args.as_slice() {
            [condition, then_branch, else_branch] => {
                frames.push(MachineFrame::IfBranch {
                    then_branch: then_branch.clone(),
                    else_branch: else_branch.clone(),
                    env: env.clone(),
                });
                Ok(MachineState::Eval {
                    expr: condition.clone(),
                    env,
                    frames,
                })
            }
            _ => Err(EvalError::message("if expects exactly 3 arguments")),
        },
        SpecialForm::Quote => match args.as_slice() {
            [expr] => Ok(MachineState::Apply {
                value: quote(expr),
                frames,
            }),
            _ => Err(EvalError::message("quote expects exactly 1 argument")),
        },
        SpecialForm::Lambda => match args.as_slice() {
            [params_expr, body @ ..] if !body.is_empty() => Ok(MachineState::Apply {
                value: Value::Closure(Rc::new(Closure {
                    name: None,
                    params: parse_params_expr(params_expr)?,
                    body: body.to_vec(),
                    env,
                })),
                frames,
            }),
            _ => Err(EvalError::message(
                "lambda expects parameters and at least one body expression",
            )),
        },
        SpecialForm::And => {
            if args.is_empty() {
                Ok(MachineState::Apply {
                    value: Value::Bool(true),
                    frames,
                })
            } else {
                continue_chain_machine(args, env, frames, |remaining, env| MachineFrame::And {
                    remaining,
                    env,
                })
            }
        }
        SpecialForm::Or => {
            if args.is_empty() {
                Ok(MachineState::Apply {
                    value: Value::Bool(false),
                    frames,
                })
            } else {
                continue_chain_machine(args, env, frames, |remaining, env| MachineFrame::Or {
                    remaining,
                    env,
                })
            }
        }
        SpecialForm::Let => match args.as_slice() {
            [Expr::Symbol(name), bindings_expr, body @ ..] if !body.is_empty() => {
                let bindings = parse_bindings(bindings_expr)?;
                start_let_machine(
                    MachineLetKind::Named {
                        name: name.clone(),
                        body: body.to_vec(),
                    },
                    bindings,
                    env,
                    frames,
                )
            }
            [bindings_expr, body @ ..] if !body.is_empty() => start_let_machine(
                MachineLetKind::Unnamed {
                    body: body.to_vec(),
                },
                parse_bindings(bindings_expr)?,
                env,
                frames,
            ),
            _ => Err(EvalError::message(
                "let expects bindings and at least one body expression",
            )),
        },
        SpecialForm::Letrec => Ok(MachineState::Apply {
            value: eval_letrec(&args, env)?,
            frames,
        }),
        SpecialForm::Begin => Ok(start_sequence_machine(args, env, frames)),
        SpecialForm::Cond => eval_cond_machine(args, env, frames),
        SpecialForm::Set => match args.as_slice() {
            [Expr::Symbol(name), value_expr] => {
                frames.push(MachineFrame::SetValue {
                    name: name.clone(),
                    env: env.clone(),
                });
                Ok(MachineState::Eval {
                    expr: value_expr.clone(),
                    env,
                    frames,
                })
            }
            _ => Err(EvalError::message("set! expects a variable and a value")),
        },
        SpecialForm::Guard => Ok(MachineState::Eval {
            expr: desugar_guard(&args)?,
            env,
            frames,
        }),
        SpecialForm::Do => Ok(MachineState::Eval {
            expr: desugar_do(&args)?,
            env,
            frames,
        }),
        SpecialForm::DefineSyntax => {
            let value = eval_define_syntax(&args, env)?;
            Ok(MachineState::Apply { value, frames })
        }
        SpecialForm::DefineRecordType => {
            let value = eval_define_record_type(&args, env)?;
            Ok(MachineState::Apply { value, frames })
        }
    }
}

fn start_sequence_machine(
    exprs: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
) -> MachineState {
    let mut iter = exprs.into_iter();
    let Some(first) = iter.next() else {
        return MachineState::Apply {
            value: Value::Void,
            frames,
        };
    };

    let remaining = iter.collect::<Vec<_>>();
    if !remaining.is_empty() {
        frames.push(MachineFrame::Sequence {
            remaining,
            env: env.clone(),
        });
    }

    MachineState::Eval {
        expr: first,
        env,
        frames,
    }
}

fn continue_chain_machine(
    exprs: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
    frame_builder: impl Fn(Vec<Expr>, EnvRef) -> MachineFrame,
) -> Result<MachineState, EvalError> {
    let mut iter = exprs.into_iter();
    let Some(first) = iter.next() else {
        return Err(EvalError::message("internal error: empty expression chain"));
    };

    let remaining = iter.collect::<Vec<_>>();
    if !remaining.is_empty() {
        frames.push(frame_builder(remaining, env.clone()));
    }

    Ok(MachineState::Eval {
        expr: first,
        env,
        frames,
    })
}

fn start_call_machine(
    procedure: Value,
    args: Vec<Expr>,
    env: EnvRef,
    frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let procedure = require_single_value("procedure position", procedure)?;
    if args.is_empty() {
        invoke_procedure_machine(procedure, Vec::new(), frames, winds, handlers)
    } else {
        continue_call_args_machine(procedure, Vec::new(), args, env, frames)
    }
}

fn continue_call_args_machine(
    procedure: Value,
    evaluated: Vec<Value>,
    args: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let mut iter = args.into_iter();
    let Some(first) = iter.next() else {
        return Err(EvalError::message(
            "internal error: empty call argument list",
        ));
    };

    frames.push(MachineFrame::CallArgs {
        procedure,
        evaluated,
        remaining: iter.collect(),
        env: env.clone(),
    });
    Ok(MachineState::Eval {
        expr: first,
        env,
        frames,
    })
}

fn start_let_machine(
    kind: MachineLetKind,
    bindings: Vec<(String, Expr)>,
    env: EnvRef,
    frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let (names, exprs): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    if exprs.is_empty() {
        finish_let_machine(kind, names, Vec::new(), env, frames)
    } else {
        continue_let_machine(kind, names, Vec::new(), exprs, env, frames)
    }
}

fn continue_let_machine(
    kind: MachineLetKind,
    names: Vec<String>,
    evaluated: Vec<Value>,
    exprs: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let mut iter = exprs.into_iter();
    let Some(first) = iter.next() else {
        return Err(EvalError::message("internal error: empty let binding list"));
    };

    frames.push(MachineFrame::LetBindings {
        kind,
        names,
        evaluated,
        remaining: iter.collect(),
        env: env.clone(),
    });
    Ok(MachineState::Eval {
        expr: first,
        env,
        frames,
    })
}

fn finish_let_machine(
    kind: MachineLetKind,
    names: Vec<String>,
    values: Vec<Value>,
    env: EnvRef,
    frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    match kind {
        MachineLetKind::Unnamed { body } => {
            let let_env = Env::new(Some(env));
            for (name, value) in names.into_iter().zip(values.into_iter()) {
                let_env.define(name, value);
            }
            Ok(start_sequence_machine(body, let_env, frames))
        }
        MachineLetKind::Named { name, body } => invoke_closure_machine(
            Rc::new(Closure {
                name: Some(name),
                params: names,
                body,
                env,
            }),
            values,
            frames,
        ),
    }
}

fn eval_cond_machine(
    clauses: Vec<Expr>,
    env: EnvRef,
    mut frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let mut iter = clauses.into_iter();
    let Some(clause) = iter.next() else {
        return Ok(MachineState::Apply {
            value: Value::Void,
            frames,
        });
    };
    let remaining = iter.collect::<Vec<_>>();

    let Expr::List(items) = clause else {
        return Err(EvalError::message("cond clauses must be lists"));
    };

    let Some((test_expr, body)) = items.split_first() else {
        return Err(EvalError::message("cond clauses cannot be empty"));
    };

    if matches!(test_expr, Expr::Symbol(symbol) if symbol == "else") {
        if !remaining.is_empty() {
            return Err(EvalError::message("cond else clause must be last"));
        }
        if body.is_empty() {
            return Err(EvalError::message("cond else clause requires a body"));
        }
        return Ok(start_sequence_machine(body.to_vec(), env, frames));
    }

    frames.push(MachineFrame::CondClause {
        body: body.to_vec(),
        remaining,
        env: env.clone(),
    });
    Ok(MachineState::Eval {
        expr: test_expr.clone(),
        env,
        frames,
    })
}

fn invoke_procedure_machine(
    procedure: Value,
    args: Vec<Value>,
    frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    match procedure {
        Value::Builtin(Builtin::CallCc) => invoke_callcc_machine(args, frames, winds, handlers),
        Value::Builtin(Builtin::Apply) => invoke_apply_machine(args, frames, winds, handlers),
        Value::Builtin(Builtin::CallWithValues) => {
            invoke_call_with_values_machine(args, frames, winds, handlers)
        }
        Value::Builtin(Builtin::DynamicWind) => {
            invoke_dynamic_wind_machine(args, frames, winds, handlers)
        }
        Value::Builtin(Builtin::Raise) => invoke_raise_machine(args, frames, winds, handlers),
        Value::Builtin(Builtin::WithExceptionHandler) => {
            invoke_with_exception_handler_machine(args, frames, winds, handlers)
        }
        Value::Builtin(Builtin::GuardProtect) => {
            invoke_guard_protect_machine(args, frames, winds, handlers)
        }
        Value::Builtin(Builtin::Map) => Ok(MachineState::Apply {
            value: apply_map(args)?,
            frames,
        }),
        Value::Builtin(builtin) => Ok(MachineState::Apply {
            value: apply_builtin(builtin, &args)?,
            frames,
        }),
        Value::Closure(closure) => invoke_closure_machine(closure, args, frames),
        Value::RecordConstructor(constructor) => Ok(MachineState::Apply {
            value: call_record_constructor(constructor, args)?,
            frames,
        }),
        Value::RecordPredicate(predicate) => Ok(MachineState::Apply {
            value: call_record_predicate(predicate, args)?,
            frames,
        }),
        Value::RecordAccessor(accessor) => Ok(MachineState::Apply {
            value: call_record_accessor(accessor, args)?,
            frames,
        }),
        Value::Continuation(continuation) => invoke_continuation_machine(continuation, args),
        _ => Err(EvalError::message("attempted to call a non-procedure")),
    }
}

fn invoke_callcc_machine(
    args: Vec<Value>,
    frames: Vec<MachineFrame>,
    _winds: &mut Vec<Rc<DynamicWindContext>>,
    _handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let [procedure] = require_exact_args("call/cc", &args, 1)? else {
        unreachable!();
    };

    let continuation = Value::Continuation(Rc::new(Continuation {
        frames: frames.clone(),
    }));
    let mut callcc_frames = frames;
    callcc_frames.push(MachineFrame::CallCcResult);
    invoke_procedure_machine(
        procedure.clone(),
        vec![continuation],
        callcc_frames,
        _winds,
        _handlers,
    )
}

fn invoke_apply_machine(
    args: Vec<Value>,
    frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::message(format!(
            "apply expects at least 2 argument(s), got {}",
            args.len()
        )));
    }

    let procedure = args[0].clone();
    let prefix_args = args[1..args.len() - 1].to_vec();
    let list_args = collect_list_items("apply", args.last().unwrap())?;
    let mut call_args = prefix_args;
    call_args.extend(list_args);
    invoke_procedure_machine(procedure, call_args, frames, winds, handlers)
}

fn invoke_call_with_values_machine(
    args: Vec<Value>,
    mut frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let [producer, consumer] = require_exact_args("call-with-values", &args, 2)? else {
        unreachable!();
    };

    frames.push(MachineFrame::CallWithValuesConsumer {
        consumer: consumer.clone(),
    });
    invoke_procedure_machine(producer.clone(), Vec::new(), frames, winds, handlers)
}

fn invoke_dynamic_wind_machine(
    args: Vec<Value>,
    mut frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let [in_thunk, body_thunk, out_thunk] = require_exact_args("dynamic-wind", &args, 3)? else {
        unreachable!();
    };

    let wind = Rc::new(DynamicWindContext {
        in_thunk: in_thunk.clone(),
        out_thunk: out_thunk.clone(),
    });
    frames.push(MachineFrame::DynamicWindEntered {
        body_thunk: body_thunk.clone(),
        wind,
    });
    invoke_procedure_machine(in_thunk.clone(), Vec::new(), frames, winds, handlers)
}

fn invoke_raise_machine(
    args: Vec<Value>,
    frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let [exception] = require_exact_args("raise", &args, 1)? else {
        unreachable!();
    };
    raise_machine(exception.clone(), frames, winds, handlers)
}

fn invoke_with_exception_handler_machine(
    args: Vec<Value>,
    mut frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let [handler_proc, thunk] = require_exact_args("with-exception-handler", &args, 2)? else {
        unreachable!();
    };

    let handler = Rc::new(ExceptionHandlerContext {
        handler: handler_proc.clone(),
        outer_frame_len: frames.len(),
        outer_wind_len: winds.len(),
        outer_handler_len: handlers.len(),
        allow_return: false,
    });
    handlers.push(handler.clone());
    frames.push(MachineFrame::WithExceptionHandlerResult { handler });
    invoke_procedure_machine(thunk.clone(), Vec::new(), frames, winds, handlers)
}

fn invoke_guard_protect_machine(
    args: Vec<Value>,
    mut frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let [handler_proc, thunk] = require_exact_args("__guard-protect", &args, 2)? else {
        unreachable!();
    };

    let handler = Rc::new(ExceptionHandlerContext {
        handler: handler_proc.clone(),
        outer_frame_len: frames.len(),
        outer_wind_len: winds.len(),
        outer_handler_len: handlers.len(),
        allow_return: true,
    });
    handlers.push(handler.clone());
    frames.push(MachineFrame::WithExceptionHandlerResult { handler });
    invoke_procedure_machine(thunk.clone(), Vec::new(), frames, winds, handlers)
}

fn raise_machine(
    exception: Value,
    frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    let Some(handler) = handlers.last().cloned() else {
        return Err(EvalError::message(format!(
            "unhandled exception: {}",
            render(&exception)
        )));
    };

    handlers.truncate(handler.outer_handler_len);
    let mut exiting = winds[handler.outer_wind_len..].to_vec();
    exiting.reverse();
    let entering = Vec::new();
    continue_raise_after_exit(exiting, entering, handler, exception, frames, winds, handlers)
}

fn continue_raise_after_exit(
    remaining: Vec<Rc<DynamicWindContext>>,
    entering: Vec<Rc<DynamicWindContext>>,
    handler: Rc<ExceptionHandlerContext>,
    exception: Value,
    mut frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    if remaining.is_empty() {
        return continue_raise_after_enter(entering, handler, exception, frames, winds, handlers);
    }

    let current = remaining[0].clone();
    let tail = remaining[1..].to_vec();
    deactivate_wind(winds, &current);
    frames.push(MachineFrame::ExceptionWindExit {
        remaining: tail,
        entering,
        handler,
        exception,
    });
    invoke_procedure_machine(current.out_thunk.clone(), Vec::new(), frames, winds, handlers)
}

fn continue_raise_after_enter(
    remaining: Vec<Rc<DynamicWindContext>>,
    handler: Rc<ExceptionHandlerContext>,
    exception: Value,
    mut frames: Vec<MachineFrame>,
    winds: &mut Vec<Rc<DynamicWindContext>>,
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
) -> Result<MachineState, EvalError> {
    if remaining.is_empty() {
        frames.truncate(handler.outer_frame_len);
        if !handler.allow_return {
            frames.push(MachineFrame::ExceptionHandlerReturned);
        }
        winds.truncate(handler.outer_wind_len);
        handlers.truncate(handler.outer_handler_len);
        return invoke_procedure_machine(
            handler.handler.clone(),
            vec![exception],
            frames,
            winds,
            handlers,
        );
    }

    let current = remaining[0].clone();
    let tail = remaining[1..].to_vec();
    frames.push(MachineFrame::ExceptionWindEnter {
        current: current.clone(),
        remaining: tail,
        handler,
        exception,
    });
    invoke_procedure_machine(current.in_thunk.clone(), Vec::new(), frames, winds, handlers)
}

fn shared_wind_prefix(
    current: &[Rc<DynamicWindContext>],
    target: &[Rc<DynamicWindContext>],
) -> usize {
    current
        .iter()
        .zip(target.iter())
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count()
}

fn activate_wind(winds: &mut Vec<Rc<DynamicWindContext>>, wind: Rc<DynamicWindContext>) {
    winds.push(wind);
}

fn deactivate_wind(winds: &mut Vec<Rc<DynamicWindContext>>, wind: &Rc<DynamicWindContext>) {
    if winds
        .last()
        .map(|current| Rc::ptr_eq(current, wind))
        .unwrap_or(false)
    {
        winds.pop();
    } else {
        winds.retain(|current| !Rc::ptr_eq(current, wind));
    }
}

fn deactivate_handler(
    handlers: &mut Vec<Rc<ExceptionHandlerContext>>,
    handler: &Rc<ExceptionHandlerContext>,
) {
    if handlers
        .last()
        .map(|current| Rc::ptr_eq(current, handler))
        .unwrap_or(false)
    {
        handlers.pop();
    } else {
        handlers.retain(|current| !Rc::ptr_eq(current, handler));
    }
}

fn invoke_closure_machine(
    closure: Rc<Closure>,
    args: Vec<Value>,
    frames: Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if args.len() != closure.params.len() {
        let proc_name = closure.name.as_deref().unwrap_or("lambda");
        return Err(EvalError::message(format!(
            "{proc_name} expects {} argument(s), got {}",
            closure.params.len(),
            args.len()
        )));
    }

    let call_env = Env::new(Some(closure.env.clone()));
    if let Some(name) = &closure.name {
        call_env.define(name.clone(), Value::Closure(closure.clone()));
    }

    for (param, value) in closure.params.iter().zip(args.into_iter()) {
        call_env.define(param.clone(), value);
    }

    let mut frames = frames;
    frames.push(MachineFrame::ProcedureBoundary);
    Ok(start_sequence_machine(
        closure.body.clone(),
        call_env,
        frames,
    ))
}

fn invoke_continuation_machine(
    continuation: Rc<Continuation>,
    args: Vec<Value>,
) -> Result<MachineState, EvalError> {
    Ok(MachineState::Apply {
        value: pack_values(args),
        frames: continuation.frames.clone(),
    })
}

fn suspend_current_procedure_machine(value: Value, mut frames: Vec<MachineFrame>) -> MachineState {
    while let Some(frame) = frames.pop() {
        if matches!(frame, MachineFrame::ProcedureBoundary) {
            break;
        }
    }

    MachineState::Apply { value, frames }
}

#[derive(Clone, Copy)]
enum Number {
    Int(i64),
    Rational(i64, i64),
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(Number, Number) -> bool,
) -> Result<Value, EvalError> {
    let numbers = collect_numbers(name, args)?;
    let numbers = require_min_args(name, &numbers, 2)?;
    Ok(Value::Bool(
        numbers.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn collect_numbers(name: &str, args: &[Value]) -> Result<Vec<Number>, EvalError> {
    args.iter().map(|value| expect_number(name, value)).collect()
}

fn expect_number(name: &str, value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Int(number) => Ok(Number::Int(*number)),
        Value::Rational(numerator, denominator) => Ok(Number::Rational(*numerator, *denominator)),
        other => Err(type_error(name, "number", other)),
    }
}

fn expect_exact_int(name: &str, value: &Value) -> Result<i64, EvalError> {
    match expect_number(name, value)? {
        Number::Int(number) => Ok(number),
        Number::Rational(_, _) => Err(EvalError::message(format!(
            "{name} expected an exact integer"
        ))),
    }
}

fn expect_string<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::String(text) => Ok(text),
        other => Err(type_error(name, "string", other)),
    }
}

fn expect_symbol<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(text) => Ok(text),
        other => Err(type_error(name, "symbol", other)),
    }
}

fn expect_vector(
    name: &str,
    value: &Value,
) -> Result<Rc<RefCell<Vec<Value>>>, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector.clone()),
        other => Err(type_error(name, "vector", other)),
    }
}

fn pack_values(values: Vec<Value>) -> Value {
    match values.as_slice() {
        [] => Value::Multi(Vec::new()),
        [value] => value.clone(),
        _ => Value::Multi(values),
    }
}

fn unpack_values(value: Value) -> Vec<Value> {
    match value {
        Value::Multi(values) => values,
        other => vec![other],
    }
}

fn require_single_value(context: &str, value: Value) -> Result<Value, EvalError> {
    match value {
        Value::Multi(values) if values.len() == 1 => Ok(values.into_iter().next().unwrap()),
        Value::Multi(values) => Err(EvalError::message(format!(
            "{context} expected 1 value, got {}",
            values.len()
        ))),
        other => Ok(other),
    }
}

fn number_to_value(number: Number) -> Value {
    match number {
        Number::Int(value) => Value::Int(value),
        Number::Rational(numerator, denominator) => Value::Rational(numerator, denominator),
    }
}

fn render_number(number: &Number) -> String {
    match number {
        Number::Int(value) => value.to_string(),
        Number::Rational(numerator, denominator) => format!("{numerator}/{denominator}"),
    }
}

fn number_is_zero(number: &Number) -> bool {
    match number {
        Number::Int(value) => *value == 0,
        Number::Rational(numerator, _) => *numerator == 0,
    }
}

fn compare_number_values(left: Number, right: Number) -> i32 {
    let (left_num, left_den) = number_fraction(left);
    let (right_num, right_den) = number_fraction(right);
    let lhs = left_num * right_den;
    let rhs = right_num * left_den;
    match lhs.cmp(&rhs) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

fn add_numbers(numbers: &[Number]) -> Number {
    numbers.iter().copied().fold(Number::Int(0), |acc, value| {
        let (left_num, left_den) = number_fraction(acc);
        let (right_num, right_den) = number_fraction(value);
        normalize_number(left_num * right_den + right_num * left_den, left_den * right_den)
    })
}

fn sub_numbers(numbers: &[Number]) -> Result<Number, EvalError> {
    let Some((first, rest)) = numbers.split_first() else {
        return Ok(Number::Int(0));
    };

    if rest.is_empty() {
        let (numerator, denominator) = number_fraction(*first);
        return Ok(normalize_number(-numerator, denominator));
    }

    Ok(rest.iter().copied().fold(*first, |acc, value| {
        let (left_num, left_den) = number_fraction(acc);
        let (right_num, right_den) = number_fraction(value);
        normalize_number(left_num * right_den - right_num * left_den, left_den * right_den)
    }))
}

fn mul_numbers(numbers: &[Number]) -> Number {
    numbers.iter().copied().fold(Number::Int(1), |acc, value| {
        let (left_num, left_den) = number_fraction(acc);
        let (right_num, right_den) = number_fraction(value);
        normalize_number(left_num * right_num, left_den * right_den)
    })
}

fn div_numbers(numbers: &[Number]) -> Result<Number, EvalError> {
    let Some((first, rest)) = numbers.split_first() else {
        return Ok(Number::Int(1));
    };

    let mut result = *first;
    for divisor in rest {
        if number_is_zero(divisor) {
            return Err(EvalError::message("division by zero"));
        }
        let (left_num, left_den) = number_fraction(result);
        let (right_num, right_den) = number_fraction(*divisor);
        result = normalize_number(left_num * right_den, left_den * right_num);
    }
    Ok(result)
}

fn number_fraction(number: Number) -> (i128, i128) {
    match number {
        Number::Int(value) => (value as i128, 1),
        Number::Rational(numerator, denominator) => (numerator as i128, denominator as i128),
    }
}

fn normalize_number(numerator: i128, denominator: i128) -> Number {
    assert!(denominator != 0);
    let (numerator, denominator) = if denominator < 0 {
        (-numerator, -denominator)
    } else {
        (numerator, denominator)
    };
    let divisor = gcd_i128(numerator, denominator);
    let numerator = numerator / divisor;
    let denominator = denominator / divisor;

    if denominator == 1 {
        Number::Int(numerator as i64)
    } else {
        Number::Rational(numerator as i64, denominator as i64)
    }
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    left = left.abs();
    right = right.abs();
    while right != 0 {
        let next = left % right;
        left = right;
        right = next;
    }
    if left == 0 { 1 } else { left }
}

fn is_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => left == right,
        (Value::Rational(left_num, left_den), Value::Rational(right_num, right_den)) => {
            left_num == right_num && left_den == right_den
        }
        (Value::Int(_), Value::Rational(_, _)) | (Value::Rational(_, _), Value::Int(_)) => {
            compare_number_values(
                expect_number("eq?", left).unwrap(),
                expect_number("eq?", right).unwrap(),
            ) == 0
        }
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left.name() == right.name(),
        (Value::Closure(left), Value::Closure(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::RecordConstructor(left), Value::RecordConstructor(right)) => Rc::ptr_eq(left, right),
        (Value::RecordPredicate(left), Value::RecordPredicate(right)) => Rc::ptr_eq(left, right),
        (Value::RecordAccessor(left), Value::RecordAccessor(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::Multi(left), Value::Multi(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| is_eq(left, right))
        }
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn is_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Pair(left_pair), Value::Pair(right_pair)) => {
            is_equal(&left_pair.car.borrow(), &right_pair.car.borrow())
                && is_equal(&left_pair.cdr.borrow(), &right_pair.cdr.borrow())
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let left = left.borrow();
            let right = right.borrow();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| is_equal(left, right))
        }
        _ => is_eq(left, right),
    }
}

fn require_exact_args<'a>(
    name: &str,
    args: &'a [Value],
    expected: usize,
) -> Result<&'a [Value], EvalError> {
    if args.len() == expected {
        Ok(args)
    } else {
        Err(EvalError::message(format!(
            "{name} expects {expected} argument(s), got {}",
            args.len()
        )))
    }
}

fn require_min_args<'a, T>(name: &str, args: &'a [T], min: usize) -> Result<&'a [T], EvalError> {
    if args.len() >= min {
        Ok(args)
    } else {
        Err(EvalError::message(format!(
            "{name} expects at least {min} argument(s), got {}",
            args.len()
        )))
    }
}

fn expect_pair(name: &str, value: &Value) -> Result<Rc<Pair>, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        other => Err(type_error(name, "pair", other)),
    }
}

fn proper_list_length(name: &str, value: &Value) -> Result<usize, EvalError> {
    let mut current = value.clone();
    let mut count = 0usize;

    loop {
        match current {
            Value::EmptyList => return Ok(count),
            Value::Pair(pair) => {
                count += 1;
                current = pair.cdr.borrow().clone();
            }
            other => return Err(type_error(name, "list", &other)),
        }
    }
}

fn collect_list_items(name: &str, value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut current = value.clone();
    let mut items = Vec::new();

    loop {
        match current {
            Value::EmptyList => return Ok(items),
            Value::Pair(pair) => {
                items.push(pair.car.borrow().clone());
                current = pair.cdr.borrow().clone();
            }
            other => return Err(type_error(name, "list", &other)),
        }
    }
}

fn append_lists(args: &[Value]) -> Result<Value, EvalError> {
    let Some(last) = args.last() else {
        return Ok(Value::EmptyList);
    };

    let mut result = last.clone();
    for list in args[..args.len() - 1].iter().rev() {
        result = append_front(list, result)?;
    }
    Ok(result)
}

fn append_front(list: &Value, tail: Value) -> Result<Value, EvalError> {
    match list {
        Value::EmptyList => Ok(tail),
        Value::Pair(pair) => Ok(Value::Pair(Rc::new(Pair {
            car: RefCell::new(pair.car.borrow().clone()),
            cdr: RefCell::new(append_front(&pair.cdr.borrow().clone(), tail)?),
        }))),
        other => Err(type_error("append", "list", other)),
    }
}

fn parse_params_expr(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::message("parameter list must be a list"));
    };
    parse_param_names(params)
}

fn parse_param_names(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|expr| match expr {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::message("parameter names must be symbols")),
        })
        .collect()
}

fn parse_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::message("let bindings must be a list"));
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) => match items.as_slice() {
                [Expr::Symbol(name), value_expr] => Ok((name.clone(), value_expr.clone())),
                _ => Err(EvalError::message("invalid let binding")),
            },
            _ => Err(EvalError::message("invalid let binding")),
        })
        .collect()
}

fn parse_do_bindings(expr: &Expr) -> Result<Vec<(String, Expr, Option<Expr>)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::message("do bindings must be a list"));
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) => match items.as_slice() {
                [Expr::Symbol(name), init] => Ok((name.clone(), init.clone(), None)),
                [Expr::Symbol(name), init, step] => {
                    Ok((name.clone(), init.clone(), Some(step.clone())))
                }
                _ => Err(EvalError::message("invalid do binding")),
            },
            _ => Err(EvalError::message("invalid do binding")),
        })
        .collect()
}

fn desugar_guard(args: &[Expr]) -> Result<Expr, EvalError> {
    let [Expr::List(header), body @ ..] = args else {
        return Err(EvalError::message("invalid guard"));
    };
    if body.is_empty() {
        return Err(EvalError::message("invalid guard"));
    }

    let Some((Expr::Symbol(name), clauses)) = header.split_first() else {
        return Err(EvalError::message("invalid guard"));
    };

    let exception_name = fresh_symbol("guard_exception");

    let mut cond_clauses = clauses.to_vec();
    if !cond_clauses.iter().any(is_else_clause) {
        cond_clauses.push(Expr::List(vec![
            Expr::Symbol("else".into()),
            Expr::List(vec![
                Expr::Symbol("raise".into()),
                Expr::Symbol(name.clone()),
            ]),
        ]));
    }

    let mut cond_items = vec![Expr::Symbol("cond".into())];
    cond_items.extend(cond_clauses);
    let cond_expr = Expr::List(cond_items);

    let let_expr = Expr::List(vec![
        Expr::Symbol("let".into()),
        Expr::List(vec![Expr::List(vec![
            Expr::Symbol(name.clone()),
            Expr::Symbol(exception_name.clone()),
        ])]),
        cond_expr,
    ]);

    let handler_lambda = Expr::List(vec![
        Expr::Symbol("lambda".into()),
        Expr::List(vec![Expr::Symbol(exception_name)]),
        let_expr,
    ]);

    let mut thunk_items = vec![Expr::Symbol("lambda".into()), Expr::List(Vec::new())];
    thunk_items.extend(body.iter().cloned());
    let thunk_lambda = Expr::List(thunk_items);

    Ok(Expr::List(vec![
        Expr::Symbol("__guard-protect".into()),
        handler_lambda,
        thunk_lambda,
    ]))
}

fn desugar_do(args: &[Expr]) -> Result<Expr, EvalError> {
    let [bindings_expr, Expr::List(test_items), body @ ..] = args else {
        return Err(EvalError::message("invalid do"));
    };
    let Some((test_expr, result_exprs)) = test_items.split_first() else {
        return Err(EvalError::message("invalid do"));
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let loop_name = fresh_symbol("do_loop");

    let named_bindings = bindings
        .iter()
        .map(|(name, init_expr, _)| {
            Expr::List(vec![Expr::Symbol(name.clone()), init_expr.clone()])
        })
        .collect::<Vec<_>>();

    let loop_args = bindings
        .iter()
        .map(|(name, _, step_expr)| {
            step_expr
                .clone()
                .unwrap_or_else(|| Expr::Symbol(name.clone()))
        })
        .collect::<Vec<_>>();

    let mut recurse_items = vec![Expr::Symbol(loop_name.clone())];
    recurse_items.extend(loop_args);
    let recurse_expr = Expr::List(recurse_items);

    let false_branch = begin_expr(
        body.iter()
            .cloned()
            .chain(std::iter::once(recurse_expr))
            .collect(),
    );
    let true_branch = begin_expr(result_exprs.to_vec());

    let loop_body = Expr::List(vec![
        Expr::Symbol("if".into()),
        test_expr.clone(),
        true_branch,
        false_branch,
    ]);

    Ok(Expr::List(vec![
        Expr::Symbol("let".into()),
        Expr::Symbol(loop_name),
        Expr::List(named_bindings),
        loop_body,
    ]))
}

fn begin_expr(exprs: Vec<Expr>) -> Expr {
    let mut items = vec![Expr::Symbol("begin".into())];
    items.extend(exprs);
    Expr::List(items)
}

fn is_else_clause(expr: &Expr) -> bool {
    matches!(expr, Expr::List(items) if matches!(items.first(), Some(Expr::Symbol(symbol)) if symbol == "else"))
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<(String, Vec<String>), EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::message(
            "define-record-type constructor spec must be a list",
        ));
    };

    let Some((Expr::Symbol(name), fields)) = items.split_first() else {
        return Err(EvalError::message(
            "invalid define-record-type constructor spec",
        ));
    };

    Ok((name.clone(), parse_param_names(fields)?))
}

fn parse_record_fields(field_exprs: &[Expr]) -> Result<Vec<(String, String)>, EvalError> {
    field_exprs
        .iter()
        .map(|field_expr| match field_expr {
            Expr::List(items) => match items.as_slice() {
                [Expr::Symbol(field_name), Expr::Symbol(accessor_name)] => {
                    Ok((field_name.clone(), accessor_name.clone()))
                }
                _ => Err(EvalError::message("invalid define-record-type field spec")),
            },
            _ => Err(EvalError::message("invalid define-record-type field spec")),
        })
        .collect()
}

fn eval_binding_values(bindings: &[(String, Expr)], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| eval(expr, env.clone()))
        .collect()
}

fn quote(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value) => Value::Int(*value),
        Expr::Rational(numerator, denominator) => Value::Rational(*numerator, *denominator),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Char(value) => Value::Char(*value),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => make_proper_list(items.iter().map(quote)),
    }
}

fn make_proper_list(items: impl IntoIterator<Item = Value>) -> Value {
    let mut values = items.into_iter().collect::<Vec<_>>();
    let mut result = Value::EmptyList;
    while let Some(value) = values.pop() {
        result = Value::Pair(Rc::new(Pair {
            car: RefCell::new(value),
            cdr: RefCell::new(result),
        }));
    }
    result
}

impl ExpansionContext {
    fn new(def_env: EnvRef) -> Self {
        Self {
            def_env,
            introduced: HashMap::new(),
        }
    }

    fn introduce_identifier(&mut self, name: &str) -> String {
        if let Some(existing) = self.introduced.get(name) {
            return existing.clone();
        }

        let fresh_name = fresh_symbol(name);
        if let Some(binding) = resolve_syntax(name, &self.def_env) {
            self.def_env.define_syntax(fresh_name.clone(), binding);
        } else if let Some(cell) = self.def_env.lookup_cell(name) {
            self.def_env.define_cell(fresh_name.clone(), cell);
        } else if let Some(builtin) = Builtin::from_name(name) {
            self.def_env
                .define(fresh_name.clone(), Value::Builtin(builtin));
        }

        self.introduced.insert(name.to_string(), fresh_name.clone());
        fresh_name
    }
}

fn fresh_symbol(name: &str) -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__macro_{id}_{name}")
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn type_error(name: &str, expected: &str, value: &Value) -> EvalError {
    EvalError::message(format!(
        "{name} expected {expected}, got {}",
        value_type_name(value)
    ))
}

fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Int(_) | Value::Rational(_, _) => "number",
        Value::Bool(_) => "boolean",
        Value::String(_) => "string",
        Value::Char(_) => "character",
        Value::Symbol(_) => "symbol",
        Value::EmptyList => "null",
        Value::Pair(_) => "pair",
        Value::Vector(_) => "vector",
        Value::Builtin(_)
        | Value::Closure(_)
        | Value::Continuation(_)
        | Value::RecordConstructor(_)
        | Value::RecordPredicate(_)
        | Value::RecordAccessor(_) => "procedure",
        Value::Multi(_) => "values",
        Value::Record(_) => "record",
        Value::Void => "void",
    }
}

fn render(value: &Value) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Rational(numerator, denominator) => format!("{numerator}/{denominator}"),
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::String(text) => format!("\"{}\"", escape_string(text)),
        Value::Char(ch) => render_char(*ch),
        Value::Symbol(name) => name.clone(),
        Value::EmptyList => "()".into(),
        Value::Pair(_) => render_pair(value),
        Value::Vector(vector) => {
            let items = vector.borrow();
            let rendered = items.iter().map(render).collect::<Vec<_>>().join(" ");
            format!("#({rendered})")
        }
        Value::Builtin(_)
        | Value::Closure(_)
        | Value::Continuation(_)
        | Value::RecordConstructor(_)
        | Value::RecordPredicate(_)
        | Value::RecordAccessor(_) => "#<procedure>".into(),
        Value::Multi(_) => "#<values>".into(),
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Void => String::new(),
    }
}

fn render_pair(value: &Value) -> String {
    let mut output = String::from("(");
    let mut current = value.clone();
    let mut first = true;

    loop {
        match current {
            Value::Pair(pair) => {
                if !first {
                    output.push(' ');
                }
                output.push_str(&render(&pair.car.borrow()));

                let cdr = pair.cdr.borrow().clone();
                match cdr {
                    Value::EmptyList => {
                        output.push(')');
                        return output;
                    }
                    Value::Pair(_) => {
                        current = cdr;
                        first = false;
                    }
                    other => {
                        output.push_str(" . ");
                        output.push_str(&render(&other));
                        output.push(')');
                        return output;
                    }
                }
            }
            _ => unreachable!("render_pair called with non-pair value"),
        }
    }
}

fn render_char(ch: char) -> String {
    match ch {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn escape_string(text: &str) -> String {
    let mut output = String::new();
    for ch in text.chars() {
        match ch {
            '\\' => output.push_str("\\\\"),
            '"' => output.push_str("\\\""),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            _ => output.push(ch),
        }
    }
    output
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let Some(ch) = self.peek() else {
            return Err(EvalError::message("unexpected end of input"));
        };

        match ch {
            '(' => self.parse_list(),
            ')' => Err(EvalError::message("unexpected ')'")),
            '"' => self.parse_string(),
            '\'' => {
                self.advance();
                Ok(Expr::List(vec![
                    Expr::Symbol("quote".into()),
                    self.parse_expr()?,
                ]))
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek() {
                Some(')') => {
                    self.advance();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::message("unterminated list")),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect('"')?;
        let mut value = String::new();

        loop {
            let Some(ch) = self.peek() else {
                return Err(EvalError::message("unterminated string literal"));
            };

            self.advance();
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let Some(escaped) = self.peek() else {
                        return Err(EvalError::message("unterminated escape sequence"));
                    };
                    self.advance();
                    value.push(match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            token.push(ch);
            self.advance();
        }

        if token.is_empty() {
            return Err(EvalError::message("expected expression"));
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ if token.starts_with("#\\") => parse_char_token(&token),
            _ if is_rational_token(&token) => parse_rational_token(&token),
            _ if is_integer_token(&token) => {
                Ok(Expr::Int(token.parse().map_err(|_| {
                    EvalError::message(format!("invalid integer literal: {token}"))
                })?))
            }
            _ => Ok(Expr::Symbol(token)),
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.advance();
            }

            if self.peek() == Some(';') {
                while !self.at_end() && self.peek() != Some('\n') {
                    self.advance();
                }
                continue;
            }

            break;
        }
    }

    fn expect(&mut self, expected: char) -> Result<(), EvalError> {
        match self.peek() {
            Some(ch) if ch == expected => {
                self.advance();
                Ok(())
            }
            Some(ch) => Err(EvalError::message(format!(
                "expected '{expected}', got '{ch}'",
            ))),
            None => Err(EvalError::message(format!(
                "expected '{expected}', got end of input",
            ))),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}

fn is_rational_token(token: &str) -> bool {
    let Some((left, right)) = token.split_once('/') else {
        return false;
    };
    is_integer_token(left) && !right.starts_with(['+', '-']) && is_integer_token(right)
}

fn is_integer_token(token: &str) -> bool {
    let digits = match token.chars().next() {
        Some('+') | Some('-') if token.len() > 1 => &token[1..],
        _ => token,
    };

    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}

fn parse_rational_token(token: &str) -> Result<Expr, EvalError> {
    let Some((left, right)) = token.split_once('/') else {
        return Err(EvalError::message(format!("invalid rational literal: {token}")));
    };
    let numerator: i128 = left
        .parse()
        .map_err(|_| EvalError::message(format!("invalid rational literal: {token}")))?;
    let denominator: i128 = right
        .parse()
        .map_err(|_| EvalError::message(format!("invalid rational literal: {token}")))?;
    if denominator == 0 {
        return Err(EvalError::message(format!("invalid rational literal: {token}")));
    }

    match normalize_number(numerator, denominator) {
        Number::Int(value) => Ok(Expr::Int(value)),
        Number::Rational(numerator, denominator) => Ok(Expr::Rational(numerator, denominator)),
    }
}

fn parse_char_token(token: &str) -> Result<Expr, EvalError> {
    let value = &token[2..];
    let ch = match value {
        "space" => ' ',
        "newline" => '\n',
        _ => {
            let mut chars = value.chars();
            let Some(ch) = chars.next() else {
                return Err(EvalError::message(format!("invalid character literal: {token}")));
            };
            if chars.next().is_some() {
                return Err(EvalError::message(format!("invalid character literal: {token}")));
            }
            ch
        }
    };
    Ok(Expr::Char(ch))
}
