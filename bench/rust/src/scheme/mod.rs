pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
struct SourceLoc {
    line: usize,
    col: usize,
}

#[derive(Clone)]
struct Identifier {
    name: String,
    unique: Option<u64>,
    captured: Option<BindingCell>,
    introduced: bool,
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Identifier")
            .field("name", &self.name)
            .field("unique", &self.unique)
            .field("introduced", &self.introduced)
            .finish()
    }
}

impl Identifier {
    fn plain(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            unique: None,
            captured: None,
            introduced: false,
        }
    }

    fn introduced(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            unique: None,
            captured: None,
            introduced: true,
        }
    }

    fn captured(name: impl Into<String>, cell: BindingCell) -> Self {
        Self {
            name: name.into(),
            unique: None,
            captured: Some(cell),
            introduced: false,
        }
    }

    fn fresh(name: impl Into<String>, unique: u64) -> Self {
        Self {
            name: name.into(),
            unique: Some(unique),
            captured: None,
            introduced: false,
        }
    }

    fn env_key(&self) -> String {
        match self.unique {
            Some(unique) => format!("#{unique}:{}", self.name),
            None => self.name.clone(),
        }
    }

    fn display_name(&self) -> &str {
        &self.name
    }

    fn is_plain(&self) -> bool {
        self.unique.is_none() && self.captured.is_none()
    }
}

#[derive(Clone, Debug)]
struct Expr {
    kind: ExprKind,
    loc: SourceLoc,
}

#[derive(Clone, Debug)]
enum ExprKind {
    Number(f64),
    Boolean(bool),
    String(String),
    Symbol(Identifier),
    List(Vec<Expr>),
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

struct VectorValue {
    elements: RefCell<Vec<Value>>,
}

#[derive(Clone, Copy)]
struct BuiltinProcedure {
    name: &'static str,
    call: BuiltinFn,
}

type BuiltinFn = fn(&[EvaluatedArg], SourceLoc) -> Result<Value, EvalError>;

#[derive(Clone)]
struct ClosureProcedure {
    name: Option<String>,
    params: Vec<Identifier>,
    body: Vec<Expr>,
    env: Environment,
}

#[derive(Clone)]
struct RecordType {
    name: String,
    field_count: usize,
    type_id: u64,
}

struct RecordValue {
    type_info: Rc<RecordType>,
    fields: RefCell<Vec<Value>>,
}

#[derive(Clone)]
struct NativeProcedure {
    name: String,
    kind: NativeProcedureKind,
}

#[derive(Clone)]
enum NativeProcedureKind {
    Map,
    RecordConstructor(Rc<RecordType>),
    RecordPredicate(Rc<RecordType>),
    RecordAccessor {
        type_info: Rc<RecordType>,
        field_index: usize,
    },
    RecordMutator {
        type_info: Rc<RecordType>,
        field_index: usize,
    },
}

#[derive(Clone)]
enum ProcedureValue {
    Builtin(BuiltinProcedure),
    Closure(Rc<ClosureProcedure>),
    Native(Rc<NativeProcedure>),
}

#[derive(Clone)]
enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Vector(Rc<VectorValue>),
    Void,
    Procedure(ProcedureValue),
    Record(Rc<RecordValue>),
}

type BindingCell = Rc<RefCell<Value>>;

#[derive(Clone)]
struct EvaluatedArg {
    expr: Expr,
    value: Value,
}

struct LetBinding {
    name: Identifier,
    value_expr: Expr,
}

struct DoBinding {
    name: Identifier,
    init_expr: Expr,
    step_expr: Option<Expr>,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
struct MacroTransformer {
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    definition_env: Environment,
}

struct EvalState {
    syntax_rules: HashMap<String, MacroTransformer>,
    next_unique: u64,
}

impl EvalState {
    fn new() -> Self {
        Self {
            syntax_rules: HashMap::new(),
            next_unique: 1,
        }
    }

    fn fresh_unique_id(&mut self) -> u64 {
        let unique = self.next_unique;
        self.next_unique += 1;
        unique
    }

    fn fresh_identifier(&mut self, name: &str) -> Identifier {
        Identifier::fresh(name.to_string(), self.fresh_unique_id())
    }
}

#[derive(Clone)]
enum MacroBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

#[derive(Clone, Default)]
struct MacroBindings {
    values: HashMap<String, MacroBinding>,
}

#[derive(Clone)]
struct Environment {
    inner: Rc<EnvironmentInner>,
}

struct EnvironmentInner {
    parent: Option<Environment>,
    bindings: RefCell<HashMap<String, BindingCell>>,
}

impl Environment {
    fn new(parent: Option<Environment>) -> Self {
        Self {
            inner: Rc::new(EnvironmentInner {
                parent,
                bindings: RefCell::new(HashMap::new()),
            }),
        }
    }

    fn child(parent: &Environment) -> Self {
        Self::new(Some(parent.clone()))
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.define_cell(name.into(), Rc::new(RefCell::new(value)));
    }

    fn define_identifier(&self, identifier: &Identifier, value: Value) {
        self.define(identifier.env_key(), value);
    }

    fn define_cell(&self, name: String, cell: BindingCell) {
        self.inner.bindings.borrow_mut().insert(name, cell);
    }

    fn lookup_identifier(
        &self,
        identifier: &Identifier,
        loc: SourceLoc,
    ) -> Result<Value, EvalError> {
        if let Some(cell) = &identifier.captured {
            return Ok(cell.borrow().clone());
        }

        let key = identifier.env_key();
        if let Some(cell) = self.lookup_cell(&key) {
            return Ok(cell.borrow().clone());
        }

        Err(err_at(
            loc,
            format!("unbound variable {}", identifier.display_name()),
        ))
    }

    fn lookup_cell(&self, name: &str) -> Option<BindingCell> {
        if let Some(cell) = self.inner.bindings.borrow().get(name) {
            return Some(cell.clone());
        }

        self.inner
            .parent
            .as_ref()
            .and_then(|parent| parent.lookup_cell(name))
    }

    fn lookup_plain_cell(&self, name: &str) -> Option<BindingCell> {
        if let Some(cell) = self.inner.bindings.borrow().get(name) {
            return Some(cell.clone());
        }

        self.inner
            .parent
            .as_ref()
            .and_then(|parent| parent.lookup_plain_cell(name))
    }

    fn set_identifier(
        &self,
        identifier: &Identifier,
        value: Value,
        loc: SourceLoc,
    ) -> Result<(), EvalError> {
        if let Some(cell) = &identifier.captured {
            *cell.borrow_mut() = value;
            return Ok(());
        }

        let key = identifier.env_key();
        let Some(cell) = self.lookup_cell(&key) else {
            return Err(err_at(
                loc,
                format!("unbound variable {}", identifier.display_name()),
            ));
        };

        *cell.borrow_mut() = value;
        Ok(())
    }
}

struct Reader {
    chars: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
}

impl Reader {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_whitespace_and_comments();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_whitespace_and_comments();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();

        if self.is_eof() {
            return Err(self.raise("unexpected end of input", None));
        }

        let loc = self.current_loc();
        match self.peek() {
            Some('(') => self.parse_list(loc),
            Some('\'') => self.parse_quoted(loc),
            Some(')') => Err(self.raise("unexpected )", Some(loc))),
            Some('"') => self.parse_string(loc),
            Some(_) => self.parse_atom(loc),
            None => Err(self.raise("unexpected end of input", Some(loc))),
        }
    }

    fn parse_list(&mut self, loc: SourceLoc) -> Result<Expr, EvalError> {
        self.advance();

        let mut elements = Vec::new();
        self.skip_whitespace_and_comments();

        while !self.is_eof() && self.peek() != Some(')') {
            elements.push(self.parse_expr()?);
            self.skip_whitespace_and_comments();
        }

        if self.is_eof() {
            return Err(self.raise("unterminated list", Some(loc)));
        }

        self.advance();
        Ok(Expr {
            kind: ExprKind::List(elements),
            loc,
        })
    }

    fn parse_quoted(&mut self, loc: SourceLoc) -> Result<Expr, EvalError> {
        self.advance();

        Ok(Expr {
            kind: ExprKind::List(vec![
                Expr {
                    kind: ExprKind::Symbol(Identifier::plain("quote")),
                    loc,
                },
                self.parse_expr()?,
            ]),
            loc,
        })
    }

    fn parse_string(&mut self, loc: SourceLoc) -> Result<Expr, EvalError> {
        self.advance();

        let mut value = String::new();
        let mut terminated = false;

        while let Some(ch) = self.advance() {
            if ch == '"' {
                terminated = true;
                break;
            }

            if ch == '\\' {
                let escaped = self
                    .advance()
                    .ok_or_else(|| self.raise("unterminated string escape", Some(loc)))?;

                match escaped {
                    '"' => value.push('"'),
                    '\\' => value.push('\\'),
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    _ => {
                        return Err(self.raise(format!("unsupported escape \\{escaped}"), Some(loc)))
                    }
                }

                continue;
            }

            value.push(ch);
        }

        if !terminated {
            return Err(self.raise("unterminated string literal", Some(loc)));
        }

        Ok(Expr {
            kind: ExprKind::String(value),
            loc,
        })
    }

    fn parse_atom(&mut self, loc: SourceLoc) -> Result<Expr, EvalError> {
        let mut token = String::new();

        while let Some(ch) = self.peek() {
            if is_whitespace(ch) || matches!(ch, '(' | ')' | '\'' | ';') {
                break;
            }

            token.push(self.advance().expect("peeked character should exist"));
        }

        if token.is_empty() {
            return Err(self.raise("expected expression", Some(loc)));
        }

        let kind = if token == "#t" {
            ExprKind::Boolean(true)
        } else if token == "#f" {
            ExprKind::Boolean(false)
        } else if let Ok(number) = token.parse::<i64>() {
            ExprKind::Number(number as f64)
        } else {
            ExprKind::Symbol(Identifier::plain(token))
        };

        Ok(Expr { kind, loc })
    }

    fn skip_whitespace_and_comments(&mut self) {
        while let Some(ch) = self.peek() {
            if is_whitespace(ch) {
                self.advance();
                continue;
            }

            if ch == ';' {
                while let Some(current) = self.peek() {
                    if current == '\n' {
                        break;
                    }
                    self.advance();
                }
                continue;
            }

            break;
        }
    }

    fn current_loc(&self) -> SourceLoc {
        SourceLoc {
            line: self.line,
            col: self.col,
        }
    }

    fn is_eof(&self) -> bool {
        self.index >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.index).copied()?;
        self.index += 1;

        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        Some(ch)
    }

    fn raise(&self, message: impl Into<String>, loc: Option<SourceLoc>) -> EvalError {
        let loc = loc.unwrap_or_else(|| self.current_loc());
        err_at(loc, message)
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
    let program = Reader::new(input).parse_program()?;

    if program.is_empty() {
        return Err(EvalError::at(1, 1, "expected expression"));
    }

    let env = create_global_env();
    let mut state = EvalState::new();
    let mut result = Value::Void;

    for expr in &program {
        result = evaluate(expr, &env, &mut state)?;
    }

    Ok(format_value(&result))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn create_global_env() -> Environment {
    let env = Environment::new(None);

    env.define("+", builtin("+", builtin_plus));
    env.define("*", builtin("*", builtin_mul));
    env.define("-", builtin("-", builtin_minus));
    env.define("/", builtin("/", builtin_div));

    env.define("<", builtin("<", builtin_lt));
    env.define(">", builtin(">", builtin_gt));
    env.define("=", builtin("=", builtin_num_eq));
    env.define("<=", builtin("<=", builtin_lte));
    env.define("zero?", builtin("zero?", builtin_zero_pred));

    env.define("not", builtin("not", builtin_not));

    env.define("cons", builtin("cons", builtin_cons));
    env.define("car", builtin("car", builtin_car));
    env.define("cdr", builtin("cdr", builtin_cdr));
    env.define("null?", builtin("null?", builtin_null_pred));
    env.define("list", builtin("list", builtin_list));
    env.define("length", builtin("length", builtin_length));
    env.define("append", builtin("append", builtin_append));
    env.define("map", native("map", NativeProcedureKind::Map));
    env.define("eq?", builtin("eq?", builtin_eq));
    env.define("eqv?", builtin("eqv?", builtin_eqv));
    env.define("equal?", builtin("equal?", builtin_equal));
    env.define("vector", builtin("vector", builtin_vector));
    env.define("make-vector", builtin("make-vector", builtin_make_vector));
    env.define("vector-ref", builtin("vector-ref", builtin_vector_ref));
    env.define("vector-set!", builtin("vector-set!", builtin_vector_set));
    env.define("vector-length", builtin("vector-length", builtin_vector_length));
    env.define("vector?", builtin("vector?", builtin_vector_pred));
    env.define("vector->list", builtin("vector->list", builtin_vector_to_list));
    env.define("list->vector", builtin("list->vector", builtin_list_to_vector));

    env.define("string?", builtin("string?", builtin_string_pred));
    env.define("number?", builtin("number?", builtin_number_pred));
    env.define("boolean?", builtin("boolean?", builtin_boolean_pred));
    env.define("pair?", builtin("pair?", builtin_pair_pred));
    env.define("symbol?", builtin("symbol?", builtin_symbol_pred));

    env
}

fn builtin(name: &'static str, call: BuiltinFn) -> Value {
    Value::Procedure(ProcedureValue::Builtin(BuiltinProcedure { name, call }))
}

fn native(name: impl Into<String>, kind: NativeProcedureKind) -> Value {
    Value::Procedure(ProcedureValue::Native(Rc::new(NativeProcedure {
        name: name.into(),
        kind,
    })))
}

fn evaluate(expr: &Expr, env: &Environment, state: &mut EvalState) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(identifier) => env.lookup_identifier(identifier, expr.loc),
        ExprKind::List(_) => evaluate_list(expr, env, state),
    }
}

fn evaluate_list(
    expr: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    let elements = expr_list(expr).expect("list expression expected");

    if elements.is_empty() {
        return Err(err_at(expr.loc, "cannot evaluate empty list"));
    }

    let head = &elements[0];
    let args = &elements[1..];

    if let Some(symbol) = expr_plain_symbol(head) {
        if symbol == "define-syntax" {
            return eval_define_syntax(args, head, env, state);
        }

        if let Some(transformer) = state.syntax_rules.get(symbol).cloned() {
            let expanded = expand_macro(&transformer, expr, state)?;
            return evaluate(&expanded, env, state);
        }

        match symbol {
            "define" => return eval_define(args, head, env, state),
            "define-record-type" => return eval_define_record_type(args, head, env, state),
            "set!" => return eval_set(args, head, env, state),
            "if" => return eval_if(args, head, env, state),
            "quote" => return eval_quote(args, head),
            "lambda" => return eval_lambda(args, head, env),
            "and" => return eval_and(args, env, state),
            "or" => return eval_or(args, env, state),
            "begin" => return eval_begin(args, env, state),
            "cond" => return eval_cond(args, head, env, state),
            "let" => return eval_let(args, head, env, state),
            "letrec" => return eval_letrec(args, head, env, state, false),
            "letrec*" => return eval_letrec(args, head, env, state, true),
            "case" => return eval_case(args, head, env, state),
            "do" => return eval_do(args, head, env, state),
            _ => {}
        }
    }

    let operator = evaluate(head, env, state)?;
    let evaluated_args = args
        .iter()
        .map(|arg| {
            Ok(EvaluatedArg {
                expr: arg.clone(),
                value: evaluate(arg, env, state)?,
            })
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    apply_procedure(operator, &evaluated_args, head.loc, state)
}

fn eval_define_syntax(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(err_at(
            head.loc,
            "define-syntax expects exactly 2 arguments",
        ));
    }

    let name_expr = &args[0];
    let Some(name) = expr_symbol_name(name_expr) else {
        return Err(err_at(name_expr.loc, "macro name must be a symbol"));
    };

    let transformer = parse_macro_transformer(&args[1], env)?;
    state.syntax_rules.insert(name.to_string(), transformer);
    Ok(Value::Void)
}

fn eval_define(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "define expects a name and value"));
    }

    let target = &args[0];

    if let Some(name) = expr_identifier(target) {
        if args.len() != 2 {
            return Err(err_at(head.loc, "define expects exactly 2 arguments"));
        }

        env.define_identifier(name, evaluate(&args[1], env, state)?);
        return Ok(Value::Void);
    }

    let Some(target_elements) = expr_list(target) else {
        return Err(err_at(head.loc, "invalid define target"));
    };

    if target_elements.is_empty() {
        return Err(err_at(head.loc, "invalid define target"));
    }

    let name_expr = &target_elements[0];
    let Some(name) = expr_identifier(name_expr) else {
        return Err(err_at(name_expr.loc, "function name must be a symbol"));
    };

    let params = target_elements[1..]
        .iter()
        .map(expect_parameter_identifier)
        .collect::<Result<Vec<_>, EvalError>>()?;

    let procedure = Value::Procedure(ProcedureValue::Closure(Rc::new(ClosureProcedure {
        name: Some(name.display_name().to_string()),
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })));

    env.define_identifier(name, procedure);
    Ok(Value::Void)
}

fn eval_define_record_type(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(err_at(
            head.loc,
            "define-record-type expects a type name, constructor, predicate, and fields",
        ));
    }

    let Some(type_name) = expr_symbol_name(&args[0]) else {
        return Err(err_at(args[0].loc, "record type name must be a symbol"));
    };

    let constructor_expr = &args[1];
    let Some(constructor_parts) = expr_list(constructor_expr) else {
        return Err(err_at(
            constructor_expr.loc,
            "record constructor must be a list",
        ));
    };

    if constructor_parts.is_empty() {
        return Err(err_at(
            constructor_expr.loc,
            "record constructor must be a non-empty list",
        ));
    }

    let constructor_name = expect_bindable_identifier(
        &constructor_parts[0],
        "record constructor name must be a symbol",
    )?;
    let constructor_fields = constructor_parts[1..]
        .iter()
        .map(|expr| {
            expect_bindable_identifier(expr, "record constructor field name must be a symbol")
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    let predicate_name =
        expect_bindable_identifier(&args[2], "record predicate name must be a symbol")?;

    let mut accessors = Vec::new();
    let mut mutators = Vec::new();

    for field_expr in &args[3..] {
        let Some(field_parts) = expr_list(field_expr) else {
            return Err(err_at(field_expr.loc, "record field must be a list"));
        };

        if !(2..=3).contains(&field_parts.len()) {
            return Err(err_at(
                field_expr.loc,
                "record field must contain a name, accessor, and optional mutator",
            ));
        }

        expect_bindable_identifier(&field_parts[0], "record field name must be a symbol")?;
        accessors.push(expect_bindable_identifier(
            &field_parts[1],
            "record accessor name must be a symbol",
        )?);

        if field_parts.len() == 3 {
            mutators.push(Some(expect_bindable_identifier(
                &field_parts[2],
                "record mutator name must be a symbol",
            )?));
        } else {
            mutators.push(None);
        }
    }

    if constructor_fields.len() != accessors.len() {
        return Err(err_at(
            head.loc,
            "record constructor and field definitions must have the same arity",
        ));
    }

    let type_info = Rc::new(RecordType {
        name: type_name.to_string(),
        field_count: accessors.len(),
        type_id: state.fresh_unique_id(),
    });

    env.define_identifier(
        &constructor_name,
        native(
            constructor_name.display_name().to_string(),
            NativeProcedureKind::RecordConstructor(type_info.clone()),
        ),
    );
    env.define_identifier(
        &predicate_name,
        native(
            predicate_name.display_name().to_string(),
            NativeProcedureKind::RecordPredicate(type_info.clone()),
        ),
    );

    for (field_index, accessor_name) in accessors.iter().enumerate() {
        env.define_identifier(
            accessor_name,
            native(
                accessor_name.display_name().to_string(),
                NativeProcedureKind::RecordAccessor {
                    type_info: type_info.clone(),
                    field_index,
                },
            ),
        );

        if let Some(mutator_name) = &mutators[field_index] {
            env.define_identifier(
                mutator_name,
                native(
                    mutator_name.display_name().to_string(),
                    NativeProcedureKind::RecordMutator {
                        type_info: type_info.clone(),
                        field_index,
                    },
                ),
            );
        }
    }

    Ok(Value::Void)
}

fn eval_set(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(err_at(head.loc, "set! expects exactly 2 arguments"));
    }

    let target = &args[0];
    let Some(identifier) = expr_identifier(target) else {
        return Err(err_at(target.loc, "set! target must be a symbol"));
    };

    let value = evaluate(&args[1], env, state)?;
    env.set_identifier(identifier, value, target.loc)?;
    Ok(Value::Void)
}

fn eval_if(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(err_at(head.loc, "if expects exactly 2 or 3 arguments"));
    }

    if is_truthy(&evaluate(&args[0], env, state)?) {
        evaluate(&args[1], env, state)
    } else if args.len() == 3 {
        evaluate(&args[2], env, state)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Expr], head: &Expr) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(err_at(head.loc, "quote expects exactly 1 argument"));
    }

    Ok(quote_expr(&args[0]))
}

fn eval_lambda(args: &[Expr], head: &Expr, env: &Environment) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "lambda expects parameters and a body"));
    }

    let params_expr = &args[0];
    let Some(params_list) = expr_list(params_expr) else {
        return Err(err_at(params_expr.loc, "lambda parameters must be a list"));
    };

    let params = params_list
        .iter()
        .map(expect_parameter_identifier)
        .collect::<Result<Vec<_>, EvalError>>()?;

    Ok(Value::Procedure(ProcedureValue::Closure(Rc::new(
        ClosureProcedure {
            name: None,
            params,
            body: args[1..].to_vec(),
            env: env.clone(),
        },
    ))))
}

fn eval_and(args: &[Expr], env: &Environment, state: &mut EvalState) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);

    for arg in args {
        result = evaluate(arg, env, state)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_or(args: &[Expr], env: &Environment, state: &mut EvalState) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);

    for arg in args {
        result = evaluate(arg, env, state)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Environment, state: &mut EvalState) -> Result<Value, EvalError> {
    evaluate_sequence(args, env, state)
}

fn eval_cond(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Some(clause_elements) = expr_list(clause) else {
            return Err(err_at(head.loc, "cond clauses must be non-empty lists"));
        };

        if clause_elements.is_empty() {
            return Err(err_at(head.loc, "cond clauses must be non-empty lists"));
        }

        let test_expr = &clause_elements[0];
        let body = &clause_elements[1..];
        let is_else_clause = matches!(expr_plain_symbol(test_expr), Some("else"));

        if is_else_clause {
            if index != args.len() - 1 {
                return Err(err_at(test_expr.loc, "else must be the last cond clause"));
            }

            return evaluate_sequence(body, env, state);
        }

        let test_value = evaluate(test_expr, env, state)?;
        if is_truthy(&test_value) {
            if body.is_empty() {
                return Ok(test_value);
            }

            return evaluate_sequence(body, env, state);
        }
    }

    Ok(Value::Void)
}

fn eval_let(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "let expects bindings and a body"));
    }

    if matches!(args[0].kind, ExprKind::Symbol(_)) {
        return eval_named_let(args, head, env, state);
    }

    let bindings = parse_let_bindings(&args[0], head.loc)?;
    let let_env = Environment::child(env);

    for binding in &bindings {
        let value = evaluate(&binding.value_expr, env, state)?;
        let_env.define_identifier(&binding.name, value);
    }

    evaluate_sequence(&args[1..], &let_env, state)
}

fn eval_letrec(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
    sequential: bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "letrec expects bindings and a body"));
    }

    let bindings = parse_let_bindings(&args[0], head.loc)?;
    let let_env = Environment::child(env);

    for binding in &bindings {
        let_env.define_identifier(&binding.name, Value::Void);
    }

    if sequential {
        for binding in &bindings {
            let value = evaluate(&binding.value_expr, &let_env, state)?;
            let_env.set_identifier(&binding.name, value, binding.value_expr.loc)?;
        }
    } else {
        let values = bindings
            .iter()
            .map(|binding| evaluate(&binding.value_expr, &let_env, state))
            .collect::<Result<Vec<_>, EvalError>>()?;

        for (binding, value) in bindings.iter().zip(values) {
            let_env.set_identifier(&binding.name, value, binding.value_expr.loc)?;
        }
    }

    evaluate_sequence(&args[1..], &let_env, state)
}

fn eval_case(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(err_at(head.loc, "case expects a key and at least 1 clause"));
    }

    let key = evaluate(&args[0], env, state)?;

    for (index, clause) in args[1..].iter().enumerate() {
        let Some(parts) = expr_list(clause) else {
            return Err(err_at(head.loc, "case clauses must be non-empty lists"));
        };

        if parts.is_empty() {
            return Err(err_at(head.loc, "case clauses must be non-empty lists"));
        }

        let datums_expr = &parts[0];
        let body = &parts[1..];

        if matches!(expr_plain_symbol(datums_expr), Some("else")) {
            if index != args.len() - 2 {
                return Err(err_at(datums_expr.loc, "else must be the last case clause"));
            }

            return evaluate_sequence(body, env, state);
        }

        let Some(datums) = expr_list(datums_expr) else {
            return Err(err_at(datums_expr.loc, "case clause datums must be a list"));
        };

        if datums
            .iter()
            .any(|datum| is_eqv(&key, &quote_expr(datum)))
        {
            return evaluate_sequence(body, env, state);
        }
    }

    Ok(Value::Void)
}

fn eval_named_let(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(err_at(
            head.loc,
            "named let expects a name, bindings, and a body",
        ));
    }

    let name_expr = &args[0];
    let Some(name) = expr_identifier(name_expr) else {
        return Err(err_at(name_expr.loc, "named let name must be a symbol"));
    };

    let bindings = parse_let_bindings(&args[1], head.loc)?;
    let evaluated_args = bindings
        .iter()
        .map(|binding| {
            Ok(EvaluatedArg {
                expr: binding.value_expr.clone(),
                value: evaluate(&binding.value_expr, env, state)?,
            })
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    let let_env = Environment::child(env);
    let procedure = Value::Procedure(ProcedureValue::Closure(Rc::new(ClosureProcedure {
        name: Some(name.display_name().to_string()),
        params: bindings
            .iter()
            .map(|binding| binding.name.clone())
            .collect(),
        body: args[2..].to_vec(),
        env: let_env.clone(),
    })));

    let_env.define_identifier(name, procedure.clone());
    apply_procedure(procedure, &evaluated_args, name_expr.loc, state)
}

fn eval_do(
    args: &[Expr],
    head: &Expr,
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "do expects bindings and a test clause"));
    }

    let bindings = parse_do_bindings(&args[0], head.loc)?;
    let Some(test_clause) = expr_list(&args[1]) else {
        return Err(err_at(args[1].loc, "do test clause must be a list"));
    };

    if test_clause.is_empty() {
        return Err(err_at(args[1].loc, "do test clause must be non-empty"));
    }

    let loop_env = Environment::child(env);
    for binding in &bindings {
        let value = evaluate(&binding.init_expr, env, state)?;
        loop_env.define_identifier(&binding.name, value);
    }

    let test_expr = &test_clause[0];
    let result_exprs = &test_clause[1..];
    let body = &args[2..];

    loop {
        if is_truthy(&evaluate(test_expr, &loop_env, state)?) {
            return evaluate_sequence(result_exprs, &loop_env, state);
        }

        evaluate_sequence(body, &loop_env, state)?;

        let next_values = bindings
            .iter()
            .map(|binding| {
                if let Some(step_expr) = &binding.step_expr {
                    evaluate(step_expr, &loop_env, state)
                } else {
                    loop_env.lookup_identifier(&binding.name, binding.init_expr.loc)
                }
            })
            .collect::<Result<Vec<_>, EvalError>>()?;

        for (binding, value) in bindings.iter().zip(next_values) {
            loop_env.set_identifier(&binding.name, value, binding.init_expr.loc)?;
        }
    }
}

fn apply_procedure(
    operator: Value,
    args: &[EvaluatedArg],
    loc: SourceLoc,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(err_at(loc, "not a procedure"));
    };

    match procedure {
        ProcedureValue::Builtin(procedure) => (procedure.call)(args, loc),
        ProcedureValue::Native(procedure) => apply_native_procedure(&procedure, args, loc, state),
        ProcedureValue::Closure(procedure) => {
            if args.len() != procedure.params.len() {
                return Err(err_at(
                    loc,
                    format!(
                        "{} expects exactly {} arguments",
                        procedure_display_name(&procedure),
                        procedure.params.len()
                    ),
                ));
            }

            let call_env = Environment::child(&procedure.env);
            for (param, arg) in procedure.params.iter().zip(args.iter()) {
                call_env.define_identifier(param, arg.value.clone());
            }

            evaluate_sequence(&procedure.body, &call_env, state)
        }
    }
}

fn apply_native_procedure(
    procedure: &NativeProcedure,
    args: &[EvaluatedArg],
    loc: SourceLoc,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    match &procedure.kind {
        NativeProcedureKind::Map => {
            if args.len() < 2 {
                return Err(err_at(loc, "map expects a procedure and at least 1 list"));
            }

            let operator = args[0].value.clone();
            let list_args = &args[1..];
            let lists = list_args
                .iter()
                .map(|arg| expect_proper_list(&arg.value, arg.expr.loc))
                .collect::<Result<Vec<_>, EvalError>>()?;

            let expected_len = lists[0].len();
            for (index, list) in lists.iter().enumerate().skip(1) {
                if list.len() != expected_len {
                    return Err(err_at(
                        list_args[index].expr.loc,
                        "map lists must have the same length",
                    ));
                }
            }

            let mut results = Vec::with_capacity(expected_len);
            for item_index in 0..expected_len {
                let applied_args = list_args
                    .iter()
                    .enumerate()
                    .map(|(list_index, arg)| EvaluatedArg {
                        expr: arg.expr.clone(),
                        value: lists[list_index][item_index].clone(),
                    })
                    .collect::<Vec<_>>();

                results.push(apply_procedure(
                    operator.clone(),
                    &applied_args,
                    args[0].expr.loc,
                    state,
                )?);
            }

            Ok(make_list(results))
        }
        NativeProcedureKind::RecordConstructor(type_info) => {
            expect_exact_args(&procedure.name, args, loc, type_info.field_count)?;
            Ok(Value::Record(Rc::new(RecordValue {
                type_info: type_info.clone(),
                fields: RefCell::new(args.iter().map(|arg| arg.value.clone()).collect()),
            })))
        }
        NativeProcedureKind::RecordPredicate(type_info) => {
            expect_exact_args(&procedure.name, args, loc, 1)?;
            Ok(Value::Boolean(matches!(
                &args[0].value,
                Value::Record(record) if record.type_info.type_id == type_info.type_id
            )))
        }
        NativeProcedureKind::RecordAccessor {
            type_info,
            field_index,
        } => {
            expect_exact_args(&procedure.name, args, loc, 1)?;
            let Value::Record(record) = &args[0].value else {
                return Err(err_at(
                    args[0].expr.loc,
                    format!("expected {}", type_info.name),
                ));
            };

            if record.type_info.type_id != type_info.type_id {
                return Err(err_at(
                    args[0].expr.loc,
                    format!("expected {}", type_info.name),
                ));
            }

            Ok(record.fields.borrow()[*field_index].clone())
        }
        NativeProcedureKind::RecordMutator {
            type_info,
            field_index,
        } => {
            expect_exact_args(&procedure.name, args, loc, 2)?;
            let Value::Record(record) = &args[0].value else {
                return Err(err_at(
                    args[0].expr.loc,
                    format!("expected {}", type_info.name),
                ));
            };

            if record.type_info.type_id != type_info.type_id {
                return Err(err_at(
                    args[0].expr.loc,
                    format!("expected {}", type_info.name),
                ));
            }

            record.fields.borrow_mut()[*field_index] = args[1].value.clone();
            Ok(Value::Void)
        }
    }
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Number(value) => Value::Number(*value),
        ExprKind::Boolean(value) => Value::Boolean(*value),
        ExprKind::String(value) => Value::String(value.clone()),
        ExprKind::Symbol(value) => Value::Symbol(value.display_name().to_string()),
        ExprKind::List(elements) => quote_list(elements),
    }
}

fn quote_list(elements: &[Expr]) -> Value {
    make_list(elements.iter().map(quote_expr).collect())
}

fn parse_let_bindings(expr: &Expr, head_loc: SourceLoc) -> Result<Vec<LetBinding>, EvalError> {
    let Some(bindings) = expr_list(expr) else {
        return Err(err_at(expr.loc, "let bindings must be a list"));
    };

    bindings
        .iter()
        .map(|binding_expr| {
            let Some(parts) = expr_list(binding_expr) else {
                return Err(err_at(head_loc, "let bindings must be pairs"));
            };

            if parts.len() != 2 {
                return Err(err_at(head_loc, "let bindings must be pairs"));
            }

            let name_expr = &parts[0];
            let Some(name) = expr_identifier(name_expr) else {
                return Err(err_at(name_expr.loc, "let binding name must be a symbol"));
            };

            Ok(LetBinding {
                name: name.clone(),
                value_expr: parts[1].clone(),
            })
        })
        .collect()
}

fn parse_do_bindings(expr: &Expr, head_loc: SourceLoc) -> Result<Vec<DoBinding>, EvalError> {
    let Some(bindings) = expr_list(expr) else {
        return Err(err_at(expr.loc, "do bindings must be a list"));
    };

    bindings
        .iter()
        .map(|binding_expr| {
            let Some(parts) = expr_list(binding_expr) else {
                return Err(err_at(head_loc, "do bindings must be pairs or triples"));
            };

            if !(2..=3).contains(&parts.len()) {
                return Err(err_at(head_loc, "do bindings must be pairs or triples"));
            }

            let name = expect_bindable_identifier(&parts[0], "do binding name must be a symbol")?;
            Ok(DoBinding {
                name,
                init_expr: parts[1].clone(),
                step_expr: parts.get(2).cloned(),
            })
        })
        .collect()
}

fn evaluate_sequence(
    exprs: &[Expr],
    env: &Environment,
    state: &mut EvalState,
) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = evaluate(expr, env, state)?;
    }

    Ok(result)
}

fn make_list(elements: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;

    for element in elements.into_iter().rev() {
        result = Value::Pair(Rc::new(PairValue {
            car: element,
            cdr: result,
        }));
    }

    result
}

fn parse_macro_transformer(
    expr: &Expr,
    definition_env: &Environment,
) -> Result<MacroTransformer, EvalError> {
    let Some(elements) = expr_list(expr) else {
        return Err(err_at(
            expr.loc,
            "define-syntax expects a syntax-rules form",
        ));
    };

    if elements.len() < 2 || !matches!(expr_plain_symbol(&elements[0]), Some("syntax-rules")) {
        return Err(err_at(
            expr.loc,
            "define-syntax expects a syntax-rules form",
        ));
    }

    let Some(literal_exprs) = expr_list(&elements[1]) else {
        return Err(err_at(
            elements[1].loc,
            "syntax-rules literals must be a list",
        ));
    };

    let literals = literal_exprs
        .iter()
        .map(|literal| {
            expr_symbol_name(literal)
                .map(ToOwned::to_owned)
                .ok_or_else(|| err_at(literal.loc, "syntax-rules literals must be symbols"))
        })
        .collect::<Result<HashSet<_>, EvalError>>()?;

    let mut rules = Vec::new();
    for rule_expr in &elements[2..] {
        let Some(rule_parts) = expr_list(rule_expr) else {
            return Err(err_at(rule_expr.loc, "syntax-rules clauses must be pairs"));
        };

        if rule_parts.len() != 2 {
            return Err(err_at(rule_expr.loc, "syntax-rules clauses must be pairs"));
        }

        rules.push(MacroRule {
            pattern: rule_parts[0].clone(),
            template: rule_parts[1].clone(),
        });
    }

    if rules.is_empty() {
        return Err(err_at(expr.loc, "syntax-rules requires at least one rule"));
    }

    Ok(MacroTransformer {
        literals,
        rules,
        definition_env: definition_env.clone(),
    })
}

fn expand_macro(
    transformer: &MacroTransformer,
    invocation: &Expr,
    state: &mut EvalState,
) -> Result<Expr, EvalError> {
    for rule in &transformer.rules {
        if let Some(bindings) = match_macro_rule(rule, invocation, &transformer.literals) {
            let expanded = expand_template(&rule.template, &bindings, None)?;
            return Ok(hygienize_expr(
                &expanded,
                &HashMap::new(),
                &transformer.definition_env,
                state,
            ));
        }
    }

    Err(err_at(invocation.loc, "no matching syntax-rules clause"))
}

fn match_macro_rule(
    rule: &MacroRule,
    invocation: &Expr,
    literals: &HashSet<String>,
) -> Option<MacroBindings> {
    let pattern_items = expr_list(&rule.pattern)?;
    let invocation_items = expr_list(invocation)?;

    if pattern_items.is_empty() || invocation_items.is_empty() {
        return None;
    }

    if expr_symbol_name(&pattern_items[0])? != expr_symbol_name(&invocation_items[0])? {
        return None;
    }

    let mut bindings = MacroBindings::default();
    if match_pattern_list(
        &pattern_items[1..],
        &invocation_items[1..],
        literals,
        &mut bindings,
    ) {
        Some(bindings)
    } else {
        None
    }
}

fn match_pattern_list(
    patterns: &[Expr],
    data: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) -> bool {
    if patterns.is_empty() {
        return data.is_empty();
    }

    if patterns.len() >= 2 && is_ellipsis(&patterns[1]) {
        for repeat_count in 0..=data.len() {
            let mut trial = bindings.clone();
            let mut matched = true;

            if !initialize_repeated_binding(&patterns[0], literals, &mut trial) {
                continue;
            }

            for datum in &data[..repeat_count] {
                if !match_pattern_repeated(&patterns[0], datum, literals, &mut trial) {
                    matched = false;
                    break;
                }
            }

            if matched
                && match_pattern_list(&patterns[2..], &data[repeat_count..], literals, &mut trial)
            {
                *bindings = trial;
                return true;
            }
        }

        return false;
    }

    let Some((datum, rest)) = data.split_first() else {
        return false;
    };

    match_pattern_once(&patterns[0], datum, literals, bindings)
        && match_pattern_list(&patterns[1..], rest, literals, bindings)
}

fn match_pattern_once(
    pattern: &Expr,
    datum: &Expr,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) -> bool {
    match &pattern.kind {
        ExprKind::Number(value) => matches!(datum.kind, ExprKind::Number(other) if other == *value),
        ExprKind::Boolean(value) => {
            matches!(datum.kind, ExprKind::Boolean(other) if other == *value)
        }
        ExprKind::String(value) => matches!(&datum.kind, ExprKind::String(other) if other == value),
        ExprKind::Symbol(identifier) => {
            let name = identifier.display_name();
            if literals.contains(name) {
                matches!(expr_symbol_name(datum), Some(other) if other == name)
            } else {
                bind_macro_value(name, MacroBinding::Single(datum.clone()), bindings)
            }
        }
        ExprKind::List(pattern_items) => {
            let Some(data_items) = expr_list(datum) else {
                return false;
            };
            match_pattern_list(pattern_items, data_items, literals, bindings)
        }
    }
}

fn match_pattern_repeated(
    pattern: &Expr,
    datum: &Expr,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) -> bool {
    if let ExprKind::Symbol(identifier) = &pattern.kind {
        let name = identifier.display_name();
        if !literals.contains(name) {
            return bind_macro_value(name, MacroBinding::Repeated(vec![datum.clone()]), bindings);
        }
    }

    match_pattern_once(pattern, datum, literals, bindings)
}

fn initialize_repeated_binding(
    pattern: &Expr,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) -> bool {
    let ExprKind::Symbol(identifier) = &pattern.kind else {
        return true;
    };

    let name = identifier.display_name();
    if literals.contains(name) {
        return true;
    }

    match bindings.values.get(name) {
        None => {
            bindings
                .values
                .insert(name.to_string(), MacroBinding::Repeated(Vec::new()));
            true
        }
        Some(MacroBinding::Repeated(_)) => true,
        Some(MacroBinding::Single(_)) => false,
    }
}

fn bind_macro_value(name: &str, value: MacroBinding, bindings: &mut MacroBindings) -> bool {
    match bindings.values.get_mut(name) {
        None => {
            bindings.values.insert(name.to_string(), value);
            true
        }
        Some(existing) => match (existing, value) {
            (MacroBinding::Single(existing_expr), MacroBinding::Single(new_expr)) => {
                syntax_eq(existing_expr, &new_expr)
            }
            (MacroBinding::Repeated(existing_values), MacroBinding::Repeated(mut new_values)) => {
                existing_values.append(&mut new_values);
                true
            }
            _ => false,
        },
    }
}

fn syntax_eq(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Number(a), ExprKind::Number(b)) => a == b,
        (ExprKind::Boolean(a), ExprKind::Boolean(b)) => a == b,
        (ExprKind::String(a), ExprKind::String(b)) => a == b,
        (ExprKind::Symbol(a), ExprKind::Symbol(b)) => a.display_name() == b.display_name(),
        (ExprKind::List(a), ExprKind::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| syntax_eq(a, b))
        }
        _ => false,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &MacroBindings,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::String(_) => Ok(template.clone()),
        ExprKind::Symbol(identifier) => match bindings.values.get(identifier.display_name()) {
            Some(MacroBinding::Single(expr)) => Ok(expr.clone()),
            Some(MacroBinding::Repeated(values)) => {
                let Some(index) = repeat_index else {
                    return Err(err_at(
                        template.loc,
                        format!(
                            "template variable {} requires ellipsis",
                            identifier.display_name()
                        ),
                    ));
                };

                values.get(index).cloned().ok_or_else(|| {
                    err_at(
                        template.loc,
                        format!(
                            "missing repetition for template variable {}",
                            identifier.display_name()
                        ),
                    )
                })
            }
            None => Ok(Expr {
                kind: ExprKind::Symbol(Identifier::introduced(
                    identifier.display_name().to_string(),
                )),
                loc: template.loc,
            }),
        },
        ExprKind::List(elements) => {
            let mut expanded = Vec::new();
            let mut index = 0;

            while index < elements.len() {
                if index + 1 < elements.len() && is_ellipsis(&elements[index + 1]) {
                    let repeat_count = template_repeat_count(&elements[index], bindings)?;
                    for repeated_index in 0..repeat_count {
                        expanded.push(expand_template(
                            &elements[index],
                            bindings,
                            Some(repeated_index),
                        )?);
                    }
                    index += 2;
                    continue;
                }

                expanded.push(expand_template(&elements[index], bindings, repeat_index)?);
                index += 1;
            }

            Ok(Expr {
                kind: ExprKind::List(expanded),
                loc: template.loc,
            })
        }
    }
}

fn template_repeat_count(template: &Expr, bindings: &MacroBindings) -> Result<usize, EvalError> {
    let mut count = None;
    collect_template_repeat_count(template, bindings, &mut count)?;
    count.ok_or_else(|| {
        err_at(
            template.loc,
            "ellipsis template must reference a repeated pattern",
        )
    })
}

fn collect_template_repeat_count(
    template: &Expr,
    bindings: &MacroBindings,
    count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(identifier) => {
            if let Some(MacroBinding::Repeated(values)) =
                bindings.values.get(identifier.display_name())
            {
                match count {
                    Some(existing) if *existing != values.len() => {
                        return Err(err_at(template.loc, "mismatched ellipsis lengths"))
                    }
                    Some(_) => {}
                    None => *count = Some(values.len()),
                }
            }
        }
        ExprKind::List(elements) => {
            for element in elements {
                if is_ellipsis(element) {
                    continue;
                }
                collect_template_repeat_count(element, bindings, count)?;
            }
        }
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::String(_) => {}
    }

    Ok(())
}

fn hygienize_expr(
    expr: &Expr,
    scope: &HashMap<String, Identifier>,
    definition_env: &Environment,
    state: &mut EvalState,
) -> Expr {
    match &expr.kind {
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::String(_) => expr.clone(),
        ExprKind::Symbol(identifier) => Expr {
            kind: ExprKind::Symbol(hygienize_symbol(identifier, scope, definition_env, state)),
            loc: expr.loc,
        },
        ExprKind::List(elements) => {
            hygienize_list(expr.loc, elements, scope, definition_env, state)
        }
    }
}

fn hygienize_list(
    loc: SourceLoc,
    elements: &[Expr],
    scope: &HashMap<String, Identifier>,
    definition_env: &Environment,
    state: &mut EvalState,
) -> Expr {
    if elements.is_empty() {
        return Expr {
            kind: ExprKind::List(Vec::new()),
            loc,
        };
    }

    let head_name = expr_symbol_name(&elements[0]);
    if matches!(head_name, Some("quote")) && elements.len() == 2 {
        return Expr {
            kind: ExprKind::List(vec![
                plain_symbol_expr("quote", elements[0].loc),
                elements[1].clone(),
            ]),
            loc,
        };
    }

    if matches!(head_name, Some("lambda")) && elements.len() >= 3 {
        let Some(params) = expr_list(&elements[1]) else {
            return Expr {
                kind: ExprKind::List(
                    elements
                        .iter()
                        .map(|element| hygienize_expr(element, scope, definition_env, state))
                        .collect(),
                ),
                loc,
            };
        };

        let mut body_scope = scope.clone();
        let mut hygienic_params = Vec::new();
        for param in params {
            hygienic_params.push(hygienize_binding_identifier(param, &mut body_scope, state));
        }

        let mut hygienic = vec![
            plain_symbol_expr("lambda", elements[0].loc),
            Expr {
                kind: ExprKind::List(hygienic_params),
                loc: elements[1].loc,
            },
        ];
        hygienic.extend(
            elements[2..]
                .iter()
                .map(|element| hygienize_expr(element, &body_scope, definition_env, state)),
        );

        return Expr {
            kind: ExprKind::List(hygienic),
            loc,
        };
    }

    if matches!(head_name, Some("let") | Some("letrec") | Some("letrec*")) && elements.len() >= 3
    {
        if let Some(bindings) = expr_list(&elements[1]) {
            let mut body_scope = scope.clone();
            let mut hygienic_bindings = Vec::new();

            for binding in bindings {
                let Some(parts) = expr_list(binding) else {
                    break;
                };

                if parts.len() != 2 {
                    break;
                }

                hygienic_bindings.push(Expr {
                    kind: ExprKind::List(vec![
                        hygienize_binding_identifier(&parts[0], &mut body_scope, state),
                        hygienize_expr(&parts[1], scope, definition_env, state),
                    ]),
                    loc: binding.loc,
                });
            }

            if hygienic_bindings.len() == bindings.len() {
                let mut hygienic = vec![
                    plain_symbol_expr(head_name.expect("binding form name"), elements[0].loc),
                    Expr {
                        kind: ExprKind::List(hygienic_bindings),
                        loc: elements[1].loc,
                    },
                ];
                hygienic.extend(
                    elements[2..]
                        .iter()
                        .map(|element| hygienize_expr(element, &body_scope, definition_env, state)),
                );

                return Expr {
                    kind: ExprKind::List(hygienic),
                    loc,
                };
            }
        }
    }

    if matches!(head_name, Some("do")) && elements.len() >= 3 {
        if let (Some(bindings), Some(test_clause)) = (expr_list(&elements[1]), expr_list(&elements[2]))
        {
            let mut body_scope = scope.clone();
            let mut hygienic_bindings = Vec::new();

            for binding in bindings {
                let Some(parts) = expr_list(binding) else {
                    break;
                };

                if !(2..=3).contains(&parts.len()) {
                    break;
                }

                let mut hygienic_parts = vec![
                    hygienize_binding_identifier(&parts[0], &mut body_scope, state),
                    hygienize_expr(&parts[1], scope, definition_env, state),
                ];

                if parts.len() == 3 {
                    hygienic_parts.push(hygienize_expr(&parts[2], &body_scope, definition_env, state));
                }

                hygienic_bindings.push(Expr {
                    kind: ExprKind::List(hygienic_parts),
                    loc: binding.loc,
                });
            }

            if hygienic_bindings.len() == bindings.len() {
                let mut hygienic = vec![
                    plain_symbol_expr("do", elements[0].loc),
                    Expr {
                        kind: ExprKind::List(hygienic_bindings),
                        loc: elements[1].loc,
                    },
                    Expr {
                        kind: ExprKind::List(
                            test_clause
                                .iter()
                                .map(|element| {
                                    hygienize_expr(element, &body_scope, definition_env, state)
                                })
                                .collect(),
                        ),
                        loc: elements[2].loc,
                    },
                ];
                hygienic.extend(
                    elements[3..]
                        .iter()
                        .map(|element| hygienize_expr(element, &body_scope, definition_env, state)),
                );

                return Expr {
                    kind: ExprKind::List(hygienic),
                    loc,
                };
            }
        }
    }

    Expr {
        kind: ExprKind::List(
            elements
                .iter()
                .map(|element| hygienize_expr(element, scope, definition_env, state))
                .collect(),
        ),
        loc,
    }
}

fn hygienize_binding_identifier(
    expr: &Expr,
    scope: &mut HashMap<String, Identifier>,
    state: &mut EvalState,
) -> Expr {
    let Some(identifier) = expr_identifier(expr) else {
        return expr.clone();
    };

    if !identifier.introduced {
        return expr.clone();
    }

    let fresh = state.fresh_identifier(identifier.display_name());
    scope.insert(identifier.display_name().to_string(), fresh.clone());
    Expr {
        kind: ExprKind::Symbol(fresh),
        loc: expr.loc,
    }
}

fn hygienize_symbol(
    identifier: &Identifier,
    scope: &HashMap<String, Identifier>,
    definition_env: &Environment,
    state: &EvalState,
) -> Identifier {
    if !identifier.introduced {
        return identifier.clone();
    }

    if let Some(bound) = scope.get(identifier.display_name()) {
        return bound.clone();
    }

    if is_special_form_name(identifier.display_name())
        || state.syntax_rules.contains_key(identifier.display_name())
    {
        return Identifier::plain(identifier.display_name().to_string());
    }

    if let Some(cell) = definition_env.lookup_plain_cell(identifier.display_name()) {
        return Identifier::captured(identifier.display_name().to_string(), cell);
    }

    Identifier::plain(identifier.display_name().to_string())
}

fn plain_symbol_expr(name: &str, loc: SourceLoc) -> Expr {
    Expr {
        kind: ExprKind::Symbol(Identifier::plain(name)),
        loc,
    }
}

fn expect_parameter_identifier(expr: &Expr) -> Result<Identifier, EvalError> {
    let Some(identifier) = expr_identifier(expr) else {
        return Err(err_at(expr.loc, "parameter must be a symbol"));
    };

    if identifier.captured.is_some() {
        return Err(err_at(expr.loc, "parameter must be a symbol"));
    }

    Ok(identifier.clone())
}

fn expect_bindable_identifier(expr: &Expr, message: &str) -> Result<Identifier, EvalError> {
    let Some(identifier) = expr_identifier(expr) else {
        return Err(err_at(expr.loc, message));
    };

    if identifier.captured.is_some() {
        return Err(err_at(expr.loc, message));
    }

    Ok(identifier.clone())
}

fn expect_exact_args(
    name: &str,
    args: &[EvaluatedArg],
    loc: SourceLoc,
    expected: usize,
) -> Result<(), EvalError> {
    if args.len() == expected {
        return Ok(());
    }

    let suffix = if expected == 1 { "" } else { "s" };
    Err(err_at(
        loc,
        format!("{name} expects exactly {expected} argument{suffix}"),
    ))
}

fn expect_min_args(
    name: &str,
    args: &[EvaluatedArg],
    loc: SourceLoc,
    min: usize,
) -> Result<(), EvalError> {
    if args.len() >= min {
        return Ok(());
    }

    let suffix = if min == 1 { "" } else { "s" };
    Err(err_at(
        loc,
        format!("{name} expects at least {min} argument{suffix}"),
    ))
}

fn expect_number(arg: &EvaluatedArg) -> Result<f64, EvalError> {
    let Value::Number(value) = arg.value else {
        return Err(err_at(arg.expr.loc, "expected number"));
    };

    Ok(value)
}

fn expect_pair_arg(arg: &EvaluatedArg) -> Result<Rc<PairValue>, EvalError> {
    let Value::Pair(pair) = &arg.value else {
        return Err(err_at(arg.expr.loc, "expected pair"));
    };

    Ok(pair.clone())
}

fn expect_vector_arg(arg: &EvaluatedArg) -> Result<Rc<VectorValue>, EvalError> {
    let Value::Vector(vector) = &arg.value else {
        return Err(err_at(arg.expr.loc, "expected vector"));
    };

    Ok(vector.clone())
}

fn expect_nonnegative_integer(arg: &EvaluatedArg) -> Result<usize, EvalError> {
    let value = expect_number(arg)?;
    if value < 0.0 || value.fract() != 0.0 {
        return Err(err_at(arg.expr.loc, "expected non-negative integer"));
    }

    Ok(value as usize)
}

fn expect_proper_list(value: &Value, loc: SourceLoc) -> Result<Vec<Value>, EvalError> {
    let mut elements = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            Value::EmptyList => return Ok(elements),
            _ => return Err(err_at(loc, "expected proper list")),
        }
    }
}

fn comparison_builtin(
    name: &str,
    args: &[EvaluatedArg],
    loc: SourceLoc,
    predicate: impl Fn(f64, f64) -> bool,
) -> Result<Value, EvalError> {
    expect_min_args(name, args, loc, 2)?;

    for pair in args.windows(2) {
        let left = expect_number(&pair[0])?;
        let right = expect_number(&pair[1])?;

        if !predicate(left, right) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn builtin_plus(args: &[EvaluatedArg], _loc: SourceLoc) -> Result<Value, EvalError> {
    let mut result = 0.0;
    for arg in args {
        result += expect_number(arg)?;
    }
    Ok(Value::Number(result))
}

fn builtin_mul(args: &[EvaluatedArg], _loc: SourceLoc) -> Result<Value, EvalError> {
    let mut result = 1.0;
    for arg in args {
        result *= expect_number(arg)?;
    }
    Ok(Value::Number(result))
}

fn builtin_minus(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_min_args("-", args, loc, 1)?;

    let first = expect_number(&args[0])?;
    if args.len() == 1 {
        return Ok(Value::Number(-first));
    }

    let mut result = first;
    for arg in &args[1..] {
        result -= expect_number(arg)?;
    }

    Ok(Value::Number(result))
}

fn builtin_div(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_min_args("/", args, loc, 2)?;

    let mut result = expect_number(&args[0])?;
    for arg in &args[1..] {
        let value = expect_number(arg)?;
        if value == 0.0 {
            return Err(err_at(arg.expr.loc, "division by zero"));
        }
        result /= value;
    }

    Ok(Value::Number(result))
}

fn builtin_lt(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    comparison_builtin("<", args, loc, |left, right| left < right)
}

fn builtin_gt(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    comparison_builtin(">", args, loc, |left, right| left > right)
}

fn builtin_num_eq(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    comparison_builtin("=", args, loc, |left, right| left == right)
}

fn builtin_lte(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    comparison_builtin("<=", args, loc, |left, right| left <= right)
}

fn builtin_zero_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("zero?", args, loc, 1)?;
    Ok(Value::Boolean(expect_number(&args[0])? == 0.0))
}

fn builtin_not(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("not", args, loc, 1)?;
    Ok(Value::Boolean(!is_truthy(&args[0].value)))
}

fn builtin_cons(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("cons", args, loc, 2)?;
    Ok(Value::Pair(Rc::new(PairValue {
        car: args[0].value.clone(),
        cdr: args[1].value.clone(),
    })))
}

fn builtin_car(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("car", args, loc, 1)?;
    Ok(expect_pair_arg(&args[0])?.car.clone())
}

fn builtin_cdr(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("cdr", args, loc, 1)?;
    Ok(expect_pair_arg(&args[0])?.cdr.clone())
}

fn builtin_null_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("null?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::EmptyList)))
}

fn builtin_list(args: &[EvaluatedArg], _loc: SourceLoc) -> Result<Value, EvalError> {
    Ok(make_list(
        args.iter().map(|arg| arg.value.clone()).collect::<Vec<_>>(),
    ))
}

fn builtin_length(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("length", args, loc, 1)?;
    Ok(Value::Number(
        expect_proper_list(&args[0].value, args[0].expr.loc)?.len() as f64,
    ))
}

fn builtin_append(args: &[EvaluatedArg], _loc: SourceLoc) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::EmptyList);
    }

    let mut result = args[args.len() - 1].value.clone();

    for arg in args[..args.len() - 1].iter().rev() {
        for element in expect_proper_list(&arg.value, arg.expr.loc)?
            .into_iter()
            .rev()
        {
            result = Value::Pair(Rc::new(PairValue {
                car: element,
                cdr: result,
            }));
        }
    }

    Ok(result)
}

fn builtin_eq(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("eq?", args, loc, 2)?;
    Ok(Value::Boolean(is_eq(&args[0].value, &args[1].value)))
}

fn builtin_eqv(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("eqv?", args, loc, 2)?;
    Ok(Value::Boolean(is_eqv(&args[0].value, &args[1].value)))
}

fn builtin_equal(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("equal?", args, loc, 2)?;
    Ok(Value::Boolean(is_equal(&args[0].value, &args[1].value)))
}

fn builtin_vector(args: &[EvaluatedArg], _loc: SourceLoc) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(args.iter().map(|arg| arg.value.clone()).collect()),
    })))
}

fn builtin_make_vector(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(err_at(loc, "make-vector expects 1 or 2 arguments"));
    }

    let length = expect_nonnegative_integer(&args[0])?;
    let fill = args
        .get(1)
        .map(|arg| arg.value.clone())
        .unwrap_or(Value::Void);

    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(vec![fill; length]),
    })))
}

fn builtin_vector_ref(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("vector-ref", args, loc, 2)?;

    let vector = expect_vector_arg(&args[0])?;
    let index = expect_nonnegative_integer(&args[1])?;
    let elements = vector.elements.borrow();
    let Some(value) = elements.get(index) else {
        return Err(err_at(args[1].expr.loc, "vector index out of bounds"));
    };

    Ok(value.clone())
}

fn builtin_vector_set(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("vector-set!", args, loc, 3)?;

    let vector = expect_vector_arg(&args[0])?;
    let index = expect_nonnegative_integer(&args[1])?;
    let mut elements = vector.elements.borrow_mut();
    let Some(slot) = elements.get_mut(index) else {
        return Err(err_at(args[1].expr.loc, "vector index out of bounds"));
    };

    *slot = args[2].value.clone();
    Ok(Value::Void)
}

fn builtin_vector_length(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("vector-length", args, loc, 1)?;

    Ok(Value::Number(
        expect_vector_arg(&args[0])?.elements.borrow().len() as f64,
    ))
}

fn builtin_vector_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("vector?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::Vector(_))))
}

fn builtin_vector_to_list(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("vector->list", args, loc, 1)?;

    Ok(make_list(
        expect_vector_arg(&args[0])?
            .elements
            .borrow()
            .iter()
            .cloned()
            .collect(),
    ))
}

fn builtin_list_to_vector(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("list->vector", args, loc, 1)?;

    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(expect_proper_list(&args[0].value, args[0].expr.loc)?),
    })))
}

fn builtin_string_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("string?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::String(_))))
}

fn builtin_number_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("number?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::Number(_))))
}

fn builtin_boolean_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("boolean?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::Boolean(_))))
}

fn builtin_pair_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("pair?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::Pair(_))))
}

fn builtin_symbol_pred(args: &[EvaluatedArg], loc: SourceLoc) -> Result<Value, EvalError> {
    expect_exact_args("symbol?", args, loc, 1)?;
    Ok(Value::Boolean(matches!(args[0].value, Value::Symbol(_))))
}

fn expr_list(expr: &Expr) -> Option<&[Expr]> {
    let ExprKind::List(elements) = &expr.kind else {
        return None;
    };

    Some(elements)
}

fn expr_identifier(expr: &Expr) -> Option<&Identifier> {
    let ExprKind::Symbol(identifier) = &expr.kind else {
        return None;
    };

    Some(identifier)
}

fn expr_symbol_name(expr: &Expr) -> Option<&str> {
    expr_identifier(expr).map(Identifier::display_name)
}

fn expr_plain_symbol(expr: &Expr) -> Option<&str> {
    let identifier = expr_identifier(expr)?;
    if identifier.is_plain() {
        Some(identifier.display_name())
    } else {
        None
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr_symbol_name(expr), Some("..."))
}

fn is_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-syntax"
            | "define-record-type"
            | "syntax-rules"
            | "set!"
            | "if"
            | "quote"
            | "lambda"
            | "and"
            | "or"
            | "begin"
            | "cond"
            | "let"
            | "letrec"
            | "letrec*"
            | "case"
            | "do"
    )
}

fn is_eq(left: &Value, right: &Value) -> bool {
    is_eqv(left, right)
}

fn is_eqv(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        (Value::Procedure(a), Value::Procedure(b)) => is_same_procedure(a, b),
        (Value::Record(a), Value::Record(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn is_same_procedure(left: &ProcedureValue, right: &ProcedureValue) -> bool {
    match (left, right) {
        (ProcedureValue::Builtin(a), ProcedureValue::Builtin(b)) => a.name == b.name,
        (ProcedureValue::Closure(a), ProcedureValue::Closure(b)) => Rc::ptr_eq(a, b),
        (ProcedureValue::Native(a), ProcedureValue::Native(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn is_equal(left: &Value, right: &Value) -> bool {
    if is_eqv(left, right) {
        return true;
    }

    match (left, right) {
        (Value::Pair(a), Value::Pair(b)) => {
            is_equal(&a.car, &b.car) && is_equal(&a.cdr, &b.cdr)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let left_elements = a.elements.borrow();
            let right_elements = b.elements.borrow();

            left_elements.len() == right_elements.len()
                && left_elements
                    .iter()
                    .zip(right_elements.iter())
                    .all(|(left, right)| is_equal(left, right))
        }
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        _ => false,
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

fn procedure_display_name(procedure: &ClosureProcedure) -> &str {
    procedure.name.as_deref().unwrap_or("lambda")
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Number(value) => format_number(*value),
        Value::Boolean(value) => {
            if *value {
                "#t".to_string()
            } else {
                "#f".to_string()
            }
        }
        Value::String(value) => format!("\"{}\"", escape_string(value)),
        Value::Symbol(value) => value.clone(),
        Value::EmptyList => "()".to_string(),
        Value::Pair(value) => format_pair(value),
        Value::Vector(value) => format_vector(value),
        Value::Void => "#<void>".to_string(),
        Value::Procedure(_) => "#<procedure>".to_string(),
        Value::Record(_) => "#<record>".to_string(),
    }
}

fn format_pair(value: &Rc<PairValue>) -> String {
    let mut parts = Vec::new();
    let mut tail = Value::Pair(value.clone());

    loop {
        match tail {
            Value::Pair(pair) => {
                parts.push(format_value(&pair.car));
                tail = pair.cdr.clone();
            }
            Value::EmptyList => return format!("({})", parts.join(" ")),
            other => return format!("({} . {})", parts.join(" "), format_value(&other)),
        }
    }
}

fn format_vector(value: &Rc<VectorValue>) -> String {
    let elements = value.elements.borrow();
    format!(
        "#({})",
        elements
            .iter()
            .map(format_value)
            .collect::<Vec<_>>()
            .join(" ")
    )
}

fn format_number(value: f64) -> String {
    if value == 0.0 {
        return "0".to_string();
    }

    if value.fract() == 0.0 {
        return format!("{value:.0}");
    }

    format!("{value}")
}

fn escape_string(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
}

fn is_whitespace(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}

fn err_at(loc: SourceLoc, message: impl Into<String>) -> EvalError {
    EvalError::at(loc.line, loc.col, message)
}

#[cfg(test)]
mod tests;
