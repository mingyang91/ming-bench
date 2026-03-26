use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::ast::{Expr, ExprKind};
use super::error::{EvalError, Position};
use super::parser::parse_program;

type EnvRef = Rc<Environment>;
type BindingCell = Rc<RefCell<Value>>;
type StringRef = Rc<RefCell<Vec<char>>>;

pub(crate) fn eval_program(input: &str) -> Result<Value, EvalError> {
    let expressions = parse_program(input)?;
    if expressions.is_empty() {
        return Err(EvalError::syntax(
            "expected at least one expression",
            Position::new(1, 1),
        ));
    }

    let environment = global_environment();
    eval_sequence(&expressions, &environment)
}

#[derive(Clone)]
pub(crate) enum Value {
    Int(i64),
    Bool(bool),
    String(StringRef),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    pub(crate) fn render(&self) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(value) => {
                if *value {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            Self::String(value) => render_string(value.borrow().as_slice()),
            Self::Char(value) => render_char(*value),
            Self::Symbol(name) => name.clone(),
            Self::List(elements) => render_list(elements),
            Self::Procedure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }
}

#[derive(Clone)]
enum Procedure {
    Primitive(Primitive),
    Lambda(LambdaProcedure),
}

#[derive(Clone)]
struct LambdaProcedure {
    name: Option<String>,
    parameters: ParameterSpec,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct ParameterSpec {
    required: Vec<String>,
    rest: Option<String>,
}

#[derive(Clone, Copy)]
enum Primitive {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    List,
    Append,
    Length,
    NullPredicate,
    PairPredicate,
    NumberPredicate,
    StringPredicate,
    BooleanPredicate,
    SymbolPredicate,
    StringCopy,
    StringSet,
    Apply,
}

impl Primitive {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::List => "list",
            Self::Append => "append",
            Self::Length => "length",
            Self::NullPredicate => "null?",
            Self::PairPredicate => "pair?",
            Self::NumberPredicate => "number?",
            Self::StringPredicate => "string?",
            Self::BooleanPredicate => "boolean?",
            Self::SymbolPredicate => "symbol?",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::Apply => "apply",
        }
    }
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, BindingCell>>,
    macros: RefCell<HashMap<String, Rc<MacroDefinition>>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            macros: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: &str, value: Value) {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            *cell.borrow_mut() = value;
            return;
        }
        self.define_cell(name, Rc::new(RefCell::new(value)));
    }

    fn define_cell(&self, name: &str, cell: BindingCell) {
        self.bindings.borrow_mut().insert(name.to_string(), cell);
    }

    fn lookup_cell(&self, name: &str) -> Option<BindingCell> {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            return Some(cell);
        }
        self.parent.as_ref().and_then(|parent| parent.lookup_cell(name))
    }

    fn lookup(&self, name: &str, pos: Position) -> Result<Value, EvalError> {
        if let Some(cell) = self.lookup_cell(name) {
            return Ok(cell.borrow().clone());
        }
        Err(EvalError::undefined_variable(name, pos))
    }

    fn set(&self, name: &str, value: Value, pos: Position) -> Result<(), EvalError> {
        if let Some(cell) = self.lookup_cell(name) {
            *cell.borrow_mut() = value;
            return Ok(());
        }
        Err(EvalError::undefined_variable(name, pos))
    }

    fn define_macro(&self, name: &str, macro_definition: MacroDefinition) {
        self.macros
            .borrow_mut()
            .insert(name.to_string(), Rc::new(macro_definition));
    }

    fn lookup_macro(&self, name: &str) -> Option<Rc<MacroDefinition>> {
        if let Some(macro_definition) = self.macros.borrow().get(name).cloned() {
            return Some(macro_definition);
        }
        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_macro(name))
    }
}

#[derive(Clone)]
struct Binding {
    name: String,
    value_expression: Expr,
}

#[derive(Clone)]
struct MacroDefinition {
    name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    env: EnvRef,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
enum MatchBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

struct ExpansionContext<'a> {
    macro_definition: &'a MacroDefinition,
    use_environment: &'a EnvRef,
    bindings: &'a HashMap<String, MatchBinding>,
    hygienic_names: HashMap<String, String>,
}

static NEXT_GENSYM: AtomicUsize = AtomicUsize::new(0);

fn global_environment() -> EnvRef {
    let environment = Environment::new(None);
    define_primitive(&environment, "+", Primitive::Add);
    define_primitive(&environment, "-", Primitive::Subtract);
    define_primitive(&environment, "*", Primitive::Multiply);
    define_primitive(&environment, "/", Primitive::Divide);
    define_primitive(&environment, "<", Primitive::LessThan);
    define_primitive(&environment, ">", Primitive::GreaterThan);
    define_primitive(&environment, "=", Primitive::Equal);
    define_primitive(&environment, "<=", Primitive::LessEqual);
    define_primitive(&environment, "not", Primitive::Not);
    define_primitive(&environment, "cons", Primitive::Cons);
    define_primitive(&environment, "car", Primitive::Car);
    define_primitive(&environment, "cdr", Primitive::Cdr);
    define_primitive(&environment, "list", Primitive::List);
    define_primitive(&environment, "append", Primitive::Append);
    define_primitive(&environment, "length", Primitive::Length);
    define_primitive(&environment, "null?", Primitive::NullPredicate);
    define_primitive(&environment, "pair?", Primitive::PairPredicate);
    define_primitive(&environment, "number?", Primitive::NumberPredicate);
    define_primitive(&environment, "string?", Primitive::StringPredicate);
    define_primitive(&environment, "boolean?", Primitive::BooleanPredicate);
    define_primitive(&environment, "symbol?", Primitive::SymbolPredicate);
    define_primitive(&environment, "string-copy", Primitive::StringCopy);
    define_primitive(&environment, "string-set!", Primitive::StringSet);
    define_primitive(&environment, "apply", Primitive::Apply);
    environment
}

fn define_primitive(environment: &EnvRef, name: &str, primitive: Primitive) {
    environment.define(
        name,
        Value::Procedure(Rc::new(Procedure::Primitive(primitive))),
    );
}

fn eval_sequence(expressions: &[Expr], environment: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Void;
    for expression in expressions {
        last_value = eval(expression, environment)?;
    }
    Ok(last_value)
}

fn eval(expression: &Expr, environment: &EnvRef) -> Result<Value, EvalError> {
    match &expression.kind {
        ExprKind::Int(value) => Ok(Value::Int(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::String(value) => Ok(Value::String(new_string_ref(value.chars().collect()))),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::Symbol(name) => environment.lookup(name, expression.pos),
        ExprKind::List(elements) => eval_list(expression.pos, elements, environment),
    }
}

fn eval_list(pos: Position, elements: &[Expr], environment: &EnvRef) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = elements.split_first() else {
        return Err(EvalError::syntax("cannot evaluate empty list", pos));
    };

    if let ExprKind::Symbol(name) = &operator.kind {
        if name == "define-syntax" {
            return eval_define_syntax(arguments, environment, pos);
        }
        if let Some(macro_definition) = environment.lookup_macro(name) {
            let expanded = expand_macro_call(
                &macro_definition,
                &Expr::list(elements.to_vec(), pos),
                environment,
            )?;
            return eval(&expanded, environment);
        }
        return match name.as_str() {
            "define" => eval_define(arguments, environment, pos),
            "set!" => eval_set(arguments, environment, pos),
            "if" => eval_if(arguments, environment, pos),
            "quote" => eval_quote(arguments, pos),
            "lambda" => eval_lambda(arguments, environment, pos),
            "begin" => eval_sequence(arguments, environment),
            "cond" => eval_cond(arguments, environment),
            "let" => eval_let(arguments, environment, pos),
            "and" => eval_and(arguments, environment),
            "or" => eval_or(arguments, environment),
            _ => apply(operator, arguments, environment, pos),
        };
    }

    apply(operator, arguments, environment, pos)
}

fn apply(
    operator_expression: &Expr,
    argument_expressions: &[Expr],
    environment: &EnvRef,
    call_pos: Position,
) -> Result<Value, EvalError> {
    let operator = eval(operator_expression, environment)?;
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::not_a_procedure(
            operator.render(),
            operator_expression.pos,
        ));
    };

    let mut arguments = Vec::with_capacity(argument_expressions.len());
    for argument_expression in argument_expressions {
        arguments.push(eval(argument_expression, environment)?);
    }

    apply_procedure(procedure.as_ref(), arguments, call_pos)
}

fn apply_procedure(
    procedure: &Procedure,
    arguments: Vec<Value>,
    call_pos: Position,
) -> Result<Value, EvalError> {
    match procedure {
        Procedure::Primitive(primitive) => apply_primitive(*primitive, arguments, call_pos),
        Procedure::Lambda(lambda) => apply_lambda(lambda, arguments, call_pos),
    }
}

fn apply_lambda(
    lambda: &LambdaProcedure,
    arguments: Vec<Value>,
    call_pos: Position,
) -> Result<Value, EvalError> {
    let name = lambda.name.as_deref().unwrap_or("lambda");
    if lambda.parameters.rest.is_some() {
        require_min_arity(
            name,
            arguments.len(),
            lambda.parameters.required.len(),
            call_pos,
        )?;
    } else {
        require_exact_arity(
            name,
            arguments.len(),
            lambda.parameters.required.len(),
            call_pos,
        )?;
    }

    let call_environment = Environment::new(Some(lambda.env.clone()));
    let mut argument_iter = arguments.into_iter();
    for parameter in &lambda.parameters.required {
        let argument = argument_iter
            .next()
            .expect("lambda arity check should ensure required arguments");
        call_environment.define(parameter, argument);
    }
    if let Some(rest_parameter) = &lambda.parameters.rest {
        call_environment.define(rest_parameter, Value::List(argument_iter.collect()));
    }

    eval_sequence(&lambda.body, &call_environment)
}

fn apply_primitive(
    primitive: Primitive,
    arguments: Vec<Value>,
    call_pos: Position,
) -> Result<Value, EvalError> {
    match primitive {
        Primitive::Add => {
            let mut total = 0_i64;
            for argument in &arguments {
                total += expect_int(argument, primitive.name(), call_pos)?;
            }
            Ok(Value::Int(total))
        }
        Primitive::Subtract => {
            require_min_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            let mut result = expect_int(&arguments[0], primitive.name(), call_pos)?;
            if arguments.len() == 1 {
                return Ok(Value::Int(-result));
            }
            for argument in &arguments[1..] {
                result -= expect_int(argument, primitive.name(), call_pos)?;
            }
            Ok(Value::Int(result))
        }
        Primitive::Multiply => {
            let mut product = 1_i64;
            for argument in &arguments {
                product *= expect_int(argument, primitive.name(), call_pos)?;
            }
            Ok(Value::Int(product))
        }
        Primitive::Divide => {
            require_min_arity(primitive.name(), arguments.len(), 2, call_pos)?;
            let mut result = expect_int(&arguments[0], primitive.name(), call_pos)?;
            for argument in &arguments[1..] {
                let divisor = expect_int(argument, primitive.name(), call_pos)?;
                if divisor == 0 {
                    return Err(EvalError::division_by_zero(call_pos));
                }
                result /= divisor;
            }
            Ok(Value::Int(result))
        }
        Primitive::LessThan | Primitive::GreaterThan | Primitive::Equal | Primitive::LessEqual => {
            apply_comparison(primitive, &arguments, call_pos)
        }
        Primitive::Not => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(!arguments[0].is_truthy()))
        }
        Primitive::Cons => {
            require_exact_arity(primitive.name(), arguments.len(), 2, call_pos)?;
            let tail = expect_list(&arguments[1], primitive.name(), call_pos)?;
            let mut elements = Vec::with_capacity(tail.len() + 1);
            elements.push(arguments[0].clone());
            elements.extend_from_slice(tail);
            Ok(Value::List(elements))
        }
        Primitive::Car => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            let elements = expect_non_empty_list(&arguments[0], primitive.name(), call_pos)?;
            Ok(elements[0].clone())
        }
        Primitive::Cdr => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            let elements = expect_non_empty_list(&arguments[0], primitive.name(), call_pos)?;
            Ok(Value::List(elements[1..].to_vec()))
        }
        Primitive::List => Ok(Value::List(arguments)),
        Primitive::Append => {
            let mut appended = Vec::new();
            for argument in &arguments {
                appended.extend_from_slice(expect_list(argument, primitive.name(), call_pos)?);
            }
            Ok(Value::List(appended))
        }
        Primitive::Length => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Int(
                expect_list(&arguments[0], primitive.name(), call_pos)?.len() as i64,
            ))
        }
        Primitive::NullPredicate => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(
                matches!(&arguments[0], Value::List(elements) if elements.is_empty()),
            ))
        }
        Primitive::PairPredicate => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(
                matches!(&arguments[0], Value::List(elements) if !elements.is_empty()),
            ))
        }
        Primitive::NumberPredicate => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(matches!(&arguments[0], Value::Int(_))))
        }
        Primitive::StringPredicate => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(matches!(&arguments[0], Value::String(_))))
        }
        Primitive::BooleanPredicate => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(matches!(&arguments[0], Value::Bool(_))))
        }
        Primitive::SymbolPredicate => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            Ok(Value::Bool(matches!(&arguments[0], Value::Symbol(_))))
        }
        Primitive::StringCopy => {
            require_exact_arity(primitive.name(), arguments.len(), 1, call_pos)?;
            let string = expect_string(&arguments[0], primitive.name(), call_pos)?;
            let copied = string.borrow().clone();
            Ok(Value::String(new_string_ref(copied)))
        }
        Primitive::StringSet => {
            require_exact_arity(primitive.name(), arguments.len(), 3, call_pos)?;
            let string = expect_string(&arguments[0], primitive.name(), call_pos)?;
            let index = expect_index(&arguments[1], primitive.name(), call_pos)?;
            let value = expect_char(&arguments[2], primitive.name(), call_pos)?;
            let mut string = string.borrow_mut();
            if index >= string.len() {
                return Err(EvalError::type_mismatch(
                    format!("{} index out of range", primitive.name()),
                    call_pos,
                ));
            }
            string[index] = value;
            Ok(Value::Void)
        }
        Primitive::Apply => {
            require_min_arity(primitive.name(), arguments.len(), 2, call_pos)?;

            let mut arguments = arguments.into_iter();
            let procedure_value = arguments
                .next()
                .expect("apply arity check should ensure a procedure argument");
            let Value::Procedure(procedure) = procedure_value else {
                return Err(EvalError::not_a_procedure(
                    procedure_value.render(),
                    call_pos,
                ));
            };

            let mut applied_arguments: Vec<Value> = arguments.collect();
            let tail_arguments = match applied_arguments.pop() {
                Some(Value::List(elements)) => elements,
                Some(_) => {
                    return Err(EvalError::type_mismatch(
                        "apply expects a list as its final argument",
                        call_pos,
                    ))
                }
                None => unreachable!("apply arity check should ensure a final list argument"),
            };
            applied_arguments.extend(tail_arguments);

            apply_procedure(procedure.as_ref(), applied_arguments, call_pos)
        }
    }
}

fn apply_comparison(
    primitive: Primitive,
    arguments: &[Value],
    call_pos: Position,
) -> Result<Value, EvalError> {
    require_min_arity(primitive.name(), arguments.len(), 2, call_pos)?;
    let mut previous = expect_int(&arguments[0], primitive.name(), call_pos)?;
    for argument in &arguments[1..] {
        let current = expect_int(argument, primitive.name(), call_pos)?;
        let matches = match primitive {
            Primitive::LessThan => previous < current,
            Primitive::GreaterThan => previous > current,
            Primitive::Equal => previous == current,
            Primitive::LessEqual => previous <= current,
            _ => false,
        };
        if !matches {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }
    Ok(Value::Bool(true))
}

fn eval_define(
    arguments: &[Expr],
    environment: &EnvRef,
    pos: Position,
) -> Result<Value, EvalError> {
    if arguments.is_empty() {
        return Err(EvalError::syntax("define expected a binding target", pos));
    }

    match &arguments[0].kind {
        ExprKind::Symbol(name) => {
            require_exact_arity("define", arguments.len(), 2, pos)?;
            let value = eval(&arguments[1], environment)?;
            environment.define(name, value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            if signature.is_empty() {
                return Err(EvalError::syntax(
                    "define expected a function name",
                    arguments[0].pos,
                ));
            }

            let ExprKind::Symbol(name) = &signature[0].kind else {
                return Err(EvalError::syntax(
                    "define expected a function name",
                    signature[0].pos,
                ));
            };

            if arguments.len() < 2 {
                return Err(EvalError::syntax("define expected a function body", pos));
            }

            let parameters = parse_parameter_spec_from_slice(&signature[1..], "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
                name: Some(name.clone()),
                parameters,
                body: arguments[1..].to_vec(),
                env: environment.clone(),
            })));
            environment.define(name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::syntax(
            "define expected a symbol or function signature",
            arguments[0].pos,
        )),
    }
}

fn eval_define_syntax(
    arguments: &[Expr],
    environment: &EnvRef,
    pos: Position,
) -> Result<Value, EvalError> {
    require_exact_arity("define-syntax", arguments.len(), 2, pos)?;

    let ExprKind::Symbol(name) = &arguments[0].kind else {
        return Err(EvalError::syntax(
            "define-syntax expected a macro name",
            arguments[0].pos,
        ));
    };

    let macro_definition = parse_macro_definition(name, &arguments[1], environment)?;
    environment.define_macro(name, macro_definition);
    Ok(Value::Void)
}

fn eval_set(arguments: &[Expr], environment: &EnvRef, pos: Position) -> Result<Value, EvalError> {
    require_exact_arity("set!", arguments.len(), 2, pos)?;

    let ExprKind::Symbol(name) = &arguments[0].kind else {
        return Err(EvalError::syntax(
            "set! expected a symbol target",
            arguments[0].pos,
        ));
    };

    let value = eval(&arguments[1], environment)?;
    environment.set(name, value, arguments[0].pos)?;
    Ok(Value::Void)
}

fn eval_if(arguments: &[Expr], environment: &EnvRef, pos: Position) -> Result<Value, EvalError> {
    require_exact_arity("if", arguments.len(), 3, pos)?;
    let condition = eval(&arguments[0], environment)?;
    let branch = if condition.is_truthy() {
        &arguments[1]
    } else {
        &arguments[2]
    };
    eval(branch, environment)
}

fn eval_quote(arguments: &[Expr], pos: Position) -> Result<Value, EvalError> {
    require_exact_arity("quote", arguments.len(), 1, pos)?;
    quote(&arguments[0])
}

fn eval_lambda(
    arguments: &[Expr],
    environment: &EnvRef,
    pos: Position,
) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(EvalError::syntax(
            "lambda expected parameters and a body",
            pos,
        ));
    }

    let parameters = parse_parameter_spec(&arguments[0], "lambda")?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda(
        LambdaProcedure {
            name: None,
            parameters,
            body: arguments[1..].to_vec(),
            env: environment.clone(),
        },
    ))))
}

fn eval_cond(arguments: &[Expr], environment: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause_expression) in arguments.iter().enumerate() {
        let ExprKind::List(clause_elements) = &clause_expression.kind else {
            return Err(EvalError::syntax(
                "cond clauses must be lists",
                clause_expression.pos,
            ));
        };

        if clause_elements.is_empty() {
            return Err(EvalError::syntax(
                "cond clause cannot be empty",
                clause_expression.pos,
            ));
        }

        let test_expression = &clause_elements[0];
        if let ExprKind::Symbol(name) = &test_expression.kind {
            if name == "else" {
                if index + 1 != arguments.len() {
                    return Err(EvalError::syntax(
                        "cond else clause must be last",
                        test_expression.pos,
                    ));
                }
                if clause_elements.len() == 1 {
                    return Err(EvalError::syntax(
                        "cond else clause expected a body",
                        test_expression.pos,
                    ));
                }
                return eval_sequence(&clause_elements[1..], environment);
            }
        }

        let test_value = eval(test_expression, environment)?;
        if test_value.is_truthy() {
            if clause_elements.len() == 1 {
                return Ok(test_value);
            }
            return eval_sequence(&clause_elements[1..], environment);
        }
    }

    Ok(Value::Void)
}

fn eval_let(arguments: &[Expr], environment: &EnvRef, pos: Position) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(EvalError::syntax("let expected bindings and a body", pos));
    }

    if let ExprKind::Symbol(name) = &arguments[0].kind {
        return eval_named_let(name, &arguments[1..], environment, pos);
    }

    let bindings = parse_bindings(&arguments[0], "let")?;
    let local_environment = Environment::new(Some(environment.clone()));
    for binding in bindings {
        let value = eval(&binding.value_expression, environment)?;
        local_environment.define(&binding.name, value);
    }

    eval_sequence(&arguments[1..], &local_environment)
}

fn eval_named_let(
    name: &str,
    arguments: &[Expr],
    environment: &EnvRef,
    pos: Position,
) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(EvalError::syntax("let expected bindings and a body", pos));
    }

    let bindings = parse_bindings(&arguments[0], "let")?;
    let parameters = bindings
        .iter()
        .map(|binding| binding.name.clone())
        .collect();
    let local_environment = Environment::new(Some(environment.clone()));
    let procedure = Rc::new(Procedure::Lambda(LambdaProcedure {
        name: Some(name.to_string()),
        parameters: ParameterSpec {
            required: parameters,
            rest: None,
        },
        body: arguments[1..].to_vec(),
        env: local_environment.clone(),
    }));
    local_environment.define(name, Value::Procedure(procedure.clone()));

    let mut initial_values = Vec::with_capacity(bindings.len());
    for binding in bindings {
        initial_values.push(eval(&binding.value_expression, environment)?);
    }

    apply_procedure(procedure.as_ref(), initial_values, pos)
}

fn parse_bindings(bindings_expression: &Expr, form_name: &str) -> Result<Vec<Binding>, EvalError> {
    let ExprKind::List(bindings) = &bindings_expression.kind else {
        return Err(EvalError::syntax(
            format!("{form_name} bindings must be a list"),
            bindings_expression.pos,
        ));
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding_expression in bindings {
        let ExprKind::List(binding_elements) = &binding_expression.kind else {
            return Err(EvalError::syntax(
                format!("{form_name} bindings must be lists"),
                binding_expression.pos,
            ));
        };

        if binding_elements.len() != 2 {
            return Err(EvalError::syntax(
                format!("{form_name} bindings must have a name and value"),
                binding_expression.pos,
            ));
        }

        let ExprKind::Symbol(name) = &binding_elements[0].kind else {
            return Err(EvalError::syntax(
                format!("{form_name} bindings must start with a symbol"),
                binding_elements[0].pos,
            ));
        };

        parsed.push(Binding {
            name: name.clone(),
            value_expression: binding_elements[1].clone(),
        });
    }

    Ok(parsed)
}

fn parse_parameter_spec(
    parameters_expr: &Expr,
    form_name: &str,
) -> Result<ParameterSpec, EvalError> {
    match &parameters_expr.kind {
        ExprKind::List(parameters) => parse_parameter_spec_from_slice(parameters, form_name),
        ExprKind::Symbol(name) => Ok(ParameterSpec {
            required: Vec::new(),
            rest: Some(name.clone()),
        }),
        _ => Err(EvalError::syntax(
            format!("{form_name} parameters must be a list or symbol"),
            parameters_expr.pos,
        )),
    }
}

fn parse_parameter_spec_from_slice(
    parameters: &[Expr],
    form_name: &str,
) -> Result<ParameterSpec, EvalError> {
    let mut required = Vec::with_capacity(parameters.len());
    let mut rest = None;
    let mut index = 0;

    while index < parameters.len() {
        match &parameters[index].kind {
            ExprKind::Symbol(name) if name == "." => {
                let Some(rest_parameter) = parameters.get(index + 1) else {
                    return Err(EvalError::syntax(
                        format!("{form_name} expected a rest parameter after '.'"),
                        parameters[index].pos,
                    ));
                };
                let ExprKind::Symbol(rest_name) = &rest_parameter.kind else {
                    return Err(EvalError::syntax(
                        format!("{form_name} rest parameter must be a symbol"),
                        rest_parameter.pos,
                    ));
                };
                if index + 2 != parameters.len() {
                    return Err(EvalError::syntax(
                        format!("{form_name} expected '.' before the final parameter"),
                        parameters[index + 2].pos,
                    ));
                }
                rest = Some(rest_name.clone());
                break;
            }
            ExprKind::Symbol(name) => required.push(name.clone()),
            _ => {
                return Err(EvalError::syntax(
                    format!("{form_name} parameters must be symbols"),
                    parameters[index].pos,
                ))
            }
        }
        index += 1;
    }

    Ok(ParameterSpec { required, rest })
}

fn parse_macro_definition(
    name: &str,
    transformer: &Expr,
    environment: &EnvRef,
) -> Result<MacroDefinition, EvalError> {
    let ExprKind::List(elements) = &transformer.kind else {
        return Err(EvalError::syntax(
            "define-syntax expected a syntax-rules transformer",
            transformer.pos,
        ));
    };

    let Some((head, rest)) = elements.split_first() else {
        return Err(EvalError::syntax(
            "define-syntax expected a syntax-rules transformer",
            transformer.pos,
        ));
    };

    let ExprKind::Symbol(head_name) = &head.kind else {
        return Err(EvalError::syntax(
            "define-syntax expected syntax-rules",
            head.pos,
        ));
    };
    if head_name != "syntax-rules" {
        return Err(EvalError::syntax(
            "define-syntax expected syntax-rules",
            head.pos,
        ));
    }
    if rest.len() < 2 {
        return Err(EvalError::syntax(
            "syntax-rules expected literals and at least one rule",
            transformer.pos,
        ));
    }

    let ExprKind::List(literal_exprs) = &rest[0].kind else {
        return Err(EvalError::syntax(
            "syntax-rules literals must be a list",
            rest[0].pos,
        ));
    };

    let mut literals = HashSet::new();
    for literal in literal_exprs {
        let ExprKind::Symbol(literal_name) = &literal.kind else {
            return Err(EvalError::syntax(
                "syntax-rules literals must be identifiers",
                literal.pos,
            ));
        };
        literals.insert(literal_name.clone());
    }

    let mut rules = Vec::with_capacity(rest.len() - 1);
    for rule_expr in &rest[1..] {
        let ExprKind::List(rule_elements) = &rule_expr.kind else {
            return Err(EvalError::syntax(
                "syntax-rules clauses must be lists",
                rule_expr.pos,
            ));
        };
        if rule_elements.len() != 2 {
            return Err(EvalError::syntax(
                "syntax-rules clauses must contain a pattern and template",
                rule_expr.pos,
            ));
        }
        rules.push(SyntaxRule {
            pattern: rule_elements[0].clone(),
            template: rule_elements[1].clone(),
        });
    }

    Ok(MacroDefinition {
        name: name.to_string(),
        literals,
        rules,
        env: environment.clone(),
    })
}

fn expand_macro_call(
    macro_definition: &MacroDefinition,
    call_expression: &Expr,
    use_environment: &EnvRef,
) -> Result<Expr, EvalError> {
    for rule in &macro_definition.rules {
        let mut bindings = HashMap::new();
        if match_pattern(&rule.pattern, call_expression, macro_definition, &mut bindings)? {
            let mut context = ExpansionContext {
                macro_definition,
                use_environment,
                bindings: &bindings,
                hygienic_names: HashMap::new(),
            };
            return instantiate_template(&rule.template, &mut context, &HashMap::new(), None);
        }
    }

    Err(EvalError::syntax(
        format!("no matching syntax-rules clause for {}", macro_definition.name),
        call_expression.pos,
    ))
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    macro_definition: &MacroDefinition,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match (&pattern.kind, &input.kind) {
        (ExprKind::Int(expected), ExprKind::Int(found)) => Ok(expected == found),
        (ExprKind::Bool(expected), ExprKind::Bool(found)) => Ok(expected == found),
        (ExprKind::String(expected), ExprKind::String(found)) => Ok(expected == found),
        (ExprKind::Char(expected), ExprKind::Char(found)) => Ok(expected == found),
        (ExprKind::Symbol(name), _) => {
            if name == "..." {
                return Ok(false);
            }
            if name == &macro_definition.name || macro_definition.literals.contains(name) {
                return Ok(matches!(&input.kind, ExprKind::Symbol(found) if found == name));
            }

            match bindings.get(name) {
                Some(MatchBinding::Single(existing)) => Ok(expr_same_structure(existing, input)),
                Some(MatchBinding::Repeated(_)) => Ok(false),
                None => {
                    bindings.insert(name.clone(), MatchBinding::Single(input.clone()));
                    Ok(true)
                }
            }
        }
        (ExprKind::List(pattern_elements), ExprKind::List(input_elements)) => {
            match_pattern_list(pattern_elements, input_elements, macro_definition, bindings)
        }
        _ => Ok(false),
    }
}

fn match_pattern_list(
    pattern_elements: &[Expr],
    input_elements: &[Expr],
    macro_definition: &MacroDefinition,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    if pattern_elements.len() >= 2 && is_ellipsis(&pattern_elements[pattern_elements.len() - 1]) {
        let fixed_patterns = &pattern_elements[..pattern_elements.len() - 2];
        let repeated_pattern = &pattern_elements[pattern_elements.len() - 2];

        if input_elements.len() < fixed_patterns.len() {
            return Ok(false);
        }

        for (pattern, input) in fixed_patterns.iter().zip(input_elements.iter()) {
            if !match_pattern(pattern, input, macro_definition, bindings)? {
                return Ok(false);
            }
        }

        initialize_repeated_bindings(repeated_pattern, macro_definition, bindings)?;
        for input in &input_elements[fixed_patterns.len()..] {
            let mut iteration_bindings = HashMap::new();
            if !match_pattern(
                repeated_pattern,
                input,
                macro_definition,
                &mut iteration_bindings,
            )? {
                return Ok(false);
            }
            if !merge_repeated_bindings(bindings, iteration_bindings) {
                return Ok(false);
            }
        }
        return Ok(true);
    }

    if pattern_elements.len() != input_elements.len() {
        return Ok(false);
    }

    for (pattern, input) in pattern_elements.iter().zip(input_elements.iter()) {
        if !match_pattern(pattern, input, macro_definition, bindings)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn initialize_repeated_bindings(
    pattern: &Expr,
    macro_definition: &MacroDefinition,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<(), EvalError> {
    let mut variables = HashSet::new();
    collect_pattern_variables(pattern, macro_definition, &mut variables);
    for variable in variables {
        match bindings.get(&variable) {
            Some(MatchBinding::Repeated(_)) | None => {}
            Some(MatchBinding::Single(_)) => {
                return Err(EvalError::syntax(
                    format!("macro variable `{variable}` used inconsistently"),
                    pattern.pos,
                ));
            }
        }
        bindings
            .entry(variable)
            .or_insert_with(|| MatchBinding::Repeated(Vec::new()));
    }
    Ok(())
}

fn merge_repeated_bindings(
    bindings: &mut HashMap<String, MatchBinding>,
    iteration_bindings: HashMap<String, MatchBinding>,
) -> bool {
    for (name, binding) in iteration_bindings {
        let values = match binding {
            MatchBinding::Single(value) => vec![value],
            MatchBinding::Repeated(values) => values,
        };

        match bindings.get_mut(&name) {
            Some(MatchBinding::Repeated(existing)) => existing.extend(values),
            Some(MatchBinding::Single(_)) => return false,
            None => {
                bindings.insert(name, MatchBinding::Repeated(values));
            }
        }
    }
    true
}

fn collect_pattern_variables(
    pattern: &Expr,
    macro_definition: &MacroDefinition,
    variables: &mut HashSet<String>,
) {
    match &pattern.kind {
        ExprKind::Symbol(name)
            if name != "..."
                && name != &macro_definition.name
                && !macro_definition.literals.contains(name) =>
        {
            let _ = variables.insert(name.clone());
        }
        ExprKind::List(elements) => {
            for element in elements {
                if !is_ellipsis(element) {
                    collect_pattern_variables(element, macro_definition, variables);
                }
            }
        }
        _ => {}
    }
}

fn instantiate_template(
    template: &Expr,
    context: &mut ExpansionContext<'_>,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Int(_)
        | ExprKind::Bool(_)
        | ExprKind::String(_)
        | ExprKind::Char(_) => Ok(template.clone()),
        ExprKind::Symbol(name) => instantiate_symbol(template.pos, name, context, scope, repeat_index),
        ExprKind::List(elements) => instantiate_template_list(template, elements, context, scope, repeat_index),
    }
}

fn instantiate_template_list(
    template: &Expr,
    elements: &[Expr],
    context: &mut ExpansionContext<'_>,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(Expr {
        kind: ExprKind::Symbol(name),
        ..
    }) = elements.first()
    {
        if name == "let" {
            return instantiate_template_let(template, elements, context, scope, repeat_index);
        }
    }

    let mut expanded = Vec::with_capacity(elements.len());
    let mut index = 0;
    while index < elements.len() {
        if index + 1 < elements.len() && is_ellipsis(&elements[index + 1]) {
            let repeat_count = repetition_count(&elements[index], context.bindings, template.pos)?;
            for current_index in 0..repeat_count {
                expanded.push(instantiate_template(
                    &elements[index],
                    context,
                    scope,
                    Some(current_index),
                )?);
            }
            index += 2;
            continue;
        }

        expanded.push(instantiate_template(
            &elements[index],
            context,
            scope,
            repeat_index,
        )?);
        index += 1;
    }

    Ok(Expr::list(expanded, template.pos))
}

fn instantiate_template_let(
    template: &Expr,
    elements: &[Expr],
    context: &mut ExpansionContext<'_>,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if elements.len() < 3 {
        return Err(EvalError::syntax(
            "macro let template expected bindings and a body",
            template.pos,
        ));
    }

    let ExprKind::List(binding_templates) = &elements[1].kind else {
        return Err(EvalError::syntax(
            "macro let template expected a binding list",
            elements[1].pos,
        ));
    };

    let mut local_scope = scope.clone();
    let mut expanded_bindings = Vec::with_capacity(binding_templates.len());
    for binding_template in binding_templates {
        let ExprKind::List(binding_elements) = &binding_template.kind else {
            return Err(EvalError::syntax(
                "macro let bindings must be lists",
                binding_template.pos,
            ));
        };
        if binding_elements.len() != 2 {
            return Err(EvalError::syntax(
                "macro let bindings must contain a name and value",
                binding_template.pos,
            ));
        }

        let binding_name = match &binding_elements[0].kind {
            ExprKind::Symbol(name) if !context.bindings.contains_key(name) => {
                let renamed = fresh_symbol(name);
                local_scope.insert(name.clone(), renamed.clone());
                Expr::symbol(renamed, binding_elements[0].pos)
            }
            _ => instantiate_template(
                &binding_elements[0],
                context,
                scope,
                repeat_index,
            )?,
        };
        let binding_value = instantiate_template(
            &binding_elements[1],
            context,
            scope,
            repeat_index,
        )?;
        expanded_bindings.push(Expr::list(
            vec![binding_name, binding_value],
            binding_template.pos,
        ));
    }

    let mut expanded = Vec::with_capacity(elements.len());
    expanded.push(Expr::symbol("let", elements[0].pos));
    expanded.push(Expr::list(expanded_bindings, elements[1].pos));
    for body_expression in &elements[2..] {
        expanded.push(instantiate_template(
            body_expression,
            context,
            &local_scope,
            repeat_index,
        )?);
    }
    Ok(Expr::list(expanded, template.pos))
}

fn instantiate_symbol(
    pos: Position,
    name: &str,
    context: &mut ExpansionContext<'_>,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(renamed) = scope.get(name) {
        return Ok(Expr::symbol(renamed.clone(), pos));
    }

    if let Some(binding) = context.bindings.get(name) {
        return match binding {
            MatchBinding::Single(value) => Ok(value.clone()),
            MatchBinding::Repeated(values) => {
                let Some(index) = repeat_index else {
                    return Err(EvalError::syntax(
                        format!("macro variable `{name}` used without ellipsis"),
                        pos,
                    ));
                };
                values.get(index).cloned().ok_or_else(|| {
                    EvalError::syntax(
                        format!("macro variable `{name}` repetition index out of range"),
                        pos,
                    )
                })
            }
        };
    }

    Ok(Expr::symbol(hygienic_identifier(name, context), pos))
}

fn repetition_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    pos: Position,
) -> Result<usize, EvalError> {
    let mut count = None;
    collect_repetition_count(template, bindings, &mut count)?;
    count.ok_or_else(|| EvalError::syntax("ellipsis template has no repeated pattern variables", pos))
}

fn collect_repetition_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(MatchBinding::Repeated(values)) = bindings.get(name) {
                match count {
                    Some(existing) if *existing != values.len() => {
                        return Err(EvalError::syntax(
                            "macro template ellipsis lengths do not match",
                            template.pos,
                        ));
                    }
                    Some(_) => {}
                    None => *count = Some(values.len()),
                }
            }
        }
        ExprKind::List(elements) => {
            for element in elements {
                if !is_ellipsis(element) {
                    collect_repetition_count(element, bindings, count)?;
                }
            }
        }
        _ => {}
    }
    Ok(())
}

fn hygienic_identifier(name: &str, context: &mut ExpansionContext<'_>) -> String {
    if is_special_form(name)
        || name == context.macro_definition.name
        || context.macro_definition.env.lookup_macro(name).is_some()
    {
        return name.to_string();
    }

    if let Some(existing) = context.hygienic_names.get(name) {
        return existing.clone();
    }

    let renamed = fresh_symbol(name);
    if let Some(cell) = context.macro_definition.env.lookup_cell(name) {
        context.use_environment.define_cell(&renamed, cell);
    }
    context
        .hygienic_names
        .insert(name.to_string(), renamed.clone());
    renamed
}

fn fresh_symbol(base: &str) -> String {
    let id = NEXT_GENSYM.fetch_add(1, Ordering::Relaxed);
    format!("__macro_{}_{}", sanitize_symbol(base), id)
}

fn sanitize_symbol(name: &str) -> String {
    let sanitized: String = name
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect();
    if sanitized.is_empty() {
        "id".to_string()
    } else {
        sanitized
    }
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-syntax"
            | "if"
            | "quote"
            | "lambda"
            | "begin"
            | "cond"
            | "let"
            | "and"
            | "or"
            | "set!"
            | "syntax-rules"
    )
}

fn is_ellipsis(expression: &Expr) -> bool {
    matches!(&expression.kind, ExprKind::Symbol(name) if name == "...")
}

fn expr_same_structure(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Int(left), ExprKind::Int(right)) => left == right,
        (ExprKind::Bool(left), ExprKind::Bool(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left), ExprKind::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_same_structure(left, right))
        }
        _ => false,
    }
}

fn quote(expression: &Expr) -> Result<Value, EvalError> {
    match &expression.kind {
        ExprKind::Int(value) => Ok(Value::Int(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::String(value) => Ok(Value::String(new_string_ref(value.chars().collect()))),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::Symbol(name) => Ok(Value::Symbol(name.clone())),
        ExprKind::List(elements) => {
            let mut quoted = Vec::with_capacity(elements.len());
            for element in elements {
                quoted.push(quote(element)?);
            }
            Ok(Value::List(quoted))
        }
    }
}

fn eval_and(arguments: &[Expr], environment: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for argument in arguments {
        last = eval(argument, environment)?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(arguments: &[Expr], environment: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);
    for argument in arguments {
        last = eval(argument, environment)?;
        if last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn expect_int(value: &Value, operator: &str, pos: Position) -> Result<i64, EvalError> {
    let Value::Int(number) = value else {
        return Err(EvalError::type_mismatch(
            format!("{operator} expects integer arguments"),
            pos,
        ));
    };
    Ok(*number)
}

fn expect_index(value: &Value, operator: &str, pos: Position) -> Result<usize, EvalError> {
    let index = expect_int(value, operator, pos)?;
    if index < 0 {
        return Err(EvalError::type_mismatch(
            format!("{operator} index out of range"),
            pos,
        ));
    }
    Ok(index as usize)
}

fn expect_char(value: &Value, operator: &str, pos: Position) -> Result<char, EvalError> {
    let Value::Char(ch) = value else {
        return Err(EvalError::type_mismatch(
            format!("{operator} expects character arguments"),
            pos,
        ));
    };
    Ok(*ch)
}

fn expect_string(value: &Value, operator: &str, pos: Position) -> Result<StringRef, EvalError> {
    let Value::String(string) = value else {
        return Err(EvalError::type_mismatch(
            format!("{operator} expects string arguments"),
            pos,
        ));
    };
    Ok(string.clone())
}

fn expect_list<'a>(
    value: &'a Value,
    operator: &str,
    pos: Position,
) -> Result<&'a [Value], EvalError> {
    let Value::List(elements) = value else {
        return Err(EvalError::type_mismatch(
            format!("{operator} expects list arguments"),
            pos,
        ));
    };
    Ok(elements)
}

fn expect_non_empty_list<'a>(
    value: &'a Value,
    operator: &str,
    pos: Position,
) -> Result<&'a [Value], EvalError> {
    let elements = expect_list(value, operator, pos)?;
    if elements.is_empty() {
        return Err(EvalError::type_mismatch(
            format!("{operator} expected a non-empty list"),
            pos,
        ));
    }
    Ok(elements)
}

fn require_exact_arity(
    name: &str,
    actual: usize,
    expected: usize,
    pos: Position,
) -> Result<(), EvalError> {
    if actual != expected {
        return Err(EvalError::wrong_arg_count(
            name,
            format!("{expected} argument(s)"),
            actual,
            pos,
        ));
    }
    Ok(())
}

fn require_min_arity(
    name: &str,
    actual: usize,
    minimum: usize,
    pos: Position,
) -> Result<(), EvalError> {
    if actual < minimum {
        return Err(EvalError::wrong_arg_count(
            name,
            format!("at least {minimum} argument(s)"),
            actual,
            pos,
        ));
    }
    Ok(())
}

fn render_string(value: &[char]) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');
    for &ch in value {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }
    rendered.push('"');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

fn new_string_ref(value: Vec<char>) -> StringRef {
    Rc::new(RefCell::new(value))
}

fn render_list(elements: &[Value]) -> String {
    let mut rendered = String::new();
    rendered.push('(');
    for (index, element) in elements.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&element.render());
    }
    rendered.push(')');
    rendered
}
