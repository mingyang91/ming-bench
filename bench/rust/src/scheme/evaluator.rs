use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::ast::{Expr, ExprKind};
use super::error::{EvalError, Position};
use super::parser::parse_program;

type EnvRef = Rc<Environment>;

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
    String(String),
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
            Self::String(value) => render_string(value),
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
    parameters: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
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
        }
    }
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: &str, value: Value) {
        self.bindings.borrow_mut().insert(name.to_string(), value);
    }

    fn lookup(&self, name: &str, pos: Position) -> Result<Value, EvalError> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Ok(value);
        }
        if let Some(parent) = &self.parent {
            return parent.lookup(name, pos);
        }
        Err(EvalError::undefined_variable(name, pos))
    }
}

#[derive(Clone)]
struct Binding {
    name: String,
    value_expression: Expr,
}

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
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => environment.lookup(name, expression.pos),
        ExprKind::List(elements) => eval_list(expression.pos, elements, environment),
    }
}

fn eval_list(pos: Position, elements: &[Expr], environment: &EnvRef) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = elements.split_first() else {
        return Err(EvalError::syntax("cannot evaluate empty list", pos));
    };

    if let ExprKind::Symbol(name) = &operator.kind {
        return match name.as_str() {
            "define" => eval_define(arguments, environment, pos),
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
    require_exact_arity(name, arguments.len(), lambda.parameters.len(), call_pos)?;

    let call_environment = Environment::new(Some(lambda.env.clone()));
    for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
        call_environment.define(parameter, argument);
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

            let parameters = parse_parameter_names(&signature[1..], "define")?;
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

    let ExprKind::List(parameters_expr) = &arguments[0].kind else {
        return Err(EvalError::syntax(
            "lambda parameters must be a list",
            arguments[0].pos,
        ));
    };

    let parameters = parse_parameter_names(parameters_expr, "lambda")?;
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
        parameters,
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

fn parse_parameter_names(parameters: &[Expr], form_name: &str) -> Result<Vec<String>, EvalError> {
    let mut names = Vec::with_capacity(parameters.len());
    for parameter in parameters {
        let ExprKind::Symbol(name) = &parameter.kind else {
            return Err(EvalError::syntax(
                format!("{form_name} parameters must be symbols"),
                parameter.pos,
            ));
        };
        names.push(name.clone());
    }
    Ok(names)
}

fn quote(expression: &Expr) -> Result<Value, EvalError> {
    match &expression.kind {
        ExprKind::Int(value) => Ok(Value::Int(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
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

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');
    for ch in value.chars() {
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
