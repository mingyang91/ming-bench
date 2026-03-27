pub mod error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(_input: &str) -> Result<String, EvalError> {
    eval_str_with_output(_input).map(|(result, _)| result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    let expressions = Parser::new(_input).parse_program()?;
    if expressions.is_empty() {
        return Err(error_at("expected expression", Position::new(1, 1)));
    }

    let env = Environment::root(root_bindings());
    let mut context = EvalContext::default();
    let value = eval_sequence(&expressions, env, &mut context)?;
    Ok((value.render(), context.output))
}

#[cfg(test)]
mod tests;

type EvalResult<T> = Result<T, EvalError>;
type EnvRef = Rc<Environment>;
type BuiltinFn = fn(&[Value], Position, &mut EvalContext) -> EvalResult<Value>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

impl Position {
    const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug)]
struct RecordType {
    name: String,
    field_names: Vec<String>,
}

#[derive(Debug)]
struct RecordInstance {
    record_type: Rc<RecordType>,
    fields: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Int(i128, Position),
    Bool(bool, Position),
    String(String, Position),
    Char(char, Position),
    Symbol(String, Position),
    List(Vec<Expr>, Position),
}

impl Expr {
    fn position(&self) -> Position {
        match self {
            Self::Int(_, position)
            | Self::Bool(_, position)
            | Self::String(_, position)
            | Self::Char(_, position)
            | Self::Symbol(_, position)
            | Self::List(_, position) => *position,
        }
    }
}

#[derive(Clone)]
enum Value {
    Int(i128),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    EmptyList,
    Pair(Box<Value>, Box<Value>),
    Record(Rc<RecordInstance>),
    Builtin {
        name: &'static str,
        function: BuiltinFn,
    },
    Closure {
        params: Vec<String>,
        body: Vec<Expr>,
        env: EnvRef,
        name: Option<String>,
    },
    RecordConstructor {
        name: String,
        record_type: Rc<RecordType>,
        field_indices: Vec<usize>,
    },
    RecordPredicate {
        name: String,
        record_type: Rc<RecordType>,
    },
    RecordAccessor {
        name: String,
        record_type: Rc<RecordType>,
        field_index: usize,
    },
    Void,
    Uninitialized,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.render())
    }
}

impl Value {
    fn render(&self) -> String {
        render_value(self, false)
    }

    fn display(&self) -> String {
        render_value(self, true)
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "number",
            Self::Bool(_) => "boolean",
            Self::String(_) => "string",
            Self::Char(_) => "character",
            Self::Symbol(_) => "symbol",
            Self::EmptyList => "list",
            Self::Pair(_, _) => "pair",
            Self::Record(_) => "record",
            Self::Builtin { .. }
            | Self::Closure { .. }
            | Self::RecordConstructor { .. }
            | Self::RecordPredicate { .. }
            | Self::RecordAccessor { .. } => "procedure",
            Self::Void => "void",
            Self::Uninitialized => "uninitialized",
        }
    }
}

#[derive(Default)]
struct EvalContext {
    output: String,
}

impl EvalContext {
    fn write(&mut self, text: &str) {
        self.output.push_str(text);
    }
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Environment {
    fn root(initial_bindings: Vec<(String, Value)>) -> EnvRef {
        Rc::new(Self {
            parent: None,
            bindings: RefCell::new(HashMap::from_iter(initial_bindings)),
        })
    }

    fn child(parent: EnvRef, initial_bindings: Vec<(String, Value)>) -> EnvRef {
        Rc::new(Self {
            parent: Some(parent),
            bindings: RefCell::new(HashMap::from_iter(initial_bindings)),
        })
    }

    fn define(&self, name: &str, value: Value) {
        self.bindings.borrow_mut().insert(name.to_string(), value);
    }

    fn reserve(&self, name: &str) {
        self.bindings
            .borrow_mut()
            .insert(name.to_string(), Value::Uninitialized);
    }

    fn lookup(&self, name: &str, position: Position) -> EvalResult<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return match value {
                Value::Uninitialized => {
                    Err(error_at(format!("uninitialized symbol: {name}"), position))
                }
                other => Ok(other),
            };
        }

        match &self.parent {
            Some(parent) => parent.lookup(name, position),
            None => Err(error_at(format!("unbound symbol: {name}"), position)),
        }
    }
}

struct Parser {
    chars: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(mut self) -> EvalResult<Vec<Expr>> {
        let mut expressions = Vec::new();
        self.skip_ignorable();
        while !self.at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignorable();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> EvalResult<Expr> {
        self.skip_ignorable();
        if self.at_end() {
            return Err(error_at("unexpected end of input", self.position()));
        }

        let start = self.position();
        match self.current_char() {
            '(' => {
                self.advance();
                self.parse_list(start)
            }
            ')' => Err(error_at("unexpected ')'", start)),
            '\'' => {
                self.advance();
                self.parse_quote(start)
            }
            '"' => {
                self.advance();
                self.parse_string(start)
            }
            _ => self.parse_atom(start),
        }
    }

    fn parse_quote(&mut self, start: Position) -> EvalResult<Expr> {
        let expression = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".to_string(), start), expression],
            start,
        ))
    }

    fn parse_list(&mut self, start: Position) -> EvalResult<Expr> {
        let mut items = Vec::new();
        loop {
            self.skip_ignorable();
            if self.at_end() {
                return Err(error_at("unterminated list", start));
            }
            if self.current_char() == ')' {
                self.advance();
                break;
            }
            items.push(self.parse_expr()?);
        }
        Ok(Expr::List(items, start))
    }

    fn parse_string(&mut self, start: Position) -> EvalResult<Expr> {
        let mut value = String::new();
        loop {
            if self.at_end() {
                return Err(error_at("unterminated string", start));
            }

            match self.current_char() {
                '"' => {
                    self.advance();
                    break;
                }
                '\\' => {
                    self.advance();
                    value.push(self.parse_escaped_character(start)?);
                }
                ch => {
                    value.push(ch);
                    self.advance();
                }
            }
        }
        Ok(Expr::String(value, start))
    }

    fn parse_escaped_character(&mut self, start: Position) -> EvalResult<char> {
        if self.at_end() {
            return Err(error_at("unterminated string", start));
        }

        let escape_position = self.position();
        let escaped = match self.current_char() {
            '"' => '"',
            '\\' => '\\',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            other => {
                return Err(error_at(
                    format!("unsupported escape sequence: \\{other}"),
                    escape_position,
                ))
            }
        };
        self.advance();
        Ok(escaped)
    }

    fn parse_atom(&mut self, start: Position) -> EvalResult<Expr> {
        let mut token = String::new();
        while !self.at_end() && !is_delimiter(self.current_char()) {
            token.push(self.current_char());
            self.advance();
        }

        let expression = match token.as_str() {
            "#t" => Expr::Bool(true, start),
            "#f" => Expr::Bool(false, start),
            _ if token.starts_with("#\\") => {
                Expr::Char(parse_character_literal(&token, start)?, start)
            }
            _ if is_integer_token(&token) => {
                let value = token
                    .parse::<i128>()
                    .map_err(|_| error_at("integer literal out of range", start))?;
                Expr::Int(value, start)
            }
            _ => Expr::Symbol(token, start),
        };
        Ok(expression)
    }

    fn skip_ignorable(&mut self) {
        loop {
            self.skip_whitespace();
            if !self.at_end() && self.current_char() == ';' {
                self.skip_comment();
            } else {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.at_end() && self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn skip_comment(&mut self) {
        while !self.at_end() && self.current_char() != '\n' {
            self.advance();
        }
    }

    fn at_end(&self) -> bool {
        self.index >= self.chars.len()
    }

    fn current_char(&self) -> char {
        self.chars[self.index]
    }

    fn position(&self) -> Position {
        Position::new(self.line, self.column)
    }

    fn advance(&mut self) {
        if self.at_end() {
            return;
        }

        let ch = self.current_char();
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

fn parse_character_literal(token: &str, position: Position) -> EvalResult<char> {
    let Some(body) = token.strip_prefix("#\\") else {
        return Err(error_at("invalid character literal", position));
    };

    match body {
        "space" => Ok(' '),
        "newline" => Ok('\n'),
        _ => {
            let mut chars = body.chars();
            match (chars.next(), chars.next()) {
                (Some(ch), None) => Ok(ch),
                _ => Err(error_at("invalid character literal", position)),
            }
        }
    }
}

fn eval_sequence(
    expressions: &[Expr],
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let mut result = Value::Void;
    for expression in expressions {
        result = eval(expression, env.clone(), context)?;
    }
    Ok(result)
}

fn eval(expression: &Expr, env: EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    match expression {
        Expr::Int(value, _) => Ok(Value::Int(*value)),
        Expr::Bool(value, _) => Ok(Value::Bool(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Char(ch, _) => Ok(Value::Char(*ch)),
        Expr::Symbol(name, position) => env.lookup(name, *position),
        Expr::List(items, position) => eval_list(items, *position, env, context),
    }
}

fn eval_list(
    items: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let Some((head, rest)) = items.split_first() else {
        return Err(error_at("cannot evaluate an empty list", position));
    };

    match head {
        Expr::Symbol(name, _) if name == "and" => eval_and(rest, env, context),
        Expr::Symbol(name, _) if name == "begin" => eval_sequence(rest, env, context),
        Expr::Symbol(name, _) if name == "cond" => eval_cond(rest, env, context),
        Expr::Symbol(name, _) if name == "or" => eval_or(rest, env, context),
        Expr::Symbol(name, _) if name == "define" => eval_define(rest, position, env, context),
        Expr::Symbol(name, _) if name == "define-record-type" => {
            eval_define_record_type(rest, position, env)
        }
        Expr::Symbol(name, _) if name == "if" => eval_if(rest, position, env, context),
        Expr::Symbol(name, _) if name == "let" => eval_let(rest, position, env, context),
        Expr::Symbol(name, _) if name == "quote" => eval_quote(rest, position),
        Expr::Symbol(name, _) if name == "lambda" => eval_lambda(rest, position, env),
        _ => eval_application(head, rest, position, env, context),
    }
}

fn eval_application(
    operator: &Expr,
    arguments: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let function = eval(operator, env.clone(), context)?;
    let mut evaluated_arguments = Vec::with_capacity(arguments.len());
    for argument in arguments {
        evaluated_arguments.push(eval(argument, env.clone(), context)?);
    }
    apply(function, evaluated_arguments, position, context)
}

fn eval_and(expressions: &[Expr], env: EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    if expressions.is_empty() {
        return Ok(Value::Bool(true));
    }

    for expression in &expressions[..expressions.len() - 1] {
        let value = eval(expression, env.clone(), context)?;
        if !is_truthy(&value) {
            return Ok(value);
        }
    }

    eval(&expressions[expressions.len() - 1], env, context)
}

fn eval_or(expressions: &[Expr], env: EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    if expressions.is_empty() {
        return Ok(Value::Bool(false));
    }

    for expression in expressions {
        let value = eval(expression, env.clone(), context)?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }

    Ok(Value::Bool(false))
}

fn eval_cond(clauses: &[Expr], env: EnvRef, context: &mut EvalContext) -> EvalResult<Value> {
    for (index, clause) in clauses.iter().enumerate() {
        match clause {
            Expr::List(items, clause_position) if !items.is_empty() => {
                if matches!(&items[0], Expr::Symbol(name, _) if name == "else") {
                    if index + 1 != clauses.len() {
                        return Err(error_at("cond else clause must be last", *clause_position));
                    }
                    if items.len() == 1 {
                        return Err(error_at(
                            "cond else clause must have a body",
                            *clause_position,
                        ));
                    }
                    return eval_sequence(&items[1..], env, context);
                }

                let test_value = eval(&items[0], env.clone(), context)?;
                if is_truthy(&test_value) {
                    if items.len() == 1 {
                        return Ok(test_value);
                    }
                    return eval_sequence(&items[1..], env, context);
                }
            }
            _ => {
                return Err(error_at(
                    "cond expected non-empty list clauses",
                    clause.position(),
                ))
            }
        }
    }

    Ok(Value::Void)
}

fn eval_define(
    arguments: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    match arguments {
        [Expr::Symbol(name, _), value_expression] => {
            eval_value_define(name, value_expression, env, context)
        }
        [Expr::List(parts, _), body @ ..] if !body.is_empty() => match parts.split_first() {
            Some((Expr::Symbol(name, _), parameters)) => {
                eval_procedure_define(name, parameters, body, env, position)
            }
            _ => Err(error_at(
                "define expected (define name expr) or (define (name args) body ...)",
                position,
            )),
        },
        _ => Err(error_at(
            "define expected (define name expr) or (define (name args) body ...)",
            position,
        )),
    }
}

fn eval_value_define(
    name: &str,
    value_expression: &Expr,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    env.reserve(name);
    let value = eval(value_expression, env.clone(), context)?;
    env.define(name, value);
    Ok(Value::Void)
}

fn eval_procedure_define(
    name: &str,
    parameters: &[Expr],
    body: &[Expr],
    env: EnvRef,
    position: Position,
) -> EvalResult<Value> {
    env.reserve(name);
    let value = build_closure(
        parameters,
        body,
        env.clone(),
        Some(name.to_string()),
        position,
    )?;
    env.define(name, value);
    Ok(Value::Void)
}

fn eval_define_record_type(
    arguments: &[Expr],
    position: Position,
    env: EnvRef,
) -> EvalResult<Value> {
    let definition = parse_record_type_definition(arguments, position)?;
    let field_names = definition
        .field_specs
        .iter()
        .map(|field_spec| field_spec.field_name.clone())
        .collect();
    let record_type = Rc::new(RecordType {
        name: definition.type_name,
        field_names,
    });
    let constructor_name = definition.constructor_name.clone();
    let predicate_name = definition.predicate_name.clone();

    env.define(
        &constructor_name,
        Value::RecordConstructor {
            name: definition.constructor_name,
            record_type: record_type.clone(),
            field_indices: definition.constructor_field_indices,
        },
    );
    env.define(
        &predicate_name,
        Value::RecordPredicate {
            name: definition.predicate_name,
            record_type: record_type.clone(),
        },
    );

    for (field_index, field_spec) in definition.field_specs.into_iter().enumerate() {
        if let Some(accessor_name) = field_spec.accessor_name {
            let accessor_binding_name = accessor_name.clone();
            env.define(
                &accessor_binding_name,
                Value::RecordAccessor {
                    name: accessor_name,
                    record_type: record_type.clone(),
                    field_index,
                },
            );
        }
    }

    Ok(Value::Void)
}

fn eval_if(
    arguments: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    match arguments {
        [condition, consequent, alternate] => {
            if is_truthy(&eval(condition, env.clone(), context)?) {
                eval(consequent, env, context)
            } else {
                eval(alternate, env, context)
            }
        }
        _ => Err(error_at("if expected 3 argument(s)", position)),
    }
}

fn eval_let(
    arguments: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    match arguments {
        [Expr::Symbol(name, _), bindings_expression, body @ ..] if !body.is_empty() => {
            eval_named_let(name, bindings_expression, body, position, env, context)
        }
        [bindings_expression, body @ ..] if !body.is_empty() => {
            eval_unnamed_let(bindings_expression, body, position, env, context)
        }
        _ => Err(error_at("let expected bindings and body", position)),
    }
}

fn eval_unnamed_let(
    bindings_expression: &Expr,
    body: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let bindings = parse_bindings(bindings_expression, position, "let")?;
    let mut bound_values = Vec::with_capacity(bindings.len());
    for (_, expression) in &bindings {
        bound_values.push(eval(expression, env.clone(), context)?);
    }
    let child_env = Environment::child(
        env,
        bindings
            .into_iter()
            .zip(bound_values)
            .map(|((name, _), value)| (name, value))
            .collect(),
    );
    eval_sequence(body, child_env, context)
}

fn eval_named_let(
    name: &str,
    bindings_expression: &Expr,
    body: &[Expr],
    position: Position,
    env: EnvRef,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let bindings = parse_bindings(bindings_expression, position, "let")?;
    let mut arguments = Vec::with_capacity(bindings.len());
    for (_, expression) in &bindings {
        arguments.push(eval(expression, env.clone(), context)?);
    }

    let closure_env = Environment::child(env, Vec::new());
    let closure = Value::Closure {
        params: bindings.iter().map(|(name, _)| name.clone()).collect(),
        body: body.to_vec(),
        env: closure_env.clone(),
        name: Some(name.to_string()),
    };
    closure_env.define(name, closure.clone());
    apply(closure, arguments, position, context)
}

fn eval_quote(arguments: &[Expr], position: Position) -> EvalResult<Value> {
    match arguments {
        [expression] => Ok(quote(expression)),
        _ => Err(error_at("quote expected 1 argument(s)", position)),
    }
}

fn eval_lambda(arguments: &[Expr], position: Position, env: EnvRef) -> EvalResult<Value> {
    match arguments {
        [Expr::List(parameters, _), body @ ..] if !body.is_empty() => {
            build_closure(parameters, body, env, None, position)
        }
        _ => Err(error_at(
            "lambda expected a parameter list and body",
            position,
        )),
    }
}

fn build_closure(
    parameter_expressions: &[Expr],
    body: &[Expr],
    env: EnvRef,
    name: Option<String>,
    position: Position,
) -> EvalResult<Value> {
    let mut parameters = Vec::with_capacity(parameter_expressions.len());
    for expression in parameter_expressions {
        match expression {
            Expr::Symbol(name, _) => parameters.push(name.clone()),
            _ => return Err(error_at("lambda parameters must be symbols", position)),
        }
    }

    Ok(Value::Closure {
        params: parameters,
        body: body.to_vec(),
        env,
        name,
    })
}

fn parse_bindings(
    bindings_expression: &Expr,
    position: Position,
    form_name: &str,
) -> EvalResult<Vec<(String, Expr)>> {
    match bindings_expression {
        Expr::List(bindings, _) => bindings
            .iter()
            .map(|binding| parse_binding(binding, form_name))
            .collect(),
        _ => Err(error_at(
            format!("{form_name} expected a binding list"),
            position,
        )),
    }
}

fn parse_binding(binding: &Expr, form_name: &str) -> EvalResult<(String, Expr)> {
    match binding {
        Expr::List(items, _) => match items.as_slice() {
            [Expr::Symbol(name, _), value_expression] => {
                Ok((name.clone(), value_expression.clone()))
            }
            _ => Err(error_at(
                format!("{form_name} expected bindings of the form (name expr)"),
                binding.position(),
            )),
        },
        _ => Err(error_at(
            format!("{form_name} expected bindings of the form (name expr)"),
            binding.position(),
        )),
    }
}

struct RecordTypeDefinition {
    type_name: String,
    constructor_name: String,
    constructor_field_indices: Vec<usize>,
    predicate_name: String,
    field_specs: Vec<RecordFieldSpec>,
}

struct RecordFieldSpec {
    field_name: String,
    accessor_name: Option<String>,
}

fn parse_record_type_definition(
    arguments: &[Expr],
    position: Position,
) -> EvalResult<RecordTypeDefinition> {
    let [type_name_expression, constructor_expression, predicate_expression, field_expressions @ ..] =
        arguments
    else {
        return Err(error_at(
            "define-record-type expected a type name, constructor, predicate, and field specifications",
            position,
        ));
    };

    let type_name = expect_symbol_expression(
        type_name_expression,
        "define-record-type expected a record type name",
    )?
    .to_string();
    let (constructor_name, constructor_field_names) =
        parse_record_constructor_spec(constructor_expression)?;
    let predicate_name = expect_symbol_expression(
        predicate_expression,
        "define-record-type expected a predicate name",
    )?
    .to_string();
    let field_specs = field_expressions
        .iter()
        .map(parse_record_field_spec)
        .collect::<EvalResult<Vec<_>>>()?;

    let mut field_indices_by_name = HashMap::with_capacity(field_specs.len());
    for (index, field_spec) in field_specs.iter().enumerate() {
        if field_indices_by_name
            .insert(field_spec.field_name.clone(), index)
            .is_some()
        {
            return Err(error_at(
                format!(
                    "define-record-type field {} is declared more than once",
                    field_spec.field_name
                ),
                position,
            ));
        }
    }

    let mut constructor_field_indices = Vec::with_capacity(constructor_field_names.len());
    for field_name in constructor_field_names {
        let Some(&field_index) = field_indices_by_name.get(&field_name) else {
            return Err(error_at(
                format!("define-record-type constructor field {field_name} is not declared"),
                position,
            ));
        };
        if constructor_field_indices.contains(&field_index) {
            return Err(error_at(
                format!(
                    "define-record-type constructor field {field_name} is declared more than once"
                ),
                position,
            ));
        }
        constructor_field_indices.push(field_index);
    }

    Ok(RecordTypeDefinition {
        type_name,
        constructor_name,
        constructor_field_indices,
        predicate_name,
        field_specs,
    })
}

fn parse_record_constructor_spec(
    constructor_expression: &Expr,
) -> EvalResult<(String, Vec<String>)> {
    let Expr::List(items, position) = constructor_expression else {
        return Err(error_at(
            "define-record-type expected a constructor specification",
            constructor_expression.position(),
        ));
    };

    let Some((constructor_name_expression, field_expressions)) = items.split_first() else {
        return Err(error_at(
            "define-record-type expected a constructor specification",
            *position,
        ));
    };

    let constructor_name = expect_symbol_expression(
        constructor_name_expression,
        "define-record-type expected a constructor name",
    )?
    .to_string();
    let field_names = field_expressions
        .iter()
        .map(|expression| {
            expect_symbol_expression(
                expression,
                "define-record-type constructor fields must be symbols",
            )
            .map(ToString::to_string)
        })
        .collect::<EvalResult<Vec<_>>>()?;

    Ok((constructor_name, field_names))
}

fn parse_record_field_spec(field_expression: &Expr) -> EvalResult<RecordFieldSpec> {
    match field_expression {
        Expr::Symbol(field_name, _) => Ok(RecordFieldSpec {
            field_name: field_name.clone(),
            accessor_name: None,
        }),
        Expr::List(items, position) => match items.as_slice() {
            [Expr::Symbol(field_name, _), Expr::Symbol(accessor_name, _)] => Ok(RecordFieldSpec {
                field_name: field_name.clone(),
                accessor_name: Some(accessor_name.clone()),
            }),
            [Expr::Symbol(field_name, _)] => Ok(RecordFieldSpec {
                field_name: field_name.clone(),
                accessor_name: None,
            }),
            _ => Err(error_at(
                "define-record-type field specifications must be field names or (field accessor)",
                *position,
            )),
        },
        _ => Err(error_at(
            "define-record-type field specifications must be field names or (field accessor)",
            field_expression.position(),
        )),
    }
}

fn expect_symbol_expression<'a>(expression: &'a Expr, message: &str) -> EvalResult<&'a str> {
    match expression {
        Expr::Symbol(name, _) => Ok(name),
        _ => Err(error_at(message, expression.position())),
    }
}

fn quote(expression: &Expr) -> Value {
    match expression {
        Expr::Int(value, _) => Value::Int(*value),
        Expr::Bool(value, _) => Value::Bool(*value),
        Expr::String(value, _) => Value::String(value.clone()),
        Expr::Char(ch, _) => Value::Char(*ch),
        Expr::Symbol(name, _) => Value::Symbol(name.clone()),
        Expr::List(items, _) => build_list(items.iter().map(quote).collect()),
    }
}

fn apply(
    function: Value,
    arguments: Vec<Value>,
    position: Position,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    match function {
        Value::Builtin { function, .. } => function(&arguments, position, context),
        Value::Closure {
            params,
            body,
            env,
            name: _,
        } => apply_closure(&params, &body, env, arguments, position, context),
        Value::RecordConstructor {
            name,
            record_type,
            field_indices,
        } => apply_record_constructor(&name, record_type, &field_indices, arguments, position),
        Value::RecordPredicate { name, record_type } => {
            apply_record_predicate(&name, record_type, &arguments, position)
        }
        Value::RecordAccessor {
            name,
            record_type,
            field_index,
        } => apply_record_accessor(&name, record_type, field_index, &arguments, position),
        other => Err(error_at(
            format!(
                "attempted to call a non-procedure value: {}",
                other.render()
            ),
            position,
        )),
    }
}

fn apply_closure(
    parameters: &[String],
    body: &[Expr],
    closure_env: EnvRef,
    arguments: Vec<Value>,
    position: Position,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    if arguments.len() != parameters.len() {
        return Err(error_at(
            format!(
                "procedure expected {} argument(s), got {}",
                parameters.len(),
                arguments.len()
            ),
            position,
        ));
    }

    let call_env = Environment::child(
        closure_env,
        parameters.iter().cloned().zip(arguments).collect(),
    );
    eval_sequence(body, call_env, context)
}

fn apply_record_constructor(
    name: &str,
    record_type: Rc<RecordType>,
    field_indices: &[usize],
    arguments: Vec<Value>,
    position: Position,
) -> EvalResult<Value> {
    expect_exact(&arguments, field_indices.len(), name, position)?;

    let mut fields = vec![Value::Void; record_type.field_names.len()];
    for (argument, field_index) in arguments.into_iter().zip(field_indices.iter().copied()) {
        fields[field_index] = argument;
    }

    Ok(Value::Record(Rc::new(RecordInstance {
        record_type,
        fields,
    })))
}

fn apply_record_predicate(
    name: &str,
    record_type: Rc<RecordType>,
    arguments: &[Value],
    position: Position,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, name, position)?;
    Ok(Value::Bool(matches!(
        value,
        Value::Record(record) if Rc::ptr_eq(&record.record_type, &record_type)
    )))
}

fn apply_record_accessor(
    name: &str,
    record_type: Rc<RecordType>,
    field_index: usize,
    arguments: &[Value],
    position: Position,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, name, position)?;
    let record = expect_record(value, name, position, &record_type)?;
    Ok(record.fields[field_index].clone())
}

fn root_bindings() -> Vec<(String, Value)> {
    vec![
        builtin("+", add),
        builtin("-", subtract),
        builtin("*", multiply),
        builtin("/", divide),
        builtin("<", less_than),
        builtin(">", greater_than),
        builtin("=", numeric_equal),
        builtin("<=", less_equal),
        builtin("not", logical_not),
        builtin("cons", cons),
        builtin("car", car),
        builtin("cdr", cdr),
        builtin("null?", is_null),
        builtin("list", list),
        builtin("length", length),
        builtin("append", append),
        builtin("string?", is_string),
        builtin("number?", is_number),
        builtin("boolean?", is_boolean),
        builtin("pair?", is_pair),
        builtin("symbol?", is_symbol),
        builtin("display", display),
        builtin("write", write),
        builtin("newline", newline),
        builtin("string-append", string_append),
        builtin("string-length", string_length),
        builtin("substring", substring),
        builtin("string->number", string_to_number),
        builtin("number->string", number_to_string),
        builtin("symbol->string", symbol_to_string),
        builtin("string->symbol", string_to_symbol),
        builtin("string-ref", string_ref),
        builtin("char?", is_char),
        builtin("abs", abs),
        builtin("modulo", modulo),
        builtin("remainder", remainder),
        builtin("quotient", quotient),
        builtin("min", min),
        builtin("max", max),
        builtin("expt", expt),
        builtin("zero?", is_zero),
        builtin("positive?", is_positive),
        builtin("negative?", is_negative),
        builtin("odd?", is_odd),
        builtin("even?", is_even),
        builtin("list-ref", list_ref),
        builtin("list-tail", list_tail),
        builtin("list?", is_list),
        builtin("eq?", is_eq),
        builtin("equal?", is_equal),
        builtin("assoc", assoc),
        builtin("map", map),
        builtin("char-alphabetic?", is_char_alphabetic),
        builtin("char-numeric?", is_char_numeric),
        builtin("char-upcase", char_upcase),
        builtin("char-downcase", char_downcase),
        builtin("char=?", char_equal),
        builtin("char<?", char_less_than),
        builtin("string=?", string_equal),
        builtin("string<?", string_less_than),
        builtin("string-ci=?", string_ci_equal),
        builtin("string-upcase", string_upcase),
        builtin("string-downcase", string_downcase),
    ]
}

fn builtin(name: &'static str, function: BuiltinFn) -> (String, Value) {
    (name.to_string(), Value::Builtin { name, function })
}

fn add(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let numbers = numeric_arguments(arguments, "+", position)?;
    Ok(Value::Int(numbers.into_iter().sum()))
}

fn subtract(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let numbers = numeric_arguments_at_least(arguments, 1, "-", position)?;
    let result = if numbers.len() == 1 {
        -numbers[0]
    } else {
        let first = numbers[0];
        numbers[1..]
            .iter()
            .fold(first, |accumulator, number| accumulator - number)
    };
    Ok(Value::Int(result))
}

fn multiply(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let numbers = numeric_arguments(arguments, "*", position)?;
    Ok(Value::Int(numbers.into_iter().product()))
}

fn divide(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let numbers = numeric_arguments_at_least(arguments, 1, "/", position)?;
    let result = if numbers.len() == 1 {
        divide_exactly(1, numbers[0], position)?
    } else {
        let mut value = numbers[0];
        for denominator in &numbers[1..] {
            value = divide_exactly(value, *denominator, position)?;
        }
        value
    };
    Ok(Value::Int(result))
}

fn divide_exactly(numerator: i128, denominator: i128, position: Position) -> EvalResult<i128> {
    if denominator == 0 {
        return Err(error_at("division by zero", position));
    }
    if numerator % denominator != 0 {
        return Err(error_at("division produced a non-integer result", position));
    }
    Ok(numerator / denominator)
}

fn less_than(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_numbers(arguments, "<", position, |left, right| left < right)
}

fn greater_than(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_numbers(arguments, ">", position, |left, right| left > right)
}

fn numeric_equal(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_numbers(arguments, "=", position, |left, right| left == right)
}

fn less_equal(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_numbers(arguments, "<=", position, |left, right| left <= right)
}

fn compare_numbers(
    arguments: &[Value],
    name: &str,
    position: Position,
    predicate: impl Fn(i128, i128) -> bool,
) -> EvalResult<Value> {
    let numbers = numeric_arguments_at_least(arguments, 2, name, position)?;
    let result = numbers
        .windows(2)
        .all(|window| predicate(window[0], window[1]));
    Ok(Value::Bool(result))
}

fn logical_not(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "not", position)?;
    Ok(Value::Bool(!is_truthy(value)))
}

fn cons(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let (car, cdr) = expect_two_arguments(arguments, "cons", position)?;
    Ok(Value::Pair(Box::new(car.clone()), Box::new(cdr.clone())))
}

fn car(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "car", position)?;
    let (car, _) = expect_pair(value, "car", position)?;
    Ok(car.clone())
}

fn cdr(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "cdr", position)?;
    let (_, cdr) = expect_pair(value, "cdr", position)?;
    Ok(cdr.clone())
}

fn is_null(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "null?", position)?;
    Ok(Value::Bool(matches!(value, Value::EmptyList)))
}

fn list(arguments: &[Value], _position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    Ok(build_list(arguments.to_vec()))
}

fn length(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "length", position)?;
    Ok(Value::Int(
        expect_proper_list(value, "length", position)?.len() as i128,
    ))
}

fn append(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let mut elements = Vec::new();
    for argument in arguments {
        elements.extend(expect_proper_list(argument, "append", position)?);
    }
    Ok(build_list(elements))
}

fn is_string(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "string?", position, |value| {
        matches!(value, Value::String(_))
    })
}

fn is_number(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "number?", position, |value| {
        matches!(value, Value::Int(_))
    })
}

fn is_boolean(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "boolean?", position, |value| {
        matches!(value, Value::Bool(_))
    })
}

fn is_pair(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "pair?", position, |value| {
        matches!(value, Value::Pair(_, _))
    })
}

fn is_symbol(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "symbol?", position, |value| {
        matches!(value, Value::Symbol(_))
    })
}

fn display(
    arguments: &[Value],
    position: Position,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "display", position)?;
    context.write(&value.display());
    Ok(Value::Void)
}

fn write(arguments: &[Value], position: Position, context: &mut EvalContext) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "write", position)?;
    context.write(&value.render());
    Ok(Value::Void)
}

fn newline(
    arguments: &[Value],
    position: Position,
    context: &mut EvalContext,
) -> EvalResult<Value> {
    expect_exact(arguments, 0, "newline", position)?;
    context.write("\n");
    Ok(Value::Void)
}

fn string_append(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let mut result = String::new();
    for argument in arguments {
        result.push_str(expect_string(argument, "string-append", position)?);
    }
    Ok(Value::String(result))
}

fn string_length(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "string-length", position)?;
    Ok(Value::Int(
        expect_string(value, "string-length", position)?
            .chars()
            .count() as i128,
    ))
}

fn substring(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    expect_exact(arguments, 3, "substring", position)?;
    let text = expect_string(&arguments[0], "substring", position)?;
    let start = expect_index(&arguments[1], "substring", position)?;
    let end = expect_index(&arguments[2], "substring", position)?;
    let chars: Vec<char> = text.chars().collect();
    if start > end || end > chars.len() {
        return Err(error_at("substring indices out of range", position));
    }
    Ok(Value::String(chars[start..end].iter().collect()))
}

fn string_to_number(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "string->number", position)?;
    let text = expect_string(value, "string->number", position)?;
    if is_integer_token(text) {
        match text.parse::<i128>() {
            Ok(number) => Ok(Value::Int(number)),
            Err(_) => Ok(Value::Bool(false)),
        }
    } else {
        Ok(Value::Bool(false))
    }
}

fn number_to_string(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "number->string", position)?;
    Ok(Value::String(
        expect_number(value, "number->string", position)?.to_string(),
    ))
}

fn symbol_to_string(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "symbol->string", position)?;
    Ok(Value::String(
        expect_symbol(value, "symbol->string", position)?.to_string(),
    ))
}

fn string_to_symbol(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "string->symbol", position)?;
    Ok(Value::Symbol(
        expect_string(value, "string->symbol", position)?.to_string(),
    ))
}

fn string_ref(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    expect_exact(arguments, 2, "string-ref", position)?;
    let text = expect_string(&arguments[0], "string-ref", position)?;
    let index = expect_index(&arguments[1], "string-ref", position)?;
    let chars: Vec<char> = text.chars().collect();
    if index >= chars.len() {
        return Err(error_at("string-ref index out of range", position));
    }
    Ok(Value::Char(chars[index]))
}

fn is_char(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "char?", position, |value| {
        matches!(value, Value::Char(_))
    })
}

fn abs(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "abs", position)?;
    let number = expect_number(value, "abs", position)?;
    let result = number
        .checked_abs()
        .ok_or_else(|| error_at("abs overflowed", position))?;
    Ok(Value::Int(result))
}

fn modulo(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let (left, right) = expect_two_arguments(arguments, "modulo", position)?;
    let dividend = expect_number(left, "modulo", position)?;
    let divisor = expect_number(right, "modulo", position)?;
    ensure_non_zero_divisor("modulo", divisor, position)?;

    let remainder = dividend % divisor;
    let result = if remainder == 0 || same_sign(remainder, divisor) {
        remainder
    } else {
        remainder + divisor
    };
    Ok(Value::Int(result))
}

fn remainder(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let (left, right) = expect_two_arguments(arguments, "remainder", position)?;
    let dividend = expect_number(left, "remainder", position)?;
    let divisor = expect_number(right, "remainder", position)?;
    ensure_non_zero_divisor("remainder", divisor, position)?;
    Ok(Value::Int(dividend % divisor))
}

fn quotient(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let (left, right) = expect_two_arguments(arguments, "quotient", position)?;
    let dividend = expect_number(left, "quotient", position)?;
    let divisor = expect_number(right, "quotient", position)?;
    ensure_non_zero_divisor("quotient", divisor, position)?;
    Ok(Value::Int(dividend / divisor))
}

fn min(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let numbers = numeric_arguments_at_least(arguments, 1, "min", position)?;
    Ok(Value::Int(
        *numbers
            .iter()
            .min()
            .expect("min requires at least one number"),
    ))
}

fn max(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let numbers = numeric_arguments_at_least(arguments, 1, "max", position)?;
    Ok(Value::Int(
        *numbers
            .iter()
            .max()
            .expect("max requires at least one number"),
    ))
}

fn expt(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let (base_value, exponent_value) = expect_two_arguments(arguments, "expt", position)?;
    let base = expect_number(base_value, "expt", position)?;
    let exponent = expect_number(exponent_value, "expt", position)?;
    let exponent = u32::try_from(exponent)
        .map_err(|_| error_at("expt expected a non-negative exponent", position))?;
    let result = base
        .checked_pow(exponent)
        .ok_or_else(|| error_at("expt overflowed", position))?;
    Ok(Value::Int(result))
}

fn is_zero(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    numeric_unary_predicate(arguments, "zero?", position, |number| number == 0)
}

fn is_positive(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    numeric_unary_predicate(arguments, "positive?", position, |number| number > 0)
}

fn is_negative(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    numeric_unary_predicate(arguments, "negative?", position, |number| number < 0)
}

fn is_odd(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    numeric_unary_predicate(arguments, "odd?", position, |number| number % 2 != 0)
}

fn is_even(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    numeric_unary_predicate(arguments, "even?", position, |number| number % 2 == 0)
}

fn list_ref(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    expect_exact(arguments, 2, "list-ref", position)?;
    let index = expect_index(&arguments[1], "list-ref", position)?;
    let mut current = &arguments[0];
    let mut remaining = index;

    loop {
        match current {
            Value::Pair(head, tail) => {
                if remaining == 0 {
                    return Ok((**head).clone());
                }
                remaining -= 1;
                current = tail.as_ref();
            }
            _ => return Err(error_at("list-ref index out of range", position)),
        }
    }
}

fn list_tail(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    expect_exact(arguments, 2, "list-tail", position)?;
    let mut remaining = expect_index(&arguments[1], "list-tail", position)?;
    let mut current = &arguments[0];

    while remaining > 0 {
        match current {
            Value::Pair(_, tail) => {
                current = tail.as_ref();
                remaining -= 1;
            }
            _ => return Err(error_at("list-tail index out of range", position)),
        }
    }

    Ok(current.clone())
}

fn is_list(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    unary_predicate(arguments, "list?", position, is_proper_list)
}

fn is_eq(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let (left, right) = expect_two_arguments(arguments, "eq?", position)?;
    Ok(Value::Bool(eq_values(left, right)))
}

fn is_equal(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let (left, right) = expect_two_arguments(arguments, "equal?", position)?;
    Ok(Value::Bool(equal_values(left, right)))
}

fn assoc(arguments: &[Value], position: Position, _context: &mut EvalContext) -> EvalResult<Value> {
    let (key, list) = expect_two_arguments(arguments, "assoc", position)?;
    let mut current = list;

    loop {
        match current {
            Value::EmptyList => return Ok(Value::Bool(false)),
            Value::Pair(entry, tail) => {
                let (entry_key, _) = expect_pair(entry, "assoc", position)?;
                if equal_values(key, entry_key) {
                    return Ok((**entry).clone());
                }
                current = tail.as_ref();
            }
            _ => return Err(error_at("assoc expected an association list", position)),
        }
    }
}

fn map(arguments: &[Value], position: Position, context: &mut EvalContext) -> EvalResult<Value> {
    expect_at_least(arguments, 2, "map", position)?;

    let procedure = arguments[0].clone();
    let lists: Vec<Vec<Value>> = arguments[1..]
        .iter()
        .map(|value| expect_proper_list(value, "map", position))
        .collect::<EvalResult<_>>()?;

    let expected_len = lists[0].len();
    if lists.iter().any(|list| list.len() != expected_len) {
        return Err(error_at("map expected lists of equal length", position));
    }

    let mut results = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        let call_arguments = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply(procedure.clone(), call_arguments, position, context)?);
    }

    Ok(build_list(results))
}

fn is_char_alphabetic(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    char_unary_predicate(arguments, "char-alphabetic?", position, |ch| {
        ch.is_alphabetic()
    })
}

fn is_char_numeric(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    char_unary_predicate(arguments, "char-numeric?", position, |ch| ch.is_numeric())
}

fn char_upcase(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "char-upcase", position)?;
    Ok(Value::Char(uppercase_char(expect_char(
        value,
        "char-upcase",
        position,
    )?)))
}

fn char_downcase(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "char-downcase", position)?;
    Ok(Value::Char(lowercase_char(expect_char(
        value,
        "char-downcase",
        position,
    )?)))
}

fn char_equal(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_characters(arguments, "char=?", position, |left, right| left == right)
}

fn char_less_than(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_characters(arguments, "char<?", position, |left, right| left < right)
}

fn string_equal(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_strings(arguments, "string=?", position, |left, right| left == right)
}

fn string_less_than(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_strings(arguments, "string<?", position, |left, right| left < right)
}

fn string_ci_equal(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    compare_strings(arguments, "string-ci=?", position, |left, right| {
        left.to_lowercase() == right.to_lowercase()
    })
}

fn string_upcase(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "string-upcase", position)?;
    Ok(Value::String(
        expect_string(value, "string-upcase", position)?
            .chars()
            .flat_map(char::to_uppercase)
            .collect(),
    ))
}

fn string_downcase(
    arguments: &[Value],
    position: Position,
    _context: &mut EvalContext,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, "string-downcase", position)?;
    Ok(Value::String(
        expect_string(value, "string-downcase", position)?
            .chars()
            .flat_map(char::to_lowercase)
            .collect(),
    ))
}

fn unary_predicate(
    arguments: &[Value],
    name: &str,
    position: Position,
    predicate: impl Fn(&Value) -> bool,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, name, position)?;
    Ok(Value::Bool(predicate(value)))
}

fn build_list(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::EmptyList, |tail, head| {
            Value::Pair(Box::new(head), Box::new(tail))
        })
}

fn expect_exact(
    arguments: &[Value],
    expected: usize,
    name: &str,
    position: Position,
) -> EvalResult<()> {
    if arguments.len() != expected {
        return Err(error_at(
            format!(
                "{name} expected {expected} argument(s), got {}",
                arguments.len()
            ),
            position,
        ));
    }
    Ok(())
}

fn expect_at_least(
    arguments: &[Value],
    minimum: usize,
    name: &str,
    position: Position,
) -> EvalResult<()> {
    if arguments.len() < minimum {
        return Err(error_at(
            format!(
                "{name} expected at least {minimum} argument(s), got {}",
                arguments.len()
            ),
            position,
        ));
    }
    Ok(())
}

fn expect_single_argument<'a>(
    arguments: &'a [Value],
    name: &str,
    position: Position,
) -> EvalResult<&'a Value> {
    expect_exact(arguments, 1, name, position)?;
    Ok(&arguments[0])
}

fn expect_two_arguments<'a>(
    arguments: &'a [Value],
    name: &str,
    position: Position,
) -> EvalResult<(&'a Value, &'a Value)> {
    expect_exact(arguments, 2, name, position)?;
    Ok((&arguments[0], &arguments[1]))
}

fn expect_number(value: &Value, name: &str, position: Position) -> EvalResult<i128> {
    match value {
        Value::Int(number) => Ok(*number),
        other => Err(error_at(
            format!("{name} expected a number, got {}", other.type_name()),
            position,
        )),
    }
}

fn numeric_unary_predicate(
    arguments: &[Value],
    name: &str,
    position: Position,
    predicate: impl Fn(i128) -> bool,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, name, position)?;
    Ok(Value::Bool(predicate(expect_number(
        value, name, position,
    )?)))
}

fn char_unary_predicate(
    arguments: &[Value],
    name: &str,
    position: Position,
    predicate: impl Fn(char) -> bool,
) -> EvalResult<Value> {
    let value = expect_single_argument(arguments, name, position)?;
    Ok(Value::Bool(predicate(expect_char(value, name, position)?)))
}

fn expect_string<'a>(value: &'a Value, name: &str, position: Position) -> EvalResult<&'a str> {
    match value {
        Value::String(text) => Ok(text),
        other => Err(error_at(
            format!("{name} expected a string, got {}", other.type_name()),
            position,
        )),
    }
}

fn expect_char(value: &Value, name: &str, position: Position) -> EvalResult<char> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(error_at(
            format!("{name} expected a character, got {}", other.type_name()),
            position,
        )),
    }
}

fn expect_symbol<'a>(value: &'a Value, name: &str, position: Position) -> EvalResult<&'a str> {
    match value {
        Value::Symbol(name_value) => Ok(name_value),
        other => Err(error_at(
            format!("{name} expected a symbol, got {}", other.type_name()),
            position,
        )),
    }
}

fn expect_index(value: &Value, name: &str, position: Position) -> EvalResult<usize> {
    let number = expect_number(value, name, position)?;
    if number < 0 {
        return Err(error_at(
            format!("{name} expected a non-negative index, got {number}"),
            position,
        ));
    }
    usize::try_from(number).map_err(|_| {
        error_at(
            format!("{name} expected a non-negative index, got {number}"),
            position,
        )
    })
}

fn expect_pair<'a>(
    value: &'a Value,
    name: &str,
    position: Position,
) -> EvalResult<(&'a Value, &'a Value)> {
    match value {
        Value::Pair(car, cdr) => Ok((car.as_ref(), cdr.as_ref())),
        other => Err(error_at(
            format!("{name} expected a pair, got {}", other.type_name()),
            position,
        )),
    }
}

fn expect_record<'a>(
    value: &'a Value,
    name: &str,
    position: Position,
    record_type: &Rc<RecordType>,
) -> EvalResult<&'a RecordInstance> {
    match value {
        Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type) => {
            Ok(record.as_ref())
        }
        Value::Record(_) => Err(error_at(
            format!("{name} expected a {}", record_type.name),
            position,
        )),
        other => Err(error_at(
            format!(
                "{name} expected a {}, got {}",
                record_type.name,
                other.type_name()
            ),
            position,
        )),
    }
}

fn expect_proper_list(value: &Value, name: &str, position: Position) -> EvalResult<Vec<Value>> {
    let mut result = Vec::new();
    let mut current = value;
    loop {
        match current {
            Value::EmptyList => return Ok(result),
            Value::Pair(head, tail) => {
                result.push((**head).clone());
                current = tail.as_ref();
            }
            other => {
                return Err(error_at(
                    format!("{name} expected a proper list, got {}", other.type_name()),
                    position,
                ))
            }
        }
    }
}

fn numeric_arguments(arguments: &[Value], name: &str, position: Position) -> EvalResult<Vec<i128>> {
    arguments
        .iter()
        .map(|value| expect_number(value, name, position))
        .collect()
}

fn numeric_arguments_at_least(
    arguments: &[Value],
    minimum: usize,
    name: &str,
    position: Position,
) -> EvalResult<Vec<i128>> {
    expect_at_least(arguments, minimum, name, position)?;
    numeric_arguments(arguments, name, position)
}

fn compare_characters(
    arguments: &[Value],
    name: &str,
    position: Position,
    predicate: impl Fn(char, char) -> bool,
) -> EvalResult<Value> {
    expect_at_least(arguments, 2, name, position)?;
    let characters = arguments
        .iter()
        .map(|value| expect_char(value, name, position))
        .collect::<EvalResult<Vec<_>>>()?;
    Ok(Value::Bool(
        characters
            .windows(2)
            .all(|window| predicate(window[0], window[1])),
    ))
}

fn compare_strings(
    arguments: &[Value],
    name: &str,
    position: Position,
    predicate: impl Fn(&str, &str) -> bool,
) -> EvalResult<Value> {
    expect_at_least(arguments, 2, name, position)?;
    let strings = arguments
        .iter()
        .map(|value| expect_string(value, name, position))
        .collect::<EvalResult<Vec<_>>>()?;
    Ok(Value::Bool(
        strings
            .windows(2)
            .all(|window| predicate(window[0], window[1])),
    ))
}

fn ensure_non_zero_divisor(name: &str, divisor: i128, position: Position) -> EvalResult<()> {
    if divisor == 0 {
        return Err(error_at(format!("{name} division by zero"), position));
    }
    Ok(())
}

fn same_sign(left: i128, right: i128) -> bool {
    left.is_negative() == right.is_negative()
}

fn is_proper_list(value: &Value) -> bool {
    let mut current = value;
    loop {
        match current {
            Value::EmptyList => return true,
            Value::Pair(_, tail) => current = tail.as_ref(),
            _ => return false,
        }
    }
}

fn eq_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => left == right,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Builtin { name: left, .. }, Value::Builtin { name: right, .. }) => left == right,
        (
            Value::RecordConstructor {
                name: left_name,
                record_type: left_type,
                field_indices: left_fields,
            },
            Value::RecordConstructor {
                name: right_name,
                record_type: right_type,
                field_indices: right_fields,
            },
        ) => {
            left_name == right_name
                && left_fields == right_fields
                && Rc::ptr_eq(left_type, right_type)
        }
        (
            Value::RecordPredicate {
                name: left_name,
                record_type: left_type,
            },
            Value::RecordPredicate {
                name: right_name,
                record_type: right_type,
            },
        ) => left_name == right_name && Rc::ptr_eq(left_type, right_type),
        (
            Value::RecordAccessor {
                name: left_name,
                record_type: left_type,
                field_index: left_index,
            },
            Value::RecordAccessor {
                name: right_name,
                record_type: right_type,
                field_index: right_index,
            },
        ) => {
            left_name == right_name
                && left_index == right_index
                && Rc::ptr_eq(left_type, right_type)
        }
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => left == right,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_head, left_tail), Value::Pair(right_head, right_tail)) => {
            equal_values(left_head, right_head) && equal_values(left_tail, right_tail)
        }
        (Value::Builtin { name: left, .. }, Value::Builtin { name: right, .. }) => left == right,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn uppercase_char(ch: char) -> char {
    ch.to_uppercase().next().unwrap_or(ch)
}

fn lowercase_char(ch: char) -> char {
    ch.to_lowercase().next().unwrap_or(ch)
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn render_value(value: &Value, display_mode: bool) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Bool(boolean) => {
            if *boolean {
                "#t".to_string()
            } else {
                "#f".to_string()
            }
        }
        Value::String(text) => {
            if display_mode {
                text.clone()
            } else {
                render_string(text)
            }
        }
        Value::Char(ch) => {
            if display_mode {
                ch.to_string()
            } else {
                render_char(*ch)
            }
        }
        Value::Symbol(name) => name.clone(),
        Value::EmptyList => "()".to_string(),
        Value::Pair(_, _) => format!("({})", render_pair_contents(value, display_mode)),
        Value::Builtin { name, .. } => format!("#<procedure:{name}>"),
        Value::Closure { name, .. } => match name {
            Some(name) => format!("#<procedure:{name}>"),
            None => "#<procedure:lambda>".to_string(),
        },
        Value::Record(record) => format!("#<record:{}>", record.record_type.name),
        Value::RecordConstructor { name, .. }
        | Value::RecordPredicate { name, .. }
        | Value::RecordAccessor { name, .. } => format!("#<procedure:{name}>"),
        Value::Void => "#<void>".to_string(),
        Value::Uninitialized => "#<uninitialized>".to_string(),
    }
}

fn render_pair_contents(value: &Value, display_mode: bool) -> String {
    match value {
        Value::Pair(head, tail) => {
            let head_rendered = render_value(head, display_mode);
            match tail.as_ref() {
                Value::EmptyList => head_rendered,
                Value::Pair(_, _) => {
                    format!(
                        "{head_rendered} {}",
                        render_pair_contents(tail, display_mode)
                    )
                }
                other => format!("{head_rendered} . {}", render_value(other, display_mode)),
            }
        }
        other => panic!("expected pair while rendering pair, got {}", other.render()),
    }
}

fn render_string(text: &str) -> String {
    let mut rendered = String::with_capacity(text.len() + 2);
    rendered.push('"');
    for ch in text.chars() {
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

fn render_char(ch: char) -> String {
    match ch {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

fn is_integer_token(token: &str) -> bool {
    let body = token
        .strip_prefix('+')
        .or_else(|| token.strip_prefix('-'))
        .unwrap_or(token);
    !body.is_empty() && body.chars().all(|ch| ch.is_ascii_digit())
}

fn error_at(message: impl Into<String>, position: Position) -> EvalError {
    EvalError::at(message, position.line, position.column)
}
