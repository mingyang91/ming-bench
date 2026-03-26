use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod error;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    Evaluator::new().eval_str(input)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

struct Evaluator {
    global_env: Rc<Env>,
}

impl Evaluator {
    fn new() -> Self {
        Self {
            global_env: create_global_env(),
        }
    }

    fn eval_str(&self, input: &str) -> Result<String, EvalError> {
        let expressions = Parser::new(input).parse_program()?;
        if expressions.is_empty() {
            return Err(EvalError::with_position(
                "input did not contain any expressions",
                1,
                1,
            ));
        }

        let mut result = Value::Void;
        for expression in &expressions {
            result = self.eval(expression, self.global_env.clone())?;
        }
        Ok(render(&result))
    }

    fn eval(&self, expr: &Expr, env: Rc<Env>) -> Result<Value, EvalError> {
        let result = match &expr.kind {
            ExprKind::Int(value) => Ok(Value::Int(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::String(value) => Ok(Value::String(value.clone())),
            ExprKind::Symbol(name) => env.lookup(name),
            ExprKind::List(elements) => self.eval_list(elements, env),
        };

        result.map_err(|error| ensure_position(error, expr.position))
    }

    fn eval_list(&self, elements: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if elements.is_empty() {
            return Err(EvalError::message("cannot evaluate an empty list"));
        }

        let operator_expr = &elements[0];
        let arguments = &elements[1..];

        if let ExprKind::Symbol(name) = &operator_expr.kind {
            return match name.as_str() {
                "define" => self.eval_define(arguments, env),
                "if" => self.eval_if(arguments, env),
                "quote" => self.eval_quote(arguments),
                "lambda" => self.eval_lambda(arguments, env),
                "and" => self.eval_and(arguments, env),
                "or" => self.eval_or(arguments, env),
                "begin" => self.eval_begin(arguments, env),
                "let" => self.eval_let(arguments, env),
                "cond" => self.eval_cond(arguments, env),
                _ => {
                    let operator = self.eval(operator_expr, env.clone())?;
                    let values = self.eval_all(arguments, env)?;
                    self.apply_procedure(operator, values)
                }
            };
        }

        let operator = self.eval(operator_expr, env.clone())?;
        let values = self.eval_all(arguments, env)?;
        self.apply_procedure(operator, values)
    }

    fn eval_define(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::message("define expects a name and a value"));
        }

        match &arguments[0].kind {
            ExprKind::Symbol(name) => {
                if arguments.len() != 2 {
                    return Err(EvalError::message(
                        "define expects exactly one value expression",
                    ));
                }

                let binding = env.define_placeholder(name.clone());
                let value = self.eval(&arguments[1], env)?;
                *binding.borrow_mut() = value;
                Ok(Value::Void)
            }
            ExprKind::List(signature) => {
                if signature.is_empty() {
                    return Err(EvalError::message("define function name must be a symbol"));
                }

                let ExprKind::Symbol(name) = &signature[0].kind else {
                    return Err(EvalError::message("define function name must be a symbol"));
                };

                if arguments.len() < 2 {
                    return Err(EvalError::message("define requires a function body"));
                }

                let binding = env.define_placeholder(name.clone());
                let closure = Value::Closure(Rc::new(ClosureValue {
                    parameters: parse_parameter_names_list(&signature[1..])?,
                    body: arguments[1..].to_vec(),
                    env: env.clone(),
                }));
                *binding.borrow_mut() = closure;
                Ok(Value::Void)
            }
            _ => Err(EvalError::message(
                "define target must be a symbol or parameter list",
            )),
        }
    }

    fn eval_if(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        require_exact_args("if", arguments.len(), 3)?;
        let condition = self.eval(&arguments[0], env.clone())?;
        if is_truthy(&condition) {
            self.eval(&arguments[1], env)
        } else {
            self.eval(&arguments[2], env)
        }
    }

    fn eval_quote(&self, arguments: &[Expr]) -> Result<Value, EvalError> {
        require_exact_args("quote", arguments.len(), 1)?;
        Ok(quote_to_value(&arguments[0]))
    }

    fn eval_lambda(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::message("lambda requires parameters and a body"));
        }

        Ok(Value::Closure(Rc::new(ClosureValue {
            parameters: parse_parameter_names(&arguments[0])?,
            body: arguments[1..].to_vec(),
            env,
        })))
    }

    fn eval_all(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(arguments.len());
        for argument in arguments {
            values.push(self.eval(argument, env.clone())?);
        }
        Ok(values)
    }

    fn eval_and(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        let mut last = Value::Bool(true);
        for argument in arguments {
            last = self.eval(argument, env.clone())?;
            if !is_truthy(&last) {
                return Ok(last);
            }
        }
        Ok(last)
    }

    fn eval_or(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        let mut last = Value::Bool(false);
        for argument in arguments {
            last = self.eval(argument, env.clone())?;
            if is_truthy(&last) {
                return Ok(last);
            }
        }
        Ok(last)
    }

    fn eval_begin(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        self.eval_sequence(arguments, env)
    }

    fn eval_let(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Err(EvalError::message("let requires bindings and a body"));
        }

        if let ExprKind::Symbol(name) = &arguments[0].kind {
            if arguments.len() < 3 {
                return Err(EvalError::message(
                    "named let requires bindings and a body",
                ));
            }

            return self.eval_named_let(name, &arguments[1], &arguments[2..], env);
        }

        if arguments.len() < 2 {
            return Err(EvalError::message("let requires bindings and a body"));
        }

        let bindings = parse_bindings(&arguments[0])?;
        let let_env = Env::new(Some(env.clone()));
        let values = self.eval_binding_values(&bindings, env)?;
        bind_values(&let_env, &bindings, values);
        self.eval_sequence(&arguments[1..], let_env)
    }

    fn eval_named_let(
        &self,
        name: &str,
        binding_expr: &Expr,
        body: &[Expr],
        env: Rc<Env>,
    ) -> Result<Value, EvalError> {
        let bindings = parse_bindings(binding_expr)?;
        let let_env = Env::new(Some(env.clone()));
        let binding = let_env.define_placeholder(name.to_string());
        let closure = Value::Closure(Rc::new(ClosureValue {
            parameters: binding_names(&bindings),
            body: body.to_vec(),
            env: let_env.clone(),
        }));
        *binding.borrow_mut() = closure.clone();
        let arguments = self.eval_binding_values(&bindings, env)?;
        self.apply_procedure(closure, arguments)
    }

    fn eval_binding_values(
        &self,
        bindings: &[BindingSpec],
        env: Rc<Env>,
    ) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(bindings.len());
        for binding in bindings {
            values.push(self.eval(&binding.init_expr, env.clone())?);
        }
        Ok(values)
    }

    fn eval_cond(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        for (index, clause_expr) in arguments.iter().enumerate() {
            let ExprKind::List(elements) = &clause_expr.kind else {
                return Err(EvalError::message("cond clause must be a non-empty list"));
            };

            if elements.is_empty() {
                return Err(EvalError::message("cond clause must be a non-empty list"));
            }

            let test_expr = &elements[0];
            if let ExprKind::Symbol(name) = &test_expr.kind {
                if name == "else" {
                    if index != arguments.len() - 1 {
                        return Err(EvalError::message("cond else clause must be last"));
                    }
                    if elements.len() == 1 {
                        return Ok(Value::Bool(true));
                    }
                    return self.eval_sequence(&elements[1..], env);
                }
            }

            let test_value = self.eval(test_expr, env.clone())?;
            if is_truthy(&test_value) {
                if elements.len() == 1 {
                    return Ok(test_value);
                }
                return self.eval_sequence(&elements[1..], env);
            }
        }

        Ok(Value::Void)
    }

    fn apply_procedure(&self, operator: Value, arguments: Vec<Value>) -> Result<Value, EvalError> {
        match operator {
            Value::Builtin(builtin) => (builtin.implementation)(&arguments),
            Value::Closure(closure) => self.apply_closure(&closure, arguments),
            _ => Err(EvalError::message("attempted to call a non-procedure")),
        }
    }

    fn apply_closure(
        &self,
        closure: &ClosureValue,
        arguments: Vec<Value>,
    ) -> Result<Value, EvalError> {
        require_exact_args("lambda", arguments.len(), closure.parameters.len())?;

        let call_env = Env::new(Some(closure.env.clone()));
        for (name, value) in closure.parameters.iter().zip(arguments) {
            call_env.define(name.clone(), value);
        }
        self.eval_sequence(&closure.body, call_env)
    }

    fn eval_sequence(&self, expressions: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expression in expressions {
            result = self.eval(expression, env.clone())?;
        }
        Ok(result)
    }
}

fn create_global_env() -> Rc<Env> {
    let env = Env::new(None);
    env.define_builtin("+", builtin_add);
    env.define_builtin("-", builtin_subtract);
    env.define_builtin("*", builtin_multiply);
    env.define_builtin("/", builtin_divide);
    env.define_builtin("<", builtin_less_than);
    env.define_builtin(">", builtin_greater_than);
    env.define_builtin("=", builtin_equal);
    env.define_builtin("<=", builtin_less_equal);
    env.define_builtin("not", builtin_not);
    env.define_builtin("cons", builtin_cons);
    env.define_builtin("car", builtin_car);
    env.define_builtin("cdr", builtin_cdr);
    env.define_builtin("null?", builtin_is_null);
    env.define_builtin("list", builtin_list);
    env.define_builtin("length", builtin_length);
    env.define_builtin("append", builtin_append);
    env.define_builtin("string?", builtin_is_string);
    env.define_builtin("number?", builtin_is_number);
    env.define_builtin("boolean?", builtin_is_boolean);
    env.define_builtin("pair?", builtin_is_pair);
    env.define_builtin("symbol?", builtin_is_symbol);
    env
}

fn bind_values(env: &Rc<Env>, bindings: &[BindingSpec], values: Vec<Value>) {
    for (binding, value) in bindings.iter().zip(values) {
        env.define(binding.name.clone(), value);
    }
}

fn binding_names(bindings: &[BindingSpec]) -> Vec<String> {
    bindings.iter().map(|binding| binding.name.clone()).collect()
}

fn parse_bindings(binding_expr: &Expr) -> Result<Vec<BindingSpec>, EvalError> {
    let ExprKind::List(binding_list) = &binding_expr.kind else {
        return Err(EvalError::message("let bindings must be a list"));
    };

    let mut bindings = Vec::with_capacity(binding_list.len());
    for entry_expr in binding_list {
        let ExprKind::List(entry) = &entry_expr.kind else {
            return Err(EvalError::message(
                "let binding must contain a name and value",
            ));
        };

        if entry.len() != 2 {
            return Err(EvalError::message(
                "let binding must contain a name and value",
            ));
        }

        let ExprKind::Symbol(name) = &entry[0].kind else {
            return Err(EvalError::message("let binding name must be a symbol"));
        };

        bindings.push(BindingSpec {
            name: name.clone(),
            init_expr: entry[1].clone(),
        });
    }
    Ok(bindings)
}

fn parse_parameter_names(parameter_expr: &Expr) -> Result<Vec<String>, EvalError> {
    let ExprKind::List(parameter_list) = &parameter_expr.kind else {
        return Err(EvalError::message("lambda parameters must be a list"));
    };
    parse_parameter_names_list(parameter_list)
}

fn parse_parameter_names_list(parameter_exprs: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut parameters = Vec::with_capacity(parameter_exprs.len());
    for parameter_expr in parameter_exprs {
        let ExprKind::Symbol(name) = &parameter_expr.kind else {
            return Err(EvalError::message("lambda parameter must be a symbol"));
        };
        parameters.push(name.clone());
    }
    Ok(parameters)
}

fn quote_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Int(value) => Value::Int(*value),
        ExprKind::Bool(value) => Value::Bool(*value),
        ExprKind::String(value) => Value::String(value.clone()),
        ExprKind::Symbol(name) => Value::Symbol(name.clone()),
        ExprKind::List(elements) => list_value(elements.iter().map(quote_to_value).collect()),
    }
}

fn list_value(values: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;
    for value in values.into_iter().rev() {
        result = Value::Pair(Rc::new(PairValue {
            car: value,
            cdr: result,
        }));
    }
    result
}

fn ensure_position(error: EvalError, position: SourceLoc) -> EvalError {
    match error {
        EvalError::WithPosition { .. } => error,
        EvalError::Message { message } => {
            EvalError::with_position(message, position.line, position.column)
        }
    }
}

fn require_min_args(name: &str, actual: usize, min: usize) -> Result<(), EvalError> {
    if actual < min {
        return Err(EvalError::message(format!(
            "{name} expected at least {min} argument(s)"
        )));
    }
    Ok(())
}

fn require_exact_args(name: &str, actual: usize, exact: usize) -> Result<(), EvalError> {
    if actual != exact {
        return Err(EvalError::message(format!(
            "{name} expected exactly {exact} argument(s)"
        )));
    }
    Ok(())
}

fn require_int(value: &Value, operator: &str) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        _ => Err(EvalError::message(format!(
            "{operator} expects numeric arguments"
        ))),
    }
}

fn require_pair<'a>(value: &'a Value, operator: &str) -> Result<&'a PairValue, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.as_ref()),
        _ => Err(EvalError::message(format!("{operator} expects a pair"))),
    }
}

fn require_proper_list(value: &Value, operator: &str) -> Result<Vec<Value>, EvalError> {
    let mut elements = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            Value::EmptyList => return Ok(elements),
            _ => {
                return Err(EvalError::message(format!(
                    "{operator} expects a proper list"
                )))
            }
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn render(value: &Value) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::String(text) => quote_string(text),
        Value::Symbol(name) => name.clone(),
        Value::EmptyList => "()".to_string(),
        Value::Pair(pair) => render_pair(pair.as_ref()),
        Value::Builtin(_) | Value::Closure(_) => "#<procedure>".to_string(),
        Value::Void => "#<void>".to_string(),
        Value::Uninitialized => "#<uninitialized>".to_string(),
    }
}

fn render_pair(pair: &PairValue) -> String {
    let mut result = String::from("(");
    let mut current = Value::Pair(Rc::new(pair.clone()));
    let mut first = true;

    loop {
        match current {
            Value::Pair(pair) => {
                if !first {
                    result.push(' ');
                }
                result.push_str(&render(&pair.car));
                current = pair.cdr.clone();
                first = false;
            }
            Value::EmptyList => {
                result.push(')');
                return result;
            }
            other => {
                result.push_str(" . ");
                result.push_str(&render(&other));
                result.push(')');
                return result;
            }
        }
    }
}

fn quote_string(value: &str) -> String {
    let mut result = String::from("\"");
    for ch in value.chars() {
        match ch {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(ch),
        }
    }
    result.push('"');
    result
}

fn builtin_add(arguments: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;
    for argument in arguments {
        total += require_int(argument, "+")?;
    }
    Ok(Value::Int(total))
}

fn builtin_subtract(arguments: &[Value]) -> Result<Value, EvalError> {
    require_min_args("-", arguments.len(), 1)?;
    let mut result = require_int(&arguments[0], "-")?;
    if arguments.len() == 1 {
        return Ok(Value::Int(-result));
    }

    for argument in &arguments[1..] {
        result -= require_int(argument, "-")?;
    }
    Ok(Value::Int(result))
}

fn builtin_multiply(arguments: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i64;
    for argument in arguments {
        total *= require_int(argument, "*")?;
    }
    Ok(Value::Int(total))
}

fn builtin_divide(arguments: &[Value]) -> Result<Value, EvalError> {
    require_min_args("/", arguments.len(), 2)?;
    let mut result = require_int(&arguments[0], "/")?;
    for argument in &arguments[1..] {
        let divisor = require_int(argument, "/")?;
        if divisor == 0 {
            return Err(EvalError::message("division by zero"));
        }
        result /= divisor;
    }
    Ok(Value::Int(result))
}

fn builtin_less_than(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, "<")
}

fn builtin_greater_than(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, ">")
}

fn builtin_equal(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, "=")
}

fn builtin_less_equal(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, "<=")
}

fn compare(arguments: &[Value], operator: &str) -> Result<Value, EvalError> {
    require_min_args(operator, arguments.len(), 2)?;
    for pair in arguments.windows(2) {
        let left = require_int(&pair[0], operator)?;
        let right = require_int(&pair[1], operator)?;
        let passes = match operator {
            "<" => left < right,
            ">" => left > right,
            "=" => left == right,
            "<=" => left <= right,
            _ => unreachable!("unexpected comparison operator"),
        };
        if !passes {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn builtin_not(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("not", arguments.len(), 1)?;
    Ok(Value::Bool(!is_truthy(&arguments[0])))
}

fn builtin_cons(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("cons", arguments.len(), 2)?;
    Ok(Value::Pair(Rc::new(PairValue {
        car: arguments[0].clone(),
        cdr: arguments[1].clone(),
    })))
}

fn builtin_car(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("car", arguments.len(), 1)?;
    Ok(require_pair(&arguments[0], "car")?.car.clone())
}

fn builtin_cdr(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("cdr", arguments.len(), 1)?;
    Ok(require_pair(&arguments[0], "cdr")?.cdr.clone())
}

fn builtin_is_null(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("null?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::EmptyList)))
}

fn builtin_list(arguments: &[Value]) -> Result<Value, EvalError> {
    Ok(list_value(arguments.to_vec()))
}

fn builtin_length(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("length", arguments.len(), 1)?;
    Ok(Value::Int(require_proper_list(&arguments[0], "length")?.len() as i64))
}

fn builtin_append(arguments: &[Value]) -> Result<Value, EvalError> {
    if arguments.is_empty() {
        return Ok(Value::EmptyList);
    }

    let mut result = arguments[arguments.len() - 1].clone();
    for prefix in arguments[..arguments.len() - 1].iter().rev() {
        let items = require_proper_list(prefix, "append")?;
        for item in items.into_iter().rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: item,
                cdr: result,
            }));
        }
    }
    Ok(result)
}

fn builtin_is_string(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("string?", arguments, |value| matches!(value, Value::String(_)))
}

fn builtin_is_number(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("number?", arguments, |value| matches!(value, Value::Int(_)))
}

fn builtin_is_boolean(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("boolean?", arguments, |value| matches!(value, Value::Bool(_)))
}

fn builtin_is_pair(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("pair?", arguments, |value| matches!(value, Value::Pair(_)))
}

fn builtin_is_symbol(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("symbol?", arguments, |value| matches!(value, Value::Symbol(_)))
}

fn type_predicate<F>(name: &str, arguments: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    require_exact_args(name, arguments.len(), 1)?;
    Ok(Value::Bool(predicate(&arguments[0])))
}

type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;
type Cell = Rc<RefCell<Value>>;

#[derive(Clone, Debug)]
struct Expr {
    kind: ExprKind,
    position: SourceLoc,
}

#[derive(Clone, Debug)]
enum ExprKind {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Copy, Debug)]
struct SourceLoc {
    line: usize,
    column: usize,
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Builtin(Builtin),
    Closure(Rc<ClosureValue>),
    Void,
    Uninitialized,
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Copy)]
struct Builtin {
    name: &'static str,
    implementation: BuiltinFn,
}

#[derive(Clone)]
struct ClosureValue {
    parameters: Vec<String>,
    body: Vec<Expr>,
    env: Rc<Env>,
}

#[derive(Clone)]
struct BindingSpec {
    name: String,
    init_expr: Expr,
}

struct Env {
    parent: Option<Rc<Env>>,
    bindings: RefCell<HashMap<String, Cell>>,
}

impl Env {
    fn new(parent: Option<Rc<Env>>) -> Rc<Self> {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    fn define_builtin(&self, name: &'static str, implementation: BuiltinFn) {
        self.define(
            name.to_string(),
            Value::Builtin(Builtin {
                name,
                implementation,
            }),
        );
    }

    fn define_placeholder(&self, name: String) -> Cell {
        let cell = Rc::new(RefCell::new(Value::Uninitialized));
        self.bindings.borrow_mut().insert(name, cell.clone());
        cell
    }

    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        let Some(cell) = self.lookup_cell(name) else {
            return Err(EvalError::message(format!("unbound variable: {name}")));
        };

        let value = cell.borrow().clone();
        if matches!(value, Value::Uninitialized) {
            return Err(EvalError::message(format!("unbound variable: {name}")));
        }

        Ok(value)
    }

    fn lookup_cell(&self, name: &str) -> Option<Cell> {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            return Some(cell);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup_cell(name))
    }
}

struct Parser<'input> {
    input: &'input str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'input> Parser<'input> {
    fn new(input: &'input str) -> Self {
        Self {
            input,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_trivia();
        while !self.is_at_end() {
            expressions.push(self.parse_expression()?);
            self.skip_trivia();
        }
        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expr, EvalError> {
        self.skip_trivia();
        if self.is_at_end() {
            return Err(self.error("unexpected end of input"));
        }

        match self.current_char().unwrap_or_default() {
            '(' => self.parse_list(),
            '\'' => {
                let position = self.current_loc();
                self.advance();
                Ok(Expr {
                    kind: ExprKind::List(vec![
                        Expr {
                            kind: ExprKind::Symbol("quote".to_string()),
                            position,
                        },
                        self.parse_expression()?,
                    ]),
                    position,
                })
            }
            '"' => self.parse_string(),
            ')' => Err(self.error("unexpected ')'")),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        self.consume('(')?;
        let mut elements = Vec::new();
        self.skip_trivia();
        while !self.is_at_end() && self.current_char() != Some(')') {
            elements.push(self.parse_expression()?);
            self.skip_trivia();
        }

        if self.is_at_end() {
            return Err(self.error("unterminated list"));
        }

        self.consume(')')?;
        Ok(Expr {
            kind: ExprKind::List(elements),
            position,
        })
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        self.consume('"')?;
        let mut result = String::new();

        while !self.is_at_end() {
            let current = self.advance().unwrap_or_default();
            if current == '"' {
                return Ok(Expr {
                    kind: ExprKind::String(result),
                    position,
                });
            }

            if current == '\\' {
                if self.is_at_end() {
                    return Err(self.error("unterminated string escape"));
                }
                result.push(match self.advance().unwrap_or_default() {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '"' => '"',
                    '\\' => '\\',
                    other => other,
                });
                continue;
            }

            result.push(current);
        }

        Err(self.error("unterminated string literal"))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        let start = self.index;
        while !self.is_at_end() && !self.is_delimiter(self.current_char().unwrap_or_default()) {
            self.advance();
        }

        let token = &self.input[start..self.index];
        let kind = match token {
            "#t" => ExprKind::Bool(true),
            "#f" => ExprKind::Bool(false),
            _ => self.parse_number_or_symbol(token),
        };

        Ok(Expr { kind, position })
    }

    fn parse_number_or_symbol(&self, token: &str) -> ExprKind {
        if is_integer_token(token) {
            if let Ok(number) = token.parse::<i64>() {
                return ExprKind::Int(number);
            }
        }

        ExprKind::Symbol(token.to_string())
    }

    fn skip_trivia(&mut self) {
        while !self.is_at_end() {
            match self.current_char().unwrap_or_default() {
                ch if ch.is_whitespace() => {
                    self.advance();
                }
                ';' => self.skip_comment(),
                _ => return,
            }
        }
    }

    fn skip_comment(&mut self) {
        while !self.is_at_end() && self.current_char() != Some('\n') {
            self.advance();
        }
    }

    fn is_delimiter(&self, ch: char) -> bool {
        ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'')
    }

    fn consume(&mut self, expected: char) -> Result<(), EvalError> {
        if self.current_char() != Some(expected) {
            return Err(self.error(&format!("expected '{expected}'")));
        }
        self.advance();
        Ok(())
    }

    fn current_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.input.len()
    }

    fn current_loc(&self) -> SourceLoc {
        SourceLoc {
            line: self.line,
            column: self.column,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let current = self.current_char()?;
        self.index += current.len_utf8();
        if current == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(current)
    }

    fn error(&self, message: &str) -> EvalError {
        EvalError::with_position(message, self.line, self.column)
    }
}

fn is_integer_token(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }

    let mut chars = token.chars();
    let first = chars.next().unwrap_or_default();
    if matches!(first, '+' | '-') {
        return !chars.as_str().is_empty() && chars.all(|ch| ch.is_ascii_digit());
    }

    token.chars().all(|ch| ch.is_ascii_digit())
}

#[cfg(test)]
mod tests;
