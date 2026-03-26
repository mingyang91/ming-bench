pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
struct SourceLoc {
    line: usize,
    col: usize,
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
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
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
    params: Vec<String>,
    body: Vec<Expr>,
    env: Environment,
}

#[derive(Clone)]
enum ProcedureValue {
    Builtin(BuiltinProcedure),
    Closure(Rc<ClosureProcedure>),
}

#[derive(Clone)]
enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Void,
    Procedure(ProcedureValue),
}

#[derive(Clone)]
struct EvaluatedArg {
    expr: Expr,
    value: Value,
}

struct LetBinding {
    name: String,
    value_expr: Expr,
}

#[derive(Clone)]
struct Environment {
    inner: Rc<EnvironmentInner>,
}

struct EnvironmentInner {
    parent: Option<Environment>,
    bindings: RefCell<HashMap<String, Value>>,
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
        self.inner.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str, loc: SourceLoc) -> Result<Value, EvalError> {
        if let Some(value) = self.inner.bindings.borrow().get(name) {
            return Ok(value.clone());
        }

        if let Some(parent) = &self.inner.parent {
            return parent.lookup(name, loc);
        }

        Err(err_at(loc, format!("unbound variable {name}")))
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
                    kind: ExprKind::Symbol("quote".to_string()),
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
            ExprKind::Symbol(token)
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
    let mut result = Value::Void;

    for expr in &program {
        result = evaluate(expr, &env)?;
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

    env.define("not", builtin("not", builtin_not));

    env.define("cons", builtin("cons", builtin_cons));
    env.define("car", builtin("car", builtin_car));
    env.define("cdr", builtin("cdr", builtin_cdr));
    env.define("null?", builtin("null?", builtin_null_pred));
    env.define("list", builtin("list", builtin_list));
    env.define("length", builtin("length", builtin_length));
    env.define("append", builtin("append", builtin_append));

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

fn evaluate(expr: &Expr, env: &Environment) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(value) => env.lookup(value, expr.loc),
        ExprKind::List(_) => evaluate_list(expr, env),
    }
}

fn evaluate_list(expr: &Expr, env: &Environment) -> Result<Value, EvalError> {
    let elements = expr_list(expr).expect("list expression expected");

    if elements.is_empty() {
        return Err(err_at(expr.loc, "cannot evaluate empty list"));
    }

    let head = &elements[0];
    let args = &elements[1..];

    if let Some(symbol) = expr_symbol(head) {
        match symbol {
            "define" => return eval_define(args, head, env),
            "if" => return eval_if(args, head, env),
            "quote" => return eval_quote(args, head),
            "lambda" => return eval_lambda(args, head, env),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "begin" => return eval_begin(args, env),
            "cond" => return eval_cond(args, head, env),
            "let" => return eval_let(args, head, env),
            _ => {}
        }
    }

    let operator = evaluate(head, env)?;
    let evaluated_args = args
        .iter()
        .map(|arg| {
            Ok(EvaluatedArg {
                expr: arg.clone(),
                value: evaluate(arg, env)?,
            })
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    apply_procedure(operator, &evaluated_args, head.loc)
}

fn eval_define(args: &[Expr], head: &Expr, env: &Environment) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "define expects a name and value"));
    }

    let target = &args[0];

    if let Some(name) = expr_symbol(target) {
        if args.len() != 2 {
            return Err(err_at(head.loc, "define expects exactly 2 arguments"));
        }

        env.define(name.to_string(), evaluate(&args[1], env)?);
        return Ok(Value::Void);
    }

    let Some(target_elements) = expr_list(target) else {
        return Err(err_at(head.loc, "invalid define target"));
    };

    if target_elements.is_empty() {
        return Err(err_at(head.loc, "invalid define target"));
    }

    let name_expr = &target_elements[0];
    let Some(name) = expr_symbol(name_expr) else {
        return Err(err_at(name_expr.loc, "function name must be a symbol"));
    };

    let params = target_elements[1..]
        .iter()
        .map(expect_parameter_symbol)
        .collect::<Result<Vec<_>, EvalError>>()?;

    let procedure = Value::Procedure(ProcedureValue::Closure(Rc::new(ClosureProcedure {
        name: Some(name.to_string()),
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })));

    env.define(name.to_string(), procedure);
    Ok(Value::Void)
}

fn eval_if(args: &[Expr], head: &Expr, env: &Environment) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(err_at(head.loc, "if expects exactly 3 arguments"));
    }

    if is_truthy(&evaluate(&args[0], env)?) {
        evaluate(&args[1], env)
    } else {
        evaluate(&args[2], env)
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
        .map(expect_parameter_symbol)
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

fn eval_and(args: &[Expr], env: &Environment) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);

    for arg in args {
        result = evaluate(arg, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_or(args: &[Expr], env: &Environment) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);

    for arg in args {
        result = evaluate(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Environment) -> Result<Value, EvalError> {
    evaluate_sequence(args, env)
}

fn eval_cond(args: &[Expr], head: &Expr, env: &Environment) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Some(clause_elements) = expr_list(clause) else {
            return Err(err_at(head.loc, "cond clauses must be non-empty lists"));
        };

        if clause_elements.is_empty() {
            return Err(err_at(head.loc, "cond clauses must be non-empty lists"));
        }

        let test_expr = &clause_elements[0];
        let body = &clause_elements[1..];
        let is_else_clause = matches!(expr_symbol(test_expr), Some("else"));

        if is_else_clause {
            if index != args.len() - 1 {
                return Err(err_at(test_expr.loc, "else must be the last cond clause"));
            }

            return evaluate_sequence(body, env);
        }

        let test_value = evaluate(test_expr, env)?;
        if is_truthy(&test_value) {
            if body.is_empty() {
                return Ok(test_value);
            }

            return evaluate_sequence(body, env);
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], head: &Expr, env: &Environment) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(err_at(head.loc, "let expects bindings and a body"));
    }

    if matches!(args[0].kind, ExprKind::Symbol(_)) {
        return eval_named_let(args, head, env);
    }

    let bindings = parse_let_bindings(&args[0], head.loc)?;
    let let_env = Environment::child(env);

    for binding in &bindings {
        let_env.define(binding.name.clone(), evaluate(&binding.value_expr, env)?);
    }

    evaluate_sequence(&args[1..], &let_env)
}

fn eval_named_let(args: &[Expr], head: &Expr, env: &Environment) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(err_at(
            head.loc,
            "named let expects a name, bindings, and a body",
        ));
    }

    let name_expr = &args[0];
    let Some(name) = expr_symbol(name_expr) else {
        return Err(err_at(name_expr.loc, "named let name must be a symbol"));
    };

    let bindings = parse_let_bindings(&args[1], head.loc)?;
    let evaluated_args = bindings
        .iter()
        .map(|binding| {
            Ok(EvaluatedArg {
                expr: binding.value_expr.clone(),
                value: evaluate(&binding.value_expr, env)?,
            })
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    let let_env = Environment::child(env);
    let procedure = Value::Procedure(ProcedureValue::Closure(Rc::new(ClosureProcedure {
        name: Some(name.to_string()),
        params: bindings
            .iter()
            .map(|binding| binding.name.clone())
            .collect(),
        body: args[2..].to_vec(),
        env: let_env.clone(),
    })));

    let_env.define(name.to_string(), procedure.clone());
    apply_procedure(procedure, &evaluated_args, name_expr.loc)
}

fn apply_procedure(
    operator: Value,
    args: &[EvaluatedArg],
    loc: SourceLoc,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(err_at(loc, "not a procedure"));
    };

    match procedure {
        ProcedureValue::Builtin(procedure) => (procedure.call)(args, loc),
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
                call_env.define(param.clone(), arg.value.clone());
            }

            evaluate_sequence(&procedure.body, &call_env)
        }
    }
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Number(value) => Value::Number(*value),
        ExprKind::Boolean(value) => Value::Boolean(*value),
        ExprKind::String(value) => Value::String(value.clone()),
        ExprKind::Symbol(value) => Value::Symbol(value.clone()),
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
            let Some(name) = expr_symbol(name_expr) else {
                return Err(err_at(name_expr.loc, "let binding name must be a symbol"));
            };

            Ok(LetBinding {
                name: name.to_string(),
                value_expr: parts[1].clone(),
            })
        })
        .collect()
}

fn evaluate_sequence(exprs: &[Expr], env: &Environment) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = evaluate(expr, env)?;
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

fn expect_parameter_symbol(expr: &Expr) -> Result<String, EvalError> {
    expr_symbol(expr)
        .map(ToOwned::to_owned)
        .ok_or_else(|| err_at(expr.loc, "parameter must be a symbol"))
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

fn expr_symbol(expr: &Expr) -> Option<&str> {
    let ExprKind::Symbol(value) = &expr.kind else {
        return None;
    };

    Some(value)
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
        Value::Void => "#<void>".to_string(),
        Value::Procedure(_) => "#<procedure>".to_string(),
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
