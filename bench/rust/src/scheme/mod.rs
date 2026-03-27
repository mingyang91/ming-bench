pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::msg("input is empty"));
    }

    let env = default_env();
    let output = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expression in &expressions {
        last = eval(expression, &env, &output)?;
    }

    let captured_output = output.borrow().clone();
    Ok((last, captured_output))
}

type EnvRef = Rc<RefCell<Environment>>;
type CellRef = Rc<RefCell<Value>>;
type OutputRef = Rc<RefCell<String>>;

static MACRO_GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Number(i128),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Number(i128),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
    CaseLambda(CaseLambdaProcedure),
}

#[derive(Clone, Copy)]
struct BuiltinProcedure {
    name: &'static str,
    func: fn(&[Value], &OutputRef) -> Result<Value, EvalError>,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    rest: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct CaseLambdaProcedure {
    name: Option<String>,
    clauses: Vec<LambdaProcedure>,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Vec<Expr>,
    template: Expr,
}

#[derive(Clone)]
struct MacroDefinition {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvRef,
}

#[derive(Clone, PartialEq)]
enum PatternBinding {
    Single(Expr),
    Sequence(Vec<Expr>),
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, CellRef>,
    macros: HashMap<String, Rc<MacroDefinition>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
            macros: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: impl Into<String>, value: Value) {
        Self::define_cell(env, name, Rc::new(RefCell::new(value)));
    }

    fn define_cell(env: &EnvRef, name: impl Into<String>, cell: CellRef) {
        env.borrow_mut().bindings.insert(name.into(), cell);
    }

    fn define_macro(env: &EnvRef, name: impl Into<String>, definition: Rc<MacroDefinition>) {
        env.borrow_mut().macros.insert(name.into(), definition);
    }

    fn lookup_cell(env: &EnvRef, name: &str) -> Option<CellRef> {
        let (cell, parent) = {
            let borrowed = env.borrow();
            (borrowed.bindings.get(name).cloned(), borrowed.parent.clone())
        };

        if cell.is_some() {
            return cell;
        }

        parent.and_then(|parent| Self::lookup_cell(&parent, name))
    }

    fn lookup_macro(env: &EnvRef, name: &str) -> Option<Rc<MacroDefinition>> {
        let (definition, parent) = {
            let borrowed = env.borrow();
            (borrowed.macros.get(name).cloned(), borrowed.parent.clone())
        };

        if definition.is_some() {
            return definition;
        }

        parent.and_then(|parent| Self::lookup_macro(&parent, name))
    }

    fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
        if let Some(cell) = Self::lookup_cell(env, name) {
            return Ok(cell.borrow().clone());
        }

        Err(EvalError::msg(format!("unbound variable: {name}")))
    }

    fn set(env: &EnvRef, name: &str, value: Value) -> Result<(), EvalError> {
        if let Some(cell) = Self::lookup_cell(env, name) {
            *cell.borrow_mut() = value;
            return Ok(());
        }

        Err(EvalError::msg(format!("unbound variable: {name}")))
    }
}

impl Value {
    fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    fn display_repr(&self) -> String {
        render_value(self, RenderMode::Display)
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn as_number(&self, procedure_name: &str) -> Result<i128, EvalError> {
        match self {
            Self::Number(number) => Ok(*number),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects numeric arguments"
            ))),
        }
    }

    fn as_string<'a>(&'a self, procedure_name: &str) -> Result<&'a str, EvalError> {
        match self {
            Self::String(value) => Ok(value),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects string arguments"
            ))),
        }
    }

    fn as_symbol<'a>(&'a self, procedure_name: &str) -> Result<&'a str, EvalError> {
        match self {
            Self::Symbol(name) => Ok(name),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects symbol arguments"
            ))),
        }
    }

    fn as_index(&self, procedure_name: &str) -> Result<usize, EvalError> {
        let number = self.as_number(procedure_name)?;
        if number < 0 {
            return Err(EvalError::msg(format!(
                "{procedure_name} expects a non-negative index"
            )));
        }

        usize::try_from(number)
            .map_err(|_| EvalError::msg(format!("{procedure_name} index is too large")))
    }
}

impl Procedure {
    fn render(&self) -> String {
        match self {
            Self::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
            Self::Lambda(lambda) => match &lambda.name {
                Some(name) => format!("#<procedure:{name}>"),
                None => "#<procedure>".to_string(),
            },
            Self::CaseLambda(case_lambda) => match &case_lambda.name {
                Some(name) => format!("#<procedure:{name}>"),
                None => "#<procedure>".to_string(),
            },
        }
    }
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);
    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_subtract);
    define_builtin(&env, "*", builtin_multiply);
    define_builtin(&env, "/", builtin_divide);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_numeric_equals);
    define_builtin(&env, "equal?", builtin_equal);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "not", builtin_not);
    define_builtin(&env, "cons", builtin_cons);
    define_builtin(&env, "list", builtin_list);
    define_builtin(&env, "null?", builtin_null_predicate);
    define_builtin(&env, "procedure?", builtin_procedure_predicate);
    define_builtin(&env, "car", builtin_car);
    define_builtin(&env, "cdr", builtin_cdr);
    define_builtin(&env, "apply", builtin_apply);
    define_builtin(&env, "display", builtin_display);
    define_builtin(&env, "write", builtin_write);
    define_builtin(&env, "newline", builtin_newline);
    define_builtin(&env, "string-append", builtin_string_append);
    define_builtin(&env, "string-length", builtin_string_length);
    define_builtin(&env, "substring", builtin_substring);
    define_builtin(&env, "string->number", builtin_string_to_number);
    define_builtin(&env, "number->string", builtin_number_to_string);
    define_builtin(&env, "symbol->string", builtin_symbol_to_string);
    define_builtin(&env, "string->symbol", builtin_string_to_symbol);
    define_builtin(&env, "string-ref", builtin_string_ref);
    define_builtin(&env, "char?", builtin_char_predicate);
    env
}

fn define_builtin(
    env: &EnvRef,
    name: &'static str,
    func: fn(&[Value], &OutputRef) -> Result<Value, EvalError>,
) {
    Environment::define(
        env,
        name,
        Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure { name, func }))),
    );
}

fn eval(expr: &Expr, env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(number) => Ok(Value::Number(*number)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Environment::lookup(env, name),
        Expr::List(items) => eval_list(items, env, output),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("cannot evaluate empty list"));
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(tail, env, output),
            "or" => return eval_or(tail, env, output),
            "begin" => return eval_begin(tail, env, output),
            "define" => return eval_define(tail, env, output),
            "define-syntax" => return eval_define_syntax(tail, env),
            "if" => return eval_if(tail, env, output),
            "let" => return eval_let(tail, env, output),
            "quote" => return eval_quote(tail),
            "set!" => return eval_set(tail, env, output),
            "case-lambda" => return eval_case_lambda(tail, env),
            "lambda" => return eval_lambda(tail, env),
            _ => {}
        }

        if let Some(definition) = Environment::lookup_macro(env, name) {
            let (expanded, expanded_env) = expand_macro_call(definition.as_ref(), tail, env)?;
            return eval(&expanded, &expanded_env, output);
        }
    }

    let procedure = eval(head, env, output)?;
    let mut arguments = Vec::with_capacity(tail.len());
    for expression in tail {
        arguments.push(eval(expression, env, output)?);
    }
    apply(procedure, arguments, output)
}

fn eval_define(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("define requires a target and value"));
    }

    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::msg(
                    "define variable form requires exactly one value",
                ));
            }

            let value = eval(&args[1], env, output)?;
            Environment::define(env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let Some((name_expr, params)) = signature.split_first() else {
                return Err(EvalError::msg("define function form requires a name"));
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::msg("function name must be a symbol"));
            };

            let formals = Expr::List(params.to_vec());
            let lambda = build_lambda(&formals, &args[1..], env, Some(name.clone()))?;
            Environment::define(env, name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::msg(
            "define target must be a symbol or parameter list",
        )),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::msg(
            "if requires condition, then branch, and else branch",
        ));
    }

    if eval(&args[0], env, output)?.is_truthy() {
        eval(&args[1], env, output)
    } else {
        eval(&args[2], env, output)
    }
}

fn eval_let(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("let requires bindings and body"));
    }

    let Expr::List(bindings) = &args[0] else {
        return Err(EvalError::msg("let bindings must be a list"));
    };

    let let_env = Environment::new(Some(Rc::clone(env)));
    for binding in bindings {
        let Expr::List(parts) = binding else {
            return Err(EvalError::msg("let bindings must be pairs"));
        };
        if parts.len() != 2 {
            return Err(EvalError::msg("let bindings must be pairs"));
        }

        let Expr::Symbol(name) = &parts[0] else {
            return Err(EvalError::msg("let binding names must be symbols"));
        };

        let value = eval(&parts[1], env, output)?;
        Environment::define(&let_env, name.clone(), value);
    }

    eval_sequence(&args[1..], &let_env, output)
}

fn eval_set(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::msg("set! requires a target and value"));
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::msg("set! target must be a symbol"));
    };

    let value = eval(&args[1], env, output)?;
    Environment::set(env, name, value)?;
    Ok(Value::Void)
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::msg("quote requires exactly one argument"));
    }

    Ok(quote_expr(&args[0]))
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("lambda requires a parameter list and body"));
    }

    build_lambda(&args[0], &args[1..], env, None)
}

fn eval_case_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    build_case_lambda(args, env, None)
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::msg("define-syntax requires a name and transformer"));
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::msg("define-syntax name must be a symbol"));
    };

    let definition = parse_syntax_rules(name, &args[1], env)?;
    Environment::define_macro(env, name.clone(), Rc::new(definition));
    Ok(Value::Void)
}

fn build_lambda(
    formals: &Expr,
    body: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    let lambda = build_lambda_procedure(formals, body, env, name)?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda(lambda))))
}

fn build_case_lambda(
    clauses: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    if clauses.is_empty() {
        return Err(EvalError::msg("case-lambda requires at least one clause"));
    }

    let mut parsed_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let Expr::List(items) = clause else {
            return Err(EvalError::msg("case-lambda clauses must be lists"));
        };

        let Some((formals, body)) = items.split_first() else {
            return Err(EvalError::msg(
                "case-lambda clauses require parameters and body",
            ));
        };

        parsed_clauses.push(build_lambda_procedure(formals, body, env, name.clone())?);
    }

    Ok(Value::Procedure(Rc::new(Procedure::CaseLambda(
        CaseLambdaProcedure {
            name,
            clauses: parsed_clauses,
        },
    ))))
}

fn build_lambda_procedure(
    formals: &Expr,
    body: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<LambdaProcedure, EvalError> {
    if body.is_empty() {
        return Err(EvalError::msg(
            "lambda requires at least one body expression",
        ));
    }

    let (param_names, rest_param) = parse_formals(formals)?;

    Ok(LambdaProcedure {
        name,
        params: param_names,
        rest: rest_param,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

fn parse_formals(formals: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match formals {
        Expr::List(params) => parse_parameter_list(params),
        Expr::Symbol(name) => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(EvalError::msg("lambda parameters must be a list or symbol")),
    }
}

fn parse_parameter_list(params: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut param_names = Vec::with_capacity(params.len());
    let mut saw_dot = false;
    let mut rest_param = None;

    for (index, param) in params.iter().enumerate() {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::msg("lambda parameters must be symbols"));
        };

        if name == "." {
            if saw_dot || index + 1 >= params.len() {
                return Err(EvalError::msg("invalid dotted parameter list"));
            }
            saw_dot = true;
            continue;
        }

        if saw_dot {
            if index + 1 != params.len() {
                return Err(EvalError::msg("invalid dotted parameter list"));
            }
            rest_param = Some(name.clone());
            break;
        }

        param_names.push(name.clone());
    }

    if saw_dot && rest_param.is_none() {
        return Err(EvalError::msg("invalid dotted parameter list"));
    }

    Ok((param_names, rest_param))
}

fn parse_syntax_rules(
    macro_name: &str,
    expr: &Expr,
    env: &EnvRef,
) -> Result<MacroDefinition, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::msg(
            "define-syntax expects a syntax-rules transformer",
        ));
    };

    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("syntax-rules form cannot be empty"));
    };

    let Expr::Symbol(name) = head else {
        return Err(EvalError::msg("invalid syntax-rules transformer"));
    };
    if name != "syntax-rules" {
        return Err(EvalError::msg(
            "define-syntax expects a syntax-rules transformer",
        ));
    }
    if tail.len() < 2 {
        return Err(EvalError::msg("syntax-rules requires literals and rules"));
    }

    let Expr::List(literal_exprs) = &tail[0] else {
        return Err(EvalError::msg("syntax-rules literals must be a list"));
    };

    let mut literals = HashSet::with_capacity(literal_exprs.len());
    for literal in literal_exprs {
        let Expr::Symbol(name) = literal else {
            return Err(EvalError::msg("syntax-rules literals must be symbols"));
        };
        literals.insert(name.clone());
    }

    let mut rules = Vec::with_capacity(tail.len() - 1);
    for rule_expr in &tail[1..] {
        let Expr::List(rule_items) = rule_expr else {
            return Err(EvalError::msg("syntax-rules rules must be lists"));
        };
        if rule_items.len() != 2 {
            return Err(EvalError::msg(
                "syntax-rules rules must contain a pattern and template",
            ));
        }

        let Expr::List(pattern_items) = &rule_items[0] else {
            return Err(EvalError::msg("syntax-rules patterns must be lists"));
        };
        let Some((keyword_expr, pattern_tail)) = pattern_items.split_first() else {
            return Err(EvalError::msg("syntax-rules pattern cannot be empty"));
        };
        let Expr::Symbol(keyword) = keyword_expr else {
            return Err(EvalError::msg("syntax-rules pattern must start with a symbol"));
        };
        if keyword != macro_name {
            return Err(EvalError::msg(format!(
                "syntax-rules pattern must start with {macro_name}"
            )));
        }

        rules.push(MacroRule {
            pattern: pattern_tail.to_vec(),
            template: rule_items[1].clone(),
        });
    }

    if rules.is_empty() {
        return Err(EvalError::msg("syntax-rules requires at least one rule"));
    }

    Ok(MacroDefinition {
        name: macro_name.to_string(),
        literals,
        rules,
        env: Rc::clone(env),
    })
}

fn expand_macro_call(
    definition: &MacroDefinition,
    arguments: &[Expr],
    call_env: &EnvRef,
) -> Result<(Expr, EnvRef), EvalError> {
    for rule in &definition.rules {
        let mut bindings = HashMap::new();
        if match_list_pattern(&rule.pattern, arguments, &definition.literals, &mut bindings) {
            let expansion_env = Environment::new(Some(Rc::clone(call_env)));
            let mut expander = TemplateExpander::new(definition, &expansion_env, bindings);
            let expanded = expander.expand(&rule.template)?;
            return Ok((expanded, expansion_env));
        }
    }

    Err(EvalError::msg(format!(
        "no matching syntax-rules clause for {}",
        definition.name
    )))
}

fn match_list_pattern(
    pattern_items: &[Expr],
    input_items: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    if pattern_items.len() >= 2 && is_ellipsis(&pattern_items[pattern_items.len() - 1]) {
        let repeat_pattern = &pattern_items[pattern_items.len() - 2];
        let fixed = &pattern_items[..pattern_items.len() - 2];
        if input_items.len() < fixed.len() {
            return false;
        }

        for (pattern, input) in fixed.iter().zip(&input_items[..fixed.len()]) {
            if !match_pattern(pattern, input, literals, bindings) {
                return false;
            }
        }

        return match_repeated_pattern(repeat_pattern, &input_items[fixed.len()..], literals, bindings);
    }

    if pattern_items.len() != input_items.len() {
        return false;
    }

    for (pattern, input) in pattern_items.iter().zip(input_items) {
        if !match_pattern(pattern, input, literals, bindings) {
            return false;
        }
    }

    true
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Number(number) => matches!(input, Expr::Number(other) if other == number),
        Expr::Bool(value) => matches!(input, Expr::Bool(other) if other == value),
        Expr::String(value) => matches!(input, Expr::String(other) if other == value),
        Expr::Symbol(name) => {
            if name == "..." {
                return false;
            }
            if literals.contains(name) {
                return matches!(input, Expr::Symbol(other) if other == name);
            }
            bind_pattern_variable(name, PatternBinding::Single(input.clone()), bindings)
        }
        Expr::List(pattern_items) => match input {
            Expr::List(input_items) => match_list_pattern(pattern_items, input_items, literals, bindings),
            _ => false,
        },
    }
}

fn match_repeated_pattern(
    pattern: &Expr,
    inputs: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Symbol(name) if name != "..." && !literals.contains(name) => {
            bind_pattern_variable(name, PatternBinding::Sequence(inputs.to_vec()), bindings)
        }
        _ => false,
    }
}

fn bind_pattern_variable(
    name: &str,
    binding: PatternBinding,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match bindings.get(name) {
        Some(existing) => existing == &binding,
        None => {
            bindings.insert(name.to_string(), binding);
            true
        }
    }
}

struct TemplateExpander<'a> {
    definition: &'a MacroDefinition,
    alias_env: EnvRef,
    bindings: HashMap<String, PatternBinding>,
    renamed: HashMap<String, String>,
}

impl<'a> TemplateExpander<'a> {
    fn new(
        definition: &'a MacroDefinition,
        alias_env: &EnvRef,
        bindings: HashMap<String, PatternBinding>,
    ) -> Self {
        Self {
            definition,
            alias_env: Rc::clone(alias_env),
            bindings,
            renamed: HashMap::new(),
        }
    }

    fn expand(&mut self, expr: &Expr) -> Result<Expr, EvalError> {
        match expr {
            Expr::Number(_) | Expr::Bool(_) | Expr::String(_) => Ok(expr.clone()),
            Expr::Symbol(name) => self.expand_symbol(name),
            Expr::List(items) => self.expand_list(items),
        }
    }

    fn expand_symbol(&mut self, name: &str) -> Result<Expr, EvalError> {
        match self.bindings.get(name) {
            Some(PatternBinding::Single(value)) => Ok(value.clone()),
            Some(PatternBinding::Sequence(_)) => Err(EvalError::msg(format!(
                "macro template used repeated variable {name} without ellipsis"
            ))),
            None => Ok(Expr::Symbol(self.hygienic_name(name))),
        }
    }

    fn expand_list(&mut self, items: &[Expr]) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        let mut index = 0;

        while index < items.len() {
            if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                self.expand_repetition(&items[index], &mut expanded)?;
                index += 2;
                continue;
            }

            expanded.push(self.expand(&items[index])?);
            index += 1;
        }

        Ok(Expr::List(expanded))
    }

    fn expand_repetition(
        &mut self,
        expr: &Expr,
        expanded: &mut Vec<Expr>,
    ) -> Result<(), EvalError> {
        match expr {
            Expr::Symbol(name) => match self.bindings.get(name) {
                Some(PatternBinding::Sequence(values)) => {
                    expanded.extend(values.iter().cloned());
                    Ok(())
                }
                Some(PatternBinding::Single(_)) => Err(EvalError::msg(format!(
                    "macro template repeated non-sequence variable {name}"
                ))),
                None => Err(EvalError::msg(format!(
                    "macro template uses ellipsis with non-pattern variable {name}"
                ))),
            },
            _ => Err(EvalError::msg(
                "macro templates only support identifier ellipses at this level",
            )),
        }
    }

    fn hygienic_name(&mut self, name: &str) -> String {
        if name == "..."
            || name == "."
            || name == self.definition.name
            || is_core_syntax(name)
            || Environment::lookup_macro(&self.definition.env, name).is_some()
        {
            return name.to_string();
        }

        if let Some(existing) = self.renamed.get(name) {
            return existing.clone();
        }

        let fresh = fresh_macro_name(name);
        if let Some(cell) = Environment::lookup_cell(&self.definition.env, name) {
            Environment::define_cell(&self.alias_env, fresh.clone(), cell);
        }
        self.renamed.insert(name.to_string(), fresh.clone());
        fresh
    }
}

fn is_core_syntax(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "begin"
            | "case-lambda"
            | "define"
            | "define-syntax"
            | "if"
            | "lambda"
            | "let"
            | "quote"
            | "set!"
            | "syntax-rules"
    )
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name) if name == "...")
}

fn fresh_macro_name(name: &str) -> String {
    let counter = MACRO_GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    let sanitized: String = name
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect();
    let suffix = if sanitized.is_empty() { "_" } else { &sanitized };
    format!("__macro_{counter}_{suffix}")
}

fn eval_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expression in expressions {
        last = eval(expression, env, output)?;
    }
    Ok(last)
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(number) => Value::Number(*number),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_and(expressions: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expression in expressions {
        let value = eval(expression, env, output)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(expressions: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    for expression in expressions {
        let value = eval(expression, env, output)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_begin(expressions: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    eval_sequence(expressions, env, output)
}

fn apply(procedure: Value, arguments: Vec<Value>, output: &OutputRef) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = procedure else {
        return Err(EvalError::msg("attempted to call a non-procedure"));
    };

    match procedure.as_ref() {
        Procedure::Builtin(builtin) => (builtin.func)(&arguments, output),
        Procedure::Lambda(lambda) => apply_lambda(lambda, arguments, output),
        Procedure::CaseLambda(case_lambda) => apply_case_lambda(case_lambda, arguments, output),
    }
}

fn apply_lambda(
    lambda: &LambdaProcedure,
    arguments: Vec<Value>,
    output: &OutputRef,
) -> Result<Value, EvalError> {
    let procedure_name = lambda.name.as_deref().unwrap_or("lambda");
    if lambda.rest.is_some() {
        ensure_at_least(procedure_name, arguments.len(), lambda.params.len())?;
    } else {
        ensure_exactly(procedure_name, arguments.len(), lambda.params.len())?;
    }

    let call_env = Environment::new(Some(Rc::clone(&lambda.env)));
    let mut arguments = arguments.into_iter();
    for name in &lambda.params {
        let value = arguments
            .next()
            .expect("arity check ensures enough arguments for fixed parameters");
        Environment::define(&call_env, name.clone(), value);
    }

    if let Some(rest_name) = &lambda.rest {
        Environment::define(
            &call_env,
            rest_name.clone(),
            Value::List(arguments.collect()),
        );
    }

    eval_sequence(&lambda.body, &call_env, output)
}

fn apply_case_lambda(
    case_lambda: &CaseLambdaProcedure,
    arguments: Vec<Value>,
    output: &OutputRef,
) -> Result<Value, EvalError> {
    let argument_count = arguments.len();
    let Some(clause) = case_lambda
        .clauses
        .iter()
        .find(|clause| lambda_accepts_arity(clause, argument_count))
    else {
        let procedure_name = case_lambda.name.as_deref().unwrap_or("case-lambda");
        return Err(EvalError::msg(format!(
            "{procedure_name} has no matching clause for {argument_count} arguments"
        )));
    };

    apply_lambda(clause, arguments, output)
}

fn lambda_accepts_arity(lambda: &LambdaProcedure, argument_count: usize) -> bool {
    if lambda.rest.is_some() {
        argument_count >= lambda.params.len()
    } else {
        argument_count == lambda.params.len()
    }
}

fn builtin_add(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    let mut total = 0_i128;
    for argument in arguments {
        total += argument.as_number("+")?;
    }
    Ok(Value::Number(total))
}

fn builtin_subtract(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_at_least("-", arguments.len(), 1)?;
    let first = arguments[0].as_number("-")?;

    if arguments.len() == 1 {
        return Ok(Value::Number(-first));
    }

    let mut total = first;
    for argument in &arguments[1..] {
        total -= argument.as_number("-")?;
    }
    Ok(Value::Number(total))
}

fn builtin_multiply(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    let mut total = 1_i128;
    for argument in arguments {
        total *= argument.as_number("*")?;
    }
    Ok(Value::Number(total))
}

fn builtin_divide(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_at_least("/", arguments.len(), 1)?;

    let mut total = if arguments.len() == 1 {
        1_i128
    } else {
        arguments[0].as_number("/")?
    };

    let divisors = if arguments.len() == 1 {
        arguments
    } else {
        &arguments[1..]
    };

    for argument in divisors {
        let divisor = argument.as_number("/")?;
        if divisor == 0 {
            return Err(EvalError::msg("division by zero"));
        }
        total /= divisor;
    }

    Ok(Value::Number(total))
}

fn builtin_less_than(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, "<", |left, right| left < right)
}

fn builtin_greater_than(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, ">", |left, right| left > right)
}

fn builtin_numeric_equals(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, "=", |left, right| left == right)
}

fn builtin_equal(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("equal?", arguments.len(), 2)?;
    Ok(Value::Bool(values_equal(&arguments[0], &arguments[1])))
}

fn builtin_less_equal(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, "<=", |left, right| left <= right)
}

fn builtin_not(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("not", arguments.len(), 1)?;
    Ok(Value::Bool(!arguments[0].is_truthy()))
}

fn builtin_cons(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("cons", arguments.len(), 2)?;
    match &arguments[1] {
        Value::List(values) => {
            let mut list = Vec::with_capacity(values.len() + 1);
            list.push(arguments[0].clone());
            list.extend(values.iter().cloned());
            Ok(Value::List(list))
        }
        _ => Err(EvalError::msg("cons expects a list as its second argument")),
    }
}

fn builtin_list(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    Ok(Value::List(arguments.to_vec()))
}

fn builtin_null_predicate(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("null?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(
        &arguments[0],
        Value::List(values) if values.is_empty()
    )))
}

fn builtin_procedure_predicate(
    arguments: &[Value],
    _output: &OutputRef,
) -> Result<Value, EvalError> {
    ensure_exactly("procedure?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::Procedure(_))))
}

fn builtin_car(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("car", arguments.len(), 1)?;
    match &arguments[0] {
        Value::List(values) if !values.is_empty() => Ok(values[0].clone()),
        Value::List(_) => Err(EvalError::msg("car expects a non-empty list")),
        _ => Err(EvalError::msg("car expects a list")),
    }
}

fn builtin_cdr(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("cdr", arguments.len(), 1)?;
    match &arguments[0] {
        Value::List(values) if !values.is_empty() => Ok(Value::List(values[1..].to_vec())),
        Value::List(_) => Err(EvalError::msg("cdr expects a non-empty list")),
        _ => Err(EvalError::msg("cdr expects a list")),
    }
}

fn builtin_apply(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_at_least("apply", arguments.len(), 2)?;

    let trailing_args = match arguments.last() {
        Some(Value::List(values)) => values.clone(),
        Some(_) => return Err(EvalError::msg("apply expects a list as its last argument")),
        None => unreachable!("arity check guarantees at least two arguments"),
    };

    let mut applied_args =
        Vec::with_capacity(arguments.len().saturating_sub(1) + trailing_args.len());
    applied_args.extend(arguments[1..arguments.len() - 1].iter().cloned());
    applied_args.extend(trailing_args);

    apply(arguments[0].clone(), applied_args, output)
}

fn builtin_display(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("display", arguments.len(), 1)?;
    output.borrow_mut().push_str(&arguments[0].display_repr());
    Ok(Value::Void)
}

fn builtin_write(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("write", arguments.len(), 1)?;
    output.borrow_mut().push_str(&arguments[0].render());
    Ok(Value::Void)
}

fn builtin_newline(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("newline", arguments.len(), 0)?;
    output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn builtin_string_append(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    let mut result = String::new();
    for argument in arguments {
        result.push_str(argument.as_string("string-append")?);
    }
    Ok(Value::String(result))
}

fn builtin_string_length(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string-length", arguments.len(), 1)?;
    let length = arguments[0].as_string("string-length")?.chars().count();
    Ok(Value::Number(length as i128))
}

fn builtin_substring(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("substring", arguments.len(), 3)?;
    let value = arguments[0].as_string("substring")?;
    let start = arguments[1].as_index("substring")?;
    let end = arguments[2].as_index("substring")?;
    let chars: Vec<char> = value.chars().collect();

    if start > end || end > chars.len() {
        return Err(EvalError::msg("substring indices are out of bounds"));
    }

    Ok(Value::String(chars[start..end].iter().collect()))
}

fn builtin_string_to_number(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string->number", arguments.len(), 1)?;
    let value = arguments[0].as_string("string->number")?;
    match value.parse::<i128>() {
        Ok(number) => Ok(Value::Number(number)),
        Err(_) => Ok(Value::Bool(false)),
    }
}

fn builtin_number_to_string(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("number->string", arguments.len(), 1)?;
    let number = arguments[0].as_number("number->string")?;
    Ok(Value::String(number.to_string()))
}

fn builtin_symbol_to_string(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("symbol->string", arguments.len(), 1)?;
    Ok(Value::String(
        arguments[0].as_symbol("symbol->string")?.to_string(),
    ))
}

fn builtin_string_to_symbol(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string->symbol", arguments.len(), 1)?;
    Ok(Value::Symbol(
        arguments[0].as_string("string->symbol")?.to_string(),
    ))
}

fn builtin_string_ref(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string-ref", arguments.len(), 2)?;
    let value = arguments[0].as_string("string-ref")?;
    let index = arguments[1].as_index("string-ref")?;
    let chars: Vec<char> = value.chars().collect();
    let Some(ch) = chars.get(index) else {
        return Err(EvalError::msg("string-ref index is out of bounds"));
    };

    Ok(Value::Char(*ch))
}

fn builtin_char_predicate(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("char?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::Char(_))))
}

fn compare_numbers(
    arguments: &[Value],
    name: &str,
    predicate: fn(i128, i128) -> bool,
) -> Result<Value, EvalError> {
    ensure_at_least(name, arguments.len(), 2)?;
    let mut previous = arguments[0].as_number(name)?;

    for argument in &arguments[1..] {
        let current = argument.as_number(name)?;
        if !predicate(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }

    Ok(Value::Bool(true))
}

fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| values_equal(left, right))
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn ensure_exactly(name: &str, actual: usize, expected: usize) -> Result<(), EvalError> {
    if actual == expected {
        Ok(())
    } else {
        Err(EvalError::msg(format!(
            "{name} expected {expected} arguments but got {actual}"
        )))
    }
}

fn ensure_at_least(name: &str, actual: usize, minimum: usize) -> Result<(), EvalError> {
    if actual >= minimum {
        Ok(())
    } else {
        Err(EvalError::msg(format!(
            "{name} expected at least {minimum} arguments but got {actual}"
        )))
    }
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    match value {
        Value::Number(number) => number.to_string(),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::String(value) => match mode {
            RenderMode::Write => render_string(value),
            RenderMode::Display => value.clone(),
        },
        Value::Char(ch) => match mode {
            RenderMode::Write => render_char(*ch),
            RenderMode::Display => ch.to_string(),
        },
        Value::Symbol(name) => name.clone(),
        Value::List(values) => render_list(values, mode),
        Value::Procedure(procedure) => procedure.render(),
        Value::Void => "#<void>".to_string(),
    }
}

fn render_list(values: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");
    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value(value, mode));
    }
    rendered.push(')');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::from("\"");
    for ch in value.chars() {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            _ => rendered.push(ch),
        }
    }
    rendered.push('"');
    rendered
}

struct Parser<'a> {
    chars: Vec<char>,
    pos: usize,
    _input: &'a str,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            _input: input,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::msg("unexpected ')'")),
            Some('\'') => self.parse_quote_shorthand(),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::msg("unexpected end of input")),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        Ok(Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            self.parse_expr()?,
        ]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        let mut values = Vec::new();
        self.skip_ignored();

        while let Some(ch) = self.peek() {
            if ch == ')' {
                self.advance();
                return Ok(Expr::List(values));
            }

            values.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Err(EvalError::msg("unterminated list"))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        let mut value = String::new();

        while let Some(ch) = self.advance() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let Some(escaped) = self.advance() else {
                        return Err(EvalError::msg("unterminated string literal"));
                    };
                    value.push(match escaped {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(EvalError::msg("unterminated string literal"))
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

        if token == "#t" {
            return Ok(Expr::Bool(true));
        }
        if token == "#f" {
            return Ok(Expr::Bool(false));
        }
        if let Ok(number) = token.parse::<i128>() {
            return Ok(Expr::Number(number));
        }

        Ok(Expr::Symbol(token))
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek() {
                Some(ch) if ch.is_whitespace() => {
                    self.advance();
                }
                Some(';') => {
                    while let Some(ch) = self.peek() {
                        if ch == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

#[cfg(test)]
mod tests;
