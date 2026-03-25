use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::EvalError;

type Env = Rc<Environment>;
type BuiltinFunc = fn(&[Value]) -> Result<Value, EvalError>;
type ExprRef = Rc<Expr>;
type MacroEnv = HashMap<String, SyntaxRules>;
type RuntimeResult<T> = Result<T, RuntimeSignal>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<ExprRef>),
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(Rc<Pair>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone)]
enum Procedure {
    Builtin {
        name: &'static str,
        func: BuiltinFunc,
    },
    Continuation {
        id: usize,
    },
    Lambda {
        params: Vec<String>,
        body: Vec<ExprRef>,
        env: Env,
    },
}

enum TailOutcome {
    Value(Value),
    Expr(ExprRef, Env),
}

struct Environment {
    parent: Option<Env>,
    bindings: RefCell<HashMap<String, Value>>,
}

struct StepBudget {
    max_steps: usize,
    remaining: Option<usize>,
}

#[derive(Clone)]
struct SyntaxRules {
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: ExprRef,
    template: ExprRef,
}

#[derive(Clone, Default)]
struct MacroBindings {
    singles: HashMap<String, ExprRef>,
    repeats: HashMap<String, Vec<ExprRef>>,
}

enum RuntimeSignal {
    Error(EvalError),
    ContinuationJump { id: usize, value: Value },
}

static NEXT_CONTINUATION_ID: AtomicUsize = AtomicUsize::new(1);

impl From<EvalError> for RuntimeSignal {
    fn from(error: EvalError) -> Self {
        Self::Error(error)
    }
}

impl Environment {
    fn new(parent: Option<Env>) -> Env {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(env: &Env, name: impl Into<String>, value: Value) {
        env.bindings.borrow_mut().insert(name.into(), value);
    }

    fn get(env: &Env, name: &str) -> Option<Value> {
        if let Some(value) = env.bindings.borrow().get(name).cloned() {
            Some(value)
        } else if let Some(parent) = &env.parent {
            Self::get(parent, name)
        } else {
            None
        }
    }
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    eval_program(input, StepBudget::unlimited())
}

pub fn eval_str_with_limit(input: &str, max_steps: usize) -> Result<String, EvalError> {
    eval_program(input, StepBudget::limited(max_steps))
}

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_program(input, StepBudget::unlimited())?, String::new()))
}

fn eval_program(input: &str, mut budget: StepBudget) -> Result<String, EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    let env = default_env();
    let mut macros = MacroEnv::new();
    let mut last = Value::Void;

    for expression in expressions {
        let Some(expanded) = expand_top_level(expression, &mut macros)? else {
            last = Value::Void;
            continue;
        };

        last = match eval(expanded, env.clone(), &mut budget) {
            Ok(value) => value,
            Err(RuntimeSignal::Error(error)) => return Err(error),
            Err(RuntimeSignal::ContinuationJump { .. }) => {
                return Err(EvalError::Runtime(
                    "continuation invoked outside of an active call/cc".into(),
                ))
            }
        };
    }

    Ok(render_result(last))
}

fn default_env() -> Env {
    let env = Environment::new(None);

    for (name, func) in [
        ("+", builtin_add as BuiltinFunc),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("<", builtin_lt),
        (">", builtin_gt),
        ("=", builtin_num_eq),
        ("<=", builtin_lte),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("append", builtin_append),
        ("reverse", builtin_reverse),
        ("eqv?", builtin_eqv),
        ("string?", builtin_is_string),
        ("string-append", builtin_string_append),
        ("string-length", builtin_string_length),
        ("number?", builtin_is_number),
        ("boolean?", builtin_is_boolean),
        ("pair?", builtin_is_pair),
        ("symbol?", builtin_is_symbol),
    ] {
        Environment::define(
            &env,
            name,
            Value::Procedure(Rc::new(Procedure::Builtin { name, func })),
        );
    }

    env
}

fn expand_top_level(expression: ExprRef, macros: &mut MacroEnv) -> Result<Option<ExprRef>, EvalError> {
    if let Expr::List(items) = expression.as_ref() {
        if list_head_symbol(items) == Some("define-syntax") {
            let (name, rules) = parse_define_syntax(items)?;
            macros.insert(name, rules);
            return Ok(None);
        }
    }

    expand_expr(expression, macros).map(Some)
}

fn parse_define_syntax(items: &[ExprRef]) -> Result<(String, SyntaxRules), EvalError> {
    ensure_exact_args("define-syntax", items.len() - 1, 2)?;

    let Expr::Symbol(name) = items[1].as_ref() else {
        return Err(EvalError::Syntax(
            "define-syntax: macro name must be a symbol".into(),
        ));
    };

    Ok((name.clone(), parse_syntax_rules(items[2].as_ref())?))
}

fn parse_syntax_rules(expression: &Expr) -> Result<SyntaxRules, EvalError> {
    let Expr::List(items) = expression else {
        return Err(EvalError::Syntax(
            "define-syntax: expected a syntax-rules form".into(),
        ));
    };

    if list_head_symbol(items) != Some("syntax-rules") {
        return Err(EvalError::Syntax(
            "define-syntax: expected a syntax-rules form".into(),
        ));
    }

    if items.len() < 3 {
        return Err(EvalError::Syntax(
            "syntax-rules: expected a literals list and at least one rule".into(),
        ));
    }

    let Expr::List(literal_items) = items[1].as_ref() else {
        return Err(EvalError::Syntax(
            "syntax-rules: literals must be a list".into(),
        ));
    };

    let mut literals = HashSet::with_capacity(literal_items.len());
    for literal in literal_items {
        let Expr::Symbol(symbol) = literal.as_ref() else {
            return Err(EvalError::Syntax(
                "syntax-rules: literals must be symbols".into(),
            ));
        };
        literals.insert(symbol.clone());
    }

    let mut rules = Vec::with_capacity(items.len() - 2);
    for rule in &items[2..] {
        let Expr::List(parts) = rule.as_ref() else {
            return Err(EvalError::Syntax(
                "syntax-rules: each rule must be a list".into(),
            ));
        };

        if parts.len() != 2 {
            return Err(EvalError::Syntax(
                "syntax-rules: each rule must contain a pattern and template".into(),
            ));
        }

        rules.push(SyntaxRule {
            pattern: parts[0].clone(),
            template: parts[1].clone(),
        });
    }

    Ok(SyntaxRules { literals, rules })
}

fn expand_expr(expression: ExprRef, macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    match expression.as_ref() {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) | Expr::Symbol(_) => Ok(expression),
        Expr::List(items) if items.is_empty() => Ok(expression),
        Expr::List(items) => {
            if let Some(name) = list_head_symbol(items) {
                if let Some(rules) = macros.get(name) {
                    return expand_macro_call(&expression, rules, macros);
                }

                return match name {
                    "quote" => Ok(expression),
                    "define" => expand_define_expr(items, macros),
                    "lambda" => expand_lambda_expr(items, macros),
                    "let" => expand_let_expr(items, macros),
                    "cond" => expand_cond_expr(items, macros),
                    "case" => expand_case_expr(items, macros),
                    _ => expand_list(items, macros),
                };
            }

            expand_list(items, macros)
        }
    }
}

fn expand_macro_call(
    expression: &ExprRef,
    rules: &SyntaxRules,
    macros: &MacroEnv,
) -> Result<ExprRef, EvalError> {
    for rule in &rules.rules {
        let mut bindings = MacroBindings::default();
        if match_pattern(
            &rule.pattern,
            expression,
            &rules.literals,
            &mut bindings,
            false,
        ) {
            return expand_expr(expand_template(&rule.template, &bindings, None)?, macros);
        }
    }

    Err(EvalError::Runtime(
        "syntax-rules: no pattern matched macro invocation".into(),
    ))
}

fn expand_define_expr(items: &[ExprRef], macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    if items.len() < 3 {
        return Ok(make_list(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone(), items[1].clone()];
    expanded.extend(expand_all(&items[2..], macros)?);
    Ok(make_list(expanded))
}

fn expand_lambda_expr(items: &[ExprRef], macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    if items.len() < 3 {
        return Ok(make_list(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone(), items[1].clone()];
    expanded.extend(expand_all(&items[2..], macros)?);
    Ok(make_list(expanded))
}

fn expand_let_expr(items: &[ExprRef], macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    if items.len() < 3 {
        return Ok(make_list(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone()];
    match items[1].as_ref() {
        Expr::Symbol(_) if items.len() >= 4 => {
            expanded.push(items[1].clone());
            expanded.push(expand_bindings(items[2].clone(), macros)?);
            expanded.extend(expand_all(&items[3..], macros)?);
        }
        _ => {
            expanded.push(expand_bindings(items[1].clone(), macros)?);
            expanded.extend(expand_all(&items[2..], macros)?);
        }
    }

    Ok(make_list(expanded))
}

fn expand_cond_expr(items: &[ExprRef], macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    let mut expanded = Vec::with_capacity(items.len());
    expanded.push(items[0].clone());

    for clause in &items[1..] {
        let Expr::List(clause_items) = clause.as_ref() else {
            expanded.push(clause.clone());
            continue;
        };

        if clause_items.is_empty() {
            expanded.push(clause.clone());
            continue;
        }

        let mut expanded_clause = Vec::with_capacity(clause_items.len());
        if list_head_symbol(clause_items) == Some("else") {
            expanded_clause.push(clause_items[0].clone());
            expanded_clause.extend(expand_all(&clause_items[1..], macros)?);
        } else {
            expanded_clause.extend(expand_all(clause_items, macros)?);
        }
        expanded.push(make_list(expanded_clause));
    }

    Ok(make_list(expanded))
}

fn expand_case_expr(items: &[ExprRef], macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    if items.len() < 2 {
        return Ok(make_list(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone(), expand_expr(items[1].clone(), macros)?];
    for clause in &items[2..] {
        let Expr::List(clause_items) = clause.as_ref() else {
            expanded.push(clause.clone());
            continue;
        };

        if clause_items.is_empty() {
            expanded.push(clause.clone());
            continue;
        }

        let mut expanded_clause = Vec::with_capacity(clause_items.len());
        expanded_clause.push(clause_items[0].clone());
        expanded_clause.extend(expand_all(&clause_items[1..], macros)?);
        expanded.push(make_list(expanded_clause));
    }

    Ok(make_list(expanded))
}

fn expand_list(items: &[ExprRef], macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    Ok(make_list(expand_all(items, macros)?))
}

fn expand_all(items: &[ExprRef], macros: &MacroEnv) -> Result<Vec<ExprRef>, EvalError> {
    items
        .iter()
        .cloned()
        .map(|item| expand_expr(item, macros))
        .collect()
}

fn expand_bindings(bindings_expr: ExprRef, macros: &MacroEnv) -> Result<ExprRef, EvalError> {
    let Expr::List(bindings) = bindings_expr.as_ref() else {
        return Ok(bindings_expr);
    };

    let mut expanded = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(parts) = binding.as_ref() else {
            return Ok(bindings_expr);
        };
        if parts.len() != 2 {
            return Ok(bindings_expr);
        }

        expanded.push(make_list(vec![
            parts[0].clone(),
            expand_expr(parts[1].clone(), macros)?,
        ]));
    }

    Ok(make_list(expanded))
}

fn match_pattern(
    pattern: &ExprRef,
    input: &ExprRef,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
    repeated: bool,
) -> bool {
    match (pattern.as_ref(), input.as_ref()) {
        (Expr::Integer(expected), Expr::Integer(actual)) => expected == actual,
        (Expr::Boolean(expected), Expr::Boolean(actual)) => expected == actual,
        (Expr::String(expected), Expr::String(actual)) => expected == actual,
        (Expr::Symbol(symbol), _) => match symbol.as_str() {
            "_" => true,
            "..." => false,
            _ if literals.contains(symbol) => matches!(input.as_ref(), Expr::Symbol(name) if name == symbol),
            _ if repeated => bind_repeated(symbol, input, bindings),
            _ => bind_single(symbol, input, bindings),
        },
        (Expr::List(pattern_items), Expr::List(input_items)) => {
            match_pattern_items(pattern_items, input_items, literals, bindings, repeated)
        }
        _ => false,
    }
}

fn match_pattern_items(
    patterns: &[ExprRef],
    inputs: &[ExprRef],
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
    repeated: bool,
) -> bool {
    if patterns.is_empty() {
        return inputs.is_empty();
    }

    if patterns.len() >= 2 && is_ellipsis(&patterns[1]) {
        for count in 0..=inputs.len() {
            let mut candidate = bindings.clone();
            seed_repeated_bindings(&patterns[0], literals, &mut candidate);
            let mut matched = true;

            for input in &inputs[..count] {
                if !match_pattern(&patterns[0], input, literals, &mut candidate, true) {
                    matched = false;
                    break;
                }
            }

            if matched
                && match_pattern_items(
                    &patterns[2..],
                    &inputs[count..],
                    literals,
                    &mut candidate,
                    repeated,
                )
            {
                *bindings = candidate;
                return true;
            }
        }

        return false;
    }

    let Some((input, rest_inputs)) = inputs.split_first() else {
        return false;
    };

    let mut candidate = bindings.clone();
    match_pattern(&patterns[0], input, literals, &mut candidate, repeated)
        && match_pattern_items(&patterns[1..], rest_inputs, literals, &mut candidate, repeated)
        && {
            *bindings = candidate;
            true
        }
}

fn bind_single(name: &str, input: &ExprRef, bindings: &mut MacroBindings) -> bool {
    if bindings.repeats.contains_key(name) {
        return false;
    }

    match bindings.singles.get(name) {
        Some(existing) => existing == input,
        None => {
            bindings.singles.insert(name.to_string(), input.clone());
            true
        }
    }
}

fn bind_repeated(name: &str, input: &ExprRef, bindings: &mut MacroBindings) -> bool {
    if bindings.singles.contains_key(name) {
        return false;
    }

    bindings
        .repeats
        .entry(name.to_string())
        .or_default()
        .push(input.clone());
    true
}

fn seed_repeated_bindings(
    pattern: &ExprRef,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) {
    match pattern.as_ref() {
        Expr::Symbol(symbol) => match symbol.as_str() {
            "_" | "..." => {}
            _ if literals.contains(symbol) => {}
            _ => {
                bindings.repeats.entry(symbol.clone()).or_default();
            }
        },
        Expr::List(items) => {
            for item in items {
                seed_repeated_bindings(item, literals, bindings);
            }
        }
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => {}
    }
}

fn expand_template(
    template: &ExprRef,
    bindings: &MacroBindings,
    repeat_index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    match template.as_ref() {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => Ok(template.clone()),
        Expr::Symbol(symbol) => expand_template_symbol(symbol, bindings, repeat_index),
        Expr::List(items) => {
            let mut expanded = Vec::with_capacity(items.len());
            let mut index = 0;

            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                    let repeat_len = template_repeat_len(&items[index], bindings)?;
                    for item_index in 0..repeat_len {
                        expanded.push(expand_template(&items[index], bindings, Some(item_index))?);
                    }
                    index += 2;
                    continue;
                }

                expanded.push(expand_template(&items[index], bindings, repeat_index)?);
                index += 1;
            }

            Ok(make_list(expanded))
        }
    }
}

fn expand_template_symbol(
    symbol: &str,
    bindings: &MacroBindings,
    repeat_index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    if symbol == "..." {
        return Err(EvalError::Syntax(
            "syntax-rules: unexpected ellipsis in template".into(),
        ));
    }

    if let Some(index) = repeat_index {
        if let Some(values) = bindings.repeats.get(symbol) {
            return values.get(index).cloned().ok_or_else(|| {
                EvalError::Syntax(format!(
                    "syntax-rules: repeated variable '{symbol}' out of bounds"
                ))
            });
        }
    }

    if let Some(value) = bindings.singles.get(symbol) {
        return Ok(value.clone());
    }

    if let Some(values) = bindings.repeats.get(symbol) {
        return match values.as_slice() {
            [value] => Ok(value.clone()),
            _ => Err(EvalError::Syntax(format!(
                "syntax-rules: repeated variable '{symbol}' used without ellipsis"
            ))),
        };
    }

    Ok(Rc::new(Expr::Symbol(symbol.to_string())))
}

fn template_repeat_len(template: &ExprRef, bindings: &MacroBindings) -> Result<usize, EvalError> {
    let mut lengths = Vec::new();
    collect_repeat_lengths(template, bindings, &mut lengths);

    let Some(first) = lengths.first().copied() else {
        return Err(EvalError::Syntax(
            "syntax-rules: ellipsis requires a repeated pattern variable".into(),
        ));
    };

    if lengths.iter().any(|length| *length != first) {
        return Err(EvalError::Syntax(
            "syntax-rules: repeated template variables must have matching lengths".into(),
        ));
    }

    Ok(first)
}

fn collect_repeat_lengths(template: &ExprRef, bindings: &MacroBindings, lengths: &mut Vec<usize>) {
    match template.as_ref() {
        Expr::Symbol(symbol) => {
            if let Some(values) = bindings.repeats.get(symbol) {
                lengths.push(values.len());
            }
        }
        Expr::List(items) => {
            for item in items {
                collect_repeat_lengths(item, bindings, lengths);
            }
        }
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => {}
    }
}

fn list_head_symbol(items: &[ExprRef]) -> Option<&str> {
    let Expr::Symbol(symbol) = items.first()?.as_ref() else {
        return None;
    };
    Some(symbol)
}

fn is_ellipsis(expression: &ExprRef) -> bool {
    matches!(expression.as_ref(), Expr::Symbol(symbol) if symbol == "...")
}

fn make_list(items: Vec<ExprRef>) -> ExprRef {
    Rc::new(Expr::List(items))
}

fn eval(expression: ExprRef, env: Env, budget: &mut StepBudget) -> RuntimeResult<Value> {
    let mut current_expression = expression;
    let mut current_env = env;

    loop {
        budget.consume()?;
        let next = match current_expression.as_ref() {
            Expr::Integer(value) => return Ok(Value::Integer(*value)),
            Expr::Boolean(value) => return Ok(Value::Boolean(*value)),
            Expr::String(value) => return Ok(Value::String(value.clone())),
            Expr::Symbol(name) => {
                return Environment::get(&current_env, name)
                    .ok_or_else(|| RuntimeSignal::Error(EvalError::UnboundVariable(name.clone())))
            }
            Expr::List(items) if items.is_empty() => {
                return Err(EvalError::Runtime("cannot evaluate an empty list".into()).into())
            }
            Expr::List(items) => match items.first().map(|item| item.as_ref()) {
                Some(Expr::Symbol(symbol)) if symbol == "quote" => {
                    TailOutcome::Value(eval_quote(items)?)
                }
                Some(Expr::Symbol(symbol)) if symbol == "if" => {
                    eval_if(items, current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "define" => {
                    TailOutcome::Value(eval_define(items, current_env.clone(), budget)?)
                }
                Some(Expr::Symbol(symbol)) if symbol == "lambda" => {
                    TailOutcome::Value(eval_lambda(items, current_env.clone())?)
                }
                Some(Expr::Symbol(symbol)) if symbol == "call/cc" => {
                    eval_call_cc(items, current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "and" => {
                    eval_and(&items[1..], current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "or" => {
                    eval_or(&items[1..], current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "let" => {
                    eval_let(items, current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "begin" => {
                    eval_sequence_tail(&items[1..], current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "cond" => {
                    eval_cond(&items[1..], current_env.clone(), budget)?
                }
                Some(Expr::Symbol(symbol)) if symbol == "case" => {
                    eval_case(items, current_env.clone(), budget)?
                }
                _ => eval_application(items, current_env.clone(), budget)?,
            },
        };

        match next {
            TailOutcome::Value(value) => return Ok(value),
            TailOutcome::Expr(expression, env) => {
                current_expression = expression;
                current_env = env;
            }
        }
    }
}

fn eval_quote(items: &[ExprRef]) -> Result<Value, EvalError> {
    ensure_exact_args("quote", items.len() - 1, 1)?;
    datum_to_value(items[1].as_ref())
}

fn eval_if(items: &[ExprRef], env: Env, budget: &mut StepBudget) -> RuntimeResult<TailOutcome> {
    ensure_exact_args("if", items.len() - 1, 3)?;
    let condition = eval(items[1].clone(), env.clone(), budget)?;
    if condition.is_truthy() {
        Ok(TailOutcome::Expr(items[2].clone(), env))
    } else {
        Ok(TailOutcome::Expr(items[3].clone(), env))
    }
}

fn eval_define(items: &[ExprRef], env: Env, budget: &mut StepBudget) -> RuntimeResult<Value> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "define".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        }
        .into());
    }

    match items[1].as_ref() {
        Expr::Symbol(name) => {
            ensure_exact_args("define", items.len() - 1, 2)?;
            let value = eval(items[2].clone(), env.clone(), budget)?;
            Environment::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) if !signature.is_empty() => {
            let Expr::Symbol(name) = signature[0].as_ref() else {
                return Err(EvalError::Syntax(
                    "define: function name must be a symbol".into(),
                )
                .into());
            };

            let params = parse_params(&signature[1..], "define")?;
            let body = items[2..].to_vec();
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params,
                body,
                env: env.clone(),
            }));

            Environment::define(&env, name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(
            EvalError::Syntax("define: expected a symbol or function signature".into()).into(),
        ),
    }
}

fn eval_lambda(items: &[ExprRef], env: Env) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        });
    }

    let Expr::List(params_expr) = items[1].as_ref() else {
        return Err(EvalError::Syntax(
            "lambda: parameter list must be a list".into(),
        ));
    };

    let params = parse_params(params_expr, "lambda")?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: items[2..].to_vec(),
        env,
    })))
}

fn eval_call_cc(items: &[ExprRef], env: Env, budget: &mut StepBudget) -> RuntimeResult<TailOutcome> {
    ensure_exact_args("call/cc", items.len() - 1, 1)?;

    let procedure = eval(items[1].clone(), env, budget)?;
    let continuation_id = next_continuation_id();
    let continuation = Value::Procedure(Rc::new(Procedure::Continuation {
        id: continuation_id,
    }));

    match apply_and_resolve(procedure, vec![continuation], budget) {
        Ok(value) => Ok(TailOutcome::Value(value)),
        Err(RuntimeSignal::ContinuationJump { id, value }) if id == continuation_id => {
            Ok(TailOutcome::Value(value))
        }
        Err(other) => Err(other),
    }
}

fn eval_and(
    items: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    if items.is_empty() {
        return Ok(TailOutcome::Value(Value::Boolean(true)));
    }

    for item in &items[..items.len() - 1] {
        let value = eval(item.clone(), env.clone(), budget)?;
        if !value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Expr(items.last().cloned().unwrap(), env))
}

fn eval_or(items: &[ExprRef], env: Env, budget: &mut StepBudget) -> RuntimeResult<TailOutcome> {
    if items.is_empty() {
        return Ok(TailOutcome::Value(Value::Boolean(false)));
    }

    for item in &items[..items.len() - 1] {
        let value = eval(item.clone(), env.clone(), budget)?;
        if value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Expr(items.last().cloned().unwrap(), env))
}

fn eval_let(
    items: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        }
        .into());
    }

    match items[1].as_ref() {
        Expr::Symbol(name) => eval_named_let(name, &items[2], &items[3..], env, budget),
        bindings => eval_plain_let(bindings, &items[2..], env, budget),
    }
}

fn eval_named_let(
    name: &str,
    bindings_expr: &ExprRef,
    body: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    let bindings = parse_bindings(bindings_expr, "let")?;
    let mut params = Vec::with_capacity(bindings.len());
    let mut args = Vec::with_capacity(bindings.len());

    for (param, expression) in bindings {
        params.push(param);
        args.push(eval(expression, env.clone(), budget)?);
    }

    let loop_env = Environment::new(Some(env));
    let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env: loop_env.clone(),
    }));
    Environment::define(&loop_env, name.to_string(), procedure.clone());
    tail_apply(procedure, args, budget)
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    let bindings = parse_bindings(bindings_expr, "let")?;
    let mut values = Vec::with_capacity(bindings.len());

    for (name, expression) in &bindings {
        values.push((name.clone(), eval(expression.clone(), env.clone(), budget)?));
    }

    let let_env = Environment::new(Some(env));
    for (name, value) in values {
        Environment::define(&let_env, name, value);
    }

    eval_sequence_tail(body, let_env, budget)
}

fn eval_cond(
    clauses: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    for clause in clauses {
        let Expr::List(items) = clause.as_ref() else {
            return Err(EvalError::Syntax("cond: each clause must be a list".into()).into());
        };

        if items.is_empty() {
            return Err(EvalError::Syntax("cond: empty clause".into()).into());
        }

        if matches!(items.first().map(|item| item.as_ref()), Some(Expr::Symbol(symbol)) if symbol == "else")
        {
            return if items.len() == 1 {
                Ok(TailOutcome::Value(Value::Boolean(true)))
            } else {
                eval_sequence_tail(&items[1..], env.clone(), budget)
            };
        }

        let test_value = eval(items[0].clone(), env.clone(), budget)?;
        if test_value.is_truthy() {
            return if items.len() == 1 {
                Ok(TailOutcome::Value(test_value))
            } else {
                eval_sequence_tail(&items[1..], env.clone(), budget)
            };
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_case(items: &[ExprRef], env: Env, budget: &mut StepBudget) -> RuntimeResult<TailOutcome> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "case".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        }
        .into());
    }

    let key = eval(items[1].clone(), env.clone(), budget)?;
    for clause in &items[2..] {
        let Expr::List(clause_items) = clause.as_ref() else {
            return Err(EvalError::Syntax("case: each clause must be a list".into()).into());
        };

        if clause_items.is_empty() {
            return Err(EvalError::Syntax("case: empty clause".into()).into());
        }

        if list_head_symbol(clause_items) == Some("else") {
            return eval_sequence_tail(&clause_items[1..], env.clone(), budget);
        }

        let Expr::List(data) = clause_items[0].as_ref() else {
            return Err(EvalError::Syntax(
                "case: clause datums must be a list or else".into(),
            )
            .into());
        };

        let mut matched = false;
        for datum in data {
            if datum_matches_key(datum, &key)? {
                matched = true;
                break;
            }
        }

        if matched {
            return if clause_items.len() == 1 {
                Ok(TailOutcome::Value(Value::Void))
            } else {
                eval_sequence_tail(&clause_items[1..], env.clone(), budget)
            };
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_sequence_tail(
    expressions: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    if expressions.is_empty() {
        return Ok(TailOutcome::Value(Value::Void));
    }

    for expression in &expressions[..expressions.len() - 1] {
        eval(expression.clone(), env.clone(), budget)?;
    }

    Ok(TailOutcome::Expr(expressions.last().cloned().unwrap(), env))
}

fn eval_application(
    items: &[ExprRef],
    env: Env,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    let operator = eval(items[0].clone(), env.clone(), budget)?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(1));

    for expression in &items[1..] {
        args.push(eval(expression.clone(), env.clone(), budget)?);
    }

    tail_apply(operator, args, budget)
}

fn tail_apply(
    procedure: Value,
    args: Vec<Value>,
    budget: &mut StepBudget,
) -> RuntimeResult<TailOutcome> {
    let Value::Procedure(procedure) = procedure else {
        return Err(EvalError::NotAProcedure(procedure.render()).into());
    };

    match procedure.as_ref() {
        Procedure::Builtin { func, .. } => Ok(TailOutcome::Value(func(&args)?)),
        Procedure::Continuation { id } => {
            ensure_exact_args("continuation", args.len(), 1)?;
            Err(RuntimeSignal::ContinuationJump {
                id: *id,
                value: args.into_iter().next().unwrap(),
            })
        }
        Procedure::Lambda { params, body, env } => {
            ensure_exact_args("lambda", args.len(), params.len())?;
            let call_env = Environment::new(Some(env.clone()));

            for (param, value) in params.iter().zip(args.into_iter()) {
                Environment::define(&call_env, param.clone(), value);
            }

            eval_sequence_tail(body, call_env, budget)
        }
    }
}

fn apply_and_resolve(
    procedure: Value,
    args: Vec<Value>,
    budget: &mut StepBudget,
) -> RuntimeResult<Value> {
    match tail_apply(procedure, args, budget)? {
        TailOutcome::Value(value) => Ok(value),
        TailOutcome::Expr(expression, env) => eval(expression, env, budget),
    }
}

fn parse_params(items: &[ExprRef], context: &str) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());

    for item in items {
        let Expr::Symbol(symbol) = item.as_ref() else {
            return Err(EvalError::Syntax(format!(
                "{context}: parameters must be symbols"
            )));
        };
        params.push(symbol.clone());
    }

    Ok(params)
}

fn parse_bindings(expression: &Expr, context: &str) -> Result<Vec<(String, ExprRef)>, EvalError> {
    let Expr::List(bindings) = expression else {
        return Err(EvalError::Syntax(format!(
            "{context}: bindings must be a list"
        )));
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(parts) = binding.as_ref() else {
            return Err(EvalError::Syntax(format!(
                "{context}: each binding must be a list"
            )));
        };

        if parts.len() != 2 {
            return Err(EvalError::Syntax(format!(
                "{context}: each binding must contain a name and an expression"
            )));
        }

        let Expr::Symbol(name) = parts[0].as_ref() else {
            return Err(EvalError::Syntax(format!(
                "{context}: binding name must be a symbol"
            )));
        };

        parsed.push((name.clone(), parts[1].clone()));
    }

    Ok(parsed)
}

fn datum_to_value(expression: &Expr) -> Result<Value, EvalError> {
    match expression {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(datum_to_value(item.as_ref())?);
            }
            Ok(list_from_vec(values))
        }
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum = 0;
    for arg in args {
        sum += expect_integer("+", arg)?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        });
    }

    let first = expect_integer("-", &args[0])?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }

    let mut total = first;
    for arg in &args[1..] {
        total -= expect_integer("-", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1;
    for arg in args {
        product *= expect_integer("*", arg)?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }

    let mut total = expect_integer("/", &args[0])?;
    for arg in &args[1..] {
        let divisor = expect_integer("/", arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= divisor;
    }
    Ok(Value::Integer(total))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("<", args, |a, b| a < b)
}

fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(">", args, |a, b| a > b)
}

fn builtin_num_eq(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("=", args, |a, b| a == b)
}

fn builtin_lte(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("<=", args, |a, b| a <= b)
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("not", args.len(), 1)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("cons", args.len(), 2)?;
    Ok(Value::Pair(Rc::new(Pair {
        car: args[0].clone(),
        cdr: args[1].clone(),
    })))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("car", args.len(), 1)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(type_mismatch("car", "pair", &args[0]));
    };
    Ok(pair.car.clone())
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("cdr", args.len(), 1)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(type_mismatch("cdr", "pair", &args[0]));
    };
    Ok(pair.cdr.clone())
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("null?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Nil)))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_from_vec(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("length", args.len(), 1)?;
    Ok(Value::Integer(list_to_vec("length", &args[0])?.len() as i64))
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut values = Vec::new();
    for arg in args {
        values.extend(list_to_vec("append", arg)?);
    }
    Ok(list_from_vec(values))
}

fn builtin_reverse(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("reverse", args.len(), 1)?;
    let mut values = list_to_vec("reverse", &args[0])?;
    values.reverse();
    Ok(list_from_vec(values))
}

fn builtin_eqv(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("eqv?", args.len(), 2)?;
    Ok(Value::Boolean(values_eqv(&args[0], &args[1])))
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("string?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::String(_))))
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let capacity = args.iter().try_fold(0usize, |total, arg| {
        Ok::<usize, EvalError>(total + expect_string("string-append", arg)?.len())
    })?;
    let mut result = String::with_capacity(capacity);

    for arg in args {
        result.push_str(expect_string("string-append", arg)?);
    }

    Ok(Value::String(result))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("string-length", args.len(), 1)?;
    Ok(Value::Integer(
        expect_string("string-length", &args[0])?.chars().count() as i64,
    ))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("number?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("boolean?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("pair?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Pair(_))))
}

fn builtin_is_symbol(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("symbol?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }

    let mut previous = expect_integer(name, &args[0])?;
    for arg in &args[1..] {
        let current = expect_integer(name, arg)?;
        if !predicate(previous, current) {
            return Ok(Value::Boolean(false));
        }
        previous = current;
    }

    Ok(Value::Boolean(true))
}

fn expect_integer(name: &str, value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Integer(integer) => Ok(*integer),
        _ => Err(type_mismatch(name, "number", value)),
    }
}

fn expect_string<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::String(string) => Ok(string),
        _ => Err(type_mismatch(name, "string", value)),
    }
}

fn datum_matches_key(datum: &ExprRef, key: &Value) -> Result<bool, EvalError> {
    Ok(values_eqv(&datum_to_value(datum.as_ref())?, key))
}

fn values_eqv(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Procedure(a), Value::Procedure(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn next_continuation_id() -> usize {
    NEXT_CONTINUATION_ID.fetch_add(1, Ordering::Relaxed)
}

fn list_to_vec(name: &str, value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut result = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Nil => return Ok(result),
            Value::Pair(pair) => {
                result.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            other => return Err(type_mismatch(name, "proper list", &other)),
        }
    }
}

fn list_from_vec(values: Vec<Value>) -> Value {
    values.into_iter().rev().fold(Value::Nil, |tail, head| {
        Value::Pair(Rc::new(Pair {
            car: head,
            cdr: tail,
        }))
    })
}

fn ensure_exact_args(name: &str, got: usize, expected: usize) -> Result<(), EvalError> {
    if got == expected {
        Ok(())
    } else {
        Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: expected.to_string(),
            got,
        })
    }
}

fn type_mismatch(name: &str, expected: &str, value: &Value) -> EvalError {
    EvalError::TypeMismatch {
        name: name.into(),
        expected: expected.into(),
        found: value.type_name().into(),
    }
}

fn render_result(value: Value) -> String {
    match value {
        Value::Void => String::new(),
        value => value.render(),
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Nil => "empty list",
            Value::Pair(_) => "pair",
            Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(value) => format!("\"{}\"", escape_string(value)),
            Value::Symbol(value) => value.clone(),
            Value::Nil => "()".into(),
            Value::Pair(_) => render_pair(self),
            Value::Procedure(procedure) => match procedure.as_ref() {
                Procedure::Builtin { name, .. } => format!("#<procedure:{name}>"),
                Procedure::Continuation { .. } => "#<procedure:continuation>".into(),
                Procedure::Lambda { .. } => "#<procedure>".into(),
            },
            Value::Void => String::new(),
        }
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
                output.push_str(&pair.car.render());
                current = pair.cdr.clone();
                first = false;
            }
            Value::Nil => {
                output.push(')');
                return output;
            }
            other => {
                output.push_str(" . ");
                output.push_str(&other.render());
                output.push(')');
                return output;
            }
        }
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<ExprRef>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_space_and_comments();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_space_and_comments();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<ExprRef, EvalError> {
        self.skip_space_and_comments();
        let ch = self
            .peek_char()
            .ok_or_else(|| EvalError::Syntax("unexpected end of input".into()))?;

        match ch {
            '(' => self.parse_list(),
            ')' => Err(EvalError::Syntax("unexpected ')'".into())),
            '\'' => {
                self.bump_char();
                let expression = self.parse_expr()?;
                Ok(Rc::new(Expr::List(vec![
                    Rc::new(Expr::Symbol("quote".into())),
                    expression,
                ])))
            }
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<ExprRef, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_space_and_comments();
            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Rc::new(Expr::List(items)));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::Syntax("unterminated list".into())),
            }
        }
    }

    fn parse_string(&mut self) -> Result<ExprRef, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        loop {
            let ch = self
                .bump_char()
                .ok_or_else(|| EvalError::Syntax("unterminated string".into()))?;

            match ch {
                '"' => return Ok(Rc::new(Expr::String(value))),
                '\\' => {
                    let escaped = self
                        .bump_char()
                        .ok_or_else(|| EvalError::Syntax("unterminated escape".into()))?;
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<ExprRef, EvalError> {
        let mut token = String::new();

        while let Some(ch) = self.peek_char() {
            if is_delimiter(ch) {
                break;
            }
            token.push(ch);
            self.bump_char();
        }

        if token.is_empty() {
            return Err(EvalError::Syntax("expected expression".into()));
        }

        match token.as_str() {
            "#t" => Ok(Rc::new(Expr::Boolean(true))),
            "#f" => Ok(Rc::new(Expr::Boolean(false))),
            _ if is_integer_token(&token) => token
                .parse::<i64>()
                .map(Expr::Integer)
                .map(Rc::new)
                .map_err(|_| EvalError::Syntax(format!("invalid integer literal: {token}"))),
            _ => Ok(Rc::new(Expr::Symbol(token))),
        }
    }

    fn skip_space_and_comments(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.bump_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::Syntax(format!(
                "expected '{expected}', found '{ch}'"
            ))),
            None => Err(EvalError::Syntax(format!("expected '{expected}'"))),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}

fn is_integer_token(token: &str) -> bool {
    let Some(first) = token.chars().next() else {
        return false;
    };

    if first == '-' || first == '+' {
        token.len() > 1 && token[1..].chars().all(|ch| ch.is_ascii_digit())
    } else {
        token.chars().all(|ch| ch.is_ascii_digit())
    }
}

impl StepBudget {
    fn unlimited() -> Self {
        Self {
            max_steps: 0,
            remaining: None,
        }
    }

    fn limited(max_steps: usize) -> Self {
        Self {
            max_steps,
            remaining: Some(max_steps),
        }
    }

    fn consume(&mut self) -> Result<(), EvalError> {
        let max_steps = self.max_steps;

        if let Some(remaining) = self.remaining.as_mut() {
            if *remaining == 0 {
                return Err(EvalError::StepLimitExceeded { max_steps });
            }
            *remaining -= 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod level27_tests {
    use super::{eval_str, eval_str_with_limit};

    #[test]
    fn test_l27_step_limit_normal() {
        let result = eval_str_with_limit("(+ 1 2)", 1000);
        assert_eq!(result, Ok("3".into()));
    }

    #[test]
    fn test_l27_step_limit_loop_within_budget() {
        let result = eval_str_with_limit(
            "(let loop ((n 50)) (if (= n 0) 'done (loop (- n 1))))",
            10000,
        );
        assert_eq!(result, Ok("done".into()));
    }

    #[test]
    fn test_l27_step_limit_infinite_loop() {
        let result = eval_str_with_limit("(let loop () (loop))", 1000);
        assert!(result.is_err(), "infinite loop should hit step limit");
    }

    #[test]
    fn test_l27_step_limit_exceeded() {
        let result = eval_str_with_limit(
            "(let loop ((n 1000)) (if (= n 0) 'done (loop (- n 1))))",
            50,
        );
        assert!(
            result.is_err(),
            "loop of 1000 iters should exceed 50-step budget"
        );
    }

    #[test]
    fn test_l27_step_limit_factorial() {
        let result = eval_str_with_limit(
            "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)",
            10000,
        );
        assert_eq!(result, Ok("3628800".into()));
    }

    #[test]
    fn test_l27_normal_eval_unaffected() {
        let result = eval_str("(let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1))))");
        assert_eq!(result, Ok("done".into()));
    }
}
