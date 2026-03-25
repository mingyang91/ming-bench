use super::{equal_values, format_number, list_to_vec, list_value, type_name, EvalError, Expr, Number, Value};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

type MacroTable = HashMap<String, MacroDefinition>;
type SyntaxBindings = HashMap<String, SyntaxBinding>;

static HYGIENE_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
struct MacroDefinition {
    parameter: String,
    body: Vec<Expr>,
}

#[derive(Clone)]
enum SyntaxBinding {
    Single(SyntaxExpr),
    Repeated(Vec<SyntaxExpr>),
}

#[derive(Clone)]
enum TransformerValue {
    Syntax(SyntaxExpr),
    Datum(Value),
}

#[derive(Clone, Default)]
struct TransformerEnv {
    bindings: SyntaxBindings,
}

#[derive(Clone)]
struct SyntaxIdentifier {
    name: String,
    introduced: bool,
}

#[derive(Clone)]
enum SyntaxExpr {
    Number(Number),
    Boolean(bool),
    String(String),
    Symbol(SyntaxIdentifier),
    List(Vec<SyntaxExpr>),
}

pub(super) fn expand_program(expressions: Vec<Expr>) -> Result<Vec<Expr>, EvalError> {
    let mut macros = MacroTable::new();
    let mut expanded = Vec::new();

    for expr in expressions {
        if let Some((name, definition)) = parse_define_syntax(&expr)? {
            macros.insert(name, definition);
        } else {
            expanded.push(expand_expr(&expr, &macros)?);
        }
    }

    Ok(expanded)
}

fn parse_define_syntax(expr: &Expr) -> Result<Option<(String, MacroDefinition)>, EvalError> {
    let items = match expr {
        Expr::List(items) if !items.is_empty() => items,
        _ => return Ok(None),
    };

    if expr_head_name(items) != Some("define-syntax") {
        return Ok(None);
    }

    if items.len() != 3 {
        return Err(EvalError::message(format!(
            "define-syntax: expected 2 argument(s), got {}",
            items.len().saturating_sub(1)
        )));
    }

    let name = match &items[1] {
        Expr::Symbol(name) => name.clone(),
        _ => return Err(EvalError::message("define-syntax: expected identifier")),
    };

    Ok(Some((name, compile_macro(&items[2])?)))
}

fn compile_macro(transformer: &Expr) -> Result<MacroDefinition, EvalError> {
    let items = match transformer {
        Expr::List(items) if items.len() >= 3 => items,
        _ => {
            return Err(EvalError::message(
                "define-syntax: expected syntax-case transformer",
            ))
        }
    };

    if expr_head_name(items) != Some("lambda") {
        return Err(EvalError::message(
            "define-syntax: expected syntax-case transformer",
        ));
    }

    let parameter = match &items[1] {
        Expr::List(params) if params.len() == 1 => match &params[0] {
            Expr::Symbol(name) => name.clone(),
            _ => {
                return Err(EvalError::message(
                    "define-syntax: expected single transformer parameter",
                ))
            }
        },
        _ => {
            return Err(EvalError::message(
                "define-syntax: expected single transformer parameter",
            ))
        }
    };

    Ok(MacroDefinition {
        parameter,
        body: items[2..].to_vec(),
    })
}

fn expand_expr(expr: &Expr, macros: &MacroTable) -> Result<Expr, EvalError> {
    match expr {
        Expr::Number(_) | Expr::Boolean(_) | Expr::String(_) | Expr::Symbol(_) => Ok(expr.clone()),
        Expr::List(items) => expand_list(items, macros),
    }
}

fn expand_list(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.is_empty() {
        return Ok(Expr::List(Vec::new()));
    }

    if let Some(name) = expr_head_name(items) {
        if let Some(definition) = macros.get(name) {
            let expanded = apply_macro(definition, &Expr::List(items.to_vec()))?;
            return expand_expr(&expanded, macros);
        }

        match name {
            "quote" => return Ok(Expr::List(items.to_vec())),
            "define" => return expand_define(items, macros),
            "set!" => return expand_set(items, macros),
            "if" => return expand_if(items, macros),
            "lambda" => return expand_lambda(items, macros),
            "case-lambda" => return expand_case_lambda(items, macros),
            "begin" | "and" | "or" => return expand_tail_forms(items, macros),
            "let" => return expand_let(items, macros),
            "cond" => return expand_cond(items, macros),
            _ => {}
        }
    }

    Ok(Expr::List(
        items.iter()
            .map(|item| expand_expr(item, macros))
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn expand_define(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Ok(Expr::List(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone(), items[1].clone()];
    match &items[1] {
        Expr::Symbol(_) => {
            expanded.push(expand_expr(&items[2], macros)?);
            expanded.extend(items[3..].iter().cloned());
        }
        Expr::List(_) => {
            expanded.extend(
                items[2..]
                    .iter()
                    .map(|item| expand_expr(item, macros))
                    .collect::<Result<Vec<_>, _>>()?,
            );
        }
        _ => expanded.extend(items[2..].iter().cloned()),
    }

    Ok(Expr::List(expanded))
}

fn expand_set(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.len() != 3 {
        return Ok(Expr::List(items.to_vec()));
    }

    Ok(Expr::List(vec![
        items[0].clone(),
        items[1].clone(),
        expand_expr(&items[2], macros)?,
    ]))
}

fn expand_if(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.len() != 4 {
        return Ok(Expr::List(items.to_vec()));
    }

    Ok(Expr::List(vec![
        items[0].clone(),
        expand_expr(&items[1], macros)?,
        expand_expr(&items[2], macros)?,
        expand_expr(&items[3], macros)?,
    ]))
}

fn expand_lambda(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Ok(Expr::List(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone(), items[1].clone()];
    expanded.extend(
        items[2..]
            .iter()
            .map(|item| expand_expr(item, macros))
            .collect::<Result<Vec<_>, _>>()?,
    );
    Ok(Expr::List(expanded))
}

fn expand_case_lambda(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.len() <= 1 {
        return Ok(Expr::List(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone()];
    for clause in &items[1..] {
        match clause {
            Expr::List(parts) if parts.len() >= 2 => {
                let mut expanded_clause = vec![parts[0].clone()];
                expanded_clause.extend(
                    parts[1..]
                        .iter()
                        .map(|item| expand_expr(item, macros))
                        .collect::<Result<Vec<_>, _>>()?,
                );
                expanded.push(Expr::List(expanded_clause));
            }
            _ => expanded.push(clause.clone()),
        }
    }

    Ok(Expr::List(expanded))
}

fn expand_tail_forms(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    let mut expanded = vec![items[0].clone()];
    expanded.extend(
        items[1..]
            .iter()
            .map(|item| expand_expr(item, macros))
            .collect::<Result<Vec<_>, _>>()?,
    );
    Ok(Expr::List(expanded))
}

fn expand_let(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Ok(Expr::List(items.to_vec()));
    }

    let mut expanded = vec![items[0].clone()];
    let (binding_index, body_index) = if matches!(&items[1], Expr::Symbol(_)) && items.len() >= 4 {
        expanded.push(items[1].clone());
        (2, 3)
    } else {
        (1, 2)
    };

    match &items[binding_index] {
        Expr::List(bindings) => {
            let mut expanded_bindings = Vec::with_capacity(bindings.len());
            for binding in bindings {
                match binding {
                    Expr::List(parts) if parts.len() == 2 => expanded_bindings.push(Expr::List(
                        vec![parts[0].clone(), expand_expr(&parts[1], macros)?],
                    )),
                    _ => expanded_bindings.push(binding.clone()),
                }
            }
            expanded.push(Expr::List(expanded_bindings));
        }
        other => expanded.push(other.clone()),
    }

    expanded.extend(
        items[body_index..]
            .iter()
            .map(|item| expand_expr(item, macros))
            .collect::<Result<Vec<_>, _>>()?,
    );
    Ok(Expr::List(expanded))
}

fn expand_cond(items: &[Expr], macros: &MacroTable) -> Result<Expr, EvalError> {
    let mut expanded = vec![items[0].clone()];
    for clause in &items[1..] {
        match clause {
            Expr::List(parts) if !parts.is_empty() => {
                let mut expanded_clause = Vec::with_capacity(parts.len());
                if matches!(&parts[0], Expr::Symbol(name) if name == "else") {
                    expanded_clause.push(parts[0].clone());
                } else {
                    expanded_clause.push(expand_expr(&parts[0], macros)?);
                }
                expanded_clause.extend(
                    parts[1..]
                        .iter()
                        .map(|item| expand_expr(item, macros))
                        .collect::<Result<Vec<_>, _>>()?,
                );
                expanded.push(Expr::List(expanded_clause));
            }
            _ => expanded.push(clause.clone()),
        }
    }

    Ok(Expr::List(expanded))
}

fn apply_macro(definition: &MacroDefinition, form: &Expr) -> Result<Expr, EvalError> {
    let mut env = TransformerEnv::default();
    env.bindings.insert(
        definition.parameter.clone(),
        SyntaxBinding::Single(expr_to_syntax(form, false)),
    );

    let result = eval_transformer_sequence(&definition.body, &env)?;
    let syntax = expect_syntax(result)?;
    Ok(syntax_to_expr(&hygienize_syntax(&syntax)))
}

fn eval_transformer_sequence(
    body: &[Expr],
    env: &TransformerEnv,
) -> Result<TransformerValue, EvalError> {
    let mut result = TransformerValue::Datum(Value::Void);
    for expr in body {
        result = eval_transformer_expr(expr, env)?;
    }
    Ok(result)
}

fn eval_transformer_expr(expr: &Expr, env: &TransformerEnv) -> Result<TransformerValue, EvalError> {
    match expr {
        Expr::Number(number) => Ok(TransformerValue::Datum(Value::Number(number.clone()))),
        Expr::Boolean(value) => Ok(TransformerValue::Datum(Value::Boolean(*value))),
        Expr::String(value) => Ok(TransformerValue::Datum(Value::String(value.clone()))),
        Expr::Symbol(name) => match env.bindings.get(name) {
            Some(SyntaxBinding::Single(value)) => Ok(TransformerValue::Syntax(value.clone())),
            Some(SyntaxBinding::Repeated(_)) => Err(EvalError::message(format!(
                "misused repeated syntax variable: {name}"
            ))),
            None => Err(EvalError::message(format!(
                "unbound transformer variable: {name}"
            ))),
        },
        Expr::List(items) => eval_transformer_list(items, env),
    }
}

fn eval_transformer_list(
    items: &[Expr],
    env: &TransformerEnv,
) -> Result<TransformerValue, EvalError> {
    if items.is_empty() {
        return Err(EvalError::message("cannot evaluate an empty transformer list"));
    }

    match expr_head_name(items) {
        Some("quote") => {
            if items.len() != 2 {
                return Err(EvalError::message(format!(
                    "quote: expected 1 argument(s), got {}",
                    items.len().saturating_sub(1)
                )));
            }
            Ok(TransformerValue::Datum(datum_to_transformer_value(&items[1])))
        }
        Some("syntax") => {
            if items.len() != 2 {
                return Err(EvalError::message(format!(
                    "syntax: expected 1 argument(s), got {}",
                    items.len().saturating_sub(1)
                )));
            }
            Ok(TransformerValue::Syntax(expand_syntax_template(&items[1], env)?))
        }
        Some("syntax-case") => eval_syntax_case(&items[1..], env),
        Some("with-syntax") => eval_with_syntax(&items[1..], env),
        Some("if") => eval_transformer_if(&items[1..], env),
        Some("begin") => eval_transformer_sequence(&items[1..], env),
        Some(name) => apply_transformer_builtin(name, &items[1..], env),
        None => Err(EvalError::message("unsupported transformer application")),
    }
}

fn eval_transformer_if(args: &[Expr], env: &TransformerEnv) -> Result<TransformerValue, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::message(format!(
            "if: expected 3 argument(s), got {}",
            args.len()
        )));
    }

    if transformer_truthy(&eval_transformer_expr(&args[0], env)?) {
        eval_transformer_expr(&args[1], env)
    } else {
        eval_transformer_expr(&args[2], env)
    }
}

fn eval_syntax_case(args: &[Expr], env: &TransformerEnv) -> Result<TransformerValue, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::message(
            "syntax-case: expected target, literals, and clause",
        ));
    }

    let target = expect_syntax(eval_transformer_expr(&args[0], env)?)?;
    let literals = parse_literals(&args[1])?;

    for clause in &args[2..] {
        let parts = match clause {
            Expr::List(parts) if parts.len() >= 2 && parts.len() <= 3 => parts,
            _ => return Err(EvalError::message("syntax-case: invalid clause")),
        };

        let (pattern, fender, template) = if parts.len() == 2 {
            (&parts[0], None, &parts[1])
        } else {
            (&parts[0], Some(&parts[1]), &parts[2])
        };

        let mut bindings = HashMap::new();
        if match_pattern(pattern, &target, &literals, &mut bindings)? {
            let clause_env = env.with_bindings(bindings);
            if let Some(fender) = fender {
                if !transformer_truthy(&eval_transformer_expr(fender, &clause_env)?) {
                    continue;
                }
            }
            return eval_transformer_expr(template, &clause_env);
        }
    }

    Err(EvalError::message("syntax-case: no matching clause"))
}

fn eval_with_syntax(args: &[Expr], env: &TransformerEnv) -> Result<TransformerValue, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::message(
            "with-syntax: expected bindings and body expressions",
        ));
    }

    let bindings = match &args[0] {
        Expr::List(bindings) => bindings,
        _ => return Err(EvalError::message("with-syntax: expected a binding list")),
    };

    let mut syntax_bindings = HashMap::new();
    for binding in bindings {
        let parts = match binding {
            Expr::List(parts) if parts.len() == 2 => parts,
            _ => return Err(EvalError::message("with-syntax: invalid binding")),
        };

        let name = match &parts[0] {
            Expr::Symbol(name) => name.clone(),
            _ => return Err(EvalError::message("with-syntax: expected syntax variable")),
        };

        let value = expect_syntax(eval_transformer_expr(&parts[1], env)?)?;
        syntax_bindings.insert(name, SyntaxBinding::Single(value));
    }

    eval_transformer_sequence(&args[1..], &env.with_bindings(syntax_bindings))
}

fn apply_transformer_builtin(
    name: &str,
    args: &[Expr],
    env: &TransformerEnv,
) -> Result<TransformerValue, EvalError> {
    let evaluated = args
        .iter()
        .map(|arg| eval_transformer_expr(arg, env))
        .collect::<Result<Vec<_>, _>>()?;

    match name {
        "syntax->datum" => {
            if evaluated.len() != 1 {
                return Err(EvalError::message(format!(
                    "syntax->datum: expected 1 argument(s), got {}",
                    evaluated.len()
                )));
            }
            Ok(TransformerValue::Datum(syntax_to_value(&expect_syntax(
                evaluated.into_iter().next().unwrap(),
            )?)))
        }
        "datum->syntax" => {
            if evaluated.len() != 2 {
                return Err(EvalError::message(format!(
                    "datum->syntax: expected 2 argument(s), got {}",
                    evaluated.len()
                )));
            }

            let mut iter = evaluated.into_iter();
            let _context = expect_syntax(iter.next().unwrap())?;
            let datum = expect_datum(iter.next().unwrap())?;
            Ok(TransformerValue::Syntax(value_to_syntax(&datum)?))
        }
        "string->symbol" => {
            if evaluated.len() != 1 {
                return Err(EvalError::message(format!(
                    "string->symbol: expected 1 argument(s), got {}",
                    evaluated.len()
                )));
            }
            Ok(TransformerValue::Datum(Value::Symbol(expect_string(
                &expect_datum(evaluated.into_iter().next().unwrap())?,
            )?)))
        }
        "string-append" => {
            let mut combined = String::new();
            for value in evaluated {
                combined.push_str(&expect_string(&expect_datum(value)?)?);
            }
            Ok(TransformerValue::Datum(Value::String(combined)))
        }
        "number->string" => {
            if evaluated.len() != 1 {
                return Err(EvalError::message(format!(
                    "number->string: expected 1 argument(s), got {}",
                    evaluated.len()
                )));
            }
            let number = expect_number_value(&expect_datum(evaluated.into_iter().next().unwrap())?)?;
            Ok(TransformerValue::Datum(Value::String(format_number(&number))))
        }
        "zero?" => unary_predicate(name, evaluated, |value| {
            Ok(matches!(
                value,
                Value::Number(number) if number.is_zero()
            ))
        }),
        "number?" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::Number(_)))),
        "string?" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::String(_)))),
        "symbol?" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::Symbol(_)))),
        "boolean?" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::Boolean(_)))),
        "pair?" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::Pair(_)))),
        "null?" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::Nil))),
        "not" => unary_predicate(name, evaluated, |value| Ok(matches!(value, Value::Boolean(false)))),
        "=" => {
            if evaluated.is_empty() {
                return Err(EvalError::message("=: expected at least 1 argument(s), got 0"));
            }
            let numbers = evaluated
                .into_iter()
                .map(expect_datum)
                .map(|value| value.and_then(|value| expect_number_value(&value)))
                .collect::<Result<Vec<_>, _>>()?;
            let all_equal = numbers
                .windows(2)
                .all(|window| window[0].compare(&window[1]) == std::cmp::Ordering::Equal);
            Ok(TransformerValue::Datum(Value::Boolean(all_equal)))
        }
        "equal?" => {
            if evaluated.len() != 2 {
                return Err(EvalError::message(format!(
                    "equal?: expected 2 argument(s), got {}",
                    evaluated.len()
                )));
            }
            let mut iter = evaluated.into_iter().map(expect_datum);
            let left = iter.next().unwrap()?;
            let right = iter.next().unwrap()?;
            Ok(TransformerValue::Datum(Value::Boolean(equal_values(
                &left, &right,
            ))))
        }
        _ => Err(EvalError::message(format!(
            "unsupported transformer builtin: {name}"
        ))),
    }
}

fn unary_predicate(
    name: &str,
    evaluated: Vec<TransformerValue>,
    predicate: impl Fn(&Value) -> Result<bool, EvalError>,
) -> Result<TransformerValue, EvalError> {
    if evaluated.len() != 1 {
        return Err(EvalError::message(format!(
            "{name}: expected 1 argument(s), got {}",
            evaluated.len()
        )));
    }

    let value = expect_datum(evaluated.into_iter().next().unwrap())?;
    Ok(TransformerValue::Datum(Value::Boolean(predicate(&value)?)))
}

fn match_pattern(
    pattern: &Expr,
    target: &SyntaxExpr,
    literals: &HashSet<String>,
    bindings: &mut SyntaxBindings,
) -> Result<bool, EvalError> {
    match pattern {
        Expr::Number(number) => Ok(matches!(
            target,
            SyntaxExpr::Number(candidate) if candidate.compare(number) == std::cmp::Ordering::Equal
        )),
        Expr::Boolean(value) => Ok(matches!(target, SyntaxExpr::Boolean(candidate) if candidate == value)),
        Expr::String(value) => Ok(matches!(target, SyntaxExpr::String(candidate) if candidate == value)),
        Expr::Symbol(name) if name == "_" => Ok(true),
        Expr::Symbol(name) if literals.contains(name) => Ok(matches!(
            target,
            SyntaxExpr::Symbol(identifier) if identifier.name == *name
        )),
        Expr::Symbol(name) => bind_single(name, target.clone(), bindings),
        Expr::List(items) => match_list_pattern(items, target, literals, bindings),
    }
}

fn match_list_pattern(
    patterns: &[Expr],
    target: &SyntaxExpr,
    literals: &HashSet<String>,
    bindings: &mut SyntaxBindings,
) -> Result<bool, EvalError> {
    let values = match target {
        SyntaxExpr::List(values) => values,
        _ => return Ok(false),
    };

    let ellipsis_index = patterns.iter().position(is_ellipsis);
    if let Some(index) = ellipsis_index {
        if index == 0 {
            return Err(EvalError::message("syntax-case: invalid ellipsis pattern"));
        }

        let prefix = &patterns[..index - 1];
        let repeated_pattern = &patterns[index - 1];
        let suffix = &patterns[index + 1..];

        if values.len() < prefix.len() + suffix.len() {
            return Ok(false);
        }

        let repeat_count = values.len() - prefix.len() - suffix.len();
        let mut local = bindings.clone();

        for (pattern, value) in prefix.iter().zip(values.iter()) {
            if !match_pattern(pattern, value, literals, &mut local)? {
                return Ok(false);
            }
        }

        let mut repeated = HashMap::<String, Vec<SyntaxExpr>>::new();
        for value in &values[prefix.len()..prefix.len() + repeat_count] {
            let mut sub = HashMap::new();
            if !match_pattern(repeated_pattern, value, literals, &mut sub)? {
                return Ok(false);
            }

            for (name, binding) in sub {
                match binding {
                    SyntaxBinding::Single(value) => repeated.entry(name).or_default().push(value),
                    SyntaxBinding::Repeated(_) => {
                        return Err(EvalError::message(
                            "syntax-case: nested ellipsis bindings are unsupported",
                        ))
                    }
                }
            }
        }

        for (name, values) in repeated {
            merge_binding(&name, SyntaxBinding::Repeated(values), &mut local)?;
        }

        for (pattern, value) in suffix
            .iter()
            .zip(values[values.len() - suffix.len()..].iter())
        {
            if !match_pattern(pattern, value, literals, &mut local)? {
                return Ok(false);
            }
        }

        *bindings = local;
        return Ok(true);
    }

    if patterns.len() != values.len() {
        return Ok(false);
    }

    let mut local = bindings.clone();
    for (pattern, value) in patterns.iter().zip(values.iter()) {
        if !match_pattern(pattern, value, literals, &mut local)? {
            return Ok(false);
        }
    }

    *bindings = local;
    Ok(true)
}

fn bind_single(
    name: &str,
    value: SyntaxExpr,
    bindings: &mut SyntaxBindings,
) -> Result<bool, EvalError> {
    match bindings.get(name) {
        Some(SyntaxBinding::Single(existing)) => Ok(syntax_eq(existing, &value)),
        Some(SyntaxBinding::Repeated(_)) => Err(EvalError::message(format!(
            "syntax-case: conflicting binding for {name}"
        ))),
        None => {
            bindings.insert(name.to_string(), SyntaxBinding::Single(value));
            Ok(true)
        }
    }
}

fn merge_binding(
    name: &str,
    binding: SyntaxBinding,
    bindings: &mut SyntaxBindings,
) -> Result<(), EvalError> {
    match (bindings.get(name), &binding) {
        (None, _) => {
            bindings.insert(name.to_string(), binding);
            Ok(())
        }
        (Some(SyntaxBinding::Single(existing)), SyntaxBinding::Single(value))
            if syntax_eq(existing, value) =>
        {
            Ok(())
        }
        (Some(SyntaxBinding::Repeated(existing)), SyntaxBinding::Repeated(values))
            if existing.len() == values.len()
                && existing
                    .iter()
                    .zip(values.iter())
                    .all(|(left, right)| syntax_eq(left, right)) =>
        {
            Ok(())
        }
        _ => Err(EvalError::message(format!(
            "syntax-case: conflicting binding for {name}"
        ))),
    }
}

fn parse_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let items = match expr {
        Expr::List(items) => items,
        _ => return Err(EvalError::message("syntax-case: expected a literal identifier list")),
    };

    let mut literals = HashSet::with_capacity(items.len());
    for item in items {
        match item {
            Expr::Symbol(name) => {
                literals.insert(name.clone());
            }
            _ => return Err(EvalError::message("syntax-case: literals must be identifiers")),
        }
    }
    Ok(literals)
}

fn expand_syntax_template(expr: &Expr, env: &TransformerEnv) -> Result<SyntaxExpr, EvalError> {
    expand_template_expr(expr, env, None)
}

fn expand_template_expr(
    expr: &Expr,
    env: &TransformerEnv,
    repeat_index: Option<usize>,
) -> Result<SyntaxExpr, EvalError> {
    match expr {
        Expr::Number(number) => Ok(SyntaxExpr::Number(number.clone())),
        Expr::Boolean(value) => Ok(SyntaxExpr::Boolean(*value)),
        Expr::String(value) => Ok(SyntaxExpr::String(value.clone())),
        Expr::Symbol(name) => {
            if let Some(binding) = env.bindings.get(name) {
                return match binding {
                    SyntaxBinding::Single(value) => Ok(value.clone()),
                    SyntaxBinding::Repeated(values) => match repeat_index {
                        Some(index) => values
                            .get(index)
                            .cloned()
                            .ok_or_else(|| EvalError::message("syntax: ellipsis out of range")),
                        None => Err(EvalError::message(format!(
                            "syntax: missing ellipsis for repeated variable {name}"
                        ))),
                    },
                };
            }

            Ok(SyntaxExpr::Symbol(SyntaxIdentifier {
                name: name.clone(),
                introduced: true,
            }))
        }
        Expr::List(items) => {
            if expr_head_name(items) == Some("quote") && items.len() == 2 {
                return Ok(SyntaxExpr::List(vec![
                    SyntaxExpr::Symbol(SyntaxIdentifier {
                        name: "quote".into(),
                        introduced: true,
                    }),
                    quoted_expr_to_syntax(&items[1]),
                ]));
            }

            let mut expanded = Vec::new();
            let mut index = 0;
            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                    let count = template_repeat_count(&items[index], env)?;
                    for repeat in 0..count {
                        expanded.push(expand_template_expr(&items[index], env, Some(repeat))?);
                    }
                    index += 2;
                } else {
                    expanded.push(expand_template_expr(&items[index], env, repeat_index)?);
                    index += 1;
                }
            }

            Ok(SyntaxExpr::List(expanded))
        }
    }
}

fn template_repeat_count(expr: &Expr, env: &TransformerEnv) -> Result<usize, EvalError> {
    let mut counts = Vec::new();
    collect_repeat_counts(expr, env, &mut counts);

    if counts.is_empty() {
        return Err(EvalError::message(
            "syntax: template ellipsis requires a repeated pattern variable",
        ));
    }

    let count = counts[0];
    if counts.iter().any(|candidate| *candidate != count) {
        return Err(EvalError::message(
            "syntax: mismatched repeated pattern variable lengths",
        ));
    }

    Ok(count)
}

fn collect_repeat_counts(expr: &Expr, env: &TransformerEnv, counts: &mut Vec<usize>) {
    match expr {
        Expr::Symbol(name) => {
            if let Some(SyntaxBinding::Repeated(values)) = env.bindings.get(name) {
                counts.push(values.len());
            }
        }
        Expr::List(items) => {
            if expr_head_name(items) == Some("quote") && items.len() == 2 {
                return;
            }

            for item in items {
                collect_repeat_counts(item, env, counts);
            }
        }
        _ => {}
    }
}

fn quoted_expr_to_syntax(expr: &Expr) -> SyntaxExpr {
    expr_to_syntax(expr, false)
}

fn expr_to_syntax(expr: &Expr, introduced: bool) -> SyntaxExpr {
    match expr {
        Expr::Number(number) => SyntaxExpr::Number(number.clone()),
        Expr::Boolean(value) => SyntaxExpr::Boolean(*value),
        Expr::String(value) => SyntaxExpr::String(value.clone()),
        Expr::Symbol(name) => SyntaxExpr::Symbol(SyntaxIdentifier {
            name: name.clone(),
            introduced,
        }),
        Expr::List(items) => SyntaxExpr::List(
            items.iter()
                .map(|item| expr_to_syntax(item, introduced))
                .collect(),
        ),
    }
}

fn syntax_to_expr(expr: &SyntaxExpr) -> Expr {
    match expr {
        SyntaxExpr::Number(number) => Expr::Number(number.clone()),
        SyntaxExpr::Boolean(value) => Expr::Boolean(*value),
        SyntaxExpr::String(value) => Expr::String(value.clone()),
        SyntaxExpr::Symbol(identifier) => Expr::Symbol(identifier.name.clone()),
        SyntaxExpr::List(items) => Expr::List(items.iter().map(syntax_to_expr).collect()),
    }
}

fn syntax_to_value(expr: &SyntaxExpr) -> Value {
    match expr {
        SyntaxExpr::Number(number) => Value::Number(number.clone()),
        SyntaxExpr::Boolean(value) => Value::Boolean(*value),
        SyntaxExpr::String(value) => Value::String(value.clone()),
        SyntaxExpr::Symbol(identifier) => Value::Symbol(identifier.name.clone()),
        SyntaxExpr::List(items) => list_value(items.iter().map(syntax_to_value)),
    }
}

fn value_to_syntax(value: &Value) -> Result<SyntaxExpr, EvalError> {
    Ok(match value {
        Value::Number(number) => SyntaxExpr::Number(number.clone()),
        Value::Boolean(value) => SyntaxExpr::Boolean(*value),
        Value::String(value) => SyntaxExpr::String(value.clone()),
        Value::Symbol(name) => SyntaxExpr::Symbol(SyntaxIdentifier {
            name: name.clone(),
            introduced: false,
        }),
        Value::Nil => SyntaxExpr::List(Vec::new()),
        Value::Pair(_) => SyntaxExpr::List(
            list_to_vec(value)?
                .into_iter()
                .map(|item| value_to_syntax(&item))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        other => {
            return Err(EvalError::message(format!(
                "datum->syntax: expected datum, got {}",
                type_name(other)
            )))
        }
    })
}

fn datum_to_transformer_value(expr: &Expr) -> Value {
    match expr {
        Expr::Number(number) => Value::Number(number.clone()),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => list_value(items.iter().map(datum_to_transformer_value)),
    }
}

fn hygienize_syntax(expr: &SyntaxExpr) -> SyntaxExpr {
    hygienize_expr(expr, &mut Vec::new())
}

fn hygienize_expr(expr: &SyntaxExpr, scopes: &mut Vec<HashMap<String, String>>) -> SyntaxExpr {
    match expr {
        SyntaxExpr::Number(_) | SyntaxExpr::Boolean(_) | SyntaxExpr::String(_) => expr.clone(),
        SyntaxExpr::Symbol(identifier) => rename_symbol(identifier, scopes),
        SyntaxExpr::List(items) if items.is_empty() => expr.clone(),
        SyntaxExpr::List(items) => match syntax_head_name(items) {
            Some("quote") => expr.clone(),
            Some("lambda") => hygienize_lambda(items, scopes),
            Some("let") => hygienize_let(items, scopes),
            Some("define") => hygienize_define(items, scopes),
            _ => SyntaxExpr::List(items.iter().map(|item| hygienize_expr(item, scopes)).collect()),
        },
    }
}

fn hygienize_lambda(items: &[SyntaxExpr], scopes: &mut Vec<HashMap<String, String>>) -> SyntaxExpr {
    if items.len() < 3 {
        return SyntaxExpr::List(items.iter().map(|item| hygienize_expr(item, scopes)).collect());
    }

    let mut scope = HashMap::new();
    let formals = hygienize_formals(&items[1], &mut scope);
    scopes.push(scope);
    let body = items[2..]
        .iter()
        .map(|item| hygienize_expr(item, scopes))
        .collect::<Vec<_>>();
    scopes.pop();

    let mut result = vec![items[0].clone(), formals];
    result.extend(body);
    SyntaxExpr::List(result)
}

fn hygienize_let(items: &[SyntaxExpr], scopes: &mut Vec<HashMap<String, String>>) -> SyntaxExpr {
    if items.len() < 3 {
        return SyntaxExpr::List(items.iter().map(|item| hygienize_expr(item, scopes)).collect());
    }

    let bindings = match &items[1] {
        SyntaxExpr::List(bindings) => bindings,
        _ => return SyntaxExpr::List(items.iter().map(|item| hygienize_expr(item, scopes)).collect()),
    };

    let mut scope = HashMap::new();
    let mut new_bindings = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            SyntaxExpr::List(parts) if parts.len() == 2 => {
                let name = match &parts[0] {
                    SyntaxExpr::Symbol(identifier) => hygienize_binder(identifier, &mut scope),
                    _ => hygienize_expr(&parts[0], scopes),
                };
                let value = hygienize_expr(&parts[1], scopes);
                new_bindings.push(SyntaxExpr::List(vec![name, value]));
            }
            _ => new_bindings.push(hygienize_expr(binding, scopes)),
        }
    }

    scopes.push(scope);
    let body = items[2..]
        .iter()
        .map(|item| hygienize_expr(item, scopes))
        .collect::<Vec<_>>();
    scopes.pop();

    let mut result = vec![items[0].clone(), SyntaxExpr::List(new_bindings)];
    result.extend(body);
    SyntaxExpr::List(result)
}

fn hygienize_define(items: &[SyntaxExpr], scopes: &mut Vec<HashMap<String, String>>) -> SyntaxExpr {
    if items.len() < 3 {
        return SyntaxExpr::List(items.iter().map(|item| hygienize_expr(item, scopes)).collect());
    }

    match &items[1] {
        SyntaxExpr::Symbol(identifier) => {
            let mut result = vec![items[0].clone(), identifier_as_expr(identifier)];
            result.extend(items[2..].iter().map(|item| hygienize_expr(item, scopes)));
            SyntaxExpr::List(result)
        }
        SyntaxExpr::List(signature) if !signature.is_empty() => {
            let mut scope = HashMap::new();
            let mut new_signature = Vec::with_capacity(signature.len());

            match &signature[0] {
                SyntaxExpr::Symbol(identifier) => {
                    new_signature.push(hygienize_binder(identifier, &mut scope));
                }
                other => new_signature.push(hygienize_expr(other, scopes)),
            }

            for parameter in &signature[1..] {
                match parameter {
                    SyntaxExpr::Symbol(identifier) if identifier.name == "." => {
                        new_signature.push(parameter.clone());
                    }
                    SyntaxExpr::Symbol(identifier) => {
                        new_signature.push(hygienize_binder(identifier, &mut scope));
                    }
                    _ => new_signature.push(hygienize_expr(parameter, scopes)),
                }
            }

            scopes.push(scope);
            let body = items[2..]
                .iter()
                .map(|item| hygienize_expr(item, scopes))
                .collect::<Vec<_>>();
            scopes.pop();

            let mut result = vec![items[0].clone(), SyntaxExpr::List(new_signature)];
            result.extend(body);
            SyntaxExpr::List(result)
        }
        _ => SyntaxExpr::List(items.iter().map(|item| hygienize_expr(item, scopes)).collect()),
    }
}

fn hygienize_formals(expr: &SyntaxExpr, scope: &mut HashMap<String, String>) -> SyntaxExpr {
    match expr {
        SyntaxExpr::Symbol(identifier) => hygienize_binder(identifier, scope),
        SyntaxExpr::List(items) => SyntaxExpr::List(
            items.iter()
                .map(|item| match item {
                    SyntaxExpr::Symbol(identifier) if identifier.name == "." => item.clone(),
                    SyntaxExpr::Symbol(identifier) => hygienize_binder(identifier, scope),
                    _ => item.clone(),
                })
                .collect(),
        ),
        _ => expr.clone(),
    }
}

fn hygienize_binder(
    identifier: &SyntaxIdentifier,
    scope: &mut HashMap<String, String>,
) -> SyntaxExpr {
    if identifier.introduced {
        let fresh = fresh_identifier(&identifier.name);
        scope.insert(identifier.name.clone(), fresh.clone());
        SyntaxExpr::Symbol(SyntaxIdentifier {
            name: fresh,
            introduced: false,
        })
    } else {
        identifier_as_expr(identifier)
    }
}

fn identifier_as_expr(identifier: &SyntaxIdentifier) -> SyntaxExpr {
    SyntaxExpr::Symbol(SyntaxIdentifier {
        name: identifier.name.clone(),
        introduced: false,
    })
}

fn rename_symbol(identifier: &SyntaxIdentifier, scopes: &[HashMap<String, String>]) -> SyntaxExpr {
    if identifier.introduced {
        for scope in scopes.iter().rev() {
            if let Some(name) = scope.get(&identifier.name) {
                return SyntaxExpr::Symbol(SyntaxIdentifier {
                    name: name.clone(),
                    introduced: false,
                });
            }
        }
    }

    identifier_as_expr(identifier)
}

fn fresh_identifier(base: &str) -> String {
    let counter = HYGIENE_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__macro_{base}_{counter}")
}

fn syntax_eq(left: &SyntaxExpr, right: &SyntaxExpr) -> bool {
    match (left, right) {
        (SyntaxExpr::Number(left), SyntaxExpr::Number(right)) => {
            left.compare(right) == std::cmp::Ordering::Equal
        }
        (SyntaxExpr::Boolean(left), SyntaxExpr::Boolean(right)) => left == right,
        (SyntaxExpr::String(left), SyntaxExpr::String(right)) => left == right,
        (SyntaxExpr::Symbol(left), SyntaxExpr::Symbol(right)) => left.name == right.name,
        (SyntaxExpr::List(left), SyntaxExpr::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| syntax_eq(left, right))
        }
        _ => false,
    }
}

fn expect_syntax(value: TransformerValue) -> Result<SyntaxExpr, EvalError> {
    match value {
        TransformerValue::Syntax(value) => Ok(value),
        TransformerValue::Datum(value) => Err(EvalError::message(format!(
            "expected syntax, got {}",
            type_name(&value)
        ))),
    }
}

fn expect_datum(value: TransformerValue) -> Result<Value, EvalError> {
    match value {
        TransformerValue::Datum(value) => Ok(value),
        TransformerValue::Syntax(_) => Err(EvalError::message("expected datum, got syntax")),
    }
}

fn expect_string(value: &Value) -> Result<String, EvalError> {
    match value {
        Value::String(value) => Ok(value.clone()),
        _ => Err(EvalError::message(format!(
            "expected string, got {}",
            type_name(value)
        ))),
    }
}

fn expect_number_value(value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Number(number) => Ok(number.clone()),
        _ => Err(EvalError::message(format!(
            "expected number, got {}",
            type_name(value)
        ))),
    }
}

fn transformer_truthy(value: &TransformerValue) -> bool {
    !matches!(value, TransformerValue::Datum(Value::Boolean(false)))
}

fn expr_head_name(items: &[Expr]) -> Option<&str> {
    match items.first() {
        Some(Expr::Symbol(name)) => Some(name.as_str()),
        _ => None,
    }
}

fn syntax_head_name(items: &[SyntaxExpr]) -> Option<&str> {
    match items.first() {
        Some(SyntaxExpr::Symbol(identifier)) => Some(identifier.name.as_str()),
        _ => None,
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name) if name == "...")
}

impl TransformerEnv {
    fn with_bindings(&self, bindings: SyntaxBindings) -> Self {
        let mut merged = self.bindings.clone();
        merged.extend(bindings);
        Self { bindings: merged }
    }
}
