use std::collections::{HashMap, HashSet};

use super::{
    apply_proc, env_define, env_lookup, eval, gensym, is_builtin, is_truthy, new_frame, Env,
    EvalError, Expr, ExprKind, Frame, Span, Value,
};

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

pub(crate) fn eval_define_syntax(
    args: &[Expr],
    env: &mut Env,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("define-syntax requires 2 arguments".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
    };
    // Check if it's syntax-rules
    if let ExprKind::List(items) = &args[1].kind {
        if !items.is_empty() {
            if let ExprKind::Symbol(s) = &items[0].kind {
                if s == "syntax-rules" {
                    let macro_val = parse_syntax_rules(&args[1], env)?;
                    env_define(env, name, macro_val);
                    return Ok(Value::Boolean(false));
                }
            }
        }
    }
    // Otherwise evaluate (e.g., lambda transformer)
    let mut output = String::new();
    let val = eval(&args[1], env, &mut output)?;
    match &val {
        Value::Procedure(..) | Value::CaseLambda(..) => {
            env_define(env, name, Value::TransformerMacro(Box::new(val)));
        }
        _ => return Err(EvalError::Type("define-syntax: expected syntax-rules or lambda".into())),
    }
    Ok(Value::Boolean(false))
}

fn parse_syntax_rules(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let items = match &expr.kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("syntax-rules: expected list".into())),
    };
    if items.is_empty() {
        return Err(EvalError::Parse("syntax-rules: empty".into()));
    }
    if !matches!(&items[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Type("expected syntax-rules".into()));
    }
    if items.len() < 2 {
        return Err(EvalError::Arity("syntax-rules: need literals and rules".into()));
    }
    let literals = match &items[1].kind {
        ExprKind::List(lits) => {
            lits.iter()
                .map(|l| match &l.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type("syntax-rules: literal must be symbol".into())),
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type("syntax-rules: expected literals list".into())),
    };
    let mut rules = Vec::new();
    for clause in &items[2..] {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                let pattern = match &parts[0].kind {
                    ExprKind::List(p) => p.clone(),
                    _ => return Err(EvalError::Type("syntax-rules: pattern must be list".into())),
                };
                rules.push((pattern, parts[1].clone()));
            }
            _ => return Err(EvalError::Type("syntax-rules: invalid rule".into())),
        }
    }
    Ok(Value::Macro {
        literals,
        rules,
        def_env: env.clone(),
    })
}

fn match_pattern(
    pattern: &[Expr],
    input: &[Expr],
    bindings: &mut HashMap<String, MacroBinding>,
    literals: &[String],
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        // Check if next pattern element is ellipsis
        if pi + 1 < pattern.len() {
            if let ExprKind::Symbol(s) = &pattern[pi + 1].kind {
                if s == "..." {
                    // Current pattern matches zero or more remaining elements
                    if let ExprKind::Symbol(var) = &pattern[pi].kind {
                        let remaining: Vec<Expr> = input[ii..].to_vec();
                        bindings.insert(var.clone(), MacroBinding::Repeated(remaining));
                        return true; // ellipsis consumes the rest
                    }
                    return false;
                }
            }
        }
        if ii >= input.len() {
            return false;
        }
        match &pattern[pi].kind {
            ExprKind::Symbol(s) if literals.contains(s) => {
                if !matches!(&input[ii].kind, ExprKind::Symbol(is) if is == s) {
                    return false;
                }
            }
            ExprKind::Symbol(s) if s == "_" => {}
            ExprKind::Symbol(s) => {
                bindings.insert(s.clone(), MacroBinding::Single(input[ii].clone()));
            }
            ExprKind::List(sub_pat) => {
                if let ExprKind::List(sub_input) = &input[ii].kind {
                    if !match_pattern(sub_pat, sub_input, bindings, literals) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            ExprKind::Integer(n) => {
                if !matches!(&input[ii].kind, ExprKind::Integer(m) if m == n) {
                    return false;
                }
            }
            ExprKind::Boolean(b) => {
                if !matches!(&input[ii].kind, ExprKind::Boolean(b2) if b2 == b) {
                    return false;
                }
            }
            ExprKind::Rational(n, d) => {
                if !matches!(&input[ii].kind, ExprKind::Rational(n2, d2) if n2 == n && d2 == d) {
                    return false;
                }
            }
            ExprKind::Float(_) | ExprKind::Str(_) | ExprKind::Char(_) => return false,
        }
        pi += 1;
        ii += 1;
    }
    ii == input.len()
}

fn find_ellipsis_var(template: &Expr, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Repeated(_))) {
                Some(s.clone())
            } else {
                None
            }
        }
        ExprKind::List(items) => {
            for item in items {
                if let Some(v) = find_ellipsis_var(item, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn is_special_form(s: &str) -> bool {
    matches!(
        s,
        "define" | "set!" | "if" | "quote" | "lambda" | "case-lambda" | "and" | "or" | "let" | "begin" | "cond"
            | "define-syntax" | "syntax-rules" | "define-record-type"
            | "letrec" | "letrec*" | "case" | "do"
            | "call/cc" | "call-with-current-continuation"
            | "syntax-case" | "syntax" | "with-syntax"
    )
}

fn expand_ellipsis(
    pattern: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
) -> Vec<Expr> {
    let var = match find_ellipsis_var(pattern, bindings) {
        Some(v) => v,
        None => return Vec::new(),
    };
    let elems = match bindings.get(&var) {
        Some(MacroBinding::Repeated(elems)) => elems,
        _ => return Vec::new(),
    };
    elems
        .iter()
        .map(|elem| {
            let mut single_bindings = bindings.clone();
            single_bindings.insert(var.clone(), MacroBinding::Single(elem.clone()));
            expand_template(pattern, &single_bindings, pattern_vars, renames)
        })
        .collect()
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
) -> Expr {
    let span = template.span;
    match &template.kind {
        ExprKind::Symbol(s) if s == "..." => template.clone(),
        ExprKind::Symbol(s) => {
            if pattern_vars.contains(s) {
                if let Some(MacroBinding::Single(expr)) = bindings.get(s) {
                    return expr.clone();
                }
            }
            if is_special_form(s) || is_builtin(s) {
                return template.clone();
            }
            let gensym_name = renames
                .entry(s.clone())
                .or_insert_with(|| gensym(s))
                .clone();
            Expr {
                kind: ExprKind::Symbol(gensym_name),
                span,
            }
        }
        ExprKind::List(items) if !items.is_empty() => {
            // Don't expand inside quoted expressions
            if let ExprKind::Symbol(s) = &items[0].kind {
                if s == "quote" {
                    return template.clone();
                }
            }
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() {
                    if let ExprKind::Symbol(s) = &items[i + 1].kind {
                        if s == "..." {
                            expanded.extend(expand_ellipsis(
                                &items[i], bindings, pattern_vars, renames,
                            ));
                            i += 2;
                            continue;
                        }
                    }
                }
                expanded.push(expand_template(&items[i], bindings, pattern_vars, renames));
                i += 1;
            }
            Expr {
                kind: ExprKind::List(expanded),
                span,
            }
        }
        _ => template.clone(),
    }
}

pub(crate) fn expand_macro_only(
    macro_val: &Value,
    items: &[Expr],
    _env: &Env,
) -> Result<(Expr, super::Frame), EvalError> {
    let (literals, rules, def_env) = match macro_val {
        Value::Macro { literals, rules, def_env } => (literals, rules, def_env),
        _ => unreachable!(),
    };
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if match_pattern(&pattern[1..], &items[1..], &mut bindings, literals) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            let mut renames = HashMap::new();
            let expanded = expand_template(template, &bindings, &pattern_vars, &mut renames);
            let frame = new_frame();
            for (original, gensym_name) in &renames {
                if let Ok(val) = env_lookup(def_env, original) {
                    frame.borrow_mut().insert(gensym_name.clone(), val);
                }
            }
            return Ok((expanded, frame));
        }
    }
    Err(EvalError::Generic("no matching syntax-rules pattern".into()))
}

pub(super) fn expand_and_eval_macro(
    macro_val: &Value,
    items: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    let (literals, rules, def_env) = match macro_val {
        Value::Macro {
            literals,
            rules,
            def_env,
        } => (literals, rules, def_env),
        _ => unreachable!(),
    };
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if match_pattern(&pattern[1..], &items[1..], &mut bindings, literals) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            let mut renames = HashMap::new();
            let expanded = expand_template(template, &bindings, &pattern_vars, &mut renames);

            // Inject definition-site bindings for hygiene
            let frame = new_frame();
            for (original, gensym_name) in &renames {
                if let Ok(val) = env_lookup(def_env, original) {
                    frame.borrow_mut().insert(gensym_name.clone(), val);
                }
            }
            env.push(frame);
            let result = eval(&expanded, env, output);
            env.pop();
            return result;
        }
    }
    Err(EvalError::Generic("no matching syntax-rules pattern".into()))
}

// ── syntax-case support ──

/// Convert a Value (datum) to an Expr (syntax object representation).
pub(crate) fn value_to_expr(val: &Value) -> Result<Expr, EvalError> {
    let span = Span::default();
    match val {
        Value::Integer(n) => Ok(Expr { kind: ExprKind::Integer(*n), span }),
        Value::Symbol(s) => Ok(Expr { kind: ExprKind::Symbol(s.clone()), span }),
        Value::Boolean(b) => Ok(Expr { kind: ExprKind::Boolean(*b), span }),
        Value::Char(c) => Ok(Expr { kind: ExprKind::Char(*c), span }),
        Value::Str(s, _) => Ok(Expr { kind: ExprKind::Str(s.borrow().clone()), span }),
        Value::Float(f) => Ok(Expr { kind: ExprKind::Float(*f), span }),
        Value::Rational(n, d) => Ok(Expr { kind: ExprKind::Rational(*n, *d), span }),
        Value::List(items) if items.is_empty() => {
            Ok(Expr { kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("quote".into()), span },
                Expr { kind: ExprKind::List(vec![]), span },
            ]), span })
        }
        _ => Err(EvalError::Type("datum->syntax: cannot convert value to syntax".into())),
    }
}

/// Convert an Expr to a Value (datum).
fn expr_to_datum(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => Value::Symbol(s.clone()),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => Value::Rational(*n, *d),
        ExprKind::List(items) => {
            let vals: Vec<Value> = items.iter().map(expr_to_datum).collect();
            super::list_from_vec(&vals)
        }
    }
}

/// syntax->datum: unwrap a syntax object to its datum value
pub(crate) fn syntax_to_datum(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("syntax->datum requires 1 argument".into()));
    }
    match &args[0] {
        Value::Syntax(expr, _) => Ok(expr_to_datum(expr)),
        _ => Err(EvalError::Type("syntax->datum: expected syntax object".into())),
    }
}

/// datum->syntax: wrap a datum as a syntax object with given lexical context
pub(crate) fn datum_to_syntax(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("datum->syntax requires 2 arguments".into()));
    }
    let expr = value_to_expr(&args[1])?;
    Ok(Value::Syntax(expr, new_frame()))
}

/// Evaluate (syntax-case stx-expr (literals) clause ...)
pub(crate) fn eval_syntax_case(
    args: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::Arity("syntax-case requires at least 3 arguments".into()));
    }

    let stx_val = eval(&args[0], env, output)?;
    let stx_expr = match &stx_val {
        Value::Syntax(e, _) => e.clone(),
        _ => return Err(EvalError::Type("syntax-case: expected syntax object".into())),
    };

    let literals = match &args[1].kind {
        ExprKind::List(lits) => {
            lits.iter()
                .map(|l| match &l.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type("syntax-case: literal must be symbol".into())),
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type("syntax-case: expected literals list".into())),
    };

    let input_items = match &stx_expr.kind {
        ExprKind::List(items) => items.clone(),
        _ => vec![stx_expr.clone()],
    };

    for clause in &args[2..] {
        let clause_items = match &clause.kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type("syntax-case: clause must be list".into())),
        };

        if clause_items.len() < 2 || clause_items.len() > 3 {
            return Err(EvalError::Arity(
                "syntax-case: clause must have 2 or 3 parts".into(),
            ));
        }

        let (fender, body) = if clause_items.len() == 3 {
            (Some(&clause_items[1]), &clause_items[2])
        } else {
            (None, &clause_items[1])
        };

        let mut bindings = HashMap::new();
        let matched = match &clause_items[0].kind {
            ExprKind::List(pattern_items) => {
                match_pattern(pattern_items, &input_items, &mut bindings, &literals)
            }
            ExprKind::Symbol(s) if s == "_" => true,
            ExprKind::Symbol(s) => {
                bindings.insert(s.clone(), MacroBinding::Single(stx_expr.clone()));
                true
            }
            _ => false,
        };

        if matched {
            let frame = new_frame();
            for (name, binding) in &bindings {
                let val = match binding {
                    MacroBinding::Single(e) => Value::Syntax(e.clone(), new_frame()),
                    MacroBinding::Repeated(es) => Value::List(
                        es.iter()
                            .map(|e| Value::Syntax(e.clone(), new_frame()))
                            .collect(),
                    ),
                };
                frame.borrow_mut().insert(name.clone(), val);
            }

            if let Some(fender_expr) = fender {
                env.push(frame.clone());
                let fender_val = eval(fender_expr, env, output)?;
                env.pop();
                if !is_truthy(&fender_val) {
                    continue;
                }
            }

            env.push(frame);
            let result = eval(body, env, output)?;
            env.pop();
            return Ok(result);
        }
    }

    Err(EvalError::Generic("syntax-case: no matching pattern".into()))
}

/// Collect syntax bindings from env for template expansion.
fn collect_syntax_bindings(
    template: &Expr,
    env: &Env,
    pattern_vars: &mut HashSet<String>,
    bindings: &mut HashMap<String, MacroBinding>,
) {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if pattern_vars.contains(s) || bindings.contains_key(s) {
                return;
            }
            if let Ok(val) = env_lookup(env, s) {
                match val {
                    Value::Syntax(e, _) => {
                        pattern_vars.insert(s.clone());
                        bindings.insert(s.clone(), MacroBinding::Single(e));
                    }
                    Value::List(ref items)
                        if !items.is_empty() && matches!(&items[0], Value::Syntax(..)) =>
                    {
                        let exprs: Vec<Expr> = items
                            .iter()
                            .filter_map(|v| {
                                if let Value::Syntax(e, _) = v {
                                    Some(e.clone())
                                } else {
                                    None
                                }
                            })
                            .collect();
                        pattern_vars.insert(s.clone());
                        bindings.insert(s.clone(), MacroBinding::Repeated(exprs));
                    }
                    _ => {}
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_syntax_bindings(item, env, pattern_vars, bindings);
            }
        }
        _ => {}
    }
}

/// Evaluate (syntax template) — constructs a syntax object from a template
pub(crate) fn eval_syntax_form(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("syntax requires 1 argument".into()));
    }
    let template = &args[0];

    // If template is a single symbol bound to Syntax, return it directly
    if let ExprKind::Symbol(s) = &template.kind {
        if let Ok(val @ Value::Syntax(..)) = env_lookup(env, s) {
            return Ok(val);
        }
    }

    let mut pattern_vars = HashSet::new();
    let mut bindings = HashMap::new();
    collect_syntax_bindings(template, env, &mut pattern_vars, &mut bindings);

    let mut renames = HashMap::new();
    let expanded = expand_template(template, &bindings, &pattern_vars, &mut renames);

    let frame = new_frame();
    for (original, gensym_name) in &renames {
        if let Ok(val) = env_lookup(env, original) {
            frame.borrow_mut().insert(gensym_name.clone(), val);
        }
    }

    Ok(Value::Syntax(expanded, frame))
}

/// Evaluate (with-syntax ((pat expr) ...) body ...)
pub(crate) fn eval_with_syntax(
    args: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("with-syntax requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("with-syntax: expected bindings list".into())),
    };

    let frame = new_frame();
    for binding in bindings_list {
        let parts = match &binding.kind {
            ExprKind::List(p) if p.len() == 2 => p,
            _ => {
                return Err(EvalError::Type(
                    "with-syntax: binding must be (pattern expr)".into(),
                ))
            }
        };
        let name = match &parts[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type("with-syntax: pattern must be symbol".into())),
        };
        let val = eval(&parts[1], env, output)?;
        let syntax_val = match val {
            v @ Value::Syntax(..) => v,
            other => {
                let expr = value_to_expr(&other)?;
                Value::Syntax(expr, new_frame())
            }
        };
        frame.borrow_mut().insert(name, syntax_val);
    }

    env.push(frame);
    let mut result = Value::Boolean(false);
    for body_expr in &args[1..] {
        result = eval(body_expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

/// Expand a transformer macro: call the procedure with the syntax object,
/// return the expanded expr + hygiene frame.
pub(crate) fn expand_transformer(
    proc: &Value,
    items: &[Expr],
    output: &mut String,
) -> Result<(Expr, Frame), EvalError> {
    let span = if items.is_empty() {
        Span::default()
    } else {
        items[0].span
    };
    let stx = Value::Syntax(
        Expr {
            kind: ExprKind::List(items.to_vec()),
            span,
        },
        new_frame(),
    );
    let result = apply_proc(proc, &[stx], output)?;
    match result {
        Value::Syntax(expanded, hygiene_frame) => Ok((expanded, hygiene_frame)),
        _ => Err(EvalError::Type(
            "transformer must return syntax object".into(),
        )),
    }
}
