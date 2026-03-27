use std::collections::{HashMap, HashSet};

use super::{
    env_define, env_lookup, eval, gensym, is_builtin, new_frame, Env, EvalError, Expr, ExprKind,
    Value,
};

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

pub(super) fn eval_define_syntax(
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
    let macro_val = parse_syntax_rules(&args[1], env)?;
    env_define(env, name, macro_val);
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
        ExprKind::List(items) => {
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
