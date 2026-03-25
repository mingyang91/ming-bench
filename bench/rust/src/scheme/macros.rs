use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use super::{Ast, AstKind, Env, Environment, EvalError, Value};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(crate) fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{base}__macro_{n}")
}

pub(crate) fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "define" | "if" | "quote" | "lambda" | "let" | "let*" | "begin"
            | "cond" | "and" | "or" | "set!" | "string-set!"
            | "define-syntax" | "syntax-rules" | "define-record-type" | "case-lambda"
            | "letrec" | "letrec*" | "case" | "do" | "when" | "unless"
            | "syntax-case" | "syntax" | "with-syntax"
            | "guard" | "call/cc" | "call-with-current-continuation"
    )
}

#[derive(Debug, Clone)]
pub(crate) enum PatternBinding {
    Single(Ast),
    Ellipsis(Vec<Ast>),
}

pub(crate) fn match_pattern(
    pattern: &[Ast],
    form: &[Ast],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    let mut pi = 0;
    let mut fi = 0;

    while pi < pattern.len() {
        if matches!(&pattern[pi].kind, AstKind::Symbol(s) if s == "...") {
            pi += 1;
            continue;
        }

        let is_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1].kind, AstKind::Symbol(s) if s == "...");

        if is_ellipsis {
            match &pattern[pi].kind {
                AstKind::Symbol(name) if !literals.contains(name) && name != "_" => {
                    let remaining = count_non_ellipsis_remaining(&pattern[pi + 2..]);
                    let available = if form.len() >= fi + remaining {
                        form.len() - fi - remaining
                    } else {
                        return false;
                    };
                    bindings.insert(
                        name.clone(),
                        PatternBinding::Ellipsis(form[fi..fi + available].to_vec()),
                    );
                    fi += available;
                    pi += 2;
                }
                _ => return false,
            }
        } else {
            if fi >= form.len() {
                return false;
            }
            match &pattern[pi].kind {
                AstKind::Symbol(name) if literals.contains(name) => {
                    if !matches!(&form[fi].kind, AstKind::Symbol(s) if s == name) {
                        return false;
                    }
                }
                AstKind::Symbol(name) if name == "_" => {}
                AstKind::Symbol(name) => {
                    bindings.insert(name.clone(), PatternBinding::Single(form[fi].clone()));
                }
                AstKind::List(sub_pat) => {
                    if let AstKind::List(sub_form) = &form[fi].kind {
                        if !match_pattern(sub_pat, sub_form, literals, bindings) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                _ => return false,
            }
            pi += 1;
            fi += 1;
        }
    }

    fi == form.len()
}

fn count_non_ellipsis_remaining(pattern: &[Ast]) -> usize {
    let mut count = 0;
    let mut i = 0;
    while i < pattern.len() {
        if matches!(&pattern[i].kind, AstKind::Symbol(s) if s == "...") {
            i += 1;
            continue;
        }
        let is_ellipsis = i + 1 < pattern.len()
            && matches!(&pattern[i + 1].kind, AstKind::Symbol(s) if s == "...");
        if is_ellipsis {
            i += 2;
        } else {
            count += 1;
            i += 1;
        }
    }
    count
}

fn expand_ellipsis_element(
    element: &Ast,
    bindings: &HashMap<String, PatternBinding>,
    renames: &mut HashMap<String, String>,
    expanded: &mut Vec<Ast>,
) {
    let var_name = match find_ellipsis_var(element, bindings) {
        Some(name) => name,
        None => return,
    };
    let values = match bindings.get(&var_name) {
        Some(PatternBinding::Ellipsis(vals)) => vals,
        _ => return,
    };
    for val in values {
        let mut local_bindings = bindings.clone();
        local_bindings.insert(var_name.clone(), PatternBinding::Single(val.clone()));
        expanded.push(expand_template(element, &local_bindings, renames));
    }
}

fn expand_template(
    template: &Ast,
    bindings: &HashMap<String, PatternBinding>,
    renames: &mut HashMap<String, String>,
) -> Ast {
    match &template.kind {
        AstKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatternBinding::Single(ast) => return ast.clone(),
                    PatternBinding::Ellipsis(_) => return template.clone(),
                }
            }
            if is_special_form(name) || name == "..." {
                template.clone()
            } else {
                let gensym_name = renames
                    .entry(name.clone())
                    .or_insert_with(|| gensym(name))
                    .clone();
                Ast {
                    kind: AstKind::Symbol(gensym_name),
                    line: template.line,
                    col: template.col,
                }
            }
        }
        AstKind::List(elements) => {
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < elements.len() {
                if matches!(&elements[i].kind, AstKind::Symbol(s) if s == "...") {
                    i += 1;
                    continue;
                }
                let is_ellipsis = i + 1 < elements.len()
                    && matches!(&elements[i + 1].kind, AstKind::Symbol(s) if s == "...");

                if is_ellipsis {
                    expand_ellipsis_element(&elements[i], bindings, renames, &mut expanded);
                    i += 2;
                } else {
                    expanded.push(expand_template(&elements[i], bindings, renames));
                    i += 1;
                }
            }
            Ast {
                kind: AstKind::List(expanded),
                line: template.line,
                col: template.col,
            }
        }
        _ => template.clone(),
    }
}

fn find_ellipsis_var(
    template: &Ast,
    bindings: &HashMap<String, PatternBinding>,
) -> Option<String> {
    match &template.kind {
        AstKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatternBinding::Ellipsis(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        AstKind::List(elements) => {
            for elem in elements {
                if let Some(name) = find_ellipsis_var(elem, bindings) {
                    return Some(name);
                }
            }
            None
        }
        _ => None,
    }
}

pub(crate) fn expand_macro_form(
    literals: &[String],
    rules: &[(Ast, Ast)],
    def_env: &Env,
    form: &[Ast],
    use_env: &Env,
) -> Result<(Ast, Env), EvalError> {
    let form_args = &form[1..];

    for (pattern, template) in rules {
        let pattern_args = match &pattern.kind {
            AstKind::List(items) if !items.is_empty() => &items[1..],
            _ => continue,
        };

        let mut bindings = HashMap::new();
        if match_pattern(pattern_args, form_args, literals, &mut bindings) {
            let mut renames = HashMap::new();
            let expanded = expand_template(template, &bindings, &mut renames);

            if !renames.is_empty() {
                let to_set: Vec<_> = renames.iter()
                    .filter_map(|(original, gensym_name)| {
                        def_env.borrow().get(original).map(|val| (gensym_name.clone(), val))
                    })
                    .collect();
                for (gensym_name, val) in to_set {
                    use_env.borrow_mut().set(gensym_name, val);
                }
            }

            return Ok((expanded, Rc::clone(use_env)));
        }
    }

    Err(EvalError::Type("no matching macro pattern".into()))
}

pub(crate) fn eval_define_syntax(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(
            "define-syntax requires 2 arguments".into(),
        ));
    }
    let name = match &args[0].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
    };
    let sr = match &args[1].kind {
        AstKind::List(items) => items,
        _ => {
            return Err(EvalError::Type(
                "define-syntax: expected syntax-rules".into(),
            ))
        }
    };
    if sr.is_empty() || !matches!(&sr[0].kind, AstKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Type(
            "define-syntax: expected syntax-rules".into(),
        ));
    }
    if sr.len() < 2 {
        return Err(EvalError::Type(
            "syntax-rules: missing literals list".into(),
        ));
    }
    let literals = match &sr[1].kind {
        AstKind::List(lits) => lits
            .iter()
            .map(|l| match &l.kind {
                AstKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(
                    "syntax-rules: expected symbol in literals".into(),
                )),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalError::Type(
                "syntax-rules: expected literals list".into(),
            ))
        }
    };
    let mut rules = Vec::new();
    for rule in &sr[2..] {
        match &rule.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                rules.push((pair[0].clone(), pair[1].clone()));
            }
            _ => return Err(EvalError::Type("syntax-rules: invalid rule".into())),
        }
    }
    env.borrow_mut().set(
        name,
        Value::Macro {
            literals,
            rules,
            def_env: Rc::clone(env),
        },
    );
    Ok(Value::Void)
}

// ---- syntax-case template expansion ----

pub(crate) fn expand_syntax_form(
    template: &Ast,
    env: &Env,
) -> (Ast, Vec<(String, String)>) {
    let mut renames = HashMap::new();
    let expanded = expand_syntax_tmpl(template, env, &mut renames);
    let rename_pairs: Vec<(String, String)> = renames.into_iter().collect();
    (expanded, rename_pairs)
}

fn expand_syntax_tmpl(
    template: &Ast,
    env: &Env,
    renames: &mut HashMap<String, String>,
) -> Ast {
    match &template.kind {
        AstKind::Symbol(name) => {
            // Check if bound to a Syntax value (pattern variable)
            match env.borrow().get(name) {
                Some(Value::Syntax { ast, .. }) => return ast,
                Some(Value::SyntaxEllipsis(_)) => return template.clone(),
                _ => {}
            }
            if is_special_form(name) || name == "..." {
                template.clone()
            } else {
                let gensym_name = renames
                    .entry(name.clone())
                    .or_insert_with(|| gensym(name))
                    .clone();
                Ast {
                    kind: AstKind::Symbol(gensym_name),
                    line: template.line,
                    col: template.col,
                }
            }
        }
        AstKind::List(elements) => {
            // Don't expand inside quote
            if matches!(elements.first(), Some(Ast { kind: AstKind::Symbol(s), .. }) if s == "quote") {
                return template.clone();
            }
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < elements.len() {
                if matches!(&elements[i].kind, AstKind::Symbol(s) if s == "...") {
                    i += 1;
                    continue;
                }
                let is_ellipsis = i + 1 < elements.len()
                    && matches!(&elements[i + 1].kind, AstKind::Symbol(s) if s == "...");

                if is_ellipsis {
                    if let Some((var_name, asts)) = find_syntax_ellipsis_var(&elements[i], env) {
                        for ast in &asts {
                            let tmp_env = Environment::with_parent(env);
                            tmp_env.borrow_mut().set(
                                var_name.clone(),
                                Value::Syntax { ast: ast.clone(), renames: vec![], source_env: None },
                            );
                            expanded.push(expand_syntax_tmpl(&elements[i], &tmp_env, renames));
                        }
                    }
                    i += 2;
                } else {
                    expanded.push(expand_syntax_tmpl(&elements[i], env, renames));
                    i += 1;
                }
            }
            Ast {
                kind: AstKind::List(expanded),
                line: template.line,
                col: template.col,
            }
        }
        _ => template.clone(),
    }
}

fn find_syntax_ellipsis_var(template: &Ast, env: &Env) -> Option<(String, Vec<Ast>)> {
    match &template.kind {
        AstKind::Symbol(name) => {
            if let Some(Value::SyntaxEllipsis(asts)) = env.borrow().get(name) {
                Some((name.clone(), asts))
            } else {
                None
            }
        }
        AstKind::List(elements) => {
            for elem in elements {
                if let result @ Some(_) = find_syntax_ellipsis_var(elem, env) {
                    return result;
                }
            }
            None
        }
        _ => None,
    }
}
