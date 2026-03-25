//! Special form evaluation helpers used by the CEK machine.

use super::{eval, gensym, Env, EvalError, Expr, ExprKind, Span, Val};

pub(super) fn parse_params(exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= exprs.len() {
                    return Err(EvalError::Parse(format!("missing rest parameter after . at {span}")));
                }
                rest_param = Some(match &exprs[i + 1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("expected rest parameter name at {span}"))),
                });
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Parse(format!("expected parameter name at {span}"))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

pub(super) fn eval_define_record_type(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Parse(format!("define-record-type: expected at least 3 arguments at {span}")));
    }

    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(elems) if !elems.is_empty() => {
            let cname = match &elems[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define-record-type: expected constructor name at {span}"))),
            };
            let fields: Vec<String> = elems[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("define-record-type: expected field name at {span}"))),
            }).collect::<Result<_, _>>()?;
            (cname, fields)
        }
        _ => return Err(EvalError::Parse(format!("define-record-type: expected constructor at {span}"))),
    };

    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse(format!("define-record-type: expected predicate name at {span}"))),
    };

    // Parse field accessors
    let mut accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(elems) if elems.len() >= 2 => {
                let field = match &elems[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define-record-type: expected field name at {span}"))),
                };
                let accessor = match &elems[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define-record-type: expected accessor name at {span}"))),
                };
                accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Parse(format!("define-record-type: expected field spec at {span}"))),
        }
    }

    let tag = gensym("record");

    // Constructor
    {
        let params = constructor_fields.clone();
        let tag_sym = tag.clone();
        let span0 = Span::new(0, 0);
        let mut list_args = vec![
            Expr::new(ExprKind::Symbol("list".into()), span0),
            Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".into()), span0),
                Expr::new(ExprKind::Symbol(tag_sym), span0),
            ]), span0),
        ];
        for p in &params {
            list_args.push(Expr::new(ExprKind::Symbol(p.clone()), span0));
        }
        let body = vec![Expr::new(ExprKind::List(list_args), span0)];

        env.define(constructor_name, Val::Lambda {
            params,
            rest_param: None,
            body,
            env: env.clone(),
        });
    }

    // Predicate
    {
        let tag_sym = tag.clone();
        let param = "__rec_v".to_string();
        let span0 = Span::new(0, 0);
        let body = vec![Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("and".into()), span0),
            Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("pair?".into()), span0),
                Expr::new(ExprKind::Symbol(param.clone()), span0),
            ]), span0),
            Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("equal?".into()), span0),
                Expr::new(ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("car".into()), span0),
                    Expr::new(ExprKind::Symbol(param.clone()), span0),
                ]), span0),
                Expr::new(ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("quote".into()), span0),
                    Expr::new(ExprKind::Symbol(tag_sym), span0),
                ]), span0),
            ]), span0),
        ]), span0)];

        env.define(pred_name, Val::Lambda {
            params: vec![param],
            rest_param: None,
            body,
            env: env.clone(),
        });
    }

    // Accessors
    for (field_name, accessor_name) in &accessors {
        let idx = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Parse(format!(
                "define-record-type: field '{}' not in constructor at {span}", field_name
            )))?;
        let span0 = Span::new(0, 0);
        let param = "__rec_v".to_string();
        let body = vec![Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("list-ref".into()), span0),
            Expr::new(ExprKind::Symbol(param.clone()), span0),
            Expr::new(ExprKind::Int((idx + 1) as i64), span0),
        ]), span0)];

        env.define(accessor_name.clone(), Val::Lambda {
            params: vec![param],
            rest_param: None,
            body,
            env: env.clone(),
        });
    }

    Ok(Val::Void)
}

pub(super) fn eval_define_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("define-syntax: expected 2 arguments at {span}")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse(format!("define-syntax: expected symbol at {span}"))),
    };
    let transformer = match &args[1].kind {
        ExprKind::List(elems) => elems,
        _ => return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules or lambda at {span}"))),
    };
    if transformer.is_empty() {
        return Err(EvalError::Parse(format!("define-syntax: empty transformer at {span}")));
    }

    // Check if transformer is a lambda (syntax-case style)
    if matches!(&transformer[0].kind, ExprKind::Symbol(s) if s == "lambda") {
        let lambda_val = eval_lambda(&transformer[1..], env, span)?;
        env.define(
            name,
            Val::SyntaxCaseMacro {
                transformer: Box::new(lambda_val),
                def_env: env.clone(),
            },
        );
        return Ok(Val::Void);
    }

    // Otherwise expect syntax-rules
    if !matches!(&transformer[0].kind, ExprKind::Symbol(s) if s == "syntax-rules")
    {
        return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules or lambda at {span}")));
    }
    if transformer.len() < 2 {
        return Err(EvalError::Parse(format!("syntax-rules: missing literals at {span}")));
    }
    let literals = match &transformer[1].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!(
                    "syntax-rules: expected literal symbol at {span}"
                ))),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalError::Parse(format!(
                "syntax-rules: expected literals list at {span}"
            )))
        }
    };
    let mut rules = Vec::new();
    for rule_expr in &transformer[2..] {
        match &rule_expr.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "syntax-rules: invalid rule at {span}"
                )))
            }
        }
    }
    env.define(
        name,
        Val::Macro {
            literals,
            rules,
            def_env: env.clone(),
        },
    );
    Ok(Val::Void)
}

pub(super) fn eval_quote(args: &[Expr], span: Span) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("quote: expected 1 argument at {span}")));
    }
    expr_to_val(&args[0])
}

pub(super) fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Float(x) => Ok(Val::Float(*x)),
        ExprKind::Rational(n, d) => Ok(Val::Rational(*n, *d)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(s) => Ok(Val::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Val> = elems.iter().map(expr_to_val).collect::<Result<_, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

pub(super) fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("lambda: missing parameters at {span}")));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
        ExprKind::Symbol(s) => {
            // (lambda rest body...) — single rest param
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {span}"))),
    };
    let body = args[1..].to_vec();
    Ok(Val::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

pub(super) fn eval_case_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    let mut clauses = Vec::new();
    for clause in args {
        match &clause.kind {
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Err(EvalError::Parse(format!("case-lambda: empty clause at {span}")));
                }
                let (params, rest_param) = match &elems[0].kind {
                    ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
                    ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Parse(format!("case-lambda: expected parameter list at {span}"))),
                };
                let body = elems[1..].to_vec();
                clauses.push((params, rest_param, body));
            }
            _ => return Err(EvalError::Parse(format!("case-lambda: expected clause at {span}"))),
        }
    }
    Ok(Val::CaseLambda { clauses, env: env.clone() })
}

pub(super) fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("string-set!: expected 3 arguments at {span}")));
    }
    // String literals are immutable (L15)
    if matches!(&args[0].kind, ExprKind::Str(_)) {
        return Err(EvalError::Runtime("string-set!: strings are immutable".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("string-set!: expected variable at {span}"))),
    };
    let idx_val = eval(&args[1], env)?;
    let idx = match idx_val {
        Val::Int(n) => n as usize,
        _ => return Err(EvalError::Type("string-set!: expected integer index".into())),
    };
    let char_val = eval(&args[2], env)?;
    let ch = match char_val {
        Val::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected character".into())),
    };
    let current = env.get(&name).ok_or_else(|| EvalError::UnboundVariable(format!("{name} at {span}")))?;
    match current {
        Val::Str(s) => {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Runtime("string-set!: index out of range".into()));
            }
            chars[idx] = ch;
            let new_str: String = chars.into_iter().collect();
            env.set(&name, Val::Str(new_str))?;
            Ok(Val::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string".into())),
    }
}

pub(super) fn eval_set_car(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("set-car!: expected 2 arguments at {span}")));
    }
    let pair_val = eval(&args[0], env)?;
    let new_car = eval(&args[1], env)?;
    match pair_val {
        Val::Pair(rc) => {
            rc.borrow_mut().0 = new_car;
            Ok(Val::Void)
        }
        _ => Err(EvalError::Type("set-car!: expected pair".into())),
    }
}

pub(super) fn eval_set_cdr(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("set-cdr!: expected 2 arguments at {span}")));
    }
    let pair_val = eval(&args[0], env)?;
    let new_cdr = eval(&args[1], env)?;
    match pair_val {
        Val::Pair(rc) => {
            rc.borrow_mut().1 = new_cdr;
            Ok(Val::Void)
        }
        _ => Err(EvalError::Type("set-cdr!: expected pair".into())),
    }
}

pub(super) fn vals_eqv(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::List(a), Val::List(b)) if a.is_empty() && b.is_empty() => true,
        (Val::Void, Val::Void) => true,
        _ => std::ptr::eq(a as *const Val, b as *const Val),
    }
}
