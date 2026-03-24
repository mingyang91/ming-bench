use crate::scheme::parser::{Expr, Pos};
use crate::scheme::{Env, Kont, State};
use crate::scheme::error::EvalError;

pub(super) fn qq_transform(expr: &Expr) -> Expr {
    let p = expr.pos();
    match expr {
        Expr::List(elems, _) if !elems.is_empty() => {
            // Check for (unquote x)
            if let Expr::Symbol(s, _) = &elems[0] {
                if s == "unquote" && elems.len() == 2 {
                    return elems[1].clone();
                }
            }
            // Process list elements, building up with cons/append
            qq_transform_list(elems, None, p)
        }
        Expr::DottedList(elems, tail, _) => {
            // Check for dotted list like `(a b . ,c)
            qq_transform_list(elems, Some(tail.as_ref()), p)
        }
        _ => {
            // Atoms are quoted
            Expr::List(vec![Expr::Symbol("quote".into(), p), expr.clone()], p)
        }
    }
}

fn qq_transform_list(elems: &[Expr], tail: Option<&Expr>, p: Pos) -> Expr {
    // Start with the tail (nil for proper list, transformed tail for dotted list)
    let base = if let Some(t) = tail {
        qq_transform(t)
    } else {
        Expr::List(vec![Expr::Symbol("quote".into(), p), Expr::List(vec![], p)], p)
    };

    // Process elements right-to-left
    elems.iter().rev().fold(base, |acc, elem| {
        // Check for (unquote-splicing x)
        if let Expr::List(inner, _) = elem {
            if inner.len() == 2 {
                if let Expr::Symbol(s, _) = &inner[0] {
                    if s == "unquote-splicing" {
                        return Expr::List(
                            vec![Expr::Symbol("append".into(), p), inner[1].clone(), acc],
                            p,
                        );
                    }
                }
            }
        }
        Expr::List(
            vec![Expr::Symbol("cons".into(), p), qq_transform(elem), acc],
            p,
        )
    })
}

pub(super) fn eval_quasiquote(expr: &Expr, env: &Env, kont: Kont) -> Result<State, EvalError> {
    let transformed = qq_transform(expr);
    Ok(State::Eval(transformed, env.clone(), kont))
}
