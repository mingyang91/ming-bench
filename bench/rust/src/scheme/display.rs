use std::fmt;
use std::rc::Rc;

use super::Val;

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Float(x) => {
                if x.fract() == 0.0 && x.is_finite() {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Val::Rational(n, d) => write!(f, "{}/{}", n, d),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Pair(rc) => {
                use std::collections::HashSet;
                let mut seen = HashSet::new();
                seen.insert(Rc::as_ptr(rc) as usize);
                let (car, cdr) = {
                    let pair = rc.borrow();
                    (format!("{}", pair.0), pair.1.clone())
                };
                write!(f, "({car}")?;
                let mut cur = cdr;
                loop {
                    match &cur {
                        Val::List(v) if v.is_empty() => break,
                        Val::List(v) => {
                            // Non-empty quoted list as tail: print elements inline
                            for e in v {
                                write!(f, " {e}")?;
                            }
                            break;
                        }
                        Val::Pair(rc2) => {
                            let ptr = Rc::as_ptr(rc2) as usize;
                            if !seen.insert(ptr) {
                                write!(f, " ...")?;
                                break;
                            }
                            let (car2, cdr2) = {
                                let p = rc2.borrow();
                                (format!("{}", p.0), p.1.clone())
                            };
                            write!(f, " {car2}")?;
                            cur = cdr2;
                        }
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Val::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } | Val::CaseLambda { .. } | Val::Builtin(..) | Val::Macro { .. }
            | Val::SyntaxCaseMacro { .. }
            | Val::CallCC | Val::DynamicWind | Val::Raise | Val::WithExcHandler
            | Val::Values | Val::CallWithValues
            | Val::Continuation(..) => write!(f, "#<procedure>"),
            Val::SyntaxObject(_) => write!(f, "#<syntax>"),
            Val::MultipleValues(_) => write!(f, "#<values>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}
