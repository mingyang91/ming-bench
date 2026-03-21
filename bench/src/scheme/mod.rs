pub mod error;
mod eval;
mod macros;
mod parse;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct Span {
    line: usize,
    col: usize,
}

/// Internal state for call/cc continuation support.
struct CcStateInner {
    expr_idx: usize,
    override_value: Option<Value>,
    return_data: Option<(usize, Value)>,
}

impl std::fmt::Debug for CcStateInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CcStateInner")
            .field("expr_idx", &self.expr_idx)
            .finish_non_exhaustive()
    }
}

/// Key for storing CcState in the environment.
const CC_KEY: &str = "\0cc";

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Char(char),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Rc<Value>,
        closure: Env,
    },
    BuiltinProc(String),
    Continuation {
        expr_idx: usize,
        cc: Rc<RefCell<CcStateInner>>,
    },
    CcState(Rc<RefCell<CcStateInner>>),
    SyntaxRules {
        name: String,
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        def_env: Env,
    },
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::String(s) => format!("\"{s}\""),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{c}"),
            Value::Nil => "()".to_string(),
            Value::Lambda { .. } | Value::BuiltinProc(_) | Value::Continuation { .. } => {
                "#<procedure>".to_string()
            }
            Value::SyntaxRules { .. } => "#<syntax>".to_string(),
            Value::CcState(_) => "#<cc-state>".to_string(),
            Value::Pair(..) => self.fmt_list(false),
        }
    }

    /// Human-readable display (no quotes around strings).
    fn display_human(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Pair(..) => self.fmt_list(true),
            _ => self.display(),
        }
    }

    fn display_for(&self, human: bool) -> String {
        if human { self.display_human() } else { self.display() }
    }

    fn fmt_list(&self, human: bool) -> String {
        let mut out = String::from("(");
        self.display_list_inner(&mut out, human);
        out.push(')');
        out
    }

    fn display_list_inner(&self, out: &mut String, human: bool) {
        let Value::Pair(car, cdr) = self else {
            unreachable!("display_list_inner called on non-pair");
        };
        out.push_str(&car.display_for(human));
        match cdr.as_ref() {
            Value::Nil => {}
            Value::Pair(..) => {
                out.push(' ');
                cdr.display_list_inner(out, human);
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.display_for(human));
            }
        }
    }

    fn to_list_vec(&self) -> Option<Vec<Value>> {
        let mut result = Vec::new();
        let mut current = self;
        while let Value::Pair(car, cdr) = current {
            result.push(car.as_ref().clone());
            current = cdr.as_ref();
        }
        matches!(current, Value::Nil).then_some(result)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::BuiltinProc(a), Value::BuiltinProc(b)) => a == b,
            (
                Value::Continuation { expr_idx: a, .. },
                Value::Continuation { expr_idx: b, .. },
            ) => a == b,
            (Value::SyntaxRules { name: a, .. }, Value::SyntaxRules { name: b, .. }) => a == b,
            _ => false,
        }
    }
}

// --- Environment ---

type Env = HashMap<String, Rc<RefCell<Value>>>;

/// Evaluate expressions from `start_idx`, updating `last` with each result.
/// Returns `Some((replay_idx, value))` on continuation escape, `None` if all
/// expressions completed normally.
/// Returns `Some((source_idx, target_idx, value))` on continuation escape.
fn eval_exprs_until_escape(
    exprs: &[(Value, Span)],
    start_idx: usize,
    cc_state: &Rc<RefCell<CcStateInner>>,
    env: &mut Env,
    output: &mut String,
    last: &mut Option<Value>,
) -> Result<Option<(usize, usize, Value)>, EvalError> {
    for (idx, (expr, span)) in exprs.iter().enumerate().skip(start_idx) {
        cc_state.borrow_mut().expr_idx = idx;

        match eval::eval(expr, env, output) {
            Ok(val) => *last = Some(val),
            Err(EvalError::ContinuationEscape) => {
                let (target_idx, value) = cc_state
                    .borrow_mut()
                    .return_data
                    .take()
                    .expect("ContinuationEscape without return data");
                return Ok(Some((idx, target_idx, value)));
            }
            Err(e) => {
                return Err(EvalError::AtPosition {
                    line: span.line,
                    col: span.col,
                    source: Box::new(e),
                });
            }
        }
    }
    Ok(None)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (result, _output) = eval_str_with_output(input)?;
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse::parse_all(input)?;
    let mut env = Env::new();
    let mut output = String::new();

    // Initialize call/cc state
    let cc_state = Rc::new(RefCell::new(CcStateInner {
        expr_idx: 0,
        override_value: None,
        return_data: None,
    }));
    env.insert(
        CC_KEY.into(),
        Rc::new(RefCell::new(Value::CcState(cc_state.clone()))),
    );

    let mut start_idx = 0;
    let mut last = None;

    loop {
        let cont_return = eval_exprs_until_escape(
            &exprs,
            start_idx,
            &cc_state,
            &mut env,
            &mut output,
            &mut last,
        )?;

        let Some((source_idx, target_idx, value)) = cont_return else {
            break;
        };
        // Only deliver override_value when the escape originates from
        // a different expression than the target. When source == target,
        // the continuation was invoked within the same top-level
        // expression and the override would be consumed by the wrong
        // call/cc during replay.
        if source_idx != target_idx {
            cc_state.borrow_mut().override_value = Some(value);
        }
        start_idx = target_idx;
    }

    let last = last.ok_or(EvalError::EmptyInput)?;
    Ok((last.display(), output))
}

#[cfg(test)]
mod tests;
