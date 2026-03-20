pub mod error;
mod parse;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::String(s) => format!("\"{s}\""),
            Value::Symbol(s) => s.clone(),
            Value::Nil => "()".to_string(),
            Value::Pair(..) => {
                let mut out = String::from("(");
                self.display_list_inner(&mut out);
                out.push(')');
                out
            }
        }
    }

    fn display_list_inner(&self, out: &mut String) {
        let Value::Pair(car, cdr) = self else {
            out.push_str(&self.display());
            return;
        };
        out.push_str(&car.display());
        match cdr.as_ref() {
            Value::Nil => {}
            Value::Pair(..) => {
                out.push(' ');
                cdr.display_list_inner(out);
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.display());
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

// --- Evaluator ---

fn eval(value: &Value) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(value.clone()),
        Value::Nil => Ok(Value::Nil),
        Value::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::Pair(..) => {
            let items = value
                .to_list_vec()
                .ok_or_else(|| EvalError::TypeError {
                    expected: "proper list".to_string(),
                    got: value.display(),
                })?;

            let [operator, args @ ..] = items.as_slice() else {
                return Ok(Value::Nil);
            };

            if let Value::Symbol(name) = operator {
                return eval_builtin(name, args);
            }

            Err(EvalError::NotAProcedure {
                value: operator.display(),
            })
        }
    }
}

fn eval_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => eval_arithmetic(name, args),
        "<" | ">" | "=" | "<=" | ">=" => eval_comparison(name, args),
        "not" => eval_not(args),
        "and" => eval_and(args),
        "or" => eval_or(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn checked_div(acc: i64, v: i64) -> Result<i64, EvalError> {
    if v == 0 {
        Err(EvalError::DivisionByZero)
    } else {
        Ok(acc / v)
    }
}

fn eval_arithmetic(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let values: Vec<i64> = args
        .iter()
        .map(|a| {
            let evaled = eval(a)?;
            match evaled {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".to_string(),
                    got: other.display(),
                }),
            }
        })
        .collect::<Result<_, _>>()?;

    let result = match op {
        "+" => values.iter().sum(),
        "*" => values.iter().product(),
        "-" => {
            let [first, rest @ ..] = values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: 0,
                });
            };
            if rest.is_empty() {
                -first
            } else {
                rest.iter().fold(*first, |acc, &v| acc - v)
            }
        }
        "/" => {
            let [first, rest @ ..] = values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: 0,
                });
            };
            rest.iter().try_fold(*first, |acc, &v| checked_div(acc, v))?
        }
        _ => unreachable!("unexpected arithmetic operator: {op}"),
    };

    Ok(Value::Integer(result))
}

fn eval_comparison(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let [lhs, rhs] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let lhs = match eval(lhs)? {
        Value::Integer(n) => n,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    let rhs = match eval(rhs)? {
        Value::Integer(n) => n,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    let result = match op {
        "<" => lhs < rhs,
        ">" => lhs > rhs,
        "=" => lhs == rhs,
        "<=" => lhs <= rhs,
        ">=" => lhs >= rhs,
        _ => unreachable!("unexpected comparison operator: {op}"),
    };
    Ok(Value::Boolean(result))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_and(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse::parse_all(input)?;
    let last = exprs
        .iter()
        .map(eval)
        .next_back()
        .ok_or(EvalError::EmptyInput)??;
    Ok(last.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
