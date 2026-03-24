use std::collections::HashMap;
use crate::scheme::EvalError;
use crate::scheme::parser::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Void,
    Builtin(String),
}

impl Value {
    pub fn to_display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

pub struct Env {
    bindings: HashMap<String, Value>,
}

impl Env {
    pub fn default_env() -> Self {
        let mut bindings = HashMap::new();
        for name in ["+", "-", "*", "/", "<", ">", "=", "<=", "not", "and", "or"] {
            bindings.insert(name.to_string(), Value::Builtin(name.to_string()));
        }
        Env { bindings }
    }
}

pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => {
            env.bindings.get(name)
                .cloned()
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(items) if items.is_empty() => {
            Err(EvalError::Runtime("empty application".into()))
        }
        Expr::List(items) => {
            let head = &items[0];
            // Special forms: and, or
            if let Expr::Symbol(name) = head {
                match name.as_str() {
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    _ => {}
                }
            }

            let func = eval(head, env)?;
            let args: Vec<Value> = items[1..].iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;

            apply_builtin(&func, &args)
        }
    }
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Builtin(name) => name.as_str(),
        _ => return Err(EvalError::Type(format!("{:?} is not a procedure", func))),
    };

    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_int(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_int(&args[0])?));
            }
            let mut result = expect_int(&args[0])?;
            for a in &args[1..] {
                result -= expect_int(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_int(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = expect_int(&args[0])?;
            for a in &args[1..] {
                let d = expect_int(a)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".into()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => cmp_op(args, |a, b| a < b),
        ">" => cmp_op(args, |a, b| a > b),
        "=" => cmp_op(args, |a, b| a == b),
        "<=" => cmp_op(args, |a, b| a <= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires exactly 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::Runtime(format!("unknown builtin: {}", name))),
    }
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected number, got {:?}", v))),
    }
}

fn cmp_op(args: &[Value], op: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = expect_int(&args[0])?;
    for a in &args[1..] {
        let curr = expect_int(a)?;
        if !op(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}
