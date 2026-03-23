use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "string?", "number?", "boolean?", "pair?", "symbol?",
];

pub fn default_env() -> Rc<RefCell<Env>> {
    let env = Env::new();
    for &name in BUILTINS {
        env.borrow_mut().define(name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let span = expr.span();
    match expr {
        Value::Int(_) | Value::Bool(_) | Value::String(_) | Value::Builtin(_) => Ok(expr.clone()),
        Value::Closure { .. } => Ok(expr.clone()),
        Value::Symbol(name, _) => env.borrow().get(name).map_err(|e| e.at(span)),
        Value::List(elems, _) => eval_list(elems, span, env),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(
    elems: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse { msg: "empty application".into() }.at(span));
    }

    if let Value::Symbol(op, _) = &elems[0] {
        match op.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "if" => return eval_if(&elems[1..], span, env),
            "define" => return eval_define(&elems[1..], span, env),
            "lambda" => return eval_lambda(&elems[1..], span, env),
            "quote" => return eval_quote(&elems[1..], span),
            "let" => return eval_let(&elems[1..], span, env),
            "begin" => return eval_begin(&elems[1..], env),
            "cond" => return eval_cond(&elems[1..], env),
            _ => {}
        }
    }

    let func = eval(&elems[0], env)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval(e, env))
        .collect::<Result<_, _>>()?;

    apply(&func, &args).map_err(|e| e.at(span))
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(ref name) => apply_builtin(name, args),
        Value::Closure { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                });
            }
            let call_env = Env::with_parent(env);
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.borrow_mut().define(param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &call_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::NotAProcedure { value: other.to_string() }),
    }
}

fn eval_if(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse { msg: "if requires 2 or 3 arguments".into() }.at(span));
    }
    let cond = eval(&args[0], env)?;
    if !is_false(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse { msg: "define requires arguments".into() }.at(span));
    }
    match &args[0] {
        Value::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(
                    EvalError::Parse { msg: "define requires exactly 2 arguments".into() }
                        .at(span),
                );
            }
            let val = eval(&args[1], env)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse { msg: "define: empty signature".into() }.at(span));
            }
            let Value::Symbol(name, _) = &sig[0] else {
                return Err(
                    EvalError::Parse { msg: "define: expected function name".into() }.at(span),
                );
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s, _) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        msg: format!("define: expected parameter name, got {other}"),
                    }
                    .at(span)),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Parse { msg: "define: empty body".into() }.at(span));
            }
            let closure = Value::Closure {
                params,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), closure);
            Ok(Value::Void)
        }
        other => Err(EvalError::Parse {
            msg: format!("define: expected symbol or list, got {other}"),
        }
        .at(span)),
    }
}

fn eval_lambda(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(
            EvalError::Parse { msg: "lambda requires params and body".into() }.at(span),
        );
    }
    let Value::List(param_list, _) = &args[0] else {
        return Err(
            EvalError::Parse { msg: "lambda: expected parameter list".into() }.at(span),
        );
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|p| match p {
            Value::Symbol(s, _) => Ok(s.clone()),
            other => Err(EvalError::Parse {
                msg: format!("lambda: expected parameter name, got {other}"),
            }
            .at(span)),
        })
        .collect::<Result<_, _>>()?;
    let body = args[1..].to_vec();
    Ok(Value::Closure {
        params,
        body,
        env: Rc::clone(env),
    })
}

fn eval_quote(args: &[Value], span: Option<Span>) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse { msg: "quote requires exactly 1 argument".into() }.at(span));
    }
    Ok(args[0].clone())
}

fn eval_and(exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(true));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env)?;
        if is_false(&val) {
            return Ok(val);
        }
    }
    eval(&exprs[exprs.len() - 1], env)
}

fn eval_or(exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(false));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env)?;
        if !is_false(&val) {
            return Ok(val);
        }
    }
    eval(&exprs[exprs.len() - 1], env)
}

fn eval_let(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse { msg: "let requires bindings and body".into() }.at(span));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(
                EvalError::Parse { msg: "named let requires bindings and body".into() }.at(span),
            );
        }
        let Value::List(bindings, _) = &args[1] else {
            return Err(
                EvalError::Parse { msg: "let: expected bindings list".into() }.at(span),
            );
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings {
            let Value::List(pair, _) = binding else {
                return Err(
                    EvalError::Parse { msg: "let: expected binding pair".into() }.at(span),
                );
            };
            if pair.len() != 2 {
                return Err(
                    EvalError::Parse { msg: "let: binding must have 2 elements".into() }.at(span),
                );
            }
            let Value::Symbol(param, _) = &pair[0] else {
                return Err(
                    EvalError::Parse { msg: "let: expected variable name".into() }.at(span),
                );
            };
            params.push(param.clone());
            inits.push(eval(&pair[1], env)?);
        }
        let body = args[2..].to_vec();
        let let_env = Env::with_parent(env);
        let closure = Value::Closure {
            params: params.clone(),
            body,
            env: Rc::clone(&let_env),
        };
        let_env.borrow_mut().define(name.clone(), closure);
        let call_env = Env::with_parent(&let_env);
        for (param, init) in params.iter().zip(inits.iter()) {
            call_env.borrow_mut().define(param.clone(), init.clone());
        }
        let body = &args[2..];
        let mut result = Value::Void;
        for expr in body {
            result = eval(expr, &call_env)?;
        }
        return Ok(result);
    }
    let Value::List(bindings, _) = &args[0] else {
        return Err(EvalError::Parse { msg: "let: expected bindings list".into() }.at(span));
    };
    let let_env = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair, _) = binding else {
            return Err(EvalError::Parse { msg: "let: expected binding pair".into() }.at(span));
        };
        if pair.len() != 2 {
            return Err(
                EvalError::Parse { msg: "let: binding must have 2 elements".into() }.at(span),
            );
        }
        let Value::Symbol(name, _) = &pair[0] else {
            return Err(
                EvalError::Parse { msg: "let: expected variable name".into() }.at(span),
            );
        };
        let val = eval(&pair[1], env)?;
        let_env.borrow_mut().define(name.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &let_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    for clause in clauses {
        let Value::List(parts, _) = clause else {
            return Err(EvalError::Parse { msg: "cond: expected clause".into() });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse { msg: "cond: empty clause".into() });
        }
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env)?;
                }
                return Ok(result);
            }
        }
        let test = eval(&parts[0], env)?;
        if !is_false(&test) {
            let mut result = test;
            for expr in &parts[1..] {
                result = eval(expr, env)?;
            }
            return Ok(result);
        }
    }
    Ok(Value::Void)
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Bool(false))
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| match a {
            Value::Int(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".into(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().sum()))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            if nums.len() == 1 {
                Ok(Value::Int(-nums[0]))
            } else {
                let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
                Ok(Value::Int(result))
            }
        }
        "*" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().product()))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            for &n in &nums[1..] {
                if n == 0 {
                    return Err(EvalError::DivisionByZero);
                }
            }
            let result = nums[1..].iter().fold(nums[0], |acc, n| acc / n);
            Ok(Value::Int(result))
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(is_false(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            match &args[1] {
                Value::List(rest, _) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(rest.iter().cloned());
                    Ok(Value::List(new_list, None))
                }
                _ => Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    got: format!("{}", args[1]),
                }),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::TypeMismatch {
                    expected: "pair".into(),
                    got: format!("{}", args[0]),
                }),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec(), None))
                }
                _ => Err(EvalError::TypeMismatch {
                    expected: "pair".into(),
                    got: format!("{}", args[0]),
                }),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::List(v, _) if v.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec(), None)),
        "append" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            match (&args[0], &args[1]) {
                (Value::List(a, _), Value::List(b, _)) => {
                    let mut result = a.clone();
                    result.extend(b.iter().cloned());
                    Ok(Value::List(result, None))
                }
                (Value::List(_, _), other) => Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    got: format!("{other}"),
                }),
                (other, _) => Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::List(elems, _) => Ok(Value::Int(elems.len() as i64)),
                _ => Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    got: format!("{}", args[0]),
                }),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::String(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Int(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::List(v, _) if !v.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Symbol(_, _))))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn compare_nums(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let nums = require_ints(args)?;
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Bool(result))
}
