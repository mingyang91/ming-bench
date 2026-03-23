use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "string-copy",
];

pub fn default_env() -> Rc<RefCell<Env>> {
    let env = Env::new();
    for &name in BUILTINS {
        env.borrow_mut().define(name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

/// Trampoline: either a final value or a tail call to bounce.
enum Trampoline {
    Done(Value),
    Bounce { expr: Value, env: Rc<RefCell<Env>> },
}

pub fn eval(
    expr: &Value,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = Rc::clone(env);

    loop {
        match eval_inner(&current_expr, &current_env, output)? {
            Trampoline::Done(val) => return Ok(val),
            Trampoline::Bounce { expr: next_expr, env: next_env } => {
                current_expr = next_expr;
                current_env = next_env;
            }
        }
    }
}

/// Core eval that returns a Trampoline — tail positions return Bounce instead of recursing.
fn eval_inner(
    expr: &Value,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    let span = expr.span();
    match expr {
        Value::Int(_) | Value::Bool(_) | Value::String(_)
        | Value::Char(_) | Value::Builtin(_) => Ok(Trampoline::Done(expr.clone())),
        Value::Closure { .. } => Ok(Trampoline::Done(expr.clone())),
        Value::Symbol(name, _) => env
            .borrow()
            .get(name)
            .map(Trampoline::Done)
            .map_err(|e| e.at(span)),
        Value::List(elems, _) => eval_list(elems, span, env, output),
        Value::Void => Ok(Trampoline::Done(Value::Void)),
    }
}

fn eval_list(
    elems: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse { msg: "empty application".into() }.at(span));
    }

    if let Value::Symbol(op, _) = &elems[0] {
        match op.as_str() {
            "and" => return eval_and(&elems[1..], env, output),
            "or" => return eval_or(&elems[1..], env, output),
            "if" => return eval_if(&elems[1..], span, env, output),
            "define" => return eval_define(&elems[1..], span, env, output),
            "lambda" => return eval_lambda(&elems[1..], span, env),
            "quote" => return eval_quote(&elems[1..], span),
            "let" => return eval_let(&elems[1..], span, env, output),
            "begin" => return eval_begin(&elems[1..], env, output),
            "cond" => return eval_cond(&elems[1..], env, output),
            "set!" => return eval_set(&elems[1..], span, env, output),
            "string-set!" => return eval_string_set(&elems[1..], span, env, output),
            _ => {}
        }
    }

    let func = eval(&elems[0], env, output)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval(e, env, output))
        .collect::<Result<_, _>>()?;

    apply(&func, &args, output).map_err(|e| e.at(span))
}

/// Apply a function. For closures, returns a Bounce for TCO.
fn apply(
    func: &Value,
    args: &[Value],
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    match func {
        Value::Builtin(ref name) => apply_builtin(name, args, output).map(Trampoline::Done),
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
            eval_body(body, &call_env, output)
        }
        other => Err(EvalError::NotAProcedure { value: other.to_string() }),
    }
}

/// Evaluate a body sequence: eval all but last eagerly, return last as Bounce for TCO.
fn eval_body(
    body: &[Value],
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if body.is_empty() {
        return Ok(Trampoline::Done(Value::Void));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, env, output)?;
    }
    Ok(Trampoline::Bounce {
        expr: body[body.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_if(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse { msg: "if requires 2 or 3 arguments".into() }.at(span));
    }
    let cond = eval(&args[0], env, output)?;
    if !is_false(&cond) {
        Ok(Trampoline::Bounce { expr: args[1].clone(), env: Rc::clone(env) })
    } else if args.len() == 3 {
        Ok(Trampoline::Bounce { expr: args[2].clone(), env: Rc::clone(env) })
    } else {
        Ok(Trampoline::Done(Value::Void))
    }
}

fn eval_define(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
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
            let val = eval(&args[1], env, output)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Trampoline::Done(Value::Void))
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
            Ok(Trampoline::Done(Value::Void))
        }
        Value::Int(_) | Value::Bool(_) | Value::String(_) | Value::Char(_)
        | Value::Builtin(_) | Value::Closure { .. } | Value::Void => Err(EvalError::Parse {
            msg: format!("define: expected symbol or list, got {}", args[0]),
        }
        .at(span)),
    }
}

fn eval_set(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Parse { msg: "set! requires exactly 2 arguments".into() }.at(span));
    }
    let Value::Symbol(name, _) = &args[0] else {
        return Err(
            EvalError::Parse { msg: "set!: first argument must be a symbol".into() }.at(span),
        );
    };
    let val = eval(&args[1], env, output)?;
    env.borrow_mut().set(name, val)?;
    Ok(Trampoline::Done(Value::Void))
}

fn eval_lambda(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
) -> Result<Trampoline, EvalError> {
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
    Ok(Trampoline::Done(Value::Closure {
        params,
        body,
        env: Rc::clone(env),
    }))
}

fn eval_quote(args: &[Value], span: Option<Span>) -> Result<Trampoline, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse { msg: "quote requires exactly 1 argument".into() }.at(span));
    }
    Ok(Trampoline::Done(args[0].clone()))
}

fn eval_and(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Bool(true)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env, output)?;
        if is_false(&val) {
            return Ok(Trampoline::Done(val));
        }
    }
    Ok(Trampoline::Bounce {
        expr: exprs[exprs.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_or(
    exprs: &[Value],
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Bool(false)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env, output)?;
        if !is_false(&val) {
            return Ok(Trampoline::Done(val));
        }
    }
    Ok(Trampoline::Bounce {
        expr: exprs[exprs.len() - 1].clone(),
        env: Rc::clone(env),
    })
}

fn eval_let(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
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
            inits.push(eval(&pair[1], env, output)?);
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
        return eval_body(&args[2..], &call_env, output);
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
        let val = eval(&pair[1], env, output)?;
        let_env.borrow_mut().define(name.clone(), val);
    }
    eval_body(&args[1..], &let_env, output)
}

fn eval_begin(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    eval_body(args, env, output)
}

fn eval_cond(
    clauses: &[Value],
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    for clause in clauses {
        let Value::List(parts, _) = clause else {
            return Err(EvalError::Parse { msg: "cond: expected clause".into() });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse { msg: "cond: empty clause".into() });
        }
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                return eval_body(&parts[1..], env, output);
            }
        }
        let test = eval(&parts[0], env, output)?;
        if !is_false(&test) {
            if parts.len() == 1 {
                return Ok(Trampoline::Done(test));
            }
            return eval_body(&parts[1..], env, output);
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn eval_string_set(
    args: &[Value],
    span: Option<Span>,
    env: &Rc<RefCell<Env>>,
    output: &Rc<RefCell<String>>,
) -> Result<Trampoline, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() }.at(span));
    }
    let Value::Symbol(var_name, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol".into(),
            got: format!("{}", args[0]),
        }
        .at(span));
    };
    let idx_val = eval(&args[1], env, output)?;
    let Value::Int(idx) = idx_val else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".into(),
            got: format!("{idx_val}"),
        }
        .at(span));
    };
    let char_val = eval(&args[2], env, output)?;
    let Value::Char(ch) = char_val else {
        return Err(EvalError::TypeMismatch {
            expected: "char".into(),
            got: format!("{char_val}"),
        }
        .at(span));
    };
    let current = env.borrow().get(var_name).map_err(|e| e.at(span))?;
    let Value::String(s) = current else {
        return Err(EvalError::TypeMismatch {
            expected: "string".into(),
            got: format!("{current}"),
        }
        .at(span));
    };
    let idx = idx as usize;
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::TypeMismatch {
            expected: format!("index in range 0..{}", chars.len()),
            got: format!("{idx}"),
        }
        .at(span));
    }
    chars[idx] = ch;
    let new_string: String = chars.into_iter().collect();
    env.borrow_mut()
        .set(var_name, Value::String(new_string))
        .map_err(|e| e.at(span))?;
    Ok(Trampoline::Done(Value::Void))
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

/// Format a value for `display` (no quotes on strings).
fn display_value(val: &Value) -> String {
    match val {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        other => other.to_string(),
    }
}

fn apply_builtin(
    name: &str,
    args: &[Value],
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
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
                Value::List(_, _) | Value::Int(_) | Value::Bool(_) | Value::String(_)
                | Value::Char(_) | Value::Symbol(_, _) | Value::Builtin(_)
                | Value::Closure { .. } | Value::Void => Err(EvalError::TypeMismatch {
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
                Value::List(_, _) | Value::Int(_) | Value::Bool(_) | Value::String(_)
                | Value::Char(_) | Value::Symbol(_, _) | Value::Builtin(_)
                | Value::Closure { .. } | Value::Void => Err(EvalError::TypeMismatch {
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
                other => Err(EvalError::TypeMismatch {
                    expected: "list".into(),
                    got: format!("{other}"),
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
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(matches!(&args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            output.borrow_mut().push_str(&display_value(&args[0]));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            output.borrow_mut().push_str(&args[0].to_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 0, got: args.len() });
            }
            output.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" => apply_string_builtin(name, args),
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::String(s) => result.push_str(s),
                    other => return Err(EvalError::TypeMismatch {
                        expected: "string".into(),
                        got: format!("{other}"),
                    }),
                }
            }
            Ok(Value::String(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => Ok(Value::Int(s.len() as i64)),
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(),
                    got: format!("{}", args[0]),
                });
            };
            let Value::Int(start) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(),
                    got: format!("{}", args[1]),
                });
            };
            let Value::Int(end) = &args[2] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(),
                    got: format!("{}", args[2]),
                });
            };
            let start = *start as usize;
            let end = *end as usize;
            if start > end || end > s.len() {
                return Err(EvalError::TypeMismatch {
                    expected: format!("valid substring indices (0..{})", s.len()),
                    got: format!("{start}..{end}"),
                });
            }
            Ok(Value::String(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Int(n)),
                    Err(_) => Ok(Value::Bool(false)),
                },
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Int(n) => Ok(Value::String(format!("{n}"))),
                other => Err(EvalError::TypeMismatch {
                    expected: "integer".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::Symbol(s, _) => Ok(Value::String(s.clone())),
                other => Err(EvalError::TypeMismatch {
                    expected: "symbol".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => Ok(Value::Symbol(s.clone(), None)),
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.clone())),
                other => Err(EvalError::TypeMismatch {
                    expected: "string".into(),
                    got: format!("{other}"),
                }),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            }
            let Value::String(s) = &args[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "string".into(),
                    got: format!("{}", args[0]),
                });
            };
            let Value::Int(idx) = &args[1] else {
                return Err(EvalError::TypeMismatch {
                    expected: "integer".into(),
                    got: format!("{}", args[1]),
                });
            };
            let idx = *idx as usize;
            let ch = s.chars().nth(idx).ok_or_else(|| EvalError::TypeMismatch {
                expected: format!("index in range 0..{}", s.len()),
                got: format!("{idx}"),
            })?;
            Ok(Value::Char(ch))
        }
        other => Err(EvalError::UnboundVariable { name: other.into() }),
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
