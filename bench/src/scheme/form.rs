use std::rc::Rc;

use crate::scheme::cont::Frame;
use crate::scheme::env::{self, Env};
use crate::scheme::error::SchemeError;
use crate::scheme::eval::State;
use crate::scheme::value::{self, LambdaData, Value};

/// `(quote datum)`
pub fn eval_quote(items: &[Value]) -> Result<State, SchemeError> {
    if items.len() != 2 {
        return Err(bad("quote"));
    }
    Ok(State::Return(items[1].clone()))
}

/// `(if test then else?)`
pub fn eval_if(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if items.len() < 3 || items.len() > 4 {
        return Err(bad("if"));
    }
    let else_expr = if items.len() == 4 {
        items[3].clone()
    } else {
        Value::Void
    };
    cont.push(Frame::If {
        then_expr: items[2].clone(),
        else_expr,
        env: env.clone(),
    });
    Ok(State::Eval(items[1].clone(), env))
}

/// `(define name expr)` or `(define (name params...) body...)`
pub fn eval_define(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if items.len() < 3 {
        return Err(bad("define"));
    }
    match &items[1] {
        Value::Symbol(name) => {
            cont.push(Frame::Define {
                name: name.clone(),
                env: env.clone(),
            });
            Ok(State::Eval(items[2].clone(), env))
        }
        Value::Pair(_, _) => define_fn(items, env, cont),
        _ => Err(bad("define")),
    }
}

fn define_fn(
    items: &[Value],
    env: Env,
    _cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    // items[1] is (name params...) or (name params . rest)
    let Value::Pair(car, cdr) = &items[1] else {
        return Err(bad("define"));
    };
    let name = expect_symbol(car, "define")?;
    let (params, rest) = parse_formals(cdr)?;
    let body: Vec<Value> = items[2..].to_vec();
    let lam = Value::Lambda(Rc::new(LambdaData {
        params,
        rest,
        body,
        env: env.clone(),
    }));
    env::define(&env, name, lam);
    Ok(State::Return(Value::Void))
}

/// `(lambda (params...) body...)` or `(lambda args body...)`
pub fn eval_lambda(items: &[Value], env: Env) -> Result<State, SchemeError> {
    if items.len() < 3 {
        return Err(bad("lambda"));
    }
    let (params, rest) = parse_formals(&items[1])?;
    let body: Vec<Value> = items[2..].to_vec();
    Ok(State::Return(Value::Lambda(Rc::new(LambdaData {
        params,
        rest,
        body,
        env,
    }))))
}

/// `(begin expr...)`
pub fn eval_begin(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    eval_body(&items[1..], env, cont)
}

/// `(set! name expr)`
pub fn eval_set(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if items.len() != 3 {
        return Err(bad("set!"));
    }
    let Value::Symbol(name) = &items[1] else {
        return Err(bad("set!"));
    };
    cont.push(Frame::SetBang {
        name: name.clone(),
        env: env.clone(),
    });
    Ok(State::Eval(items[2].clone(), env))
}

/// `(and expr...)`
pub fn eval_and(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let args = &items[1..];
    if args.is_empty() {
        return Ok(State::Return(Value::Boolean(true)));
    }
    if args.len() == 1 {
        return Ok(State::Eval(args[0].clone(), env));
    }
    cont.push(Frame::And {
        remaining: args[1..].to_vec(),
        env: env.clone(),
    });
    Ok(State::Eval(args[0].clone(), env))
}

/// `(or expr...)`
pub fn eval_or(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let args = &items[1..];
    if args.is_empty() {
        return Ok(State::Return(Value::Boolean(false)));
    }
    if args.len() == 1 {
        return Ok(State::Eval(args[0].clone(), env));
    }
    cont.push(Frame::Or {
        remaining: args[1..].to_vec(),
        env: env.clone(),
    });
    Ok(State::Eval(args[0].clone(), env))
}

/// `(cond clause...)` — desugar to nested if.
pub fn eval_cond(
    items: &[Value],
    env: Env,
    _cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let desugared = desugar_cond(&items[1..])?;
    Ok(State::Eval(desugared, env))
}

fn desugar_cond(clauses: &[Value]) -> Result<Value, SchemeError> {
    if clauses.is_empty() {
        return Ok(Value::Void);
    }
    let clause = value::to_vec(&clauses[0])?;
    if clause.is_empty() {
        return Err(bad("cond"));
    }
    let is_else = matches!(&clause[0], Value::Symbol(s) if s == "else");
    if is_else {
        return wrap_begin(&clause[1..]);
    }
    let test = clause[0].clone();
    let consequent = wrap_begin(&clause[1..])?;
    let alternate = desugar_cond(&clauses[1..])?;
    Ok(build_if(test, consequent, alternate))
}

/// `(let bindings body...)` or named let `(let name bindings body...)`
pub fn eval_let(
    items: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if items.len() < 3 {
        return Err(bad("let"));
    }
    if let Value::Symbol(name) = &items[1] {
        return eval_named_let(name.clone(), &items[2..], env, cont);
    }
    eval_basic_let(&items[1], &items[2..], env, cont)
}

fn eval_basic_let(
    bindings_val: &Value,
    body: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    let bindings = value::to_vec(bindings_val)?;
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for b in &bindings {
        let pair = value::to_vec(b)?;
        if pair.len() != 2 {
            return Err(bad("let binding"));
        }
        let Value::Symbol(name) = &pair[0] else {
            return Err(bad("let binding"));
        };
        params.push(name.clone());
        inits.push(pair[1].clone());
    }
    let lam = Value::Lambda(Rc::new(LambdaData {
        params,
        rest: None,
        body: body.to_vec(),
        env: env.clone(),
    }));
    let mut call = vec![lam];
    call.extend(inits);
    crate::scheme::eval::start_call(call, env, cont)
}

fn eval_named_let(
    name: String,
    rest: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if rest.len() < 2 {
        return Err(bad("named let"));
    }
    let bindings = value::to_vec(&rest[0])?;
    let body: Vec<Value> = rest[1..].to_vec();
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for b in &bindings {
        let pair = value::to_vec(b)?;
        if pair.len() != 2 {
            return Err(bad("named let"));
        }
        let Value::Symbol(pname) = &pair[0] else {
            return Err(bad("named let"));
        };
        params.push(pname.clone());
        inits.push(pair[1].clone());
    }
    let new_env = env::new_env(Some(env.clone()));
    let lam = Value::Lambda(Rc::new(LambdaData {
        params,
        rest: None,
        body,
        env: new_env.clone(),
    }));
    env::define(&new_env, name, lam.clone());
    let mut call = vec![lam];
    call.extend(inits);
    crate::scheme::eval::start_call(call, env, cont)
}

/// `(define-syntax name (syntax-rules ...))`
pub fn eval_define_syntax(items: &[Value], env: Env) -> Result<State, SchemeError> {
    if items.len() != 3 {
        return Err(bad("define-syntax"));
    }
    let Value::Symbol(name) = &items[1] else {
        return Err(bad("define-syntax"));
    };
    let mac = crate::scheme::syntax::parse_syntax_rules(&items[2], env.clone())?;
    env::define(&env, name.clone(), Value::SyntaxRules(Rc::new(mac)));
    Ok(State::Return(Value::Void))
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Evaluate a body (sequence of exprs) with the last in tail position.
pub fn eval_body(
    body: &[Value],
    env: Env,
    cont: &mut Vec<Frame>,
) -> Result<State, SchemeError> {
    if body.is_empty() {
        return Ok(State::Return(Value::Void));
    }
    if body.len() == 1 {
        return Ok(State::Eval(body[0].clone(), env));
    }
    cont.push(Frame::Seq {
        remaining: body[1..].to_vec(),
        env: env.clone(),
    });
    Ok(State::Eval(body[0].clone(), env))
}

fn parse_formals(formals: &Value) -> Result<(Vec<String>, Option<String>), SchemeError> {
    match formals {
        Value::Symbol(name) => Ok((vec![], Some(name.clone()))),
        Value::Nil => Ok((vec![], None)),
        Value::Pair(_, _) => parse_param_list(formals),
        _ => Err(bad("lambda formals")),
    }
}

fn parse_param_list(val: &Value) -> Result<(Vec<String>, Option<String>), SchemeError> {
    let mut params = Vec::new();
    let mut cur = val.clone();
    loop {
        match cur {
            Value::Nil => return Ok((params, None)),
            Value::Symbol(rest) => return Ok((params, Some(rest))),
            Value::Pair(car, cdr) => {
                params.push(expect_symbol(&car, "parameter")?);
                cur = (*cdr).clone();
            }
            _ => return Err(bad("parameter list")),
        }
    }
}

fn build_if(test: Value, then_br: Value, else_br: Value) -> Value {
    value::from_vec(vec![
        Value::Symbol("if".into()),
        test,
        then_br,
        else_br,
    ])
}

fn wrap_begin(exprs: &[Value]) -> Result<Value, SchemeError> {
    if exprs.len() == 1 {
        return Ok(exprs[0].clone());
    }
    let mut v = vec![Value::Symbol("begin".into())];
    v.extend_from_slice(exprs);
    Ok(value::from_vec(v))
}

fn expect_symbol(val: &Value, context: &str) -> Result<String, SchemeError> {
    match val {
        Value::Symbol(s) => Ok(s.clone()),
        _ => Err(bad(context)),
    }
}

fn bad(form: &str) -> SchemeError {
    SchemeError::BadSyntax { form: form.into() }
}
