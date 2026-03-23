use std::cell::RefCell;
use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::error::{ErrorKind, EvalError, Span};
use crate::scheme::parser::{Expr, ExprKind, gcd};
use crate::scheme::macros;
use crate::scheme::value::{CapturedCont, StringMutability, Value};

// ── numeric tower ───────────────────────────────────────────

/// Internal numeric representation for arithmetic operations.
#[derive(Debug, Clone, Copy)]
enum NumVal {
    Int(i64),
    Rat(i64, i64),   // always simplified, den > 0
    Flt(f64),
}

impl NumVal {
    fn from_value(val: &Value) -> Result<Self, EvalError> {
        match val {
            Value::Integer(n) => Ok(NumVal::Int(*n)),
            Value::Rational(n, d) => Ok(NumVal::Rat(*n, *d)),
            Value::Float(f) => Ok(NumVal::Flt(*f)),
            other => Err(ErrorKind::TypeMismatch {
                expected: "number".into(),
                got: other.to_display_string(),
            }.into()),
        }
    }

    fn to_value(self) -> Value {
        match self {
            NumVal::Int(n) => Value::Integer(n),
            NumVal::Rat(n, d) => Value::Rational(n, d),
            NumVal::Flt(f) => Value::Float(f),
        }
    }

    fn to_f64(self) -> f64 {
        match self {
            NumVal::Int(n) => n as f64,
            NumVal::Rat(n, d) => n as f64 / d as f64,
            NumVal::Flt(f) => f,
        }
    }

}

/// Simplify a rational, returning Int if denominator is 1.
fn make_rat(num: i64, den: i64) -> NumVal {
    if den == 0 {
        // should not happen in normal use; caller checks
        return NumVal::Int(0);
    }
    let g = gcd(num.unsigned_abs(), den.unsigned_abs()) as i64;
    let (mut n, mut d) = (num / g, den / g);
    if d < 0 {
        n = -n;
        d = -d;
    }
    if d == 1 {
        NumVal::Int(n)
    } else {
        NumVal::Rat(n, d)
    }
}

fn num_add(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x + y),
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => make_rat(an * bd + bn * ad, ad * bd),
        (NumVal::Int(x), NumVal::Rat(n, d)) | (NumVal::Rat(n, d), NumVal::Int(x)) => {
            make_rat(x * d + n, d)
        }
        (NumVal::Flt(x), NumVal::Flt(y)) => NumVal::Flt(x + y),
        (NumVal::Flt(f), other) | (other, NumVal::Flt(f)) => NumVal::Flt(f + other.to_f64()),
    }
}

fn num_sub(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x - y),
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => make_rat(an * bd - bn * ad, ad * bd),
        (NumVal::Int(x), NumVal::Rat(n, d)) => make_rat(x * d - n, d),
        (NumVal::Rat(n, d), NumVal::Int(x)) => make_rat(n - x * d, d),
        (NumVal::Flt(x), NumVal::Flt(y)) => NumVal::Flt(x - y),
        (NumVal::Flt(f), other) => NumVal::Flt(f - other.to_f64()),
        (other, NumVal::Flt(f)) => NumVal::Flt(other.to_f64() - f),
    }
}

fn num_mul(a: NumVal, b: NumVal) -> NumVal {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => NumVal::Int(x * y),
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => make_rat(an * bn, ad * bd),
        (NumVal::Int(x), NumVal::Rat(n, d)) | (NumVal::Rat(n, d), NumVal::Int(x)) => {
            make_rat(x * n, d)
        }
        (NumVal::Flt(x), NumVal::Flt(y)) => NumVal::Flt(x * y),
        (NumVal::Flt(f), other) | (other, NumVal::Flt(f)) => NumVal::Flt(f * other.to_f64()),
    }
}

fn num_div(a: NumVal, b: NumVal) -> Result<NumVal, EvalError> {
    match (a, b) {
        (NumVal::Int(x), NumVal::Int(y)) => {
            if y == 0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(make_rat(x, y))
        }
        (NumVal::Rat(an, ad), NumVal::Rat(bn, bd)) => {
            if bn == 0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(make_rat(an * bd, ad * bn))
        }
        (NumVal::Int(x), NumVal::Rat(n, d)) => {
            if n == 0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(make_rat(x * d, n))
        }
        (NumVal::Rat(n, d), NumVal::Int(y)) => {
            if y == 0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(make_rat(n, d * y))
        }
        (NumVal::Flt(x), NumVal::Flt(y)) => {
            if y == 0.0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(NumVal::Flt(x / y))
        }
        (NumVal::Flt(f), other) => {
            let y = other.to_f64();
            if y == 0.0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(NumVal::Flt(f / y))
        }
        (other, NumVal::Flt(f)) => {
            if f == 0.0 { return Err(ErrorKind::DivisionByZero.into()); }
            Ok(NumVal::Flt(other.to_f64() / f))
        }
    }
}

fn num_neg(a: NumVal) -> NumVal {
    match a {
        NumVal::Int(n) => NumVal::Int(-n),
        NumVal::Rat(n, d) => NumVal::Rat(-n, d),
        NumVal::Flt(f) => NumVal::Flt(-f),
    }
}

/// Convert a float to an exact rational using simple fraction detection.
fn float_to_exact(f: f64) -> NumVal {
    if f == f.floor() {
        return NumVal::Int(f as i64);
    }
    // Use a denominator limit approach to find the best rational approximation
    let sign = if f < 0.0 { -1i64 } else { 1 };
    let f_abs = f.abs();
    let mut best_num = 0i64;
    let mut best_den = 1i64;
    let mut best_err = f64::MAX;
    for d in 1..=10000i64 {
        let n = (f_abs * d as f64).round() as i64;
        let err = (f_abs - n as f64 / d as f64).abs();
        if err < best_err {
            best_err = err;
            best_num = n;
            best_den = d;
            if err < f64::EPSILON {
                break;
            }
        }
    }
    make_rat(sign * best_num, best_den)
}

// ── continuation frames ──────────────────────────────────────

/// An entry on the dynamic-wind stack, tracking active in/out thunks.
#[derive(Clone)]
struct WindEntry {
    id: u64,
    in_thunk: Value,
    out_thunk: Value,
}

/// An operation to perform during continuation wind transfer.
#[derive(Clone)]
enum WindOp {
    /// Pop wind stack, call out-thunk
    CallOut(Value),
    /// Call in-thunk, then push entry onto wind stack
    CallInAndPush(WindEntry),
}

/// An exception handler entry.
#[derive(Clone)]
enum HandlerEntry {
    /// Installed by `with-exception-handler`.
    WithExceptionHandler { handler: Value },
    /// Installed by `guard`.
    Guard {
        var: String,
        clauses: Vec<Expr>,
        env: Rc<Env>,
        guard_k: Vec<Frame>,
        guard_wind: Vec<WindEntry>,
        guard_handlers: Vec<HandlerEntry>,
    },
}

/// A frame on the explicit continuation stack (CEK machine).
#[derive(Clone)]
enum Frame {
    CallFunc { arg_exprs: Vec<Expr>, env: Rc<Env>, span: Span },
    CallArg { func: Value, done: Vec<Value>, rest: Vec<Expr>, env: Rc<Env>, span: Span },
    Seq { rest: Vec<Expr>, env: Rc<Env> },
    Def { name: String, env: Rc<Env> },
    SetVar { name: String, env: Rc<Env> },
    IfTest { then_br: Expr, else_br: Option<Expr>, env: Rc<Env> },
    AndRest { rest: Vec<Expr>, env: Rc<Env> },
    OrRest { rest: Vec<Expr>, env: Rc<Env> },
    LetInit {
        name: String,
        done_n: Vec<String>,
        done_v: Vec<Value>,
        rest: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    NamedLetInit {
        loop_name: String,
        name: String,
        done_n: Vec<String>,
        done_v: Vec<Value>,
        rest: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    LetStarInit {
        name: String,
        rest: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    CondTest { clause_body: Vec<Expr>, rest_clauses: Vec<Expr>, env: Rc<Env> },
    CaseKey { clauses: Vec<Expr>, env: Rc<Env> },
    DoInit {
        var_names: Vec<String>,
        done_vals: Vec<Value>,
        rest_inits: Vec<Expr>,
        step_exprs: Vec<Option<Expr>>,
        test: Expr,
        result_exprs: Vec<Expr>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    DoTest {
        vars: Vec<String>,
        step_exprs: Vec<Option<Expr>>,
        test: Expr,
        result_exprs: Vec<Expr>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
    DoStep {
        vars: Vec<String>,
        step_exprs: Vec<Option<Expr>>,
        test: Expr,
        result_exprs: Vec<Expr>,
        body: Vec<Expr>,
        done_vals: Vec<Value>,
        rest_indices: Vec<usize>,
        env: Rc<Env>,
    },
    MapStep {
        func: Value,
        lists: Vec<Vec<Value>>,
        index: usize,
        results: Vec<Value>,
        env: Rc<Env>,
        span: Span,
    },
    ForEachStep {
        func: Value,
        lists: Vec<Vec<Value>>,
        index: usize,
        env: Rc<Env>,
        span: Span,
    },
    /// After in-thunk returns: push wind entry, call body-thunk.
    DynamicWindAfterIn {
        body_thunk: Value,
        out_thunk: Value,
        wind_entry: WindEntry,
        env: Rc<Env>,
        span: Span,
    },
    /// After body-thunk returns: pop wind entry, call out-thunk, save body value.
    DynamicWindAfterBody {
        out_thunk: Value,
        env: Rc<Env>,
        span: Span,
    },
    /// After out-thunk returns: return saved body value.
    DynamicWindAfterOut {
        body_val: Value,
    },
    /// Process a sequence of wind operations during continuation transfer.
    WindTransfer {
        ops: Vec<WindOp>,
        final_val: Value,
        env: Rc<Env>,
        span: Span,
    },
    /// Pop exception handler when guard/with-exception-handler body returns normally.
    PopHandler,
    /// After guard wind transfer completes, test guard clauses.
    GuardClauseTest {
        var: String,
        raised_val: Value,
        clauses: Vec<Expr>,
        env: Rc<Env>,
    },
    /// After evaluating a guard clause test, dispatch on result.
    GuardClauseDispatch {
        clause_body: Vec<Expr>,
        rest_clauses: Vec<Expr>,
        raised_val: Value,
        env: Rc<Env>,
    },
    /// After producer thunk returns in call-with-values, call consumer with results.
    CallWithValuesConsumer {
        consumer: Value,
        env: Rc<Env>,
        span: Span,
    },
    /// After evaluating define-syntax body (non-syntax-rules), store as SyntaxTransformer.
    DefSyntax { name: String, env: Rc<Env> },
    /// After syntax transformer returns, evaluate the resulting syntax object.
    ExpandSyntax { call_env: Rc<Env> },
    /// After evaluating syntax-case scrutinee, match patterns and eval body.
    SyntaxCaseMatch {
        literals: Vec<String>,
        clauses: Vec<Expr>,
        env: Rc<Env>,
    },
    /// After evaluating with-syntax binding expr, bind and continue.
    WithSyntaxBind {
        pattern: Expr,
        rest_bindings: Vec<(Expr, Expr)>,
        body: Vec<Expr>,
        env: Rc<Env>,
    },
}

/// CEK machine state.
enum State {
    Eval(Expr, Rc<Env>),
    Apply(Value, Vec<Value>, Rc<Env>, Span),
    Ret(Value),
}

// ── public entry points ──────────────────────────────────────

/// Evaluate a sequence of top-level expressions, returning the last result.
pub fn eval_top_level(exprs: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let mut k: Vec<Frame> = Vec::new();
    let mut wind: Vec<WindEntry> = Vec::new();
    let mut wind_id: u64 = 0;
    let mut handlers: Vec<HandlerEntry> = Vec::new();
    let mut state = begin_seq(exprs, env, &mut k);
    run_loop(&mut state, &mut k, &mut wind, &mut wind_id, &mut handlers)
}

// ── main loop ────────────────────────────────────────────────

fn run_loop(
    state: &mut State,
    k: &mut Vec<Frame>,
    wind: &mut Vec<WindEntry>,
    wind_id: &mut u64,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<Value, EvalError> {
    loop {
        *state = match std::mem::replace(state, State::Ret(Value::Void)) {
            State::Eval(expr, env) => step_eval(expr, &env, k, wind, handlers)?,
            State::Apply(func, args, env, span) => {
                step_apply(func, args, &env, k, (wind, wind_id), span, handlers)
                    .map_err(|e| e.with_span(span))?
            }
            State::Ret(val) => match k.pop() {
                Some(frame) => step_ret(val, frame, k, wind, handlers)?,
                None => return Ok(val),
            },
        };
    }
}

/// Start evaluating a non-empty sequence of expressions.
fn begin_seq(exprs: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> State {
    debug_assert!(!exprs.is_empty());
    if exprs.len() > 1 {
        k.push(Frame::Seq { rest: exprs[1..].to_vec(), env: Rc::clone(env) });
    }
    State::Eval(exprs[0].clone(), Rc::clone(env))
}

// ── step_eval ────────────────────────────────────────────────

fn step_eval(expr: Expr, env: &Rc<Env>, k: &mut Vec<Frame>, wind: &[WindEntry], handlers: &mut Vec<HandlerEntry>) -> Result<State, EvalError> {
    let span = expr.span;
    let result = match expr.kind {
        ExprKind::Integer(n) => Ok(State::Ret(Value::Integer(n))),
        ExprKind::Float(f) => Ok(State::Ret(Value::Float(f))),
        ExprKind::Rational(n, d) => Ok(State::Ret(Value::Rational(n, d))),
        ExprKind::Boolean(b) => Ok(State::Ret(Value::Boolean(b))),
        ExprKind::Str(s) => Ok(State::Ret(Value::new_str(s))),
        ExprKind::Char(c) => Ok(State::Ret(Value::Char(c))),
        ExprKind::Symbol(name) => env
            .get(&name)
            .map(State::Ret)
            .ok_or_else(|| EvalError::from(ErrorKind::UnboundVariable { name })),
        ExprKind::List(elems) => step_eval_list(elems, env, k, span, wind, handlers),
    };
    result.map_err(|e| e.with_span(span))
}

fn step_eval_list(
    mut elems: Vec<Expr>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    span: Span,
    wind: &[WindEntry],
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    if elems.is_empty() {
        return Ok(State::Ret(Value::List(Vec::new())));
    }
    if let ExprKind::Symbol(ref name) = elems[0].kind {
        match name.as_str() {
            "if" => return step_if(&elems[1..], env, k),
            "define" => return step_define(&elems[1..], env, k),
            "set!" => return step_set_bang(&elems[1..], env, k),
            "quote" => return eval_quote(&elems[1..]).map(State::Ret),
            "lambda" => return eval_lambda(&elems[1..], env).map(State::Ret),
            "begin" => return step_begin(&elems[1..], env, k),
            "and" => return step_and(&elems[1..], env, k),
            "or" => return step_or(&elems[1..], env, k),
            "let" => return step_let(&elems[1..], env, k),
            "let*" => return step_let_star(&elems[1..], env, k),
            "cond" => return step_cond(&elems[1..], env, k),
            "case" => return step_case(&elems[1..], env, k),
            "letrec" => return step_letrec(&elems[1..], env, k),
            "letrec*" => return step_letrec_star(&elems[1..], env, k),
            "do" => return step_do(&elems[1..], env, k),
            "define-syntax" => return step_define_syntax(&elems[1..], env, k),
            "define-record-type" => return step_define_record_type(&elems[1..], env),
            "guard" => return step_guard(&elems[1..], env, k, wind, handlers),
            "syntax-case" => return step_syntax_case(&elems[1..], env, k),
            "syntax-template" => return step_syntax_template(&elems[1..], env),
            "with-syntax" => return step_with_syntax(&elems[1..], env, k),
            _ => {}
        }
        // Check for macro invocation
        if let Some(Value::SyntaxRules {
            ref literals,
            ref rules,
            ref def_env,
        }) = env.get(name)
        {
            let (expanded, new_env) =
                macros::expand_macro(literals, rules, def_env, &elems[1..], env, span)?;
            return Ok(State::Eval(expanded, new_env));
        }
        // Check for syntax-case macro transformer
        if let Some(Value::SyntaxTransformer(ref transformer)) = env.get(name) {
            let transformer = *transformer.clone();
            let call_expr = Expr { kind: ExprKind::List(elems), span };
            let stx = Value::SyntaxObject(call_expr);
            k.push(Frame::ExpandSyntax { call_env: Rc::clone(env) });
            return Ok(State::Apply(transformer, vec![stx], Rc::clone(env), span));
        }
    }
    // Function call: evaluate function position first, then args.
    let func_expr = elems.remove(0);
    k.push(Frame::CallFunc { arg_exprs: elems, env: Rc::clone(env), span });
    Ok(State::Eval(func_expr, Rc::clone(env)))
}

// ── special forms ────────────────────────────────────────────

fn step_if(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(ErrorKind::BadSyntax {
            form: "if".into(),
            message: "expected 2 or 3 arguments".into(),
        }
        .into());
    }
    k.push(Frame::IfTest {
        then_br: args[1].clone(),
        else_br: args.get(2).cloned(),
        env: Rc::clone(env),
    });
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_define(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "define".into(),
            message: "missing name".into(),
        }
        .into());
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "expected (define name expr)".into(),
                }
                .into());
            }
            k.push(Frame::Def {
                name: name.clone(),
                env: Rc::clone(env),
            });
            Ok(State::Eval(args[1].clone(), Rc::clone(env)))
        }
        ExprKind::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "missing function name".into(),
                }
                .into());
            }
            let name = match &name_and_params[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(ErrorKind::BadSyntax {
                        form: "define".into(),
                        message: "function name must be a symbol".into(),
                    }
                    .into())
                }
            };
            let (params, rest_param) = parse_params(&name_and_params[1..], "define")?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                closure_env: Rc::clone(env),
            };
            env.define(name, lambda);
            Ok(State::Ret(Value::Void))
        }
        _ => Err(ErrorKind::BadSyntax {
            form: "define".into(),
            message: "invalid define syntax".into(),
        }
        .into()),
    }
}

fn step_set_bang(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() != 2 {
        return Err(ErrorKind::BadSyntax {
            form: "set!".into(),
            message: "expected (set! name expr)".into(),
        }
        .into());
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "set!".into(),
                message: "first argument must be a symbol".into(),
            }
            .into())
        }
    };
    k.push(Frame::SetVar {
        name,
        env: Rc::clone(env),
    });
    Ok(State::Eval(args[1].clone(), Rc::clone(env)))
}

fn step_begin(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Ok(State::Ret(Value::Void));
    }
    Ok(begin_seq(args, env, k))
}

fn step_and(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Ok(State::Ret(Value::Boolean(true)));
    }
    if args.len() > 1 {
        k.push(Frame::AndRest {
            rest: args[1..].to_vec(),
            env: Rc::clone(env),
        });
    }
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_or(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Ok(State::Ret(Value::Boolean(false)));
    }
    if args.len() > 1 {
        k.push(Frame::OrRest {
            rest: args[1..].to_vec(),
            env: Rc::clone(env),
        });
    }
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_let(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "missing bindings".into(),
        }
        .into());
    }
    // Named let: (let name ((var init) ...) body...)
    if let ExprKind::Symbol(loop_name) = &args[0].kind {
        if args.len() < 3 {
            return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "expected (let name ((var init) ...) body...)".into(),
            }
            .into());
        }
        let mut bindings = parse_let_bindings(&args[1])?;
        let body = args[2..].to_vec();
        if bindings.is_empty() {
            let func_env = Env::extend(env, vec![loop_name.clone()], vec![Value::Void]);
            let lambda = Value::Lambda {
                params: Vec::new(),
                rest_param: None,
                body: body.clone(),
                closure_env: Rc::clone(&func_env),
            };
            func_env.define(loop_name.clone(), lambda);
            return Ok(begin_seq(&body, &func_env, k));
        }
        let (first_name, first_expr) = bindings.remove(0);
        k.push(Frame::NamedLetInit {
            loop_name: loop_name.clone(),
            name: first_name,
            done_n: Vec::new(),
            done_v: Vec::new(),
            rest: bindings,
            body,
            env: Rc::clone(env),
        });
        return Ok(State::Eval(first_expr, Rc::clone(env)));
    }
    // Regular let
    let mut bindings = parse_let_bindings(&args[0])?;
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "missing body".into(),
        }
        .into());
    }
    let body = args[1..].to_vec();
    if bindings.is_empty() {
        let local_env = Env::extend(env, Vec::new(), Vec::new());
        return Ok(begin_seq(&body, &local_env, k));
    }
    let (first_name, first_expr) = bindings.remove(0);
    k.push(Frame::LetInit {
        name: first_name,
        done_n: Vec::new(),
        done_v: Vec::new(),
        rest: bindings,
        body,
        env: Rc::clone(env),
    });
    Ok(State::Eval(first_expr, Rc::clone(env)))
}

fn step_let_star(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "let*".into(),
            message: "expected (let* ((var init) ...) body...)".into(),
        }
        .into());
    }
    let bindings = parse_let_bindings(&args[0])?;
    let body = args[1..].to_vec();
    if bindings.is_empty() {
        let local_env = Env::extend(env, Vec::new(), Vec::new());
        return Ok(begin_seq(&body, &local_env, k));
    }
    // Evaluate bindings sequentially, each in the environment of the previous
    let (first_name, first_expr) = bindings[0].clone();
    let rest = bindings[1..].to_vec();
    k.push(Frame::LetStarInit {
        name: first_name,
        rest,
        body,
        env: Rc::clone(env),
    });
    Ok(State::Eval(first_expr, Rc::clone(env)))
}

fn step_cond(clauses: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if let Some((i, clause)) = clauses.iter().enumerate().next() {
        match &clause.kind {
            ExprKind::List(elems) if !elems.is_empty() => {
                if let ExprKind::Symbol(s) = &elems[0].kind {
                    if s == "else" {
                        return Ok(begin_seq(&elems[1..], env, k));
                    }
                }
                k.push(Frame::CondTest {
                    clause_body: elems[1..].to_vec(),
                    rest_clauses: clauses[i + 1..].to_vec(),
                    env: Rc::clone(env),
                });
                return Ok(State::Eval(elems[0].clone(), Rc::clone(env)));
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "cond".into(),
                    message: "each clause must be a list".into(),
                }
                .into())
            }
        }
    }
    Ok(State::Ret(Value::Void))
}

fn step_case(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "case".into(),
            message: "missing key expression".into(),
        }
        .into());
    }
    let clauses = args[1..].to_vec();
    k.push(Frame::CaseKey {
        clauses,
        env: Rc::clone(env),
    });
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

fn step_letrec(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "letrec".into(),
            message: "expected (letrec ((var init) ...) body...)".into(),
        }
        .into());
    }
    let bindings = parse_let_bindings(&args[0])?;
    let body = args[1..].to_vec();
    let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let voids: Vec<Value> = vec![Value::Void; names.len()];
    let letrec_env = Env::extend(env, names, voids);
    if bindings.is_empty() {
        return Ok(begin_seq(&body, &letrec_env, k));
    }
    // Build a sequence: (set! name1 init1) (set! name2 init2) ... body...
    let mut seq_exprs: Vec<Expr> = Vec::new();
    for (name, init) in &bindings {
        let span = init.span;
        seq_exprs.push(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("set!".into()), span },
                Expr { kind: ExprKind::Symbol(name.clone()), span },
                init.clone(),
            ]),
            span,
        });
    }
    seq_exprs.extend(body);
    Ok(begin_seq(&seq_exprs, &letrec_env, k))
}

fn step_letrec_star(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "letrec*".into(),
            message: "expected (letrec* ((var init) ...) body...)".into(),
        }
        .into());
    }
    let bindings = parse_let_bindings(&args[0])?;
    let body = args[1..].to_vec();
    // Create env with all names bound to Void, then sequentially evaluate and define
    let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let voids: Vec<Value> = vec![Value::Void; names.len()];
    let letrec_env = Env::extend(env, names, voids);
    if bindings.is_empty() {
        return Ok(begin_seq(&body, &letrec_env, k));
    }
    // Use Seq + Def frames to evaluate each init and define it sequentially
    // Build a sequence of define expressions in the letrec_env
    let mut seq_exprs: Vec<Expr> = Vec::new();
    for (name, init) in &bindings {
        // (set! name init)
        let span = init.span;
        seq_exprs.push(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("set!".into()), span },
                Expr { kind: ExprKind::Symbol(name.clone()), span },
                init.clone(),
            ]),
            span,
        });
    }
    // Add body expressions
    seq_exprs.extend(body);
    Ok(begin_seq(&seq_exprs, &letrec_env, k))
}

fn step_do(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "do".into(),
            message: "expected (do ((var init step) ...) (test expr ...) body...)".into(),
        }
        .into());
    }
    let var_clauses = match &args[0].kind {
        ExprKind::List(clauses) => clauses,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "do".into(),
                message: "variable clauses must be a list".into(),
            }
            .into())
        }
    };
    let mut var_names = Vec::new();
    let mut init_exprs = Vec::new();
    let mut step_exprs: Vec<Option<Expr>> = Vec::new();
    for clause in var_clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 && parts.len() <= 3 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: "do".into(),
                            message: "variable name must be a symbol".into(),
                        }
                        .into())
                    }
                };
                var_names.push(name);
                init_exprs.push(parts[1].clone());
                step_exprs.push(parts.get(2).cloned());
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "do".into(),
                    message: "each variable clause must be (var init) or (var init step)"
                        .into(),
                }
                .into())
            }
        }
    }
    let test_clause = match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => parts,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "do".into(),
                message: "test clause must be (test expr ...)".into(),
            }
            .into())
        }
    };
    let test = test_clause[0].clone();
    let result_exprs = test_clause[1..].to_vec();
    let body = args[2..].to_vec();

    if init_exprs.is_empty() {
        let do_env = Env::extend(env, Vec::new(), Vec::new());
        k.push(Frame::DoTest {
            vars: var_names,
            step_exprs,
            test: test.clone(),
            result_exprs,
            body,
            env: Rc::clone(&do_env),
        });
        return Ok(State::Eval(test, Rc::clone(&do_env)));
    }
    let first = init_exprs[0].clone();
    let rest_inits = init_exprs[1..].to_vec();
    k.push(Frame::DoInit {
        var_names,
        done_vals: Vec::new(),
        rest_inits,
        step_exprs,
        test,
        result_exprs,
        body,
        env: Rc::clone(env),
    });
    Ok(State::Eval(first, Rc::clone(env)))
}

fn dispatch_case(
    key: Value,
    clauses: &[Expr],
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
) -> Result<State, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(elems) if elems.len() >= 2 => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &elems[0].kind {
                    if s == "else" {
                        return Ok(begin_seq(&elems[1..], env, k));
                    }
                }
                // Check datum list
                let datums = match &elems[0].kind {
                    ExprKind::List(d) => d,
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: "case".into(),
                            message: "each clause datum must be a list".into(),
                        }
                        .into())
                    }
                };
                for datum in datums {
                    let datum_val = expr_to_value(datum)?;
                    if scheme_eqv(&key, &datum_val) {
                        return Ok(begin_seq(&elems[1..], env, k));
                    }
                }
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "case".into(),
                    message: "invalid case clause".into(),
                }
                .into())
            }
        }
    }
    // No match, no else
    Ok(State::Ret(Value::Void))
}

fn step_define_syntax(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() != 2 {
        return Err(ErrorKind::BadSyntax {
            form: "define-syntax".into(),
            message: "expected (define-syntax name transformer)".into(),
        }
        .into());
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "define-syntax".into(),
                message: "name must be a symbol".into(),
            }
            .into())
        }
    };
    // Try parsing as syntax-rules first
    if let ExprKind::List(ref elems) = args[1].kind {
        if !elems.is_empty() {
            if let ExprKind::Symbol(ref s) = elems[0].kind {
                if s == "syntax-rules" {
                    let val = parse_syntax_rules(&args[1], env)?;
                    env.define(name, val);
                    return Ok(State::Ret(Value::Void));
                }
            }
        }
    }
    // Not syntax-rules — evaluate the body expression (e.g., a lambda)
    k.push(Frame::DefSyntax { name, env: Rc::clone(env) });
    Ok(State::Eval(args[1].clone(), Rc::clone(env)))
}

fn parse_syntax_rules(expr: &Expr, env: &Rc<Env>) -> Result<Value, EvalError> {
    let elems = match &expr.kind {
        ExprKind::List(e) => e,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "syntax-rules".into(),
                message: "expected (syntax-rules ...)".into(),
            }
            .into())
        }
    };
    if elems.is_empty() || !matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
        return Err(ErrorKind::BadSyntax {
            form: "define-syntax".into(),
            message: "expected syntax-rules".into(),
        }
        .into());
    }
    if elems.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "syntax-rules".into(),
            message: "missing literals list".into(),
        }
        .into());
    }
    let literals = match &elems[1].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::from(ErrorKind::BadSyntax {
                    form: "syntax-rules".into(),
                    message: "literal must be a symbol".into(),
                })),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "syntax-rules".into(),
                message: "expected literals list".into(),
            }
            .into())
        }
    };
    let mut rules = Vec::new();
    for clause in &elems[2..] {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                let pattern = match &parts[0].kind {
                    ExprKind::List(pat) if !pat.is_empty() => pat[1..].to_vec(),
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: "syntax-rules".into(),
                            message: "pattern must be (name ...)".into(),
                        }
                        .into())
                    }
                };
                rules.push((pattern, parts[1].clone()));
            }
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "syntax-rules".into(),
                    message: "each rule must be (pattern template)".into(),
                }
                .into())
            }
        }
    }
    Ok(Value::SyntaxRules {
        literals,
        rules,
        def_env: Rc::clone(env),
    })
}

// ── define-record-type ───────────────────────────────────────

/// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
fn step_define_record_type(args: &[Expr], env: &Rc<Env>) -> Result<State, EvalError> {
    // Minimum: type-name, constructor, predicate, at least one field spec
    if args.len() < 3 {
        return Err(ErrorKind::BadSyntax {
            form: "define-record-type".into(),
            message: "expected (define-record-type <name> (constructor field ...) pred (field accessor) ...)".into(),
        }.into());
    }
    // Type name
    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(ErrorKind::BadSyntax {
            form: "define-record-type".into(),
            message: "type name must be a symbol".into(),
        }.into()),
    };
    // Constructor: (constructor-name field ...)
    let (ctor_name, ctor_fields) = match &args[1].kind {
        ExprKind::List(elems) if !elems.is_empty() => {
            let name = match &elems[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(ErrorKind::BadSyntax {
                    form: "define-record-type".into(),
                    message: "constructor name must be a symbol".into(),
                }.into()),
            };
            let fields: Vec<String> = elems[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::from(ErrorKind::BadSyntax {
                    form: "define-record-type".into(),
                    message: "constructor field must be a symbol".into(),
                })),
            }).collect::<Result<Vec<_>, _>>()?;
            (name, fields)
        }
        _ => return Err(ErrorKind::BadSyntax {
            form: "define-record-type".into(),
            message: "expected (constructor-name field ...)".into(),
        }.into()),
    };
    // Predicate name
    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(ErrorKind::BadSyntax {
            form: "define-record-type".into(),
            message: "predicate must be a symbol".into(),
        }.into()),
    };
    // Field specs: (field-name accessor-name)
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                let field = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(ErrorKind::BadSyntax {
                        form: "define-record-type".into(),
                        message: "field name must be a symbol".into(),
                    }.into()),
                };
                let accessor = match &parts[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(ErrorKind::BadSyntax {
                        form: "define-record-type".into(),
                        message: "accessor name must be a symbol".into(),
                    }.into()),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(ErrorKind::BadSyntax {
                form: "define-record-type".into(),
                message: "field spec must be (field-name accessor-name)".into(),
            }.into()),
        }
    }

    // Create a unique type tag
    let type_tag = Rc::new(());

    // Define constructor
    env.define(ctor_name, Value::RecordConstructor {
        type_tag: Rc::clone(&type_tag),
        type_name: type_name.clone(),
        field_names: ctor_fields.clone(),
    });

    // Define predicate
    env.define(pred_name, Value::RecordPredicate {
        type_tag: Rc::clone(&type_tag),
    });

    // Define accessors - map field name to index in constructor field order
    for (field, accessor) in &field_accessors {
        let idx = ctor_fields.iter().position(|f| f == field).ok_or_else(|| {
            EvalError::from(ErrorKind::BadSyntax {
                form: "define-record-type".into(),
                message: format!("field '{}' not in constructor", field),
            })
        })?;
        env.define(accessor.clone(), Value::RecordAccessor {
            type_tag: Rc::clone(&type_tag),
            type_name: type_name.clone(),
            field_name: field.clone(),
            field_index: idx,
        });
    }

    Ok(State::Ret(Value::Void))
}

// ── guard special form ───────────────────────────────────────

fn step_guard(
    args: &[Expr],
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    wind: &[WindEntry],
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "guard".into(),
            message: "expected (guard (var clause ...) body ...)".into(),
        }
        .into());
    }
    let header = match &args[0].kind {
        ExprKind::List(elems) if elems.len() >= 2 => elems,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "guard".into(),
                message: "expected (guard (var clause ...) body ...)".into(),
            }
            .into())
        }
    };
    let var = match &header[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "guard".into(),
                message: "guard variable must be a symbol".into(),
            }
            .into())
        }
    };
    let clauses = header[1..].to_vec();
    let body = args[1..].to_vec();

    // Save guard continuation state (before pushing PopHandler)
    let guard_k = k.clone();
    let guard_wind = wind.to_vec();
    let guard_handlers = handlers.clone();

    // Push PopHandler so normal body return pops the handler
    k.push(Frame::PopHandler);

    // Install guard handler
    handlers.push(HandlerEntry::Guard {
        var,
        clauses,
        env: Rc::clone(env),
        guard_k,
        guard_wind,
        guard_handlers,
    });

    // Evaluate body
    if body.is_empty() {
        Ok(State::Ret(Value::Void))
    } else {
        Ok(begin_seq(&body, env, k))
    }
}

fn step_guard_clauses(
    raised_val: &Value,
    clauses: &[Expr],
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
) -> Result<State, EvalError> {
    if clauses.is_empty() {
        return Err(ErrorKind::UserRaise {
            value: raised_val.to_display_string(),
        }
        .into());
    }
    match &clauses[0].kind {
        ExprKind::List(elems) if !elems.is_empty() => {
            if let ExprKind::Symbol(s) = &elems[0].kind {
                if s == "else" {
                    return Ok(begin_seq(&elems[1..], env, k));
                }
            }
            k.push(Frame::GuardClauseDispatch {
                clause_body: elems[1..].to_vec(),
                rest_clauses: clauses[1..].to_vec(),
                raised_val: raised_val.clone(),
                env: Rc::clone(env),
            });
            Ok(State::Eval(elems[0].clone(), Rc::clone(env)))
        }
        _ => Err(ErrorKind::BadSyntax {
            form: "guard".into(),
            message: "invalid guard clause".into(),
        }
        .into()),
    }
}

// ── step_ret ─────────────────────────────────────────────────

fn step_ret(val: Value, frame: Frame, k: &mut Vec<Frame>, wind: &mut Vec<WindEntry>, handlers: &mut Vec<HandlerEntry>) -> Result<State, EvalError> {
    match frame {
        Frame::CallFunc { arg_exprs, env, span } => {
            if arg_exprs.is_empty() {
                Ok(State::Apply(val, Vec::new(), env, span))
            } else {
                // Evaluate arguments right-to-left (R7RS allows any order).
                // This ensures continuations captured inside args see
                // unevaluated siblings, enabling reentrant call/cc patterns.
                let mut args_rev = arg_exprs;
                args_rev.reverse();
                let next = args_rev.remove(0);
                k.push(Frame::CallArg {
                    func: val,
                    done: Vec::new(),
                    rest: args_rev,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::CallArg {
            func,
            mut done,
            rest,
            env,
            span,
        } => {
            done.push(val);
            if rest.is_empty() {
                // Arguments were evaluated right-to-left; restore original order.
                done.reverse();
                Ok(State::Apply(func, done, env, span))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::CallArg {
                    func,
                    done,
                    rest,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::Seq { rest, env } => {
            if rest.len() == 1 {
                Ok(State::Eval(
                    rest.into_iter().next().expect("checked len"),
                    env,
                ))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::Seq {
                    rest,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::Def { name, env } => {
            env.define(name, val);
            Ok(State::Ret(Value::Void))
        }
        Frame::SetVar { name, env } => {
            if !env.set(&name, val) {
                return Err(ErrorKind::UnboundVariable { name }.into());
            }
            Ok(State::Ret(Value::Void))
        }
        Frame::IfTest {
            then_br,
            else_br,
            env,
        } => {
            if val.is_truthy() {
                Ok(State::Eval(then_br, env))
            } else if let Some(e) = else_br {
                Ok(State::Eval(e, env))
            } else {
                Ok(State::Ret(Value::Void))
            }
        }
        Frame::AndRest { rest, env } => {
            if !val.is_truthy() {
                Ok(State::Ret(val))
            } else if rest.len() == 1 {
                Ok(State::Eval(
                    rest.into_iter().next().expect("checked len"),
                    env,
                ))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::AndRest {
                    rest,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::OrRest { rest, env } => {
            if val.is_truthy() {
                Ok(State::Ret(val))
            } else if rest.len() == 1 {
                Ok(State::Eval(
                    rest.into_iter().next().expect("checked len"),
                    env,
                ))
            } else {
                let mut rest = rest;
                let next = rest.remove(0);
                k.push(Frame::OrRest {
                    rest,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::LetInit {
            name,
            mut done_n,
            mut done_v,
            rest,
            body,
            env,
        } => {
            done_n.push(name);
            done_v.push(val);
            if rest.is_empty() {
                let local_env = Env::extend(&env, done_n, done_v);
                if body.is_empty() {
                    Ok(State::Ret(Value::Void))
                } else {
                    Ok(begin_seq(&body, &local_env, k))
                }
            } else {
                let mut rest = rest;
                let (next_name, next_expr) = rest.remove(0);
                k.push(Frame::LetInit {
                    name: next_name,
                    done_n,
                    done_v,
                    rest,
                    body,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next_expr, env))
            }
        }
        Frame::LetStarInit {
            name,
            rest,
            body,
            env,
        } => {
            // Bind this variable in a new env extending the current one
            let new_env = Env::extend(&env, vec![name], vec![val]);
            if rest.is_empty() {
                Ok(begin_seq(&body, &new_env, k))
            } else {
                let mut rest = rest;
                let (next_name, next_expr) = rest.remove(0);
                k.push(Frame::LetStarInit {
                    name: next_name,
                    rest,
                    body,
                    env: Rc::clone(&new_env),
                });
                Ok(State::Eval(next_expr, new_env))
            }
        }
        Frame::NamedLetInit {
            loop_name,
            name,
            mut done_n,
            mut done_v,
            rest,
            body,
            env,
        } => {
            done_n.push(name);
            done_v.push(val);
            if rest.is_empty() {
                let params = done_n;
                let func_env =
                    Env::extend(&env, vec![loop_name.clone()], vec![Value::Void]);
                let recursive_lambda = Value::Lambda {
                    params,
                    rest_param: None,
                    body,
                    closure_env: Rc::clone(&func_env),
                };
                func_env.define(loop_name, recursive_lambda.clone());
                Ok(State::Apply(recursive_lambda, done_v, env, Span::default()))
            } else {
                let mut rest = rest;
                let (next_name, next_expr) = rest.remove(0);
                k.push(Frame::NamedLetInit {
                    loop_name,
                    name: next_name,
                    done_n,
                    done_v,
                    rest,
                    body,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next_expr, env))
            }
        }
        Frame::ForEachStep { .. } | Frame::MapStep { .. } => {
            step_ret_iterate(val, frame, k)
        }
        Frame::CondTest {
            clause_body,
            rest_clauses,
            env,
        } => {
            if val.is_truthy() {
                if clause_body.is_empty() {
                    Ok(State::Ret(val))
                } else {
                    Ok(begin_seq(&clause_body, &env, k))
                }
            } else {
                step_cond(&rest_clauses, &env, k)
            }
        }
        Frame::CaseKey { clauses, env } => {
            // val is the key; dispatch through clauses using eqv?
            dispatch_case(val, &clauses, &env, k)
        }
        Frame::DoInit { .. }
        | Frame::DoTest { .. }
        | Frame::DoStep { .. } => step_ret_do(val, frame, k),
        Frame::DynamicWindAfterIn { .. }
        | Frame::DynamicWindAfterBody { .. }
        | Frame::DynamicWindAfterOut { .. }
        | Frame::WindTransfer { .. }
        | Frame::PopHandler
        | Frame::GuardClauseTest { .. }
        | Frame::GuardClauseDispatch { .. } => {
            step_ret_wind(val, frame, k, wind, handlers)
        }
        Frame::CallWithValuesConsumer { consumer, env, span } => {
            let call_args = match val {
                Value::MultipleValues(vs) => vs,
                single => vec![single],
            };
            Ok(State::Apply(consumer, call_args, env, span))
        }
        Frame::DefSyntax { name, env } => {
            env.define(name, Value::SyntaxTransformer(Box::new(val)));
            Ok(State::Ret(Value::Void))
        }
        Frame::ExpandSyntax { call_env, .. } => {
            match val {
                Value::SyntaxObject(expr) => Ok(State::Eval(expr, call_env)),
                other => Err(ErrorKind::BadSyntax {
                    form: "syntax-case".into(),
                    message: format!("transformer must return syntax, got {}", other.to_display_string()),
                }.into()),
            }
        }
        Frame::SyntaxCaseMatch { literals, clauses, env } => {
            step_ret_syntax_case(val, &literals, &clauses, &env, k)
        }
        Frame::WithSyntaxBind { pattern, rest_bindings, body, env } => {
            step_ret_with_syntax(val, &pattern, rest_bindings, body, &env, k)
        }
    }
}

// ── step_ret_iterate (for-each / map continuation frames) ────

fn step_ret_iterate(val: Value, frame: Frame, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    match frame {
        Frame::ForEachStep { func, lists, index, env, span } => {
            let next_index = index + 1;
            if next_index >= lists[0].len() {
                Ok(State::Ret(Value::Void))
            } else {
                let next_args: Vec<Value> = lists.iter().map(|l| l[next_index].clone()).collect();
                k.push(Frame::ForEachStep {
                    func: func.clone(),
                    lists,
                    index: next_index,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Apply(func, next_args, env, span))
            }
        }
        Frame::MapStep { func, lists, index, mut results, env, span } => {
            results.push(val);
            let next_index = index + 1;
            if next_index >= lists[0].len() {
                Ok(State::Ret(Value::List(results)))
            } else {
                let next_args: Vec<Value> = lists.iter().map(|l| l[next_index].clone()).collect();
                k.push(Frame::MapStep {
                    func: func.clone(),
                    lists,
                    index: next_index,
                    results,
                    env: Rc::clone(&env),
                    span,
                });
                Ok(State::Apply(func, next_args, env, span))
            }
        }
        _ => unreachable!("step_ret_iterate called with non-iterate frame"),
    }
}

// ── step_ret_wind (dynamic-wind / guard continuation frames) ─

fn step_ret_wind(
    val: Value,
    frame: Frame,
    k: &mut Vec<Frame>,
    wind: &mut Vec<WindEntry>,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    match frame {
        Frame::DynamicWindAfterIn {
            body_thunk,
            out_thunk,
            wind_entry,
            env,
            span,
        } => {
            // in-thunk finished; push wind entry and call body-thunk
            wind.push(wind_entry);
            k.push(Frame::DynamicWindAfterBody {
                out_thunk,
                env: Rc::clone(&env),
                span,
            });
            Ok(State::Apply(body_thunk, Vec::new(), env, span))
        }
        Frame::DynamicWindAfterBody { out_thunk, env, span } => {
            // body-thunk finished; pop wind entry, save body value, call out-thunk
            wind.pop();
            k.push(Frame::DynamicWindAfterOut { body_val: val });
            Ok(State::Apply(out_thunk, Vec::new(), env, span))
        }
        Frame::DynamicWindAfterOut { body_val } => {
            // out-thunk finished; return saved body value
            Ok(State::Ret(body_val))
        }
        Frame::WindTransfer {
            mut ops,
            final_val,
            env,
            span,
        } => {
            // Process next wind operation (ignore the value from previous thunk call)
            if ops.is_empty() {
                Ok(State::Ret(final_val))
            } else {
                let op = ops.remove(0);
                match op {
                    WindOp::CallOut(thunk) => {
                        // Wind stack was already truncated; call out-thunk
                        k.push(Frame::WindTransfer {
                            ops,
                            final_val,
                            env: Rc::clone(&env),
                            span,
                        });
                        Ok(State::Apply(thunk, Vec::new(), env, span))
                    }
                    WindOp::CallInAndPush(entry) => {
                        let in_thunk = entry.in_thunk.clone();
                        k.push(Frame::WindTransfer {
                            ops,
                            final_val,
                            env: Rc::clone(&env),
                            span,
                        });
                        wind.push(entry);
                        Ok(State::Apply(in_thunk, Vec::new(), env, span))
                    }
                }
            }
        }
        Frame::PopHandler => {
            handlers.pop();
            Ok(State::Ret(val))
        }
        Frame::GuardClauseTest { var, raised_val, clauses, env } => {
            // Ignore val (from wind transfer); test guard clauses
            let guard_env = Env::extend(&env, vec![var], vec![raised_val.clone()]);
            step_guard_clauses(&raised_val, &clauses, &guard_env, k)
        }
        Frame::GuardClauseDispatch { clause_body, rest_clauses, raised_val, env } => {
            if val.is_truthy() {
                if clause_body.is_empty() {
                    Ok(State::Ret(val))
                } else {
                    Ok(begin_seq(&clause_body, &env, k))
                }
            } else {
                step_guard_clauses(&raised_val, &rest_clauses, &env, k)
            }
        }
        _ => unreachable!("step_ret_wind called with non-wind frame"),
    }
}

// ── step_ret_do (do-loop continuation frames) ──────────────

fn step_ret_do(val: Value, frame: Frame, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    match frame {
        Frame::DoInit {
            var_names,
            mut done_vals,
            rest_inits,
            step_exprs,
            test,
            result_exprs,
            body,
            env,
        } => {
            done_vals.push(val);
            if rest_inits.is_empty() {
                let do_env = Env::extend(&env, var_names.clone(), done_vals);
                k.push(Frame::DoTest {
                    vars: var_names,
                    step_exprs,
                    test: test.clone(),
                    result_exprs,
                    body,
                    env: Rc::clone(&do_env),
                });
                Ok(State::Eval(test, Rc::clone(&do_env)))
            } else {
                let mut rest_inits = rest_inits;
                let next = rest_inits.remove(0);
                k.push(Frame::DoInit {
                    var_names,
                    done_vals,
                    rest_inits,
                    step_exprs,
                    test,
                    result_exprs,
                    body,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next, env))
            }
        }
        Frame::DoTest {
            vars,
            step_exprs,
            test,
            result_exprs,
            body,
            env,
        } => {
            if val.is_truthy() {
                if result_exprs.is_empty() {
                    Ok(State::Ret(Value::Void))
                } else {
                    Ok(begin_seq(&result_exprs, &env, k))
                }
            } else {
                let step_indices: Vec<usize> = step_exprs
                    .iter()
                    .enumerate()
                    .filter_map(|(i, s)| s.as_ref().map(|_| i))
                    .collect();
                if step_indices.is_empty() {
                    if !body.is_empty() {
                        k.push(Frame::DoTest {
                            vars,
                            step_exprs,
                            test: test.clone(),
                            result_exprs,
                            body: body.clone(),
                            env: Rc::clone(&env),
                        });
                        let mut all = body;
                        all.push(test);
                        Ok(begin_seq(&all, &env, k))
                    } else {
                        k.push(Frame::DoTest {
                            vars,
                            step_exprs,
                            test: test.clone(),
                            result_exprs,
                            body,
                            env: Rc::clone(&env),
                        });
                        Ok(State::Eval(test, env))
                    }
                } else {
                    let first_idx = step_indices[0];
                    let first_step = step_exprs[first_idx]
                        .clone()
                        .expect("filtered by step_indices");
                    let remaining_indices = step_indices[1..].to_vec();
                    if !body.is_empty() {
                        k.push(Frame::DoStep {
                            vars,
                            step_exprs,
                            test,
                            result_exprs,
                            body: body.clone(),
                            done_vals: Vec::new(),
                            rest_indices: remaining_indices,
                            env: Rc::clone(&env),
                        });
                        let mut all = body;
                        all.push(first_step);
                        Ok(begin_seq(&all, &env, k))
                    } else {
                        k.push(Frame::DoStep {
                            vars,
                            step_exprs,
                            test,
                            result_exprs,
                            body,
                            done_vals: Vec::new(),
                            rest_indices: remaining_indices,
                            env: Rc::clone(&env),
                        });
                        Ok(State::Eval(first_step, env))
                    }
                }
            }
        }
        Frame::DoStep {
            vars,
            step_exprs,
            test,
            result_exprs,
            body,
            mut done_vals,
            rest_indices,
            env,
        } => {
            done_vals.push(val);
            if rest_indices.is_empty() {
                let mut new_vals: Vec<Value> = Vec::with_capacity(vars.len());
                let mut step_val_iter = done_vals.into_iter();
                for (i, var) in vars.iter().enumerate() {
                    if step_exprs[i].is_some() {
                        new_vals.push(
                            step_val_iter.next().expect("step val count matches"),
                        );
                    } else {
                        new_vals.push(
                            env.get(var).expect("do var bound in env"),
                        );
                    }
                }
                let parent = Env::parent_or_self(&env);
                let new_env = Env::extend(
                    &parent,
                    vars.clone(),
                    new_vals,
                );
                k.push(Frame::DoTest {
                    vars,
                    step_exprs,
                    test: test.clone(),
                    result_exprs,
                    body,
                    env: Rc::clone(&new_env),
                });
                Ok(State::Eval(test, new_env))
            } else {
                let mut rest_indices = rest_indices;
                let next_idx = rest_indices.remove(0);
                let next_step = step_exprs[next_idx]
                    .clone()
                    .expect("filtered by step_indices");
                k.push(Frame::DoStep {
                    vars,
                    step_exprs,
                    test,
                    result_exprs,
                    body,
                    done_vals,
                    rest_indices,
                    env: Rc::clone(&env),
                });
                Ok(State::Eval(next_step, env))
            }
        }
        _ => unreachable!("step_ret_do called with non-Do frame"),
    }
}

// ── step_apply ───────────────────────────────────────────────

fn step_apply(
    func: Value,
    args: Vec<Value>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    wind_state: (&mut Vec<WindEntry>, &mut u64),
    span: Span,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    let (wind, wind_id) = wind_state;
    match func {
        Value::Builtin(ref name) => match name.as_str() {
            "apply" => {
                if args.len() < 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let last = args.pop().expect("checked len >= 2");
                let tail = require_list(&last)?;
                let f = args.remove(0);
                args.extend(tail);
                Ok(State::Apply(f, args, Rc::clone(env), span))
            }
            "map" => {
                if args.len() < 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let func = args.remove(0);
                let mut lists: Vec<Vec<Value>> = Vec::new();
                for arg in args {
                    lists.push(require_list(&arg)?);
                }
                if lists.is_empty() || lists[0].is_empty() {
                    return Ok(State::Ret(Value::List(Vec::new())));
                }
                let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
                k.push(Frame::MapStep {
                    func: func.clone(),
                    lists,
                    index: 0,
                    results: Vec::new(),
                    env: Rc::clone(env),
                    span,
                });
                Ok(State::Apply(func, first_args, Rc::clone(env), span))
            }
            "for-each" => {
                if args.len() < 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let func = args.remove(0);
                let mut lists: Vec<Vec<Value>> = Vec::new();
                for arg in args {
                    lists.push(require_list(&arg)?);
                }
                if lists.is_empty() || lists[0].is_empty() {
                    return Ok(State::Ret(Value::Void));
                }
                let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
                k.push(Frame::ForEachStep {
                    func: func.clone(),
                    lists,
                    index: 0,
                    env: Rc::clone(env),
                    span,
                });
                Ok(State::Apply(func, first_args, Rc::clone(env), span))
            }
            "call/cc" | "call-with-current-continuation" => {
                if args.len() != 1 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 1,
                        got: args.len(),
                    }
                    .into());
                }
                let proc = args.into_iter().next().expect("checked len");
                let captured = CapturedCont(Rc::new((k.clone(), wind.clone(), handlers.clone())));
                let cont_val = Value::Continuation(captured);
                Ok(State::Apply(proc, vec![cont_val], Rc::clone(env), span))
            }
            "dynamic-wind" => {
                if args.len() != 3 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 3,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let in_thunk = args.remove(0);
                let body_thunk = args.remove(0);
                let out_thunk = args.remove(0);
                *wind_id += 1;
                let entry = WindEntry {
                    id: *wind_id,
                    in_thunk: in_thunk.clone(),
                    out_thunk: out_thunk.clone(),
                };
                k.push(Frame::DynamicWindAfterIn {
                    body_thunk,
                    out_thunk,
                    wind_entry: entry,
                    env: Rc::clone(env),
                    span,
                });
                // Call in-thunk with no args
                Ok(State::Apply(in_thunk, Vec::new(), Rc::clone(env), span))
            }
            "raise" => {
                if args.len() != 1 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 1,
                        got: args.len(),
                    }
                    .into());
                }
                let raised_val = args.into_iter().next().expect("checked len");
                if let Some(handler_entry) = handlers.pop() {
                    match handler_entry {
                        HandlerEntry::WithExceptionHandler { handler } => {
                            Ok(State::Apply(handler, vec![raised_val], Rc::clone(env), span))
                        }
                        HandlerEntry::Guard {
                            var,
                            clauses,
                            env: guard_env,
                            guard_k,
                            guard_wind,
                            guard_handlers,
                        } => {
                            // Compute wind transfer ops
                            let common_len = wind
                                .iter()
                                .zip(guard_wind.iter())
                                .take_while(|(a, b)| a.id == b.id)
                                .count();
                            let mut ops: Vec<WindOp> = Vec::new();
                            for entry in wind[common_len..].iter().rev() {
                                ops.push(WindOp::CallOut(entry.out_thunk.clone()));
                            }
                            for entry in &guard_wind[common_len..] {
                                ops.push(WindOp::CallInAndPush(entry.clone()));
                            }

                            // Restore to guard point
                            *k = guard_k;
                            wind.truncate(common_len);
                            *handlers = guard_handlers;

                            // Push clause test frame (will be reached after wind transfer)
                            k.push(Frame::GuardClauseTest {
                                var,
                                raised_val,
                                clauses,
                                env: guard_env,
                            });

                            if ops.is_empty() {
                                Ok(State::Ret(Value::Void))
                            } else {
                                k.push(Frame::WindTransfer {
                                    ops,
                                    final_val: Value::Void,
                                    env: Rc::clone(env),
                                    span,
                                });
                                Ok(State::Ret(Value::Void))
                            }
                        }
                    }
                } else {
                    Err(ErrorKind::UserRaise {
                        value: raised_val.to_display_string(),
                    }
                    .into())
                }
            }
            "values" => {
                match args.len() {
                    1 => Ok(State::Ret(args.into_iter().next().expect("checked len"))),
                    _ => Ok(State::Ret(Value::MultipleValues(args))),
                }
            }
            "call-with-values" => {
                if args.len() != 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let producer = args.remove(0);
                let consumer = args.remove(0);
                k.push(Frame::CallWithValuesConsumer {
                    consumer,
                    env: Rc::clone(env),
                    span,
                });
                Ok(State::Apply(producer, Vec::new(), Rc::clone(env), span))
            }
            "with-exception-handler" => {
                if args.len() != 2 {
                    return Err(ErrorKind::WrongArgCount {
                        expected: 2,
                        got: args.len(),
                    }
                    .into());
                }
                let mut args = args;
                let handler = args.remove(0);
                let thunk = args.remove(0);

                // Push PopHandler frame so handler is removed when thunk returns
                k.push(Frame::PopHandler);

                // Install handler
                handlers.push(HandlerEntry::WithExceptionHandler { handler });

                // Call thunk
                Ok(State::Apply(thunk, Vec::new(), Rc::clone(env), span))
            }
            _ => {
                let result = apply_builtin(name, &args, env)?;
                Ok(State::Ret(result))
            }
        },
        Value::Lambda {
            params,
            rest_param,
            body,
            closure_env,
        } => {
            let local_env = bind_args(&params, &rest_param, args, &closure_env)?;
            if body.is_empty() {
                Ok(State::Ret(Value::Void))
            } else {
                Ok(begin_seq(&body, &local_env, k))
            }
        }
        Value::Continuation(captured) => {
            apply_continuation(captured, args, env, k, wind, span, handlers)
        }
        Value::RecordConstructor { type_tag, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(ErrorKind::WrongArgCount {
                    expected: field_names.len(),
                    got: args.len(),
                }.into());
            }
            Ok(State::Ret(Value::Record {
                type_tag,
                type_name,
                fields: args,
            }))
        }
        Value::RecordPredicate { type_tag } => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }.into());
            }
            let result = match &args[0] {
                Value::Record { type_tag: ref t, .. } => Rc::ptr_eq(&type_tag, t),
                _ => false,
            };
            Ok(State::Ret(Value::Boolean(result)))
        }
        Value::RecordAccessor { type_tag, type_name, field_name: _, field_index } => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }.into());
            }
            match &args[0] {
                Value::Record { type_tag: ref t, fields, .. } if Rc::ptr_eq(&type_tag, t) => {
                    Ok(State::Ret(fields[field_index].clone()))
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: type_name,
                    got: other.to_display_string(),
                }.into()),
            }
        }
        Value::SyntaxRules { .. } | Value::SyntaxTransformer(_) => Err(ErrorKind::NotAProcedure {
            value: "#<macro>".into(),
        }
        .into()),
        Value::SyntaxObject(_) => Err(ErrorKind::NotAProcedure {
            value: "#<syntax>".into(),
        }
        .into()),
        other => Err(ErrorKind::NotAProcedure {
            value: other.to_display_string(),
        }
        .into()),
    }
}

fn apply_continuation(
    captured: CapturedCont,
    args: Vec<Value>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
    wind: &mut Vec<WindEntry>,
    span: Span,
    handlers: &mut Vec<HandlerEntry>,
) -> Result<State, EvalError> {
    if args.len() != 1 {
        return Err(ErrorKind::WrongArgCount {
            expected: 1,
            got: args.len(),
        }
        .into());
    }
    let val = args.into_iter().next().expect("checked len");
    let (target_frames, target_wind, target_handlers) = captured
        .0
        .downcast_ref::<(Vec<Frame>, Vec<WindEntry>, Vec<HandlerEntry>)>()
        .expect("continuation frame type");

    // Compute common prefix length between current and target wind stacks
    let common_len = wind
        .iter()
        .zip(target_wind.iter())
        .take_while(|(a, b)| a.id == b.id)
        .count();

    // Build wind operations: unwind current (innermost first), rewind target (outermost first)
    let mut ops: Vec<WindOp> = Vec::new();
    for entry in wind[common_len..].iter().rev() {
        ops.push(WindOp::CallOut(entry.out_thunk.clone()));
    }
    for entry in &target_wind[common_len..] {
        ops.push(WindOp::CallInAndPush(entry.clone()));
    }

    // Restore target continuation stack and handlers
    *k = target_frames.clone();
    *handlers = target_handlers.clone();
    wind.truncate(common_len);

    if ops.is_empty() {
        Ok(State::Ret(val))
    } else {
        k.push(Frame::WindTransfer {
            ops,
            final_val: val,
            env: Rc::clone(env),
            span,
        });
        Ok(State::Ret(Value::Void))
    }
}

fn bind_args(
    params: &[String],
    rest_param: &Option<String>,
    args: Vec<Value>,
    closure_env: &Rc<Env>,
) -> Result<Rc<Env>, EvalError> {
    let mut names = params.to_vec();
    let vals = match rest_param {
        Some(rest) => {
            if args.len() < params.len() {
                return Err(ErrorKind::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                }
                .into());
            }
            let mut v = args[..params.len()].to_vec();
            names.push(rest.clone());
            v.push(Value::list_from_vec(args[params.len()..].to_vec()));
            v
        }
        None => {
            if args.len() != params.len() {
                return Err(ErrorKind::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                }
                .into());
            }
            args
        }
    };
    Ok(Env::extend(closure_env, names, vals))
}

// ── quote / lambda / params (unchanged) ──────────────────────

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(ErrorKind::BadSyntax {
            form: "quote".into(),
            message: "expected 1 argument".into(),
        }
        .into());
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Rational(n, d) => Ok(Value::Rational(*n, *d)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::new_str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Value> = elems
                .iter()
                .map(expr_to_value)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "lambda".into(),
            message: "expected (lambda (params) body...)".into(),
        }
        .into());
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, "lambda")?,
        ExprKind::Symbol(s) => (Vec::new(), Some(s.clone())),
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "lambda".into(),
                message: "expected parameter list".into(),
            }
            .into())
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        closure_env: Rc::clone(env),
    })
}

fn parse_params(
    param_exprs: &[Expr],
    form: &str,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() || i + 2 != param_exprs.len() {
                    return Err(ErrorKind::BadSyntax {
                        form: form.into(),
                        message: "expected exactly one parameter after dot".into(),
                    }
                    .into());
                }
                match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(rest) => rest_param = Some(rest.clone()),
                    _ => {
                        return Err(ErrorKind::BadSyntax {
                            form: form.into(),
                            message: "rest parameter must be a symbol".into(),
                        }
                        .into())
                    }
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: form.into(),
                    message: "parameter must be a symbol".into(),
                }
                .into())
            }
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let binding_list = match &expr.kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "bindings must be a list".into(),
            }
            .into())
        }
    };
    let mut result = Vec::new();
    for binding in binding_list {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => match &pair[0].kind {
                ExprKind::Symbol(var) => result.push((var.clone(), pair[1].clone())),
                _ => {
                    return Err(ErrorKind::BadSyntax {
                        form: "let".into(),
                        message: "binding name must be a symbol".into(),
                    }
                    .into())
                }
            },
            _ => {
                return Err(ErrorKind::BadSyntax {
                    form: "let".into(),
                    message: "each binding must be (var init)".into(),
                }
                .into())
            }
        }
    }
    Ok(result)
}

// ── builtins (unchanged) ─────────────────────────────────────

fn apply_builtin(name: &str, args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "floor" | "ceiling" | "round" | "truncate"
        | "sqrt" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log"
        | "gcd" | "lcm" => {
            apply_numeric_builtin(name, args)
        }
        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "list-ref" | "list-tail" | "list?" | "assoc" | "member"
        | "set-car!" | "set-cdr!"
        | "assq" | "assv" | "memq" | "memv" => {
            apply_list_builtin(name, args)
        }
        // Dynamic c[ad]+r handler
        n if n.len() >= 4 && n.starts_with('c') && n.ends_with('r')
            && n[1..n.len()-1].bytes().all(|b| b == b'a' || b == b'd') => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let mut val = args[0].clone();
            for ch in n[1..n.len()-1].bytes().rev() {
                match ch {
                    b'a' => val = value_car(&val)?,
                    b'd' => val = value_cdr(&val)?,
                    _ => unreachable!(),
                }
            }
            Ok(val)
        }
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?" | "char<=?" | "char>=?" | "char>?"
        | "char-ci=?" | "char-ci<?" | "char-ci>?" | "char-ci<=?" | "char-ci>=?"
        | "char-lower-case?" | "char-upper-case?" | "char-whitespace?"
        | "string=?" | "string<?" | "string>?" | "string>=?" | "string<=?"
        | "string-ci=?" | "string-ci<?" | "string-ci>?" | "string-ci<=?" | "string-ci>=?"
        | "string-upcase" | "string-downcase" => {
            apply_char_cmp_builtin(name, args)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string-set!" | "string->list" | "list->string"
        | "char->integer" | "integer->char"
        | "make-string" | "string" => apply_string_builtin(name, args),
        "not" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "string?" | "number?" | "integer?" | "rational?" | "exact?" | "inexact?"
        | "boolean?" | "pair?" | "symbol?" | "char?"
        | "exact->inexact" | "inexact->exact"
        | "numerator" | "denominator"
        | "procedure?" | "complex?" | "real?" => {
            apply_type_builtin(name, args)
        }
        "display" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            env.write_output(&args[0].to_display_output());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            env.write_output(&args[0].to_display_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 0, got: args.len() }.into());
            }
            env.write_output("\n");
            Ok(Value::Void)
        }
        "write-char" => {
            if args.is_empty() || args.len() > 2 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            env.write_output(&c.to_string());
            Ok(Value::Void)
        }
        // I/O stubs — bound but not fully implemented
        "call-with-input-file" | "call-with-output-file"
        | "open-input-file" | "open-output-file"
        | "close-input-port" | "close-output-port"
        | "read" | "read-char" | "peek-char" => {
            Err(ErrorKind::TypeMismatch {
                expected: "supported operation".into(),
                got: format!("{} is not supported in this interpreter", name),
            }.into())
        }
        "current-input-port" | "current-output-port" => {
            Ok(Value::Symbol(format!("#<{}>", name)))
        }
        "input-port?" | "output-port?" | "eof-object?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(false))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(scheme_eq(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(args[0] == args[1]))
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(scheme_eqv(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let len = require_int(&args[0])? as usize;
            let fill = if args.len() == 2 {
                args[1].clone()
            } else {
                Value::Integer(0)
            };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let vec_ref = match &args[0] {
                Value::Vector(v) => v,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "vector".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            let v = vec_ref.borrow();
            if idx >= v.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid vector index".into(),
                    got: format!("index {} for vector of length {}", idx, v.len()),
                }
                .into());
            }
            Ok(v[idx].clone())
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount { expected: 3, got: args.len() }.into());
            }
            let vec_ref = match &args[0] {
                Value::Vector(v) => v,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "vector".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let idx = require_int(&args[1])? as usize;
            let mut v = vec_ref.borrow_mut();
            if idx >= v.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid vector index".into(),
                    got: format!("index {} for vector of length {}", idx, v.len()),
                }
                .into());
            }
            v[idx] = args[2].clone();
            Ok(Value::Void)
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "vector".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::list_from_vec(v.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "vector".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let elems = require_list(&args[0])?;
            Ok(Value::Vector(Rc::new(RefCell::new(elems))))
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let mut elems = require_list(&args[0])?;
            elems.reverse();
            Ok(Value::list_from_vec(elems))
        }
        "syntax->datum" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::SyntaxObject(expr) => expr_to_value(expr),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "syntax object".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "datum->syntax" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let datum_expr = value_to_expr(&args[1], Span::default());
            Ok(Value::SyntaxObject(datum_expr))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_type_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_, _))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let result = match &args[0] {
                Value::Integer(_) => true,
                Value::Rational(_, _) => false,
                Value::Float(f) => *f == f.floor() && f.is_finite(),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            Ok(Value::Float(f))
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Integer(_) => Ok(args[0].clone()),
                Value::Rational(_, _) => Ok(args[0].clone()),
                Value::Float(f) => {
                    let result = float_to_exact(*f);
                    Ok(result.to_value())
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "number".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "rational".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "rational".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(elems) if !elems.is_empty()
            ) || matches!(&args[0], Value::Pair(_))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Builtin(_) | Value::Lambda { .. } | Value::Continuation(_)
                | Value::RecordConstructor { .. } | Value::RecordPredicate { .. }
                | Value::RecordAccessor { .. }
            )))
        }
        "complex?" | "real?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Integer(_) | Value::Float(_) | Value::Rational(_, _)
            )))
        }
        other => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", other),
        }
        .into()),
    }
}

fn apply_numeric_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut acc = NumVal::Int(0);
            for arg in args {
                acc = num_add(acc, NumVal::from_value(arg)?);
            }
            Ok(acc.to_value())
        }
        "-" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let first = NumVal::from_value(&args[0])?;
            if args.len() == 1 {
                return Ok(num_neg(first).to_value());
            }
            let mut acc = first;
            for arg in &args[1..] {
                acc = num_sub(acc, NumVal::from_value(arg)?);
            }
            Ok(acc.to_value())
        }
        "*" => {
            let mut acc = NumVal::Int(1);
            for arg in args {
                acc = num_mul(acc, NumVal::from_value(arg)?);
            }
            Ok(acc.to_value())
        }
        "/" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let first = NumVal::from_value(&args[0])?;
            if args.len() == 1 {
                return Ok(num_div(NumVal::Int(1), first)?.to_value());
            }
            let mut acc = first;
            for arg in &args[1..] {
                acc = num_div(acc, NumVal::from_value(arg)?)?;
            }
            Ok(acc.to_value())
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| (a - b).abs() < f64::EPSILON || a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "abs" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match NumVal::from_value(&args[0])? {
                NumVal::Int(n) => Ok(Value::Integer(n.abs())),
                NumVal::Rat(n, d) => Ok(make_rat(n.abs(), d).to_value()),
                NumVal::Flt(f) => Ok(Value::Float(f.abs())),
            }
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 {
                return Err(ErrorKind::DivisionByZero.into());
            }
            let r = a % b;
            let result = if r != 0 && (r > 0) != (b > 0) { r + b } else { r };
            Ok(Value::Integer(result))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 {
                return Err(ErrorKind::DivisionByZero.into());
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 {
                return Err(ErrorKind::DivisionByZero.into());
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let mut result = NumVal::from_value(&args[0])?;
            for arg in &args[1..] {
                let n = NumVal::from_value(arg)?;
                if n.to_f64() < result.to_f64() {
                    result = n;
                }
            }
            Ok(result.to_value())
        }
        "max" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let mut result = NumVal::from_value(&args[0])?;
            for arg in &args[1..] {
                let n = NumVal::from_value(arg)?;
                if n.to_f64() > result.to_f64() {
                    result = n;
                }
            }
            Ok(result.to_value())
        }
        "expt" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let base = require_int(&args[0])?;
            let exp = require_int(&args[1])?;
            if exp < 0 {
                return Err(ErrorKind::TypeMismatch {
                    expected: "non-negative exponent".into(),
                    got: exp.to_string(),
                }
                .into());
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            Ok(Value::Boolean(n % 2 == 0))
        }
        "floor" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            Ok(Value::Integer(f.floor() as i64))
        }
        "ceiling" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            Ok(Value::Integer(f.ceil() as i64))
        }
        "round" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            Ok(Value::Integer(f.round() as i64))
        }
        "truncate" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            Ok(Value::Integer(f.trunc() as i64))
        }
        "sqrt" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            let r = f.sqrt();
            if r == r.floor() && r.is_finite() {
                Ok(Value::Integer(r as i64))
            } else {
                Ok(Value::Float(r))
            }
        }
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let f = NumVal::from_value(&args[0])?.to_f64();
            let r = match name {
                "sin" => f.sin(),
                "cos" => f.cos(),
                "tan" => f.tan(),
                "asin" => f.asin(),
                "acos" => f.acos(),
                "atan" => f.atan(),
                "exp" => f.exp(),
                "log" => f.ln(),
                _ => unreachable!(),
            };
            Ok(Value::Float(r))
        }
        "gcd" => {
            if args.is_empty() {
                return Ok(Value::Integer(0));
            }
            let mut result = require_int(&args[0])?.unsigned_abs();
            for arg in &args[1..] {
                result = gcd(result, require_int(arg)?.unsigned_abs());
            }
            Ok(Value::Integer(result as i64))
        }
        "lcm" => {
            if args.is_empty() {
                return Ok(Value::Integer(1));
            }
            let mut result = require_int(&args[0])?.unsigned_abs();
            for arg in &args[1..] {
                let b = require_int(arg)?.unsigned_abs();
                if result == 0 && b == 0 {
                    result = 0;
                } else {
                    result = result / gcd(result, b) * b;
                }
            }
            Ok(Value::Integer(result as i64))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_list_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            value_car(&args[0])
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            value_cdr(&args[0])
        }
        "null?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(elems) if elems.is_empty()
            )))
        }
        "list" => Ok(Value::list_from_vec(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let elems = require_list(&args[0])?;
            Ok(Value::Integer(elems.len() as i64))
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                let elems = require_list(arg)?;
                result.extend(elems);
            }
            Ok(Value::list_from_vec(result))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let elems = require_list(&args[0])?;
            let idx = require_int(&args[1])? as usize;
            if idx >= elems.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid list index".into(),
                    got: format!("index {} for list of length {}", idx, elems.len()),
                }
                .into());
            }
            Ok(elems[idx].clone())
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            // list-tail returns the actual tail pair, not a copy
            let idx = require_int(&args[1])? as usize;
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = value_cdr(&current)?;
            }
            Ok(current)
        }
        "list?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(value_is_list(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let key = &args[0];
            let alist = require_list(&args[1])?;
            for entry in &alist {
                let car = match value_car(entry) {
                    Ok(c) => c,
                    Err(_) => continue,
                };
                if car == *key {
                    return Ok(entry.clone());
                }
            }
            Ok(Value::Boolean(false))
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            match &args[0] {
                Value::Pair(cell) => {
                    cell.borrow_mut().0 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "mutable pair".into(),
                    got: args[0].to_display_string(),
                }
                .into()),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            match &args[0] {
                Value::Pair(cell) => {
                    cell.borrow_mut().1 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "mutable pair".into(),
                    got: args[0].to_display_string(),
                }
                .into()),
            }
        }
        "caar" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            value_car(&value_car(&args[0])?)
        }
        "cadr" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            value_car(&value_cdr(&args[0])?)
        }
        "cdar" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            value_cdr(&value_car(&args[0])?)
        }
        "cddr" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            value_cdr(&value_cdr(&args[0])?)
        }
        "member" | "memq" | "memv" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let key = &args[0];
            let cmp: fn(&Value, &Value) -> bool = match name {
                "memq" => scheme_eq,
                "memv" => scheme_eqv,
                _ => |a, b| a == b,
            };
            let elems = require_list(&args[1])?;
            for (i, elem) in elems.iter().enumerate() {
                if cmp(elem, key) {
                    return Ok(Value::list_from_vec(elems[i..].to_vec()));
                }
            }
            Ok(Value::Boolean(false))
        }
        "assq" | "assv" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let key = &args[0];
            let cmp: fn(&Value, &Value) -> bool = match name {
                "assq" => scheme_eq,
                _ => scheme_eqv,
            };
            let alist = require_list(&args[1])?;
            for entry in &alist {
                let car = match value_car(entry) {
                    Ok(c) => c,
                    Err(_) => continue,
                };
                if cmp(&car, key) {
                    return Ok(entry.clone());
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_char_cmp_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Boolean(c.is_alphabetic()))
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Boolean(c.is_ascii_digit()))
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Char(c.to_ascii_uppercase()))
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let c = require_char(&args[0])?;
            Ok(Value::Char(c.to_ascii_lowercase()))
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_char(&args[0])?;
            let b = require_char(&args[1])?;
            Ok(Value::Boolean(a == b))
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_char(&args[0])?;
            let b = require_char(&args[1])?;
            Ok(Value::Boolean(a < b))
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() == *b.borrow()))
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() < *b.borrow()))
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(
                a.borrow().to_lowercase() == b.borrow().to_lowercase(),
            ))
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let s = require_string(&args[0])?;
            Ok(Value::new_str(s.borrow().to_uppercase()))
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let s = require_string(&args[0])?;
            Ok(Value::new_str(s.borrow().to_lowercase()))
        }
        "char<=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])? <= require_char(&args[1])?))
        }
        "char>=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])? >= require_char(&args[1])?))
        }
        "char>?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])? > require_char(&args[1])?))
        }
        "char-ci=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.eq_ignore_ascii_case(&require_char(&args[1])?)))
        }
        "char-ci<?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.to_ascii_lowercase() < require_char(&args[1])?.to_ascii_lowercase()))
        }
        "char-ci>?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.to_ascii_lowercase() > require_char(&args[1])?.to_ascii_lowercase()))
        }
        "char-ci<=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.to_ascii_lowercase() <= require_char(&args[1])?.to_ascii_lowercase()))
        }
        "char-ci>=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.to_ascii_lowercase() >= require_char(&args[1])?.to_ascii_lowercase()))
        }
        "char-lower-case?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.is_ascii_lowercase()))
        }
        "char-upper-case?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.is_ascii_uppercase()))
        }
        "char-whitespace?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(require_char(&args[0])?.is_whitespace()))
        }
        "string>?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() > *b.borrow()))
        }
        "string>=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() >= *b.borrow()))
        }
        "string<=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(*a.borrow() <= *b.borrow()))
        }
        "string-ci<?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(a.borrow().to_lowercase() < b.borrow().to_lowercase()))
        }
        "string-ci>?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(a.borrow().to_lowercase() > b.borrow().to_lowercase()))
        }
        "string-ci<=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(a.borrow().to_lowercase() <= b.borrow().to_lowercase()))
        }
        "string-ci>=?" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            let a = require_string(&args[0])?;
            let b = require_string(&args[1])?;
            Ok(Value::Boolean(a.borrow().to_lowercase() >= b.borrow().to_lowercase()))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn apply_string_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::Str(s, _) => result.push_str(&s.borrow()),
                    other => {
                        return Err(ErrorKind::TypeMismatch {
                            expected: "string".into(),
                            got: other.to_display_string(),
                        }
                        .into())
                    }
                }
            }
            Ok(Value::new_str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Integer(s.borrow().len() as i64)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 3,
                    got: args.len(),
                }
                .into());
            }
            let s_ref = match &args[0] {
                Value::Str(s, _) => s,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "string".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let s = s_ref.borrow();
            let start = require_int(&args[1])? as usize;
            let end = require_int(&args[2])? as usize;
            if start > s.len() || end > s.len() || start > end {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid substring indices".into(),
                    got: format!("start={}, end={}, length={}", start, end, s.len()),
                }
                .into());
            }
            Ok(Value::new_str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => {
                    let text = s.borrow();
                    if let Ok(n) = text.parse::<i64>() {
                        Ok(Value::Integer(n))
                    } else if let Ok(f) = text.parse::<f64>() {
                        Ok(Value::Float(f))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::new_str(n.to_string())),
                Value::Float(f) => Ok(Value::new_str(format!("{}", f))),
                Value::Rational(n, d) => Ok(Value::new_str(format!("{}/{}", n, d))),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "number".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::new_str(s.clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "symbol".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::Symbol(s.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                }
                .into());
            }
            let s_ref = match &args[0] {
                Value::Str(s, _) => s,
                other => {
                    return Err(ErrorKind::TypeMismatch {
                        expected: "string".into(),
                        got: other.to_display_string(),
                    }
                    .into())
                }
            };
            let s = s_ref.borrow();
            let idx = require_int(&args[1])? as usize;
            if idx >= s.len() {
                return Err(ErrorKind::TypeMismatch {
                    expected: "valid string index".into(),
                    got: format!("index {} for string of length {}", idx, s.len()),
                }
                .into());
            }
            Ok(Value::Char(s.as_bytes()[idx] as char))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                }
                .into());
            }
            match &args[0] {
                Value::Str(s, _) => Ok(Value::new_mutable_str(s.borrow().clone())),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(ErrorKind::WrongArgCount { expected: 3, got: args.len() }.into());
            }
            match &args[0] {
                Value::Str(s, StringMutability::Mutable) => {
                    let idx = require_int(&args[1])? as usize;
                    let ch = require_char(&args[2])?;
                    let mut borrowed = s.borrow_mut();
                    if idx >= borrowed.len() {
                        return Err(ErrorKind::TypeMismatch {
                            expected: "valid string index".into(),
                            got: format!("index {} for string of length {}", idx, borrowed.len()),
                        }
                        .into());
                    }
                    // SAFETY: replacing a single byte with a single-byte char (ASCII assumption matching string-ref)
                    // Safe because we work on bytes directly
                    let bytes = unsafe { borrowed.as_bytes_mut() };
                    bytes[idx] = ch as u8;
                    Ok(Value::Void)
                }
                Value::Str(_, StringMutability::Immutable) => {
                    Err(ErrorKind::ImmutableString.into())
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }
                .into()),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Str(s, _) => {
                    let chars: Vec<Value> = s.borrow().chars().map(Value::Char).collect();
                    Ok(Value::list_from_vec(chars))
                }
                other => Err(ErrorKind::TypeMismatch {
                    expected: "string".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let chars = require_list(&args[0])?;
            let mut s = String::with_capacity(chars.len());
            for v in &chars {
                match v {
                    Value::Char(c) => s.push(*c),
                    other => return Err(ErrorKind::TypeMismatch {
                        expected: "char".into(),
                        got: other.to_display_string(),
                    }.into()),
                }
            }
            Ok(Value::new_str(s))
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                other => Err(ErrorKind::TypeMismatch {
                    expected: "char".into(),
                    got: other.to_display_string(),
                }.into()),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            let n = require_int(&args[0])?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(ErrorKind::TypeMismatch {
                    expected: "valid Unicode code point".into(),
                    got: format!("{}", n),
                }.into()),
            }
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }
        .into()),
    }
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        Value::Float(f) if *f == f.floor() && f.is_finite() => Ok(*f as i64),
        other => Err(ErrorKind::TypeMismatch {
            expected: "integer".into(),
            got: other.to_display_string(),
        }
        .into()),
    }
}

fn require_char(val: &Value) -> Result<char, EvalError> {
    match val {
        Value::Char(c) => Ok(*c),
        other => Err(ErrorKind::TypeMismatch {
            expected: "char".into(),
            got: other.to_display_string(),
        }
        .into()),
    }
}

fn require_string(val: &Value) -> Result<&std::rc::Rc<std::cell::RefCell<String>>, EvalError> {
    match val {
        Value::Str(s, _) => Ok(s),
        other => Err(ErrorKind::TypeMismatch {
            expected: "string".into(),
            got: other.to_display_string(),
        }
        .into()),
    }
}

/// Scheme `eq?` — identity comparison (not structural equality).
fn scheme_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
        (Value::Void, Value::Void) => true,
        (Value::Str(x, _), Value::Str(y, _)) => Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        _ => false,
    }
}

/// Scheme `eqv?` — like eq? but compares numbers and characters by value.
fn scheme_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
        (Value::Void, Value::Void) => true,
        (Value::Str(x, _), Value::Str(y, _)) => Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => Rc::ptr_eq(x, y),
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        _ => false,
    }
}

// ── pair / list helpers ──────────────────────────────────────

fn value_car(val: &Value) -> Result<Value, EvalError> {
    match val {
        Value::Pair(cell) => Ok(cell.borrow().0.clone()),
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(ErrorKind::TypeMismatch {
            expected: "pair".into(),
            got: val.to_display_string(),
        }
        .into()),
    }
}

fn value_cdr(val: &Value) -> Result<Value, EvalError> {
    match val {
        Value::Pair(cell) => Ok(cell.borrow().1.clone()),
        Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
        _ => Err(ErrorKind::TypeMismatch {
            expected: "pair".into(),
            got: val.to_display_string(),
        }
        .into()),
    }
}

/// Check if a value is a proper list (with Floyd's cycle detection for Pair chains).
fn value_is_list(val: &Value) -> bool {
    match val {
        Value::List(_) => true,
        Value::Pair(_) => {
            // Floyd's tortoise-and-hare cycle detection
            let mut slow = val.clone();
            let mut fast = val.clone();
            loop {
                // Advance fast by 2
                match value_cdr(&fast) {
                    Ok(f1) => match &f1 {
                        Value::List(_) => return true,
                        Value::Pair(_) => match value_cdr(&f1) {
                            Ok(f2) => fast = f2,
                            Err(_) => return false,
                        },
                        _ => return false,
                    },
                    Err(_) => return false,
                }
                // Advance slow by 1
                match value_cdr(&slow) {
                    Ok(s1) => slow = s1,
                    Err(_) => return false,
                }
                // Check if fast caught up with slow (cycle)
                match (&slow, &fast) {
                    (Value::List(_), _) => return true,
                    (Value::Pair(a), Value::Pair(b)) => {
                        if Rc::ptr_eq(a, b) {
                            return false; // cycle detected
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => false,
    }
}

/// Convert a value to a Vec, or return a type error.
fn require_list(val: &Value) -> Result<Vec<Value>, EvalError> {
    val.to_vec().ok_or_else(|| {
        ErrorKind::TypeMismatch {
            expected: "list".into(),
            got: val.to_display_string(),
        }
        .into()
    })
}

fn compare_nums(args: &[Value], cmp: impl Fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::WrongArgCount {
            expected: 2,
            got: args.len(),
        }
        .into());
    }
    for pair in args.windows(2) {
        let a = NumVal::from_value(&pair[0])?.to_f64();
        let b = NumVal::from_value(&pair[1])?.to_f64();
        if !cmp(a, b) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

// ── syntax-case support ─────────────────────────────────────

/// Convert a Value back to an Expr (for datum->syntax).
fn value_to_expr(val: &Value, span: Span) -> Expr {
    match val {
        Value::Integer(n) => Expr { kind: ExprKind::Integer(*n), span },
        Value::Float(f) => Expr { kind: ExprKind::Float(*f), span },
        Value::Rational(n, d) => Expr { kind: ExprKind::Rational(*n, *d), span },
        Value::Boolean(b) => Expr { kind: ExprKind::Boolean(*b), span },
        Value::Str(s, _) => Expr { kind: ExprKind::Str(s.borrow().clone()), span },
        Value::Char(c) => Expr { kind: ExprKind::Char(*c), span },
        Value::Symbol(s) => Expr { kind: ExprKind::Symbol(s.clone()), span },
        Value::List(elems) => {
            let exprs: Vec<Expr> = elems.iter().map(|e| value_to_expr(e, span)).collect();
            Expr { kind: ExprKind::List(exprs), span }
        }
        // For anything else, create a symbol placeholder
        _ => Expr { kind: ExprKind::Symbol(val.to_display_string()), span },
    }
}

/// (syntax-case expr (literals) clause ...)
fn step_syntax_case(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 3 {
        return Err(ErrorKind::BadSyntax {
            form: "syntax-case".into(),
            message: "expected (syntax-case expr (literals) clause ...)".into(),
        }.into());
    }
    // Parse literals
    let literals = match &args[1].kind {
        ExprKind::List(lits) => lits.iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::from(ErrorKind::BadSyntax {
                form: "syntax-case".into(),
                message: "literal must be a symbol".into(),
            })),
        }).collect::<Result<Vec<_>, _>>()?,
        _ => return Err(ErrorKind::BadSyntax {
            form: "syntax-case".into(),
            message: "expected literals list".into(),
        }.into()),
    };
    let clauses = args[2..].to_vec();
    // Evaluate the scrutinee expression
    k.push(Frame::SyntaxCaseMatch { literals, clauses, env: Rc::clone(env) });
    Ok(State::Eval(args[0].clone(), Rc::clone(env)))
}

/// After evaluating syntax-case scrutinee, match patterns.
fn step_ret_syntax_case(
    val: Value,
    literals: &[String],
    clauses: &[Expr],
    env: &Rc<Env>,
    _k: &mut Vec<Frame>,
) -> Result<State, EvalError> {
    let input_expr = match val {
        Value::SyntaxObject(e) => e,
        _ => return Err(ErrorKind::BadSyntax {
            form: "syntax-case".into(),
            message: format!("expected syntax object, got {}", val.to_display_string()),
        }.into()),
    };

    let input_elems = match &input_expr.kind {
        ExprKind::List(elems) => elems.as_slice(),
        _ => std::slice::from_ref(&input_expr),
    };

    for clause in clauses {
        let clause_elems = match &clause.kind {
            ExprKind::List(e) => e,
            _ => return Err(ErrorKind::BadSyntax {
                form: "syntax-case".into(),
                message: "clause must be a list".into(),
            }.into()),
        };
        if clause_elems.len() < 2 {
            return Err(ErrorKind::BadSyntax {
                form: "syntax-case".into(),
                message: "clause must have pattern and body".into(),
            }.into());
        }
        let pattern_elems = match &clause_elems[0].kind {
            ExprKind::List(p) => p.as_slice(),
            _ => std::slice::from_ref(&clause_elems[0]),
        };
        let body = &clause_elems[clause_elems.len() - 1];

        // Try matching
        if let Some(bindings) = macros::syntax_case_match(pattern_elems, input_elems, literals) {
            // Extend env with pattern bindings as SyntaxObjects
            let mut names = Vec::new();
            let mut values = Vec::new();
            for (name, binding) in &bindings {
                match binding {
                    macros::Binding::One(expr) => {
                        names.push(name.clone());
                        values.push(Value::SyntaxObject(expr.clone()));
                    }
                    macros::Binding::Many(exprs) => {
                        names.push(name.clone());
                        values.push(Value::List(
                            exprs.iter().map(|e| Value::SyntaxObject(e.clone())).collect(),
                        ));
                    }
                }
            }
            let new_env = Env::extend(env, names, values);
            return Ok(State::Eval(body.clone(), new_env));
        }
    }
    Err(ErrorKind::BadSyntax {
        form: "syntax-case".into(),
        message: "no matching pattern".into(),
    }.into())
}

/// (syntax-template template) — the #' form.
/// Substitutes pattern variables (SyntaxObjects in env) into the template.
fn step_syntax_template(args: &[Expr], env: &Rc<Env>) -> Result<State, EvalError> {
    if args.len() != 1 {
        return Err(ErrorKind::BadSyntax {
            form: "syntax".into(),
            message: "expected (syntax template)".into(),
        }.into());
    }
    let template = &args[0];

    // If template is a single symbol bound to a SyntaxObject, return it directly
    if let ExprKind::Symbol(ref name) = template.kind {
        if let Some(val) = env.get(name) {
            if let Value::SyntaxObject(_) = val { return Ok(State::Ret(val)) }
        }
    }

    // Build bindings map from env: collect symbols in template that are SyntaxObjects
    let syms = collect_syntax_symbols(template);
    let mut bindings = std::collections::HashMap::new();
    for sym in &syms {
        if let Some(val) = env.get(sym) {
            match val {
                Value::SyntaxObject(expr) => {
                    bindings.insert(sym.clone(), macros::Binding::One(expr));
                }
                Value::List(elems) if !elems.is_empty() && elems.iter().all(|e| matches!(e, Value::SyntaxObject(_))) => {
                    let exprs: Vec<Expr> = elems.iter().map(|e| match e {
                        Value::SyntaxObject(expr) => expr.clone(),
                        _ => unreachable!(),
                    }).collect();
                    bindings.insert(sym.clone(), macros::Binding::Many(exprs));
                }
                _ => {}
            }
        }
    }

    let expanded = macros::syntax_template_subst(template, &bindings, template.span);
    Ok(State::Ret(Value::SyntaxObject(expanded)))
}

/// Collect all symbol names used in a template expression.
fn collect_syntax_symbols(expr: &Expr) -> Vec<String> {
    let mut out = Vec::new();
    collect_syntax_symbols_inner(expr, &mut out);
    out
}

fn collect_syntax_symbols_inner(expr: &Expr, out: &mut Vec<String>) {
    match &expr.kind {
        ExprKind::Symbol(s) if s != "..." => {
            if !out.contains(s) {
                out.push(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_syntax_symbols_inner(e, out);
            }
        }
        _ => {}
    }
}

/// (with-syntax ((pattern expr) ...) body ...)
fn step_with_syntax(args: &[Expr], env: &Rc<Env>, k: &mut Vec<Frame>) -> Result<State, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "with-syntax".into(),
            message: "expected (with-syntax ((pat expr) ...) body ...)".into(),
        }.into());
    }
    let bindings_list = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(ErrorKind::BadSyntax {
            form: "with-syntax".into(),
            message: "expected bindings list".into(),
        }.into()),
    };
    let body = args[1..].to_vec();

    // Parse binding pairs
    let mut binding_pairs: Vec<(Expr, Expr)> = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                binding_pairs.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(ErrorKind::BadSyntax {
                form: "with-syntax".into(),
                message: "each binding must be (pattern expr)".into(),
            }.into()),
        }
    }

    if binding_pairs.is_empty() {
        // No bindings, just eval body
        return Ok(begin_seq(&body, env, k));
    }

    let first_pattern = binding_pairs[0].0.clone();
    let first_expr = binding_pairs[0].1.clone();
    let rest = binding_pairs[1..].to_vec();

    k.push(Frame::WithSyntaxBind {
        pattern: first_pattern,
        rest_bindings: rest,
        body,
        env: Rc::clone(env),
    });
    Ok(State::Eval(first_expr, Rc::clone(env)))
}

/// After evaluating a with-syntax binding expr, match and continue.
fn step_ret_with_syntax(
    val: Value,
    pattern: &Expr,
    rest_bindings: Vec<(Expr, Expr)>,
    body: Vec<Expr>,
    env: &Rc<Env>,
    k: &mut Vec<Frame>,
) -> Result<State, EvalError> {
    let input_expr = match &val {
        Value::SyntaxObject(e) => e.clone(),
        _ => return Err(ErrorKind::BadSyntax {
            form: "with-syntax".into(),
            message: format!("expected syntax object, got {}", val.to_display_string()),
        }.into()),
    };

    // Match pattern against the syntax object's expr
    let (pattern_slice, input_slice);
    let pattern_vec;
    let input_vec;
    match &pattern.kind {
        ExprKind::List(p) => {
            pattern_vec = p.clone();
            pattern_slice = pattern_vec.as_slice();
            match &input_expr.kind {
                ExprKind::List(i) => {
                    input_vec = i.clone();
                    input_slice = input_vec.as_slice();
                }
                _ => {
                    input_vec = vec![input_expr.clone()];
                    input_slice = input_vec.as_slice();
                }
            }
        }
        _ => {
            // Single symbol pattern — match against entire expr
            pattern_vec = vec![pattern.clone()];
            pattern_slice = pattern_vec.as_slice();
            input_vec = vec![input_expr];
            input_slice = input_vec.as_slice();
        }
    }

    if let Some(bindings) = macros::syntax_case_match(pattern_slice, input_slice, &[]) {
        let mut names = Vec::new();
        let mut values = Vec::new();
        for (name, binding) in &bindings {
            match binding {
                macros::Binding::One(expr) => {
                    names.push(name.clone());
                    values.push(Value::SyntaxObject(expr.clone()));
                }
                macros::Binding::Many(exprs) => {
                    names.push(name.clone());
                    values.push(Value::List(
                        exprs.iter().map(|e| Value::SyntaxObject(e.clone())).collect(),
                    ));
                }
            }
        }
        let new_env = Env::extend(env, names, values);

        if rest_bindings.is_empty() {
            Ok(begin_seq(&body, &new_env, k))
        } else {
            let next_pattern = rest_bindings[0].0.clone();
            let next_expr = rest_bindings[0].1.clone();
            let remaining = rest_bindings[1..].to_vec();
            k.push(Frame::WithSyntaxBind {
                pattern: next_pattern,
                rest_bindings: remaining,
                body,
                env: new_env.clone(),
            });
            Ok(State::Eval(next_expr, new_env))
        }
    } else {
        Err(ErrorKind::BadSyntax {
            form: "with-syntax".into(),
            message: "pattern match failed".into(),
        }.into())
    }
}
