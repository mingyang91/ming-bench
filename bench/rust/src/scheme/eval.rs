use crate::scheme::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::{Env, Value};

pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator { env: Env::new() }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        // We need to work with self.env directly but can't pass &mut self.env
        // while also passing &mut self. So we swap it out temporarily.
        let mut env = std::mem::replace(&mut self.env, Env::new());
        let result = self.eval_in_env(expr, &mut env);
        self.env = env;
        result
    }

    fn is_builtin(name: &str) -> bool {
        matches!(name, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
            | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "append" | "number?" | "boolean?" | "string?" | "pair?" | "symbol?"
            | "zero?" | "modulo" | "remainder" | "abs")
    }

    fn eval_in_env(&mut self, expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
        match expr {
            Expr::Integer(n) => Ok(Value::Integer(*n)),
            Expr::Boolean(b) => Ok(Value::Boolean(*b)),
            Expr::Str(s) => Ok(Value::Str(s.clone())),
            Expr::Symbol(s) => {
                if let Some(v) = env.get(s) {
                    return Ok(v.clone());
                }
                if Self::is_builtin(s) {
                    return Ok(Value::Symbol(s.clone()));
                }
                Err(EvalError::UnboundVariable(s.clone()))
            }
            Expr::List(elems) => {
                if elems.is_empty() {
                    return Ok(Value::List(vec![]));
                }
                self.eval_list(elems, env)
            }
        }
    }

    fn eval_list(&mut self, elems: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if let Expr::Symbol(name) = &elems[0] {
            match name.as_str() {
                "define" => return self.eval_define(&elems[1..], env),
                "if" => return self.eval_if(&elems[1..], env),
                "quote" => return self.eval_quote(&elems[1..]),
                "lambda" => return self.eval_lambda(&elems[1..], env),
                "let" => return self.eval_let(&elems[1..], env),
                "begin" => return self.eval_begin(&elems[1..], env),
                "cond" => return self.eval_cond(&elems[1..], env),
                "and" => return self.eval_and(&elems[1..], env),
                "or" => return self.eval_or(&elems[1..], env),
                "not" => {
                    if elems.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "not: expected 1 argument, got {}",
                            elems.len() - 1
                        )));
                    }
                    let val = self.eval_in_env(&elems[1], env)?;
                    return Ok(Value::Boolean(!val.is_truthy()));
                }
                _ => {}
            }
        }

        // Evaluate operator
        let op = self.eval_in_env(&elems[0], env)?;

        // Evaluate arguments
        let args: Vec<Value> = elems[1..]
            .iter()
            .map(|e| self.eval_in_env(e, env))
            .collect::<Result<Vec<_>, _>>()?;

        match &op {
            Value::Lambda { params, body, env: closure_env } => {
                if params.len() != args.len() {
                    return Err(EvalError::Arity(format!(
                        "lambda: expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                // Start from closure env, overlay calling env's defs
                let mut call_env = closure_env.clone();
                call_env.merge_all(env);
                call_env.push_frame();
                for (p, a) in params.iter().zip(args.into_iter()) {
                    call_env.define(p.clone(), a);
                }
                let mut result = Value::Void;
                for expr in body {
                    result = self.eval_in_env(expr, &mut call_env)?;
                }
                // Propagate any defines back to the calling env
                // (needed for internal defines that are used after lambda returns)
                Ok(result)
            }
            Value::Symbol(name) => self.apply_builtin(name, &args),
            _ => {
                // Try as builtin if it was a symbol in the source
                if let Expr::Symbol(name) = &elems[0] {
                    return self.apply_builtin(name, &args);
                }
                Err(EvalError::Type(format!("not a procedure: {}", op)))
            }
        }
    }

    fn eval_define(&mut self, args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse("define: missing arguments".into()));
        }
        match &args[0] {
            // (define x expr)
            Expr::Symbol(name) => {
                if args.len() != 2 {
                    return Err(EvalError::Arity("define: expected 2 arguments".into()));
                }
                let val = self.eval_in_env(&args[1], env)?;
                env.define(name.clone(), val);
                Ok(Value::Void)
            }
            // (define (f params...) body...)
            Expr::List(name_and_params) => {
                if name_and_params.is_empty() {
                    return Err(EvalError::Parse("define: empty name list".into()));
                }
                let name = match &name_and_params[0] {
                    Expr::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("define: expected symbol as name".into())),
                };
                let params: Vec<String> = name_and_params[1..]
                    .iter()
                    .map(|e| match e {
                        Expr::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Parse("define: expected symbol as parameter".into())),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda {
                    params: params.clone(),
                    body: body.clone(),
                    env: env.clone(),
                };
                // Define first, then update closure env to include itself for recursion
                env.define(name.clone(), lambda);
                // Re-create with updated env so recursive calls find the function
                let recursive_lambda = Value::Lambda {
                    params,
                    body,
                    env: env.clone(),
                };
                env.define(name, recursive_lambda);
                Ok(Value::Void)
            }
            _ => Err(EvalError::Parse("define: expected symbol or list".into())),
        }
    }

    fn eval_if(&mut self, args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::Arity("if: expected 2 or 3 arguments".into()));
        }
        let cond = self.eval_in_env(&args[0], env)?;
        if cond.is_truthy() {
            self.eval_in_env(&args[1], env)
        } else if args.len() == 3 {
            self.eval_in_env(&args[2], env)
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_quote(&self, args: &[Expr]) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity("quote: expected 1 argument".into()));
        }
        Ok(Self::expr_to_value(&args[0]))
    }

    fn expr_to_value(expr: &Expr) -> Value {
        match expr {
            Expr::Integer(n) => Value::Integer(*n),
            Expr::Boolean(b) => Value::Boolean(*b),
            Expr::Str(s) => Value::Str(s.clone()),
            Expr::Symbol(s) => Value::Symbol(s.clone()),
            Expr::List(elems) => {
                Value::List(elems.iter().map(Self::expr_to_value).collect())
            }
        }
    }

    fn eval_lambda(&self, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity("lambda: expected at least 2 arguments".into()));
        }
        let params = match &args[0] {
            Expr::List(param_exprs) => {
                param_exprs
                    .iter()
                    .map(|e| match e {
                        Expr::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Parse("lambda: expected symbol as parameter".into())),
                    })
                    .collect::<Result<Vec<_>, _>>()?
            }
            _ => return Err(EvalError::Parse("lambda: expected parameter list".into())),
        };
        let body = args[1..].to_vec();
        Ok(Value::Lambda {
            params,
            body,
            env: env.clone(),
        })
    }

    fn eval_let(&mut self, args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse("let: missing arguments".into()));
        }
        // Named let: (let name ((var init) ...) body...)
        if let Expr::Symbol(name) = &args[0] {
            if args.len() < 3 {
                return Err(EvalError::Parse("named let: missing bindings or body".into()));
            }
            let bindings = match &args[1] {
                Expr::List(b) => b,
                _ => return Err(EvalError::Parse("named let: expected bindings list".into())),
            };
            let mut params = Vec::new();
            let mut init_vals = Vec::new();
            for binding in bindings {
                match binding {
                    Expr::List(pair) if pair.len() == 2 => {
                        if let Expr::Symbol(var) = &pair[0] {
                            params.push(var.clone());
                            init_vals.push(self.eval_in_env(&pair[1], env)?);
                        } else {
                            return Err(EvalError::Parse("let: expected symbol in binding".into()));
                        }
                    }
                    _ => return Err(EvalError::Parse("let: expected (var expr) binding".into())),
                }
            }
            let body = args[2..].to_vec();
            // Create recursive lambda for the named let
            let mut loop_env = env.clone();
            let lambda = Value::Lambda {
                params: params.clone(),
                body: body.clone(),
                env: loop_env.clone(),
            };
            loop_env.define(name.clone(), lambda);
            let recursive_lambda = Value::Lambda {
                params: params.clone(),
                body: body.clone(),
                env: loop_env.clone(),
            };
            loop_env.define(name.clone(), recursive_lambda);
            // Call with initial values
            let mut call_env = loop_env;
            call_env.push_frame();
            for (p, v) in params.iter().zip(init_vals.into_iter()) {
                call_env.define(p.clone(), v);
            }
            let mut result = Value::Void;
            for expr in &body {
                result = self.eval_in_env(expr, &mut call_env)?;
            }
            return Ok(result);
        }
        // Regular let: (let ((var expr) ...) body...)
        let bindings = match &args[0] {
            Expr::List(b) => b,
            _ => return Err(EvalError::Parse("let: expected bindings list".into())),
        };
        let mut new_env = env.clone();
        new_env.push_frame();
        for binding in bindings {
            match binding {
                Expr::List(pair) if pair.len() == 2 => {
                    if let Expr::Symbol(var) = &pair[0] {
                        let val = self.eval_in_env(&pair[1], env)?;
                        new_env.define(var.clone(), val);
                    } else {
                        return Err(EvalError::Parse("let: expected symbol in binding".into()));
                    }
                }
                _ => return Err(EvalError::Parse("let: expected (var expr) binding".into())),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = self.eval_in_env(expr, &mut new_env)?;
        }
        Ok(result)
    }

    fn eval_begin(&mut self, args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expr in args {
            result = self.eval_in_env(expr, env)?;
        }
        Ok(result)
    }

    fn eval_cond(&mut self, clauses: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        for clause in clauses {
            match clause {
                Expr::List(parts) if !parts.is_empty() => {
                    // Check for else clause
                    if let Expr::Symbol(s) = &parts[0] {
                        if s == "else" {
                            let mut result = Value::Void;
                            for expr in &parts[1..] {
                                result = self.eval_in_env(expr, env)?;
                            }
                            return Ok(result);
                        }
                    }
                    let test = self.eval_in_env(&parts[0], env)?;
                    if test.is_truthy() {
                        let mut result = test;
                        for expr in &parts[1..] {
                            result = self.eval_in_env(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                _ => return Err(EvalError::Parse("cond: expected clause".into())),
            }
        }
        Ok(Value::Void)
    }

    fn apply_builtin(&mut self, name: &str, args: &[Value]) -> Result<Value, EvalError> {
        match name {
            "+" => {
                let mut sum: i64 = 0;
                for arg in args {
                    sum += self.expect_integer(arg, "+")?;
                }
                Ok(Value::Integer(sum))
            }
            "-" => {
                if args.is_empty() {
                    return Err(EvalError::Arity("-: expected at least 1 argument".into()));
                }
                if args.len() == 1 {
                    let n = self.expect_integer(&args[0], "-")?;
                    return Ok(Value::Integer(-n));
                }
                let mut result = self.expect_integer(&args[0], "-")?;
                for arg in &args[1..] {
                    result -= self.expect_integer(arg, "-")?;
                }
                Ok(Value::Integer(result))
            }
            "*" => {
                let mut product: i64 = 1;
                for arg in args {
                    product *= self.expect_integer(arg, "*")?;
                }
                Ok(Value::Integer(product))
            }
            "/" => {
                if args.is_empty() {
                    return Err(EvalError::Arity("/: expected at least 1 argument".into()));
                }
                let mut result = self.expect_integer(&args[0], "/")?;
                for arg in &args[1..] {
                    let divisor = self.expect_integer(arg, "/")?;
                    if divisor == 0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    result /= divisor;
                }
                Ok(Value::Integer(result))
            }
            "<" => self.compare_numbers(args, "<", |a, b| a < b),
            ">" => self.compare_numbers(args, ">", |a, b| a > b),
            "=" => self.compare_numbers(args, "=", |a, b| a == b),
            "<=" => self.compare_numbers(args, "<=", |a, b| a <= b),
            ">=" => self.compare_numbers(args, ">=", |a, b| a >= b),
            "cons" => {
                if args.len() != 2 {
                    return Err(EvalError::Arity("cons: expected 2 arguments".into()));
                }
                match &args[1] {
                    Value::List(tail) => {
                        let mut new_list = vec![args[0].clone()];
                        new_list.extend(tail.iter().cloned());
                        Ok(Value::List(new_list))
                    }
                    _ => {
                        // Dotted pair - for now represent as list
                        Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
                    }
                }
            }
            "car" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity("car: expected 1 argument".into()));
                }
                match &args[0] {
                    Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                    _ => Err(EvalError::Type("car: expected non-empty list".into())),
                }
            }
            "cdr" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity("cdr: expected 1 argument".into()));
                }
                match &args[0] {
                    Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                    _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
                }
            }
            "null?" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity("null?: expected 1 argument".into()));
                }
                Ok(Value::Boolean(matches!(&args[0], Value::List(e) if e.is_empty())))
            }
            "list" => Ok(Value::List(args.to_vec())),
            "length" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity("length: expected 1 argument".into()));
                }
                match &args[0] {
                    Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                    _ => Err(EvalError::Type("length: expected list".into())),
                }
            }
            "append" => {
                let mut result = Vec::new();
                for arg in args {
                    match arg {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
                Ok(Value::List(result))
            }
            "number?" => {
                if args.len() != 1 { return Err(EvalError::Arity("number?: expected 1 argument".into())); }
                Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
            }
            "boolean?" => {
                if args.len() != 1 { return Err(EvalError::Arity("boolean?: expected 1 argument".into())); }
                Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
            }
            "string?" => {
                if args.len() != 1 { return Err(EvalError::Arity("string?: expected 1 argument".into())); }
                Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
            }
            "pair?" => {
                if args.len() != 1 { return Err(EvalError::Arity("pair?: expected 1 argument".into())); }
                Ok(Value::Boolean(matches!(&args[0], Value::List(e) if !e.is_empty())))
            }
            "symbol?" => {
                if args.len() != 1 { return Err(EvalError::Arity("symbol?: expected 1 argument".into())); }
                Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
            }
            "zero?" => {
                if args.len() != 1 { return Err(EvalError::Arity("zero?: expected 1 argument".into())); }
                Ok(Value::Boolean(matches!(&args[0], Value::Integer(0))))
            }
            "modulo" | "remainder" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("{name}: expected 2 arguments"))); }
                let a = self.expect_integer(&args[0], name)?;
                let b = self.expect_integer(&args[1], name)?;
                if b == 0 { return Err(EvalError::DivisionByZero); }
                Ok(Value::Integer(a % b))
            }
            "abs" => {
                if args.len() != 1 { return Err(EvalError::Arity("abs: expected 1 argument".into())); }
                let n = self.expect_integer(&args[0], "abs")?;
                Ok(Value::Integer(n.abs()))
            }
            _ => Err(EvalError::UnboundVariable(name.into())),
        }
    }

    fn compare_numbers(
        &self,
        args: &[Value],
        name: &str,
        cmp: fn(i64, i64) -> bool,
    ) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!(
                "{name}: expected at least 2 arguments"
            )));
        }
        let mut prev = self.expect_integer(&args[0], name)?;
        for arg in &args[1..] {
            let curr = self.expect_integer(arg, name)?;
            if !cmp(prev, curr) {
                return Ok(Value::Boolean(false));
            }
            prev = curr;
        }
        Ok(Value::Boolean(true))
    }

    fn eval_and(&mut self, exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Boolean(true));
        }
        let mut result = Value::Boolean(true);
        for expr in exprs {
            result = self.eval_in_env(expr, env)?;
            if !result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Boolean(false));
        }
        for expr in exprs {
            let result = self.eval_in_env(expr, env)?;
            if result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(Value::Boolean(false))
    }

    fn expect_integer(&self, val: &Value, context: &str) -> Result<i64, EvalError> {
        match val {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!(
                "{context}: expected number, got {val}"
            ))),
        }
    }
}
