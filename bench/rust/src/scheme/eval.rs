use std::rc::Rc;
use crate::scheme::EvalError;
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::{Env, LambdaData, Value};

pub struct Evaluator {
    env: Env,
    output: String,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator { env: Env::new(), output: String::new() }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        let mut env = std::mem::replace(&mut self.env, Env::new());
        let result = self.eval_in_env(expr, &mut env);
        self.env = env;
        result
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    fn is_builtin(name: &str) -> bool {
        matches!(name, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
            | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "append" | "number?" | "boolean?" | "string?" | "pair?" | "symbol?"
            | "zero?" | "modulo" | "remainder" | "abs"
            | "display" | "write" | "newline" | "char?"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol" | "string-ref"
            | "string-copy")
    }

    fn eval_in_env(&mut self, expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
        let pos = expr.pos_str();
        match &expr.kind {
            ExprKind::Integer(n) => Ok(Value::Integer(*n)),
            ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            ExprKind::Char(c) => Ok(Value::Char(*c)),
            ExprKind::Str(s) => Ok(Value::Str(s.clone())),
            ExprKind::Symbol(s) => {
                if let Some(v) = env.get(s) {
                    return Ok(v.clone());
                }
                if Self::is_builtin(s) {
                    return Ok(Value::Symbol(s.clone()));
                }
                Err(EvalError::UnboundVariable(format!("{s} at {pos}")))
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Ok(Value::List(vec![]));
                }
                self.eval_list(elems, env, &pos)
            }
        }
    }

    fn eval_list(&mut self, elems: &[Expr], env: &mut Env, call_pos: &str) -> Result<Value, EvalError> {
        if let ExprKind::Symbol(name) = &elems[0].kind {
            match name.as_str() {
                "define" => return self.eval_define(&elems[1..], env, call_pos),
                "if" => return self.eval_if(&elems[1..], env, call_pos),
                "quote" => return self.eval_quote(&elems[1..], call_pos),
                "lambda" => return self.eval_lambda(&elems[1..], env, call_pos),
                "let" => return self.eval_let(&elems[1..], env, call_pos),
                "begin" => return self.eval_begin(&elems[1..], env),
                "cond" => return self.eval_cond(&elems[1..], env, call_pos),
                "and" => return self.eval_and(&elems[1..], env),
                "or" => return self.eval_or(&elems[1..], env),
                "string-set!" => return self.eval_string_set(&elems[1..], env, call_pos),
                "not" => {
                    if elems.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "not: expected 1 argument, got {} at {call_pos}",
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
            Value::Lambda(data) => {
                if data.params.len() != args.len() {
                    return Err(EvalError::Arity(format!(
                        "lambda: expected {} arguments, got {} at {call_pos}",
                        data.params.len(),
                        args.len()
                    )));
                }
                let mut call_env = data.env.clone();
                call_env.merge_all(env);
                call_env.push_frame();
                for (p, a) in data.params.iter().zip(args.into_iter()) {
                    call_env.define(p.clone(), a);
                }
                let mut result = Value::Void;
                for expr in &*data.body {
                    result = self.eval_in_env(expr, &mut call_env)?;
                }
                Ok(result)
            }
            Value::Symbol(name) => self.apply_builtin(name, &args, call_pos),
            _ => {
                if let ExprKind::Symbol(name) = &elems[0].kind {
                    return self.apply_builtin(name, &args, call_pos);
                }
                Err(EvalError::Type(format!("not a procedure: {} at {call_pos}", op)))
            }
        }
    }

    fn eval_define(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("define: missing arguments at {pos}")));
        }
        match &args[0].kind {
            ExprKind::Symbol(name) => {
                if args.len() != 2 {
                    return Err(EvalError::Arity(format!("define: expected 2 arguments at {pos}")));
                }
                let val = self.eval_in_env(&args[1], env)?;
                env.define(name.clone(), val);
                Ok(Value::Void)
            }
            ExprKind::List(name_and_params) => {
                if name_and_params.is_empty() {
                    return Err(EvalError::Parse(format!("define: empty name list at {pos}")));
                }
                let name = match &name_and_params[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define: expected symbol as name at {pos}"))),
                };
                let params: Vec<String> = name_and_params[1..]
                    .iter()
                    .map(|e| match &e.kind {
                        ExprKind::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Parse(format!("define: expected symbol as parameter at {pos}"))),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda(Rc::new(LambdaData {
                    params: params.clone(),
                    body: body.clone(),
                    env: env.clone(),
                }));
                env.define(name.clone(), lambda);
                let recursive_lambda = Value::Lambda(Rc::new(LambdaData {
                    params,
                    body,
                    env: env.clone(),
                }));
                env.define(name, recursive_lambda);
                Ok(Value::Void)
            }
            _ => Err(EvalError::Parse(format!("define: expected symbol or list at {pos}"))),
        }
    }

    fn eval_if(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {pos}")));
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

    fn eval_quote(&self, args: &[Expr], pos: &str) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("quote: expected 1 argument at {pos}")));
        }
        Ok(Self::expr_to_value(&args[0]))
    }

    fn expr_to_value(expr: &Expr) -> Value {
        match &expr.kind {
            ExprKind::Integer(n) => Value::Integer(*n),
            ExprKind::Boolean(b) => Value::Boolean(*b),
            ExprKind::Char(c) => Value::Char(*c),
            ExprKind::Str(s) => Value::Str(s.clone()),
            ExprKind::Symbol(s) => Value::Symbol(s.clone()),
            ExprKind::List(elems) => {
                Value::List(elems.iter().map(Self::expr_to_value).collect())
            }
        }
    }

    fn eval_lambda(&self, args: &[Expr], env: &Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!("lambda: expected at least 2 arguments at {pos}")));
        }
        let params = match &args[0].kind {
            ExprKind::List(param_exprs) => {
                param_exprs
                    .iter()
                    .map(|e| match &e.kind {
                        ExprKind::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Parse(format!("lambda: expected symbol as parameter at {pos}"))),
                    })
                    .collect::<Result<Vec<_>, _>>()?
            }
            _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {pos}"))),
        };
        let body = args[1..].to_vec();
        Ok(Value::Lambda(Rc::new(LambdaData {
            params,
            body,
            env: env.clone(),
        })))
    }

    fn eval_let(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Parse(format!("let: missing arguments at {pos}")));
        }
        // Named let: (let name ((var init) ...) body...)
        if let ExprKind::Symbol(name) = &args[0].kind {
            if args.len() < 3 {
                return Err(EvalError::Parse(format!("named let: missing bindings or body at {pos}")));
            }
            let bindings = match &args[1].kind {
                ExprKind::List(b) => b,
                _ => return Err(EvalError::Parse(format!("named let: expected bindings list at {pos}"))),
            };
            let mut params = Vec::new();
            let mut init_vals = Vec::new();
            for binding in bindings {
                match &binding.kind {
                    ExprKind::List(pair) if pair.len() == 2 => {
                        if let ExprKind::Symbol(var) = &pair[0].kind {
                            params.push(var.clone());
                            init_vals.push(self.eval_in_env(&pair[1], env)?);
                        } else {
                            return Err(EvalError::Parse(format!("let: expected symbol in binding at {pos}")));
                        }
                    }
                    _ => return Err(EvalError::Parse(format!("let: expected (var expr) binding at {pos}"))),
                }
            }
            let body = args[2..].to_vec();
            let mut loop_env = env.clone();
            let lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                body: body.clone(),
                env: loop_env.clone(),
            }));
            loop_env.define(name.clone(), lambda);
            let recursive_lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                body: body.clone(),
                env: loop_env.clone(),
            }));
            loop_env.define(name.clone(), recursive_lambda);
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
        // Regular let
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {pos}"))),
        };
        let mut new_env = env.clone();
        new_env.push_frame();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(var) = &pair[0].kind {
                        let val = self.eval_in_env(&pair[1], env)?;
                        new_env.define(var.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected symbol in binding at {pos}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: expected (var expr) binding at {pos}"))),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = self.eval_in_env(expr, &mut new_env)?;
        }
        Ok(result)
    }

    fn eval_string_set(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 3 {
            return Err(EvalError::Arity(format!("string-set!: expected 3 arguments at {pos}")));
        }
        let var_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type(format!("string-set!: expected variable at {pos}"))),
        };
        let idx = self.eval_in_env(&args[1], env)?;
        let idx = self.expect_integer(&idx, "string-set!", pos)? as usize;
        let ch = self.eval_in_env(&args[2], env)?;
        let ch = match ch {
            Value::Char(c) => c,
            _ => return Err(EvalError::Type(format!("string-set!: expected char at {pos}"))),
        };
        let s = env.get(&var_name).ok_or_else(|| {
            EvalError::UnboundVariable(format!("{var_name} at {pos}"))
        })?.clone();
        match s {
            Value::Str(mut string) => {
                let mut chars: Vec<char> = string.chars().collect();
                chars[idx] = ch;
                string = chars.into_iter().collect();
                env.set(&var_name, Value::Str(string));
                Ok(Value::Void)
            }
            _ => Err(EvalError::Type(format!("string-set!: expected string at {pos}"))),
        }
    }

    fn eval_begin(&mut self, args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expr in args {
            result = self.eval_in_env(expr, env)?;
        }
        Ok(result)
    }

    fn eval_cond(&mut self, clauses: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        for clause in clauses {
            match &clause.kind {
                ExprKind::List(parts) if !parts.is_empty() => {
                    if let ExprKind::Symbol(s) = &parts[0].kind {
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
                _ => return Err(EvalError::Parse(format!("cond: expected clause at {pos}"))),
            }
        }
        Ok(Value::Void)
    }

    fn apply_builtin(&mut self, name: &str, args: &[Value], pos: &str) -> Result<Value, EvalError> {
        match name {
            "+" => {
                let mut sum: i64 = 0;
                for arg in args {
                    sum += self.expect_integer(arg, "+", pos)?;
                }
                Ok(Value::Integer(sum))
            }
            "-" => {
                if args.is_empty() {
                    return Err(EvalError::Arity(format!("-: expected at least 1 argument at {pos}")));
                }
                if args.len() == 1 {
                    let n = self.expect_integer(&args[0], "-", pos)?;
                    return Ok(Value::Integer(-n));
                }
                let mut result = self.expect_integer(&args[0], "-", pos)?;
                for arg in &args[1..] {
                    result -= self.expect_integer(arg, "-", pos)?;
                }
                Ok(Value::Integer(result))
            }
            "*" => {
                let mut product: i64 = 1;
                for arg in args {
                    product *= self.expect_integer(arg, "*", pos)?;
                }
                Ok(Value::Integer(product))
            }
            "/" => {
                if args.is_empty() {
                    return Err(EvalError::Arity(format!("/: expected at least 1 argument at {pos}")));
                }
                let mut result = self.expect_integer(&args[0], "/", pos)?;
                for arg in &args[1..] {
                    let divisor = self.expect_integer(arg, "/", pos)?;
                    if divisor == 0 {
                        return Err(EvalError::DivisionByZero(format!("at {pos}")));
                    }
                    result /= divisor;
                }
                Ok(Value::Integer(result))
            }
            "<" => self.compare_numbers(args, "<", pos, |a, b| a < b),
            ">" => self.compare_numbers(args, ">", pos, |a, b| a > b),
            "=" => self.compare_numbers(args, "=", pos, |a, b| a == b),
            "<=" => self.compare_numbers(args, "<=", pos, |a, b| a <= b),
            ">=" => self.compare_numbers(args, ">=", pos, |a, b| a >= b),
            "cons" => {
                if args.len() != 2 {
                    return Err(EvalError::Arity(format!("cons: expected 2 arguments at {pos}")));
                }
                match &args[1] {
                    Value::List(tail) => {
                        let mut new_list = vec![args[0].clone()];
                        new_list.extend(tail.iter().cloned());
                        Ok(Value::List(new_list))
                    }
                    _ => {
                        Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
                    }
                }
            }
            "car" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("car: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                    _ => Err(EvalError::Type(format!("car: expected non-empty list at {pos}"))),
                }
            }
            "cdr" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("cdr: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                    _ => Err(EvalError::Type(format!("cdr: expected non-empty list at {pos}"))),
                }
            }
            "null?" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("null?: expected 1 argument at {pos}")));
                }
                Ok(Value::Boolean(matches!(&args[0], Value::List(e) if e.is_empty())))
            }
            "list" => Ok(Value::List(args.to_vec())),
            "length" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("length: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                    _ => Err(EvalError::Type(format!("length: expected list at {pos}"))),
                }
            }
            "append" => {
                let mut result = Vec::new();
                for arg in args {
                    match arg {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => return Err(EvalError::Type(format!("append: expected list at {pos}"))),
                    }
                }
                Ok(Value::List(result))
            }
            "number?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("number?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
            }
            "boolean?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("boolean?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
            }
            "string?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
            }
            "pair?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("pair?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::List(e) if !e.is_empty())))
            }
            "symbol?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("symbol?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
            }
            "zero?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("zero?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Integer(0))))
            }
            "modulo" | "remainder" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("{name}: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], name, pos)?;
                let b = self.expect_integer(&args[1], name, pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::Integer(a % b))
            }
            "abs" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("abs: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "abs", pos)?;
                Ok(Value::Integer(n.abs()))
            }
            "display" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("display: expected 1 argument at {pos}"))); }
                self.output.push_str(&args[0].to_scheme_display());
                Ok(Value::Void)
            }
            "write" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("write: expected 1 argument at {pos}"))); }
                self.output.push_str(&args[0].to_write_string());
                Ok(Value::Void)
            }
            "newline" => {
                if !args.is_empty() { return Err(EvalError::Arity(format!("newline: expected 0 arguments at {pos}"))); }
                self.output.push('\n');
                Ok(Value::Void)
            }
            "char?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
            }
            "string-append" => {
                let mut result = String::new();
                for arg in args {
                    match arg {
                        Value::Str(s) => result.push_str(s),
                        _ => return Err(EvalError::Type(format!("string-append: expected string at {pos}"))),
                    }
                }
                Ok(Value::Str(result))
            }
            "string-length" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-length: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                    _ => Err(EvalError::Type(format!("string-length: expected string at {pos}"))),
                }
            }
            "substring" => {
                if args.len() != 3 { return Err(EvalError::Arity(format!("substring: expected 3 arguments at {pos}"))); }
                let s = match &args[0] {
                    Value::Str(s) => s,
                    _ => return Err(EvalError::Type(format!("substring: expected string at {pos}"))),
                };
                let start = self.expect_integer(&args[1], "substring", pos)? as usize;
                let end = self.expect_integer(&args[2], "substring", pos)? as usize;
                Ok(Value::Str(s[start..end].to_string()))
            }
            "string->number" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string->number: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s) => match s.parse::<i64>() {
                        Ok(n) => Ok(Value::Integer(n)),
                        Err(_) => Ok(Value::Boolean(false)),
                    },
                    _ => Err(EvalError::Type(format!("string->number: expected string at {pos}"))),
                }
            }
            "number->string" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("number->string: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "number->string", pos)?;
                Ok(Value::Str(n.to_string()))
            }
            "symbol->string" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("symbol->string: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Symbol(s) => Ok(Value::Str(s.clone())),
                    _ => Err(EvalError::Type(format!("symbol->string: expected symbol at {pos}"))),
                }
            }
            "string->symbol" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string->symbol: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s) => Ok(Value::Symbol(s.clone())),
                    _ => Err(EvalError::Type(format!("string->symbol: expected string at {pos}"))),
                }
            }
            "string-copy" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-copy: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s) => Ok(Value::Str(s.clone())),
                    _ => Err(EvalError::Type(format!("string-copy: expected string at {pos}"))),
                }
            }
            "string-ref" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string-ref: expected 2 arguments at {pos}"))); }
                let s = match &args[0] {
                    Value::Str(s) => s,
                    _ => return Err(EvalError::Type(format!("string-ref: expected string at {pos}"))),
                };
                let idx = self.expect_integer(&args[1], "string-ref", pos)? as usize;
                Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                    EvalError::Type(format!("string-ref: index out of bounds at {pos}"))
                })?))
            }
            _ => Err(EvalError::UnboundVariable(format!("{name} at {pos}"))),
        }
    }

    fn compare_numbers(
        &self,
        args: &[Value],
        name: &str,
        pos: &str,
        cmp: fn(i64, i64) -> bool,
    ) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!(
                "{name}: expected at least 2 arguments at {pos}"
            )));
        }
        let mut prev = self.expect_integer(&args[0], name, pos)?;
        for arg in &args[1..] {
            let curr = self.expect_integer(arg, name, pos)?;
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

    fn expect_integer(&self, val: &Value, context: &str, pos: &str) -> Result<i64, EvalError> {
        match val {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!(
                "{context}: expected number, got {val} at {pos}"
            ))),
        }
    }
}
