use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::scheme::EvalError;
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::{Env, LambdaData, MacroData, Value};

pub struct Evaluator {
    env: Env,
    output: String,
    gensym_counter: u64,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator { env: Env::new(), output: String::new(), gensym_counter: 0 }
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
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "modulo" | "remainder" | "quotient" | "abs" | "min" | "max" | "expt"
            | "list-ref" | "list-tail" | "list?" | "assoc" | "map"
            | "display" | "write" | "newline" | "char?"
            | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol" | "string-ref"
            | "string-copy" | "apply"
            | "string=?" | "string<?" | "string-ci=?"
            | "string-upcase" | "string-downcase"
            | "eq?" | "equal?" | "eqv?")
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
                    return Ok(v);
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
                "set!" => return self.eval_set(&elems[1..], env, call_pos),
                "define-syntax" => return self.eval_define_syntax(&elems[1..], env, call_pos),
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
                _ => {
                    if let Some(Value::Macro(macro_data)) = env.get(name) {
                        return self.expand_and_eval_macro(&macro_data, elems, env, call_pos);
                    }
                }
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
                self.call_lambda(&data, args, call_pos)
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

    fn eval_set(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("set!: expected 2 arguments at {pos}")));
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("set!: expected symbol at {pos}"))),
        };
        let val = self.eval_in_env(&args[1], env)?;
        if !env.set(&name, val) {
            return Err(EvalError::UnboundVariable(format!("{name} at {pos}")));
        }
        Ok(Value::Void)
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
                let (params, rest_param) = Self::parse_params(&name_and_params[1..], "define", pos)?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda(Rc::new(LambdaData {
                    params: params.clone(),
                    rest_param: rest_param.clone(),
                    body: body.clone(),
                    env: env.clone(),
                }));
                env.define(name.clone(), lambda);
                let recursive_lambda = Value::Lambda(Rc::new(LambdaData {
                    params,
                    rest_param,
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

    fn parse_params(param_exprs: &[Expr], context: &str, pos: &str) -> Result<(Vec<String>, Option<String>), EvalError> {
        let mut params = Vec::new();
        let mut rest_param = None;
        let mut i = 0;
        while i < param_exprs.len() {
            match &param_exprs[i].kind {
                ExprKind::Symbol(s) if s == "." => {
                    if i + 1 != param_exprs.len() - 1 {
                        return Err(EvalError::Parse(format!("{context}: expected exactly one parameter after '.' at {pos}")));
                    }
                    match &param_exprs[i + 1].kind {
                        ExprKind::Symbol(r) => rest_param = Some(r.clone()),
                        _ => return Err(EvalError::Parse(format!("{context}: expected symbol after '.' at {pos}"))),
                    }
                    break;
                }
                ExprKind::Symbol(s) => params.push(s.clone()),
                _ => return Err(EvalError::Parse(format!("{context}: expected symbol as parameter at {pos}"))),
            }
            i += 1;
        }
        Ok((params, rest_param))
    }

    fn eval_lambda(&self, args: &[Expr], env: &Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!("lambda: expected at least 2 arguments at {pos}")));
        }
        let (params, rest_param) = match &args[0].kind {
            ExprKind::List(param_exprs) => Self::parse_params(param_exprs, "lambda", pos)?,
            _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {pos}"))),
        };
        let body = args[1..].to_vec();
        Ok(Value::Lambda(Rc::new(LambdaData {
            params,
            rest_param,
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
            let loop_env = env.child();
            let lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                rest_param: None,
                body: body.clone(),
                env: loop_env.clone(),
            }));
            loop_env.define(name.clone(), lambda);
            let recursive_lambda = Value::Lambda(Rc::new(LambdaData {
                params: params.clone(),
                rest_param: None,
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
        let mut new_env = env.child();
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
        })?;
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

    fn call_lambda(&mut self, data: &LambdaData, args: Vec<Value>, pos: &str) -> Result<Value, EvalError> {
        if let Some(ref rest) = data.rest_param {
            if args.len() < data.params.len() {
                return Err(EvalError::Arity(format!(
                    "lambda: expected at least {} arguments, got {} at {pos}",
                    data.params.len(),
                    args.len()
                )));
            }
            let mut call_env = data.env.child();
            for (p, a) in data.params.iter().zip(args.iter()) {
                call_env.define(p.clone(), a.clone());
            }
            call_env.define(rest.clone(), Value::List(args[data.params.len()..].to_vec()));
            let mut result = Value::Void;
            for expr in &*data.body {
                result = self.eval_in_env(expr, &mut call_env)?;
            }
            Ok(result)
        } else {
            if data.params.len() != args.len() {
                return Err(EvalError::Arity(format!(
                    "lambda: expected {} arguments, got {} at {pos}",
                    data.params.len(),
                    args.len()
                )));
            }
            let mut call_env = data.env.child();
            for (p, a) in data.params.iter().zip(args.into_iter()) {
                call_env.define(p.clone(), a);
            }
            let mut result = Value::Void;
            for expr in &*data.body {
                result = self.eval_in_env(expr, &mut call_env)?;
            }
            Ok(result)
        }
    }

    fn call_proc(&mut self, proc: &Value, args: Vec<Value>, pos: &str) -> Result<Value, EvalError> {
        match proc {
            Value::Lambda(data) => self.call_lambda(data, args, pos),
            Value::Symbol(name) => self.apply_builtin(name, &args, pos),
            _ => Err(EvalError::Type(format!("apply: not a procedure: {} at {pos}", proc))),
        }
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
                        Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
                    }
                }
            }
            "car" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("car: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                    Value::Pair(a, _) => Ok(*a.clone()),
                    _ => Err(EvalError::Type(format!("car: expected non-empty list at {pos}"))),
                }
            }
            "cdr" => {
                if args.len() != 1 {
                    return Err(EvalError::Arity(format!("cdr: expected 1 argument at {pos}")));
                }
                match &args[0] {
                    Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                    Value::Pair(_, b) => Ok(*b.clone()),
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
                Ok(Value::Boolean(matches!(&args[0], Value::List(e) if !e.is_empty()) || matches!(&args[0], Value::Pair(_, _))))
            }
            "symbol?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("symbol?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
            }
            "zero?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("zero?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::Integer(0))))
            }
            "modulo" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("modulo: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], "modulo", pos)?;
                let b = self.expect_integer(&args[1], "modulo", pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                let r = a % b;
                let result = if r != 0 && ((r > 0) != (b > 0)) { r + b } else { r };
                Ok(Value::Integer(result))
            }
            "remainder" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("remainder: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], "remainder", pos)?;
                let b = self.expect_integer(&args[1], "remainder", pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::Integer(a % b))
            }
            "quotient" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("quotient: expected 2 arguments at {pos}"))); }
                let a = self.expect_integer(&args[0], "quotient", pos)?;
                let b = self.expect_integer(&args[1], "quotient", pos)?;
                if b == 0 { return Err(EvalError::DivisionByZero(format!("at {pos}"))); }
                Ok(Value::Integer(a / b))
            }
            "abs" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("abs: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "abs", pos)?;
                Ok(Value::Integer(n.abs()))
            }
            "min" => {
                if args.is_empty() { return Err(EvalError::Arity(format!("min: expected at least 1 argument at {pos}"))); }
                let mut result = self.expect_integer(&args[0], "min", pos)?;
                for arg in &args[1..] {
                    let n = self.expect_integer(arg, "min", pos)?;
                    if n < result { result = n; }
                }
                Ok(Value::Integer(result))
            }
            "max" => {
                if args.is_empty() { return Err(EvalError::Arity(format!("max: expected at least 1 argument at {pos}"))); }
                let mut result = self.expect_integer(&args[0], "max", pos)?;
                for arg in &args[1..] {
                    let n = self.expect_integer(arg, "max", pos)?;
                    if n > result { result = n; }
                }
                Ok(Value::Integer(result))
            }
            "expt" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("expt: expected 2 arguments at {pos}"))); }
                let base = self.expect_integer(&args[0], "expt", pos)?;
                let exp = self.expect_integer(&args[1], "expt", pos)?;
                Ok(Value::Integer(base.pow(exp as u32)))
            }
            "positive?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("positive?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "positive?", pos)?;
                Ok(Value::Boolean(n > 0))
            }
            "negative?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("negative?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "negative?", pos)?;
                Ok(Value::Boolean(n < 0))
            }
            "odd?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("odd?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "odd?", pos)?;
                Ok(Value::Boolean(n % 2 != 0))
            }
            "even?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("even?: expected 1 argument at {pos}"))); }
                let n = self.expect_integer(&args[0], "even?", pos)?;
                Ok(Value::Boolean(n % 2 == 0))
            }
            "list-ref" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("list-ref: expected 2 arguments at {pos}"))); }
                let lst = match &args[0] {
                    Value::List(e) => e,
                    _ => return Err(EvalError::Type(format!("list-ref: expected list at {pos}"))),
                };
                let idx = self.expect_integer(&args[1], "list-ref", pos)? as usize;
                lst.get(idx).cloned().ok_or_else(|| EvalError::Type(format!("list-ref: index out of bounds at {pos}")))
            }
            "list-tail" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("list-tail: expected 2 arguments at {pos}"))); }
                let lst = match &args[0] {
                    Value::List(e) => e,
                    _ => return Err(EvalError::Type(format!("list-tail: expected list at {pos}"))),
                };
                let idx = self.expect_integer(&args[1], "list-tail", pos)? as usize;
                Ok(Value::List(lst[idx..].to_vec()))
            }
            "list?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("list?: expected 1 argument at {pos}"))); }
                Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
            }
            "assoc" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("assoc: expected 2 arguments at {pos}"))); }
                let key = &args[0];
                let lst = match &args[1] {
                    Value::List(e) => e,
                    _ => return Err(EvalError::Type(format!("assoc: expected list at {pos}"))),
                };
                for item in lst {
                    match item {
                        Value::List(pair) if !pair.is_empty() => {
                            if pair[0] == *key {
                                return Ok(item.clone());
                            }
                        }
                        _ => {}
                    }
                }
                Ok(Value::Boolean(false))
            }
            "map" => {
                if args.len() < 2 { return Err(EvalError::Arity(format!("map: expected at least 2 arguments at {pos}"))); }
                let proc = args[0].clone();
                let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match a {
                    Value::List(e) => Ok(e),
                    _ => Err(EvalError::Type(format!("map: expected list at {pos}"))),
                }).collect::<Result<Vec<_>, _>>()?;
                let len = lists[0].len();
                let mut result = Vec::with_capacity(len);
                for i in 0..len {
                    let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                    result.push(self.call_proc(&proc, call_args, pos)?);
                }
                Ok(Value::List(result))
            }
            "char-alphabetic?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-alphabetic?: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                    _ => Err(EvalError::Type(format!("char-alphabetic?: expected char at {pos}"))),
                }
            }
            "char-numeric?" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-numeric?: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                    _ => Err(EvalError::Type(format!("char-numeric?: expected char at {pos}"))),
                }
            }
            "char-upcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-upcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                    _ => Err(EvalError::Type(format!("char-upcase: expected char at {pos}"))),
                }
            }
            "char-downcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("char-downcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                    _ => Err(EvalError::Type(format!("char-downcase: expected char at {pos}"))),
                }
            }
            "char=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("char=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                    _ => Err(EvalError::Type(format!("char=?: expected chars at {pos}"))),
                }
            }
            "char<?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("char<?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                    _ => Err(EvalError::Type(format!("char<?: expected chars at {pos}"))),
                }
            }
            "string=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                    _ => Err(EvalError::Type(format!("string=?: expected strings at {pos}"))),
                }
            }
            "string<?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string<?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                    _ => Err(EvalError::Type(format!("string<?: expected strings at {pos}"))),
                }
            }
            "string-ci=?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("string-ci=?: expected 2 arguments at {pos}"))); }
                match (&args[0], &args[1]) {
                    (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                    _ => Err(EvalError::Type(format!("string-ci=?: expected strings at {pos}"))),
                }
            }
            "string-upcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-upcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                    _ => Err(EvalError::Type(format!("string-upcase: expected string at {pos}"))),
                }
            }
            "string-downcase" => {
                if args.len() != 1 { return Err(EvalError::Arity(format!("string-downcase: expected 1 argument at {pos}"))); }
                match &args[0] {
                    Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                    _ => Err(EvalError::Type(format!("string-downcase: expected string at {pos}"))),
                }
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
            "apply" => {
                if args.len() < 2 {
                    return Err(EvalError::Arity(format!("apply: expected at least 2 arguments at {pos}")));
                }
                let proc = args[0].clone();
                let last = &args[args.len() - 1];
                let tail = match last {
                    Value::List(elems) => elems.clone(),
                    _ => return Err(EvalError::Type(format!("apply: last argument must be a list at {pos}"))),
                };
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                all_args.extend(tail);
                self.call_proc(&proc, all_args, pos)
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
            "eq?" | "eqv?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("{name}: expected 2 arguments at {pos}"))); }
                Ok(Value::Boolean(args[0] == args[1]))
            }
            "equal?" => {
                if args.len() != 2 { return Err(EvalError::Arity(format!("equal?: expected 2 arguments at {pos}"))); }
                Ok(Value::Boolean(args[0] == args[1]))
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

    // ── Macro support ──────────────────────────────────────────────

    fn gensym(&mut self, base: &str) -> String {
        self.gensym_counter += 1;
        format!("__{}_gs{}", base, self.gensym_counter)
    }

    fn is_special_form(name: &str) -> bool {
        matches!(name, "define" | "if" | "quote" | "lambda" | "let" | "begin"
            | "cond" | "and" | "or" | "set!" | "string-set!" | "not"
            | "define-syntax" | "syntax-rules")
    }

    fn eval_define_syntax(&mut self, args: &[Expr], env: &mut Env, pos: &str) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("define-syntax: expected 2 arguments at {pos}")));
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("define-syntax: expected symbol at {pos}"))),
        };
        let macro_data = self.parse_syntax_rules(&args[1], env, pos)?;
        env.define(name, Value::Macro(Rc::new(macro_data)));
        Ok(Value::Void)
    }

    fn parse_syntax_rules(&self, expr: &Expr, env: &Env, pos: &str) -> Result<MacroData, EvalError> {
        let elems = match &expr.kind {
            ExprKind::List(e) => e,
            _ => return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {pos}"))),
        };
        if elems.is_empty() || !matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
            return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {pos}")));
        }
        if elems.len() < 3 {
            return Err(EvalError::Parse(format!("syntax-rules: need literals and rules at {pos}")));
        }
        let literals = match &elems[1].kind {
            ExprKind::List(lits) => lits.iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("syntax-rules: expected symbol in literals at {pos}"))),
            }).collect::<Result<Vec<_>, _>>()?,
            _ => return Err(EvalError::Parse(format!("syntax-rules: expected literals list at {pos}"))),
        };
        let mut rules = Vec::new();
        for rule_expr in &elems[2..] {
            match &rule_expr.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    rules.push((pair[0].clone(), pair[1].clone()));
                }
                _ => return Err(EvalError::Parse(format!("syntax-rules: expected (pattern template) at {pos}"))),
            }
        }
        Ok(MacroData { literals, rules, def_env: env.clone() })
    }

    fn expand_and_eval_macro(
        &mut self,
        macro_data: &Rc<MacroData>,
        input: &[Expr],
        env: &mut Env,
        pos: &str,
    ) -> Result<Value, EvalError> {
        let md = Rc::clone(macro_data);
        for (pattern, template) in &md.rules {
            if let Some(bindings) = Self::match_pattern(pattern, input, &md.literals) {
                let pat_vars: HashSet<String> = bindings.keys().cloned().collect();
                let mut gensym_map: HashMap<String, String> = HashMap::new();
                let mut gensym_values: Vec<(String, Value)> = Vec::new();
                self.build_gensym_map(template, &pat_vars, &md.def_env, &mut gensym_map, &mut gensym_values);
                let expanded = Self::expand_template(template, &bindings, &gensym_map);
                let mut eval_env = env.child();
                for (gs, val) in gensym_values {
                    eval_env.define(gs, val);
                }
                return self.eval_in_env(&expanded, &mut eval_env);
            }
        }
        Err(EvalError::Parse(format!("no matching macro pattern at {pos}")))
    }

    fn match_pattern(
        pattern: &Expr,
        input: &[Expr],
        literals: &[String],
    ) -> Option<HashMap<String, Vec<Expr>>> {
        // pattern is (macro-name p1 p2 ...), input is [macro-name, arg1, arg2, ...]
        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => e,
            _ => return None,
        };
        let pat_tail = &pat_elems[1..];
        let inp_tail = &input[1..];
        let mut bindings: HashMap<String, Vec<Expr>> = HashMap::new();
        let mut pi = 0;
        let mut ii = 0;
        while pi < pat_tail.len() {
            let has_ellipsis = pi + 1 < pat_tail.len()
                && matches!(&pat_tail[pi + 1].kind, ExprKind::Symbol(s) if s == "...");
            if has_ellipsis {
                let var = match &pat_tail[pi].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return None,
                };
                let remaining_after = pat_tail.len() - pi - 2;
                let available = inp_tail.len().checked_sub(ii + remaining_after)?;
                bindings.insert(var, inp_tail[ii..ii + available].to_vec());
                ii += available;
                pi += 2;
            } else {
                if ii >= inp_tail.len() { return None; }
                match &pat_tail[pi].kind {
                    ExprKind::Symbol(s) if literals.contains(s) => {
                        match &inp_tail[ii].kind {
                            ExprKind::Symbol(is) if is == s => {}
                            _ => return None,
                        }
                    }
                    ExprKind::Symbol(s) => {
                        bindings.insert(s.clone(), vec![inp_tail[ii].clone()]);
                    }
                    _ => return None,
                }
                pi += 1;
                ii += 1;
            }
        }
        if ii != inp_tail.len() { return None; }
        Some(bindings)
    }

    fn build_gensym_map(
        &mut self,
        template: &Expr,
        pat_vars: &HashSet<String>,
        def_env: &Env,
        gensym_map: &mut HashMap<String, String>,
        gensym_values: &mut Vec<(String, Value)>,
    ) {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if pat_vars.contains(s) || s == "..." || Self::is_special_form(s)
                    || Self::is_builtin(s) || gensym_map.contains_key(s)
                {
                    return;
                }
                if let Some(val) = def_env.get(s) {
                    if matches!(val, Value::Macro(_)) { return; }
                    let gs = self.gensym(s);
                    gensym_values.push((gs.clone(), val));
                    gensym_map.insert(s.clone(), gs);
                } else {
                    let gs = self.gensym(s);
                    gensym_map.insert(s.clone(), gs);
                }
            }
            ExprKind::List(elems) => {
                for e in elems {
                    self.build_gensym_map(e, pat_vars, def_env, gensym_map, gensym_values);
                }
            }
            _ => {}
        }
    }

    fn expand_template(
        template: &Expr,
        bindings: &HashMap<String, Vec<Expr>>,
        gensym_map: &HashMap<String, String>,
    ) -> Expr {
        match &template.kind {
            ExprKind::Symbol(s) => {
                if let Some(exprs) = bindings.get(s) {
                    if exprs.len() == 1 {
                        return exprs[0].clone();
                    }
                }
                if let Some(gs) = gensym_map.get(s) {
                    return Expr { kind: ExprKind::Symbol(gs.clone()), line: 0, col: 0 };
                }
                template.clone()
            }
            ExprKind::List(elems) => {
                let mut expanded = Vec::new();
                let mut i = 0;
                while i < elems.len() {
                    let has_ellipsis = i + 1 < elems.len()
                        && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...");
                    if has_ellipsis {
                        if let ExprKind::Symbol(s) = &elems[i].kind {
                            if let Some(exprs) = bindings.get(s) {
                                for e in exprs {
                                    expanded.push(e.clone());
                                }
                            }
                        }
                        i += 2;
                    } else {
                        expanded.push(Self::expand_template(&elems[i], bindings, gensym_map));
                        i += 1;
                    }
                }
                Expr { kind: ExprKind::List(expanded), line: template.line, col: template.col }
            }
            _ => template.clone(),
        }
    }
}
