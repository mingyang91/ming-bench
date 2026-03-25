use crate::scheme::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Evaluator
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        match expr {
            Expr::Integer(n) => Ok(Value::Integer(*n)),
            Expr::Boolean(b) => Ok(Value::Boolean(*b)),
            Expr::Str(s) => Ok(Value::Str(s.clone())),
            Expr::Symbol(s) => Err(EvalError::UnboundVariable(s.clone())),
            Expr::List(elems) => {
                if elems.is_empty() {
                    return Ok(Value::List(vec![]));
                }
                self.eval_list(elems)
            }
        }
    }

    fn eval_list(&mut self, elems: &[Expr]) -> Result<Value, EvalError> {
        // Check for special forms first
        if let Expr::Symbol(name) = &elems[0] {
            match name.as_str() {
                "and" => return self.eval_and(&elems[1..]),
                "or" => return self.eval_or(&elems[1..]),
                "not" => {
                    if elems.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "not: expected 1 argument, got {}",
                            elems.len() - 1
                        )));
                    }
                    let val = self.eval(&elems[1])?;
                    return Ok(Value::Boolean(!val.is_truthy()));
                }
                _ => {}
            }
        }

        // Check if operator is a symbol (builtin)
        if let Expr::Symbol(name) = &elems[0] {
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| self.eval(e))
                .collect::<Result<Vec<_>, _>>()?;
            return self.apply_builtin(name, &args);
        }

        // Evaluate operator
        let op = self.eval(&elems[0])?;
        Err(EvalError::Type(format!("not a procedure: {}", op)))
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

    fn eval_and(&mut self, exprs: &[Expr]) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Boolean(true));
        }
        let mut result = Value::Boolean(true);
        for expr in exprs {
            result = self.eval(expr)?;
            if !result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, exprs: &[Expr]) -> Result<Value, EvalError> {
        if exprs.is_empty() {
            return Ok(Value::Boolean(false));
        }
        for expr in exprs {
            let result = self.eval(expr)?;
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
