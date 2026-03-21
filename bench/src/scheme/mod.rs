pub mod error;
mod env;
mod eval;
mod parser;
mod value;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let ctx = eval::EvalContext::new();

    let mut i = 0;
    let mut last = value::Value::Void;

    while i < exprs.len() {
        ctx.current_expr_idx.set(i);
        let (expr, span) = &exprs[i];
        match eval::eval(expr, &env, *span, &ctx) {
            Ok(val) => {
                last = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let data = ctx
                    .cont_return_data
                    .borrow_mut()
                    .take()
                    .expect("ContinuationReturn without data");
                *ctx.callcc_override.borrow_mut() = Some(data.value);
                i = data.expr_idx;
            }
            Err(e) => return Err(e),
        }
    }

    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    let env = env::Env::new();
    let ctx = eval::EvalContext::new();

    let mut i = 0;
    let mut last = value::Value::Void;

    while i < exprs.len() {
        ctx.current_expr_idx.set(i);
        let (expr, span) = &exprs[i];
        match eval::eval(expr, &env, *span, &ctx) {
            Ok(val) => {
                last = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let data = ctx
                    .cont_return_data
                    .borrow_mut()
                    .take()
                    .expect("ContinuationReturn without data");
                *ctx.callcc_override.borrow_mut() = Some(data.value);
                i = data.expr_idx;
            }
            Err(e) => return Err(e),
        }
    }

    Ok((last.to_string(), ctx.output.into_inner()))
}

#[cfg(test)]
mod tests;
