pub mod error;

pub use error::EvalError;

mod parser;
mod value;
mod env;
mod eval;
mod macros;

use env::Env;
use value::ValueKind;

const EVAL_STACK_SIZE: usize = 256 * 1024 * 1024; // 256 MB

fn run_with_stack<F, T>(f: F) -> T
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    std::thread::Builder::new()
        .stack_size(EVAL_STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn eval thread")
        .join()
        .expect("eval thread panicked")
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let input = input.to_string();
    run_with_stack(move || {
        let exprs = parser::parse(&input)?;
        let env = Env::default_env();
        let mut result = value::Value::unpos(ValueKind::Void);
        for expr in exprs {
            result = eval::eval(&expr, &env)?;
        }
        Ok(result.to_display())
    })
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let input = input.to_string();
    run_with_stack(move || {
        let exprs = parser::parse(&input)?;
        let env = Env::default_env();
        let (result, output) = eval::with_output_capture(|| {
            let mut result = value::Value::unpos(ValueKind::Void);
            for expr in exprs {
                result = eval::eval(&expr, &env)?;
            }
            Ok(result.to_display())
        });
        Ok((result?, output))
    })
}

#[cfg(test)]
mod tests;
