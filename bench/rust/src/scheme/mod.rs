pub mod error;
mod runtime;

pub use error::EvalError;
pub use runtime::{eval_str, eval_str_with_output};

#[cfg(test)]
mod tests;
