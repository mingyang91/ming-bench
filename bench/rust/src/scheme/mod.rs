pub mod error;
mod interpreter;

pub use error::EvalError;
pub use interpreter::{eval_str, eval_str_with_output};

#[cfg(test)]
mod tests;
