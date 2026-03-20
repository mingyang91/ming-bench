mod ast;
mod engine;
mod env;
pub mod error;
mod evaluator;
mod forms;
mod macros;
mod parser;
mod runtime;

pub use engine::eval_str;
pub use error::EvalError;

#[cfg(test)]
mod tests;
