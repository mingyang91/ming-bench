#[path = "evaluator/runtime.rs"]
mod runtime;

pub(crate) use runtime::{apply_callable, eval_program, eval_program_with_output};
