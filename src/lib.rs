#![deny(warnings)]
#![deny(clippy::unwrap_used)]
// Pedantic lints: idiomatic Rust style, enforced for all code.
#![warn(clippy::too_many_lines)]
#![warn(clippy::excessive_nesting)]
#![warn(clippy::manual_filter_map)]
#![warn(clippy::manual_find_map)]
#![warn(clippy::manual_flatten)]
#![warn(clippy::manual_try_fold)]
#![warn(clippy::manual_let_else)]
#![warn(clippy::needless_range_loop)]
#![warn(clippy::explicit_counter_loop)]
#![warn(clippy::explicit_iter_loop)]
#![warn(clippy::vec_init_then_push)]
#![warn(clippy::needless_collect)]
#![warn(clippy::uninlined_format_args)]

pub mod scheme;
