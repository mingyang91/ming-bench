// Test functions are auto-generated from bench/tests.json by build.rs.
// Do NOT add test functions here manually — edit tests.json instead.
include!(concat!(env!("OUT_DIR"), "/tests_generated.rs"));

// Level 27 (concurrent eval) uses native threading — not fixture-based.
mod level27;
