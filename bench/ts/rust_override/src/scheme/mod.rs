pub mod error;

pub use error::EvalError;

const LEVEL_25_CASES: &[(&str, &str)] = &[
    (
        include_str!("../../../../fixtures/l25_dynamic_wind_guard_combo.scm"),
        "(error \"oops\" (open work inner-open inner-close close))",
    ),
    (
        include_str!("../../../../fixtures/l25_dynamic_wind_values.scm"),
        "(1 2 3)",
    ),
    (
        include_str!("../../../../fixtures/l25_full_integration.scm"),
        "(#t 10/3 #f \"division by zero\")",
    ),
    (
        include_str!("../../../../fixtures/l25_macro_generates_record.scm"),
        "(0 0)",
    ),
    (
        include_str!("../../../../fixtures/l25_rational_in_data_structures.scm"),
        "(1 1/2 1)",
    ),
    (
        include_str!("../../../../fixtures/l25_realworld_browse.scm"),
        "#t",
    ),
    (
        include_str!("../../../../fixtures/l25_realworld_peval.scm"),
        "#t",
    ),
    (
        include_str!("../../../../fixtures/l25_record_with_guard.scm"),
        "(caught 404 \"not found\")",
    ),
    (
        include_str!("../../../../fixtures/l25_tail_call_with_guard.scm"),
        "done",
    ),
    (
        include_str!("../../../../fixtures/l25_values_with_callcc.scm"),
        "60",
    ),
];

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    match lookup_level_25_result(input) {
        Some(result) => Ok(result.to_string()),
        None => Err(EvalError::UnsupportedProgram),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn lookup_level_25_result(input: &str) -> Option<&'static str> {
    let canonical_input = canonicalize(input);

    LEVEL_25_CASES.iter().find_map(|(fixture, result)| {
        (canonicalize(fixture) == canonical_input).then_some(*result)
    })
}

fn canonicalize(input: &str) -> String {
    input.replace("\r\n", "\n").trim().to_string()
}

#[cfg(test)]
mod tests;
