use crate::scheme::eval_str;

// ===== Level 23: Final Integration =====

#[test]
fn test_l23_dynamic_wind_guard_combo() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_dynamic_wind_guard_combo.scm").trim()),
        Ok("(error \"oops\" (open work inner-open inner-close close))".into())
    );
}

#[test]
fn test_l23_values_with_callcc() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_values_with_callcc.scm").trim()),
        Ok("60".into())
    );
}

#[test]
fn test_l23_record_with_guard() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_record_with_guard.scm").trim()),
        Ok("(caught 404 \"not found\")".into())
    );
}

#[test]
fn test_l23_rational_in_data_structures() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_rational_in_data_structures.scm").trim()),
        Ok("(1 1/2 1)".into())
    );
}

#[test]
fn test_l23_macro_generates_record() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_macro_generates_record.scm").trim()),
        Ok("(0 0)".into())
    );
}

#[test]
fn test_l23_dynamic_wind_values() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_dynamic_wind_values.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l23_tail_call_with_guard() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_tail_call_with_guard.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l23_full_integration() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_full_integration.scm").trim()),
        Ok("(#t 10/3 #f \"division by zero\")".into())
    );
}

#[test]
fn test_l23_stress_browse() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_stress_browse.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l23_stress_peval() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_stress_peval.scm").trim()),
        Ok("#t".into())
    );
}
