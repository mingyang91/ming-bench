use crate::scheme::eval_str;

// ===== Level 16: dynamic-wind =====

#[test]
fn test_l16_dynamic_wind_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_dynamic_wind_basic.scm").trim()),
        Ok("((in body out) 42)".into())
    );
}

#[test]
fn test_l16_dynamic_wind_nonlocal_exit() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_dynamic_wind_nonlocal_exit.scm").trim()),
        Ok("(in body out)".into())
    );
}

#[test]
fn test_l16_dynamic_wind_reentry() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_dynamic_wind_reentry.scm").trim()),
        Ok("(in body out in body out)".into())
    );
}

#[test]
fn test_l16_dynamic_wind_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_dynamic_wind_nested.scm").trim()),
        Ok("(outer-in inner-in inner-body inner-out outer-out)".into())
    );
}

#[test]
fn test_l16_dynamic_wind_exit_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_dynamic_wind_exit_nested.scm").trim()),
        Ok("(outer-in inner-in inner-out outer-out)".into())
    );
}

#[test]
fn test_l16_dynamic_wind_value() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_dynamic_wind_value.scm").trim()),
        Ok("42".into())
    );
}
