use crate::scheme::eval_str;

// ===== Level 13: Numeric/Char/String Utilities =====

#[test]
fn test_l13_abs() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_abs.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l13_modulo() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_modulo.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_remainder() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_remainder.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_quotient() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_quotient.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_min_max() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_min_max.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_expt() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_expt.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_zero() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_zero.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_positive_negative() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_positive_negative.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_odd_even() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_odd_even.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_combined() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_combined.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_list_ref() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_list_ref.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_list_tail() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_list_tail.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_list_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_list_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_assoc() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_assoc.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_map_multi() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_map_multi.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_char_alpha() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_char_alpha.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_char_numeric() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_char_numeric.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_char_case() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_char_case.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_char_compare() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_char_compare.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_char_literal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_char_literal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_string_equal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_string_equal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_string_less() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_string_less.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_string_ci() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_string_ci.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_string_upcase() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_string_upcase.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_string_downcase() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_string_downcase.scm").trim()),
        Ok("#t".into())
    );
}
