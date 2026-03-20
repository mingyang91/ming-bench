use crate::scheme::eval_str;

// ===== Level 26: Numeric Utilities =====

#[test]
fn test_l26_abs() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_abs.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l26_modulo() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_modulo.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_remainder() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_remainder.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_quotient() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_quotient.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_min_max() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_min_max.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_expt() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_expt.scm").trim()),
        Ok("#t".into())
    );
}
