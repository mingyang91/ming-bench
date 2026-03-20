use crate::scheme::eval_str;

// ===== Level 23: Recursive Local Bindings =====

#[test]
fn test_l23_letrec_simple() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_letrec_simple.scm").trim()),
        Ok("120".into())
    );
}

#[test]
fn test_l23_letrec_mutual() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_letrec_mutual.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l23_letrec_star() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_letrec_star.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l23_letrec_shadow() {
    assert_eq!(
        eval_str(include_str!("fixtures/l23_letrec_shadow.scm").trim()),
        Ok("42".into())
    );
}
