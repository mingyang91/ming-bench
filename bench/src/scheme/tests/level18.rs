use crate::scheme::eval_str;

// ===== Level 18: Tail Position in All Forms =====

#[test]
fn test_l18_tco_cond() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_tco_cond.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l18_tco_named_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_tco_named_let.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l18_tco_and_or() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_tco_and_or.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l18_tco_begin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_tco_begin.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l18_tco_let_body() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_tco_let_body.scm").trim()),
        Ok("done".into())
    );
}
