use crate::scheme::eval_str;

// ===== Level 16: Tail Position in All Forms =====

#[test]
fn test_l16_tco_cond() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_tco_cond.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l16_tco_named_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_tco_named_let.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l16_tco_and_or() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_tco_and_or.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l16_tco_begin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_tco_begin.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l16_tco_let_body() {
    assert_eq!(
        eval_str(include_str!("fixtures/l16_tco_let_body.scm").trim()),
        Ok("done".into())
    );
}
