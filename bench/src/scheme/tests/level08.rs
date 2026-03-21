use crate::scheme::eval_str;

// ===== Level 8: Tail Call Optimization (All Forms) =====

#[test]
fn test_l08_tco_loop() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_loop.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l08_tco_fact_iter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_fact_iter.scm").trim()),
        Ok("2432902008176640000".into())
    );
}

#[test]
fn test_l08_tco_mutual_recursion() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_mutual_recursion.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l08_tco_cond() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_cond.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l08_tco_named_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_named_let.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l08_tco_and_or() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_and_or.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l08_tco_begin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_begin.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l08_tco_let_body() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_tco_let_body.scm").trim()),
        Ok("done".into())
    );
}
