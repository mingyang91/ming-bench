use crate::scheme::eval_str;

// ===== Level 13: Tail Call Optimization =====

#[test]
fn test_l13_tco_loop() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_tco_loop.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l13_tco_fact_iter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_tco_fact_iter.scm").trim()),
        Ok("2432902008176640000".into())
    );
}

#[test]
fn test_l13_tco_mutual_recursion() {
    assert_eq!(
        eval_str(include_str!("fixtures/l13_tco_mutual_recursion.scm").trim()),
        Ok("#t".into())
    );
}
