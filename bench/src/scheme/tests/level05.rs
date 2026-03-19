use crate::scheme::eval_str;

// ===== Level 5: Lambda & Closures =====

#[test]
fn test_l05_lambda_call() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_lambda_call.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l05_lambda_multi_param() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_lambda_multi_param.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l05_define_fn() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_define_fn.scm").trim()),
        Ok("25".into())
    );
}

#[test]
fn test_l05_closure() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_closure.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l05_higher_order() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_higher_order.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l05_factorial() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_factorial.scm").trim()),
        Ok("120".into())
    );
}

#[test]
fn test_l05_fibonacci() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_fibonacci.scm").trim()),
        Ok("55".into())
    );
}
