use crate::scheme::eval_str;

// ===== Level 2: Variables, Conditionals & Lambda =====

#[test]
fn test_l02_if_true() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_if_true.scm").trim()),
        Ok("1".into())
    );
}

#[test]
fn test_l02_if_false() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_if_false.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l02_if_expr() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_if_expr.scm").trim()),
        Ok("10".into())
    );
}

#[test]
fn test_l02_define_var() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_define_var.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l02_define_use() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_define_use.scm").trim()),
        Ok("4".into())
    );
}

#[test]
fn test_l02_define_multi() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_define_multi.scm").trim()),
        Ok("30".into())
    );
}

#[test]
fn test_l02_quote() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_quote.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l02_lambda_call() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_lambda_call.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l02_lambda_multi_param() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_lambda_multi_param.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l02_define_fn() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_define_fn.scm").trim()),
        Ok("25".into())
    );
}

#[test]
fn test_l02_closure() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_closure.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l02_higher_order() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_higher_order.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l02_factorial() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_factorial.scm").trim()),
        Ok("120".into())
    );
}

#[test]
fn test_l02_fibonacci() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_fibonacci.scm").trim()),
        Ok("55".into())
    );
}
