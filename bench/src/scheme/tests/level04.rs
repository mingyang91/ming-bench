use crate::scheme::eval_str;

// ===== Level 4: Define & If =====

#[test]
fn test_l04_if_true() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_if_true.scm").trim()),
        Ok("1".into())
    );
}

#[test]
fn test_l04_if_false() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_if_false.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l04_if_expr() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_if_expr.scm").trim()),
        Ok("10".into())
    );
}

#[test]
fn test_l04_define_var() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_define_var.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l04_define_use() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_define_use.scm").trim()),
        Ok("4".into())
    );
}

#[test]
fn test_l04_define_multi() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_define_multi.scm").trim()),
        Ok("30".into())
    );
}

#[test]
fn test_l04_quote() {
    assert_eq!(
        eval_str(include_str!("fixtures/l04_quote.scm").trim()),
        Ok("(1 2 3)".into())
    );
}
