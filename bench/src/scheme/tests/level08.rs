use crate::scheme::eval_str;

// ===== Level 8: Let, Begin, Cond =====

#[test]
fn test_l08_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_let.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l08_nested_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_nested_let.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l08_begin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_begin.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l08_cond() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_cond.scm").trim()),
        Ok("20".into())
    );
}

#[test]
fn test_l08_cond_else() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_cond_else.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l08_begin_define() {
    assert_eq!(
        eval_str(include_str!("fixtures/l08_begin_define.scm").trim()),
        Ok("2".into())
    );
}
