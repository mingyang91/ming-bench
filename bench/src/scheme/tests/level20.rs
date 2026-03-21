use crate::scheme::eval_str;

// ===== Level 20: define-record-type =====

#[test]
fn test_l20_record_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l20_record_basic.scm").trim()),
        Ok("(#t 3 4)".into())
    );
}

#[test]
fn test_l20_record_predicate() {
    assert_eq!(
        eval_str(include_str!("fixtures/l20_record_predicate.scm").trim()),
        Ok("(#t #f #f #f)".into())
    );
}

#[test]
fn test_l20_record_multiple() {
    assert_eq!(
        eval_str(include_str!("fixtures/l20_record_multiple.scm").trim()),
        Ok("(#t #f #t #f)".into())
    );
}

#[test]
fn test_l20_record_with_procedures() {
    assert_eq!(
        eval_str(include_str!("fixtures/l20_record_with_procedures.scm").trim()),
        Ok("(25 0 2)".into())
    );
}

#[test]
fn test_l20_record_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l20_record_nested.scm").trim()),
        Ok("(0 4)".into())
    );
}
