use crate::scheme::eval_str;

// ===== Level 7: Recursive List Programs =====

#[test]
fn test_l07_count() {
    assert_eq!(
        eval_str(include_str!("fixtures/l07_count.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l07_append() {
    assert_eq!(
        eval_str(include_str!("fixtures/l07_append.scm").trim()),
        Ok("(1 2 3 4)".into())
    );
}

#[test]
fn test_l07_reverse() {
    assert_eq!(
        eval_str(include_str!("fixtures/l07_reverse.scm").trim()),
        Ok("(3 2 1)".into())
    );
}

#[test]
fn test_l07_map() {
    assert_eq!(
        eval_str(include_str!("fixtures/l07_map.scm").trim()),
        Ok("(1 4 9 16)".into())
    );
}

#[test]
fn test_l07_filter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l07_filter.scm").trim()),
        Ok("(3 4 5)".into())
    );
}
