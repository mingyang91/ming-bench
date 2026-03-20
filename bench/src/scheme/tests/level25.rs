use crate::scheme::eval_str;

// ===== Level 25: Vectors =====

#[test]
fn test_l25_vector_create_ref() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_vector_create_ref.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l25_vector_set() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_vector_set.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l25_vector_predicate() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_vector_predicate.scm").trim()),
        Ok("(#t #f 3)".into())
    );
}

#[test]
fn test_l25_vector_conversion() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_vector_conversion.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l25_vector_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_vector_nested.scm").trim()),
        Ok("3".into())
    );
}
