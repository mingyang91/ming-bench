use crate::scheme::eval_str;

// ===== Level 14: Deep Equality, Letrec, Case & Vectors =====

#[test]
fn test_l14_equal_numbers() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_equal_numbers.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l14_equal_strings() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_equal_strings.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l14_equal_nested_lists() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_equal_nested_lists.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l14_equal_different_types() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_equal_different_types.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l14_letrec_simple() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_letrec_simple.scm").trim()),
        Ok("120".into())
    );
}

#[test]
fn test_l14_letrec_mutual() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_letrec_mutual.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l14_letrec_star() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_letrec_star.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l14_letrec_shadow() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_letrec_shadow.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l14_case_symbol() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_case_symbol.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l14_case_number() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_case_number.scm").trim()),
        Ok("\"two\"".into())
    );
}

#[test]
fn test_l14_case_else() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_case_else.scm").trim()),
        Ok("0".into())
    );
}

#[test]
fn test_l14_case_no_match() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_case_no_match.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l14_vector_create_ref() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_vector_create_ref.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l14_vector_set() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_vector_set.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l14_vector_predicate() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_vector_predicate.scm").trim()),
        Ok("(#t #f 3)".into())
    );
}

#[test]
fn test_l14_vector_conversion() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_vector_conversion.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l14_vector_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_vector_nested.scm").trim()),
        Ok("3".into())
    );
}
