use crate::scheme::eval_str;

// ===== Level 19: Exact Arithmetic & Rationals =====

#[test]
fn test_l19_exact_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_exact_basic.scm").trim()),
        Ok("(#t #t #t #f)".into())
    );
}

#[test]
fn test_l19_rational_arithmetic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_rational_arithmetic.scm").trim()),
        Ok("(1 1/2 1/2 2/3)".into())
    );
}

#[test]
fn test_l19_exact_division() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_exact_division.scm").trim()),
        Ok("(1/3 5/2 2)".into())
    );
}

#[test]
fn test_l19_exact_inexact_conversion() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_exact_inexact_conversion.scm").trim()),
        Ok("(0.3333333333333333 1/2 5.0)".into())
    );
}

#[test]
fn test_l19_rational_comparison() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_rational_comparison.scm").trim()),
        Ok("(#t #t #t)".into())
    );
}

#[test]
fn test_l19_numerator_denominator() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_numerator_denominator.scm").trim()),
        Ok("(3 4 5 1)".into())
    );
}

#[test]
fn test_l19_rational_simplify() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_rational_simplify.scm").trim()),
        Ok("(3/2 10 1/3)".into())
    );
}

#[test]
fn test_l19_exact_predicates() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_exact_predicates.scm").trim()),
        Ok("(#t #f #t #t)".into())
    );
}
