use crate::scheme::eval_str;

// ===== Level 21: Pair Mutation & Cycle Detection =====

#[test]
fn test_l21_set_cdr() {
    assert_eq!(
        eval_str(include_str!("fixtures/l21_set_cdr.scm").trim()),
        Ok("(10 . 20)".into())
    );
}

#[test]
fn test_l21_shared_structure() {
    assert_eq!(
        eval_str(include_str!("fixtures/l21_shared_structure.scm").trim()),
        Ok("99".into())
    );
}

#[test]
fn test_l21_circular_list_detection() {
    assert_eq!(
        eval_str(include_str!("fixtures/l21_circular_list_detection.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l21_write_circular() {
    // Must terminate (not infinite-loop on circular structure)
    assert_eq!(
        eval_str(include_str!("fixtures/l21_write_circular.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l21_deep_recursion_no_leak() {
    assert_eq!(
        eval_str(include_str!("fixtures/l21_deep_recursion_no_leak.scm").trim()),
        Ok("done".into())
    );
}

#[test]
fn test_l21_stress_conform() {
    assert_eq!(
        eval_str(include_str!("fixtures/l21_stress_conform.scm").trim()),
        Ok("#t".into())
    );
}
