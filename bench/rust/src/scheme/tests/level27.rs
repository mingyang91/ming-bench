use crate::scheme::eval_str;

// ===== Level 27: do loops =====

#[test]
fn test_l27_do_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_do_basic.scm").trim()),
        Ok("10".into())
    );
}

#[test]
fn test_l27_do_parallel_step() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_do_parallel_step.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_do_fibonacci() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_do_fibonacci.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_do_vector_fill() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_do_vector_fill.scm").trim()),
        Ok("#(0 1 4 9 16)".into())
    );
}

#[test]
fn test_l27_do_no_step() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_do_no_step.scm").trim()),
        Ok("#t".into())
    );
}
