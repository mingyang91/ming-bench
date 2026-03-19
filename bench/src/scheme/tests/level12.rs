use crate::scheme::eval_str;

// ===== Level 12: Variadic & apply =====

#[test]
fn test_l12_rest_args() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_rest_args.scm").trim()),
        Ok("(2 3)".into())
    );
}

#[test]
fn test_l12_rest_args_empty() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_rest_args_empty.scm").trim()),
        Ok("()".into())
    );
}

#[test]
fn test_l12_apply_builtin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_apply_builtin.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l12_apply_prefix_args() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_apply_prefix_args.scm").trim()),
        Ok("10".into())
    );
}

#[test]
fn test_l12_apply_user_fn() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_apply_user_fn.scm").trim()),
        Ok("15".into())
    );
}

#[test]
fn test_l12_apply_as_value() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_apply_as_value.scm").trim()),
        Ok("6".into())
    );
}
