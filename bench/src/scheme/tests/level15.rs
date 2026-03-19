use crate::scheme::eval_str;

// ===== Level 15: define-syntax / syntax-rules =====

#[test]
fn test_l15_simple_macro() {
    assert_eq!(
        eval_str(include_str!("fixtures/l15_simple_macro.scm").trim()),
        Ok("1".into())
    );
}

#[test]
fn test_l15_my_and_macro() {
    assert_eq!(
        eval_str(include_str!("fixtures/l15_my_and_macro.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l15_swap_hygiene() {
    assert_eq!(
        eval_str(include_str!("fixtures/l15_swap_hygiene.scm").trim()),
        Ok("(2 1)".into())
    );
}

#[test]
fn test_l15_variadic_pattern() {
    assert_eq!(
        eval_str(include_str!("fixtures/l15_variadic_pattern.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l15_nested_macro() {
    assert_eq!(
        eval_str(include_str!("fixtures/l15_nested_macro.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l15_macro_keeps_definition_site_binding() {
    assert_eq!(
        eval_str(include_str!("fixtures/l15_macro_keeps_definition_site_binding.scm").trim()),
        Ok("10".into())
    );
}
