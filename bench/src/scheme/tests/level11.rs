use crate::scheme::eval_str_with_output;

// ===== Level 11: Display, Write, Newline =====

#[test]
fn test_l11_display_number() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l11_display_number.scm").trim()).unwrap();
    assert_eq!(output, "42");
}

#[test]
fn test_l11_display_string() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l11_display_string.scm").trim()).unwrap();
    assert_eq!(output, "hello");
}

#[test]
fn test_l11_write_string() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l11_write_string.scm").trim()).unwrap();
    assert_eq!(output, "\"hello\"");
}

#[test]
fn test_l11_newline() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l11_newline.scm").trim()).unwrap();
    assert_eq!(output, "\n");
}

#[test]
fn test_l11_display_list() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l11_display_list.scm").trim()).unwrap();
    assert_eq!(output, "(1 2 3)");
}

#[test]
fn test_l11_multiple_display() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l11_multiple_display.scm").trim()).unwrap();
    assert_eq!(output, "123");
}
