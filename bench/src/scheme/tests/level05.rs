use crate::scheme::eval_str;
use crate::scheme::eval_str_with_output;

// ===== Level 5: Display, Write & String/Symbol Operations =====

#[test]
fn test_l05_display_number() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l05_display_number.scm").trim()).unwrap();
    assert_eq!(output, "42");
}

#[test]
fn test_l05_display_string() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l05_display_string.scm").trim()).unwrap();
    assert_eq!(output, "hello");
}

#[test]
fn test_l05_write_string() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l05_write_string.scm").trim()).unwrap();
    assert_eq!(output, "\"hello\"");
}

#[test]
fn test_l05_newline() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l05_newline.scm").trim()).unwrap();
    assert_eq!(output, "\n");
}

#[test]
fn test_l05_display_list() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l05_display_list.scm").trim()).unwrap();
    assert_eq!(output, "(1 2 3)");
}

#[test]
fn test_l05_multiple_display() {
    let (_, output) =
        eval_str_with_output(include_str!("fixtures/l05_multiple_display.scm").trim()).unwrap();
    assert_eq!(output, "123");
}

#[test]
fn test_l05_string_append() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_string_append.scm").trim()),
        Ok("\"hello world\"".into())
    );
}

#[test]
fn test_l05_string_length() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_string_length.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l05_substring() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_substring.scm").trim()),
        Ok("\"world\"".into())
    );
}

#[test]
fn test_l05_string_to_number() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_string_to_number.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l05_number_to_string() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_number_to_string.scm").trim()),
        Ok("\"42\"".into())
    );
}

#[test]
fn test_l05_symbol_string_roundtrip() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_symbol_string_roundtrip.scm").trim()),
        Ok("hello".into())
    );
}

#[test]
fn test_l05_string_ref_and_char() {
    assert_eq!(
        eval_str(include_str!("fixtures/l05_string_ref_and_char.scm").trim()),
        Ok("#t".into())
    );
}
