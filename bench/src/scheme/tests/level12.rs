use crate::scheme::eval_str;

// ===== Level 12: String & Symbol Operations =====

#[test]
fn test_l12_string_append() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_string_append.scm").trim()),
        Ok("\"hello world\"".into())
    );
}

#[test]
fn test_l12_string_length() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_string_length.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l12_substring() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_substring.scm").trim()),
        Ok("\"world\"".into())
    );
}

#[test]
fn test_l12_string_to_number() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_string_to_number.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l12_number_to_string() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_number_to_string.scm").trim()),
        Ok("\"42\"".into())
    );
}

#[test]
fn test_l12_symbol_string_roundtrip() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_symbol_string_roundtrip.scm").trim()),
        Ok("hello".into())
    );
}

#[test]
fn test_l12_string_ref_and_char() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_string_ref_and_char.scm").trim()),
        Ok("#t".into())
    );
}
