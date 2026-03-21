use crate::scheme::eval_str;

// ===== Level 6: Mutable Strings (R5RS) =====
// Requirement-change level: string-set! is deprecated at L14 (strings become immutable).

fn current_level() -> u32 {
    std::env::var("BENCH_LEVEL")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(u32::MAX)
}

#[test]
fn test_l06_string_set() {
    if current_level() > 13 {
        return;
    } // deprecated at L14
    assert_eq!(
        eval_str(include_str!("fixtures/l06_string_set.scm").trim()),
        Ok("\"Horld\"".into())
    );
}

#[test]
fn test_l06_string_copy() {
    // string-copy is NOT deprecated — still valid after L14
    assert_eq!(
        eval_str(include_str!("fixtures/l06_string_copy.scm").trim()),
        Ok("\"hello\"".into())
    );
}

#[test]
fn test_l06_string_set_multiple() {
    if current_level() > 13 {
        return;
    } // deprecated at L14
    assert_eq!(
        eval_str(include_str!("fixtures/l06_string_set_multiple.scm").trim()),
        Ok("\"HELLO\"".into())
    );
}
