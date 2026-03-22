use crate::scheme::eval_str;

// ===== Level 3: Lists, Recursion, Let/Begin/Cond & Predicates =====

#[test]
fn test_l03_cons() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_cons.scm").trim()),
        Ok("(1)".into())
    );
}

#[test]
fn test_l03_cons_chain() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_cons_chain.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l03_car() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_car.scm").trim()),
        Ok("1".into())
    );
}

#[test]
fn test_l03_cdr() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_cdr.scm").trim()),
        Ok("(2 3)".into())
    );
}

#[test]
fn test_l03_null_true() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_null_true.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_null_false() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_null_false.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l03_list() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_list.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l03_length() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_length.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l03_count() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_count.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l03_append() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_append.scm").trim()),
        Ok("(1 2 3 4)".into())
    );
}

#[test]
fn test_l03_reverse() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_reverse.scm").trim()),
        Ok("(3 2 1)".into())
    );
}

#[test]
fn test_l03_map() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_map.scm").trim()),
        Ok("(1 4 9 16)".into())
    );
}

#[test]
fn test_l03_filter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_filter.scm").trim()),
        Ok("(3 4 5)".into())
    );
}

#[test]
fn test_l03_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_let.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l03_nested_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_nested_let.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l03_begin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_begin.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l03_cond() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_cond.scm").trim()),
        Ok("20".into())
    );
}

#[test]
fn test_l03_cond_else() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_cond_else.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l03_begin_define() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_begin_define.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l03_string_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_string_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_number_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_number_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_boolean_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_boolean_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_pair_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_pair_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_symbol_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_symbol_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_stress_nqueens() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_stress_nqueens.scm").trim()),
        Ok("#t".into())
    );
}
