use crate::scheme::eval_str;

// ===== Level 2: Arithmetic =====

#[test]
fn test_l02_add() {
    assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
}

#[test]
fn test_l02_sub() {
    assert_eq!(eval_str("(- 10 3)"), Ok("7".into()));
}

#[test]
fn test_l02_mul() {
    assert_eq!(eval_str("(* 4 5)"), Ok("20".into()));
}

#[test]
fn test_l02_div() {
    assert_eq!(eval_str("(/ 10 2)"), Ok("5".into()));
}

#[test]
fn test_l02_variadic_add() {
    assert_eq!(eval_str("(+ 1 2 3 4)"), Ok("10".into()));
}

#[test]
fn test_l02_unary_minus() {
    assert_eq!(eval_str("(- 10)"), Ok("-10".into()));
}

#[test]
fn test_l02_nested_arith() {
    assert_eq!(eval_str("(+ (* 2 3) (- 10 4))"), Ok("12".into()));
}
