use crate::scheme::eval_str;

// ===== Level 12: Variadic & apply =====

#[test]
fn test_l12_rest_args() {
    assert_eq!(
        eval_str("(define (f x . rest) rest) (f 1 2 3)"),
        Ok("(2 3)".into())
    );
}

#[test]
fn test_l12_rest_args_empty() {
    assert_eq!(
        eval_str("(define (f . all) all) (f)"),
        Ok("()".into())
    );
}

#[test]
fn test_l12_apply_builtin() {
    assert_eq!(eval_str("(apply + '(1 2 3))"), Ok("6".into()));
}

#[test]
fn test_l12_apply_prefix_args() {
    assert_eq!(eval_str("(apply + 1 2 '(3 4))"), Ok("10".into()));
}

#[test]
fn test_l12_apply_user_fn() {
    assert_eq!(
        eval_str(
            "(define (sum . args) (if (null? args) 0 (+ (car args) (apply sum (cdr args))))) (sum 1 2 3 4 5)"
        ),
        Ok("15".into())
    );
}

#[test]
fn test_l12_apply_as_value() {
    assert_eq!(
        eval_str("(define f apply) (f + '(1 2 3))"),
        Ok("6".into())
    );
}
