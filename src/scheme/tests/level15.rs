use crate::scheme::eval_str;

// ===== Level 15: define-syntax / syntax-rules =====

#[test]
fn test_l15_simple_macro() {
    assert_eq!(
        eval_str(
            "(define-syntax my-if (syntax-rules () ((my-if c t f) (if c t f)))) (my-if #t 1 2)"
        ),
        Ok("1".into())
    );
}

#[test]
fn test_l15_my_and_macro() {
    assert_eq!(
        eval_str(
            "(define-syntax my-and (syntax-rules () ((my-and) #t) ((my-and x) x) ((my-and x rest ...) (if x (my-and rest ...) #f)))) (my-and 1 2 3)"
        ),
        Ok("3".into())
    );
}

#[test]
fn test_l15_swap_hygiene() {
    assert_eq!(
        eval_str(
            "(define-syntax swap! (syntax-rules () ((swap! a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (define x 1) (define y 2) (swap! x y) (list x y)"
        ),
        Ok("(2 1)".into())
    );
}

#[test]
fn test_l15_variadic_pattern() {
    assert_eq!(
        eval_str(
            "(define-syntax my-list (syntax-rules () ((my-list) '()) ((my-list x rest ...) (cons x (my-list rest ...))))) (my-list 1 2 3)"
        ),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l15_nested_macro() {
    assert_eq!(
        eval_str(
            "(define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...) #f)))) (define x 0) (when #t (set! x 1) (set! x (+ x 1))) x"
        ),
        Ok("2".into())
    );
}

#[test]
fn test_l15_macro_keeps_definition_site_binding() {
    assert_eq!(
        eval_str(
            "(define x 10) (define-syntax get-x (syntax-rules () ((get-x) x))) (let ((x 20)) (get-x))"
        ),
        Ok("10".into())
    );
}
