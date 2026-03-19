use crate::scheme::eval_str;

// ===== Level 7: Recursive List Programs =====

#[test]
fn test_l07_count() {
    assert_eq!(
        eval_str(
            "(define (count lst) (if (null? lst) 0 (+ 1 (count (cdr lst))))) (count '(a b c))"
        ),
        Ok("3".into())
    );
}

#[test]
fn test_l07_append() {
    assert_eq!(
        eval_str(
            "(define (my-append a b) (if (null? a) b (cons (car a) (my-append (cdr a) b)))) (my-append '(1 2) '(3 4))"
        ),
        Ok("(1 2 3 4)".into())
    );
}

#[test]
fn test_l07_reverse() {
    assert_eq!(
        eval_str(
            "(define (reverse lst) (define (rev-iter l acc) (if (null? l) acc (rev-iter (cdr l) (cons (car l) acc)))) (rev-iter lst '())) (reverse '(1 2 3))"
        ),
        Ok("(3 2 1)".into())
    );
}

#[test]
fn test_l07_map() {
    assert_eq!(
        eval_str(
            "(define (map f lst) (if (null? lst) '() (cons (f (car lst)) (map f (cdr lst))))) (map (lambda (x) (* x x)) '(1 2 3 4))"
        ),
        Ok("(1 4 9 16)".into())
    );
}

#[test]
fn test_l07_filter() {
    assert_eq!(
        eval_str(
            "(define (filter pred lst) (if (null? lst) '() (if (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) (filter pred (cdr lst))))) (filter (lambda (x) (> x 2)) '(1 2 3 4 5))"
        ),
        Ok("(3 4 5)".into())
    );
}
