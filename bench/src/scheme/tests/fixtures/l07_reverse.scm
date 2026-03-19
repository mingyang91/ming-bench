(define (reverse lst) (define (rev-iter l acc) (if (null? l) acc (rev-iter (cdr l) (cons (car l) acc)))) (rev-iter lst '()))
(reverse '(1 2 3))
