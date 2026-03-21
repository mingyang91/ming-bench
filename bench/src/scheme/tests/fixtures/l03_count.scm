(define (count lst) (if (null? lst) 0 (+ 1 (count (cdr lst))))) (count '(a b c))
