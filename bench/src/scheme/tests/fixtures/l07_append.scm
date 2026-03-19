(define (my-append a b) (if (null? a) b (cons (car a) (my-append (cdr a) b))))
(my-append '(1 2) '(3 4))
