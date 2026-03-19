(define (apply-twice f x) (f (f x))) (apply-twice (lambda (x) (+ x 1)) 0)
