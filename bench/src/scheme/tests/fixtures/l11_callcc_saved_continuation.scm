(define saved #f)
(define (get-cont) (call/cc (lambda (k) (set! saved k) 10)))
(define val (get-cont))
(if (= val 10) (saved 42) val)
