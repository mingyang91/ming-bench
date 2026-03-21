(define saved #f)
(define first? #t)
(define result ((lambda (a b) (+ b 1)) 1 (call/cc (lambda (k) (set! saved k) 2))))
(if first? (begin (set! first? #f) (saved 42)) result)
