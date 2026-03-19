(define saved #f)
(define first? #t)
(define result ((lambda () (call/cc (lambda (k) (set! saved k) 1)) (if first? (begin (set! first? #f) 2) 3))))
(if (= result 2) (saved 99) result)
