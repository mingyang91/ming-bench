(define (call-with-escape f) (call/cc (lambda (k) (f k))))
(+ 1 (call-with-escape (lambda (exit) (exit 10) 999)))
