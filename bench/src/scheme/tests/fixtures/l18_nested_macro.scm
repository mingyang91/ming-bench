(define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...) #f))))
(define x 0)
(when #t (set! x 1) (set! x (+ x 1)))
x
