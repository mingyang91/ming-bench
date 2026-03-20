(define-syntax while (syntax-rules () ((while test body ...) (let loop () (when test body ... (loop))))))
(define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...) #f))))
(define n 1000000)
(define i 0)
(while (< i n) (set! i (+ i 1)))
i
