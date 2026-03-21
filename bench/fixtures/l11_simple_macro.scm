(define-syntax my-if (syntax-rules () ((my-if c t f) (if c t f))))
(my-if #t 1 2)
