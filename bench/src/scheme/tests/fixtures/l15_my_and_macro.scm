(define-syntax my-and (syntax-rules () ((my-and) #t) ((my-and x) x) ((my-and x rest ...) (if x (my-and rest ...) #f))))
(my-and 1 2 3)
