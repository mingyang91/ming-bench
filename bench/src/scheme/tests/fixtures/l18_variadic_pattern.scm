(define-syntax my-list (syntax-rules () ((my-list) '()) ((my-list x rest ...) (cons x (my-list rest ...)))))
(my-list 1 2 3)
