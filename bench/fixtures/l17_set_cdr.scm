;; set-car! and set-cdr! for pair mutation
(define p (cons 1 2))
(set-car! p 10)
(set-cdr! p 20)
p
