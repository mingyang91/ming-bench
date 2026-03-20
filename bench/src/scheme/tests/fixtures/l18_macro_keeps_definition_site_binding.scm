(define x 10)
(define-syntax get-x (syntax-rules () ((get-x) x)))
(let ((x 20)) (get-x))
