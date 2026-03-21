(define (church-true x y) x)
(define (church-false x y) y)
(define (church-if b t f) (b t f))
(define (church-not b) (lambda (x y) (b y x)))
(define result (church-if (church-not church-false) (quote yes) (quote no)))
result
