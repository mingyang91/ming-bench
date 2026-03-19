(define (make-adder n) (lambda (x) (+ x n))) ((make-adder 3) 4)
