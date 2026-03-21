(define (sum . args) (if (null? args) 0 (+ (car args) (apply sum (cdr args)))))
(sum 1 2 3 4 5)
