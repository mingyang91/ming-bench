(define sum 0)
(define (add-up n) (if (= n 0) sum (begin (set! sum (+ sum n)) (add-up (- n 1)))))
(add-up 10)
