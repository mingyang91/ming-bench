(define (loop n) (if (= n 0) (quote done) (loop (- n 1)))) (loop 1000000)
