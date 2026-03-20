(define (loop n) (if (= n 0) (quote done) (begin 1 2 (loop (- n 1)))))
(loop 1000000)
