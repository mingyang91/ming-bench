(define (loop n) (if (= n 0) (quote done) (let ((m (- n 1))) (loop m))))
(loop 1000000)
