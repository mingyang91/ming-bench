(define (loop n) (cond ((= n 0) (quote done)) (else (loop (- n 1)))))
(loop 1000000)
