(define (loop n) (if (= n 0) #t (and #t (loop (- n 1))))) (loop 1000000)
