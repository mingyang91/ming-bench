(define (fact-iter n acc) (if (= n 0) acc (fact-iter (- n 1) (* n acc))))
(fact-iter 20 1)
