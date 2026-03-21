(define p (make-parameter 10))
(and (= (p) 10)
     (procedure? p))