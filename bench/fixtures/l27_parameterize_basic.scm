(define p (make-parameter 10))
(and (= (parameterize ((p 20)) (p)) 20)
     (= (p) 10))