(define p (make-parameter "hello" string-length))
(and (= (p) 5)
     (= (parameterize ((p "world!")) (p)) 6))