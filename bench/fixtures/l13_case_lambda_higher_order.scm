(define f (case-lambda
  (() 0)
  ((x) (* x x))
  ((x y) (+ x y))))
(and (= (apply f '()) 0)
     (= (apply f '(5)) 25)
     (= (apply f '(3 4)) 7))