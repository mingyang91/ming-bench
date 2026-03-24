(define f (case-lambda
  (() 0)
  ((x) x)
  ((x y) (+ x y))))
(and (= (f) 0)
     (= (f 5) 5)
     (= (f 3 4) 7))