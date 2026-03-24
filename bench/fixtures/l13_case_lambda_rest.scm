(define f (case-lambda
  ((x) (list 'one x))
  ((x y . rest) (list 'many x y rest))))
(and (equal? (f 1) '(one 1))
     (equal? (f 1 2 3 4) '(many 1 2 (3 4))))