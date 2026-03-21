(define len (case-lambda
  (() 0)
  ((lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))))))
(len '(a b c))