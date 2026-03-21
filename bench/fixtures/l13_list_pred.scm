(and (list? '(1 2 3))
     (list? '())
     (not (list? (cons 1 2)))
     (not (list? 42)))