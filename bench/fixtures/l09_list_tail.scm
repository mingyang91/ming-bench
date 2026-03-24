(and (equal? (list-tail '(a b c d) 2) '(c d))
     (equal? (list-tail '(a b c) 0) '(a b c))
     (null? (list-tail '(a b c) 3)))