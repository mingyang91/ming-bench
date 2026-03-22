(equal? (do ((a 1 b) (b 2 a)) ((= a 2) (list a b)))
        '(2 1))