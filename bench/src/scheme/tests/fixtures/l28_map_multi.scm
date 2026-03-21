(and (equal? (map + '(1 2 3) '(10 20 30)) '(11 22 33))
     (equal? (map * '(2 3) '(4 5)) '(8 15))
     (equal? (map list '(a b) '(1 2)) '((a 1) (b 2))))