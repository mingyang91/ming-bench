(and (equal? (assoc 'b '((a 1) (b 2) (c 3))) '(b 2))
     (not (assoc 'z '((a 1) (b 2))))
     (equal? (assoc 2 '((1 "one") (2 "two") (3 "three"))) '(2 "two")))