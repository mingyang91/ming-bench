(and (not (procedure? 42))
     (not (procedure? "hello"))
     (not (procedure? '(1 2 3)))
     (not (procedure? #t)))