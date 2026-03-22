(define h (case-lambda (() 0) ((x) x)))
(eq? (procedure-name h) 'h)