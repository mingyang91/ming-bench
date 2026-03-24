;; write on a shared/circular structure must not infinite-loop
;; The exact output format for cycles is implementation-defined,
;; but the operation must terminate.
(define p (list 1 2))
(set-cdr! (cdr p) p)
;; Just verify that equal? can detect the cycle too
;; (by not infinite-looping on a self-referential structure)
(pair? p)
