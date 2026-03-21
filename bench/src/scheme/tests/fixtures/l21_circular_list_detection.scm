;; Detect circular lists: length must not infinite-loop
;; A proper implementation should detect the cycle or report an error
(define p (list 1 2 3))
(set-cdr! (cddr p) p)
;; p is now circular: (1 2 3 1 2 3 ...)
;; list? on a circular list must return #f (not loop forever)
(list? p)
