;; syntax-case with fender (guard expression)
(define-syntax safe-div
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       #'(if (zero? b) 'division-by-zero (/ a b))))))

(list (safe-div 10 2) (safe-div 10 0))
