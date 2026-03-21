;; syntax-case with ellipsis patterns
(define-syntax my-list
  (lambda (stx)
    (syntax-case stx ()
      ((_ elem ...)
       #'(cons* elem ... '())))))

(define (cons* . args)
  (if (null? (cdr args))
      (car args)
      (cons (car args) (apply cons* (cdr args)))))

(my-list 1 2 3)
