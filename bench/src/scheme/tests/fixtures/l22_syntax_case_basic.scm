;; Basic syntax-case macro
(define-syntax my-when
  (lambda (stx)
    (syntax-case stx ()
      ((_ test body ...)
       #'(if test (begin body ...))))))

(define x 0)
(my-when #t (set! x 1) (set! x (+ x 1)))
x
