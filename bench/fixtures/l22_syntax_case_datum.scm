;; syntax->datum and datum->syntax for computed templates
(define-syntax make-adder
  (lambda (stx)
    (syntax-case stx ()
      ((kw n)
       (with-syntax ((name (datum->syntax #'kw
                     (string->symbol
                       (string-append "add-" (number->string (syntax->datum #'n)))))))
         #'(define (name x) (+ x n)))))))

(make-adder 5)
(add-5 10)
