;; Nested dynamic-wind: proper nesting order
(define log '())
(define (push! x) (set! log (cons x log)))

(dynamic-wind
  (lambda () (push! 'outer-in))
  (lambda ()
    (dynamic-wind
      (lambda () (push! 'inner-in))
      (lambda () (push! 'inner-body) 99)
      (lambda () (push! 'inner-out))))
  (lambda () (push! 'outer-out)))

(reverse log)
