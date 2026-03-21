;; Basic dynamic-wind: in-thunk, body, out-thunk all execute in order
(define log '())
(define (push! x) (set! log (cons x log)))

(dynamic-wind
  (lambda () (push! 'in))
  (lambda () (push! 'body) 42)
  (lambda () (push! 'out)))

(list (reverse log) 42)
