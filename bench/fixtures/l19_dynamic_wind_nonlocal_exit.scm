;; dynamic-wind out-thunk runs even on non-local exit via call/cc
(define log '())
(define (push! x) (set! log (cons x log)))

(call/cc
  (lambda (exit)
    (dynamic-wind
      (lambda () (push! 'in))
      (lambda () (push! 'body) (exit 'escaped) (push! 'unreachable))
      (lambda () (push! 'out)))))

(reverse log)
