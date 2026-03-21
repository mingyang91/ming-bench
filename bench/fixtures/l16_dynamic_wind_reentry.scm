;; dynamic-wind in-thunk fires on continuation re-entry
(define log '())
(define (push! x) (set! log (cons x log)))
(define saved #f)

(dynamic-wind
  (lambda () (push! 'in))
  (lambda ()
    (call/cc (lambda (k) (set! saved k)))
    (push! 'body))
  (lambda () (push! 'out)))

;; Re-enter the dynamic extent
(if (< (length log) 6)
    (saved 'again))

(reverse log)
