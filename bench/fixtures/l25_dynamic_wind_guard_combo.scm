;; Exception + dynamic-wind + continuation: resource cleanup
(define log '())
(define (push! x) (set! log (cons x log)))

(guard (exn
    ((string? exn)
     (list 'error exn (reverse log))))
  (dynamic-wind
    (lambda () (push! 'open))
    (lambda ()
      (push! 'work)
      (dynamic-wind
        (lambda () (push! 'inner-open))
        (lambda () (raise "oops"))
        (lambda () (push! 'inner-close))))
    (lambda () (push! 'close))))
