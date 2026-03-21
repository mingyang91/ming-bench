;; Non-local exit from nested dynamic-wind: both out-thunks fire
(define log '())
(define (push! x) (set! log (cons x log)))

(call/cc
  (lambda (exit)
    (dynamic-wind
      (lambda () (push! 'outer-in))
      (lambda ()
        (dynamic-wind
          (lambda () (push! 'inner-in))
          (lambda () (exit 'done))
          (lambda () (push! 'inner-out))))
      (lambda () (push! 'outer-out)))))

(reverse log)
