;; guard + dynamic-wind: cleanup runs on exception
(define log '())
(define (push! x) (set! log (cons x log)))

(guard (exn
    ((symbol? exn) (list 'caught exn (reverse log))))
  (dynamic-wind
    (lambda () (push! 'in))
    (lambda () (push! 'body) (raise 'fail))
    (lambda () (push! 'out))))
