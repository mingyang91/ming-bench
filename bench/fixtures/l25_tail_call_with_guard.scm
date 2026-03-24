;; TCO must work inside guard body
(define (countdown n)
  (guard (exn (#t exn))
    (if (= n 0) 'done (countdown (- n 1)))))

(countdown 100000)
