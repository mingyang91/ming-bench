;; Full integration: macros + records + exceptions + values + continuations
(define-record-type <result>
  (make-result ok val)
  result?
  (ok result-ok)
  (val result-val))

(define-syntax try
  (syntax-rules ()
    ((_ body handler)
     (guard (exn (#t (make-result #f exn)))
       (make-result #t body)))))

(define (safe-divide a b)
  (try (call-with-values
         (lambda () (values a b))
         (lambda (x y)
           (if (zero? y) (raise "division by zero") (/ x y))))
       (lambda (e) e)))

(let ((r1 (safe-divide 10 3))
      (r2 (safe-divide 10 0)))
  (list (result-ok r1) (result-val r1)
        (result-ok r2) (result-val r2)))
