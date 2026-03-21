;; guard with else clause catches anything
(guard (exn
    ((number? exn) 'number)
    (else 'other))
  (raise '(complex error)))
