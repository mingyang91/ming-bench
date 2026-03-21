;; with-exception-handler: low-level exception handling
(call/cc
  (lambda (exit)
    (with-exception-handler
      (lambda (exn) (exit (+ exn 100)))
      (lambda () (raise 42)))))
