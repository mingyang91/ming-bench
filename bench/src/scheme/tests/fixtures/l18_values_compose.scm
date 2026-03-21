;; Nested call-with-values
(call-with-values
  (lambda ()
    (call-with-values
      (lambda () (values 3 4))
      (lambda (a b) (values (* a a) (* b b)))))
  +)
