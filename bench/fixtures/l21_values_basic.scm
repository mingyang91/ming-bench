;; call-with-values: basic multiple values
(call-with-values
  (lambda () (values 1 2 3))
  +)
