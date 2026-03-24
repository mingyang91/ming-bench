;; Zero values with a consumer that takes no args
(call-with-values
  (lambda () (values))
  (lambda () 42))
