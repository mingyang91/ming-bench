;; call-with-values with destructuring consumer
(call-with-values
  (lambda () (values 10 20 30))
  (lambda (a b c) (list c b a)))
