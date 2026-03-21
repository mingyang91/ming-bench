;; dynamic-wind preserves multiple return values
(call-with-values
  (lambda ()
    (dynamic-wind
      (lambda () #f)
      (lambda () (values 1 2 3))
      (lambda () #f)))
  list)
