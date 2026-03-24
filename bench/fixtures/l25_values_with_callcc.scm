;; values + call/cc: continuation captures multiple values
(call-with-values
  (lambda ()
    (call/cc
      (lambda (k)
        (k 10 20 30))))
  (lambda (a b c) (+ a b c)))
