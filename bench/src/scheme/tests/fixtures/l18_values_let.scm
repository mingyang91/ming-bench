;; call-with-values used for let-values pattern
(call-with-values
  (lambda ()
    (let ((x 100))
      (values x (* x 2) (* x 3))))
  (lambda (a b c) (+ a b c)))
