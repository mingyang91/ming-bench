(let ((v (make-vector 5 0)))
  (do ((i 0 (+ i 1)))
      ((= i 5) v)
    (vector-set! v i (* i i))))