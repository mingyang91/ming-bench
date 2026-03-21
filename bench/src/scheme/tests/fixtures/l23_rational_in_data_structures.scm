;; Rationals in lists, vectors, and as record fields
(define-record-type <fraction-pair>
  (make-fp a b)
  fp?
  (a fp-a)
  (b fp-b))

(let ((fp (make-fp 1/3 2/3))
      (v (vector 1/4 1/2 3/4))
      (lst (list 1/6 1/3 1/2)))
  (list (+ (fp-a fp) (fp-b fp))
        (vector-ref v 1)
        (apply + lst)))
