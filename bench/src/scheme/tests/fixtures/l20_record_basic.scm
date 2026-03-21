;; Basic define-record-type
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(let ((p (make-point 3 4)))
  (list (point? p) (point-x p) (point-y p)))
