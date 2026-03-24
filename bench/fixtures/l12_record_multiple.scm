;; Multiple record types are distinct
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define-record-type <color>
  (make-color r g b)
  color?
  (r color-r)
  (g color-g)
  (b color-b))

(let ((p (make-point 1 2))
      (c (make-color 255 0 0)))
  (list (point? p) (point? c) (color? c) (color? p)))
