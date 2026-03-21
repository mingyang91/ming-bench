;; Records work with higher-order functions
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define (distance p)
  (let ((dx (point-x p))
        (dy (point-y p)))
    (+ (* dx dx) (* dy dy))))

(let ((points (list (make-point 3 4) (make-point 0 0) (make-point 1 1))))
  (map distance points))
