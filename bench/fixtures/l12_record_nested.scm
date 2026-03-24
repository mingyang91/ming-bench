;; Records containing other records
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define-record-type <segment>
  (make-segment start end)
  segment?
  (start segment-start)
  (end segment-end))

(let ((seg (make-segment (make-point 0 0) (make-point 3 4))))
  (list (point-x (segment-start seg))
        (point-y (segment-end seg))))
