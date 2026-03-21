;; Record predicate distinguishes from other types
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(list (point? (make-point 1 2)) (point? '(1 2)) (point? 42) (point? "hello"))
