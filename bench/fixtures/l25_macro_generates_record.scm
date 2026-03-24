;; Macro that generates record usage
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define-syntax define-origin
  (syntax-rules ()
    ((_ name)
     (define name (make-point 0 0)))))

(define-origin my-origin)
(list (point-x my-origin) (point-y my-origin))
