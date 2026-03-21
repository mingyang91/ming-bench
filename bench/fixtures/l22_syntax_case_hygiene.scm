;; syntax-case preserves hygiene like syntax-rules
(define-syntax swap!
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       #'(let ((tmp a))
           (set! a b)
           (set! b tmp))))))

(define x 1)
(define y 2)
(define tmp 999)  ;; should not interfere
(swap! x y)
(list x y tmp)
