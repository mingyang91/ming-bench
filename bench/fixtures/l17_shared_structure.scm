;; Shared structure: mutation visible through aliases
(define a (list 1 2 3))
(define b a)
(set-car! a 99)
(car b)
