;; Stress test for macro expansion: deeply nested macro calls
;; Tests that macro expansion is not exponential
(define-syntax my-add
  (syntax-rules ()
    ((_ x) x)
    ((_ x y rest ...)
     (+ x (my-add y rest ...)))))

;; Expand to (+ 1 (+ 2 (+ 3 ... (+ 99 100)...)))
;; 100 nested expansions — should complete quickly if linear
(my-add 1 2 3 4 5 6 7 8 9 10
        11 12 13 14 15 16 17 18 19 20
        21 22 23 24 25 26 27 28 29 30
        31 32 33 34 35 36 37 38 39 40
        41 42 43 44 45 46 47 48 49 50)
