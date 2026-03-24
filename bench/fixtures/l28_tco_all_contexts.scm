;; TCO must work in ALL tail contexts, not just basic function calls
;; Each sub-test runs 100K iterations — stack overflow if not TCO

;; TCO in if
(define (tco-if n) (if (= n 0) 'if-ok (tco-if (- n 1))))
(tco-if 100000)

;; TCO in cond
(define (tco-cond n) (cond ((= n 0) 'cond-ok) (#t (tco-cond (- n 1)))))
(tco-cond 100000)

;; TCO in begin
(define (tco-begin n) (begin (if (= n 0) 'begin-ok (tco-begin (- n 1)))))
(tco-begin 100000)

;; TCO in let body
(define (tco-let n) (let ((x n)) (if (= x 0) 'let-ok (tco-let (- x 1)))))
(tco-let 100000)

;; TCO in and
(define (tco-and n) (and #t (if (= n 0) 'and-ok (tco-and (- n 1)))))
(tco-and 100000)

;; TCO in or
(define (tco-or n) (or #f (if (= n 0) 'or-ok (tco-or (- n 1)))))
(tco-or 100000)

;; TCO in case
(define (tco-case n) (case (= n 0) ((#t) 'case-ok) (else (tco-case (- n 1)))))
(tco-case 100000)

;; Final result: list of all passing contexts
(list (tco-if 100000) (tco-cond 100000) (tco-begin 100000)
      (tco-let 100000) (tco-and 100000) (tco-or 100000)
      (tco-case 100000))
