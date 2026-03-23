;; Chain of call/cc captures and resumes
;; Tests that continuation state doesn't accumulate (leak frames)
(define (chain n acc)
  (if (= n 0)
      acc
      (chain (- n 1)
             (+ acc (call/cc (lambda (k) (k 1)))))))
(chain 100 0)
