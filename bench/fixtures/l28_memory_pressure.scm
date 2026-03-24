;; Allocate 1M cons cells (not 10M — must fit in 30s/1GB container)
;; Each iteration allocates a list and discards it
;; Tests that the interpreter doesn't leak memory on abandoned allocations
(define (pressure n)
  (if (= n 0)
      'done
      (begin
        (list 1 2 3 4 5 6 7 8 9 10)
        (pressure (- n 1)))))
(pressure 1000000)
