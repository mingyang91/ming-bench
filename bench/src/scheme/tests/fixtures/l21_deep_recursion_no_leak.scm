;; Deep tail-recursive allocation must not OOM
;; Creates and discards 1M cons cells — tests that old cells are reclaimable
(define (alloc-loop n)
  (if (= n 0)
      'done
      (begin
        (list 1 2 3 4 5)  ;; allocate and discard
        (alloc-loop (- n 1)))))

(alloc-loop 1000000)
