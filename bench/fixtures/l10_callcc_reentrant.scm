;; Reentrant continuation: invoke saved continuation multiple times
(let ((k-save #f) (count 0))
  (set! count (+ count (call/cc (lambda (k) (set! k-save k) 1))))
  (if (< count 3) (k-save 1) count))
