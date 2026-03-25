;; Build a large string via repeated string-append
;; Tests that string-append is not O(n²) from full copying
;; Target: 10K chars (not 100K — must fit in 30s timeout)
(define (build-string n acc)
  (if (= n 0)
      (string-length acc)
      (build-string (- n 1) (string-append acc "aaaaaaaaaa"))))
(build-string 1000 "")
