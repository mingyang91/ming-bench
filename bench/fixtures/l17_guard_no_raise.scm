;; guard body completes normally — no exception
(guard (exn
    ((number? exn) 'caught))
  (+ 10 20))
