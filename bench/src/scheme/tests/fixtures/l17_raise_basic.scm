;; Basic raise + guard: catch a raised value
(guard (exn
    ((number? exn) (+ exn 1)))
  (raise 41))
