;; guard with string exception
(guard (exn
    ((string? exn) (string-append "caught: " exn)))
  (raise "boom"))
