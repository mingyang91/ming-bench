;; dynamic-wind returns the body's value
(+ 1 (dynamic-wind
       (lambda () #f)
       (lambda () 41)
       (lambda () #f)))
