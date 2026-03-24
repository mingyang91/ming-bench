;; set! on unbound variable must error
(let () (set! nonexistent 42))
