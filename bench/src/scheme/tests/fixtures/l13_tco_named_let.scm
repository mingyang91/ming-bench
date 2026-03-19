(let loop ((n 1000000)) (if (= n 0) (quote done) (loop (- n 1))))
