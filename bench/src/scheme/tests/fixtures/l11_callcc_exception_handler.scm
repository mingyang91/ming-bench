(define (with-handler handler thunk) (call/cc (lambda (exit) (define (raise msg) (exit (handler msg))) (thunk raise))))
(with-handler (lambda (msg) (list (quote error) msg)) (lambda (raise) (raise 42) (quote unreachable)))
