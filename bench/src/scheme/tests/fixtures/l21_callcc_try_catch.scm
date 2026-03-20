(define-syntax try (syntax-rules (catch) ((try body catch handler) (call/cc (lambda (exit) (define (throw v) (exit (handler v))) (body throw))))))
(try (lambda (throw) (+ 1 (throw 42) 999)) catch (lambda (v) (list (quote caught) v)))
