;; Coroutine scheduler using call/cc
(let ((tasks '()) (results '()))
  (define (spawn thunk) (set! tasks (cons thunk tasks)))
  (define (yield-val v k) (set! results (cons v results)) (set! tasks (cons k tasks)))
  (define (run-all) (if (null? tasks) results (let ((t (car tasks))) (set! tasks (cdr tasks)) (t) (run-all))))
  (spawn (lambda () (call/cc (lambda (k) (yield-val 1 (lambda () (k #f))))) (call/cc (lambda (k) (yield-val 2 (lambda () (k #f)))))))
  (spawn (lambda () (call/cc (lambda (k) (yield-val 10 (lambda () (k #f))))) (call/cc (lambda (k) (yield-val 20 (lambda () (k #f)))))))
  (run-all)
  (length results))
