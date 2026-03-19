(define (find-negative lst) (call/cc (lambda (return) (define (loop l) (cond ((null? l) #f) ((< (car l) 0) (return (car l))) (else (loop (cdr l))))) (loop lst))))
(find-negative '(3 7 -2 5))
