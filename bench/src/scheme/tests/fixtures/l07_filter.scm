(define (filter pred lst) (if (null? lst) '() (if (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) (filter pred (cdr lst)))))
(filter (lambda (x) (> x 2)) '(1 2 3 4 5))
