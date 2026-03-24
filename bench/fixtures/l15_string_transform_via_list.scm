(define (char-upcase c)
  (let ((n (char->integer c)))
    (if (and (>= n 97) (<= n 122))
        (integer->char (- n 32))
        c)))

(list->string (map char-upcase (string->list "hello")))
