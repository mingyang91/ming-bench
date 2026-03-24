;; Records + exception handling
(define-record-type <err>
  (make-err code msg)
  err?
  (code err-code)
  (msg err-msg))

(guard (exn
    ((err? exn)
     (list 'caught (err-code exn) (err-msg exn))))
  (raise (make-err 404 "not found")))
