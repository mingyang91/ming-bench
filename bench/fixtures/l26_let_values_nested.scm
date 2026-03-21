(= (let-values (((a b) (values 1 2)))
     (let-values (((c d) (values (+ a 10) (+ b 20))))
       (+ c d)))
   33)