(and (string=? (string-upcase "hello") "HELLO")
     (string=? (string-upcase "Hello World") "HELLO WORLD")
     (string=? (string-upcase "123") "123"))