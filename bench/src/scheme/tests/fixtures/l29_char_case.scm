(and (char=? (char-upcase #\a) #\A)
     (char=? (char-downcase #\A) #\a)
     (char=? (char-upcase #\Z) #\Z)
     (char=? (char-downcase #\z) #\z))