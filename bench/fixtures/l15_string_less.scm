(and (string<? "abc" "abd")
     (string<? "abc" "abcd")
     (not (string<? "abd" "abc"))
     (not (string<? "abc" "abc")))