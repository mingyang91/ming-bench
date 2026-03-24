(and (string=? "abc" "abc")
     (not (string=? "abc" "ABC"))
     (not (string=? "abc" "abd")))