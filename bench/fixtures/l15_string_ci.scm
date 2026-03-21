(and (string-ci=? "ABC" "abc")
     (string-ci=? "Hello" "hello")
     (not (string-ci=? "abc" "abd")))