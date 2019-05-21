; Test error "unexpected end-of-file reading block comment"
(pretty-print
 #| foo
   #| bar |#
    baz "pickle  ; not eof on string since we're in block comment