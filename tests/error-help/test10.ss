; Test error "invalid character #\\~a~a~a"

(memv #\401 (string->list "abcd"))

