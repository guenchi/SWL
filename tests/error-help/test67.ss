; Test error "unexpected end-of-file reading unquote-splicing"
(define r (list 1 2 3))
(set! r `(0 ,@   

