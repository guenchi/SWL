; Test error "unexpected end-of-file reading list"

(lambda (x y z
  (cond
    [(< x 1) y]
    [else z]))

