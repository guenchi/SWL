; Test error "expected one item after dot (.)"

(define foo
  (lambda (a b . )
    'zapp))

