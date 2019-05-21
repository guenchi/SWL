; Test error "more than one item found after dot (.)"

(define foo
  (lambda [able baker . charlie delta epsilon]
    'wow))

