; Test error "mark #~s= missing"

'(what about this?) ; separate top-level S-expression, so ok.

(begin
  (reverse '(a b . #77#))
  (cons 1 2))