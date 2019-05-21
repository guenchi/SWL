; Test error "expected close brace terminating gensym syntax"

(define #{foo |bar|
  (lambda (zap doodle)
    zap))

