; Test error "expected close brace terminating gensym syntax"

(define foo
  (lambda (#{foo |bar| none)
    'quack))

