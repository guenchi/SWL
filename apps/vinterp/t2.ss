;; my-odd?  is defined here in t2.ss,
;; my-even? is defined in t1.ss.

(define my-odd?
  (lambda (x)
    (not (my-even? x))))
