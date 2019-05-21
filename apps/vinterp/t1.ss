;; my-even? is defined here in t1.ss,
;; my-odd?  is defined in t2.ss.

(define my-even?
  (lambda (x)
    (or (zero? x) (my-odd? (- x 1)))))

(define ! (lambda (x) (if (zero? x) 1 (* x (! (- x 1))))))

