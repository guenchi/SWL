(let ()
  (define fac
    (lambda (x) (if (zero? x) 1 (* x (fac (- x 1))))))
  (define map
    (lambda (f ls)
      (if (null? ls) '() (cons (f (car ls)) (map f (cdr ls))))))
  (list (fac 4) (map (lambda (x) x) (read)) (map fac '(1 2 3 4 5 6))))

(let ((x 1))
  (let ((y x))
    (let ((z y))
      (list x y z))))

(let ((x 4))
  (+ x 5))

(let ((x 4) (y (read)) (z 7))
  (cons z (+ x 5)))

(let ((f (lambda (a b) 'foo)))
  (f 1 2))
