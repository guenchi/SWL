;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define inspector-rows (make-parameter 20))

(define inspector-cols (make-parameter 50))

(define inspector-font
  (make-parameter
    (create <font> 'courier 12 '(normal roman))))

(define inspect-length
  (make-parameter #f))

(define inspect-level
  (make-parameter #f))

;; ---- hooks for buttons (woo woo!)

(define inspector-extra-buttons
  (make-parameter '()))

(define inspector-add-button
  (lambda (pred label func)
    (inspector-extra-buttons
      (cons (list pred label func)
	(inspector-extra-buttons)))))

(let ()
  
(define compose
  (lambda (a b)
    (lambda (x)
      (a (b x)))))

(define-syntax gf
  (syntax-rules ()
    [(gf name) (lambda (x) (send x name))]))

(inspector-add-button (compose pair? (gf value)) "car" (gf car))
(inspector-add-button (compose pair? (gf value)) "cdr" (gf cdr))
(inspector-add-button
  (lambda (obj) (eq? (send obj type-sym) 'continuation))
  "link"
  (gf link))

)