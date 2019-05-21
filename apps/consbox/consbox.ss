;; Copyright (c) 1996 J. Michael Ashley
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; (load "../common/scrollframe.ss") ; already included in SWL heap

(module (sdraw <consbox> <fixed-consbox>)

(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)

#;  ; already exported by swl:macros
(define-syntax with-values
  (syntax-rules ()
    ((_ e1 e2) (call-with-values (lambda () e1) e2))))

(define-swl-class (<consbox> parent x1 y1 x2 y2 t1 t2) (<base>)
  (ivars (x1 x1) (y1 y1) (x2 x2) (y2 y2)
         (r1 (create <rectangle> parent x1 y1 (+ x1 (/ (- x2 x1) 2)) y2))
         (r2 (create <rectangle> parent (+ x1 (/ (- x2 x1) 2)) y1 x2 y2))
         (t1 (build-text parent x1 y1 (+ x1 (/ (- x2 x1) 2)) y2 t1))
         (t2 (build-text parent (+ x1 (/ (- x2 x1) 2)) y1 x2 y2 t2)))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public 
    (get-coords () (values x1 y1 x2 y2))
    (show () (show r1) (show r2) (show t1) (show t2))))

(define build-text
  (lambda (parent x1 y1 x2 y2 t)
    (let ((x (+ x1 (/ (- x2 x1) 2))) (y (+ y1 (/ (- y2 y1) 2))))
      (if (and (instance? t) (isa? t <consbox>))
          (with-values (get-coords t)
            (lambda (x1 y1 x2 y2)
              (if (<= x1 x x2)
                  (create <line> parent x y (+ x1 (/ (- x2 x1) 2)) 
                    (if (< y y1) y1 y2)
                    with (arrow-style: 'last))
                  (create <line> parent x y 
                    (if (< x x1) x1 x2)
                    (+ y1 (/ (- y2 y1) 2))
                    with (arrow-style: 'last)))))
          (create <canvas-text> parent x y with
            (anchor: 'center)
            (title: (format "~s" t))
            (width: (- x2 x1)))))))

(define-swl-class (<fixed-consbox> parent x y t1 t2)
              (<consbox> parent (- x 30) (- y 15) (+ x 30) (+ y 15) t1 t2)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))

(define sdraw
  (lambda (ls)
    (unless (pair? ls) (assertion-violationf 'sdraw "~s is not a pair" ls))
    (let* ([top (create <toplevel> with
                  (title: (format "Box-and-Pointer Diagram for ~s" ls)))]
           [scframe (create <scrollframe> top)]
           [canvas (create <canvas> scframe with (background-color: 'white))]
           [ht (#%|#make-eq-hash-table| 23)])
      (pack (create <button> top with
              (title: "done")
              (action: (lambda (self) (destroy top))))
            (anchor: 'n)
            (fill: 'x))
      (with-values
        (let f ([ls ls] [x 50] [y 50])
          (let ([a (#%|#eq-hash-cell| ht ls #f)])
            (if (cdr a)
                (assertion-violationf 'sdraw "doesn't handle cyclic data structures yet")
                (begin
                  (set-cdr! a #t)
                  (with-values (if (pair? (cdr ls))
                                   (f (cdr ls) (+ x 90) y)
                                   (values (cdr ls) x y))
                    (lambda (t2 max-x1 max-y1)
                      (with-values (if (pair? (car ls))
                                       (f (car ls) x (+ max-y1 60))
                                       (values (car ls) x y))
                        (lambda (t1 max-x2 max-y2)
                          (values
                            (make <fixed-consbox> canvas x y t1 t2)
                            (max max-x1 max-x2)
                            (max max-y1 max-y2))))))))))
        (lambda (t max-x max-y)
          (set-scroll-region! canvas 0 0 (+ max-x 50) (+ max-y 50))
          (pack scframe (expand: #t) (fill: 'both)))))))

(sdraw '(1 2))

)
