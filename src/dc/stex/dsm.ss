;;; dsm.ss
;;; Copyright (c) 1998 Cadence Research Systems

;;; authors: R. Kent Dybvig and Oscar Waddell

(define-syntax with-implicit
  (lambda (x)
    (syntax-case x ()
      ((_ (tid id ...) . body)
       (andmap identifier? #'(tid id ...))
       #'(with-syntax ((id (datum->syntax-object #'tid 'id)) ...) . body)))))

(define-syntax define-syntactic-monad
  (lambda (x)
    (syntax-case x ()
      ((_ name formal ...)
       (andmap identifier? #'(name formal ...))
       #'(define-syntax name
         (lambda (x)
             (syntax-case x (lambda case-lambda)
               ((key lambda more-formals . body)
                (with-implicit (key formal ...)
                  #'(lambda (formal ... . more-formals) . body)))
               ((key case-lambda (more-formals . body) (... ...))
                (with-implicit (key formal ...)
                  #'(case-lambda ((formal ... . more-formals) . body) (... ...))))
               ((key proc ((x e) (... ...)) arg (... ...))
                (andmap identifier? #'(x (... ...)))
                (with-implicit (key formal ...)
                  (for-each
                    (lambda (x)
                      (unless (let mem ((ls #'(formal ...)))
                                (and (not (null? ls))
                                     (or (free-identifier=? x (car ls))
                                         (mem (cdr ls)))))
                        (assertion-violationf 'name "unrecognized identifier ~s" x)))
                    #'(x (... ...)))
                  (with-syntax (((t (... ...))
                                 (generate-temporaries #'(arg (... ...)))))
                    #'(let ((p proc) (x e) (... ...) (t arg) (... ...))
                        (p formal ... t (... ...))))))
               ((key proc) #'(key proc ())))))))))

(define-syntax make-undefined
  (syntax-rules ()
    ((_ x ...)
     (begin (define-syntax x (lambda (z) (assertion-violationf 'x "undefined"))) ...))))
