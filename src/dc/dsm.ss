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
            (syntax-case x (lambda)
              ((key lambda more-formals . body)
               (with-implicit (key formal ...)
                 #'(lambda (formal ... . more-formals) . body)))
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
                 #'(let ((x e) (... ...))
                     (proc formal ... arg (... ...)))))
              ((key proc) #'(key proc ())))))))))

(define-syntax make-undefined
  (lambda (x)
    (syntax-case x ()
      [(_ id ...)
       (andmap identifier? #'(id ...))
       #'(begin
           (define-syntax id
             (lambda (x) (assertion-violationf #f "attempt to reference ~s" 'id)))
           ...)])))

