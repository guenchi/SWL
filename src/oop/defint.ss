;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define-syntax define-integrable
  (lambda (x)
    (define make-residual-name
      (lambda (name)
        (datum->syntax-object
          name
          (string->symbol
            (string-append
              "residual-"
              (symbol->string (syntax-object->datum name)))))))
    (syntax-case x (lambda)
      [(ignore (name . formals) . body)
       (syntax (define-integrable name (lambda formals . body)))]
      [(ignore name (lambda formals form1 form2 ...))
       (identifier? (syntax name))
       (with-syntax ([xname (make-residual-name (syntax name))])
         (syntax
           (module ((name xname))
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   [_ (identifier? x) #'xname]
                   [(_ arg (... ...))
                    #'((fluid-let-syntax
                         ([name (identifier-syntax xname)])
                         (lambda formals form1 form2 ...))
                       arg
                       (... ...))])))
             (define xname
               (fluid-let-syntax
                 ([name (identifier-syntax xname)])
                 (lambda formals form1 form2 ...))))))])))
