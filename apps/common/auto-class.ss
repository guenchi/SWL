; In response to mail from JohnZ (not sure I agree with these changes)
;   - support automatic generation of setters / getters for ivars
;   - automatically clone formals as ivars
;   - make optional syntax optional
;   - auto documentation methods

(define-syntax define-auto-class
  (let ()
    (define-syntax with-values
      (syntax-rules ()
        [(_ p c) (call-with-values (lambda () p) c)]))
    (define collect
      (lambda (ls)
        (let loop ([ivs '()]
                   [inhd '()]
                   [inhtbl '()]
                   [priv '()]
                   [prot '()]
                   [pub '()]
                   [ls ls])
          (if (null? ls)
              (values ivs inhd inhtbl priv prot pub)
              (syntax-case (car ls) (ivars
                                      inherited
                                      inheritable
                                      private
                                      protected
                                      public)
                [(ivars x ...)
                 (loop #'(x ...) inhd inhtbl priv prot pub (cdr ls))]
                [(inherited x ...)
                 (loop ivs #'(x ...) inhtbl priv prot pub (cdr ls))]
                [(inheritable x ...)
                 (loop ivs inhd #'(x ...) priv prot pub (cdr ls))]
                [(private x ...)
                 (loop ivs inhd inhtbl #'(x ...) prot pub (cdr ls))]
                [(protected x ...)
                 (loop ivs inhd inhtbl priv #'(x ...) pub (cdr ls))]
                [(public x ...)
                 (loop ivs inhd inhtbl priv prot #'(x ...) (cdr ls))])))))
    (define unique-ids
      (lambda (fmls ls)
        (cond
          [(null? fmls) '()]
          [(syn-mem (car fmls) ls) (unique-ids (cdr fmls) ls)]
          [else (cons (car fmls) (unique-ids (cdr fmls) ls))])))
    (define syn-mem
      (lambda (id ls)
        (and (not (null? ls))
             (or (free-identifier=? id (car ls))
                 (syn-mem id (cdr ls))))))
    (define syn-car
      (lambda (x) (syntax-case x () [(car . cdr) #'car])))
    (define id->string
      (lambda (x)
        (cond
          [(string? x) x]
          [(identifier? x) (id->string (syntax-object->datum x))]
          [(symbol? x) (symbol->string x)]
          [else (assertion-violationf 'id->string "what the hell is ~s" x)])))
    (define compound-id
      (lambda (template . ids)
        (datum->syntax-object
          template
          (string->symbol
            (apply string-append (map id->string ids))))))
    (define make-auto-setters
      (lambda (template ivs pubs set!)
        (let loop ([ivs ivs])
          (if (null? ivs)
              '()
              (let ([setter (compound-id template "set-" (car ivs) "!")])
                (if (syn-mem setter pubs)
                    (loop (cdr ivs))
                    (cons (with-syntax ([setter setter]
                                        [ivar (car ivs)]
                                        [set! set!])
                            #'(setter (x) (set! ivar x)))
                          (loop (cdr ivs)))))))))
    (define make-auto-getters
      (lambda (template ivs pubs)
        (if (null? ivs)
            '()
            (let ([getter (compound-id template "get-" (car ivs))])
              (if (syn-mem getter pubs)
                  (make-auto-getters template (cdr ivs) pubs)
                  (cons (with-syntax ([getter getter] [ivar (car ivs)])
                          #'(getter () ivar))
                        (make-auto-getters template (cdr ivs) pubs)))))))
    (lambda (x)
      (syntax-case x ()
        [(_ (name formal ...) (base base-formal ...) x ...)
         (with-values
           (collect #'(x ...))
           (lambda (ivs inhd inhtbl priv prot pub)
             (let ([ivar-names (map syn-car ivs)])
               (let ([auto-ivs
                      (unique-ids
                        #'(formal ...)
                        (append ivar-names inhd inhtbl))]
                     [pub-names (map syn-car pub)])
                 (let ([ivar-names (append ivar-names auto-ivs)])
                   (let ([ivs (append ivs (map list auto-ivs auto-ivs))]
                         [auto-set
                          ;;; fairly gross:  set! must appear to be introduced
                          ;;; at the same time as the subclass name or we get
                          ;;; invalid syntax errors
                          (make-auto-setters #'_ ivar-names pub-names
                            (datum->syntax-object #'name 'set!))]
                         [auto-get
                          (make-auto-getters #'_ ivar-names pub-names)])
                     (with-syntax ([ivs ivs]
                                   [inhd inhd]
                                   [inhtbl inhtbl]
                                   [priv priv]
                                   [prot prot]
                                   [pub
                                    (append pub auto-get auto-set
                                      (with-syntax ([priv-names
                                                     (map syn-car priv)]
                                                    [prot-names
                                                     (map syn-car prot)]
                                                    [pub-names
                                                     (map syn-car
                                                          (append
                                                            pub
                                                            auto-get
                                                            auto-set))])
                                        (list #'(list-methods ()
                                                  '(methods-names:
                                                     (private . priv-names)
                                                     (protected . prot-names)
                                                     (public
                                                       list-methods
                                                       .
                                                       pub-names))))))])
                       #'(define-swl-class
                           (name formal ...)
                           (base base-formal ...)
                           (ivars . ivs)
                           (inherited . inhd)
                           (inheritable . inhtbl)
                           (private . priv)
                           (protected . prot)
                           (public . pub)))))))))]))))

; optional:
;--------------------- make pretty-printer happy -------------------

(pretty-format 'define-class
  '(define-class (stuff ...) (stuff ...)
    #f
    ivar
    #f
    ihtd
    #f
    ihtbl
    #f
    (p #f (bracket x fml 0 ...) ...)
    #f
    (p #f (bracket x fml 0 ...) ...)
    #f
    (p #f (bracket x fml 0 ...) ...)))
(pretty-format 'define-auto-class
  '(define-auto-class (stuff ...) (stuff ...)
    #f
    ivar
    #f
    ihtd
    #f
    ihtbl
    #f
    (p #f (bracket x fml 0 ...) ...)
    #f
    (p #f (bracket x fml 0 ...) ...)
    #f
    (p #f (bracket x fml 0 ...) ...)))
