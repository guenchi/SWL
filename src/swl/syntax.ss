;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syntactic abstractions
;;

(module swl:macros (define-swl-class
                    define-swl-api-class
                    (swl:safety-check trust-all-swl-users?)
                    swl:api-procedure
                    swl:api-syntax
                    swl:api-class
                    swl:tutorial
                    define-generic
                    (grid swl:keyword-syntax->tcl)
                    make-menu
                    mvlet
                    with-values
                    with-mutex
                    on-error
                    isa?
                    (set-option! swl:keyword->setter)
                    (get-option swl:keyword->getter)
                    (swl:file-dialog swl:keyword-syntax->tcl)
                    (pack swl:keyword-syntax->tcl)
                    (create swl:keyword->setter)
                    scheme-version-case)

(import swl:oop)

;; Prohibit send-base outside of class definition.
; not needed anymore, I suspect
; (define-syntax send-base (lambda (x) (syntax-error x)))

(define-syntax define-swl-class
  ;* Automatically adds a public method named "class-name" that
  ;* returns a symbol corresponding to the name of the class.
  ;; Only needed for compiling SWL, factor out of exported bindings.
  (lambda (x)
    (syntax-case x ()
      [(_ (name . mfmls) (base . mactls)
          ivars
          inherited
          inheritable
          private
          protected
          (public m ...))
      (with-implicit (name swl:oop swl:macros swl:module-setup)
       (syntax
        ; Introduce module so that we can force import swl:oop and swl:macros.
        ; Without this, we have a problem trying to instantiate SWL classes in
        ; code like:
        ;  (let () (import swl:macros) (create <toplevel> with (title: "test")))
         (module (name)
           (import swl:oop)
           (import swl:macros)
           (import swl:module-setup)
           (define-class (name . mfmls) (base . mactls)
             ivars
             inherited
             inheritable
             private
             protected
             (public m ... (class-name () 'name)))
         )))])))

(define-syntax define-swl-api-class
  ;* Automatically adds a public method named "class-name" that
  ;* returns a symbol corresponding to the name of the class.
  ;* Expands into \scheme{swl:api-class} so that the documentation
  ;* processor will include the relevant documentation.
  ;; Only needed for compiling SWL, factor out of exported bindings.
  (lambda (x)
    (syntax-case x ()
      [(_ (name . mfmls) (base . mactls)
          ivars
          inherited
          inheritable
          private
          protected
          (public m ...))
       (syntax
         (swl:api-class (name . mfmls) (base . mactls)
           ivars
           inherited
           inheritable
           private
           protected
           (public m ...)))])))

;; Changed to  (create <class> positional-args ... with keyword-args ...)
;; see notes on the create macro definition below.

;; Note
;;    The motivation for the original implementation of compute-setters
;;    was to give useful error messages when an unknown option was
;;    specified.  I'm scrapping that for now because it did not complain
;;    about options that were inappropriate to a particular instance,
;;    only about options that do not exist anywhere in the core system.
;;    Stripping this out means we will get error messages about unknown
;;    method set-foo! instead of unknown option foo from a source expression
;;    like (create <toplevel> (foo: 'bar)).  Moreover, the error will be
;;    signaled at runtime not compile time.

(ctdef trust-all-swl-users?
  (make-parameter #f
    (lambda (x)
      (if (boolean? x)
          (begin
            (when x (printf "
  Caution:  This setting should only be used to build a SWL system that
            only trusted users have access to.  If it is possible for some
            other user to call methods on SWL objects, this setting is not
            recommended.
"))
          x)
          (assertion-violationf 'trusted-swl-use? "not a boolean ~s" x)))))

(define-syntax swl:safety-check
  (lambda (form)
    (syntax-case form ()
      ((_ . body)
       (if (and (trust-all-swl-users?) (= (optimize-level) 3))
           (syntax (void))
           (syntax (begin . body)))))))

(define-syntax swl:api-procedure
; install is either define or set!
  (syntax-rules () [(_ install name e) (install name e)]))

(define-syntax swl:api-syntax
  (syntax-rules (returns)
    [(_ name transformer) (define-syntax name transformer)]))

(define-syntax swl:api-class
  (lambda (form)
    (syntax-case form ()
      [(_ . whatever) (syntax (define-swl-class . whatever))])))

(define-syntax swl:tutorial
  (lambda (form)
    (syntax-case form ()
      [(_ name illustrates uses . body)
       (syntax (begin . body))])))

(define-syntax define-generic
  (lambda (form)
    (syntax-case form ()
;     [(_ name class) (syntax (define-generic name))]
      [(_ name)
       (syntax
         (define-syntax name
           (lambda (form)
             (syntax-case form ()
               [(xxx who . args) (syntax (send who name . args))]
               [_
                (identifier? form)
                (syntax
                  (lambda (who . args)
                    (send-apply who name args)))]))))])))

(define-generic isa?)

(module ((swl:keyword->setter swl:upto-colon swl:compound-id)
         (swl:keyword->getter swl:upto-colon swl:compound-id))

  (module ((swl:upto-colon swl:id->string)
           (swl:compound-id swl:id->string))
  
    (ctdef swl:id->string
      (lambda (x)
        (cond
          [(string? x) x]
          [(identifier? x) (swl:id->string (syntax-object->datum x))]
          [(symbol? x) (symbol->string x)])))

    (ctdef swl:compound-id
      (lambda (template . ids)
        (datum->syntax-object template
          (string->symbol (apply string-append (map swl:id->string ids))))))
  
    (ctdef swl:upto-colon
      (lambda (who id)
        (let ((x (swl:id->string id)))
          (let ((len (string-length x)))
            (if (char=? #\: (string-ref x (- len 1)))
                (substring x 0 (- len 1))
                (assertion-violationf who "invalid option syntax ~a, expected colon suffix" x))))))
  )

(ctdef swl:keyword->setter
  (lambda (who)
    (lambda (x)
      (syntax-case x ()
        [(key . val)
         (with-syntax ([setter
                        (swl:compound-id (syntax key)
                          "set-" (swl:upto-colon who (syntax key)) "!")])
           (syntax (setter . val)))]))))

(ctdef swl:keyword->getter
  (lambda (who)
    (lambda (x)
      (syntax-case x ()
        [key
         (identifier? (syntax key))
         (with-syntax ([getter
                        (swl:compound-id (syntax key)
                          "get-" (swl:upto-colon who (syntax key)))])
           (syntax getter))]))))
)

(swl:api-syntax set-option!
  ;* \formdef{set-option!}{syntax}{(set-option! \var{widget} (keyword \var{value}) \dots)}
  ;* \ret{unspecified}
  ;* The \scheme{set-option!} macro makes it possible to change several
  ;* widget attributes at one time using the same keyword argument
  ;* syntax provided by the \scheme{create} macro.
  ;* The list of possible keywords is the same as for \scheme{create}
  ;* and \scheme{get-option}.
  (lambda (form)
    (syntax-case form ()
      ((_ e1 op1 op2 ...)
       (with-syntax (((setter ...)
                      (map (swl:keyword->setter 'set-option!)
                           (syntax (op1 op2 ...)))))
         (syntax
           (let ((inst e1))
             (send inst . setter) ...)))))))

(swl:api-syntax get-option
  ;* \formdef{get-option}{syntax}{(get-option \var{widget} keyword \dots)}
  ;* \ret{see below}
  ;* The \scheme{get-option} macro returns the values for one or more
  ;* widget attributes corresponding to the given keywords.
  ;* Multiple values are returned when several keywords are given.
  ;* The list of possible keywords is the same as for \scheme{create}
  ;* and \scheme{set-option!}.
  (lambda (form)
    (syntax-case form ()
      ((_ e1 op ...)
       (with-syntax
         (((getter ...) (map (swl:keyword->getter 'get-option)
                             (syntax (op ...)))))
         (syntax
           (let-syntax
             ((list
                (lambda (x)
                  (syntax-case x ()
                    [(kwd e) (syntax e)]
                    [(kwd . whatever) (syntax (list . whatever))]))))
             (let ([inst e1]) (list (send inst getter) ...)))))))))


(ctdef swl:keyword-syntax->tcl
  (rec munge
    (lambda (who ls keys bugs)
      (define syntax-lookup
        (lambda (id ls)
          (let loop ((id id) (ls ls))
            (syntax-case ls ()
              (() #f)
              (((x . y) . rest)
               (literal-identifier=? id (syntax x))
               (syntax y))
              ((x . rest)
               (loop id (syntax rest)))))))
      (syntax-case ls ()
        (()
         (if (null? bugs)
             '()
             (assertion-violationf who (apply string-append bugs))))
        (((key val) . rest)
         (let ((hit (syntax-lookup (syntax key) keys)))
           (if hit
               (with-syntax ((hit hit) (rest (munge who (cdr ls) keys bugs)))
                 (syntax ('hit val . rest)))
               (munge who (cdr ls) keys
                 (cons (format "~n  unknown option ~s"
                         (syntax-object->datum (car ls)))
                       bugs)))))
        (else (assertion-violationf who
                "malformed option ~s"
                (syntax-object->datum #'else)))))))

(swl:api-syntax swl:file-dialog
  ;* \formdef{swl:file-dialog}{syntax}{(swl:file-dialog \var{title} \var{kind} (keyword \var{value}) \dots)}
  ;* \ret{see below}
  ;*
  ;* The \scheme{swl:file-dialog} macro displays the native file selection dialog
  ;* for the current platform.  The \var{title} should evaluate to a string,
  ;* \var{kind} should evaluate to one of the symbols \scheme{save} or \scheme{open}.
  ;* The optional keyword arguments are:
  ;*  \scheme{parent:} which specifies the \scheme{<toplevel>} served by the dialog,
  ;*  \scheme{default-file:} which indicates the filename initially displayed
  ;*    in the text entry field of the dialog,
  ;*  \scheme{default-dir:} which names the directory to display initially
  ;*    if the current directory is not to be used,
  ;*  \scheme{default-extension:} which (under Windows) gives an extension to use
  ;*    if the filename entered by the user has none, and
  ;*  \scheme{file-types:} which allows the display to be restricted to files
  ;*    with particular extensions.
  ;* If supplied, the \scheme{file-types:} keyword takes an association list
  ;* of the form \scheme{(("file type" ("extension-pattern" ...)) ...)}.
  (let ((keys
          (syntax
            ((default-extension: . -defaultextension)
             (file-types: . -filetypes)
             (default-dir: . -initialdir)
             (default-file: . -initialfile)
             (parent: . -parent)))))
     (define default-dir?
       (lambda (ls)
         (syntax-case ls (default-dir:)
           (() #f)
           (((default-dir: x) . rest) #t)
           ((op . rest) (default-dir? #'rest)))))
     (lambda (form)
       (syntax-case form ()
         ((_ title kind op ...)
          (with-syntax (((op ...)
                        (swl:keyword-syntax->tcl 'swl:file-dialog
                          (if (default-dir? #'(op ...))
                              (syntax (op ...))
                              (syntax ((default-dir: (current-directory)) op ...)))
                          keys '())))
            (syntax
              (let ([file
                     (swl:tcl-eval
                       (let ((x kind))
                         (case x
                           ((save) '|tk_getSaveFile|)
                           ((open) '|tk_getOpenFile|)
                           (else (assertion-violationf 'swl:file-dialog "unknown dialog type ~s" x))))
                       '-title title op ...)])
                (and (not (string=? file "")) file)))))))))

(swl:api-syntax pack
  ;* \formdef{pack}{syntax}{(pack \var{widget} (keyword \var{value}) \dots)}
  ;* \ret{unspecified}
  ;*
  ;* The \scheme{pack} macro makes more complex widget layouts possible
  ;* using a set of keyword arguments including \scheme{anchor:}, \scheme{expand:},
  ;* \scheme{fill:}, and \scheme{side:}.
  ;* These keywords determine the placement of the given widget within
  ;* the available area of its parent.  A \scheme{<frame>} is often useful
  ;* to group several widgets together.
  ;* \scheme{anchor:} specifies where the widget to be packed is anchored.
  ;* Legal values are \scheme{n}, \scheme{s}, \scheme{e}, \scheme{w}, \scheme{ne}, \scheme{se},
  ;* \scheme{sw}, \scheme{nw}, and \scheme{center}.
  ;* \scheme{expand:} is a boolean indicating whether or not the widget should 
  ;* expand to fill its parent.  \scheme{fill:} specifies how the widget should
  ;* fill its parent: \scheme{x} means fill horizontally, \scheme{y} means fill
  ;* vertically, \scheme{both} fills both horizontally and vertically, and
  ;* \scheme{none} avoids filling.  \scheme{side:} determines what side of the
  ;* parent widget this widget is anchored at.  Legal values are:
  ;* \scheme{top}, \scheme{bottom}, \scheme{left}, and \scheme{right}.
  ;*
  ;* To change the layout of a widget repack it using a different set
  ;* of attributes.  When a widget is hidden via the \scheme{hide} method
  ;* it currently forgets its previous layout.
  ;*
  ;* The whole process of geometry management will likely be revised soon.
  (let ((keys
          (syntax
            ((expand: . -expand)
             (side: . -side)
             (anchor: . -anchor)
             (fill: . -fill)))))
  (lambda (form)
    (syntax-case form ()
      ((_ e op ...)
       (with-syntax (((pkop ...)
                     (swl:keyword-syntax->tcl 'pack (syntax (op ...)) keys '())))
         (syntax (send e pack pkop ...))))))))

;; not ready for API yet.
(define-syntax grid
  ;;
  ;;  currently no way to unset sticky properties, or to specify column/row
  ;;  minwidth or weight
  ;;
  ;* \formdef{grid}{syntax}{(grid widget (keyword value) ...)}
  ;*
  ;* The \scheme{grid} macro makes more complex widget layouts possible
  ;* using a set of keyword arguments including \scheme{sticky:},
  ;* \scheme{column:}, \scheme{row:},
  ;* \scheme{columnspan:}, and \scheme{rowspan:}.
  ;* These keywords determine the placement of the given widget within
  ;* the available area of its parent.  A \scheme{<frame>} is often useful
  ;* to group several widgets together.
  ;*
  ;* The \scheme{column:} and \scheme{row:} options determine the position of
  ;* the widget on the grid.  The \scheme{row-span:} \scheme{column-span:}
  ;* options determine how many rows or columns of the grid the widget spans.
  ;* The \scheme{sticky:} option is a symbol made of some combination of \scheme{n}, \scheme{s},
  ;* \scheme{e}, and \scheme{w} indicating to which sides the widget adheres in the
  ;* region it occupies.
  ;*
  ;* To change the layout of a widget regrid it using a different set
  ;* of attributes.
  ;*
  ;* The whole process of geometry management will likely be revised soon.
  (let ((keys
          (syntax
            ((row: . -row)
             (row-span: . -rowspan)
             (column: . -column)
             (column-span: . -columnspan)
             (sticky: . -sticky)))))
  (lambda (form)
    (syntax-case form ()
      ((_ e op ...)
       (with-syntax (((gop ...)
                     (swl:keyword-syntax->tcl 'grid (syntax (op ...)) keys '())))
         (syntax (send e grid gop ...))))))))

(define-syntax mvlet
  (syntax-rules ()
    ((mvlet (fmls p) c ...)
     (call-with-values
       (lambda () p)
       (lambda fmls c ...)))))

(define-syntax with-values
  (syntax-rules ()
    ((_ p c)
     (call-with-values
       (lambda () p)
       c))))

(define-syntax on-error  ; this version permits (on-error (on-error ...) e1 e2 ...)
 ; syntax:
 ;    (on-error (with-message msg e0) e1 e2 ...) ; gives you the error message
 ; or
 ;    (on-error e0 e1 e2 ...)
  (syntax-rules ()
    [(on-error e0 e1 e2 ...)
     (guard (c [#t #;(pretty-print `(on-error-error ,(with-output-to-string (lambda () (display-condition c)))) #%$console-output-port)
                   (fluid-let-syntax ([with-message
                                       (syntax-rules ()
                                         [(_ msg expr)
                                          (let ([msg (with-output-to-string (lambda () (display-condition c)))])
                                            expr)])])
                     e0)])
       (let () e1 e2 ...))]))

; avoided making s free in closures
(define-syntax with-mutex
  (syntax-rules ()
    [(with-mutex sem e0 e1 ...)
     (let ([s sem])
       (send s wait)
       ((let ([thunk
               (guard (c [#t (lambda () (#%raise c))])
                 (call-with-values
                   (lambda () e0 e1 ...)
                   (case-lambda
                     [(arg) (lambda () arg)]
                     [args (lambda () (apply values args))])))])
          (send s signal)
          thunk)))]))

(swl:api-syntax create
  ;* \formdef{create}{syntax}{(create widget-class \var{make-arg} \dots with (keyword \var{value}) \dots)}
  ;* \ret{instance}
  ;*
  ;* Widgets are created using the \scheme{create} macro
  ;* which initializes the widget instance and provides convenient
  ;* keyword syntax for specifying initial widget attributes.
  ;* The \var{make-arg}s are positional and must precede the
  ;* optional keyword arguments.
  ;* The auxiliary keyword \scheme{with} marks the end of the positional arguments
  ;* and the start of the keyword arguments.
  ;* The keyword arguments are optional and may appear in any order.
  ;*
  ;* Every attribute that can be set with a method called
  ;* \scheme{set-}\var{attribute-name}\scheme{!} can be initialized with
  ;* a keyword of the form \var{attribute-name}\scheme{:}.
  ;* For example, the title of a button called \scheme{button1} can be set
  ;* to \scheme{"Done"} by the method call
  ;* \scheme{(set-title! button1 "Done")}.  Alternatively the title of the button
  ;* can be
  ;* set to \scheme{"Done"} when it is first created by adding \scheme{(title: "Done")}
  ;* to the list of keyword arguments in the call to \scheme{create}.
  ;*
  ;* \mybf{Note:} The \scheme{create} macro extends the usual initialization sequence.
  ;* Any class to be instantiated via \scheme{create} must supply (or inherit)
  ;* a public \scheme{init} method that accepts the same arguments
  ;* as the class.  This method is called with the actual parameters
  ;* (the values of the positional arguments) passed to
  ;* the class just after the instance is created.
  (lambda (form)
    (define split-args
      (lambda (args)
        (let loop ((args args) (pos '()) (key '()))
          (syntax-case args (with)
            (() (values pos key))
            ((with . rest) (values pos (syntax rest)))
            ((x . y) (loop (syntax y) (cons (syntax x) pos) key))))))
    (syntax-case form ()
      [(_ <class> arg ...)
       (mvlet ((positional keyword) (split-args (syntax (arg ...))))
         (let ((positional (reverse positional)))
           (with-syntax ([(pos-arg ...) positional]
                         [(temp ...) (generate-temporaries positional)]
                         [(setter ...)
                          (map (swl:keyword->setter 'create) keyword)])
             (syntax
               (let ([temp pos-arg] ...)
                 (let ([inst (make <class> temp ...)])
                   (send inst init temp ...)
                   (send inst . setter)
                   ...
                   inst))))))])))

(swl:api-syntax make-menu
  ;* \formdef{make-menu}{syntax}{(make-menu (label \var{item}) (label \var{item}) \dots)}
  ;* \ret{an instance of \scheme{<menu>}}
  ;*
  ;* This macro simplifies the construction of complex menus.
  ;* The subforms are pairs of \var{label} and \var{item}, where \var{label} must
  ;* evaluate to a string, and \var{item} is either a procedure of no
  ;* arguments or a menu.  If \var{item} is a procedure, the procedure is
  ;* invoked whenever the corresponding \var{label} is chosen from the menu.
  ;* If \var{item} is a menu, the menu is posted as a cascade
  ;* whenever the corresponding \var{label} is chosen from the menu.
  (lambda (form)
    (syntax-case form ()
      ((k (label item) (l2 i2) ...)
       (syntax (create <menu>
                 (simple-menu-list->menu-items `((,label . ,item) (,l2 . ,i2) ...))))))))

(define-syntax scheme-version-case
  (lambda (x)
    (syntax-case x (else >=)
      [(_ (else . body)) #'(begin . body)]
      [(_ ((key) . body) more ...)
       (if (string=?
             (format "~s" #%$scheme-version)
             (syntax-object->datum #'key))
           #'(begin . body)
           #'(scheme-version-case more ...))]
      [(_ ((>= key) . body) more ...)
       (if (string>=?
             (format "~s" #%$scheme-version)
             (syntax-object->datum #'key))
           #'(begin . body)
           #'(scheme-version-case more ...))])))

;; I'm updating the pretty printer as a side-effect
;; of expansion so that even the documentation compiler (who merely expands
;; the source) gets the modifications.

(pretty-format 'create '(create what e #f ...))
(pretty-format 'swl:callback-lambda '(swl:callback-lambda formals #f x ...))
(pretty-format
  'event-case
  '(event-case
     ((k v) 0 ...)
     #f
     (((bracket x ...) ...) 0 e ...)
     ...))
(pretty-format 'set-option! '(set-option! inst #f (x ...) ...))
(pretty-format 'get-option '(get-option inst x ...))
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
)

(eval-when (compile)
(import swl:oop)
(import swl:macros)
) ; end eval-when
