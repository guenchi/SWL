;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; NOTE:  common bug causing TeX limits to be exceeded is missing \ret{...}
;;        in documentation comment.

;; When we are compiling documentation, this file should be loaded before
;; processing and expanding the rest of the source.
;; The documentation form defines a make-documentation thunk which is
;; called after the whole set of documentation has been processed.
;; 
;; In this file we should define all the macros that will be used to process
;; and record info about the source code.
;; 
;; (documentation e ...)
;;    => (define make-documentation (lambda () e ...))
;; 
;; (chapter name e ...)
;;    => (begin (printf "\\chapter{name}~n") e ...)
;; 
;; (section name e ...)
;;    => (begin (printf "\\section{name}~n") e ...)

(define omit-methods
  ;; methods to omit from documentation.  these are internal-use methods
  ;; this may go away once we have the module system in place
  '(add-to-menu!
    adopt
    apply-parent-eval
    disown
    ensure-configured-for
    get-application-context
    get-balloon-help
    get-balloon-help-delay
    grid
    mark-name
    pack
    parent-eval
    place
    print-rep
    rename-this-primitive-map
    scheme->tcl
    set-balloon-help!
    set-balloon-help-delay!
    set-parent!
    set-window-pos!
    set-window-size!
    tag-bind
    tag-config
    tag-range
    tag-ranges
    y-position
))


; need to print documentation for duplicated methods and group it
; according to class.  eg.  the insert, select-range, etc. methods
; common to entry, text, listbox, classes
;
; don't bother doing this for init method, though

(define-syntax class-section
  (syntax-rules ()
    [(_ header intro class-list)
     (section header
       intro
       (for-each
         print-class
         (order (keyword-val 'class) class-cname class-list)))]))


(documentation

  (chapter "Tutorials" "chapter:tutorials"
    (for-each
      print-tutorial
      (order (keyword-val 'tutorial) tutorial-name
        '(define-class
          <toplevel>
          <label> 
          <button>
          <canvas>
          <entry>
          <menu>
          swl:file-dialog
          <photo>))))

  (chapter "Classes" "chapter:classes"
    (warningf #f "stupidly assuming all classes instantiated via create")
    (class-section "Foundations"
      (void)
      '(<toplevel> <frame>))
    (class-section "Labels, Buttons, and Menus"
      (void)
      '(<label>
        <button>
        <radiobutton>
        <checkbutton>
        <option-button>
        <menu>
        <command-menu-item>
        <cascade-menu-item>
        <separator-menu-item>
        <radio-menu-item>
        <check-menu-item>))

    (class-section "Attributes"
      (void)
      '(<font>
        <rgb>
        <photo>
        <bitmap>))

    (class-section "Displaying and Editing Text"
      (void)
      '(<listbox>
        <entry>
        <text>
        <markup>
        <floating-mark>
        <fixed-mark>
        <tab-stop>))

    (class-section "Control Widgets"
      (void)
     '(<scrollbar> <scale>))

    (class-section "Drawing"
      (void)
     '(<canvas>
       <rectangle>
       <oval>
       <arc>
       <line>
       <polygon>
       <canvas-text>
       <canvas-image>
       <canvas-sub-window>)))

; group methods w/ same name
;   sort by class
;   sort by arity
;
;   list classes for each method (remember to include subclasses)
;
; suppress when no documentation comment

; damn.  putting the \endentryheader in the documentation comments \ret{}
; is a mistake because it will foobar attempts to group methods w/ like
; interfaces...

; result ::= (group ...)
; group  ::= (subgroup ...) ; methods in each subgroup have same name
; subgroup ::= (method ...) ; each method has same name and same interface

  (chapter "Methods" "chapter:methods"
    (for-each
      print-method-group
      (map (lambda (ms)
             (group-methods method-interface
               (sort (lambda (m1 m2) (< (method-interface m1) (method-interface m2))) ms)))
           (group-methods method-name (sort-methods (compute-methods))))))

; testing just init  (short and sweet)
  #;(chapter "Methods"
    (for-each
      print-method-group
      (map (lambda (ms)
             (group-methods method-interface
               (sort (lambda (m1 m2) (< (method-interface m1) (method-interface m2))) ms)))
           (group-methods
             method-name
             (filter
               (compute-methods)
               (lambda (m) (eq? (method-name m) 'init)))))))

; quick test
  #;(chapter "Methods with names less than \"cat\" (for testing)"
    (for-each
      print-method
      (filter
        (sort-methods (compute-methods))
        (lambda (m)
          (string<? (symbol->string (method-name m)) "cat")))))

  (chapter "Syntactic Extensions" "chapter:macros"
    (for-each
      print-syntax
      (keyword-val 'syntax)))

  (chapter "Procedures" "chapter:procedures"
    (for-each
      print-procedure
      (keyword-val 'procedure)))

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; application-specific transformers
;;
;; Note that since we don't expand subforms this is not suited to nested
;; structures.  For example, if we have one swl:api-class within another,
;; we'll miss the documentation for the inner one.

(define-syntax define-generic
  (lambda (form)
    (syntax-case form ()
      ((x name . whatever)
       (annotate 'generics (datum name))))))

(define generic?
  (lambda (name)
    (memq name (keyword-val 'generics))))

(define-syntax swl:tutorial
  (lambda (form)
    (syntax-case form (illustrates follows *comment*)
      ((_ name (illustrates i ...) (follows u ...) (*comment* . comment) . body)
       (annotate 'tutorial
         (make-tutorial
            (datum name)
            (datum comment)
            (datum (i ...))
            (datum (u ...))
            (datum ((*comment* . comment) . body))))))))

(define-structure (tutorial name comment illustrates follows body))

(define-syntax swl:api-syntax
  (lambda (form)
    (syntax-case form ()
      ((_ name . body)
       (annotate 'syntax
         (make-syntax
           (datum name)
           (keyword-val '*comment* (datum body))))))))

(define-structure (syntax name comment))

(define-syntax swl:api-procedure
  (lambda (form)
    (syntax-case form (returns)
      ((_ installer name . body)
       (annotate 'procedure
         (make-procedure
           (datum name)
           (keyword-val '*comment* (datum body))))))))

(define-structure (procedure name comment))

(define-syntax swl:api-class
  (lambda (form)
    (syntax-case form ()
      [(_ (name . args) (base-class . baseargs) . body)
       (annotate 'class
         (build-class 'swl:api-class
           (datum name)
           (datum args)
           (datum base-class)
           (datum baseargs)
           (datum body)))])))

(define-syntax import (lambda (form) #'(begin)))

(define-syntax define-class
  (lambda (form)
    (syntax-case form ()
      [(_ (name . args) (base-class . baseargs) . body)
       (annotate 'class
         (build-class 'define-class
           (datum name)
           (datum args)
           (datum base-class)
           (datum baseargs)
           (datum body)))])))

(define-syntax define-swl-class (identifier-syntax define-class))
(define-syntax define-swl-api-class (identifier-syntax swl:api-class))

(define-structure (class maker comment
                   cname cargs bname bargs
                   ivars inhtd ihtbl
                   priv prot pub)
  ([inherited-methods #f]
   [sub-classes #f]))

(define build-class
  (lambda (maker classname classargs basename baseargs body)
    (make-class
      maker
      (keyword-val '*comment* body)
      classname
      classargs
      basename
      baseargs
      (keyword-val 'ivars body)
      (keyword-val 'inherited body)
      (keyword-val 'inheritable body)
      (map (build-method classname) (keyword-val 'private body))
      (map (build-method classname) (keyword-val 'protected body))
      (map (build-method classname) (keyword-val 'public body)))))
 
(define-structure (method name margs interface comment classname))

(define build-method
  (lambda (classname)
    (lambda (x)
      (make-method
        (car x)
        (cadr x)
        (compute-interface (cadr x))
        (keyword-val '*comment* x)
        classname))))

(define compute-interface
  (lambda (x)
    (let f ((x x) (n 0))
    (cond
      [(null? x) n]
      [(symbol? x) (fx- -1 n)]
      [(pair? x) (f (cdr x) (fx+ n 1))]))))

(define compound-id
  (lambda (template . ids)
    (define id->string
      (lambda (x)
        (cond
          [(string? x) x]
          [(identifier? x) (id->string (syntax-object->datum x))]
          [(symbol? x) (symbol->string x)])))
    (datum->syntax-object
      template
      (string->symbol
        (apply string-append (map id->string ids))))))

(define-syntax with-fields
  (lambda (x)
    (syntax-case x ()
      [(_ x (struct id ...) e ...)
       (with-syntax ([(get ...)
                      (map (lambda (x) (compound-id #'struct #'struct "-" x))
                           #'(id ...))])
         #'(let ([z x]) (let ([id (get z)] ...) e ...)))])))

(define remove-omitted-methods
  (lambda (ls)
    (let loop ([ls ls] [rest '()])
      (if (null? ls)
          rest
          (loop (cdr ls)
                (if (memq (method-name (car ls)) omit-methods)
                    rest
                    (cons (car ls) rest)))))))

(define compute-methods
  (lambda ()
    (let loop ((cs (keyword-val 'class)) (ms '()))
      (if (null? cs)
          ms
          (loop (cdr cs)
                (append
                  (remove-omitted-methods (class-pub (car cs)))
                  ms))))))

(define compute-inherited-methods!
  (lambda (cls)
    (with-fields cls (class bname pub inherited-methods)
      (or inherited-methods
          (and (eq? bname '<base>)
               (begin (set-class-inherited-methods! cls '()) '()))
          (let ([pubnames (map method-name pub)])
            (let ([ims
                   (let ([base (find-class bname)])
                     (filter
                       (append
                         (compute-inherited-methods! base)
                         (class-pub base))
                       (lambda (m)
                         (and (not (memq (method-name m) pubnames))
                              (not (memq (method-name m) omit-methods))))))])
              (set-class-inherited-methods! cls ims)
              ims))))))

(define group-methods
  (lambda (method-field ls)
    (let lp ([ls ls] [cur #f] [tmp '()] [ans '()])
      (if (null? ls)
          (reverse (if (null? tmp) ans (cons tmp ans)))
          (let* ([first (car ls)]
                 [field (method-field first)])
            (if (eqv? cur field)
                (lp (cdr ls) field (cons first tmp) ans)
                (lp (cdr ls) field (list first)
                    (if (null? tmp) ans (cons tmp ans)))))))))

(define (interface<? i1 i2)
  (define (f i) (if (< i 0) (fx- -1 (* i 2)) (* i 2)))
  (< (f i1) (f i2)))

(define sort-methods
  (lambda (ls)
    (sort (lambda (m1 m2)
            (let ([s1 (symbol->string (method-name m1))]
                  [s2 (symbol->string (method-name m2))])
              (or (string<? s1 s2)
                  (and (string=? s1 s2)
                    (interface<?
                      (compute-interface (method-margs m1))
                      (compute-interface (method-margs m2)))))))
          
          ls)))

(define find-classes
  (lambda (key field)
    (let lp ([cs (keyword-val 'class)] [res '()])
      (if (null? cs)
          res
          (lp (cdr cs)
              (if (eq? key (field (car cs))) (cons (car cs) res) res))))))

(define find-class
  (lambda (cname)
    (ormap
      (lambda (x) (and (eq? cname (class-cname x)) x))
      (keyword-val 'class))))

(define uniq
  (lambda (ls)
    (if (null? ls)
        '()
        (if (memq (car ls) (cdr ls))
            (uniq (cdr ls))
            (cons (car ls) (uniq (cdr ls)))))))

(define find-subclasses
  (lambda (cname)
    (let ([class (find-class cname)])
      (or (class-sub-classes class)
          (let go ([workls (list class)] [ans '()])
            (if (null? workls)
                (begin (set-class-sub-classes! class ans) ans)
                (let ([subs (find-classes (class-cname (car workls)) class-bname)])
                  (go (append subs (cdr workls)) (append subs ans)))))))))

; except for cache could abstract w/ find-subclasses
; gack.
(define find-subclasses-excluding
  (lambda (cname exclude)
    (let ([class (find-class cname)])
      (let go ([workls (list class)] [ans '()])
        (if (null? workls)
            ans
            (let ([subs
                   (filter
                     (find-classes (class-cname (car workls)) class-bname)
                     (lambda (sub) (not (memq (class-cname sub) exclude))))])
              (go (append subs (cdr workls)) (append subs ans))))))))

(define find-api-subclasses
  (lambda (classname)
    (filter
      (find-subclasses classname)
      (lambda (c) (eq? (class-maker c) 'swl:api-class)))))

(define foldl
  (lambda (ls base fn)
    (if (null? ls) base (fn (car ls) (foldl (cdr ls) base fn)))))

(define filter
  (lambda (ls pred)
    (foldl ls '()
      (lambda (x rest)
        (if (pred x) (cons x rest) rest)))))

; should/could cache this stuff
; will be called on with syms containing lower-case alpha-numeric chars, <, >, !, :, ?, and -,
; or integers.  may have to do some compression here
(define conjure-label
  (lambda items
    (let ((op (open-output-string)))
      (for-each (lambda (s) (display s op)) items)
      (let ((s (get-output-string op)))
        (let lp ((i (fx- (string-length s) 1)))
          (unless (fx< i 0)
            (case (string-ref s i)
              [(#\<) (string-set! s i #\A)]
              [(#\>) (string-set! s i #\B)]
              [(#\!) (string-set! s i #\C)]
              [(#\:) (string-set! s i #\D)]
              [(#\?) (string-set! s i #\E)]
              [(#\-) (string-set! s i #\F)])
            (lp (fx- i 1))))
        s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Output routines
;;

(define print-tutorial
  (lambda (tut)
    (with-fields tut (tutorial name comment illustrates follows body)
      (section (format "\\tutorial{\\scheme{~a}}" name)
        (for-each
          (lambda (x) (printf "\\index{\\scheme{~a}!example}~n" x))
          illustrates)
        (for-each
          (lambda (thing)
            (if (and (pair? thing) (eq? (car thing) '*comment*))
                (for-each (lambda (x) (display x) (newline)) (cdr thing))
                (begin
                  (printf "\\schemedisplay~n")
                  (parameterize ([pretty-one-line-limit 80]
                                 [pretty-line-length 85])
                    (pretty-print thing))
                  (printf "\\endschemedisplay~n"))))
          body)))))

(define print-syntax
  (lambda (syn)
    (with-fields syn (syntax name comment)
      ;; the \endentryheader is introduced by the \ret{...} form
      ;; which must be present in the documentation comment
      (printf "\\entryheader~n")
      (for-each (lambda (x) (display x) (newline)) comment))))

(define print-procedure
  (lambda (proc)
    (with-fields proc (procedure name comment)
      ;; the \endentryheader is introduced by the \ret{...} form
      ;; which must be present in the documentation comment
      (printf "\\entryheader~n")
      (for-each (lambda (x) (display x) (newline)) comment))))

(define print-class
  (lambda (cls)
    (define (prmethods cname m*)
      (let ([m* (sort-methods m*)])
        (define (multiple? m)
          (let ([mname (method-name m)])
            (let f ([m* m*])
              (if (eq? (method-name (car m*)) mname)
                  (let ([m* (cdr m*)])
                    (and (not (null? m*))
                         (eq? (method-name (car m*)) mname)))
                  (f (cdr m*))))))
        (print-list m*
          (lambda (m)
            (let ([n (compute-interface (method-margs m))])
              (if (multiple? m)
                  (printf "~n\\hpageref{~a}{\\scheme{~a}(~a)}"
                    (conjure-label (method-name m) n cname)
                    (method-name m)
                    (if (< n 0) (format "~d+" (fx- -1 n)) n))
                  (printf "~n\\hpageref{~a}{\\scheme{~a}}"
                    (conjure-label (method-name m) n cname)
                    (method-name m))))))))
    (with-fields cls (class maker cname cargs comment pub)
      ;; the \endentryheader is introduced by the \ret{...} form
      ;; which must be present in the documentation comment
      (when (eq? maker 'swl:api-class)
        (printf "\\entryheader % ~a~n" cname)
        (printf "\\label{~a}~n" (conjure-label cname))
        (printf "\\formdef{~a}{class}{~a}~n" cname
          (list* 'create cname (pretty-args cargs)))
        (for-each (lambda (x) (display x) (newline)) comment)
        (unless (null? pub)
          (printf "\\methods{")
          (prmethods cname (remove-omitted-methods pub))
          (printf "}~n"))
        (let ((inherited (compute-inherited-methods! cls)))
          (unless (null? inherited)
            (printf "\\inhmethods{")
            (prmethods cname inherited)
            (printf "}~n")))))))

;; (define print-method
;;   (lambda (m)
;;     (with-fields m (method name margs comment classname)
;;       ;; the \endentryheader is introduced by the \ret{...} form
;;       ;; which must be present in the documentation comment
;;       (unless (null? comment)
;;         ; find API subclasses that inherit this method
;;         (let ([cs
;;                (filter
;;                  (cons (find-class classname) (find-subclasses classname))
;;                  (lambda (c) (eq? (class-maker c) 'swl:api-class)))])
;;           (unless (null? cs)
;;             (printf "% method: ~a  class: ~a~n" (cons name margs) classname)
;;             (printf "\\entryheader~n")
;; ;(do-label-hack name)
;;             (let ([args (pretty-args margs)])
;;               (printf "\\formdef{~a}{method}{~a}~n" name
;;                 (list* name "\\var{inst}" args))
;;               (printf "\\formdef{~a}{method}{~a}~n" name
;;                 (list* 'send "\\var{inst}" name args)))
;;             (for-each (lambda (x) (display x) (newline)) comment)
;;             (printf "\\classes{")
;;             (print-list cs
;;               (lambda (c) (printf "~n\\hpageref{~a}{\\scheme{~a}}"
;;                 (class-cname c) (class-cname c))))
;;             (printf "}~n")))
;;         (newline)))))

; should be unecessary once we get new version of hprep...
(define do-label-hack
  (lambda (name)
    (unless (getprop name 'already-labeled)
      (putprop name 'already-labeled #t)
      (printf "\\label{~a}~n" name))))

(define find-ret
  (lambda (string)
    (let ([ip (open-input-string string)])
      (let ([first (read ip)])
        (let ([second (read ip)])
          (and (eq? first 'ret) (eq? second '{) string))))))

; returns
; ((ret . ((classes . comments) ...)) ...)
(define make-doc cons)
(define doc-ret car)
(define doc-clsdocs cdr)
(define make-clsdoc cons)
(define clsdoc-classes car)
(define clsdoc-comments cdr)

(define process-method-comments
  ; - compute list of api-classes to which each comment applies
  ;     return the set difference of all ...
  ; - figure out what the return val is
  ; - strip out \ret{...} line (always first line in list of comment lines)
  (lambda (ms)
    (let ((ms (filter ms (lambda (m) (not (null? (method-comment m)))))))
      (let loop ((ms ms) (docs '()) (cs '()))
        (if (null? ms)
            (map (lambda (doc)                 ; gack, why recons.
                   (make-doc (doc-ret doc)
                         (map (lambda (x)
                                (let ([cname (car x)])
                                  (make-clsdoc
                                    (filter
                                      (cons (find-class cname)
                                            (find-subclasses-excluding cname (remq cname cs)))
                                      (lambda (c) (eq? (class-maker c) 'swl:api-class)))
                                    (clsdoc-comments x))))
                              (doc-clsdocs doc))))
                 docs)
            (with-fields (car ms) (method classname comment)
              (let ([ret (find-ret (car comment))])
                (let ([hit (assoc ret docs)])
                  (if hit
                      (begin
                        (set-cdr! hit
                          (cons (make-clsdoc classname (cdr comment))
                                (cdr hit)))
                        (loop (cdr ms) docs (cons classname cs)))
                      (loop (cdr ms)
                            (cons (make-doc
                                    ret
                                    (list (make-clsdoc
                                            classname
                                            (cdr comment))))
                                  docs)
                            (cons classname cs)))))))))))

; Current approach:
;   - partition methods into groups that have same \ret{return value}
;   - partition methods into groups that share the same documentation
;   - print list of classes for which the documentation holds
;   - print comments under that list

(define print-method-group
  (lambda (group)
    ; input is a list of methods w/ same name
    ; grouped into sublists that have the same interfaces
    (for-each
      (lambda (sublist)
        (unless (null? sublist)
          (let ([first (car sublist)])
            (if (andmap (lambda (x) (null? (method-comment x))) sublist)
                (warningf #f "no documentation for ~s with interface ~s"
                  (method-name first)
                  (method-interface first))
                (with-fields first (method name margs comment classname)
                  (let ([cs (uniq
                              (foldl sublist '()
                                (lambda (m rest)
                                   (let ((classname (method-classname m)))
                                     (cons (find-class classname)
                                           (append (find-api-subclasses classname) rest))))))])
                    (unless (null? cs)
                      (let ((mcs (process-method-comments sublist))) ; ((ret . ((classes . comments) ...)) ...)
                        (cond
                          [(ormap (lambda (x) (and (not (doc-ret x)) x)) mcs)
                           (warningf #f "missing \\ret{...} in documentation for ~s" name)]
                          [(not (null? (cdr mcs)))
                           (warningf #f "documented different return values for ~s in ~s" name
                             (foldl mcs '()
                               (lambda (doc rest)
                                 (foldl (doc-clsdocs doc) rest
                                   (lambda (clsdoc rest)
                                     (foldl (clsdoc-classes clsdoc) rest
                                       (lambda (c rest) (cons (class-cname c) rest))))))))]
                          [else
                           (printf "% method: ~a  class: ~a~n" (cons name margs) classname)
                          ; put the labels first, even if there are multipe docs
                           (printf "\\entryheader~n")
                           (for-each
                             (lambda (doc)
                               (for-each
                                 (lambda (clsdoc)
                                   (for-each
                                     (lambda (c)
                                       (printf "\\label{~a}~n"
                                         (conjure-label name (compute-interface margs) (class-cname c))))
                                     (clsdoc-classes clsdoc)))
                                 (doc-clsdocs doc)))
                             mcs)
                           (let ([args (pretty-args margs)])
                             (printf "\\formdef{~a}{method}{~a}~n"
                               name (list* name "\\var{inst}" args))
                             (printf "\\formdef{~a}{method}{~a}~n"
                               name (list* 'send "\\var{inst}" name args)))
                           (for-each
                             (lambda (doc)
                               (display (doc-ret doc)) (newline)
                               (for-each
                                 (lambda (clsdoc)
                                   (let ([classes (clsdoc-classes clsdoc)]
                                         [comments (clsdoc-comments clsdoc)])
                                     (cond
                                       [(null? classes) (void)]  ; must have been non-api classes
                                       [(null? comments)
                                        ";only way we can have this is if the documentation was a single line that specified \\ret{...}"
                                        (warningf #f "documentation for ~s method of ~s specifies return value only"
                                           name (map class-cname classes))]
                                       [else
                                        #;(for-each
                                          (lambda (c)
                                            (printf "\\label{~a}~n" (conjure-label name (compute-interface margs) (class-cname c))))
                                          classes)
                                        (printf "\\noindent~n")
                                        (for-each (lambda (line) (display line) (newline)) comments)
                                        (printf "\\classes{")
                                        (print-list (sort (lambda (x y) (string<? (symbol->string (class-cname x)) (symbol->string (class-cname y)))) classes)
                                          (lambda (c)
                                            (printf "~n\\hpageref{~a}{\\scheme{~a}}" (conjure-label (class-cname c)) (class-cname c))))
                                        (printf "}~n")])))
                                 (doc-clsdocs doc)))
                             mcs)])))))))
          (newline)))
      group)))

(define pretty-args
  (lambda (x)
    (cond
      [(null? x) '()]
      [(symbol? x) (format "\\var{~a} \\dots" x)]
      [(pair? x)
       (let ([rest (pretty-args (cdr x))])
         (cons (format "\\var{~a}" (car x))
               (if (or (pair? rest) (null? rest)) rest (list rest))))])))

;; This garbage is only needed because we haven't pushed the comment
;; documentation into syntax-object wraps.

(define-syntax module
  (syntax-rules ()
    [(_ (export ...) b1 b2 ...) (begin b1 b2 ...)]
    [(_ name (export ...) b1 b2 ...) (begin b1 b2 ...)]))

#; (ignore-syntax module)
(ignore-syntax define)
(ignore-syntax safety-check)
(ignore-syntax let-syntax)
(ignore-syntax letrec-syntax)
;; this must be last
(define-syntax define-syntax (lambda (x) (syntax (void))))

