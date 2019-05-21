;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;; This whole thing REALLY needs to be rewritten.
;; (especially now that we've scrapped the multiple backend idea here and
;;  moved this stuff into the prep.ss / hprep.ss code)
;;
;; If output seems to be truncated, we're probably missing a macro
;; definition.  Really need to move the application-specific transformers
;; to the application code.

;; The ignore-syntax garbage is only needed because we haven't pushed
;; the comment documentation into syntax-object wraps.

;; TODO
;;
;;  - scrap the old backend-case stuff (only using tex case for prep/hprep)
;;  - label and ref stuff needed
;;  - sorting of tutorials is broken..
;;  - stage front and back end so we can generate good HTML and good LaTeX
;;  - abstract the fine-tuning flags passed to barf-class
;;    and make it possible to specify these in the source documents
;;    (pragma?)
;;  - move macro definitions to application source
;;  - would be nice if output / sorting routines could be moved to application
;;    source
;;  - need macros that permit a high-level description of the appropriate
;;    output for the various documentation things and make it easy to access
;;    things like source, and different fields of the expression
;;  - hack reader?
;;  x inherit method documentation when method has no direct documentation

;; The idea right now is to munge the file to a temp file
;; and then run a new expander over it to expand the file into a
;; documentation structure.
;; (at some point could do this by composing ports?)
;; A final walk of that documentation structure thing should give us
;; a piece of documentation.

;; appears to be a hack to avoid having to read up strings from
;; the invoking shell
(case-sensitive #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Driver
;;

(define root-class '(swl:api-class))
  ;; what classes to document.  this is a list of the keywords
  ;; used to define the classes.  show this guy and his derived classes

(define omit-keywords
  ;; what keywords to omit.  this is a list of getters for the guys
  ;; we want to skip
  '(get-application-context
    get-parent
    get-balloon-help
    get-balloon-help-delay))

(define omit-methods
  ;; what methods to omit.  this is a list naming internal-use methods
  ;; this will probably go away once we have the module system in place
  '(mark-name
    print-rep
    set-parent!
    grid
    pack
    place
    adopt
    disown
    get-application-context
    set-balloon-help!
    get-balloon-help
    set-balloon-help-delay!
    get-balloon-help-delay
    ensure-configured-for
    tag-bind
    tag-ranges
    set-window-pos!
    set-window-size!
    rename-this-primitive-map
    y-position
    apply-parent-eval
    parent-eval
    scheme->tcl
    tag-range
    tag-config))

(define fsm   ; fold, spindle, mutilate
  (lambda (output mode files)
    ;; collect structured documentation in *anno* table.
    (for-each process-file (map symbol->string files))
    (with-output-to-file output (lambda () (make-documentation)) mode)))

(define fsm-generics
  (lambda (output files)
    (for-each do-file (map symbol->string files))
    (let ([op (open-output-file output 'truncate)])
      (format-classes (barf-generics op)))))

(define process-file
  ;; Rewrites source files with documentation comments embedded in list
  ;; structure of program.  Should modify reader to store documentation
  ;; comments in annotations and modify expander to provide convenient
  ;; access to these things.
  (lambda (filename)
    (define munge
      (lambda ()
        (define start
          (lambda (c f)
            (case c
              ((#!eof) (void))
              ((#\;)
               (read-char)
               (comment (peek-char) f))
              (else ;(
                (when f (write-char #\)) (newline))
                (write-char (read-char))
                (start (peek-char) #f)))))
        ; already read the #\;
        (define comment
          (lambda (c f)
            (case c
              ((#\*)
               (read-char)
               (doc-comment (peek-char) f))
              (else (write-char #\;)
                    (comment-help c f)))))
        (define comment-help
          (lambda (c f)
            (case c
              ((#\newline #\return)
               (write-char (read-char))
               (if f (doc-white (peek-char)) (start (peek-char) f)))
              (else
               (write-char (read-char))
               (comment-help (peek-char) f)))))
        ; already read #\; #\*
        (define doc-comment
          (lambda (c f)
            (unless f (printf "(*comment*~n  ")) ;)
            (write-char #\")
            (doc-string c)))
        (define doc-string
          (lambda (c)
            (case c
              ((#\newline #\return)
               (write-char #\")
               (write-char (read-char))
               (doc-white (peek-char)))
              ((#\" #\\)
               (write-char #\\)
               (write-char (read-char))
               (doc-string (peek-char)))
              (else
                (write-char (read-char))
                (doc-string (peek-char))))))
        (define doc-white
          (lambda (c)
            (if (char-whitespace? c)
                (begin (write-char (read-char))
                       (doc-white (peek-char)))
                (start (peek-char) #t))))
        (start (peek-char) #f)))
    (let ([op (open-output-string)])
      (parameterize ((current-output-port op))
        (with-input-from-file filename munge))
      (let ([ip (open-input-string (get-output-string op))])
        (let lp ()
          (let ((x (read ip)))
            (unless (eof-object? x) (expand x) (lp))))))))

;; since we're going to want to map / for-each the lists, return () if not found

(define keyword-val
  (case-lambda
    [(x) (keyword-val x *anno*)]
    [(x ls)
     (let loop ([ls ls])
       (cond
         [(null? ls) '()]
         [(and (pair? (car ls)) (eq? (caar ls) x)) (cdar ls)]
         [else (loop (cdr ls))]))]))

(define *anno* '())

(define annotate
  (lambda (what rec)
    (let ((x (assq what *anno*)))
      (if x
          (set-cdr! x (cons rec (cdr x)))
          (set! *anno* (cons (cons what (list rec)) *anno*))))))

(define-syntax ignore-syntax
  (lambda (form)
    (syntax-case form ()
      [(_ name)
       (syntax
         (define-syntax name
           (lambda (form)
             (syntax-case form ()
               [(k . anything) (syntax (begin . anything))]
               [x (syntax 'ignored)]))))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; documentation macros
;;

(define make-documentation)
(define-syntax documentation
  (syntax-rules ()
    [(_ e ...) (set! make-documentation (lambda () e ...))]))

(define-syntax chapter
  (syntax-rules ()
    [(_ name label e ...)
     (begin (printf "\\chapter{~a\\label{~a}}~n" name label) e ...)]))

(define-syntax section
  (syntax-rules ()
    [(_ name e ...)
     (begin (printf "\\section{~a}~n" name) e ...)]))

(define order
  (lambda (items field order)
    (let lp ([ls order] [result '()])
      (if (null? ls)
          (reverse result)
          (let ([hit
                 (ormap (lambda (x) (and (eq? (field x) (car ls)) x)) items)])
            (unless hit (assertion-violationf 'order "missing item for ~a" (car ls)))
            (lp (cdr ls) (cons hit result)))))))

(define print-list
  (lambda (ls print)
    (let loop ([ls ls] [n 0])
      (cond
        [(null? ls) (void)]
        [(null? (cdr ls)) (print (car ls))]
        [(null? (cddr ls))
         (print (car ls))
         (printf (if (> n 0) ", and " " and "))
         (print (cadr ls))]
        [else
         (print (car ls))
         (display ", ")
         (loop (cdr ls) (fx+ n 1))]))))

(define find-children
  (lambda (base ls is-parent?)
    (let loop ((ls ls) (kids '()) (other '()))
      (cond
        ((null? ls) (values kids other))
        ((is-parent? base (car ls))
         (loop (cdr ls) (cons (car ls) kids) other))
        (else 
         (loop (cdr ls) kids (cons (car ls) other)))))))

(define name<=?
  (lambda (thing-name)
    (lambda (x y)
      (string<=?
        (symbol->string (thing-name x))
        (symbol->string (thing-name y))))))




; ---------------------------- likely trash stuff below here...










;; eventually abstract this and format-classes, right now trying to get it done.

(define format-tutorials
  (lambda (format-tutorial)
    (let ([tutorials (keyword-val 'tutorial *anno*)])
      (let loop ([x (sort-tutorials 'intro tutorials)])
        (if (anno-tutorial? x)
            (format-tutorial x)
            (for-each (lambda (x) (loop x)) x))))))

(define format-syntactic-extensions
  (lambda (format-syntax)
    (let ([macros (keyword-val 'syntax *anno*)])
      (for-each format-syntax (sort (name<=? anno-syntax-name) macros)))))

(define format-procedures
  (lambda (format-procedure)
    (let ([procedures (keyword-val 'procedure *anno*)])
      (for-each
        format-procedure
        (sort (name<=? anno-procedure-name) procedures)))))

(define format-classes
  (lambda (format-class)
    (let ([classes (keyword-val 'class *anno*)])
      (let loop ([x (sort-classes '<tk-object> classes '())] [ind 0])
        (if (anno-class? x)
            (format-class x ind)
            (for-each (lambda (x) (loop x (+ ind 1))) x))))))

(define format-methods
  (lambda (format-method)
    (let ([methods (keyword-val 'method *anno*)])
      (for-each
        format-method
        (sort (name<=? anno-method-name) methods)))))
;; eventually abstract this and sort-classes, right now trying to get it done.

(define sort-tutorials
  (lambda (who tuts)
    (call-with-values
      (lambda () (find-children who tuts
                    (lambda (base x) (memq base (anno-tutorial-follows x)))))
      (lambda (kids others)
        (map (lambda (tut)
               (list tut (sort-tutorials (anno-tutorial-name tut) others)))
             (sort (name<=? anno-tutorial-name) kids))))))

(define sort-classes
  (lambda (who classes base-methods)
    (call-with-values
      (lambda () (find-children who classes
                   (lambda (base x)
                     (eq? base (anno-class-bname x)))))
      (lambda (kids others)
        (map (lambda (class)
               (set-anno-class-base-methods! class base-methods)
               (list class
                     (sort-classes
                       (anno-class-cname class)
                       others
                       (shadow base-methods (anno-class-pub class)))))
             (sort (name<=? anno-class-cname) kids))))))

(define meth-name-memq
  (lambda (x ls)
    (let ([s (anno-method-name x)])
      (let loop ([ls ls])
        (if (null? ls)
            #f
            (if (eq? s (anno-method-name (car ls)))
                (car ls)
                (loop (cdr ls))))))))

(define shadow
  (lambda (base sub)
    (let loop ((base base))
      (cond
        ((null? base) sub)
        ((meth-name-memq (car base) sub)
         (loop (cdr base)))
        (else (cons (car base) (loop (cdr base))))))))

(define format-class-toc
  (lambda (class ind)
    (printf (make-string ind #\space))
    (printf
      "class: ~s  base = ~s~n"
      (anno-class-cname class)
      (anno-class-bname class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the actual output routines
;;

(define barf-tutorial
  (lambda (op)
    (lambda (tut)
      (let ((name (anno-tutorial-name tut))
            (comment (anno-tutorial-comment tut))
            (body (anno-tutorial-body tut)))
            (emit-comment op "TUTORIAL: " name)
            (emit-index op name "using")
        (for-each
          (lambda (x) (emit-index op x "using"))
          (anno-tutorial-illustrates tut))
        (emit-heading op 'section "Using \\protect " (scheme-font name))
        ;; HACK ----------------------------------------------
        (for-each
          (lambda (thing)
            (if (and (pair? thing) (eq? (car thing) '*comment*))
                (foreach-in-environment op 'paragraph
                  (lambda (x) (emit-line op x))
                  (cdr thing))
                (foreach-in-environment op 'code
                  (lambda (x) (pretty-print x op))
                  (list thing))))
          body)
        (emit-skip op 'medium)))))

(define barf-syntax
  (lambda (op)
    (lambda (syntax)
      (let ((name (anno-syntax-name syntax))
            (comment (anno-syntax-comment syntax)))
        (in-environment op 'entry
          (lambda ()
            (emit-comment op "MACRO: " name)
            (emit-index op name)
            (emit-heading op 'section (scheme-font name))
            (foreach-in-environment op 'indent
              (lambda (x) (emit-line op x))
              comment)
            (emit-skip op 'medium)))))))

(define barf-procedure
  (lambda (op)
    (lambda (procedure)
      (let ((name (anno-procedure-name procedure))
            (comment (anno-procedure-comment procedure)))
        (in-environment op 'entry
          (lambda ()
            (emit-comment op "PROCEDURE: " name)
            (emit-index op name)
            (emit-heading op 'section (scheme-font name))
            (foreach-in-environment op 'indent
              (lambda (x) (emit-line op x))
              comment)
            (emit-skip op 'medium)))))))

(define barf-class
  (lambda (op makers omit-methods omit-keywords)
    (define getter?
      (lambda (methname)
        (let ((str (symbol->string methname)))
          (and (fx> (string-length str) 4)
               (string=? (substring str 0 4) "get-")))))
    (define getter->keyword
      (lambda (methname)
        (let ((str (symbol->string methname)))
          (string-append (substring str 4 (string-length str)) ":"))))
    (define grep-keywords
      (lambda (pubs omit-keywords)
        (let loop ([pubs pubs] [keys '()])
          (if (null? pubs)
              (sort string<=? keys)
              (let ([name (anno-method-name (car pubs))])
                (if (and (not (memq name omit-keywords)) (getter? name))
                    (loop (cdr pubs) (cons (getter->keyword name) keys))
                    (loop (cdr pubs) keys)))))))
    (lambda (class ind)
      (when (memq (anno-class-maker class) makers)
        (let ([comment (anno-class-comment class)]
              [cname (anno-class-cname class)]
              [cargs (anno-class-cargs class)]
              [bname (anno-class-bname class)]
              [bargs (anno-class-bargs class)]
              [ivars (anno-class-ivars class)]
              [inhtd (anno-class-inhtd class)]
              [ihtbl (anno-class-ihtbl class)]
              [priv (anno-class-priv class)]
              [prot (anno-class-prot class)]
              [pub (anno-class-pub class)]
              [base-methods (anno-class-base-methods class)])
          (let ((methods (shadow base-methods pub)))
            (in-environment op 'entry
              (lambda ()
                (emit-comment op "CLASS: " cname)
                (emit-formdef op 'class cname
                   ; this gets it right for all but the <menu> class
                   (if (memq 'init (map anno-method-name methods))
                       `(create ,cname . ,cargs)
                       `(make ,cname . ,cargs)))
                (in-environment op 'indent
                  (lambda ()
                    (foreach-in-environment op 'paragraph
                      (lambda (x) (emit-line op x))
                      comment)
                    (let ([keys (grep-keywords methods omit-keywords)])
                      (unless (null? keys)
                        (emit-skip op 'small)
                        (emit-hack op
                          "The following keywords may be used with the "
                          (scheme-font "create") ", "
                          (scheme-font "set-option!") ", and "
                          (scheme-font "get-option") " macros.")
                        (in-environment op 'indent
                          (lambda ()
                            (emit-setting op 'ragged-right)
                            (emit-list op keys schemefont)))))
    ;               (emit-item op #f "Base: " bname)
    ;               (emit-skip op 'small)
    ;               (unless (null? inhtd)
    ;                 (emit-item op #f "Inherited instance variables: " inhtd)
    ;                 (emit-list op inhtd schemefont)
    ;                 (emit-skip op 'small))
    ;               (emit-item op #f "Ivars: " (map car ivars))
    ;               (emit-skip op 'small)
    ;               (unless (null? ihtbl)
    ;                 (emit-item op #f "Inheritable instance variables: ")
    ;                 (emit-list op ihtbl schemefont)
    ;                 (emit-skip op 'small))
                    (unless (null? methods)
                      (emit-hack op (boldface "Methods: ")) ; was emit-item
                      (emit-list op (map anno-method-name methods) schemefont)
'                     (foreach-in-environment op 'indent
                        (barf-method op class omit-methods)
                        (sort (name<=? anno-method-name) methods)))))))))))))

(define barf-method
  (lambda (op class omit-methods)
    (lambda (method)
      (let ([comment (anno-method-comment method)]
            [name (anno-method-name method)]
            [args (anno-method-margs method)])
        (unless (memq name omit-methods)
          (in-environment op 'entry
            (lambda ()
              (emit-comment op "Method: " name)
              (emit-formdef op 'method name
                (if (generic? name)
                    (list* name 'instance args)
                    (list* 'send 'instance name args)))
              (let ([comment (if (null? comment)
                                 (find-method-documentation method class)
                                 comment)])
                (when (null? comment)
                  (warningf #f "no documentation for ~s method ~s"
                    (anno-class-cname class) name))
                (foreach-in-environment op 'paragraph
                  (lambda (x) (emit-line op x))
                  comment)))))))))

(define lookup-class
  (lambda (name)
    (let loop ([classes (keyword-val 'class *anno*)])
      (cond
        ((null? classes)
         (assertion-violationf 'lookup-class "class ~s not found" name))
        ((eq? name (anno-class-cname (car classes)))
         (car classes))
        (else (loop (cdr classes)))))))

(define find-method-documentation
  (lambda (method class)
    (let ((found (meth-name-memq method (anno-class-base-methods class))))
      (or (and found
               (let ([doc (anno-method-comment found)])
                 (if (null? doc)
                      (find-method-documentation method
                        (lookup-class (anno-class-bname class)))
                      doc)))
          '()))))

(define barf-generics
  (lambda (op)
    (lambda (class ind)
      (for-each
        (lambda (m)
          (pretty-print `(define-generic ,(anno-method-name m)) op))
        (anno-class-pub class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Back-end
;;

(define-syntax backend-case
  (lambda (x)
    (syntax-case x ()
      ((_) (assertion-violationf #f "No back end defined for target ~s" target))
      ((_ ((key) . body) . rest)
       (eq? (syntax-object->datum (syntax key)) target)
       (syntax (begin . body)))
      ((_ failed . rest)
       (syntax (backend-case . rest))))))

(define unfinished
  (lambda ()
    (assertion-violationf #f "back end ~a unfinished" target)))

(define emit-formdef
  (lambda (op what name example)
    (fprintf op "\\formdef{~a}{~a}{~a}~n" name what example)))

(define emit-item
  (lambda (op label . rest)
    (backend-case
      ((tex)
       (if label
           (begin (fprintf op "\\item[")
                  ((emit-tex op) label)
                  (fprintf op "]"))
           (fprintf op "\\item "))
       (for-each (emit-tex op) rest)
       (newline op))
      ((html) (unfinished)))))

(define emit-hack
  (lambda (op . rest)
    (for-each (emit-tex op) rest))) 

(define emit-line
  (lambda (op . rest)
    (backend-case
      ((tex)
       (for-each (emit-tex op) rest)
       (newline op))
      ((html)
       ";; Note: need to barf out a paragraph separator if line is blank ???"
       (unfinished)))))

(define emit-heading
  (lambda (op kind . what)
    (unless (memq kind '(chapter section subsection))
      (assertion-violationf 'emit-heading "unknown heading type ~s" kind))
    (backend-case
      ((tex)
       (fprintf op "\\~a{" kind)
       (display "\\protect " op)
       (for-each (emit-tex op) what)
       (fprintf op "}~n"))
      ((html)
       (unfinished)))))

(define emit-skip
  (lambda (op size)
'   (backend-case
      ((tex)
       (fprintf op "\\vspace{~aex}"
         (case size [(small) 0.25] [(medium) .5] [(large) 1])))
      ((html)
       (unfinished)))))

(define emit-comment
  (lambda (op . what)
    (backend-case
      ((tex)
       (fprintf op "~n%%%     ")
       (for-each (emit-tex op) what)
       (newline op))
      ((html)
       (unfinished)))))

(define emit-setting
  (lambda (op what)
    (backend-case
      ((tex)
       (case what
         ((ragged-right) (fprintf op "\\raggedright~n"))))
      ((html)
       (unfinished)))))

;; If subheadings are specified, the intent is to write them as
;; subheadings in the index:
;;
;;    blivits  76, 129
;;       using  101
;;       habitat  221

(define emit-index
  (lambda (op what . subheading)
    (backend-case
      ((tex)
       (fprintf op "\\index{")
       ((emit-tex op) (scheme-font what))
       (if (null? subheading)
           (fprintf op "}%~n")
           (begin (fprintf op "! ")
                  (for-each
                    (let ((emit (emit-tex op)))
                      (lambda (x) (emit (scheme-font x))))
                    subheading)
                  (fprintf op "}%~n"))))
      ((html) (unfinished)))))

(define foreach-in-environment
  (lambda (op name f ls)
    (unless (null? ls)
      (in-environment op name (lambda () (for-each f ls))))))

(define in-environment
  (lambda (op name k)
    (backend-case
      ((tex)
       (fprintf op "{~n")
       (case name
         [(entry) (fprintf op "\\entryheader~n")]
         [(paragraph) (void)]
         [(code) (fprintf op "\\schemedisplay~n")]
         [(indent)
'         (fprintf op
            "\\begin{list}{
                           \\setlength{\\leftmargin}{3ex}
                           \\setlength{\\listparindent}{0pt}}\\item~n")]
         [else (void)])
       (k)
       (case name
         [(entry) (fprintf op "\\endentryheader~n~n")]
         [(paragraph) (void)]
         [(code) (fprintf op "\\endschemedisplay~n")]
         [(indent)
'(fprintf op "\\end{list}~n")]
         [else (void)])
       (fprintf op "}~n"))
      ((html) (unfinished)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Backend helpers
;;

(define-syntax typeface? (identifier-syntax procedure?))

(define-syntax apply-typeface
  (syntax-rules ()
    ((_ op x) (x op))))

(define-syntax make-typeface
  (syntax-rules ()
    ((_ (op x) . body)
     (lambda (x) (lambda (op) . body)))))

(define boldface
  (make-typeface (op x)
    (backend-case
      ((tex)
; we're inserting a \def for it, so I don't know why this doesn't work...
; \def is in ref.stex and href.stex
       (fprintf op "\\mybf{")
       ((emit-tex op) x)
       (fprintf op "}"))
      ((html)
       (unfinished)))))

(define italic
  (make-typeface (op x)
    (backend-case
      ((tex)
       (fprintf op "{\\it ")
       ((emit-tex op) x)
       (fprintf op "}"))
      ((html)
       (unfinished)))))

(define scheme-font
  (make-typeface (op x)
    (backend-case
      ((tex)
       (fprintf op "\\scheme{")
       ((emit-tex op) x)
       (fprintf op "}"))
      ((html)
       (unfinished)))))

;; Eventually this could be more clever and actually look at the
;; x it's given.
(define schemefont scheme-font) 

;; < > { and }  are handled by JohnZ's latex preprocessor
;; which also converts the various @i @s @t etc. to their
;; TeX equivalents.  Eventually handle this using inparse.

(define write-tex-safe-string
  (let ([needs-escape?
         (lambda (ch)
           (memv ch '(#\$ #\& #\% #\^ #\_)))])    ; #\# and #\\ must be handled by user in documentation comments
    (lambda (string output-port)
      (let loop ([i 0] [len (string-length string)])
        (unless (fx= i len)
          (let ([ch (string-ref string i)])
            (when (needs-escape? ch) (display #\\ output-port))
            (display ch output-port)
            (loop (fx+ i 1) len)))))))

(define emit-tex-item
  (lambda (x op)
    (cond
      [(string? x) (write-tex-safe-string x op)]
      [(symbol? x) (write-tex-safe-string (symbol->string x) op)]
      [(boolean? x) (write-tex-safe-string (if x "#t" "#f") op)]
      [(typeface? x) (apply-typeface op x)]
      [else (display x op)])))

(define emit-tex
  (lambda (op)
    (lambda (x)
      (cond
        [(pair? x)
         (display "(" op)
         (let loop ([x x])
           (cond
             [(null? x) (display ")" op)]
             [(pair? x)
              (emit-tex-item (car x) op)
              (cond
                [(null? (cdr x)) (display ")" op)]
                [else (display " " op) (loop (cdr x))])]
             [else
              (display " . " op)
              (emit-tex-item x op)
              (write-char #\) op)]))]
        [else (emit-tex-item x op)]))))

;; This is pretty doofus.  emit-it is a thing that emits an item.
;; From the funky application it's clear that it's intended to be
;; one of these boldface, italic, or whatever typeface procs.

(define emit-list
  (lambda (op ls emit-it)
    (let loop ((ls ls) (n 0))
      (cond
        ((null? ls) (void))
        ((null? (cdr ls)) ((emit-it (car ls)) op))
        ((null? (cddr ls))
         ((emit-it (car ls)) op)
         (fprintf op (if (> n 0) ", and " " and "))
         ((emit-it (cadr ls)) op))
        (else ((emit-it (car ls)) op)
              (fprintf op ", ")
              (loop (cdr ls) (fx+ n 1)))))))

;; for now let's just make 4 columns, eventually, try to determine best
;; number of columns?
';; COMMENTED OUT
(let-syntax
  ((num-cols (identifier-syntax 5)))
  (define list->table
    (lambda (ls op)
      (define barf-item
        (lambda (what op i)
          (fprintf op
            (cond
              [(fxzero? i) "~a "]
              [(fx= (fx+ i 1) num-cols) "& ~a \\\\~n"]
              [else "& ~a "])
            what)))
      (fprintf op "\\begin{tabular}{*{~a}{p{1.5in}}}~n" num-cols)
      (let loop ([ls ls] [i 0])
        (cond
          [(null? ls)
           (unless (fx= i num-cols)
             (barf-item "" op i)
             (loop '() (fx+ i 1)))]
          [else
           (barf-item (car ls) op i)
           (loop (cdr ls) (fxmodulo (fx+ i 1) num-cols))]))
      (fprintf op "\\end{tabular}~n"))))

