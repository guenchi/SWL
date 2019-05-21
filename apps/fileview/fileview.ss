;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define test
  (lambda (filename)
    (set! fv
      (create <fileview>
        (create <toplevel> with (title: "FileView"))
        filename))))

;; Todo
;;  - Should catch destroy message for control-panel and set flag back
;;  - sc viewer
;;      - do something w/ source of ref and binding, needs help w/ markups
;;         - highlight the matching reference / binding when a
;;           binding / reference is passed over with the mouse
;;             - to minimize the number of tags needed, create a special tag for
;;               each binding site (which highlights that site) and apply that
;;               tag to all references.
;;             - similarly create tag that highlights all the refs
;;      - pretty-printing of source
;;
;;  * BUG:  hide method on frame doesn't do what it needs to
;;  * BUG:  the statline frame isn't getting packed right because
;;          when it resizes it can be hidden (scrollframe being greedy?)
;;  + need paren-balancing code
;;  - more viness:
;;     - regexp searching
;;     - n and N to go next/prev
;;  ? should set-height! and set-width! operate on the same basis as for texts?
;;  x put search back into text widget
;;  - may eventually want the application classes to be derived from some
;;    larger application class that has appropriate methods ...
;;  - abstract the scrolling textview stuff from fileview into something
;;    other folks can use
;;  ? eventually add horizontal scrollbar when lines wrap
;;  ? eventually make vertical scrollbar disappear when not needed

(require "cmacros.so")
(require "../common/scrollframe.ss")
(require "../common/optionbox.ss")
(require "../common/selectbox.ss")

(define source-info-filename
  (lambda (x)
    (and (pair? x)
         (let ((x (cdr x)))
           (if (vector? x)
               (vector-ref x 1)    ;; #2(annotation "lsdkfldsjf")
               x)))))
(define source-info-filepos car)
(define source-info-modified cddr)

;; for current hack, move these guys above the fileview class
;; eventually, move these guys into some kind of structure that
;; can be passed around or something.

(define referenced-markup (create <markup> with (border-width: 2)))
(define assigned-markup (create <markup> with (border-width: 2)))
(define referenced-call-markup (create <markup> with (border-width: 2)))
(define call-markup (create <markup> with (border-width: 2)))
(define if-markup (create <markup> with (border-width: 2)))
(define symref-markup (create <markup> with (border-width: 2)))
(define primref-markup (create <markup> with (border-width: 2)))
(define set-markup (create <markup> with (border-width: 2)))
(define define-markup (create <markup> with (border-width: 2)))
(define case-lambda-markup (create <markup> with (border-width: 2)))
(define quote-markup (create <markup> with (border-width: 2)))
(define seq-markup (create <markup> with (border-width: 2)))
(define rec-binding-markup (create <markup> with (border-width: 2)))
(define foreign-markup (create <markup> with (border-width: 2)))
(define hilite-markup (create <markup> with (background-color: 'yellow)))

;; Should catch destroy message and set flag back
(define show-control-panel
  (let ((top #f))
    (lambda (text)
      (thread-critical-section
        (if top
            (send top raise)
            (let* ((tl (create <toplevel> with (title: "Control Panel")))
                   (sf (create <scrollframe> tl))
                   (sel (create <selectbox> sf))
                   (o1 (create <option-box> tl
                         `(("Normal" .
                           ,(get-font text))
                           ("Bold" .
                           ,(make-font (get-font text) (add: '(bold))))
                           ("Italic" .
                           ,(make-font (get-font text) (add: '(italic))))
                          set-font!
                          (lambda (m) (or (get-font m) (get-font text)))
                          with (title: "Font")
                               (relief: 'sunken)
                               (border-width: 3)))
                   (o2 (create <option-box> tl
                         `(("Normal" . ,(get-foreground-color text))
                           ("Red" . red)
                           ("Green" . darkgreen)
                           ("Blue" . blue)
                           ("Yellow" . yellow)
                           ("White" . white))
                          set-foreground-color!
                          (lambda (m)
                            (or (get-foreground-color m)
                                (get-foreground-color text)))
                          with (title: "Foreground")
                               (relief: 'sunken)
                               (border-width: 3)))
                   (o3 (create <option-box> tl
                         `(("Normal" . ,(get-background-color text))
                           ("Red" . red)
                           ("Green" . darkgreen)
                           ("Blue" . blue)
                           ("Yellow" . yellow)
                           ("White" . white)
                           ("Black" . black))
                          set-background-color!
                          (lambda (m)
                            (or (get-background-color m)
                                (get-background-color text)))
                          with (title: "Background")
                               (relief: 'sunken)
                               (border-width: 3)))
                   (o4 (create <option-box> tl
                          '(("Normal" . flat)
                            ("Raised" . raised)
                            ("Sunken" . sunken)
                            ("Ridge" . ridge)
                            ("Groove" . groove))
                          set-relief!
                          (lambda (m) (or (get-relief m) 'flat))
                          with (title: "Relief")
                               (relief: 'sunken)
                               (border-width: 3)))
                   (select (lambda (mkup)
                             (send o1 operate-on mkup)
                             (send o2 operate-on mkup)
                             (send o3 operate-on mkup)
                             (send o4 operate-on mkup))))
            (for-each (lambda (label mkup)
                        (send sel add-choice label (lambda () (select mkup))))
              '("Value Referenced" "Function Referenced" "Assigned"
                "#(call ...)" "#(if ...)" "#(symref ...)" "#(primref ...)"
                "#(set ...)" "#(define ...)" "#(case-lambda ...)"
                "#(quote ...)" "#(seq ...)" "#(rec-binding ...)"
                "#(foreign ...)")
              (list referenced-markup referenced-call-markup assigned-markup
                    call-markup if-markup symref-markup primref-markup
                    set-markup define-markup case-lambda-markup quote-markup
                    seq-markup rec-binding-markup foreign-markup))
            (pack sf (side: 'left) (expand: #t) (fill: 'y))
            (pack o1 (side: 'left) (expand: #t) (fill: 'y))
            (pack o2 (side: 'left) (expand: #t) (fill: 'y))
            (pack o3 (side: 'left) (expand: #t) (fill: 'y))
            (pack o4 (side: 'left) (expand: #t) (fill: 'y))
            (set! top tl)))))))


;; whatever this does (if it ever does anything) we can move it
;; to the common area  (don't want to move it until we have a name)

(define-swl-class (<nebula>) (<base>)
  ;* hypothetical base class for all nebula apps.
  ;* might have methods useful to the scheme window manager, etc.
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))


;; note, when doing the regexp search, need to
;; escape chars like ( and ) which are used for
;; regexps
;;
;;  (would like to have something mimicking vi)

(define-generic set-key-press-method!)
(define-generic get-key-press-method)

(load "../common/flex-text.ss")

(define-swl-class (<fileview> parent filename) (<nebula>)
  ;* displays a file in the window context given by parent
  (ivars (frame #f) (text #f) (label #f) (entry #f)
         (filename #f)
         (lastsearch #f) (searchopts '(forward)))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (parent fname)
     (set! filename fname)   ;; last minute hacking while trying to babysit
     (set! frame (create <frame> parent))
     (let ([scrframe (create <scrollframe> frame)]
           [statline (create <frame> frame)])
       (set! label (create <label> statline))
       (set! entry (create <entry> statline))
       (set! text (create <flex-text> scrframe
                     with (insert-width: 3) (wrap: 'none)))
       (send self view fname '(0 . 0))
       (pack frame (fill: 'both))
       (pack scrframe (expand: #t) (side: 'top) (fill: 'both))
       (pack statline (side: 'top) (anchor: 'w))
       (pack (create <button> statline
               with
                 (title: "Control Panel")
                 (action: (lambda (self) (show-control-panel text))))
             (side: 'left)))
     (pack label (side: 'left) (anchor: 'w))
     (pack entry (side: 'left) (anchor: 'w))
     (set-cursor-pos! text '(0 . 0))
     (set-focus text)
     (set-key-press-method! text
       (lambda (text key mods)
         (event-case ((key= key) (modifier= mods))
           (([#\/])
            (set! searchopts '(forward))
            (set-focus entry))
           (([#\?])
            (set! searchopts '(backward))
            (set-focus entry))
           (([prior]) (vscroll text -1 'pages))
           (([next]) (vscroll text 1 'pages))
           (([left] [#\h]) (move-char text -1))
           (([right] [#\l]) (move-char text 1))
           (([up] [#\k]) (move-line text -1))
           (([down] [#\j]) (move-line text 1))
           (([#\n]) (send self find-next)))))
     (set-action! entry
       (lambda (entry)
         (send text find (get-string entry) 'insert)
         (delete-all entry)
         (set-focus text)))
     (thread-fork (lambda () (send self markup)))]
    [markup ()
     (when (string? filename)
       (time (let ([sp (open-input-file filename)])
               (let loop ([x (read sp #t)])
                 (unless (eof-object? x)
                   (annotate (sc-expand x #t) text)
                   (loop (read sp #t)))))))]
    [view (fname position)
     (when (string? fname)
       (set! filename fname)
       (delete-all text)
       (read-file! filename text)
       (make-visible text position)
       (set-cursor-pos! text position)
       (set-title! label filename))]
    [find (string start-pos)
     (when string
       (set! lastsearch string)
       (let ((p (search text string start-pos searchopts)))
         (when p (set-cursor-pos! text p) (make-visible text p))))]
    [find-next ()
     (let ([p (get-cursor-pos text)])
       (send self find lastsearch
         (add-offset text p (if (memq 'backward searchopts) -1 1))))]
    [hide () (when frame (send frame hide))]
    [show () (when frame (send frame show))]))

(define read-file!
  (lambda (filename txt)
    (let ([buf (make-string 2048)])
      (let loop ([ip (open-input-file filename)])
        (let ([x (block-read ip buf 2048)])
          (unless (eof-object? x)
            (insert txt (if (< x 2048) (substring buf 0 x) buf))
            (loop ip)))))))

(define ref-call-font (create <font> 'courier 12 '(bold roman)))

(define-syntax markup-sexp
  (syntax-rules ()
    [(_ markup widget source)
     (let ([s source])
       (when (pair? s)
         (let* ([filename (source-info-filename s)]
                [start (source-info-filepos s)]
                [end
                 (let ([ip (open-input-file filename)])
                   (file-position ip start)
                   (read ip)
                   (let ([end (file-position ip)])
                     (close-input-port ip)
                     end))])
           (apply-markup markup widget
             (send widget add-offset '(0 . 0) start)
             (send widget add-offset '(0 . 0) end)))))]))

(define markup-call
  (lambda (widget source)
    (markup-sexp call-markup widget source)))

(define markup-if
  (lambda (widget source)
    (markup-sexp if-markup widget source)))

(define-swl-class (<binding-site> plex widget) (<base>)
  (ivars
    (refs '()) (plex plex) (widget widget) (len #f) (markup #f) (ref-markup #f))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (plex widget)
     (let ((src (prelex-source plex)) (name (prelex-name plex)))
        (when (pair? src)
          (set! len (string-length (symbol->string name)))
          (letrec ((mkup (create <markup> with
                           (border-width: 2)
                           (mouse-enter-method:
                             (lambda (mkup text x y mods)
                               (send self for-refs apply-markup)))
                           (mouse-leave-method:
                             (lambda (mkup text x y mods)
                               (send self for-refs remove-markup))))))
             (let* ([filename (source-info-filename src)]
                    [i (add-offset widget '(0 . 0) (source-info-filepos src))]
                    [j (add-offset widget i len)]
                    [refmk (create <markup> with
                             (mouse-enter-method:
                               (lambda (self text x y mods)
                                 (apply-markup hilite-markup widget i j)))
                             (mouse-leave-method:
                               (lambda (self text x y mods)
                                 (remove-markup hilite-markup widget i j))))])
               (apply-markup mkup widget i j)
               (when (prelex-value-ref plex)
                 (apply-markup referenced-markup widget i j))
               (when (prelex-fun-ref plex)
                 (apply-markup referenced-call-markup widget i j))
               (when (prelex-assigned plex)
                 (apply-markup assigned-markup widget i j))
               (set! markup mkup)
               (set! ref-markup refmk)))))]
    [get-ref-markup () ref-markup]
    [add-ref (source) (set! refs (cons source refs))]
    [for-refs (markup-op)
     (for-each
       (lambda (source)
         (when (pair? source)
           (let ([filename (source-info-filename source)]
                 [i (add-offset widget '(0 . 0) (source-info-filepos source))])
             (markup-op hilite-markup widget i (add-offset widget i len)))))
       refs)]))

(define get-binding-site
  (lambda (plex env)
    (let ((x (assq plex env)))
      (unless x
        (printf "Warning:  couldn't find ~s in env~n" plex)
        (cdr x))
      (and x (cdr x)))))

(define markup-ref
  (lambda (widget source plex env)
    (when (pair? source)
      (let ((i (add-offset widget '(0 . 0) (source-info-filepos source))))
        (let ((name (prelex-name plex)))
           (let* ((len (string-length (symbol->string name)))
                  (j (add-offset widget i len)))
             (let ((binding-site (get-binding-site plex env)))
               (when binding-site
                 (let ((ref-mkup (send binding-site get-ref-markup)))
                   (send binding-site add-ref source)
                   (when ref-mkup (apply-markup ref-mkup widget i j)))))
             (when (prelex-value-ref plex)
               (apply-markup referenced-markup widget i j))
             (when (prelex-fun-ref plex)
                (apply-markup referenced-call-markup widget i j))
             (when (prelex-assigned plex)
                (apply-markup assigned-markup widget i j))))))))

(define markup-set
  (lambda (widget source)
    (markup-sexp set-markup widget source)))

(define markup-define
  (lambda (widget source)
    (markup-sexp define-markup widget source)))

(define markup-symref
  (lambda (widget source)
    (markup-sexp symref-markup widget source)))

(define markup-case-lambda
  (lambda (widget source)
    (markup-sexp case-lambda-markup widget source)))

(define markup-primref
  (lambda (widget source)
    (markup-sexp primref-markup widget source)))

(define markup-quote
  (lambda (widget source datum)
    (markup-sexp quote-markup widget source)))

(define markup-seq
  (lambda (widget source)
    (markup-sexp seq-markup widget source)))

(define markup-rec-binding
  (lambda (widget source)
    (markup-sexp rec-binding-markup widget source)))

(define markup-foreign
  (lambda (widget source)
    (markup-sexp foreign-markup widget source)))


(define annotate
  (lambda (x widget)
    (define extend-env
      (lambda (f v e)
        (if (null? v)
            e
            (cons (cons (car v) (f (car v))) (extend-env f (cdr v) e)))))
    (define annotate-clause
      (lambda (env)
        (lambda (clause)
          (let ([formals (car clause)]
                [interface (cadr clause)]
                [body (caddr clause)])
            (let ([env
                   (extend-env
                     (lambda (plex) (create <binding-site> plex widget))
                     formals
                     env)])
              (annotate body env))))))
    (define annotate
      (lambda (x env)
        (if (not (vector? x))
          (begin (printf "Warning: annotate encountered non-vector ~s~n" x)
                 x)
          (c-record-case x
            ((call) (source fun args)
             (markup-call widget source)
             (annotate fun env)
             (for-each (lambda (x) (annotate x env)) args))
            ((if) (source test then else)
             (markup-if widget source)
             (annotate test env)
             (annotate then env)
             (annotate else env))
            ((ref) (source var)
             (markup-ref widget source var env))
            ((set) (source var exp)
             (markup-set widget source)
             (annotate exp env))
            ((symset) (source type var exp)
             (markup-set widget source)
             (annotate exp env))
            ((symref) (source type var)
             (markup-symref widget source))
            ((case-lambda) (source libspec clauses)
             (markup-case-lambda widget source)
             (for-each (annotate-clause env) clauses))
            ((primref) (source opt-lvl var flags)
             (markup-primref widget source))
            ((quote) (src datum)
             (markup-quote widget src datum))
            ((seq) (e1 e2)                     ;; no source for now
             (markup-seq widget #f)
             (annotate e1 env)
             (annotate e2 env))
            ((rec-binding) (source vars val-exps body)
             (markup-rec-binding widget source)
             (let ((env (extend-env
                          (lambda (plex) (create <binding-site> plex widget))
                          vars
                          env)))
               (for-each (lambda (x) (annotate x env)) val-exps)
               (annotate body env)))
            ((foreign) (source who params result)
             (markup-foreign widget source))
            (else (assertion-violationf 'annotate "forgot to handle ~a~n" x))))))
    (annotate x '())))

