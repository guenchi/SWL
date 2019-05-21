;; Copyright (c) 1996 Carl Bruggeman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;; NetPorte "Doorway to the Net" (Porte is french for ``door'')
;;;
;;; Carl Bruggeman 1996
;
;  BUGS:
;   0) doctype / comment stuff not being stripped  <! ...>  ??
;   1) get OS support in static libraries for ``resolve''
;   2) (collect-notify #t) causes xthread to hang (interaction with NB IO)
;   3) get Oscar to do scroll by "place" rather than pack to see if slow
;      scrolling with embedded pictures is faster
;   4) better bullet symbol than "mid-dot"
;   5) use non-blocking socket operations and fork a process to do the
;      gethostbyname calls so that the application never blocks
;   6) left margin doesn't seem to work right (need to apply margin,
;      above, below, line-spaceing to whole paragraphs rather than newlines??)
;   7) PRE still needs parser support
;   8) non-html files should be processed as text
;   9) underline on 12 pt and sometimes on 14 point looks too thick
;  10) limited to one form per page
;  11) no support for multiple selections (need list box)
;  12) align middle ==> treat as center
;  13) <PRE> ==> flag passed to rd-html-token to say whether in <PRE> mode
;       -- PRE mode just puts out spaces and newlines verbatim but still
;          allows markups to be recognized
;
;  TODO:
;   * recognize and download things like .ps .tar .gz
;   * set image size things in advance and fill later
;   * use fancier HTTP protocol that lets us have progress meter, etc.
;   1) Caching at two additional levels:
;      a) cache parsed HTML trees incore
;      b) cache windows (html-view objects)
;      c) profile to see if it would help to cache markups
;         (need time-based profiler)
;   2) Support additional constructs
;      a) Table support
;      b) Forms support (including ISMAP and ISINDEX)
;      c) Frames
;   3) need to recompute when the window is resized:
;        - hrule
;        - others?  (centering ?)
;   4) Additional image support
;      -additional types: jpeg, etc.
;      -additional attributes: shape, area, map, etc.
;   6) Bookmarks (with visual recently-visited links)
;   7) add inlined Scheme markups

;; Gack.
(define left-arrow-bitmap
"#define larrow_width 18
#define larrow_height 18
static unsigned char larrow_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x18, 0x00, 0x00, 0x1e, 0x00, 0x80, 0x1f, 0x00, 0xe0, 0x07, 0x00,
   0xf8, 0x01, 0x00, 0xf8, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x80, 0x1f, 0x00,
   0x00, 0x1e, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};")
(define right-arrow-bitmap
"#define rarrow_width 18
#define rarrow_height 18
static unsigned char rarrow_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x60, 0x00, 0x00, 0xe0, 0x01, 0x00, 0xe0, 0x07, 0x00, 0x80, 0x1f, 0x00,
   0x00, 0x7e, 0x00, 0x00, 0x7c, 0x00, 0x80, 0x1f, 0x00, 0xe0, 0x07, 0x00,
   0xe0, 0x01, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};")

;(require "../common/scrollframe.ss")
(require "../common/flex-button.ss")
(require "html.ss")
(require "www.ss")
(require "MRA.ss")
(require "file-type.ss")

(define-syntax push!
  (lambda (x)
    (syntax-case x ()
      [(_ var val)
       (with-syntax ([set! (datum->syntax-object (syntax var) 'set!)])
         (syntax (set! var (cons val var))))])))

(define-syntax pop!
  (lambda (x)
    (syntax-case x ()
      [(_ var)
       (with-syntax ([set! (datum->syntax-object (syntax var) 'set!)])
         (syntax (when (pair? var) (set! var (cdr var)))))])))

(define-syntax top-val
  (lambda (x)
    (syntax-case x ()
      [(_ var) (syntax (if (pair? var) (car var) var))])))

(define-swl-class (<html-view> parent browser) (<text> parent)
  (ivars [browser browser]
         [base-URL #f]
         [disp-cnt 40] [disp-rows 50]
         ;; [bullet (string (integer->char 183))]
         [bullet "*"]
         [default-background-color #f] [width 1]

         [bk-thread #f]
         [background (thread-make-msg-queue 'html-background)]
         [entry-points '()]

         [markup-position #f]
         [left-margin 30] [par 0] [line-format #f] [line-space 0]
         [anchor #f] [frag #f] [item #f] [underline #f] [color #f]
         [italic #f] [bold #f] [font-size 0] 
         [fixed-width #f] [pre? #f]

         [title #f] [option #f] [sel-name #f] [sel-value #f]

         [default-line-space 0]
         [P-above 4]
         [HR-above 5]  [HR-below 5]
         [H1-above 12] [H1-below 8] [H1-lm 2]
         [H2-above 8]  [H2-below 4]  [H2-lm 4]
         [H3-above 4]  [H3-below 2]  [H3-lm 12]
         [H4-above 3]  [H4-below 1]  [H4-lm 18]
         [H5-above 3]  [H5-below 0]  [H5-lm 18]
         [H6-above 3]  [H6-below 0]  [H6-lm 22]

         [form-action #f] [form-method #f] [form-env #f]
         [def-input-size 40]
         [def-input-rows 4]
         [def-input-cols 60]
  )
  (inherited)
  (inheritable)
  (private
    [flush-markup (fixed?)
     (let ([pos (get-cursor-pos self)]
           [size (top-val font-size)]
           [an (top-val anchor)]
           [lm (top-val left-margin)])
;(fprintf panic-op "flush-markup: ~s~%" fixed?)
       (when (and markup-position (not (send self pos=? pos markup-position)))
         (let ([mkup (create <markup>)]
               [font (get-font self)])
           (when an
             (set-mouse-press-method! mkup
               (lambda (mkup text x y mods)
                 (event-case ((modifier= mods))
                   [([left-button])
;(fprintf panic-op "~s~%" an)
                    (send browser history-add)
                    (send browser view-URL an)]
                   [else (void)])))
             (set-mouse-enter-method! mkup
               (lambda (mkup text x y mods)
                 (set-mouse-cursor! self 'hand2)
                 (send browser display-status (URL->string an))))
             (set-mouse-leave-method! mkup
               (lambda (mkup text x y mods)
                 (set-mouse-cursor! self 'left_ptr)
                 (send browser display-status ""))))
           (when (top-val frag)
             (set! entry-points (cons (cons (top-val frag) pos) entry-points))
             (pop! frag))
           (set-left-margin! mkup lm)
           ; this seems to help margin, but may be indenting too far somehow.
           (set-paragraph-indent! mkup lm)
           ; shouldn't this be case, carl?
           (cond
             [(memq (top-val line-format) '(UL OL MENU))
              (let ([stop-1 (max 0 (- lm 10))] [stop-2 lm])
                (set-tabs! mkup (tab-stops (right: stop-1) (left: stop-2))))]
             [(memq (top-val line-format) '(DL))
              (let ([stop-1 (+ 30 lm)])
                (set-tabs! mkup (tab-stops (left: stop-1))))]
             [(memq (top-val line-format) '(CENTER))
              (let ([w width])
                (set-tabs! mkup (tab-stops (center: (quotient w 2)))))]
             [#t ;;(eq? (top-val line-format) 'P)
'             (set-paragraph-indent! mkup (+ (top-val par) lm))]
             [else
              (assertion-violationf 'flush-markup "invalid line format: ~s" line-format)])
           (unless (= (top-val line-space) default-line-space)
             ;;;(set-line-space! mkup (top-val line-space))
             ;;;(set-below-paragraph-space! mkup (top-val line-space))
             ;;(set-above-paragraph-space! mkup (top-val line-space))
             (void))

           (with-values (send font get-values)
             (lambda (family ignored style)
               (let ([style
                      (list (if (or (top-val bold) (memq 'bold style))
                                'bold
                                'normal)
                            (if (or (top-val italic) (memq 'italic style))
                                'italic
                                'roman))]
                     [family
                      (if (or fixed? (top-val fixed-width)) 'courier family)]
                     [points
                      (case (top-val size)
                        [(3) 24]
                        [(2) 18]
                        [(1) 14]
                        [(0) 12]
                        [(-1) 10]
                        [else 8])])
                 (set-font! mkup (create <font> family points style)))))

           (when (top-val color) (set-foreground-color! mkup (top-val color)))
           (when (top-val underline) (set-underline! mkup #t))
           (apply-markup mkup self markup-position pos)))
           (set! markup-position pos)
           (void))]
    [html-image (src alt width height border)
     (let ([anch (top-val anchor)]
           [image-fn (send browser cache-name src)])
       (define ibox
         (if anch
             (let ([ibox (create <flex-button> self)])
               (set-mouse-press-method! ibox
                 (lambda (ibox x y mods)
                   (event-case ((modifier= mods))
                       [([left-button])
                        (send browser history-add)
                        (send browser view-URL anch)]
                       [else (void)])))
               (set-mouse-enter-method! ibox
                 (lambda (ibox x y mods)
                   (set-mouse-cursor! self 'hand2)
                   (send browser display-status (URL->string anch))))
               (set-mouse-leave-method! ibox
                 (lambda (ibox x y mods)
                   (set-mouse-cursor! self 'left_ptr)
                   (send browser display-status "")))
               ibox)
             (create <label> self)))
       (set-traversal-thickness! ibox 0)
       (when border (set-border-width! ibox (or (string->number border) 0)))
       (send self insert-widget-at (get-cursor-pos self) ibox)
       (thread-send-msg background
         (lambda ()
           (if (file-exists? image-fn)
               (show-html-image-file ibox image-fn alt)
               (let ([netip (send browser open-URL src)]
                     [bufsize 4084])
;(fprintf panic-op "background image load: ~s~%" src)
                 (if netip
                     (let ([tmpop (open-output-file image-fn 'truncate)]
                           [buffer (make-string bufsize)])
                       (let loop ()
                         (let ([cnt (block-read netip buffer bufsize)])
                           (unless (eof-object? cnt)
                             (block-write tmpop buffer cnt)
                             (loop))))
                       (send browser display-status "")
                       (close-output-port tmpop)
                       (show-html-image-file ibox image-fn alt))
                     (set-title! ibox alt)))))))]
    [show-html-image-file (ibox fn alt)
      (let ([x (file-type fn)])
        (case x
          [(GIF87 GIF89) (set-title! ibox (create <photo> with (filename: fn)))]
          [(BITMAP) (set-title! ibox (create <bitmap> with (data: (swl:file->string fn))))]
          [else 
           (set-title! ibox alt)]))]
    [get-attr (name attr-list)
      (let loop ([ls attr-list])
        (if (null? ls) 
            #f
            (let ([attr (car ls)])
              (if (pair? attr)
                  (if (eq? (car attr) name)
                      (cdr attr)
                      (loop (cdr ls)))
                  (if (eq? attr name) attr #f)))))]
    [cvt-num (x def)
      (cond
        [(string? x) (or (string->number x) def)]
        [(number? x) x]
        [else def])]
    [flush-option ()
      (when (top-val option)
        (let ([opt-txt (get-output-string (top-val option))]
              [value (top-val sel-value)]
              [name (top-val sel-name)])
          (define mk-action
            (lambda (button)
              (lambda (button)
                (set-title! button opt-txt)
                (extend-form-env! name (if value value opt-txt)))))
          (set! form-sel-opts 
             (cons (cons opt-txt mk-action) form-sel-opts)))
        (pop! sel-value))]
    [html-out (txt)
      (cond
       [(top-val title) (display txt (top-val title))]
       [(top-val option) (display txt (top-val option))]
       [(and (string=? txt " ")
             (zero? (get-x/char (get-cursor-pos self))))
        (void)]
       [else
        (send-base self insert-at 'end txt)])]
    [html-line-break () 
      (unless (zero? (get-x/char (get-cursor-pos self)))
        (send-base self insert-at 'end #\newline))]
    [html-vskip (a b)
      (unless (zero? (get-x/char (get-cursor-pos self)))
      (let ([pos1 (get-cursor-pos self)]
            [mkup (create <markup>)])
        (set-above-paragraph-space! mkup a)
        (set-below-paragraph-space! mkup b)
        (send-base self insert-at 'end #\newline)
        (let ([pos2 (get-cursor-pos self)])
          (apply-markup mkup self (cons 0 (get-y/char pos1)) pos2))))]
    [start (token fixed?)
     (let* ([mval (markup-value token)] [attrs (cdr mval)])
      (flush-markup fixed?)
      (case (car mval)
        [(A) (let ([href (get-attr 'href attrs)]
                   [name (get-attr 'name attrs)])
               (when href (push! anchor href))
               (when href (push! underline #t))
               (when href (push! color 'blue))
               (when name (push! frag name)))]
        [(B STRONG) (push! bold #t)]
        [(U) (push! underline #t)]
        [(I VAR EM CITE) (push! italic #t)]
        [(TT SAMP KBD CODE) (push! fixed-width #t)]
        [(PRE XMP LISTING) (push! fixed-width #t)]
        [(BR) (html-line-break)] ; may want to avoid flushing markups here
        [(HR) (html-line-break)
         ; need to add this guy to list of things that need to be recomputed
         ; when size of view changes
              (push! line-format 'center)
              (let ([rule (create <frame> self
                            with (border-width: 1) 
                                 (height: 2)
                                 (width: (max 1 (- width (quotient width 10))))
                                 (relief: 'sunken))]
                     [p (get-cursor-pos self)])
;(fprintf panic-op "width=~s~%" width)
                (send-base self insert-at 'end #\tab)
                (insert-widget-at self 'end rule))
              (html-vskip HR-above HR-below)]
        [(TITLE) (push! title (open-output-string))]
        [(HEAD HTML META LINK) (void)]
        [(BODY)
          (let ([bg (get-attr 'bgcolor attrs)])
            (when (and bg
                      (fx= (string-length bg) 7)
                      (char=? (string-ref bg 0) #\#)
                      (string->number (substring bg 1 7) 16))
              (set-background-color! self (tk->color bg))))]
        [(MENU UL DIR)
          (push! line-format (car mval))
          (push! left-margin (+ (top-val left-margin) 30))]
        [(OL) (push! left-margin (+ (top-val left-margin) 30))
              (push! line-format 'OL)
              (push! item 0)]
        [(LI) (html-line-break)
              (send-base self insert-at 'end #\tab)
              (if (number? item)
                  (begin
                    (set! item (+ item 1))
                    (html-out (format "~s." item)))
                  (html-out bullet))
              (send-base self insert-at 'end #\tab)]
        [(DL) (let ([compact (get-attr 'compact attrs)])
                (push! line-format 'DL))]
        [(DT) (html-line-break)]
        [(DD) (html-line-break)
              (send-base self insert-at 'end #\tab)]
        [(BASE) (let ([href (get-attr 'href attrs)])
                  (set! base-URL (merge-URL base-URL href)))]
        [(FONT) (let ([size (get-attr 'size attrs)])
                  (let ([size (and size (string->number size))])
                    (when size (push! font-size size))))]
        [(P) ;(push! line-format 'P)
              (html-vskip P-above P-above)]
        [(H1) (push! left-margin H1-lm)
              (html-line-break)
              (push! line-format 'center)
              (send-base self insert-at 'end #\tab)
              (push! font-size 3)
              (push! bold #t)]
        [(H2) (push! left-margin H2-lm)
              (html-line-break)
              (push! font-size 2)
              (push! bold #t)]
        [(H3) (push! left-margin H3-lm)
              (html-line-break)
              (push! font-size 2)
              (push! italic #t)]
        [(H4) (push! left-margin H4-lm)
              (html-line-break)
              (push! font-size 1)
              (push! bold #t)]
        [(H5) (push! left-margin H5-lm)
              (html-line-break)
              (push! font-size 1)
              (push! italic #t)]
        [(H6) (push! left-margin H6-lm)
              (html-line-break)
              (push! font-size 1)
              (push! bold #t)]
        [(CENTER)
              (html-line-break)
              (push! line-format 'center)
              (send-base self insert-at 'end #\tab)]
        [(ADDRESS)
              (html-line-break)
              (push! italic #t)]
        [(BLOCKQUOTE)
              (html-line-break)
              (push! italic #t)
              (push! left-margin (+ (top-val left-margin) 8))]
        [(FORM) (let ([action (get-attr 'action attrs)]
                      [method (get-attr 'method attrs)])
                 (flush-form)
                 (when (and action method)
                   (set! form-action action)
                   (set! form-method method)
                   (set! form-env '()))
                 (void))]
        [(INPUT) (let ([type (or (get-attr 'type attrs) "default")]
                       [name (get-attr 'name attrs)]
                       [value (get-attr 'value attrs)]
                       [size (cvt-num (get-attr 'size attrs) def-input-size)]
                       [maxl (cvt-num (get-attr 'maxlength attrs) #f)]
                       [chkd (get-attr 'checked attrs)]
                       [src (get-attr 'src attrs)])
                  (let ([type (string->symbol type)])
                   (when (and form-action (or name (eq? type 'SUBMIT)))
                    (send self insert-widget-at (get-cursor-pos self)
                      (case type
                       ; [(CHECKBOX) (void)]
                       ; [(RADIO) (void)]
                       ; [(IMAGE) (void)]
                       [(hidden)
                        (extend-form-env! name (or value ""))
                        (create <label> self
                          with (title: (format "[Hidden: ~a]" (or value ""))))]
                       ; [(RESET)  (void)]
                       [(submit)
                        (when name (extend-form-env! name (or value "")))
                        (create <button> self
                          with (action: (lambda (button) (html-query)))
                               (title: (or value "Submit")))]
                       [(text password default)
                        (let ([tbox (create <entry> self with (width/char: size))])
                          (when (eq? type 'PASSWORD) (set-blank! tbox #\*))
                          (insert tbox (or value ""))
                          (extend-form-env! name tbox)
                          tbox)]
                       [else
                        (let ([fmt "[input type '~s' not supported]"])
                          (create <label> self 
                            with (title: (format fmt type))))]))
                    (void))))]
        [(TEXTAREA) (let ([name (get-attr 'name attrs)]
                          [cols (cvt-num (get-attr 'cols attrs) def-input-cols)]
                          [rows (cvt-num (get-attr 'rows attrs) def-input-rows)]
                          [value (get-attr 'value attrs)]
                          [pos (get-cursor-pos self)])
                      (when (and form-action name)
                        (let ([tbox (create <text> self
                                with (width/char: cols) (height/char: rows))])
                          (insert tbox (or value ""))
                          (send self insert-widget-at pos tbox)
                          (extend-form-env! name tbox))
                        (void)))]
        [(SELECT) (let ([name (get-attr 'name attrs)]
                        [size (get-attr 'size attrs)]
                        [mult (get-attr 'mult attrs)])
                    (when (and form-action name)
                      (set! form-sel-opts '())
                      (push! sel-name name)
                      (void)))]
        [(OPTION) (let ([sel (get-attr 'selected attrs)]
                        [val (get-attr 'value attrs)])
                    (flush-option)
                    (unless (top-val option)
                      (push! option (open-output-string)))
                    (push! sel-value val)
                    (void))]
        [(IMG) (let ([i (get-attr 'src attrs)]
                     [a (or (get-attr 'alt attrs) "UNSUPPORTED IMAGE TYPE")]
                     [w (get-attr 'width attrs)]
                     [h (get-attr 'height attrs)]
                     [b (get-attr 'border attrs)])
                  (cond
                    [i (html-image i a w h b)]
                    [else (html-out (format "[~a]" a))]))]
        [else (fprintf (swl:bug-port) "doesn't handle <~a> yet!~%" (car mval))])
       (void))]
    [end (token fixed?)
     (let ([mval (markup-value token)])
      (flush-markup fixed?)
      (case mval
        [(A) (pop! anchor)
             (pop! color)
             (pop! underline)
             (pop! frag)]
        [(B STRONG) (pop! bold)]
        [(U) (pop! underline)]
        [(I VAR EM CITE) (pop! italic)]
        [(TT SAMP KBD CODE PRE) (pop! fixed-width)]
        [(TITLE) 
           (set-title! browser (get-output-string (top-val title)))
           (pop! title)]
        [(HEAD BODY HTML META LINK) (void)]
        [(MENU UL DIR DL)
           (html-line-break)
           (pop! line-format)
           (pop! left-margin)]
        [(OL) (html-line-break)
              (pop! left-margin)
              (pop! line-format)
              (pop! item)]
        [(LI) (void)]
        [(FONT) (pop! font-size)]
        [(P) (void)]
        [(H1) (html-vskip H1-above H1-below)
              (pop! line-format)
              (pop! font-size)
              (pop! bold)
              (pop! left-margin)]
        [(H2) (html-vskip H2-above H2-below)
              (pop! font-size)
              (pop! bold)
              (pop! left-margin)]
        [(H3) (html-vskip H3-above H3-below)
              (pop! font-size)
              (pop! italic)
              (pop! left-margin)]
        [(H4) (html-vskip H4-above H4-below)
              (pop! font-size)
              (pop! bold)
              (pop! left-margin)]
        [(H5) (html-vskip H5-above H5-below)
              (pop! font-size)
              (pop! italic)
              (pop! left-margin)]
        [(H6) (html-vskip H6-above H6-below)
              (pop! font-size)
              (pop! bold)
              (pop! left-margin)]
        [(CENTER) (pop! line-format)]
        [(ADDRESS) (pop! italic)]
        [(BLOCKQUOTE)
              (html-line-break)
              (pop! italic)
              (pop! left-margin)]
        [(FORM) (void)]
        [(SELECT)
          (flush-option)
          (pop! option)
          (pop! sel-name)
          (pretty-print form-sel-opts panic-op)
          (when (pair? form-sel-opts)
            (let ([opts (reverse form-sel-opts)])
              (let ([b (create <option-button> self (caadr opts) with (title: (caar opts)))])
                (let ([m (map (lambda (p) (cons (car p) ((cdr p) b))) opts)])
                  (send b set-options! m)
                  ((cdadr opts) b)
                  (send self insert-widget-at 'end b)))))
          (void)]
        [(OPTION) 
          (flush-option)
          (void)]
        [else (fprintf (swl:bug-port) "doesn't handle </~a> yet!~%" mval)])
      (void))]
    [flush-form ()
      (set! form-action #f)
      (set! form-method #f)
      (set! form-env '())]
    [extend-form-env! (name value)
      (let ([name (if (string? name) (string->symbol name) name)])
        (let ([p (assq name form-env)])
          (if p
              (set-cdr! p value)
              (set! form-env (cons (cons name value) form-env)))))]
    [html-query ()
      (unless (string-ci=? form-method "get")
        (assertion-violationf 'html-query "query method ~s is not supported" form-method))
      (when (string-ci=? form-method "get")
        (let loop ([l form-env] [v '()])
          (cond
            [(null? l)
             (let ([query (apply string-append v)]
                   [URL (merge-URL base-URL form-action)])
               (string-set! query 0 #\?) ; set first "&" to a "?"
;(fprintf (swl:bug-port) "Query: ~s~%" (merge-URL URL query))
               (send browser view-URL (merge-URL URL query))
               )]
            [(not (pair? (car l)))
             (assertion-violationf 'html-query "corrupt environment")]
            [else
             (let ([name (format "&~a=" (caar l))]
                   [value (cdar l)])
               (let ([val (cond
                            [(string? value) value]
                            [(isa? value <entry>) (send value get-string)]
                            [(isa? value <text>) 
                             (send value get-string '(0 . 0) 'end)]
                            [else 
                             (assertion-violationf 'html-query "bogus environment")])])
                 (loop (cdr l) (list* name (form-urlencode val) v))))])))]
  )
  (protected)
  (public
    [init (sframe browser)
      (set! default-background-color (get-background-color self))
      (send-base self init sframe)
      (set! bk-thread
        (thread-fork-group
          (lambda ()
            (let loop ()
              (let ([thunk (thread-receive-msg background)])
                (thunk)
                (loop))))
          1000))]
    [goto-frag (loc)
      (let ([dest (assoc loc entry-points)])
        (when dest (send self move-to-top (cdr dest))))]
    [load-html (URL ip token-read op token-write)
      (set! base-URL URL)
      (set! markup-position (get-cursor-pos self))
      (let loop ([token (token-read ip #t)]
                 [pre? #f]
                 [nomarkup? #t]
                 [i disp-cnt]
                 [r disp-rows])
 (pretty-print token panic-op)
 (flush-output-port panic-op)
        (if (eof-object? token)
            (flush-markup (or nomarkup? pre?))
            (begin
              (send self display-html token (or nomarkup? pre?))
              (when op (token-write token op))
;(fprintf (swl:bug-port) "pre=~s~%" pre?)
;(fprintf (swl:bug-port) "nomarkup=~s~%" nomarkup?)
              (let ([next-token (token-read ip (or nomarkup? pre?))])
                (let ([pre? 
                       (if (markup? token)
                           (let ([mval (markup-value token)])
                             (case (markup-type token)
                               [(start-tag) (if (eq? (car mval) 'pre) #t pre?)]
                               [(end-tag)   (if (eq? mval 'pre) #f pre?)]
                               [else pre?]))
                           pre?)]
                      [nomarkup? (and nomarkup? (string? token))])
                  (cond
                    [(zero? i)
                     (if (> (get-y/char (get-cursor-pos self)) r)
                         (begin
                           (swl:sync-display)
                           (loop next-token pre? nomarkup?
                                 disp-cnt (+ r disp-rows)))
                         (loop next-token pre? nomarkup? disp-cnt r))]
                    [else (loop next-token pre? nomarkup? (fx- i 1) r)]))))))
      (set-enabled! self #f)
      (void)]
    [display-html (token pre?)
      (cond
        [(string? token) (html-out token)]
        [else
         (case (markup-type token)
           [(start-tag)
            (let loop ([attrs (cdr (markup-value token))])
              (unless (null? attrs)
                (let ([attr (car attrs)])
                  (cond
                    [(pair? attr)
                     (case (car attr)
                       [href
                        (when (string? (cdr attr))
                          (set-cdr! attr (merge-url base-url (cdr attr))))]
                       [src
                        (when (string? (cdr attr))
                          (set-cdr! attr (merge-url base-url (cdr attr))))]
                       [(width height alt name content rev bgcolor value
                          method action border)
                        (void)]
                       [else
                        (fprintf (swl:bug-port)
                          "unhandled attribute for <~a>: ~s~%"
                          (car (markup-value token)) attr)])]
                    [(memq attr '()) (void)]
                    [else
                     (fprintf (swl:bug-port)
                       "unhandled attribute for <~a>: ~s~%"
                       (car (markup-value token)) attr)]))
                (loop (cdr attrs))))
            (start token pre?)]
           [(end-tag) (end token pre?)]
           [else (void)])])]
    [delete-view ()
      (set-enabled! self #t)
      (delete-all self)
      ;;(set! width (get-width self))  ; get width often returns 1 ???
      (set-background-color! self default-background-color)
      (set! entry-points '())
      (set! markup-position #f)
      (set! left-margin 30)
      (set! par 0)
      (set! line-format #f)
      (set! justify 'left)
      (set! title #f)
      (set! anchor #f) 
      (set! frag #f)
      (set! item #f)
      (set! underline #f)
      (set! color #f)
      (set! italic #f)
      (set! bold #f)
      (set! font-size 0)
      (set! fixed-width #f)]
     [configure (w h)
      (set! width w)]
  )
)

; 24, 18, 14, 12, 10
(define base-font (create <font> 'times 14 '(roman normal)))

(define-swl-class (<html-browser> home-page cache-dir) (<toplevel>)
  (ivars [cache-dir cache-dir]
         [sframe #f] [win #f] [view-rows 50] [view-cols 110]
         [status-line #f] [html-entry #f]
         [currURL #f] [hist-back '()] [hist-forw '()]
         [MRA-list #f] [MRA-menu #f]
  )
  (inherited)
  (inheritable)
  (private
    [zap-slashes (str)
     (let ([new (string-copy str)])
       (do ([i (- (string-length str) 1) (- i 1)])
           ((fx< i 0) new)
         (when (char=? (string-ref str i) #\/)
           (string-set! new i #\|))))]
    [set-curr-URL (URL)
      (delete-all html-entry)
      (insert html-entry (URL->string URL))
      (set! currURL URL)]
    [make-view ()
      (create <html-view> sframe self
        with
;;           (height/char: view-rows) (width/char: view-cols)
             (wrap: 'word) (font: base-font) (mouse-cursor: 'left_ptr))]
    [warn-URL (err URL)
     (let ([URI (URL->string URL)])
       (let ([msg (case err
                    [(bad-host-address)
                     (format "Bad host address: ~a" uri)]
                    [(cannot-connect)
                     (format "Cannot connect to: ~a" uri)]
                    [(bad-port) (format "Bad port: ~a" uri)]
                    [(cannot-create-socket)
                     (format "Cannot create socket: ~a" uri)]
                    [(unsupported-access-type)
                     (format "Unsupported access type: ~a" uri)]
                    [else (format "URI error: ~a" uri)])])
         (send self display-status msg)
         #f))]
    [history-backward ()
     (when currURL
       (when (or (null? hist-forw) (not (eq? currURL (car hist-forw))))
         (set! hist-forw (cons currURL hist-forw))))
     (unless (null? hist-back)
       (let ((URL (car hist-back)))
          (set! hist-back (cdr hist-back))
          (send self view-URL URL)))]
    [history-forward ()
     (send self history-add)
     (unless (null? hist-forw)
       (let ((URL (car hist-forw)))
         (set! hist-forw (cdr hist-forw))
         (send self view-URL URL)))]
  )
  (protected)
  (public
    [init (home-page cache-dir)
     (send-base self init)
     (set-height! self (round (* 5/6 (swl:screen-height))))
     (set-width! self  (round (* 3/4 (swl:screen-width))))
(swl:sync-display)
;(fprintf (swl:bug-port) "sw = ~s sh = ~s w = ~s h = ~s" (swl:screen-width) (swl:screen-height) (get-width self) (get-height self))
     (let* ((sf (create <scrollframe> self))
            (menubar (create <frame> self))
            (menu-h-backward
              (create <button> menubar with
                (title: (create <bitmap> with (data: left-arrow-bitmap)))
                (action: (lambda (button) (history-backward)))))
            (menu-h-forward
              (create <button> menubar with
                (title: (create <bitmap> with (data: right-arrow-bitmap)))
                (action: (lambda (button) (history-forward)))))
            (menu-control
              (create <cascade-menu-item> with
                (title: "Control")
                (menu: (make-menu
                         ("Exit" (lambda (menu-item) (destroy self)))))))
            (menu-history
              (create <cascade-menu-item>
                with (title: "Go")
                     (menu:
                       (make-menu
                         ((format "~a" home-page)
                          (lambda (menu-item)
                            (send self view-URL (merge-URL #f home-page))))
                       ))))
            (lab1 (create <label> menubar with (title: "     Open URL:")))
            (entry (create <entry> menubar with (width/char: 50))))
       (set! sframe sf)
       (set! html-entry entry)
       (set-action! entry
         (lambda (entry)
           (send self view-URI (get-string entry))))
       (set! win (make-view))
       (set! status-line (create <label> self with (anchor: 'w)))
       (set-title! self "HTML Browser")
       (set! MRA-list (create <MRA-list> 20))
       (set! MRA-menu menu-history)
       (pack menu-h-backward (side: 'left))
       (pack menu-h-forward (side: 'left))
       (set-menu! self
         (create <menu>
           (list menu-control menu-history)))
       (pack lab1 (side: 'left))
       (pack entry (side: 'left) (fill: 'x))
       (pack menubar (side: 'top) (fill: 'x))
       (pack sf (expand: #t) (fill: 'both))
       (pack win (expand: #t) (fill: 'both))
       (pack status-line (anchor: 'w) (fill: 'x)))
     (send self view-URI home-page)]
    [history-add ()
     (when currURL
       (when (or (null? hist-back) (not (eq? currURL (car hist-back))))
         (set! hist-back (cons currURL hist-back))))]
    [display-status (stat)
     (set-title! status-line stat)
     ; without sync we don't post some status message in a timely manner
     ; (primarily "connect..." type stuff where TK is too busy loading
     ; doing too many sync displays, such as when running the mouse over
     ; a bunch of anchors, however, will cause the cursor to flicker
     ; and the status line to fall behind.  We need some sort of compromise.)
     ;(swl:sync-display)
     (void)]
    [cache-name (URL)
     (string-append
       cache-dir "/" (URL-addr URL)
       "|"  (zap-slashes (URL-path URL))
       "%" (if (URL-query URL) (URL-query URL) ""))]
    [open-URL (URL)
     (send self display-status (format "Contacting host: ~a" (URL-addr URL)))
     (let ([netip (www-open-URL URL)])
       (if (symbol? netip)
           (warn-URL netip URL)
           (begin
             (send self display-status (format "Connected: ~a" (URL-addr URL)))
             netip)))]
    [view-URI (URI)
      (let ([URL (merge-URL currURL URI)])
        (send self history-add)
        (send self view-URL URL))]
    [view-URL (URL)
     (let ([fn (send self cache-name URL)])
       (cond
         [(and currURL
               (string=? (URL-addr URL) (URL-addr currURL))
               (string=? (URL-path URL) (URL-path currURL))
               (equal? (URL-query URL) (URL-query currURL)))
          (void)]
         [#f ;;; check for cached html-views here
          #f]
         [(file-exists? fn)
          (let ([ip (www-open-http-file fn)])
;(fprintf (swl:bug-port) "cache hit for ~s~%" URL)
            (send win delete-view)
            (set-curr-URL URL)
            (send win load-html URL ip (lambda (ip x) (read ip)) #f #f)
            (close-input-port ip))]
         [else
          (send MRA-list add (URL->string URL)
             (lambda (menu-item) (send self view-URL URL)))
          (set-menu! MRA-menu (create <menu> (simple-menu-list->menu-items (send MRA-list list))))
;(fprintf (swl:bug-port) "cache miss for ~s~%" URL)
          (let ([netip (send self open-URL URL)]
                [cache-op (open-output-file fn)])
            (when netip
;(fprintf (swl:bug-port) "opened (~a) URL: ~s~%" netip URL)
              (send win delete-view)
              (set-curr-URL URL)
              (send win load-html URL netip read-html-token cache-op write)
              (close-output-port cache-op)
              (close-input-port netip)))]))
     (when (URL-loc URL)
       (send win goto-frag (URL-loc URL)))]
))

(set! panic-op (open-output-file "/dev/null" 'append))
(define go
  (lambda (uri)
(assertion-violationf 'np.ss "swl:make-application has been subsumed by swl:begin-application\n")
    (swl:make-application
      (lambda ()
          " ;;; flush cache while testing caching functions"
          (let* ([home (read (car (process "echo $HOME")))]
                 [cache-dir (format "~a/.netport" home)])
            (system
              (format
                "if [ -d ~a ] ; then
echo using directory ~a for caching
else
mkdir ~a;
echo creating directory ~a for caching
fi"
                cache-dir
                cache-dir
                cache-dir
                cache-dir))
            (printf "no longer flushing cache~n")
;            (system "/bin/rm -f $HOME/.netport/*")
            (set! browser (create <html-browser> uri cache-dir)))))))

; collect handler needs to be rewritten so that it doesn't write to a
; non-blocking port so that it doesn't throw out of a critical section
; The current solution is to avoid using collect-notify
;(collect-notify #t)

;(set! x (thread-fork-group (lambda () (go "http://www.altavista.digital.com"))))

;(fluid-let ((panic-op (open-output-file "/dev/null" 'append)))
(set! x (thread-fork-group (lambda () (go "http://www.cs.indiana.edu/"))))
;)

;(set! x (thread-fork-group (lambda () (go "http://www.cs.indiana.edu/proglang/proglang.html"))))
;(set! x (thread-fork-group (lambda () (go "file:.cs.home-page.html"))))
