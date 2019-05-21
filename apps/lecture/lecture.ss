;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; Todo
;;   - forward/back-script need to take an argument n that says how far to go
;;       - would be trivial to add this and to add a goto slide n capability
;;         but I doubt my ability to test it from home
;;   - move to separate top-level windows
;;   - add mode-switch button that goes between "lecture" and normal
;;     mouse bindings.
;;       - in non-lecture mode need forward/backward buttons we can use.
;;
;; Bugs:
;;  - always starts with "slide" slide (possibly empty)

;; * restored the append type so we can eventually search for beginning
;;   of an entire slide.  Fixed file->script to understand append slides
;;   intermixed with Scheme slides.

(module (lecture)
  (import swl:oop)
  (import swl:macros)
  (import swl:option)
  (import swl:generics)

; this is dumb:

(define left-arrow
"
#define larrow_width 18
#define larrow_height 18
static unsigned char larrow_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x18, 0x00, 0x00, 0x1e, 0x00, 0x80, 0x1f, 0x00, 0xe0, 0x07, 0x00,
   0xf8, 0x01, 0x00, 0xf8, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x80, 0x1f, 0x00,
   0x00, 0x1e, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
")
                  
(define right-arrow
"
#define rarrow_width 18
#define rarrow_height 18
static unsigned char rarrow_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x60, 0x00, 0x00, 0xe0, 0x01, 0x00, 0xe0, 0x07, 0x00, 0x80, 0x1f, 0x00,
   0x00, 0x7e, 0x00, 0x00, 0x7c, 0x00, 0x80, 0x1f, 0x00, 0xe0, 0x07, 0x00,
   0xe0, 0x01, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
")


;;   - need a way to visualize the whole lecture of slides and jump to
;;     specific point
;;   x need way to skip history items (shift-click)
;;       - should print the first line in the window title (or something)
;;   x make the slide screen non-editable
;;   - font support
;;   - need to be able to resize the screens
;;   ? should going backward in history erase current expression typed in repl?
;;   ? scroll bar on slide screen?  (seems bogus as a slide if you scroll)
;;   ? should lecture history skip over "append" slides when going backwards?
;;
;; Proposed directions for the lecture script language
;;   - make it an HTML extension, or permit HTML to be embedded in slides
;;       - font change stuff
;;       - fancier stuff (in-line images)
;;   - provide naming mechanism for slides so that a slide can be redisplayed
;;     without copying all the text of the slide.

;  ;(unless (top-level-bound? 'new-repl)
;    (require "../repl/repl.ss")
;    (require "../common/scrollframe.ss")
;  ;)

;; all this needs is a few more side effects and it will be perfect (groan)

;; The lecture tool usurps the mouse buttons normally used to cut
;; and paste in a text widget.  We intercept the other mouse button
;; notifications (mouse-motion mouse-release) to prevent the
;; appearance of partial cut and paste functionality.
;; (this is done in <slide-text> and <slide-repl-text>)

(define-swl-class (<slide-text> parent show-pos) (<text> parent)
  (ivars (script '()) (current '()) (lecture-mode? #f)
    (two-button-mouse? #f) (last-slide-type #f) (repl #f)
    (repl-maker (thread-make-msg-queue 'repl-maker))
    (repl-writer (thread-make-msg-queue 'repl-writer))
    (total 0) (curno 0) (show-pos show-pos))
  (inherited)
  (inheritable)
  (private
    [ensure-repl (start-k)
     (critical-section
       (if repl
         (start-k repl)
         (thread-send-msg repl-maker start-k)))]
    [back-script (n)
     ;; efficiency, what's that?
     (let scan ([n n])
       (unless (or (zero? n) (eq? script current))
         (let loop ([s script])
           (if (eq? (cdr s) current) (set! current s) (loop (cdr s))))
         (set! curno (- curno 1))
         (scan (- n 1))))
     (show-pos curno total)]
    [forward-script (n)
     ;; efficiency, what's that?
     (let scan ([n n])
       (unless (or (zero? n) (null? current) (null? (cdr current)))
         (set! current (cdr current))
         (set! curno (+ curno 1))
         (scan (- n 1))))
     (show-pos curno total)]
    [eval-if-possible ()
     (ensure-repl
       (lambda (repl)
         (send repl key-press #\newline (add-modifiers))))])
  (protected)
  (public
    [jump-to (n)
     (critical-section
       (unless (= n curno)    ;; have to check this to avoid loop
         (if (< n curno)
             (back-script (- curno n))
             (forward-script (- n curno)))
         (send self show-slide)))]
    [key-press (key mods)
     (when (event-case ((key= key) (modifier= mods))
             (([up]) (vscroll self -1 'units) #f)
             (([down]) (vscroll self 1 'units) #f)
             (([prior]) (vscroll self -1 'pages) #f)
             (([next]) (vscroll self 1 'pages) #f)
             (([left] [#\p] [#\P]) (back-script 1) #t)
             (([right] [#\n] [#\N]) (forward-script 1) #t)
             (([#\return] [#\newline]) (eval-if-possible) #f)
             (else #f))
       (event-case ((key= key) (modifier= mods))
         (([shift]) (void))
         (else (send self show-slide))))]
    [key-release (key mods) (void)]
    [mouse-press (x y mods)
      ;; It would probably be better software engineering to use nested
      ;; event-case'es to show the current slide only when shift is not
      ;; pressed.
      ;; The following is probably easier to read.
      (if (not lecture-mode?)
          (send-base self mouse-press x y mods)
          (if two-button-mouse?
              (event-case ((modifier= mods))
                (([left-button])
                 (back-script 1)
                 (event-case ((modifier= mods))
                   (([shift]) (void))
                   (else (send self show-slide))))
                (([right-button])
                 (if (eq? last-slide-type 'scheme)
                     (begin
                       (set! last-slide-type #f)
                       (eval-if-possible))
                     (begin
                       (forward-script 1)
                       (event-case ((modifier= mods))
                         (([shift]) (void))
                         (else (send self show-slide))))))
                (else (send-base self mouse-press x y mods)))
              (event-case ((modifier= mods))
                (([shift left-button])
                 (back-script 1))
                (([left-button])
                 (back-script 1)
                 (send self show-slide))
                (([shift middle-button])
                 (forward-script 1))
                (([middle-button])
                 (forward-script 1)
                 (send self show-slide))
                (([right-button])
                 (eval-if-possible))
                (else (send-base self mouse-press x y mods)))))]
    [show-slide ()
     (unless (null? current)
       (let ([x (car current)])
         (set! last-slide-type (slide-type x))
         (set-enabled! self #t)
         (case (slide-type x)
           [(slide)
            (delete-all self)
            (insert-at self '(0 . 0) (slide-content x))]
           [(append)
            (delete-all self)
            (insert self (slide-content x))] ;; scroll to end
           [(scheme)
            (thread-send-msg repl-writer (slide-content x))])
         (set-enabled! self #f)))]
    [next-slide () (forward-script 1)]
    [prev-slide () (back-script 1)]
    [evaluate () (eval-if-possible)]
    [set-script! (s)
     (delete-all self)
     (set! script s)
     (set! current s)
     (set! total (length s))
     (set! curno (min 1 (max 0 total)))
     (send self show-slide)
     (show-pos curno total)]
    [set-two-button-mouse! (val)
     (set! two-button-mouse? val)]
    [get-two-button-mouse () two-button-mouse?]
    [set-lecture-mode! (val)
     (set! lecture-mode? val)]
    [get-lecture-mode () lecture-mode?]
    [destroy ()
      (set! repl-maker #f)  ;;; allow thread to be collected
      (set! repl-writer #f) ;;; allow thread to be collected
      (send-base self destroy)]
    [init (parent show-pos)
      (send-base self init parent)
      (thread-fork-group
        (lambda ()
          (critical-section
            (let loop ()
              (let ((start-k (thread-receive-msg repl-maker)))
                (new-repl
                  (lambda (r) (set! repl r) (start-k r))
                  (lambda () (set! repl #f))))
              (loop)))))
      (thread-fork-group
        (lambda ()
          (let loop ()
            (let ((sexp (thread-receive-msg repl-writer)))
              (ensure-repl
                (lambda (repl)
                  (send repl set-current-expression! sexp))))
            (loop))))
      (ensure-repl (lambda (x) #f))
      (set-focus self)]))


(define lecture
  (case-lambda
    [() (lecture #f)]
    [(filename)
     (swl:begin-application
       (lambda (token)
     (let* ([top
             (create <toplevel> with
               (title: (or filename "Lecture Tool"))
               (destroy-request-handler:
                 (lambda (self)
                   (swl:end-application token)
                   #t)))]
            [menubar (create <frame> top)]
            [pad1 (create <frame> menubar with (width: (in->pixels 1)))]
            [pad2 (create <frame> menubar with (width: (in->pixels 1)))]
            [scale (create <scale> menubar with (min: 1)
                   (slider-length: 21)
                   (width: 7) (orientation: 'horizontal))]
            [lab1 (create <label> menubar with (title: "Current slide:"))]
            [sf (create <scrollframe> top
                  with (default-vscroll: #t) (sticky-hscroll: #t))]
            [slide
             (create <slide-text> sf
               (lambda (n total)
                 (set-max! scale total)
                 (set-value! scale n))
               with (background-color: 'white))]
            [forward (create <button> menubar
                       with
                       (action:
                         (lambda (button)
                           (send slide next-slide)
                           (send slide show-slide)))
                       (title: (create <bitmap> with (data: right-arrow))))]
            [backward (create <button> menubar
                       with
                       (action:
                         (lambda (button)
                           (send slide prev-slide)
                           (send slide show-slide)))
                       (title: (create <bitmap> with (data: left-arrow))))]
            [evaluate (create <button> menubar
                        with
                        (action:
                          (lambda (button) (send slide evaluate)))
                        (title: "Eval"))]
            [file
             (create <cascade-menu-item>
               with
               (title: "File")
               (menu:
                 (make-menu
                   ("Load Script"
                     (lambda (menu-item)
                       (thread-fork
                         (lambda ()
                           (let ([filename
                                  (swl:file-dialog "Select script file" 'open
                                    (parent: top)
                                    (default-dir: (current-directory)))])
                             (when filename
                               (send slide set-script!
                                 (file->script filename))))))))
                   ("Exit" (lambda (menu-item) (destroy top))))))]
            [font-item
             (create <command-menu-item> with (title: "Font...")
              (action:
                (lambda (item)
                  (swl:font-dialog top "Select a font for slide text"
                    (swl:font-families 'fixed)
                    '(-8 -10 -12 -14 -16 -18 -20 -22 -24 8 10 12 14 16 18 20 22 24)
                    '(bold normal)
                    (lambda () (send slide get-font))
                    (lambda (fnt) (when fnt (send slide set-font! fnt)))))))]
            [lecture-mode
             (create <check-menu-item> with
               (title: "Lecture mode")
               (action:
                 (lambda (menu-item)
                   (send slide set-lecture-mode!
                     (not (send slide get-lecture-mode))))))]
            [two-buttons
             (create <check-menu-item> with
               (title: "2-button mouse")
               (action:
                 (lambda (menu-item)
                   (send slide set-two-button-mouse!
                     (not (send slide get-two-button-mouse))))))]
            [pref-menu (make <menu> (list lecture-mode two-buttons font-item))]
            [prefs
             (create <cascade-menu-item> with
               (title: "Preferences")
               (menu: pref-menu))]
            [setup (thread-make-msg-queue 'setup)])

       (set-menu! top (create <menu> (list file prefs)))
       (set-action! scale (lambda (scale n) (send slide jump-to n)))
       (when filename (send slide set-script! (file->script filename)))
       (pack menubar (side: 'top) (anchor: 'w))
       (pack pad1 (side: 'left)) ;; gross geometry manager hack
       (pack backward (side: 'left) (anchor: 's))
       (pack forward (side: 'left) (anchor: 's))
       (pack evaluate (side: 'left) (anchor: 's))
       (pack pad2 (side: 'left)) ;; gross geometry manager hack
       (pack lab1 (side: 'left) (anchor: 's))
       (pack scale (side: 'right))
;      (pack slide (expand: #t) (fill: 'both) (side: 'top))
       (pack sf (expand: #t) (fill: 'both))
    ;; thoroughly disgusting hack to set (lecture mode)
       (send lecture-mode invoke pref-menu)
    ;; thoroughly disgusting hack to set (two-buttons)
       (send two-buttons invoke pref-menu)
       (thread-send-msg setup #t)
       (lambda () (send top destroy)))))
(show-lecture-help)
     (void)]))

(define show-lecture-help
  (let ((first #t))
    (lambda ()
      (when first
        (set! first #f)
        (printf "

The lecture window now supports these key bindings:

  right arrow and `n' move to the next script.
  left arrow and `p' move to the previous script.
  return evaluates the Scheme expression at the REPL prompt.

  holding shift and pressing left/right moves through the script
  without displaying the slides.

  up and down arrows scroll the display line by line if possible

")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Parsing the lecture scripts
;;

(define make-slide (lambda (type s) (cons type s)))
(define slide-type car)
(define slide-content cdr)

;; quite hacked.  bangs on file-position quite a bit.
;; also potentially buggy that it assumes first thing is a slide.
;;
;; if we've found a Scheme expression, we take only the
;; first whole expression (scripts may contain return
;; values which are ignored)

(define file->script
  (lambda (filename)
    (define read-block
      (lambda (ip start end where)
        (let ([len (fx- end start)])
          (let ([buf (make-string len)])
            (file-position ip start)
            (block-read ip buf len)
            (file-position ip where)
            buf))))
    (define adjust-end!
      (lambda (ip start)
        (file-position ip start)
        (read ip)
        (file-position ip)))
    (define readit
      (lambda (ip type main ans)
        (call-with-values
          (lambda () (read-slide-token ip))
          (lambda (next start end)
            (if (not type)
                (reverse ans)
                (let ([pos (file-position ip)])
                  (let ([content
                         (case type
                           [(slide)  (read-block ip start end pos)]
                           [(append) (string-append main
                                       (read-block ip start end pos))]
                           [(scheme) (read-block ip start
                                       (adjust-end! ip start)
                                       pos)])])
                     (readit ip next (if (eq? type 'scheme) main content)
                       (cons (make-slide type content) ans)))))))))
    (readit (open-input-file filename) 'slide "" '())))


(define read-slide-token
  (lambda (ip)
    (let ([start (file-position ip)])
      (define scan-for
        (lambda (ip end type char cnt)
          (let loop ([c (read-char ip)] [cnt cnt])
            (cond
              [(fxzero? cnt)
               (unread-char c ip)
               (values type start end)]
              [(eof-object? c) (values #f start end)]
              [(char=? c char) (loop (read-char ip) (fx- cnt 1))]
              [else (scan ip end #f)]))))
      (define scan
        (lambda (ip end newline?)
          (let loop ([c (read-char ip)] [end end] [type #f] [newline? newline?])
            (if (eof-object? c)
                (values type start end)
                (case c
                  [(#\newline)
                   (let ((end (file-position ip)))
                     (loop (read-char ip) end type #t))]
                  [(#\-)
                   (if newline?
                       (scan-for ip end 'slide #\- 2)
                       (loop (read-char ip) end type #f))]
                  [(#\+)
                   (if newline?
                       (scan-for ip end 'append #\+ 2)
                       (loop (read-char ip) end type #f))]
                  [(#\>)
                   (if newline?
                       (scan-for ip end 'scheme #\space 1)
                       (loop (read-char ip) end 'scheme #f))]
                  [else (loop (read-char ip) end type #f)])))))
      (scan ip 0 #t))))

)
