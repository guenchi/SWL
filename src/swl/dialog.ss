;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; Todo
;;  - easy kills
;;     - pass in parent frame / toplevel or #f to create a toplevel so the dialog
;;       can be embedded in another window (change the destroy calls)
;;  - really need to implement a better geometry manager --- all these frames, yikes
;;  * find a way to avoid the following hacks used to center the window above its
;;    parent:
;;      - critical-section (needed so that update idletasks does something useful)
;;      - initial toplevel geometry of +2000+2000 (try to keep ugly stuff offscreen)
;;      - update idletasks

(swl:api-class (<option-button> parent default) (<proto-label> parent #f)
  ;; when button-1 is pressed, check to see if mouse was over button
  ;; if so, post menu  (can get event w/o mouse over button due to grab?)
  ;;
  ;; when posting menu, only bother if it actually contains entries
  ;;
  ;;
  ;% \swlapi{class}{<option-button>}{(create <option-button> parent default)}
  ;* \ret{instance}
  ;* An option button allows the user to select an item from a list
  ;* of choices presented as a menu that appears over the button.
  ;* An option button displays the currently selected text string or image.
  ;*
  ;* Text is displayed in a uniform font (determined by \scheme{set-font!})
  ;* and is displayed as multiple lines if the text contains newlines
  ;* or its length exceeds the wrap length (see \scheme{set-wrap-length!}).
  ;* The text displayed by a button can be changed by the \scheme{set-title!}
  ;* method.
  ;*
  ;* Because the underlying widget library (Tcl/Tk) behaves differently
  ;* under Windows,
  ;* option buttons do not respond to the usual SWL event notification
  ;* methods.
  ;;
  (ivars (options '()) (menu #f) (tkvar (gensym)) (default default))
  (inherited handle)       ; handle is also used as the Tk var for the radiobutton menu entries
  (inheritable handle)
  (private)
  (protected)  ; no show-pressed or show-released  (no Tag bind for this guy)
  (public
    [init (parent default)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (unless (string? default)
       (assertion-violationf 'create "<option-button> default value ~s must be a string" default))
     (set! handle (swl:new-handle parent))
     ; note:  using handle for the varName attribute since radio-menu-items
     ; currently use the menu name as their -variable attribute...
     (swl:tcl-eval '|tk_optionMenu| handle tkvar default)
     (set-title! self default)
     (send-base self init parent)
     ; restore Tk bindings:
     (swl:tcl-eval 'bindtags handle '|Menubutton|)]
    [set-options! (ls)
     ; hold menu w/ Scheme pointer
     ;* \ret{unspecified}
     ;*   ls is a list of strings or pairs whose car is a string and whose
     ;*   cdr is a procedure of no arguments
     (let ([title (get-title self)])
       (let ([m (create <menu>
                  (map (lambda (opt)
                         (call-with-values
                           (lambda ()
                             (cond
                               [(pair? opt) (values (car opt) (cdr opt))]
                               [else (values opt #f)]))
                           (lambda (txt proc)
                             (unless (string? txt)
                               (assertion-violationf 'set-options! "~s is not a string" txt))
                             (unless (or (not proc) (procedure? proc))
                               (assertion-violationf 'set-options! "~s is not a procedure" proc))
                             (create <radio-menu-item> with
                               (title: txt)
                               (selected: (string=? txt title))
                               (action:
                                 (lambda (item)
                                   (set-title! self txt)
                                   (when proc (proc))))))))
                       ls))])
         (send m set-parent! self)
         (set-tk-option '-menu m (lambda (x) #t) 'valid)
         (set! options ls)
         (set! menu m)))]
    [get-options ()
     ;* \ret{option list (see set-options!)}
     ;* The behavior when the list is mutated is undefined.
     options]
    [set-title! (txt)
     (send-base self set-title! txt)
     (swl:tcl-eval 'set tkvar txt)
     (void)]
    [set-active-background-color! (val)
     ;* \ret{unspecified}
     ;; not too sure about the merit of this method
     ;* Sets the background color used when the mouse cursor is over
     ;* the widget to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-activebackground val swl:color? 'set-active-background-color!)]
    [get-active-background-color ()
     ;* \ret{see below}
     ;; not too sure about the merit of this method
     ;* Returns the background color used when the mouse cursor is
     ;* over the widget.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-activebackground tk->color 'get-active-background-color)]
    [set-active-foreground-color! (val)
     ;* \ret{unspecified}
     ;; not too sure about the merit of this method
     ;* Sets the foreground color used when the mouse cursor is over
     ;* the widget to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-activeforeground val swl:color? 'set-active-foreground-color!)]
    [get-active-foreground-color ()
     ;* \ret{see below}
     ;; not too sure about the merit of this method
     ;* Returns the foreground color used when the mouse cursor is
     ;* over the widget.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-activeforeground tk->color 'get-active-foreground-color)]
    [set-disabled-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the foreground color used when the widget is disabled
     ;* to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-disabled val swl:color? 'set-disabled-foreground-color!)]
    [get-disabled-foreground-color ()
     ;* \ret{see below}
     ;* Returns the foreground color used when the widget is disabled.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-disabled tk->color 'get-disabled-foreground-color)]
    [set-enabled! (val)
     ;* \ret{unspecified}
     ;; with a little more care we might get this via inheritance
     ;; I didn't think it was worth adding lots of depth to the class tree
     ;; for just a few couple little methods
     ;* \var{val} is a boolean value indicating whether or not the widget is
     ;* enabled and responsive to user interaction.
     (set-tk-option '-state
       (and (boolean? val) (if val 'normal 'disabled))
       (lambda (x) x)
       'set-enabled!)]
    [get-enabled ()
     ;* \ret{boolean}
     ;* Returns a boolean value indicating whether or not the widget is
     ;* enabled and responsive to user interaction.
     (get-tk-option '-state (lambda (x) (not (string=? x "disabled"))) 'get-enabled)]))

; unfortunate aspect of using macros to rewrite classnames as refs
; to runtime class ids is that we get hosed by cyclic dependencies
(define isa-option-button? (lambda (x) (isa? x <option-button>)))

(swl:api-procedure define swl:font-dialog
  ;* \formdef{swl:font-dialog}{procedure}{(swl:font-dialog parent title families sizes styles current-font new-font cleanup)}
  ;* \ret{See below}
  ;* This procedure displays a font dialog
  ;* for the given \scheme{parent} (an instance of \scheme{<toplevel>} or \scheme{#f}, if none).
  ;* SWL attempts to place the dialog centered above the parent, if given, or
  ;* centered on the screen.
  ;* The \scheme{title} is used as the title of the toplevel window that is
  ;* created.
  ;* The \scheme{families}, \scheme{sizes}, and \scheme{styles} parameters are
  ;* lists of values appropriate for creating instances of the \scheme{<font>}
  ;* class.
  ;* The \scheme{current-font} procedure is invoked with no arguments, and should
  ;* return an instance of \scheme{<font>} used to supply default values for the
  ;* widgets displaying the various options.
  ;* When the ``OK'' button is pressed, the \scheme{new-font} procedure is invoked
  ;* with an instance of the selected font.
  ;* When the ``Cancel'' button is pressed, \scheme{new-font} is invoked with \scheme{#f}.
  ; get-current should return the current font
  ; return-selected is invoked with either #f or a font
  (lambda (parent title families sizes styles get-current return-selected)
    (define fmt-family (lambda (x) (symbol->string x)))
    (define fmt-size
      (lambda (x)
        (format (if (negative? x) "~a pixels" "~a points") (abs x))))
    (define build-options
      (lambda (options fmt update)
        (map (lambda (x) (cons (fmt x) (lambda () (update x))))
             options)))
    (define build-menu
      (lambda (parent options fmt default update)
        (let ([formatted (map fmt options)])
          (create <option-button> parent (fmt default) with
            (width/char: (apply max 5 (map string-length formatted)))
            (options: (build-options options fmt update))))))
    (unless (or (not parent) (isa? parent <toplevel>))
      (assertion-violationf 'swl:font-dialog "parent must be #f or an instance of <toplevel>"))
    (let ([font (get-current)])
      (let-values ([(family size style) (send font get-actual-values)])
        (define build-check
          (lambda (parent value update)
            (let ([cb (create <checkbutton> parent with
                        (title: (symbol->string value))
                        (anchor: 'w)
                        (action: (lambda (b) (update value))))])
              (when (memq value style) (send cb invoke))
              (pack cb (side: 'top) (fill: 'x)))))
        (critical-section
        (let* ([top (create <toplevel> with (title: (or title "Choose font"))
                      (transient: parent)
                      (geometry: "+2000+2000")
                      (resizable: #f #f))]
               [N (create <frame> top)]
               [S (create <frame> top)]
               [NW (create <frame> N)]
               [NE (create <frame> N with (relief: 'ridge) (border-width: 3))]
               [NWN (create <frame> NW)]
               [NWNW (create <frame> NWN with (relief: 'ridge) (border-width: 3))]
               [NWNE (create <frame> NWN with (relief: 'ridge) (border-width: 3))]
               [sample (create <entry> NW with (width/char: 10) (font: font))]
               [ok (create <button> S with (title: "Ok")
                     (action: (lambda (b) (destroy top) (return-selected font))))]
               [cancel (create <button> S with (title: "Cancel")
                         (action: (lambda (b) (destroy top) (return-selected #f))))]
               [saved-text #f]
               [size-mb #f])
          (define sync
            (lambda ()
              (define find-nearest
                (lambda (n ls)
                  (let loop ([best (car ls)] [ls (cdr ls)])
                    (if (null? ls)
                        best
                        (loop (if (< (abs (- n best)) (abs (- n (car ls))))
                                  best
                                  (car ls))
                              (cdr ls))))))
              (let ([avail-sizes
                     (sort <
                       (let lp ([ls sizes])
                         (if (null? ls)
                             '()
                             (if (swl:font-available? family (car ls) style)
                                 (cons (car ls) (lp (cdr ls)))
                                 (lp (cdr ls))))))])
                (if (null? avail-sizes)
                    (begin
                      (set-title! size-mb "(none)")
                      (set-enabled! size-mb #f)
                      (unless saved-text
                        (set! saved-text (get-string sample))
                        (delete-all sample)
                        (set-enabled! sample #f)))
                    (let ([new-size (find-nearest size avail-sizes)])
                      (set-title! size-mb (fmt-size new-size))
                      (set! font (create <font> family new-size style))
                      (set-font! sample font)
                      (when saved-text
                        (set-enabled! sample #t)
                        (insert sample saved-text)
                        (set! saved-text #f))
                      (set-enabled! size-mb #t)
                      (send size-mb set-options!
                        (build-options avail-sizes fmt-size
                          (lambda (x) (update size x)))))))))
          (define-syntax update
            (syntax-rules ()
              [(_ var val) (begin (set! var val) (sync))]))
          (define opposite
            (lambda (x)
              (case x
                [(bold) 'normal]
                [(normal) 'bold]
                [(italic) 'roman]
                [(roman) 'italic]
                [else #f])))
          (define-syntax toggle
            (syntax-rules ()
              [(_ val)
               (let ([flag #f])
                 (lambda (b)
                   (set! flag (not flag))
                   (if flag
                       (set! style (cons val (remq val (remq (opposite val) style))))
                       (set! style
                         (remq val
                           (cond
                             [(opposite val) => (lambda (x) (cons x style))]
                             [else style]))))
                   (sync)))]))
        ; arrange for the font dialog to be destroyed / hidden / shown when the parent is
          (when parent (send parent add-follower top))
          (pack (create <label> NWNW with (title: "Family") (anchor: 'w)) (side: 'top))
          (pack (build-menu NWNW families fmt-family family (lambda (x) (update family x))) (side: 'top))
          (pack (create <label> NWNE with (title: "Size") (anchor: 'w)) (side: 'top))
          (set! size-mb (build-menu NWNE sizes fmt-size size (lambda (x) (update size x))))
          (pack size-mb (side: 'top))
          (for-each
            (lambda (s)
              (when (memq s styles) (build-check NE s (toggle s))))
            '(bold italic overstrike underline))
          (pack N (side: 'top) (fill: 'x))
          (pack S (side: 'top))
          (pack ok (side: 'left))
          (pack cancel (side: 'left))
          (pack NW (side: 'left) (fill: 'y))
          (pack NE (side: 'right) (fill: 'y))
          (pack NWN (side: 'top) (fill: 'x))
          (send sample insert "Sample text")
          (pack sample (side: 'top) (fill: 'x))
          (pack NWNW (side: 'left) (fill: 'y))
          (pack NWNE (side: 'left) (fill: 'y))
          (sync)
          (with-values
            (if parent
                (values
                  (get-root-x parent)
                  (get-root-y parent)
                  (send parent get-width)
                  (send parent get-height))
                (values 0 0 (swl:screen-width) (swl:screen-height)))
            (lambda (x y width height)
              (swl:raw-tcl-eval "update idletasks")
              (send top set-geometry!
                (format "+~s+~s"
                  (+ x (quotient (- width (send top get-width)) 2))
                  (+ y (quotient (- height (send top get-height)) 2))))))))))))


(module (swl:color-dialog)

(define-swl-class (<selectbox> parent) (<listbox> parent)
  (ivars (thunks (vector #f)) (count 0) (action (lambda x (void))))
  (inherited)
  (inheritable)
  (private
; This looks buggy to me, and I don't think we need it for the color dialog,
; but we may someday want to fix it...
;   [vector-shuffle! (vec to from)
;    (let ([len (vector-length vec)])
;      (let loop ([i to] [j from])
;        (unless (fx= i from)
;          (vector-set! vec i (and (fx< j len) (vector-ref vec j)))
;          (loop (fx+ i 1) (fx+ j 1)))))]
    [index->thunk (i) (vector-ref thunks i)])
  (protected)
  (public
    [mouse-press (x y mods)
     (send-base self mouse-press x y mods)
     (event-case ((modifier= mods))
       (([double left-button]) (action self)))]
    [add-choice (label proc)
     (let ((len (vector-length thunks)))
       (when (= count len)
         (let ((newvec (make-vector (* 2 len)))
               (oldvec thunks))
           (let loop ((n 0))
             (unless (= n len)
               (vector-set! newvec n (vector-ref oldvec n))
               (loop (+ n 1))))
           (set! thunks newvec)))
        (send self insert count label)
        (vector-set! thunks count proc)
        (set! count (+ count 1)))]
    [delete-all () (send self delete 0 'end)]
    [delete (from to)
; Probably buggy, so let's just not let them delete
;    (let ([from (if (eq? from 'end) (item-count self) from)]
;          [to (if (eq? to 'end) (item-count self) to)])
;      (let ([min (min from to)] [max (max from to)])
;        (send-base self delete min max)
;        (vector-shuffle! thunks min max)
;        (set! count (item-count self))))
     (void)
     ]
    [delete (i)
; Probably buggy...
;    (send-base self delete i)
;    (vector-shuffle! thunks i (fx+ i 1))
     (void)
    ]
    [init (parent)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (send-base self init parent)
     (set-select-mode! self 'single)
     (set-export-selection! self #f)]
    [select (index)
     (unless (negative? index) 
       (clear-selection self 0 'end)
       (send-base self select index)
       (let ((f (index->thunk index)))
         (when f (f))))]
    [set-action! (proc)
     (unless (procedure? proc)
       (assertion-violationf 'set-action! "~s is not a procedure" proc))
     (set! action proc)]
    [get-action () action]))

(define swl:color-dialog
  (lambda (parent title get-current return-selected)
    (let-values ([(red green blue) (swl:get-rgb (get-current))])
      (let* ([top (create <toplevel> with (title: (or title "Choose color"))
                    (transient: parent)
                    (resizable: #f #f))]
             [Left (create <frame> top)]
             [sample (create <frame> Left with
                       (height: 70)
                       (border-width: 10)
                       (relief: 'flat))]
             [Rf (create <frame> Left)]
             [Gf (create <frame> Left)]
             [Bf (create <frame> Left)]
             [Right (create <scrollframe> top with (sticky-hscroll: #t))]
             [Sel (create <selectbox> Right)]
             [Btf (create <frame> Left with (border-width: 8))]
             [color-db (swl:get-named-colors)]
             [ok (create <button> Btf with (title: "Ok")
                   (action:
                     (lambda (b)
                       (let ([color (send sample get-background-color)])
                         (destroy top)
                         (return-selected
                           (or (ormap
                                 (lambda (c)
                                   (let ([rgb (cdr c)])
                                     (and (fx= red (car rgb))
                                          (fx= green (cadr rgb))
                                          (fx= blue (caddr rgb))
                                          (caar c))))
                                 color-db)
                               color))))))]
             [cancel (create <button> Btf with (title: "Cancel")
                       (action: (lambda (b) (destroy top) (return-selected #f))))]
             [inits '()])
        (define show-color
          (lambda ()
            (set-background-color! sample (make <rgb> red green blue))))
        (define mkslider
          (lambda (title frm update!)
            (let ([lab (create <label> frm with (title: title)
                         (accept-focus: #f))]
                  [ent (create <entry> frm with (width/char: 4))]
                  [sli (create <scale> frm with
                         (min: 0)
                         (max: 255)
                         (length: 260)
                         (resolution: 1)
                         (orientation: 'horizontal)
                         (show-value: #f))])
              (set-action! ent
                (lambda (e)
                  (let ([n (string->number (send e get-string))])
                    (when (and n (fixnum? n) (<= 0 n 255))
                      (send sli set-value! n)))))
              (pack lab (side: 'left))
              (pack ent (side: 'left))
              (pack sli (side: 'left) (expand: #t) (fill: 'x))
              ; if actions are installed too early, we end up setting(
              ; everything to zero
              (set! inits
                (cons
                  (lambda ()
                    (set-action! sli
                      (lambda (self n)
                        (send ent delete-all)
                        (send ent insert (number->string n))
                        (update! n))))
                  inits))
              sli)))
        (let ([R (mkslider "R" Rf (lambda (v) (set! red v) (show-color)))]
              [G (mkslider "G" Gf (lambda (v) (set! green v) (show-color)))]
              [B (mkslider "B" Bf (lambda (v) (set! blue v) (show-color)))])
          (for-each
            (lambda (name.rgb)
              (let ([name (car (car name.rgb))] [rgb (cdr name.rgb)])
                (send Sel add-choice (symbol->string name)
                  (lambda ()
                    (set-value! R (car rgb))
                    (set-value! G (cadr rgb))
                    (set-value! B (caddr rgb))))))
            color-db)
          (set-value! R red)
          (set-value! G green)
          (set-value! B blue)
          (show-color)
          (for-each (lambda (x) (x)) inits)
         ; arrange for the font dialog to be destroyed / hidden / shown when the parent is
          (when parent (send parent add-follower top))
          (pack Left (side: 'left) (fill: 'x))
          (pack Right (side: 'left) (fill: 'both))
          (pack sample (side: 'top) (fill: 'both))
          (pack Rf (side: 'top) (expand: #t) (fill: 'x))
          (pack Gf (side: 'top) (expand: #t) (fill: 'x))
          (pack Bf (side: 'top) (expand: #t) (fill: 'x))
          (pack Btf (side: 'top))
          (pack ok (side: 'left))
          (pack cancel (side: 'left))
          (show Sel)
          (let-values ([(x y width height)
                        (if parent
                            (values
                              (get-root-x parent)
                              (get-root-y parent)
                              (send parent get-width)
                              (send parent get-height))
                            (values 0 0 (swl:screen-width) (swl:screen-height)))])
            (swl:raw-tcl-eval "update idletasks")
            (send top set-geometry!
              (format "+~s+~s"
                (+ x (quotient (- width (send top get-width)) 2))
                (+ y (quotient (- height (send top get-height)) 2)))))
)))))

)

#!eof

Add something like the following to accelerate font dialog
also, save results in the global preferences cache
and, be sure to kill the thread when the system exits

(define prefetch
  (lambda ()
    (import swl:oop)
    (import swl:option)
    (import swl:threads)
    (define sleep-delay 35)
    (define ls '(clean courier fixed lucidatypewriter terminal))
    (thread-fork
      (lambda ()
        (for-each
          (lambda (style)
            (for-each
              (lambda (size)
                (let loop ([ls ls])
                  (unless (null? ls)
                    (swl:font-available? (car ls) size style)
(printf "Check ~s ~s ~s ... " (car ls) size style)
(flush-output-port)
(pretty-print (swl:font-available? (car ls) size style))
                    (thread-sleep sleep-delay)
                    (loop (cdr ls)))))
              '(6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)))
; fix this to be the actual font specs we search for
          '(() (bold roman)))))))

