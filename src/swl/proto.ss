;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define-swl-class (<proto-textual> parent tcl-name) (<tk-widget> parent tcl-name)
  ;* An abstract class that understands some attributes common to
  ;* widgets that display text.
  (ivars)
  (inherited handle parent)
  (inheritable handle parent)
  (private)
  (protected)
  (public
    [set-font! (val)
     ;* \ret{unspecified}
     ;* Sets the font for text displayed in the widget.
     ;* \var{val} is an instance of \scheme{<font>} (see the description
     ;* of the \scheme{make-font} macro).
     (set-tk-option '-font val swl:font? 'set-font!)]
    [get-font ()
     ;* \ret{see below}
     ;* Returns an instance of \scheme{<font>} describing the font
     ;* for text displayed in the widget.
     (get-tk-option '-font tk->font 'get-font)]
    [set-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the foreground color for the instance to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-foreground val swl:color? 'set-foreground-color!)]
    [get-foreground-color ()
     ;* \ret{see below}
     ;* Returns the foreground color for the instance.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-foreground tk->color 'get-foreground-color)]
    [set-height/char! (val)
     ;* \ret{unspecified}
     ;* Sets the height of the widget in lines of text.
     ;* The default is zero indicating the widget should size itself.
     (set-tk-option '-height val swl:distance-unit? 'set-height!)]
    [get-height/char ()
     ;* \ret{integer}
     ;* Returns the height of the widget in lines of text.
     ;* If zero then the widget is determining its own size.
     (get-tk-option '-height string->number 'get-height)]
    [set-width/char! (val)
     ;* \ret{unspecified}
     ;* Sets the width of the widget in characters.
     ;* The default is zero indicating the widget should size itself.
     (set-tk-option '-width val swl:distance-unit? 'set-width!)]
    [get-width/char ()
     ;* \ret{integer}
     ;* Returns the width of the widget in characters.
     ;* If zero then the widget is determining its own size.
     (get-tk-option '-width string->number 'get-width)]
    [set-height! (val)
     ;* \ret{unspecified}
     ;* It is not presently possible to set the height of this widget
     ;* in pixels.  See \scheme{set-height/char!}.
     (assertion-violationf 'set-height! "cannot set height in pixels for ~a at present" (class-name self))]
    [set-width! (val)
     ;* \ret{unspecified}
     ;* It is not presently possible to set the width of this widget
     ;* in pixels.  See \scheme{set-width/char!}.
     (assertion-violationf 'set-width! "cannot set width in pixels for ~a at present" (class-name self))]))

(define-swl-class (<proto-label> parent tcl-name) (<proto-textual> parent tcl-name)
  ;* An abstract class for widgets that can display text or images.
  (ivars (image #f))   ;; determines whether image / text is displayed and saves image from GC
  (inherited handle parent)
  (inheritable handle parent)
  (private)
  (protected)
  (public
; merged with set-title!  get-title  (since only one can be displayed at a time)
;   [set-image! (img)
;    ;* Image support (bitmap, GIF, PPM, and PGM) is coming soon.
;    (set! image img) ;; saves image from collection
;    (set-tk-option '-image img isa-image? 'set-image!)]
;   [get-image ()
;    ;* Image support (bitmap, GIF, PPM, and PGM) is coming soon.
;    (get-tk-option '-image
;      (lambda (x) (swl:lookup-widget (string->symbol x))) 'get-image)]
    [set-title! (val)
     ;* \ret{unspecified}
     ;* Sets the content displayed by the widget to the string, \scheme{<bitmap>},
     ;* or \scheme{<photo>} given by \var{val}.
     ;; race condition here too
     (if (string? val)
         (begin
           (when image
             (set-tk-option '-image '() null? 'set-title!)
             (set! image #f)) ;; drop pointer to image so it can be collected
           (set-tk-option '-text val (lambda (x) x) 'set-title!))
         (begin
           (set-tk-option '-image val isa-image? 'set-title!)
           ;; saves image from collection
           (set! image val)))]
    [get-title ()
     ;* \ret{see below}
     ;* Returns the string, \scheme{<bitmap>}, or \scheme{<photo>} displayed by the widget.
     ;; stupid race condition here
     ;; what we really need is some way to prevent two threads from pinging
     ;; this pair of methods.
     (if image
         (get-tk-option '-image
           (lambda (x) (swl:lookup-widget (string->symbol x))) 'get-title)
         (get-tk-option '-text (lambda (x) x) 'get-title))]
    [set-wrap-length! (val)
     ;* \ret{unspecified}
     ;* Lines longer than \var{val} pixels will be wrapped onto the
     ;* next line.  If zero, lines break only where there are
     ;* newline characters in the displayed string.
     (set-tk-option '-wraplength val swl:distance-unit? 'set-wrap-length)]
    [get-wrap-length ()
     ;* \ret{integer}
     ;* Returns the wrap limit for the widget.  See \scheme{set-wrap-length!}.
     (get-tk-option '-wraplength string->number 'get-wrap-length)]
    [set-underline-index! (val)
     ;* \ret{unspecified}
     ;* \var{val} specifies the zero-based index of a character to
     ;* be underline when the string is displayed.  Set to
     ;* \scheme{#f} to disable the underline.
     (if val
         (set-tk-option '-underline val fixnum? 'set-underline-index!)
         (set-tk-option '-underline "" (lambda (x) x) 'set-underline-index!))]
    [get-underline-index ()
     ;* \ret{integer}
     ;* Returns the zero-based index of a character to
     ;* be underline when the string is displayed.
     ;* If \scheme{#f} the underline is disabled.
     (get-tk-option '-underline string->number 'get-underline-index)]
    [set-justify! (val)
     ;* \ret{unspecified}
     ;* Determines how lines are to be justified when multiple lines
     ;* are displayed.  Legal values are \scheme{left}, \scheme{right}, and \scheme{center}.
     (set-tk-option '-justify val swl:justify? 'set-justify!)]
    [get-justify ()
     ;* \ret{symbol}
     ;* Returns a symbol describing how lines are to be justified when
     ;* multiple lines
     ;* are displayed.
     (get-tk-option '-justify string->symbol 'get-justify)]
    [set-anchor! (val)
     ;* \ret{unspecified}
     ;* Determines where information in the widget is displayed.
     ;* Legal values are \scheme{n}, \scheme{s}, \scheme{e}, \scheme{w}, \scheme{ne}, \scheme{se},
     ;* \scheme{sw}, \scheme{nw}, and \scheme{center}.
     (set-tk-option '-anchor val swl:anchor? 'set-anchor!)]
    [get-anchor ()
     ;* \ret{symbol}
     ;* Returns a symbol describing where information in the
     ;* widget is displayed.
     (get-tk-option '-anchor string->symbol 'get-anchor)]))

(swl:api-procedure define swl:cut-buffer
  ;* \formdef{swl:cut-buffer}{procedure}{(swl:cut-buffer)}
  ;* \formdef{swl:cut-buffer}{procedure}{(swl:cut-buffer \var{text})}
  ;* \ret{see below}
  ;* This parameter implements a global cut buffer used to emulate the
  ;* behavior of paste in xterm.  When text is selected in a widget for
  ;* which get-export-selection returns true, subsequently clicking the mouse
  ;* saves the current selection in this global cut buffer before clearing
  ;* the selection.  When attempting to paste in a widget, we insert the
  ;* result of \scheme{(swl:get-selection 'primary)}, if available, or the
  ;* contents of the cut buffer.
  (make-parameter #f
    (lambda (x)
      (if (or (string? x) (not x))
          x
          (assertion-violationf 'swl:cut-buffer "~s is not a string or #f" x)))))

(define-swl-class (<proto-select-scroll-textual> parent tcl-name)
              (<proto-textual> parent tcl-name)
  ;* An abstract class for textual widgets that support selection
  ;* and scrolling.
  (ivars (outside? #f) (thread #f) (ox #f) (oy #f)
         (sel-start-pos #f) (sel-end-pos #f) (sel-anchor #f)
         (select-mode 'char))
  (inherited handle parent)
  (inheritable handle parent thread sel-start-pos sel-end-pos select-mode sel-anchor)
  (private)
  (protected
    [scroll-error (n qualifier who)
     (assertion-violationf who "~s is not ~a as required for ~s qualifier" n
       (case qualifier
         ((fraction) "a flonum or fixnum")
         ((pages units) "a fixnum"))
       qualifier)]
    [sanitized-xy->index (x y)
     ;; this gives me a way to finesse the y component in <entry>
     (xy->index self x y)]
    [paste-it ()
     (let ((str (or (swl:get-selection 'primary) (swl:cut-buffer))))
       (when (string? str)
         (send self insert str)))]
    [begin-selection (pos)
     ;; don't need to (clear-selection self) since select range has that effect
     (when (get-export-selection self)
       (let ([current-selection (swl:get-selection 'primary)])
         (when current-selection
           (swl:cut-buffer current-selection))))
     (mvlet ((start end)
             (case select-mode
               [(word) (values (word-start self pos) (word-end self pos))]
               [(line) (values (line-start self pos) (line-end self pos))]
               [else (values pos pos)]))
        (set! sel-anchor start)
        (select-range self start end))]
    [update-selection (cur mods select-mode)
     ;; feel a bit uneasy set!ing inherited sel-start-pos code to communicate
     ;; with the inherited selection mechanism...
     (if (not sel-anchor)
         (begin-selection (get-cursor-pos self))
         (mvlet ((start end)
                 (mvlet ((start end)
                         (if (pos<? self cur sel-anchor)
                             (values cur sel-anchor)
                             (values sel-anchor cur)))
                   (case select-mode
                     [(char) (values start end)]
                     [(word) (values
                               (send self word-start start)
                               (send self word-end end))]
                     [(line) (values
                               (send self line-start start)
                               (send self line-end end))])))
           (select-range self start end)))]
    [select-to-cursor (mods)
     (thread-critical-section
       (when sel-anchor
         (update-selection (get-cursor-pos self) mods select-mode)))]
    [end-selection (x y) (set! sel-anchor #f) (set! select-mode 'char)]
    [update-scroll (x y mods)
     ;; how about some gross manifest constants?  2, -2, 50
     (set! ox x)
     (set! oy y)
     (let ((scroll-it
             (lambda ()
               (let ([horz
                      (cond
                        [(< ox 0) (hscroll self -2 'units) #t]
                        [(> ox (get-width self)) (hscroll self 2 'units) #t]
                        [else #f])]
                     [vert
                      (cond
                        [(< oy 0) (vscroll self -2 'units) #t]
                        [(> oy (get-height self)) (vscroll self 2 'units) #t]
                        [else #f])])
                 (and (or horz vert)
                      (begin
                        (update-selection
                          (sanitized-xy->index ox oy) mods select-mode)
                        #t))))))
       (unless thread
         (set! thread (scroll-it))            ;; try to scroll at least once
         (when thread
           (set! thread
             (thread-fork
               (lambda ()
                 (let loop ()
                   (thread-sleep 50)
                   ;; check whether thread is thread-self to avoid race cond.
                   (when (and thread (eq? thread (thread-self)) (scroll-it))
                     (loop)))))))))]
    [end-scroll () (when thread (set! thread #f))])
  (public
    [mouse-motion (x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button])
        ;; If we're getting mouse motion for the widget, and the mouse is
        ;; outside the widget, try to scroll.
        (when outside? (update-scroll x y modifiers))
        (update-selection (sanitized-xy->index x y) modifiers select-mode))
       (else (send-base self mouse-motion x y modifiers)))]
    [mouse-press (x y modifiers)
     (event-case ((modifier= modifiers))
       (([shift left-button])
        (update-selection (sanitized-xy->index x y) modifiers select-mode))
       (([left-button])
        (event-case ((modifier= modifiers))
          (([triple]) (set! select-mode 'line))
          (([double]) (set! select-mode 'word))
          (else (set! select-mode 'char)))
        (set-focus self)
        (let ([new (sanitized-xy->index x y)])
          (begin-selection new)
          (set-cursor-pos! self new)))
       (else (send-base self mouse-press x y modifiers)))]
    [mouse-release (x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button]) (end-scroll) (end-selection x y))
       (([middle-button]) (paste-it)))
     (send-base self mouse-release x y modifiers)]
    [mouse-enter (x y modifiers)
     (set! outside? #f)
     (end-scroll)
     (send-base self mouse-enter x y modifiers)]
    [mouse-leave (x y modifiers)
     (set! outside? #t)
     (event-case ((modifier= modifiers))
       (([left-button]) (update-scroll x y modifiers)))
     (send-base self mouse-leave x y modifiers)]
    [key-press (key mods)
     (event-case ((key= key) (modifier= mods))
       (([control prior]) (hscroll self 0 'fraction))
       (([control next]) (hscroll self 1 'fraction))
       (([prior]) (hscroll self -1 'pages))
       (([next]) (hscroll self 1 'pages))
       (([shift_l] [shift_r])
        ;; When shift key is pressed, do a begin-selection in case we're
        ;; going to do some selecting
           ;; this is somewhat broken
           ;; hoped to make it possible to compose selection extensions done
           ;; with mouse or keyboard
           ;; fix needed in listbox.ss too
        (thread-critical-section
          (unless sel-anchor
            (begin-selection (get-cursor-pos self)))))
       (([shift left] [shift right] [shift up] [shift down]
         [shift prior] [shift next] [shift home] [shift end])
        ;; This gets done after the derived class has handled
        ;; left / right / up / down / page-up / page-down
        (select-to-cursor mods))
       (([control_l] [control_r] [alt_l] [alt_r] [caps_lock_l])
        ;; don't clear selection
        (void))
       (else (clear-selection self)
             (send-base self key-press key mods)))]
    [hscroll (n qualifier)
     ;* \ret{unspecified}
     ;* Scrolls the view in the widget horizontally.
     ;* If \var{qualifier} is \scheme{fraction}, then \var{n} indicates
     ;* what fraction of the widget contents should be moved out of
     ;* view to the left.
     ;* Otherwise, \var{n} is an integer indicating how many pages or
     ;* units to scroll
     ;* (negative values scroll left, positive scroll right), and
     ;* \var{qualifier} is either \scheme{pages}, or \scheme{units} (characters).
     (case qualifier
       ((fraction)
        (if (or (flonum? n) (fixnum? n))
            (swl:tcl-eval handle 'xview 'moveto n)
            (scroll-error n qualifier 'hscroll)))
       ((pages units)
        (if (fixnum? n)
            (swl:tcl-eval handle 'xview 'scroll n qualifier)
            (scroll-error n qualifier 'hscroll)))
       (else (assertion-violationf 'hscroll "invalid qualifier ~s" qualifier)))
     (void)]
    [vscroll (n qualifier)
     ;* \ret{unspecified}
     ;* Scrolls the view in the widget vertically.
     ;* If \var{qualifier} is \scheme{fraction}, then \var{n} indicates
     ;* what fraction of the widget contents should be moved out of
     ;* view to the top.
     ;* Otherwise, \var{n} is an integer indicating how many pages or
     ;* units to scroll
     ;* (negative values scroll up, positive scroll down), and
     ;* \var{qualifier} is either \scheme{pages}, or \scheme{units} (characters).
     (case qualifier
       ((fraction)
        (if (or (flonum? n) (fixnum? n))
            (swl:tcl-eval handle 'yview 'moveto n)
            (scroll-error n qualifier 'vscroll)))
       ((pages units)
        (if (fixnum? n)
            (swl:tcl-eval handle 'yview 'scroll n qualifier)
            (scroll-error n qualifier 'vscroll)))
       (else (assertion-violationf 'vscroll "invalid qualifier ~s" qualifier)))
     (void)]
    [get-horizontal-view ()
     ;* \ret{see below}
     ;* Returns a list of two floating-point values describing what
     ;* part of the widget's content is in view horizontally.
     ;* The first number indicates what fraction of the widget's
     ;* content is out of
     ;* view to the left and the second number indicates what fraction of
     ;* the widget's content is not out of view to the right.
     (swl:tcl->scheme (swl:tcl-eval handle 'xview))]
    [get-vertical-view ()
     ;* \ret{list}
     ;* Returns a list of two floating-point values describing what
     ;* part of the widget's content is in view vertically.
     ;* The first number indicates what fraction of the widget's
     ;* content is out of
     ;* view to the top and the second number indicates what fraction of
     ;* the widget's content is not out of view to the bottom.
     (swl:tcl->scheme (swl:tcl-eval handle 'yview))]
    [set-hscroll-notify! (val)
     ;* \ret{unspecified}
     ;* Specifies a procedure to be invoked when the horizontal view
     ;* or content of
     ;* the widget changes.  The procedure is passed two numbers between
     ;* 0 and 1 indicating what fraction of the widget's content is to
     ;* the left of the first position in the widget, and what fraction
     ;* is to the left of the end position of the widget.  Thus, the first
     ;* number indicates what fraction of the widget's content is out of
     ;* view to the left and the second number indicates what fraction of
     ;* the widget's content is not out of view to the right.
     ;*
     ;* If \var{val} is \scheme{#f}, then the existing notify action is cancelled.
     (if val
         (begin
           (swl:safety-check
             (unless (procedure? val)
               (assertion-violationf 'set-hscroll-notify! "~s is not a procedure" val)))
           (let ((cb (swl:procedure->callback val handle)))
             (prop-set! '-xscrollcommand cb)
             (set-tk-option '-xscrollcommand cb (lambda (x) x)
               'set-hscroll-notify!)))
         (begin
           (prop-set! '-xscrollcommand #f)
           (set-tk-option '-xscrollcommand "" (lambda (x) x)
             'set-hscroll-notify!)))]
    [get-hscroll-notify ()
     ;* \ret{see below}
     ;* Returns the procedure (or \scheme{#f} if none) that is notified when
     ;* the horizontal view or content of the widget is changed.
     (let ((cb (prop-ref '-xscrollcommand)))
        (and cb (send cb callback->procedure)))]
    [set-vscroll-notify! (val)
     ;* \ret{unspecified}
     ;* Specifies a procedure to be invoked when the vertical view
     ;* or content of
     ;* the widget changes.  The procedure is passed two numbers between
     ;* 0 and 1 indicating what fraction of the widget's content is above
     ;* the first position in the widget, and what fraction
     ;* is above the end position of the widget.  Thus, the first
     ;* number indicates what fraction of the widget's content is out of
     ;* view to the top and the second number indicates what fraction of
     ;* the widget's content is not out of view to the bottom.
     ;*
     ;* If \var{val} is \scheme{#f}, then the existing notify action is cancelled.
     (if val
         (begin
           (swl:safety-check
             (unless (procedure? val)
               (assertion-violationf 'set-vscroll-notify! "~s is not a procedure" val)))
           (let ((cb (swl:procedure->callback val handle)))
             (prop-set! '-yscrollcommand cb)
             (set-tk-option '-yscrollcommand cb (lambda (y) y)
               'set-vscroll-notify!)))
         (begin
           (prop-set! '-yscrollcommand #f)
           (set-tk-option '-yscrollcommand "" (lambda (x) x)
             'set-vscroll-notify!)))]
    [get-vscroll-notify ()
     ;* \ret{see below}
     ;* Returns the procedure (or \scheme{#f} if none) that is notified when
     ;* the vertical view or content of the widget is changed.
     (let ((cb (prop-ref '-yscrollcommand)))
        (and cb (send cb callback->procedure)))]
    [set-export-selection! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean value that determines whether or not
     ;* selection in the widget should also be the X selection.
     ;* When \scheme{#t} selecting in the widget deselects the current
     ;* X selection and vice versa.
     (set-tk-option '-exportselection val boolean? 'set-export-selection!)]
    [get-export-selection ()
     ;* \ret{boolean}
     ;* Returns a boolean that indicates whether or not selection
     ;* in the widget is also the X selection.
     (get-tk-option '-exportselection tk->boolean 'get-export-selection)]
    [set-select-background-color! (val)
     ;* \ret{unspecified}
     ;* Sets the background color for selected items in the
     ;* widget to \var{val}, which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-selectbackground val swl:color? 'set-select-background-color!)]
    [get-select-background-color ()
     ;* \ret{see below}
     ;* Returns the background color for selected items in the widget.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-selectbackground tk->color 'get-select-background-color)]
    [set-select-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the foreground color for selected items in the
     ;* widget to \var{val}, which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-selectforeground val swl:color? 'set-select-foreground-color!)]
    [get-select-foreground-color ()
     ;* \ret{see below}
     ;* Returns the foreground color for selected items in the widget.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-selectforeground tk->color 'get-select-foreground-color)]
    [set-select-border-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the raised 3-D border drawn around
     ;* selected items.  Setting to zero effectively disables the 3-D effect.
     (set-tk-option '-selectborderwidth val swl:distance-unit? 'set-select-border-width!)]
    [get-select-border-width ()
     ;* \ret{integer}
     ;* Returns the width in pixels of the raised 3-D border drawn around
     ;* selected items.
     (get-tk-option '-selectborderwidth string->number 'get-select-border-width)]
    [destroy ()
     ;; must kill any existing threads before we continue
     (when thread (thread-kill thread))
     (send-base self destroy)]))

(define-swl-class (<proto-editable> parent tcl-name)
              (<proto-select-scroll-textual> parent tcl-name)
  ;; parent class for <text> and <entry>
  (ivars (saved '()) (extend-saved? #f))
  (inherited handle parent thread sel-start-pos sel-end-pos select-mode sel-anchor)
  (inheritable handle parent thread sel-start-pos sel-end-pos select-mode sel-anchor)
  (private
    [echo-char (char)
     (thread-critical-section
       (delete-selection-at-cursor self)
       (insert self char))])
  (protected)
  (public
    [init (parent)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (set-insert-off-time! self 0)
     (send-base self init parent)]
    [key-press (key modifiers)
     ;; Here we always send to base class since we're adding to the set
     ;; of behaviors defined there.
     (event-case ((key= key) (modifier= modifiers))
       (([left] [control #\b] [kp_left]) (move-char self -1))
       (([right] [control #\f] [kp_right]) (move-char self 1))
       (([backspace] [control #\h] [kp_delete]) (delete-char self -1))
       (([control #\e] [end] [kp_end]) (cursor-eol self))
       (([control #\a] [home] [kp_home]) (cursor-bol self))
       (([control #\k])
        (if (cursor-eol? self)
            (unless (cursor-eof? self) (delete-char self 1))
            (delete-eol self)))
       (([control #\u])
        (cursor-bol self)
        (delete-eol self))
       (([control #\y]) (send self insert (send self get-saved-text)))
       (([delete] [control #\d]) (delete-char self 1))
       ;; To avoid inserting characters when control or alt is pressed
       ;; and the sequence is not otherwise handled.
       (([control] [alt] [caps_lock_l]) (void))
       (([#\space - #\~]) (echo-char key))
       (([kp_0]) (echo-char #\0))
       (([kp_1]) (echo-char #\1))
       (([kp_2]) (echo-char #\2))
       (([kp_3]) (echo-char #\3))
       (([kp_4]) (echo-char #\4))
       (([kp_5]) (echo-char #\5))
       (([kp_6]) (echo-char #\6))
       (([kp_7]) (echo-char #\7))
       (([kp_8]) (echo-char #\8))
       (([kp_9]) (echo-char #\9))
       (([kp_add]) (echo-char #\+))
       (([kp_subtract]) (echo-char #\-))
       (([kp_multiply]) (echo-char #\*))
       (([kp_divide]) (echo-char #\/))
       (([kp_decimal]) (echo-char #\.))      ; no doubt wrong for overseas
       (([kp_separator]) (echo-char #\,))    ; no doubt wrong for overseas
       (([shift insert]) (paste-it)) ; inherited protected method
       )
     (set! extend-saved?
       (event-case ((key= key) (modifier= modifiers))
         (([control #\k]) #t)
         (else #f)))
     (send-base self key-press key modifiers)]
    [set-saved-text! (s)
     ;* \ret{unspecified}
     ;* records the given string as the saved text to be restored by a yank.
     (unless (string? s) (assertion-violationf 'save-text "~s is not a string" s))
     (set! saved (if extend-saved? (cons s (cons "\n" saved)) (list s)))]
    [get-saved-text ()
     ;* \ret{string}
     ;* returns, as a Scheme string, the text preserved by save-text!
     ;* this is the yank operation.
     (apply string-append (reverse saved))]    ; don't want anyone to string-set! our local state
    [set-insert-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color for the insertion cursor of the widget
     ;* widget to \var{val}, which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-insertbackground val swl:color? 'set-insert-color!)]
    [get-insert-color ()
     ;* \ret{see below}
     ;* Returns the color for the insertion cursor of the widget
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-insertbackground tk->color 'get-insert-color)]
    [set-insert-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the insertion cursor for the widget.
     (set-tk-option '-insertwidth val swl:distance-unit? 'set-insert-width!)]
    [get-insert-width ()
     ;* \ret{integer}
     ;* Returns the width in pixels of the insertion cursor for the widget.
     (get-tk-option '-insertwidth string->number 'get-insert-width)]
    [set-insert-border-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the 3-D border drawn around the
     ;* insertion cursor for the widget.
     (set-tk-option '-insertborderwidth val swl:distance-unit? 'set-insert-border-width!)]
    [get-insert-border-width ()
     ;* \ret{integer}
     ;* Returns the width in pixels of the 3-D border drawn around the
     ;* insertion cursor for the widget.
     (get-tk-option '-insertborderwidth string->number 'get-insert-border-width)]
    [set-insert-on-time! (val)
     ;* \ret{unspecified}
     ;* Specifies the number of milliseconds the insertion cursor for
     ;* this widget will be visible during each blink cycle.
     (set-tk-option '-insertontime val
       (lambda (x) (and (fixnum? x) (not (fxnegative? x))))
       'set-insert-on-time!)]
    [get-insert-on-time ()
     ;* \ret{integer}
     ;* Returns the number of milliseconds the insertion cursor for
     ;* this widget will be visible during each blink cycle.
     (get-tk-option '-insertontime string->number 'get-insert-on-time)]
    [set-insert-off-time! (val)
     ;* \ret{unspecified}
     ;* Specifies the number of milliseconds the insertion cursor for
     ;* this widget will be hidden during each blink cycle.
     (set-tk-option '-insertofftime val
       (lambda (x) (and (fixnum? x) (not (fxnegative? x))))
       'set-insert-off-time!)]
    [get-insert-off-time ()
     ;* \ret{integer}
     ;* Returns the number of milliseconds the insertion cursor for
     ;* this widget will be hidden during each blink cycle.
     (get-tk-option '-insertofftime string->number 'get-insert-off-time)]
    [set-disabled-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the foreground color used when the widget is disabled
     ;* to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
(on-error (void)
; not valid in older versions of Tk
     (set-tk-option '-disabledforeground val swl:color? 'set-disabled-foreground-color!)
)
    ]
    [get-disabled-foreground-color ()
     ;* \ret{see below}
     ;* Returns the foreground color used when the widget is disabled.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
(on-error #f
     (get-tk-option '-disabledforeground tk->color 'get-disabled-foreground-color)
)
    ]
    [set-disabled-background-color! (val)
     ;* \ret{unspecified}
     ;* Sets the background color used when the widget is disabled
     ;* to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
(on-error (void)
; not valid in older versions of Tk
     (set-tk-option '-disabledbackground val swl:color? 'set-disabled-background-color!)
)
    ]
    [get-disabled-background-color ()
     ;* \ret{see below}
     ;* Returns the background color used when the widget is disabled.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
(on-error #f
     (get-tk-option '-disabledbackground tk->color 'get-disabled-background-color)
)
    ]
    [set-enabled! (val)
     ;* \ret{unspecified}
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


;; also needs modal stuff
(define-swl-class (<proto-button> parent tcl-name) (<proto-label> parent tcl-name)
  ;* An abstract class that captures some common properties of buttons.
  ;*
  ;* Note:
  ;* several button capabilities have been carried over from Tk
  ;* for historic reasons and may be removed after further consideration.
  (ivars (action (lambda (self) (void))))
  (inherited handle parent)
  (inheritable handle parent)
  (private)
  (protected
    [show-pressed ()
     (prop-set! 'old-relief (get-relief self))
     (set-relief! self 'sunken)]
    [show-released (x y modifiers)
     ;; kill the property
     (let ([old-relief (prop-ref 'old-relief)])
       (when old-relief (set-relief! self old-relief))
       ;; only invoke the stupid button if it's been released
       ;; within the button confines (may break for 1 pixel buttons)
       (when (and (> x 0)
                  (> y 0)
                  (< x (get-width self))
                  (< y (get-height self)))
         (invoke self)))]
    [button-action (name)
     (swl:tcl-eval handle name)
     (void)])
  (public
    [mouse-enter (x y modifiers)
     (send-base self mouse-enter x y modifiers)
     (activate self)]
    [mouse-leave (x y modifiers)
     (send-base self mouse-leave x y modifiers)
     (deactivate self)]
    [mouse-press (x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button])
        (when (get-enabled self) (show-pressed)))
       (else (send-base self mouse-press x y modifiers)))]
    [mouse-release (x y modifiers)
     ;; Retrieve and reset the old relief before invoking the action
     ;; since action could change the old  or new relief.
  ; eventually come back and replace the (invoke self) garbage
  ; with code that invokes our action.  this could basically
  ; signal an event queue where a preemptable thread would unblock
  ; and run the code.  might need to fork a thread if it uses up
  ; its time quantum because we don't want to hold up other events
  ; just because some bozo forgot to fork a thread for his huge
  ; computation
     (event-case ((modifier= modifiers))
       (([left-button])
        ;; bug.  what if the enabled state has changed since they
        ;; pressed the button
        (when (get-enabled self) (show-released x y modifiers)))
       (else (send-base self mouse-release x y modifiers)))]
    [key-press (key modifiers)
     ;* \ret{unspecified}
     ;* A button executes its action when \mytt{space} or \mytt{return} is pressed.
     (event-case ((key= key) (modifier= modifiers))
       (([#\space] [#\return]) (invoke self))
       (else (send-base self key-press key modifiers)))]
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
    [set-action! (val)
     ;* \ret{unspecified}
     ;* \var{val} specifies a procedure of no arguments to be called when
     ;* the widget is activated by clicking the mouse or by pressing
     ;* \mytt{Return} or \mytt{Space} when the widget has keyboard focus.
     (swl:safety-check
        (unless (procedure? val)
           (assertion-violationf 'set-action! "~s is not a procedure" val)))
     (set! action val)]
    [get-action ()
     ;* \ret{procedure}
     ;* Returns the procedure of no arguments to be invoked when the
     ;* widget is clicked or pressed.
     action]
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
     (get-tk-option '-state (lambda (x) (not (string=? x "disabled"))) 'get-enabled)]
    [activate ()
     ;* \ret{unspecified}
     ;* Redisplays the button using the "active-foreground" color
     ;* (so it looks as it would if the mouse were over the widget).  At
     ;* present it has no effect if the button has been disabled.
     ;* (This seems to be of pretty limited usefulness).
     (when (and handle (get-enabled self))
       (swl:tcl-eval handle 'config '-state 'active))
     (void)]
    [deactivate ()
     ;* \ret{unspecified}
     ;* Redisplays the button using the "normal-foreground" color
     ;* (so it looks as it would if the mouse were not over the widget).  At
     ;* present it has no effect if the button has been disabled.
     ;* (This seems to be of pretty limited usefulness).
     (when (and handle (get-enabled self))
       (swl:tcl-eval handle 'config '-state 'normal))
     (void)]
    [flash ()
     ;* \ret{unspecified}
     ;* flashes the button briefly. expect this to go away.
     (button-action 'flash)]
    [invoke ()
     ;* \ret{see below}
     ;* Invokes the action (if any) associated with this widget by
     ;* \scheme{set-action!} as if the widget had been pressed or clicked.
     ;* (This also seems of limited use)
     (action self)]))

(define-swl-class (<proto-fancybutton> parent tcl-name)
              (<proto-button> parent tcl-name)
  ;* Abstract class that deals with some of the fancier buttons
  ;* that show their state by means of  two alternative images
  ;* or a small square or diamond (the indicator) filled or not filled
  ;* with a color.
  ;*
  ;* Note:
  ;* we may soon provide an easier way to query the state of
  ;* checkbuttons and radiobuttons that are derived from this class.
  (ivars (selectimage #f))
  (inherited handle parent)
  (inheritable handle parent)
  (private)
  (protected)
  (public
    [mouse-press (x y modifiers)
     ;* \ret{unspecified}
     ;* Because the indicator provides visual feedback, we don't change
     ;* the relief when a fancy button is pressed.
     (void)]
    [set-title! (val)
     ; documentation inherited
     ;% \ret{unspecified}
     ;% Sets the text displayed by the widget to the string given by \var{val}.
     (set! selectimage #f)
     (send-base self set-title! val)]
    [set-select-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color used to show when the widget is
     ;* selected.  If the indicator is drawn (see \scheme{set-draw-indicator!})
     ;* it is filled with this color when the widget is selected, otherwise
     ;* this color is used as the background color for the widget when
     ;* selected.
     ;* \var{val} is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     ;* The color is red.
     (set-tk-option '-selectcolor val swl:color? 'set-select-color!)]
    [get-select-color ()
     ;* \ret{see below}
     ;* Returns the color used to show when the widget is selected
     ;* (see \scheme{set-select-color!}).
     (get-tk-option '-selectcolor tk->color 'get-select-color)]
    [set-draw-indicator! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean value determining whether or not the state
     ;* of the widget is indicated by drawing a small square or diamond
     ;* filled or not filled with the select color (see \scheme{set-select-color!}).
     (set-tk-option '-indicatoron val boolean? 'set-draw-indicator!)]
    [get-draw-indicator ()
     ;* \ret{see below}
     ;* Returns a boolean value indicating whether or not the state
     ;* of the widget is depicted by drawing a small square or diamond
     ;* filled or not filled.
     (get-tk-option '-indicatoron tk->boolean 'get-draw-indicator)]
    [set-select-image! (val)
     ;* \ret{unspecified}
     ;* Sets the image to be displayed when this item is selected.
     (set-tk-option '-selectimage val swl:image? 'set-select-image!)
     (set! selectimage val)]
    [get-select-image ()
     ;* \ret{see below}
     ;* Returns the image displayed when this item is selected.
     (get-tk-option '-selectimage tk->image 'get-select-image)]
    [invoke ()
     ;* \ret{see below}
     ;* Invokes the action bound to this widget.
     (swl:tcl-eval handle 'invoke)
     (send-base self invoke)]
    [deselect ()
     ;* \ret{unspecified}
     ;* deselects the widget.
     (button-action 'deselect)]
    [select ()
     ;* \ret{unspecified}
     ;* selects the widget.
     (button-action 'select)]))

(define-swl-class (<proto-slider> parent tcl-name) (<tk-widget> parent tcl-name)
  ;* Abstract class for scales and scrollbars.
  (ivars (drag #f) (thread #f) (action (lambda args (void))))
  (inherited handle parent)
  (inheritable handle parent drag action)
  (private)
  (protected
    [identify (x y) (swl:tcl-eval handle 'identify x y)]
    [set-drag! (x y) (void)]
    [set-slider-relief! (relief) (void)]
    [small-slide (fn) (void)]
    [big-slide (fn) (void)]
    [activate (x y) (void)]
    [deactivate (x y) (void)]
    [slider-hit (x y) (void)]
    [end-slide () (when thread (set! thread #f))]
    [begin-slide (thunk)
     ;; do the action once up front
     (unless thread
       (set! thread
         (thread-fork
           (lambda ()
             (thunk)
             (thread-sleep (get-repeat-delay self))
             (let f ([delay (get-repeat-interval self)])
               (thread-sleep delay)
               (when (eq? thread (thread-self)) ;; resolve race condition
                 (thunk)
                 (f delay)))))))])
  (public
    [key-press (key modifiers)
     (event-case ((key= key) (modifier= modifiers))
       (([control up] [control left] [prior]) (big-slide -))
       (([control down] [control right] [next]) (big-slide +))
       (([up] [left]) (small-slide -))
       (([down] [right]) (small-slide +))
       (else (send-base self key-press key modifiers)))]
    [mouse-enter (x y modifiers)
     (send-base self mouse-enter x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button]) (void))
       (else (activate x y)))]
    [mouse-leave (x y modifiers)
     (send-base self mouse-leave x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button]) (void))
       (else (deactivate x y)))]
    [mouse-press (x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button]) (slider-hit x y))
       (else (send-base self mouse-press x y modifiers)))]
    [mouse-release (x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button])
        (end-slide)
        (set! drag #f)
        (set-slider-relief! 'raised))
       (else (send-base self mouse-release x y modifiers)))]
    [mouse-motion (x y modifiers)
     (event-case ((modifier= modifiers))
       (([left-button]) (when drag (drag x y)))
       (else
        (activate x y)
        (send-base self mouse-motion x y modifiers)))]
    [set-action! (val)
     ;* \ret{unspecified}
     ;* \var{val} specifies a procedure to be called when
     ;* the widget is activated by clicking or dragging the mouse
     ;* or by pressing the arrow keys when the widget has keyboard focus.
     ;*
     ;* For instances of \scheme{<scale>} the procedure is passed two arguments:
     ;* the instance and the current value of the scale.
     ;*
     ;* For instances of \scheme{<scrollbar>} the procedure is passed
     ;* the instance and
     ;* two additional arguments matching the arguments of the \scheme{hscroll}
     ;* and \scheme{vscroll} methods of the various scrollable widgets.
     (swl:safety-check
       (unless (procedure? val)
         (assertion-violationf 'set-action! "~s is not a procedure" val)))
     (set! action val)]
    [get-action ()
     ;* \ret{procedure}
     ;* Returns the procedure that is called when the widget is manipulated by
     ;* dragging or clicking the mouse, or pressing arrow keys when the widget
     ;* has input focus.  See \scheme{set-action!} for more details.
     action]
    [set-orientation! (val)
     ;* \ret{unspecified}
     ;* Specifies the desired orientation of the widget.
     ;* \scheme{val} is either \mytt{horizontal} or \mytt{vertical}.
     (set-tk-option '-orient val (lambda (x) (memq x '(horizontal vertical)))
       'set-orientation!)]
    [get-orientation ()
     ;* \ret{symbol}
     ;* Returns the orientation of the widget,
     ;* either \mytt{horizontal} or \mytt{vertical}.
     (get-tk-option '-orient string->symbol 'get-orientation)]
    [set-active-background-color! (val)
     ;* \ret{unspecified}
     ;; could have played games to get it by inheritance
     ;; but since I'm not too sure about the merit of this method
     ;; why bother?
     ;* Sets the foreground color used when the mouse cursor is over
     ;* the widget to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-activebackground val swl:color? 'set-active-background-color!)]
    [get-active-background-color ()
     ;* \ret{see below}
     ;; could have played games to get it by inheritance
     ;; but since I'm not too sure about the merit of this method
     ;; why bother?
     ;* Returns the foreground color used when the mouse cursor is
     ;* over the widget.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-activebackground tk->color 'get-active-background-color)]
    [set-trough-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color for the trough (in which the slider
     ;* of the widget moves) to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-troughcolor val swl:color? 'set-trough-color!)]
    [get-trough-color ()
     ;* \ret{see below}
     ;* Returns the color for the trough (in which the slider
     ;* of the widget moves) to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-troughcolor tk->color 'get-trough-color)]
    [set-repeat-delay! (val)
     ;* \ret{unspecified}
     ;* Sets the number of milliseconds a button or key must be held down
     ;* before it begins to autorepeat.
     (set-tk-option '-repeatdelay val (lambda (x) (and (fixnum? x) (not (fxnegative? x)))) 'set-repeat-delay!)]
    [get-repeat-delay ()
     ;* \ret{integer}
     ;* Returns the number of milliseconds a button or key must be held down
     ;* before it begins to autorepeat.
     (get-tk-option '-repeatdelay string->number 'get-repeat-delay)]
    [set-repeat-interval! (val)
     ;* \ret{unspecified}
     ;* Sets the number of milliseconds to delay between auto-repeats.
     (set-tk-option '-repeatinterval val (lambda (x) (and (fixnum? x) (not (fxnegative? x)))) 'set-repeat-interval!)]
    [get-repeat-interval ()
     ;* \ret{integer}
     ;* Returns the number of milliseconds to delay between auto-repeats.
     (get-tk-option '-repeatinterval string->number 'get-repeat-interval)]
    [set-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels
     ;* of the narrow dimension of the scrollbar or scale.
     (set-tk-option '-width val swl:distance-unit? 'set-width!)]
    [destroy ()
     ;; must kill thread before proceeding.
     (when thread (thread-kill thread))
     (send-base self destroy)]))

