;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Canvas
;;

(module ((<canvas> make-canvasitem-mouse-leave-cbproc))

(swl:api-class (<canvas> parent) (<container-widget> parent 'canvas)
  ;% \swlapi{class}{<canvas>}{(create <canvas> parent)}
  ;* \ret{instance}
  ;* A canvas
  ;* provides screen real-estate on which a variety of figures can be
  ;* drawn:  rectangles, ovals, arcs, lines, images, polygons, text,
  ;* and sub-windows.  Items on the canvas can be moved, raised,
  ;* lowered, etc.  Canvases can generate Encapsulated PostScript
  ;* representations of part or all of their area.
  ;* Canvases can be scrolled vertically and horizontally.
  ;*
  ;* See the descriptions of the various canvas items for more details on the
  ;* kinds of figures that can appear on a canvas.
  (ivars (children '()) (item #f) (last-item #f))
  (inherited handle)
  (inheritable handle)
  (private
    [priv-find-item ()
     (or item
         (let ([x
                (id->canvas-item
                  (string->number
                    (swl:tcl-eval handle 'find 'withtag 'current)))])
           (set! item x)
           x))]
    [invalidate-item-cache (x) (when (eq? x item) (set! item #f))]
    [leave-find-item ()
     (let ([x (or item (priv-find-item))]) (set! last-item #f) (set! item #f) x)]
    [real-disown (id child)
     (invalidate-item-cache child)
     (bintree-delete! id child children)
     (swl:tcl-eval handle 'delete id)]
    [setup-callback (event cbproc)
     (swl:tcl-eval handle 'bind 'all event cbproc)
     ;; protect the cbproc from GC
     (prop-set! event cbproc)]
    [rect-find (x1 y1 x2 y2 name)
     (swl:safety-check
       (unless (and (swl:distance-unit? x1) (swl:distance-unit? y1)
                    (swl:distance-unit? x2) (swl:distance-unit? y2)
                    (<= x1 x2) (<= y1 y2))
         (assertion-violationf (format "find-~a" name) "invalid coordinates ~s ~s ~s ~s"
            x1 y1 x2 y2)))
     (let ((ids (swl:tcl-eval handle 'find name x1 y1 x2 y2)))
       (if (eq? ids "")
           '()
           (let ((ids (swl:tcl->scheme ids)))
              (if (pair? ids)
                  (map (lambda (x) (id->canvas-item x)) ids)
                  (id->canvas-item ids)))))]
    [bintree-insert! (id child tree)
     ;; eventually replace w/ a balanced binary tree implementation
     (thread-critical-section
       (set! children (cons (cons id child) children)))]
    [bintree-delete! (id child tree)
     ;; eventually replace w/ a balanced binary tree implementation
     (thread-critical-section
       (set! children
         (let loop ((ls children))
           (cond
             ((null? ls) '())
             ((= id (caar ls)) (cdr ls))
             (else (cons (car ls) (loop (cdr ls))))))))]
    [bintree-lookup (id tree)
     ;; eventually replace w/ a balanced binary tree implementation
     (let ((x (assv id tree))) (and x (cdr x)))]
    [for-leaves (fn tree)
     ;; eventually replace w/ a balanced binary tree implementation
     (for-each (lambda (x) (fn (cdr x))) tree)]
    [id->canvas-item (id) (bintree-lookup id children)]
    [screen->canvas (op n)
     (swl:safety-check
       (unless (swl:oknum? n)
         (assertion-violationf (if (eq? op 'canvasx) 'screen->canvas-x 'screen->canvas-y)
            "invalid screen coordinate ~s" n)))
     (flonum->fixnum (string->number (swl:tcl-eval handle op n)))])
  (protected
    [torch-kids ()
     ;; would like to find some way to avoid the Tk overhead of seeing
     ;; whether we can kill the figures...
     ;; (maybe call special parent-is-killing-you method)
     (for-leaves (lambda (item) (send item destroy)) children)])
  (public
    [mouse-motion (x y mods)
     ;% \ret{unspecified}
     (let ([item (priv-find-item)])
       (when item
         (if (eq? item last-item)
             (send item mouse-motion x y mods)
             (send item mouse-enter x y mods)))
       (set! last-item item))]
    [mouse-press (x y mods)
     ;% \ret{unspecified}
     (let ((item (priv-find-item)))
       (when item (send item mouse-press x y mods)))]
    [mouse-release (x y mods)
     ;% \ret{unspecified}
     (let ((item (priv-find-item)))
       (when item (send item mouse-release x y mods)))]
    [mouse-enter (x y mods)
     ;% \ret{unspecified}
     (let ((item (priv-find-item)))
       (when item (send item mouse-enter x y mods)))]
    [mouse-leave (x y mods)
     ;% \ret{unspecified}
     (let ((item (priv-find-item)))
       (when item (send item mouse-leave x y mods)))]
    [key-press (key mods)
     ;% \ret{unspecified}
     (let ((item (priv-find-item)))
       (when item (send item key-press key mods)))]
    [key-release (key mods)
     ;% \ret{unspecified}
     (let ((item (priv-find-item)))
       (when item (send item key-release key mods)))]

    [scheme->tcl (op)
     ;% \ret{unspecified}
     (display handle op)]  ;; reprise here for efficiency
    [init (parent)
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (send-base self init parent)
      ;; The usual mechanism we use to correlate the Tk widget that
      ;; received the event with it's Scheme counterpart does not work
      ;; for canvases.  Instead we use the 'current tag supported by Tk
      ;; canvases.  Unfortunately, for leave events we've already left,
      ;; so no canvas item has the 'current tag.  Hence the cache here.
      ;; This should work as long as the order of operations (with leave
      ;; coming after any others) is respected.
;    (let* ([item #f]
;           [priv-find-item
;            (lambda ()
;              (or item
;                  (let ((x (id->canvas-item
;                             (string->number
;                               (swl:tcl-eval handle 'find 'withtag 'current)))))
;                    (set! item x)
;                    x)))]
;           [clear-cache (lambda (x) (when (eq? x item) (set! item #f)))]
;           [leave-find-item
;            (lambda ()
;              (let ([x (or item (priv-find-item))]) (set! item #f) x))])
;      (set! invalidate-item-cache clear-cache) 
;      (setup-callback '|<KeyPress>|
;        (make-canvasitem-keypress-cbproc priv-find-item))
;      (setup-callback '|<KeyRelease>|
;        (make-canvasitem-keyrelease-cbproc priv-find-item))
;      (setup-callback '|<ButtonPress>|
;        (make-canvasitem-mousepress-cbproc priv-find-item))
;      (setup-callback '|<Double-ButtonPress>|
;        (make-canvasitem-multi-mousepress-cbproc priv-find-item 2048))
;      (setup-callback '|<Triple-ButtonPress>|
;        (make-canvasitem-multi-mousepress-cbproc priv-find-item 4096))
;      (setup-callback '|<ButtonRelease>|
;        (make-canvasitem-mouserelease-cbproc priv-find-item))
;      (setup-callback '|<Double-ButtonRelease>|
;        (make-canvasitem-multi-mouserelease-cbproc priv-find-item 2048))
;      (setup-callback '|<Triple-ButtonRelease>|
;        (make-canvasitem-multi-mouserelease-cbproc priv-find-item 4096))
;      (setup-callback '|<Motion>|
;        (make-canvasitem-mousemotion-cbproc priv-find-item))
;      (setup-callback '|<Enter>|
;        (make-canvasitem-mouse-enter-cbproc priv-find-item))
 ;;; need this guy to help us invalidate the item-cache
       (setup-callback '|<Leave>|
         (make-canvasitem-mouse-leave-cbproc (lambda () (leave-find-item))))
;)
    ]
    [get-items ()
     ;* \ret{list}
     ;* Returns a list of canvas items contained in this canvas.
     (map cdr children)]
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
    [set-hscroll-increment! (val)
     ;* \ret{unspecified}
     ;* Constrains horizontal scrolling so that the x-coordinate
     ;* at the left edge of the widget is an even multiple of
     ;* \var{val} pixels.  The \scheme{hscroll}
     ;* method will scroll by \var{val} pixels when scrolling by \scheme{units}.
     ;* If \var{val} is zero, scrolling is unconstrained.
     (set-tk-option '-xscrollincrement val swl:distance-unit? 'set-hscroll-increment!)]
    [get-hscroll-increment ()
     ;* \ret{integer}
     ;* Returns the value set by \scheme{set-hscroll-increment!}.
     (get-tk-option '-xscrollincrement string->number 'get-hscroll-increment)]
    [set-vscroll-increment! (val)
     ;* \ret{unspecified}
     ;* Constrains vertical scrolling so that the y-coordinate
     ;* at the top edge of the widget is an even multiple of
     ;* \var{val} pixels.  The \scheme{vscroll}
     ;* method will scroll by \var{val} pixels when scrolling by \scheme{units}.
     ;* If \var{val} is zero, scrolling is unconstrained.
     (set-tk-option '-yscrollincrement val swl:distance-unit? 'set-vscroll-increment!)]
    [get-vscroll-increment ()
     ;* \ret{integer}
     ;* Returns the value set by \scheme{set-vscroll-increment!}.
     (get-tk-option '-yscrollincrement string->number 'get-vscroll-increment)]
    [set-confine! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean that determines whether or not the
     ;* canvas is prevented from scrolling the view of the canvas outside the
     ;* region defined by \scheme{set-scroll-region!}.
     (set-tk-option '-confine val boolean? 'set-confine!)]
    [get-confine ()
     ;* \ret{boolean}
     ;* Returns a boolean indicating whether or not the
     ;* canvas is prevented from scrolling the view of the canvas outside the
     ;* region defined by \scheme{set-scroll-region!}.
     (get-tk-option '-confine tk->boolean 'get-confine)]
    [set-close-enough! (val)
     ;* \ret{unspecified}
     ;* \var{val} determines how close the mouse cursor must be before
     ;* it is considered to be inside an item.
     (set-tk-option '-closeenough val swl:distance-unit? 'set-close-enough!)]
    [get-close-enough ()
     ;* \ret{a floating point number}
     ;* Returns a floating-point value indicating
     ;* how close the mouse cursor must be before
     ;* it is considered to be inside an item.
     (get-tk-option '-closeenough string->number 'get-close-enough)]
    [set-scroll-region! (x1 y1 x2 y2)
     ;* \ret{unspecified}
     ;* The region defined by \var{x1}, \var{y1}, \var{x2}, and \var{y2} is
     ;* taken to be the boundary of the information on the canvas.
     (swl:safety-check
       (unless (and (swl:distance-unit? x1) (swl:distance-unit? y1)
                    (swl:distance-unit? x2) (swl:distance-unit? y2))
         (assertion-violationf 'set-scroll-region! "bad value(s) ~s ~s ~s ~s" x1 y1 x2 y2)))
     (set-tk-option '-scrollregion (list x1 y1 x2 y2)
       (lambda (x) x) 'set-scroll-region!)]
    [get-scroll-region ()
     ;* \ret{list}
     ;* Returns a list of the coordinates defining
     ;* the boundary of the information on the canvas.
     (get-tk-option '-scrollregion swl:tcl->scheme 'get-scroll-region)]
    [adopt (id child)
     ;* \ret{unspecified}
     ;* internal use
     (bintree-insert! id child children)
     handle]
    [disown (id child)
     ;* \ret{unspecified}
     ;* internal use
     (real-disown id child)]
    [screen->canvas-x (screenx)
     ;* \ret{integer}
     ;; just now decided the gridspacing option is stupid since we can
     ;; do that kind of computation in Scheme
     ;* Returns the canvas x-coordinate corresponding to the given
     ;* screen x-coordinate.
     (screen->canvas 'canvasx screenx)]
    [screen->canvas-y (screeny)
     ;* \ret{integer}
     ;; just now decided the gridspacing option is stupid since we can
     ;; do that kind of computation in Scheme
     ;* Returns the canvas y-coordinate corresponding to the given
     ;* screen y-coordinate.
     (screen->canvas 'canvasy screeny)]
    [find-item (relpos item)
     ;* \ret{see below}
     ;* Returns the canvas item above or below \var{item} in
     ;* the display list or \scheme{#f} if none.
     ;*  \var{relpos} is either \scheme{above} or \scheme{below}.
     (swl:safety-check
       (unless (memq relpos '(above below))
         (assertion-violationf 'find-item "expected above or below instead of ~s" relpos))
       (unless (isa-canvas-item? item)
         (assertion-violationf 'find-item "~s is not a canvas item" item)))
     (let ((id (string->number (swl:tcl-eval handle 'find relpos item))))
       (and id (id->canvas-item id)))]
    [find-closest (x y)
     ;* \ret{see below}
     ;* Returns the topmost canvas item closest to the canvas coordinates
     ;* \var{x} and \var{y} or \scheme{#f} if none.
     (swl:safety-check
       (unless (and (swl:distance-unit? x) (swl:distance-unit? y))
         (assertion-violationf 'find-closest "invalid coordinates ~s ~s" x y)))
     (let ((id (string->number (swl:tcl-eval handle 'find 'closest x y))))
       (and id (id->canvas-item id)))]
    [find-enclosed (x1 y1 x2 y2)
     ;* \ret{see below}
     ;* Returns a list of the items completely enclosed by the rectangular
     ;* region defined by \var{x1}, \var{y1}, \var{x2}, and \var{y2}.
     (rect-find x1 y1 x2 y2 'enclosed)]
    [find-overlapping (x1 y1 x2 y2)
     ;* \ret{see below}
     ;* Returns a list of the items overlapped by the rectangular
     ;* region defined by \var{x1}, \var{y1}, \var{x2}, and \var{y2}.
     (rect-find x1 y1 x2 y2 'overlapping)]
    [postscript ()
     ;* \ret{string}
     ;* generate Encapsulated PostScript for the canvas
     ;*
     ;* (This may be enriched in the future.)
     (swl:tcl-eval handle 'postscript)]
    [hscroll (n qualifier)
     ;* \ret{unspecified}
     ;* Scrolls the view in the widget horizontally.
     ;* If \var{qualifier} is \scheme{fraction}, then \var{n} indicates
     ;* what fraction of the widget contents should be moved out of
     ;* view to the left.
     ;* Otherwise, \var{n} is an integer indicating how many pages or
     ;* units to scroll
     ;* (negative values scroll left, positive scroll right), and
     ;* \var{qualifier} is either \scheme{pages}, or \scheme{units} (pixels).
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
     ;* \var{qualifier} is either \scheme{pages}, or \scheme{units} (pixels).
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
    [destroy ()
     ;* \ret{unspecified}
     ;; copied this from container-widget because the disown methods are diff
     ;* Destroys the widget and its children.
     (send self hide)
     (critical-section
       (vector-set! self 0
         (let ([err (lambda (msg . args)
                      (assertion-violationf msg "instance has been destroyed"))])
           (swl:dispatch-table err
             [scheme->tcl (op) (display handle op)]
             [disown (id child)
              (real-disown id child)
              (when (null? children) (send-base self destroy))]
; this is gross, but then so is it all
             [key-press (k m) (void)]
             [key-release (k m) (void)]
             [mouse-press (x y m) (void)]
             [mouse-enter (x y m) (void)]
             [mouse-leave (x y m) (void)]
             [mouse-motion (x y m) (void)]
             [mouse-release (x y m) (void)]
             [configure (w h) (void)]
             [hide () (void)]
             [destroy () (void)])))
       (torch-kids))]
))

(ctdef make-canvasitem-mouse-leave-cbproc
  (lambda (get-canvas-item)
    (swl:callback-lambda (widget x y state)
      (let ((item (get-canvas-item)))
        ;; see comments above in init method concerning the 'current tag
        (when item (send item mouse-leave x y state))))))

)

; (define make-canvasitem-mouse-enter-cbproc
;   (lambda (get-canvas-item)
;     (swl:callback-lambda (widget x y state)
;       (let ((item (get-canvas-item)))
;         ;; see comments above in init method concerning the 'current tag
;         (when item (send item mouse-enter x y state))))))
; 
; (define make-canvasitem-keypress-cbproc
;   (lambda (get-canvas-item)
;     (swl:callback-lambda (widget keysym-decimal state)
;       (let ((item (get-canvas-item)))
;         (when item
;           (send item key-press (decimal->key keysym-decimal) state))))))
; 
; (define make-canvasitem-keyrelease-cbproc
;   (lambda (get-canvas-item)
;     (swl:callback-lambda (widget keysym-decimal state)
;       (let ((item (get-canvas-item)))
;         (when item
;           (send item key-release (decimal->key keysym-decimal) state))))))
; 
; ;; or-ing button number into the modifiers being passed to mouse-press method
; (define make-canvasitem-mousepress-cbproc
;   (lambda (get-canvas-item)
;     (swl:callback-lambda (widget x y button-number state)
;       (let ((item (get-canvas-item)))
;         (when item
;           (send item mouse-press x y
;             (fxlogor (fxsll 1 (fx+ 7 button-number)) state)))))))
; 
; ;; here button-number should already be implied in the state field, though
; ;; we may be missing exactly the button that was released which would be
; ;; bogus
; (define make-canvasitem-mouserelease-cbproc
;   (lambda (get-canvas-item)
;     (swl:callback-lambda (widget x y state)
;       (let ((item (get-canvas-item)))
;         (when item (send item mouse-release x y state))))))
; 
; (define make-canvasitem-multi-mousepress-cbproc
;   (lambda (get-canvas-item bit)
;     (swl:callback-lambda (widget x y button-number state)
;       (let ((item (get-canvas-item)))
;         (when item
;           (send item mouse-press x y
;             (fxlogor bit (fxlogor (fxsll 1 (fx+ 7 button-number)) state))))))))
; 
; (define make-canvasitem-multi-mouserelease-cbproc
;   (lambda (get-canvas-item bit)
;     (swl:callback-lambda (widget x y state)
;       (let ((item (get-canvas-item)))
;         (when item
;           (send item mouse-release x y (fxlogor bit state)))))))
; 
; (define make-canvasitem-mousemotion-cbproc
;   (lambda (get-canvas-item)
;     (swl:callback-lambda (widget x y state)
;       (let ((item (get-canvas-item)))
;         (when item (send item mouse-motion x y state))))))

