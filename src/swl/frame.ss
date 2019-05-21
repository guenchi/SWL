;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame
;;

; should consider the following wm functionality
;  - wm grid window ?baseWidth baseHeight widthInc heightInc?
;  - wm group window ?pathName?
;      - perhaps <toplevel> should take optional parent argument that
;        sets this group thing automatically (or does a send parent adopt self)
;        that does this for us.
;  - wm iconbitmap window ?bitmap?
;  - wm iconbitmask window ?bitmap?
;  - wm iconname window ?newName?
;  - wm iconposition window ?x y?
;  - wm overrideredirect window ?boolean?
;  - wm protocol window WM_TAKE_FOCUS {s_eval set-focus callback on this window}
;  - wm protocol window WM_SAVE_YOURSELF ???


(swl:api-class (<frame> parent) (<container-widget> parent 'frame)
  ;% \swlapi{class}{<frame>}{(create <frame> parent)}
  ;* \ret{instance}
  ;;
  ;* A frame provides a simple container for other widgets.
  ;* Frames are primarily useful for
  ;* building more complex window layouts and consequently
  ;* have only a few visual attributes that may be changed.
  (ivars)
  (inherited children)
  (inheritable children)
  (private)
  (protected)
  (public))

;; Tk toplevel doesn't actually support -height and -width as frames do,
;; but we can probably hack that in at some point

;; deriving toplevel from frame since it supports several methods that
;; frames don't (the 'wm' and 'winfo' functionality)

;; wishlist:
;wm geometry
;wm iconposition

; need to read more to get clue about difference between
; wm geometry toplevel ...
; and set-width! toplevel ...  set-height! toplevel ...
; (obviously the screen placement, but perhaps also difference
;  between requested and actual size?)

;; To prevent passive applications from being collected, we need to hang
;; onto all toplevels with non-weak pointers.   See the toplevels list
;; we hang on to in the code for <toplevel>.  As long as the code for
;; toplevel class is alive, this list should be alive.

(swl:api-class (<toplevel>) (<container-widget> #f 'toplevel)
  ;% \swlapi{class}{<toplevel>}{(create <toplevel>)}
  ;* \ret{instance}
  ;;
  ;* A toplevel is like a frame except that it exists as a free-standing window
  ;* on the X display.  A toplevel can display a title and may or may not
  ;* allow the user to resize it.  A toplevel can be iconified or withdrawn
  ;* from the screen entirely.  When iconified a toplevel may display
  ;* a different string.
  ;* The actual appearance of the toplevel will be determined in part by
  ;* the window manager used.
  ;; grab fallback-queue as ivar so we don't migrate when we do the
  ;; insert-widget stuff that makes us collectable when hidden
  (ivars (kinfolk '()) (hitlist '())
         (fbq (swl:fallback-queue)) (menu #f)
         (destroy-request-handler (lambda (x) #t)))
  (inherited parent handle children)
  (inheritable parent handle children)
  (private
    [visible? () (string=? "1" (swl:tcl-eval 'winfo 'ismapped self))]
    [for-kin (f)
     (let ([kin kinfolk])    ; avoid cycles when windows are mutual kin
       (set! kinfolk '())
       (for-each (lambda (x) (unless (eq? x #!bwp) (f x))) kin)
       (set! kinfolk kin))])
  (protected)
  (public
    [init ignore-args
     ;% \ret{unspecified}
     (send-base self init)
     (swl:tcl-eval 'bindtags handle '(all |SWL|))
     (swl:tcl-eval 'wm 'protocol handle '|WM_DELETE_WINDOW|
       (swl:make-destroy-notify handle))
     ;; sticky by default since visible by default
     (swl:insert-sticky-widget handle self fbq)
     (swl:widget-guardian self)]
    [set-title! (val)
     ;* \ret{unspecified}
     ;* Sets the text displayed by the toplevel when it is not iconified
     ;* to the string given by \var{val}.
     (swl:safety-check
       (unless (string? val) (assertion-violationf 'set-title! "bad value ~s" val)))
     (swl:tcl-eval 'wm 'title handle val)
     (void)]
    [get-title ()
     ;* \ret{see below}
     ;* Returns the string displayed by the toplevel when it is not iconified.
     (swl:tcl-eval 'wm 'title handle)]
    [set-destroy-request-handler! (proc)
     ;* \ret{unspecified}
     ;* Installs a new destroy request handler for this toplevel window.
     ;* When a destroy request is received,
     ;* \var{proc} is invoked and passed this toplevel as an argument.
     ;* If \var{proc} returns \scheme{#f}, then the destroy request is refused.
     ;* Otherwise the toplevel is destroyed.
     (unless (procedure? proc)
       (assertion-violationf 'set-destroy-request-handler! "~s is not a procedure" proc))
     (set! destroy-request-handler proc)]
    [get-destroy-request-handler ()
     ;* \ret{procedure}
     ;* Returns the destroy request handler for this toplevel window.
     destroy-request-handler]
    [set-icon-title! (val)
     ;* \ret{unspecified}
     ;* Sets the text displayed by the toplevel when it is iconified
     ;* to the string given by \var{val}.
     (swl:safety-check
       (unless (string? val) (assertion-violationf 'set-icon-title! "bad value ~s" val)))
     (swl:tcl-eval 'wm 'iconname handle val)
     (void)]
    [get-icon-title ()
     ;* \ret{string}
     ;* Returns the string displayed by the toplevel when it is iconified.
     (swl:tcl-eval 'wm 'iconname handle)]
    [set-resizable! (width height)
     ;* \ret{unspecified}
     ;* \var{width} and \var{height} are boolean values that determine
     ;* whether the toplevel can be resized interactively along those
     ;* dimensions.  \scheme{set-min-size!} and
     ;* \scheme{set-max-size!} have no effect on dimensions for which
     ;* resizing has been prohibited.\label{set-resizable!}
     (swl:safety-check
       (unless (and (boolean? width) (boolean? height))
         (assertion-violationf 'set-resizable! "~s and ~s must be boolean" width height)))
     (swl:tcl-eval 'wm 'resizable handle width height)
     (void)]
    [get-resizable ()
     ;* \ret{see below}
     ;* returns multiple values corresponding to the arguments to \scheme{set-resizable!}
     ; it dices, it slices, it's implemented 27 ways...
     (let ([s (swl:tcl-eval 'wm 'resizable handle)])
       (if (not (fx= 3 (string-length s)))
           (let ([p (open-input-string s)])
             (let* ([w (read p)] [h (read p)])
               (values (fxpositive? w) (fxpositive? h))))
           (values
             (char=? (string-ref s 0) #\1)
             (char=? (string-ref s 2) #\1))))]
    [set-min-size! (width height)
     ;* \ret{unspecified}
     ;* Sets the minimum size for this toplevel to \var{width} and \var{height}.
     ;* See also \ref{set-resizable!}.
     (swl:safety-check
       (unless (and (fixnum? width) (fx>= width 0)
                    (fixnum? height) (fx>= height 0))
         (assertion-violationf 'set-min-size! "invalid argument(s) ~s ~s" width height)))
     (swl:tcl-eval 'wm 'minsize handle width height)
     (void)]
    [get-min-size ()
     ;* \ret{see below}
     ;* returns multiple values corresponding to the arguments to \scheme{set-min-size!}
     (apply values (swl:tcl->scheme (swl:tcl-eval 'wm 'minsize handle)))]
    [set-max-size! (width height)
     ;* \ret{unspecified}
     ;* Sets the maximum size for this toplevel to \var{width} and \var{height}.
     ;* See also \ref{set-resizable!}.
     (swl:safety-check
       (unless (and (fixnum? width) (fx>= width 0)
                    (fixnum? height) (fx>= height 0))
         (assertion-violationf 'set-max-size! "invalid argument(s) ~s ~s" width height)))
     (swl:tcl-eval 'wm 'maxsize handle width height)
     (void)]
    [get-max-size ()
     ;* \ret{see below}
     ;* returns multiple values corresponding to the arguments to \scheme{set-max-size!}
     (apply values (swl:tcl->scheme (swl:tcl-eval 'wm 'maxsize handle)))]
    [set-aspect-ratio-bounds! (min max)
     ;* \ret{unspecified}
     ;* Constrains the aspect ratio for this toplevel to lie between
     ;* \var{min} and \var{max} which are both ratios of width to height.
     ;* If \var{min} and \var{max} are both \scheme{#f}, then any existing contraint
     ;* on the aspect ratios is cancelled.
     (swl:safety-check
       (unless (and (number? min) (number? max))
         (assertion-violationf 'set-aspect-ratio-bounds! "invalid argument(s) ~s ~s" min max)))
     (swl:tcl-eval 'wm 'aspect handle (numerator min) (denominator min)
       (numerator max) (denominator max))
     (void)]
    [get-aspect-ratio-bounds ()
     ;* \ret{see below}
     ;* Returns the contraints on the aspect ratio for this toplevel as
     ;* minumum and maximum ratios of width to height.  The values returned
     ;* are \scheme{#f} if no constraints are in effect.
     (let ((s (swl:tcl-eval 'wm 'aspect handle)))
       (if (string=? s "")
           (values #f #f)
           (apply (lambda (minn mind maxn maxd)
                    (values (/ minn mind) (/ maxn maxd)))
                  (swl:tcl->scheme s))))]
    [set-geometry! (geom)
     ;* \ret{unspecified}
     ;* Sets the geometry for this toplevel to the width, height,
     ;* and/or screen position specified by the string \var{geom}
     ;* which has the general form \scheme{"\var{width}x\var{height}+\var{x}+\var{y}"}.
     ;* Either \var{width} and \var{height} or \var{x} and \var{y} may be omitted.
     ;* A \scheme{+} before \var{x} (\var{y}) designates \var{x} (\var{y}) as a pixel
     ;* offset from the left (top) of the screen to the left (top) edge
     ;* of the window.
     ;* If \var{x} (\var{y}) is preceded by \scheme{-} instead of \scheme{+}, it indicates
     ;* a pixel offset from the right (bottom) of the screen to the right 
     ;* (bottom) edge of the window.
     ;* If \var{geom} is \scheme{#f} instead of a string, the geometry of the window
     ;* reverts to the size requested internally by the widgets it contains.
     (swl:safety-check
       (unless (or (string? geom) (eq? geom #f))
         (assertion-violationf 'set-geometry! "invalid geometry specification ~s" geom)))
     (swl:tcl-eval 'wm 'geometry handle (or geom '()))
     (void)]
    [get-geometry ()
     ;* \ret{string}
     ;* Returns a string describing the current geometry of the toplevel.
     (swl:tcl-eval 'wm 'geometry handle)]
;;
;; set-grid seems less useful than the -setgrid option on text and listbox
;; widgets (maybe useful when we roll our own widgets, but even then we're
;; going to want support from the geometry manager instead of having to do
;; this on the basis of <toplevel>).
;;
;;     [set-grid! (base-width base-height width-inc height-inc)
;;      ;* This constrains the geometry of the toplevel to integral
;;      ;* multiples of \var{width-inc} and \var{height-inc}
;;      (swl:safety-check
;;        (unless (and (fixnum? base-width) (fixnum? base-height)
;;                     (fixnum? width-inc) (fixnum? height-inc)
;;                     (fx>= base-width 0) (fx>= base-height 0)
;;                     (fx>= width-inc 0) (fx>= height-inc 0))
;;          (assertion-violationf 'set-grid! "invalid argument(s) ~s ~s ~s ~s"
;;            base-width base-height width-inc height-inc)))
;;      (swl:tcl-eval 'wm 'grid handle base-width base-height width-inc height-inc)
;;      (void)]
;;      [get-grid ()
;;       ;* Returns multiple values corresponding to the arguments to \scheme{set-grid!}.
;;       (let ((s (swl:tcl-eval 'wm 'grid handle)))
;;         (if (string=? s "")
;;             (values width height 1 1)
;;             (apply values (swl:tcl->scheme s))))]
;;    [set-grid! (cancel)
;;     ;* this seems stupid, why not just (set-grid! toplevel width height 1 1)
;;     ;* not sure why Tk provides this... (maybe some window managers broken?)
;;     (if cancel
;;         (assertion-violationf 'set-grid! "incorrect number of arguments")
;;         (swl:tcl-eval 'wm 'grid handle '()))]
;;
;;     [set-focus-model! (what)
;;      ;* Sets the focus model for this toplevel window.  Legal values are
;;      ;* \scheme{active}, indicating that the window will claim the input focus
;;      ;* even when the focus is in another application, and \scheme{passive}
;;      ;* indicating that the application will never claim focus for itself
;;      ;* but will accept focus from the window manager.  The default is a
;;      ;* passive focus model.
;;      (swl:safety-check
;;        (unless (memq what '(active passive))
;;          (assertion-violationf 'set-focus-model! "~s must be active or passive" what)))
;;      (swl:tcl-eval 'wm 'focusmodel handle what)
;;      (void)]
;;     [get-focus-model ()
;;      ;* Returns the focus model for this toplevel window.
;;      (string->symbol (swl:tcl-eval 'wm 'focusmodel handle))]
    [set-transient! (master)
     ;* \ret{unspecified}
     ;* If \var{master} is another \scheme{<toplevel>}, then this toplevel becomes
     ;* a transient serving \var{master}.  Window managers often display
     ;* transients with less decoration and manage them specially relative
     ;* to the specified master.  Setting \var{master} to \scheme{#f} cancels the
     ;* transient status of the window.
     (swl:safety-check
       (unless (or (eq? master #f)
                   (and (isa-toplevel? master) (not (eq? master self))))
         (assertion-violationf 'set-transient! "~s must be #f or another <toplevel>" master)))
     (swl:tcl-eval 'wm 'transient handle (or master '()))
     (when (visible?)
       (swl:tcl-eval 'wm 'withdraw handle)
       (swl:tcl-eval 'wm 'deiconify handle))]
    [get-transient ()
     ;* \ret{see below}
     ;* Returns the \scheme{<toplevel>} for which this \scheme{<toplevel>} is serving
     ;* as a transient, or \scheme{#f} if none.
     (let ([s (swl:tcl-eval 'wm 'transient handle)])
       (and (not (string=? s ""))
            (swl:lookup-widget (string->symbol s))))]
    [set-override-redirect! (bool)
     ;* \ret{unspecified}
     ;* If set to true, this toplevel will be ignored by the window manager
     ;* (ie. no decorative border will be drawn, etc.).  Changes in this
     ;* setting may not take effect until with toplevel is hidden and shown.
     (swl:tcl-eval 'wm 'overrideredirect handle (and bool #t))
     (void)]
    [get-override-redirect ()
     ;* \ret{boolean}
     ;* Indicates whether this toplevel window is to be ignored by the
     ;* window manager (ie. displayed without decorative border, etc.).
     (string=? (swl:tcl-eval 'wm 'overrideredirect handle) "1")]
    [show ()
     ;* \ret{unspecified}
     ;* Makes the toplevel visible in its usual size
     ;* (deiconifies the window if necessary).
     (unless (visible?)
       (for-kin (lambda (x) (send x show)))
       (swl:tcl-eval 'wm 'deiconify self)
       (swl:insert-sticky-widget handle self fbq)  ;; make un-collectable
       (void))]
    [hide ()
     ;* \ret{unspecified}
     ;* Withdraws the window from the display so that it is completely hidden.
     ;* If unreachable, a toplevel may be collected while hidden.
     ;; maybe all widgets should be collectable, for now I'll skip it.
     (when (visible?)
       (for-kin (lambda (x) (send x hide)))
       (swl:tcl-eval 'wm 'withdraw handle)
       (swl:insert-widget handle self fbq)  ;; make collectable
       (void))]
    [iconify ()
     ;* \ret{unspecified}
     ;* Removes the window from the display and shows it instead
     ;* as an icon.  The \scheme{show} method can be used to reverse
     ;* the process.
     ;* Note that \scheme{iconify} differs from \scheme{hide} in that the
     ;* latter removes all traces of the window from the screen
     ;* while the former leaves behind an icon.
     (swl:tcl-eval 'wm 'iconify handle)
     (void)]
    [set-parent! (val)
     ;* \ret{unspecified}
     ;* It is an error to attempt to set the parent of a toplevel
     ;* since a toplevel has no parent.
     (assertion-violationf 'set-parent! "cannot set parent for <toplevel>")]
    [get-parent ()
     ;* \ret{see below}
     ;* This method returns \scheme{#f}
     ;* since a toplevel has no parent.
     #f]
    [destroy ()
     ;* \ret{unspecified}
     ;* Destroys this toplevel window if \scheme{destroy-request-handler}
     ;* returns a non-false value.
     (when (destroy-request-handler self)
       ; the collector would get around to doing all this destroying
       ; for us, but some of these widgets will want to be notified NOW
       ; (eg. for swl:help-menu and swl:application-menu code)
       (for-kin (lambda (x) (send x destroy)))
       (for-each (lambda (thunk) (thunk)) hitlist)
       (when menu (send menu destroy))
       (send-base self destroy))]
    [set-menu! (m)
     ;* \ret{unspecified}
     ;* Sets the menu for this toplevel window.
     (unless (isa-menu? m)
       (assertion-violationf 'set-menu! "expected an instance of <menu>: ~s" m))
     (send m set-parent! self)
     (set-tk-option '-menu m (lambda (x) x) 'set-menu!)
     (set! menu m)]
    [get-menu ()
     ;* \ret{see below}
     ;* Returns the menu instance associated with this toplevel,
     ;* or \scheme{#f} if none.
     menu]
    [add-follower (widget)
     ;* \ret{unspecified}
     ;* Adds the specified \scheme{widget} to the list of widgets following
     ;* this widget.  When the \scheme{show}, \scheme{hide}, or \scheme{destroy}
     ;* message is sent to this instance, the message is also forwarded to each of
     ;* the followers.
     (unless (memq widget kinfolk)
       (let ([pr (weak-cons widget kinfolk)])
         (send widget on-destroy (lambda () (set-car! pr #!bwp)))
         (set! kinfolk pr)))]
    [remove-follower (widget)
     ;* \ret{unspecified}
     ;* Removes the specified \scheme{widget} from the list of follower widgets.
     ;; Using list mutation so that the follower can #f itself in its destroy method.
     (remq! widget kinfolk)]
    [on-destroy (thunk)
     ;* \ret{unspecified}
     ;* Adds \scheme{thunk} to a list of procedures to be invoked when
     ;* the widget is actually destroyed.  In particular, these procedures
     ;* are not invoked if the destroy-request-handler returns \scheme{#f}.
     ;; maybe we could replace the use of on-destroy with destroy-request-handler
     (set! hitlist (cons thunk hitlist))]
    [set-prefs-tag! (tag)
     ;* \ret{unspecified}
     ;* Load preferences identified by the given tag.
     ;* This simply enables us to use the \scheme{(prefs-tag: ...)} syntax
     ;* with the \scheme{create} macro.  This must be set before we set the
     ;* \scheme{menu:} option.
     (send-base self load-prefs tag)]
))

;; COMMENT FOR THE show method of <balloon>
     ;; x y width height define dimensions of a rectangular region that
     ;; the balloon help must avoid overlapping
     ;;
     ;; What a nuisance.  set-geometry! doesn't seem to work reliably
     ;; with fvwm:  for example, if you show the balloon help, then move
     ;; the master window and re-show the balloon help, the help window
     ;; first appears in it's previous location (even though we set-geometry!
     ;; right off the bat).  Subsequently posting the help causes it to
     ;; appear in the right place.
     ;;       fixed it by using foreign-procedure Tk_MoveWindow

(define-swl-class (<balloon>) (<toplevel>)
  (ivars (save #f) (shadow #f) (label #f) (pad (+ 4 (cm->pixels .25))))
  (inherited)
  (inheritable)
  (private)
  (protected
    [fit (pos min max)
     (and (< min max)
          (if (<= min pos) (if (< pos max) pos max) min))])
  (public
    [init ignore-args
     (send-base self init)
     (set-width! self 0)
     (set-height! self 0)
     (send self set-override-redirect! #t)
     (send self rename-this-primitive-map #f)
;    (set! shadow
;      (create <toplevel>
;        with (background-color: 'lemonchiffon3)
;             (override-redirect: #t) (width: 0) (height: 0)))
;    (send shadow rename-this-primitive-map #f)
     (set! label (create <label> self
             with (background-color: 'lemonchiffon1)
                  (border-width: 3)))]
    [mouse-leave (x y mods)
     ;% \ret{unspecified}
     (send self hide)]
    [hide ()
     ;% \ret{unspecified}
     (send label rename-this-primitive-map #f)
     (send self rename-this-primitive-map #f)
;    (send shadow rename-this-primitive-map #f)
    ]
    [show (content x y width height)
     ;* \ret{unspecified}
     ;* Makes the baloon visible at the specified coordinates, width,
     ;* and height.
     (set-title! label content)
     (let ((w (send label get-desired-width))
           (h (send label get-desired-height))
           (wmax (swl:screen-width))
           (hmax (swl:screen-height)))
       (call-with-values
         (lambda ()
           (let ([newy (or (fit y 0 (- y h pad))
                           (fit y (+ y height pad) (- hmax h pad)))])
             (if newy
                 (values (fit x 0 (- wmax w pad)) newy)
                 (let ([x (or (fit x 0 (- x w pad))
                              (fit x (+ x width pad) (- wmax w pad)))])
                   (if x (values x (fit y 0 (- hmax h pad))) (values #f #f))))))
         (lambda (x y)
           (when (and x y)
;            (send shadow raise)
             (send self raise)
             (send label set-window-size! w h)
             (send self set-window-size! w h)
;            (send shadow set-window-size! w h)
             (send self set-window-pos! x y)
             (send self rename-this-primitive-map #t)
;            (send shadow set-window-pos! (+ x 4) (+ y 4))
;            (send shadow rename-this-primitive-map #t)
             (send label rename-this-primitive-map #t)
             (swl:sync-display)))))]))

(define isa-toplevel? (lambda (x) (isa? x <toplevel>)))
