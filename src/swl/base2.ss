;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; for development only   --   eventually we may support some kind of event
;;    filter mechanism that could be used to do this...
;  (define events #f)
;  (define-syntax swl:announce-event
;    (lambda (x)
;      (syntax-case x ()
;        [(_ kind . body)
;         (syntax
;           (when (or (eq? events 'all)
;                     (and (pair? events) (memq 'kind events)))
;             . body))])))

(eval-when (compile eval)
(define-syntax swl:announce-event (lambda (x) (syntax (void))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; table and guardian for widgets
;;
;;   handle is symbol or number

(define swl:widget-guardian (make-guardian))

(define swl:crh-error-count 0) ;;; keep an eye on this

(swl:api-procedure define swl:sync-display
  ;* \formdef{swl:sync-display}{procedure}{(swl:sync-display)}
  ;* \ret{unspecified}
  ;* Because display updates are buffered by Tcl/Tk, the application
  ;* state can be out of sync with the display.
  ;* While this is normally invisible,
  ;* \scheme{swl:sync-display} can be used to flush pending display
  ;* updates to the screen in cases where it would otherwise be noticed.
  ;* It is often necessary to flushing display before getting the width or
  ;* height of a widget that has just been created, because Tk
  ;* defers geometry calculations until rendering the widget.
  (lambda ()
    (swl:tcl-eval 'update 'idletasks)
    (void)))

(define swl:install-collect-request-handler
  ;;
  ;; Note: this is a Chez Scheme interrupt handler, which may be invoked from
  ;; from any thread. Temporarily set some sane parameter values so we're
  ;; not at the mercy of the interrupted thread's parameters on an error.
  ;;
  (lambda () 
    (define base-collect-request-handler (collect-request-handler))
    (collect-request-handler
     (lambda ()
       (base-collect-request-handler) ;;; do the collection before doing any more allocation
       (let ((k (call/cc (lambda (k) k))))
         (with-exception-handler
           (lambda (c)
             (set! swl:crh-error-count (1+ swl:crh-error-count))
             (parameterize ([console-output-port (swl:bug-port)])
               (#%raise c)))
           (lambda ()
             (parameterize
               ([reset-handler (lambda () (k k))] ;;; don't throw out of crh
                [thread-group-servers ;;; become a server
                 (cons (thread-self) (thread-group-servers))]
                )
               (let loop ((x (swl:widget-guardian)) (n 0))
                 (when x
                   (send x destroy)
                   (loop (swl:widget-guardian) (fx+ n 1))))))))))))

(define swl:insert-widget
  (lambda (handle instance fallback-q)
    (#%$set-top-level-value! handle (weak-cons instance fallback-q))))

(define swl:insert-sticky-widget
  (lambda (handle instance fallback-q)
    (#%$set-top-level-value! handle (cons instance fallback-q))))

(define swl:lookup-widget
  (lambda (handle)
    (let ((x (#%$top-level-value handle)))
      (and (pair? x) (car x)))))

(define swl:lookup-fallback-queue
  (lambda (handle)
    (let ((x (#%$top-level-value handle)))
      (and (pair? x) (cdr x)))))

(define swl:delete-widget
  (lambda (handle)
    (#%$set-top-level-value! handle #f)))

(swl:api-procedure define swl:screen-width
  ;* \formdef{swl:screen-width}{procedure}{(swl:screen-width)}
  ;* \ret{width of the screen, in pixels}
  ;*
  ;* This procedure returns width of the screen in pixels.
  (lambda ()
    (string->number (swl:tcl-eval 'winfo 'screenwidth #\.))))

(swl:api-procedure define swl:screen-height
  ;* \formdef{swl:screen-height}{procedure}{(swl:screen-height)}
  ;* \ret{height of the screen, in pixels}
  ;*
  ;* This procedure returns height of the screen in pixels.
  (lambda ()
    (string->number (swl:tcl-eval 'winfo 'screenheight #\.))))

;; Not sure I want him to be API procedure for the same reasons
;; that xy->widget seems problematic
(define swl:current-focus
  ;* Returns the widget that currently has focus, or #f if none.
  (lambda ()
    (let ([s (swl:tcl-eval 'focus)])
      (and (not (string=? s ""))
           (swl:lookup-widget (string->symbol s))))))

;; not sure we want to advertise this as an API procedure.
;; seems like a real abstraction breaker.
(define xy->widget
  ;* This procedure returns the widget that covers the given
  ;* root-window x- and y-coordinates, or \scheme{#f} if none.
  (lambda (x y)
    (let ([s (swl:tcl-eval 'winfo 'containing x y)])
      (and (not (string=? s ""))
           (swl:lookup-widget (string->symbol s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tk-Widget
;;

;; pass in #f if you're a toplevel
(define swl:new-handle
  (let ((op (open-output-string)) (num -1))
    (lambda (parent)
      (string->symbol
        (thread-critical-section
          (set! num (+ num 1))
          (when parent (send parent scheme->tcl op))
          (display #\. op)
          (unless parent (display #\w op))
          (display num op)
          (get-output-string op))))))

(define-swl-class (<tk-prewidget> handle) (<tk-object>)
  ;* This is the abstract class from which widgets and non-widget
  ;* Tk entities (images, menus, markups) are derived.
  (ivars (handle handle))
  (inherited)
  (inheritable handle)
  (private)
  (protected)
  (public
    [scheme->tcl (op)
     (display handle op)]
    [init ignore-args
     ;* \ret{unspecified}
     ;* To prevent widgets from being prematurely garbage-collected,
     ;* this method must be called when a widget is first created.
     ;* The \scheme{create} macro does this automatically.
     ;* Classes derived from \scheme{<tk-widget>} can redefine \scheme{init}
     ;* to add additional initialization code, but the subclass's
     ;* \scheme{init} method should first call the base \scheme{init} method.
     (swl:insert-widget handle self (swl:fallback-queue))
     (swl:widget-guardian self)]
    [destroy ()
     (swl:delete-widget handle)
     (send-base self destroy)]))

(define-swl-class (<tk-prewidget2> handle) (<tk-prewidget> handle)
  ;* abstract class from which <canvas-item> and <tk-prewidget3>
  ;* are derived.
  (ivars)
  (inherited handle)
  (inheritable handle)
  (private)
  (protected)
  (public
    [configure (width height)
     ;* \ret{unspecified}
     ;* When resized, a widget is notified of its width and height 
     ;* by this method.
(swl:announce-event configure
(print self)
(printf " got configure ~a x ~a message~n" width height)
)
     (void)]
    [mouse-enter (x y modifiers)
     ;* \ret{unspecified}
     ;* When the mouse enters a widget, this method is invoked with the
     ;* x- and y-coordinates within the widget and the set of modifiers
     ;* describing the state of the mouse buttons and keys such as
     ;* \mytt{shift} and \scheme{control}.  For more details see the description
     ;* of \hpageref{event-case}{\scheme{event-case}}.
(swl:announce-event mouse-enter
(print self)
(printf " got mouse-enter at (~s, ~s) ~s~n" x y modifiers)
)
      (let ((help-text (prop-ref 'balloon-help-text)))
        (when help-text
          (swl:show-balloon-help
            help-text
            (get-root-x self)
            (get-root-y self)
            (get-width self)
            (get-height self))))
     (void)]
    [mouse-leave (x y modifiers)
     ;* \ret{unspecified}
     ;* This method is invoked with the x- and y-coordinates and the set of
     ;* modifiers describing the state of the mouse-buttons and modifier
     ;* keys when the mouse left the widget.
(swl:announce-event mouse-leave
(print self)
(printf " got mouse-leave at (~s, ~s) ~s~n" x y modifiers)
)
     (swl:hide-balloon-help)
     (void)]
    [key-press (key modifiers)
     ;* \ret{unspecified}
     ;* When a key is pressed, this method notifies the widget that has
     ;* input focus with a description of the key and any modifiers in
     ;* effect at the time.  \scheme{key} is either a Scheme character or
     ;* symbol.  \scheme{modifiers} describes the states of mouse-buttons
     ;* and special keys when the event happened.  For more details see
     ;* the description
     ;* of \hpageref{event-case}{\scheme{event-case}}.
(swl:announce-event key-press
(print self)
(printf " got key-press ~s ~s~n" key modifiers)
)
     (void)]
    [key-release (key modifiers)
     ;* \ret{unspecified}
     ;* This method notifies a widget that the given \mytt{key} has been
     ;* released with \mytt{modifiers} in effect.  (See \scheme{key-press})
(swl:announce-event key-release
(print self)
(printf " got key-release ~s ~s~n" key modifiers)
)
     (void)]
    [mouse-press (x y modifiers)
     ;* \ret{unspecified}
     ;* When a mouse button is pressed within a widget,
     ;* this method is invoked with the x- and y-coordinates
     ;* and the state of the modifier keys and mouse-buttons.
     ;* (See \scheme{event-case})
(swl:announce-event mouse-press
(print self)
(printf " got mouse-press at (~s, ~s) ~s~n" x y modifiers)
)
     (void)]
    [mouse-release (x y modifiers)
     ;* \ret{unspecified}
     ;* When a mouse button is released in a widget,
     ;* this method is invoked with the x- and y-coordinates
     ;* and the state of the modifier keys and mouse-buttons.
     ;* (See \scheme{event-case})
(swl:announce-event mouse-release
(print self)
(printf " got mouse-release at (~s, ~s) ~s~n" x y modifiers)
)
     (void)]
    [mouse-motion (x y modifiers)
     ;* \ret{unspecified}
     ;* This method is invoked with the x- and y-coordinates
     ;* of the mouse and the state of the modifiers whenever
     ;* the mouse is moved within a widget.
(swl:announce-event mouse-motion
(print self)
(printf " got mouse-motion at (~s, ~s) ~s~n" x y modifiers)
)
     (void)]
    [raise ()
     ;* \ret{unspecified}
     ;* Raises a widget to the top of the Z-order so that it obscures
     ;* any widgets it overlaps.
     (swl:tcl-eval 'raise handle)
     (void)]
    [lower ()
     ;* \ret{unspecified}
     ;* Lowers a widget to the bottom of the Z-order so that any
     ;* widgets overlapping it now obscure it.
     (swl:tcl-eval 'lower handle)
     (void)]
    [set-balloon-help! (txt)
     ;* \ret{unspecified}
     (prop-set! 'balloon-help-text txt)]
    [get-balloon-help ()
     ;% \ret{see below}
;returns #f if none
     (prop-ref 'balloon-help-text)]
    [set-balloon-help-delay! (ms)
     ;% \ret{unspecified}
;skip for now
    (void)]
    [get-balloon-help-delay ()
     ;% \ret{see below}
;skip for now
    (void)]))

(define-swl-class (<tk-prewidget3> handle) (<tk-prewidget2> handle)
  ;* abstract class from which <menu> and <tk-widget> are derived
  ;* (perhaps canvas-item fits in here too)
  (ivars)
  (inherited handle)
  (inheritable handle)
  (private)
  (protected
    [set-tk-option (tclname value err-check name)
     (swl:safety-check
       (unless (err-check value) (assertion-violationf name "bad value ~s" value)))
     (swl:tcl-eval handle 'config tclname value)
     (#3%void)]
    [get-tk-option (tclname conversion name)
     (conversion (swl:tcl-eval handle 'cget tclname))])
  (public
    [get-root-x ()
     ;* \ret{integer}
     ;* Returns the x-coordinate of the widget relative to the root window.
     (string->number (swl:tcl-eval 'winfo 'rootx handle))]
    [get-root-y ()
     ;* \ret{integer}
     ;* Returns the y-coordinate of the widget relative to the root window.
     (string->number (swl:tcl-eval 'winfo 'rooty handle))]
    [get-width ()
     ;* \ret{integer}
     ;* Returns the current width, in pixels, of the widget.
     (string->number (swl:tcl-eval 'winfo 'width handle))]
    [get-height ()
     ;* \ret{integer}
     ;* Returns the current height, in pixels, of the widget.
     (string->number (swl:tcl-eval 'winfo 'height handle))]
    [get-desired-width ()
     ;* \ret{integer}
     ;* Returns the width, in pixels, that the widget requests from its
     ;* window manager.
     (string->number (swl:tcl-eval 'winfo 'reqwidth handle))]
    [get-desired-height ()
     ;* \ret{integer}
     ;* Returns the height, in pixels, that the widget requests from its
     ;* window manager.
     (string->number (swl:tcl-eval 'winfo 'reqheight handle))]
    [set-focus ()
     ;* \ret{unspecified}
     ;* Gives this widget keyboard focus:  all
     ;* key-press / key-release events are directed to this widget
     ;* until the focus is set to some other widget via the keyboard
     ;* traversal mechanism or via the \scheme{set-focus} method.
     (swl:tcl-eval 'focus handle)
     (void)]
    [visible? ()
     ;* \ret{boolean}
     ;* Returns \scheme{#t} if the widget is visible, \scheme{#f} otherwise.
     (tk->boolean (swl:tcl-eval 'winfo 'ismapped handle))]
    [set-background-color! (val)
     ;* \ret{unspecified}
     ;* Sets the default background color for the instance to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-background val swl:color? 'set-background-color!)]
    [get-background-color ()
     ;* \ret{see below}
     ;* Returns the default background color for the instance.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-background tk->color 'get-background-color)]
    [set-border-width! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the border drawn around the instance.
     (set-tk-option '-borderwidth val swl:distance-unit? 'set-border-width!)]
    [get-border-width ()
     ;* \ret{see below}
     ;* Returns the width in pixels of the border drawn around the instance.
     (get-tk-option '-borderwidth string->number 'get-border-width)]
    [set-mouse-cursor! (val)
     ;* \ret{unspecified}
     ;* Sets the mouse cursor to be displayed when the mouse pointer is in the
     ;* widget.  \scheme{val} is either a symbol naming a standard X cursor in
     ;* \mytt{cursorfont.h} or a list of two or three items where the first is
     ;* a symbol as before and the other items are colors specifying the
     ;* foreground and (if present) background color for the cursor.
     (swl:safety-check
       (unless (mouse-cursor? val)
         (assertion-violationf 'set-mouse-cursor! "invalid cursor ~s" val)))
     (set-tk-option '-cursor
       (if (eq? val 'default) '() val)
       (lambda (x) #t)
       'set-mouse-cursor!)]
    [get-mouse-cursor ()
     ;* \ret{see below}
     ;* Returns the mouse cursor displayed when the mouse pointer is in the
     ;* widget.  The return value is in one of the forms accepted by
     ;* \scheme{set-mouse-cursor!}.
     (get-tk-option '-cursor tk->mouse-cursor 'get-mouse-cursor)]
    [set-relief! (val)
     ;* \ret{unspecified}
     ;* Determines how the border of the widget is drawn.
     ;* Legal values are \scheme{flat}, \scheme{sunken}, \scheme{raised},
     ;* \scheme{ridge}, \scheme{groove}.  See also \scheme{set-border-width!}.
     ;* If the widget border width is set to zero, setting relief
     ;* will have no visible effect.
     (set-tk-option '-relief val swl:relief? 'set-relief!)]
    [get-relief ()
     ;* \ret{see below}
     ;* Returns a symbol describing how the border of the widget is drawn.
     (get-tk-option '-relief string->symbol 'get-relief)]
    [set-accept-focus! (val)
     ;* \ret{unspecified}
     ;* The boolean \var{val} determines wether or not this widget
     ;* accepts focus when offered by the keyboard traversal mechanism.
     ;* Keyboard traversal allows the user to move input focus between
     ;* neighboring widgets by pressing \mytt{Tab} and \mytt{Shift-Tab}.
     (set-tk-option '-takefocus val boolean? 'set-accept-focus!)]
    [get-accept-focus ()
     ;* \ret{boolean}
     ;* Returns a boolean indicating whether or not this widget accepts
     ;* focus when offered by the keyboard traversal mechanism.
     ;* See the description of \scheme{set-accept-focus!}.
     (get-tk-option '-takefocus tk->boolean 'get-accept-focus)]
    [get-application-context ()
     ;* \ret{see below}
     ;* Returns the application context to which event messages
     ;* for this widget are dispatched.  The result is currently
     ;* a fallback queue created by \scheme{swl:make-fallback-queue},
     ;* and can be passed to \scheme{swl:application-modal}.
     (swl:lookup-fallback-queue handle)]
;; [set-window-size! (w h)
;;      ;* \ret{unspecified}
;;  (unless (and (fixnum? w) (fixnum? h) (fx>= w 0) (fx>= h 0))
;;    (assertion-violationf 'set-window-size! "invalid argument(s) ~s ~s" w h))
;;  ;; eventually cache the handle locally
;;  (swl:resize-window (swl:name-to-window (symbol->string handle)) w h)]
;; [set-window-pos! (x y)
;;      ;* \ret{unspecified}
;; ;; have to replace window managers if we want to make this useful.
;;  (unless (and (fixnum? x) (fixnum? y) (fx>= x 0) (fx>= y 0))
;;    (assertion-violationf 'set-window-pos! "invalid argument(s) ~s ~s" x y))
;;  ;; eventually cache the handle locally
;;  (swl:move-window (swl:name-to-window (symbol->string handle)) x y)]
;; [rename-this-primitive-map (vis)
;;      ;* \ret{see below}
;; (printf "called primitive window-map ~s on ~s~n" vis (class-name self))
;;  (let ([who (swl:name-to-window (symbol->string handle))])
;;    (if vis (swl:map-window who) (swl:unmap-window who)))]
))


;; Notes:
;;   - now tk instance is created at time that the thing is made,
;;     so setter/getters don't have to check to see if it's been created,
;;     only check if it's dead.
;;   - should verify that parent is a legitimate parent object
;;   * would like to avoid calling Tcl in get-height and get-width
;;     we could do this by caching the height/width supplied in the
;;     configure method.  only problem is that some user could
;;     call the configure method without actually changing the Tk widget's
;;     size.  until more of this is in Scheme it's awkward to determine
;;     in configure whether we're being called because we really need to
;;     do something or because something was done.

(define-swl-class (<tk-widget> parent tcl-name)
              (<tk-prewidget3>
                 (and tcl-name      ;; hack to support <menu> and <option-button>
                   (let ([handle (swl:new-handle parent)])
                     (swl:tcl-eval tcl-name handle)
                     handle)))
  ;* This is the abstract class from which all regular widgets
  ;* are derived.
  (ivars (parent parent))
  (inherited handle)
  (inheritable handle parent)
  (private
    [save-state-show-hack (manager options)
     ;; This is a truly meager hack toward the goal of making show a suitable
     ;; antidote to hide regardless of what geometry manager is handling the
     ;; widget.  Of course to do it right we have to solve the order-of-eval
     ;; bugs inherent in the design of Tk's geometry managers.
     ;;
     ;; As a bonus hack, we accumulate the options given for a widget
     ;; (as long as the same geometry manager is used) adding the new options
     ;; to the end of the list so we get a little closer to the semantics of
     ;; multiple calls to pack.  Thank you for shopping hacks-R-us.
     ;;
     ;; Note to Wally:  setting visible to true is clearly broken if you add
     ;; support for "pack forget widget".
     (let ([old-manager (prop-ref 'manager)])
       (if (eq? manager old-manager)
           (prop-set! 'geom-opts (append (prop-ref 'geom-opts) options))
           (begin
             (prop-set! 'geom-opts options)
             (prop-set! 'manager manager))))]
    [error-check-pack-opts (ls)
     ;; Note:  the pack macro guarantees us a list of key val ...
     ;;        where keys are known to be among those recognized here...
     (let loop ((ls ls) (bugs '()))
       (if (null? ls)
           (unless (null? bugs) (assertion-violationf 'pack (apply string-append bugs)))
           (let ((bug
                   (case (car ls)
                     ((-expand)
                      (unless (boolean? (cadr ls))
                         "~n  value for expand: must be boolean"))
                     ((-fill)
                      (unless (memq (cadr ls) '(x y both none))
                        "~n  value for fill: must be x, y, both, or none"))
                     ((-side)
                      (unless (memq (cadr ls) '(left right top bottom))
                        "~n  value for side: must be left, right, top, or bottom"))
                     ((-anchor)
                      (unless (memq (cadr ls) '(n s e w nw ne sw se center))
                        "~n  value for anchor: must center, n, s, e, w, nw, ne, sw, or se"))
                     ;; if macro is used, unreachable...
                     (else (format "~n  unknown option: ~s" (car ls))))))
              (loop (cddr ls)
                    (if (string? bug)
                        (cons bug bugs)
                        bugs)))))]
    [error-check-grid-opts (ls)
     ;; need to steal code from error-check-pack-opts or (heaven forbid)
     ;; abstract it.
     (let loop ((ls ls))
       (unless (null? ls)
         (case (car ls)
           ((-column -row -columnspan -rowspan)
            (unless (fixnum? (cadr ls))
              (assertion-violationf 'grid
                "value for ~s must be integer"
                (let ([s (symbol->string (car ls))])
                  (substring s 1 (string-length s))))))
           ((-sticky) (void))   ;; assume checked by macro somewhere
           (else (assertion-violationf 'grid "unrecognized option ~s" (car ls))))
         (when (null? (cdr ls))
           (assertion-violationf 'grid "missing value for option ~s" (car ls)))
         (loop (cddr ls))))]
    [error-check-place-opts (ls)
     (warningf 'place "error checks not implemented")])
  (protected
    [tk-destroy () (unless (tk-destroyed?) (swl:tcl-eval 'destroy handle))]
    [tk-destroyed? () (string=? "0" (swl:tcl-eval 'winfo 'exists handle))])
  (public
    [init ignore-args
     ;* \ret{unspecified}
     ;* To prevent widgets from being prematurely garbage-collected,
     ;* this method must be called when a widget is first created.
     ;* The \scheme{create} macro does this automatically.
     ;* Classes derived from \scheme{<tk-widget>} can redefine \scheme{init}
     ;* to add additional initialization code, but the subclass's
     ;* \scheme{init} method should first call the base \scheme{init} method.
     (send-base self init)
     (set-accept-focus! self #t) ;; make keyboard traversal possible
     ;; Need the "all" bindtag to get <Alt-Key> binding that posts menus
     ;; for which a letter is underlined.
     (swl:tcl-eval 'bindtags handle '(all |SWL|))
     (when parent (send parent adopt self))]
    [key-press (key mods)
(swl:announce-event key-press
(print self)
(printf " got key-press ~s ~s~n" key mods)
)
     (event-case ((key= key) (modifier= mods))
       (([shift #\tab]) (when parent (focus-prev parent self)))
       (([#\tab]) (when parent (focus-next parent self))))]
    [set-traversal-background-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color for the traversal highlight region when the widget
     ;* does not have input focus.
     ;* \var{val} is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-highlightbackground val swl:color? 'set-traversal-background-color!)]
    [get-traversal-background-color ()
     ;* \ret{see below}
     ;* Returns the color for the traversal highlight region when the widget
     ;* does not have input focus.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-highlightbackground tk->color 'get-traversal-background-color)]
    [set-traversal-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color for the traversal highlight region when the widget
     ;* has input focus.
     ;* \var{val} is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-highlightcolor val swl:color? 'set-traversal-color!)]
    [get-traversal-color ()
     ;* \ret{see below}
     ;* Returns the color for the traversal highlight region when the widget
     ;* has input focus.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-highlightcolor tk->color 'get-traversal-color)]
    [set-traversal-thickness! (val)
     ;* \ret{unspecified}
     ;* Sets the width in pixels of the traversal highlight region.
     (set-tk-option '-highlightthickness val swl:distance-unit? 'set-traversal-thickness!)]
    [get-traversal-thickness ()
     ;* \ret{integer}
     ;* Returns the width in pixels of the traversal highlight region.
     (get-tk-option '-highlightthickness string->number 'get-traversal-thickness)]
    [set-parent! (p)
     ;* \ret{unspecified}
     ;* (temporarily unimplemented, or perhaps never to be implemented)
(assertion-violationf 'implementation "have to do set-parent! still")]
    [get-parent ()
     ;* \ret{see below}
     ;* Returns the parent of the widget.
     parent]
    [show ()
     ;* \ret{unspecified}
     ;* Makes the widget visible in its parent.
     ;* The order in which widgets are shown determines the order
     ;* in which they appear in their parent.
     ;*
     ;* Planned revisions include eliminating this order dependency.
     (let ([manager (prop-ref 'manager)])
       (case manager
         [(grid) (swl:tcl-eval 'grid handle)]
         [(pack place)
          ;; some hacks for now until we get our own geometry manager
          ;; really there's no point to these hacks unless I somehow
          ;; cure the order dependency  (maybe the new grid implementation
          ;; will be worth looking at)
          (let ([geom-opts (prop-ref 'geom-opts)])
            (if geom-opts
                (apply swl:tcl-eval manager handle geom-opts) ;; HACK
                (swl:tcl-eval manager handle)))]
         ;; Tk BUG:  if we make grid the default, we get arithmetic
         ;; overflow given a sequence of show's
         [else (swl:tcl-eval 'pack handle) (prop-set! 'manager 'pack)]))
     (void)]
    [hide ()
     ;* \ret{unspecified}
     ;* Removes the widget from the screen.
     ;* Due to the current order dependency of \scheme{show}
     ;* the widget may not appear at the same place when
     ;* subsequently redisplayed via \scheme{show}.
     ;* This is soon to be changed.
     (let ([manager (prop-ref 'manager)])
       (when manager (swl:tcl-eval manager 'forget handle)))
     (void)]
    [destroy ()
     ;% \ret{unspecified}
     ;% Destroys the widget.
     (when handle
       (critical-section
         (when parent (send parent disown self)) ;; when is for toplevels
         (tk-destroy)
         (send-base self destroy)
         (set! parent #f)
         (set! handle #f)))]
    [grid grid-opts
     ;* \ret{unspecified}
     ;* Handy Tcl/Tk geometry manager.
     (swl:safety-check (error-check-grid-opts grid-opts))
     (apply swl:tcl-eval 'grid handle grid-opts)
     (save-state-show-hack 'grid grid-opts)
     (void)]
    [pack pack-opts
     ;* \ret{unspecified}
     ;* This
     ;* Tcl/Tk geometry manager is handy for specifying complex layouts
     ;* if you already know how
     ;* to use it, but takes a fair bit of space to describe fully.
     ;* The \scheme{pack} macro supports a restricted but useful form of pack,
     ;* and better documentation can be found there.
     ;*
     ;* Currently
     ;* planning to replace this with \scheme{<vbox>} and \scheme{<hbox>}
     ;* frame widgets.
     (swl:safety-check (error-check-pack-opts pack-opts))
     (apply swl:tcl-eval 'pack handle pack-opts)
     (save-state-show-hack 'pack pack-opts)
     (void)]
    [place place-opts
     ;* \ret{unspecified}
     ;* An extremely powerful geometry manager provided by Tk.
     ;* See the \scheme{place} macro for more details.
     (swl:safety-check (error-check-place-opts place-opts))
     (apply swl:tcl-eval 'place handle place-opts)
     (save-state-show-hack 'place place-opts)
     (void)]
;    [handle-selection (selection handler)
; I don't see how we can support this using fallbacks.  Control has to return
; to Scheme before the fallback can be evaluated, and by that time, we've
; returned some value (namely "") to Tcl as the result of s_eval.  Perhaps
; we could arrange for the handler script to evaluate:
;
;  proc handler {offset max} {            ; need to gensym all these var names
;    set waiting 1
;    s_eval <fallback-id> $offset $max
;    while {$waiting} {
;      after 100
;    }
;    return $result
;  }
;
; where the fallback would be something like:
;
;  (lambda (offset max)
;    (let ((result (handler-proc offset max)))
;      (if (not result)
;          (swl:tcl-eval 'error "selection request denied")
;          (begin
;            (sw:tcl-eval 'set 'result result)
;            (sw:tcl-eval 'set 'waiting 0)))))
;
; Even if this worked, it would be too much of a hack, especially since
; we recently implemented true callbacks into Scheme.  For now, we'll
; leave this commented out until all interested parties are willing to
; move to version 5.9f or later of Chez Scheme.
;      
;     ;* This method installs or removes a handler that serves requests
;     ;* for the specified selection when that selection is owned by this
;     ;* widget (see the \scheme{claim-selection} method).
;     ;*
;     ;* If handler is \scheme{#f} remove any existing handler for this widget
;     ;* and the specified selection.  Otherwise, handler must be a procedure
;     ;* of two arguments, offset and count, where offset indicates a
;     ;* starting character position in the selection, and count indicates
;     ;* the maximum number of characters to return.  If the handler returns
;     ;* a string whose length is equal to count, the handler may be invoked
;     ;* again until it returns a string whose length is less than count.
;     ;* If the handler returns \scheme{#f}, the request for the given selection
;     ;* is rejected as if no selection existed and an error is signalled
;     ;* (returning an error to Tcl causes the request to be refused).
;     (unless (or (not handler) (procedure? handler))
;       (assertion-violationf 'swl:handle-selection "~s must be a procedure or #f" handler))
;     (let ([cb
;            (or (prop-ref 'handle-selection-callback)
;                (let ([cb
;                       (swl:procedure->callback
;                         (lambda (offset count)
;                           (let ([result (handler offset count)])
;                             ;; should we check string-length of result
;                             ;; to see that it's <= count?
;                             (or result
;                                 (swl:tcl-eval 'error
;                                   "selection request refused by handler"))))
;                         handle)])
;                  (prop-set! 'handle-selection-callback cb)
;                  cb))])
;       (prop-set! 'handle-selection-notify handler)
;       (swl:tcl-eval 'selection 'handle '-selection
;         (swl:resolve-selection 'handle-selection selection)
;         handle
;         (if (not handler) '() cb)))
;     (void)]
    [claim-selection (selection notify-proc)
     ;* \ret{unspecified}
     ;* Make this widget the owner of the specified selection
     ;* and notify the previous owner of that selection (if any)
     ;* that it has lost ownership.
     ;* When this widget loses the selection, invoke the given
     ;* notify thunk.
     ;* When a widget claims ownership of the primary selection,
     ;* the currently (or subsequently) selected contents of
     ;* the widget are then
     ;* available via \scheme{swl:get-selection}.
     ;* See also \scheme{swl:clear-selection}.
     (unless (procedure? notify-proc)
       (assertion-violationf 'swl:claim-selection "~s must be a procedure" notify-proc))
     (if (eq? notify-proc void)
         (swl:tcl-eval 'selection 'own '-selection
           (swl:resolve-selection 'claim-selection selection)
           handle)
         (let ([cb
                (or (prop-ref 'lost-selection-callback)
                    (let ([cb
                           (swl:procedure->callback
                             (lambda ()
                               (prop-set! 'lost-selection-notify #f)
                               (notify-proc))
                             handle)])
                      (prop-set! 'lost-selection-callback cb)
                      cb))])
           (prop-set! 'lost-selection-notify notify-proc)
           (swl:tcl-eval 'selection 'own '-command cb
             '-selection (swl:resolve-selection 'claim-selection selection)
             handle)))
     (void)]
))

(define isa-tk-widget? (lambda (x) (send x isa? <tk-widget>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Container-Widget
;;
;;   something that knows about having children
;;   (may need some method for iterating something over the kids)
;;
;;   The keyboard traversal stuff here is rather bogus.  It should
;;   be based on some more visual aspect of the layout, or at give
;;   the programmer more convenient control over it.  Right now it
;;   just traverses the children list (ordered by the way they were
;;   created).

(define-swl-class (<container-widget> parent tcl-name) (<tk-widget> parent tcl-name)
  ;* Abstract class for widgets that have children.
  (ivars (children '()))
  (inherited parent handle)
  (inheritable children parent handle)
  (private
    [real-disown (child)
     (thread-critical-section (set! children (remq child children)))]
    [try-focus (kid caller)
     (and (get-accept-focus kid)
          (visible? kid)
          (begin
            (if (isa-container-widget? kid)
                (case caller
                  [(focus-next) (send kid focus-next #f)]
                  [(focus-prev) (send kid focus-prev #f)])
                (set-focus kid))
            #t))]
    [focus-scan (who kids caller)
     ;; if who is #f then take first for focus-next last for focus-prev
     ;; (implements focus-first and focus-last)
     (let loop ([ks kids] [found? (not who)] [punt #f])
       (cond
         [(null? ks)
          (unless punt  ;; keep us from looping if noone accepts focus.
            (if found?
                (if parent
                    (case caller  ;; methods not first class ...
                      [(focus-next) (send parent focus-next self)]
                      [(focus-prev) (send parent focus-prev self)])
                    (loop kids found? #t))
                (assertion-violationf caller "~s is not a child of ~s" who self)))]
         [found?
          (let ([fst (car ks)])
            (unless (eq? fst who)
              (or (try-focus fst caller) (loop (cdr ks) found? punt))))]
         [(eq? (car ks) who) (loop (cdr ks) #t punt)]
         [else (loop (cdr ks) #f punt)]))])
  (protected
    [torch-kids () (for-each (lambda (child) (send child destroy)) children)])
  (public
    [adopt (child)
     ;? (internal use)
     ;* \ret{unspecified}
     ;* records \var{child} as a child of this instance.
     ;; really should only get adopted once, main idea is to keep chilluns
     ;; from being collected away.
     (thread-critical-section
       (set! children (cons child children)))]
    [disown (child)
     ;? (internal use)
     ;* \ret{unspecified}
     ;* removes \var{child} from the set of children for this instance.
     (real-disown child)]
    [set-height! (val)
     ;* \ret{unspecified}
     ;; get-height is in <tk-prewidget> above
     ;* Sets the height in pixels of the widget.
     (set-tk-option '-height val swl:distance-unit? 'set-height!)]
    [set-width! (val)
     ;* \ret{unspecified}
     ;; get-width is in <tk-prewidget> above
     ;* Sets the width in pixels of the widget.
     (set-tk-option '-width val swl:distance-unit? 'set-width!)]
    [focus-prev (who)
     ;* \ret{unspecified}
     ;* Set keyboard focus to the widget before \scheme{who}.
     (focus-scan who children 'focus-prev)]
    [focus-next (who)
     ;* \ret{unspecified}
     ;* Set keyboard focus to the widget after \scheme{who}.
     (focus-scan who (reverse children) 'focus-next)]
    [key-press (key modifiers)
     ;% \ret{unspecified}
     ;; Should probably do this stuff as part of set-focus method on a
     ;; container.
     (event-case ((key= key) (modifier= modifiers))
       (([shift #\tab])
        (unless (null? children)
          (let ((last (car (last-pair children))))
            (or (try-focus last 'focus-prev)
                (focus-prev self last)))))
       (([#\tab])
        (unless (null? children)
          (let ((first (car children)))
            (or (try-focus first 'focus-next)
                (focus-next self first)))))
       (else (send-base self key-press key modifiers)))]
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;;
;;   ;; start of the grid stuff which I'm not sure I want to be public
;;   ;; lest we decide to radically change it when we drop Tk
;;   ;;
;;   ;;;;;  NONE OF THIS WORKS YET!   (currently in design phase)
;;   ;;
;;       [insert-item (column row depth widget)
;;        ;; Yes, this is an inefficient way to communicate w/ the grid
;;        ;; geometry manager (since it accepts scads of options).
;;        ;; This will be a good candidate for some sort of buffering
;;        ;; on the Tk port.  Could make the Tk port a thread parameter...
;;        ;;
;;        ;* Inserts the widget at \var{column} and \var{row} in the layout managed
;;        ;* by this container.
;;   "
;;   *** DANGER ***
;;   Tk segmentation faults when column / row are > 128
;;     (actually, seg faults when -row is 200)
;;   *** DANGER ***
;;   "
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'set-grid-item! "column must be a non-negative integer"))
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'set-grid-item! "row must be a non-negative integer"))
;;          ;; potentially more efficient to do string matching on the handles
;;          ;; of child and self
;;          (unless (memq widget children)
;;            (assertion-violationf 'set-grid-item! "~s is not a child of this ~s" widget self)))
;;        (swl:tcl-eval 'grid widget '-column column '-row row)
;;        (void)]
;;       [lookup-items (column row)
;;   ;; return a list of the items
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'get-grid-item "column must be a non-negative integer"))
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'get-grid-item "row must be a non-negative integer")))
;;        (swl:lookup-widget
;;          (string->symbol
;;            (swl:tcl-eval 'grid 'slaves handle '-column column '-row row)))]
;;   ; 
;;   ; this sticky stuff is actually a widget-level method
;;   ; 
;;   ; NOTE:  Kent suggests making everything parent-level and forcing the
;;   ;        user to wrap the widgets with something  like (center ...) if
;;   ;        they want it centered.  and (sticky ... nsew) if they want it
;;   ;        sticky.
;;   ; 
;;       [set-grid-sticky! (flags)
;;        ;; Yes, this is an inefficient way to communicate w/ the grid
;;        ;; geometry manager (since it accepts scads of options).
;;        ;; This will be a good candidate for some sort of buffering
;;        ;; on the Tk port.  Could make the Tk port a thread parameter...
;;        ;* For now, \var{flags} is the symbol \scheme{center}, or a symbol whose
;;        ;* name is some combination of
;;        ;* \scheme{n}, \scheme{s}, \scheme{e}, \scheme{w}, indicating the desired placement
;;        ;* of the widget within its cell:  centered, or adhering to one
;;        ;* or more sides of the cell.
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'set-grid-sticky! "column must be a non-negative integer"))
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'set-grid-sticky! "row must be a non-negative integer"))
;;          ;; potentially more efficient to do string matching on the handles
;;          ;; of child and self
;;          (unless (memq widget children)
;;            (assertion-violationf 'set-grid-sticky! "~s is not a child of this ~s" widget self)))
;;        (swl:tcl-eval 'grid widget '-column column '-row row)
;;        (void)]
;;       [get-grid-sticky ()
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'get-grid-sticky "column must be a non-negative integer"))
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'get-grid-sticky "row must be a non-negative integer")))
;;        (swl:lookup-widget
;;          (string->symbol
;;            (swl:tcl-eval 'grid 'slaves handle '-column column '-row row)))]
;;   
;;       [set-minimum-height! (row height)
;;        ;* Sets the minimum height for the given \var{row} in the layout
;;        ;* for this container to \var{height} pixels.
;;        (swl:safety-check
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'set-minimum-height! "row must be a non-negative integer"))
;;          (unless (swl:distance-unit? height)
;;            (assertion-violationf 'set-minimum-height! "invalid height ~s" height)))
;;        (swl:tcl-eval 'grid 'rowconfig handle '-minsize height)
;;        (void)]
;;       [get-minimum-height (row)
;;        ;* Returns the minimum height, in pixels, for the given \var{row} in
;;        ;* the layout for this container.
;;        (swl:safety-check
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'get-minimum-height "row must be a non-negative integer")))
;;        (string->number (swl:tcl-eval 'grid 'rowconfig handle '-minsize))]
;;       [set-minimum-width! (column width)
;;        ;* Sets the minimum width for the given \var{column} in the layout
;;        ;* for this container to \var{width} pixels.
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'set-minimum-width! "column must be a non-negative integer"))
;;          (unless (swl:distance-unit? width)
;;            (assertion-violationf 'set-minimum-width! "invalid width ~s" width)))
;;        (swl:tcl-eval 'grid 'columnconfig handle '-minsize width)
;;        (void)]
;;       [get-minimum-width (column)
;;        ;* Returns the minimum width, in pixels, for the given \var{column} in
;;        ;* the layout for this container.
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'get-minimum-width "column must be a non-negative integer")))
;;        (string->number (swl:tcl-eval 'grid 'columnconfig handle '-minsize))]
;;       [set-row-weight! (row weight)
;;        ;; not sure I really want this to be public.
;;        ;* Sets the relative weight for \var{row} in the layout
;;        ;* for this container to \var{weight} (a floating point value).
;;        ;* The weight is used to determine how excess vertical space
;;        ;* is distributed among the rows when the container is resized.
;;        ;*
;;        ;* If weight is set to \scheme{#f}, widgets in this row are clipped
;;        ;* rather than resized when the container shrinks below their
;;        ;* desired height.  This is a curiosity of the underlying Tk Toolkit.
;;        ;?
;;        ;? A strange quirk of Tk (4.1b1 at least) is that when
;;        ;? weight is zero (Tk's default) widgets are
;;        ;? clipped instead of resized when the container shrinks to
;;        ;? encroach their bounds.
;;        (swl:safety-check
;;          (unless (and (fixnum? row) (fx>= row 0))
;;            (assertion-violationf 'set-row-weight! "row must be a non-negative integer"))
;;          (unless (or (not weight) (and (>= weight 0) (or (fixnum? weight) (flonum? weight))))
;;            (assertion-violationf 'set-row-weight! "invalid weight ~s" weight)))
;;        (swl:tcl-eval 'grid 'rowconfig handle '-weight weight)
;;        (void)]
;;       [get-row-weight (row)
;;        ;* Returns the relative weight for \var{row} in the layout
;;        ;* for this container as a floating point value, or \scheme{#f}
;;        ;* if widgets in this row are clipped rather than resized.
;;        (swl:safety-check
;;          (unless (and (fixnum? row) (fx>= row 0))
;;            (assertion-violationf 'get-row-weight "row must be a non-negative integer")))
;;        (let ((x (swl:tcl-eval 'grid 'rowconfig handle '-weight)))
;;          (let ((v (string->number x)))
;;            (and (not (zero? v)) v)))]
;;       [set-column-weight! (column weight)
;;        ;; not sure I really want this to be public.
;;        ;* Sets the relative weight for \var{column} in the layout
;;        ;* for this container to \var{weight} (a floating point value).
;;        ;* The weight is used to determine how excess horizontal space
;;        ;* is distributed among the columns when the container is resized.
;;        ;*
;;        ;* If weight is set to \scheme{#f}, widgets in this column are clipped
;;        ;* rather than resized when the container shrinks below their
;;        ;* desired width.  This is a curiosity of the underlying Tk Toolkit.
;;        ;?
;;        ;? A strange quirk of Tk (4.1b1 at least) is that when
;;        ;? weight is zero (Tk's default) widgets are
;;        ;? clipped instead of resized when the container shrinks to
;;        ;? encroach their bounds.
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (fx>= column 0))
;;            (assertion-violationf 'set-column-weight! "column must be a non-negative integer"))
;;          (unless (or (not weight) (and (>= weight 0) (or (fixnum? weight) (flonum? weight))))
;;            (assertion-violationf 'set-column-weight! "invalid weight ~s" weight)))
;;        (swl:tcl-eval 'grid 'columnconfig handle '-weight weight)
;;        (void)]
;;       [get-column-weight (column)
;;        ;* Returns the relative weight for \var{column} in the layout
;;        ;* for this container as a floating point value, or \scheme{#f}
;;        ;* if widgets in this column are clipped rather than resized.
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (fx>= column 0))
;;            (assertion-violationf 'get-column-weight "column must be a non-negative integer")))
;;        (let ((x (swl:tcl-eval 'grid 'columnconfig handle '-weight)))
;;          (let ((v (string->number x)))
;;            (and (not (zero? v)) v)))]
;;       [bounding-box (column row)
;;        ;* Returns the bounding box for the given position in the grid layout
;;        ;* for this container as a list of four values.  The first two describe
;;        ;* the pixel offset of the grid position from the top-left corner of
;;        ;* the container.  The second two indicate the width and height of
;;        ;* this position.
;;        (swl:safety-check
;;          (unless (and (fixnum? column) (>= column 0))
;;            (assertion-violationf 'bounding-box "column must be a non-negative integer"))
;;          (unless (and (fixnum? row) (>= row 0))
;;            (assertion-violationf 'bounding-box "row must be a non-negative integer")))
;;        (swl:tcl->scheme (swl:tcl-eval 'grid 'bbox handle column row))]
;;   ;;
;;   ;; end of the grid stuff which I'm not sure I want to be public
;;   ;; lest we decide to radically change it when we drop Tk
;;   ;;
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    [destroy ()
     ;* \ret{unspecified}
     ;* Destroys the widget and its children.
     (send self hide)
     (critical-section
       (vector-set! self 0
         (let ([err (lambda (msg . args)
                      (assertion-violationf msg "instance has been destroyed"))])
           (swl:dispatch-table err
             [scheme->tcl (op) (display handle op)]
             [disown (child) (real-disown child)]
; this is gross
             [key-press (k m) (void)]
             [key-release (k m) (void)]
             [mouse-press (x y m) (void)]
             [mouse-enter (x y m) (void)]
             [mouse-leave (x y m) (void)]
             [mouse-motion (x y m) (void)]
             [mouse-release (x y m) (void)]
             [configure (w h) (void)]
             [destroy () (void)])))
       (torch-kids)
       (when (null? children) (send-base self destroy)))]))

(define isa-container-widget? (lambda (x) (isa? x <container-widget>)))

;; swl:set-grab widget   releases any existing grab and sets grab to this widget
;; swl:set-grab #f       releases any existing grab
;; should error check that it's a widget 
;; watchdog stuff is for debugging code that uses grab (eg. menus)

#; ; unused
(define swl:set-grab
  (let ([curr #f] [watchdog #f])
    (define debug-grab? #f)       ; #f --> causes optimizer to drop debug code
    (case-lambda
      [(who) (swl:set-grab who 'local)]
      [(who type)
       (when debug-grab?
         (let ([op (swl:bug-port)])
           (fprintf op "SET-GRAB ~s~n"
             (and who
                  (let ([op (open-output-string)])
                    (send who scheme->tcl op)
                    (get-output-string op))))
           (fprintf op "===> swl:set-grab ~s " (if who 'on 'off))
           (when who (send who print op))
           (fprintf op " ~s~n" type)
           (flush-output-port op)))
       (critical-section
         (when debug-grab? (when watchdog (thread-kill watchdog)))
         (unless (eq? who curr)
           (swl:sync-display)
           (when curr (swl:tcl-eval 'grab 'release curr))
           (when (and who (isa? who <tk-prewidget>))
             (case type
               [(global)
                (when debug-grab?
                  (set! watchdog
                    (thread-fork
                      (lambda ()
                        (thread-sleep 3000)
                        (let ([op (swl:bug-port)])
                          (fprintf op "WATCHDOG:  Woof!~n")
                          (fprintf op "(eq? (thread-self) watchdog) ==> ~s~n"
                            (eq? (thread-self) watchdog))
                          (swl:set-grab #f 'local))))))
                (swl:tcl-eval 'catch #\{ 'grab 'set '-global who #\})]
               [(local) (swl:tcl-eval 'catch #\{ 'grab 'set who #\})]
               [else (assertion-violationf 'swl:set-grab "~s must be global or local" type)]))
           (set! curr who)))])))

