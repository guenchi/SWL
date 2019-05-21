;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;; Abstrax changes:
;;   see notes below on easy changes to provide Abstrax with the
;;   menu-item "hide" functionality they like.  (actually it's
;;   trivial for them to get hide-like functionality vai set-menu-items!)

; BUGS
;   - when invoking top-level menu that's been posted but not torn off
;     it stays posted after we choose an item (from sub-menu)
;   - I no longer remember what the hell the previous bug description means.
;     (I probably fixed it at some point.)
;   - probably need code audit to abstract things
;   - may need more critical sections
;   - not clear how to protect user against menu actions that loop indefinitely

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Menu
;;

; Skipping support for
;  -transient
;  -activeborderwidth

(swl:api-class (<menu> items) (<tk-prewidget3> #f)
  ;% \swlapi{class}{<menu>}{(create <menu> items)}
  ;* \ret{instance}
  ;;
  ;* A menu displays a list of labels that perform actions or post cascaded
  ;* menus when selected.
  ;* A list of \scheme{items} may be supplied when a menu is created,
  ;* or installed via the \scheme{set-menu-items!} method.
  ;* The items are expected to be instances of the various menu item
  ;* classes described later.
  ;* The order of items in the list determines their order on the menu.
  ;* The \scheme{make-menu} macro simplifies the construction of instances of
  ;* \scheme{<menu>}. 
  ;* Tear-off menus are disabled until a Tk bug with tear-offs is fixed.
  ;; cascade is the cascade menu posted if any
  ;;
  ;; items is a list of items if the menu hasn't yet been realized
  ;;   (via set-parent!) otherwise it is a vector
  ;;
  (ivars (items items) (item-count (length items)) (parent #f))
  (inherited handle)
  (inheritable)
  (private
    [raw-check (who)
     ;; check for attempts to use menu before parented (will catch pop-ups)
     ;; (items is a list if the menu hasn't been realized yet, eventually
     ;;  items is a vector)
     ;; eventually we'll address the problem of reparenting widgets, etc.
     ;; and this won't matter.
     (unless handle
       (assertion-violationf who "menu is not attached to a toplevel, menu, or option-button"))]
    [initialize-items! (who ls)
     ;; called from thread-critical-section
     (let loop ([items ls] [i 0])
       (if (null? items)
           (set! item-count i)
           (loop (cdr items)
                 (let ([item (car items)])
                   (unless (isa-menu-item? item)
                     (assertion-violationf who "invalid menu item ~s" item))
                   (send (car items) add-to-menu! self i)
                   (fx+ i 1)))))
     (set! items (list->vector ls))])
  (protected
    [set-tk-option (tclname val test who)
     (raw-check who)
     (send-base set-tk-option tclname val test who)]
    [get-tk-option (tclname conversion who)
     (raw-check who)
     (send-base get-tk-option tclname conversion who)])
  (public
    [init args
     ;% \ret{unspecified}
     (void)]
    [y-position (y)
     ;% \ret{an integer}
     (raw-check 'y-position)
     (string->number
       (swl:tcl-eval self 'yposition
         (string-append "@" (number->string y 10))))]
    [set-parent! (p)
     ;* \ret{unspecified}
     ;* Informs the menu of its parent.  \var{p} must be an instance of
     ;* \scheme{<toplevel>} or \scheme{<menu>}.
     (set! parent p)
     (swl:safety-check
       (unless (or (isa? p <toplevel>) (isa-menu? p) (isa-option-button? p))
         (assertion-violationf #f "menu ~s must be attached to a toplevel or cascade" self)))
     (when handle (assertion-violationf #f "menu ~s is already assigned to ~s" self p))
     (set! handle (swl:new-handle p))
     (swl:insert-widget handle self (swl:fallback-queue))
     (swl:widget-guardian self)
     (swl:tcl-eval 'menu handle '-tearoff #f)
     (swl:tcl-eval 'wm 'protocol handle '|WM_DELETE_WINDOW|
       (swl:make-destroy-notify handle))
;;   (swl:tcl-eval 'bindtags handle '|SWL|)
     ;; If people want tear off menus let them roll their own.
     ;; to help with standard look and feel, we could provide them with some
     ;; bitmap they can use as the image of the tear off thing and a tearoff
     ;; method on menus
     (thread-critical-section
       (initialize-items! #f items))]
    [set-menu-items! (ls)
     ;* \ret{unspecified}
     ;* Sets the list of menu items to \var{ls}.  Menu contents can 
     ;* be manipulated with all the list-processing tools available
     ;* in Scheme.
     ;* It is undefined what happens when the list contains duplicates.
     ;* The list must contain only menu items.
     (raw-check 'set-menu-items!)
     (thread-critical-section
       (swl:tcl-eval handle 'delete 0 'end)
       (initialize-items! 'set-menu-items! ls))]
    [get-menu-items ()
     ;* \ret{see below}
     ;* Returns a copy of the list of menu items contained in this menu.
     (raw-check 'get-menu-items)     ;; do we really need this????
     (vector->list items)]
    [y->item (y)
     ;* \ret{see below}
     ;* Returns the menu item with this y-coordinate in the menu.
     (raw-check 'y->item)
     (let ([index
            (string->number
              (swl:tcl-eval self 'index
                (string-append "@" (number->string y 10))))])
       (and index (<= 0 index item-count) (vector-ref items index)))]
    [destroy ()
     ;* \ret{unspecified}
     ;* Destroys the menu and any sub-menus.
     (critical-section
       (when handle
         (swl:delete-widget handle)
         (for-each (lambda (x) (send x destroy)) (vector->list items))
         (when (tk->boolean (swl:tcl-eval 'winfo 'exists handle))
           (swl:tcl-eval 'destroy handle)
           (set! items #f)
           (set! handle #f))))]
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
    [set-select-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color used to show when the item is
     ;* selected.  If the indicator is drawn (see \scheme{set-draw-indicator!})
     ;* it is filled with this color when the item is selected, otherwise
     ;* this color is used as the background color for the item when
     ;* selected.
     ;* \var{val} is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     ;* The color is red.
     (set-tk-option '-selectcolor val swl:color? 'set-select-color!)]
    [get-select-color ()
     ;* \ret{see below}
     ;* Returns the color used to show when the item is selected
     ;* (see \scheme{set-select-color!}).
     (get-tk-option '-selectcolor tk->color 'get-select-color)]
    [get-parent () parent]
))

;  when menus are torn off, need to make them act like toplevels
;      - WM_DELETE_PROTOCOL, title, etc.
;  
;  MENU
;  
;  -activebackground
;  -activeborderwidth
;  -activeforeground
;  -background
;  -borderwidth
;  -cursor
;  -disabledforeground
;  -font
;  -foreground
;  -relief
;  -selectcolor
;  -takefocus
;  -transient
;  
;  -tearoff
;  -postcommand
;  -tearoffcommand
;  
;  
;  
;  SEPARATOR:   invoke / activate are NOPs
;  -background
;  
;  all other menu items:
;  -activebackground
;  -activeforeground
;  -accelerator
;  -background
;  -bitmap
;  -font
;  -foreground
;  -image
;  -label
;  -state
;  -underline
;  		-command
;  
;  
;  COMMAND items:
;  
;  
;  CASCADE:
;  -menu
;  
;  
;  RADIOBUTTON items:
;  -indicatoron
;  -selectcolor
;  -selectimage
;  		-value
;                  -variable
;  
;  CHECKBUTTON:
;  -indicatoron
;  -selectcolor
;  -selectimage
;  		-offvalue
;  		-onvalue
;                  -variable
;  
;     - maybe don't bother caching options up, make the menus up front
;     - I don't know, seems like we should be able to make a list of items
;       hand them to a menu and say go for it dude.
;          - get-*-value guys have to query some menu somewhere for a value
;            (maybe cons up a dummy menu somewhere for them)
;            (else they go to their parent for the info they don't know
;             when they hvae no index)
;          - set-*-valoue guys could be cached up in a list
;  
;  need to have a default-menu-object that can be used when no opts set
;  menu's init method passes menu and index that items can use to do stuff
;  set option maps across those procs
;  get option can take the first, or default if none
;  
;  (create <menu> (list of menu-items))
;  
;   [init (items)
;    (let loop ((items items) (i 1))
;      (unless (null? items)
;        (add-to-menu (car items) self i)
;        (loop (cdr items) (fx+ i 1))))]
;  
;  when menu is attached to something (or posted 1st time)
;  it creates the actual Tk menu from its members
;  
; * should mouse-enter / leave /press / release / key-press / release etc.
;   be sent to menu items???

(define-swl-class (<proto-menu-item> type) (<tk-object>)
  (ivars (options '()) (menu #f) (handle #f) (type type))
  (inherited)
  (inheritable handle menu)
  (private)
  (protected
    [set-tk-option (tclname value err-check name)
     (swl:safety-check
       (unless (err-check value) (assertion-violationf name "bad value ~s" value)))
     (thread-critical-section
       (let ((found (memq tclname options)))
         (if found
             (set-car! (cdr found) value)
             (set! options `(,tclname ,value . ,options))))
         (when menu
           (swl:tcl-eval menu 'entryconfig handle tclname value)))
     (#3%void)]
    [get-tk-option (tclname conversion name)
     (let ((found (memq tclname options)))
       (if found
           (if (eq? tclname '-state)
               (conversion (cadr found)) ;; set-enabled! converts val
               (cadr found))
           (case tclname
             ((-activebackground -activeforeground -background
               -font -foreground)
              (conversion (swl:tcl-eval (swl:thedefaultmenu) 'cget tclname)))
             ((-label) "")
             ((-state -indicatoron) #t)  ; default #t
             (else #f))))])
  (public
    [get-parent () menu]
    [activate (menu)
     ;* \ret{unspecified}
     ;* Activate \var{menu}.
     (when (send self get-enabled)
       (let ((i handle))
         (and i (swl:tcl-eval menu 'entryconfig i '-state 'active))))
     (void)]
    [deactivate (menu)
     ;* \ret{unspecified}
     ;* Deactivate \var{menu}.
     (when (send self get-enabled)
       (let ((i handle))
         (and i (swl:tcl-eval menu 'entryconfig i '-state 'normal))))
     (void)]
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
    [add-to-menu! (m i)
     ;% \ret{unspecified}
     ;* internal use only
     (unless (isa? m <menu>) (assertion-violationf 'add-to-menu! "~s is not a <menu>" m))
     (unless (and (fixnum? i) (fx>= i 0))
       (assertion-violationf 'add-to-menu! "~s is not a valid index" i))
     (when (and menu (not (eq? m menu)))
       (assertion-violationf '<menu> "menu item ~s already belongs to another menu" self))
     (thread-critical-section
       (set! menu m)
       (set! handle i)
       (apply swl:tcl-eval menu 'add type options))
     (void)]
;     [remove-from-menu ()
;      ;* internal use only
;      (set! menu #f)
;      (set! handle #f)]
; This stuff could be used to support visibility for Lou and Wally
; basically we just have to hack the initialize-items! method of
; <menu> so that it skips over the "invisible" items (simple test)
; then we add a <menu> re-initialize method that does the obvious thing.
;
;    [visible? () visible?]
;    [hide ()
;     (set! visible? #f)
;     (send menu re-initialize)]
;    [show ()
;     (set! visible? #t)
;     (send menu re-initialize)]
;    [remove-from-menu (menu menu-live?)
;     ;* internal use
;     ;;; this looks broken, "menus" and "handles" are globals here...
;     ;;; moreover, it's not being used anywhere and was documented as
;     ;;; internal use only
;     (thread-critical-section
;       (call-with-values
;         (lambda ()
;           (let loop ([ms menus] [hs handles])
;             (cond
;               [(null? ms)
;                (assertion-violationf 'remove-from-menu "~s is not in ~s" self menu)]
;               [(eq? menu (car ms))
;                (when menu-live? (swl:tcl-eval menu 'delete (car hs)))
;                (values (cdr ms) (cdr hs))]
;               [else
;                (call-with-values
;                  (lambda () (loop (cdr ms) (cdr hs)))
;                  (lambda (mrest hrest)
;                    (values
;                      (cons (car ms) mrest)
;                      (cons (car hs) hrest))))])))
;         (lambda (ms hs) (set! menus ms) (set! handles hs))))]
    [init ignore-args
     ;* \ret{unspecified}
     ;* Initializes the instance.
     (void)]))

(define isa-menu-item?
  (lambda (x)
    (and (instance? x) (send x isa? <proto-menu-item>))))

(define-swl-class (<proto-menu-item2> type) (<proto-menu-item> type)
  (ivars (image #f) (action #f))
  (inherited handle menu)
  (inheritable handle menu)
  (private
    [drop-image ()
     ;; drop pointer to image so it can be collected
     (when image
       (set! image #f)
       (set-tk-option '-image '() null? 'set-title!))]
    [set-my-title (val)
     (cond
       [(string? val)
        (drop-image)
        (let ([val
               (case (machine-type)
                 [(ppcosx i3osx)
                  ;; apparently Mac OS X won't let us have
                  ;; menu titles containing an ellipsis.
                  ;; For example, the "Go to line..." menu
                  ;; item showed up as <Error>.
                  ;; 
                  ;; This "solution" is a bit extreme, but we've got
                  ;; zero time now to improve it...
                  (list->string (remove #\. (string->list val)))]
                 [else val])])
          (set-string-title! val))]
       [else
        (set-tk-option '-image val isa-image? 'set-title!)
        ;; save image from collection
        (set! image val)])]
    [set-string-title! (string)
     (unless (string? string)
       (assertion-violationf 'set-title! "~s is not a string" string))
     (let ([len (string-length string)])
       (let loop ([i 0])
         (cond
           [(fx= i len)
            (set-tk-option '-label string (lambda (x) #t) 'set-title!)]
           [(char=? #\_ (string-ref string i))
            (set-tk-option '-underline i fixnum? #f)
            (set-tk-option '-label
              (string-append
                (substring string 0 i)
                (substring string (fx+ i 1) len))
              string?
              #f)]
           [else (loop (fx+ i 1))])))]
    [get-string-title ()
     (let ((i (get-tk-option '-underline (lambda (x) x) 'get-title))
           (txt (get-tk-option '-label (lambda (x) x) 'get-title)))
       (if i
           (string-append
             (substring txt 0 i)
             "_"
             (substring txt i (string-length txt)))
           txt))])
  (protected
    [run-action ()
     (let ([proc (send self get-action)])
       (when (and (procedure? proc) (send self get-enabled))
         (proc self)))])
  (public
    [invoke (m)
     ;* \ret{unspecified}
     ;* Invokes the menu action.
     (unless (isa? m <menu>) (assertion-violationf 'invoke "~s is not a <menu>" m))
     (run-action)]
    [set-action! (val)
     ;* \ret{unspecified}
     ;* \var{val} specifies a procedure of one argument (self) to be called when
     ;* the left mouse button is released over this menu item or when
     ;* \mytt{Return} or \mytt{Space} are pressed when this item has keyboard focus.
     ;* Action procedures of toggles like radio-menu-items and check-menu-items should
     ;* query the state of self, using the \scheme{get-selected} method, to determine
     ;* how to proceed.  The action procedure is invoked as notification that the
     ;* state may have changed.  The procedure should rely on the state within the
     ;* menu item and not maintain its own state.  This helps with the automatic
     ;* preferences enabling code.
     (swl:safety-check
       (unless (procedure? val)
         (assertion-violationf 'set-action! "~s is not a procedure" val)))
     (set! action                  ; hold w/ strong pointer
       (swl:procedure->callback
         (lambda x (send self invoke menu))
         (swl:fbq-handle (swl:fallback-queue))))
     (prop-set! 'real-action-proc val)
     (set-tk-option '-command action (lambda x #t) 'unreachable)]
    [get-action ()
     ;* \ret{procedure}
     ;* Returns a procedure of one argument (self) that is invoked when this
     ;* menu item is chosen with the keyboard or mouse.
     (prop-ref 'real-action-proc)]
    [set-title! (val)
     ;* \ret{unspecified}
     ;* Sets the text displayed by the menu item to the string or image supplied
     ;* as \var{val}.
     ;* If \var{val} is a pair, then the first element (a string or image) is displayed
     ;* as the title and the second (a string) is displayed at the right side of the
     ;* menu item (eg. to show a keyboard short-cut).
     ;* If the title contains an underscore character, the following character
     ;* is displayed with an underline.  (There is no escape for this yet.)
     ;* The underlined characters in the titles of menu items in a
     ;* \scheme{<toplevel>} widget's menu bar indicate keys that can be
     ;* pressed with the \scheme{Alt} key to post the corresponding menus.
     ;* See \hpageref{menu-tutorial}{the \scheme{menu} tutorial} for an example.
     (cond
       [(pair? val)
        (set-tk-option '-accelerator (cdr val) string? 'set-title!)
        (set-my-title (car val))]
       [else (set-my-title val)])]
    [get-title ()
     ;* \ret{see below}
     ;* Returns the string displayed by the widget.
     (let ([acc (get-tk-option '-accelerator (lambda (x) x) 'get-title)]
           [ttl (or image (get-string-title))])
       (if acc (cons ttl acc) ttl))]
    [set-enabled! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean value indicating whether or not the widget is
     ;* enabled and responsive to user interaction.
     (set-tk-option '-state
       (if (boolean? val) (if val 'normal 'disabled) val)
       (lambda (x) (memq x '(normal disabled)))
       'set-enabled!)]
    [get-enabled ()
     ;* \ret{boolean}
     ;* Returns a boolean value indicating whether or not the widget is
     ;* enabled and responsive to user interaction.
     (get-tk-option '-state (lambda (x) (not (eq? x 'disabled))) 'get-enabled)]
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
     (get-tk-option '-foreground tk->color 'get-foreground-color)]))

(define-swl-api-class (<separator-menu-item>) (<proto-menu-item> 'separator)
  ;* \ret{instance}
  ;* A separator-menu-item is drawn as a line separating the menu items
  ;* it is placed between.  It has no associated action and exists purely
  ;* for decoration.
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [get-enabled ()
     ;* \ret{boolean}
     ;* Always returns false.
     ;* Always \scheme{#f} for separator-menu-items.
     #f]
    [get-title ()
     ;* \ret{see below}
     ;* Always the empty string for separator-menu-items.
     ""]
    [activate (menu) (void)]
    [deactivate (menu) (void)]))

(define-swl-api-class (<command-menu-item>) (<proto-menu-item2> 'command)
  ;* \ret{instance}
  ;* The command-menu-item displays a string or image and invokes the
  ;* specified action when selected from the menu.
  (ivars)
  (inherited handle)
  (inheritable)
  (private)
  (protected)
  (public))

(define-swl-api-class (<cascade-menu-item>) (<proto-menu-item2> 'cascade)
  ;* \ret{instance}
  ;* A cascade-menu-item displays a string or image and displays a sub-menu
  ;* when selected from the menu.
  ;* When a menu is used as the menu bar of a \scheme{<toplevel>}, the
  ;* first level cascade-menu-items form the row of menus items visible in the
  ;* menu bar.
  (ivars (sub-menu #f) (posted? #f))
  (inherited handle menu)
  (inheritable)
  (private
    [initialize-sub-menu ()
     (set-parent! sub-menu menu)
     (set-tk-option '-menu sub-menu isa-menu? 'set-menu!)])
  (protected)
  (public
    [invoke (m)
     ;; error test is done by the base method
     ;; (unless (isa? m <menu>) (assertion-violationf 'invoke "~s is not a <menu>" m))
     (send-base self invoke m)
     (when (and (not posted?) sub-menu (send self get-enabled))
       (set! posted? #t)
       (swl:tcl-eval m 'postcascade handle))
     sub-menu]
    [deactivate (m)
     ;* \ret{unspecified}
     ;* Deactivates the menu item.
     (thread-critical-section   ;; actually the critical section is pointless
       (when posted?
         (set! posted? #f)      ;; break loop
         (send m hide-cascade #f))
       (send-base self deactivate m))]
    [set-menu! (m)
     ;* \ret{unspecified}
     ;* Sets the menu to be posted when this item is chosen to \var{menu}
     ;* which must be an instance of \scheme{<menu>}.
     (set! sub-menu m)
     (when menu (initialize-sub-menu))]
    [get-menu ()
     ;* \ret{see below}
     ;* Returns the menu to be posted when this item is chosen to \var{menu}
     ;* which must be an instance of \scheme{<menu>}.  Returns \scheme{#f} if none.
     sub-menu]
    [get-menu-items ()
     ;* \ret{see below}
     ;* Returns a copy of the list of menu items contained in this menu.
     (if (not sub-menu) '() (send sub-menu get-menu-items))]
    [add-to-menu! (m i)
     ;% \ret{unspecified}
     (send-base self add-to-menu! m i)
     (when sub-menu (initialize-sub-menu))]))

(define isa-cascade? (lambda (x) (isa? x <cascade-menu-item>)))

;; Tk's interface to radio and check items is gross.
(define-swl-class (<proto-fancy-menu-item> type) (<proto-menu-item2> type)
  (ivars (selectimage #f) (selected? #f) (tkvar #f) (tkval "1") (index #f))
    ; tkval needs to be something that will be string=? to the result of
    ; (swl:tcl-eval 'set tkvar) when we need to test whether the button is set.
    ; needs to be other than #t/#f
  (inherited handle)
  (inheritable handle tkvar tkval)
  (private)
  (protected
    [select-on-add? (x) #f]
    [finish-add ()
     ; after set of index and tkvar
     (when selected? (send self set-selected! #t))])
  (public
    [set-prefs-key! (v)
     ;* \ret{unspecified}
     ;* Sets the preferences key for this menu item.
     ;* When set, the \scheme{set-selected!} method
     ;* stores the current state in the preferences cache
     ;* for the current application.
     ;* When set, the applications preferences must already
     ;* have been loaded (via \scheme{load-prefs})
     ;* before \scheme{set-selected!} is called.
     ;* Saved preferences override settings installed
     ;* by \scheme{set-selected!} until the menu item is actually
     ;* installed in a menu that is attached to a window.
     (prop-set! 'prefs-key v)
     ]
    [get-prefs-key ()
     ;* \ret{symbol or \scheme{#f}}
     ;* Returns the preferences key for this menu item.
     (prop-ref 'prefs-key)]

    [set-title! (val)
     ;% \ret{unspecified}
     (set! selectimage #f)
     (send-base self set-title! val)]
    [set-draw-indicator! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean value determining whether or not the state
     ;* of the item is indicated by drawing a small square or diamond
     ;* filled or not filled with the select color (see \scheme{set-select-color!}).
     (set-tk-option '-indicatoron val boolean? 'set-draw-indicator!)]
    [get-draw-indicator ()
     ;* \ret{see below}
     ;* Returns a boolean value indicating whether or not the state
     ;* of the item is depicted by drawing a small square or diamond
     ;* filled or not filled.
     (get-tk-option '-indicatoron tk->boolean 'get-draw-indicator)]
    [set-select-color! (val)
     ;* \ret{unspecified}
     ;* Sets the color used to show when the item is
     ;* selected.  If the indicator is drawn (see \scheme{set-draw-indicator!})
     ;* it is filled with this color when the item is selected, otherwise
     ;* this color is used as the background color for the item when
     ;* selected.
     ;* \var{val} is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     ;* The color is red.
     (set-tk-option '-selectcolor val swl:color? 'set-select-color!)]
    [get-select-color ()
     ;* \ret{see below}
     ;* Returns the color used to show when the item is selected
     ;* (see \scheme{set-select-color!}).
     (get-tk-option '-selectcolor tk->color 'get-select-color)]
    [set-select-image! (val)
     ;* \ret{unspecified}
     ;* Sets the image to be displayed when this item is selected.
     (set-tk-option '-selectimage val swl:image? 'set-select-image!)
     (set! selectimage val)]
    [get-select-image ()
     ;* \ret{see below}
     ;* Returns the image displayed when this item is selected.
     (get-tk-option '-selectimage tk->image 'get-select-image)]
    [add-to-menu! (m i)
    ; must install parent first so that get-prefs can walk the widget tree
     (send-base self add-to-menu! m i)
     (let ([key (send self get-prefs-key)])
       (let ([x (if key (send self get-pref key #!bwp) #!bwp)]) ; want the actual pref value
         (cond
           [(eq? x #!bwp)] ; no preference
           [(select-on-add? x)
            (send self set-selected! #t)
            (run-action)]
           [else (send self set-selected! #f)])))
     (set! index i)
     (set! tkvar m)
     (void)]
    [set-selected! (val)
     ;* \ret{unspecified}
     ;* Marks this item selected so that its selector is drawn.
     (if (not (and index tkvar))
         (set! selected? val)
         (begin
           (let ([key (send self get-prefs-key)])
             (when key (send self set-pref! key val)))
           (swl:tcl-eval 'set tkvar (and val tkval))))
     (void)]
    [get-selected ()
     ;* \ret{boolean}
     ;* Returns the selected status of the item.
     (if (and index tkvar)
         (string=? tkval (swl:tcl-eval 'set tkvar))
         selected?)]
))

(define-swl-api-class (<radio-menu-item>) (<proto-fancy-menu-item> 'radiobutton)
 ;; could use the item's index instead
  ;* \ret{instance}
  ;* A radio-menu-item displays a string and a selector or an image that
  ;* indicates whether or not the option represented by this item is selected.
  ;* Radio-menu-items on the same menu are mutually exlusive: at most one
  ;* can be selected.
  (ivars)
  (inherited handle tkvar tkval)
  (inheritable)
  (private)
  (protected
    [select-on-add? (pref) (equal? pref (get-title self))])
  (public
    [invoke (m)
     ;* \ret{unspecified}
     ;* Invokes the menu action.
    ; set selected first so that invoke picks up the new value!
     (send self set-selected! #t)
     (send-base self invoke m)
     (void)]
    [add-to-menu! (m i)
     (let ([myval (or (get-title self) (number->string i))])
       (set! tkval myval)
       ; must set tkval first, in order for get-pref to work
       (send-base self add-to-menu! m i)
       (swl:tcl-eval m 'entryconfig i '-variable tkvar '-value myval))
     (finish-add)
     (void)]
    [set-pref! (key val) (send-base self set-pref! key (and val tkval))]
#;
  [get-pref (key default)
;---------------
; Expedient hack
;---------------
; should factor in default here, but we only need this to work for the
; console.ss preferences code, and then we can take SWL out and shoot it
;---------------
     (equal? tkval (send-base self get-pref key #f))]
))

;; Tk's interface to radio and check items is gross.

(define-swl-api-class (<check-menu-item>) (<proto-fancy-menu-item> 'checkbutton)
  ;* \ret{instance}
  ;* A check-menu-item displays a string and a selector or an image that
  ;* indicates whether or not the option represented by this item is selected.
  ;* Check-menu-items on the same menu are not mutually exlusive: zero or more
  ;* may be selected.
  (ivars)
  (inherited handle tkvar tkval)
  (inheritable)
  (private)
  (protected
    [select-on-add? (bool) bool])
  (public
    [invoke (m)
     ;* \ret{unspecified}
     ;* Invokes the menu action.
     ; Tcl/Tk code has already set the tkvar for us in this case,
     ; now we can update our preferences.
     (send self set-selected! (send self get-selected))
     (send-base self invoke m)
     (void)]
    [add-to-menu! (m i)
     (send-base self add-to-menu! m i)
     (let ([myvar (gensym)])
       (set! tkvar myvar)
       (swl:tcl-eval m 'entryconfig i '-variable tkvar))
     (finish-add)
     (void)]))

; - nuke the tearoff item, instead provide a method for tearing off a menu
;   and let them invoke that in an action if they want
;
; api-syntax make-menu has been moved into syntax.ss

; was:
;      (syntax (create <menu> `((,label . ,item) (,l2 . ,i2) ...)))))))

(define simple-menu-list->menu-items
  (let ([f
         (lambda (item)
           (if (pair? item)
               (let ([x (cdr item)] [label (car item)])
                 (unless (or (string? label)
                             (and (pair? label)
                                  (string? (car label))
                                  (string? (cdr label)))
                             (memq label '(swl-menu help-menu)))
                   (assertion-violationf 'make-menu "invalid menu item label ~s" label))
                 (cond
                   [(eq? label 'swl-menu) (swl:application-menu)]
                   [(eq? label 'help-menu) (swl:help-menu)]
                   [(procedure? x)
                    (create <command-menu-item>
                      with (title: label) (action: x))]
                   [(isa-menu? x)
                    (create <cascade-menu-item>
                      with (title: label) (menu: x))]
                   [else
                    (assertion-violationf 'make-menu "~s must be procedure or menu" x)]))
               (assertion-violationf 'simple-menu-list->menu-items
                 "~s is not a pair"
                 item)))])
    (lambda (ls) (map f ls))))

(define isa-menu? (lambda (x) (isa? x <menu>)))

;; used for item queries when they don't know the default values
(define swl:thedefaultmenu
  (let ((menu #f))
    (lambda ()
      (or menu
          (let ((x (create <menu> '())))
            (set! menu x)
            x)))))

