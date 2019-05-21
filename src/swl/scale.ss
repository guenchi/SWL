;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scale
;;

;; skipping the label option -- easy to get from compound widget
;; OTOH may be less efficient to do that
;; yet, it would certainly be more flexible,
;; and we eventually want labels etc to be as efficient as possible

(swl:api-class (<scale> parent) (<proto-slider> parent 'scale)
  ;% \swlapi{class}{<scale>}{(create <scale> parent)}
  ;* \ret{instance}
  ;;
  ;* A scale allows the user to interactively select a value
  ;* from a range of integer or floating point 
  ;* values by dragging a slider bar along a trough whose length
  ;* represents the range.
  ;* The slider can be moved by
  ;* clicking or dragging the mouse or using the arrow keys
  ;* when the slider has keyboard focus.
  ;* When the slider's value changes, the value is passed to the
  ;* procedure specified by \scheme{set-action!}.
  ;*
  ;* A scale can display the currently selected value underneath the slider
  ;* and can also show tick marks.  Scales can be oriented horizontally
  ;* or vertically.
  (ivars (old-value #f))
  (inherited handle drag action)
  (inheritable handle)
  (private)
  (protected
    [small-slide (fn)
     (let ([v (get-value self)]
           [from (get-min self)]
           [to (get-max self)]
           [res (get-resolution self)])
       (let ([newv (fn v res)])
         (set-value! self
           (cond [(< newv from) from] [(> newv to) to] [else newv]))))]
    [big-slide (fn)
     (let ((v (get-value self))
           (from (get-min self))
           (to (get-max self))
           (skip (get-skip-increment self)))
       (let ([newv (fn v skip)])
         (set-value! self
           (cond [(< newv from) from] [(> newv to) to] [else newv]))))]
    [slider-hit (x y)
     (let ([hit (identify x y)])
       (cond
         [(string=? hit "slider")
          (set-drag! x y)
          (set-slider-relief! 'sunken)]
         [(or (string=? hit "arrow1") (string=? hit "trough1"))
          (begin-slide (lambda () (small-slide -)))]
         [(or (string=? hit "arrow2") (string=? hit "trough2"))
          (begin-slide (lambda () (small-slide +)))]))]
    [set-drag! (x y)
     (set! drag
       (let ([old-val (get-value self)]
             [padding (+ (get-slider-length self)
                         (* 2 (get-traversal-thickness self))
                         (* 4 (get-border-width self)))])
         (case (get-orientation self)
           [(vertical)
            (let ([d (/ (- (get-max self) (get-min self))
                        (- (get-height self) padding))]
                  [old-y y])
              (lambda (x y)
                (set-value! self (- old-val (* d (- old-y y))))))]
           [(horizontal)
            (let ([d (/ (- (get-max self) (get-min self))
                        (- (get-width self) padding))]
                  [old-x x])
              (lambda (x y)
                (set-value! self (- old-val (* d (- old-x x))))))])))]
    [activate (x y)
     (swl:tcl-eval handle 'config '-state
       (if (string=? "slider" (identify x y)) 'active 'normal))
     (void)] 
    [deactivate (x y)
     (swl:tcl-eval handle 'config '-state 'normal)
     (void)]
    [set-slider-relief! (relief)
; once we go to tk4.1
;    (swl:tcl-eval handle 'config '-sliderrelief relief)
     (void)])
  (public
    [key-press (key modifiers)
     ;% \ret{unspecified}
     (event-case ((key= key) (modifier= modifiers))
       (([home]) (set-value! self (get-min self)))
       (([end]) (set-value! self (get-max self)))
       (else (send-base self key-press key modifiers)))]
    [mouse-press (x y modifiers)
     ;% \ret{unspecified}
     (event-case ((modifier= modifiers))
       (([control left-button])
        (let ([hit (identify x y)])
          (cond
            [(string=? hit "trough1") (set-value! self (get-min self))]
            [(string=? hit "trough2") (set-value! self (get-max self))])))
       (else (send-base self mouse-press x y modifiers)))]
    [set-digits! (val)
     ;* \ret{unspecified}
     ;* Sets the number of significant digits in the value of the scale.
     ;* If zero, then the scale chooses the smallest value that distinguishes
     ;* each position of the slider.
     (set-tk-option '-digits val (lambda (x) (and (fixnum? x) (not (fxnegative? x))))
         'set-digits!)]
    [get-digits ()
     ;* \ret{see below}
     ;* Returns the number of significant digits in the value of the scale.
     ;* If zero, then the scale chooses the smallest value that distinguishes
     ;* each position of the slider.
     (get-tk-option '-digits string->number 'get-digits)]
    [set-show-value! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean indicating whether or not the scale should
     ;* show its current value beside the slider.
     (set-tk-option '-showvalue val boolean? 'set-show-value!)]
    [get-show-value ()
     ;* \ret{see below}
     ;* returns a boolean indicating whether or not the scale
     ;* shows its current value beside the slider.
     (get-tk-option '-showvalue tk->boolean 'get-show-value)]
    [set-min! (val)
     ;* \ret{unspecified}
     ;* sets the low value of the range represented by the scale
     ;* to the integer or floating point number \var{val}.
     (set-tk-option '-from val swl:oknum? 'set-min!)]
    [get-min ()
     ;* \ret{see below}
     ;* returns the low value of the range represented by the scale.
     (get-tk-option '-from string->number 'get-min)]
    [set-max! (val)
     ;* \ret{unspecified}
     ;* sets the high value of the range represented by the scale
     ;* to the integer or floating point number \var{val}.
     (set-tk-option '-to val swl:oknum? 'set-max!)]
    [get-max ()
     ;* \ret{see below}
     ;* returns the high value of the range represented by the scale.
     (get-tk-option '-to string->number 'get-max)]
    [set-resolution! (val)
     ;* \ret{unspecified}
     ;* sets the resolution of the scale's value to \var{val}.  If greater than
     ;* zero, the value of the scale will be rounded to an even multiple
     ;* of \var{val}.
     (set-tk-option '-resolution val swl:oknum? 'set-resolution!)]
    [get-resolution ()
     ;* \ret{see below}
     ;* returns the resolution of the scale's value.
     (get-tk-option '-resolution string->number 'get-resolution)]
    [set-slider-length! (val)
     ;* \ret{unspecified}
     ;* sets the length of the slider along its long dimension to \var{val}
     ;* pixels.
     (set-tk-option '-sliderlength val swl:distance-unit? 'set-slider-length!)]
    [get-slider-length ()
     ;* \ret{see below}
     ;* returns the length in pixels of the slider along its long dimension.
     (get-tk-option '-sliderlength string->number 'get-slider-length)]
    [set-skip-increment! (val)
     ;* \ret{unspecified}
     ;* sets the size of the skips made when the scale is clicked on in
     ;* a way that jumps.
     (set-tk-option '-bigincrement val swl:oknum? 'set-skip-increment!)]
    [get-skip-increment ()
     ;* \ret{see below}
     ;* returns the size of the skips made when the scale is clicked on in
     ;* a way that jumps.
     (get-tk-option '-bigincrement string->number 'get-skip-increment)]
    [set-length! (val)
     ;* \ret{unspecified}
     ;* sets the length in pixels of the long dimension of the scale.
;; shouldn't let the length be set smaller than slider length, or should
;; adjust slider length to match...
     (set-tk-option '-length val swl:distance-unit? 'set-length!)]
    [get-length ()
     ;* \ret{see below}
     ;* returns the length in pixels of the long dimension of the scale.
     (get-tk-option '-length string->number 'get-length)]
    [set-value! (value)
     ;* \ret{unspecified}
     ;* sets the current value of the scale to \var{value}.
     ;* The \var{action} procedure, if any, is invoked with the instance
     ;* and the new value.
     (swl:tcl-eval handle 'set value)
;; call Tcl/Tk to get the result of applying the min, max, and resolution
;; settings to this value, could handle on Scheme side trading some hair
;; for performance.
     (let ((newval (get-value self)))
       (when (not (and old-value (= newval old-value))) (action self newval))
       (set! old-value newval))
     (void)]
    [set-action! (val)
     ;* \ret{unspecified}
     ;* \var{val} specifies a procedure to be called when
     ;* the value of the scale is changed.
     ;* The procedure is passed two arguments:  the instance and the
     ;* current value of the scale.
     (swl:safety-check
       (unless (procedure? val)
         (assertion-violationf 'set-action! "~s is not a procedure" val)))
     (set! action val)
     (action self (get-value self))]
    [get-value ()
     ;* \ret{see below}
     ;* returns the current value of the scale.
     (string->number (swl:tcl-eval handle 'get))]
    [set-foreground-color! (val)
     ;* \ret{unspecified}
     ;* Sets the default foreground color for the scale to \var{val},
     ;* which is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (set-tk-option '-foreground val swl:color? 'set-foreground-color!)]
    [get-foreground-color ()
     ;* \ret{see below}
     ;* Returns the default foreground color for the scale.
     ;* The value returned is either a symbol naming a color in
     ;* \mytt{/usr/lib/X11/rgb.txt} or an instance of \scheme{<rgb>}.
     (get-tk-option '-foreground tk->color 'get-foreground-color)]
    [set-font! (val)
     ;* \ret{unspecified}
     ;; could have gotten this by inheritance with more effort
     ;* Sets the default font for text displayed by the scale.
     ;* \var{val} is an instance of \scheme{<font>} (see the description
     ;* of the \scheme{make-font} macro).
     ;* This affects the display of the scale's value.
     (set-tk-option '-font val swl:font? 'set-font!)]
    [get-font ()
     ;* \ret{see below}
     ;* Returns an instance of \scheme{<font>} describing the default font
     ;* for text displayed by the scale.
     (get-tk-option '-font tk->font 'get-font)]))

