;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scrollbar
;;

;;   -ignoring activerelief and elementborderwidth

;; recently replaced lots of calls to (get-action self) with references
;; to inherited ivar action since any subclass that changes the return
;; value (or introduces side-effects into) get-action without going through
;; the base set-action! is broken

(swl:api-class (<scrollbar> parent) (<proto-slider> parent 'scrollbar)
  ;% \swlapi{class}{<scrollbar>}{(create <scrollbar> parent)}
  ;* \ret{instance}
  ;;
  ;* The size and position of the scrollbar's slider provides information
  ;* about what is visible in an associated window.  Clicking on the arrows
  ;* at either end of the scrollbar, or dragging on the slider typically
  ;* scrolls the view in the associated window.  Like scales, scrollbars
  ;* can be oriented horizontally or vertically.
  ;*
  ;* Soon we will provide classes that abstract the process of
  ;* linking a scrollbar with another widget.
  (ivars)
  (inherited handle drag action)
  (inheritable handle)
  (private)
  (protected
    [set-drag! (x y)
     ;; Tk's delta function actually handles orientation for us, but why
     ;; bother doing extra work on Scheme side?
     (set! drag
       (let* ([view (get-view self)]
              [start (car view)]
              [end (cadr view)]
              [fn (cond
                    ((get-jump-scroll self)
                     (lambda (a b) (set-view! self a b)))
                    (action
                     (lambda (a b) (action self a 'fraction)))
                    (else (lambda (a b) (void))))])
         (case (get-orientation self)
           [(vertical)
            (let ([old-y y])
              (lambda (x y)
                (let ([delta
                       (string->number
                         (swl:tcl-eval handle 'delta 0 (- y old-y)))])
                  (fn (+ start delta) (+ end delta)))))]
           [(horizontal)
            (let ([old-x x])
              (lambda (x y)
                (let ([delta
                       (string->number
                         (swl:tcl-eval handle 'delta (- x old-x) 0))])
                  (fn (+ start delta) (+ end delta)))))])))]

; need to make a post-pass to see if there is really any sharing between
; scale.ss and scrollbar.ss:  for example, we could replace small/big-slide
; with approx [do-slide (amt qualifier) (action amt qualifier))]

    [small-slide (fn)
     ; fn is more gainfully employed in scale.ss  (?)
     (when action (action self (fn 1) 'units))
     (void)]
    [big-slide (fn)
     ; fn is more gainfully employed in scale.ss  (?)
     (when action (action self (fn 1) 'pages))
     (void)]
    [slider-hit (x y)
     (let ([hit (identify x y)])
       (set-slider-relief! 'sunken)
       (cond
         [(string=? hit "slider")
          (set-drag! x y)]
         [(string=? hit "arrow1")
          (begin-slide (lambda () (small-slide -)))]
         [(string=? hit "arrow2")
          (begin-slide (lambda () (small-slide +)))]
         [(string=? hit "trough1")
          (begin-slide (lambda () (big-slide -)))]
         [(string=? hit "trough2")
          (begin-slide (lambda () (big-slide +)))]))]
    [set-slider-relief! (relief)
     (swl:tcl-eval handle 'config '-activerelief relief)
     (void)]
    [activate (x y)
     (swl:tcl-eval handle 'activate (identify x y))
     (void)]
    [deactivate (x y)
     (swl:tcl-eval handle 'activate 'none)
     (void)])
  (public
    [key-press (key modifiers)
     ;% \ret{unspecified}
     (let ([view (get-view self)])
       (event-case ((key= key) (modifier= modifiers))
         (([home])
          (set-view! self 0 (- (cadr view) (car view)))
          (when action (action self (car (get-view self)) 'fraction)))
         (([end])
          (set-view! self (- 1 (- (cadr view) (car view))) 1)
          (when action (action self (car (get-view self)) 'fraction)))
         (else (send-base self key-press key modifiers))))]
    [mouse-press (x y modifiers)
     ;% \ret{unspecified}
     (event-case ((modifier= modifiers))
       (([control left-button])
        (let ([hit (identify x y)])
          (cond
            [(or (string=? hit "trough1") (string=? hit "arrow1"))
             (key-press self (keysym->keycode 'home) modifiers)]
            [(or (string=? hit "trough2") (string=? hit "arrow2"))
             (key-press self (keysym->keycode 'end) modifiers)])))
       (else (send-base self mouse-press x y modifiers)))]
    [mouse-release (x y modifiers)
     ;% \ret{unspecified}
     ;; We're just extending the functionality of proto-sliders here,
     ;; so always call base method.  i.e. the send-base should not be
     ;; inside an else clause.
     (event-case ((modifier= modifiers))
       (([left-button])
        (when (get-jump-scroll self)
          (when action (action self (car (get-view self)) 'fraction)))))
     (send-base self mouse-release x y modifiers)]
    [set-jump-scroll! (val)
     ;* \ret{unspecified}
     ;* \var{val} is a boolean that determines whether or not
     ;* the scrollbar waits until it is released before notifying
     ;* the associated widget.  The default is \scheme{#f} meaning that
     ;* controlled widgets are continuously notified as the scrollbar
     ;* is dragged.
     (set-tk-option '-jump val boolean? 'set-jump-scroll!)]
    [get-jump-scroll ()
     ;* \ret{see below}
     ;* Returns a boolean that indicates whether or not
     ;* the scrollbar waits until it is released before notifying
     ;* the associated widget.
     (get-tk-option '-jump tk->boolean 'get-jump-scroll)]
    [set-view! (top bottom)
     ;* \ret{unspecified}
     ;* Sets the size and position of the scrollbar based on information from
     ;* the controlled widget about what fraction of its content is out of
     ;* view to the \var{top} (or left) and what fraction of its content
     ;* is \var{not} out of view to the \var{bottom} or right.
     (swl:tcl-eval handle 'set top bottom)
     (void)]
    [get-view ()
     ;* \ret{see below}
     ;* Returns the values set in the most recent call to \scheme{set-view!}.
     (swl:tcl->scheme (swl:tcl-eval handle 'get))]))

