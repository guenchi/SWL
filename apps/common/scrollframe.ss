;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; BUGS
;;   - currently possible to get scrollframe into ugly infinite loop where
;;     it tries to add a scrollbar and that changes the display enough that
;;     another scrollbar can be removed, but now we don't need the first
;;     scrollbar, ad nauseum.


;; (create <scrollframe> parent
;;    with (sticky-hscroll: #t)
;;         (default-vscroll: #t))
;;
;; creates a scrollframe that has a vscroll-bar by default on startup
;; and whose hscroll-bar is initially hidden, but once shown, remains visible 


;; Todo
;;  - eliminate lameness (need for flag, critical-section blah, etc.)
;;  - smooth scroll implemented via place
;;  - eliminate icky apply
;;    [cure SWL or fix compiler to optimize (lambda x (apply f a b c x))]
;;  - abstract scrollbar stuff

(define-swl-class (<scrollframe> parent) (<frame> parent)
  ;* A scrollframe acts much like any other frame except that
  ;* it has scrollbars that can be used to scroll the view in
  ;* the children of the frame.
  ;*
  ;* At present a scrollframe only permits one child to be
  ;* displayed at a time.  When a new child of the scroll-
  ;* frame is created, it becomes the currently visible thing.
  ;* (gack order of evaluation, but it's probably convenient)
  ;* To redisplay another child, use the make-visible method.
  (ivars (hscroll-bar #f) (vscroll-bar #f) (current #f)
         (vsticky #f) (hsticky #f) (live? #t))
  (inherited children)
  (inheritable)
  (private
    [make-vscroll-bar (top bot)
     (thread-critical-section
       (when live?
         (set! vscroll-bar
           (create <scrollbar> self with (orientation: 'vertical)))
         (set-view! vscroll-bar top bot)))]
    [make-hscroll-bar (left right)
     (thread-critical-section
       (when live?
         (set! hscroll-bar
           (create <scrollbar> self with (orientation: 'horizontal)))
         (set-view! hscroll-bar left right)))]
    [real-make-visible (child)
     (thread-critical-section
       (unless (eq? current child)
         (when current
           (hide current)
           (set-vscroll-notify! current (lambda args (void)))
           (set-hscroll-notify! current (lambda args (void))))
         (set! current child)
         (unless (isa? child <entry>)
           (set-vscroll-notify! child
             (lambda (top bot)
               (when live?
                 (if (and (= top 0) (= bot 1))
                     (when vscroll-bar
                       (if vsticky
                         (set-view! vscroll-bar 0 1)
                           (begin (destroy vscroll-bar)
                                  (set! vscroll-bar #f))))
                     (if vscroll-bar
                         (set-view! vscroll-bar top bot)
                         (begin (make-vscroll-bar top bot) (repack child))))))))
         (set-hscroll-notify! child
           (lambda (left right)
             (when live?
  ;;; Tk seems to be giving us some really weird initial values
  ;;; (like 0 0  and  1 1) who knows what else they can give us
             (if (or (and (= left 0) (or (= right 0) (= right 1)))
                     (and (= right 1) (or (= left 0) (= left 1))))
  ;;; was:
  ;;;        (if (and (= left 0) (= right 1)) ...)
                 (when hscroll-bar
                   (if hsticky
                       (set-view! hscroll-bar 0 1)
                       (begin (destroy hscroll-bar) (set! hscroll-bar #f))))
                 (if hscroll-bar
                     (set-view! hscroll-bar left right)
                     (begin (make-hscroll-bar left right) (repack child)))))))
         (repack child)))]
    [repack (child)
     ;; can't wait to have a real geometry mangler.
     (when live?
       (when vscroll-bar
         (set-action! vscroll-bar
           (lambda (self n q) (vscroll child n q))))
       (when hscroll-bar
         (set-action! hscroll-bar
           (lambda (self n q) (hscroll child n q))))
       (hide child)
       (when vscroll-bar (pack vscroll-bar (side: 'left) (fill: 'y)))
       (when hscroll-bar (pack hscroll-bar (side: 'bottom) (fill: 'x)))
       (pack child (expand: #t) (fill: 'both)))])
  (protected)
  (public
    [make-visible (child)
     (unless (memq child children)
       (assertion-violationf 'make-visible "~s is not a child of this scrollframe" child))
     (real-make-visible child)]
    [adopt (child)
     (send-base self adopt child)
     (unless (or current (isa? child <scrollbar>))
       (real-make-visible child))]
    [set-sticky-vscroll! (val)
     (unless (boolean? val)
       (assertion-violationf 'set-sticky-vscroll! "~s is not boolean" val))
     (set! vsticky val)]
    [set-sticky-hscroll! (val)
     (unless (boolean? val)
       (assertion-violationf 'set-sticky-hscroll! "~s is not boolean" val))
     (set! hsticky val)]
    [get-sticky-vscroll () vsticky]
    [get-sticky-hscroll () hsticky]
    [set-default-vscroll! (val)
     ;; kind of a hack, don't really need a setter for this, but that
     ;; works out conveniently for the (default-vscroll: #t) syntax of
     ;; create.
     (unless (boolean? val)
       (assertion-violationf 'set-default-vscroll! "~s is not boolean" val))
     (when val
       (make-vscroll-bar 0 1)
       (send self set-sticky-vscroll! #t))]
    [set-default-hscroll! (val)
     (unless (boolean? val)
       (assertion-violationf 'set-default-hscroll! "~s is not boolean" val))
     (when val
       (make-hscroll-bar 0 1)
       (send self set-sticky-hscroll! #t))]
    [get-default-vscroll () (and vscroll-bar #t)]
    [get-default-hscroll () (and hscroll-bar #t)]
    [for-children (fn) (for-each fn children)]
    [destroy ()
     ;; rather unfortunate, but it seems hscroll/vscroll-notify can be
     ;; invoked just before this widget is destroyed and run (on their
     ;; fallback queues) just after.  Throwing a flag at it doesn't
     ;; thrill me, but it should fix error messages like:
     ;;   Fallback thread (5) error in scheme->tcl: instance has been destroyed.
     ;;
     (set! live? #f)
     (send-base self destroy)]))
 
