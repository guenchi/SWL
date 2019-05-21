;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; Todo
;;   - do this with a macro instead.
;;   - flesh out the other indirected methods
;;   - get rid of init method hackery once object system is fixed.

(define-swl-class (<flex-text> parent) (<text> parent)
  (ivars (key-press-method #f) (mouse-press-method #f))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (p)
     (send-base self init p)
     (set! key-press-method
       (lambda (self key mod)
         (send-base self key-press key mod)))
     (set! mouse-press-method
       (lambda (self x y mod)
         (send-base self mouse-press x y mod)))]
    [set-key-press-method! (val) (set! key-press-method val)]
    [get-key-press-method () key-press-method]
    [set-mouse-press-method! (val) (set! mouse-press-method val)]
    [get-mouse-press-method () mouse-press-method]
    [key-press (key mods)
     (key-press-method self key mods)]
    [mouse-press (x y mods)
     (mouse-press-method self x y mods)]))

