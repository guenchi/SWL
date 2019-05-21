;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; Todo
;;  - do this with a macro instead
;;  - trash the init method hack once object system is fixed
;;  - replace specific flex-* with define-flex-class macro like Wally did

(define-swl-class (<flex-button> parent) (<button> parent)
  (ivars
    (key-press-method #f)
    (key-release-method #f)
    (mouse-enter-method #f)
    (mouse-leave-method #f)
    (mouse-press-method #f)
    (mouse-motion-method #f))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (p)
     (send-base self init p)
     (set! key-press-method
       (lambda (self key mod) (send-base self key-press key mod)))
     (set! key-release-method
       (lambda (self key mod)
         (send-base self key-release key mod)))
     (set! mouse-enter-method
       (lambda (self x y mod)
         (send-base self mouse-enter x y mod)))
     (set! mouse-leave-method
       (lambda (self x y mod)
         (send-base self mouse-leave x y mod)))
     (set! mouse-press-method
       (lambda (self x y mod)
         (send-base self mouse-press x y mod)))
     (set! mouse-motion-method
       (lambda (self x y mod)
         (send-base self mouse-motion x y mod)))]
    [set-key-press-method! (val) (set! key-press-method val)]
    [get-key-press-method () key-press-method]
    [set-key-release-method! (val) (set! key-release-method val)]
    [get-key-release-method () key-release-method]
    [set-mouse-press-method! (val) (set! mouse-press-method val)]
    [get-mouse-press-method () mouse-press-method]
    [set-mouse-enter-method! (val) (set! mouse-enter-method val)]
    [get-mouse-enter-method () mouse-enter-method]
    [set-mouse-leave-method! (val) (set! mouse-leave-method val)]
    [get-mouse-leave-method () mouse-leave-method]
    [set-mouse-motion-method! (val) (set! mouse-motion-method val)]
    [get-mouse-motion-method () mouse-motion-method]
    [key-press (key mods)
     (key-press-method self key mods)]
    [key-release (key mods)
     (key-release-method self key mods)]
    [mouse-motion (x y mods)
     (mouse-motion-method self x y mods)]
    [mouse-enter (x y mods)
     (mouse-enter-method self x y mods)]
    [mouse-leave (x y mods)
     (mouse-leave-method self x y mods)]
    [mouse-press (x y mods)
     (mouse-press-method self x y mods)]))

