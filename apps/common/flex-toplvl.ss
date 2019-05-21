;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;; destroys toplevel if its destroy-method returns true.

;; just an example.  really should do this w/ a macro.

;; need to spruce this up so it has the other event-notify methods
;; (cf. flex-text/button.ss)

(define-swl-class (<flex-toplevel>) (<toplevel>)
  (ivars (destroy-method (lambda (x) #t)))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [set-destroy-method! (proc) (set! destroy-method proc)]
    [get-destroy-method () destroy-method]
    [destroy ()
     (when (destroy-method self) (send-base self destroy))]))

