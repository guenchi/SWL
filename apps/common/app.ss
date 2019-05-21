;;
;; Copyright (c) 1998 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defines <app-toplevel>, which creates a toplevel application with
;; and a framed mini-buffer. <app-toplevel>
;; is designed to work with instances of <app-text>.
;;

(define-swl-class (<search-entry> parent) (<entry> parent)
  (ivars
   [escape-thunk #f]
   )
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
   [set-escape-thunk! (et) (set! escape-thunk et)]
   
   [key-press (key modifiers)
     (event-case ((key= key) (modifier= modifiers))
       (([escape]) (escape-thunk))
       (else (send-base self key-press key modifiers)))]
   )
  )


(define-swl-class (<app-toplevel>) (<toplevel>)
  (ivars
   [app-menu-items #f]
   [menu-swl #f]
   [mini-frame #f]
   [mini-buffer #f]
   [mini-search #f]
   [text #f]
   )
  (inherited)
  (inheritable mini-buffer mini-search)
  (private)
  (protected)
  (public
   [init ()
     (send-base self init)
     (set! mini-frame (create <frame> self))
     (set! mini-buffer (create <entry> mini-frame with
			       (background-color: 'white)
                               (foreground-color: 'black)
                               (disabled-foreground-color: 'blue)))
     (set! mini-search (create <search-entry> mini-frame with
			       (disabled-background-color: 'white)
			       (disabled-foreground-color: 'black)
			       (background-color: 'white)))
     
     (pack mini-frame (expand: #f) (fill: 'x))
     (pack mini-buffer (expand: #t) (fill: 'x))
     ]

   [get-text-widget () text]

   [notify-text (tx)
     (send tx init-mini mini-buffer mini-search)
     (set! text tx)]

   [set-font! (fnt)
     (unless text (assertion-violationf 'set-font! "text widget not set"))
     (send text set-font! fnt)
     (send mini-buffer set-font! fnt)
     (send mini-search set-font! fnt)]
   )
  )

#!eof
