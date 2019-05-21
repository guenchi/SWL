;; Copyright (c) 1996 John Zuckerman and Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; NOTE:  this functionality has been superceded by the sticky: options of
;;        scrollframe.ss

;; Todo
;;  - hide scrollbars when not needed
;;     - avoid creating vscroll for things that isa? <entry>
;;  - smooth scroll implemented via place
;;  - handle multiple children
;;  - eliminate icky apply
;;    [cure SWL or fix compiler to optimize (lambda x (apply f a b c x))]
;;
;; 4/7/96 (johnz) once created, a scrollbar can no longer be destroyed.
;;   (perhaps should be a class option.)

(define-swl-class (<scrollframe> parent) (<frame> parent)
  ;* A scrollframe acts much like any other frame except that
  ;* it has scrollbars that can be used to scroll the view in
  ;* the children of the frame.
  ;*
  ;* The implementation is currently limited to a single child.
  (ivars (hscroll-bar #f) (vscroll-bar #f))
  (inherited children)
  (inheritable)
  (private
    [repack (child)

;; no dice
;     (swl:sync-display)
;     (let ((wd (get-width self))
;           (ht (get-height self))
;           (x 0)
;           (y 0))
;(printf "wd ht = ~s~n" (cons wd ht))
;       (unless (or (zero? wd) (zero? ht))
;       (hide child)
;       (when vscroll-bar
;         (set! x (get-width vscroll-bar))
;         (swl:tcl-eval 'place vscroll-bar '-x (- wd x) '-y 0))
;       (when hscroll-bar
;         (set! y (get-height hscroll-bar))
;         (swl:tcl-eval 'place hscroll-bar '-y (- ht y) '-x 0))
;       (swl:tcl-eval 'place child '-x 0 '-y 0 '-relwidth (float (/ (- wd x) wd))
;                 '-relheight (float (/ (- ht y) ht)))))

     (hide child)
     (when vscroll-bar (pack vscroll-bar (side: 'left) (fill: 'y)))
     (when hscroll-bar (pack hscroll-bar (side: 'bottom) (fill: 'x)))
     (pack child (expand: #t) (fill: 'both))

])
  (protected)
  (public
    [adopt (child)
      (send-base self adopt child)
      (unless (isa? child <scrollbar>)
	(unless (<= (length children) 3) ;;; 2 for scrollbars (for now)
	  (assertion-violationf '<scrollframe>
            "current implementation does not support multiple children"))
	(unless (isa? child <entry>)
	  (set-vscroll-notify! child
	    (lambda (top bot)
	      (if vscroll-bar
		(set-view! vscroll-bar top bot)
		(unless (or (and (= top 0) (= bot 1))
			  (and (= top 0) (= bot 0)))
		;; (fprintf (swl:bug-port) "creating vbar! top=~s bot=~s~n" top bot)
		  (set! vscroll-bar
		    (create <scrollbar> self
		      with (orientation: 'vertical)))
		  (set-view! vscroll-bar top bot)
		  (set-action! vscroll-bar
		    (lambda (self n q) (vscroll child n q)))
		  (repack child))))))
	(set-hscroll-notify! child
	  (lambda (left right)
	    (if hscroll-bar
	      (set-view! hscroll-bar left right)
	      (unless (or (and (= left 0) (= right 1))
			(and (= left 1) (= right 1)))
		;; (fprintf (swl:bug-port) "creating hbar! left=~s right=~s~n" left right)
		(set! hscroll-bar
		  (create <scrollbar> self with (orientation: 'horizontal)))
		(set-view! hscroll-bar left right)
		(set-action! hscroll-bar
		  (lambda (self n q) (hscroll child n q)))
		(repack child)))))
	(repack child))]
    [destroy ()
     (hide self)
     (for-each hide children)
     (send-base self destroy)]))
