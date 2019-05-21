;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(let-syntax ([define-quiet-thing
              (lambda (form)
                (syntax-case form ()
                  [(_ name rent)
                   (with-capture (syntax name) (send-base set!)
                     (syntax
                       (define-svelte-class (name parent) (rent parent)
                         (public
                           [init (parent)
                             (send-base self init parent)
                             (set-option! self
                               (border-width: 0) (relief: 'flat))]
                           [mouse-enter args (void)]
                           [mouse-leave args (void)])))) ]))])
  (define-quiet-thing <quiet-button> <button>))

(define-svelte-class (<viewtext> parent iwid) (<text> parent)
  (ivars [iwid iwid])
  (public
    [init (parent iwid)
      (send-base self init parent)
      (set-option! self (wrap: 'none) (mouse-cursor: 'left_ptr))]
    [key-press (key modifiers)
      (event-case ((key= key) (modifier= modifiers))
	(([#\backspace] [delete] [prior]) (vscroll self -1 'pages))
	(([#\space] [next]) (vscroll self 1 'pages))
	(([up] [#\k] [control #\p]) (vscroll self -1 'units))
	(([down] [#\j] [control #\n]) (vscroll self 1 'units))	
	(([left] [#\h] [control #\b]) (hscroll self -1 'units))
	(([right] [#\l] [control #\f]) (hscroll self 1 'units))
	(([#\<]) (send self make-visible '(0 . 0)))
	(([#\>]) (send self make-visible 'end))

	(([#\q] [#\Q]) (send iwid key-signal 'quit))
	(([shift left] [#\B] [#\b]) (send iwid key-signal 'left))
	(([shift right] [#\F] [#\f]) (send iwid key-signal 'right))
	(else
;;;	  (printf "key: ~s mods: ~s~n" key modifiers)
	  (void)
	  ))]
;;;    [configure (w h)
;;;      (printf "conf: ~s ~s~n" w h)
;;;      (send-base self configure w h)]
    [mouse-enter args
      (set-focus self)]))

(define-svelte-class (<twoscrolltext> parent iwid) (<frame> parent)
  (ivars [twid #f] [vsb #f] [hsb #f] [v? #f] [h? #f]
    [twid2 #f] [vsb2 #f]
    [font #f]
    [iwid iwid])
  (public
    [get-text () twid]
    [set-font! (fn)
      (set! font fn)
      (set-option! twid (font: fn))
      (when twid2 (set-option! twid2 (font: fn)))]
    [remove-sub () (when twid2
		     (set-option! twid (height/char: (inspector-rows)))
		     (destroy twid2) (set! twid2 #f)
		     (when vsb2 (destroy vsb2) (set! vsb2 #f)))]
    [get-sub (n)
      (let ([n (min n 6)])
	(cond
	  [twid2
	    (set-option! twid2 (height/char: n))
	    (set-option! twid (height/char: (- (inspector-rows) n)))
	    (when font (set-option! twid2 (font: font)))
	    twid2]
	  [else
	    (set! twid2 (create <viewtext> self iwid with
			  (wrap: 'none) (border-width: 0)
			  (vpad: 0) (hpad: 0)
			  (insert-width: 0) (traversal-thickness: 0)
			  (width/char: (inspector-cols))))
	    (set-option! twid2 (height/char: n))
	    (set-option! twid (height/char: (- (inspector-rows) n)))
	    (when font (set-option! twid2 (font: font)))
	    (grid twid2 (row: 2) (column: 0) (sticky: 'ew))

	    (when (= n 6)
	      (set! vsb2 (create <scrollbar> self with (orientation: 'vertical)
			   (border-width: 2)
			   (width: 10)))
	      (set-action! vsb2 (lambda (sb n q) (vscroll twid2 n q)))
	      (set-vscroll-notify! twid2 (lambda (t b) (set-view! vsb2 t b)))

	      (grid vsb2 (row: 2) (column: 1) (sticky: 'ns)))
	    twid2]))]
    [init (parent iwid)
      (send-base self init parent)
      (set-option! self (border-width: 0))
      (set! twid (create <viewtext> self iwid with
		   (wrap: 'none) (border-width: 0)
		   (vpad: 0) (hpad: 0)
		   (insert-width: 0) (traversal-thickness: 0)) )
      (set-option! twid (width/char: (inspector-cols))
	(height/char: (inspector-rows)))
      (set! vsb (create <scrollbar> self with (orientation: 'vertical)
		  (border-width: 2)
		  (width: 10)))
      (set! hsb (create <scrollbar> self with (orientation: 'horizontal)
		  (border-width: 2)
		  (width: 10)))

      (grid twid (row: 0) (column: 0))

      (set-action! vsb (lambda (sb n q) (vscroll twid n q)))
      (set-action! hsb (lambda (sb n q) (hscroll twid n q)))

      (set-vscroll-notify! twid (lambda (t b)
				  (set-view! vsb t b)))
      (set-hscroll-notify! twid (lambda (l r)
				  (set-view! hsb l r)))

      (grid vsb (row: 0) (column: 1) (sticky: 'ns))
      (grid hsb (row: 1) (column: 0) (sticky: 'ew))]))

#!eof

(define-svelte-class (<scrolltext> parent) (<frame> parent)
  (ivars [twid #f] [vsb #f] [hsb #f] [v? #f] [h? #f])
  (public
    [get-text () twid]
    [set-font! (fn) (set-option! twid (font: fn))]
    [init (parent)
      (send-base self init parent)
      (set-option! self (border-width: 0))
      (set! twid (create <viewtext> self with
		   (wrap: 'none) (border-width: 0) (vpad: 0) (hpad: 0)
		   (insert-width: 0) (traversal-thickness: 0)) )
      (set-option! twid (width/char: (inspector-cols)) (height/char: (inspector-rows))
	(border-width: 0) (hpad: 0) (vpad: 0))
      (set! vsb (create <scrollbar> self with (orientation: 'vertical)
		  (border-width: 2)
		  (width: 10)))
      (set! hsb (create <scrollbar> self with (orientation: 'horizontal)
		  (border-width: 2)
		  (width: 10)))

      (grid twid (row: 0) (column: 0))

      (set-action! vsb (lambda (sb n q) (vscroll twid n q)))
      (set-action! hsb (lambda (sb n q) (hscroll twid n q)))

      (set-vscroll-notify! twid (lambda (t b)
				  (set-view! vsb t b)))
      (set-hscroll-notify! twid (lambda (l r)
				  (set-view! hsb l r)))

      (grid vsb (row: 0) (column: 1) (sticky: 'ns))
      (grid hsb (row: 1) (column: 0) (sticky: 'ew))


;;; horizontal scroll-bars are now sticky!

;;;   (letrec
;;;	([v-notify
;;;	   (lambda (top bot)
;;;	     (if (and (= top 0) (= bot 1))
;;;		 (when v?
;;;		   (set! v? #f)
;;;		   (hide vsb)
;;;		   (set-option! twid
;;;		     (width/char: (+ (get-option twid width/char:) 1)))
;;;		   (mv))
;;;		 (begin
;;;		   (unless v?
;;;		     (set! v? #t)
;;;		     (let ([start (send twid get-width)])
;;;		       (set-option! twid
;;;			 (width/char: (- (get-option twid width/char:) 1)))
;;;		       (let ([end (send twid get-width)])
;;;			 (set-option! vsb (width: 6))
;;;			 (grid vsb (row: 0) (column: 1) (row-span: 1)
;;;			   (sticky: 'ns))))
;;;		     (mv))
;;;		   (set-view! vsb top bot))))]
;;;	 [mv (lambda () (send twid make-visible (send twid get-cursor-pos)))]
;;;	 [h-notify
;;;	   (lambda (left right)
;;;	     (if (and (= left 0) (= right 1))
;;;		 #f
;;;		 (begin
;;;		   (unless h?
;;;		     (set! h? #t)
;;;		     (grid hsb (row: 1) (column: 0) (column-span: 1) (sticky: 'ew))
;;;		     (set-option! twid
;;;		       (height/char: (sub1 (get-option twid height/char:))))
;;;		     (mv))
;;;		   (set-view! hsb left right))))])
;;;	(set-vscroll-notify! twid v-notify)
;;;	(set-hscroll-notify! twid h-notify))
      ]))
