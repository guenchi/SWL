(define-swl-class (<popup>) (<toplevel>)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init ()
      (set-width! self 1)
      (set-height! self 1)
      (set-title! self "")
      (send self set-override-redirect! #t)
      (hide self)
      (swl:sync-display)
      (send-base self init)
      ]
    [mouse-leave (x y mods)
      (send-base self mouse-leave x y mods)
      (send self destroy)
      ]
    ))

	  
(define make-popup
  (lambda (ttls acts)
    (let ([pop (create <popup>)])
      (for-each
       (lambda (ttl act)
	 (let ((b 
		(create <button> pop with
			(title: ttl)
			(border-width: 1)
			(mouse-cursor: 'left_ptr)
			(action:
			 (lambda (b)
			   (hide pop)
			   (swl:sync-display)
			   (act))))))
	   (pack b (fill: 'x))))
       ttls
       acts)
      pop)))


(define show-popup 
  (lambda (p x y)
    (send p set-geometry! (format "+~s+~s" x y))
    (send p raise)
    (show p)))

#!eof

(define p (make-popup '("one" "Two much") `(,void ,void)))
(show-popup p 100 100)


