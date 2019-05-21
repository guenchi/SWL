;;
;; vtool.ss - the version tool
;;

(define-swl-class (<voval> canvas x1 y1 x2 y2 version design)
              (<oval> canvas x1 y1 x2 y2)
  (ivars (canvas canvas)
	 (x1 x1)
	 (x2 x2)
	 (y1 y1)
	 (y2 y2)
	 (version version)
	 (design design)
	 (orig-cursor-icon #f)
	 (marker-rect #f)
	 )
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
   ;;;
   ;;; Must supply an init method here as number of formals differs
   ;;; from that of superclass.  Without our own init method we would 
   ;;; pick up one from the wrong superclass.
   ;;;
   [init (p x1 y1 x2 y2 . args)
     (send-base self init p x1 y1 x2 y2)
     ]
   [mouse-press (x y modifiers)
     (send design unparse-expr)
     (send design delete '(0 . 0) 'end)
     (send design insert-at '(0 . 0) (version-item version))
     (send design parse-expr)
     (send (send design get-curr-marker) remove-marker)
     (send design set-curr-version! version)
     (send design set-curr-marker! self)
     (send self draw-marker)
     ]
   [mouse-enter (x y modifiers)
     (set! orig-cursor-icon (send canvas get-mouse-cursor))
     (send canvas set-mouse-cursor! 'hand2)
     ]
   [mouse-leave (x y modifiers)
     (send canvas set-mouse-cursor! orig-cursor-icon)
     ]
   [draw-marker ()
     (set! marker-rect
       (create <rectangle> canvas (- x1 2) (- y1 2) (+ x2 2) (+ y2 2)))
     (send design set-curr-marker! self)
     ]
   [remove-marker ()
     (when marker-rect
       (send marker-rect destroy)
       (set! marker-rect #f))]
   ))


(define-swl-class (<vline> canvas x1 y1 x2 y2) (<line> canvas x1 y1 x2 y2)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
   [mouse-press (x y modifiers)
;;;	(pb "vline: mouse press: x=~s y=~s~n" x y)
	#f
	]
   ))


(define-swl-class (<vtool> parent) (<canvas> parent)
  (ivars
   (design #f)
   )
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
   [get-design () design]
   [set-design! (v) (set! design v)]
   
   [redraw ()
     (define rad 3)
     (define step 20)
     (define root 15)
     (torch-kids)
     (let ((versions (send design get-versions))
	   (curr-version (send design get-curr-version)))
       (define draw-version
	 (lambda (prevx prevy x y version)
	   (if (eqv? prevx x)
	       (unless (eqv? prevy y)
		 (create <vline> self prevx (+ prevy rad) x (- y rad)))
	       (begin
		 (create <vline> self prevx (+ prevy rad) prevx (- y rad))
		 (create <oval> self (- prevx 2) (- y 2) (+ prevx 2) (+ y 2))
		 (create <vline> self (+ prevx rad) y (- x rad) y)))
	   (let* ((vtag (version-tag version))
		  (voval
		   (create <voval> self (- x rad) (- y rad) (+ x rad) (+ y rad)
			   version design with
			   (fill-color: (cond [(eq? vtag #t) 'green]
					      [(eq? vtag #f) 'red]
					      [else 'black])))))
	     (when (eq? version curr-version)
	       (send voval draw-marker))
	     (unless (boolean? vtag)
	       (create <canvas-text> self (+ 15 x) y with
		       (title: (version-tag version))
		       (anchor: 'w))))
	   (void)))
       (define draw-versions
	 (lambda (px py nx ny vtree) ;;; p-previous, n-next
	   (if (null? vtree)
	       ny
	       (begin
		 (draw-version px py nx ny (vtree-version vtree))
		 (let f ((vtrees (vtree-children vtree))
			 (px nx)
			 (py ny)
			 (nx (+ step nx))
			 (ny (+ step ny)))
		   (cond [(null? vtrees) ny]
			 [(null? (cdr vtrees))
			  (draw-versions px py px ny (car vtrees))]
			 [else 
			  (let ([ny (draw-versions px py nx ny (car vtrees))])
			    (f (cdr vtrees) px py px ny))]
			 ))))))
       (draw-versions root root root root versions))]
   )
  )


;;
;; ADT: vtree - an n-ary tree of versions
;;
;; A vtree is a structure that contains an element (version), and a list
;; of zero or more sub-vtrees (children).
;;
;;
;; <a . (b c)>  -->   a
;;                    |-. 
;;                    | |
;;                    b c
;;


(define-structure (version tag item))
(define-structure (vtree version children))


;;
;; vtree-add-version (v cv vtree)
;;
;; if v is a version in vtree, 
;;      return a new vtree with cv as a child of v,
;; else return vtree.
;;
(define vtree-add-version
  (lambda (v cv vtree)
    (let ((new-child (make-vtree cv '())))
      (define add-version
	(lambda (vtree)
	  (cond [(eq? v (vtree-version vtree))
		 (make-vtree v (cons new-child (vtree-children vtree)))]
		[else
		 (let ((children
			(let f ((vtrees (vtree-children vtree)))
			  (if (null? vtrees)
			      '()
			      (let ((first (add-version (car vtrees))))
				(if (eq? first (car vtrees))
				    (let ((rest (f (cdr vtrees))))
				      (if (eq? rest (cdr vtrees))
					  vtrees
					  (cons first rest)))
				    (cons first (cdr vtrees))))))))
		   (if (eq? children (vtree-children vtree))
		       vtree
		       (make-vtree (vtree-version vtree) children)))])))
      (add-version vtree))))


			     
	     
	   

