(require "../common/popup.ss")


(define-swl-class (<help-text> parent) (<text> parent)
  (ivars
   (libdir #f)
   (token-delimiters '(#\space #\tab #\newline #\return #\page
			       #\( #\) #\[ #\] #\' #\" #\` #\, #\@))
   )
  (inherited handle)
  (inheritable handle)
  (private)
  (protected)
  (public
   [init (parent)
    (send-base self init parent)
    (set! libdir (getenv "SWL_ROOT"))
    ]
   [word-start (pos)
     (let* ([ix (send self get-cursor-pos)]
	    [x (car ix)]
	    [y (cdr ix)]
	    [str (send self get-string (cons 0 y) ix)]
	    [newx 
	     (let f ((ofs (1- x)))
	       (if (positive? ofs)
		   (let ((ch (string-ref str ofs)))
		     (if (memq ch token-delimiters)
			 (1+ ofs)
			 (f (1- ofs))))
		   0))])
       (cons newx y))]
   [word-end (pos)
     (let* ([ix (send self get-cursor-pos)]
	    [x (car ix)]
	    [y (cdr ix)]
	    [str
	     (let ((s (send self get-string ix (cons 0 (1+ y)))))
	       (if s s ""))]
	    [len (string-length str)]
	    [newx 
	     (let f ((ofs 0))
	       (if (< ofs len)
		   (let ((ch (string-ref str ofs)))
		     (if (memq ch token-delimiters)
			 (+ x ofs)
			 (f (1+ ofs))))
		   (+ x len)))])
       (cons newx y))]
   [mouse-press (x y mods)
     (send-base self mouse-press x y mods)
     (event-case ((modifier= mods))
       [([left-button double])
	(let ((msg
	       (let ((s (swl:get-selection)))
		 (if s s ""))))
	  (let ((len (string-length msg)))
	    (let g ((i 0))
	      (when (fx< i len)
		(string-set! msg i (char-downcase (string-ref msg i)))
		(g (fx1+ i)))))
	  (unless (top-level-bound? 'swl:help-data)
	    (set! swl:help-viewer-queue #f)
	    (set! swl:help-data
	      (let ((ip (open-input-file
			 (string-append libdir "/" "swl-help.ss"))))
		(let f ((acc '()) (datum (read ip)))
		  (if (eof-object? datum)
		      acc
		      (f (cons datum acc) (read ip)))))))
	  (let ((hit (assq (string->symbol msg) swl:help-data))
		(make-action
		 (lambda (help-entry)
		   (lambda () 
		     (unless swl:help-viewer-queue
		       (new-viewer 'help))
		     ;; new-viewer executes in another thread!
		     (let f ((i 20)) ;;; give it up to 10 secs to come up
		       (when (positive? i)
			 (unless swl:help-viewer-queue
			   (thread-sleep 500)
			   (f (1- i)))))
		     (unless swl:help-viewer-queue
		       (assertion-violationf '<help-text> "help viewer failed"))
		     (thread-send-msg swl:help-viewer-queue
				      (string-append
				       libdir "/" (car help-entry)))
		     (swl:clear-selection)
		     ))))
	    (when hit
	      (let ((pop (make-popup
			  (cons "Identifier Help"
				(cons #f
				      (map cdr (cdr hit))))
			  (cons #f
				(cons 'hrule
				      (map make-action (cdr hit)))))))
		(show-popup pop
			    (+ (send self get-root-x) x -8)
			    (+ (send self get-root-y) y -8))))
	    ))]
       )]
   )
  )


  #!eof

