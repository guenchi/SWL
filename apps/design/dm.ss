(define gidx 0)
(define gdm #f)
(define blink-binding #f)

(define-swl-class (<design-markup>) (<markup>)
  (ivars
    (blink-thread #f)
    (syncat #f)
    (mini-text "")
    (expr #f)
    (exprdm #f)
    (env #f)
    (sidx #f)
    (eidx #f)
    (parent #f)
    (idx #f)
    (holes '()) ;;; a hole is a pair of offsets
    (text #f)
    (pop-ttls #f)
    (pop-acts #f)
    (pop #f)
    )
  (inherited)
  (inheritable)
  (private)
  (protected
    [idx1+ (ix) (send text add-offset ix 1)]
    [idx1- (ix) (send text add-offset ix -1)]
    )
  (public
    [get-syncat () syncat]
    [get-expr () expr]
    [get-exprdm () exprdm]
    [get-env () env]
    [get-sidx () sidx]
    [get-eidx () eidx]
    [get-parent () parent]
    [get-holes () holes]
    [get-text () text]
    [get-mini-text () mini-text]
    [get-pop-ttls () pop-ttls]
    [get-pop-acts () pop-acts]
    
    [set-syncat! (v) (set! syncat v)]
    [set-expr! (v) (set! expr v)]
    [set-exprdm! (v) (set! exprdm v)]
    [set-env! (v) (set! env v)]
    [set-sidx! (v) (set! sidx v)]
    [set-eidx! (v) (set! eidx v)]
    [set-parent! (v) (set! parent v)]
    [cons-hole! (v) (set! holes (cons v holes))]
    [set-text! (v) (set! text v)]
    [set-mini-text! (v) (set! mini-text v)]
    [set-pop-ttls! (v) (set! pop-ttls v)]
    [set-pop-acts! (v) (set! pop-acts v)]
    
    [class-name () '<design-markup>]
    [enable-blinking ()
      (if blink-thread
	(assertion-violationf '<design-markup> "enable-blinking: already enabled")
	(set! blink-thread 
	  (thread-fork-group
	    (lambda ()
	      (thread-name "<design-markup>:blinker")
	      (let ((color (send self get-background-color)))
		(interrupt-handler
		  (lambda ()
		    (send self set-background-color! color)
		    (thread-kill)))
		(let loop ()
		  (if (memq color '(white yellow pink))
		    (send self set-background-color! 'gray)
		    (send self set-background-color! 'white))
		  (thread-sleep 300)
		  (send self set-background-color! color)
		  (thread-sleep 300)
		  (loop)))))))]
    [disable-blinking ()
     (critical-section
       (when blink-thread
	 (thread-break blink-thread)
	 (set! blink-thread #f)))]
    [toggle-blinking () (if blink-thread
			  (send self disable-blinking)
			  (send self enable-blinking))]
    [move-to (dstidx)
      (send text unparse-expr)
      (let ((str (send text get-string sidx eidx)))
	(if (send text pos<? sidx dstidx)
	    (let ((dstmk (send text floating-mark dstidx))
		  (sp 
		   (let f ([ch (send text get-char eidx)])
		     (if (char-whitespace? ch)
			 (begin 
			   (send text delete eidx)
			   (cons ch (f (send text get-char eidx))))
			 '()))))
	      (send text delete sidx eidx)
	      (send text insert-at dstmk (list->string sp))
	      (send text insert-at dstmk str)
	      (send text set-cursor-pos! dstmk)
	      )
	    (begin
	      (send text delete sidx eidx)
	      (let ((dstmk (send text floating-mark dstidx))
		    (sp 
		     (let f ([spix (idx1- sidx)] [ans '()])
		       (let ([ch (send text get-char spix)])
			 (if (char-whitespace? ch)
			     (begin 
			       (send text delete spix)
			       (f (idx1- spix) (cons ch ans)))
			     ans)))))
		(send text insert-at dstmk str)
		(send text insert-at dstmk (list->string sp))
		(send text set-cursor-pos! dstmk)
		)))
      (send text raw-indent-current-line)
      (send text parse-expr)
      (send text add-version (string-append "move " (make-pretty str)))
      )]
    
    [mouse-press (text x y mods)
      (define not-ok
	(lambda () (send text display-mini "not a binding or binding form")))
      (define nested-form?
	(lambda (src dest)
	  (if (eq? src dest)
	    #t
	    (let ((parent (send src get-parent)))
	      (and parent (nested-form? parent dest))))))
      (send text clear-mini)
      (when pop (send pop destroy))
      (case syncat
	[(varref-lexical)
	 (set! pop 
	   (make-popup
	    '("Substitute")
	    (list 
	     (lambda () 
	       (let ((v (assq expr env)))
		 (when v
		   (let ((dm (cdr v)))
		     (when (eq? 'binding (send dm get-syncat))
		       (let* ((rhsdm (cadr (send dm get-exprdm)))
			      (rhssidx (send rhsdm get-sidx))
			      (rhseidx (send rhsdm get-eidx))
			      (rhsstr (send text get-string rhssidx rhseidx))
			      (ostr (send text get-string sidx eidx))
			      )
			 (send text unparse-expr)
			 (send text set-enabled! #t)
			 (send text delete sidx eidx)
			 (send text insert-at sidx rhsstr)
			 (send text set-enabled! #f)
			 (send text parse-expr)
			 (send text add-version
			       (string-append "subst ["
					      ostr
					      " -> "
					      (make-pretty rhsstr)
					      "]"))
			 ))))))
	     )))
	 (show-popup pop
		     (+ (send text get-root-x) x)
		     (+ (send text get-root-y) y))]
	[(boundvar binding bindings let letstar letrec keyword)
	 (if (not blink-binding)
	   (begin 
	     (case syncat
	       [(binding)
		(send self enable-blinking)
		(set! blink-binding self)]
	       [(boundvar)
		(set! blink-binding (send self get-parent))
		(send blink-binding enable-blinking)]
	       [else #f]))
	   (let ((srcbdm blink-binding))
	     (send blink-binding disable-blinking)
	     (set! blink-binding #f)
	     (let ((dstbdm
		    (case syncat
		      [(bindings) (car (send self get-exprdm))]
		      [(binding) self]
		      [(boundvar) (send self get-parent)]
		      [else #f]))
		   (dstformdm
		    (case syncat
		      [(bindings) (send self get-parent)]
		      [(binding) (send (send self get-parent) get-parent)]
		      [(boundvar)
		       (send (send (send self get-parent) get-parent) get-parent)]
		      [(keyword)
		       (if (not (memq expr '(let letstar letrec)))
			   (not-ok)
			   (send self get-parent))]
		      [(let letstar letrec) self]
		      [else (assertion-violationf 'design:mouse-press "invalid syncat: ~s" syncat)])))
	       (unless (eq? srcbdm dstbdm)
		 (let* ((srcformdm (send (send srcbdm get-parent) get-parent))
			(srcform (send srcformdm get-expr))
			(srcbexp (send srcbdm get-expr))
			(srcbvar (car srcbexp))
			(srcbsidx (send srcbdm get-sidx))
			(srcformbody (caddr (send srcformdm get-expr))))
		   (let* ((dstform (send dstformdm get-expr))
			  (dstbvars (map car (cadr dstform)))
			  (dstexprdm (send dstformdm get-exprdm))
			  (dstbsdm (cadr dstexprdm))
			  (dstbssidx (send dstbsdm get-sidx))
			  (dstformbody (caddr (send dstformdm get-expr)))
			  (dstidx (if dstbdm
				      (let ((dstbsidx (send dstbdm get-sidx)))
					(if (send text pos<? srcbsidx dstbsidx)
					    (send dstbdm get-eidx)
					    (send dstbdm get-sidx)))
				      (idx1+ dstbssidx))))
		     (cond
		       [(eq? srcformdm dstformdm) (send srcbdm move-to dstidx)] ;;; same form
		       [(nested-form? dstformdm srcformdm) ;;; pushing down
			(if (or (occurs-free-except-for? srcbvar srcformbody dstform)
				(memq srcbvar dstbvars))
			    (send text display-mini "illegal move down")
			    (send srcbdm move-to dstidx))
			]
		       [(nested-form? srcformdm dstformdm) ;;; lifting up
			(if (or (occurs-free-except-for? srcbvar dstformbody srcform)
				(memq srcbvar dstbvars))
			    (send text display-mini "illegal move up")
			    (send srcbdm move-to dstidx))
			]
		       [else ;;; lateral move
			(if (or (occurs-free-except-for? srcbvar srcformbody srcform)
				(memq srcbvar dstbvars))
			    (send text display-mini "illegal lateral move")
			    (send srcbdm move-to dstidx))
			])))))))]
	[else (not-ok)])
      (send-base self mouse-press text x y mods)]
    [mouse-enter (text x y mods)
      (send text display-mini mini-text)
      (case syncat
	[(varref-lexical)
	 (let ((v (assq expr env)))
	   (when v
	     (let ((dm (cdr v)))
	       (case (send dm get-syncat)
		 [(binding) (send (car (send dm get-exprdm)) enable-blinking)]
		 [(boundvar) (send dm enable-blinking)]
		 ))))]
	[(binding)
	 (unless (eq? self blink-binding)
	   (send self set-background-color! 'pink))]
	[(boundvar)
	 (unless (eq? (send self get-parent) blink-binding)
	   (send (send self get-parent) set-background-color! 'pink))]
	)
      (send-base self mouse-enter text x y mods)]
    [mouse-leave (text x y mods)
      (send text clear-mini)
      (case syncat
	[(varref-lexical)
	 (let ((v (assq expr env)))
	   (when v
	     (let ((dm (cdr v)))
	       (case (send dm get-syncat)
		 [(binding) (send (car (send dm get-exprdm)) disable-blinking)]
		 [(boundvar) (send dm disable-blinking)]
		 ))))]
	[(binding)
	 (unless (eq? self blink-binding)
	   (send self set-background-color! 'white))]
	[(boundvar)
	 (unless (eq? (send self get-parent) blink-binding)
	   (send (send self get-parent) set-background-color! 'white))]
	)
      (send-base self mouse-leave text x y mods)]
    [destroy ()
      (send self disable-blinking)
      (send-base self destroy)]
    [init ignore-args
      (set! gidx (1+ gidx))
      (set! idx gidx)
      (send-base self init)
      ]
    )
  )

(define make-pretty
  (lambda (str)
    (let ((ip (open-input-string str))
	  (op (open-output-string)))
      (parameterize ((print-level 2) (print-length 2))
	(pretty-print (read ip) op)
	(let ((ostr (get-output-string op)))
	  (substring ostr 0 (1- (string-length ostr))))))))
