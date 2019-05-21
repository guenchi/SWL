;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define verb
  (lambda args
    (apply printf args)
    (newline)
    (flush-output-port)))

(simplemodule (exports vinspect letsdoit)


(define get-help-strings
  (lambda ()
    (let ([hs (let ([ip (open-input-file
;		  "/u/ehilsdal/swl/apps/inspector/help.txt")])
			  "/dev/null")])
		(let loop ()
		  (let ([name (read ip)])
		    (if (eof-object? name)
			(begin (close-input-port ip) '())
			(let ([text (read ip)])
			  (if (eof-object? text)
			      (assertion-violationf 'get-help-strings
				"malformed help.txt file")
			      (cons (list name text) (loop))))))))])
      (set! get-help-strings (lambda () hs))
      hs)))

(define make-help-menu
  (lambda (inspect-window)
    (let ([hs (get-help-strings)])
      (make <menu>
	(simple-menu-list->menu-items
	  (map (lambda (item)
		 (cons (car item)
		   (lambda (menu-item)
		     (send inspect-window show-text (car item) (cadr item)))))
	    hs))))))

;; ---- See the end of the file for tests

; An <inspector-state> contains all the information which the
; inspector should display at any given time.  This doesn't include
; information for the Go and Back buttons, though, so it may be
; misnamed.  Contained is a string representing the type of the
; currently displayed object, and a list of <text-state>s
; corresponding to the inspector's text windows. 
; We also need the real object.

;; an inspector-state can have a bunch of methods.
;;   (destroy) -- kills self, and any running thread
;;   (set-depth! d)
;;   (get-depth)
;;   (set-level! l)
;;   (get-level)
;;   (refresh)

;; Inspector-state is a subclass of frame for no good reason I can
;; fathom, apart from needing to catch the destroy method before it is
;; handed to the text-widget inside.  

(define-svelte-class (<inspector-state> parent i-object ge i-lev i-len)
  (<frame> parent)
  (ivars [parent parent] [i-object i-object] [ge ge] [i-lev i-lev]
    [i-len i-len] [menustr #f] [typestr #f]
    [text-state #f] [pcb #f])
  (private
    [update-me ()
      (send (send text-state get-text) delete-all)
      (mv-let ([(ps0 ps1 ps1size) (send i-object get-inspector-state
			    parent ge i-lev i-len)]
	       [(t0) (send text-state get-text)])
	(mv-let ([(str0 m0) (flatten-ps ps0)]
		 [(str1 m1) (if ps1 (flatten-ps ps1)
				(values #f '()))])
	  (send t0 insert str0)
	  (send t0 make-visible '(0 . 0))
	  (if str1 
	      (let ([t1 (send text-state get-sub ps1size)])
		(send t1 delete-all)
		(send t1 insert str1)
		(send t1 make-visible '(0 . 0))
		(apply-deep-ms m1 t1))	; thread bug! move me
	      (send text-state remove-sub))
	  (swl:sync-display)
	  (thread-yield)
	  (thread-quantum 100)
	  (apply-deep-ms m0 t0)
	  ))])
  (public
    [init args
      (send-base self init parent)
      (set! menustr
	(mv-let ([(str ms)
		  (flatten-ps (flat-half-pprint (send i-object value)
				'ignored 40))])
	  str))
      (set! typestr (send i-object type-string))
      (set! text-state (create <twoscrolltext> self parent))
      (set-option! self (border-width: 0))
      (set! pcb (thread-fork (lambda () (update-me)) 5000))
      (grid text-state (row: 0) (column: 0) (sticky: 'nsew))]
    [refresh ()
      (thread-kill pcb)
      (set! pcb (thread-fork (lambda () (update-me)) 5000))]
    [destroy ()
      (thread-kill pcb)
      (send-base self destroy)]
    [get-i-object () i-object]
    [get-object () (send i-object value)]
    [get-menustr () menustr]
    [get-typestr () typestr]
    [get-text-state () text-state]
    [set-font! (fn)
      (send text-state set-font! fn)]))

(define-svelte-class (<text-inspector-state> parent name str)
  (<frame> parent)
  (ivars [parent parent] [name name] [str str] [menustr #f] [text-state #f])
  (public
    [init (parent name str)
      (send-base self init parent)
      (set! menustr name)
      (set! text-state (create <twoscrolltext> self parent))
      (let ([t0 (send text-state get-text)])
	(send t0 insert str)
	(send t0 make-visible '(0 . 0)))
      (set-option! self (border-width: 0))
      (grid text-state (row: 0) (column: 0) (sticky: 'nsew))]
    [refresh () (void)]
    [get-object () (gensym)]
    [get-menustr () menustr]
    [get-typestr () name]
    [get-text-state () text-state]
    [set-font! (fn)
      (send text-state set-font! fn)]))

;; ---- <inspectwin>

(define-svelte-class (<inspect-window> obj) (<toplevel>)
  (ivars
    (current-font (inspector-font))

    (stack '()) (pseudo-stack '())
    (ge (lambda args #f))
    (i-lev (inspect-level))
    (i-len (inspect-length))
    (go-mb #f)
    (back-b #f) (forward-b #f) (refresh-b #f) (type-lab #f)
    (extra-bs '())
    
    (quit-register #f)
    (curr-state #f))
  (private
    [spawn-self ()
      (unless (isa? (car pseudo-stack) <text-inspector-state>)
	(parameterize ([inspect-level i-lev]
		       [inspect-length i-len]
		       [inspector-font current-font])
	  (create <inspect-window>
	    (send (car pseudo-stack) get-object))))]
    [update-buttons ()
      (set-option! forward-b (enabled: (not (eq? stack pseudo-stack))))
      (set-option! back-b (enabled: (not (null? (cdr pseudo-stack)))))
      (for-each (lambda (x) (send x destroy)) extra-bs)
      (set! extra-bs '())
      (unless (isa? (car pseudo-stack) <text-inspector-state>)
	(let ([obj (send (car pseudo-stack) get-i-object)])
	  (for-each (lambda (ls)
		      (apply (lambda (pred label fun)
			       (when (pred obj)
				 (push-onto! extra-bs
				   (create <button> self with
				     (title: label)
				     (action:
				       (lambda (button)
					 (send self show-object
					   (fun obj))))))))
			ls))
	    (inspector-extra-buttons)))
	(let loop ((ls extra-bs) (n 3))
	  (unless (null? ls)
	    (grid (car ls) (row: 1) (column: n) (sticky: 'nsw))
	    (loop (cdr ls) (+ n 1)))))]
    [push! (insp-state)
      (set! pseudo-stack (cons insp-state pseudo-stack))
      (set! stack pseudo-stack)]
    [display-state (inspect-state)
      (let ([type-title (send inspect-state get-typestr)]
	    [text-state (send inspect-state get-text-state)])
	(when curr-state (hide curr-state))
	(set! curr-state inspect-state)
	(set-option! type-lab (title: type-title))
	(grid curr-state (row: 4) (column: 0) (column-span: 5) (sticky: 'nsew))
	(swl:sync-display))]
    [update-menu ()
      (set-option! go-mb
	(menu:
	  (create <menu>
	    (simple-menu-list->menu-items
	      (maplist (lambda (stack)
			 (cons
			   (send (car stack) get-menustr)
			   (lambda args
			     (set! pseudo-stack stack)
			     (update-buttons)
			     (display-state (car stack)))))
		stack)))))]
    [update-my-font ()
      (for-each (lambda (state)
		  (send state set-font! current-font))
	stack)])
  (public
    [set-quit-register! (proc)
      (set! quit-register proc)]
    [key-signal (sym)
      (case sym
	[quit (send self destroy)]
	[left (send back-b invoke)]
	[right (send forward-b invoke)])]
    [destroy ()
      (when quit-register (quit-register))
      (send-base self destroy)]
    [show-text (label str)
      (let ([inspect-state
	      (create <text-inspector-state> self label str
		with (font: current-font))])
	(push! inspect-state)
	(update-buttons)
	(display-state inspect-state)
	(update-menu))] 
    [reset-and-show (object)
      (set! pseudo-stack '())
      (set! stack '())
      (send self show-object (make-inspect-object object))]
    [show-object (inspect-object)
      (unless (and (not (null? pseudo-stack))
		   (eq? (send inspect-object value)
		     (send (car pseudo-stack) get-object)))
	(let ([inspect-state
		(create <inspector-state> self inspect-object ge i-lev i-len
		  with (font: current-font))])
	  (push! inspect-state)
	  (update-buttons)
	  (display-state inspect-state)
	  (update-menu)))]
    [init (obj)
      (send-base self init)
      (set-option! self (title: "Inspectscape"))
      (set! go-mb (create <cascade-menu-item> with (title: "Go")))
      (set-menu! self
        (create <menu>
          (list
            (create <cascade-menu-item> with
              (title: "File")
              (menu:
                (make-menu
                  ("Quit" (lambda (menu-item) (send self destroy)))
                  ("Spawn" (lambda (menu-item) (spawn-self))))))
            go-mb
; Due to Tk 8.0.4 bug on unix, a command-menu-item in the menubar
; of a <toplevel> can't be invoked.  I'm supplying a patch with SWL
; to fix this.
            (create <command-menu-item> with
              (title: "Font")
              (action:
                (lambda (item)
                  (swl:font-dialog self
                    "Select a font for the inspector"
                    (swl:font-families 'fixed)
                    '(-8 -10 -12 -14 -16 -18 -20 -22 -24 8 10 12 14 16 18 20 22 24)
                    '(bold normal)
                    (lambda () current-font)
                    (lambda (fnt)
                      (when fnt (set! current-font fnt) (update-my-font)))))))
            (swl:help-menu)
            (swl:application-menu))))

      (set! back-b
	(create <button> self with (title: "<")
	  (action: (lambda (button)
		     (set! pseudo-stack (cdr pseudo-stack))
		     (update-buttons)
		     (display-state (car pseudo-stack))))))
      (set! forward-b
	(create <button> self with (title: ">")
	  (action: (lambda (button)
		     (set! pseudo-stack
		       (let loop ([curr stack] [next (cdr stack)])
			 (if (eq? next pseudo-stack) curr
			     (loop next (cdr next)))))
		     (update-buttons)
		     (display-state (car pseudo-stack))))))
      (set! refresh-b
	(create <button> self with (title: "R")
	  (action: (lambda (button) (send (car pseudo-stack) refresh)))))
      (set! type-lab (create <label> self))
      (grid back-b (row: 1) (column: 0) (sticky: 'w))
      (grid forward-b (row: 1) (column: 1) (sticky: 'w))
      (grid refresh-b (row: 1) (column: 2) (sticky: 'w))
      (grid type-lab (row: 2) (column: 1) (column-span: 4) (sticky: 'w))
      (grid (create <frame> self with
	      (height: 6) (relief: 'sunken) (border-width: 2))
	(row: 3) (column: 0) (column-span: 5) (sticky: 'new))
      (send self show-object (make-inspect-object obj))]))

;; ---- toplevel and tests

(define vinspect
  (lambda (object)
    (swl:begin-application
      (lambda (token)
        (create <inspect-window> object with
          (destroy-request-handler:
            (lambda (self) (swl:end-application token) #t))))
      void)
    (void)))

(assertion-violationf 'apps/inspect/top.ss "propagate the swl:begin-application change to the letsdoit code --- swl:make-application is no more")

(define letsdoit
  (let ([old-eh (error-handler)]
	[viewer #f])
    (case-lambda
      [()
       (error-handler
	 (lambda args
	   (call/cc
	     (lambda (k)
	       (swl:make-application
		 (lambda ()
		   (if viewer
		       (send viewer reset-and-show k)
		       (set! viewer
			 (create <inspect-window> k
			   with (quit-register:
				  (lambda () (set! viewer #f)))))))
		 5000)
	       (apply old-eh args)))))]
      [(ignored)
       (error-handler old-eh)])))

  ) ; end simplemodule

(swl:help-menu "for Inspector"
  (lambda ()
    (let ([root (getenv "SWL_ROOT")])
      (if (not root)
          (warning-dialog #f
            "Can't locate documentation.  (SWL_ROOT environment variable not set)"
            'oops)
          (new-viewer (string-append root "/inspect.html"))))))

(define vinspect-test
  (lambda ()
    (let ((nasty
	    '#(asdfasdf #&(zinger (bin . 88) . #f) (((aa ())))))

	  (really-nasty
	    '#8=#(asdfasdf #8#
		   #&#9=(zinger . #10=((bin #9# . 88) . #f)) (((aa #10# ())))))

	  (super-nasty
	    '#0=(0 #1=(1 #2=(2 #3=(3 #4=(4 #5=(5 #6=(6 #7=(7 #0# #1# #2# #3# #4# #5# #6# #7#))))))))))
      (vinspect nasty)
      (prompt-read "enter something to continue: ")
      (vinspect really-nasty)
      (prompt-read "enter something to continue: ")
      (vinspect super-nasty))))

(simplemodule (exports vdebug)
;  (define continuation-link
;    (parameterize ([subset-mode 'system])
;      (eval '\#continuation-link)))

  ; oops... apparently #continuation-link doesn't split the continuation.
  (define continuation-link
    (lambda (c)
      (((inspect/object c) 'link) 'value)))

  (define vdebug
    (lambda ()
      (let ([le (last-exception)])
	(if (not le)
	    (warningf 'vdebug "Nothing to debug")
	    (vinspect (continuation-link (thread-exception->k le)))))))
  )

(display
  "
There are two top-level procedures which are (somewhat) useful:

(vinspect <thing>) -- pops up an inspector window viewing <thing>  
(vdebug)           -- pops up an inspector window viewing the last 
		      error continuation in this thread.
(letsdoit)	   -- yeah
")
