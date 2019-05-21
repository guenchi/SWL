;; Copyright (c) 1996-1997 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


(assertion-violationf 'apps/design/design.ss "swl:make-application is replaced by new swl:begin-application")

(require "../common/scrollframe.ss")
(require "../common/warning-dialog.ss")
(require "../common/auxio.ss")
(require "../edit/edit-text.ss")
(require "auxread.ss" 'once)
(require "../common/popup.ss" 'once)
(require "../common/app.ss")
(require "vtool.ss" 'once)

(define gidx 0)
(define gdm #f)
(define blink-binding #f)

(require "dm.ss" 'once)


(define-swl-class (<design-text> parent) (<edit-text> parent)
  (ivars
    (design-markups '())
    (parsed? #f)
    (parsed-expr #f)
    (vtool-visible? #f)
    (vtool #f)
    (versions '()) ;;; versions is a vtree
    (curr-version #f)
    (curr-marker #f) ;;; used by vtool
    (mlock #f)
    (munlock #f)
    )
  (inherited handle end-exp)
  (inheritable)
  (private)
  (protected
    )
  (public
    [get-vtool () vtool]
    [get-design-markups () design-markups]
    [get-versions () versions]
    [get-curr-version () curr-version]
    [get-curr-marker () curr-marker]
    [get-parsed-expr () parsed-expr]
    
    [set-vtool! (v) (set! vtool v)]
    [set-lock-unlock! (mlk munlk)
     (set! mlock mlk)
     (set! munlock munlk)]
    [set-design-markups! (v) (set! design-markups v)]
    [set-versions! (v) (set! versions v)]
    [set-curr-version! (v) (set! curr-version v)]
    [set-curr-marker! (v) (set! curr-marker v)]
	
    [cons-design-markups! (v) (set! design-markups (cons v design-markups))]

    [init (parent) (send-base self init parent)]

    [load-file (name)
      (let ([end (send self get-end-mk)])
	(swl:block-read-file
	 name
	 (lambda (string) (send self raw-insert-at end string))))
      ]
    
    [add-version (tag)
      (let ((nv (make-version tag (send self get-string '(0 . 0) 'end))))
	(if curr-version
	    (set! versions (vtree-add-version curr-version nv versions))
	    (set! versions (make-vtree nv '())))
	(set! curr-version nv))
      (send vtool redraw)
      ]
    
    [unparse-expr ()
      (send self set-enabled! #t)
      (send mlock set-enabled! #t)
      (send munlock set-enabled! #f)
      (send self clear-mini)
      (for-each
	(lambda (mkup)
	  (send mkup remove-markup self '(0 . 0) 'end))
	(send self get-design-markups))
      (set! blink-binding #f)
      (send self set-design-markups! '())
      (set! parsed? #f)]
    
    [parse-expr ()
      (unless gdm (set! gdm (create <design-markup>)))
      (set! parsed? #t)
      (send self set-enabled! #f)
      (send mlock set-enabled! #f)
      (send munlock set-enabled! #t)
      (parse-it self)]
    
    [mouse-press (x y mods)
      (unless parsed? (send-base self mouse-press x y mods))]
    [mouse-release (x y mods)
      (unless parsed? (send-base self mouse-release x y mods))]
    [mouse-motion (x y mods)
      (unless parsed? (send-base self mouse-motion x y mods))]
    [toggle-vtool ()
      (set! vtool-visible? (not vtool-visible?))
      (if vtool-visible?
	  (begin 
	    (show (send vtool get-parent))
	    (show vtool)
	    (send vtool redraw))
	  (hide (send vtool get-parent)))
      ]
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-swl-class (<design-toplevel> load-name) (<app-toplevel>)
  (ivars)
  (inherited mini-buffer mini-search)
  (inheritable mini-buffer mini-search)
  (private)
  (protected)
  (public
   [init (load-name)
     (define-syntax command-menu
       (syntax-rules ()
	 ((_ (str1 str2 act) ...)
	  (create <menu> (list (command-menu-item str1 str2 act) ...)))))

     (define command-menu-item
       (lambda (str1 str2 act)
	 (create <command-menu-item> with
		 (title: (cons str1 str2))
		 (action: act))
	 ))

     (define command-menu-item-disabled
       (lambda (str1 str2 act)
	 (create <command-menu-item> with
		 (title: (cons str1 str2))
		 (enabled: #f)
		 (action: act))
	 ))

     (define make-backup-file
       (lambda (name)
	 (system (string-append "mv " name " " name ".backup"))))

     (send-base self init)
     (let* ((scrolled-frame
	     (create <scrollframe> self with
		     (default-vscroll: #t)
		     (sticky-hscroll: #t)
		     ))
; If we put this on a Tk menu and set it to a long file name,
; the menu will wrap onto two lines (very ugly) so instead,
; we'll set the window title to the document name.
;	    (filename-label
;	     (create <command-menu-item> with
;		     (title: "Unnamed File")
;		     (background-color: 'white)
;		     ))
	    (filename #f)       ; probably have same filename bugs to fix here as I fixed in the repl
	    (design
	     (create <design-text> scrolled-frame with
		     (background-color: 'white)
		     (wrap: 'none)
		     ))
	    (vtool
	     (let* ((top
		     (create <toplevel> with (title: "Design History")))
		    (vt (create <vtool> top with
				(background-color: 'white)
				(design: design))))
	       (pack vt (expand: #t) (fill: 'both))
	       (hide top)
	       vt))
	    ) ;;; widget bindings

       (send self notify-text design)

       (send self set-destroy-request-handler!
	     (lambda (self)
	       (let ([killit
		      (lambda ()
			(destroy (send vtool get-parent))
			#t)])
		 (if (send design buffer-modified?)
		     (let ([ans
			    (warning-dialog
			     self
			     "
     The edit buffer is not saved to disk.    
     Do you really want to quit?    
"
			     '(yes no cancel))])
		       (and (eq? ans 'yes) (killit)))
		     (killit)))))

       (letrec
	   ((buffer-modified-menu-item (create <command-menu-item>))
             
	    (new-action (lambda (item) (new-design)))

	    (paste-action
	     (lambda (item) (send design action-paste)))
             
	    (open-action
	     (lambda (item)
	       (send item set-enabled! #f)
	       (thread-fork
		(lambda ()
		  (let ((name (swl:file-dialog "Open" 'open
                                (default-dir: (current-directory))
                                (parent: self))))
		    (if name
			(begin
			  (set-title! self name)
			  (set! filename name)
			  (send save-menu-item set-enabled! #t)
			  (send design load-file name))
			(send item set-enabled! #t)
			))))))
	    (save-action
	     (lambda (item)
	       (make-backup-file filename)
	       (write-file-from-widget filename design)
	       (set-title! buffer-modified-menu-item "")
	       (send design set-buffer-to-safe!)
	       ))
	    (save-as-action
	     (lambda (item)
	       (thread-fork
		(lambda ()
		  (let ((name (swl:file-dialog "Save As" 'save
                                (default-dir: (current-directory))
                                (parent: self))))
		    (if name
			(begin
			  (set-title! self name)
			  (set! filename name)
			  (send save-menu-item set-enabled! #t)
			  (delete-file filename)
			  (write-file-from-widget filename design)
			  (set-title! buffer-modified-menu-item "")
			  (send design set-buffer-to-safe!)
			  #t)
			#f)
		    )))))
               
	    (new-menu-item
	     (command-menu-item "New"  "    M-n"
				(lambda (item) (new-action item))))

	    (open-menu-item
	     (command-menu-item "Open" "    M-o"
				(lambda (item) (open-action item))))

	    (save-menu-item
	     (command-menu-item-disabled "Save" "    M-s"
					 (lambda (item) (save-action item))))

	    (save-as-menu-item
	     (command-menu-item "Save as" "    M-a"
				(lambda (item) (save-as-action item))))

	    (quit-menu-item
	     (command-menu-item "Quit" "    M-q"
				(lambda (item) (send self destroy))))

	    (copy-menu-item
	     (command-menu-item "Copy"  "    M-c"
				(lambda (item) (send design action-copy))))

	    (cut-menu-item
	     (command-menu-item "Cut"   "    M-u"
				(lambda (item) (send design action-cut))))

	    (paste-menu-item
	     (command-menu-item-disabled "Paste" "    M-p"
					 (lambda (item) (paste-action item))))

	    (search-forward-menu-item
	     (command-menu-item "Search Forward"    "    C-s"
				(lambda (item) (send design search-forward))))

	    (search-backward-menu-item
	     (command-menu-item "Search Backward"   "    C-r"
				(lambda (item) (send design search-backward))))

	    ) ;;; action/menu-item bindings

	 (letrec ((orig-cursor (send design get-mouse-cursor))
		  (menu-file
		   (create <cascade-menu-item> with
			   (title: "File")
			   (menu: (create <menu>
					  (list new-menu-item open-menu-item
						save-menu-item save-as-menu-item
						quit-menu-item)))
			   ))
		  (menu-edit
		   (create <cascade-menu-item> with
			   (title: "Edit")
			   (menu: (create <menu>
					  (list
					   copy-menu-item
					   cut-menu-item
					   paste-menu-item
					   search-forward-menu-item
					   search-backward-menu-item
					   )))
			   ))
		  (menu-preferences 
		   (create <cascade-menu-item> with
			   (title: "Preferences")
			   (enabled: #f)))
		  (menu-lock
; Due to a bug in Tk 8.0.4, these command menu items will not
; function properly under unix when part of a menu used as a
; menubar (ie. menu: of a <toplevel>) as is the case here.
; I'm putting together a patch to tk8.0.4/library/menu.tcl
; that fixes this and plan to distribute it w/ SWL.
		   (create <command-menu-item> with
			   (title: "Lock")
			   (action:
			    (lambda (item)
			      (send design unparse-expr)
			      (let ((rt (real-time))
				    (good? (send design parse-expr)))
				(send design add-version good?)
				)
			      (send design set-mouse-cursor! 'left_ptr)
			      ))))
		  (menu-unlock
		   (create <command-menu-item> with
			   (title: "Unlock")
			   (enabled: #f)
			   (action:
			    (lambda (item)
			      (let ((rt (real-time)))
				(send design unparse-expr)
				)
			      (send design set-mouse-cursor! orig-cursor)
			      ))))
		  (menu-help
		   (create <cascade-menu-item> with
; leaving it to someone else to convert this to html
; as I did for repl and editor
			   (title: "Help")
			   (menu:
			    (make-menu
			     ("Key Bindings" list)
			     ("" list)
			     ("C- is the Control key; M- is the Meta key" list)
			     ("" list)
			     ("C-a        Move to beginning of line" list)
			     ("C-b        Move back one character" list)
			     ("C-d        Delete current character (or selection)" list)
			     ("C-e        Move to end of line" list)
			     ("C-f        Move forward one character" list)
			     ("C-h        Backspace and remove one character" list)
			     ("C-k        Delete rest of line" list)
			     ("C-n        Move to next line" list)
			     ("C-p        Move to prev line" list)
			     ("C-r        Search backward for string" list)
			     ("C-s        Search forward for string" list)
			     ("" list)
			     ("M-b        Box the enclosing (or adjacent) expression" list)
			     ("M-<        Move to beginning of buffer" list)
			     ("M->        Move to end of buffer" list)
			     ("" list)
			     ("" list)
			     ("1.3" list)
			     ))))
		  ) ;;; menu bindings
              
	   (send design set-menu-items!
		 buffer-modified-menu-item
		 new-menu-item open-menu-item
		 save-menu-item save-as-menu-item
		 quit-menu-item
		 copy-menu-item
		 cut-menu-item
		 paste-menu-item
		 #f ;;; outline-mode-menu-item
		 )

	   (pack scrolled-frame (expand: #t) (fill: 'both))
	   (pack design (expand: #t) (fill: 'both))
           (set-menu! self
             (create <menu>
               (list menu-file
                     menu-edit
                     menu-preferences
                     menu-lock
                     menu-unlock
                     menu-help
                     (swl:application-menu))))

	   (send design set-vtool! vtool)
	   (send design set-lock-unlock! menu-lock menu-unlock)
	   
	   (thread-fork-group
	    (lambda ()
	      (set-menu!
	       menu-preferences
	       (make-menu
		("Font..."
                 (lambda (item)
                   (swl:font-dialog self "Select a font for the editor"
                     (swl:font-families 'fixed)
                     '(-8 -10 -12 -14 -16 -18 -20 -22 -24 8 10 12 14 16 18 20 22 24)
                     '(bold normal)
                     (lambda () (send design get-font))
                     (lambda (fnt) (when fnt (send design set-font! fnt))))))
		("Toggles"
		 (let ((the-box
			(create <check-menu-item> with
				(title: "The Box")
				(action:
				 (lambda (item)
				   (send design show-the-box (send self get-selected))))))
		       (version-tool
			(create <check-menu-item> with
				(title: "Version Tool")
				(action:
				 (lambda (item)
				   (send design toggle-vtool))))))
		   (let ((menu (create <menu> (list the-box version-tool))))
		     menu)))
		))
	      (set-enabled! menu-preferences #t)))
	   (when load-name
	     (send design load-file load-name))
	   (set! mydesign design))))]
   ))


(define new-design
  (case-lambda
    [() (new-design #f)]
    [(load-name)
     (swl:make-application
      (lambda ()
	(create <design-toplevel> load-name with
		(title: "Design"))))
     (void)]))



;; parse-it: returns #t is parsing successful, else #f.
;;   sets the text-widget's design-markups instance variable
;;   as a side-effect.

;; within parse-it,
;;   a parser procedure takes:   env
;;   a parser procedure returns: expr, start, end, new env


(define parse-it
  (lambda (text-wgt)
    (let* ((str (send text-wgt get-string '(0 . 0) 'end))
	   (p (open-input-string str)))
      (call/cc
       (lambda (escape)	
	 (define offset 0)
	 (define pk-token
	   (lambda ()
	     (design:peek-token offset p)))
	 (define rd-token
	   (lambda ()
	     (mvlet ((type token start end) (design:read-token offset p))
	       (set! offset end)
	       (values type token start end))))
	 (define pk-type
	   (lambda ()
	     (mvlet ((type token start end) (design:peek-token 0 p))
	       type)))
	 (define type->color
	   (lambda (v)
	     (cond
	       [(number? v) 'omit]
	       [(boolean? v) 'omit]
	       [(char? v) 'omit]
	       [(symbol? v) 'omit]
	       [else 'omit])))
	 (define ofs->idx
	   (lambda (ofs) (send text-wgt offset->index ofs)))
	 (define perror
	   (lambda (who ofs fmt . args)
	     (let ((err (string-append "Parsing error: ("
				       (symbol->string who) ") "
				       (apply format (cons fmt args)))))
	       (send text-wgt display-mini err)
	       (let ((dm (create-dm #f #f)))
		 (send dm set-mini-text! err)
		 (send dm set-background-color! 'red)
		 (send dm apply-markup text-wgt (ofs->idx ofs) (ofs->idx (1+ ofs))))
  	       (escape #f))))
	 (define create-dm
	   (lambda (parent env)
	     (letrec ((dm (create <design-markup> with
				  (env: env)
				  (parent: parent)
				  (text: text-wgt)
				  )))
	       (send text-wgt cons-design-markups! dm)
	       dm)
	     ))
	 (define apply-dm
	   (lambda (dm syncat expr exprdm start end color)
	     (let ((holes (send dm get-holes))
		   (parent-dm (send dm get-parent)))
	       (send dm set-syncat! syncat)
	       (send dm set-mini-text! (symbol->string syncat))
	       (send dm set-expr! expr)
	       (send dm set-exprdm! exprdm)
	       (send dm set-sidx! (ofs->idx start))
	       (send dm set-eidx! (ofs->idx end))
	       (unless (eq? color 'white)
		 (send dm set-background-color! color))
	       (if (eq? color 'omit)
		   (when parent-dm
		     (when holes ;;; propagate holes of children to parent
		       (for-each
			(lambda (hole) (send parent-dm cons-hole! hole))
			holes)))
		   (begin
		     (send dm apply-markup text-wgt
			   (ofs->idx start) (ofs->idx end))
		     (when holes ;;; children have set the hole(s)
		       (for-each
			(lambda (hole)
			  (send dm remove-markup text-wgt
				(ofs->idx (car hole)) (ofs->idx (cdr hole))))
			holes))
		     (when parent-dm ;;; we become a hole in the parent
		       (send parent-dm cons-hole! (cons start end))))
		   ))))
	 (define p-return
	   (case-lambda
	     [(dm syncat expr exprdm start end env)
	      (p-return dm syncat expr exprdm start end env 'omit)]
	     [(dm syncat expr exprdm start end env color)
	      (apply-dm dm syncat expr exprdm start end color)
	      (values expr dm start end env)]))
	 (define parse-expr
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (pk-token))
	       (when (eof-object? token)
		 (perror 'parse-expr start "premature end of expression"))
	       (if (eq? type 'quote)
		   (parse-abbrevquote parent-dm env)
		   (case type
		     ((lparen lbrack) (parse-form parent-dm env))
		     ((atomic)
		      (if (symbol? token)
			  (let ((dm (create-dm parent-dm env)))
			    (rd-token)
			    (if (assq token env)
				(p-return dm 'varref-lexical token 'varref-dm
					  start end env 'yellow)
				(p-return dm 'varref token 'varref-dm
					  start end env)
				))
			  (parse-literal parent-dm env)))
		     (else (perror 'parse-expr start "bad token type: ~a" type)
			   ))))))
	 
	 (define parse-form
	   (lambda (parent-dm env)
	     (define f-lambda
	       (lambda (lstart dm env)
		 (mvlet ((_ ksubdm start end env) (parse-keyword dm env 'lambda))
		   (mvlet ((_ subdm start end env) (parse-lparen dm env))
		     (mvlet ((formals fsubdm start end env) (parse-formals* dm env))
		       (mvlet ((_ subdm start end env) (parse-rparen dm env))
			 (mvlet ((body bsubdm start end env) (parse-expr dm env))
			   (mvlet ((_ subdm start end env) (parse-rparen dm env))
			     (p-return dm 'lambda
				       `(lambda ,formals ,body) `(,ksubdm ,fsubdm ,bsubdm)
				       lstart end env)))))))))
	     (define f-letform
	       (lambda (kind lstart dm env)
		 (mvlet ((_ ksubdm start end env) (parse-keyword dm env kind))
		   (mvlet ((bindings bsubdms start end env) (parse-bindings dm env))
		     (mvlet ((body bsubdm start end env) (parse-expr dm env))
		       (mvlet ((_ subdm start end env) (parse-rparen dm env))
			 (p-return dm kind
				   `(let ,bindings ,body) `(,ksubdm ,bsubdms ,bsubdm)
				   lstart end env 'white)))))))
	     (define f-define
	       (lambda (lstart dm env)
		 (mvlet ((_ ksubdm start end env) (parse-keyword dm env 'define))
		   (mvlet ((var vsubdm start end env) (parse-boundvar dm env))
		     (mvlet ((expr esubdm start end env) (parse-expr dm env))
		       (mvlet ((_ subdm start end env) (parse-rparen dm env))
			 (p-return dm 'define
				   `(define ,var ,expr) `(,ksubdm ,vsubdm ,esubdm)
				   lstart end env)))))))
	     (define f-if
	       (lambda (lstart dm env)
		 (mvlet ((_ ksubdm start end env) (parse-keyword dm env 'if))
		   (mvlet ((e1 e1subdm start end env) (parse-expr dm env))
		     (mvlet ((e2 e2subdm start end env) (parse-expr dm env))
		       (if (memq (pk-type) '(rparen rbrack))
			   (mvlet ((_ subdm start end env) (parse-rparen dm env))
			     (p-return dm 'if `(if ,e1 ,e2) `(,ksubdm ,e1subdm ,e2subdm)
				       lstart end env))
			   (mvlet ((e3 e3subdm start end env) (parse-expr dm env))
			     (mvlet ((_ subdm start end env) (parse-rparen dm env))
			       (p-return dm 'if
					 `(if ,e1 ,e2 ,e3)
					 `(,ksubdm ,e1subdm ,e2subdm ,e3subdm)
					 lstart end env)))))))))
	     (define f-app
	       (lambda (lstart dm env)
		 (mvlet ((exprs esubdm start end env) (parse-expr+ dm env))
		   (mvlet ((_ subdm start end env) (parse-rparen dm env))
		     (p-return dm 'app
			       exprs esubdm
			       lstart end env)))))
	     (define f-quoteform
	       (lambda (lstart dm env)
		 (mvlet ((_ ksubdm start end env) (parse-keyword dm env 'quote))
		   (mvlet ((expr dsubdm start end env) (parse-datum dm env))
		     (mvlet ((_ subdm start end env) (parse-rparen parent-dm env))
		       (p-return dm 'quote `(quote ,expr) `(,ksubdm ,dsubdm)
				 lstart end env))))))

	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((type token lstart lend) (rd-token)) ;;; lparen
		 (mvlet ((type token start end) (pk-token))
		   (if (memq type '(rparen rbrack))
		       (begin ;;; empty list
			 (rd-token)
			 (p-return dm 'atom '() 'quote-dm lstart end env))
		       (begin
			 (case token 
			   [(lambda) (f-lambda lstart dm env)]
			   [(let)    (f-letform 'let lstart dm env)]
			   ;; semantics not in place for these yet
			   ;; [(let*)   (f-letform 'let* lstart dm env)]  
			   ;; [(letrec) (f-letform 'letrec lstart dm env)]
			   [(define) (f-define lstart dm env)]
			   [(if)     (f-if lstart dm env)]
			   [(quote)  (if (eq? type 'atomic)
					 (f-quoteform lstart dm env)
					 (perror 'parse-form start
						 "invalid context for abbrevquote"))]
			   [else     (f-app lstart dm env)]))))))))
	  
	 (define parse-abbrevquote
	   (lambda (parent-dm env)
	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((_ ksubdm qstart end env) (parse-keyword dm env 'quote))
		 (mvlet ((expr dsubdm start end env) (parse-datum dm env))
		   (p-return dm 'quote `(quote ,expr) `(,ksubdm ,dsubdm)
			     qstart end env))))))
	 (define parse-binding
	   (lambda (parent-dm env)
	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((_ subdm lstart end env) (parse-lparen dm env))
		 (mvlet ((bvar bvsubdm start end env) (parse-boundvar dm env))
		   (mvlet ((expr esubdm start end env) (parse-expr dm env))
		     (mvlet ((_ subdm start end env) (parse-rparen dm env))
		       (let ((binddm `(,bvsubdm ,esubdm)))
			 (p-return dm 'binding `(,bvar ,expr) binddm
				   lstart end (cons (cons bvar dm) env) 'white)))))))))
	 (define parse-bindings
	   (lambda (parent-dm env)
	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((_ subdm lstart end env) (parse-lparen dm env))
		 (mvlet ((bindings bsubdms start end env) (parse-bindings* dm env))
		   (mvlet ((_ subdm start end env) (parse-rparen dm env))
		     (p-return dm 'bindings bindings bsubdms lstart end env)))))))
	 (define parse-bindings*
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (pk-token))
	       (if (memq type '(rparen rbrack))
		   (values '() '() start end env)
		   (mvlet ((binding bsubdm start end env) (parse-binding parent-dm env))
		     (mvlet ((bindings bsubdms start end env) (parse-bindings* parent-dm env))
		       (values (cons binding bindings) (cons bsubdm bsubdms)
			       start end env)))))))
	 (define parse-formals*
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (pk-token))
	       (if (memq type '(rparen rbrack))
		   (values '() '() start end env)
		   (mvlet ((formal fsubdm start end env) (parse-boundvar parent-dm env))
		     (mvlet ((formals fsubdms start end env) (parse-formals* parent-dm env))
		       (values (cons formal formals) (cons fsubdm fsubdms)
			       start end (cons (cons formal fsubdm) env))))))))
	 (define parse-body*
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (pk-token))
	       (if (memq type '(rparen rbrack))
		   (values '() '() start end env)
		   (mvlet ((expr subdm start end env) (parse-expr parent-dm env))
		     (mvlet ((exprs subdms start end env) (parse-body* parent-dm env))
		       (values (cons expr exprs) (cons subdm subdms)
			       start end env)))))))
	 (define parse-expr+
	   (lambda (parent-dm env)
	     (mvlet ((expr subdm start end env) (parse-expr parent-dm env))
	       (mvlet ((type token start end) (pk-token))
		 (if (memq type '(rparen rbrack))
		     (values (list expr) (list subdm) start end env)
		     (mvlet ((exprs subdms start end env) (parse-expr+ parent-dm env))
		       (values (cons expr exprs) (cons subdm subdms)
			       start end env)))))))
	 (define parse-literal ;;; sym or num or char or bool or nil
	   (lambda (parent-dm env)
	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((type token lstart end) (rd-token))
		 (cond
		   [(and (eq? type 'atomic)
			 (or (number? token) (char? token)
			     (boolean? token) (symbol? token)))
		    (p-return dm 'literal token 'literal-dm lstart end env
			      (type->color token))]
		   [else (perror 'parse-literal lstart "literal expected: ~a" token)])))))
	 (define parse-boundvar
	   (lambda (parent-dm env)
	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((type token start end) (rd-token))
		 (if (and (eq? type 'atomic) (symbol? token))
		     (p-return dm 'boundvar token 'boundvar-dm start end env 'tan)
		     (perror 'parse-boundvar start "symbol expected: ~a" token))))))
	 (define parse-datum
	   (lambda (parent-dm env)
	     (let ((dm (create-dm parent-dm env)))
	       (mvlet ((type token lstart end) (rd-token))
		 (cond
		   [(eq? type 'atomic) ;;; literal or symbol?
		    (p-return dm 'datum token 'datum-dm lstart end env)]
		   [(memq type '(lparen lbrack)) ;;; ( datum ... )?
		    (mvlet ((type token start end) (pk-token))
		      (if (memq type '(rparen rbrack))
			  (mvlet ((_ subdm start end env) (parse-rparen dm env))
			    (p-return dm 'datum '() 'datum-dm lstart end env))
			  (mvlet ((datum datumdm start end env) (parse-datum dm env))
			    (mvlet ((datums datumsdm start end env) (parse-datum* dm env))
			      (mvlet ((type token start end) (pk-token))
				(cond [(memq type '(rparen rbrack))
				       (mvlet ((type token start end) (rd-token))
					 (p-return dm 'datum
						   (cons datum datums)
						   (cons datumdm datumsdm)
						   lstart end env))]
				      [(eq? type 'dot)
				       (mvlet ((type token start end) (rd-token))
					 (mvlet ((dotdatum dotdatumdm start end env)
						 (parse-datum dm env))
					   (mvlet ((_ subdm start end env) (parse-rparen dm env))
					     (p-return dm 'datum
						       (cons (cons datum datums) dotdatum)
						       (cons (cons datumdm datumsdm) dotdatumdm)
						       lstart end env))))]
				      [else (perror 'parse-datum start
						    "datum expected: ~a" token)]
				      ))))))]
		   [else (perror 'parse-datum lstart "datum expected: ~a" token)])))))
	 (define parse-datum*
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (pk-token))
	       (if (memq type '(dot rparen rbrack))
		   (values '() '() start end env)
		   (mvlet ((datum datumdm start end env) (parse-datum parent-dm env))
		     (mvlet ((datums datumdms start end env) (parse-datum* parent-dm env))
		       (values (cons datum datums) (cons datumdm datumdms)
			       start end env)))))))
	 (define parse-keyword
	   (lambda (parent-dm env keyword)
	     (mvlet ((type token start end) (rd-token))
	       (when (not (eq? token keyword))
		 (perror 'parse-keyword start
			 "keyword expected: ~a got: ~a" keyword token))
	       (values token 'keyword-dm start end env))))
	 (define parse-lparen
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (rd-token))
	       (when (not (memq type '(lparen lbrack)))
		 (perror 'parse-lparen start "lparen expected: ~a" token))
	       (values 'lparen 'lparen-dm start end env))))
	 (define parse-rparen
	   (lambda (parent-dm env)
	     (mvlet ((type token start end) (rd-token))
	       (when (not (memq type '(rparen rbrack)))
		 (perror 'parse-rparen start "rparen expected: ~a" token))
	       (values 'rparen 'rparen-dm start end env))))
  
	 (parse-expr #f '())
	 #t
	 )))))




(current-expand eps-expand)
(require "syncase.ss" 'once)

(define occurs-free-except-for?
  (lambda (var expr except)
    (if (eq? expr except)
      #f
      (syncase expr
	(keywords let lambda if quote define)
	[(constant datum) #f]
	[(variable v) (eq? var v)]
	[(let ((bvar bexpr) ..) body)
	 (or (ormap (lambda (e) (occurs-free-except-for? var e except)) bexpr..)
	     (and (not (memq var bvar..))
	          (occurs-free-except-for? var body except)))]
	[(lambda (formal ..) body)
	 (if (memq var formal..)
	   #f
	   (occurs-free-except-for? var body except))]
	[(if e1 e2)
	 (or (occurs-free-except-for? var e1 except)
	     (occurs-free-except-for? var e2 except))]
	[(if e1 e2 e3)
	 (or (occurs-free-except-for? var e1 except)
	     (occurs-free-except-for? var e2 except)
	     (occurs-free-except-for? var e3 except))]
	[(quote e) #f]
	[(define v e) (occurs-free-except-for? var e except)]
	[(e ..) (ormap (lambda (e) (occurs-free-except-for? var e except))
		  e..)]
	[else (assertion-violationf 'occurs-free-except-for? "invalid syntax: ~a~n" expr)]))))
	
(current-expand sc-expand)

#!eof

Expr     -> Form  | Literal | Varref | AbbrevQuote
Form     -> ( Lambda | ( Define | ( Let<*> | ( If | ( QuoteForm | ( App | ()
  Lambda   -> lambda ( Formals* ) Expr ) ;;; no sequences
  Define   -> define Var Expr )
  Let      -> let Bindings Expr ) ;;; no sequences
  LetStar  -> let* Bindings Expr ) ;;; no sequences
  Letrec   -> letrec Bindings Expr ) ;;; no sequences
  If       -> if Expr1 Expr2 Expr3 ) | ( if Expr1 Expr2 )
  QuoteForm-> quote Datum )
  App      -> Expr+ )
Expr+    -> Expr | Expr Expr+    
AbbrevQuote-> ' Datum
Bindings -> ( Bindings* )
Bindings*-> <epsilon> | Binding Bindings*
Binding  -> ( Var Expr )
Formals* -> <epsilon> | Var Formals*
Body*    -> <epsilon> | Expr Body* ;;; unused in pure functional subset
Datum    -> Literal | Sym | ( Datum* ) | ( Datum Datum* . Datum )
Datum*   -> <epsilon> | Datum Datum*
Literal  -> num | char 




every design markup points to its parent

every design markup shows its "type" in the minibuf.





hier
----

base1        base
base1        tk-object
base2        tk-prewidget    init ignore-args
base2        tk-prewidget2
canvasitem   canvas-item
canvasitem   2pt-figure
canvasitem   oval


   tk-prewidget2 (#f for handle)
   canvas-item
   multi-pt-figure
   line
