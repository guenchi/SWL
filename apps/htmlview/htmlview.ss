;;
;; Copyright (c) 1996-1998 Carl Bruggeman & John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notes:
;;
;; Markup application must be delayed until after the corresponding
;; (marked up) text has been flushed.  This is done by creating an
;; 'mproc' for each markup. An mproc is a thunk that when called after
;; the corresponding text has been flushed, applies the markup to the text.
;;
;; Mprocs are also useful in handling tables, as they may be reapplied
;; with the live widget, after first being applied to the hidden widget.
;;
;;
;; Todo
;;
;; DONE. consider: moving <edit-text> search code into new <app-text> class
;; (renamed from <help-text>).  This would provide same search to
;; repl, editor, and html-viewer.
;; 
;; http://scheme.com/tspl2d/tspl.html
;; DONE. get images working.
;;
;; get web page materials from Mark Leone before May 1.
;;
;; think about text searching across multiple files.  Use index or
;; raw file computed off-line?
;;
;; DONE. double clicking on keywords, should work in repl also.
;;       use list of keyboard -> list of urls for this.
;;
;; DONE. tables: draw 50 lines, then render.  
;;
;; are there anchor attributes for color and underline?  if so, can
;; provide anchors for all keywords, identifiers, etc. invisibly.
;;
;; DONE. don't literally render whitespace (i.e., some pars are indented
;; which shouldn't be).
;;
;; bump-nesting-level stuff is very kludgey.  
;;

(require "../common/scrollframe.ss" 'once)
(require "../common/flex-button.ss" 'once)
(require "../common/arrows.ss" 'once)
(require "../common/app.ss" 'once)
(require "html.ss")
(require "html-text.ss")
(require "www.ss")


(define-swl-class (<html-toplevel> name) (<app-toplevel>)
  (ivars
   [help-thread #f]
   [help-data #f]
   [scrollframe #f]
   [null-stop #f]
   [font #f]
   [tx #f]
   [url-cache '()]
   [url-history '()]
   [url-history-pointer -1]
   [viewer-thread #f]
   [viewer-title #f]
   [menu-go #f]
   [curr-cvalue #f]
   )
  (inherited mini-buffer mini-search)
  (inheritable mini-buffer mini-search)
  (private)
  (protected)
  (public
   [set-viewer-title! (ttl)
    (when ttl
      (set! viewer-title ttl)
      (send self set-title! (format "HTML Viewer: ~a" ttl)))]
   [destroy ()
     (when help-thread
       (thread-kill help-thread)
       (set! swl:help-viewer-queue #f))
     (send-base self destroy)
     ]
   [init (name) ;;; name can be a url string, or the symbol help.
     (define-syntax simple-menu-item
       (syntax-rules ()
	 ((_ str1 str2 act (argname argval) ...)
	  (create <command-menu-item> with
		  (title: (cons str1 str2))
		  (action: act)
		  (argname argval) ...
		  ))))
     (send-base self init)
     (set! scrollframe
       (create <scrollframe> self with
	       (default-vscroll: #t) (sticky-hscroll: #t)))
     (let*
	 ((menu-control 
	   (create <cascade-menu-item> with
		   (title: "_File")
		   (menu:
		    (create
		     <menu>
		     (list
		      (simple-menu-item
		       "_Close"    "Alt+q"
		       (lambda (item) (send self destroy)))
		      )))))
;	  (menu-preferences 
;	   (create <cascade-menu-item> with
;		   (title: "Preferences")
;		   (enabled: #f)))
;	  (label-title
;	   (create <command-menu-item> with
;		   (background-color: 'white)
;		   (title: "")))
	  (menu-h-backward
	   (create <command-menu-item> with
		   (title:
; Due to Tk 8.0.4 bug, menubar items cannot display images or bitmaps
; under Windows 95.  So we punt here to "<".
; Due to Tk 8.0.4 bug, menubar command items cannot be invoked under
; unix.  I'm providing a patch with SWL that fixes this.
                     "<")
		   (font: (create <font> 'courier 12 '(bold)))
		   (action: (lambda (button) (send self history-backward)))))
	  (m-go
	   (create <cascade-menu-item> with
		   (title: "_Go")))
	  (menu-h-forward
	   (create <command-menu-item> with
		   ;; (title: ">")
		   (title: ">")
		   (font: (create <font> 'courier 12 '(bold)))
		   (action: (lambda (button) (send self history-forward)))))
	  ) ;;; menu bindings

       (set! menu-go m-go)
       (set-menu! self
         (create <menu>
           (list menu-control
;                menu-preferences
                 menu-h-backward
                 menu-go
                 menu-h-forward
                 (swl:application-menu))))
       (set-border-width! (get-menu self) 0)
       (set-relief! (get-menu self) 'flat)
       
     (set! null-stop (make <tab-stop> 0 'left))
     (set! font (create <font> 'times 14 '(normal roman)))

     (pack scrollframe (expand: #t) (fill: 'both))
     (if (eq? name 'help) ;;; create the help viewer
         (let ((base-url #f))
           (send self set-title! "Help")
           (when swl:help-viewer-queue
             (assertion-violationf '<htmlview-toplevel> "help queue already exists"))
           (set! swl:help-viewer-queue (thread-make-msg-queue 'htmlview-help))
           (let ((swllib (getenv "SWL_LIBRARY")))
             (unless swllib
               (assertion-violationf '<htmlview-toplevel> "SWL_LIBRARY not set"))
             (set! base-url (parse-url swllib)))
           (set! help-thread
             (thread-fork
              (lambda ()
                (let f ((msg (thread-receive-msg swl:help-viewer-queue)))
                  (send self raise)
                  (send self view-url
                        (merge-url base-url (parse-url msg)))
                  (f (thread-receive-msg swl:help-viewer-queue)))))))
         (begin
           (send self set-title! "HTML Viewer")
           (send self view-url
                 (merge-url (parse-url (string-append (current-directory) "/"))
                            (parse-url name)))))
)
     ]

   [view-url (url)
;;;(printf "view-url path=~s loc=~s~n" (url-path url) (url-loc url))
     (when viewer-thread
       (critical-section
	 (when viewer-thread (thread-kill viewer-thread))))
     (send self history-update-view)
     (let ((cache-hit (assoc (url-path url) url-cache)))
       (if cache-hit
	   (let* ((cvalue (cdr cache-hit))
		  (wgt (car cvalue))
		  (ttl (cadr cvalue))
		  )
	     (let ((ncvalue (list wgt ttl)))
	       (send self view-cached-url ncvalue #f url)
	       (send self history-add curr-cvalue (url-path url))))
	   (send self view-new-url url)))
     ]

   [view-new-url (url)
     (set! viewer-thread
       (thread-fork
	(lambda ()
	  (thread-name "HTML viewer")
	  (set! tx (create <html-text> scrollframe with
			   (wrap: 'word)
			   (background-color: 'white)
			   (font: font)
			   (width/char: 100)
			   (tabs: (list null-stop))))
	  (send self notify-text tx)
	  (send tx prep-visible self url font)
	  (send scrollframe make-visible tx)
	  (set-focus tx)
	  (send tx display-mini "Loading . . .")
	  (let ((ip (www-open-url url)))
	    (if (port? ip)
		(send tx read-and-display-html ip)
		(assertion-violationf 'view-url "www-open-url returned: ~s [url=~s]~n"
		       ip url)))
	  (when (url-loc url) (send tx goto-anchor (url-loc url)))
	  (send tx display-mini "Loading . . . done.")
	  (set-cursor-pos! tx '(0 . 0))  ; so that searches start at top
	  (let ((cvalue (list tx viewer-title)))
	    (set! url-cache (cons (cons (url-path url) cvalue) url-cache))
	    (set! curr-cvalue cvalue)
	    (send self history-add curr-cvalue (url-path url))
	    ))))]
   
   [view-cached-url (cvalue view url)
     (send tx turn-search-off)
     (let* ((wgt (car cvalue))
	    (ttl (cadr cvalue))
	    )
       (set! tx wgt)
       (pack tx (expand: #t) (fill: 'both))
       (when ttl (send self set-viewer-title! ttl))
       (send scrollframe make-visible tx)
       (if view
	   (send tx vscroll (car view) 'fraction)
	   (when (url-loc url) (send tx goto-anchor (url-loc url))))
       (send tx set-focus))
     (set! curr-cvalue cvalue)
     ]

   [history-add (cvalue path)
     
     (critical-section
       (set! url-history 
	 (let f ((ptr url-history-pointer)
		 (his url-history))
	   (if (positive? ptr)
	       (f (1- ptr) (cdr his))
	       his)))
       (set! url-history-pointer 0)
       (set! url-history
	 (cons (cons path (list cvalue #f)) url-history)))
     (send self history-update-view)
     (send self history-rebuild-menu)
     ]

   [history-rebuild-menu ()
     (define-syntax simple-menu-item
       (syntax-rules ()
	 ((_ str act (argname argval) ...)
	  (create <check-menu-item> with
		  (title: (or str "<untitled>"))
		  (action: act)
		  (argname argval) ...
		  ))))
     (let ((marked-menu-item #f))
       (set-menu! menu-go
	 (create <menu>
	   (let f ((h 0) (his url-history) (acc '()))
	     (if (null? his)
		 acc
		 (let ((smi (simple-menu-item
			     (cadr (cadr (car his))) ;;; title
			     (lambda (item)
			       (send self history-update-view)
			       (set! url-history-pointer h)
			       (send marked-menu-item set-draw-indicator! #f)
			       (send item set-draw-indicator! #t)
			       (set! marked-menu-item item)
			       (send self view-cached-url
				     (cadr (list-ref url-history h))
				     (caddr (list-ref url-history h))
				     #f))
			     (draw-indicator: (eqv? h url-history-pointer))
			     (select-color: 'lightgray))))
		   (when (eqv? h url-history-pointer)
		     (set! marked-menu-item smi))
		   (f (1+ h) (cdr his) (cons smi acc))))))))
       ]

   [history-forward ()
     (when (positive? url-history-pointer)
       (send self history-update-view)
       (set! url-history-pointer (1- url-history-pointer))
       (send self history-rebuild-menu)
       (let ((his (list-ref url-history url-history-pointer)))
	 (send self view-cached-url (cadr his) (caddr his) #f)))
     ]

   [history-backward ()
     (when (< url-history-pointer (1- (length url-history)))
       (send self history-update-view)
       (set! url-history-pointer (1+ url-history-pointer))
       (send self history-rebuild-menu)
       (let ((his (list-ref url-history url-history-pointer)))
	 (send self view-cached-url (cadr his) (caddr his) #f)))
     ]

   [history-update-view ()
     (when (nonnegative? url-history-pointer)
       (let* ((hentry (list-ref url-history url-history-pointer))
	      (hvalue (cdr hentry))
	      (cvalue (car hvalue))
	      (wgt (car cvalue)))
	 (set-car! (cdr hvalue) (send wgt get-vertical-view))
;;;	 (printf "history-update-view ptr=~s view=~s~n"
;;;		 url-history-pointer (cdr hvalue))
	 ))]
   )
  )


(define new-viewer
  (rec new-viewer
    (case-lambda 
      [() (new-viewer (string-append (getenv "SWL_ROOT") "/" "index.html"))]
      [(name)
       ; use default web browser on Mac OS X
       (case (machine-type)
         [(ppcosx i3osx) (system (format "/usr/bin/open ~a" name))]
         [else
          (swl:begin-application
            (lambda (token)
              (console-error-port (swl:error-log-port))
              (let ([top
                     (create <html-toplevel> name with
                       (destroy-request-handler:
                         (lambda (self)
                           (swl:end-application token)
                           #t)))])
                (lambda () (send top destroy)))))])

       (void)])))

;(swl:register-application "New HTML Viewer" new-viewer)

#!eof
