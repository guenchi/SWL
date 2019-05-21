;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(simplemodule (exports make-inspect-object)

;; ------------------------------------------------------------

;; inspect-object
;;   simple
;;     number, character, empty-list, boolean, etc
;;   taggable (heapy?)
;;     opaque
;;       string
;;       symbol
;;       port
;;	 coded
;;         closure
;;         continuation
;;     box
;;     pair
;;     vector

;;; methods:

(define-svelte-class (<inspect-object> obj) (<base>)
  (ivars (handle (obj 'value)) (ihandle obj))
  (inheritable ihandle handle)
  (public
    [type-sym  () (ihandle 'type)]
    [type-string () (assertion-violationf 'type-string "Supposed to be virtual")]
    [print (op) (display "#<inspect-object>" op)]
    [print () (send self print (current-output-port))]
    [value () handle]
    [get-tag (ge) #f]
    ))

(simplemodule (exports make-inspect-object make-inspect-object2)

(define-svelte-class (<simple-inspect-object> obj) (<inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string ()
      (cond
	[(number? handle) "number"]
	[(char? handle) "character"]
	[(boolean? handle) "boolean"]
	[(null? handle) "empty list"]
	[(eq? (void) handle) "void object"]
	[(eq? handle #!bwp) "broken weak pointer"]
	[else "simple object"])]
    [get-inspector-state (iwid ge length level)
      (values (format "~s~n" handle) #f 0)]))

(define-svelte-class (<taggable-inspect-object> obj) (<inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [tag (ge) (ge 'tag handle)]
    [get-inspector-state (iwid ge len lev)
      (let ([ls (half-pprint-and-shared handle iwid lev len 0
		  (inspector-cols))])
	(if (null? (cdr ls))
	    (values (car ls) #f 0)
	    (values (car ls) (cdr ls) (+ (length (cdr ls)) 1))))
;;;      (values (half-pprint handle iwid lev len 0 (inspector-cols))
;;;	#f 0)
      ]))

(simplemodule (exports make-inspect-object make-inspect-object2)

(define-svelte-class (<box-inspect-object> obj) (<taggable-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string () "box"]
    [unbox () (make-inspect-object2 (ihandle 'unbox))]))

(define-svelte-class (<pair-inspect-object> obj)
  (<taggable-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string ()
      (let ([len (send self length)])
	(case (car len)
	  [proper (format "proper list (length ~s)" (cadr len))]
	  [circular (format "circular list (``length'' ~s)" (cadr len))]
	  [improper
	    (if (pair? (cdr handle))
		(format "improper list (``length'' ~s.5)" (cadr len))
		(format "pair"))]))]
    [length () (ihandle 'length)]
    [car () (make-inspect-object2 (ihandle 'car))]
    [cdr () (make-inspect-object2 (ihandle 'cdr))]))

(define-svelte-class (<vector-inspect-object> obj)
  (<taggable-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string () (format "vector (length ~s)" (send self length))]
    [length () (ihandle 'length)]
    [ref (n) (make-inspect-object2 (ihandle 'ref n))]))

(define-svelte-class (<opaque-inspect-object> obj)
  (<taggable-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public))

(simplemodule (exports make-inspect-object make-inspect-object2)

; for strings, I feel I shouldn't confuse people, so I only let them
; select characters if they're viewing the string itself.
(define-svelte-class (<string-inspect-object> obj)
  (<opaque-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string () (format "string (length ~s)" (send self length))]
    [length () (ihandle 'length)]
    [ref (n) (make-inspect-object2 (ihandle 'ref n))]
    [get-inspector-state (iwid ge length level)
      (values (list #\" (string-ps handle iwid) #\") #f 0)]))

(define-svelte-class (<symbol-inspect-object> obj)
  (<opaque-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string ()
      (if (send self uninterned?) "symbol (uninterned)" "symbol")]
    [name () (make-inspect-object2 (ihandle 'name))]
    [uninterned? () (ihandle 'uninterned?)]
    [top-level-value () (make-inspect-object2 (ihandle 'top-level-value))]
    [top-level-bound? () (top-level-bound? handle)]
    [property-list () (make-inspect-object2 (ihandle 'property-list))]
    [get-inspector-state (iwid ge length level)
      (values
	(list #\newline
	  "name: " (half-pprint (symbol->string handle) iwid
		     5 5 6 (inspector-cols))
	  #\newline
	  (if (send self top-level-bound?)
	      (list #\newline "top-level-value:" #\newline 2

		(half-pprint (top-level-value handle) iwid
		  5 5 2 (inspector-cols)))
	      0)
	  (if (and (eq? (subset-mode) 'system)
		   (not ((#%eval '\#unbound-object?)
			 ((#%eval '\#system-value) handle))))
	      (list #\newline "system-value:" #\newline 2
		(half-pprint ((#%eval '\#system-value) handle) iwid
		  5 5 2 (inspector-cols)))
	      0)
	  (if (not (null? (property-list handle)))
	      (list #\newline "property list:" #\newline 2
		(half-pprint (property-list handle) iwid
		  5 5 2 (inspector-cols)))
	      0))
	#f
	0)]))

(define-svelte-class (<port-inspect-object> obj)
  (<opaque-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string ()
      (if (send self input?)
	  (if (send self output?) "input-output port" "input port")
	  "output port")]
    [input? () (ihandle 'input?)]
    [output? () (ihandle 'output?)]
    [closed? () (ihandle 'closed?)]
    [handler () (make-inspect-object2 (ihandle 'handler))]
    [output-size () (ihandle 'output-size)]
    [output-index () (ihandle 'output-index)]
    [output-buffer () (make-inspect-object2 (ihandle 'output-buffer))]
    [input-size () (ihandle 'input-size)]
    [input-index () (ihandle 'input-index)]
    [input-buffer () (make-inspect-object2 (ihandle 'input-buffer))]
    [get-inspector-state (iwid ge length level)
      (values "#<port>" #f 0)]))

(define white-markup (create <markup> with (foreground-color: 'white)))

(define-svelte-class (<coded-inspect-object> obj) (<opaque-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [length () (ihandle 'length)]
    [name-ref (n) ((ihandle 'ref n) 'name)]
    [ref (n) (make-inspect-object2 ((ihandle 'ref n) 'ref))]
    [set! (n val) ((ihandle 'ref n) 'set! val)]
    [source () (let ([source-obj ((ihandle 'code) 'source)])
		 (and source-obj (make-inspect-object2 source-obj)))]
    [name () ((ihandle 'code) 'name)]
    [freevars-ps (iwid)
      (let ([len (send self length)])
	(if (zero? len)
	    0
	    (list #\newline "free variables:"
	      (let f ([n 0])
		(if (= n len)
		    '()
		    (let* ([name (send self name-ref n)]
			   [ival (send self ref n)]
			   [val (send ival value)])
		      (cons
			(list
			  (vector white-markup
			    (if name
				(format "~n ~s. ~a:~n    " n name)
				(format "~n ~s: " n)))
			  (flat-half-pprint val iwid
			    (- (inspector-cols) 4)))
			(f (+ n 1)))))))))]))

(simplemodule (exports make-inspect-object make-inspect-object2)

(define-svelte-class (<closure-inspect-object> obj)
  (<coded-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string ()
      (let ([name (send self name)])
	(if name (format "procedure ~a" name) "procedure"))]
    [get-inspector-state (iwid ge length level)
      (let* ([len (send self length)]
	     [source (send self source)]
	     [ps
	       (send self freevars-ps iwid)]
	     [ps2 
	       (if source
		   (list "mangled code:" #\newline 2
		     (flat-half-pprint (letify (send source value)) iwid
		       (- (inspector-cols) 2)))
		   0)])
	(values ps (and source ps2) (if source 3 0)))]))

(define-svelte-class (<continuation-inspect-object> obj)
  (<coded-inspect-object> obj)
  (inherited handle ihandle)
  (inheritable handle ihandle)
  (public
    [type-string ()
      (let ([name (send self name)])
	(if name (format "continuation in ~a" name) "continuation"))]
    [link () (make-inspect-object2 (ihandle 'link))]
    [call-source ()
      (let ([source-obj (ihandle 'source)])
	(and source-obj (make-inspect-object2 source-obj)))]
    [get-inspector-state (iwid ge length level)
      (let* ([len (send self length)]
	     [source (send self source)]
	     [link (send self link)]
	     [call-source (send self call-source)]
	     [ps
	       (list 
		 "link:  "
		 (half-pprint (send link value) iwid
		   5 5 7 (inspector-cols))
		 #\newline
		 (send self freevars-ps iwid))]
	     [ps2 (list
		    (if call-source
			(list "mangled source:"
			  #\newline 2
			  (flat-half-pprint (letify (send call-source value))
			    iwid (- (inspector-cols) 2))
			  #\newline)
			0)
		    (if source
			(list "mangled procedure source:" #\newline 2
			  (flat-half-pprint (letify (send source value))
			    iwid (- (inspector-cols) 2)))
			0))])
	(values ps (and (or source call-source) ps2)
	  (+ 1 (if source 2 0) (if call-source 2 0))))]))

  (simplemodule (exports make-inspect-object make-inspect-object2)

    (define make-inspect-object
      (lambda (obj)
	(make-inspect-object2 (inspect/object obj))))

    (define make-inspect-object2
      (lambda (iobj)
	(case (iobj 'type)
	  [simple (make <simple-inspect-object> iobj)]
	  [box (make <box-inspect-object> iobj)]
	  [pair (make <pair-inspect-object> iobj)]
	  [vector (make <vector-inspect-object> iobj)]
	  [string (make <string-inspect-object> iobj)]
	  [procedure (make <closure-inspect-object> iobj)]
	  [continuation (make <continuation-inspect-object> iobj)]
	  [port (make <port-inspect-object> iobj)]
	  [symbol (make <symbol-inspect-object> iobj)]
	  [else
	    (assertion-violationf 'make-inspect-object
	      "unhandled case ~s" obj)])))

    )
  )
  )
  )
  )
  )
