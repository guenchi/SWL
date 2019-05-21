#!eof  ; not maintained

;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; ---- fontmenu.ss

;; ----------------------------------------------------------------------------
;; 9/10/97 - johnz -
;; Extensive changes for support of Windows under Tk 4.2.  Dropped the concept
;; of font restrictions, which required a lookup of all available fonts on
;; the system (an operation that is not supported by Windows).   
;;
;; The new approach computes a minimal set of fonts that meets
;; the specified _requirements_, rather than the maximal set of
;; fonts subject to the specified _restrictions_.  Thus, a query
;; of the "server" is no longer required.
;; ----------------------------------------------------------------------------

;; warning: loading this file uses the identifiers
;;   simplemodule, usual-font-menu-proc, font-menu-proc,
;;   make-font-menu-proc

;; A font-menu ``object'' is a procedure which takes one argument: a
;; procedure which accepts a <font> object (i.e., this will be your
;; callback).  It returns a <menu> object appropriate for placing in
;; the menu: field of a <menubutton> widget.

;; How does one make a font-menu ``object?'' With the macro
;; font-menu-proc 

;;; (font-menu-proc (key ...) requirement ...)

; each key should be one of:
;    foundry: family: weight: slant: setwidth: style: pixel-size: points:
;    resx: resy: spacing: average-width: registry: encoding:
; each requirement should be
;    (key possible-value ...)

;;; ---- example

; To get a menu of roman fonts between 10 and 17 pts, selectable by
; point, weight and name, do:

; (define menu-generator
;   (font-menu-proc (points: weight: name:)
;      (slant: r) (points: 10 11 12 13 14 15 16 17) (registry: iso8859)
;      ))

; then, when you want to actually create a menu (say, one that sets
; the font of the text widget ``text0''), use

; (define menu (menu-generator (lambda (fn) (set-option! text0 (font: fn)))))

;;; ---- caveats, conditions and kvetches:

; I think X is a _bit_ smarter than I am in picking resx and resy
; fonts (i.e., it tries to match the current screen resolution).  Oh
; well.

; Since an iso8859 registry is so usual, I've also defined the macro
; usual-font-menu-proc which only selects iso8859 fonts.

;;; ------------------------------
;   Implementation
;;; ------------------------------

(define-syntax usual-font-menu-proc
  (syntax-rules ()
    [(_ (key ...) (r-key val ...) ...)
     (font-menu-proc (key ...)
       (registry: iso8859) (r-key val ...) ...)]))

(define-syntax font-menu-proc
  (syntax-rules ()
    [(_ (key ...) (r-key val ...) ...)
     (make-font-menu-proc
      (list (lambda (font) (get-option font key)) ...)
      (list (map (lambda (v)
		   (lambda (font) (set-option! font (r-key v))))
		 '(val ...))
	    ...))]))

(define-syntax fixed-font-menu-proc
  (syntax-rules ()
    [(_)
     (usual-font-menu-proc
      (points: weight:)
      (points: 8 10 12 14 16 18 20 22 24)
      (weight: medium bold)
      (slant: r)
      (spacing: m))]))


(define-syntax simplemodule
  (lambda (x)
    (syntax-case x (exports)
      [(_ (exports v ...) e0 e1 ...)
       (with-syntax ([(t ...) (generate-temporaries (syntax (v ...)))])
	 (syntax 
	   (begin
	     (define v) ...
	     (let ((t #f) ...)
	       (let () e0 e1 ... (set! t v) ...)
	       (set! v t) ...))))])))

(simplemodule (exports make-font-menu-proc)

;; <fatfont> needs to be _create_d, not made.
;; presumably because make-font doesn't support it?

(define-swl-class
  (<fatfont> foundry family weight slant setwidth style pixel-size points
    resx resy spacing average-width registry encoding)
  (<font> foundry family weight slant setwidth points)
  (ivars
    (style style)
    (pixel-size pixel-size)
    (resx resx)
    (resy resy)
    (spacing spacing)
    (average-width average-width)
    (registry registry)
    (encoding encoding)
    )
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [dup ()
      (dup-font
       ;; dup-font cannot be a method because a reference to
       ;; the class is not allowed in a method body.
       (send self get-foundry)
       (send self get-family)
       (send self get-weight)
       (send self get-slant)
       (send self get-setwidth)
       (send self get-style)
       (send self get-pixel-size)
       (send self get-points)
       (send self get-resx)
       (send self get-resy)
       (send self get-spacing)
       (send self get-average-width)
       (send self get-registry)
       (send self get-encoding))]
    [init ignore-args (void)] ;;; favor instantiation time
    [scheme->tcl (op)
      (display (format "\"~a\"" (send self get-descriptor)) op)]
    [get-descriptor ()
      (format  "-~a-~a-~a-~a-~a-~a-~a-~a0-~a-~a-~a-~a-~a-~a"
	       (or (send self get-foundry) '*) (or (send self get-family) '*)
	       (or (send self get-weight) '*)  (or (send self get-slant) '*)
	       (or (send self get-setwidth) '*) (or style '*)
	       (or pixel-size '*) (or (send self get-points) '*)
	       (or resx '*) (or resy '*) (or spacing '*)
	       (or average-width '*) (or registry '*) (or encoding '*))]
    [get-name ()
       (let ([foundry (send self get-foundry)]
	     [family (or (send self get-family) "")])
	 (if foundry
	     (string-append foundry "-" family)
	     family))]
    [get-style () style]
    [get-pixel-size () pixel-size]
    [get-resx () resx]
    [get-resy () resy]
    [get-spacing () spacing]
    [get-average-width () average-width]
    [get-registry () registry]
    [get-encoding () encoding]
    
    [set-style! (x) (set! style x)]
    [set-pixel-size! (x) (set! pixel-size x)]
    [set-resx! (x) (set! resx x)]
    [set-resy! (x) (set! resy x)]
    [set-spacing! (x) (set! spacing x)]
    [set-average-width! (x) (set! average-width x)]
    [set-registry! (x) (set! registry x)]
    [set-encoding! (x) (set! encoding x)]
    ))

(define dup-font
  (lambda (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
    (create <fatfont> a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)))


;; A font tree has strings at each internal node, and
;; strings-and-fonts at leaves.  Right now it is balanced, even though
;; that can lead to menus with only one choice.

(define make-font-menu-proc
  (lambda (getters setters*)
    (let ([font-tree (make-font-tree getters setters*)])
      (lambda (callback)
	(letrec ([make-font-submenu
		  (lambda (node)
		    (if (pair? (cdr node)) ; internal: (name . (node ...))
			(cons (car node)
			      (make <menu>
				    (simple-menu-list->menu-items
				     (map make-font-submenu (cdr node)))))
			(cons (car node) ; leaf: (name . font)
			      (let ([font (cdr node)])
				(lambda args
				  (callback font))))))])
	  (make <menu> (simple-menu-list->menu-items
			(map make-font-submenu font-tree))))))))


(define make-font-tree
  (lambda (getters setters*)
    (define choose-most-appropriate-font car)
    (let ([applicable-fonts (get-applicable-fonts setters*)])
      (let build-keys ([keys getters]
		       [fonts applicable-fonts])
	(if (null? keys)
	    (choose-most-appropriate-font fonts)
	    (let* ([breakup (partition fonts (car keys))])
	      (map (lambda (x)
		     (cons (thing->name (car x))
			   (build-keys (cdr keys) (cdr x))))
		   (sort (lambda (x y) (thing<=? (car x) (car y)))
			 breakup))))))))

(define reduce
  (lambda (ls)
    (cond [(null? ls) '()]
	  [(eq? (car ls) #f) (reduce (cdr ls))]
	  [else (cons (car ls) (reduce (cdr ls)))])))
  
(define get-applicable-fonts
  (lambda (setters*)
    (let ((candidates 
	   ;;
	   ;; a setter is a procedure that takes a font and sets
	   ;; a field to a value, thereby restricting its scope.
	   ;;
	   (cross-product
	    (lambda (font setter)
	      (let ((nfont (send font dup)))
		(setter nfont)
		nfont))
	    (list (create <fatfont> #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
	    setters*)))
      candidates)))

(define cross-product
  (lambda (f vals effect-sets)
    ;;
    ;; f - takes a value and a side-effecting proc and returns a new value.
    ;; effect-set - list of procs to side-effect values.
    ;; 
    (if (null? effect-sets)
	vals
	(let ((vals (cross-product f vals (cdr effect-sets)))
	      (effect-set (car effect-sets)))
	  (let g ((acc '()) (effect-set effect-set))
	    (if (null? effect-set)
		acc
		(let ((proc (car effect-set)))
		  (let h ((acc acc) (vals vals))
		    (if (null? vals)
			(g acc (cdr effect-set))
			(h (cons (f (car vals) proc) acc) (cdr vals)))))))))))
    


(define thing->name
  (lambda (x)
    (cond [(string? x) x]
	  [(symbol? x) (symbol->string x)]
	  [(number? x) (number->string x)]
	  [(not x) "<any>"]
	  [else (assertion-violationf 'thing->name "arg has wrong type: ~a" x)])))

(define thing<=?
  (lambda (x y)
    (cond
      [(and (string? x) (string? y)) (string<=? x y)]
      [(and (number? x) (number? y)) (<= x y)]
      [else (thing<=? (thing->name x) (thing->name y))])))


(define partition
  ;; returns a list of lists, each headed by the partition value.
  (letrec ([new-table
	    (lambda (key val table)
	      (cond
		[(null? table) (list (list key val))]
		[else
		 (let ([e (car table)])
		   (if (equal? (car e) key)
		       (cons
			(cons (car e)
			      (cons val (cdr e)))
			(cdr table))
		       (cons e (new-table key val (cdr table)))))]))])
    (lambda (ls avatar-func)
      (let loop ([ls ls] [table '()])
	(if (null? ls)
	    table
	    (loop (cdr ls) (new-table (avatar-func (car ls))
				      (car ls)
				      table)))))))

)	; close simplemodule

;(define test
;  (let ([menuproc
;	  (usual-font-menu-proc (points: weight:) ; keys
;	    (points: 12 13 14 15 16 17 18 19 20)
;           (weight: medium bold)
;	    (slant: r)
;	    )])		     ; requirements
;    (lambda ()
;      (let ([tl #f]
;	    [txt #f]
;	    [mb #f])
;	(set! tl (create <toplevel>))
;	(set! txt (create <text> tl))
;	(set! mv (create <menubutton> tl with (title: "hi")))
;	(set-option! mv (menu: (menuproc
;				 (lambda (f)
;				   (set-option! txt (font: f))))))
;	(pack mv (side: 'top))
;	(pack txt (side: 'top)))))
;  )
