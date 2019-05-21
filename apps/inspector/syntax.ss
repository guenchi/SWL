;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define-syntax define-svelte-class
  (let ()
    (define collect
      (lambda (template ls)
	(let loop
	  ([ivs (datum->syntax-object template '(ivars))]
	   [inhd (datum->syntax-object template '(inherited))]
	   [inhtbl (datum->syntax-object template '(inheritable))]
	   [priv (datum->syntax-object template '(private))]
	   [prot (datum->syntax-object template '(protected))]
	   [pub (datum->syntax-object template '(public))]
	   [ls ls])
	  (if (null? ls)
	      (list ivs inhd inhtbl priv prot pub)
	      (syntax-case (car ls)
		(ivars inherited inheritable private protected public)
		[(ivars thing ...)
		 (loop (car ls) inhd inhtbl priv prot pub (cdr ls))]
		[(inherited thing ...)
		 (loop ivs (car ls) inhtbl priv prot pub (cdr ls))]
		[(inheritable thing ...)
		 (loop ivs inhd (car ls) priv prot pub (cdr ls))]
		[(private thing ...)
		 (loop ivs inhd inhtbl (car ls) prot pub (cdr ls))]
		[(protected thing ...)
		 (loop ivs inhd inhtbl priv (car ls) pub (cdr ls))]
		[(public thing ...)
		 (loop ivs inhd inhtbl priv prot (car ls) (cdr ls))])))))
    (lambda (x)
      (syntax-case x ()
	[(_ (name formal ...) (base base-formal ...) thing ...)
	 (with-syntax
	   ([(new-thing ...) (collect (syntax _) (syntax (thing ...)))])
	   (syntax
	     (define-swl-class (name formal ...) (base base-formal ...)
	       new-thing ...)))]))))

(define-syntax with-capture
  (syntax-rules ()
    [($ context (name ...) exp0 exp1 ...)
     (with-syntax ((name (datum->syntax-object context 'name))
		   ...)
       exp0 exp1 ...)]))

(define-syntax define-constant
  (syntax-rules ()
    [($ name exp)
     (define-syntax name (identifier-syntax exp))]))

(define-syntax push-onto!
  (lambda (x)
    (syntax-case x ()
      [(_ var val)
       (with-capture (syntax _) (set!)
	 (syntax (set! var (cons val var))))])))

(define-syntax tostringy
  (syntax-rules ()
    [(_ var exp)
     (let ([var (open-output-string)])
       exp
       (get-output-string var))]))

(define-syntax multi-set-option!
  (syntax-rules ()
    [(_ (wid ...) thing ...)
     (begin
       (set-option! wid thing ...)
       ...)]))

;; ---- usual-macros.ss
;; Implementations of usually used macros using syntax-case

(define-syntax mv-let
  (syntax-rules ()
    [(_ () b0 b1 ...)
     (begin b0 b1 ...)]
    [(_ ((formals e) decl ...) b0 b1 ...)
     (let ((t (lambda () e)))
       (mv-let (decl ...)
	 (call-with-values t (lambda formals b0 b1 ...))))]))

(define-syntax mv-let*
  (syntax-rules ()
    [(_ () b0 b1 ...)
     (begin b0 b1 ...)]
    [(_ ((formals e) decl ...) b0 b1 ...)
     (call-with-values (lambda () e)
       (lambda formals
	 (mv-let* (decl ...) b0 b1 ...)))]))

(define-syntax mv-set!
  (lambda (x)
    (syntax-case x ()
      [(_ (v ...) e)
       (andmap identifier? (syntax (v ...)))
       (with-syntax ([(t ...) (generate-temporaries (syntax (v ...)))])
	 (with-capture (syntax _) (set!) ; aaaaaarrgh!!!
	   (syntax
	     (call-with-values (lambda () e)
	       (lambda (t ...)
		 (set! v t) ...)))))])))

(define-syntax simplemodule
  (lambda (x)
    (syntax-case x (exports)
      [(_ (exports v ...) e0 e1 ...)
       (if (and (top-level-bound? 'let-me-in!)
		(top-level-value 'let-me-in!))
	   (syntax (begin e0 e1 ...))
	   (with-syntax ([(t ...) (generate-temporaries (syntax (v ...)))])
	     (syntax 
	       (begin
		 (define v) ...
		 (let ((t #f) ...)
		   (let () e0 e1 ... (set! t v) ...)
		   (set! v t) ...)))))])))
