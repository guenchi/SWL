;; $Id: syncase.ss 1.1 Tue, 11 Jun 2002 20:42:11 -0500 owaddell $


;; the syncase selector-dispatcher.  Documentation tbw.

(extend-syntax (syncase)
  [(syncase subj alpha ...)
   (symbol? 'subj)
   ((synlambda (subj) alpha ...) subj)]
  [(syncase subj alpha ...)
   (with ((subject (gensym)))
     ((synlambda (subject) alpha ...) subj))])

(extend-syntax (synlambda keywords)
  ((synlambda (formal0 formal1 ...) (keywords key ...) alpha ...)
   (with ((all-keys (append '(key ...) *core-keywords*)))
     (synlambda* () () (formal0 formal1 ...) all-keys (alpha ...))))
  ((synlambda (formal0 formal1 ...) alpha ...)
   (synlambda (formal0 formal1 ...) (keywords) alpha ...)))

(extend-syntax (synlambda* else keywords)
  [(synlambda* (seen-clause ...) (seen-match-pred ...)
     (formal0 formal1 ...) keys ())
   (with (((match ...) (map (lambda (x) (gensym)) '(seen-match-pred ...))))
     (with (((h ...) (if (= (length '(seen-clause ...))
			    (length '(seen-match-pred ...)))
			 '(match ...)
			 (append '(match ...)'(else)))))
       (with (((reved-seen-clause ...) (reverse '(seen-clause ...)))
	      ((reved-seen-match-pred ...) (reverse '(seen-match-pred ...))))
	 (let ((match reved-seen-match-pred) ...)
	   (lambda (formal0 formal1 ...)
	     (syncase** formal0 keys
	       (h reved-seen-clause)
	       ...))))))]
  [(synlambda* (seen-clause ...) (seen-match-pred ...)
     formals keys ([else b0 b1 ...]))
   (synlambda* ((begin b0 b1 ...) seen-clause ...) (seen-match-pred ...)
     formals keys ())]
  [(synlambda* (seen-clause ...) (seen-match-pred ...)
     formals keys ([pat b0 b1 ...] alpha ...))
   (not (eq? 'pat 'else))
   (synlambda* ([pat b0 b1 ...] seen-clause ...)
     ((predicate-generator pat keys) seen-match-pred ...)
     formals keys (alpha ...))]
  [(synlambda* foo ...)
   (begin (write "Bad News") (newline)
     (pretty-print `(synlambda* ,'(foo ...))) #t)
   (list 'foo ...)])
   
(extend-syntax (syncase** guard else)
  [(syncase** subj keys) (void)]
  [(syncase** subj keys [else body]) body]
  [(syncase** subj keys [match [pat (guard pred-exp) b0 b1 ...]] alpha ...)
   (with ((does-not-match (gensym))
	  (let-pairs (make-let-binds 'pat 'subj 'keys)))
     (let ((does-not-match
	     (lambda ()
	       (syncase** subj keys alpha ...))))
       (if (match subj)
	   (let* let-pairs
	     (if pred-exp
		 (begin b0 b1 ...)
		 (does-not-match)))
	   (does-not-match))))]
  [(syncase** subj keys [match [pat b0 b1 ...]] alpha ...)
   (syncase** subj keys [match [pat (guard #t) b0 b1 ...]] alpha ...)])
   
;; -------------------------------------------------------------------------
(extend-syntax (predicate-generator)
  ((predicate-generator pat keywords)
   (lambda (subj) (pat-match-generator pat keywords))))

(extend-syntax (pat-match-generator constant variable ..)
  ((pat-match-generator pat keywords)
   (and (symbol? 'pat) (memq 'pat 'keywords))
   (eq? 'pat subj))
  ((pat-match-generator pat keywords)
   (symbol? 'pat)
   #t)
  ((pat-match-generator (constant x) keywords)
   (not (or (pair? subj) (symbol? subj))))
  ((pat-match-generator (variable x) keywords)
   (symbol? subj))
  ((pat-match-generator (new-pat ..) keywords)
   (and (list? subj)
     (andmap (predicate-generator new-pat keywords) subj)))
  ((pat-match-generator (x . y) keywords)
   (and (symbol? 'x) (not (memq 'x 'keywords)) (null? 'y))
   (and (pair? subj)
     (null? (cdr subj))))
  ((pat-match-generator (x . y) keywords)
   (and (symbol? 'x) (not (memq 'x 'keywords)))
   (and (pair? subj)
     (let ((subj (cdr subj)))
       (pat-match-generator y keywords))))
  ((pat-match-generator (x . y) keywords)
   (and (symbol? 'x) (memq 'x 'keywords) (null? 'y))
   (and (pair? subj)
     (and (eq? 'x (car subj))
       (null? (cdr subj)))))
  ((pat-match-generator (x . y) keywords)  
   (and (symbol? 'x) (memq 'x 'keywords))
   (and (pair? subj)
     (and (eq? 'x (car subj))
       (let ((subj (cdr subj)))
	 (pat-match-generator y keywords)))))
  ((pat-match-generator (x . y) keywords)
   (null? 'y)
   (and (pair? subj)
     (let ((subj (car subj)))
       (pat-match-generator x keywords))
     (null? (cdr subj))))
  ((pat-match-generator (x . y) keywords)
   (and (pair? subj)
     (let ((subj (car subj)))
       (pat-match-generator x keywords))
     (let ((subj (cdr subj)))
       (pat-match-generator y keywords))))
  ((pat-match-generator x keywords)
   (equal? 'x subj)))



(define dotted?
  (lambda (x)
    (and (not (null? (cdr x))) (pair? (cdr x)) (eq? (car (cdr x)) '..))))

(define make-let-binds
  (lambda (pat subj keywords)
    (letrec
      ([let-binds
	 (lambda (pat subj)
	   (cond
	     [(symbol? pat)
	      (if (not (memq pat keywords))
		  (list (list pat subj))
		  '())]
	     [(pair? pat)
	      (cond
		[(eq? (car pat) 'constant) (list (list (cadr pat) subj))]
		[(eq? (car pat) 'variable) (list (list (cadr pat) subj))]
		[(eq? (car pat) 'call) (let-binds (cdr pat) subj)]
		[(dotted? pat)
		 (if (symbol? subj)
		     (do-dotted (car pat) 'x subj)
		     (let ((gloc (gensym)))
		       (cons (list gloc subj)
			 (do-dotted (car pat) 'x gloc))))]
		[else
		  (append
		    (let-binds (car pat) `(car ,subj))
		    (let-binds (cdr pat) `(cdr ,subj)))])]
             [else '()]))]
       [do-dotted
	 (lambda (pat subj loc)
	   (cond
	     ((symbol? pat)
	      (if (memq pat keywords)
		  '()
		  (list (make-dotted-bind pat subj loc))))
	     ((pair? pat)
	      (cond
		[(eq? (car pat) 'constant)
		 (list (make-dotted-bind (cadr pat) subj loc))]
		[(eq? (car pat) 'variable)
		 (list (make-dotted-bind (cadr pat) subj loc))]
		[(eq? (car pat) 'call) (do-dotted (cdr pat) sub locj)]
		[(dotted? pat) (do-dotted (car pat) subj loc)]
		[else
		  (append
		    (do-dotted (car pat) `(car ,subj) loc)
		    (do-dotted (cdr pat) `(cdr ,subj) loc))]))
	     (else '())))]
       [make-dotted-bind
	 (lambda (pat subj loc)
	   (list
	     (string->symbol (string-append (symbol->string pat) ".."))
	     (if (eq? subj 'x)
		 loc
		 `(map (lambda (x) ,subj) ,loc))))])
      (let-binds pat subj))))

;; core-keywords can be added to if desired.

(define *core-keywords* '())

