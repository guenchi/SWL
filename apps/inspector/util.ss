;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define maplist
  (lambda (f ls)
    (when (null? ls)
      (assertion-violationf 'maplist "Can't maplist on the empty list"))
    (let loop ([ls ls] [tail (cdr ls)])
      (if (null? tail)
	  (list (f ls))
	  (cons (f ls) (loop tail (cdr tail)))))))


(simplemodule (exports half-pprint-and-shared half-pprint flat-half-pprint)

  (define digits
    (lambda (x)
      (if (zero? x) 1
	  (inexact->exact (ceiling (/ (log (+ x 1)) (log 10)))))))

  (define make-deep-ge
    (lambda (obj lev len)
      (if (and (not lev) (not len) (really-cyclic? obj lev len))
	  (let* ([shared (reverse (find-shared obj lev len))]
		 [shared-len (length shared)]
		 [obj-vec (make-vector shared-len)]
		 [label-vec (make-vector shared-len)]
		 [handler-vec (make-vector shared-len)]
		 [flag-vec (make-vector shared-len #f)]
		 [todo '()])
	    (let loop ([ls shared] [n 0])
	      (unless (null? ls)
		(vector-set! handler-vec n
		  (lambda (msg)
		    (case msg
		      [size (+ 2 (digits n))]
		      [string
			(unless (vector-ref flag-vec n)
			  (vector-set! flag-vec n #t)
			  (push-onto! todo n))
			(vector-ref label-vec n)])))
		(vector-set! label-vec n
		  (let* ([str (format "[~s]" n)]
			 [m (rec m (create <markup> with
				     (mouse-enter-method:
				       (lambda args
					 (set-option! m (underline: #t))))
				     (mouse-leave-method:
				       (lambda args
					 (set-option! m (underline: #f))))))])
		    (vector m str)))
		(vector-set! obj-vec n (car ls))
		(loop (cdr ls) (fx+ n 1))))
	    (case-lambda
	      [(x)
	       (let ([ls (memq x shared)])
		 (and ls (vector-ref handler-vec (- shared-len (length ls)))))]
	      [()
	       (if (null? todo)
		   (values)
		   (let ([n (car todo)])
		     (set! todo (cdr todo))
		     (let ([labstr (vector-ref label-vec n)])
		       (values n labstr (+ 2 (digits n))
			 (vector-ref obj-vec n)))))]))
	  (case-lambda
	    [(x) #f]
	    [() (values)]))))

  ;; returns a list of ps's, one for each shared thing.  The first is
  ;; the main one.
  (define half-pprint-and-shared
    (lambda (obj iwid lev len start limit)
      (let ([ge (make-deep-ge obj lev len)])
	(let ([ps (inspector-ps (pretty-print-struct obj ge lev len)
		    iwid start limit)]
	      [self-flag? #f])
	  (let ([sub-ps-ls
		  (let loop ([acc '()])
		    (call-with-values (lambda () (ge))
		      (case-lambda
			[() acc]
			[(n prefix prefix-size sub-obj)
			 (if (eq? sub-obj obj)
			     (begin (set! self-flag? (list prefix #\: #\newline))
			       (loop acc))
			     (let ([sub-pps (flat-pretty-print-struct sub-obj ge
					      (- (inspector-cols) prefix-size))])
			       (loop (cons
				       (list n (list prefix #\: 1
						 (inspector-ps sub-pps iwid 0
						   (inspector-cols))))
				       acc))))])))])
	    (cons (list (if self-flag?
			    self-flag?
			    0)
		    ps)
	      (let ([ls (map cadr (sort (lambda (a b) (< (car a) (car b)))
				    sub-ps-ls))])
		(if (null? ls)
		    ls
		    (let loop ([head (car ls)] [tail (cdr ls)])
		      (if (null? tail)
			  (list head)
			  (list* head #\newline (loop (car tail) (cdr tail)))))))))))))

  (define half-pprint
    (lambda (obj iwid lev len start limit)
      (let ([ge (make-deep-ge obj lev len)])
	(let ([pps (pretty-print-struct obj ge lev len)])
	  (inspector-ps pps iwid start limit)))))

  (define make-flat-ge
    (lambda (obj)
      (lambda (x) #f)))

  (define flat-half-pprint
    (lambda (obj iwid space)
      (let ([ge (make-flat-ge obj)])
	(let ([pps (flat-pretty-print-struct obj ge space)])
	  (inspector-ps pps iwid 0 (+ space 1))))))

  )

(simplemodule (exports inspector-ps string-ps)

  (define-svelte-class (<container-markup> children) (<markup>)
    (ivars [children children])
    (public
      [set-relief! (r)
	(send-base self set-relief! r)
	(for-each (lambda (child) (send child set-relief! r))
	  children)]))

  (define-syntax pps-case
    (syntax-rules (simple container ref)
      [(_ (a ...) clause ...)
       (let ((x (a ...))) (ps-case clause ...))]
      [(_ x (type (a b c) e0 e1 ...) ...)
       (case (vector-ref x 0)
	 [(quote type)
	  (let ([a (vector-ref x 1)]
		[b (vector-ref x 2)]
		[c (vector-ref x 3)])
	    e0 e1 ...)]
	 ...
	 [else
	   (assertion-violationf 'pps-case "unmatched ps ~s" x)])]))

;;;(define simple-markup (lambda (obj) 'simple))
;;;(define contents-markup (lambda (a b) (values 'pref 'cont)))

  (define simple-markup
    (lambda (obj iwid)
      (rec m (create <markup> with (border-width: 2)
	       (mouse-enter-method:
		 (lambda x (set-option! m (relief: 'raised))))
	       (mouse-leave-method:
		 (lambda x (set-option! m (relief: 'flat))))
	       (mouse-press-method:
		 (lambda x (set-option! m (relief: 'flat))
		   (send iwid show-object (make-inspect-object obj))))))))

  (define contents-markup		; returns prefix and total
    (lambda (obj children iwid)
      (let ([cm (create <container-markup> children with (border-width: 2))])
	(let ([pm (create <markup> with
		    (mouse-enter-method:
		      (lambda x (set-option! cm (relief: 'raised))))
		    (mouse-leave-method:
		      (lambda x (set-option! cm (relief: 'flat))))
		    (mouse-press-method:
		      (lambda x
			(set-option! cm (relief: 'flat))
			(send iwid show-object (make-inspect-object obj)))))])
	  (values pm cm)))))

  (define (string-ps str iwid)
    (let ([len (string-length str)])
      (let f ([i 0])
	(if (= i len)
	    '()
	    (let ([ch (string-ref str i)])
	      (cons (vector (simple-markup ch iwid)
		      (if (char=? ch #\nul) #\? ch))
		(f (+ i 1))))))))

  (define (inspector-ps pps iwid initial limit)
    (define ips	
      (lambda (pps pos)
	(pps-case pps
	  [simple (size obj str)
	    (let ([m (simple-markup obj iwid)])
	      (values m (vector m str)))]
	  [ref (size obj thing)
	    (let ([m (simple-markup obj iwid)])
;	      (set-option! m (underline: #t))
	      (values m (vector m (thing 'string))))]
	  [container (size obj contents)
	    (mv-let ([(new-pos prefix suffix)
		      (cond
			[(box? obj) (values (+ 2 pos) '"#&" '"")]
			[(pair? obj) (values (+ 1 pos) '"(" '")")]
			[(vector? obj) (values (+ 2 pos) '"#(" '")")])])
	      (mv-let ([(c-ms c-pps)
			(ips-list contents new-pos 0
			  (if (<= (+ new-pos size) limit) 1
			      (list #\newline new-pos)))])
		(mv-let ([(pm tm)
			  (contents-markup obj c-ms iwid)])
		  (values tm
		    (vector tm
		      (list (vector pm prefix)
			c-pps
			suffix))))))])))
    (define ips-list
      (lambda (ls pos delim next)
	(cond
	  [(null? ls)
	   (values '() '())]
	  [(eq? (car ls) 'elipsis)
	   (values '() (list delim '"..."))]
	  [(eq? (car ls) 'dot)
	   (mv-let ([(m ps) (ips-list (cdr ls) pos next next)])
	     (values m (list delim #\. ps)))]
	  [else
	    (mv-let ([(head-m head-ps) (ips (car ls) pos)])
	      (mv-let ([(tail-m tail-ps) (ips-list (cdr ls) pos next next)])
		(values (cons head-m tail-m)
		  (list* delim head-ps tail-ps))))])))
    (mv-let ([(ignored ps) (ips pps initial)])
      ps))

  )

(simplemodule (exports flatten-ps apply-deep-ms)

  ;;  (simplemodule (exports open-xy-output-string
  ;;		  get-xy-output-string
  ;;		  get-xy-xy)

  ;; removed critical sections

  (define make-xy-handler
    (lambda () 
      (let ([point 0] [row 0] [col 0] [heads '()])
	(lambda (msg . args)
	  (record-case (cons msg args)
	    [clear-output-port (p)
	      (set-port-output-index! p 0)
	      (set! point 0)
	      (set! row 0)
	      (set! col 0)
	      (set! heads '())]
	    [close-port (p)
	      (clear-output-port p)
	      (mark-port-closed! p)]
	    [flush-output-port (p) (void)]
	    [port-name (p) '"xy-output-string"]
	    [write-char (c p)
	      (let ([b (port-output-buffer p)]
		    [i (port-output-index p)]
		    [s (port-output-size p)])
		(when (fx= i s)
		  (get-xy-xy p)		; updates row and col
		  (set! heads (cons (string-copy b) heads))
		  (set-port-output-index! p 0)
		  (set! point 0))
		(write-char c p))]
	    [get-output-string (p)
	      (let* ([os (substring (port-output-buffer p)
			   0 (port-output-index p))]
		     [os (apply string-append (reverse (cons os heads)))])
		(clear-output-port p)
		os)]
	    [get-xy (op)
	      (let loop ([buf (port-output-buffer op)]
			 [p point]
			 [index (port-output-index op)]
			 [r row]
			 [c col])
		(cond
		  [(fx= p index)
		   (set! row r)
		   (set! col c)
		   (set! point p)
		   (cons c r)]
		  [(char=? (string-ref buf p) #\newline)
		   (loop buf (fx+ p 1) index (fx+ r 1) 0)]
		  [else
		    (loop buf (fx+ p 1) index r (fx+ c 1))]))]
	    [else
	      (assertion-violationf 'xy-port "operation ~s not handled" msg)])))))

  (define open-xy-output-string
    (lambda ()
      (make-output-port (make-xy-handler) (make-string 1000))))

  (define get-xy-xy
    (lambda (op)
      ((port-handler op) 'get-xy op)))

  (define get-xy-output-string
    (lambda (op)
      ((port-handler op) 'get-output-string op)))
  ;;    )

  (simplemodule (exports flatten-ps apply-deep-ms)

    (define-syntax ps-case
      (syntax-rules (else simple marked spaces complex)
	[(_ (a ...) clause ...)
	 (let ((x (a ...))) (ps-case clause ...))]
	[(_ x)
	 (assertion-violationf 'ps-case "unmatched ps ~s" x)]
	[(_ x (else e0 e1 ...))
	 (begin e0 e1 ...)]
	[(_ x (simple (v) e0 e1 ...) clause ...)
	 (if (or (string? x) (char? x))
	     (let ([v x]) e0 e1 ...)
	     (ps-case x clause ...))]
	[(_ x (marked (m p) e0 e1 ...) clause ...)
	 (if (vector? x)
	     (let ([m (vector-ref x 0)] [p (vector-ref x 1)]) e0 e1 ...)
	     (ps-case x clause ...))]
	[(_ x (spaces (n) e0 e1 ...) clause ...)
	 (if (number? x)
	     (let ([n x]) e0 e1 ...)
	     (ps-case x clause ...))]
	[(_ x (complex (v) e0 e1 ...) clause ...)
	 (if (or (pair? x) (null? x))
	     (let ([v x]) e0 e1 ...)
	     (ps-case x clause ...))]))
  
    (define-constant my-mark? vector?)
    (define-constant make-my-mark vector)

    (define apply-deep-ms
      (lambda (m tx)
	(if (vector? m)
	    (begin
	      (apply-markup (vector-ref m 0) tx
		(vector-ref m 1)
		(vector-ref m 2))
	      (apply-deep-ms (vector-ref m 3) tx))
	    (for-each (lambda (m) (apply-deep-ms m tx)) m))))

    (define flatten-ps
      (let ([op (open-xy-output-string)])
	(lambda (ps)
	  (let ([ms (let fps ([ps ps])
		      (ps-case ps
			[simple (thing) (display thing op) '()]
			[complex (ls)
			  (let fps-c ([ls ls])
			    (if (null? ls)
				'()
				(let ([head (fps (car ls))])
				  (if (null? head)
				      (fps-c (cdr ls))
				      (cons head (fps-c (cdr ls)))))))]
			[spaces (n)
			  (let loop ([n n])
			    (unless (zero? n)
			      (write-char #\space op)
			      (loop (- n 1))))
			  '()]
			[marked (m ps)
			  (let* ([start (get-xy-xy op)]
				 [ms (fps ps)]
				 [end (get-xy-xy op)])
			    (make-my-mark m start end ms))]))])
	    (values (get-xy-output-string op)
	      ms)))))

  

    )
  )

(simplemodule (exports pretty-print-struct
		flat-pretty-print-struct)

  (define-syntax decr
    (syntax-rules ()
      [(_ x)
       (and x (fx- x 1))]))

  (define-syntax limit?
    (syntax-rules ()
      [(_ x)
       (and x (fxzero? x))]))

  (define-constant space-size 1)	; space used for a space
  (define-constant dot-size 2)		; space used for a dotted ending
  (define-constant elip-size 3)		; space used for an elipsis ending
  (define-constant box-size 2)
  (define-constant list-size 2)
  (define-constant vec-size 3)

;;; pretty-print-struct := (simple size obj str)
;;;			:= (container size obj (pps ...))
;;;			:= (ref size obj thing)

  (define make-simple (lambda (a b c) (vector 'simple a b c)))
  (define make-container (lambda (a b c) (vector 'container a b c)))
  (define make-ref (lambda (a b c) (vector 'ref a b c)))

  (define pps->size (lambda (thing) (vector-ref thing 1)))

  (define pretty-print-struct
    (lambda (x-base ge lev len)
      (define pps-pair	
	(lambda (head tail ge lev lslen)
	  (cond
	    [(limit? lslen) (values elip-size '(elipsis))]
	    [(null? tail)
	     (let ([head-pps (pps head ge ge lev)])
	       (values (pps->size head-pps) (list head-pps)))]
	    [(ge tail) =>
	     (lambda (thing)
	       (let ([head-pps (pps head ge ge lev)])
		 (if (limit? (decr lslen))
		     (values (+ (pps->size head-pps) space-size elip-size)
		       (list head-pps 'elipsis))
		     (values (+ (pps->size head-pps) space-size dot-size
			       (thing 'size))
		       (list head-pps 'dot
			 (make-ref (thing 'size) tail thing))))))]
	    [(pair? tail)
	     (let ([head-pps (pps head ge ge lev)])
	       (mv-let ([(tail-size tail-ls)
			 (pps-pair (car tail) (cdr tail) ge lev (decr lslen))])
		 (values (+ (pps->size head-pps) space-size tail-size)
		   (cons head-pps tail-ls))))]
	    [else
	      (let ([head-pps (pps head ge ge lev)])
		(mv-let ([(tail-size tail-ls)
			  (pps-pair tail '() ge lev (decr lslen))])
		  (values (+ (pps->size head-pps) space-size dot-size tail-size)
		    (list* head-pps 'dot tail-ls))))])))

      (define pps-vec
	(lambda (v ge lev lslen)
	  (let ([len (vector-length v)])
	    (if (zero? len)
		(values 0 '())
		(let f ([pos 0] [lslen lslen] [size-acc -1] [acc '()])
		  (cond
		    [(= pos len)
		     (values size-acc (reverse acc))]
		    [(limit? lslen)
		     (values (+ size-acc space-size elip-size)
		       (reverse (cons 'elipsis acc)))]
		    [else
		      (let ([head (pps (vector-ref v pos) ge ge lev)])
			(f (+ pos 1) (decr lslen)
			  (+ space-size size-acc (pps->size head))
			  (cons head acc)))]))))))

      (define pps
	(lambda (x ge next-ge lev)
	  (cond
	    [(ge x) =>
	     (lambda (thing)
	       (make-ref (thing 'size) x thing))]
	    [(box? x)
	     (if (limit? lev)
		 (make-container (+ box-size elip-size) x (list 'elipsis))
		 (let ([contents (pps (unbox x) next-ge next-ge (decr lev))])
		   (make-container (+ (pps->size contents) box-size)
		     x (list contents))))]
	    [(pair? x)
	     (if (limit? lev)
		 (make-container (+ list-size elip-size) x (list 'elipsis))
		 (mv-let ([(size ls) (pps-pair (car x) (cdr x) next-ge (decr lev) len)])
		   (make-container (+ list-size size) x ls)))]
	    [(vector? x)			
	     (if (limit? lev)
		 (make-container (+ vec-size elip-size) x (list 'elipsis))
		 (mv-let ([(size ls) 
			   (pps-vec x next-ge (decr lev) len)])
		   (make-container (+ vec-size size) x ls)))]
	    [else
	      (let ([str (format "~s" x)])
		(make-simple (string-length str) x str))])))
      (pps x-base (lambda (x) #f) ge lev)
;;;      (if (and (not lev) (not len) (really-cyclic? x-base lev len))
;;;	  (make-simple 31 #f "cyclic object (not handled yet)")
;;;	  (pps x-base (lambda (x) #f) ge lev))
      ))

  (define flat-pretty-print-struct
    (lambda (x ge space)
      (define fpps-pair			; returns values: size ls
	(lambda (head tail ge space)
	  (cond
	    [(null? tail)
	     (fpps head ge ge space
	       (lambda (pps)
		 (values (pps->size pps) (list pps)))
	       (lambda ()
		 (values elip-size '(elipsis))))]
	    [(ge tail) =>		
	     (lambda (thing)
	       (fpps head ge ge (- space elip-size space-size)
		 (lambda (pps)
		   (let* ([head-size (pps->size pps)]
			  [thing-size (thing 'size)]
			  [space-left
			    (- space head-size space-size dot-size)])
		     (if (> thing-size space-left)
			 (values
			   (+ head-size space-size elip-size)
			   (list pps 'elipsis))
			 (values
			   (+ head-size space-size dot-size thing-size)
			   (list pps 'dot (make-ref thing-size tail thing))))))
		 (lambda ()
		   (values elip-size '(elipsis)))))]
	    [(pair? tail)
	     (fpps head ge ge (- space elip-size space-size)
	       (lambda (pps)
		 (let ([head-size (pps->size pps)])
		   (mv-let ([(tail-size ls)
			     (fpps-pair (car tail) (cdr tail) ge
			       (- space (+ head-size space-size)))])
		     
		     (values (+ head-size space-size tail-size)
		       (cons pps ls)))))
	       (lambda ()
		 (values elip-size '(elipsis))))]
	    [else
	      (fpps head ge ge (- space elip-size space-size)
		(lambda (pps)
		  (let ([head-size (pps->size pps)])
		    (fpps tail ge ge
		      (- space (+ head-size space-size dot-size))
		      (lambda (pps2)
			(let ([tail-size (pps->size pps2)])
			  (values (+ head-size space-size dot-size tail-size)
			    (list pps 'dot pps2))))
		      (lambda ()
			(values (+ head-size space-size elip-size)
			  (list pps 'elipsis))))))
		(lambda ()
		  (values elip-size '(elipsis))))])))

      (define fpps
	(lambda (x ge next-ge space succ fail)
	  (cond
	    [(ge x) =>
	     (lambda (thing)
	       (let ([thing-size (thing 'size)])
		 (if (<= thing-size space)
		     (succ (make-ref thing-size x thing))	
		     (fail))))]
	    [(box? x)
	     (fpps (unbox x) next-ge next-ge (- space box-size)
	       (lambda (pps)
		 (let ([total-size (+ (pps->size pps) box-size)])
		   (if (<= total-size space)
		       (succ (make-container total-size x (list pps)))
		       (fail))))
	       (lambda ()
		 (let ([fail-size (+ elip-size box-size)])
		   (if (<= fail-size space)
		       (succ (make-container fail-size x (list 'elipsis)))
		       (fail)))))]
	    [(pair? x)
	     (mv-let ([(size ls) (fpps-pair (car x) (cdr x) next-ge
				   (- space list-size))])
	       (let ([total-size (+ size list-size)])
		 (if (<= total-size space)
		     (succ (make-container total-size x ls))
		     (fail))))]
	    [(vector? x)			
	     (if (zero? (vector-length x))
		 (if (> 4 space) (fail) (succ (make-container 4 x '())))
		 (let ([ls (vector->list x)])
		   (mv-let ([(size ls) (fpps-pair (car ls) (cdr ls) next-ge
					 (- space vec-size))])
		     (let ([total-size (+ size vec-size)])
		       (if (<= total-size space)
			   (succ (make-container total-size x ls))
			   (fail))))))]
	    [else
	      (let* ([str (format "~s" x)]
		     [len (string-length str)])
		(if (<= len space)
		    (succ (make-simple len x str))
		    (fail)))])))
      (fpps x (lambda (x) #f) ge space
	(lambda (x) x)
	(lambda ()
	  (make-simple 3 x "...")))))

  )

(simplemodule (exports make-namer)

  (define \#make-eq-hash-table
    (parameterize ([subset-mode 'system])
      (eval '\#make-eq-hash-table)))

  (define \#eq-hash-cell
    (parameterize ([subset-mode 'system])
      (eval '\#eq-hash-cell)))

  (define \#eq-get-hash
    (parameterize ([subset-mode 'system])
      (eval '\#eq-get-hash)))

  (define make-namer
    (lambda ()
      (let ([t (\#make-eq-hash-table 5)]
	    [n 0])
	(case-lambda
	  [(x) (let ([a (\#eq-get-hash t x #f)])
		 a)]
	  [(x name)
	   (let ([name (or name (begin (set! n (+ n 1)) (format "[~s]" n)))])
	     (let ([a (\#eq-hash-cell t x (vector
					    'hooofoo
;					    (create <markup>)
					    #f))])
	       (vector-set! (cdr a) 1 name)))]))))

  )


(simplemodule (exports find-shared really-cyclic?)

  (define \#make-eq-hash-table
    (parameterize ([subset-mode 'system])
      (eval '\#make-eq-hash-table)))

  (define \#eq-hash-cell
    (parameterize ([subset-mode 'system])
      (eval '\#eq-hash-cell)))

;;; with respect to really-cyclic?
  (define find-shared
    (lambda (x lev len)
      (let ([acc '()])
      
	(define do-shared
	  (lambda (x curlev lstlen)
	    (cond
	      [(pair? x) (do-structure x curlev lstlen do-pair)]
	      [(vector? x) (do-structure x curlev 0 do-vector)]
	      [(box? x) (do-structure x curlev 0 do-box)]
	      [(or (procedure? x) (port? x) (gensym? x) (string? x))
	       (do-structure x curlev 0 dont-do)])))

;;; (x . seen-it?)
;;; where seen-it is:  #f (not seen) 'seen, or 'saved

;;; if #f, recurse
;;; if 'seen, save and do nothing
;;; if 'saved, do nothing 

	(define do-structure
	  (let ([ht (\#make-eq-hash-table 23)])
	    (lambda (x curlev lstlen sub-cyclic?)
	      (unless (eq? curlev lev)
		(let ([a (\#eq-hash-cell ht x #f)])
		  (let ([seen-it (cdr a)])
		    (if seen-it
			(unless (eq? seen-it 'saved)
			  (set! acc (cons x acc))
			  (set-cdr! a 'saved))
			(begin
			  (set-cdr! a 'seen)
			  (sub-cyclic? x curlev lstlen)))))))))

	(define dont-do
	  (lambda (x curlev lstlen)
	    (void)))

	(define do-pair
	  (lambda (x curlev lstlen)
	    (unless (eq? lstlen len)
	      (do-shared (car x) (fx+ curlev 1) 0)
	      (do-shared (cdr x) curlev (fx+ lstlen 1)))))

	(define do-vector
	  (lambda (x curlev lstlen)
	    (let ([n (vector-length x)] [curlev (fx+ curlev 1)])
	      (let across ([i (fx- (if len (fxmin len n) n) 1)])
		(when (fx>= i 0)
		  (do-shared (vector-ref x i) curlev 0)
		  (across (fx- i 1)))))))

	(define do-box
	  (lambda (x curlev lstlen)
	    (do-shared (unbox x) (fx+ curlev 1) 0)))

	(do-shared x 0 0)
	acc)))

  (define (really-cyclic? x lev len)

    (define cyclic?
      (lambda (x curlev lstlen)
	(cond
	  [(pair? x) (cyclic-structure? x curlev lstlen cyclic-pair?)]
	  [(vector? x) (cyclic-structure? x curlev 0 cyclic-vector?)]
	  [(box? x) (cyclic-structure? x curlev 0 cyclic-box?)]
	  [else #f])))

    (define cyclic-structure?
      (let ([ht (\#make-eq-hash-table 23)])
	(lambda (x curlev lstlen sub-cyclic?)
	  (and (not (eq? curlev lev))
	       (let ([a (\#eq-hash-cell ht x #f)])
		 (let ([oldlev (cdr a)])
		   (if oldlev
		       (or (not (if (= oldlev curlev) len lev))
			   (sub-cyclic? x curlev lstlen))
		       (begin (set-cdr! a curlev)
			 (or (sub-cyclic? x curlev lstlen)
			     (begin (set-cdr! a #f) #f))))))))))

    (define cyclic-pair?
      (lambda (x curlev lstlen)
	(and (not (eq? lstlen len))
	     (or (cyclic? (car x) (fx+ curlev 1) 0)
		 (cyclic? (cdr x) curlev (fx+ lstlen 1))))))

    (define cyclic-vector?
      (lambda (x curlev lstlen)
	(let ([n (vector-length x)] [curlev (fx+ curlev 1)])
	  (let across ([i (fx- (if len (fxmin len n) n) 1)])
	    (and (fx>= i 0)
		 (or (cyclic? (vector-ref x i) curlev 0)
		     (across (fx- i 1))))))))

    (define cyclic-box?
      (lambda (x curlev lstlen)
	(cyclic? (unbox x) (fx+ curlev 1) 0)))

    (cyclic? x 0 0)

    )

  )

#!eof

(define agglomerate-objects
  (lambda objs
    (vector (lambda (msg self . args)
	      (for-each
		(lambda (object)
		  (apply
		    (safe-instance-dispatch-table object msg)
		    msg object args))
		objs))
      (vector 'class 'ignore-me))))
