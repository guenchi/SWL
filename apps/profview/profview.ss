;; TODO
;;
;;   - shouldn't markup both branches of the IF
;;   - colorize

;; Usage:
;;  (load "profview.ss")
;;  (compile-profile #t) ; enable generation of profiling code
;;  (load "myfile.ss")   ; load (or compile-file) application source files
;;  (run-my-application) ; run your application
;;  (p-view)             ; view profiling information

;; Also useful:
;;  (profile-clear)      ; clear profiling information

(module (p-view)
(import swl:oop)
(import swl:macros)
(import swl:option)
(import swl:generics)
(import swl:threads)

(define-swl-class (<p-text> parent) (<text> parent)
  (ivars (cursor-pos-method (lambda (self index) (void))))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [init (p)
     (send-base self init p)]
    [key-press (key mods)
     (event-case ((key= key) (modifier= mods))
       (([left])  (move-char self -1))
       (([right]) (move-char self  1))
       (([up])    (move-line self -1))
       (([down])  (move-line self  1))
       (([prior]) (vscroll self -1 'pages))
       (([next])  (vscroll self  1 'pages)))
     (cursor-pos-method self (get-cursor-pos self))]
    [set-cursor-pos-method! (method) (set! cursor-pos-method method)]
    [set-cursor-pos! (index)
     (cursor-pos-method self index)
     (send-base self set-cursor-pos! index)]))

;; ast's from expander have source right there, but in later passes,
;; the source is the initial record...
;;  of course prelex's decided to have their source in a diff place.
(define find-source
  (lambda (source)
    (if (vector? source)
        (vector-ref
          source
          (case (vector-ref source 0) [(prelex) 3] [else 1]))
        source)))

(define source-info-filename
  (lambda (src)
    (set! source-info-filename
      (cond
        [(record? src)
         (let* ([source-sfd (record-field-accessor (record-type-descriptor src) 'sfd)]
                [sfd-name (record-field-accessor (record-type-descriptor (source-sfd src)) 'name)])
           (lambda (src) (sfd-name (source-sfd src))))] 
        [(top-level-bound? 'source-file-descriptor-name)
         (lambda (src) (source-file-descriptor-name (cdr x)))]
        [(vector? src)
         (let ()
           (define-structure (source-file-descriptor name))
           (lambda (src) (source-file-descriptor-name (cdr x))))]
        [else (assertion-violationf 'profview "cannot determine how to access source-file name")]))
    (source-info-filename src)))

(define source-info-filepos
  (lambda (src)
    (set! source-info-filepos
      (cond
        [(record? src)
         (record-field-accessor (record-type-descriptor src) 'bfp)]
        [else car]))
    (source-info-filepos src)))

(define boxify!
  (lambda (ranges)
    (unless (null? (cdr ranges))
      (let ([left (apply min (map caar ranges))]
	    [right (apply max (map cadr ranges))])
	(set-car! (cdar ranges) right)
	(let loop ([ranges (cdr ranges)])
	  (let ([next (cdr ranges)])
	    (cond
	     [(null? next)
	      (set-car! (caar ranges) left)]
	     [else
	      (set-car! (caar ranges) left)
	      (set-car! (cdar ranges) right)
	      (loop next)])))))
    ranges))

(define get-ranges
  (lambda (p start)
    (let ([end (begin (file-position p start)
		      (read p)
		      (file-position p))])
      (file-position p start)
      (cons end (get-lines p start end)))))

(define get-lines
  (lambda (p fp end)
    (cond
     [(< fp end)
      (let ([ls (find-start p fp)])
	(call-with-values (lambda () (find-end p (+ ls 1) (+ ls 1) end))
	  (lambda (le fp)
	    (cons (cons ls le) (get-lines p fp end)))))]
     [else '()])))

(define find-start
  (lambda (p fp)
    (let ([c (read-char p)])
      (if (memv c '(#\space #\tab))
	  (find-start p (+ fp 1))
	  fp))))

(define find-end
  (lambda (p last fp end)
    (if (< fp end)
	(let ([c (read-char p)]
	      [fp (+ fp 1)])
	  (cond
	   [(eof-object? c) (values last fp)]
	   [(eqv? c #\newline) (values last fp)]
	   [(memv c '(#\space #\tab)) (find-end p last fp end)]
	   [else (find-end p fp fp end)]))
	(values last end))))

; dump    :==  (record . count)
; record  :==  #(type (offset . source-descriptor) field ...)

(define p-view
  (case-lambda
    [()
     (collect (collect-maximum-generation))
     (p-view ((let () (import $system) $profile-dump)))]
    [(dump)
     (define gather-filedata
       (lambda (dump)
	 (let loop ([dump dump] [table '()])
	   (cond
	    [(null? dump) table]
	    [else
	     (let* ([item (car dump)]
		    [count (cdr item)]
		    [record (car item)]
		    [source (find-source record)])
	       (cond
		[(and source (string? (source-info-filename source)))
		 (let ([filename (source-info-filename source)]
		       [offset (source-info-filepos source)])
		   (let ([hit (assoc filename table)])
		     (cond
		      [hit
		       (set-cdr! hit (add offset count (cdr hit)))
		       (loop (cdr dump) table)]
		      [else
		       (loop (cdr dump)
			     (cons (list filename (cons offset count))
				   table))])))]
		[else (loop (cdr dump) table)]))]))))
     (define add
       (lambda (offset count ls)
	 (let ([x (assv offset ls)])
	   (cond
	    [x (set-cdr! x (+ (cdr x) count)) ls]
	    [else (cons (cons offset count) ls)]))))
     (for-each
      (lambda (x)
	; x = (filename . ((offset . count) ...))
	(let ([p (open-input-file (car x))])
	  (make-profview
	   p
	   (map
	    (lambda (x)
	      (list* (car x) (cdr x) (get-ranges p (car x))))
	    (sort (lambda (x y) (< (car x) (car y))) (cdr x))))))
      (gather-filedata dump))]))

(define make-profview
  (lambda (ip data)
    ; data = ((start count end (left right) ...) ...)
    ;   sorted by increasing start offset
    (let* ((filename (port-name ip))
	   (top (create <toplevel> with (title: filename)))
	   (sf2 (create <scrollframe> top))
	   (txt (create <p-text> sf2 with (wrap: 'none) (width/char: 80)))
	   (f1  (create <frame> top))
	   ;(f2  (create <frame> f1))
	   ;(bback (create <button> f2 with (title: "<") (enabled: #f)))
	   ;(bfore (create <button> f2 with (title: ">") (enabled: #f)))
	   (cnt (create <label> f1 with (title: "Key:")))
	   (sf1 (create <scrollframe> f1))
	   (dun (create <button> f1 with (title: "Done")
			(action: (lambda (button) (destroy top)))))
	   (sel (create <p-text> sf1 with (wrap: 'none)
			(width/char: 10) (height/char: 3)))
	   (cnts (let loop ([ls (sort < (map cadr data))] [last #f])
		   (if (null? ls)
		       '()
		       (let ([x (car ls)])
			 (if (eqv? x last)
			     (loop (cdr ls) last)
			     (cons x (loop (cdr ls) x))))))))
      (define colorize
	(let loop ([ls cnts] [alst '()] [i 0]
			     [n (let ([n (length cnts)])
				  (if (< n 2)
				      1
				      (- n 1)))])
	  (if (not (null? ls))
	      (loop (cdr ls) (cons (cons (car ls) (/ i n)) alst)
		    (+ i 1) n)
	      (lambda (cnt)
		(let ([x (cdr (assq cnt alst))])
		  '(let ([c (+ (round (* x 127)) 128)])
		     (make <rgb> c c c))
		  (let ([shift (lambda (x)
				 (+ (round (* x 63)) 192))]
			[x (* x 7)])
		    (let ([red (cond
				[(< x 1) x]
				[(< x 2) 1]
				[(< x 3) (- 3 x)]
				[(< x 5) 0]
				[(< x 6) (- x 5)]
				[else 1])]
			  [green (cond
				  [(< x 1) 0]
				  [(< x 2) (- x 1)]
				  [(< x 4) 1]
				  [(< x 5) (- 5 x)]
				  [(< x 6) 0]
				  [else (- x 6)])]
			  [blue (cond
				 [(< x 3) 0]
				 [(< x 4) (- x 3)]
				 [else 1])])
		      (make <rgb> (shift red) (shift green) (shift blue))))
		  )))))
      (define offset->pos
	(lambda (offset)
	  (add-offset txt '(0 . 0) offset)))
      (define find-closest
	(lambda (index)
	  (let loop ([ls data] [last #f])
	    (if (not (null? ls))
		(let ([x (car ls)])
		  (cond
		   [(pos<=? txt (offset->pos (car x)) index)
		    (if (pos>? txt (offset->pos (caddr x)) index)
			(loop (cdr ls) (cadr x))
			(loop (cdr ls) last))]
		   [else last]))
		last))))
      (pack cnt (side: 'top))
      (pack sf1 (side: 'top) (expand: #t) (fill: 'both))
      ;(pack bback (side: 'left) (fill: 'x))
      ;(pack bfore (side: 'left) (fill: 'x))
      ;(pack f2 (anchor: 'center) (side: 'top) (fill: 'none))
      (pack f1 (side: 'left) (fill: 'y))
      (pack sf2 (side: 'left) (fill: 'both) (expand: #t))
      (pack dun (side: 'top) (fill: 'none))
      (let ((buf (make-string 2048)))
	(file-position ip 0)
	(let loop ((ip ip))
	  (let ((x (block-read ip buf 2048)))
	    (unless (eof-object? x)
	      (insert-at txt 'end (if (< x 2048) (substring buf 0 x) buf))
	      (loop ip)))))
      (close-input-port ip)
      (let loop ([ls cnts])
	(unless (null? ls)
	  (let ([cnt (car ls)])
	    (define (pad x)
	      (let ([s (number->string x)])
		(let ([l (string-length s)])
		  (if (< l 10)
		      (string-append (make-string (- 10 l) #\space) s)
		      s))))
	    (insert-at sel '(0 . 0) (if (eq? ls cnts)
					(pad cnt)
					(string-append (pad cnt) "
")))
	    (apply-markup (create <markup> with
				  (background-color: (colorize cnt)))
			  sel '(0 . 0) '(10 . 0))
	    (loop (cdr ls)))))
      (for-each
       (lambda (x)
	 (let ([count (cadr x)]
	       [color (colorize (cadr x))]
	       [ranges (cdddr x)])
	   (let ([mkup (create <markup> with
			       (border-width: 1)
			       (relief: 'groove)
			       (mouse-press-method:
				(lambda (mkup txt x y mods)
				  (critical-section
				   (set-title! cnt (number->string count))
				   (set-background-color! cnt color))))
			       (background-color: color))])
	     (for-each
	      (lambda (range)
		(apply-markup mkup txt (car range) (cdr range)))
	      (boxify!
	       (map (lambda (range)
		      (cons (offset->pos (car range))
			    (offset->pos (cdr range))))
		    ranges))))))
       data)
      '(send txt set-cursor-pos-method!
	(let ([def (get-background-color cnt)])
	  (lambda (self index)
	    (let ([x (find-closest index)])
	      (cond
	       [(integer? x)
		(set-title! cnt (number->string x))
		(set-background-color! cnt (colorize x))]
	       [else
		(set-title! cnt "")
		(set-background-color! cnt def)])))))
      )))
)
