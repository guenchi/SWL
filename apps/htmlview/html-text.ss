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
;; Bugs
;;
;; button/image wgts in table cells are parented to the scrollframe so that
;; they can later be reinserted into the visible widget.  This messes up
;; scrolling (the b/i wgt bleeds out of the visible wgt's viewport).
;;
;; Possible fixes: could create two wgts (one for each of hidden and visible
;; text wgts) but this seems like overkill.  Could try to fix text wgt/
;; scrollframe wgt so that bleed doesn't happen (Tk 4.2 bug?).
;;

(require "../common/app-text.ss" 'once)
(require "../common/flex-button.ss" 'once)


(define-swl-class (<html-text> parent) (<app-text> parent)
  (ivars
   (viewer #f)
   (hidden-wgt? #f) ;;; #t if this is the hidden html-text widget
   (hidden-wgt #f) ;;; the actual hidden widget
   (visible-wgt #f) ;;; the actual visible widget
   (inserted-wgt-offsets '())
   (oport #f)
   (iport #f)
   (redirection-level #f)
   (mprocs #f) ;;; flushable markup procs (delayed markups)
   (cprocs #f) ;;; handles wgt size changes
   (hidden-mprocs #f) ;;; mprocs created by the hidden widget
   (pending-mprocs #f) ;;; markup procs held pending level-0 end-tag
   (this-line #f) ;;; number of line currently being output to generic port
   (chars-this-line #f) ;;; current char offset within this-line
   [font-sizes #7(6 8 10 12 14 18 24)]
   [tag-nesting-level #f]
   [table-hidden-lines 10] ;;; nbr table lines processed in the hidden widget
   [table-segment-lines 50]
   [table-segment-count #f]
   [table-between-rows? #f]
   [table-widths #f]
   [table-indexes #f]
   [table-col #f]
   [table-row #f]
   [table-nowrap #f]
   [table-wgts #f]
   [attribute-stack #f]
   [H1-above 8]  [H1-below 9] [H1-lm 0]
   [H2-above 7]  [H2-below 8] [H2-lm 0]
   [H3-above 6]  [H3-below 7] [H3-lm 0]
   [H4-above 3]  [H4-below 6]  [H4-lm 10]
   [H5-above 3]  [H5-below 5]  [H5-lm 15]
   [H6-above 3]  [H6-below 4]  [H6-lm 20]
   [P-above 4]   [P-below 4]
   [font-cache '()]
   [markup-cache '()]
   [background-font #f]
   [bullet "*"]
   [after-table-idx #f] ;;; 1st pos after end of last table
   [pre-mode? #f]
   [anchors #f]
   [menu-tags '(UL MENU DIR OL ul menu dir ol)]
   )
  (inherited mini-buffer key-prefix end-mk handle)
  (inheritable mini-buffer key-prefix end-mk handle)
  (private
   [html-color? (str)
     (and (string? str)
	  (fx= (string-length str) 7)
	  (char=? (string-ref str 0) #\#)
	  (string->number (substring str 1 7) 16))]

   [get-x (pos)
     (send self make-visible pos)
     (let ((x (send self bounding-box pos)))
       (unless x
	 (assertion-violationf '<html-text>:get-x "bounding box failed for pos: ~s" pos))
       (car x)
       )]

   [vector-stretch (vec newlen)
     (let ([new (make-vector newlen)]
	   [len (vector-length vec)])
       (unless (vector? vec)
	 (assertion-violationf 'vector-stretch "vec is not a vector: ~s" vec))
       (do ((i 0 (fx1+ i)))
	   ((fx= i len))
	 (vector-set! new i (vector-ref vec i)))
       (when (positive? len)
	 (let ((last (vector-ref vec (1- len))))
	   (do ((i len (fx1+ i))) ;;; assign last elt to all new cells
	       ((fx= i newlen))
	     (vector-set! new i last))))
       new)]

   [set-hidden-html-text! (tx) (set! hidden-html-text tx)]

   [html-out (val)
     (if (fxpositive? redirection-level)
	 (display val (top-val 'oport))
	 (display val oport))]

   [html-error (str)
     (html-out #\newline)
     (html-out str)
     (html-out #\newline)]

   [html-flush ()
     (if (fxpositive? redirection-level)
	 (flush-output-port (top-val 'oport))
	 (flush-output-port oport))]

   [display-token (tok)
     (if (string? tok)
	 (unless table-between-rows? ;;; disallow text between table rows
	   (html-out tok))
	 (case (token-type tok)
	   [(start-tag) (send self handle-start-tag tok)]
	   [(end-tag) (send self handle-end-tag tok)]
	   [(entity-name)
	    (let ((val (token-value tok)))
	      (cond
		[(string=? val "nbsp") (html-out " ")] ;;; temp hack
		[(string=? val "copy") (html-out "(c)")] ;;; temp hack
		[(string=? val "lt") (html-out "<")]
		[(string=? val "gt") (html-out ">")]
		[else (html-out (string-append "&" val ";"))]))]
	   [(decl) (void)]
	   [else
            (when (swl:bug) (fprintf (swl:bug-port) "unknown token type for token: ~s~n" tok))]))]

   [push! (name val)
     (set! attribute-stack (cons (cons name val) attribute-stack))]

   [pop! ()
     (if (null? attribute-stack)
	 #f
	 (let ((t (car attribute-stack)))
	   (set! attribute-stack (cdr attribute-stack))
	   t))]

   [pop-until! (name) ;;; returns stack items up to and including name
     (let f ((ls (list (pop!))))
       (if (or (not (car ls)) (eq? (caar ls) name))
	   (reverse ls)
	   (f (cons (pop!) ls))))]
   
   [top ()
     (if (pair? attribute-stack)
	 (car attribute-stack)
	 #f)]

   [top-val (name)
     (let ((x (assq name attribute-stack)))
       (if x
	   (cdr x)
	   #f))]

   [nested-val (name)
     (let f ((s attribute-stack) (num 1))
       (if (pair? s)
	   (let ((x (car s)))
	     (if (eq? name (car x))
		 (if (positive? num)
		     (f (cdr s) (1- num))
		     (cdr x))
		 (f (cdr s) num)))
	   #f))]

   [on-stack? (ls) (ormap (lambda (x) (top-val x)) ls)]

   [count-items (items)
     (let f ((i 0) (attrs attribute-stack))
       (if (null? attrs)
	   i
	   (if (memq (caar attrs) items)
	       (f (fx1+ i) (cdr attrs))
	       (f i (cdr attrs)))))]

   [get-attr! (name attrs)
      (let loop ([ls attrs])
	(if (null? ls) 
	    #f
	    (let ([attr (car ls)])
	      (if (pair? attr)
		  (if (eq? (car attr) name)
		      (begin
			(set-car! ls #f)
			(cdr attr))
		      (loop (cdr ls)))
		  (if (eq? attr name)
		      (begin
			(set-car! ls #f)
			attr)
		      (loop (cdr ls)))))))]
	   
   [reduce1 (ls)
     (let loop ((ls ls))
       (if (null? ls)
	   '()
	   (if (eq? (car ls) #f)
	       (loop (cdr ls))
	       (cons (car ls) (loop (cdr ls))))))]

   [flush-mprocs ()
    (if hidden-wgt?
	(set! hidden-mprocs
	  (append hidden-mprocs 
		  (let f ((mprocs mprocs))
		    (if (null? mprocs)
			'()
			(let ((x ((car mprocs) self))) ;; need l-r eval order
			  (cons x (f (cdr mprocs))))))))
	(for-each (lambda (mproc) (mproc self)) mprocs))
       (set! mprocs '())]

   [html-line-break () (html-out #\newline)]

   [html-vskip (a b)
     (push! 'above-paragraph-space a)
     (push! 'below-paragraph-space b)]

   [index-of-output () (cons chars-this-line this-line)]

   [open-html-output-port ()
     (let* ([ob (make-string 2000)]
	    [buflen (string-length ob)]
	    [idx 0])
       ;;
       ;; don't use trace-lambda unless trace output port properly bound
       ;;
       (define widget-write
	 (lambda (b i)
	   (if (eqv? (string-length b) i)
	       (send self insert b)
	       (send self insert (substring b 0 i)))
	   (flush-mprocs)))
       (define handler
	 (lambda (xmsg . xargs)
	   (record-case (cons xmsg xargs)
	     [write-char (c p) ;;; always called
	       (disable-interrupts)
	       (string-set! ob idx c)
	       (set! idx (fx1+ idx))
	       (when (char=? c #\newline)
		 (set! this-line (fx1+ this-line))
		 (set! chars-this-line -1))
	       (set! chars-this-line (fx1+ chars-this-line))
	       (when (fx= idx buflen)
		 (widget-write ob buflen)
		 (set! idx 0))
	       (enable-interrupts)]
	     [flush-output-port (p)
	       (critical-section
		 (when (fx> idx 0)
		   (widget-write ob idx)
		   (set! idx 0))
		 (flush-mprocs))
	       (swl:raw-tcl-eval "update") ;;; hack needed for bounding box
	       ]
	     [close-port (p)
	       (flush-output-port p)
	       (set-port-output-size! p 0)
	       (mark-port-closed! p)]
	     [port-name (p) "html-output"]
	     [clear-output-port (p) (set! idx 0)]
	     [else
	      (assertion-violationf 'html-port "operation ~s not handled" xmsg)])))
	
       (let ([p (make-output-port handler ob)])
	 ;;; (set-port-output-size! p (fx1- (string-length ob)))
	 (set-port-output-size! p 0) ;;; explicit buffering
	 (set! this-line 0)
	 (set! chars-this-line 0)
	 p))]

    [port-alive? () (and oport (not (port-closed? oport)))]

    [bump-tag-nesting-level (tag val)
      ;;
      ;; this is very kludgey, and also error prone if we forget to add
      ;; new entries to the list below. Should solve this another way.
      ;;
      (unless (memq tag '(TR TH TD P HR BODY HTML HEAD TABLE TABLESEGMENT
                          tr th td p hr body html head table tablesegment))
	(set! tag-nesting-level (+ tag-nesting-level val)))]

    [process-hidden-widget (i)
      (let ((rowvec (vector-ref table-indexes i)))
	(let f ((j 0) (cur-x #f))
	  (when (< j table-col)
	    (let* ((i1 (vector-ref rowvec j))
		   (i2 (vector-ref rowvec (1+ j)))
		   (left-x (if cur-x cur-x 5))
		   (right-x (get-x i2))
		   (width (- right-x left-x))
		   (owidth (vector-ref table-widths j)))
;;;(printf "process-hidden-widget: i=~s j=~s i1=~s i2=~s width=~s~n"
;;;   i j i1 i2 width)
	      (when (> width owidth)
		(vector-set! table-widths j width))
	      (f (1+ j) right-x)))))]

    [image-file-type (filename)
     (let ([s (make-string 512)]
	   [ip (open-input-file filename)])
       (let ([n (block-read ip s 512)])
	 (close-input-port ip)
	 (cond
	   [(eof-object? n) 'empty]
	   [(and (>= n 6) (string-ci=? (substring s 0 3) "GIF"))
	    (read (open-input-string (substring s 0 5)))]
	   [(and (>= n 10) (string-ci=? (substring s 6 10) "JFIF"))
	    'jpeg]
	   [(and (>= n 8) (string-ci=? (substring s 0 8) "#DEFINE "))
	    'bitmap]
	   [(let loop ([i 0])
	      (if (fx= i n)
		  #t
		  (and (< 0 (char->integer (string-ref s i)) 128)
		       (loop (fx+ i 1)))))
	    'ascii]
	   [else 'data])))]

    ) ;;; end private

  (protected ;;;        ** ** PROTECTED ** **

    )
  
  (public ;;;            ** ** PUBLIC ** **

   [apply-background-color ()
     (send self set-background-color! (top-val 'background-color))]

   [get-after-table-idx () after-table-idx]

   [set-index-of-output! (i)
     (set! chars-this-line (car i))
     (set! this-line (cdr i))]

   [set-table-segment-count! (c) (set! table-segment-count c)]

   [set-iport! (ip) (set! iport ip)]

   [prep-hidden (vis-wgt vwr base-url fnt)
     (set! hidden-wgt? #t)
     (set! visible-wgt vis-wgt)
     (set! viewer vwr)
     (send self delete-all)
     (let ((hidden-height (+ table-hidden-lines 20))) ;;; hack needed to keep
       (send self set-height/char! hidden-height)) ;;; bounding-box working
     (send self set-cursor-pos! '(0 . 0))
     (send self set-font! fnt)
     (send self prep-html-text base-url fnt)
     ]

   [prep-visible (vwr base-url fnt)
     (set! hidden-wgt? #f)
     (set! visible-wgt self)
     (set! viewer vwr)
     (send self prep-html-text base-url fnt)
     ]

   [prep-html-text (base-url fnt)
     (set! oport (open-html-output-port))
     (set! attribute-stack '())
     (push! 'base-font 3)
     (push! 'base-url base-url)
     (push! 'left-margin 0)
     (push! 'link-color "#0000ff")
     (set! background-font fnt)
     (set! after-table-idx '(0 . 0))
     (set! anchors '())
     (set! mprocs '())
     (set! cprocs '())
     (set! hidden-mprocs '())
     (set! pending-mprocs '())
     (set! redirection-level 0)
     (set! tag-nesting-level 0)
     ]
   
   [insert (what)
     (if hidden-wgt?
	 (send-base self insert what)
	 ;;
	 ;; insert-at doesn't move viewport
	 ;;
	 (send self insert-at 'insert what))]

   [insert-widget-at (idx wgt)
     (send-base self insert-widget-at idx wgt)
     (set! inserted-wgt-offsets
       (cons (send self index->offset idx) inserted-wgt-offsets))
     (set! chars-this-line (1+ chars-this-line))]

   [get-string (beg-idx end-idx)
     ;; default get-string returns empty character for each embedded
     ;; widget.  Modify so that each embedded widget is represented
     ;; by a single space in the returned string.
     (define get-offsets
       (lambda ()
	 (let ((bi (send self index->offset beg-idx))
	       (ei (send self index->offset end-idx)))
	   (sort < 
		 (let f ((os inserted-wgt-offsets) (acc '()))
		   (if (null? os)
		       acc
		       (let ((offset (car os)))
			 (if (and (>= offset bi) (<= offset ei))
			     (f (cdr os) (cons (- offset bi) acc))
			     (f (cdr os) acc)))))))))

     (let* ((str (send-base self get-string beg-idx end-idx))
	    (strlen (string-length str)))
       (if (null? inserted-wgt-offsets)
	   str
	   ;; insert a space for each embedded widget
	   (apply string-append
		  (cdr
		   (let f ((start 0)
			   (prev-ofs 0)
			   (offsets (get-offsets)))
		     (if (null? offsets)
			 (list " " (substring str start strlen))
			 (let* ((new-ofs (car offsets))
				(end (+ start (- new-ofs prev-ofs))))
                           (if (fx< new-ofs prev-ofs)
                               (f end prev-ofs (cdr offsets))
  			       (cons " "
			             (cons (substring str start end)
				       (f end
					  (1+ new-ofs);; skip 1 space for wgt
					  (cdr offsets))))))))))))]

   [set-cursor-pos! (ix)
     ;; permit cursor changes only when output is not occurring.
     (unless iport
       (send-base self set-cursor-pos! ix)
       )
     ]

   [key-press-no-prefix (key mods)
      ;; permit only cursor movement keystrokes, and only when
      ;; output is not occurring.
      (unless iport
	(event-case ((key= key) (modifier= mods))
		    [([alt #\q]) (send viewer destroy)]
		    [([up]) (send self vscroll -1 'units)]
		    [([down]) (send self vscroll 1 'units)]
		    [([left]) (send self hscroll -1 'units)]
		    [([right]) (send self hscroll 1 'units)]
		    [([control #\n] [down]
		      [control #\p] [up]
		      [control #\b] [left]
		      [control #\f] [right]
		      [control #\a]
		      [control #\e]
		      [control #\r]
		      [control #\s]
		      [home] [end] [page_up] [page_down]
		      )
		     (send-base self key-press-no-prefix key mods)])
	)
      ]

      
   [do-table ()
      (pop-until! 'html-tag) ;;; hidden widget handles this tag
      (set! table-nowrap #f)
      (html-out #\newline)
      (html-flush)
      
      (unless hidden-wgt
	(critical-section
	  (set! hidden-wgt (<html-text>:create viewer))
	  (hide hidden-wgt)
	  ))
      (send hidden-wgt prep-hidden self
	    viewer
	    (top-val 'base-url)
	    (send self get-font))
      ;;
      ;; pad hidden wgt to length of master wgt so that later the
      ;; hidden wgt markup indexes will also be valid in self
      ;; (see handle-end-tag).
      ;;
      (send hidden-wgt insert
	    (send self get-string
		  (send hidden-wgt get-after-table-idx)
		  end-mk))
      (send hidden-wgt set-index-of-output! (index-of-output))
      (send hidden-wgt set-iport! iport)
      (send hidden-wgt set-table-segment-count! 0)
      (send hidden-wgt prep-table)
      (send hidden-wgt handle-start-tag
	    (make-token 'start-tag (list 'TABLE)))
      (send hidden-wgt handle-start-tag ;;; start the 1st segment
	    (make-token 'start-tag (list 'TABLESEGMENT)))
      (send hidden-wgt read-and-display-table)
      (send hidden-wgt set-iport! #f) ;;;debug -remove later
    ]

   [prep-table ()
     (set! table-indexes (make-vector table-hidden-lines))
     ;; 1 elt initial vector handles null row boundary condition.
     (do ((i 0 (1+ i))) ((= i table-hidden-lines))
       (vector-set! table-indexes i '#1((0 . 0))))
     (set! table-widths '#1(0))
     (push! 'left-margin 0)
     (set! table-row -1)
     (set! table-wgts '())]
  
   [read-and-display-table ()
     (let f ((tok (read-html-token iport #f)))
       (if (eof-object? tok)
	   (html-error "premature eof in table.")
	   (begin
	     (display-token tok)
	     (unless (and (token? tok)
			  (eq? (token-type tok) 'end-tag)
			  (eq? (token-value tok) 'table))
	       (f (read-html-token iport #f))))))
     ]

   [read-and-display-html (ip)
     (set! iport ip)
     (let f ((tok (read-html-token ip pre-mode?)))
       (if (eof-object? tok)
	   #f
	   (begin
	     (display-token tok)
	     (f (read-html-token ip pre-mode?)))))
     ;;
     ;; generate end tags for any dangling start tags
     ;;
     (let f ((html-tag (top-val 'html-tag)))
       (when html-tag
;;;(printf "Genning end tag for dangling start tag: ~s~n" html-tag)
	 (send self handle-end-tag (make-token 'end-tag html-tag))
	 (f (top-val 'html-tag))))
     (html-flush)
     (set! iport #f) ;;; signals cursor changes permitted
     ]

   [handle-start-tag (t)
     (let* ([tval (token-value t)]
	    [html-tag (car tval)]
	    [attrs (cdr tval)])

;;;(printf "handle-start-tag! tval=~s nesting=~s hidden?=~s stack=~s~n"
;;;	tval tag-nesting-level hidden-wgt? attribute-stack)
       ;;
       ;; if incoming tag is a separator (e.g., <P>) and a paragraph is
       ;; on the stack, generate end-tag while preserving the intervening
       ;; stack fragment (so that, e.g., font declarations are preserved).
       ;;
       (unless (memq html-tag '(I B TT i b tt)) ;;; some common non-separators
	 (when (memq html-tag
		     '(p table br hr ul menu dir ol li h1 h2 h3 h4 h5 h6
                       P TABLE BR HR UL MENU DIR OL LI H1 H2 H3 H4 H5 H6))
	   (let ((tag (on-stack? '(p P))))
	     (when tag
	       (let ((stack-frag 
		      (let f ((frag '()))
			(if (not (eq? tag (top-val 'html-tag)))
			    (begin
			      (f (append frag (pop-until! 'html-tag))))
			    frag))))
		 (send self handle-end-tag (make-token 'end-tag tag))
		 (set! attribute-stack (append stack-frag attribute-stack))
		 )))))

       (push! 'html-tag html-tag)
       (push! html-tag html-tag) ;;; value as key
       (bump-tag-nesting-level html-tag 1)
       (push! 'index (index-of-output))

       (case html-tag
	 [(i var em cite I VAR EM CITE) (push! 'italic #t)]
	 [(tt samp kbd code TT SAMP KBD CODE) (push! 'fixed-width #t)]
	 [(b strong B STRONG) (push! 'bold #t)]
	 [(a A) (let ([href (get-attr! 'href attrs)]
		    [name (get-attr! 'name attrs)])
		(when href
		  (push! 'link (parse-url href))
		  ;; (printf "href=~s link val=~s~n" href (top))
		  )
		(when href (push! 'underline #t))
		(when href (push! 'color (top-val 'link-color)))
		(when name (push! 'anchor name)))]
	 [(address ADDRESS)
	  (html-line-break)
	  (push! 'italic #t)]
	 [(base BASE) (push! 'base-url (get-attr! 'href attrs))]
	 [(blockquote BLOCKQUOTE)
	  (html-line-break)
	  (push! 'italic #t)
	  (push! 'left-margin (+ (top-val 'left-margin) 8))]
	 [(body BODY)
	  (let ([lnk (get-attr! 'link attrs)]
		[vlnk (get-attr! 'vlink attrs)]
		[txt (get-attr! 'text attrs)]
		[background (get-attr! 'background attrs)]
		[bgcolor (get-attr! 'bgcolor attrs)]
		;; [leftmargin (get-attr! 'leftmargin attrs)]
		;; [topmargin (get-attr! 'topmargin attrs)]
		;; [textmargin (get-attr! 'text attrs)]
		)
	    (when (html-color? vlnk) (push! 'vlink-color vlnk))
	    (when (html-color? lnk) (push! 'link-color lnk))
	    (when (html-color? txt) (push! 'color txt))
	    (when (html-color? bgcolor)
	      (push! 'background-color (tk->color bgcolor))
	      (set-background-color! self (tk->color bgcolor)))
	    (when background
	      (send self html-image
		    (parse-url background) "file not found" #f #f #f))
	    )]
	 [(br BR)
	  (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	  (html-line-break)]
	 [(center CENTER)
	  (html-line-break)
	  (push! 'line-format 'center)
	  (html-out #\tab)]
	 [(dd DD)
	  (html-line-break)
	  (html-out #\tab)]
	 [(dl DL)
	  (let ([compact (get-attr! 'compact attrs)])
	    (push! 'line-format 'DL))]
	 [(dt DT) (html-line-break)]
	 [(font FONT)
	  (let ([size (get-attr! 'size attrs)]
		[base-font (top-val 'base-font)])
	    (let ([size (cond
			  [(char=? (string-ref size 0) #\+)
			   (+ base-font
			      (string->number
			       (substring size 1 (string-length size))))]
			  [(char=? (string-ref size 0) #\-)
			   (- base-font
			      (string->number
			       (substring size 1 (string-length size))))]
			  [(string->number size) => (lambda (n) (1- n))]
			  [else base-font])])
	      (when (and (>= size 0) (<= size 6))
		(push! 'base-font size))))]
	 [(form FORM)
	  (let ([action (get-attr! 'action attrs)]
		[method (get-attr! 'method attrs)])
; wonder what John meant for this to do
;	    (flush-form)
	    (when (and action method)
	      (set! form-action action)
	      (set! form-method method)
	      (set! form-env '()))
	    (void))]
	 [(h1 H1)
	  (push! 'left-margin H1-lm)
	  (html-line-break)
	  (html-vskip H1-above H1-below)
	  (push! 'base-font 5)
	  (push! 'bold #t)]
	 [(h2 H2)
	  (push! 'left-margin H2-lm)
	  (html-line-break)
	  (html-vskip H2-above H2-below)
	  (push! 'base-font 5)
	  (push! 'bold #t)]
	 [(h3 H3)
	  (push! 'left-margin H3-lm)
	  (html-line-break)
	  (html-vskip H3-above H3-below)
	  (push! 'base-font 4)
	  (push! 'bold #t)]
	 [(h4 H4)
	  (push! 'left-margin H4-lm)
	  (html-line-break)
	  (html-vskip H4-above H4-below)
	  (push! 'base-font 3)
	  (push! 'bold #t)]
	 [(h5 H5)
	  (push! 'left-margin H5-lm)
	  (html-line-break)
	  (html-vskip H5-above H5-below)
	  (push! 'base-font 3)
	  (push! 'bold #t)
	  (push! 'italic #t)]
	 [(h6 H6)
	  (push! 'left-margin H6-lm)
	  (html-line-break)
	  (html-vskip H6-above H6-below)
	  (push! 'base-font 3)
	  (push! 'italic #t)]
	 [(head html meta link HEAD HTML META LINK) (void)]
	 [(hr HR)
	  (html-line-break)
	  (html-line-break)
	  (html-flush)
	  (let* ([rule (create <frame> (send self get-parent) with
			       (border-width: 1) 
			       (height: 4)
			       (width: (max 1 (- (get-width self) 10)))
			       (relief: 'sunken))]
		 [cproc
		  (lambda (w h) (send rule set-width! (max 1 (- w 10))))]
		 )
	    (if hidden-wgt?
		(set! table-wgts
		  (cons (cons rule (index-of-output)) table-wgts))
		(begin 
		  (send self insert-widget-at (index-of-output) rule)
		  (set! cprocs (cons cproc cprocs))
		  ))
	    (html-line-break)
	    )]
	 [(img IMG)
	  (let ([src (get-attr! 'src attrs)]
		[a (or (get-attr! 'alt attrs) "UNSUPPORTED IMAGE TYPE")]
		[w (get-attr! 'width attrs)]
		[h (get-attr! 'height attrs)]
		[b (get-attr! 'border attrs)])
	    (cond
	      [src (send self html-image
			 (merge-url (top-val 'base-url) (parse-url src))
			 a w h b)]
	      [else (html-out (format "[~a]" a))]))]
	 [(li LI)
	  (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	  (unless (on-stack? menu-tags)
	    (when (swl:bug) (fprintf (swl:bug-port) "LI without start list tag~n"))
	    (send self handle-start-tag (make-token 'start-tag (list 'UL))))
	  (html-line-break)
	  (if (number? (top-val 'list-item-num))
	      (let ((next-num (1+ (top-val 'list-item-num))))
		(push! 'list-item-num next-num)
		(html-out (format "~s." next-num)))
	      (html-out bullet))
	  (html-out #\tab)
	  (push! 'p-left-margin (top-val 'tab1))
          ]
	 [(ul menu dir ol UL MENU DIR OL)
	  (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	  (when (and (= (count-items menu-tags) 1)
		     (not (on-stack? '(above-paragraph-space))))
	    (html-line-break))
	  (push! 'line-format html-tag)
	  (push! 'left-margin (+ (top-val 'left-margin) 30))
	  (if (memq html-tag '(ol OL))
	      (push! 'list-item-num 0)
	      (push! 'tab1 (+ (top-val 'left-margin) 15)))
	  ]
	 [(p P)
	  (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	  (html-line-break)
	  (html-vskip P-above P-below)
	  ]
	 [(pre PRE)
          (push! 'fixed-width #t)
          (push! 'pre #t)
	  (set! pre-mode? #t)]
	 [(table tablesegment TABLE TABLESEGMENT)
	  ;;
	  ;; Support for subset of html tables (non-nestable)
	  ;;
	  ;; Tables are tricky.  We render the table in two passes:
	  ;;
	  ;; In pass one we redirect rendering to the off-screen
	  ;; hidden widget, and use this to compute the exact pixel
	  ;; values for table columns (markup tabs).
	  ;;
	  ;; In pass two we copy the text from the hidden widget to the
	  ;; live widget, reapplying any markups created while the hidden
	  ;; widget was active. Then we apply a markup for the entire table
	  ;; that contains the computed tabs.
	  ;;
	  ;; If a table is longer than table-segment-lines, we partition
	  ;; the table into multiple segments, rendering each segment
	  ;; separately, then go back and apply a table tab markup for the
	  ;; entire table. (this is done to get something on the screen
	  ;; quickly for long tables.)  Otherwise, we render the table
	  ;; as a single segment.
	  ;;
	  ;; Images and horizontal rules must be rendered as inserted
	  ;; widgets.  This complicates tables still further.  Each inserted
	  ;; widget and its index is saved for later insertion into the
	  ;; visible widget after the table-segment text has been rendered
	  ;; in the visible widget.
	  ;;
	  (if hidden-wgt?
	      (begin
		(when (nested-val 'TABLE)
		  (begin
		      (html-error "ERROR: nested tables are not allowed.")
		      (let f ((tok (read-html-token iport #f)))
			(if (eof-object? tok)
			    (html-error "ERROR: premature eof in table.")
			    (unless (and (token? tok)
					 (eq? (token-type tok) 'end-tag)
					 (eq? (token-value tok) 'table))
			      (f (read-html-token iport #f)))))
		      (pop-until! 'html-tag) ;;; clean up stack
		      )
		  ;; broken attempt at nested tables
		  ;;(let ((nested-wgt
		  ;;        (<html-text>:create (send self get-parent))))
		  ;;  (send nested-wgt set-iport! iport)
		  ;;  (send nested-wgt set-height/char! 24)
		  ;;  (send self insert-widget-at 'end nested-wgt)
		  ;;  (send nested-wgt prep-visible
		  ;;	  viewer
		  ;;	  (top-val 'base-url)
		  ;;	  (send self get-font))
		  ;;  (send nested-wgt do-table)
		  ;;  (send nested-wgt set-iport! #f) ;;; debug -remove later
		  ;;  )
		  )
		)
	      (send self do-table))
	  ]
	 [(tr TR)
	  (set! table-between-rows? #f)
	  (when (eqv? table-segment-count table-segment-lines)
;;;(printf "genning pseudo table tags!!!! stack=~s~n" attribute-stack)
	    (pop-until! 'html-tag) ;;; back out the TR stack items
	    (send self handle-end-tag (make-token 'end-tag 'TABLESEGMENT))
	    (send self handle-start-tag
		  (make-token 'start-tag (list 'TABLESEGMENT)))
	    (push! 'html-tag 'TR) ;;; restore TR stack items
	    (push! 'TR 'TR)
	    (push! 'index (index-of-output))
	    (set! table-segment-count 0))
	  (set! table-segment-count (1+ table-segment-count))
	  (set! table-row (1+ table-row))
	  (set! table-col -1)]
	 [(td th TD TH)
	  (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	  (set! table-col (1+ (or table-col -1)))
	  (when (eq? html-tag 'th) (push! 'bold #t))
	  (let ((nowrap (get-attr! 'nowrap attrs)))
	    (when nowrap (set! table-nowrap #t))
	    (unless (= table-col 0) (html-out #\tab))
	    (let ((align (get-attr! 'align attrs)))
	      (when (= table-col (vector-length table-widths))
		(do ((i 0 (1+ i))) ((= i table-hidden-lines))
		  (vector-set! table-indexes i
			       (vector-stretch
				(vector-ref table-indexes i)
				(1+ table-col))))
		(set! table-widths
		  (vector-stretch table-widths (1+ table-col))))
	      (vector-set! (vector-ref table-indexes table-row)
			   table-col
			   (index-of-output))
	      ))]
	 [(title TITLE)
          (push! 'oport (open-output-string))
          (set! redirection-level (1+ redirection-level))
          ]
	 [(u U) (push! 'underline #t)]
	 [else (when (swl:bug) (fprintf (swl:bug-port) "doesn't handle tag <~a> yet!~%" html-tag))]
	 ) ;;; end case

       (when (memq html-tag '(br hr img BR HR IMG)) ;;; strictly non-paired tag?
	 ;; generate an end tag for it
	 (send self handle-end-tag (make-token 'end-tag html-tag)))

       (when (ormap (lambda (a) a) attrs)
         (when (swl:bug)
  	   (fprintf (swl:bug-port) "unhandled attribute(s) ~a for tag <~a>~n"
		 (reduce1 attrs)
		 html-tag)))
       )]

   [handle-end-tag (t)
     (let ([html-tag (token-value t)]
	   [markup-needed? #f]
	   [font-needed? #f]
	   [index '(0 . 0)]
	   [italic #f]
	   [bold #f]
	   [base-font #f]
	   [fixed-width #f]
	   [end-tag-pos (index-of-output)]
	   [nesting-level tag-nesting-level]
           [oport #f]
	   [background-font background-font]
	   [link #f]
	   ;;
	   ;; markup cache items - must be in cache key (see new-mproc)
	   ;;
	   [table-tabs #f]
           [tab1 #f]
	   [p-above-below #f]
	   [left-margin #f]
           [p-left-margin #f]
	   [underline-color #f]
	   )
       (define new-markup
	 (lambda (family size style)
	   (let ((mkup (create <markup>)))
	     (when table-tabs
	       (set-tabs! mkup table-tabs)
	       (when table-nowrap (set-wrap! mkup 'none))
	       )
	     (when tab1 (set-tabs! mkup (list tab1)))
	     (when p-above-below
	       (when (car p-above-below)
		 (set-above-paragraph-space! mkup (car p-above-below)))
	       (when (cdr p-above-below)
		 (set-below-paragraph-space! mkup (cdr p-above-below))))
	     (when underline-color
	       (when (car underline-color)
		 (set-underline! mkup (car underline-color)))
	       (when (cdr underline-color)
		 (set-foreground-color!
		  mkup
		  (tk->color (cdr underline-color)))))
	     (when left-margin
	       (set-left-margin! mkup left-margin)
	       (set-paragraph-indent! mkup left-margin))
	     (when p-left-margin (set-left-margin! mkup p-left-margin))
	     (when link
	       (set-mouse-press-method! mkup
		  (lambda (mkup text x y mods)
		    (event-case ((modifier= mods))
				[([left-button])
				 (send viewer view-url
				       (merge-url (top-val 'base-url) link))]
				[else (void)])))
	       (set-mouse-enter-method! mkup
		  (lambda (mkup text x y mods)
		    (set-mouse-cursor! text 'hand2)
		    ;;; (send viewer display-status (URL->string link))
		    ))
	       (set-mouse-leave-method! mkup
		  (lambda (mkup text x y mods)
		    (set-mouse-cursor! text 'left_ptr)
		    ;;; (send viewer display-status "")
		    )))
	     (when font-needed?
	       (set-font!
		mkup 
		(let* ([key (vector family size style)]
		       [cfont (assoc key font-cache)])
		  (if cfont
		      (cdr cfont)
		      (let ((nfont (create <font> family size style)))
			(set! font-cache (cons (cons key nfont) font-cache))
			nfont)))))
	     mkup)))
       
       (define new-mproc
	 (lambda () 
	   (lambda (wgt)
	     (let* ([bi index]
		    [ei end-tag-pos]
		    [old-font
		     (if (not font-needed?)
			 background-font
			 (let ((mkups (send wgt markups-at bi)))
			   (if (pair? mkups)
			       (let ((mfont (send (car mkups) get-font)))
				 (if mfont
				     mfont
				     background-font))
			       (send wgt get-font))))])
	       (mvlet ((family size style) (send old-font get-values))
                 (define choose-alt
                   (lambda (test sym opp ls)
                     (if test
                         (cons sym (remq opp (remq sym ls)))
                         ls)))
		 (let ([style (choose-alt italic 'italic 'roman
                                (choose-alt bold 'bold 'normal style))]
		       [size (if base-font (vector-ref font-sizes base-font) size)]
		       [family (if fixed-width 'courier family)])
		   (let ([mkup
			  (if link
			      ;; links are usually unique--don't bother with
			      ;; cache
			      (new-markup family size style)
			      (let* ([mkey (vector table-widths ;;; cache key
						 tab1
						 table-nowrap
						 p-above-below
						 left-margin
						 p-left-margin
						 underline-color
                                                 family
                                                 size
                                                 style)]
				     [cmkup (assoc mkey markup-cache)])
;;;(printf "applying markup bi=~s ei=~s mcache len=~s mkey=~s~n" bi ei (length markup-cache) mkey)
				(if cmkup
				    (cdr cmkup)
				    (let ((nmkup (new-markup family size style)))
				      (set! markup-cache
					(cons (cons mkey nmkup) markup-cache))
				      nmkup))))])
		     (send mkup apply-markup wgt bi ei)
		     (lambda (wgt) (send mkup apply-markup wgt bi ei)))))))))

;;;(printf "HANDLE-END-TAG!! tag=~s end-pos=~s hidden?=~s stack=~s nesting-lvl=~s~n"
;;;  html-tag end-tag-pos hidden-wgt? attribute-stack tag-nesting-level)

       (when (on-stack? (list html-tag))
	 ;;
	 ;; if necessary, generate end-tags for intervening stack items
	 ;;
	 (let f ((toptag (top-val 'html-tag))) 
	   (unless (or (eq? toptag html-tag)
		       (not toptag))
;;;(printf "genning end-tag for ~s~n" toptag)
	     (send self handle-end-tag (make-token 'end-tag toptag))
	     (f (top-val 'html-tag))))

	 (bump-tag-nesting-level html-tag -1)

	 ;;
	 ;; pop all attributes for this tag
	 ;;
	 (let f ((attr (pop!)))
;;;(printf "attr ~s~n" attr)
	   (if (or (not attr)
		   (and (eq? (car attr) 'html-tag)
			(eq? (cdr attr) html-tag)))
	       (void) ;;; no more attrs this tag
	       (let ((attr-name (car attr))
		     (attr-value (cdr attr)))
		 ;; (printf "pop attrs name=~s value=~s~n" attr-name attr-value)
		 (when (case attr-name
			 [(index) (set! index attr-value) #f]
			 [(italic)
			  (set! italic attr-value)
			  (set! font-needed? #t)
			  #t]
			 [(bold)
			  (set! bold attr-value)
			  (set! font-needed? #t)
			  #t]
			 [(fixed-width)
			  (set! fixed-width #t)
			  (set! font-needed? #t)
			  #t]
			 [(base-font)
			  (set! base-font attr-value)
			  (set! font-needed? #t)
			  #t]
			 [(left-margin) (set! left-margin attr-value) #t]
			 [(p-left-margin) (set! p-left-margin attr-value) #t]
			 [(link) (set! link attr-value) #t]
			 [(underline)
			  (if underline-color
			      (set-car! underline-color attr-value)
			      (set! underline-color (cons attr-value #f)))
			  #t]
			 [(color)
			  (if underline-color
			      (set-cdr! underline-color attr-value)
			      (set! underline-color (cons #f attr-value)))
			  #t]
			 [(anchor)
			  (set! anchors
			    (cons (cons attr-value end-tag-pos) anchors))
			  #f]
			 [(above-paragraph-space)
			  (if p-above-below
			      (set-car! p-above-below attr-value)
			      (set! p-above-below (cons attr-value #f)))
			  #t]
			 [(below-paragraph-space)
			  (if p-above-below
			      (set-cdr! p-above-below attr-value)
			      (set! p-above-below (cons #f attr-value)))
			  #t]
			 [(tab1)
			  (set! tab1 (make <tab-stop> attr-value 'left))
			  #t]
			 [(oport)
			  (set! oport attr-value)
			  (set! redirection-level (1- redirection-level))
			  #f]
			 [else #f])
		   (set! markup-needed? #t))
		 (f (pop!)))))

	 (case html-tag
	   [(i b tt I B TT) #f] ;;; some common tags
	   [(a A)
	    ;; hack for name anchors (which don't cause output)
	    (when (equal? (index-of-output) index)
	      (skip-html-whitespace iport))
	    ]
	   [(tr TR)
	    ;;
	    ;; in case of short row, extend offsets into remaining columns
	    ;;
	    (do ((i (1+ table-col) (1+ i))) ((= i (vector-length table-widths)))
	      (vector-set! (vector-ref table-indexes table-row)
			   i
			   (index-of-output)))
	    (when (= table-row (1- table-hidden-lines))
	      (html-flush)
	      (do ((i 0 (1+ i))) ((= i table-hidden-lines))
		(process-hidden-widget i))
	      (set! table-row -1))
	    (html-out #\newline)
	    (set! table-between-rows? #t) ;;; assume another row is to follow
	    ]
	   [(tablesegment table TABLESEGMENT TABLE)
	    (set! after-table-idx (index-of-output))
	    (html-flush)
	    (do ((i 0 (1+ i))) ((> i table-row))
	      (process-hidden-widget i))
	    (set! markup-needed? #t)
	    (set! table-tabs
	      (reverse
	       (let f ((i 0) (x 0) (ls '()))
		 (if (>= i table-col)
		     ls
		     (let ((width (vector-ref table-widths i)))
		       (f (1+ i)
			  (+ x width 15)
			  (cons (make <tab-stop> (+ x width 15) 'left)
				ls)))))))
	    ;;(printf "Table widths!!! ~s len(table-tabs)~s~n"
	    ;;   table-widths (length table-tabs))
	    (set! table-between-rows? #f)
	    ]
	   [(h1 h2 h3 h4 h5 h6 H1 H2 H3 H4 H5 H6)
	    (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	    (html-line-break)
	    ]
	   [(ul menu dir ol UL MENU DIR OL)
	     (when (and (= (count-items menu-tags) 0)
			(not (on-stack? '(below-paragraph-space))))
	       (html-line-break))
	     ]
	   [(pre PRE) (unless (top-val 'pre) (set! pre-mode? #f))]
	   [(title TITLE)
	    (unless (on-stack? '(pre PRE)) (skip-html-whitespace iport))
	    (send viewer set-viewer-title! (get-output-string oport))
	    ]
	   )

;;;(when markup-needed?
;;;  (printf "markup needed for bi=~s ei=~s lvl=~s~n" index end-tag-pos
;;;   tag-nesting-level))
	 
	 ;;
	 ;; pending-mprocs must be applied in reverse order of creation
	 ;; (which is the order they are in now).
	 ;;
	 (cond
	   [(and markup-needed? (fxzero? tag-nesting-level))
	    (set! mprocs (append mprocs (cons (new-mproc) pending-mprocs)))
	    (set! pending-mprocs '())]
	   [markup-needed?
	    (set! pending-mprocs (cons (new-mproc) pending-mprocs))]
	   [(and (zero? tag-nesting-level) (pair? pending-mprocs))
	    (set! mprocs (append mprocs pending-mprocs))
	    (set! pending-mprocs '())])
	
	 (when (memq html-tag '(tablesegment table TABLESEGMENT TABLE))
	   (html-flush)
	   (when (memq html-tag '(tablesegment TABLESEGMENT))
	     (send visible-wgt insert (send self get-string index end-mk))
	     (send visible-wgt set-index-of-output! (index-of-output)))
	   (for-each (lambda (mproc) (mproc visible-wgt)) hidden-mprocs)
	   (for-each (lambda (table-wgt)
		       (let ((wgt (car table-wgt))
			     (idx (cdr table-wgt)))
			 (send visible-wgt insert-widget-at idx wgt)
			  ))
		     table-wgts)
	   (set! hidden-mprocs '()))
	 ))]

   [html-image (url alt width height border)
;;;     (printf "html-image url=~s~n" url)
     (let* ([link (top-val 'link)])
       (define ibox
           (if link
               (let ([ibox (create <flex-button> (send self get-parent)
                                   with
                                   (border-width: 0))])
                 (set-mouse-press-method! ibox
                   (lambda (ibox x y mods)
                     (event-case ((modifier= mods))
                                 [([left-button])
                                  (send viewer view-url
                                        (merge-url (top-val 'base-url) link))]
                                 [else (void)])))
                 (set-mouse-enter-method! ibox
                   (lambda (ibox x y mods)
                     (set-mouse-cursor! visible-wgt 'hand2)
                     ))
                 (set-mouse-leave-method! ibox
                   (lambda (ibox x y mods)
                     (set-mouse-cursor! visible-wgt 'left_ptr)
                     ))
                 ibox)
               (create <label> (send self get-parent) with (border-width: 0))))
       (set-traversal-thickness! ibox 0)
       (html-flush)
       (when hidden-wgt? ;;; doing a table
         (set! table-wgts (cons (cons ibox (index-of-output)) table-wgts)))
       (send self insert-widget-at (index-of-output) ibox)
       (let ([netip (www-open-url (merge-url (top-val 'base-url) url))]
             [buf (open-output-string)])
         (if (port? netip)
             (begin
               (let loop ()
                 (let ([c (read-char netip)])
                   (unless (eof-object? c) (write-char c buf) (loop))))
               (send self show-html-image ibox
                 (let ([op (open-output-string)])
                   (swl:base64-encode (get-output-string buf) op)
                   (get-output-string op))
                 alt))
               (begin
;;;(printf "html-image: www-open-url returned ~s [url=~s]~n"
;;;  netip url)
                 (set-title! ibox alt)))))]

    [show-html-image (ibox data alt)
     (on-error (set-title! ibox alt)
       (set-title! ibox (create <photo> with (data: data))))]

   [goto-anchor (loc)
;;;(printf "goto-anchor loc=~s anchors=~s~n" loc anchors)
      (let ([dest (assoc loc anchors)])
        (when dest (send self move-to-top (cdr dest))))]

   [anchor->line (loc)
      (let ([dest (assoc loc anchors)])
        (when dest (cddr dest)))]

   [configure (w h)
;;;(printf "configure!! h=~s w=~s~n" h w)	      
     (send-base self configure w h)
     (for-each (lambda (cproc) (cproc w h)) cprocs)
     ]
   
    ) ;;; end public
  )


(define <html-text>:create
  ;;
  ;; needed because recursive class references are not allowed.
  ;;
  (lambda (parent)
    (let ((hw (create <html-text> parent)))
      (send hw set-height/char! 5) ;;; bounding box hack 
      (send hw set-width/char! 100)
      (send hw set-wrap! 'none)
      (send hw set-tabs!
	    (list (make <tab-stop> 0 'left)))
      '(let ((mk (create <markup> with
			(left-margin: 5))))
	(send mk apply-markup hw '(0 . 0) 'end))
      (pack hw)
      (swl:raw-tcl-eval "update") ;;; needed for bounding-box
      hw)))
    

#!eof
