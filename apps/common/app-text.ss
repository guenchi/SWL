;; Copyright (c) 1996-1998 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Defines <app-text>, a text widget for applications that include
;; mini buffers.
;;
;;
;; Marks and Indexes
;; -----------------
;;
;; A mark denotes the char immediately to the right of the mark
;; location (mark locations lie between characters).
;;
;; All marks move when text is inserted before them.
;;
;; Note: _all_ methods that operate on a range of locations, and that
;; take an end location as an argument, perform their operations
;; _exclusive_ of the end location. (e.g., get-string returns a string
;; up to but not including the end location.)
;;
;; end-mk always points just past the last inserted buffer char.
;; We define end-mk because the official Tk end mark (named 'end in swl)
;; always points one past an extra newline character (yecch!).
;;
;; insert is a predefined mark for the insertion point. The text-widget
;; "cursor" is really the insert mark.
;;


(require "../common/help-text.ss" 'once)

(module (<app-text>)

(define-record action (pos type data))
  ; pos is actually the zero-based displacement from the begin-exp mark
  ;     to the point at which the edit was initiated, since using
  ;     (col . line) offsets leads to problems when trying to undo
  ;     a backspace after undoing an insert when the text was at the
  ;     end of a line.
  ;
  ;     Using relative begin-exp displacements instead of absolute '(0 . 0)
  ;     displacements helps in cases such as the interaction window, where
  ;     the position of begin-exp changes (e.g. when output is printed above
  ;     the entry area).

(define-swl-class (<app-text> parent) (<text> parent)  ;;; Was: (<help-text> parent) see src/swl/make.ss comment about <help-text>
  (ivars
   (key-prefix #f)
   (end-mk #f)
   (mini-buffer #f)
   (mini-search #f)
   (mini-active? #f)
   (search-offset #f)
   (search-alive? #f)
   (search-beg #f)
   (search-end #f)
   (search-msg #f)
   (search-markup (create <markup> with (background-color: 'orange)))
   (search-buffer #f)
   (last-search "")
   (last-line "")
   (undo-history '())
   (redo-history '())
   (pending #f)
   (edit-action #f)
   (undo-mark #f)
   (undo-enabled? #t)
   (begin-exp '(0 . 0)) ; start of editable area
   )
  (inherited handle)
  (inheritable mini-buffer key-prefix end-mk handle edit-action begin-exp)
  
  (private
   [do-the-search (forward?)
     (define search-action
       (lambda any
	 (hide-mini-search)
	 (set! search-msg (if forward? "Forward search" "Backward search"))
	 (send self display-mini search-msg)
	 (let* ((search-string (send mini-search get-string))
		(buflen (string-length search-buffer))
		(len (string-length search-string))
		(match-ofs
		 ((if forward?
		      (lambda (a b ofs) (substring-match a b ofs))
		      (lambda (a b ofs) (substring-match-rev a b ofs)))
		  search-string search-buffer search-offset))
		)
           (set! last-search search-string)
	   (send self display-mini
		 (string-append search-msg ": " search-string))
	   (if match-ofs
	       (begin
		 (when search-beg
		   (send search-markup remove-markup
			 self search-beg search-end))
		 (set! search-beg
		   (send self add-offset '(0 . 0) match-ofs))
		 (set! search-end
		   (send self add-offset '(0 . 0) (+ match-ofs len)))
		 (send self set-cursor-pos! search-beg)
		 (send search-markup apply-markup self search-beg search-end)
		 (set! search-offset match-ofs)
		 )
	       (begin 
		 (send self display-mini
		       (string-append search-msg " failed; wrapping: "
				      search-string))
		 (set! search-offset (if forward? -1 buflen))
		 (set! search-msg
		   (if forward?
		       "Wrapped forward search"
		       "Wrapped backward search"))
		 )))))
      (if search-alive?
          (search-action)
          (begin
            (set! search-alive? #t)
            (let ([ofs (send self index->offset (cursor))])
              (if forward?
                  (set! search-offset ofs)
                  (set! search-offset (1- ofs))))
            (set! search-buffer
              (send self get-string '(0 . 0) end-mk))
            (with-mini-input
              (if forward? "Forward Search:" "Backward Search:")
              last-search
              search-action)))]
    [shift-pending () (when (and undo-enabled? pending) (push-action! pending) (set! pending #f))]
    [update-undo-menu-items ()
     (send self notify-undo-state
      ; undo?
       (or (and pending #t) (not (null? undo-history)))
      ; redo?
       (not (null? redo-history))
      ; modified?
       (not (and (not pending)
                 (eq? undo-mark
                      (and (pair? undo-history)
                           (car undo-history))))))]
    [push-action! (x)
     (let ([data (action-data x)])
       (unless (string? data)
         (set-action-data! x
           (list->string
             (case (action-type x)
               [(insert delete) (reverse data)]
               [(backspace) data]
               [else
                (assertion-violationf 'push-action! "unrecognized action type ~s"
                  (action-type x))])))))
     (set! redo-history '())
     (set! undo-history (cons x undo-history))
     (update-undo-menu-items)]
    [log-action (pos type data)
     (when undo-enabled?
       (if pending
           (if (and (eq? type (action-type pending))
                    (or (char? data)
                        ; combine indent text only if automatically inserted
                        (and (eq? edit-action 'indenting) pos
                             (send self pos=? pos (get-cursor-pos self)))))
               (set-action-data! pending
                 (if (string? data)
                     (append (string->list data) (action-data pending))
                     (cons data (action-data pending))))
               (begin
                 (push-action! pending)
                 (set-pending! pos type data)))
           (set-pending! pos type data))
        ; update in case we've added a pending while undo lists are empty
        (update-undo-menu-items))]
    [set-pending! (pos type data)
     (let ([pos (send self compute-displacement begin-exp (or pos (get-cursor-pos self)))])
       (if (char? data)
           (set! pending (make-action pos type (list data)))
           (begin
             (push-action! (make-action pos type data))
             (set! pending #f))))]
   ) ;;; end private

  (protected ;;;             ** ** PROTECTED ** **

    [with-mini-input (prompt default action)
     (send self display-mini prompt)
     (set! mini-active? #t)
     (hide mini-buffer)
     (send mini-buffer set-width/char! (string-length prompt))
     (pack mini-buffer (expand: #f) (side: 'left))
     (send mini-search delete-all)
     (send mini-search insert default)
     (send mini-search select-range 0 'end)
     (pack mini-search (expand: #t) (fill: 'x) (side: 'left))
     (send mini-search set-enabled! #t)
     (send mini-search set-focus)
     (send mini-search set-action!
       (lambda (e)
         (set-focus self)
         (hide-mini-search)
         (action e)))]

   [hide-mini-search ()
      (set! mini-active? #f)
      (hide mini-search)
      (send self clear-mini)
      (pack mini-buffer (expand: #t) (side: 'left) (fill: 'x))]
    
   [cursor () (send self get-cursor-pos)]

   [substring-match (str1 str2 ofs)
;;; (printf "substr  s1=~s  s2=~s  ofs=~s~n" str1 str2 ofs)
      ;;
      ;; returns offset of match (of str1 in str2), or #f if none or bad ofs
      ;;
      (let ((len1 (string-length str1))
            (len2 (string-length str2)))
        (if (zero? len1)
          ofs
          (if (negative? ofs)
            #f
            (let loop ((i ofs))
              (if (fx>= i len2)
                #f
                (let loop2 ((j 0) (k i))
                  (cond
                    [(fx= j len1) i]
                    [(fx>= k len2) #f]
                    [(char-ci=? (string-ref str1 j) (string-ref str2 k))
                     (loop2 (fx1+ j) (fx1+ k))]
                    [else (loop (fx1+ i))])))))))]

   [substring-match-anchored (str1 str2 ofs)
;;; (printf "substr-ma:  s1=~s s2=~s ofs=~s~n" str1 str2 ofs)
      ;;
      ;; returns ofs if match or #f if none or bad ofs
      ;;
      (let ((len1 (string-length str1))
            (len2 (string-length str2)))
        (if (zero? len1)
          ofs
          (if (negative? ofs)
            #f
            (let loop ((j 0) (k ofs))
              (cond
                [(fx= j len1) ofs]
                [(fx>= k len2) #f]
                [(char-ci=? (string-ref str1 j) (string-ref str2 k))
                 (loop (fx1+ j) (fx1+ k))]
                [else #f])))))]

   [substring-match-rev (str1 str2 ofs)
;;; (printf "substr-mr  s1=~s  s2=~s  ofs=~s~n" str1 str2 ofs)
      ;;
      ;; returns ofs of match or #f if none or bad ofs
      ;;
      (let* ((len1 (string-length str1))
             (len1m1 (1- len1))
             (len2 (string-length str2)))
        (cond
          [(negative? ofs) #f] ;;; match not possible
          [(fx>= ofs len2) #f] ;;; invalid offset
          [(zero? len1) ofs] ;;; anything matches
          [else
            (let loop ((i ofs))
              (if (fx< i len1m1)
                #f ;;; insufficient chars avail for matching
                (let loop2 ((j len1m1) (k i))
                  (cond
                    [(fxnegative? j) (fx1+ k)]
                    [(char-ci=? (string-ref str1 j) (string-ref str2 k))
                     (loop2 (fx1- j) (fx1- k))]
                    [else (loop (fx1- i))]))))]))]

   ) ;;; end protected

  (public
   [init (parent)
     (send-base self init parent)
     (set! end-mk (send self floating-mark '(0 . 0)))
    ]

   [init-mini (mb ms)
     (set! mini-buffer mb)
     (set! mini-search ms)
     (send mini-search set-escape-thunk!
	   (lambda () (send self turn-search-off)))
     ]

    [delete-char (disp)
     (let ([pos (get-cursor-pos self)])
       (let ([s
              (if (< disp 0)
                  (get-string self (add-offset self pos disp) pos)
                  (get-string self pos (add-offset self pos disp)))])
         (log-action pos
           (if (< disp 0) 'backspace 'delete)
           (if (= (string-length s) 1) (string-ref s 0) s))))
     (fluid-let ([edit-action 'delete-char])
       ; let the other delete methods know we've logged it already
       (send-base self delete-char disp))]

    [delete-eol ()
     (let ([pos (get-cursor-pos self)])
       (log-action pos 'delete
         (get-string self pos (line-end self pos))))
     (send-base self delete-eol)]

    [delete (idx)
     (unless (eq? edit-action 'delete-char)
       (log-action idx 'delete (string (send self get-char idx))))
     (send-base self delete idx)
     (send self turn-search-off)]

    [delete (idx1 idx2)
      ;; this method is used for several things besides cutting the selection. 
     (let ([s (get-string self idx1 idx2)])
       (unless (eq? edit-action 'delete-char) (log-action idx1 'delete s))
       (send self set-saved-text! s)) ; so we can yank it back
     (send-base self delete idx1 idx2)
     (send self turn-search-off)]

    [raw-delete (idx1 idx2)
     (send-base self delete idx1 idx2)]

    [insert-at (idx what)
     (log-action idx 'insert
       (if (string? what)
           what
           (if (eq? idx 'insert) what (string what))))
     (send-base self insert-at idx what)
     (send self turn-search-off)]

    [raw-insert-at (idx what) ; bypass undo
     (send-base self insert-at idx what)]
   
    [move-char (offset)
     (shift-pending)
     (send-base self move-char offset)]

    [move-line (offset)
     (shift-pending)
     (send-base self move-line offset)]

    [set-cursor-pos! (idx)
     (unless (eq? edit-action 'indenting) (shift-pending))
     (send-base self set-cursor-pos! idx)
    ]

   [get-end-mk () end-mk]

   [offset->index (ofs) (send self add-offset '(0 . 0) ofs)]

   [index->offset (idx) (send self compute-displacement '(0 . 0) idx)]
   
   [compute-displacement (start idx)
     (define ballpark
       (lambda ()
	 (let loop ((ofs 1))
	   (if (send self pos<=? idx (send self add-offset start ofs))
	       ofs
	       (loop (+ ofs ofs))))))
     (if (send self pos=? idx 'end) ;;; all idxs > 'end are pos=? to 'end
	 (1+ (send self compute-displacement start (send self add-offset 'end -1)))
	 (let* ((guess-high (ballpark))
		(guess-low (quotient guess-high 2)))
	   (define bsearch
	     (lambda (low high)
	       (let* ((mid (quotient (+ low high) 2))
		      (mid-idx (send self add-offset start mid)))
		 (cond
		   [(send self pos=? idx mid-idx) mid]
		   [(send self pos<? idx mid-idx) (bsearch low (1- mid))]
		   [else (bsearch (1+ mid) high)]))))
	   (bsearch guess-low guess-high)))]
   
   [clear-mini ()
      (when mini-buffer
	(send mini-buffer set-enabled! #t)
	(send mini-buffer delete 0 'end)
	(send mini-buffer set-enabled! #f)
	)]

   [get-mini ()
     ;;
     ;; returns mini-buffer string (w/o trailing newline) or #f if none
     ;;
     (if mini-buffer
	 (let* ((buf (send mini-buffer get-string))
		(str (substring buf 0 (string-length buf))))
;;;(fprintf (swl:bug-port) "get-mini str=~s~n" str)
	   (and (positive? (string-length str)) str))
	 #f)]

   [display-mini (str)
     (when mini-buffer
;;;(fprintf (swl:bug-port) "display-mini ~s~n" str)
       (send self clear-mini)
       (send mini-buffer set-enabled! #t)
       (send mini-buffer insert-at 0 str)
       (send mini-buffer set-cursor-pos! 0)
       (send mini-buffer set-enabled! #f)
       )]

   [key-press (key mods)
      (if key-prefix
	(send self key-press-prefix key mods)
	(send self key-press-no-prefix key mods))
      ]

   [key-press-prefix (key mods)
      (void)]

   [key-press-no-prefix (key mods)
      (event-case ((key= key) (modifier= mods))
	[([control #\s]) (send self search-forward)]
	[([control #\r]) (send self search-backward)]
        [([control #\g]) (send self ask-goto-line)]
        [([control #\z]) (send self undo)]
        [([alt #\z]) (send self redo)]
	[([up] [down] [left] [right])
	 (send self turn-search-off)
	 (send-base self key-press key mods)
	 ]
	[else
 	  (unless (number? key) ;;; kludge test for modifier key-only
	      (send self turn-search-off)
	      )
	  (send-base self key-press key mods)
	  ]
	)]
    
   [turn-search-off ()
     (when mini-active? (hide-mini-search) (send self set-focus))
     (when search-alive?
       (set! search-alive? #f)
       (set! search-buffer #f)
       (send self clear-mini)
       (when search-beg
	 (send search-markup remove-markup self search-beg search-end)
	 (set! search-beg #f)))]

   [search-forward ()
      (when search-alive? (set! search-offset (1+ search-offset)))
      (do-the-search #t)]

   [search-backward ()
      (when search-alive? (set! search-offset (1- search-offset)))
      (do-the-search #f)]

   [mouse-press (x y mods)
      (send self turn-search-off)
      (send-base self mouse-press x y mods)]
      
   [action-copy ()
      (critical-section
	(let ((selstr (swl:get-selection)))
	  (when selstr
	    (swl:clear-clipboard)
	    (swl:append-clipboard selstr)
	    )))]

   [action-cut ()
     (critical-section
	(let ((selrange (send self get-selected-range)))
	  (when selrange
	    (let ((selstr (swl:get-selection)))
	      (when selstr
		(swl:clear-clipboard)
		(swl:append-clipboard selstr)
		(send self delete (car selrange) (cdr selrange))
		)))))]

   [action-paste ()
     (critical-section
       (let ((selstr (swl:get-clipboard)))
	 (when selstr
	   (send self insert-at (cursor) selstr))))]

   [ask-goto-line ()
    (with-mini-input "Go to line:" last-line
      (lambda (e)
        (let ([s (get-string e)])
          (let ([n (string->number s)])
            (when (fixnum? n)
              (let ([n (max 1 n)])
                (set! last-line (number->string n))
                (set-cursor-pos! self (cons 0 (- n 1)))
                (set-focus self)))))))]

    [undo ()
     (shift-pending)
     (unless (null? undo-history)
       (send self turn-search-off)
       (let ([x (car undo-history)])
         (set! undo-history (cdr undo-history))
         (set! redo-history (cons x redo-history))
         (update-undo-menu-items)
         (let ([pos (action-pos x)] [data (action-data x)])
           (case (action-type x)
             [(insert)
              (let ([pos (add-offset self begin-exp pos)]
                    [end (add-offset self begin-exp (+ pos (string-length data)))])
                (send-base self delete pos end)
                (send-base self set-cursor-pos! pos))]
             [(backspace)
              (send-base self insert-at
                (add-offset self begin-exp (- pos (string-length data)))
                data)
              (send-base self set-cursor-pos! (add-offset self begin-exp pos))]
             [(delete backspace)
              (let ([pos (add-offset self begin-exp pos)])
                (send-base self insert-at pos data)
                (send-base self set-cursor-pos! pos))]
             [else
              (assertion-violationf 'undo "unrecognized action type ~s" (action-type x))]))))]

    [redo ()
     (shift-pending)
     (unless (null? redo-history)
       (send self turn-search-off)
       (let ([x (car redo-history)])
         (set! redo-history (cdr redo-history))
         (set! undo-history (cons x undo-history))
         (update-undo-menu-items)
         (let ([pos (action-pos x)] [data (action-data x)])
           (case (action-type x)
             [(insert)
              (send-base self insert-at (add-offset self begin-exp pos) data)
              (send-base self set-cursor-pos!
                (add-offset self begin-exp (+ pos (string-length data))))]
             [(backspace)
              (let ([pos (add-offset self begin-exp pos)]
                    [new (add-offset self begin-exp (- pos (string-length data)))])
                (send-base self delete new pos)
                (send-base self set-cursor-pos! new))]
             [(delete)
              (let ([pos (add-offset self begin-exp pos)]
                    [end
                     (add-offset self begin-exp (+ pos (string-length data)))])
                (send-base self delete pos end)
                (send-base self set-cursor-pos! pos))]
             [else
              (assertion-violationf 'redo "unrecognized action type ~s" (action-type x))]))))]

    [notify-undo-state (undo? redo? modified?)
     (void)]

    [mark-undo-state ()
     (shift-pending)
     (set! undo-mark (and (pair? undo-history) (car undo-history)))]

    [discard-undo-state! ()
     (set! undo-history '())
     (set! redo-history '())
     (set! pending #f)
     (set! undo-mark #f)
     (update-undo-menu-items)
     ]

    [set-undo-enabled! (val) (set! undo-enabled? val)]
    [get-undo-enabled () undo-enabled?]

;    [move-word (n)
;     ; move +/- n words
;    ]

    ) ;;; end public
  )

)


#!eof



(require "../common/scrollframe.ss" 'once)

(define new-app-text 
  (case-lambda
    [(height width)
     (let* ((top (create <app-toplevel>))
	    (scrolled-frame
              (create <scrollframe> top with
		(default-vscroll: #t) (sticky-hscroll: #t)))
	    (app-text
	      (create <app-text> scrolled-frame with
		(height/char: height) (width/char: width)
		(background-color: 'white)
		(wrap: 'none)
		)))
       (pack scrolled-frame (expand: #t) (fill: 'both))
       (pack app-text)
       (send top notify-text app-text)
       app-text)]
    [() (new-app-text 24 80)]))
