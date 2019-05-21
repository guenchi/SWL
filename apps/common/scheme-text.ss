;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Defines <scheme-text>, a text widget for applications the edit/manipulate
;; Scheme code.
;;
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
;; always points one past an extra newline character (yecch!). 'end is
;; not a valid index for the <scheme-text> widget class.
;;
;; insert is a predefined mark for the insertion point. The text-widget
;; "cursor" is really the insert mark.
;;

(require "../common/app-text.ss" 'once)


(define-swl-class (<scheme-text> parent) (<app-text> parent)
  (ivars
    (paren-markup (create <markup> with (background-color: 'gold)))
    (box-markup (create <markup> with
                  (border-width: 2)
                  (relief: 'ridge)
                  (background-color: 'gainsboro)
                  ))
    (highlight-markup (create <markup> with (background-color: 'gold)))
    (paren-markup-removal-proc #f)
    (flash-removal-thread #f)
    (end-exp #f)
    (show-the-box? #f)
    (auto-indent? #t)
    (outline-mode? #t)
    (outline-cursor #f)
;;;    (collapsoids '())
;;;    (collapsoid-pressed #f)
;   (mouse-press-ix #f)
    )
  (inherited mini-buffer key-prefix end-mk handle edit-action begin-exp)
  (inheritable mini-buffer box-markup paren-markup highlight-markup
    key-prefix begin-exp end-exp end-mk handle)
  
  (private

    [do-paren-markup (beg end)
     (let ([procs (if (and show-the-box? (<= (- (cdr end) (cdr beg)) 100))
                      (compute-the-box beg end box-markup)
                      (compute-pairing beg (idx1- end)))])
       (let ([apply-show (car procs)] [remove-show (cdr procs)])
         (apply-show)
         (set! paren-markup-removal-proc remove-show)))]

    [compute-show-coords ()
      (let ((be begin-exp) (ee end-exp))
        (when (send self pos<? (cursor) begin-exp)
          ;; hack to permit the box and paren matching above entry area
          (set! end-exp begin-exp)
          (set! begin-exp '(0 . 0)))
        ;;
        ;; returns the range of a match, or #f if none.
        ;;
        (let
            ((ans 
              (let* ((csr (cursor))
                     (csr1- (idx1- csr))
                     (csr2- (idx1- csr1-))
                     (csr3- (idx1- csr2-))
                     (chr (send self get-char csr1-)))
                (if (send self pos=? begin-exp csr)
                    #f
                    (and
                     (or (eqv? chr #\() (eqv? chr #\[) (eqv? chr #\))
                         (eqv? chr #\]))
                     ;; paren must not be part of a char constant
                     (not (and (eqv? (send self get-char csr2-) #\\)
                               (eqv? (send self get-char csr3-) #\#)))
                     (let* ((forward? (if (memv chr '( #\( #\[ )) #t #f))
                            (b-pos (if forward? csr1- begin-exp))
                            (e-pos (if forward? end-exp csr))
                            (idx (find-matching-paren b-pos e-pos forward?)))
                       (and idx
                            (if forward?
                                ;; return a pair of indexes as a range
                                ;; (end is exclusive)
                                (cons csr1- (idx1+ idx))
                                (cons idx csr)
                                ))))))))
          (set! begin-exp be)
          (set! end-exp ee)
          ans))]

    [compute-flash-coords ()
      ;;
      ;; returns exact coords of an enclosing match, or a left-hand match,
      ;; or a right-hand match, or #f.
      ;;
      (define find-enclosing-match
        (lambda ()
          (let* ((idx-beg (find-unmatched-open-paren (cursor))))
            (and idx-beg
              (let ((idx-end (find-matching-paren idx-beg end-exp #t)))
                (and idx-end (cons idx-beg (idx1+ idx-end))))))))

      (define find-left-match
        (lambda ()
          (let ((idx-beg (find-matching-paren begin-exp (cursor) #f)))
            (and idx-beg
              (let ((idx-end (find-matching-paren idx-beg end-exp #t)))
                (and idx-end (cons idx-beg (idx1+ idx-end)))
                )))))

      (define find-right-match
        (lambda ()
          (let ((idx-end (find-matching-paren (cursor) end-exp #t)))
            (and idx-end
              (let ((idx-beg
                      (find-matching-paren begin-exp (idx1+ idx-end) #f)))
                (and idx-beg (cons idx-beg (idx1+ idx-end)))
                )))))

      (let ((be begin-exp) (ee end-exp))
        (when (send self pos<? (cursor) begin-exp)
          ;; hack to permit the box and paren matching above entry area
          (set! end-exp begin-exp)
          (set! begin-exp '(0 . 0)))
        (let ((ans (or (find-enclosing-match)
                       (find-left-match)
                       (find-right-match))))
          (set! begin-exp be)
          (set! end-exp ee)
          ans))]

    [compute-pairing (pos1 pos2)
      ;;
      ;; returns pair of procedures: one to apply markup, one to remove it.
      ;;
      ;; note that pos1 and pos2 are independent positions, not a range
      ;; (hence pos2 points to the actual char to be marked up).
      ;;
      (cons
        (lambda () ;;; apply pairing
          (send paren-markup apply-markup self pos1 (idx1+ pos1))
          (send paren-markup apply-markup self pos2 (idx1+ pos2)))
        (lambda () ;;; remove pairing
          (send paren-markup remove-markup self pos1 (idx1+ pos1))
          (send paren-markup remove-markup self pos2 (idx1+ pos2))
          ))]

    [find-unmatched-open-paren (end-idx)
      ;;
      ;; Returns the index of the first unmatched open paren or #f if none.
      ;;
      ;; end-idx is the end coordinate of a range (and hence is
      ;; exclusive of the last range character).
      ;;
      (define paren-char car)
      (define paren-index cdr)
      (define backward-aux
        (lambda (idx ofs)
          (let ((new-idx (send self add-offset idx ofs)))
              (if (send self pos<? new-idx begin-exp)
                begin-exp
                new-idx))))
      (define find-aux
        (lambda (beg-idx)
          (let* ([parens (find-parens beg-idx end-idx)]
                 [pairings '([ #\( . #\) ] [ #\[ . #\] ])])
            (let loop ([parens parens] [stack '()])
              (if (not (pair? parens))
                #f
                (let* ([paren-top (car parens)]
                       [ch (paren-char paren-top)])
                  (case ch
                    [(#\( #\[)
                     (if (null? stack)
                       (paren-index paren-top)
                       (let ((paired-char (cdr (assv ch pairings)))
                             (stack-top-char (paren-char (car stack))))
                         (if (eqv? stack-top-char paired-char)
                           (loop (cdr parens) (cdr stack))
                           (paren-index paren-top))))]
                    [(#\) #\])
                     (loop (cdr parens) (cons paren-top stack))])))))))
      ;;
      ;; do stepwise search for beginning index.  This takes about 2n time
      ;; in the worst case but wins big for the most frequent (closest)
      ;; cases. Initial distance (128) must be enough to cover a whole
      ;; line in order to avoid problems with character constants.
      ;;
      (let ((icnt -128))
        (let loop ((bx (backward-aux end-idx icnt)) (count icnt))
;;; (fprintf (swl:bug-port) "find-unmatched-open-paren count=~s~n" count)
          (let ((open-paren (find-aux bx)))
            (if open-paren
              open-paren
              (if (send self pos=? bx begin-exp)
                #f
                (loop (backward-aux bx count) (+ count count)))))))]

    [flash-the-box ()
;;; (fprintf (swl:bug-port) "flash-the-box~n")
     (assertion-violationf 'flash-the-box "disabled in favor of undo/redo")
#;
      (unless flash-removal-thread
        (let ((coords (compute-flash-coords)))
          (when coords
            (let* ((idx-beg (car coords))
                   (idx-end (cdr coords))
                   (procs (compute-the-box idx-beg idx-end box-markup))
                   (apply-flash (car procs))
                   (remove-flash (cdr procs)))
              (when paren-markup-removal-proc ;;; avoid c-s in general case
                (critical-section
                  (when paren-markup-removal-proc
                    (paren-markup-removal-proc)
                    (set! paren-markup-removal-proc #f))))
              (set! paren-markup-removal-proc remove-flash)
              (apply-flash)
              (set! flash-removal-thread
                (thread-fork
                 (lambda ()
                   (thread-name "flash")
                   (thread-sleep 1200)
                   (critical-section
                     (set! flash-removal-thread #f)
                     (remove-paren-markup)
                     (show-paren-markup) ;;; restore show markup, if any
                     ))
                 (thread-default-quantum)
                 (thread-highest-priority)))
              (thread-yield) ;;; need flash remover to be sleeping
              ))))]

    [scan-until-token (str i)
      (let ([len (string-length str)])
        (let loop ([i i] [ofs 0])
          (cond
            [(fx>= i len) #f]
            [(char=? (string-ref str i) #\;)
             (let ((ofs (scan-until-newline str (fx1+ i))))
               (if ofs
                 (loop (fx+ i ofs 2) (fx+ ofs 2))
                 #f))]
            [(char-whitespace? (string-ref str i)) (loop (fx1+ i) (fx1+ ofs))]
            [else ofs])))]

    [get-length-of-line (line) (car (send self line-end (cons 0 line)))]

    [str->spans (str start-line start-col)
      ;;
      ;; Returns list of spans, one span per line, in reverse order, where
      ;; each span is a pair of indexes: (span-start . span-end).
      ;;
      ;; If a line contains non-whitespace chars, the start span points to
      ;; the first character and end span points one past the last character.
      ;;
      ;; If a line contains only whitespace, the start span will point
      ;; one past the last char and the end span will point to the first
      ;; char (this makes sense for subsequently applying min and max
      ;; operations).
      ;;
      (define make-span
        (lambda (lr lc rr rc) (cons (cons lc lr) (cons rc rr))))
      
;;; (fprintf (swl:bug-port) "str->spans str=~s sl=~s sc=~s~n" str start-line start-col)
      
      (let* ([len (string-length str)])
        (let loop ([i 0]
                   [line start-line] [col start-col]
                   [c1 start-col] [c2 start-col]
                   [seen-non-ws? #f] [ls '()])
          (cond
            [(fx>= i len)
             (cons (make-span line c1 line c2) ls)]
            [else
              (let ([c (string-ref str i)]
                    [col+1 (fx1+ col)]
                    [i+1 (fx1+ i)]
                    )
                (cond
                  [(eqv? c #\newline)
                   (loop i+1 (fx1+ line) 0 0 0 #f
                     (cons (make-span line c1 line c2) ls))]
                  [seen-non-ws? ;;; already seen non-whitespace?
                    (if (char-whitespace? c)
                      (loop i+1 line col+1 c1 c2 #t ls)
                      (loop i+1 line col+1 c1 col+1 #t ls))]
                  [else ;;; only whitespace seen so far
                    (if (char-whitespace? c)
                      (loop i+1 line col+1 col+1 0 #f ls)
                      (loop i+1 line col+1 col col+1 #t ls))]))])))]

    [find-parens (beg-idx end-idx)
      ;;
      ;; returns a parse list of (paren-char . index) pairs or #f if
      ;; the str doesn't scan.  list is in right-to-left (reverse) order.
      ;;

;;; (fprintf (swl:bug-port) "  find-parens bpos=~s epos=~s~n" beg-idx end-idx)
      (let* ([str (get-string self beg-idx end-idx)]
             [len (string-length str)]
             [comment? #f]
             )
        (let loop ([i 0]
                   [x (get-x/char beg-idx)]
                   [y (get-y/char beg-idx)]
                   [ls '()])
          (if (fx>= i len)
            (begin
;;; (fprintf (swl:bug-port) "        find-parens return=~s~n" ls)
              ls)
            (let ([ch (string-ref str i)])
              (cond
                [(eqv? ch #\;)
                 (set! comment? #t)
                 (loop (fx1+ i) (fx1+ x) y ls)
                 ]
                [(memv ch '(#\( #\) #\[ #\]))
                 (loop (fx1+ i) (fx1+ x) y (cons (cons ch (cons x y)) ls))]
                [(eqv? ch #\newline)
                 (set! comment? #f)
                 (loop (fx1+ i) 0 (fx1+ y) ls)]
                [comment? (loop (fx1+ i) (fx1+ x) y ls)]
                [(eqv? ch #\\)
                 (loop (fx+ i 2) (fx+ x 2) y ls)]
                [(eqv? ch #\#)
                 (if (and (fx< (fx+ i 2) len)
                       (char=? (string-ref str (fx1+ i)) #\\))
                   (loop (fx+ i 3) (fx+ x 3) y ls)
                   (loop (fx1+ i) (fx1+ x) y ls))]
                [(memv ch '(#\" #\|))
                 (let loop2 ([i (fx1+ i)] [x (fx1+ x)] [y y])
                   (if (fx>= i len)
                     #f
                     (let ((ch2 (string-ref str i)))
                       (cond
                         [(eqv? ch2 #\newline)
                          (loop2 (fx1+ i) 0 (fx1+ y))]
                         [(eqv? ch2 #\\)
                          (loop2 (fx+ i 2) (fx+ x 2) y)]
                         [(eqv? ch2 ch)
                          (loop (fx1+ i) (fx1+ x) y ls)]
                         [else (loop2 (fx1+ i) (fx1+ x) y)]))))]
                [else (loop (fx1+ i) (fx1+ x) y ls)])))))]

    [with-match (pos dir handler)
     (define scan
       (lambda (pos)
         (if (eq? dir 'forward)
             (let f ([inc 128] [last pos])
               (let ([end (add-offset self last inc)])
                 (let ([ps (find-parens last end)])
                   (or (and (pair? ps)
                            (not (null? ps))
                            (cdr (car (last-pair ps))))
                       (and (send self pos<? end end-exp)
                            (f (* inc 2) end))))))
             (let f ([inc -128] [last pos])
               (let ([start (add-offset self last inc)])
                 (let ([ps (find-parens start last)])
                   (or (and (pair? ps)
                            (not (null? ps))
                            (cdr (car ps)))
                       (and (send self pos>? start begin-exp)
                            (f (* inc 2) start))))))) ))
     (let ([pos (or pos (get-cursor-pos self))])
       (case dir
         [(forward)
          (let ([new (or (and (memv (send self get-char pos) '(#\) #\])) pos)
                         (find-matching-paren pos end-exp #t)
                         (scan pos))])
            (when new (handler (add-offset self new 1))))]
         [(backward)
          (let ([new (or (find-matching-paren begin-exp pos #f) (scan pos))])
            (when new (handler new)))]
         [else (assertion-violationf 'with-match "unrecognized direction ~s" dir)]))]
    ) ;;; end private

  (protected ;;;             ** ** PROTECTED ** **

   [compute-the-box (box-beg box-end mkup)
    (assertion-violationf 'compute-the-box "disabled in favor of undo/redo")
#|
      ;;
      ;; returns pair of procedures: one to apply the box, one to remove it.
      ;;
      (define make-span
        (lambda (ly lx ry rx) (cons (cons lx ly) (cons rx ry))))
      (define span-start car) (define span-end cdr)
      (define get-x car) (define get-y cdr)
      (define span-start-x (lambda (x) (get-x (span-start x))))
      (define span-end-x (lambda (x) (get-x (span-end x))))
      (define span-start-y (lambda (x) (get-y (span-start x))))
      (define span-end-y (lambda (x) (get-y (span-end x))))
      (define spans->the-box
        (lambda (spans)
          (if (= (length spans) 1)
            spans
            (let* ((last-span (car spans)) ;;; spans are in reverse order
                   (first-spans (reverse (cdr spans)))
                   (first-span (car first-spans))
                   (mid-spans (cdr first-spans))
                   (bx (span-start-x first-span))
                   (by (span-start-y first-span))
                   (ex (span-end-x last-span))
                   (ey (span-end-y last-span))
                   (min-x (apply min (map span-start-x spans)))
                   (max-x+1 (apply max (map span-end-x spans))) ;; 1 past max x
                   )
              ;;
              ;; if not too many lines, add spaces needed for a
              ;; smooth-sided box.
              ;;
              (when (<= (- ey by) 100) 
                (let loop ((y (1- ey)) (spans (cdr spans)))
                  (when (>= y by)
                    (let ((span (car spans))
                          (extra (- max-x+1 (get-length-of-line y))))
                      (when (positive? extra)
                        (let ((old-csr (cursor)))
                          (if (< (span-start-x span) (span-end-x span))
                            (send-base self insert-at (span-end span)
                              (make-string extra #\space))
                            ;; special case: line is empty or all white-space
                            (send self insert-at (cons 0 y) 
                              (make-string extra #\space)))
                          ;; insert-at may have moved cursor
                          (send self set-cursor-pos! old-csr)))
                      (loop (1- y) (cdr spans))))))
              
              (cons (make-span by bx by max-x+1)
                (cons (make-span ey min-x ey ex)
                  (map (lambda (span)
                         (let ((y (span-start-y span)))
                           (make-span y min-x y max-x+1)))
                    mid-spans)))))))
      
      (let* ((spans (str->spans
                      (send self get-string box-beg box-end)
                      (get-y/char box-beg)
                      (get-x/char box-beg)))
             (the-box (spans->the-box spans)))
        (cons
          (lambda ()  ;;; apply the box
;;; (fprintf (swl:bug-port) "applying the box!!~n")
            (for-each
              (lambda (span)
                (send mkup apply-markup self
                  (span-start span) (span-end span)))
              the-box))
          (lambda () ;;; remove the box
            (for-each
              (lambda (span)
                (send mkup remove-markup self
                  (span-start span) (span-end span)))
              the-box))))
|#
      ]

    [find-matching-paren (beg-idx end-idx forward?)
      ;;
      ;; Finds the paren that matches the one at the beginning of the string
      ;; (end of string if forward? = #f) denoted by the position range.
      ;;
      ;; Returns: index of matching char #f if no match.
      ;;
      (define top-char caar)
      (define opposing-chars (if forward? '(#\) #\]) '(#\( #\[)))
      (define pairings (if forward?
                         '([  #\) . #\(  ]    (  #\] . #\[  ))
                         '([  #\( . #\)  ]    (  #\[ . #\]  ))))
      (define forward-aux
        (lambda (idx ofs)
          (let ((new-idx (send self add-offset idx ofs)))
            (if (send self pos>? new-idx end-idx)
              end-idx
              new-idx))))
      (define backward-aux
        (lambda (idx ofs)
          (let ((new-idx (send self add-offset idx ofs)))
            (if (send self pos<? new-idx beg-idx)
              beg-idx
              new-idx))))
      (define find-aux
        (lambda (beg-idx end-idx)
          (let ([parens (find-parens beg-idx end-idx)])
            (if (not (pair? parens))
              #f
              (let* ([parens (if forward? (reverse! parens) parens)]
                     [cursor-char (top-char parens)])
                (let loop ([parens (cdr parens)] [stack '()])
                  (if (null? parens)
                    #f
                    (let ([opposing-char (top-char parens)])
                      (if (memv opposing-char opposing-chars)
                        (let ((paired-char
                                (cdr (assv opposing-char pairings))))
                          (if (null? stack)
                            (if (eqv? cursor-char paired-char)
                              (cdar parens)
                              #f)
                            (if (eqv? (top-char stack) paired-char)
                              (loop (cdr parens) (cdr stack))
                              #f)))
                        (loop (cdr parens) (cons (car parens) stack)))))))))))

;;; (fprintf (swl:bug-port) "find-matching-paren!!! beg=~s end=~s~n" beg-idx end-idx)
      ;;
      ;; do stepwise search for other index.  This takes 2n time
      ;; in the worst case but wins big for the most frequent (closest)
      ;; cases.  Initial distance (128) must be enough to cover a whole
      ;; line in order to avoid problems with character constants.
      ;;
      (if forward?
        (let loop ((ex (forward-aux beg-idx 128)) (count 128))
;;; (fprintf (swl:bug-port) "find-matching-paren count=~s~n" count)
          (let ((match (find-aux beg-idx ex)))
            (if match
              (begin
;;; (fprintf (swl:bug-port) "        **forward match found: ~s~n" match)
                match)
              (if (send self pos=? ex end-idx)
                #f
                (loop (forward-aux ex count) (+ count count))))))
        (let loop ((bx (backward-aux end-idx -128)) (count -128))
;;; (fprintf (swl:bug-port) "find-matching-paren count=~s~n" count)
          (let ((match (find-aux bx end-idx)))
            (if match
              (begin
;;; (fprintf (swl:bug-port) "        **backward match found: ~s~n" match)
                match)
              (if (send self pos=? bx beg-idx)
                #f
                (loop (backward-aux bx count) (+ count count)))))))]

    [idx1+ (idx) (send self add-offset idx 1)]
    [idx1- (idx) (send self add-offset idx -1)]

    [scan-until-newline (str i)
;;; (fprintf (swl:bug-port) "scan-until-newline str=~s i=~s~n" str i)
      (let ([len (string-length str)])
        (let loop ([i i] [ofs 0])
          (cond
            [(fx>= i len) #f]
            [(char=? (string-ref str i) #\newline) ofs]
            [else (loop (fx1+ i) (fx1+ ofs))])))]

    [compute-lines (str)
      (let ((len (string-length str)))
        (let loop ((lines 1) (i 0))
          (if (fx= i len)
            lines
            (if (char=? (string-ref str i) #\newline)
              (loop (fx1+ lines) (fx1+ i))
              (loop lines (fx1+ i))))))]

    [str-whitespace? (str)
      (let ((len (string-length str)))
        (and (> len 0)
          (let loop ([i 0])
            (if (fx= i len)
              #t
              (if (char-whitespace? (string-ref str i))
                (loop (fx1+ i))
                #f)))))]

    [remove-paren-markup ()
;;; (fprintf (swl:bug-port) "remove paren markup~n")
      (when flash-removal-thread ;;; avoid critical section in general case.
        (critical-section
          (when flash-removal-thread
            (thread-wake flash-removal-thread)
            (thread-reschedule flash-removal-thread (thread-highest-priority))
            (thread-yield)
            )))
      (when paren-markup-removal-proc
        (paren-markup-removal-proc)
        (set! paren-markup-removal-proc #f))]

    [show-paren-markup ()
      (let ((coords (compute-show-coords)))
        (when coords
          (do-paren-markup (car coords) (cdr coords))))]

    [new-cursor-mark (kind)
      (case kind
        [(fixed) (send self fixed-mark (cursor))]
        [(floating) (send self floating-mark (cursor))]
        [else (assertion-violationf 'new-cursor-mark "bad kind ~s" kind)])]

    [move-to-char (n)
;;;(fprintf (swl:bug-port) "move-to-char n=~s~n" n)
      (let* ((ins (cursor))
             (new-ins (send self add-offset ins (- n (get-x/char ins)))))
        (cond
          [(send self pos<? new-ins begin-exp)
           (send self set-cursor-pos! begin-exp)]
          [(send self pos>? new-ins end-exp)
           (send self set-cursor-pos! end-exp)]
          [else (send self set-cursor-pos! new-ins)]))]
    ) ;;; end protected

  (public
    [init (parent)
      (send-base self init parent)
      (set! begin-exp (send self fixed-mark '(0 . 0)))
      (set! end-exp (send self floating-mark '(0 . 0)))
      ]

    [cursor-bol () (move-to-char 0)]

    [show-sexpression-at (pos bfp efp)
     (let ([start (send self offset->index bfp)]
           [end (send self offset->index efp)])
       (when pos
         (set-cursor-pos! self
           (case pos
             [(start) start]
             [(end) end]
             [else (assertion-violationf 'show-sexpression-at "invalid position ~s, expected start, end, or #f" pos)])))
       (remove-paren-markup)
       (send paren-markup apply-markup self start end)
       (set! paren-markup-removal-proc
         (lambda ()
           (send self clear-mini)
           (send paren-markup remove-markup self start end))))]

    [set-auto-indent! (v) (set! auto-indent? v)]
    [get-auto-indent () auto-indent?]

    [destroy ()
      (if flash-removal-thread (thread-kill flash-removal-thread))
      (send-base self destroy)
      ]

    [move-char (n)
      (let* ((csr (cursor))
             (new-csr (send self add-offset csr n)))
        (cond
          [(and (send self pos>=? csr begin-exp)
                (send self pos<? new-csr begin-exp))
           (send self set-cursor-pos! begin-exp)]
          [(send self pos>? new-csr end-exp)
           (send self set-cursor-pos! end-exp)]
          [else (send self set-cursor-pos! new-csr)]))]

    [delete-char (disp)
     (let ([new-csr (send self add-offset (cursor) disp)])
       (when (and (send self pos>=? new-csr begin-exp)
                  (send self pos<=? new-csr end-exp))
         (send-base self delete-char disp)))]

    [move-line (n)
      (let* ((csr (cursor))
             (lin (get-y/char csr))
             (new-lin (+ lin n))
             (new-csr (cons (get-x/char csr)
                            (if (positive? new-lin) new-lin 0))))
        (cond
          [(and (send self pos>=? csr begin-exp)
                (send self pos<? new-csr begin-exp))
           #f]
          [(send self pos>? new-csr end-exp)
           #f]
          [else (send self set-cursor-pos! new-csr)]))]

    [raw-indent-current-line ()
      (let* ((csr (cursor))
             (csr-y (get-y/char csr))
             (csr-x (get-x/char csr))
             (open-pos (find-unmatched-open-paren (cons 0 csr-y)))
             )
        (when (and (positive? csr-y) open-pos)
          (let* ((str (send self get-string open-pos csr))
                 (tok (scan-until-token str 1))
                 (indent (cond
                           [(not tok) 1]
                           [(substring-match-anchored "if " str (1+ tok)) 4]
                           [(substring-match-anchored "or " str (1+ tok)) 4]
                           [(substring-match-anchored "and " str (1+ tok)) 5]
                           [(char-alphabetic? (string-ref str (1+ tok))) 2]
                           [else 1]))
                 (cur-line (send self get-string-at-line csr-y))
                 (cur-span (car (str->spans cur-line csr-y 0)))
                 (left-x (get-x/char (car cur-span)))
                 (right-x (get-x/char (cdr cur-span)))
                 (nonblank-line? (< left-x right-x))
                 (new-start (+ indent (get-x/char open-pos)))
                 (idx0 (cons 0 csr-y))
                 (delta (- new-start left-x))
                 )
            (when (positive? delta)
              (send self insert-at idx0 (make-string delta #\space)))
            (when (negative? delta)
              (send self delete idx0 (send self add-offset idx0 (- delta))))
            (when (not (zero? delta))
              (if (and nonblank-line? (nonnegative? (+ csr-x delta)))
                (send self set-cursor-pos! (cons (+ csr-x delta) csr-y))
                (send self set-cursor-pos! (cons new-start csr-y))))
            )))]

    [indent-current-line ()
      (fluid-let ([edit-action 'indenting])         ; kludgey now that this code has moved outside the class where this ivar originates
        (send self raw-indent-current-line))]

    [show-the-box (v)
      (remove-paren-markup)
      (set! show-the-box? v)
      (show-paren-markup)]

    [key-press-prefix (key mods)
      (define old-key-prefix key-prefix)
      (set! key-prefix #f)
      (case old-key-prefix
        [(escape)
         (event-case ((key= key) (modifier= mods))
           [([control #\e]) (send self set-cursor-pos! end-exp)]
           [([control #\a]) (send self set-cursor-pos! begin-exp)]
           [([shift_l] [shift_r] [control_l] [control_r])
            (set! key-prefix 'escape) ;;; doesn't undo an escape
            ]
           [else
             (send self display-mini "unknown escape prefix")])]
        [else
          (send self display-mini "unknown prefix command")
          ]
        )
      ]

    [key-press-no-prefix (key mods)
      (define pass-it-on
        (lambda ()
          (send-base self key-press-no-prefix key mods)))
      (event-case ((key= key) (modifier= mods))
        [([alt] [control])
         (event-case ((key= key) (modifier= mods))
           [([alt #\<] [control home]) (send self set-cursor-pos! begin-exp)]
           [([alt #\>] [control end]) (send self set-cursor-pos! end-exp)]
           [([control #\0]) (send self move-to-match 'forward)]
           [([control #\9]) (send self move-to-match 'backward)]
           [([control #\)]) (send self select-to-match 'forward)]
           [([control #\(]) (send self select-to-match 'backward)]
           [else (pass-it-on)]
           )]
        [([control] [#\backspace] [delete])
         (event-case ((key= key) (modifier= mods))
           [([control #\d] [delete])
            (let ([sel (send self get-selected-range)])
              (if sel
                  (send self delete (car sel) (cdr sel))
                  (send self delete (cursor))))]
           [([control #\h] [#\backspace])
            (let ([sel (send self get-selected-range)])
              (if sel
                  (send self delete (car sel) (cdr sel))
                  (when (send self pos>? (cursor) begin-exp)
                    (delete-char self -1))))]
           [([control #\q])
            (set! key-prefix 'c-q)
            (send self display-mini "Key prefix: C-q")]
           [else (pass-it-on)])]
        [([#\return] [#\newline])
         (pass-it-on)
         (when auto-indent? (send self indent-current-line))
         (when (str-whitespace? (send self get-string (cursor) end-exp))
           (send self delete (cursor) end-exp))
         (send self make-visible (cursor))]
        [([#\tab])
         (if auto-indent? (send self indent-current-line) (pass-it-on))]
        [else (pass-it-on)])
      ]

    [insert-at (idx what)
      (when (send self pos<=? begin-exp idx end-exp)
        (remove-paren-markup)
        (send-base self insert-at idx what)
        (show-paren-markup))]

    [unrestricted-insert-at (idx what)
      (remove-paren-markup)
      (send-base self raw-insert-at idx what)
      (show-paren-markup)]

    [raw-insert-at (idx what)
     ; fix unrestricted-insert-at if you modify this
      (send-base self raw-insert-at idx what)
      ]

    [delete-eol ()
     (remove-paren-markup)
     (send-base self delete-eol)
     (show-paren-markup)]

    [delete (idx)
     (remove-paren-markup)
     (send-base self delete idx)
     (show-paren-markup)]

    [delete (idx1 idx2)
     (remove-paren-markup)
     (send-base self delete idx1 idx2)
     (show-paren-markup)]

    [raw-delete (idx1 idx2)
     (send-base self raw-delete idx1 idx2)]

    [get-begin-exp () begin-exp]
    [get-end-exp () end-exp]

;;;    [get-collapsoids () collapsoids]

;;;    [set-collapsoids! (cs) (set! collapsoids cs)]

    [set-cursor-pos! (pos)
      (remove-paren-markup)
      (send-base self set-cursor-pos! pos)
      (show-paren-markup)]
    
;       [mouse-press (x y mods)
;         (remove-paren-markup)
;   ;     (set! mouse-press-ix (send self xy->index x y))
;   ;;; (fprintf (swl:bug-port) "mouse-press-ix: ~s~n" mouse-press-ix)
;   ;;;      (set! collapsoid-pressed (send self collapsoid-at mouse-press-ix))
;         (send-base self mouse-press x y mods)
;         ]
;   
;       [mouse-release (x y mods)
;         (send-base self mouse-release x y mods)
;   ;;;      (when outline-mode?
;   ;;;     (let ((ix (send self xy->index x y)))
;   ;;;       (if collapsoid-pressed
;   ;;;         (event-case ((modifier= mods))
;   ;;;           [([shift right-button])
;   ;;;            (if (send self pos=? ix (send collapsoid-pressed get-mk))
;   ;;;              (send collapsoid-pressed expand-all)
;   ;;;              (send collapsoid-pressed move-to ix))
;   ;;;            ]
;   ;;;           [([right-button])
;   ;;;            (if (send self pos=? ix (send collapsoid-pressed get-mk))
;   ;;;              (send collapsoid-pressed expand)
;   ;;;              (send collapsoid-pressed move-to ix))
;   ;;;            ]
;   ;;;           )
;   ;;;         (event-case ((modifier= mods))
;   ;;;           [([right-button])
;   ;;;            (let ((ch (send self get-char ix)))
;   ;;;              (when (and ch
;   ;;;                      (or (char=? ch #\()
;   ;;;                        (char=? ch #\[)))
;   ;;;                (let ((match (find-matching-paren ix end-exp #t)))
;   ;;;                  (when match
;   ;;;                    (let ((cloid (create <collapsoid> with
;   ;;;                                   (background-color: 'blue)
;   ;;;                                   )))
;   ;;;                      (send cloid collapse self ix match <collapsoid>)
;   ;;;                      (set! collapsoids (cons cloid collapsoids))
;   ;;;                      )))))
;   ;;;            ]))))
;         (show-paren-markup)
;         ]
;   
;       [mouse-motion (x y mods)
;   ;;;      (if outline-mode?
;   ;;;     (let* ((ix (send self xy->index x y))
;   ;;;            (ch (send self get-char ix)))
;   ;;;       (if (and ch
;   ;;;                (or (char=? ch #\()
;   ;;;                    (char=? ch #\[)
;   ;;;                    (and (char=? ch #\$)
;   ;;;                         (send self collapsoid-at ix)
;   ;;;                 )))
;   ;;;         (event-case ((modifier= mods))
;   ;;;           [([right-button])
;   ;;;            (if collapsoid-pressed
;   ;;;              (send self set-mouse-cursor! 'right_ptr)
;   ;;;              (send-base self mouse-motion x y mods))
;   ;;;            ]
;   ;;;           [else 
;   ;;;             (unless outline-cursor
;   ;;;               (set! outline-cursor (send self get-mouse-cursor))
;   ;;;               (send self set-mouse-cursor! 'left_ptr))
;   ;;;             (send-base self mouse-motion x y mods)
;   ;;;             ])
;   ;;;         (event-case ((modifier= mods))
;   ;;;           [([right-button])
;   ;;;            (if collapsoid-pressed
;   ;;;              (send self set-mouse-cursor! 'right_ptr)
;   ;;;              (send-base self mouse-motion x y mods))
;   ;;;            ]
;   ;;;           [else
;   ;;;             (when outline-cursor
;   ;;;               (send self set-mouse-cursor! outline-cursor)
;   ;;;               (set! outline-cursor #f))
;   ;;;             (send-base self mouse-motion x y mods)])))
;           (send-base self mouse-motion x y mods)
;   ;;;     )
;         ]
;   
;   ;;;    [collapsoid-at (ix)
;   ;;;      (let ((mkups (send self markups-at ix)))
;   ;;;     (ormap (lambda (o) (if (isa? o <collapsoid>) o #f)) mkups))
;   ;;;      ]

    [get-string-at-line (line)
      (send self get-string (cons 0 line) (idx1- (cons 0 (1+ line))))]

    [move-to-match (dir)
     (with-match #f dir (lambda (pos) (set-cursor-pos! self pos)))]

    [select-to-match (dir)
     (with-match #f dir
       (lambda (pos)
         (case dir
           [(forward)
            (with-match pos 'backward
              (lambda (start)
                (set-cursor-pos! self pos)
                (select-range self start pos)))]
           [(backward)
            (with-match pos 'forward
              (lambda (end)
                (set-cursor-pos! self pos)
                (select-range self pos end)))]
           [else
            (assertion-violationf 'select-to-match "unrecognized direction ~s" dir)])))]

    [undo ()
     (remove-paren-markup)
     (send-base self undo)
     (show-paren-markup)]

    [redo ()
     (remove-paren-markup)
     (send-base self redo)
     (show-paren-markup)]

    ) ;;; end public
  )



#!eof



(require "../common/scrollframe.ss" 'once)

(define new-scheme-text 
  (case-lambda
    [(top height width)
     (let* ((frame
              (create <frame> top with
                (background-color: 'white)))
            (mini-frame (create <frame> top))
            (scrolled-frame
              (create <scrollframe> frame with
                (default-vscroll: #t) (sticky-hscroll: #t)))
            (scheme-text
              (create <scheme-text> scrolled-frame with
                (height/char: height) (width/char: width)
                (background-color: 'white)
                (wrap: 'none)
                )))
       (pack mini-frame (expand: #f) (fill: 'x))
       (pack scrolled-frame (expand: #t) (fill: 'both))
       (pack scheme-text)
       (pack frame)
       (send scheme-text realize)
       scheme-text)]
    [(top)
     (new-scheme-text top 24 80)]))

               

(define-swl-class (<collapsoid>) (<markup>)
  (ivars
    (text #f)
    (nested-collapsoids #f)
    (nesting-offset #f)
    (mk #f)
    (text-wgt #f)
    )
  (inherited)
  (inheritable)
  (private)
  (protected
    [class-name () '<collapsoid>]  ;;; johnz - kludge: oscar to remove
    
    [idx1+ (ix) (send text-wgt add-offset ix 1)]
    )
  
  (public
    [get-mk () mk]
    [set-mk! (v)
      (if (pair? v)
        (set! mk (send text-wgt fixed-mark v))
        (set! mk v))]

    [collapse (twgt ix match collapsoid-class)
      (define iota
        (lambda (a b)
          (if (send text-wgt pos<? a b)
            (cons a (iota (idx1+ a) b))
            (list b))))
      
      (set! text-wgt twgt)
      
      (let* ((all-cloids (send text-wgt get-collapsoids))
             (cloids
               (let f ((cloids all-cloids) (in '()) (out '()))
                 (if (null? cloids)
                   (cons in out)
                   (let ((cloid (car cloids)))
                     (if (send text-wgt pos<=? ix (send cloid get-mk) match)
                       (f (cdr cloids) (cons cloid in) out)
                       (f (cdr cloids) in (cons cloid out))))))))
        (let ((cloids-in (car cloids))
              (cloids-out (cdr cloids)))
          (for-each
            (lambda (cloid) (send cloid change-to-nested ix))
            cloids-in)
          (set! nested-collapsoids cloids-in)
          (send text-wgt set-collapsoids! cloids-out)
          (set! text (send text-wgt get-string ix (idx1+ match)))
          (send text-wgt delete ix (idx1+ match))
          (send text-wgt raw-insert-at ix "$")
          (send self apply-markup text-wgt ix (idx1+ ix))
          (set! mk (send text-wgt fixed-mark ix))
          ))
      ]
    [reapply (ix)
      (let ((new-ix (send text-wgt add-offset ix nesting-offset)))
        (send self apply-markup text-wgt new-ix (idx1+ new-ix))
        (set! mk (send text-wgt fixed-mark new-ix)))
      ]
    [move-to (ix)
      (send self remove-markup text-wgt mk (idx1+ mk))
      (send text-wgt delete mk)
      (set! mk (send text-wgt fixed-mark ix))
      (send text-wgt insert-at mk #\$)
      (send self apply-markup text-wgt mk (idx1+ mk))
      ]
    [change-to-nested (ix)
      (set! nesting-offset
        (- (send text-wgt index->offset mk) (send text-wgt index->offset ix)))
      (set! mk 'nested)
      ]
    [expand ()
      ;;
      ;; Fast clicking may cause multiple activations of this method.
      ;; The when expression guards against this.
      ;;
      (when nested-collapsoids
        (let ((csr (send text-wgt get-cursor-pos)))
          (send self remove-markup text-wgt mk (idx1+ mk))
          (send text-wgt delete mk)
          (send text-wgt raw-insert-at mk text)
          (for-each
            (lambda (cloid) (send cloid reapply mk))
            nested-collapsoids)
          (send text-wgt set-cursor-pos! csr)
          (send text-wgt set-collapsoids!
            (append nested-collapsoids
              (remq self (send text-wgt get-collapsoids))))
          (set! nested-collapsoids #f)))
      ]
    [expand-all ()
      ;;
      ;; Fast clicking may cause multiple activations of this method.
      ;; The when expression guards against this.
      ;;
      (when nested-collapsoids
        (let ((ncloids nested-collapsoids))
          (send self expand)
          (for-each
            (lambda (cloid) (send cloid expand-all))
            ncloids)))
      ]
    )
  )
