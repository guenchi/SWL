;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Marks and Indexes
;; -----------------
;;
;; (see also scheme-text.ss)
;;
;; The fixed mark begin-exp is set to the beginning of the user input area.
;; begin-exp is set to the location of the end-exp mark immediately
;; upon a repl-port input operation.
;; 
;; The floating mark end-exp is set to the end of the user input area.
;; If user input is null, begin-exp equals end-exp.  
;;

;;
;; History System
;; --------------
;;
;; The user is always in one of two modes: browse mode or edit mode.
;; Browse mode occurs whenever the user is scrolling through the
;; history buffer.  Edit mode is the default, and is set whenever
;; the user has edited the current (unevaled) expression. The two
;; modes are mutually exclusive.
;;
;; On entering browse mode (Alt-P), if there is a current expression,
;; it is cached in the history buffer.  history-idx is set to the number
;; of the currently browsed item.
;;
;; On leaving browse mode for any reason, history-browse-mode? is set to #f
;; and any unevaled history item is discarded. The 0th history item
;; is the oldest.
;;

(define-swl-class (<repl-text> parent) (<scheme-text> parent)
  (ivars
   (repl-port-read-mode? #f)
   (rprm-begin-exp #f)
   (repl-port-read-extra-char #f)
   (chars-read 0)
   (write-mk #f)
   (my-thread #f)
   (exit-k #f)
   (history-list '())
   (history-size 0)
   (history-idx 0)
   (history-browse-mode? #f)
   (history-counter #f)
   (search-string #f)
   (search-offset #f)
   [flush-output-thread #f]
   (c/r-ready (thread-make-msg-queue 'c/r-ready))
   (waiter-reading-sem (let ((sem (make <sem> 1 'reading)))
                         (send sem wait)
                         sem))
   (ints-ok #t)
   (traffic-light #f)
   (repl-port #f) ;;; an input/output port 
   (entered? #f)
   (transcript-menu-item #f)
   (cd-menu-item #f)
   (undo-menu-item #f)
   (redo-menu-item #f)
   (copy-menu-item #f)
   (cut-menu-item #f)
   (paste-menu-item #f)
   (new-menu-item #f)
   (open-menu-item #f)
   (disable-items '())
   (input-markup (create <markup> with
;                        (background-color: 'lightgoldenrodyellow)
;                        (background-color: 'white)
                         (foreground-color: 'blue)))
   )
  (inherited mini-buffer box-markup paren-markup highlight-markup
    key-prefix begin-exp end-exp end-mk)
  (inheritable mini-buffer box-markup paren-markup highlight-markup
    key-prefix begin-exp end-exp end-mk)

  (private
    [check-expression ()
     (when repl-port-read-mode? (thread-send-msg c/r-ready #t))]
    [repl-port-read-mode (flag)
      (if flag
          (begin
            (set! rprm-begin-exp begin-exp)
            (set! repl-port-read-mode? #t))
          (set! repl-port-read-mode? #f))]
          
    [activate (mi)
      (send self turn-search-off)
      (when (send mi get-enabled) ((send mi get-action) mi))]
    [waiter-read ()
      (change-traffic 'green)
      (for-each (lambda (x) (send x set-enabled! #t)) disable-items)
     ; this is invoked by the waiter running in the repl-thread, so
     ; it sees the correct value of transcript-output-port parameter.
      (set-title! transcript-menu-item
        (if (transcript-output-port)
            "_Transcript off"
            #777="_Transcript on"))
      (send waiter-reading-sem signal)
      (mvlet ((dat sdat) (repl-read))
        (critical-section
          (change-traffic 'red)
          (for-each (lambda (x) (send x set-enabled! #f)) disable-items)
          (send waiter-reading-sem wait)
          (when (and sdat
                     (or (zero? history-size)
                         (not (string=? (car history-list) sdat))))
            (history-add! sdat))
          dat))]

    [repl-read ()
      ;;
      ;; Returns two values:
      ;;
      ;;  * the next expression as a Scheme datum.
      ;;  * the next expression as a string obtained directly from the
      ;;    widget buffer.
      ;;
      (define trim-right
        (lambda (s)
          (let loop ([i (- (string-length s) 1)])
            (and (fx>= i 0)
                 (if (char-whitespace? (string-ref s i))
                     (loop (fx- i 1))
                     (substring s 0 (fx+ i 1)))))))

      (let ([cip (console-input-port)])
        (if (eq? cip repl-port)
            (let* ([oldbe (send self fixed-mark begin-exp)]
                   [old-cr chars-read]
                   [dat
                    (dynamic-wind
                      (lambda () (repl-port-read-mode #t))
                      (lambda () (read repl-port))
                      (lambda () (repl-port-read-mode #f)))]
                   [sdat (trim-right (send self get-string oldbe begin-exp))])
              (values dat sdat))
            (values (read cip) #f)))]

    [compute-eos (str)
      ;;
      ;; returns the offset of the 1st character beyond the logical e.o.s.
      ;; (or zero if only whitespace).
      ;;
      (let loop ([j (fx1- (string-length str))])
        (cond [(fx= j -1) 0]
              [(char-whitespace? (string-ref str j)) (loop (fx1- j))]
              [else (fx1+ j)]))]

    [change-traffic (col)
      (when traffic-light (send traffic-light set-color! col))]

    [cursor () (send self get-cursor-pos)]

    [display-history-counter (count)
     (let ([pad
            (lambda (str len)
              (let ([slen (string-length str)])
                (if (< slen len)
                    (set! str
                      (string-append (make-string (- len slen) #\space) str)))
                (substring str 0 len)))])
     ; if history count exceeds 999, then
     ; this is more stable than we thought!
       (when history-counter
         (send history-counter set-title!
           (pad (or (and count (number->string count)) "") 3))))]

    [history-add! (str)
      (set! history-list (cons str history-list))
      (set! history-size (1+ history-size))]

    [history-delete! (idx)
      (set! history-list
        (let loop ((i (1- history-size)) (ls history-list))
          (if (null? ls)
            '()
            (if (= idx i)
              (loop (1- i) (cdr ls))
              (cons (car ls) (loop (1- i) (cdr ls)))))))
      (set! history-size (1- history-size))]

    [history-leave-browse-mode ()
;;; (fprintf (swl:bug-port) "history leave browse mode!!!~n")
      (when history-browse-mode?
        (send self discard-undo-state!)
        (let* ((bxi begin-exp)
               (beg (cons 0 (get-y/char bxi)))
               (end (idx1- bxi)))
          (send highlight-markup remove-markup self beg end))
        (history-delete! (1- history-size)) ;;; discard cached exp
        (display-history-counter #f)
        (history-terminate-search)
        (set! history-browse-mode? #f)
        )]

    [history-match (forward? anchored?)
;;;(fprintf (swl:bug-port) "history-match ~s ~s~n" forward? anchored?)
      (let ((sub-match
              (if anchored?
                (lambda (a1 a2 a3) (substring-match-anchored a1 a2 a3))
                (if forward?
                  (lambda (a1 a2 a3) (substring-match a1 a2 a3))
                  (lambda (a1 a2 a3) (substring-match-rev a1 a2 a3)))))
            (bump (if forward? 1+ 1-)))
        (define match
          (lambda (str start-idx start-ofs history-limit)
            ;;
            ;; returns pair (hist-idx . str-ofs) or #f if no match
            ;;
            (let loop ((h-idx start-idx) (ofs start-ofs))
              (if (or (negative? h-idx) (> h-idx history-limit))
                #f
                (let* ((hstr (list-ref history-list
                               (- (1- history-size) h-idx)))
                       (ofs (if ofs
                              (bump ofs)
                              (if (or anchored? forward?)
                                0
                                (1- (string-length hstr)))))
                       (match-ofs (sub-match str hstr ofs)))
                  (if match-ofs
                    (cons h-idx match-ofs)
                    (loop (bump h-idx) #f)))))))
        (let ((ofs (scan-until-newline search-string 0)))
          (send self display-mini
            (format "searching: ~a"
              (if ofs
                (string-append
                  (substring search-string 0 ofs) " ...")
                search-string))))
        (let* ((idx (if history-browse-mode?
                      history-idx
                      (if forward? 0 (1- history-size))))
               (limit (if history-browse-mode?
                        (- history-size 2)
                        (1- history-size)))
               (result (match search-string idx search-offset limit)))
          (if result
              (let ((ofs (cdr result)))
                (set! search-offset ofs)
                (history-move (cons 'abs (car result)))
                (if anchored? 
                    (send self cursor-eol)
                    (send self set-cursor-pos!
                          (add-offset self begin-exp ofs))))
              (send self display-mini "no matching history entry"))))]

    [history-move (n)
      ;;
      ;; +n/-n moves nth next/prev item into buffer
      ;; 'first/'last moves to the first/last item
      ;; (abs . n) moves the nth item into buffer
      ;;
      ;; also moves current exp into history, if necessary.
      ;;
;;;(fprintf (swl:bug-port) "history move n=~s hbm=~s~n"
;;;  n history-browse-mode?)
;;;(fprintf (swl:bug-port) "  history-size=~s~n" history-size)

      (unless history-browse-mode? ;;; not currently browsing
        (when (and (positive? history-size) ;;; there is history
                (or (and (number? n) (negative? n)) ;;; moving into it
                  (and (pair? n) (eq? (car n) 'abs))
                  (eq? n 'first)))
          (set! history-browse-mode? #t)
          (let* ((bxi begin-exp)
                 (beg (cons 0 (get-y/char bxi)))
                 (end (idx1- bxi)))
            (send highlight-markup apply-markup self beg end))
          (history-add!
            (send self get-string begin-exp end-exp)) ;;; cache cur exp
          (set! history-idx (1- history-size))))

      (when history-browse-mode?
        (let* ([new-pos
                 (cond
                   [(number? n) (+ history-idx n)]
                   [(and (pair? n) (eq? (car n) 'abs)) (cdr n)]
                   [(eq? n 'last) (1- history-size)]
                   [(eq? n 'first) 0]
                   [else (assertion-violationf 'history-move "invalid arg: ~s~n" n)])]
               [history-new (cond
                              [(negative? new-pos) 0]
                              [(< new-pos history-size) new-pos]
                              [else (1- history-size)])])
          (display-history-counter (1+ history-new))
          (unless (= history-idx history-new) ;;; already at end?
            (let* ((v-rows (send self get-height/char))
                   (v-top (yview-top-line))
                   (y-ins (get-y/char begin-exp))
                   (y-extra (- v-rows (- y-ins (1- v-top)))) ;;; extra lines
                   (str (list-ref history-list
                          (- (1- history-size) history-new))))
              (set! history-idx history-new)
              (let* ((str-lines (compute-lines str))
                     (pad-lines (+ (- y-extra str-lines) 2)) ;;; +2 empirical.
                     (pad-str (if (positive? pad-lines)
                                (make-string pad-lines #\newline)
                                ""))
                     (eos #f)
                     )
                (critical-section
                  (set! end-exp (send self floating-mark begin-exp))
                  ;;
                  ;; insert text w/o moving insert mark, else disturbs viewport.
                  ;;
                  (send self insert-at begin-exp str)
                  (set! eos (send self fixed-mark end-exp))
                  (send self insert-at eos pad-str)
                  (send self raw-delete end-exp end-mk)
                  (set! end-exp (send self floating-mark eos))
                  (send self set-cursor-pos! begin-exp)
                  (send self cursor-eol)
                  (show-paren-markup)
                  ; we don't want to be able to undo the history insert/delete
                  (send self discard-undo-state!)))
              (when (= history-idx (1- history-size))
                (history-leave-browse-mode))
              ))))]

    [open-repl-port ()
      (let* ([ibuf (make-string 100)]
             [obuf (make-string 2000)]
             [ibuf-s (string-length ibuf)]
             [ibuf-i 0]
             [ibuf-n 0]
             [obuf-s (string-length obuf)]
             [obuf-i 0] ;;; pseudo output-index 
             [line-buf (open-input-string "")]
             [seen-newline-sem (make <sem> 1 'sem:newline)]
             [name "repl"]
             [eof-msg "unexpected end-of-file"]
             [emlen (string-length eof-msg)]
             )
        (define sfd
          (let ([ip (open-bytevector-input-port #vu8())])
            (make-source-file-descriptor "current expression" ip)))
        (define format-condition
          (lambda (c)
           ; effectively suppress "at char ... of current expression"
            (or (and (message-condition? c)
                     (format-condition? c)
                     (irritants-condition? c)
                     (guard (c [#t #f])
                       (apply format (condition-message c) (condition-irritants c))))
                (with-output-to-string (lambda () (display-condition c))))))
        (define end-of-well-formed-exp
         ; We now require all expressions on a single input line to be
         ; well-formed.  It is possible to get into trouble when an earlier
         ; expression on the line modifies the contents of console-input-port,
         ; e.g. via clear-input-port, read-char, etc., but this seems like a
         ; throw-away case now that we have a separate current-input window.
         ; We made this choice because it was frustrating that we could give
         ; nice error feedback for the first expression on the line, but errors
         ; in the remaining expressions were going through the usual error path
         ; which includes printing a big ugly error message.
          (lambda ()
            (define (find-newline-after last p)
             ; return position of last newline only if we've seen an expr
              (let ([s (port-input-buffer p)])
                (let ([end (file-position p)])
                  (do ([i last (fx+ i 1)]
                       [nl? #f
                        (or nl?
                            (and (char=? (string-ref s i) #\newline)
                                 (+ i 1)))])
                      ((or nl? (fx= i end)) nl?)))))
            (let ([p (open-input-string (send self get-string rprm-begin-exp end-exp))])
              (guard (c [#t (let ([msg (format-condition c)])
                              (unless (and (>= (string-length msg) emlen)
                                           (string=? (substring msg 0 emlen) eof-msg))
                                (send self display-mini msg)
                                (when (#%$src-condition? c)
                                  (let ([src (#%$src-condition-src c)])
                                    (let ([i (send self index->offset begin-exp)])
                                      (send self show-sexpression-at #f 
                                        (+ i (source-object-bfp src))
                                        (+ i (source-object-efp src))))))))
                            #f])
                (send self clear-mini)
                (let loop ([last 0])
                  (let-values ([(type value bfp efp) (#%read-token p sfd)])
                    (or (and (> last 0) (find-newline-after last p))
                        (case type
                          [(eof) #f]
                          [else
                           (file-position p last)
                           (get-datum/annotations p sfd last)
                           (loop (file-position p))]))))))))

        ;;
        ;; don't use trace-lambda unless trace output port properly bound
        ;;
        (define widget-block-read
          ;;
          ;; Reads lines one at a time from buffer.
          ;; Returns num chars placed in str.  May block on c/r-ready.
          ;;
          (lambda (str cnt)
            (unless repl-port
              (assertion-violationf 'widget-block-read "port already closed"))
            (send self discard-undo-state!) ; there's no turning back now
            (let ((n (block-read line-buf str cnt)))
              (if (not (eof-object? n))
                  (begin 
                    ;; this will only ensure we copy into the transcript
                    ;; what user types after evaluating something like
                    ;; (read) while in the repl.
                    (when (and (transcript-output-port) (fixnum? n))
                      (block-write (transcript-output-port) str n))
                    n)
                  (let loop ()
                    (critical-section
                      ;; Get input one line at a time, so that multiple
                      ;; expression key-ahead works.

                      (let* ([bxpos (send begin-exp get-pos)]
                             [expos (send end-exp get-pos)]
                             [bxl (cdr bxpos)]
                             [exl (cdr expos)]
                             [end-pos
                              ; doesn't necessarily end on the same line as
                              ; bxpos, e.g., if we have input like:
                              ;  > 1 2 3 (quote hello
                              ;            )
                              (if repl-port-read-mode?
                                  (let ([i (end-of-well-formed-exp)])
                                    (and i (send self add-offset bxpos i)))
                                  (if (= bxl exl) expos `(0 . ,(1+ bxl))))])
                        (if (or (equal? bxpos expos)
                                (not (and entered?
                                          (eq? #\newline (send self get-char (cursor)))
                                          (send self pos=? (cursor) end-exp)))
                                (and repl-port-read-mode? (not end-pos)))
                            (begin
                              (thread-receive-msg c/r-ready)
                              (loop))
                            (begin
                              (send input-markup apply-markup self begin-exp end-pos)
                              (set! begin-exp (send self fixed-mark end-pos))
                              (set! write-mk (send self fixed-mark end-pos))
                              (let ((ln (send self get-string bxpos end-pos)))
                                (set! line-buf (open-input-string ln)))
                              (widget-block-read str cnt))))))))))

        (define widget-block-write
          (lambda (buf i)
            (critical-section
              (when repl-port
                (set! write-mk (send self floating-mark write-mk))
                (set! begin-exp (send self floating-mark begin-exp))
                (if (eqv? (string-length buf) i)
                    (send self unrestricted-insert-at write-mk buf)
                    (send self unrestricted-insert-at write-mk
                          (substring buf 0 i)))
                (when (transcript-output-port)
                  (block-write (transcript-output-port) buf i))
                (set! write-mk (send self fixed-mark write-mk))
                (set! begin-exp (send self fixed-mark begin-exp))
                (send self make-visible end-exp)
                ))))
        
        (define console-read-flush ;;; mimics chez console port behavior
          (lambda (p)
            (when (eq? (console-input-port) p)
              (flush-output-port (console-output-port))
              )))
        
        (define handler
          (lambda (xmsg . xargs)
              (define (clear-output p)
                (set-port-output-size! p 0)
                (set-port-output-index! p 0))
              (define (clear-input p)
                (critical-section
                  (set! ibuf-i 0)
                  (set! ibuf-n 0)
                  (set! line-buf (open-input-string ""))))
              (record-case (cons xmsg xargs)
                [block-write (p str i)
                 (flush-output-port p)
                 (widget-block-write str i)]
                [write-char (c p)
                  (disable-interrupts) ;;; want max efficiency here
                    (string-set! obuf obuf-i c) (set! obuf-i (fx1+ obuf-i))
                  (enable-interrupts)
                  (if (= obuf-i obuf-s)
                      (flush-output-port p)
                      (when (char=? c #\newline)
                        (send seen-newline-sem signal)
                        (void)))]
                [file-position (p . pos)
                 (if (null? pos)
                     chars-read
                     (assertion-violationf 'repl-port "cannot reposition"))]
                [flush-output-port (p)
                   (critical-section
                     (when (fx> obuf-i 0)
                       (widget-block-write obuf obuf-i)
                       (set! obuf-i 0)))]
                [close-port (p)
                 ; Chez Scheme doesn't let you close console input/output port
                 (void)
;                (thread-kill flush-output-thread)
;                (set-port-output-size! p 0)
;                (set-port-input-size! p 0)
;                (mark-port-closed! p)
                ]
                [port-name (p) name]
                [read-char (p)
                  (critical-section
                    (let ([c (peek-char p)])
                      (set! chars-read (fx1+ chars-read))
                      (set! ibuf-i (fx1+ ibuf-i))
                      c))]
                [peek-char (p)
                  (critical-section
                    (console-read-flush p)
                    (let ((ans
                           (if (fx< ibuf-i ibuf-n)
                               (string-ref ibuf ibuf-i)
                               (let ([n (widget-block-read ibuf ibuf-s)])
                                 (set! ibuf-i 0)
                                 (set! ibuf-n n)
                                 (string-ref ibuf 0)))))
                      ans))]
                [unread-char (c p)
                  (critical-section
                    (when (fx= ibuf-i 0)
                      (assertion-violationf 'unread-char "tried to unread too far on ~s" p))
                    (set! ibuf-i (fx1- ibuf-i))
                    (set! chars-read (fx1- chars-read)))]
                [char-ready? (p)
                  (let ((ans (or (fx< ibuf-i ibuf-n)
                                 (not (send self pos=? begin-exp end-exp)))))
                    ans)]
                [clear-input-port (p) (clear-input p)]
                [clear-output-port (p) (clear-output p)]
                [else (assertion-violationf 'repl-port "operation ~s not handled" xmsg)])))
        
        (let ([iop (make-input/output-port handler ibuf obuf)])
          (set-port-input-size! iop 0) ;;; always invoke handler
          (set-port-output-size! iop 0) ;;; always invoke handler
          (set! flush-output-thread
            (thread-fork
              (lambda ()
                (let f ()
                  ;;
                  ;; This is not the fairness hack - it slows down the
                  ;; flush rate, improving performance.
                  ;;
                  (thread-name "flush")
                  (thread-sleep 100)
                  (send seen-newline-sem wait)
                  (flush-output-port iop)
                  (f)))))
          (set! repl-port-read-extra-char
            (lambda ()
              (if (fx< ibuf-i ibuf-n)
                  (let ((ch (string-ref ibuf ibuf-i)))
                    (set! ibuf-i (fx1+ ibuf-i))
                    ch)
                  #f)))
          iop))]

    [history-terminate-search ()
     (when search-string
       (set! search-string #f)
       (send self clear-mini))]

    [yview-top-line ()
      (1+ ;;; base is relative to 1
        (inexact->exact
          (round
            (* (get-y/char end-mk) (car (send self get-vertical-view))))))]

    [port-alive? () (and repl-port (not (port-closed? repl-port)))]

    [remove-unprintables (str)
     ;;
     ;; remove things that upset the Tk text widget.
     ;;                  
     (list->string                       
      (let f ((ls (string->list str)))
        (cond [(null? ls) '()]
              [(< (char->integer (car ls)) (char->integer #\newline))
               (f (cdr ls))]
              [else (cons (car ls) (f (cdr ls)))])))]

    ) ;;; end private

  (protected ;;;        ** ** PROTECTED ** **

    [show-paren-markup ()
     (send-base show-paren-markup)
     (check-expression)]

    )
  
  (public ;;;            ** ** PUBLIC ** **

    [init (parent)
      (send-base self init parent)
      (set! write-mk (send self fixed-mark '(0 . 0)))
    ]

    [set-menu-items! (mi-trans mi-undo mi-redo mi-copy mi-cut mi-paste mi-new mi-open
                      mi-cd disitems)
      (set! transcript-menu-item mi-trans)
      (set! undo-menu-item mi-undo)
      (set! redo-menu-item mi-redo)
      (set! copy-menu-item mi-copy)
      (set! cut-menu-item mi-cut)
      (set! paste-menu-item mi-paste)
      (set! new-menu-item mi-new)
      (set! open-menu-item mi-open)
      (set! cd-menu-item mi-cd)
      (set! disable-items disitems)
      (send self notify-undo-state #f #f #f)
      ]

    [notify-undo-state (undo? redo? modified?)
     ; base method is NOP, but including this in case we change base class w/o
     ; scrutinizing each and every method
     (send-base self notify-undo-state undo? redo? modified?)
     (set-enabled! undo-menu-item undo?)
     (set-enabled! redo-menu-item redo?)]

    [toggle-transcript (top)
     (if (string=? #777# (get-title transcript-menu-item))
         (let ([name
                (swl:file-dialog "Save transcript to" 'save
                  (parent: top)
                  (default-dir: (current-directory)))])
           (when name
             (send self insert-expression
               (format "(transcript-on ~s)\n" name))))
         (send self insert-expression "(transcript-off)\n"))]

    [destroy ()
      (console-error-port (swl:error-log-port))
; Seems like we should be able to do this, and if we don't,
; won't we simply get confusing error messages when the port
; attempts to flush the output to the (destroyed) text widget?
; Trouble is we need some way of avoiding cascading errors.
;     (mark-port-closed! repl-port)
      (unless (eq? (thread-self) my-thread) ;;; may be called by my-thread
        (thread-break my-thread 'shutdown exit-k)
        (thread-yield))
      (when flush-output-thread (thread-kill flush-output-thread))
      (send-base self destroy)
      ]

    [insert-expression (str)
    ; Using self's insert method because it's got all the machinery for
    ; managing the various marks (begin-exp, end-exp, write-mark) and for
    ; sending messages to unblock threads waiting on c/r-ready?.
    ; A little extra gyration here to try to put cursor at the same position
    ; within the saved expression.
     (with-mutex waiter-reading-sem
       (critical-section
         (let ([saved-exp (send self get-string begin-exp end-exp)]
               [here (cursor)]
               [end (send end-exp get-pos)])
           (let ([adjust
                 ; It seems we have to adjust the cursor position when the
                 ; cursor starts out on the same line as the begin-exp mark.
                 ; Can't see why this is needed, but it seems to work
                 ; consistently on Linux and Windows.
                  (let ([pos (send begin-exp get-pos)])
                    (and (= (cdr here) (cdr pos)) (car pos)))])
             (send self delete begin-exp end-exp)
             (send self insert str)
             (send self insert saved-exp)
             (on-error 'ignore
               (let ([new (send end-exp get-pos)])
                 (set-cursor-pos! self
                   (cons (if adjust
                             (- (car here) adjust)
                             (- (car new) (- (car end) (car here))))
                         (- (cdr new) (- (cdr end) (cdr here)))))))))))]

    [set-current-expression! (str)
     (with-mutex waiter-reading-sem
       (critical-section
         (send self delete begin-exp end-exp)
         (send self insert str)
         (send self set-cursor-pos! end-exp)))]

    [history-first () (history-move 'first)]
    [history-last () (history-move 'last)]
    [history-backward () (history-move -1)]
    [history-forward () (history-move +1)]
    
    [history-get ()
      ;; the car of history-list is the last history item.
      history-list]

    [history-set! (hi)
      (set! history-list hi)
      (set! history-size (length hi))
      (set! history-idx 0)
      ]

    [history-clear ()
      (history-leave-browse-mode)
      (set! history-list '())
      (set! history-size 0)
      (set! history-idx 0)]

    [key-press-prefix (key mods)
      (define hist-mat
        (lambda (forward? anchored?)
          (set! key-prefix #f)        ; unfortunate hack (usually reset in base method --- if we don't reset it then Esc-p leaves us in prefix mode, which confuses things if we press enter next)
          (send self clear-mini)
          (unless search-string
            (set! search-string (send self get-string begin-exp end-exp))
            (set! search-offset #f))
          (if (and anchored? (eqv? search-string ""))
              (history-move (if forward? 1 -1))
              (history-match forward? anchored?))))
      
      (case key-prefix
        [(escape)
         (event-case ((key= key) (modifier= mods))
           [([#\P]) (hist-mat #f #f)]
           [([#\p]) (hist-mat #f #t)]
           [([#\N]) (hist-mat #t #f)]
           [([#\n]) (hist-mat #t #t)]
           [else
             (send-base self key-press-prefix key mods)])]
        [else
          (send-base self key-press-prefix key mods)])
      ]

    [key-press-no-prefix (key mods)
      (set! entered? #f)
      (event-case ((key= key) (modifier= mods))
        [([alt])
         (event-case ((key= key) (modifier= mods))
           [([alt #\q]) (send (send self get-parent) destroy)]
           [([alt #\d]) (activate cd-menu-item)]
           [([alt #\c]) (activate copy-menu-item)]
           [([alt #\x]) (activate cut-menu-item)]
           [([alt #\v]) (activate paste-menu-item)]
           [([alt #\n]) (activate new-menu-item)]
           [([alt #\o]) (activate open-menu-item)]
           [else
            (send-base self key-press-no-prefix key mods)])]
        [([control] [up] [down] [#\backspace] [delete])
         (event-case ((key= key) (modifier= mods))
           [([control #\c]) (send self interrupt)]
           [([control #\n] [down])
            (send self turn-search-off)
            (let* ([end-line (get-y/char (idx1- end-exp))]
                   [cur-line (get-y/char (cursor))])
              (if (and (= cur-line end-line)
                       (or history-browse-mode?
                           (send self pos=? begin-exp end-exp)))
                (send self history-forward)
                (send-base self key-press-no-prefix key mods)))]
           [([control #\p] [up])
            (send self turn-search-off)
            (let* ([begin-line (get-y/char begin-exp)]
                   [cur-line (get-y/char (cursor))])
              (if (and (= cur-line begin-line)
                       (or history-browse-mode?
                           (send self pos=? begin-exp end-exp)))
                (send self history-backward)
                (send-base self key-press-no-prefix key mods)))]
           [([control #\j] [control #\newline] [control #\return])
            (send self set-cursor-pos! end-exp)
           ; totally gross, use string instead of character here to force entered? true
           ; so that it actually enters the expression
            (send self insert "\n")]
           [else
            (send-base self key-press-no-prefix key mods)])]
        [([escape])
         (set! key-prefix 'escape)
         (send self display-mini "Key prefix: Esc")
         ]
        [else
         (send-base self key-press-no-prefix key mods)])
      ]

    [interrupt ()
      (send self turn-search-off)
      (when (port-alive?)
        (critical-section
          ; disable check-expression, otherwise input like (load "foo') Ctrl+c
          ; will screw up and the reset won't actually happen as it should
          (fluid-let ([repl-port-read-mode? #f])
            (history-leave-browse-mode)
            (history-terminate-search)
            (send self raw-delete end-exp end-mk)
            (set! begin-exp (send self fixed-mark end-exp))
            (set! write-mk (send self fixed-mark end-exp))
            (send self set-cursor-pos! end-exp))
          (thread-break my-thread 'interrupt)))]

    [delete-eol ()
      (let ((ofs (cursor)))
        (when (and (send self pos<=? begin-exp ofs))
         ; undo history is discarded when we leave browse mode,
         ; we do delete afterwards so it will be recorded in new
         ; undo history
          (history-leave-browse-mode)
          (history-terminate-search)
          (send-base self delete-eol)))]

    [delete-char (disp)
      (let ((ofs (send self add-offset (cursor) disp)))
        (when (and (send self pos<=? begin-exp ofs end-exp))
         ; undo history is discarded when we leave browse mode,
         ; we do delete afterwards so it will be recorded in new
         ; undo history
          (history-leave-browse-mode)
          (history-terminate-search)
          (send-base self delete-char disp)))]

    [delete (idx)
      (when (and (send self pos<=? begin-exp idx end-exp))
         ; undo history is discarded when we leave browse mode,
         ; we do delete afterwards so it will be recorded in new
         ; undo history
        (history-leave-browse-mode)
        (history-terminate-search)
        (send-base self delete idx))]

    [delete (beg end)
      (when (and (send self pos<=? begin-exp beg)
                 (send self pos<=? end end-exp))
         ; undo history is discarded when we leave browse mode,
         ; we do delete afterwards so it will be recorded in new
         ; undo history
        (history-leave-browse-mode)
        (history-terminate-search)
        (send-base self delete beg end))]

    [raw-delete (beg end)
      (send-base self delete beg end)]

    [insert (x)
     ;;
     ;; avoids markup bug that causes markup to bleed between ()
     ;;
     (set! entered? (or (string? x) (eqv? x #\newline)))
     (unless (send self pos<=? begin-exp (cursor) end-exp)
       (send self set-cursor-pos! end-exp))
     (history-leave-browse-mode)
     (history-terminate-search)
     (send-base self insert x)
     ;; following voodoo so we can read from the console input port, e.g.
     ;; in the debugger/inspector
     ;; if user pastes a selection that ends in a newline (+ whitespace)
     ;; process as if the newline had been typed by user.
     (when (or (and repl-port-read-mode? (string? x))
               (and (not repl-port-read-mode?)
                    (memv x '(#\newline #\return)))
               (and (string? x)
                    (not (string=? x ""))
                    (let nl? ([i (fx- (string-length x) 1)])
                      (and (fx>= i 0)
                           (let ([c (string-ref x i)])
                             (or (char=? c #\newline)
                                 (nl? (fx- i 1))))))))
       (thread-send-msg c/r-ready #t))]

    [repl-new-cafe (hcount)
      (set! my-thread (thread-self))
      (set! repl-port (open-repl-port))
      (set! history-counter hcount)
      (display-history-counter #f) ; after set! of history-counter
      (send self set-focus)
      ;;
      ;; do not use parameterize here, or user-forked threads won't
      ;; pick up the correct values.
      ;; 
      (console-input-port repl-port) (console-output-port repl-port)
      (console-error-port repl-port)

; maybe do this redirection conditionally depending on setting of some
; preference
;  - might be nice to have a swl:repl-start-hook that gets invoked as the first
;    thing done in the new repl thread (w/o echoing perhaps?)
;  - for now, going with the global interaction window for current i/o ports
;    (for the record, Kent's approved this)
; 
;     (current-input-port repl-port)
;     (current-output-port repl-port)

      (when (top-level-bound? 'trace-output) (trace-output repl-port))
      (when (top-level-bound? 'trace-output-port)
        (trace-output-port repl-port))

      (display (#%$scheme-greeting) (console-output-port))
      (newline (console-output-port))

      (waiter-prompt-and-read
        (lambda (n)

          (critical-section
            (flush-output-port repl-port) ;;; needed for correct oldwmk
            (let ([oldwmk (send self fixed-mark write-mk)])
              (let ([wps (waiter-prompt-string)])
                (unless (string=? wps "")
                  (let loop ([n n])
                    (unless (fxzero? n)
                      (display wps (console-output-port))
                      (loop (fx- n 1))))
                  (display #\space (console-output-port))))
              (flush-output-port repl-port)
              (set! write-mk oldwmk)
              (send self make-visible end-exp)
              (let ([x (waiter-read)])
                (send self raw-delete end-exp end-mk)
                x)))))

      (waiter-write
       (lambda (x)
         (unless (eq? x (void))
           (when (string? x) (set! x (remove-unprintables x)))
           (pretty-print x (console-output-port))
         (flush-output-port (console-output-port)))))

      (call/cc
       (lambda (k)
         (set! exit-k k)
         (new-cafe eval)))
      (set! exit-k #f) ;;; see destroy method
      (void)]

    [set-traffic-light! (tl) (set! traffic-light tl)]

        ) ;;; end public
      )

#!eof
