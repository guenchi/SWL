; wow, the critical sections here are totally brain-dead
;  - instead, perhaps we can serialize access using a dispatch thread
;    that reads "events" from a thread message queue

; OLD NEW PLAN
;  - single thread managing port internals, all port messages dispatched to
;    this thread for service
;  - elegant, but we need a bounded work queue so that one or more fast-writing
;    threads cannot swamp the port handler thread
;     - in the limit, a bounded queue w/ size 1 is just a slow
;       critical section
;
; NEW NEW PLAN
;  - much faster and simpler to service output messages directly
;    within a critical section
;     - pay as you go solution --- can't have threads ganging up on
;       the poor unfortunate service thread (bounded queue code didn't
;       just drop in place, but I was able to use a semaphore for the
;       job, and it worked ok, but was slow and the code was more complex
;  - let's have a flusher thread that either polls or is waked when
;    we transition from clean to dirty
;     - polling was dirt simple and actually performed well w/ little load
;     * we still retain the advantage of having just one thread to talk
;       to the widget internals, so that part of the code is single-threaded
;         - seems as if multiple threads might be able to starve the flusher,
;           but I haven't seen it in practice (haven't tested w/ lots of threads
;           yet, though)
;  - flush-output-port port message will simply enqueue a true-flush
;    request with the value of get-output-string called from w/in a
;    critical section
;  - keep line-length and line-count limitations (port could compute
;    the number of lines to purge, maybe it could even avoid sending
;    them somehow, but that doesn't seem to be crucial)
;  ? maybe we can go the same route with the input code and dispense with
;    the code that creates a queue to block on until the message is serviced

#|

* Control-C binding doesn't make sense here (which thread would we interrupt?)

* test all port messages char-ready? etc. before and after the changes

? any bugs w/ mini-buffer not being there ---> key-press-prefix

? things to add:
   - Edit menu
      - search
      - copy/cut/paste the way johnz did them
   - "send EOF"  -- they can do this already w/ destroy, but then they
                    lose view of what was before in the buffer...

|#

; - Modify font dialog we can include color as well.
;    
; * threads.ss
; ---> fix keyboard-interrupt-handler
;       - currently seems to cause panic in thread-become-console! complaining
;         that a new-cafe must not be installed.
;       - see the PRCS branch called 5am-thread-hacking where I started
;         to fix this by making console-thread a per-thread parameter
; - prevent editing of text that has been read or written
; - figure out why there is a long delay when it first starts up
;   (something in the message queue stuff?)
;     * turns out it's a known issue w/ Tk and KDE's window manager
; - [someday] modify to show which characters of the input have actually been
;   consumed?

(module (open-interaction-window)

 ; export user-visible version
 (module (open-interaction-window)
   (define open-interaction-window
     (let ()
       (import swl:module-setup)
       (case-lambda
         [() (open-interaction-window)]
         [(title) (open-interaction-window title)]))))

 ; install the actual system version
 (module ()

  (import swl:oop)
  (import swl:macros)
  (import swl:option)
  (import swl:generics)
  (import swl:threads)
  (import swl:module-setup) ; exports open-interaction-window that we're setting

  (set! open-interaction-window
    (rec open-interaction-window
      (case-lambda
        [() (open-interaction-window "Interaction window")]
        [(title-val)
         (define max-chars (* 100 1024))
         (define top #f)
         (define txt #f)
         (define lineq (thread-make-msg-queue 'input))
         (define input-markup #f)
         (define output-markup #f)
         (define title (string-copy title-val))
         (define focus-on-input #t)   ; these defaults will keep students
         (define raise-on-input #t)   ; out of trouble
         (define raise-on-output #t)
         (define-swl-class (<my-text> p) (<scheme-text> p)
           (ivars [last-mark #f] [killable (make <sem> 1)])
           (inherited begin-exp end-exp)
           (inheritable)
           (private
             [split-and-enqueue (bol)
              (remove-paren-markup)
              (let ([start (if (send self pos>? last-mark bol) last-mark bol)])
                (let ([line (send self get-string start 'insert)])
                  (set! last-mark (send self fixed-mark 'insert))
                  (send input-markup apply-markup txt start last-mark)
                  (let ([ip (open-input-string line)] [op (open-output-string)])
                    (let loop ()
                      (let ([c (read-char ip)])
                        (if (eof-object? c)
                            (let ([i (port-output-index op)])
                             ; if we didn't consume all the input that was
                             ; pasted in, back up and let the user edit it
                              (set! last-mark (send self add-offset last-mark (- i))))
                            (begin
                              (write-char c op)
                              (when (char=? c #\newline)
                                (send self discard-undo-state!)
                                (thread-send-msg lineq (get-output-string op)))
                              (loop))))))))])
           (protected)
           (public
             [init (p)
              (send-base self init p)
              (set! last-mark (send self fixed-mark (send self get-cursor-pos)))]
             [flush-output! (str)
              (send killable wait)
              (let ([enabled? (send self get-undo-enabled)])
                ; disable undo so that our inserts and deletes are not recorded
                (send self set-undo-enabled! #f)
                (let ([mk (send self floating-mark last-mark)])
                  (send self raw-insert-at mk str)
                  (let ([prev last-mark])
                    (set! last-mark (send txt fixed-mark mk))
                    (send output-markup apply-markup txt prev last-mark)
                    (let ([last (send self add-offset last-mark 0)])
                      (let ([excess (- (send self index->offset last) max-chars)])
                        (when (> excess 0)
                          (let ([cut (send self add-offset '(0 . 0) excess)])
                            (send-base self delete '(0 . 0)
                              (if (= (cdr cut) (cdr last))
                                  cut
                                  (cons 0 (+ (cdr cut) 1)))))))))
                 ; limit paren-bounce search to user input (not text printed to console)
                  (set! begin-exp last-mark)
                  (send self make-visible last-mark))
                (send self set-undo-enabled! enabled?))
              (send killable signal)]
             [prepare-to-die () (send killable wait)]
             [insert (x)
              (if (char? x)
                  (send-base self insert x)
                  (let ([bol (send self line-start (send self get-cursor-pos))])
                    (send self set-cursor-pos! 'end)
                    (send-base self insert-at 'insert x)
                    (split-and-enqueue bol)))]
             [delete (index1)
              (when (and (send self pos>=? index1 last-mark)
                         (not (and (send self cursor-bol?)
                                   (send self cursor-eol?))))
                (send-base self delete index1))]
             [delete (index1 index2)
              (when (and (send self pos>=? index1 last-mark)
                         (send self pos>=? index2 last-mark))
                (send-base self delete index1 index2))]
             [delete-char (disp)
              (let ([new (send self add-offset 'insert disp)])
                (when (send self pos>=? new last-mark)
                  (send-base self delete-char disp)))]
             [delete-eol ()
              (when (send self pos>=? 'insert last-mark)
                (send-base self delete-eol))]
             [set-cursor-pos! (new)
             ; don't set position outside editable region, but do scroll the
             ; position into view so that searches will scroll hits into view
              (if (send self pos>=? new last-mark)
                  (send-base self set-cursor-pos! new)
                  (send self make-visible new))]
             [key-press (key mods)
             ; get beginning of line before the insert
              (let ([bol (send self line-start (send self get-cursor-pos))])
                (send-base self key-press key mods)
                (event-case ((key= key) (modifier= mods))
                  (([alt #\q]) (send top destroy))
                  (([#\newline] [#\return] [control #\j] [control #\m])
                   (split-and-enqueue bol))))]
             [cursor-bol () (send self set-cursor-pos! last-mark)]))

         (define select-font
           (lambda (obj key for-what)
             (swl:font-dialog top for-what
               (swl:font-families 'fixed)
               '(6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 28 32)
               '(bold normal italic)
               (lambda () (or (send obj get-font) (send txt get-font)))
               (lambda (fnt)
                 (when fnt
                  ; use top for prefs since <markup> class doesn't understand
                  ; preferences yet
                   (send top set-pref! key fnt)
                   (send obj set-font! fnt))))))

         (define window-sem (make <sem> 1))

         (define show-window
           (lambda (msg)
             (send window-sem wait)
             (unless top (real-show-window))
             (when (string? msg) (send txt flush-output! msg))
             (send window-sem signal)))

         (define-syntax pref-check-menu-item
           (syntax-rules ()
             [(_ keyvar label)
              (create <check-menu-item> with
                (title: label)
                (selected: keyvar)
                (prefs-key: 'keyvar)
                (action:
                  (lambda (item)
                    (set! keyvar (send item get-selected)))))]))

         (define-syntax pref-radio-menu-item
           (syntax-rules ()
             [(_ keyvar val)
              (pref-radio-menu-item keyvar val (format "~s" val))]
             [(_ keyvar val label)
              (create <radio-menu-item> with
                (title: label)
                (selected: (equal? val keyvar))
                (prefs-key: 'keyvar)
                (action:
                  (lambda (item)
                    (when (send item get-selected)
                      (set! keyvar val)))))]))

         (define real-show-window
           (lambda ()
            ; q is used to pause real-show-window until the top-level
            ; interaction window has been created and initialized
             (let ([q (thread-make-msg-queue 'show-window)])
               (swl:begin-application
                 (lambda (token)
                   (thread-fork
                     (lambda ()
                       (set! top
                         (create <app-toplevel> with
                           (title: title)
                           (prefs-tag: 'interaction-window)
                           (destroy-request-handler:
                             (lambda (self)
                               (send window-sem wait)
                               (send txt prepare-to-die)
                               (let ([q lineq])
                                 (set! lineq (thread-make-msg-queue 'input))
                                 (set! top #f)
                                 (set! txt #f)
                                 (thread-send-msg q "")
                                 (swl:end-application token)
                                 (send window-sem signal))
                               #t))))
                       (let ([sf (create <scrollframe> top with (sticky-vscroll: #t))])
                        ; safer to create everything fresh
                         (set! txt (create <my-text> sf with (background-color: 'white)))
                         (send top notify-text txt)
                         (set! input-markup (create <markup>))
                         (set! output-markup (create <markup>))
                         (set-font! txt
                           (send top get-pref 'default-font
                             (create <font> 'courier 12 '())))
                         (let ([font (send top get-pref 'input-font #f)])
                           (when font (set-font! input-markup font)))
                         (let ([font (send top get-pref 'output-font #f)])
                           (when font (set-font! output-markup font)))
                         (send top set-menu! 
                           (make-menu
                             ("_File"
                               (make-menu
                                 ("_Save transcript..."
                                   (lambda (item)
                                     (let ([filename
                                            (swl:file-dialog
                                              "Specify a file in which to save the transcript."
                                              'save
                                              (parent: top))])
                                       (when filename
                                         (with-output-to-file filename
                                           (lambda ()
                                             (display (send txt get-string '(0 . 0) 'end)))
                                           'truncate)))))
                                 ('("_Close" . "Alt-q") (lambda (item) (send top destroy)))))
                             ("_Edit"
                               (make-menu
                                 ; not advertising the Undo / Redo functionality
                                 ; even though it's available via key bindings
                                 ('("_Go to line..." . "Ctrl+g")
                                   (lambda (item)
                                     (send txt ask-goto-line)))
                                 ('("Forward S-expression" . "Ctrl+0")
                                   (lambda (item)
                                     (send txt move-to-match 'forward)))
                                 ('("Backward S-expression" . "Ctrl+9")
                                   (lambda (item)
                                     (send txt move-to-match 'backward)))
                                 ('("Select S-expression Forward" . "Ctrl+)")
                                   (lambda (item)
                                     (send txt select-to-match 'forward)))
                                 ('("Select S-expression Backward" . "Ctrl+(")
                                   (lambda (item)
                                     (send txt select-to-match 'backward)))
                                 ('("Search _Forward" . "Ctrl+f")
                                   (lambda (item)
                                     (send txt search-forward)))
                                 ('("Search _Reverse" . "Ctrl+r")
                                   (lambda (item)
                                     (send txt search-backward
                                       )))
                               ))
                             ("_Preferences"
                               (make-menu
                                 ("_Default Font"
                                   (lambda (item)
                                     (select-font txt 'default-font
                                       "Select a default text font")))
                                 ("_Input Font"
                                   (lambda (item)
                                     (select-font input-markup 'input-font
                                       "Select a font for user input")))
                                 ("_Output Font"
                                   (lambda (item)
                                     (select-font output-markup 'output-font
                                       "Select a font for program output")))
                                 ("_Limit scrollback"
                                   (create <menu>
                                     (list
                                      ; unlimited output buffer size is an invitation to crash
                                       (pref-radio-menu-item max-chars (* 1 1024) "1K chars")
                                       (pref-radio-menu-item max-chars (* 10 1024) "10K chars")
                                       (pref-radio-menu-item max-chars (* 100 1024) "100K chars")
                                       (pref-radio-menu-item max-chars (* 1024 1024) "1M chars")
                                       (pref-radio-menu-item max-chars (* 10 1024 1024) "10M chars"))))
                                 ("_Toggles"
                                   (create <menu>
                                     (list (create <check-menu-item> with
                                             (title: "_Auto-indent input")
                                             (selected: (send txt get-auto-indent))
                                             (prefs-key: 'auto-indent)
                                             (action:
                                               (lambda (item)
                                                 (send txt set-auto-indent!
                                                   (send item get-selected)))))
                                           (pref-check-menu-item focus-on-input "_Focus on input")
                                           (pref-check-menu-item raise-on-input "_Raise on input")
                                           (pref-check-menu-item raise-on-output "Raise on _output")
                                           (create <check-menu-item> with
                                             (title: "_Wrap text")
                                             (selected: #t)
                                             (prefs-key:
                                               (case (machine-type)
                                                 [(i3nt ti3nt i3osx ppcosx) 'wrap-text]
                                                 [else #f]))
                                             (enabled:
                                              ; force word-wrap 'char for X11-based Tcl/Tk to avoid
                                              ; bugs w/ overly long lines blowing internal Xlib limits
                                               (case (machine-type)
                                                 [(i3nt ti3nt i3osx ppcosx) #t]
                                                 [else #f]))
                                             (action:
                                               (lambda (item)
                                                 (send txt set-wrap!
                                                   (if (send item get-selected) 'char 'none)))))
                                           )))
                                 ("_Save preferences"
                                   (lambda (ignore)
                                     (send top save-prefs! 'interaction-window)))))
                             ('help-menu "quick hack -- ignore this part")))
                         (set-relief! (get-menu top) 'flat)
                         (set-border-width! (get-menu top) 0)
                         (pack sf (expand: #t) (fill: 'both))
                         (send txt set-focus)
                         (thread-send-msg q #t))))
                   (lambda () (on-error 'cry (when top (send top destroy))))))
               (thread-receive-msg q))))

         (define inputq (thread-make-msg-queue "input"))

         (define sop (open-output-string))

         (module (mark-dirty schedule-flush)

           (define flushq (thread-make-msg-queue "flusher"))
           (define flush-ready (make <sem> 1))

           (define mark-dirty
             (lambda ()
               (send flush-ready signal)))

           (define schedule-flush
             (lambda (s)
               (thread-send-msg flushq s)))

           (thread-fork
            ; separate thread so flush-output-port can enqueue its goods directly
             (rec loop
               (lambda ()
                 (let ([s (thread-receive-msg flushq)])
                   ; avoid flushing empty strings since this screws up the placement
                   ; of the output markups so that output interspersed with input is all
                   ; marked as input (even though it seems to be read properly)
                   (unless (eqv? s "") (show-window s)))
                 (loop))))

           (thread-fork
             (rec loop
               (lambda ()
                 (thread-sleep 30)
                 (send flush-ready wait)
                 (thread-send-msg flushq
                   (critical-section
                     (#3%get-output-string sop)))
                 (loop))))

            )

         (define handler
           (lambda args

             (define punt
               (lambda (args)
                 (let ([rq (thread-make-msg-queue "read")])
                   (thread-send-msg inputq (list* (car args) rq (cdr args)))
                   (thread-receive-msg rq))))

             (record-case args
               [write-char (c p)
                (write-char c sop)
                (mark-dirty)]
               [block-write (p s n) ; args validated by block-write primitive
                ; allow interleaving so block-write is not atomic
                (do ([i 0 (+ i 1)]) ((= i n))
                  (write-char (string-ref s i) sop))
                (mark-dirty)]
               [flush-output-port (p)
                (critical-section (schedule-flush (get-output-string sop)))]
               [clear-output-port (p)
                (critical-section (clear-output-port sop))]
               [read-char (p) (punt args)]
               [block-read (p s n) (punt args)]
               [peek-char (p) (punt args)]
               [char-ready? (p) (punt args)]
               [clear-input-port (p) (punt args)]
               [unread-char (c p) (punt args)]
               [port-name (p) title]
               [file-length (p) 0]
               [close-port (p) (void)]
               [file-position (p) (most-negative-fixnum)]
               [file-position (p n)
                (assertion-violationf 'file-position "reposition on port ~s failed." p)]
               [else
                (assertion-violationf (car args) "unsupported operation on ~a" title)])))

         (thread-fork ; service input queue
          ; perhaps this could be simplified now that we've discarded the
          ; central port control queue, but it's probably not worth it in
          ; the long term
           (lambda ()
             (define buf "")
             (define len 0)
             (define next 0)
             (define get-line!
               (lambda (p)
                 (show-window #f)
                 (let ([current (swl:current-focus)])
                   (on-error 'ignore ; window may be killed, we don't care
                     (set-title! top
                       (string-append title " (awaiting user input)"))
                     (when (or focus-on-input raise-on-input)
                       (unless (thread-msg-waiting? lineq)
                         (when raise-on-input (send top show) (send top raise))
                         (when focus-on-input (send txt set-focus)))))
                   (let ([s (thread-receive-msg lineq)])
                     (on-error 'ignore
                       (set-title! top title)
                       (when (and focus-on-input current)
                         (send current set-focus)))
                     (set! buf s)
                     (set! next 0)
                     (set! len (string-length s))))))
             (let loop ()
               (let ([msg (thread-receive-msg inputq)])
                 (thread-send-msg (cadr msg) ; rq
                   (record-case msg
                     [peek-char (rq p)
                      (when (fx= next len) (get-line! p))
                      (if (fx= next len) #!eof (string-ref buf next))]
                     [read-char (rq p)
                      (when (fx= next len) (get-line! p))
                      (if (fx= next len)
                          #!eof
                          (let ([c (string-ref buf next)])
                            (set! next (fx+ next 1))
                            c))]
                     [block-read (rq p s n)
                      (let copy ([i next] [j 0])
                        (cond
                          [(fx= j n) (set! next i) n]
                          [(fx= i len) (get-line! p) (copy 0 j)]
                          [else
                           (string-set! s j (string-ref buf i))
                           (copy (fx+ i 1) (fx+ j 1))]))]
                     [char-ready? (rq p)
; bogus if we allow enqueue of empty lines
                      (or (not (fx= next len)) (and lineq (thread-msg-waiting? lineq)))]
                     [clear-input-port (rq p)
                      (set! buf "")
                      (set! next 0)
                      (set! len 0)
                      (when lineq
                        (let purge ()
                          (when (thread-msg-waiting? lineq)
                            (thread-receive-msg lineq)
                            (purge))))]
                     [unread-char (rq c p)
                      (cond
                        [(fx= next len)
                         (set! buf (string c))
                         (set! next 0)
                         (set! len 1)]
                        [(fx= next 0)
                         (set! buf (string-append (string c) buf))
                         (set! len (fx+ len 1))]
                        [else
                         (set! next (fx- next 1))])])))
               (loop))))

        ; no buffering, we need to go through the handler
         (make-input/output-port handler "" "")])))

 )

)




;--------------------------------------------------------------------------
; test code

#!eof


(define p (open-interaction-window))

(define repl
  (let ([n 0])
    (case-lambda
      [() (repl "> ")]
      [(prompt)
       (fluid-let ([n (+ n 1)])
         (call/cc
           (lambda (done)
             (parameterize ([exit-handler done])
               (let ([again (call/cc (lambda (k) k))])
                 (display n p)
                 (display prompt p)
                 (let ([result
                        (parameterize ([reset-handler
                                        (lambda () (again again))])
                          (let ([x (read p)])
                            (if (eof-object? x) (done) (eval x))))])
                   (unless (equal? result (void)) (pretty-print result p))
                   (again again)))))))
       (void)])))


(define (test n)
  (display "check for duplicate i and missing line 4225 around line 4226\n")
  (display "check for duplicate i and missing line 4681 around line 4682\n")
  (flush-output-port p)
  (do ([i 0 (+ i 1)]) ((= i n)) (fprintf p "i = ~s\n" i))
  (flush-output-port p))
 

; check that we're okay with the delayed flushing even with explicit
; calls to clear-output-port

(define (postflush p n)
  (unless (= n 0)
    (display "postflush " p)
    (write n p)
    (newline p)
    (flush-output-port p)
    (clear-output-port p)
    (postflush p (- n 1))))

#!eof

(let ([ip (open-interaction-window "Type stuff here")]
      [op (open-interaction-window "Read the output here")])
  (let f ()
    (write-char (char-upcase (read-char ip)) op)
    (f)))

(begin
  (set! stop #f)
  (let ([p (open-interaction-window "Check Race")])
    (import swl:threads)
    (thread-fork
      (lambda ()
        (let loop ([n 0])
          (unless stop (fprintf p "n = ~s\n" n) (loop (+ n 1)))))))
  (printf "ok, now try closing the window periodicallyn"))

; will we have a bug if we:
;  - have infinite loop generating output to window
;  - click exit swl (or invoke abort)
;  - this won't run destroy method, so we might get interaction windows
;    forking like crazy

(let ([p (open-interaction-window)])
  (import swl:threads)
  (let ([q (thread-make-msg-queue 'test)])
    (thread-fork
      (lambda ()
        (thread-send-msg q 'ok)
        (let f ()
          (display "testing\n" p)
          (f))))
    (thread-receive-msg q)
    (thread-sleep 2000)
    (abort)))


; I've also seen this pop up new (spurious windows)
; test how well we're protecting the internal port structures
(begin
  (define stop #f)
  (let ([p (open-interaction-window)])
    (import swl:threads)
(define sem (make-sem 1))
    (define (foo c)
      (thread-fork
        (lambda ()
          (let f ()
            (unless stop
              (sem 'wait)
              (write-char c p)
              (sem 'signal)
              (f))))))
    (thread-fork
      (lambda ()
        (let f () (flush-output-port p) (thread-sleep 100) (unless stop (f)))))
    (for-each foo (string->list "abcdefg\n"))
    (void)))


; (time (test0 1000000))    ==> 6210 ms   w/ 369 collections
; (time (test1 1000000))    ==> 1510 ms   w/  45 collections
; (time (test2 1000000))    ==>  120 ms   w/  no collections

; test speed of with-mutex in the case where there is no contention
(define test0
  (lambda (n)
    (import swl:macros)
    (let ([s (make <sem> 1)])
      (let loop ([n n] [x 0])
        (if (fx= n 0)
            x
            (let ([x (with-mutex s (fx+ n 1))]) (loop (fx- n 1) x)))))))

; test speed of <sem> class without safety of dynamic-wind / guard
; stuff in the case where there is no contention
(define test1
  (lambda (n)
    (import swl:macros)
    (let ([s (make <sem> 1)])
      (let loop ([n n] [x 0])
        (if (fx= n 0)
            x
            (let ([x
                   (begin
                     (send s wait)
                     (let ([new (fx+ n 1)]) (send s signal) new))])
              (loop (fx- n 1) x)))))))

; test speed of make-sem procedure w/o safety of dynamic-wind / guard
; stuff in the case where there is no contention
(define test2
  (lambda (n)
    (import swl:macros)
    (let ([s (make-sem 1)])
      (let loop ([n n] [x 0])
        (if (fx= n 0)
            x
            (let ([x
                   (begin
                     (s 'wait)
                     (let ([new (fx+ n 1)]) (s 'signal) new))])
              (loop (fx- n 1) x)))))))

; test speed of make-sem procedure w safety of guard
; stuff in the case where there is no contention
(define-syntax catch-error  ; this version permits (on-error (on-error ...) e1 e2 ...)
  (syntax-rules ()
    [(_ proc e1 e2 ...)
     (guard (c [#t (proc c)])
       (let () e1 e2 ...))]))

; even w/ error handler overhead, this is 1.32 times faster than test 4
(define test3
  (lambda (n)
    (import swl:macros)
    (let ([s (make-sem 1)])
      (let loop ([n n] [x 0])
        (if (fx= n 0)
            x
            (let ([x
                   (begin
                     (s 'wait)
                     (catch-error
                       (lambda (c) (s 'signal) (#%raise c))
                       (let ([new (fx+ n 1)]) (s 'signal) new)))])
              (loop (fx- n 1) x)))))))

(define test4
  (lambda (n)
    (import swl:macros)
    (import swl:threads)
    (import swl:oop)
    (let ([s (make <sem> 1)])
      (let loop ([n n] [x 0])
        (if (fx= n 0)
            x
            (let ([x
                   (begin
                     (send s wait)
                     (catch-error
                       (lambda (c) (send s signal) (#%raise c))
                       (let ([new (fx+ n 1)]) (send s signal) new)))])
              (loop (fx- n 1) x)))))))

(define test4-new
  (lambda (n)
    (import swl:macros)
    (import swl:threads)
    (import swl:oop)
    (let ([s (make <sem> 1)])
      (let loop ([n n] [x 0])
        (if (fx= n 0)
            x
            (let ([x (with-mutex s (fx+ n 1))])
              (loop (fx- n 1) x)))))))

; does with-mutex surrender the mutex when a thread blocks?
; no, it seems to keep the mutex
;  -- bit strange though, when using two different repls,
;     I found the printfs all started going to the second one
;     when foo unblocked from the (read p) call

(define bar
  (lambda ()
    (printf "bar n = ~s\n" n)
    (with-mutex m (if (> n 500000) (set! n 0) (set! n (+ n 1))))
    (bar)))

(define foo
  (lambda (p)
    (let ([x
           (with-mutex m
             (set! n (+ n 1))
             (let ([my-n n])
               (printf
                 "foo got mutex, set n to ~s, and is waiting for input\n"
                 n)
               (fprintf p "talk to foo> ")
               (let ([x (read p)])
                 (printf "foo got ~s\n" x)
                 (printf "  n=~s ~a\n" n (if (= n my-n) "... cool" " ... RATS!"))
                 x)))])
      (unless (eq? x 'done) (foo p)))))
