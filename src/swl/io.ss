;; Copyright (c) 1996 Carl Bruggeman and Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;;
;; Notes (johnz) - this version does _not_ do port-level buffering
;;  in order to get newline semantics correct.
;;

;; Notes (Oscar) [obsoleted by johnz's fixes]
;;
;;   This version of the non-blocking I/O ports uses port-level buffering
;;   in order to sacrifice less speed.  This means inlined calls to
;;   write-char in library routines such as pretty-print can simply hammer
;;   characters into the port buffer.  It also means they completely bypass
;;   the port-handler and its mutex.  Because write operations may block
;;   (eg. to a pipe), it is possible for one thread to block in the nb-write
;;   of flush-output-port while another thread diddles the port buffer
;;   and output index directly.  To avoid problems in flush-output-port,
;;   we enter a critical section, set the port-output-index to 0, and
;;   copy the buffer in nb-write before waiting for the underlying output
;;   channel to become writable.
;;   
;;   I tried another implementation that eschews port-level buffering to
;;   force all port-operations to go through the port-handler (which is
;;   guarded by a mutex).  The output port is then buffered internally.
;;   This simplifies the code somewhat, but trashes performance (see the
;;   third set of numbers for (time (foo)) below).
;;
;;   Both these versions fix a serious bug in the 0.9j SWL io.ss (and all
;;   earlier nb-io.ss) which could hang the system for something as simple
;;   as (test 4) where test is:
;;
;;      (define test
;;        (lambda (n)
;;          (define foo (lambda (x) (flush-output-port) x))
;;          (let ([q (thread-make-msg-queue 'test)])
;;            (let loop ([n n])
;;              (unless (fxzero? n)
;;                (printf "forked thread ~s~n" n)
;;                (thread-fork-group (lambda () (thread-send-msg q (foo n))))
;;                (loop (fx- n 1))))
;;            (let loop ([n n])
;;              (unless (fxzero? n)
;;                (printf "thread ~s completed~n" (thread-receive-msg q))
;;                (loop (fx- n 1)))))))

; The performance consequence of this correctness stuff.
;
; (define foo  
;   (lambda ()
;     (let ((s (make-string 79 #\*)))
;       (let f ((n 5000))
;         (unless (fxzero? n)
;           (display s) (newline) (f (fx- n 1)))))))
;
;;;;;;;;;;;;;;;;;;;; SWL 0.9j    (with broken io.ss)
;(time (foo))
;    no collections
;    920 ms elapsed cpu time
;    3815 ms elapsed real time
;    570472 bytes allocated
;
;;;;;;;;;;;;;;;;;;;; SWL 0.9k  (port-level buffering)
;(time (foo))
;    1 collection
;    1140 ms elapsed cpu time, including 0 ms collecting
;    6818 ms elapsed real time, including 2 ms collecting
;    1545048 bytes allocated, including 1050712 bytes reclaimed
;
;;;;;;;;;;;;;;;;;;;; SWL 0.9k  (no port-level buffering, but internal buffering)
;(time (foo))
;    89 collections
;    5260 ms elapsed cpu time, including 80 ms collecting
;    10678 ms elapsed real time, including 87 ms collecting
;    94681808 bytes allocated, including 94707944 bytes reclaimed

;; BUGS
;;  - closing only one side of a two-way port currently closes both sides.
;;  ? need to come back and think about whether input ports can be compromised
;;    like the output ports were (right now I doubt it)


(define swl:init-io-subsystem
  (lambda ()
    (system-set! 'transcript-output-port
      (thread-make-parameter
        #f
        (lambda (x)
          (let ([curr (transcript-output-port)])
            (unless (or (not x) (output-port? x))
              (assertion-violationf 'transcript-on "~s is not #f or an output port" x))
            (when (and x curr (not (eq? x curr)))
              (warningf #f
                "transcript ~s replaces transcript ~s already in progress"
                (port-name x)
                (port-name curr)))
            x))))
    (system-set! 'transcript-on
      (lambda (fn)
        (let ([op (open-output-file fn 'truncate)])
          (flush-output-port (console-output-port))
          (transcript-output-port op)
          (fprintf op "Chez Scheme (SWL) Transcript [~a]~n" (date-and-time)))))
    (system-set! 'transcript-off
      (lambda ()
        ;; Do not close the port here, leave that for the garbage collector.
        ;; Another thread may be writing to the same transcript.  Ideally the
        ;; only threads writing to the transcript would be those forked by the
        ;; thread that initiates transcript-on.  With a copy-on-write semantics
        ;; for thread parameters, we could close the port when transcript-off
        ;; is called from any of the threads sharing the same parameter value.
        ;; As it stands, we've no good way to prevent the other threads from
        ;; writing to a closed output port.
        (let ((p (transcript-output-port)))
          (and p (flush-output-port p)))
        (transcript-output-port #f)))))

(let ()

(define-syntax mutex-acquire
  (syntax-rules ()
    ((_ m) (thread-receive-msg m))))
(define-syntax mutex-release
  (syntax-rules ()
    ((_ m) (thread-send-msg m #t))))
(define-syntax with-mutex
  (syntax-rules ()
    ((_ mutex e0 e1 ...)
     (let ((m mutex))
       (dynamic-wind 
        (lambda () (mutex-acquire m))
        (lambda () e0 e1 ...)
        (lambda () (mutex-release m)))))))

(define-syntax mutex-dispatcher
  (syntax-rules ()
    ((_ pattern mutex delegate (key fmls e0 e1 ...) ...)
     (let ((m mutex))
       (letrec ((key (lambda fmls e0 e1 ...)) ...)
         (with-mutex m
           (record-case pattern
             [key fmls (key . fmls)] ...
             [else delegate])))))))

(define swl:set-channel-option!
  (lambda (chan option value)
    (swl:tcl-eval 'fconfigure chan option value)))

(define swl:get-channel-option
  (lambda (chan option) (swl:tcl-eval 'fconfigure chan option)))

(define swl:os-close
  (lambda (channame) (swl:tcl-eval 'close channame)))


(define mk-nb-input-handler
  (lambda (channame channel name delegate)
    (define gc-sticky
      (swl:procedure->callback
        (lambda () (thread-send-msg wait-q #t))
        'swl:system-queue))
    (define wait-q (thread-make-msg-queue 'nb-read))
    (define nb-read
      (lambda (p channel str idx cnt block?)
          (let loop ([idx idx] [cnt cnt])
            (when block?
              (swl:tcl-eval 'fileevent channame 'readable gc-sticky)
              (thread-receive-msg wait-q)
              (swl:tcl-eval 'fileevent channame 'readable '()))
            (let ([res (swl:os-c-read-n channel str idx cnt)])
              (cond
                [(fx= res -1) (assertion-violationf 'nb-read (swl:os-get-error))]
                [(fx> res 0)
                 (when (and (transcript-output-port)
                            (eq? p (console-input-port)))
                   (block-write (transcript-output-port) str res))
                 res]
                [(fx= res 0)
                 (if (swl:os-eof channel) #!eof (and block? (loop idx cnt)))])))))
    (define sem (thread-make-msg-queue 'nb-read-sem))
    (thread-send-msg sem #t)
    (swl:set-channel-option! channame '-blocking 0)
    (swl:set-channel-option! channame '-translation 'binary)
    (lambda (args)
      (mutex-dispatcher args sem delegate
        [block-read (p str cnt)
         (console-io-bug-flush p)
         (let ([i (port-input-index p)]
               [s (port-input-size p)]
               [b (port-input-buffer p)])
           (if (fx< i s)
               (let ([cnt (fxmin cnt (fx- s i))])
                 (do ([i i (fx+ i 1)] [j 0 (fx+ j 1)])
                     ((fx= j cnt))
                     (string-set! str j (string-ref b i)))
                 (set-port-input-index! p (fx+ i cnt))
                 cnt)
               (nb-read p channel str 0 cnt #t)))]
        [read-char (p)
         (let ([c (peek-char p)])
           (unless (eof-object? c)
             (set-port-input-index! p (fx+ (port-input-index p) 1)))
           c)]
        [peek-char (p)
         (console-io-bug-flush p)
         (let ([i (port-input-index p)]
               [s (port-input-size p)]
               [b (port-input-buffer p)])
           (if (fx< i s)
               (string-ref b i)
               (let ([res (nb-read p channel b 0 (string-length b) #t)])
                 (if (eof-object? res)
                     res
                     (begin (set-port-input-index! p 0)
                            (set-port-input-size! p res)
                            (string-ref b 0))))))]
        [unread-char (c p)
         (let ([i (port-input-index p)])
           (when (fx= i 0)
             (assertion-violationf 'unread-char "tried to unread too far on ~s" p))
           (set-port-input-index! p (fx- i 1)))]
        [char-ready? (p)
         (console-io-bug-flush p)
         (let ([i (port-input-index p)] [s (port-input-size p)])
           (or (fx< i s)
               (let ([b (port-input-buffer p)])
                 (let ([res (nb-read p channel b 0 (string-length b) #f)])
                   (and res
                        (not (eof-object? res))
                        (begin (set-port-input-index! p 0)
                               (set-port-input-size! p res)
                               #t))))))]
        [clear-input-port (p) (set-port-input-size! p 0)]
        [close-port (p)
         (set-port-input-size! p 0)
         (mark-port-closed! p)
         (swl:set-channel-option! channame '-blocking 1)
         (swl:os-close channame)
         (void)]
        [port-name (p) name]
        [file-position (p) (most-negative-fixnum)]
        [port-number (p)
         (on-error #f
           (let ([s (swl:get-channel-option channame '-sockname)])
             (let ([len (string-length s)])
               (let loop ([i (- len 1)])
                 (if (or (fx< i 0) (char=? (string-ref s i) #\space))
                     (string->number (substring s (+ i 1) len))
                     (loop (- i 1)))))))]
        [gchack () gc-sticky]
        ))))

  ; Carl says:
  ;
  ; Chez Scheme current behavior in the C kernel code is to check in
  ; speek-char, sblock_read, and schar-ready to flush the console output
  ; port if the operation is to be performed on the console input port.
  ; Unless this behavior is modified in Chez Scheme, we must mimic it
  ; here because at least the inspector depends on this behavior (although
  ; the remainder of the system appears to work without it).

  ; Real fix is to make the console a two-way port and implement flushing
  ; as appropriate

(define console-io-bug-flush
  (lambda (p)
    (when (eq? p (console-input-port))
      (flush-output-port (console-output-port)))))

(define mk-nb-output-handler
  (lambda (channame channel name delegate newline-queue obufsize)
    (define wait-q (thread-make-msg-queue 'nb-write))
    (define gc-sticky
      (swl:procedure->callback
        (lambda () (thread-send-msg wait-q #t 'urgent))
        'swl:system-queue))
    (define idx 0)
    (define buf) ;;; do double-buffering
    (define buf1 (make-string (1+ obufsize)))
    (define buf2 (make-string (1+ obufsize)))
    (define swap-buffers
      (lambda ()
        (set! idx 0)
        (set! buf (if (eq? buf buf1) buf2 buf1))))
    (define nb-write
      (lambda (p str cnt)
        (when (eq? str buf1) ;;; perf hack; halves frequency of fileevents
          (swl:tcl-eval 'fileevent channame 'writable gc-sticky)
          (thread-receive-msg wait-q)
          (swl:tcl-eval 'fileevent channame 'writable '()))
        (when (and (transcript-output-port) (eq? p (console-output-port)))
          (block-write (transcript-output-port) str cnt))
        (let ([res (swl:os-c-write-n channel str 0 cnt)])
          (cond
            [(fx= res -1) (assertion-violationf 'nb-write (swl:os-get-error))]
            [(fx= res cnt) (void)]))))
        
    (define sem (thread-make-msg-queue 'nb-write-sem))
    (thread-send-msg sem #t)
    (swl:set-channel-option! channame '-blocking 0)
    (swl:set-channel-option! channame '-translation 'binary)
    (set! buf buf1)
    (case-lambda
      [(op c p)
       (case op
         [(write-char)
          (let loop () 
            (if (fx< idx obufsize)
                (begin
                  (disable-interrupts) ;;; [ "lightweight c-s" for common case
                  (string-set! buf idx c) (set! idx (fx1+ idx))
                  (enable-interrupts)  ;;; ]
                  (when (fx>= idx obufsize) ;;; may be > by 1 if preempted
                    (critical-section ;;; protect buf from preemption
                      (with-mutex sem ;;; protect buf in case we yield
                         (when (fx>= idx obufsize)
                           (let ([b buf] [i idx])
                             (swap-buffers)
                             (nb-write p b i)))))))
                (begin
                  ;;
                  ;; To reach here we must have multiple threads producing
                  ;; output and nb-write is probably blocked. Best thing we
                  ;; can do now is to yield and try again.
                  ;;
                  (thread-yield)
                  (loop))))
          (when (and (char=? c #\newline)
                     (not (thread-msg-waiting? newline-queue)))
            (thread-send-msg newline-queue #t))
          ]
         [else delegate])]
      [(args)
      (record-case args
        [block-write (p str cnt)
          (when (fx> idx 0)
            (critical-section ;;; see above
              (with-mutex sem
                (when (fx> idx 0)
                  (let ([b buf] [i idx])
                    (swap-buffers)
                    (nb-write p b i))))))
          (with-mutex sem
            (nb-write p str cnt))]
        [flush-output-port (p)
          (critical-section ;;; see above
            (with-mutex sem
              (when (fx> idx 0)
                (let ([b buf] [i idx])
                  (swap-buffers)
                  (nb-write p b i)))))
          (swl:os-flush channel)
          (void)]
        [close-port (p)
          (flush-output-port p)
          (set-port-output-size! p 0)
          (mark-port-closed! p)
          (swl:set-channel-option! channame '-blocking 1)
          (swl:os-close channame)
          (void)]
        [port-name (p) name]
        [clear-output-port (p) (set! idx 0)]
        [gchack () gc-sticky]
        [else delegate]
        )])))

(set! swl:open-nb-input-port
  (lambda (channel name buffer)
    (let ([p
           (make-input-port
             (let ([handler
                    (mk-nb-input-handler
                      channel (swl:get-channel channel)
                      name
                      (lambda (args) (assertion-violationf #f (format "~s" args))))])
               (lambda args (handler args)))
             buffer)])
      (clear-input-port p)
      p)))

(set! swl:open-nb-output-port
  (lambda (channel name buffer)
    (let ([newline-queue (thread-make-msg-queue 'nlq)])
      (letrec ([flush-output-thread
                (thread-fork
                 (lambda ()
                   (let f ()
                     (thread-name "nb-output")
                     (thread-receive-msg newline-queue)
                     (flush-output-port p)
                     ;; slow the flush rate for improved performance.
                     (thread-sleep 50)
                     (f))))]
               [p (make-output-port
                   (let ([handler
                          (mk-nb-output-handler
                           channel
                           (swl:get-channel channel)
                           name
                           (lambda (args) (assertion-violationf #f (format "~s" args)))
                           newline-queue
                           (string-length buffer))])
                     (lambda args (handler args)))
                   buffer)])
        (clear-output-port p)
        (set-port-output-size! p 0) ;;; always invoke handler
        p))))

(set! swl:make-nb-input-output-port
  (lambda (name in-chan ibufsize out-chan obufsize)
    (let ((delegate-flag (string #\a)))
      (let ([newline-queue (thread-make-msg-queue 'nlq)])
        (define (safe-flush p)
          (critical-section
            (unless (port-closed? p) (flush-output-port p))))
        (define p
          (make-input/output-port
            (let ([ih
                   (mk-nb-input-handler
                     in-chan
                     (swl:get-channel in-chan)
                     name
                     delegate-flag)])
              (let ([oh
                     (mk-nb-output-handler
                       out-chan
                       (swl:get-channel out-chan)
                       name
                       delegate-flag
                       newline-queue
                       obufsize)])
                (define (unhandled-operation op)
                  ; name is a valid "who" argument if we're a socket port
                  (assertion-violationf (format "~s" name) "unhandled operation: ~s" op))
                (case-lambda
                  [(op a1 a2)
                   (let ([result (oh op a1 a2)])
                     (if (eq? result delegate-flag)
                         (let ([result (ih (list op a1 a2))])
                           (if (eq? result delegate-flag)
                               (unhandled-operation op)
                               result))
                         result))]
                  [args
                   (let ([result (oh args)])
                     (if (eq? result delegate-flag)
                         (let ([result (ih args)])
                           (if (eq? result delegate-flag)
                               (unhandled-operation (car args))
                               result))
                         result))])))
            (make-string ibufsize)
            ""))
        (clear-input-port p)
        (clear-output-port p)
        (set-port-output-size! p 0) ;;; always invoke handler
        (thread-fork
          (lambda ()
            (thread-name "nb-io")
            (call/cc
              (lambda (k)
                (thread-become-server!
                  (lambda () (safe-flush p) (k)))))
            (let f ()
              (thread-receive-msg newline-queue)
              (safe-flush p)
              (unless (port-closed? p) (thread-sleep 100) (f)))))
        p))))

) ; end let

