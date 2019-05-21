;; Copyright (c) 1996,1997 Carl Bruggeman & John Zuckerman.
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; Redefining printf is a shame since we'll lose the nice compile-time
; check on number of arguments ...

(eval-when (compile eval) (internal-defines-as-letrec* #t))

(module swl:threads
        (install-thread-system
         thread-become-server!
         thread-critical-section
         system-ref
         system-set!
         thread-default-quantum
         thread-fork
         thread-fork-group
         thread-highest-priority
         thread-kill
         thread-lowest-priority
         thread-make-msg-queue
         thread-make-parameter
         thread-msg-waiting?
         thread-name
         thread-receive-msg
         thread-receiver-waiting?
         thread-reschedule
         thread-run-queue-idle?
         thread-send-msg
         thread-sleep
         thread-sleep-check
         thread-yield
         <sem>
         thread-self
         thread-pps
         console-error-port
         unified-interaction-environment
)

(include "../../apps/common/semaphore.ss")

(define thread-become-server!)
(define thread-conout)
(define thread-default-quantum)
(define thread-fork)
(define thread-fork-group)
(define thread-highest-priority)
(define thread-kill)
(define thread-lowest-priority)
(define thread-make-msg-queue)
(define thread-make-parameter)
(define thread-msg-waiting?)
(define thread-name)
(define thread-receive-msg)
(define thread-receiver-waiting?)
(define thread-reschedule)
(define thread-run-queue-idle?)
(define thread-self)
(define thread-send-msg)
(define thread-sleep)
(define thread-sleep-check)
(define thread-yield)

(define thread-pps)
(define console-error-port)
(define unified-interaction-environment)

(define thread-current-input-port)
(define thread-current-output-port)

; These definitions are to limit scope of assignment w/o having to fix the code
(define cafe-level)
(define thread-r)

;;
;; WARNING: (This means you!) casually making changes to this file may
;; introduce bugs that can be extremely difficult to diagnose.
;; 

;; 10/13/98 - RKD - fixed prune-broken-weak-pointers! so that it doesn't
;;   assume that the list is nonempty (thanks to Bob Burger).  also changed
;;   redefine-parameter! to use top-level-value to reference parameters
;;   so that we don't have to disable cp0, and removed code to disable cp0.
;;
;; 1/97 - Extensive changes by John Zuckerman. It is now okay to
;;   use parameterize.
;;
;; 2/24/97 - corrected bug that prevented sleep queue from being checked
;;   often enough.
;;
;; 3/2/97 - added trace-output to the set of thread parameters.

;;
;; 3/5/97 - enqueue contained an unprotected critical section, which
;;    caused a many-threads test (foo) to fail.  Also, some other
;;    primitives lacked or had misplaced critical-sections.  Corrected
;;    these problems.
;;
;; 3/17/97 - the system interrupt counter is now a per-thread parameter
;;    (tcb-sic).  This enables us to eliminate the wind-out/wind-in
;;    from yields and starts.
;;
;;    Removed thread-load and thread-compile-file, which are no longer
;;    needed, since we no longer wind-out on a thread context switch.
;;
;; 3/21/97 - removed kbd-interrupt flag and made immediate interrupt
;;    of the console and running threads the default behavior.
;;
;; 3/24/97 - per discussions with Carl, changed thread-base-winders to '();
;;    decommissioned thread-critical-section.
;;
;; 4/11/97 - tuned the timer-interrupt-handler for better performance.
;;
;; 4/17/97 - added parameter thread-default-ticks.
;;
;; 4/18/97 - changed the term for an indefinitely looping thread from
;;    interpreter to server, which seems more consistent with
;;    common usage.
;;
;; 4/19/97 - removed (unused) defn for thread-dynamic-wind.
;;
;; 7/3/97 - changes to support improved user interface for debugger.
;;    * added structure: exception information block (eib).
;;    * added new threaded parameter process-exception.  Parameter
;;      last-exception (formerly last-error) is now non-threaded.
;;    * corrected problems with weak pointer lists (weak lists
;;      are now encapsulated by an ADT).
;;    * corrected problems with kbd interrupt handling.
;;
;; 7/7/97 - corrected bugs.
;;    * Problems with stack frames.
;;      Removed extra stack frame stuff.  Don't believe we need this anymore.
;;    * problems with detaching killed threads.
;;    * fixed problem with inspecting the exception of a thread
;;      that doesn't have one.
;;    * moved exception assignment from thread-error into
;;      error-handler, so that repl can call thread-error as part
;;      of parsing without affecting last- and process-exception.
;;
;; 7/10/97 - changed term "process" to "thread-group", which is less
;;   misleading.
;;
;; 8/11/97
;;    * corrected bug in timer-interrupt-handler: non-preemptable
;;      threads were scheduled as preemptable.  This was caused by
;;      changes introduced with the deactivation of thread-critical-section
;;      and the timed-preemption-counter.
;;
;;    * there may now be many server threads within a single thread group.
;;      thread-group-server is renamed to thread-group-servers, and
;;      holds a list of threads that are servers for the group.  A server
;;      is now given a local (per-thread) interrupt and reset handler.
;;
;;    * Overhaul of threaded parameters.  These may now be assigned
;;      per-thread or per-group (and may still default to a global value
;;      if no thread assignments have occurred).  The new extended
;;      interface is as follows:
;;
;;        To query a threaded parameter for its assignment mode:
;;        
;;        (<parameter> 'get-assignment-mode <any>)
;;        
;;          Returns the symbol "thread" if parameter assignment
;;          causes the value to be stored in the thread.
;;          Returns the symbol "group" if parameter assignment
;;          causes the value to be stored in the thread group
;;          (the default).
;;        
;;        To change the assignment mode of a threaded parameter:
;;        
;;        (<parameter> 'set-assignment-mode! x) 
;;          where x is one of the symbols: thread or group.
;; 
;;          Sets the assignment location of the parameter's value for
;;          the current thread.   If thread, the current parameter
;;          value (either group or global) is copied into the thread.
;;          If group, mode must already be set to thread or a warning
;;          will be issued. Otherwise, the current thread value is
;;          discarded.

;;        Other interface operations:
;;        
;;        (<parameter> 'get-global <any>)
;;        (<parameter> 'set-global! <new-value>)
;;        
;;        (<parameter> 'get-group <any>)
;;        (<parameter> 'set-group! <new-value>)
;;
;;        (<parameter> 'disable-mode-changes <any>) - disables change of
;;           assignment mode for this parameter.
;;
;;        (<parameter> 'get-value-for <thread or thread-group>)
;;
;;    * changed thread-group-name to thread-name, and thread-group-exception
;;      to thread-exception.  Made both parameters per-thread by default.
;;
;; 8/14/97 - redefined queue priority scheme.  enqueue takes a precedence
;;   argument, tcb has a precedence field, and qcb has a precedence procedure
;;   (precedence-proc) of one or two arguments, as follows: with one argument,
;;   a thread, precedence-proc returns the thread's queue precedence value.
;;   With two arguments, a thread and a new precedence, precedence-proc
;;   assigns the (enqueued) thread the new precedence value.
;;
;;   This permits precedence handling specialized for the run-queue
;;   and the sleep-queue.
;;
;;   redefined yield to take an optional precedence argument, which is
;;   simply passed to enqueue.
;;
;; 8/21/97 - minor change to eliminate possibility of forking, but
;;   not enqueuing a thread, which causes ??? to be output by pps for
;;   that thread's queue name.  Changed name queue-head to thread->queue.
;;
;; 12/26/97 - renamed thread-trav-run-queue to thread-traverse-run-queue.
;;
;; 12/29/97
;;   * thread-bootstrapped? is now a global variable.
;;   * fixed yield reentrancy test, which was broken by timer
;;     interrupt optimization.
;;   * added error-handler-continuation parameter to provide
;;     closer emulation of standard Chez Scheme user interface.
;;   * changed all calls to reset and error to thread-reset and
;;     thread-error.  This prevents problems if the user redefines
;;     reset or error. (This is a hack; better would be to load
;;     threads.ss in system mode.)
;;   * removed timed preemption counter code and tpc field from tcb and eib.
;;     removed disabled definitions of thread-critical-section,
;;     enable/disable-timed-preemption.  Feature was nonfunctional and
;;     added unnecessary complexity.
;;
;; 3/02/98 - restored inspector throw/catch code in the waiter.  This had
;;   been erroneously disabled in the 12/29/97 changes.
;;
;; 5/5/98
;;   * added thread-wake primitive.
;;   * changed thread-lowest- and thread-highest-priority to be procedures
;;     rather than global variables.

;;
;; Bugs
;;

;; 8/13/97 - kbd-int of the expr (let f () (thread-yield) (F))
;;   is highly likely to cause a panic.  Problem is in yield:
;;   we interrupt with current winders set to base winders.  Probable
;;   fix: need to extend crit sec protection around the change
;;   of winders.

;;
;; Definitions
;;

;;
;; Caveats
;;
;; * Be careful not to print cyclic structures to thread-conout when 
;;   debugging the thread system - this causes warning-handler to be 
;;   invoked, which may send output to a nonblocking output port if 
;;   installed, thoroughly confusing things.
;;
;; * some critical-sections are necessary in threads.ss because of
;;   the new capability of the keyboard interrupt handler to cause
;;   a forced context switch.  There are two problems with this:
;;   thread system performance is compromised somewhat; it is difficult
;;   to correctly identify all of the critical sections that exist in
;;   the thread code.
;;
;;   Keyboard initiated preemption should be viewed as a potentially
;;   unsafe operation.  Perhaps it should be disabled by default.
;;
;; * a thread continuation (tk) must be a procedure of one argument; the
;;   argument is a value to be passed to the (blocked) thread,
;;   or a post-restoration task to be performed prior to returning
;;   to the caller.
;;
;; * the timer-interrupt-hook is user code that may execute anything,
;;   including yield.  It will not be run if the timer interrupt occurs 
;;   when timed preemption is disabled.
;;
;; * In this version of threads.ss dynamic-wind winders are disabled during
;;   a thread context switch.  This avoids problems with arbitrary
;;   user code being executed in winders.
;;
;; * ticks are not charged to the current continuation whenever the
;;   timer is disabled (e.g., by critical-section).  This can adversely
;;   impact scheduling in certain cases.
;;
;; * a thread can be made effectively non-interruptable by installing
;;   an interrupt handler that doesn't reset or yield.
;;   

;;
;; Tips
;;
;; * to install for standalone testing: (1) uncomment the lines near the
;;   #!eof that start a new thread cafe; (2) load the file
;;   and eval: (install-thread-system).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define thread-bootstrapped? #f)

(define system-ref
  (lambda (symbol)
    (if (#%$sgetprop symbol '*flags* #f)
        (#%$top-level-value symbol)
        (top-level-value symbol))))

(define system-set!
  (lambda (symbol value)
    (if (#%$sgetprop symbol '*flags* #f)
        (#%$set-top-level-value! symbol value)
        (set-top-level-value! symbol value))))

(define-structure (tcb num
                       tk
                       params
                       tgcb ;;; thread group control block
                       next
                       prev
                       sic ;;; system interrupt counter
                       starts
                       winders
                       quantum
                       precedence))

(module ()
(let ()
  ;;
  ;; referenced during SWL build process.
  ;;
  (set! thread-base-winders '())

  (set! thread-r (make-tcb #f #f #f #f #f #f #f #f thread-base-winders 0 #f))

  (set! thread-default-quantum
    (make-parameter
     10 ;;; in milliseconds of cpu-time, not timer ticks
     (lambda (x)
       (unless (and (fixnum? x) (fxpositive? x))
         (assertion-violationf 'thread-default-quantum "invalid quantum ~s" x))
       x)))
  )
)

(define-syntax thread-critical-section ;;; normal critical-section semantics
  (syntax-rules ()
    [(_ e1 e2 ...)
     (dynamic-wind
      (lambda () (disable-interrupts))
      (lambda () e1 e2 ...)
      (lambda () (enable-interrupts)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define install-thread-system  ;; ** comment out for testing
  (lambda ()



(define make-weak-list
  ;;
  ;; weak-list ADT - encapsulates a weak-pointer list.
  ;;
  (lambda (ls pred)
    (letrec
        ((wls (let f ((ls ls) (acc '()))
                (if (null? ls)
                    acc
                    (f (cdr ls) (weak-cons (car ls) acc)))))
         (self 
          (case-lambda
            [(msg)
             (critical-section
               (case msg
                 [(get-list)
                  (self 'prune-broken-weak-pointers!)
                  (map (lambda (x) x) wls)]
                 [(prune-broken-weak-pointers!)
                  (let loop ([ls wls] [prev #f])
                    (cond
                      [(null? ls) (void)]
                      [(pred (car ls)) (loop (cdr ls) ls)]
                      ;; (car ls) must be a broken weak pointer
                      [prev 
                       (set-cdr! prev (cdr ls)) ;;; splice out broken wp
                       (loop (cdr ls) prev)]
                      [else
                       (loop (cdr ls) ls)]))
                  (unless (or (null? wls) (pred (car wls)))
                    (set! wls (cdr wls)))]
                 [else
                  (thread-error 'weak-list "invalid message: ~a" msg)]
                 ))]
            [(msg elt)
             (critical-section
               (case msg
                 [(cons)
                  (set! wls (weak-cons elt wls))
                  self]
                 [else
                  (thread-error 'weak-list "invalid message: ~a ~a" msg elt)]
                 ))]
            [any
             (thread-error 'weak-list "invalid message: ~a" any)])))
      self)))

(define convert-to-threaded-parameters
  (lambda ()
    (define-syntax redefine-parameter!
      (lambda (x)
        (syntax-case x ()
          [(_ sym)
           (identifier? (syntax sym))
           (syntax
            (let* ([old-param (system-ref 'sym)]
                   [filter (lambda (x) (old-param x) (old-param))])
              (system-set! 'sym (thread-make-parameter (old-param) filter))))])))

    ;; threaded parameters

    (redefine-parameter! thread-priority)
;;    (redefine-parameter! current-input-port)
;;    (redefine-parameter! current-output-port)
    (redefine-parameter! console-input-port)
    (redefine-parameter! console-output-port)
    (redefine-parameter! reset-handler)
    (redefine-parameter! interrupt-handler)
    (redefine-parameter! break-handler)
    (begin
      (exit-handler thread-exit-handler)
      (redefine-parameter! exit-handler))
    (begin
     (base-exception-handler texception-handler)
     (redefine-parameter! base-exception-handler))
    (redefine-parameter! exception-handler-continuation)
;    (redefine-parameter! warning-handler)
    (redefine-parameter! abort-handler)
; Now set up initially as the right thing
;   (redefine-parameter! thread-name)
    (redefine-parameter! thread-exception)
    (begin
      (redefine-parameter! thread-group-servers)
      (thread-group-servers 'disable-mode-changes 0))
    (redefine-parameter! current-eval)
; Now set up initially as the right thing
;   (redefine-parameter! cafe-level)
    (redefine-parameter! waiter-prompt-and-read)
    (redefine-parameter! waiter-prompt-string)
    (redefine-parameter! waiter-write)
    (redefine-parameter! optimize-level)
    (redefine-parameter! compile-interpret-simple)
    (redefine-parameter! generate-inspector-information)
    
    (redefine-parameter! print-length)
    (redefine-parameter! print-level) 
    (redefine-parameter! print-graph)
    (redefine-parameter! print-radix) 
    (redefine-parameter! print-gensym)
    (redefine-parameter! print-brackets)
    (redefine-parameter! print-vector-length)
    (redefine-parameter! pretty-line-length)
    (redefine-parameter! pretty-one-line-limit)
    (redefine-parameter! pretty-initial-indent)
    (redefine-parameter! pretty-maximum-lines)
    (redefine-parameter! gensym-prefix)
    (redefine-parameter! gensym-count)
    (redefine-parameter! case-sensitive)
    (redefine-parameter! collect-notify)
    (when (top-level-bound? 'trace-output-port)
      (redefine-parameter! trace-output-port))
    (when (top-level-bound? 'trace-output)
      (redefine-parameter! trace-output))
    ;;
    ;; these are not threaded parameters (they're globals):
    ;;   current-directory  subset-mode  scheme-start
    ;;   collect-request-handler  collect-generation-radix
    ;;   collect-trip-bytes


    ;; make interaction-environment a quasi thread parameter
    ;; by using a thread parameter to decide whether or not to use
    ;; the shared location
    (set! unified-interaction-environment
      (thread-make-parameter #t ; default is easy for students
        (lambda (x) x)))
    (system-set! 'interaction-environment
      (let ([p (thread-make-parameter (interaction-environment) (lambda (x) x))]
            [e (interaction-environment)])
        (case-lambda
          [() (if (unified-interaction-environment) e (p))]
          [(x)
           (unless (#%environment? x)
             (assertion-violationf 'interaction-environment "~s is not an environment" x))
           (if (unified-interaction-environment) (set! e x) (p x))])))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define convert-chez-primitives
  (lambda () 
    ;;
    ;; This marks the "top-level" continuation for the debugger
    ;;
    (define top-level
      (scheme-version-case
       (("5.0b")
        (lambda (eval x)
          (call-with-values   
              (lambda () (call/cc (lambda (k) (eval x))))
            (lambda args (for-each (waiter-write) args)))))
       (else
        (lambda (eval x)
          (call/cc ;;; grab continuation & start a new stack segment
           (rec top-level
             (lambda (k)
               ((system-ref '$current-stack-link) 
                (system-ref '$null-continuation)) ;;; toss what's below
               (call-with-values
                   (lambda () (eval x))
                 (lambda args (for-each (waiter-write) args)))
               (k))))))))
    
    (define waiter
      (lambda (cafe eval)
        (let ([x ((waiter-prompt-and-read) cafe)])
          (when (eof-object? x) (exit))
          (let ((k (call/cc (lambda (k) k))))
            (when (and (pair? k) (eq? (car k) 'inspect))
              (newline (console-output-port))
              (inspect-continuation (thread-exception))
              ((cdr k)))
            ;; Force interrupt handler to do a throw in order to cancel
            ;; mutex's, critical-sections, etc., which could prevent the
            ;; inspector from doing i/o.
            (parameterize
                ([interrupt-handler
                  (lambda ()
                    ;;(fprintf thread-conout "waiter ihandler!!~n")
                    (call/cc (lambda (k2) (k (cons 'inspect k2)))))]
                 [exception-handler-continuation
                  (lambda ()
                    (fprintf (console-error-port)
                             "Type (debug) to enter the debugger.~%")
                    (flush-output-port (console-error-port)))]
                 )
              (top-level eval x))
            (waiter cafe eval)))))

    (define thread-new-cafe
      (case-lambda
        [() (new-cafe eval)]
        [(eval) (thread-new-cafe eval (1+ (cafe-level)))]
        [(eval n)
         (unless (procedure? eval)
           (thread-error 'new-cafe "~s is not a procedure" eval))
         (unless (and (integer? n) (exact? n) (<= 0 n))
           (thread-error 'new-cafe "~s is not a nonnegative exact integer" n))
         (call/cc
          (lambda (k1)
            (let ([k2 (call/cc (lambda (k) k))])
              ;;(fprintf conout "thread-new-cafe starting sic=~s~n"
              ;; (- (disable-interrupts) 1))
              ;;(enable-interrupts) (flush-output-port conout)
              (parameterize
                  ([exit-handler k1]
                   [cafe-level n]
                   [interrupt-handler
                    (lambda () (newline (console-output-port)) (thread-reset))]
                   [reset-handler (lambda () (k2 k2))]
                   )
                (waiter (cafe-level) eval)))))]))

    (system-set! 'standard-new-cafe new-cafe)
    (system-set! 'new-cafe thread-new-cafe)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; queue control block
(define-structure (qcb info data rdata sends precedence-proc))

;; thread group control block
(define-structure (tgcb tpri params threads shot? name))

;; exception information block - similar to a tcb, because we
;;   want to capture most of the state of the tcb at time of error.
;;
(define-structure (eib tcb pri tk quantum sleep tgcb sic starts msg))

  
(define all-threads (make-weak-list '() tcb?))
(define thread-count 1)
(define thread-r-expire-time)
(define run-queue)
(define sleep-queue)
(define console-thread)
(define current-winders)

(define conout (console-output-port))
(define conin (console-input-port))

(define sic
  (lambda () 
    (void)
#;  (fprintf conout "sic=~a~n"
             (let ((i (1- (disable-interrupts)))) (enable-interrupts) i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions of thread system (guts of the system)
;;
;; yield  - Suspend the current thread and start another.
;;
;; Returns a value to the caller (to support message queues).
;;
;; Must be reentrant, as gc interrupts (which may invoke arbitrary
;; user code), timer ints, and dynamic-wind thunks (again invoking
;; user code) may cause recursion at arbitrary times.
;;
;; Extreme care must be taken to get the critical sections right in this
;; procedure.  After modifying, re-test using the test-cases at the bottom
;; of this file.
;;
(define yield
  (let () 
    (define post-task?
      (lambda (tk)
        (and (pair? tk) (eq? (car tk) tcookie))))
    (define set-post-task!
      (lambda (k) 
        (let ((tk (tcb-tk thread-r)))
          (if (post-task? tk) ;;; deferred post-task?
              ;; yes, run the post-task on restart
              (set-tcb-tk! thread-r (lambda (x) (k tk)))
              (set-tcb-tk! thread-r k)))))
    (define start
      (lambda ()
;;;(fprintf conout "start: cur=~a q=~s sic=~a..."
;;;  (tcb-num thread-r) (run-queue-nums)
;;;  (let ((i (1- (disable-interrupts)))) (enable-interrupts) i))
;;;(flush-output-port conout)

        (thread-current-input-port (current-input-port))
        (thread-current-output-port (current-output-port))
        (sleep-chk)

        (when (queue-empty? run-queue)
          (panic 'start "no runnable threads~%"))
        ;;
        ;; perform the context switch
        ;;
        (let ((prev-sic (- (disable-interrupts) 2))) ;;; get interrupt counter
          (enable-interrupts) ;;; cancel interrupt counter get
          (unless (fxnonnegative? prev-sic)
            (panic 'start "bad system interrupt counter"))
          (set-tcb-sic! thread-r prev-sic)
          (set! thread-r (dequeue run-queue))
          (set! thread-r-tgcb (tcb-tgcb thread-r))
          ;;
          ;; adjust system interrupt counter for the new thread
          ;;
          (let loop ((sic (tcb-sic thread-r)))
            (unless (fx= sic prev-sic)
              (if (fx> sic prev-sic)
                  (begin 
                    (disable-interrupts)
                    (loop (fx1- sic)))
                  (begin
                    (enable-interrupts)
                    (loop (fx1+ sic)))))))

        (let ((num (tcb-num thread-r)))
          (unless (and (number? num) (positive? num))
            (panic 'start "tcb-num=~s~n" num)))

        (set-tcb-starts! thread-r (1+ (tcb-starts thread-r)))
        ;;
        ;; setting current-input-port and current-output-port here
        ;; is necessary to side-effect certain Chez global system
        ;; variables, as part of swapping-in the new parameters.
        ;;
        (current-input-port (thread-current-input-port))
        (current-output-port (thread-current-output-port))
;;      (#3%current-input-port (current-input-port))
;;      (#3%current-output-port (current-output-port))
        ;;
        (set! thread-r-expire-time
          (if (zero? (tcb-quantum thread-r))
              (expt 2 50) ;;; ages from now
              (+ (cpu-time) (tcb-quantum thread-r))))
        ;;
        ;; New thread context is now set up (except for the wind-in, which
        ;; happens after the throw).
        ;;
        ;; We must throw out of the current continuation here in order to
        ;; cancel the critical-section (see also thunk->tk).
        ;;
        ((tcb-tk thread-r) (void))
        (panic 'start "shouldn't reach here~%")))
    
    (define yield
      (lambda (queue precedence)
       ;;
       ;; Yield
       ;; -----
       ;;
;;;(fprintf conout "yield: num=~a rq=~s idle?=~s sq=~s idle?=~s sic=~a q=~s..."
;;;(tcb-num thread-r)
;;; (run-queue-nums) thread-run-queue-idle?
;;; (sleep-queue-nums) thread-sleep-queue-idle?
;;; (let ((i (1- (disable-interrupts)))) (enable-interrupts) i)
;;; (if (symbol? queue) queue (queue-name queue))
;;; )
;;;(flush-output-port conout)

       (let ((cw (current-winders))
             (ow (tcb-winders thread-r)))
         ;;
         ;; The value of tcb-winders is invariant outside of yield.  Its
         ;; assignment here does not create a critical section.
         ;;
         ;; On a normal entry (not a reentry), ow = thread-base-winders
         ;;
         (set-tcb-winders! thread-r cw)
         ;;
         ;; Normalize winders before the grab and throw.  This effectively
         ;; disables any winder application caused by the throw itself.
         ;;
         (current-winders thread-base-winders) ;;; must precede k grab
         ;;
         ;; Current thread continuation has now been stripped of its excess
         ;; winders.  It is now safe to grab the continuation and do the
         ;; throw.
         ;;
         (let ((x ;;; x is the return value or post-restoration task
                (call/1cc 
                 (lambda (k)
                   ;;(unless (eq? ow thread-base-winders)
                   ;;   (swl:tcl-eval 'bell)) ;;; signal reentry (testing)
                   ;;  (fprintf conout "reentering! tnum=~s"
                   ;;      (tcb-num thread-r))
                   ;;  (sic))
                   ;;
                   ;; Disable all interrupts before setting thread state.
                   ;; This should be done with a critical-section, not
                   ;; with paired disable/enable-interrupt calls (otherwise
                   ;; new threads, which do not go through the restart point
                   ;; when started for the first time, will receive an
                   ;; incorrect system interrupt counter).
                   ;;
                   (critical-section
                     (cond
                       [(eq? queue 'completed) ;;; this thread dies
                        (set-tcb-tk! thread-r #f) ;;; drop ref
                        (set-tcb-winders! thread-r #f) ;;; drop ref
                        (enqueue thread-r (make-queue 'completed))
                        (start)]
                       [(eq? queue 'error)
                        ;; retain the tk for the inspector
                        (set-tcb-winders! thread-r #f) ;;; drop ref
                        (enqueue thread-r (make-queue 'error))
                        (start)]
                       [(symbol? queue)
                        (thread-error 'thread-yield
                                      "unknown function: ~s" queue)]
                       [else
                        (set-post-task! k)
                        (enqueue thread-r queue precedence)
                        (start)]))))))
           ;;
           ;; Restart Point
           ;; -------------
           ;;
           ;; Thread's k has been restarted - restore rest of thread state.
           ;;
;;;(fprintf conout "restart: tnum=~a newtim=~s"
;;;  (tcb-num thread-r)
;;;  (let ((otime (set-timer 0))) (set-timer otime) otime))
;;;(sic)
;;;(unless (eq? ow thread-base-winders)
;;;  (fprintf conout "  reentry catch! num=~s" (tcb-num thread-r))
;;;  (sic))

           (current-winders cw)

           (set-tcb-winders! thread-r ow) ;;; must follow winder restore

;(parameterize ([print-graph #t]) (pretty-print `(yield ,(tcb-num thread-r) ,queue ,precedence) #%$console-output-port))
;(when (top-level-bound? 'fratrat) (pretty-print `(this-is-our-yield ,(eq? ow thread-base-winders) ,(post-task? x)) #%$console-output-port))
;(when (top-level-bound? 'fratrat) (thread-pps #%$console-output-port))
           (if (post-task? x)
               (if (eq? ow thread-base-winders) ;;; reentering?
                   ((cdr x)) ;;; no, do post-restoration task now (tbreak)
                   (set-tcb-tk! thread-r x)) ;;; reentry - defer the post-task
               x)))))

    (case-lambda
      [()
       (yield run-queue (thread-priority))]
      [(queue)
       (yield queue
              (cond [(symbol? queue) #f]
                    [(eq? queue run-queue) (thread-priority)]
                    [else
                     ((qcb-precedence-proc (tcb-num queue)) thread-r)]))]
      [(queue precedence)
       (yield queue precedence)])))


(define sleep-chk
  ;;
  ;; If there are any threads sleeping, get real-time from OS and awaken
  ;; any threads that need to be awakened.  Return the amount of time
  ;; until the next thread needs to be awakened, or #f if none left
  ;; in sleep queue.
  ;;
  (lambda ()
    (if (queue-empty? sleep-queue)
        #f
        (let ((rt (real-time)))
          (critical-section
            (let wake-thread ((t (tcb-next sleep-queue)) (tm #f))
              (cond
                [(eq? t sleep-queue) tm]
                [(> rt (tcb-precedence t))
                 (let ((next (tcb-next t)))
                   (enqueue t)
                   (wake-thread next tm))]
                [else
                 (wake-thread (tcb-next t) (- (tcb-precedence t) rt))])))))))

(define thunk->tk
  (lambda (thunk)
    (call/cc
     (lambda (e)
       (let ((ow (current-winders)))
         (current-winders thread-base-winders)
         (call/cc
          (lambda (tk)
            (current-winders ow)
            (e tk)))
         (thunk)
         (yield 'completed) ;;; abandon the thread on a 'completed queue
         )))))

(define fork-thread-group
  (lambda (thunk quantum priority params)
    (critical-section
      (let* ([params (or (and params (vector-copy params)) '#())]
             [thread-group
              (make-tgcb priority params (make-weak-list '() tcb?) #f #f)]
             [t (make-thread (thunk->tk thunk) quantum thread-group)])
        (fluid-let ([thread-r t] [thread-r-tgcb thread-group])
          (thread-priority priority)
          ;;
          ;; these parameter values are not inherited from the caller
          ;;
          (thread-exception #f)
          (interrupt-handler (interrupt-handler 'get-global 0)))
        (enqueue t)
        t))))

(define fork-thread
  (lambda (thunk quantum priority)
    (let ([thread-group (tcb-tgcb thread-r)])
      (critical-section
        (let ((t (make-thread (thunk->tk thunk) quantum thread-group)))
          (fluid-let ([thread-r t] [thread-r-tgcb thread-group])
            (thread-priority 'set-assignment-mode! 'thread)
            (thread-priority priority)
            (thread-exception 'set-assignment-mode! 'thread)
            (thread-exception #f)
            (thread-name 'set-assignment-mode! 'thread)
            (thread-name ""))
          (enqueue t)
          t)))))

(define make-thread
  (lambda (tk quantum thread-group)
;;;(fprintf conout "make thread c=~a qua=~a pro=~a~n"
;;; tk quantum thread-group)
    (let* ([thread (make-tcb thread-count
                             tk
                             '#()
                             thread-group
                             #f ;;; next 
                             #f ;;; prev
                             0 ;;; system interrupt counter
                             0 ;;; starts
                             thread-base-winders
                             quantum
                             #f ;;; precedence
                             )])
      (critical-section
        (set! thread-count (+ thread-count 1))
        (all-threads 'cons thread)
        ((tgcb-threads thread-group) 'cons thread)
        thread))))

(define get-thread-groups
  (lambda ()
    (let ((threads
           ;; sort by num of 1st thread of each thread-group
           (sort (lambda (t1 t2)
                   (< (tcb-num (car (thread-group-threads t1)))
                      (tcb-num (car (thread-group-threads t2)))))
                 (all-threads 'get-list))))
      (let loop ((ls (cdr threads)) (tgcbs (list (tcb-tgcb (car threads)))))
        (if (null? ls)
            tgcbs
            (if (eq? (tcb-tgcb (car ls)) (car tgcbs))
                (loop (cdr ls) tgcbs) ;;; already have thread's proc
                (loop (cdr ls) (cons (tcb-tgcb (car ls)) tgcbs))))))))

(define kill
  (lambda (thread)
    ;; (fprintf conout "Thread ~s terminating~%" (tcb-num thread))
    (when (eq? thread console-thread) 
      (fprintf conout "Console thread killed~n")
      (abort))
    (let ((queue (thread-make-msg-queue 'killed)))
      (if (eq? thread thread-r)
          (yield queue)
          (enqueue thread queue)))))

(define panic
  (lambda (who fmt . args)
    (set-timer 0)
    (#%fprintf conout "~%~%PANIC (SWL thread system) in ~a: ~a~%"
               who (#%apply #%format fmt args))
    (thread-pps conout)
    (abort)))


(define thread-identifier
  (case-lambda
    [() (thread-identifier thread-r)]
    [(t)
     (let ((tnum (tcb-num t))
           (pname (thread-name 'get-value-for t)))
       (cond [(not (string=? pname ""))
              (format "[~a(~a)]" pname tnum)]
             [(eq? t console-thread)
              (format "[console(~a)]" tnum)]
             [else
              (format "[~a]" tnum)]))]))


(define report ;;; adapted from 7.ss
  (lambda (p what who msg . args)
;;; (fprintf (swl:bug-port) "report tid=~s what=~s who=~s msg=~s args=~s~n" (thread-identifier) what who msg args)
    (let ([who (if who (format "~a" who) "")]
          [msg (parameterize ([print-level 3] [print-length 6])
                 (apply format msg args))])
      (fprintf p "~%~a ~a~a~a.~%"
               (thread-identifier)
               what
               (if (string=? who "") "" (format " in ~a" who))
               (if (string=? msg "") "" (format ": ~a" msg))))
;;;    (if (eib? (thread-exception))  **debug**
;;;     (begin
;;;       (fprintf p "call stack:-~n")
;;;       (let f ((obj (inspect/object (eib-tk (thread-exception)))))
;;;         (when (eq? (obj 'type) 'continuation)
;;;           (fprintf p "~a~n" ((obj 'code) 'name))
;;;           (f (obj 'link)))))
;;;     (fprintf p "no continuation~n"))
;;;    (sic)
    ))


(define texception-handler 
  (lambda (c)
    (unless thread-r (panic who msg args))
    (unless (memq thread-r (thread-group-servers))
      (set-tcb-tk! thread-r (call/cc (lambda (k) k))))
    (call/cc
     (lambda (k)
       (let ([str (with-output-to-string
                    (lambda ()
                      (display-condition c)))])
         (thread-exception (thread->eib thread-r k str))
         (last-exception (thread-exception))
         (display str (console-error-port))
         (newline (console-error-port))
         (flush-output-port (console-error-port))
         ((exception-handler-continuation)))))
    (if (memq thread-r (thread-group-servers))
        (thread-reset)
        (yield 'error))))

(define tbreak-handler
  (case-lambda
    [(who msg . args)
     (apply report (console-output-port) "Break" who msg args)
     (tbreak)]
    [(who)
     (report (console-output-port) "Break" who "")
     (tbreak)]
    [()
     (tbreak)]))


(define thread-error
  (lambda (who msg . args)
    (apply #%error who msg args)))

;;
;; thread-reset - invoke the user's reset-handler only for server
;; threads.
;;
(define thread-reset
  (lambda ()
;;;(fprintf conout "thread-reset! tnum=~s~n" (tcb-num thread-r))
    (if (memq thread-r (thread-group-servers))
        (begin
          ;;(fprintf conout "calling r-h=~s~n" (reset-handler))
          ((reset-handler)))
        (warningf 'reset "attempt to reset a non-server thread"))))

(define thread-exit
  (lambda args
    ;;(fprintf conout "thread-exit! tnum=~s eh=~s~n"
    ;;  (tcb-num thread-r) (exit-handler))
    (apply (exit-handler) args)))

(define thread-exit-handler
  (let ((eh (exit-handler)))
    (lambda args
      ;;(fprintf conout "thread-exit-handler! tnum=~s~n"
      ;;  (tcb-num thread-r))
      (apply eh args))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; queue primitives for threads

(define enqueue ;;; place thread on a queue at priority
  (case-lambda
    [(thread)
     (enqueue thread run-queue (thread-priority 'get-value-for thread))]
    [(thread queue)
     (enqueue thread queue ((qcb-precedence-proc (tcb-num queue)) thread))]
    [(thread queue precedence)
     (unless (tcb? thread)
       (panic 'enqueue "arg is not a thread"))
     (when (tcb-next thread) (dequeue-thread thread))
     (critical-section ;;; c-s must cover the loop
       (let ((precedence-proc (qcb-precedence-proc (tcb-num queue))))
         (let loop ([q queue])
           (cond
             [(or (eq? queue (tcb-prev q))
                  (>= precedence (precedence-proc (tcb-prev q))))
              (set-tcb-prev! thread (tcb-prev q))
              (set-tcb-next! thread q)
              (set-tcb-next! (tcb-prev q) thread)
              (set-tcb-prev! q thread)
              (precedence-proc thread precedence)]
             [else (loop (tcb-prev q))]))))]))

(define dequeue
  (lambda (q)
    (critical-section
      (dequeue-thread (tcb-next q)))))

(define dequeue-thread
  (lambda (thread)
    (critical-section
      (let ([p (tcb-prev thread)])
        (set-tcb-next! p (tcb-next thread))
        (set-tcb-prev! (tcb-next thread) p)
        (set-tcb-next! thread #f)
        (set-tcb-prev! thread #f))
      (when (queue-empty? run-queue)
        (set! thread-run-queue-idle? #t))
      (when (queue-empty? sleep-queue)
        (set! thread-sleep-queue-idle? #t))
      thread)))

(define queue-empty?
  (lambda (x)
    (eq? (tcb-next x) x)))

(define make-queue
(rec thread-make-msg-queue ; so error messages identify plaintiff correctly
  (lambda (info)
    (let ([q (make-tcb (make-qcb info '() '() 0
                                 (case-lambda ;;; "null" precedence proc
                                   [(t) 0]
                                   [(t v) (void)])
                                 )
                       #f ;;; tk
                       '#()
                       #f ;;; thread group
                       #f ;;; next
                       #f ;;; prev
                       0 ;;; system interrupt counter
                       0 ;;; starts
                       'queue
                       0 ;;; quantum
                       #f ;;; precedence
                       )])
      (set-tcb-prev! q q)
      (set-tcb-next! q q)
      q))))

(define thread->queue
  (lambda (t)
    (critical-section
      (let loop ([t t])
        (cond
          [(eq? (tcb-next t) #f) (if (eq? t thread-r) run-queue #f)]
          [(number? (tcb-num t)) (loop (tcb-next t))]
          [else t])))))

(define queue-name
  (lambda (t)
    (let ([q (thread->queue t)])
      (cond
        [(eq? t thread-r) 'running]
        [(eq? q #f) '???]
        [else (qcb-info (tcb-num q))]))))

(define traverse-queue
  (lambda (constructor base)
    (lambda (q-head)
      (let loop ([q (tcb-next q-head)])
        (cond
          [(eq? q q-head) base]
          [else (constructor q (loop (tcb-next q)))])))))


(define queue-length
  (lambda (q)
    ((traverse-queue (lambda (x y) (+ 1 y)) 0) q)))

(define run-queue-length
  (lambda () (queue-length run-queue)))

(define run-queue-list
  (lambda ()
    (thread-traverse-run-queue cons '())))

(define run-queue-nums
  (lambda ()
    (thread-traverse-run-queue (lambda (t l) (cons (tcb-num t) l)) '())))

(define sleep-queue-nums
  (lambda ()
    ((traverse-queue (lambda (t l) (cons (tcb-num t) l)) '()) sleep-queue)))

(define format-run-queue
  (lambda ()
    (map tcb-num (run-queue-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-handler
  (lambda ()
    ;;
    ;; Note: if we are in a critical-section, two keyboard interrupts
    ;; will force the keyboard interrupt handler to be called.
    ;;
    (if (eq? console-thread thread-r)
        (tbreak console-thread 'kbd-int)
        (begin
          ;(fprintf conout "Breaking the console and running threads.~n")
          (tbreak console-thread 'kbd-int)
          (tbreak thread-r 'kbd-int)
          ))))


(define tcookie (cons 1 1))

(define tbreak
  (case-lambda
    [() (tbreak thread-r)]
    [(thread) (tbreak thread 'break)]
    [(thread reason) (tbreak thread reason (lambda () ((interrupt-handler))))]
    [(thread reason thunk) (tbreak thread reason thunk #t)] 
    [(thread reason thunk run-now?)
     (define thunk->post-task
       (lambda (t) (cons tcookie t)))
     (unless (tcb? thread)
       (thread-error 'thread-break "not a thread: ~s" thread))
     ;;
     ;;(when (eq? thread console-thread)
     ;;  (fprintf conout "interrupted console thread!~n"))
     ;;(when (eq? thread thread-r)
     ;; (fprintf conout "interrupted thread-r!~n"))
     ;;(fprintf conout "tbreak! tnum=~s reason=~s run-now?=~s~n"
     ;; (tcb-num thread) reason run-now?)
     ;;
     (let ((exc-msg (if (eq? reason 'kbd-int) "Keyboard Interrupt" "Break")))
       (if (eq? thread thread-r)
           (begin
             ;;(fprintf conout "tbreak! thread is running - breaking now~n")
             (call/cc
              (lambda (k)
                (when reason
                  (thread-exception (thread->eib thread-r k exc-msg))
                  (last-exception (thread-exception)))
                (thunk))))
           (critical-section
             (let ((tk (tcb-tk thread)) (q (thread->queue thread)))
               ;; (fprintf conout "tbreak! setting post-task~n")
               (set-tcb-tk!
                thread
                (lambda (x)
                  (tk
                   (thunk->post-task
                    (lambda ()
                      ;; (fprintf conout "performing post-task~n")
                      (call/cc
                       (lambda (k) ;;; don't use tk; it's already shot
                         (when reason
                           (thread-exception (thread->eib thread-r k exc-msg))
                           (last-exception (thread-exception)))
                         (thunk)
                         ;; (fprintf conout "post-task thunk didn't throw~n")
                         ;;
                         ;; thunk didn't throw.  We must check for messages
                         ;; as a sender may have run since we were last
                         ;; enqueued.
                         ;;
                         (if (thread-msg-waiting? q)
                             (thread-receive-msg q)
                             (yield q))
                         )))))))
               (if run-now?
                   (begin
                     ;;
                     ;; We run the broken thread now to ensure that the
                     ;; post-task is performed now. Otherwise, post-tasks
                     ;; registered by subsequent calls to tbreak may be
                     ;; skipped, or messages delivered to the thread may
                     ;; be dropped.
                     ;;
                     (enqueue thread run-queue (thread-highest-priority))
                     (yield run-queue (thread-highest-priority)))
                   (enqueue thread))))))]))


(define thread->eib
  (letrec ((remove-newlines
            (lambda (s)
              (list->string
               (reverse
                (let f ((l (string->list s)) (acc '()))
                  (if (null? l)
                      acc
                      (if (char=? (car l) #\newline)
                          (f (cdr l) acc)
                          (f (cdr l) (cons (car l) acc))))))))))
    (lambda (t k msg)
      (make-eib t
                (thread-priority 'get-value-for t)
                k
                (tcb-quantum t)
                (tcb-precedence t)
                (tcb-tgcb t)
                (tcb-sic t)
                (tcb-starts t)
                (if (string? msg) (remove-newlines msg) msg)))))

              
(define thread-debug
  (lambda ()
    (let ((te (thread-exception 'get-value-for thread-r)))
      (cond [te (inspect-continuation te)]
            [(last-exception) (inspect-continuation (last-exception))]
            [else (warningf 'debug "Nothing to debug")]))))


(define pad-right
      (lambda (s len)
        (list->string
         (reverse
          (let ((l (reverse (string->list s))))
            (let f ((l l) (n (- len (length l))))
              (if (positive? n)
                  (f (cons #\space l) (1- n))
                  l)))))))

(define inspect-continuation
  ;;
  ;; entity: either a thread (tcb) or an exception info block (eib).
  ;;
  (lambda (entity)
   (let ([ip (console-input-port)] [op (console-output-port)])
    (define gobble-whitespace
      (lambda ()
        (let f ()
          (when (char-ready? ip)
            (when (char-whitespace? (peek-char ip))
              (read-char ip)
              (f))))))
    (define num-ready?
      (lambda () (and (char-ready? ip) (char-numeric? (peek-char ip)))))
    (define inspect-tk
      (lambda (tk)
        (inspect ((inspect/object tk) 'value))
;       (inspect (((inspect/object tk) 'link) 'value))
        ))
    (clear-input-port ip)
    (let ((k (call/cc (lambda (k) k))))
      (parameterize ([interrupt-handler (lambda () (newline op) (k k))])
        (let again ()
          (let* ((thread (if (tcb? entity) entity (eib-tcb entity)))
                 (entity-tk (if (tcb? entity) (tcb-tk entity) (eib-tk entity)))
                 )
            (fprintf op "~a debug> " (thread-identifier thread))
            (flush-output-port op)
            (let ([ans (read ip)])
              (case ans
                [(e) (flush-output-port op) (void)]
                [(q r) (thread-reset)]
                [(a) (abort)]
                [(n) (new-cafe) (again)]
                [(pps) (thread-pps) (again)]
                [(pid)
                 (fprintf op "The debugger is running in ~a.~n"
                         (thread-identifier))
                 (again)]
                [(p)
                 (newline op)
                 (fprintf op "Thread                    Exception~n")
                 (fprintf op "------                    ---------~n")
                 (let loop ((tcbs (all-threads 'get-list)))
                   (if (null? tcbs)
                       (begin (newline op) (again))
                       (let ((eib
                              (thread-exception 'get-value-for (car tcbs))))
                         (when eib
                           (fprintf op "~a ~a~n"
                                   (pad-right
                                    (thread-identifier (eib-tcb eib))
                                    25)
                                   (eib-msg eib)))
                         (loop (cdr tcbs)))))]
                [(i)
                 (gobble-whitespace)
                 (if (num-ready?)
                     (let* ([num (read ip)] [thread (thread-find num)])
                       (unless thread
                         (fprintf op "No such thread: ~s.~%" num)
                         (again))
                       (let ((eib (thread-exception 'get-value-for thread)))
                         (unless (and eib (eq? (eib-tcb eib) thread))
                           (fprintf op "Thread ~s has no exception.~%" num)
                           (again))
                         (inspect-tk (eib-tk eib))
                         (again)))
                     (begin 
                       (inspect-tk entity-tk)
                       (again)))]
                [(t)
                 (gobble-whitespace)
                 (unless (num-ready?)
                   (fprintf op "thread number> ")
                   (flush-output-port op))
                 (let ([num (read ip)])
                   (unless (and (fixnum? num) (positive? num))
                     (fprintf op "Not a thread number.~n")
                     (again))
                   (let ([thread (thread-find num)])
                     (cond
                       [(not thread)
                        (fprintf op "No such thread: ~s~%" num)
                        (again)]
                       [(eq? thread thread-r)
                        ;; thread-r may contain a shot one-shot (no frames
                        ;; to inspect)
                        (fprintf op "Can't inspect self.~n")
                        (again)]
                       [else
                        (inspect-tk (tcb-tk thread))
                        (again)])))]
                [(?)
                 (fprintf op
                  "
Type e to exit the debugger and resume the computation
     q or r to exit the debugger (discarding the computation)
     a to abort scheme
     i to inspect ~a continuation of thread ~a
     i <num> to inspect exception continuation of thread <num>
     t <num> to inspect continuation of thread <num>
     p to print thread exception messages
     pid to print the identifier of the debugger thread
     pps to print all threads

"
                  (if (tcb? entity) "thread" "exception")
                  (tcb-num thread))
                 (again)]
                [else
                 (unless (eof-object? ans)
                   (fprintf op "Invalid command.  Type ? for options.~%")
                   (again))])))))))))


(define thread-group-threads
  (lambda (t)
    ((tgcb-threads (tcb-tgcb t)) 'get-list)))

;;
;; end of definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! current-winders (system-ref '$current-winders))

(set! thread-make-parameter
  (let ([next-index 0]
        [delegated (list 'delegated)]
        [unassigned (list 'unassigned)]
        )
    (define vector-stretch
      (lambda (vec len)
        (let ([new (make-vector len delegated)]
              [end (vector-length vec)])
          (unless (vector? vec)
            (panic 'vector-stretch "vec is not a vector: ~s len=~s tnum=~s"
                   vec len (tcb-num thread-r)))
          (do ((i 0 (fx+ i 1)))
              ((fx= i end))
            (vector-set! new i (vector-ref vec i)))
          new)))
    (lambda (global-val filter)
      (let ([index next-index]
            [mode-changes-enabled? #t])
        (define in-bounds?
          (lambda (vec) (fx< index (vector-length vec))))
        (define delegated?
          (lambda (vec)
            (or (not (in-bounds? vec))
                (eq? (vector-ref vec index) delegated))))
        (define unassigned?
          (lambda (vec)
            (or (not (in-bounds? vec))
                (eq? (vector-ref vec index) unassigned))))
        (define get-value
          ;; written so there is no critical section (for efficiency)
          (lambda (thread)
            (let ((tp (tcb-params thread)))
              (if (fx< index (vector-length tp))
                  (let ((val (vector-ref tp index)))
                    (if (not (or (eq? val delegated)
                                 (eq? val unassigned)))
                        val
                        (let ((gp (tgcb-params (tcb-tgcb thread))))
                          (if (fx< index (vector-length gp))
                              (let ((val (vector-ref gp index)))
                                (if (not (eq? val delegated))
                                    val
                                    global-val))
                              global-val))))
                  (let ((gp (tgcb-params (tcb-tgcb thread))))
                    (if (fx< index (vector-length gp))
                        (let ((val (vector-ref gp index)))
                          (if (not (eq? val delegated))
                              val
                              global-val))
                        global-val))))))
                  
        (define new-parameter
          (case-lambda
            [() (get-value thread-r)]
            [(x)
             ;;
             ;; hack: the Chez inspector is not thread-friendly; it turns off
             ;; the timer before setting parameters. We can force it back
             ;; on here.
             ;;
             (when thread-bootstrapped?
               (let ((tm (set-timer 0))) (set-timer (1+ tm))))
             ;;
             (let ([val (filter x)])
               (critical-section
                 (let ([tp (tcb-params thread-r)])
                   (if (not (delegated? tp))
                       (vector-set! tp index val) ;;; thread mode
                       (let ([gp (tgcb-params thread-r-tgcb)]) ;;; group mode
                         (if (in-bounds? gp)
                             (vector-set! gp index val)
                             (let ([ngp (vector-stretch gp (1+ index))])
                               (set-tgcb-params! thread-r-tgcb ngp)
                               (vector-set! ngp index val))))))))]
            [(k x) ;;; extended operations
             (if (eq? k 'get-value-for)
                 ;; avoid critical section for this case
                 (cond [(tcb? x) (get-value x)]
                       [(tgcb? x)
                        (let ((gp (tgcb-params x)))
                          (if (delegated? gp)
                              global-val
                              (vector-ref gp index)))]
                       [else
                        (thread-error "threaded parameter"
                               "not a thread or thread-group: ~s" x)])
                 (critical-section
                   (let ([tp (tcb-params thread-r)]
                         [gp (tgcb-params thread-r-tgcb)])
                     (case k
                       [(get-global) global-val]
                       [(set-global!)
                        (let ([val (filter x)])
                          (set! global-val val))]
                       [(get-group)
                        (if (delegated? gp)
                            (warningf "threaded parameter" "no group value")
                            (vector-ref gp index))]
                       [(set-group!)
                        (let ([val (filter x)])
                          (if (in-bounds? gp)
                              (vector-set! gp index val)
                              (let ([ngp (vector-stretch gp (1+ index))])
                                (set-tgcb-params! thread-r-tgcb ngp)
                                (vector-set! ngp index val))))]
                       [(get-assignment-mode)
                        (if (delegated? tp) 'group 'thread)]
                       [(set-assignment-mode!)
                        (unless mode-changes-enabled?
                          (thread-error "threaded parameter"
                                 "assignment mode may not be changed"))
                        (case x
                          [(thread)
                           (cond
                             [(not (delegated? tp))
                              (warning
                               "threaded parameter"
                               "assignment mode already set to thread")]
                             [(in-bounds? tp)
                              (vector-set! tp index unassigned)]
                             [else
                              (let ([ntp (vector-stretch tp (1+ index))])
                                (set-tcb-params! thread-r ntp)
                                (vector-set! ntp index unassigned))])]
                          [(group)
                           (if (delegated? tp)
                               (warningf "threaded parameter"
                                        "assignment mode already set to group")
                               (vector-set! tp index delegated))]
                          [else (thread-error
                                 "threaded parameter"
                                 "invalid assignment mode ~a" x)])]
                       [(disable-mode-changes) (set! mode-changes-enabled? #f)]
                       [(get-index) index]
                       [else
                        (thread-error "threaded parameter"
                               "invalid extended operation: ~s" k)]))))]
            ))
        (set! next-index (1+ next-index))
        new-parameter))))

(set! thread-number
  (case-lambda
    [() (thread-number thread-r)]
    [(t) (unless (tcb? t) (thread-error 'thread-number "~s is not a thread" t))
         (tcb-num t)]))

(set! thread-quantum
  (let ([max-quantum (expt 2 25)])
    (case-lambda
      [(v) (thread-quantum thread-r v)]
      [(t v)
       (unless (and (fixnum? v) (fx<= 0 v max-quantum))
         (thread-error 'thread-quantum
                       "~s is not between zero and ~s" v max-quantum))
       (set-tcb-quantum! t v)
       ])))

(set! thread-self (lambda () thread-r))
(set! thread-console? (lambda () (eq? thread-r console-thread)))

(set! thread-find
  (case-lambda
    [(num) (thread-find num (all-threads 'get-list))]
    [(num ls)
     (unless (number? num)
       (thread-error 'thread-find "~s is not a number" num))
     (let loop ([ls ls])
       (cond
         [(null? ls) #f]
         [(pair? ls)
          (if (tcb? (car ls))
              (if (= (tcb-num (car ls)) num)
                  (car ls)
                  (loop (cdr ls)))
              (loop (cdr ls)))]
         [else (thread-error 'thread-find "~s is not a pair" ls)]))]))

(set! thread->k 
  (case-lambda
    [() (thread->k thread-r)]
    [(t) (unless (tcb? t)
           (thread-error 'thread->k "~s is not a thread" t))
         (tcb-tk t)]))

(set! thread-exception->k 
  (case-lambda
    [() (thread-exception->k thread-r)]
    [(e) (unless (eib? e)
           (thread-error 'thread-exception->k "~s is not an exception" e))
         (eib-tk e)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; thread message queue primitives
;;
;; -- only thread synchronization mechanism currently provided 
;;    by thread system
;;
;;  thread-receive-msg: if datum available on queue
;;                         then dequeue and return datum
;;                         else suspend thread on internal queue
;;
;;  thread-send-msg:    if thread waiting on internal queue
;;                         then unblock it and have it return datum
;;                         else enqueue datum
;;
;;  thread-msg-waiting?: #t if message already on the queue.
;;
;;  thread-receiver-waiting?: #t if receiver already waiting on the queue.
;;  
;;  thread-reschedule: move a thread to a new slot in its queue based on
;;                     the specified priority.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! thread-make-msg-queue make-queue)

(set! thread-send-msg
  (case-lambda
    [(queue datum) (thread-send-msg queue datum #f)]
    [(queue datum mode)
     (unless (and (tcb? queue) (qcb? (tcb-num queue)))
       (thread-error 'thread-send-msg "~s is not a thread queue" queue))
     ;; (fprintf conout "thread-send! tnum=~s q=~s d=~s~n"
     ;;   (tcb-num thread-r) (qcb-info (tcb-num queue)) datum)
     (critical-section
       (if (thread-receiver-waiting? queue)
           (let ([thread (dequeue queue)])
             (let ([k (tcb-tk thread)])
               (set-tcb-tk! thread (lambda (x) (k datum)))
               (if (eq? mode 'urgent)
                   (critical-section
                     ;; (fprintf conout "*** urgent msg sent to ~s by ~s~n"
                     ;;   (tcb-num thread) (tcb-num (thread-self)))
                     (enqueue thread run-queue (thread-highest-priority))
                     (yield run-queue (thread-highest-priority))
                     ;; (fprintf conout "*** urgent msg handled~n")
                     )
                   (begin
                     ;;   (fprintf conout "*** msg sent to ~s runq=~s~n"
                     ;;     (tcb-num thread) (run-queue-nums))
                     (enqueue thread)
                     ))))
           (let ([hdr (tcb-num queue)])
             (set-qcb-data! hdr (cons datum (qcb-data hdr)))
             (set-qcb-sends! hdr (1+ (qcb-sends hdr))))
           ))]))

(set! thread-msg-waiting?
  (lambda (queue)
    (unless (and (tcb? queue) (qcb? (tcb-num queue)))
      (thread-error 'thread-msg-waiting? "~s is not a thread queue" queue))
    (let ([hdr (tcb-num queue)])
      (critical-section
        (or (pair? (qcb-rdata hdr))
            (pair? (qcb-data hdr)))))))

(set! thread-receiver-waiting?
  (lambda (queue)
    (unless (and (tcb? queue) (qcb? (tcb-num queue)))
      (thread-error 'thread-receiver-waiting?
                    "~s is not a thread queue" queue))
    (not (queue-empty? queue))))

(set! thread-receive-msg
  (lambda (queue) 
    ;;  (fprintf conout "thread-receive! tnum=~s q=~s~n"
    ;;    (tcb-num thread-r) (qcb-info (tcb-num queue)))
    (unless (and (tcb? queue) (qcb? (tcb-num queue)))
      (thread-error 'thread-receive-msg "~s is not a thread queue" queue))
    (let ([hdr (tcb-num queue)])
      (critical-section ;;; yes, the whole thing is a critical section
        (when (and (null? (qcb-rdata hdr))
                   (pair? (qcb-data hdr)))
          (set-qcb-rdata! hdr (reverse (qcb-data hdr)))
          (set-qcb-data! hdr '()))
        (if (null? (qcb-rdata hdr)) ;;; no message waiting?
            (yield queue) ;;; wait for one
            (let ((dat (qcb-rdata hdr)))
              (set-qcb-rdata! hdr (cdr dat))
              (car dat)))))))


(set! thread-reschedule
  (lambda (thread priority)
    (unless (tcb? thread)
      (thread-error 'thread-reschedule
                    "1st argument is not a thread: ~s" thread))
    (when (eq? thread thread-r)
      (thread-error 'thread-reschedule "can't reschedule self"))
    (unless (fixnum? priority)
      (thread-error 'thread-reschedule
                    "2nd argument is not a fixnum: ~s" priority))
    (enqueue thread (thread->queue thread) priority)
    ))

(set! thread-highest-priority most-negative-fixnum)
(set! thread-lowest-priority most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; thread system primitives

(set! thread-yield yield)

(set! thread-fork-group
  (case-lambda
    [(thunk)
     (thread-fork-group thunk (tcb-quantum thread-r))]
    [(thunk quantum)
     (thread-fork-group thunk quantum (tgcb-tpri thread-r-tgcb))]
    [(thunk quantum priority)
     (thread-fork-group thunk quantum priority (tgcb-params thread-r-tgcb))]
    [(thunk quantum priority params)
     (unless (nonnegative? quantum)
       (thread-error 'thread-fork-group
                     "quantum must be a nonnegative integer"))
     (unless (fixnum? priority)
       (thread-error 'thread-fork-group "priority must be a fixnum"))
     (fork-thread-group thunk quantum priority params)]
    ))

(set! thread-fork
  (case-lambda
    [(thunk)
     (thread-fork thunk (thread-default-quantum))]
    [(thunk quantum)
     (thread-fork thunk quantum (tgcb-tpri thread-r-tgcb))]
    [(thunk quantum priority)
     (unless (nonnegative? quantum)
       (thread-error 'thread-fork
                     "quantum must be a nonnegative integer"))
     (unless (fixnum? priority)
       (thread-error 'thread-fork "priority must be a fixnum"))
     (fork-thread thunk quantum priority)]))

(set! thread-sleep
  (lambda (ms)
    (let ((rt (real-time)))
      (unless (and (fixnum? ms) (fx>= ms 0))
        (thread-error 'thread-sleep "~s is not a nonnegative fixnum" ms))
      (yield sleep-queue (+ ms rt)))))

(set! thread-wake
  (lambda (thread)
    (unless (eq? sleep-queue (thread->queue thread))
      (thread-error 'thread-wake "thread is not sleeping"))
    (enqueue thread)))

(set! thread-sleep-check sleep-chk)

(set! thread-become-console!
  (case-lambda
    [() (thread-become-console! thread-r)]
    [(thread)
     (thread-become-server!
      (lambda ()
        (panic 'thread-become-console!
               "shouldn't reach here (new-cafe installed?)~%"))
      thread)
     (set! console-thread thread)]))

(set! thread-become-server!
  (case-lambda
    [() (thread-become-server!
         (lambda ()
           (warning
            #f
            "server's reset handler unassigned: thread-num=~s thread-name=~s"
            (tcb-num (thread-self)) (thread-name))
           (yield 'error)))]
    [(rhandler) (thread-become-server! rhandler thread-r)]
    [(rhandler thread)
     (tbreak thread ;;; install new parameter values
             #f
             (lambda ()
               ;;
               ;; Set the default interrupt behavior to reset the thread on
               ;; an interrupt, rather than to suspend it.
               ;;
               ;; It is conceivable that a server thread should be suspended
               ;; after an interrupt, and hence should not be reset, but
               ;; the more likely case, e.g., a repl, is that it should
               ;; continue to run.
               ;;
               ;; In extreme cases, however, the default may make it
               ;; impossible to regain console control from an errant
               ;; server thread-group.
               ;;
               (interrupt-handler 'set-assignment-mode! 'thread)
               (interrupt-handler
                (lambda ()
;;;**             (printf "tbs int for ~a~n" (thread-identifier))
                  (thread-reset)))
               ;; use top-level-value to avoid argument-count warning
               ((top-level-value 'reset-handler) 'set-assignment-mode! 'thread)
               (reset-handler rhandler)
               ;; (fprintf conout "server thread new params installed num=~s~n"
               ;; (tcb-num (thread-self)))
               (thread-group-servers (cons thread (thread-group-servers)))))
     ]))

(set! thread-kill
  (case-lambda
    [() (kill thread-r)]
    [(val) 
     (cond [(tcb? val) (kill val)]
           [(number? val)
            (let ((thread (thread-find val)))
              (if (tcb? thread)
                  (kill thread)
                  (thread-error 'thread-kill "no such thread: ~a" val)))]
           [else (thread-error 'thread-kill "invalid argument: ~a" val)])]))


(set! thread-traverse-run-queue
  (lambda (constructor base)
    (critical-section
      ((traverse-queue constructor base) run-queue))))

(set! thread-status
  (lambda (t)
    (define reduce
      (lambda (ls)
        (let f ((ls ls))
          (cond [(null? ls) '()]
                [(car ls) (cons (car ls) (f (cdr ls)))]
                [else (f (cdr ls))]))))
    (let ([state (queue-name t)]
          [id (thread-identifier t)])
      (reduce
       (list
        (if (null? (cdr (memq t (thread-group-threads t))))
            (pad-right id 25)
            (string-append "  " (pad-right id 23)))
        (if (and (eq? t console-thread)
                 (or (< (string-length id) 8)
                     (not (string=? "[console" (substring id 0 8)))))
            'console
            #f)
;;;     (let ((nums (map tcb-num (thread-group-threads t))))
;;;       (if (null? (cdr nums))
;;;           #f ;;; single-thread thread-group
;;;           nums))
        (if (and state (not (eq? state 'sleep))) state #f)
        (cond [(eq? state 'sleep)
               `(sleeping ,(/ (- (tcb-precedence t) (real-time)) 1000.0))]
              [(memq state '(break completed error kbd-int killed))
               #f]
              [else
               (cond [(zero? (thread-priority 'get-value-for t)) #f]
                     [(= (thread-priority 'get-value-for t)
                         (thread-lowest-priority))
                      `(lowest priority)]
                     [(= (thread-priority 'get-value-for t)
                         (thread-highest-priority))
                      `(highest priority)]
                     [else `(priority ,(thread-priority 'get-value-for t))])]
              )
        (if (zero? (tcb-quantum t))
            'nonpreemptable
            (/ (tcb-quantum t) 1000.0))
        (if (memq t (thread-group-servers 'get-value-for t))
            'server
            #f)
        `(starts ,(tcb-starts t))
        )))))

(set! thread-ps
  (case-lambda
    [() (thread-ps thread-r)]
    [(thread) (map thread-status (thread-group-threads thread))]))

(set! thread-ps-all
  (lambda ()
    (let ((threads
           (sort
            (lambda (t1 t2)
              (let ((tg1 (thread-group-threads t1))
                    (tg2 (thread-group-threads t2)))
                (if (memq t1 tg2)
                    (< (tcb-num t1) (tcb-num t2))
                    (let ((m1 (apply min (map tcb-num tg1)))
                          (m2 (apply min (map tcb-num tg2))))
                      (< m1 m2)))))
            (all-threads 'get-list))))
      (map thread-status threads))))

(set! thread-pps
  (case-lambda
    [() (thread-pps (console-output-port))]
    [(op)
     (newline op)
     (fprintf op "~s ~s~n~n"
              (cons 'run-queue (run-queue-nums))
              (cons 'sleep-queue (sleep-queue-nums)))
     (fprintf op "Thread                    Attributes~n")
     (fprintf op "------                    ----------~n")
     (parameterize ([collect-generation-radix 1])
       ((collect-request-handler)))
     (for-each
      (lambda (thread)
        (for-each (lambda (item) (fprintf op "~a " item)) thread)
        (newline op)
        )
      (thread-ps-all))
     (newline op)]))

; (set! ps-num (lambda () (length (all-threads 'get-list))))

;;
;; ** uncomment for testing
;;
;;;(define install-thread-system
;;;  (lambda ()                            ;; )) ;; for paren balance
;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
         

(set! console-error-port
  (thread-make-parameter
     conout
     (lambda (x)
       (unless (output-port? x)
         (thread-error 'console-error-port "~s is not an output port" x))
       x)))

(set! thread-conout conout)    
(set! thread-break tbreak)
(set! thread-timer-interrupt-hook void)
(set! thread-run-queue-idle? #t)
(set! thread-sleep-queue-idle? #t)

(set! run-queue (make-queue 'ready))
(set-qcb-precedence-proc!
 (tcb-num run-queue)
 (case-lambda
   [(t) (tcb-precedence t)]
   [(t v) (set-tcb-precedence! t v)
          (set! thread-run-queue-idle? #f)]))
       
(set! sleep-queue (make-queue 'sleep))
(set-qcb-precedence-proc!
 (tcb-num sleep-queue)
 (case-lambda
   [(t) (tcb-precedence t)]
   [(t v) (set-tcb-precedence! t v)
          (set! thread-sleep-queue-idle? #f)]))
          
    
;; fabricate initial console thread-group and thread
(set! thread-r-tgcb (make-tgcb 0 '#() (make-weak-list '() tcb?) #f #f))
(set! thread-r
  (make-tcb thread-count ;;; num
            'dummy-k     ;;; tk
            '#()         ;;; thread params
            thread-r-tgcb;;; group params
            #f           ;;; next
            #f           ;;; prev
            0            ;;; sic
            0            ;;; starts
            thread-base-winders
            (thread-default-quantum)
            #f ;;; precedence
            ))
(set! thread-count (+ thread-count 1))
(all-threads 'cons thread-r)
((tgcb-threads thread-r-tgcb) 'cons thread-r)
(set! console-thread thread-r)

(system-set! 'standard-debug debug)
(system-set! 'debug thread-debug)
(system-set! 'reset thread-reset)
(system-set! 'exit thread-exit)

(set! thread-quantum-remaining
  (lambda () 
    (let ((qm (- thread-r-expire-time (cpu-time))))
      (if (negative? qm)
          0
          qm))))

;;
;; Define parameters that are unique to the thread system.

;; These are first created as regular parameters, then are converted to
;; threaded parameters by convert-to-threaded-parameters so that they may
;; be assigned appropriate threaded parameter index numbers (for efficiency,
;; more-frequently assigned parameters should have lower numbers).
;;
(set! interrupt-handler
  (make-parameter
   (lambda () (yield (make-queue 'interrupt)))
   (lambda (x)
;;;**     (printf "ih filter for ~a~n" (thread-identifier))
     (if (procedure? x) 
         x 
         (thread-error 'interrupt-handler "~s is not a procedure" x)))))

(set! swl:repl-key   ; used by Editor's Save-and-execute to locate its repl
  (thread-make-parameter
    'default
    (lambda (x) x)))

(set! cafe-level
  (thread-make-parameter
   0 
   (lambda (x) 
     (if (integer? x) x (thread-error 'cafe-level "~s is not a integer" x)))))
; This is necessary so that new-cafe in one repl doesn't affect the
; cafe-level of new-cafe run in another repl.
(cafe-level 'set-assignment-mode! 'thread)

(set! thread-exception
  (make-parameter
   #f 
   (lambda (x) 
     (if (or (eib? x) (not x))
         x
         (thread-error 'thread-exception
                "~s is not an exception information block" x)))))
    
(set! thread-group-servers
  (make-parameter
   '()
   (lambda (x) 
     (if (and (list? x)
              (andmap (lambda (t) (tcb? t)) x))
         x
         (thread-error 'thread-group-servers
                       "~s is not a list of threads" x)))))

(set! thread-name
  (thread-make-parameter
   ""
   (lambda (x) 
     (if (string? x)
         x
         (thread-error 'thread-name "~s is not a string" x)))))

(set! thread-priority
  (make-parameter
   0
   (lambda (x)
     (if (fixnum? x)
         x
         (thread-error 'thread-priority "~s is not a fixnum" x)))))

(set! exception-handler-continuation
  (make-parameter
   (lambda () (void))
   (lambda (x)
     (if (procedure? x)
         x
         (thread-error 'exception-handler-continuation
                       "~s is not a procedure" x)))))


(set! last-exception
  (make-parameter
   #f
   (lambda (x) 
     (if (or (eib? x) (not x))
         x
         (thread-error 'last-exception
                "~s is not an exception information block" x)))))

;;
;; install new parameter default values here
;;
(break-handler tbreak-handler)

(convert-to-threaded-parameters)

(set! thread-current-input-port (thread-make-parameter (current-input-port) values))
(set! thread-current-output-port (thread-make-parameter (current-output-port) values))

(convert-chez-primitives)

;;
;; install new global (non-threaded) parameter values
;;
(keyboard-interrupt-handler kbd-handler)

(set! thread-default-ticks
  (make-parameter
   2000 ;;; Chez Scheme timer ticks
   (lambda (x)
     (unless (and (fixnum? x) (fxpositive? x))
       (thread-error 'thread-default-ticks "invalid value ~s" x))
     x)))

(timer-interrupt-handler
 ;;
 ;; thread-timer-interrupt-hook may yield. Therefore it is best to
 ;; call it from tail position so that the timer interrupt frame
 ;; doesn't appear if the thread is inspected.
 ;;
 (lambda ()
   (set-timer (thread-default-ticks))
   ;; If our time is up, and: there are sleepers, or others are ready and
   ;; we're lower than or equal to priority of the next guy (larger is lower),
   ;; then yield.  Else keep on truckin'.
   (if (and (zero? (thread-quantum-remaining))
            (or (not thread-sleep-queue-idle?)
                (and (not thread-run-queue-idle?)
                     (fx>= (tcb-precedence thread-r)
                           (tcb-precedence (tcb-next run-queue))))))
       (yield)
       (thread-timer-interrupt-hook))))

(set-timer (thread-default-ticks))
(yield) ;;; give the first thread a proper start

(thread-become-console!) ;;; must follow yield

(set! thread-bootstrapped? #t)
;;
;; ---thread system is now up and running

;;
;; ** uncomment for testing
;;
;;;(parameterize ((waiter-prompt-string "t>"))
;;;  (new-cafe))

(set! install-thread-system #f)


(let ([base-collect-request-handler (collect-request-handler)])
  (collect-request-handler
    (lambda ()
      (base-collect-request-handler)
      (for-each
        (lambda (tcb)
          ((tgcb-threads (tcb-tgcb tcb))
           'prune-broken-weak-pointers!))
        (all-threads 'get-list)))))

)) ;;; end install-thread-system
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)

(eval-when (compile eval) (internal-defines-as-letrec* #f))
(eval-when (compile) (import swl:threads))

; Prevent inlining of these primitives in the rest of the code that
; we compile, but permit inlining within the code of the thread system
; (esp. since we redefine several of these in terms of inlinable forms).
;
; * It would be nice if we could just trim out (or replace) the inline
;   handlers that could cause problems.  For example, we could prevent
;   (read-char) from going through the hardwired current-input-port,
;   but we could allow the (read-char ip) variant to be inlined.
;

#!eof


-------------------------------
Test cases for yield reentrancy
-------------------------------

dynamic-wind tests:
-------------------

(dynamic-wind yield yield void)
(dynamic-wind yield yield yield)


yield timer test:
-----------------

(begin
  (set! thread-bootstrapped? #f) ;;; else parameter timer hack messes up test
  (timer-interrupt-handler
   (lambda () ;;; ensure that timer int causes a yield
     (fprintf thread-conout "tim int!~n")
     (thread-yield)))
  (thread-fork-group
     (lambda ()
       (let ((counter 0))
         (dynamic-wind
          (lambda () (set! counter (1+ counter)))
          (lambda () 
            (let f ((n 500))
              (when (positive? n)
                (fprintf thread-conout "setting timer to ~s~n" n)
                (set-timer n)
                (thread-yield)
                (set-timer 0)
                (f (1- n)))))
          (lambda () (set! counter (1- counter))))
         (when (zero? counter) 
           (fprintf thread-conout "reentrancy-test successful.~n")
           (abort))
         (fprintf thread-conout
                  "error: reentrancy-test counter=~s ~n"
                  counter)
         (abort)))
     10)
  (thread-sleep 99999))


repl test:
----------

Start a SWL repl and eval (collect-notify #t), followed by (let f () (f)).
Let it run for awhile. Notifications should appear in repl window only.

Note that notification occurs only if collection actually occurs
within the execution of the repl thread.


-------------------
Other useful tests
-------------------



Many Threads Test
-----------------

(define foo
  (lambda (n)
    (let foo ([n n])
      (unless (zero? n)
        (thread-fork-group (lambda () (+ 2 2)))
        (foo (- n 1))))
    (thread-yield)))

(let f ((i 0)) (foo 500) (printf "i=~s~n" i) (f (1+ i)))

let it run awhile.



Many Queues Test
----------------

(define qtest
  (lambda (n)
    (let ((q0 (thread-make-msg-queue 'test)))
      (thread-send-msg q0 (+ n n))
      (let ((q 
             (let foo ([n n] [q q0])
               (if (zero? n)
                   q
                   (let ((q2 (thread-make-msg-queue
                              (string-append "test" (number->string n)))))
                     (thread-fork-group
                      (lambda ()
                        (thread-send-msg q2 (1- (thread-receive-msg q)))))
                     (foo (- n 1) q2))))))
        (when (thread-receiver-waiting? q)
          (thread-error 'qtest "receiver waiting"))
        (let ((msg (thread-receive-msg q)))
          (unless (= msg n)
            (thread-error 'qtest "wrong msg: ~s" msg))
          (printf "qtest ok~n"))))))

(let f ((i 0)) (printf "i=~s num=~s" i (ps-num)) (flush-output-port) (qtest 1000) (thread-yield) (f (1+ i)))

let it run awhile.


many senders/many receivers test
--------------------------------

(define srtest
  (lambda (n)
    (let ((q0 (thread-make-msg-queue 'test))
          (tot 0)
          (msgs 0))
      (thread-fork-group
       (lambda () 
         (let foo ([n n])
           (if (zero? n)
               #f
               (begin
                 (thread-fork-group (lambda () (thread-send-msg q0 n)))
                 (foo (- n 1)))))))
      (thread-fork-group
       (lambda () 
         (let foo ([n n])
           (if (zero? n)
               #f
               (begin
                 (thread-fork-group
                  (lambda ()
                    (let ((msg (thread-receive-msg q0)))
                      (critical-section
                        (set! tot (+ tot msg))
                        (set! msgs (1+ msgs))))))
                 (foo (- n 1)))))))
      (let f ()
        (if (< msgs n)
            (f)
            (if (= (/ (* n (1+ n)) 2) tot)
                (printf "srtest ok~n")
                (thread-error 'srtest "bad tot: ~s" tot)))))))

(let f ((i 0)) (display i) (srtest 1000) (thread-yield) (f (1+ i)))

let it run awhile.

-------------------
performance testing
-------------------

In general, thread context-switch overhead should be no worse than
about 6% when multiple threads are running.

Thread system performance is optimized when only one thread is running
at the current priority.  Overhead in this case should be about 3%.

Thread system overhead is inversely proportional to the value
of thread-default-ticks.  


optimized case performance test:

(define (fact n) (if (zero? n) 1 (* n (fact (1- n)))))
(define (loop c t) (if (zero? c) #f (begin (t) (loop (1- c) t))))
(time (loop 10000 (lambda () (fact 100))))

Also, verify that context switches occur at the right frequency.



-----------------------
interrupt/break testing
-----------------------

check that a keyboard interrupt transfers to the debugger,
and that resetting and exiting from the debugger work properly.



interrupt handler
-----------------

(define t2
  (thread-fork-group
   (lambda ()
     (parameterize ((interrupt-handler
                         (lambda () (fprintf thread-conout "howdy!~n"))))
       (let f () (f))))
   10))

(thread-break t2)

