;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(module swl:eventloop
  (swl:make-fallback-queue
   swl:event-dispatch
   swl:application-modal
 )

(import swl:oop)
(import swl:macros)
(import swl:foreign)
(import swl:module-setup)

;; Note to Wally:
;;   I think you can get the "single-threaded" feel you want by changing
;;   mode (in swl:event-dispatch) to something other than 'dispatch.
;;   Don't worry, the optimizer will clean up the apparent eq? test at
;;   compile time.


;;
;; 1/15 (johnz) this is almost identical to the standard Chez error handler.
;; it is not needed to work with threads.  assuming it was temporary hack.
;; 
;;(define swl:report-error
;;  (lambda (what who msg args)
;;    (let ([who (if who (format "~a" who) "")]
;;          [msg (parameterize ([print-level 3] [print-length 6])
;;                  (apply format msg args))])
;;      (let ([who (if (string=? who "") "" (format " in ~a" who))]
;;            [msg (if (string=? msg "") "" (format ": ~a" msg))])
;;        (fprintf (console-output-port) "~%~a~a~a.~%" what who msg)
;;        (flush-output-port (console-output-port))))))

(define swl:default-quantum thread-default-quantum)
;;  old paranoid code
;;
;;  (define swl:event-dispatch
;;    (lambda ()
;;      (let loop ()
;;        (call-with-values
;;          (lambda () (do-event))
;;          (lambda (fallback-id args)
;;            (when fallback-id
;;              (when (eq? (swl:bug) 'dispatch)
;;                (fprintf (swl:bug-port) "x-thread ~s ~s~n" fallback-id args))
;;              (let ([fbqueue
;;                     ;; this grossness explained above
;;                     (and 
;;  args
;;                          (not (null? args))
;;                          (let ([handle (car args)])
;;                            (and (symbol? handle)
;;                                 (swl:lookup-fallback-queue handle))))]
;;                    [cbproc (swl:lookup-callback fallback-id)])
;;                (if fbqueue
;;                    (thread-send-msg (unbox fbqueue) (cons cbproc args))
;;  (begin
;;  (fprintf (swl:bug-port) "Fallback that had no fbq token: ~s ~s~n" fallback-id args)
;;  (flush-output-port (swl:bug-port))
;;                     ;; this grossness explained above
;;                    (swl:apply-callback-proc cbproc args)))
;;  )
;;              (loop)))))))

;; Wally found that a race condition existed in destroying
;; <container-widget>s where there could still be configure
;; events pending for containers after they were destroyed.
;; Since the problem manifested as a fallback whose widget-hanlde
;; has no fallback-queue, we silently drop these bogus fallbacks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get next event from the C event-loop.  If no threads are running
;; then block on the C side so we don't hog CPU.

(define swl:events-dispatched 0)
(define-syntax swl:make-fallback (identifier-syntax cons))
(define-syntax swl:fallback? (identifier-syntax pair?))
(define-syntax swl:fallback-proc (identifier-syntax car))
(define-syntax swl:fallback-args (identifier-syntax cdr))

(define swl:event-dispatch
  (let ()
    (define swl:set-max-block-time
      (lambda (interval) (swl:tcl-eval 'after interval '())))
    (define get-handle car)
    (define mode 'dispatch)
    (define handle-one-event
      (lambda (fallback-id args)
(when (swl:bug)
  (fprintf (swl:bug-port) "swl:event-dispatch:  ~s ~s~n" fallback-id args))
        (let ([fbqueue (swl:lookup-fallback-queue (get-handle args))]
              [cbproc (swl:lookup-callback fallback-id)])       
(when (swl:bug)
  (fprintf (swl:bug-port) "swl:event-dispatch:  fbqueue ~s cbproc ~s~n"
    (if fbqueue 'found 'missing)
    (if cbproc 'found 'missing)))
          (when fbqueue
            (if (eq? mode 'dispatch)
                (begin
                  (thread-send-msg (swl:fbq-queue fbqueue)
                    (swl:make-fallback cbproc args))
                  (set! swl:events-dispatched (fx1+ swl:events-dispatched)))
                (swl:apply-callback-proc cbproc args))))))
    (lambda ()
      (unless thread-sleep-queue-idle?
        (swl:set-max-block-time (thread-sleep-check)))
      (mvlet ((fallback-id args) (swl:do-one-event thread-run-queue-idle?))
         (when fallback-id
           (handle-one-event fallback-id args))))))

;; added level of indirection to support swl:application-modal
;; (may eventually want to change the thread system to use objects
;;  instead of structures in case there is some indirection in the
;;  low-level queues that we could exploit here)

;; NOTE: if you change the swl:fbq stuff here, change it also in
;;       init.ss   (swl:insert-widget 'swl:system-queue #f (make-swl:fbq q 'swl:system-queue))

;; Not sure this guy really needs to be API procedure.
(define swl:make-fallback-queue
  ;* \formdef{swl:make-fallback-queue}{procedure}{(swl:make-fallback-queue)}
  ;* \ret{see below}
  ;*
  ;* This procedure returns a new event queue and forks off a thread
  ;* to service callback requests delivered to this queue.
  ;* See also the \ref{swl:fallback-queue}
  ;* thread parameter.
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      (let* ([q (thread-make-msg-queue 'idle)]
             [handle (swl:new-handle #f)]
             [fbq (make-swl:fbq q handle)])
        ;; Fork a thread rather than a thread-group so that the fallback
        ;; thread sees the actual parameters of the application for which
        ;; it is servicing events.
        (thread-fork
          (lambda ()
            (thread-become-server!)
            (call/cc reset-handler)
            (thread-name "Fallback")
            (let loop ()
              (let ([fb (thread-receive-msg q)])
                (cond
                  [(swl:fallback? fb)
                   (swl:apply-callback-proc
                     (swl:fallback-proc fb)
                     (swl:fallback-args fb))
                   (loop)]
                  [(eq? fb 'exit) (swl:delete-widget handle)]
                  [else
                   (warningf #f "unexpected fallback ~s" fb)
                   (loop)]))))
          (thread-default-quantum)
          -1) ;;; run with high priority
        ; record handle so that menu-items have a way to construct
        ; suitable callback-procs.  (see menu.ss)
        (swl:insert-sticky-widget handle "fbq" fbq)
        fbq))))

; BUG:  this guy needs to pass configure events (etc.) through 
(swl:api-procedure define swl:application-modal
  ;* \formdef{swl:application-modal}{procedure}{(swl:application-modal \var{application} \var{thunk})}
  ;* \ret{see below}
  ;*
  ;* This procedure returns the result of evaluating \var{thunk}.
  ;* During the evaluation of \var{thunk}, user-interface events
  ;* intended for \var{application} are ignored.  This can be used,
  ;* for example, to implement a file-selection dialog that prevents
  ;* the user from manipulating the calling application until the
  ;* dialog is dismissed.  Note that the threads of the application
  ;* are not suspended; its stream of events is merely redirected
  ;* to a bit sink.  See \ref{swl:begin-application}.
  ;   See also \ref{swl:fallback-queue}
  ;   and \ref{get-application-context}.
  (lambda (application thunk)
    (unless (procedure? thunk)
      (assertion-violationf 'swl:application-modal "~s is not a procedure" thunk))
    (let ([resume (thread-make-msg-queue 'modal)]
          [oldq (swl:fbq-queue application)])
; oops!
; with nested warning dialogs, we really need to be making a stack of these things
; or turn it into a flag in the fallback queue
;  *** CANT BE FLAG, has to be number so we can do stack stuff w/it
;  ugh, more critical section stuff
; maybe combination of modal-count and tmp queue
      (set-swl:fbq-queue! application (thread-make-msg-queue '/dev/null))
; calling swl:begin-application just to get a new fallback queue I think:
; problem is: it holds mutex until start-k returns
      (swl:begin-application
        (lambda (token)
          (let ([vals #f])
; wait this guy hasn't even returned his exit-k yet
; so if we get an error while running this start-k,
; we'd expect begin-app to bail us out via dynamic wind mutex thing
            (dynamic-wind
              void
              (lambda ()
(on-error (set! vals #f)
                (call-with-values thunk (lambda v (set! vals v))))
)
              (lambda ()
                (swl:end-application token)
                (thread-send-msg resume vals))))
          void))
      (let ([vals (thread-receive-msg resume)])
       ; if thunk errors out, vals is still #f otherwise vals is list
        (set-swl:fbq-queue! application oldq)
(if (not vals)
(assertion-violationf 'swl:application-modal "thunk error out\n")
        (apply values vals)))))
)


;; notion is that we stash application along with every widget
;; then when we get a callback for that widget, we grab its application
;; and run with that.
;;
;; to go modal, we would grab the application for the top-level widget
;; of our application and send it a go-modal message.
;;
;; ways to get the application object for a widget:
;;   - table entry associated with every widget handle as it is now
;;     [fast lookup for the event-dispatcher, but more hacky to give
;;      other widgets access to your application]
;;   - have a get-application method on widgets that returns the application
;;     in which they're running (ie. the one servicing their callbacks)
;;     (this could just ask the parent who knows).
;;     or we could have the method access the lookup table described in the
;;     previous option, so we have best of both worlds.
;;
;; ways to associate an application with a widget:
;;   - currently the application for a widget is whatever application ran
;;     the code that created the widget.  (thread parameter)
;;   - could provide a method that bangs on the application stashed in the
;;     lookup table (or ivar) wherever we store this info
;;       - may want this to propagate down from parent to child
;;   - could be part of the context we pass around:  instead of just passing
;;     in a parent for the widgets, we'd pass in a context that knows about
;;       - parent
;;       - application
;;   - could be that parent widget encodes that info, or makes it available
;;     to children who call the "get-application-context" method
;;

; (define-swl-class (<swl-application> thunk) (<base>)
;   (ivars (fallback-queue (swl:make-fallback-queue))
;          (thread (thread-fork-group thunk)))
;   (inherited)
;   (inheritable)
;   (private)
;   (protected)
;   (public
;     [get-fallback-queue () fallback-queue]
;     [set-fallback-queue! (q) (set! fallback-queue q)]
;     [go-modal (thunk)
;      (let ((q fallback-queue))
;        (send self set-fallback-queue! (thread-make-msg-queue 'wait-modal))
;        (swl:make-application thunk)
;        (send self set-fallback-queue! q))]))

)

(parameterize ((subset-mode 'system))
  (parameterize ((run-cp0 (lambda (cp0 x) x))) ;prevent propagation of as-time-goes-by
    (eval '(set! $as-time-goes-by
            ; top-level-value is to prevent optimizer from propagating reference
             (let ((atgb $as-time-goes-by))
               (lambda (e t)
                 (fprintf (console-output-port)
                   "Warning (time): thread system overhead skews timing results~%")
                 (set! $as-time-goes-by atgb)
                 (atgb e t)))))))

(eval-when (compile)
  (import swl:eventloop)
)

