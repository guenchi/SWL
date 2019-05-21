;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define warning-dialog
  (let ([ls '()] [thread #f])
    (import swl:oop)
    (import swl:threads)
    (import swl:macros)
    (import swl:option)
    (import swl:generics)
    (import swl:module-setup)
    (case-lambda
      [(flag/parent msg) (warning-dialog flag/parent msg '(ok))]
      [(flag/parent msg actions)
       (define make-dialog
         (lambda (block? parent x y width height)
           (let ([msg-queue (thread-make-msg-queue 'warning-dialog)])
             (define top #f)
             (define time-to-go? (not block?))
             (define exit-notify
               (lambda ()
                 (set! time-to-go? #t)
                 (on-error 'ignore (destroy top))))
             (swl:begin-application
               (lambda (token)
                 (thread-fork
                   (lambda ()
                     (set! top
                       (create <toplevel> with
                         (title: "Warning")
                         (destroy-request-handler:
                           (lambda (self)
                             (critical-section
                               (when time-to-go?
                                 (set! ls (remq self ls))
                                 (swl:end-application token))
                               time-to-go?)))))
; supposedly the slow raise bug has been fixed in Tcl/Tk 8.3.4, so this
; shouldn't totally tank performance, and it's about the only cross-platform
; mechanism I can find for keeping the dialog on top.
                     (critical-section
                       (set! ls (cons top ls))
                       (unless thread
                         (set! thread
                           (thread-fork
                             (rec loop
                               (lambda ()
                                 (thread-sleep 500)
                                 (when (critical-section
                                         (if (null? ls)
                                             (begin (set! thread #f) #f)
                                             (begin (send (car ls) raise) #t)))
                                   (loop))))))))
                     (on-error (exit-notify)
                       (when parent (set-transient! top parent))
                       (hide top)
                       (swl:sync-display)
                       (let* ([frame (create <frame> top)]
                              [msglab
                               (create <label> frame with
                                 (title: msg)
                                 (justify: 'left)
                                 (background-color: 'white)
                                 (border-width: 6)
                                 (wrap-length: 400)
                                 (relief: 'flat)
                                 (font: (create <font> 'helvetica 12 '(roman bold))))]
                              [bframe
                               (create <frame> frame with
                                 (border-width: 6)
                                 (background-color: 'white))]
                              [make-button
                               (lambda (sym)
                                 (create <button> bframe with
                                   (title: (symbol->string sym))
                                   (action:
                                     (lambda (self)
                                       (if block?
                                           (thread-send-msg msg-queue sym)
                                           (send top destroy))))))]
                              [buttons (map make-button actions)])
                         (pack frame (expand: #t) (fill: 'both))
                         (pack msglab (expand: #t) (fill: 'both))
                         (pack bframe (side: 'bottom) (fill: 'x))
                         (for-each
                           (lambda (btn) (pack btn (expand: #t) (side: 'left)))
                           buttons)
                         (swl:sync-display)
                         (let ([tw (send top get-width)]
                               [th (send top get-height)])
                           (send top set-geometry!
                             (format "+~a+~a"
                               (+ x (quotient (- width tw) 2))
                               (+ y (quotient (- height th) 2))))
                           (show top)
                           (swl:sync-display))
                         (when (= (length buttons) 1)
                           (send (car buttons) set-focus))))))
                 exit-notify))
             (when block?
               (let ([msg (thread-receive-msg msg-queue)])
                 (exit-notify)
                 msg)))))
       (if (and (instance? flag/parent) (isa? flag/parent <toplevel>))
           (swl:application-modal
             (send flag/parent get-application-context)
             (lambda ()
               (make-dialog #t flag/parent
                 (get-root-x flag/parent)
                 (get-root-y flag/parent)
                 (get-width flag/parent)
                 (get-height flag/parent))))
           (make-dialog
             (case flag/parent
               [(noblock)
                 (unless (and (list? actions) (= (length actions) 1))
                   (assertion-violationf 'warning-dialog "noblock is valid only when there is a single button"))
                 #f]
               [else #t])
             #f 0 0
             (swl:screen-width)
             (swl:screen-height)))])))

