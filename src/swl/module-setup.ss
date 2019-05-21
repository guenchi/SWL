(module swl:module-setup (swl:fallback-queue
                          swl:begin-application
                          swl:end-application
                          open-interaction-window
                          swl:load-foreign
                          swl:run-before-args
                          swl:run-before-first-thread-fork
                          swl:init-preferences-hack
                          swl:get-registry
                          swl:add-editor
                          swl:remove-editor
                          swl:get-editors
                          swl:insert-repl
                          swl:remove-repl
                          swl:lookup-repl
                          swl:repl-key
                          request?
                          make-request
                          request-reason
                          request-denied
                          )

  (define swl:fallback-queue)
    ;; initialized in teventloop.ss (after thread system has been
    ;; initialized) so that it becomes a thread parameter

  (define swl:begin-application)
  (define swl:end-application)
    ;; initialized in init.ss, but used in teventloop.ss

  (define open-interaction-window)
    ;; initialized in console.ss, but used in init.ss so that we get
    ;; console installed early enough to be global current i/o port
    ;; shared by all threads (though copy-on-write)

  (define swl:load-foreign)
    ;; initialized by build script, called by init.ss

  (define swl:run-before-args void)
    ;; plan is to doctor this via a patch file
    ;; it's run by swl:startup (in init.ss) just
    ;; after we setup the application queue stuff
    ;; (so we can register application instances)
    ;; but before loading of the command-line args
    ;; or startup of the first repl / editor

  (define swl:run-before-first-thread-fork
    (lambda ()
     ; install the graphical interaction window as current i/o ports
     ; before starting up first repl
      (let ([p (open-interaction-window)])
        (current-input-port p)
        (current-output-port p))))

  (define swl:init-preferences-hack) ; see preferences.ss

 ; installed by init-preferences-hack in preferences.ss, used by server.ss
 ; and preferences.ss
  (define swl:get-registry (lambda (key) #f))

  (module (swl:add-editor swl:remove-editor swl:get-editors
           swl:insert-repl swl:remove-repl swl:lookup-repl
           swl:repl-key)
    ; should be more general --- not just interested in editors
    (define swl:repl-key)
    (define editors '())
    (define repls '())
    (define swl:add-editor
      (lambda (ed)
        (critical-section
          (set! editors (cons ed editors)))))
    (define swl:remove-editor
      (lambda (ed)
        (critical-section
          (set! editors (remq ed editors)))))
    (define swl:get-editors
      (lambda ()
        (critical-section
          (list-copy editors))))
    (define swl:insert-repl
      (lambda (key inst)
        (critical-section
          (cond
            [(assq key repls) => (lambda (a) (set-cdr! a inst))]
            [else (set! repls (cons (cons key inst) repls))]))))
    (define swl:remove-repl
      (lambda (key)
        (critical-section
          (set! repls (remq (assq key repls) repls)))))
    (define swl:lookup-repl
      (lambda (key)
        (critical-section
          (cond
            [(assq key repls) => cdr]
            [else #f])))))

  (define-record #{request |*d04pAm*~+YGT\\&|} (reason denied))

)

(eval-when (compile) (import swl:module-setup))
