(define start-swl-server
  (lambda (args)
    
    (import swl:foreign)
    (import swl:macros)
    (import swl:module-setup)
    
   ; It appears that clients may only connect via the specified address,
   ; so this prevents remote access to our SWL server.  (An attacker could
   ; set up a proxy to forward connections to the local machine.)
    (define hostip "127.0.0.1")
    
    (define open-swl-server
      (lambda (p)
        (on-error #f
          (let ([cookie (gensym)])
            (define server-port
              (swl:open-tcp-server hostip 0
                (lambda (cp hostname clientport)
                  (when (eq? (read cp) cookie)
                    (let* ([dir (read cp)] [args (read cp)])
                      (swl:begin-application
                        (lambda (token)
                          (define thread
                            (thread-fork
                              (lambda ()
                                (call/cc
                                  (lambda (k)
                                    (parameterize ([current-directory dir]
                                                   [abort-handler k]
                                                   [exit-handler k]
                                                   [interrupt-handler k]
                                                   [reset-handler k]
                                                   [exception-handler-continuation k])
                                      "; defined in init.ss"
                                      (do-command-line args))))
                                (set! thread #f)
                                (swl:end-application token))))
                          (lambda ()
                            (when thread
                              (case (warning-dialog #f ; want this dialog to block
                                      (format "Still processing command-line arguments ~s" args)
                                      '(|keep processing| |stop processing|))
                                [(|stop processing|)
                                 (on-error 'ignore
                                   (thread-kill thread)
                                   (set! thread #f))
                                 (swl:end-application token)])))
                          ))))
                  (close-port cp))))
           ; protect from gc
            (#%$sputprop 'gc-protect '*server-port* server-port)
            (truncate-file p 0)
            (write cookie p)
            (newline p)
            (write ((port-handler server-port) 'port-number p) p)
            (flush-output-port p)))))
    
    (on-error (with-message msg (warning-dialog 'noblock (format "Unable to contact or initialize SWL server.\n\n~a" msg)))
      (let ([ip (case (machine-type)
                  [(i3nt ppcnt)
                   (let ([key (format "HKEY_LOCAL_MACHINE\\Software\\Chez Scheme\\swl~a\\ServerFile" swl:version)])
                     (on-error (assertion-violationf #f "Unable to determine server port.\nCheck registry setting for ~a" key)
                       (open-input-output-file
                         (or (swl:get-registry key #t)
                             (assertion-violationf #f "missing registry entry for ~a" key))
                         '(exclusive mode #o600))))]
                  [else (open-input-output-file
                          (format "/tmp/.swl_~a" (getenv "USER"))
                         '(exclusive mode #o600))])])
        (on-error (open-swl-server ip)
          (let* ([cookie (read ip)] [port-number (read ip)])
            (unless (and (gensym? cookie) (fixnum? port-number) (fx<= 0 port-number #xFFFF))
              (assertion-violationf #f "malformed server info file ~s" ip))
            (let ([p (swl:open-tcp-client hostip port-number #f)])
              (fprintf p "~s ~s ~s" cookie (current-directory) args)
              (close-port p)
              (close-port ip)
              (exit 0))))
        (close-port ip)))
))

