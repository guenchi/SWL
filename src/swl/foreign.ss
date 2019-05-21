;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(module swl:foreign (
    swl:foreign-init
    swl:do-one-event
    swl:peek-event
    swl:tcl-init
    swl:raw-tcl-eval
    swl:watch-channel
    swl:open-tcp-client
    swl:open-tcp-server
    swl:install-non-blocking-console
    swl:get-channel
    swl:os-get-error
    swl:os-bytes-ready
    swl:os-eof
    swl:os-flush
    swl:os-c-read-n
    swl:os-c-write-n
    swl:name-to-window
    swl:map-window
    swl:unmap-window
    swl:move-window
    swl:resize-window
    swl:debug-mem
    swl:open-dde-file)

(define swl:do-one-event)
(define swl:peek-event)
(define swl:tcl-init)
(define swl:raw-tcl-eval)
(define swl:watch-channel)
(define swl:open-tcp-client)
(define swl:open-tcp-server)
(define swl:install-non-blocking-console)
(define swl:get-channel)
(define swl:os-get-error)
(define swl:os-bytes-ready)
(define swl:os-eof)
(define swl:os-flush)
(define swl:os-c-read-n)
(define swl:os-c-write-n)
(define swl:name-to-window)
(define swl:map-window)
(define swl:unmap-window)
(define swl:move-window)
(define swl:resize-window)
(define swl:debug-mem)
(define swl:open-dde-file)

(define swl:default-buffer-size 2048)

(define swl:foreign-init
  (lambda () 

    ;; The foreign-entry SWL_DoOneEvent returns
    ;;   0  no fallback to process, *args not touched
    ;;  -n  do fallback n, no args
    ;;   n  do fallback n, args have been hammered into the args array
    ;;      if the args didn't fit, then the first byte of the args array
    ;;      will be 0 and n is actually the size of string needed for
    ;;      the args
    ;; 
    (define make-event-handler
      (lambda (foreign-entry)
	(let* ((default-bufsize 1)
	       (buf (make-bytevector default-bufsize))
               (transcoder (make-transcoder (utf-8-codec))))
	  (rec loop
	    (lambda (block?)
              (critical-section
                (let ((result (foreign-entry block? buf (bytevector-length buf))))
                  (cond
                    ((fxzero? result) (values #f #f))
                    ((fxnegative? result) (values (fx- result) #f))
                    (else 
                     ; why doesn't this ever reduce the size of buf? (buf doesn't get very large?)
		     (if (fx= (bytevector-u8-ref buf 0) 0)
			 (let ([newlen (fxmax result (* 2 (bytevector-length buf)))])
			   (set! buf (make-bytevector newlen))
			   (loop block?))
			 (values result
				 (read (open-input-string (bytevector->string buf transcoder))))))))))))))
 
    ;; Process an event on the C side.  Flag passed in determines whether
    ;; or not it should block and wait for the event.
    ;; returns 2 values:
    ;;   index of callback to invoke
    ;;   list of args to the callback (or #f if none)

    (set! swl:do-one-event
      (make-event-handler
       (foreign-procedure "SWL_DoOneEvent" (boolean u8* integer-32) integer-32)))

    (set! swl:peek-event (foreign-procedure "SWL_PeekEvent" () boolean))

    (set! swl:raw-tcl-eval
      (lambda (str)
        (assertion-violationf 'swl:tcl-eval
	       "system not initialized with swl:tcl-init before ~s" str)))

    (set! swl:tcl-init
      (let ([fp-tcl-init (foreign-procedure "SWL_TclInit"
					    (integer-32 integer-32) boolean)])
	(lambda (queue-size argbuf-size)
	  (unless (fp-tcl-init queue-size argbuf-size)
	    (assertion-violationf 'swl:tcl-init
		   "unable to initialize graphics library (check DISPLAY)"))
	  (set! swl:raw-tcl-eval
	    (let ([foreign-tcl
		   (foreign-procedure "SWL_TclEval" (string u8*) string)]
		  [err-flag (make-bytevector 1)])
	      (lambda (str)
		(let ([x (foreign-tcl str err-flag)])
		  (if (fx= (bytevector-u8-ref err-flag 0) 1)
		      x
		      (#%error #f x)))))))))

    (set! swl:watch-channel
      ;; handler is a thunk that will be called when the given channel
      ;; is readable / writable (depends on flag passed in)
      (lambda (channel flag handler)
        (unless (and '(channel? channel) (symbol? flag))
          (assertion-violationf 'swl:watch-file
		 "invalid argument(s) ~s ~s" channel flag))
        (let ([cb (swl:procedure->callback handler 'swl:system-queue)])
          (swl:tcl-eval 'fileevent channel
			(case flag
			  [(read) 'readable]
			  [(write) 'writable]
			  [else (assertion-violationf 'swl:watch-file "invalid flag ~s" flag)])
			cb)
          cb)))

    (swl:api-procedure set! swl:open-tcp-client
      ;* \formdef{swl:open-tcp-client}{procedure}{(swl:open-tcp-client \var{hostname} \var{port-number} \var{async?)}}
      ;* \ret{an input-output port}
      ;* Returns an input/output port connected by a socket
      ;* to the specified port number on the given host.
      ;* If the async flag is true, the socket is connected asynchronously, i.e.,
      ;* the port object is returned without waiting for the actual connection
      ;* to be made.
      (lambda (host port async)
	(unless (and (integer? port)
		     (string? host)
		     (boolean? async))
	  (assertion-violationf 'swl:open-tcp-client
		 "invalid argument(s): ~s ~s ~s" port host async))
	(let ([channel (swl:tcl-eval 'socket (if async '-async #\space) host port)])
	  (swl:make-nb-input-output-port
	   (parameterize ((print-radix 10)) (format "~a:~a" host port))
	   channel swl:default-buffer-size
	   channel swl:default-buffer-size))))

    (swl:api-procedure set! swl:open-tcp-server
      ;* \formdef{swl:open-tcp-server}{procedure}{(swl:open-tcp-server \var{host} \var{port} \var{handler})}
      ;* \ret{an input-output port}
      ;* Creates a server socket for the given \var{port}.  \var{host} specifies
      ;* a fully qualified domain name or numerical IP address of the
      ;* network interface to use for the connection.  If \var{host} is \scheme{#f}
      ;* then the server accepts connections via any interface.
      ;* When a connection is made, \var{handler} is passed three arguments:
      ;* an input/output port connected to the client via socket, the hostname
      ;* of the client and the port number for the client.
      ;* \scheme{swl:open-tcp-server} returns a port that cannot be read from or
      ;* written to, but which can be closed to terminate the server.
      (lambda (host port handler)
        (unless (and (integer? port)
                     (or (not host) (string? host))
                     (procedure? handler))
          (assertion-violationf 'swl:open-tcp-server
                 "invalid argument(s): ~s ~s ~s" port host handler))
        (let ([cb (swl:procedure->callback
               ; each swl:system-queue callback is run in a separate thread
; This should probably play the same games that we're
; currently playing in server.ss to ensure that the
; various thread parameters are set properly.
                   (lambda (channel hostname port-num)
                     (handler
                      (swl:make-nb-input-output-port
                       (parameterize ((print-radix 10))
                         (format "~a:~a" hostname port-num))
                       channel swl:default-buffer-size
                       channel swl:default-buffer-size)
                      (symbol->string hostname) port-num))
                   'swl:system-queue)])
          (let ([channel
                 (if host
                     (swl:tcl-eval 'socket '-server cb '-myaddr host port)
                     (swl:tcl-eval 'socket '-server cb port))])
            (swl:make-nb-input-output-port cb channel 0 channel 0)))))

#;  (set! swl:install-non-blocking-console
      (lambda ()
        (when (memq (machine-type) '(ppcnt i3nt))
          (assertion-violationf 'swl:install-non-blocking-console
                 "not supported under Windows NT"))
        (let ([p (swl:make-nb-input-output-port
                  "non-blocking console"
                  "stdin" swl:default-buffer-size
                  "stdout" swl:default-buffer-size)])
          ;;
          ;; install nonblocking port as the new global default.
          ;;
          (console-output-port 'set-global! p)
          (console-input-port 'set-global! p)
          (current-input-port 'set-global! p)
          (current-output-port 'set-global! p)
          ;;
          ;; Install nonblocking port for current thread.  This is essential
          ;; because these parameters have been previously assigned for
          ;; the thread.
          ;;
          (current-input-port p)
          (current-output-port p)
          )))

    (set! swl:get-channel
      (let ((getit (foreign-procedure "SWL_GetChannel" (string) integer-32)))
        (lambda (name)
          (getit
            (if (string? name)
                name
                (parameterize ([print-radix 10]) (format "~s" name)))))))

    (set! swl:os-get-error (foreign-procedure "SWL_GetError" () string))

    (set! swl:os-bytes-ready
      (foreign-procedure "Tcl_InputBuffered" (integer-32) integer-32))
    
    (set! swl:os-eof
      (foreign-procedure "Tcl_Eof" (integer-32) boolean))
    
    (set! swl:os-flush
      (foreign-procedure "Tcl_Flush" (integer-32) integer-32))
    
    (set! swl:os-c-read-n
      (let ([fp (foreign-procedure "SWL_channel_read" (int u8* int int) int)])
        (lambda (channel str index count)
          (let ([bv (make-bytevector count)])
            (let ([n (fp channel bv 0 count)])
              (when (fx> n 0)
                (do ([i 0 (fx+ i 1)] [j index (fx+ j 1)])
                    ((fx= i n))
                 ; this performs a bogus conversion, but since we have no idea
                 ; of the source representation, we don't have any better option
                  (string-set! str j (integer->char (bytevector-u8-ref bv i)))))
              n)))))
    
    (set! swl:os-c-write-n
      (let ([fp (foreign-procedure "SWL_channel_write" (int u8* int int) int)])
        (lambda (channel str index count)
          (let ([bv (make-bytevector count)])
            (when (fx> count 0)
              (do ([i 0 (fx+ i 1)] [j index (fx+ j 1)])
                  ((fx= i count))
               ; this performs a bogus conversion as above
                (bytevector-u8-set! bv i (char->integer (string-ref str j)))))
            (fp channel bv 0 count)))))

    (set! swl:name-to-window
      (foreign-procedure "SWL_name_to_window"
                         (string)
                         integer-32))

    (set! swl:map-window (foreign-procedure "Tk_MapWindow" (integer-32) void))
    (set! swl:unmap-window (foreign-procedure "Tk_UnmapWindow" (integer-32) void))

    (set! swl:move-window
      (foreign-procedure "Tk_MoveWindow"
                         (integer-32 integer-32 integer-32)
                         void))

    (set! swl:resize-window
      (foreign-procedure "Tk_ResizeWindow"
                         (integer-32 integer-32 integer-32)
                         void))

    (set! swl:debug-mem (foreign-procedure "SWL_mem_dbg" () void))

    (set! swl:tcl-finalize (foreign-procedure "Tcl_Finalize" () void))

    (when (memq (machine-type) '(ppcnt i3nt))
      (load-shared-object "crtdll")
      (set! swl:open-dde-file
        (foreign-procedure "SWL_open_dde_file" (string string string)
          unsigned-32)))

; Only for old version of SWL based on Tk 4.2
;   (set! swl:query-font
;     (foreign-procedure "SWL_queryFont" (string) boolean))

    ))
)

(eval-when (compile)
(import swl:foreign)
)

