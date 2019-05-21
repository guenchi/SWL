;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; when using the Tk file-event-handler the Tk side indicates a read is ready
;; by indicating callback 1 should be invoked
;;
;; By allocating this callback here and retaining it so its slot isn't reclaimed
;; we ensure that it's id is 1.  A bit gross, but we don't plan to use the non-
;; threaded eventloop much longer anyway.

(define read-proc (swl:callback-lambda () (printf "not called~n")))
(define read-proc-id 1)

(define bob-prompt-and-read
  (rec wpar
    (lambda (n)
      (display "swl" (console-output-port))
      (do ((n n (- n 1)))
          ((= n 0)
           (write-char #\space (console-output-port))
           (flush-output-port (console-output-port)))
          (display ">" (console-output-port)))
      (let event-loop ()
        (call-with-values
          (lambda () (wait-for-event))
          (lambda (fallback-id args)
            (if (not fallback-id)
                (wpar n)
                (if (fx= fallback-id read-proc-id)
                    (read (console-input-port))
                    (begin
                      (swl:apply-callback-proc (swl:lookup-callback fallback-id) args)
                      (event-loop))))))))))

(define single-threaded-bob
  (lambda (args)
    (waiter-prompt-and-read bob-prompt-and-read)
    (for-each load args)
    (new-cafe)
    (when (root-window) (send (root-window) destroy))
    (exit)))

