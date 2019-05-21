;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


; This file is now included by threads.ss

(define-swl-class (<sem> max . name) (<base>)
  (ivars (max (begin
                (unless (and (fixnum? max) (fxpositive? max))
                  (assertion-violationf '<sem> "invalid maximum value ~s" max))
                max))
         (n 0)
         (q (thread-make-msg-queue (if (pair? name) (car name) 'semaphore)))
	 )
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [wait ()
     (thread-critical-section
       (set! n (fx+ n 1))
       (when (fx> n max) (thread-receive-msg q)))]
    [signal ()
     (thread-critical-section
       (unless (fxzero? n)
         (let ([waiting n])
           (set! n (fx- n 1))
           (when (fx> waiting max) (thread-send-msg q 1)))))]))

