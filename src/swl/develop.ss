;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


;; A number of convenience functions for the benefit of developers.

(define cd current-directory)

(define pb ;;; depends on loaded syntax
  (lambda args
    (letrec
        ((repify
          (lambda (arg)
            (cond
              [(instance? arg) (send arg print-rep)]
              [(pair? arg)
               (cons (repify (car arg)) (repify (cdr arg)))]
              [(vector? arg)
               (list->vector (map repify (vector->list arg)))]
              [else arg]))))
      (thread-critical-section
        (apply fprintf (swl:bug-port) (repify args))))))

(define crh (collect-request-handler))

(define bad-guys
  (lambda ()
    (let loop ([ls (oblist)])
      (cond
        [(null? ls) '()]
        [(swl:lookup-widget (car ls)) =>
         (lambda (w)
           (cons w (loop (cdr ls))))]
        [else (loop (cdr ls))]))))

