;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; handy for output ports, no newline tacked on, could beef up to insert
;; whitespace between things for us, like scheme->tcl-concat does currently

;; actually doesn't seem to be used (much) any more

(define swl:display*
  (lambda (op . args)
    (for-each (lambda (x) (display x op)) args)))

;;; governs tracing of calls to swl:tcl-eval, which can be really handy, but
;;; can also be quite noisy.


(define swl:bug-port
  (make-parameter (current-output-port)
    (lambda (x)
      (if (output-port? x)
          x
          (assertion-violationf 'swl:bug-port "invalid port ~a" x)))))

(define swl:bug (make-parameter #f (lambda (x) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tcl <--> Scheme conversion

;; convert from (Tcl) string to Scheme object.
;; for now we have the convention that
;;   (swl:tcl->scheme "123")   ==>  123
;;   (swl:tcl->scheme "1 2 3") ==>  (1 2 3)
;; maybe this should also do things like look up widgets...

(define swl:tcl->scheme
  (lambda (str)
    (let ((ip (open-input-string str)))
      (let* ((first (read ip))
             (next  (read ip)))
        (if (eof-object? next)
            (if (eof-object? first) #f first)
            (cons first
                  (cons next
                        (let loop ((x (read ip)))
                           (if (eof-object? x)
                               '()
                               (cons x (loop (read ip))))))))))))

;; args can be symbols and characters (for Tk tokens)
;; as well as instances of various classes with a scheme->tcl method
;;
;; To avoid gonzo allocation, share the buffer from the string port
;; between foreign-procedure calls
;;
;; Need critical section here (at least if threaded) due
;; to shared state (buffer).  We could probably make the tcl-output-port
;; a thread parameter and play some games there to avoid critical section.
;;
;; Since the interface between Scheme and Tk is string based, it made
;; sense to avoid copying and optimize it even in a gnarly way.

(define swl:tcl-eval
  (let ()

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; (display-tcl-safe-string sting output-port)
     ;;
     ;; Displays the given string to the output-port escaping any Tcl
     ;; special chars along the way.
     
     (define display-tcl-safe-string
       (let ((needs-escape? (lambda (ch) (memv ch '(#\" #\[ #\] #\$ #\\)))))
         (lambda (string output-port)
           (display #\" output-port)
           (let loop ((i 0) (len (string-length string)))
             (unless (fx= i len)
               (let ((ch (string-ref string i)))
                 (unless (eq? ch #\nul)
                   (when (needs-escape? ch) (display #\\ output-port))
                   (display ch output-port))
                 (loop (fx+ i 1) len))))
           (display #\" output-port))))

     (define display-tcl-safe-symbol
       (lambda (symbol op)
         (let ([s (symbol->string symbol)])
           (let loop ([i 0] [len (string-length s)])
             (unless (fx= i len)
               (let ([ch (string-ref s i)])
                 (when (memq ch '(#\space #\tab #\newline #\" #\[ #\] #\$ #\\))
                   (write-char #\\ op))
                 (write-char ch op))
               (loop (fx+ i 1) len))))))

      ;;  Elsewhere scheme->tcl is a method but unfortunately methods can
      ;;  only function on instances, and much of what we need to run through
      ;;  scheme->tcl are not instances.
      ;;
      ;;  generic-scheme->tcl
      ;;    symbols, characters, and fix/float numbers are displayed
      ;;    strings are tcl-quoted
      ;;    booleans are displayed as 1 or 0
      ;;    instances are displayed by their scheme->tcl methods
      ;;    pairs
      ;;      (x-1 . fixnum) is displayed as a text index:  x.fixnum
      ;;      else displayed roughly as a tcl list (no improper lists)
      ;;    () is displayed as tcl empty list {}

      ; should profile to sort clauses better
 
     (define generic-scheme->tcl
       (lambda (x op)
         (define-syntax text-index?
           (syntax-rules () [(_ x) (fixnum? (cdr x))]))
         (cond
           ((symbol? x) (display-tcl-safe-symbol x op))
           ((or (fixnum? x) (flonum? x)) (display x op))
           ((string? x) (display-tcl-safe-string x op))
           ((pair? x)
            (if (text-index? x)
                ;; adjust for Tk's 1-based lines
                (begin (display (fx+ (cdr x) 1) op)
                       (write-char #\. op)
                       (display (car x) op))
                (begin
                  ;; code seems to work, but didn't think hard about it
                  ;; (sort of makes Tcl list)
                  (display "{" op)
                  (let loop ((x x))
                    (cond
                      ((null? x) (display "}" op))
                      (else
                        (generic-scheme->tcl (car x) op)
                        (cond
                      ((null? (cdr x)) (display "}" op))
                      ;; tcl might not like dotted notation
                      (else (display " " op) (loop (cdr x))))))))))
           ((instance? x) (send x scheme->tcl op))
           ((boolean? x) (write-char (if x #\1 #\0) op))
           ((char? x) (write-char x op))
           ((null? x) (display "{}" op))
           ((number? x) (display (exact->inexact x) op))
           (else (assertion-violationf 'scheme->tcl "don't know what to do with ~a" x)))))

    (define noncopying-scheme->tcl-concat
      (let ([op (open-output-string)])
        (lambda (ls delim)
          (if (null? ls)
              ""
              (begin (set-port-output-index! op 0)
                     (let loop ([ls ls] [op op])
                       (cond
                         [(null? (cdr ls))
                          (generic-scheme->tcl (car ls) op)
                          (write-char #\nul op)
                          (port-output-buffer op)]
                         [else
                          (generic-scheme->tcl (car ls) op)
                          (display delim op)
                          (loop (cdr ls) op)])))))))
    (lambda args
      (critical-section
        (parameterize ((print-radix 10))
          (let ([str (noncopying-scheme->tcl-concat args #\space)])
            (when (swl:bug) (fprintf (swl:bug-port) "swl:tcl-eval: ")
              (let loop ((i 0) (len (string-length str)) (bp (swl:bug-port)))
                (unless (fx= i len)
                  (let ((ch (string-ref str i)))
                    (if (char=? ch #\nul)
                        (newline bp)
                        (begin
                          (write-char ch bp)
                          (loop (fx+ i 1) len bp)))))))
            (let ([x (swl:raw-tcl-eval str)])
              (when (swl:bug) (fprintf (swl:bug-port) "returned: ~s~n" x))
              x)))))))

