;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; Unused:
; (define swl:read-from-process
;   (lambda (string)
;     (let ([proc (process string)])
;       (let ([ip (car proc)])
; 	(let loop ([line (swl:read-line ip)])
; 	  (if (not line)
; 	    (begin (close-input-port (car proc))
; 	      (close-output-port (cadr proc))
; 	      '())
; 	    (cons line (loop (swl:read-line ip)))))))))

(define swl:read-line
  (let ([op (open-output-string)])
    (lambda (ip)
      (let loop ([x (read-char ip)])
	(cond
	  [(eof-object? x) #f]
	  [(char=? x #\newline) (get-output-string op)]
	  [else (write-char x op) (loop (read-char ip))])))))
