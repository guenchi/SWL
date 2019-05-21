;; Copyright (c) 1996 Carl Bruggeman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; NOTE:  temporarily unsupported.

(define nb-process
  (lambda (str)
    (let* ([proc (process str)]
           [ip (open-nb-input-port
                 (port-fd (car proc))
                 (car proc) ; use ip as name so collector doesn't close ip
                 (make-string 1024))]
           [op (open-nb-output-port
                 (port-fd (cadr proc))
                 (cadr proc) ; use op as name so collector doesn't close op
                 (make-string 1024))])
      (list ip op (caddr proc)))))

