;; Copyright (c) 1996 Carl Bruggeman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define www-open-URL
  (lambda (URL)
    (let ([addr (URL-addr url)] [port (URL-port url)] [file (URL-path url)])
      (case (URL-type url)
        [http (www-open-http addr port file (URL-query url))]
        [file (www-open-http-file file)]
        [else 'unsupported-access-type]))))

(define www-open-http
  (lambda (addr port file query)
(when (swl:bug) (fprintf (swl:bug-port) "www-open-http: ~s ~s ~s ~s~n" addr port file query))
    (let ([port (or port 80)]
          [file-string (if query (string-append file "?" query) file)])
      (let ([sock (swl:open-tcp-client addr port #f)])
        (fprintf sock "GET ~a~n" file-string)
        (flush-output-port sock)
        sock))))

'(define www-open-http
  (lambda (addr port file query)
    (let ([port (or port "80")]
          [file-string (if query (string-append file "?" query) file)])
      (fprintf (swl:bug-port) "host: ~a~%" addr)
      (fprintf (swl:bug-port) "port: ~a~%" port)
      (let ([fd (os-host-connect addr port)])
        (case fd
          [(-1) 'bad-port]
          [(-2) 'bad-host-address]
          [(-3) 'cannot-create-socket]
          [(-4) 'cannot-connect]
          [else (let ([netip (open-nb-input/output-port
                               (format "http://~a:~a~a" addr port file-string)
                               fd (make-string 4086)
                               fd (make-string 252))])
                  (fprintf (swl:bug-port) "GET ~a~%" file-string)
                  (fprintf netip "GET ~a~%" file-string)
                  (flush-output-port netip)
                  netip)])))))

(define www-open-http-file
  (lambda (file)
    (if (file-exists? file)
        (let ([ip (open-input-file file)])
          ; we  use ip as the name of the port to keep the collector from
          ; from finding that it is inaccessible and closing the fd. (Ack!)
'         (open-nb-input-port (port-fd ip) ip (make-string 4086))
ip)
        'file-not-found)))

