;; This file is intended to help with Makefile.vc under Windows NT.
;; It converts \ to / in filenames for the benefit of Scheme I/O primitives.
;;
;; Unix:   cat src ... > dest
;; NT:     echo replace dest src ... | $(Scheme) ..\..\nt\cat.ss
;; 
;; Unix:   cat src ... >> dest
;; NT:     echo append dest src ... | $(Scheme) ..\..\nt\cat.ss

(optimize-level 3)
(define cat-to
  (lambda (mode dest srcs)
    (define buf-size 4096)
(printf "cat-to: ~s ~s ~s~n" mode dest srcs)
    (let ([op (open-output-file dest mode)]
          [buf (make-string buf-size)])
      (for-each
        (lambda (src)
          (call-with-input-file src
            (lambda (ip)
              (let lp ()
                (let ([n (block-read ip buf buf-size)])
                  (unless (eof-object? n) (block-write op buf n) (lp)))))))
        srcs)
      (close-output-port op))))

(define read-filename
  (lambda ()
    (let eat-whitespace ()
      (let ([c (peek-char)])
        (unless (eof-object? c)
          (when (char-whitespace? c) (read-char) (eat-whitespace)))))
    (let ([op (open-output-string)])
      (let lp ()
        (let ([x (read-char)])
          (case x
            [(#\space #\tab #\newline #!eof) (get-output-string op)]
            [(#\\) (write-char #\/ op) (lp)]
            [else (write-char x op) (lp)]))))))

(let ([mode (read)])
  (unless (memq mode '(replace append))
    (assertion-violationf 'cat.ss
      "invalid mode ~s.  Valid modes are replace or append"
      mode))
  (let ([dest (read-filename)])
    (let lp ([srcs '()])
      (let ([src (read-filename)])
        (if (fxzero? (string-length src))
            (cat-to mode dest (reverse srcs))
            (lp (cons src srcs)))))))

