;; This is intended to help with Makefile.vc under Windows NT.
;; It translates \ to / for benefit of Scheme I/O primitives.

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

(let lp ([files '()])
  (let ([src (read-filename)])
    (if (fxzero? (string-length src))
        (for-each delete-file files)
        (lp (cons src files)))))

