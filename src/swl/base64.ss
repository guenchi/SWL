; Not optimized beyond adding a few defines for fixnum operators.

(module swl:base64 (swl:base64-encode)
 ; see http://www.freesoft.org/CIE/RFC/1521/7.htm

  (define pad #\=)
  (define line-max 76)

  (define-syntax >= (identifier-syntax fx>=))
  (define-syntax > (identifier-syntax fx>))
  (define-syntax = (identifier-syntax fx=))
  (define-syntax + (identifier-syntax fx+))
  (define-syntax - (identifier-syntax fx-))

  (define b64table
    (let ()
      (define (next-char c)
        (integer->char (+ 1 (char->integer c))))
      (define expand-range
        (lambda (s)
          (let ([ls (string->list s)])
            (if (null? (cdr ls))
                ls
                (let loop ([start (car ls)] [end (cadr ls)])
                  (if (char=? start end)
                      (list end)
                      (cons start (loop (next-char start) end))))))))
      (list->string
        (apply append
          (map expand-range '("AZ" "az" "09" "+" "/"))))))

 ; read three characters
 ; write as four characters
 ; pad at end with one or two pad characters depending on whether
 ;   we were left with 24, 16, or 8 bits to encode at the end.

  (define swl:base64-encode
    (lambda (s op)
      (let ([len (string-length s)])
        (define get-bits
          (lambda (i)
            (fxlogor (get-char i 16)
              (fxlogor (get-char (+ i 1) 8) (get-char (+ i 2) 0)))))
        (define get-char
          (lambda (i shift)
            (if (>= i len)
                0
                (fxsll (char->integer (string-ref s i)) shift))))
        (define write-encoded
          (lambda (bits)
            (put-char bits 3/4)
            (put-char bits 2/4)
            (put-char bits 1/4)
            (put-char bits 0/4)))
        (define write-padded
          (lambda (bits n)
            (put-char bits 3/4)
            (put-char bits 2/4)
            (if (= n 1)
                (put-char bits 1/4)
                (write-char pad op))
            (write-char pad op)))
        (define put-char
          (lambda (bits shift)
            (write-char
              (string-ref b64table (fxlogand (fxsra bits (* 24 shift)) #x3F))
              op)))
        (let loop ([i 0] [n 4])
          (let ([next (+ i 3)])
            (cond
              [(= next len) (write-encoded (get-bits i))]
              [(> next len) (write-padded (get-bits i) (- next len))]
              [else
               (write-encoded (get-bits i))
               (if (= n line-max)
                   (begin (newline op) (loop next 4))
                   (loop next (+ n 4)))]))))))

)

(eval-when (compile) (import swl:base64))

