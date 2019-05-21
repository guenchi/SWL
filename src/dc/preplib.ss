;; eventually, it would be nice if the entire state machine were here;
;; i.e., if all parsing were done by preplib, with prep.ss and hprep.ss
;; providing output generation procedures

(define-syntax state-case
  (lambda (x)
    (syntax-case x ()
      ((_ (var exp) c1 c2 ...)
       (identifier? (syntax var))
       (syntax (let ((var exp)) (state-case-help var c1 c2 ...)))))))

(define-syntax state-case-help
  (lambda (x)
    (syntax-case x (else)
      ((_ var (else e1 e2 ...))
       (syntax (begin e1 e2 ...)))
      ((_ var ((k ...) e1 e2 ...) c ...)
       (syntax (if (or (state-case-test var k) ...)
                   (begin e1 e2 ...)
                   (state-case-help var c ...)))))))

(define-syntax state-case-test
  (lambda (x)
    (syntax-case x (eof -)
      ((_ var eof)
       (syntax (eof-object? var)))
      ((_ var (char1 - char2))
       (syntax (and (char? var) (char<=? char1 var char2))))
      ((_ var char)
       (syntax (and (char? var) (char=? var char)))))))

(define read-alpha-command
  ; return symbol representing command; assume \ already seen and scan
  ; maximal string of alphabetic chars, e.g., \scheme => symbol scheme
  ; returns || when no command is recognized
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (peek-char ip))
        [((#\a - #\z) (#\A - #\Z))
         (let loop ()
           (write-char (read-char ip) buf)
           (state-case (c (peek-char ip))
             [((#\a - #\z) (#\A - #\Z)) (loop)]
             [else (string->symbol (get-output-string buf))]))]
        [else '||]))))

(define read-command
  ; like read-alpha-command, but allows single nonalphabetic char
  ; commands, e.g., \' => |'|
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (peek-char ip))
        [((#\a - #\z) (#\A - #\Z))
         (let loop ()
           (write-char (read-char ip) buf)
           (state-case (c (peek-char ip))
             [((#\a - #\z) (#\A - #\Z)) (loop)]
             [else (string->symbol (get-output-string buf))]))]
        [(eof) '||]
        [else (read-char ip) (string->symbol (string c))]))))

(define read-open-brace
  (lambda (ip)
    (if (eqv? (peek-char ip) #\{)
        (read-char ip)
        (input-error "open brace expected"))))

(define read-bracketed-text
  (let ((buf (open-output-string)))
    (case-lambda
      [(ip) (read-open-brace ip) (read-bracketed-text ip 1)]
      [(ip depth)
       (state-case (c (read-char ip))
         [(#\}) (if (= depth 1)
                    (get-output-string buf)
                    (begin (write-char #\} buf)
                           (read-bracketed-text ip (- depth 1))))]
         [(#\{) (write-char #\{ buf) (read-bracketed-text ip (+ depth 1))]
         [(eof) (input-error "file ended within bracketed text")]
         [else (write-char c buf) (read-bracketed-text ip depth)])])))

(define read-optional-arg
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (peek-char ip))
        [(#\[)
         (read-char ip)
         (let loop ((depth 0))
           (state-case (c (read-char ip))
             [(#\]) (if (= depth 0)
                        (get-output-string buf)
                        (begin (write-char c buf) (loop depth)))]
             [(#\{) (write-char c buf) (loop (+ depth 1))]
             [(#\}) (write-char c buf) (loop (- depth 1))]
             [(eof) (assertion-violationf #f "file ended within optional argument")]
             [else (write-char c buf) (loop depth)]))]
        [else #f]))))

(define input-error
  (lambda (msg . args)
    (let ((ip current-ifile))
      (assertion-violationf #f "~a at character ~d of ~s"
        (apply format msg args)
        (file-position ip)
        (port-name ip)))))

(define unexpected-command
  (lambda (cmd)
    (input-error "unexpected command '\~a'" cmd)))

(define math-file-name
  (let ((seq -1))
    (lambda ()
      (set! seq (+ seq 1))
      (format "math/~d" seq))))

(define name->label
  (call-with-values
    (lambda ()
      (let ((s (make-string 256 #\nul)))
        (define fill!
          (lambda (i ls)
            (if (null? ls)
                (values i s)
                (begin
                  (string-set! s (char->integer (car ls)) (integer->char i))
                  (fill! (+ i 1) (cdr ls))))))
        (fill! 0 '(
           #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\.  #\/ #\:
           #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~
           #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
           #\A #\a #\B #\b #\C #\c #\D #\d #\E #\e #\F #\f
           #\G #\g #\H #\h #\I #\i #\J #\j #\K #\k #\L #\l
           #\M #\m #\N #\n #\O #\o #\P #\p #\Q #\q #\R #\r
           #\S #\s #\T #\t #\U #\u #\V #\v #\W #\w #\X #\x
           #\Y #\y #\Z #\z))))
    (lambda (r t)
      (lambda (s)
        (do ((n (string-length s))
             (i 0 (+ i 1))
             (a 0 (+ (* a r)
                     (char->integer
                       (string-ref t
                         (char->integer (string-ref s i)))))))
            ((= i n) (string->symbol (number->string a 36))))))))

(define next-index-uid
  (lambda (ip)
    (define next-count
      (lambda (fn)
        (cond
          [(assoc fn index-uid-counters) =>
           (lambda (a)
             (let ((n (+ (cdr a) 1)))
               (set-cdr! a n)
               n))]
          [else
           (set! index-uid-counters
             (cons (cons fn 0) index-uid-counters))
           0])))
    (format "~a~d" (port-name ip) (next-count (port-name ip)))))

(define read-integer ; return integer or #f if none found
  (lambda (ip)
    (string->number
      (list->string
        (let loop ()
          (state-case (c (peek-char ip))
            [((#\0 - #\9)) (read-char ip) (cons c (loop))]
            [else '()]))))))

(define read-def-pattern
  (lambda (ip)
    (let loop ((i 1))
      (state-case (c (peek-char ip))
        [(#\{) '()]
        [(#\#)
         (read-char ip)
         (let ((n (read-integer ip)))
           (if (eq? n i)
               (cons n (loop (+ i 1)))
               (input-error "invalid \\def argument specifier")))]
        [(eof) (assertion-violationf #f "unexpected eof after \\def")]
        [else (read-char ip) (cons c (loop i))]))))

(define read-arg
  (lambda (ip cmd)
    (state-case (c (read-char ip))
      [(#\\) (format "\\~a" (read-command ip))]
      [(#\{) (read-bracketed-text ip 1)]
      [(eof) (assertion-violationf #f "unexpected eof reading ~a arguments" cmd)]
      [else (string c)])))

(define read-args
  (lambda (ip pattern cmd)
    (let loop ((pattern pattern))
      (if (null? pattern)
          '()
          (let ((x (car pattern)))
            (cond
              [(integer? x)
               (let ((arg (read-arg ip cmd)))
                 (cons arg (loop (cdr pattern))))]
              [(eqv? x #\space)
               (suppress-white-space ip)
               (loop (cdr pattern))]
              [(eqv? (read-char ip) x) (loop (cdr pattern))]
              [else (input-error "~a use does not match pattern" cmd)]))))))

(define expand-template
  (lambda (op template args cmd)
    (let ((sip (open-input-string template)))
      (let loop ()
        (state-case (c (read-char sip))
          [(#\\)
           (write-char c op)
           (state-case (c (peek-char sip))
             [(#\#) (read-char sip) (write-char c op)]
             [else (void)])
           (loop)]
          [(#\#)
           (state-case (c (peek-char sip))
             [(#\#) (read-char sip) (write-char #\# op)]
             [else (let ((n (read-integer sip)))
                     (let ((n (and n (- n 1))))
                       (unless (and n (< n (length args)))
                         (assertion-violationf #f "invalid argument specifier in ~a template"
                           cmd))
                       (display (list-ref args n) op)))])
           (loop)]
          [(eof) (void)]
          [else (write-char c op) (loop)])))))

(define (suppress-white-space ip)
  (state-case (c (peek-char ip))
    [(#\space #\tab #\newline) (read-char ip) (suppress-white-space ip)]
    [else (void)]))

(define mathcmds
  '(|{| |}| |,| |;| |Rightarrow| |W| bigl bigr cr dots infty
    le lfloor log neq over pi pm pmatrix rfloor rm sigma sqrt sum times))

;; shared states for parsing index entries
;;; s4.1 & s4.2 should use local buf's
(define s4.1
  (lambda (ip op depth ls)
  ; depth is number of open braces seen
  ; ls is list of levels seen so far
    (state-case (c (read-char ip))
      [(#\})
       (if (= depth 1)
           (let ((s (get-output-string op)))
             (values (reverse (cons (cons #f s) ls)) ""))
           (begin (write-char #\} op)
                  (s4.1 ip op (- depth 1) ls)))]
      [(#\{) (write-char #\{ op) (s4.1 ip op (+ depth 1) ls)]
      [(#\|)
       (if (= depth 1)
           (let ((s (get-output-string op)))
             (values (reverse (cons (cons #f s) ls))
                     (read-bracketed-text ip 1)))
           (assertion-violationf #f "| seen in \\index{} nested within {}"))]
      [(#\@)
       (if (= depth 1)
           (s4.2 ip op 1 ls (get-output-string op))
           (assertion-violationf #f "@ seen in \\index{} nested within {}"))]
      [(#\!)
       (if (= depth 1)
           (let ((s (get-output-string op)))
             (s4.1 ip op 1 (cons (cons #f s) ls)))
           (assertion-violationf #f "! seen in \\index{} nested within {}"))]
      [(#\")
       (state-case (c (read-char ip))
         ((eof) (assertion-violationf #f "file ended within \\index{}"))
         (else
          ; leave out quote; reinsert later
          (write-char c op)
          (s4.1 ip op depth ls)))]
      [(#\\)
       (let ((cmd (read-command ip)))
         (case cmd
           [(emph scheme)
            (fprintf op "\\~a{~a}" cmd (read-bracketed-text ip))
            (s4.1 ip op depth ls)]
           [else (unexpected-command cmd)]))]
      [(eof) (assertion-violationf #f "file ended within \\index{}")]
      [else (write-char c op) (s4.1 ip op depth ls)])))

(define s4.2
  (lambda (ip op depth ls sort-key)
  ; depth is number of open braces seen
  ; ls is list of levels seen so far
  ; sort-key is sort key part of current level
    (state-case (c (read-char ip))
      [(#\})
       (if (= depth 1)
           (let ((s (get-output-string op)))
             (values (reverse (cons (cons sort-key s) ls)) ""))
           (begin (write-char #\} op)
                  (s4.2 ip op (- depth 1) ls sort-key)))]
      [(#\{) (write-char #\{ op) (s4.2 ip op (+ depth 1) ls sort-key)]
      [(#\|)
       (if (= depth 1)
           (let ((s (get-output-string op)))
             (values (reverse (cons (cons sort-key s) ls))
                     (read-bracketed-text ip 1)))
           (assertion-violationf #f "| seen in \\index{} nested within {}"))]
      [(#\@) (assertion-violationf #f "at sign seen after at sign in \\index{}")]
      [(#\!)
       (if (= depth 1)
           (let ((s (get-output-string op)))
             (s4.1 ip op 1 (cons (cons sort-key s) ls)))
           (assertion-violationf #f "! seen in \\index{} nested within {}"))]
      [(#\")
       (state-case (c (read-char ip))
         ((eof) (assertion-violationf #f "file ended within \\index{}"))
         (else
          ; leave out quote; reinsert later
          (write-char c op)
          (s4.2 ip op depth ls sort-key)))]
      [(#\\)
       (let ((cmd (read-command ip)))
         (case cmd
           [(emph scheme)
            (fprintf op "\\~a{~a}" cmd (read-bracketed-text ip))
            (s4.2 ip op depth ls sort-key)]
           [else (unexpected-command cmd)]))]
      [(eof) (assertion-violationf #f "file ended within \\index{}")]
      [else (write-char c op) (s4.2 ip op depth ls sort-key)])))

(define strip-sort-key
  ; presently strips only \scheme{ and matching }
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (read-char ip))
        [(#\\)
         (let ((cmd (read-command ip)))
           (case cmd
             [(scheme)
              (read-open-brace ip)
              (display (read-bracketed-text ip 1) buf)
              (strip-sort-key ip)]
             [else (unexpected-command cmd)]))]
        ((eof) (get-output-string buf))
        (else (write-char c buf) (strip-sort-key ip))))))

(define base-macros-file "preplib.stex")

;; support for definitions
(define-syntax global-def
  (syntax-rules ()
    ((_ name expr)
     (set-def! 'name '() #f expr))))

(define set-def!
  (lambda (cmd env conditional? proc)
    (if (null? env)
        (putprop cmd 'def (cons conditional? proc))
        (set-car! env (cons (list* cmd conditional? proc) (car env))))))

(define lookup-env
  ; local to get-def and conditional?
  (lambda (cmd env)
    (cond
      [(null? env) (getprop cmd 'def '(#f . #f))]
      [(assq cmd (car env)) => cdr]
      [else (lookup-env cmd (cdr env))])))

(define get-def
  (lambda (cmd env)
    (cdr (lookup-env cmd env))))

(define conditional?
  (lambda (cmd env)
    (car (lookup-env cmd env))))

;; support for counters
(define get-counter-value
  (lambda (count)
    (getprop count 'counter #f)))

(define set-counter-value!
  (lambda (count value)
    (putprop count 'counter value)))

(define add-subcounter!
  (lambda (counter subcounter)
    (putprop counter 'subcounters
      (cons subcounter (getprop counter 'subcounters '())))))

(define subcounters
  (lambda (counter)
    (getprop counter 'subcounters '())))

(define scan-if
  (let ((buf (open-output-string)))
    (lambda (ip def-env)
      (let loop ((depth 0) (then-part #f))
        (state-case (c (read-char ip))
          [(#\\)
           (let ((cmd (read-command ip)))
             (cond
               [(conditional? cmd def-env)
                (fprintf buf "\\~a" cmd)
                (loop (+ depth 1) then-part)]
               [(and (= depth 0) (eq? cmd 'else))
                (if then-part
                    (input-error "extra \\else found")
                    (loop depth (get-output-string buf)))]
               [(eq? cmd 'fi)
                (if (= depth 0)
                    (if then-part
                        (values then-part (get-output-string buf))
                        (values (get-output-string buf) ""))
                    (begin
                      (fprintf buf "\\~a" cmd)
                      (loop (- depth 1) then-part)))]
               [else
                (fprintf buf "\\~a" cmd)
                (loop depth then-part)]))]
          [(#\{)
           (fprintf buf "{~a}" (read-bracketed-text ip 1))
           (loop depth then-part)]
          [(#\}) (input-error "unmatched } within \if ... \fi")]
          [(eof) (input-error
                   "unexpected end-of-file within \if ... \fi")]
          [else (write-char c buf) (loop depth then-part)])))))

(define new-conditional
  (lambda (ifcmd def-env default cmdtrue cmdfalse)
    (let ((cell (cons default (void))))
      (set-def! ifcmd def-env #t
        (P lambda ()
          (call-with-values
            (lambda () (scan-if ip def-env))
            (lambda (then-part else-part)
              (let ((part (if (car cell) then-part else-part)))
                (P s0 ([ip (open-input-string part)] [ips (cons ip ips)])))))))
      (when cmdtrue
        (set-def! cmdtrue def-env #f
          (P lambda ()
            (set-car! cell #t)
            (P s0))))
      (when cmdfalse
        (set-def! cmdfalse def-env #f
          (P lambda ()
            (set-car! cell #f)
            (P s0)))))))

(define snewif
  (lambda (ip def-env)
    (state-case (c (read-char ip))
      [(#\\)
       (let ((ifcmd (read-command ip)))
         (let ((ifcmd-str (symbol->string ifcmd)))
           (unless (and (> (string-length ifcmd-str) 2)
                        (string=? (substring ifcmd-str 0 2) "if"))
             (input-error "invalid conditional name ~a" ifcmd))
           (let ((cmd (substring ifcmd-str 2 (string-length ifcmd-str))))
             (new-conditional ifcmd def-env #f
               (string->symbol (string-append cmd "true"))
               (string->symbol (string-append cmd "false"))))))]
      [else (input-error "unexpected character following \\newif")])))
