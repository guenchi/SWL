;;; scheme-prep.ss
;;; Copyright (c) 1998 Cadence Research Systems

;;; authors: R. Kent Dybvig and Oscar Waddell

;;; TODO
;;; - don't add " and \ escape chars within \index {}
;;; - handle undelimited " and \ escape characters within index entries

;;; primitive commands

;;; \endschemedisplay
;;; \genlab
;;; \hindex
;;; \index
;;; \raw
;;; \scheme
;;; \schemedisplay
;;; \schlbrace
;;; \schrbrace
;;; \var
;;; \xdef
;;; \xedef

;;; \schemeoutput[mode]{filename} ; hack that diverts schemedisplay code to file
;;; \schemeoutput{}               ; hack that closes the open scheme output file

;;; commands inserted into the output

;;; \$, \&, \%, \#, \{, \}, \\
;;; \dots
;;; \endschemedisplay
;;; \hindex
;;; \is
;;; \label
;;; \scheme
;;; \schemeblankline
;;; \schemedisplay
;;; \schemelinestart      ; relies on defn of \schemeindent=
;;; \schatsign
;;; \schbackslash
;;; \schcarat
;;; \schdot
;;; \schtilde
;;; \schunderscore
;;; \si
;;; \var
;;; \vdots
;;; any undefined input command
;;; anything else inserted by \raw

;;; Valid within \scheme{...}
;;;   \dots
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \
;;;   all other characters besides } and newline

;;; Valid within \schemedisplay ... \endschemedisplay
;;;   \dots
;;;   \raw{<text>}
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \vdots
;;;   \
;;;   all other characters besides }
;;;   \label{text} may appear immediately after \schemedisplay

;;;   index and hindex entries follow makeindex 2.13 syntax except that
;;;   the special characters !, @, |, ", and \ are freely allowed within
;;;   embeded {, } pairs in the "actual entry" portion of a level (the
;;;   portion following @, if any).  a modified version of makeindex 2.13
;;;   that supports this extension (with the -d [allow delimited special
;;;   chars] flag) is available here as well.
;;;
;;;   in \index input, we also allow \scheme{...} to appear, with embedded
;;;   special characters.

;;; \index syntax
;;;   \index{levels}
;;;   \index{levels|pageformat}
;;;   levels --> level
;;;          --> level!levels
;;;   level  --> keyentry                     (key and entry same)
;;;   level  --> key@entry                    (separate key and entry)
;;;
;;;   Valid within keyentry
;;;     \scheme{...}

(eval-when (compile eval) (case-sensitive #t))

(let ()
(include "dsm.ss")
(let ()
(include "preplib.ss")

(define insert-file
  (lambda (fn op)
    (let ((ip (open-input-file fn)))
      (let loop ()
        (let ((c (read-char ip)))
          (unless (eof-object? c)
            (write-char c op)
            (loop)))))))

(define copy-through-newline
  (lambda (ip op)
    (state-case (c (read-char ip))
      [(#\newline) (write-char c op)]
      [(eof) (void)]
      [else (write-char c op) (copy-through-newline ip op)])))

(define dovar
  (lambda (ip op)
    (read-open-brace ip)
    (display "\\var{" op)
    (let f ()
      (state-case (c (read-char ip))
        [(#\}) (write-char c op)]
        [(#\_)
         (display "$_" op)
         (state-case (c (read-char ip))
           [(eof) (input-error "unexpected eof in \\var{}")]
           [(#\{) (fprintf op "{~a}" (read-bracketed-text ip 1))]
           [else (write-char c op)])
         (write-char #\$ op)
         (f)]
        [(eof) (input-error "unexpected eof in \\var{}")]
        [else (write-char c op) (f)]))))
 
(define (sscheme ip op) ; within \scheme
  ; unlike schemedisplay, does not allow { or }.  { and } must be expressed
  ; as \schlbrace and \schrbrace
  (state-case (c (read-char ip))
    [(#\{) (input-error "unexpected { within \\scheme{}")]
    [(#\}) (void)]
    [(#\\)
     ; use read-alpha-command instead of read-alpha-command to avoid
     ; improper handling of special characters that follow a shash,
     ; including }
     (let ((cmd (read-alpha-command ip)))
       (case cmd
         [(dots)
          (display "{\\dots}" op)
          (sscheme ip op)]
         [(var)
          (dovar ip op)
          (sscheme ip op)]
         [(schlbrace)
          (display "\\{" op)
          (sscheme ip op)]
         [(schrbrace)
          (display "\\}" op)
          (sscheme ip op)]
         [else ; assume random \ possibly followed by alphabetic chars
          (fprintf op "{\\schbackslash}~a" cmd)
          (sscheme ip op)]))]
    [(#\.) (display "{\\schdot}" op) (sscheme ip op)]
    [(#\~) (display "{\\schtilde}" op) (sscheme ip op)]
    [(#\^) (display "{\\schcarat}" op) (sscheme ip op)]
    [(#\@) (display "{\\schatsign}" op) (sscheme ip op)]
    [(#\_) (display "{\\schunderscore}" op) (sscheme ip op)]
    [(#\$ #\& #\% #\#) (fprintf op "\\~c" c) (sscheme ip op)]
    [(#\newline) (input-error "line ended within \\scheme{}")]
    [(eof) (input-error "file ended within \\scheme{}")]
    [else (write-char c op) (sscheme ip op)]))

(define sschemedisplay
  (P lambda () ; within a schemedisplay
    ; n? is true if we've just passed a new line within the display; we use
    ;   it to determine whether to insert \\\schemelinestart.  (We insert
    ;   \schemelinestart after \\ to prevent \\ from sucking up ensuing
    ;   whitespace---including spaces used for indentation on the following
    ;   line.)
    (fprintf op "\\schemelinestart~%")
    (when sout (newline sout))
    (let loop ((n? #f))
      (state-case (c (read-char ip))
        [(#\\)
         ; use read-alpha-command instead of read-alpha-command to avoid
         ; improper handling of special characters that follow a shash
         (let ((cmd (read-alpha-command ip)))
           (case cmd
             [(endschemedisplay) (fprintf op "\\endschemedisplay")]
             [else
              (when n? (fprintf op "\\schemelinestart~%"))
              (case cmd
                [(dots)
                 (display "{\\dots}" op)
                 (loop #f)]
                [(var)
                 (dovar ip op)
                 (loop #f)]
                [(raw)
                 (fprintf op "\\raw{~a}" (read-bracketed-text ip))
                 (loop #f)]
                [(vdots)
                 (display "{\\vdots}" op)
                 (loop #f)]
                [else ; assume random \ possibly followed by alphabetic chars
                 (fprintf op "{\\schbackslash}~a" cmd)
                 (when sout (fprintf sout "\\~a" cmd))
                 (loop #f)])]))]
        [else
         (when n? (fprintf op "\\schemelinestart~%"))
         (when sout (write-char c sout))
         (state-case (c c)
           [(#\;)
           ; convert ;=> into \is and ;== into \si
            (state-case (c (peek-char ip))
              [(#\=)
               (read-char ip)
               (when sout (write-char c sout))
               (state-case (c (peek-char ip))
                 ((#\>)
                  (when sout (write-char c sout))
                  (read-char ip)
                  (display "\\is" op)
                  (loop #f))
                 ((#\=)
                  (when sout (write-char c sout))
                  (read-char ip)
                  (display "\\si" op)
                  (loop #f))
                 (else
                  (when sout (write-char c sout))
                  (display ";=" op)
                  (loop #f)))]
              [else
               (write-char #\; op)
               (loop #f)])]
           [(#\.)
            (display "{\\schdot}" op)
            (loop #f)]
           [(#\~)
            (display "{\\schtilde}" op)
            (loop #f)]
           [(#\^)
            (display "{\\schcarat}" op)
            (loop #f)]
           [(#\@)
            (display "{\\schatsign}" op)
            (loop #f)]
           [(#\_)
            (display "{\\schunderscore}" op)
            (loop #f)]
           [(#\$ #\& #\% #\# #\{ #\})
            (fprintf op "\\~c" c)
            (loop #f)]
           [(#\newline)
            (state-case (c (peek-char ip))
              ((#\newline)
               (read-char ip)
               (fprintf op "~%\\schemeblankline~%")
               (when sout (newline sout))
               (loop #t))
              (else
               (fprintf op "\\\\~%")
               (loop #t)))]
           [(eof) (assertion-violationf #f "file ended within schemedisplay")]
           [else (write-char c op) (loop #f)])]))))

(define (sindex ip op)
  ; 1. read entire contents of \index{} form, w/o intepreting \scheme{...}
  ;    separate at !s into 1 or more levels plus page format
  ;    after |, if present; separate levels at @ into sort key
  ;    and text, if present
  ; 2. for each level,
  ;    a. compute output sort key
  ;       - if input sort key is given, use it
  ;       - otherwise use stripped version of input text
  ;       - insert quotes where needed
  ;    b. preprocess input text to produce output text
  ;       - expand \scheme{...}
  ;       - insert quotes where needed
  ; 3. produce output
  ;    a. print \index{
  ;    b. for each level, if output text is same as output sort key,
  ;          print <output text>.
  ;       otherwise print <output text>@<output sort key>
  ;    c. separate levels with !
  ;    d. print |<page format> if present in input
  ;    e. print }
  (call-with-values
    (lambda () (parse-index ip #t))
    (lambda (levels page-format)
      (let ((keys (map (lambda (s) (insert-quotes (open-input-string s) #f))
                       (map (lambda (level)
                              (if (car level)
                                  (car level)
                                  ; strip text to create sort key
                                  (strip-sort-key
                                    (open-input-string (cdr level)))))
                            levels)))
            (texts (map (lambda (level)
                          (insert-quotes
                            (open-input-string
                              (expand-entry (open-input-string (cdr level))))
                          #f))
                        levels)))
        (let f ((keys keys) (texts texts) (delim #\{))
          (unless (null? keys)
            (write-char delim op)
            (let ((key (car keys)) (text (car texts)))
              (if (string=? key text)
                  (display text op)
                  (fprintf op "~a@~a" key text)))
            (f (cdr keys) (cdr texts) #\!)))
        (unless (string=? page-format "") (fprintf op "|~a" page-format))
        (write-char #\} op)))))

(define expand-entry
  ; expands \scheme{} forms
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (read-char ip))
        [(#\\)
         (let ((cmd (read-command ip)))
           (case cmd
             [(scheme)
              (read-open-brace ip)
              (display "\\scheme{" buf)
              (sscheme ip buf)
              (write-char #\} buf)
              (expand-entry ip)]
             [else
              (fprintf buf "\\~a" cmd)
              (expand-entry ip)]))]
        ((eof) (get-output-string buf))
        (else (write-char c buf) (expand-entry ip))))))

(define insert-quotes
  (let ((buf (open-output-string)))
   ; if proper-nesting? is true, the characters ", @, !, and | lose their
   ; special meaning within nested groups.
    (lambda (ip proper-nesting?)
      (let loop ()
        (state-case (c (read-char ip))
          [(#\" #\@ #\! #\|)
           (write-char #\" buf)
           (write-char c buf)
           (loop)]
          [(#\\)
           (state-case (c (peek-char ip))
             [(#\@ #\! #\|) (write-char #\" buf)]
             [else (void)])
           (write-char c buf)
           (loop)]
          [(#\{)
           (if proper-nesting?
               (fprintf buf "{~a}" (read-bracketed-text ip 1))
               (write-char c buf))
           (loop)]
          ((eof) (get-output-string buf))
          (else (write-char c buf) (loop)))))))

(define-syntactic-monad P
  ip                   ; current input port
  op                   ; current output port
  ips                  ; input port stack; does not include ip
  ops                  ; output port stack; does not include op
  ifiles               ; stack of input files [(cons ip ips) w/o string ports]
  eofconts             ; stack of continuations to call on eof
  sout                 ; output port for code within schemedisplay or #f
)
 
(define process-string
  (P lambda (s k)
    (P s0
      ([ip (open-input-string s)]
       [ips (cons ip ips)]
       [eofconts (cons (P lambda ()
                         (P k
                           ([op (car ops)] [ops (cdr ops)])
                           (get-output-string op)))
                       eofconts)]
       [op (open-output-string)]
       [ops (cons op ops)]))))

(define s0
  (P lambda ()
    (state-case (c (read-char ip))
      [(#\\)
       (let ((cmd (read-command ip)))
         (cond
           [(get-def cmd '()) =>
            (lambda (proc)
              (unless (command-symbol? cmd)
                (suppress-white-space ip))
              (P proc))]
           [else (fprintf op "\\~a" cmd) (P s0)]))]
      [(#\%)
       (write-char #\% op)
       (copy-through-newline ip op)
       (P s0)]
      [(eof)
       (close-input-port ip)
       (if (null? ips)
           (void)
           (P (car eofconts)
             ([ip (car ips)]
              [ips (cdr ips)]
              [ifiles (if (eq? ip (car ifiles)) (pop-ifile ifiles) ifiles)]
              [eofconts (cdr eofconts)])))]
      [else (write-char c op) (P s0)])))

;;; global constants
(define genlab-prefix "s")  ; unique string for this processor

;;; global variables
(define current-ifile) ; for the benefit of genlab & input-error
(define genlab-counters)

(set! go
  (lambda (fn)
    (let ((ip (open-input-file (format "~a.stex" fn)))
          (op (open-output-file (format "~a.tex" fn) 'truncate)))
      (fprintf op "%%% DO NOT EDIT THIS FILE~%")
      (fprintf op "%%% Edit the .stex version instead~%~%")
      (fluid-let ((current-ifile '())
                  (genlab-counters '()))
        (P s0
          ([ip ip]
           [op op]
           [ips '()]
           [ops '()]
           [ifiles (push-ifile ip '())]
           [eofconts (list s0)]
           [sout #f]))
        (close-output-port op)))))

(global-def genlab
  (P lambda ()
    (display (genlab) op)
    (P s0)))

(global-def hindex
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (lab)
        (fprintf op "\\hindex{~a}" lab)
        (read-open-brace ip)
        (sindex ip op)
        (P s0)))))

(global-def index
  (P lambda ()
   ; do the job of index-prep.ss
    (let ((lab (genlab)))
      (fprintf op "\\label{~a}\\hindex{~a}" lab lab))
    (read-open-brace ip)
    (sindex ip op)
    (P s0)))

(global-def raw
  (P lambda ()
    (display (read-bracketed-text ip) op)
    (P s0)))

; Better to handle includes in the Makefile.
;(global-def include
;  (P lambda ()
;    (let ((fn (read-bracketed-text ip)))
;      (display "\\include{" op)
;      (display fn op)
;      (printf "Processing include ~a~n" fn)
;      (display "}" op)
;      (go fn))
;    (P s0)))
;
; This isn't right anyway since I can't remember the right semantics.
;(global-def input
;  (P lambda ()
;    (let ((fn (read-bracketed-text ip)))
;      (display "\\input{" op)
;      (display fn op)
;      (printf "Processing input ~a~n" fn)
;      (display "}" op)
;      (go fn))
;    (P s0)))

(global-def scheme
  (P lambda ()
    (read-open-brace ip)
    (display "\\scheme{" op)
    (sscheme ip op)
    (write-char #\} op)
    (P s0)))

(global-def schemedisplay
  (P lambda ()
    (state-case (c (peek-char ip))
      [(#\\)
       (read-char ip)
       (let ((cmd (read-command ip)))
         (case cmd
           [(label)
            (fprintf op "\\schemedisplay\\label{~a}~%" (read-bracketed-text ip))
            (unless (eqv? (read-char ip) #\newline)
              (input-error "expected newline after \\schemedisplaylabel{...}"))]
           [(raw)
            (fprintf op "\\schemedisplay\\raw{~a}~%" (read-bracketed-text ip))]
           [else
            (input-error "unexpected \\~a following \\schemedisplay" cmd)]))]
      [else
       (fprintf op "\\schemedisplay~%")])
    (P sschemedisplay)
    (P s0)))

(global-def schemeoutput
  (P lambda ()
    (let* ([mode (if (equal? (read-optional-arg ip) "append") 'append 'truncate)]
           [filename (read-bracketed-text ip)])
       (when sout (close-output-port sout))
       (if (eq? filename "")
           (P s0 ([sout #f]))
           (P s0 ([sout (open-output-file filename mode)]))))))

(global-def var
  (P lambda ()
    (dovar ip op)
    (P s0)))

(global-def def
  (P lambda ()
    (let* ((cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\def syntax")]))
           (pattern (read-def-pattern ip))
           (template (read-bracketed-text ip)))
      (fprintf op "\\def\\~a" cmd)
      (for-each
        (lambda (x)
          (cond
            [(char? x) (write-char x op)]
            [(number? x) (fprintf op "#~a" x)]
            [else (assertion-violationf #f "clearly have no clue")]))
        pattern)
      (fprintf op "{~a}" template)
      (P s0))))

(global-def xdef
  (P lambda ()
    (let* ((cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\xdef syntax")]))
           (pattern (read-def-pattern ip))
           (template (read-bracketed-text ip)))
      (set-def! cmd '() #f
        (P lambda ()
          (P s0
            ([ip (open-input-string
                   (expand-template template
                     (read-args ip pattern cmd) cmd))]
             [ips (cons ip ips)]
             [eofconts (cons s0 eofconts)]))))
      (P s0))))

(global-def xedef
  (P lambda ()
    (let* ((cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\xedef syntax")]))
           (pattern (read-def-pattern ip))
           (template (read-bracketed-text ip)))
      (P process-string () template
        (P lambda (template)
          (set-def! cmd '() #f
            (P lambda ()
              (P s0
                ([ip (open-input-string
                       (expand-template template
                         (read-args ip pattern cmd) cmd))]
                 [ips (cons ip ips)]
                 [eofconts (cons s0 eofconts)]))))
          (P s0))))))
))
