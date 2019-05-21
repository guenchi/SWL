;;; TODO
;;;  * check for only \emph and see(<text>) in index pageformat
;;;
;;;  * simplify where possible
;;;    - by inlining minor commands
;;;    - by specializing commands
;;;    - by expanding complex commands
;;;
;;;  * make sure changes to .stex source don't materially change .dvi file
;;;
;;;  * make html version of converter
;;;    - take latex .aux file as input for refs, pagerefs, and citations
;;;    - consider including pagerefs in html table of contents
;;;    - must be smarter about vskip, used in examples.stex
;;;    - consider two pass algorithm: one to do essentially what xprep.ss
;;;      does, although perhaps with fewer or different character expansions
;;;      in \scheme mode, and one to convert the resulting latex
;;;      code into html.
;;;    - adapt html.bst and bib2html conversions from bib2html distribution
;;;      - insert \citation, \bibstyle, and \bibdata cmds into .aux file
;;;      - use html style; run bibtex to produce approximate source
;;;      - do bib2html conversions with scheme program

;;; Valid top-level commands (latex standard first, then tspl-specific)
;;;   \'
;;;   \\                                      (used in flushleft env)
;;;   \<space>
;;;   $ ... $                                 (see below)
;;;   { ... }
;;;   %<comment>
;;;   \[ ... \]                               (see below)
;;;   \begin{alphalist} ... \end{alphalist}
;;;   \begin{enumerate} ... \end{enumerate}
;;;   \begin{eqnarray*} ... \end{eqnarray*}   (see below)
;;;   \begin{itemize} ... \end{itemize}
;;;   \begin{flushleft} ... \end{flushleft}
;;;   \bigskip
;;;   \chapter{<text>}
;;;   \cite{...}
;;;   \dots
;;;   \emph{...}
;;;   \epsfbox{...}
;;;   \index<args>                            (see below)
;;;   \item
;;;   \label{...}
;;;   \noindent
;;;   \pagebreak[n]
;;;   \pageref{...}
;;;   \ref{...}
;;;   \section{<text>}
;;;   \smallskip
;;;   \vdots
;;;   \vskip                                  (used only in fft section)

;;;   \ChezScheme
;;;   \begin{grammar} ... \end{grammar}
;;;   \ang{...}
;;;   \bar
;;;   \entryheader ... \endentryheader
;;;   \exercise
;;;   \formdef<args>
;;;   \itemvdots
;;;   \kplus
;;;   \kstar
;;;   \longcode                               (\schemedisplay prefix)
;;;   \longis
;;;   \noskip                                 (\schemedisplay prefix)
;;;   \noskipentryheader ... \noskipendentryheader
;;;   \orbar
;;;   \parheader{<text>}
;;;   \returns
;;;   \scheme{...}                            (see below)
;;;   \schemedisplay ... \endschemedisplay    (see below)
;;;   \suppress\formdef<args>
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}

;;; Valid within $ ... $
;;;   \{ \}
;;;   \,
;;;   \;
;;;   \Rightarrow
;;;   \cr
;;;   \dots
;;;   \infty
;;;   \le
;;;   \log
;;;   \neq
;;;   \over
;;;   \pi
;;;   \pm
;;;   \pmatrix
;;;   \rm
;;;   \sigma
;;;   \sqrt
;;;   \sum
;;;   \times
;;;   \W                                      (fft helper defined in tspl.cls)

;;; Valid within \[ ... \]
;;;   everything valid within $ ... $
;;;   \fftcases                               (fft helper defined in tspl.cls)

;;; Valid within \begin{eqnarray*} ... \end{eqnarray*}
;;;   everything valid within $ ... $
;;;   \\
;;;   \fftcases                               (fft helper defined in tspl.cls)

;;; Valid within \scheme{...}
;;;   \dots
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \
;;;   all other characters besides } and newline

;;; Valid within \schemedisplay ... \endschemedisplay
;;;   \dots
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \vdots
;;;   \
;;;   all other characters besides }

;;; Valid within index
;;;   \index{levels}
;;;   \index{levels|pageformat}
;;;   levels --> level
;;;          --> level!levels
;;;   level --> keyentry                      (key and entry same)
;;;   level --> key@entry                     (separate key and entry)
;;;
;;;   pageformat                              (presently unchecked)
;;;     emph
;;;     see(<text>)
;;;   Valid within keyentry
;;;     \scheme{...}
;;;   Valid within entry
;;;     \scheme{...}
;;;     \var{...}
;;;     \ChezScheme


(let ()
(include "preplib.ss")

(define copy-through-newline
  (lambda (ip op)
    (state-case (c (read-char ip))
      [(#\newline) (write-char c op)]
      [(eof) (void)]
      [else (write-char c op) (copy-through-newline ip op)])))

(define (s0 ip op)
  (state-case (c (read-char ip))
    [(#\\)
     (let ((cmd (read-command ip)))
       (cond
         [(assq cmd defs) =>
          (lambda (a)
            (let ((pattern (cadr a)) (template (cddr a)))
              (let ((buf (open-output-string)))
                (expand-template buf template (read-args ip pattern cmd) cmd)
                (s0 (open-input-string (get-output-string buf)) op)))
            (s0 ip op))]
         [else
          (case cmd
            [(|[|) ;]
             (fprintf op "\\[") ;]
             (smathdisplay ip op)
             (s0 ip op)]
            [(begin end)
             (read-open-brace ip)
             (let ((env (read-bracketed-text ip 1)))
               (cond
                 [(member env '("enumerate" "grammar" "itemize" "document"
                                "flushleft" "alphalist"))
                  (fprintf op "\\~a{~a}" cmd env)]
                 [(equal? env "eqnarray*")
                  (fprintf op "\\~a{~a}" cmd env)
                  (seqnarray* ip op)]
                 [else (assertion-violationf #f "unrecognized environment \\~a{~a}" cmd env)]))
             (s0 ip op)]
            [(|'| |\| | | |ChezScheme| ang bar bigskip dots
              endentryheader endnoskipentryheader entryheader exercise
              item itemvdots kplus kstar
              longcode longis medskip noindent noskip noskipentryheader orbar
              returns smallskip
bf tt _
              vdots vskip)
             (fprintf op "\\~a" cmd)
             (s0 ip op)]
            [(latex)
             (display (read-bracketed-text ip) op)
             (s0 ip op)]
            [(whenhtml)
             (read-bracketed-text ip)
             (s0 ip op)]
            [(whenlatex)
             (s0 (open-input-string (read-bracketed-text ip)) op)
             (s0 ip op)]
            [(include)
             (let ((filename (read-bracketed-text ip)))
               (let ((ip (open-input-file filename)))
                 (s0 ip op)
                 (close-input-port ip)))
             (s0 ip op)]
            [(primpageref)
             (fprintf op "\\pageref{~a}"
               (name->label (read-bracketed-text ip)))
             (s0 ip op)]
            [(formdef)
             (read-open-brace ip)
             (s5 ip op #f)
             (s0 ip op)]
            [(index)
             (read-open-brace ip)
             (s4 ip op)
             (s0 ip op)]
            [(pagebreak) ; read up [...] argument
             (display "\\pagebreak" op)
             (let loop ()
               (state-case (c (read-char ip)) ; [
                 [(#\]) (write-char c op)]
                 [(eof) (assertion-violationf #f "unexpected eof in \pagebreak[]")]
                 [else (write-char c op) (loop)]))
             (s0 ip op)]
            [(scheme)
             (read-open-brace ip)
             (display "\\scheme{" op)
             (s3 ip op)
             (write-char #\} op)
             (s0 ip op)]
            [(schemedisplay)
             (display "\\schemedisplay" op)
             (copy-through-newline ip op)
             (s2 ip op #f)]
            [(suppress)
             (unless (state-case (c (read-char ip))
                       [(#\\) (eq? (read-command ip) 'formdef)]
                       [else #f])
               (input-error ip "expected \\formdef following \\suppress"))
             (read-open-brace ip)
             (s5 ip op #t)
             (s0 ip op)]
            [(def)
             (let ((cmd (state-case (c (read-char ip))
                           [(#\\) (read-command ip)]
                           [else (input-error ip "invalid \\def syntax")])))
               (let ((pattern (read-def-pattern ip)))
                 (let ((template (read-bracketed-text ip)))
                   (set! defs (cons (list* cmd pattern template) defs)))))
             (s0 ip op)]
            [(var)
             (read-open-brace ip)
             (dovar ip op)
             (s0 ip op)]
            [else (unexpected-command ip cmd)])]))]
    [(#\$)
     (write-char #\$ op)
     (s$ ip op)
     (s0 ip op)]
    [(#\%)
     (write-char #\% op)
     (let f ()
       (state-case (c (read-char ip))
         ((#\newline) (write-char c op) (s0 ip op))
         ((eof) (s0 ip op))
         (else (write-char c op) (f))))]
    [(#\{)
     (write-char c op)
     (fluid-let ((defs defs) (group-level (+ group-level 1)))
       (s0 ip op))
     (s0 ip op)]
    [(#\})
     (unless (> group-level 0)
       (input-error ip "unmatched close brace"))
     (write-char c op)]
    [(eof) (void)]
    [else (write-char c op) (s0 ip op)]))

(define (s$ ip op) ; within $...$
  (state-case (c (read-char ip))
    [(#\\)
     (let ((cmd (read-command ip)))
       (cond
         [(memq cmd mathcmds)
          (fprintf op "\\~a" cmd)
          (s$ ip op)]
         [else (unexpected-command ip cmd)]))]
    [(#\$) (write-char #\$ op)]
    [(eof) (assertion-violationf #f "unexpected eof within $ ... $")]
    [else (write-char c op) (s$ ip op)]))

(define (smathdisplay ip op) ; within \[ ... \]
  (state-case (c (read-char ip))
    [(#\\)
     (let ((cmd (read-command ip)))
       (cond
         [(memq cmd mathcmds)
          (fprintf op "\\~a" cmd)
          (smathdisplay ip op)]
         [else
          (case cmd ;[[
            [(|]|) (fprintf op "\\]")]
            [(fftcases)
             (fprintf op "\\~a" cmd)
             (smathdisplay ip op)]
            [else (unexpected-command ip cmd)])]))]
    [(eof) (assertion-violationf #f "unexpected eof within \[ ... \]")]
    [else (write-char c op) (smathdisplay ip op)]))

(define (seqnarray* ip op) ; within \begin{eqnarray*} ... \end{eqnarray*}
  (state-case (c (read-char ip))
    [(#\\)
     (let ((cmd (read-command ip)))
       (cond
         [(memq cmd mathcmds)
          (fprintf op "\\~a" cmd)
          (seqnarray* ip op)]
         [else
          (case cmd
            [(end)
             (read-open-brace ip)
             (let ((env (read-bracketed-text ip 1)))
               (cond
                 [(equal? env "eqnarray*")
                  (fprintf op "\\~a{~a}" cmd env)]
                 [else (input-error ip "expected \\end{eqnarray*}")]))]
            [(|\| fftcases)
             (fprintf op "\\~a" cmd)
             (seqnarray* ip op)]
            [else (unexpected-command ip cmd)])]))]
    [(eof) (assertion-violationf #f "unexpected eof within eqnarray*")]
    [else (write-char c op) (seqnarray* ip op)]))

(define (s2 ip op n?) ; within a schemedisplay
  ; n? is true if we've just passed a new line within the display; we
  ; use it to determine whether to insert \\\null.
  ; Why do we include \null after \\?
  (state-case (c (read-char ip))
    [(#\\)
     ; use read-alpha-command instead of read-alpha-command to avoid
     ; improper handling of special characters that follow a shash
     (let ((cmd (read-alpha-command ip)))
       (case cmd
         [(endschemedisplay)
          (fprintf op "~%\\endschemedisplay")
          (s0 ip op)]
         [else
          (when n? (fprintf op "\\\\\\null~%"))
          (case cmd
            [(dots)
             (display "{\\dots}" op)
             (s2 ip op #f)]
            [(var)
             (read-open-brace ip)
             (dovar ip op)
             (s2 ip op #f)]
            [(vdots)
             (display "{\\vdots}" op)
             (s2 ip op #f)]
            [else ; assume random \ possibly followed by alphabetic chars
             (fprintf op "{\\schbackslash}~a" cmd)
             (s2 ip op #f)])]))]
    [else
     (when n? (fprintf op "\\\\\\null~%"))
     (state-case (c c)
       [(#\;) (s2.2 ip op)]
       [(#\.) (display "{\\schdot}" op) (s2 ip op #f)]
       [(#\~) (display "{\\schtilde}" op) (s2 ip op #f)]
       [(#\^) (display "{\\schcarat}" op) (s2 ip op #f)]
       [(#\@) (display "{\\schatsign}" op) (s2 ip op #f)]
       [(#\_) (display "{\\schunderscore}" op) (s2 ip op #f)]
       [(#\$ #\& #\% #\# #\{ #\})
        (fprintf op "\\~c" c)
        (s2 ip op #f)]
       [(#\newline)
        (state-case (c (peek-char ip))
          ((#\newline)
           (read-char ip)
           (fprintf op "~%\\schemeblankline~%")
           (s2 ip op #f))
          (else (s2 ip op #t)))]
       [(eof) (assertion-violationf #f "file ended within schemedisplay")]
       [else (write-char c op) (s2 ip op #f)])]))

(define (s2.2 ip op) ; seen ; in schemedisplay
  ; convert ;=> into \is and ;== into \si
  (state-case (c (peek-char ip))
    [(#\=)
     (read-char ip)
     (state-case (c (peek-char ip))
       ((#\>)
        (read-char ip)
        (display "\\is" op)
        (s2 ip op #f))
       ((#\=)
        (read-char ip)
        (display "\\si" op)
        (s2 ip op #f))
       (else
        (display ";=" op)
        (s2 ip op #f)))]
    [else
     (write-char #\; op)
     (s2 ip op #f)]))

(define dovar
  (lambda (ip op)
    (display "\\var{" op)
    (let f ()
      (state-case (c (read-char ip))
        [(#\}) (write-char c op)]
        [(#\_)
         (display "$_" op)
         (state-case (c (read-char ip))
           [(eof) (assertion-violationf #f "unexpected eof in \\var{}")]
           [else
            (write-char c op)
            (write-char #\$ op)
            (f)])]
        [(eof) (assertion-violationf #f "unexpected eof in \\var{}")]
        [else (write-char c op) (f)]))))

(define (s3 ip op) ; within \scheme
  ; unlike schemedisplay, does not allow { or }
  (state-case (c (read-char ip))
    [(#\{) (input-error ip "unexpected { within \\scheme{}")]
    [(#\}) (void)]
    [(#\\)
     ; use read-alpha-command instead of read-alpha-command to avoid
     ; improper handling of special characters that follow a shash,
     ; including }
     (let ((cmd (read-alpha-command ip)))
       (case cmd
         [(dots)
          (display "{\\dots}" op)
          (s3 ip op)]
         [(var)
          (read-open-brace ip)
          (dovar ip op)
          (s3 ip op)]
         [else ; assume random \ possibly followed by alphabetic chars
          (fprintf op "{\\schbackslash}~a" cmd)
          (s3 ip op)]))]
    [(#\.) (display "{\\schdot}" op) (s3 ip op)]
    [(#\~) (display "{\\schtilde}" op) (s3 ip op)]
    [(#\^) (display "{\\schcarat}" op) (s3 ip op)]
    [(#\@) (display "{\\schatsign}" op) (s3 ip op)]
    [(#\_) (display "{\\schunderscore}" op) (s3 ip op)]
    [(#\$ #\& #\% #\#)
     (fprintf op "\\~c" c)
     (s3 ip op)]
    [(#\newline) (assertion-violationf #f "line ended within \\scheme{}")]
    [(eof) (assertion-violationf #f "file ended within \\scheme{}")]
    [else (write-char c op) (s3 ip op)]))

(define (s4 ip op)
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
    (lambda () (s4.1 ip (open-output-string) 1 '()))
    (lambda (levels page-format)
      (reconstitute-index-form levels page-format ip op))))

(define reconstitute-index-form
  (lambda (levels page-format ip op)
    (let ((keys (map (lambda (s) (insert-quotes (open-input-string s)))
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
                            (expand-entry (open-input-string (cdr level))))))
                      levels)))
     ; hprep.ss generates the same sequence of uids and uses the labels
     ; emitted here (into the .aux file) to put page numbers into the
     ; html index
      (fprintf op "\\label{~a}" (next-index-uid ip))
      (display "\\index" op)
      (let f ((keys keys) (texts texts) (delim #\{))
        (unless (null? keys)
          (write-char delim op)
          (let ((key (car keys)) (text (car texts)))
            (if (string=? key text)
                (display text op)
                (fprintf op "~a@~a" key text)))
          (f (cdr keys) (cdr texts) #\!)))
      (unless (string=? page-format "") (fprintf op "|~a" page-format))
      (write-char #\} op))))

(define expand-entry
  ; expands \emph{}, \scheme{}, |'|, and \var{} forms
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (read-char ip))
        [(#\\)
         (let ((cmd (read-command ip)))
           (case cmd
             [(emph)
              (fprintf buf "\\emph{~a}" (read-bracketed-text ip))
              (expand-entry ip)]
             [(scheme)
              (read-open-brace ip)
              (display "\\scheme{" buf)
              (s3 ip buf)
              (write-char #\} buf)
              (expand-entry ip)]
             [(var)
              (read-open-brace ip)
              (dovar ip buf)
              (expand-entry ip)]
             [(|ChezScheme| |'|)
              (fprintf buf "\\~a" cmd)
              (expand-entry ip)]
             [else (unexpected-command ip cmd)]))]
        ((eof) (get-output-string buf))
        (else (write-char c buf) (expand-entry ip))))))

(define insert-quotes
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (read-char ip))
        ((#\" #\@ #\! #\|)
         (write-char #\" buf)
         (write-char c buf)
         (insert-quotes ip))
        ((#\\)
         (write-char #\\ buf)
         (state-case (c (peek-char ip))
           ((#\") (read-char ip) (write-char c buf))
           (else (insert-quotes ip))))
        ((eof) (get-output-string buf))
        (else (write-char c buf) (insert-quotes ip))))))

(define (s5 ip op suppress?) ; seen \formdef{
  ;; normal syntax:
  ;;    \formdef{name}{type}{form}
  ;;    with implicit \scheme{} around name and form
  ;; for application, constant, variable reference:
  ;;    \formdef{}{name}{type}{form}
  ;;    with implicit \scheme{} around form
  ;; * read up name (through })
  ;; * read up type ({ through })
  ;; * process form as if in \scheme{
  ;; * emit \formdef{<type>}{<form>}
  ;; * emit \formsummary{<name>}{<type>}{<form>}
  ;; * emit \index{<name>|emph}, suitably munged 
  ;; suppress \formdef if suppress? is true (but leave \formsummary and \index)
  (let ((name (let ((t (read-bracketed-text ip 1)))
                (if (string=? t "")
                    (state-case (c (read-char ip))
                      ((#\{) (read-bracketed-text ip 1))
                      (else (assertion-violationf #f "invalid \\formdef")))
                    (format "\\scheme{~a}" t)))))
    (state-case (c (read-char ip))
      ((#\{)
       (let ((type (read-bracketed-text ip 1)))
         (state-case (c (read-char ip))
           ((#\{)
            ; read form as implicit \scheme{
            (let ((form (let ((buf (open-output-string)))
                          (s3 ip buf)
                          (get-output-string buf))))
              (unless suppress?
                (fprintf op "\\formdef{~a}{~a}" type form))
              ; strip name to create sort key for index and summary
              (let ((key (strip-sort-key (open-input-string name))))
               ; generate page number label for primpageref
                (let* ((label (name->label key)))
                  (unless (getprop label 'already-labeled)
                    (putprop label 'already-labeled #t)
                    (fprintf op "\\label{~a}" label)))
                (fprintf op "\\formsummary{~a}{~a}{~a}" key type form)
                ; emit index entry
                (reconstitute-index-form (list (cons key name))
                  "|emph" ip op))))
           (else (assertion-violationf #f "invalid \\formdef")))))
      (else (assertion-violationf #f "invalid \\formdef")))))

(set! go
  (lambda (fn)
    (let ((ip (open-input-file (format "~a.stex" fn)))
          (op (open-output-file (format "~a.tex" fn) 'truncate)))
      (fluid-let ((index-uid-counter -1)
                  (defs '())
                  (group-level 0))
        (fprintf op "%%% DO NOT EDIT THIS FILE~%")
        (fprintf op "%%% Edit the .stex version instead~%")
        (newline op)
        (when (file-exists? base-macros-file)
          (let ((ip (open-input-file base-macros-file)))
            (s0 ip op)
            (close-input-port ip)))
        (s0 ip op)
        (close-output-port op)
        (close-input-port ip)))))
)

; (abort-handler (reset-handler))
