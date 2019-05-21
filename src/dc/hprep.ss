;;; TODO
;;;
;;; - multilevel index entries
;;; - consider running hprep on output of prep
;;; - update documentation
;;; - rename preplib.stex to basemacros.stex
;;; - use read-arg in place of read-bracketed-text where appropriate
;;; - update prep.ss to use new preplib.ss, similar structure
;;; - change documentation or implementation of \var; presently,
;;;      only first character after _ is sup'd
;;; - create local def-env's w/o side effects; eliminate other side effects
;;; - use input-error in place of error where appropriate
;;; - print line numbers and/or lines of surrounding text on input errors


;;; OLD TODO
;;;    - move load of base-macros-file "preplib.stex" to heap build time
;;;    - implement class file notion
;;;    - try to add \primpageref{<identifier>} command to refer to first
;;;      formdef of each primitive; e.g., \primpageref{null?} should refer
;;;      to the page where null? is described; may be a pain to support
;;;      in latex version; we may not be able to use arbitary Scheme
;;;      identifiers as labels
;;;    - do we need to convert `` and '' into html equivalents (" and "?)?

;;;    - fancy up index with headers, quick access links
;;;    - try to handle more subscripts/superscripts in htmlmath
;;;    - check conformance...known problems:
;;;      - <img> used within <pre> (for $\Rightarrow$)
;;;      - <sub> used within <pre>

;;;    - consider including note in index & summary re: page numbers
;;;    - consider adding some color; also hrule (or fancy hrule) before
;;;      formdefs
;;;    - consider readjusting math size
;;;    - replace --- with em-dash, -- with en-dash if available
;;;    - consider adding back pointers to cite-points from bib entries

;;;    - update ReadMe
;;;    - clean up and make prep.ss and hprep.ss consistent

;;;    - allow comments in math mode
;;;    - handle ~ in xprep.ss?
;;;    - merged formsummarys & indexes

;;; Valid top-level commands (latex standard first, then tspl-specific)
;;;   ~
;;;   \'e, \'o
;;;   \\
;;;   $ ... $                                 (see below)
;;;   { ... }                                 (groups)
;;;   \begingroup ... \endgroup
;;;   \bgroup ... \egroup
;;;   %<comment>
;;;   \[ ... \]                               (see below)
;;;   \begin{alphalist} ... \end{alphalist}
;;;   \begin{enumerate} ... \end{enumerate}
;;;   \begin{eqnarray*} ... \end{eqnarray*}   (see below)
;;;   \begin{itemize} ... \end{itemize}
;;;   \begin{thebibliography} ... \end{thebibliography}
;;;   \begin{divertoutput}[i] ... \end{divertoutput}
;;;      i selects a currently opened file:
;;;          0 first file opened, 1 second file, ...
;;;          or -1 most recent, -2 next most recent, ...
;;;   \cite{...}
;;;   \emph{...}
;;;   \epsfbox{...}
;;;   \include{filename}
;;;   \index<args>                            (see below)
;;;   \item
;;;   \label{...}
;;;   \pagebreak[n]
;;;   \pageref{...}
;;;   \ref{...}
;;;   \vskip                                  (used only in fft section)
;;;      must be nothing else on line following vskip <amount>
;;;   \def
;;;   \newenvironment
;;;   \newif
;;;   \newcounter
;;;   \setcounter
;;;   \addtocounter
;;;   \stepcounter
;;;   \tt
;;;   \bf
;;;   \bibitem

;;;   \formdef<args>
;;;   \suppress\formdef<args>
;;;   \raw{...}
;;;   \scheme{...}                            (see below)
;;;   \schemedisplay ... \endschemedisplay    (see below)
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \newlabeltag
;;;   \labeltag
;;;   \primpageref
;;;   \openhtmlfile
;;;   \currentoutputfile
;;;   \closehtmlfile
;;;   \makeindex
;;;   \makesummary

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
;;;   \label{text} may appear immediately after \schemedisplay

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


(eval-when (compile eval) (case-sensitive #t))

(let ()
(include "dsm.ss")
(let ()
(include "preplib.ss")

(define open-html-file
  (lambda (ip title)
    (define file-name-root
      (lambda (s)
        (let loop ((i (- (string-length s) 1)))
          (cond
            ((< i 0) s)
            ((char=? (string-ref s i) #\.) (substring s 0 i))
            (else (loop (- i 1)))))))
    (define next-count
      (lambda (root)
        (cond
          [(assoc root output-file-counters) =>
           (lambda (a)
             (let ((n (+ (cdr a) 1)))
               (set-cdr! a n)
               n))]
          [else
           (set! output-file-counters
             (cons (cons root 0) output-file-counters))
           0])))
   ; generate sequence of names foo.html, foo_1.html, foo_2.html, ...
    (let ((op (let ((fn (let ((root (file-name-root (port-name ip))))
                          (let ((n (next-count root)))
                            (if (= n 0)
                                (format "~a.html" root)
                                (format "~a_~d.html" root n))))))
                (open-output-file fn 'truncate))))
      (fprintf op "<!-- DO NOT EDIT THIS FILE-->~%")
      (fprintf op "<!-- Edit the .stex version instead-->~%~%")
      (fprintf op "<html>~%")
      (fprintf op "<head>~%<title>~a</title>~%</head>~%" title)
      (fprintf op "<body>~%")
      op)))

(define close-html-port
  (lambda (p)
    (fprintf p "</body>~%")
    (fprintf p "</html>")
    (close-output-port p)))

(define make-label
  (lambda (ref pageref)
    (cons ref pageref)))
(define label-ref car)
(define label-pageref cdr)
(define put-ref
  (lambda (sym ref pageref)
    (putprop sym 'label (make-label ref pageref))))
(define get-ref
  (lambda (sym which-ref)
    (cond
      [(getprop sym 'label) => which-ref]
      [else (warningf #f "reference to undefined label ~a" sym)
            "???"])))

(define get-cite
  (lambda (key)
    (or (getprop key 'cite)
        (begin
           (warningf #f "bib entry ~a not found" key)
           "???"))))

(define get-url
  (lambda (name)
    (or (getprop name 'url)
        (begin
          (warningf #f "url for ~a not found" name)
          "undefined"))))

(define read-aux-file
  (lambda (fn)
    (when (file-exists? fn)
      (let ((ip (open-input-file fn)))
        (fluid-let ((current-ifile ip))
          (let loop ((newline? #f))
            (state-case (c (read-char ip))
              [(#\newline) (loop #t)]
              [(#\\)
               (when newline?
                 (let ((cmd (read-command ip)))
                   (case cmd
                     [(|@|)
                      (let ((cmd (read-command ip)))
                        (case cmd
                          [(input)
                           (read-aux-file (read-bracketed-text ip))]))]
                     [(bibcite)
                      (let* ((key (read-bracketed-text ip))
                             (cite (read-bracketed-text ip)))
                        (putprop (string->symbol key) 'cite cite))]
                     [(newlabel)
                      (let* ((name (read-bracketed-text ip))
                             (ref (begin (read-open-brace ip)
                                         (read-bracketed-text ip)))
                             (pageref (read-bracketed-text ip)))
                        (put-ref (string->symbol name) ref pageref))])))
               (loop #f)]
              [(eof) (void)]
              [else (loop #f)])))))))

;; structures
(define-structure (index-entry url keys texts pageno pageformat))
(define-structure (summary-entry url key form type pageno))

(define s$
  (lambda (ip op) ; within $...$
    (let ((s (let ((buf (open-output-string)))
               (let loop ()
                 (state-case (c (read-char ip))
                   [(#\\)
                    (let ((cmd (read-command ip)))
                      (cond
                        [(memq cmd mathcmds)
                         (fprintf buf "\\~a" cmd)
                         (loop)]
                        [else (unexpected-command cmd)]))]
                   [(#\$) (get-output-string buf)]
                   [(eof) (input-error "unexpected eof within $ ... $")]
                   [else (write-char c buf) (loop)])))))
      (emit-math s op))))

(define (scomment ip op)
  (state-case (c (read-char ip))
    [(eof #\newline) (void)]
    [else (scomment ip op)]))

(define seqnarray*
  (lambda (ip op) ; within \begin{eqnarray*} ... \end{eqnarray*}
    (let ((s (let ((buf (open-output-string)))
               (let loop ()
                 (state-case (c (read-char ip))
                   [(#\\)
                    (let ((cmd (read-command ip)))
                      (cond
                        [(memq cmd mathcmds)
                         (fprintf buf "\\~a" cmd)
                         (loop)]
                        [else
                         (case cmd
                           [(end)
                            (read-open-brace ip)
                            (if (equal? (read-bracketed-text ip 1) "eqnarray*")
                                (get-output-string buf)
                                (input-error "expected \\end{eqnarray*}"))]
                           [(|\| fftcases)
                            (fprintf buf "\\~a" cmd)
                            (loop)]
                           [else (unexpected-command cmd)])]))]
                   [(eof) (assertion-violationf #f "unexpected eof within eqnarray*")]
                   [else (write-char c buf) (loop)])))))
      (punt-to-latex (format "\\begin{eqnarray*}~a\\end{eqnarray*}" s) op))))

(define smathdisplay
  (lambda (ip op) ; within \[ ... \]
    (let ((s (let ((buf (open-output-string)))
               (let loop ()
                 (state-case (c (read-char ip))
                   [(#\\)
                    (let ((cmd (read-command ip)))
                      (cond
                        [(memq cmd mathcmds)
                         (fprintf buf "\\~a" cmd)
                         (loop)]
                        [else
                         (case cmd ;[
                           [(|]|) (get-output-string buf)]
                           [(fftcases)
                            (fprintf buf "\\~a" cmd)
                            (loop)]
                           [else (unexpected-command cmd)])]))]
                   [(eof) (assertion-violationf #f "unexpected eof within \[ ... \]")]
                   [else (write-char c buf) (loop)])))))
      (emit-math s op))))

(define emit-math
  (lambda (s op)
    (cond
      [(htmlmath (open-input-string s)) =>
       (lambda (html) (display html op))]
      [else (punt-to-latex (format "$~a$" s) op)])))

(define htmlmath
  (lambda (ip)
    (let ((buf (open-output-string)))
      (let loop ((closers '()))
        (state-case (c (read-char ip))
          [(#\\)
           (let ((cmd (read-command ip)))
             (case cmd
               [(|{| |}|)
                (fprintf buf "~a" cmd)
                (loop closers)]
               [(|,| |;|)
                (loop closers)]
               [(dots)
                (fprintf buf "...")
                (loop closers)]
               [(log)
                (fprintf buf "log")
                (loop closers)]
               [(pm)
                (fprintf buf "&plusmn;")
                (loop closers)]
               [(times)
                (fprintf buf " &times; ")
                (loop closers)]
               [(|Rightarrow| |W| bigl bigr cr infty le lfloor neq over
                 pi pmatrix rfloor rm sigma sqrt sum)
                #f]
               [else (unexpected-command cmd)]))]
          [(#\{) (loop (cons "" closers))]
          [(#\}) (display (car closers) buf) (loop (cdr closers))]
          [(#\_)
           (state-case (c (read-char ip))
             [(#\{)
  ; looks too ugly & we probably can't allow nesting anyway
  ;            (fprintf buf "<sub>")
  ;            (loop (cons "</sub>" closers))
              #f]
             [(eof) (assertion-violationf #f "unexpected eof in math after _")]
             [else (fprintf buf "<sub>~a</sub>" c) (loop closers)])]
          [(#\^)
           (state-case (c (read-char ip))
             [(#\{)
  ; looks too ugly & we probably can't allow nesting anyway
  ;            (fprintf buf "<sup>")
  ;            (loop (cons "</sup>" closers))
              #f]
             [(eof) (assertion-violationf #f "unexpected eof in math after ^")]
             [else (fprintf buf "<sup>~a</sup>" c) (loop closers)])]
          [((#\a - #\z) (#\A - #\Z))
           (fprintf buf "<em>")
           (let loop ((c c))
             (write-char c buf)
             (state-case (c (peek-char ip))
               [((#\a - #\z) (#\A - #\Z)) (read-char ip) (loop c)]
               [else (void)]))
           (fprintf buf "</em>")
           (loop closers)]
          [(#\<) (fprintf buf " &lt; ") (loop closers)]
          [(#\>) (fprintf buf " &gt; ") (loop closers)]
          [(#\= #\+ #\-) (fprintf buf " ~c " c) (loop closers)]
          [((#\0 - #\9) #\space #\newline #\/ #\. #\( #\) #\[ #\] #\! #\| #\,)
           (write-char c buf) (loop closers)]
          [(eof) (get-output-string buf)]
          [else (input-error
                   "unexpected character ~s in math mode"
                   c)])))))

(define punt-to-latex
  (lambda (s op)
    (define latex-header
"\\documentclass[12pt]{article}
\\begin{document}
\\pagestyle{empty}
\\input mathmacros
")
    (define latex-trailer
"
\\end{document}
")
    (cond
      [(assoc s latex-cache) =>
       (lambda (a)
         (fprintf op "<img src=\"~a\">" (cdr a)))]
      [else
       (let* ((fn (math-file-name))
              (texfn (format "~a.tex" fn))
              (giffn (format "~a.gif" fn)))
         (fprintf op "<img src=\"~a\">" giffn)
         (set! latex-cache (cons (cons s (format "~a" giffn)) latex-cache))
        ; don't rewrite file unless different to avoid need to remake gif file
         (let ((s (format "~a~a~a" latex-header s latex-trailer)))
           (unless (guard (c [#t #f])
                     (let ((texip (open-input-file texfn)))
                       (let* ((n (string-length s)) (s1 (make-string n)))
                         (and (= (block-read texip s1 n) n)
                              (eof-object? (read-char texip))
                              (string=? s1 s)))))
             (let ((texop (open-output-file texfn 'truncate)))
               (display s texop)
               (close-output-port texop)))))])))

(define dovar
  (lambda (ip op)
    (read-open-brace ip)
    (display "<var>" op)
    (let f ()
      (state-case (c (read-char ip))
        [(#\}) (display "</var>" op)]
        [(#\_)
         (display "<sub>" op)
         (state-case (c (read-char ip))
           [(eof) (assertion-violationf #f "unexpected eof in \\var{}")]
           [else
            (write-char c op)
            (display "</sub>" op)
            (f)])]
        [(eof) (assertion-violationf #f "unexpected eof in \\var{}")]
        [else (write-char c op) (f)]))))

(define (slabel name text op)
  (let ((uid (gensym)))
    (slabel* name uid op)
    (format "<a name=\"~a\">~a</a>" uid)))

(define (slabel* name uid op)
  (let ((url (format "~a#~a" (port-name op) uid)))
    (pretty-print `(putprop ',name 'url ,url) ox-op)))

(define sref
  (lambda (ip op which-ref)
    (let ((name (string->symbol (read-bracketed-text ip))))
      (fprintf op "<a href=\"~a\">~a</a>"
        (get-url name)
        (get-ref name which-ref)))))

(define sprimpageref
  (lambda (ip op)
    (let ((name (name->label (read-bracketed-text ip))))
      (fprintf op "<a href=\"~a\">~a</a>"
        (get-url name)
        (get-ref name label-pageref)))))

(define sformdef
  (lambda (ip op suppress?) ; seen \formdef{ or \suppress\formdef{
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
                            (sscheme ip buf)
                            (get-output-string buf))))
                ; strip name to create sort key for index and summary
                (let ((key (strip-sort-key (open-input-string name))))
                  (let* ((label (name->label key))
                         (pageno (get-ref label label-pageref))
                         (seen-tag (getprop label 'tag))
                         (tag (or seen-tag (gensym)))
                         (url (format "~a#~a" (port-name op) tag)))
                    (unless seen-tag ; tag only first in sequence of formdefs
                      (putprop label 'tag tag)
                     ; synchronize with slabel
                      (pretty-print `(putprop ',label 'url ,url) ox-op)
                      (fprintf op "<a name=\"~a\"></a>" tag))
                    (set! summary-entries
                      (cons (make-summary-entry url key form type pageno)
                            summary-entries)))
                  (unless suppress?
                    (fprintf op "<br><strong>~a:</strong> <tt>~a</tt>~%"
                      type form))
                  ; emit index entry
                  (reconstitute-index-form ip op
                    (list (cons key name))
                    "|emph"))))
             (else (assertion-violationf #f "invalid \\formdef")))))
        (else (assertion-violationf #f "invalid \\formdef"))))))

(define sscheme
  (lambda (ip op) ; within \scheme
    ; unlike schemedisplay, does not allow { or }
    (state-case (c (read-char ip))
      [(#\{) (input-error "unexpected { within \\scheme{}")]
      [(#\}) (void)]
      [(#\\)
       ; use read-alpha-command instead of read-alpha-command to avoid
       ; improper handling of special characters that follow a s\ash,
       ; including }
       (let ((cmd (read-alpha-command ip)))
         (case cmd
           [(dots)
            (display "..." op)
            (sscheme ip op)]
           [(var)
            (dovar ip op)
            (sscheme ip op)]
           [else ; assume random \ possibly followed by alphabetic chars
            (fprintf op "\\~a" cmd)
            (sscheme ip op)]))]
      [(#\&) (display "&amp;" op) (sscheme ip op)]
      [(#\<) (display "&lt;" op) (sscheme ip op)]
      [(#\>) (display "&gt;" op) (sscheme ip op)]
      [(#\newline) (assertion-violationf #f "line ended within \\scheme{}")]
      [(eof) (assertion-violationf #f "file ended within \\scheme{}")]
      [else (write-char c op) (sscheme ip op)])))

(define sschemedisplay
  (let ((buf (open-output-string)))
    (define code-okay?
      (lambda (ip) ; looks for lines starting with spaces
        (let loop ((newline? #f) (spaces 0))
          (state-case (c (read-char ip))
            [(#\newline) (loop #t spaces)]
            [(#\space) (loop #f (if newline? (+ spaces 1) spaces))]
            [(eof) (> spaces 3)]
            [else (loop #f spaces)]))))
    (lambda (ip op) ; within a schemedisplay
      (clear-output-port buf)
      (let loop ((newlines 0) (junk? #f))
        (state-case (c (read-char ip))
          [(#\\)
           ; use read-alpha-command instead of read-alpha-command to avoid
           ; improper handling of special characters that follow a s\ash
           (let ((cmd (read-alpha-command ip)))
             (case cmd
               [(endschemedisplay)
                (when (and (> newlines 5) (not junk?))
                  (let ((s (get-output-string buf)))
                    (when (code-okay? (open-input-string s))
                      (fprintf code-op "~%~a~%" s))))]
               [(dots)
                (display "..." op)
                (loop newlines #t)]
               [(var)
                (dovar ip op)
                (loop newlines #t)]
               [(vdots)
                (punt-to-latex "\\vdots" op)
                (loop newlines #t)]
               [else ; assume random \ possibly followed by alphabetic chars
                (fprintf buf "\\~a" cmd)
                (fprintf op "\\~a" cmd)
                (loop newlines junk?)]))]
          [(#\newline) (newline op) (newline buf) (loop (+ newlines 1) junk?)]
          [(#\;)
          ; convert ;=> into \is and ;== into \si
           (state-case (c (peek-char ip))
             [(#\=)
              (read-char ip)
              (state-case (c (peek-char ip))
                [(#\>)
                 (read-char ip)
                 (display ";=>" buf)
                 (punt-to-latex "$\\Rightarrow$" op)]
                [(#\=)
                 (read-char ip)
                 (display "   " buf)
                 (fprintf op "<img src=\"gifs/ghostRightarrow.gif\">")]
                [else
                 (display ";=" buf)
                 (display ";=" op)])]
             [else
              (write-char #\; buf)
              (write-char #\; op)])
           (loop newlines junk?)]
          [(#\&) (write-char #\& buf) (display "&amp;" op) (loop newlines junk?)]
          [(#\<) (write-char #\< buf) (display "&lt;" op) (loop newlines junk?)]
          [(#\>) (write-char #\> buf) (display "&gt;" op) (loop newlines junk?)]
          [(eof) (input-error "file ended within schemedisplay")]
          [else (write-char c buf) (write-char c op) (loop newlines junk?)])))))

(define sindex
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
  (lambda (ip op)
    (call-with-values
      (lambda () (s4.1 ip (open-output-string) 1 '()))
      (lambda (levels page-format)
        (reconstitute-index-form ip op levels page-format)))))

(define reconstitute-index-form
  (lambda (ip op levels page-format)
    (let ((keys (map (lambda (level)
                       (if (car level)
                           (car level)
                           ; strip text to create sort key
                           (strip-sort-key (open-input-string (cdr level)))))
                     levels))
          (texts (map (lambda (level)
                        (expand-entry (open-input-string (cdr level))))
                      levels))
          (uid (string->symbol (next-index-uid ip))))
      (let* ((pageno (get-ref uid label-pageref))
             (tag (gensym))
             (url (format "~a#~a" (port-name op) tag)))
        (set! index-entries
          (cons (make-index-entry url keys texts pageno page-format)
                index-entries))
        (fprintf op "<a name=\"~a\"></a>" tag)))))

(define expand-entry
  ; expands \emph{}, \scheme{}, \', and \var{} forms
  ; perhaps this should just be packaging of s0
  (let ((buf (open-output-string)))
    (lambda (ip)
      (state-case (c (read-char ip))
        [(#\\)
         (let ((cmd (read-command ip)))
           (case cmd
             [(|'|)
              (state-case (c (read-char ip))
                [(#\e) (fprintf buf "&eacute;")]
                [(#\o) (fprintf buf "&oacute;")]
                [else (input-error "invalid \\' command")])
              (expand-entry ip)]
             [(emph)
              (fprintf buf "<em>~a</em>" (read-bracketed-text ip))
              (expand-entry ip)]
             [(scheme)
              (read-open-brace ip)
              (display "<tt>" buf)
              (sscheme ip buf)
              (display "</tt>" buf)
              (expand-entry ip)]
             [(var)
              (dovar ip buf)
              (expand-entry ip)]
             [else (unexpected-command cmd)]))]
        ((#\~) (write-char #\space buf) (expand-entry ip))
        ((eof) (get-output-string buf))
        (else (write-char c buf) (expand-entry ip))))))

(define smakeindex
 ; insert indexspace between letters?
 ; links per starting letter?
  (lambda (op)
    (define print-page
      (lambda (entry)
        (let ((pageno (format "\\raw{<a href=\"~a\">}~a\\raw{</a>}"
                        (index-entry-url entry)
                        (index-entry-pageno entry)))
              (pageformat (index-entry-pageformat entry)))
          (if (string=? pageformat "")
              (fprintf op ", ~a" pageno)
              (fprintf op ", \\~a{~a}" pageformat pageno)))))
     (define print-item
       (lambda (entry inkeys level)
         (let f ((keys (index-entry-keys entry))
                 (texts (index-entry-texts entry)))
           (if (eq? keys inkeys)
               (let f ((texts texts) (level level))
                 (if (null? texts)
                     (print-page entry)
                     (begin
                       (fprintf op "~%\\")
                       (do ((i level (- i 1))) ((= i 0)) (display "sub" op))
                       (fprintf op "indexitem ~a" (car texts))
                       (f (cdr texts) (+ level 1)))))
               (f (cdr keys) (cdr texts))))))
    (fprintf op "\\begin{theindex}~%")
    (let ((ls (sort (lambda (x y)
                      (let f ((xkeys (index-entry-keys x))
                              (ykeys (index-entry-keys y)))
                        (cond
                          [(null? xkeys)
                           (or (not (null? ykeys))
                               (< (pageno->number (index-entry-pageno x))
                                  (pageno->number (index-entry-pageno y))))]
                          [(null? ykeys) #f]
                          [else
                           (case (key-cmp (car xkeys) (car ykeys))
                             [(<) #t]
                             [(>) #f]
                             [else (f (cdr xkeys) (cdr ykeys))])])))
                    index-entries)))
      (let loop ((ls ls) (last-keys '()))
        (unless (null? ls)
          (let* ((entry (car ls)) (keys (index-entry-keys entry)))
            (let f ((keys keys) (last-keys last-keys) (level 0))
              (if (null? last-keys)
                  (if (null? keys)
                      (print-page entry)
                      (print-item entry keys level))
                  (if (eq? (key-cmp (car keys) (car last-keys)) '=)
                      (f (cdr keys) (cdr last-keys) (+ level 1))
                      (print-item entry keys level))))
            (loop (cdr ls) keys)))))
    (fprintf op "\\end{theindex}~%")))

(define pageno->number
  (lambda (s)
    (let ((sip (open-input-string s)))
      (let loop ((a -10000))
        (state-case (c (read-char sip))
          [(#\i)
           (state-case (c (peek-char sip))
             [(#\v) (read-char sip) (loop (+ a 4))]
             [(#\x) (read-char sip) (loop (+ a 9))]
             [else (loop (+ a 1))])]
          [(#\v)
           (state-case (c (peek-char sip))
             [(#\x) (read-char sip) (loop (+ a 5))]
             [else (loop (+ a 5))])]
          [(#\x) (loop (+ a 10))]
          [(eof) a]
          [else (or (string->number s) -1)])))))

(define char-table
  (let ((s (make-string 256 #\nul)))
    (define fill!
      (lambda (i ls)
        (unless (null? ls)
          (for-each
            (lambda (c) (string-set! s (char->integer c) (integer->char i)))
            (if (char? (car ls)) (list (car ls)) (car ls)))
          (fill! (+ i 1) (cdr ls)))))
    (fill! 1 '(
       #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\.  #\/ #\:
       #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~
       #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
       (#\A #\a) (#\B #\b) (#\C #\c) (#\D #\d) (#\E #\e) (#\F #\f)
       (#\G #\g) (#\H #\h) (#\I #\i) (#\J #\j) (#\K #\k) (#\L #\l)
       (#\M #\m) (#\N #\n) (#\O #\o) (#\P #\p) (#\Q #\q) (#\R #\r)
       (#\S #\s) (#\T #\t) (#\U #\u) (#\V #\v) (#\W #\w) (#\X #\x)
       (#\Y #\y) (#\Z #\z)))
    s))

(define char-cvt
 ; place all non-alphabetic characters up front; commonize upper and
 ; lower case characters
  (lambda (c)
    (string-ref char-table (char->integer c))))

(define key-cmp
  ; returns <, =, > if x<y, =y, >y
  (lambda (s1 s2)
    (let ([n1 (string-length s1)] [n2 (string-length s2)])
      (let f ([i 0])
        (if (fx= i n2)
            (if (fx= i n1) '= '>)
            (if (fx= i n1)
                '<
                (let ([c1 (char-cvt (string-ref s1 i))]
                      [c2 (char-cvt (string-ref s2 i))])
                  (if (char<? c1 c2)
                      '<
                      (if (char=? c1 c2) (f (fx+ i 1)) '>)))))))))

(define (smakesummary op)
  (let ((ls (sort (lambda (x y)
                    (case (key-cmp (summary-entry-key x) (summary-entry-key y))
                      [(<) #t]
                      [(>) #f]
                      [else (< (pageno->number (summary-entry-pageno x))
                               (pageno->number (summary-entry-pageno y)))]))
                  (reverse summary-entries))))
    (fprintf op "<table>~%")
    (fprintf op "<tr><th align=left>Form</th><th align=left>Category</th><th align=right>Page</th></tr>~%")
    (fprintf op "<tr><td colspan=3><hr></td></tr>")
    (for-each
      (lambda (entry)
        (fprintf op "<tr><td nowrap><tt>~a</tt></td><td>~a</td>"
          (summary-entry-form entry)
          (summary-entry-type entry))
        (fprintf op "<td align=right><a href=\"~a\">~a</a></td></tr>~%"
          (summary-entry-url entry)
          (summary-entry-pageno entry)))
      ls)
    (fprintf op "</table>~%")))

(define insert-file
  (lambda (fn op)
    (let ((ip (open-input-file fn)))
      (let loop ()
        (let ((c (read-char ip)))
          (unless (eof-object? c)
            (write-char c op)
            (loop)))))))

(make-undefined ip op def-env pending groups ips ops ifiles
   ofiles alphalist-counters undos labeltag)

(define-syntactic-monad P
  ip                   ; current input port
  op                   ; current output port
  def-env              ; definition environment
  pending              ; stack of pending stex "environments"
  groups               ; stack of pending groups
  ips                  ; input port stack; does not include ip
  ops                  ; output port stack; does not include op
  ifiles               ; stack of input files [(cons ip ops) w/o string ports]
  ofiles               ; stack of output files [(cons op ops) w/o string ports]
  alphalist-counters   ; stack of counters to support alphalist environment
  undos                ; stack of lists of thunks to call when group ends
  labeltag             ; current generated label tag
)

(define push-ifile
  (lambda (ip ifiles)
    (set! current-ifile ip)
    (cons ip ifiles)))
(define pop-ifile
  (lambda (ifiles)
    (let ((ifiles (cdr ifiles)))
      (unless (null? ifiles) (set! current-ifile (car ifiles)))
      ifiles)))

(define sinclude
  (P lambda (fn)
    (let ((new-ip
           (guard (c [#t (warningf #f "cannot open ~a" fn) #f])
             (open-input-file fn))))
      (if new-ip
          (P s0
            ([ip new-ip]
             [ips (cons ip ips)]
             [ifiles (push-ifile new-ip ifiles)]))
          (P s0)))))

(define sbegingroup
  (P lambda (g)
    (P s0
      ([groups (cons g groups)]
       [def-env (cons '() def-env)]
       [undos (cons '() undos)]))))

(define sendgroup
  (P lambda (g)
    (unless (eq? (car groups) g)
      (input-error "unmatched ~a"
        (if (eq? g 'bgroup) "close brace or egroup" "endgroup")))
    (for-each (lambda (x) (x)) (car undos))
    (P s0
      ([groups (cdr groups)]
       [def-env (cdr def-env)]
       [undos (cdr undos)]))))

(define s0
  (P lambda ()
    (state-case (c (read-char ip))
      [(#\\)
       (let ((cmd (read-command ip)))
         (suppress-white-space ip)
         (cond
           [(get-def cmd def-env) => (lambda (proc) (P proc))]
           [else (unexpected-command cmd)]))]
      [(#\~)
       (write-char #\space op)
       (P s0)]
      [(#\$)
       (s$ ip op)
       (P s0)]
      [(#\%) (scomment ip op) (P s0)]
      [(#\{) (P sbegingroup () 'group)]
      [(#\}) (P sendgroup () 'group)]
      [(#\newline)
       (write-char c op)
       (let loop ((par? #f))
         (state-case (c (peek-char ip))
           [(#\newline) (read-char ip) (write-char c op) (loop #t)]
           [(#\space #\tab) (read-char ip) (write-char c op) (loop par?)]
           [(#\%) (read-char ip) (scomment ip op) (loop par?)]
           [else (when par? (fprintf op "<p>~%")) (P s0)]))]
      [(eof)
       (close-input-port ip)
       (cond
         [(null? ips) (void)]
         [(eq? ip (car ifiles))
          (P s0 ([ip (car ips)] [ips (cdr ips)] [ifiles (pop-ifile ifiles)]))]
         [else
          (P s0 ([ip (car ips)] [ips (cdr ips)]))])]
      [else (write-char c op) (P s0)])))

;;; global state variables
(define code-op)
(define current-ifile) ; for the benefit of input-error
(define index-entries)
(define index-uid-counters)
(define latex-cache)
(define output-file-counters)
(define ox-op)
(define summary-entries)

(set! go
  (lambda (fn auxfn title)
    (read-aux-file auxfn)

    (let ((oxfn (format "~a.ox" fn)))
      (if (file-exists? oxfn)
          (load oxfn)
          (warningf #f "no ox file found"))
      (let* ((ip (open-input-file (format "~a.stex" fn)))
             (cp (open-output-file "code.ss" 'truncate)))
        (fluid-let ((code-op cp)
                    (current-ifile '())
                    (index-entries '())
                    (index-uid-counters '())
                    (latex-cache '())
                    (output-file-counters '())
                    (ox-op (open-output-file oxfn 'truncate))
                    (summary-entries '()))
         ; must bind output-file-counters before calling open-html-file
          (let ((op (open-html-file ip title)))
            (insert-file "code-header" cp)
            (P sinclude
              ([ip ip]
               [op op]
               [def-env '()]
               [pending '(top)]
               [groups '(top)]
               [ips '()]
               [ops '()]
               [ifiles (push-ifile ip '())]
               [ofiles (list op)]
               [alphalist-counters '()]
               [undos (list '())]
               [labeltag #f])
              base-macros-file)
          (close-html-port op)
          (close-output-port cp)))))))

(global-def def
  (let ((buf (open-output-string)))
    (P lambda ()
      (let* ((cmd (state-case (c (read-char ip))
                    [(#\\) (read-command ip)]
                    [else (input-error "invalid \\def syntax")]))
             (pattern (read-def-pattern ip))
             (template (read-bracketed-text ip)))
        (set-def! cmd def-env #f
          (P lambda ()
            (expand-template buf template
              (read-args ip pattern cmd) cmd)
            (P s0
              ([ip (open-input-string (get-output-string buf))]
               [ips (cons ip ips)])))))
    (P s0))))

(global-def newenvironment
  (P lambda ()
    (let* ((cmd (string->symbol (read-bracketed-text ip)))
           (argcnt (or (read-optional-arg ip) 0))
           (b (begin (suppress-white-space ip) (read-bracketed-text ip)))
           (e (begin (suppress-white-space ip) (read-bracketed-text ip))))
      (unless (= argcnt 0)
        (input-error "environment arguments are not supported"))
      (let ((endcmd (string->symbol (format "end~a" cmd))))
        (set-def! cmd def-env #f
          (P lambda ()
            (P s0 ([ip (open-input-string b)] [ips (cons ip ips)]))))
        (set-def! endcmd def-env #f
          (P lambda ()
            (P s0 ([ip (open-input-string e)] [ips (cons ip ips)]))))))
    (P s0)))

(global-def begin
  (P lambda ()
    (let ((cmd (string->symbol (read-bracketed-text ip))))
      (cond
        [(get-def cmd def-env) =>
           (lambda (proc) (P proc ([pending (cons cmd pending)])))]
        [else (input-error "undefined command \\begin{~a}" cmd)]))))

(global-def end
  (P lambda ()
    (let* ((cmd (string->symbol (read-bracketed-text ip)))
           (endcmd (string->symbol (format "end~a" cmd))))
      (unless (eq? (car pending) cmd)
        (input-error "unmatched \\end{~a}" cmd))
      (cond
        [(get-def endcmd def-env) =>
           (lambda (proc) (P proc ([pending (cdr pending)])))]
        [else (input-error "undefined command \\end{~a}" cmd)]))))

;;; built-in environments
(global-def enumerate ; built-in to coordinate with \item
  (P lambda ()
    (fprintf op "<ol>")
    (P s0)))

(global-def endenumerate
  (P lambda ()
    (fprintf op "</ol>")
    (P s0)))

(global-def itemize ; built-in to coordinate with \item
  (P lambda ()
    (fprintf op "<ul>")
    (P s0)))

(global-def enditemize
  (P lambda ()
    (fprintf op "</ul>")
    (P s0)))

(global-def alphalist
  (P lambda ()
    (fprintf op "<table>")
    (P s0 ([alphalist-counters (cons 0 alphalist-counters)]))))

(global-def endalphalist
  (P lambda ()
    (unless (= (car alphalist-counters) 0) (fprintf op "</td></tr>"))
    (fprintf op "</table>")
    (P s0 ([alphalist-counters (cdr alphalist-counters)]))))

(global-def eqnarray* ; no endeqnarray*---we finish the job here
  (P lambda ()
    (fprintf op "<p>~%")
    (seqnarray* ip op)
    (fprintf op "<p>~%")
    (P s0 ([pending (cdr pending)]))))

(global-def thebibliography
  (P lambda ()
    (read-optional-arg ip) ; ignore spacing argument
    (P s0)))

(global-def endthebibliography
  (P lambda ()
    (P s0)))

(global-def divertoutput
  (P lambda ()
    (let* ((level-str (or (read-optional-arg ip) "0"))
           (level (let ((i (string->number level-str))
                        (n (length ofiles)))
                    (and (fixnum? i)
                         (if (fx< -1 i n)
                             (fx- n i 1)
                             (and (fx<= (- n) i -1)
                                  (fx- -1 i)))))))
      (unless level
        (input-error "invalid \\outputlevel level ~a" level-str))
      (P s0 ([op (list-ref ofiles level)] [ops (cons op ops)])))))

(global-def enddivertoutput
  (P lambda ()
    (P s0 ([op (car ops)] [ops (cdr ops)]))))

(global-def begingroup
  (P lambda ()
    (P sbegingroup () 'begingroup)))

(global-def endgroup
  (P lambda ()
    (P sendgroup () 'begingroup)))

(global-def bgroup
  (P lambda ()
    (P sbegingroup () 'bgroup)))

(global-def egroup
  (P lambda ()
    (P sendgroup () 'bgroup)))

(global-def |[| ;]
  (P lambda ()
    (fprintf op "<p>~%")
    (smathdisplay ip op)
    (fprintf op "<p>~%")
    (P s0)))

(global-def raw
  (P lambda ()
    (display (read-bracketed-text ip) op)
    (P s0)))

(global-def newif
  (P lambda ()
    (snewif ip def-env)
    (P s0)))

(global-def newcounter
  (P lambda ()
    (let* ((name-str (read-bracketed-text ip))
           (counter (string->symbol name-str))
           (within (read-optional-arg ip)))
      (when (get-counter-value counter)
        (input-error "newcounter of existing counter ~a" counter))
      (when within
        (let ((within (string->symbol within)))
          (unless (get-counter-value within)
            (input-error "newcounter of ~a within unknown counter ~a"
              counter within))
          (add-subcounter! within counter)))
      (set-counter-value! counter 0)
      (set-def! (string->symbol (string-append "the" name-str)) def-env #f
        (P lambda ()
          (write (get-counter-value counter) op)
          (P s0))))
    (P s0)))

(global-def setcounter
  (P lambda ()
    (let* ((counter (string->symbol (read-bracketed-text ip)))
           (num-str (read-bracketed-text ip))
           (old-value (get-counter-value counter))
           (new-value (string->number num-str)))
      (unless old-value
        (input-error "setcounter of unknown counter ~a" counter))
      (unless new-value
        (input-error "invalid setcounter value ~a" num-str))
      (set-counter-value! counter new-value))
    (P s0)))

(global-def addtocounter
  (P lambda ()
    (let* ((counter (string->symbol (read-bracketed-text ip)))
           (num-str (read-bracketed-text ip))
           (old-value (get-counter-value counter))
           (incr (string->number num-str)))
      (unless old-value
        (input-error "addtocounter of unknown counter ~a" counter))
      (unless new-value
        (input-error "invalid addtocounter increment ~a" num-str))
      (set-counter-value! counter (+ old-value incr)))
    (P s0)))

(global-def stepcounter
  (P lambda ()
    (let* ((counter (string->symbol (read-bracketed-text ip)))
           (old-value (get-counter-value counter)))
      (unless old-value
        (input-error "~s of unknown counter ~a" cmd counter))
      (set-counter-value! counter (+ old-value 1))
      (for-each
        (lambda (x) (set-counter-value! x 0))
        (subcounters counter)))
    (P s0)))

(global-def item
  (P lambda ()
    (suppress-white-space ip)
    (case (car pending)
      [(enumerate itemize)
       (fprintf op "<li>")
       (P s0)]
      [(alphalist)
       (let ((n (car alphalist-counters)))
         (unless (= n 0) (fprintf op "</td></tr>"))
         (when (> n 25)
           (input-error "too many alphalist items"))
         (fprintf op "<tr valign=top><td>~c.</td><td>"
           (string-ref "abcdefghijklmnopqrstuvwxyz" n))
         (P s0 ([alphalist-counters (cons (+ n 1) (cdr alphalist-counters))])))]
      [else (input-error "\\item outside of proper environment")])))

(global-def pagebreak
  (P lambda ()
    (read-optional-arg ip) ; ignore [...]
    (P s0)))

(global-def var
  (P lambda ()
    (dovar ip op)
    (P s0)))

(global-def |'|
  (P lambda ()
    (state-case (c (read-char ip))
      [(#\e) (fprintf op "&eacute;")]
      [(#\o) (fprintf op "&oacute;")]
      [else (input-error "invalid \\' command \\'~a" c)])
    (P s0)))

(global-def vskip
  (P lambda ()
    ; it's a pain to parse tex amounts, so we choose to ignore
    ; everything up to the next line break instead...watch out!
    (let f ()
      (state-case (c (read-char ip))
        ((#\newline eof) (P s0))
        (else (f))))))

(global-def tt
  (P lambda ()
    (fprintf op "<tt>")
    (set-car! undos (cons (lambda () (fprintf op "</tt>")) (car undos)))
    (P s0)))

(global-def bf
  (P lambda ()
    (fprintf op "<b>")
    (set-car! undos (cons (lambda () (fprintf op "</b>")) (car undos)))
    (P s0)))

(global-def include
  (P lambda ()
    (P sinclude () (read-bracketed-text ip))))

(global-def newlabeltag
  (P lambda ()
    (P s0 ([labeltag (gensym)]))))

(global-def labeltag
  (P lambda ()
    (display labeltag op)
    (P s0)))

(global-def label
  (P lambda ()
    (unless labeltag
      (input-error "\label{} before \newlabeltag"))
    (slabel* (string->symbol (read-bracketed-text ip)) labeltag op)
    (P s0)))

(global-def ref
  (P lambda ()
    (sref ip op label-ref)
    (P s0)))

(global-def pageref
  (P lambda ()
    (sref ip op label-pageref)
    (P s0)))

(global-def primpageref
  (P lambda ()
    (sprimpageref ip op)
    (P s0)))

(global-def cite
  (P lambda ()
    (write-char #\[ op)
    (let ((keys (let ((sip (open-input-string (read-bracketed-text ip)))
                      (buf (open-output-string)))
                  (let loop ()
                    (state-case (c (read-char sip))
                      [(#\,)
                       (let ((key (get-output-string buf)))
                         (cons key (loop)))]
                      [(eof)
                       (list (get-output-string buf))]
                      [else
                       (write-char c buf)
                       (loop)])))))
      (do ((keys keys (cdr keys)) (sep "" ","))
          ((null? keys) (write-char #\] op))
          (let ((key (string->symbol (car keys))))
            (fprintf op "~a<a href=\"~a\">~a</a>"
              sep (get-url key) (get-cite key)))))
    (P s0)))

(global-def epsfbox
  (P lambda ()
    (fprintf op "<p>~%")
    (punt-to-latex (format "\\input{epsf.sty}\\epsfbox{~a}" (read-bracketed-text ip)) op)
    (fprintf op "<p>~%")
    (P s0)))

(global-def bibitem
  (P lambda ()
    (let ((key (string->symbol (read-bracketed-text ip))))
      (fprintf op "<p>[~a] " (slabel key (get-cite key) op)))
    (P s0)))

(global-def openhtmlfile
  (P lambda ()
    (let ((title-str (read-bracketed-text ip)))
      (let ((new-op (open-html-file (car ifiles) title-str)))
        (P s0
          ([op new-op]
           [ops (cons op ops)]
           [ofiles (cons new-op ofiles)]))))))

(global-def currentoutputfile
  (P lambda ()
    (display (port-name (car ofiles)) op)
    (P s0)))

(global-def closehtmlfile
  (P lambda ()
    (unless (eq? op (car ofiles))
      (input-error "invalid context for \\closehtmlfile"))
    (when (null? ops)
      (input-error "\\closehtmlfile cannot close primary html file"))
    (close-html-port op)
    (P s0 ([op (car ops)] [ops (cdr ops)] [ofiles (cdr ofiles)]))))

(global-def formdef
  (P lambda ()
    (read-open-brace ip)
    (sformdef ip op #f)
    (P s0)))

(global-def suppress
  (P lambda ()
    (unless (state-case (c (read-char ip))
              [(#\\) (eq? (read-command ip) 'formdef)]
              [else #f])
      (input-error "expected \\formdef following \\suppress"))
    (read-open-brace ip)
    (sformdef ip op #t)
    (P s0)))

(global-def scheme
  (P lambda ()
    (read-open-brace ip)
    (display "<tt>" op)
    (sscheme ip op)
    (display "</tt>" op)
    (P s0)))

(global-def schemedisplay
  (P lambda ()
    ; trailing whitespace has been suppressed
    (state-case (c (peek-char ip))
      [(#\\)
       (read-char ip)
       (let ((cmd (read-command ip)))
         (unless (eq? cmd 'label)
           (input-error
             "unexpected \\~a following \\schemedisplay" cmd)))
       (display (slabel (string->symbol (read-bracketed-text ip)) "" op)
                op)]
      [else (void)])
    (display "<pre>" op)
    (sschemedisplay ip op)
    (display "</pre>" op)
    (P s0)))

(global-def index
  (P lambda ()
    (read-open-brace ip)
    (sindex ip op)
    (P s0)))

'(global-def makeindex
  (P lambda ()
    (smakeindex op)))

(global-def makeindex
  (P lambda ()
    (let ((buf (open-output-string)))
      (smakeindex buf)
      (P s0 ([ip (open-input-string (get-output-string buf))]
             [ips (cons ip ips)])))))

(global-def makesummary
  (P lambda ()
    (smakesummary op)
    (P s0)))

(new-conditional 'ifhtml '() #t #f #f)
(new-conditional 'iflatex '() #f #f #f)
))

