;; Copyright (c) 1996 Carl Bruggeman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;; html.ss
;;;
;;; parser macros from read.ss which is Copyright (C) 1996 R. Kent Dybvig

;;
;; html.ss is a lexical scanner for html.  It returns single html tokens
;; (e.g., <A HREF="xxx">, </A>, <B>, etc).
;; 


(eval-when (compile load) (current-expand sc-expand))

(let ()
(define intern string->symbol)

;;; A state-case expression must take the following form:
;;;   (state-case var eof-clause clause ... else-clause)
;;; eof-clause and else-clause must take the form
;;;   (eof exp1 exp2 ...)
;;;   (else exp1 exp2 ...)
;;; and the remaining clauses must take the form
;;;   (char-set exp1 exp2 ...)
;;; The value of var must be an eof object or a character.
;;; state-case selects the first clause matching the value of var and
;;; evaluates the expressions exp1 exp2 ... of that clause.  If the
;;; value of var is an eof-object, eof-clause is selected.  Otherwise,
;;; the clauses clause ... are considered from left to right.  If the
;;; value of var is in the set of characters defined by the char-set of
;;; a given clause, the clause is selected.  If no other clause is
;;; selected, else-clause is selected.

;;; char-set may be
;;;   * a single character, e.g., #\a, or
;;;   * a list of subkeys, each of which is
;;;     - a single character, or
;;;     - a character range, e.g., (#\a - #\z)
;;; For example, (#\$ (#\a - #\z) (#\A - #\Z)) specifies the set
;;; containing $ and the uppercase and lowercase letters.

(define-syntax state-case
  (lambda (x)
    (define state-case-test
      (lambda (cvar k)
        (with-syntax ((cvar cvar))
          (syntax-case k (-)
            (char
             (char? (syntax-object->datum #'char))
             #'(char=? cvar char))
            ((char1 - char2)
             (and (char? (syntax-object->datum #'char1))
                  (char? (syntax-object->datum #'char2)))
             #'(char<=? char1 cvar char2))))))
    (define state-case-help
      (lambda (cvar clauses)
        (syntax-case clauses (else)
          (((else exp1 exp2 ...))
           #'(begin exp1 exp2 ...))
          ((((k ...) exp1 exp2 ...) . more)
           (with-syntax (((test ...)
                          (map (lambda (k) (state-case-test cvar k))
                               #'(k ...)))
                         (rest (state-case-help cvar #'more)))
             #'(if (or test ...) (begin exp1 exp2 ...) rest)))
          (((k exp1 exp2 ...) . more)
           (with-syntax ((test (state-case-test cvar #'k))
                         (rest (state-case-help cvar #'more)))
             #'(if test (begin exp1 exp2 ...) rest))))))
    (syntax-case x (eof)
      ((_ cvar (eof exp1 exp2 ...) more ...)
       (identifier? #'cvar)
       (with-syntax ((rest (state-case-help #'cvar #'(more ...))))
         #'(if (eof-object? cvar)
               (begin exp1 exp2 ...)
               rest))))))

;;; xdefine, xcall, xmvcall, and xvalues manage the implicit arguments
;;; received by most of the procedures defined in this file.  This
;;; simplifies the code and makes it much easier to add new arguments
;;; universally.  The implicit identifiers are:
;;;   ip: input port
;;;   fp: current file position
;;;   tb: token buffer

(define-syntax with-implicit
  (lambda (x)
    (syntax-case x ()
      ((_ (tid id ...) . body)
       (andmap identifier? #'(tid id ...))
       #'(with-syntax ((id (datum->syntax-object #'tid 'id)) ...) . body)))))

(let ()
(define-syntax xdefine
  (lambda (x)
    (syntax-case x ()
      ((key (name . args) . body)
       (with-implicit (key ip fp tb pre?)
         #'(define (name ip fp tb pre? . args) . body))))))

(define-syntax xcall
  (lambda (x)
    (syntax-case x ()
      ((key p arg ...)
       (with-implicit (key ip fp tb pre?)
         #'(p ip fp tb pre? arg ...))))))

(define-syntax xmvlet
  (lambda (x)
    (syntax-case x ()
      ((key ((arg ...) exp) . body)
       (with-implicit (key ip fp tb pre?)
         #'(call-with-values
             (lambda () exp)
             (lambda (ip fp tb pre? arg ...) . body)))))))

(define-syntax xvalues
  (lambda (x)
    (syntax-case x ()
      ((key arg ...)
       (with-implicit (key ip fp tb pre?)
         #'(values ip fp tb pre? arg ...))))))

;;; token-buffers are represented as strings and are expanded as necessary
;;; by the stretch form, which otherwise behaves much like string-set!
(define-syntax with-stretch-buffer
  (lambda (x)
    (syntax-case x ()
      ((key i c . body)
       (with-implicit (key tb)
         #'(let ((g i))
             (let ((tb (if (fx= g (string-length tb)) (string-stretch tb 0) tb)))
               (string-set! tb g c)
               . body)))))))

(define-syntax with-stretch-buffer-N
  (lambda (x)
    (syntax-case x ()
      ((key i s . body)
       (with-implicit (key tb)
         #'(let ((gi i) (gs s))
             (let ((len (string-length gs)))
               (let ((tb (if (fx>= (+ gi len) (string-length tb))
                             (string-stretch tb len)
                             tb)))
                 (do ([j gi (fx+ j 1)]
                      [k 0 (fx+ k 1)])
                     ((fx= k len))
                   (string-set! tb j (string-ref gs k)))
                 . body))))))))

;;; with-read-char, with-peek-char, and with-unread-char manage the fp
;;; (file-position) value
(define-syntax with-read-char
  (lambda (x)
    (syntax-case x ()
      ((key id . body)
       (identifier? #'id)
       (with-implicit (key ip fp)
         #'(let ((id (read-char ip)) (fp (fx+ fp 1))) . body))))))

(define-syntax with-peek-char
  (lambda (x)
    (syntax-case x ()
      ((key id . body)
       (identifier? #'id)
       (with-implicit (key ip)
         #'(let ((id (peek-char ip))) . body))))))

(define-syntax with-unread-char
  (lambda (x)
    (syntax-case x ()
      ((key e . body)
       (with-implicit (key ip fp)
         #'(begin (unread-char e ip) (let ((fp (fx- fp 1))) . body)))))))

;;; define-state, state-return, *state, *state-case, and *state-return
;;; manage the continuation and beginning file position (bfp) passed to
;;; all states in the lexical scanner.  They are defined in terms of
;;; xdefine and xcall.  We don't use true mrvs because we end up with
;;; producers containing mv calls, which the compiler currently punts
;;; on (allocates closures for).

(define-syntax define-state
  (lambda (x)
    (syntax-case x ()
      ((key (name arg ...) . body)
       (with-implicit (key xdefine bfp k)
         #'(xdefine (name bfp k arg ...) . body))))))

(define-syntax state-return
  (lambda (x)
    (syntax-case x ()
      ((key type value)
       (with-implicit (key xcall bfp k)
         #'(xcall k bfp 'type value))))))

(define-syntax *state ; move to state
  (lambda (x)
    (syntax-case x ()
      ((key s arg ...)
       (with-implicit (key xcall bfp k)
         #'(xcall s bfp k arg ...))))))

(define-syntax *state/k ; call state w/explicit continuation
  (lambda (x)
    (syntax-case x ()
      ((key s k arg ...)
       (with-implicit (key xcall bfp)
         #'(xcall s 0 k arg ...))))))

;;; define-parser and *parser manage the beginning file position, token
;;; type, and token value passed to all parser continuations.  They are
;;; defined in terms of xdefine and xcall.

(define-syntax define-parser
  (lambda (x)
    (syntax-case x ()
      ((key name . body)
       (with-implicit (key xdefine bfp type value)
         #'(xdefine (name bfp type value) . body))))))

(define-syntax *parser
  (lambda (x)
    (syntax-case x ()
      ((key p)
       (with-implicit (key xcall bfp type value)
         #'(xcall p bfp type value))))))

(define string-stretch
  (lambda (old inc)
    (let loop ([n (string-length old)])
      (let ([s (fx+ n (fx+ n 5))])
        ; being careful about how much we stretch the buffer keeps the 
        ; actual allocated size of the buffer a power of 2
        (cond
          [(fx= n 0)
           ; choose initial size to properly align string, accounting for
           ; 8-byte alignment, 4-byte length, and one null byte
           (if (fx> inc 11) (loop 11) (make-string 11))]
          [(fx> (fx+ inc n) s)
           (loop s)]
          [else
           (let ([new (make-string s)] 
                 [len (string-length old)])
             (do ([i 0 (fx+ i 1)])
                 ((fx= i len) new)
               (string-set! new i (string-ref old i))))])))))

(let ()

(define-state (rd-html-token)
  (with-read-char c
    (state-case c
      [eof (state-return eof c)]
      [(#\<) (*state rd-token)]
      [(#\&) (*state rd-entity 0 c)]
      [(#\space #\tab #\newline #\return #\page)
       (if pre?
           (*state rd-data 0 c) 
           (*state rd-whitespace 0 c))]
      [else (*state rd-data 0 c)])))

(define-state (rd-whitespace i c)
  (state-case c
    [eof 
     (with-stretch-buffer i #\space
       (state-return data (substring tb 0 (fx+ i 1))))]
    [(#\space #\tab #\newline #\return #\page)
     (with-read-char c
       (*state rd-whitespace i c))]
    [else
     (with-stretch-buffer i #\space
       (*state rd-data (fx+ i 1) c))]))
  
(define-state (rd-data i c)
  (state-case c
    [eof (state-return data (substring tb 0 i))]
    [(#\< #\&)
     (with-unread-char c
       (state-return data (substring tb 0 i)))]
    [(#\space #\tab #\newline #\return #\page)
     (cond
       [pre? (with-stretch-buffer i c
               (with-read-char c
                 (*state rd-data (fx+ i 1) c)))]
       [else (with-read-char c (*state rd-whitespace i c))])]
    [else
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-data (fx+ i 1) c)))]))

(define-state (rd-token)
  (with-stretch-buffer 0 #\< 
    (with-read-char c
      (*state rd-name rd-start-tag rd-token-rest 1 c))))

(define-state (rd-token-rest i c)
  (state-case c
    [eof (*state rd-data 1 c)]
    [(#\/) (with-stretch-buffer 1 c
             (with-read-char c
               (*state rd-name rd-end-tag rd-data 2 c)))]
    [(#\!) (with-stretch-buffer 1 c
             (with-read-char c 
               (*state rd-token-decl 2 c)))]
    [(#\?) (with-stretch-buffer 1 c
             (with-read-char c
               (*state rd-token-proc-instr 2 c)))]
    [else (*state rd-data 1 c)]))

(define-state (rd-name k-succ k-fail i c)
  (state-case c
    [eof (*state k-fail i c)]
    [((#\a - #\z) (#\A - #\Z))
     (*state rd-name-rest k-succ i i c)]
    [else
     (*state k-fail i c)]))
  
(define-state (rd-name-rest k-succ i j c)
  (with-stretch-buffer j c
    (with-read-char c
      (state-case c
        [eof (*state rd-data (fx+ j 1) c)]
        [((#\a - #\z) (#\A - #\Z) (#\0 - #\9) #\. #\- #\_)
         (*state rd-name-rest k-succ i (fx+ j 1) c)]
        [else
         (with-unread-char c
           (*state k-succ (fx+ j 1) (extr-name tb i (fx+ j 1))))]))))

(define-state (rd-start-tag i tag)
  (*state rd-start-tag-rest i tag '()))

(define-state (rd-start-tag-rest i tag attr-list)
  (with-read-char c
    (state-case c
      [eof   (state-return start-tag (cons tag (reverse attr-list)))]
      [(#\>) (state-return start-tag (cons tag (reverse attr-list)))]
      [(#\space #\tab #\newline #\return #\page)
       (with-stretch-buffer i c
         (*state rd-start-tag-rest (fx+ i 1) tag attr-list))]
      [else (*state rd-attr-list i c tag attr-list)])))
         
(define-state (rd-attr-list i c tag attr-list)
  (define-state (rd-equal1 i name)
    (with-read-char c
      (state-case c
        [eof (*state rd-data i c)]
        [(#\space #\tab #\newline #\return #\page)
         (with-stretch-buffer i c
           (*state rd-equal1 (fx+ i 1) name))]
        [(#\=)
         (with-stretch-buffer i c
           (*state rd-equal2 (fx+ i 1) name))]
        [(#\>)
         (with-unread-char c
           (*state rd-start-tag-rest i tag (cons name attr-list)))]
        [else (*state rd-attr-list i c tag (cons name attr-list))])))
  (define-state (rd-equal2 i name)
    (with-read-char c
      (state-case c 
        [eof (*state rd-data i c)]
        [(#\space #\tab #\newline #\return #\page)
         (with-stretch-buffer i c
           (*state rd-equal2 (fx+ i 1) name))]
        [else (*state rd-value i c name)])))
  (define-state (rd-value i c name)
    (with-stretch-buffer i c
      (state-case c
        [eof   (*state rd-value-str i i c name)]
        [(#\") (*state rd-value-dquote i (fx+ i 1) c name)]
        [(#\') (*state rd-value-squote i (fx+ i 1) c name)]
        [else  (*state rd-value-str i (fx+ i 1) c name)])))
  (define-state (rd-value-dquote i j c name)
    (with-read-char c
      (state-case c
        [eof (*state rd-data j c)]
        [(#\>) (*state rd-value-end (fx+ i 1) j c name)]
        [(#\") (*state rd-value-end+ (fx+ i 1) j c name)]
        [else  (with-stretch-buffer j c
                 (*state rd-value-dquote i (fx+ j 1) c name))])))
  (define-state (rd-value-squote i j c name)
    (with-read-char c
      (state-case c
        [eof (*state rd-data j c)]
        [(#\>) (*state rd-value-end (fx+ i 1) j c name)]
        [(#\') (*state rd-value-end+ (fx+ i 1) j c name)]
        [else  (with-stretch-buffer j c
                 (*state rd-value-squote i (fx+ j 1) c name))])))
  (define-state (rd-value-str i j c name)
    (with-read-char c
      (state-case c
        [eof (*state rd-data j c)]
        [(#\>) (*state rd-value-end i j c name)]
        [(#\space #\tab #\newline #\return #\page)
         (*state rd-value-end i j c name)]
        [else  (with-stretch-buffer j c
                 (*state rd-value-str i (fx+ j 1) c name))])))
  (define-state (rd-value-end i j c name)
    (with-unread-char c
      (let ([attr (cons name (substring tb i j))])
        (*state rd-start-tag-rest j tag (cons attr attr-list)))))
  (define-state (rd-value-end+ i j c name)
    (with-stretch-buffer j c
      (let ([attr (cons name (substring tb i j))])
        (*state rd-start-tag-rest j tag (cons attr attr-list)))))
  (*state rd-name rd-equal1 rd-data i c))

(define-state (rd-end-tag i name)
  (with-read-char c
    (state-case c
      [eof   (*state rd-data i c)]
      [(#\>) (state-return end-tag name)]
      [(#\space #\tab #\newline #\return #\page)
       (with-stretch-buffer i c (*state rd-end-tag i name))]
      [else (*state rd-data i c)])))

(define-state (rd-token-decl i c)
  (state-case c
    [eof   (*state rd-data i c)]
    [(#\>) (state-return decl (substring tb 2 i))]
    [(#\-) (with-stretch-buffer i c
             (with-read-char c
               (*state rd-comment-1 (fx+ i 1) c)))]
    [else  (with-stretch-buffer i c
             (with-read-char c
               (*state rd-token-decl (fx+ i 1) c)))]))

(define-state (rd-comment-1 i c)
  (state-case c
    [eof   (*state rd-data i c)]
    [(#\-) (with-stretch-buffer i c
             (with-read-char c
               (*state rd-comment-2 (fx+ i 1) c)))]
    [else (*state rd-data i c)]))

(define-state (rd-comment-2 i c)
  (state-case c
    [eof   (*state rd-data i c)]
    [(#\-) (with-stretch-buffer i c
             (with-read-char c
               (*state rd-comment-3 (fx+ i 1) c)))]
    [else  (with-stretch-buffer i c
             (with-read-char c
               (*state rd-comment-2 (fx+ i 1) c)))]))

(define-state (rd-comment-3 i c)
  (state-case c
    [eof   (*state rd-data i c)]
    [(#\-) (with-stretch-buffer i c
             (with-read-char c
               (*state rd-token-decl (fx+ i 1) c)))]
    [else  (with-stretch-buffer i c
             (with-read-char c
               (*state rd-comment-2 (fx+ i 1) c)))]))

(define-state (rd-token-proc-instr i c)
  (state-case c
    [eof   (*state rd-data i c)]
    [(#\>) (state-return proc (substring tb 2 i))]
    [else  (with-stretch-buffer i c
             (with-read-char c
               (*state rd-token-proc-instr (fx+ i 1) c)))]))

(define-state (rd-entity i c)
  (with-stretch-buffer i c
    (with-read-char c
      (state-case c
        [eof   (*state rd-data (fx+ i 1) c)]
        [(#\;) (*state rd-data (fx+ i 1) c)]
        [((#\a - #\z) (#\A - #\Z)) (*state rd-entity-name (fx+ i 1) c)]
        [(#\#) (with-stretch-buffer (fx+ i 1) c
                 (with-read-char c
                   (*state rd-entity-num (fx+ i 2) c)))]
        [else (*state rd-data (fx+ i 1) c)]))))
        
(define-state (rd-entity-name i c)
  (with-stretch-buffer i c
    (with-read-char c
      (state-case c
        [eof (state-return entity-name (substring tb 1 (fx+ i 1)))]
        [(#\;) (state-return entity-name (substring tb 1 (fx+ i 1)))]
        [((#\a - #\z) (#\A - #\Z) (#\0 - #\9))
         (*state rd-entity-name (fx+ i 1) c)]
        [else (with-unread-char c
                (state-return entity-name (substring tb 1 (fx+ i 1))))]))))

(define-state (rd-entity-num i c)
  (with-stretch-buffer i c
    (with-read-char c
      (state-case c
        [eof (state-return entity-num (extr-num tb 2 i))]
        [((#\0 - #\9)) (*state rd-entity-num (fx+ i 1) c)]
        [(#\;) (state-return entity-num (extr-num tb 2 i))]
        [else (with-unread-char c
                (state-return entity-num (extr-num tb 2 i)))]))))

(define-parser parse-html
  (case type
   [(data) value]
   [(proc) (make-token type value)]
   [(decl) (make-token type value)]
   [(end-tag) (make-token type value)]
   [(start-tag) (make-token type value)]
   [(entity-name)
    ;;;(or (getprop (intern value) 'html-entity)  ;; johnz: returning the num is useless
    (make-token type value)
   ;;; )
   ]
   [(entity-num) (string (integer->char value))]
   [(eof) value]
   [else (assertion-violationf 'parse-html "bad token type (~s) for value: ~s~%" type value)]))

(define extr-name
  (lambda (str start end)
    (let ([s (make-string (- end start))])
      (do ([i start (fx+ i 1)]
           [j 0 (fx+ j 1)])
          ((fx= i end))
        (string-set! s j (char-downcase (string-ref str i))))
      (intern s))))

(define extr-num
  (lambda (str i j)
    (string->number (substring str i (fx+ j 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parsing URLs

(define-state (rd-URI)
  (with-read-char c
    (state-case c
      [eof (*state rd-http-path 'rel "" #f 0 0 c)]
      [((#\a - #\z) (#\A - #\Z))
       (*state rd-URI-type 0 c)]
      [else
       (*state rd-http-path 'rel "" #f 0 0 c)])))

(define-state (rd-URI-type i c)
  (state-case c
    [eof (*state rd-http-path 'rel "" #f 0 i c)]
    [(#\:) (*state rd-URI-rest (intern (substring tb 0 i)))]
    [((#\a - #\z) (#\A - #\Z))
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-URI-type (fx+ i 1) c)))]
    [else (*state rd-http-path 'rel "" #f 0 i c)]))

(define-state (rd-URI-rest type)
  (case type
    [(http)   (*state rd-URL-addr rd-http-path type 0)]
    [(ftp)    (*state rd-URL-addr rd-ftp-path type 0)]
    [(gopher) (*state rd-URL-addr rd-gopher-path type 0)]
    [(file)   (*state rd-info type 0)]
    [(mailto) (*state rd-info type 0)]
    [(news)   (*state rd-info type 0)]
    [else (*state rd-info type 0)]))

(define-state (rd-URL-addr k-succ type i)
  (with-read-char c
    (state-case c
      [eof (*state rd-info 'URL-error i)]
      [(#\/) (with-stretch-buffer i c
               (*state rd-URL-addr1 k-succ type (fx+ i 1)))]
      [else (*state rd-http-path 'rel "" #f 0 0 c)])))

(define-state (rd-URL-addr1 k-succ type i)
  (with-read-char c
    (state-case c
      [eof (*state rd-info 'URL-error i)]
      [(#\/) (with-stretch-buffer i c
               (*state rd-URL-addr2 k-succ type (fx+ i 1)))]
      [else (with-stretch-buffer i c
              (*state rd-info 'URL-error (fx+ i 1)))])))

(define-state (rd-URL-addr2 k-succ type i)
  (with-read-char c
    (state-case c
      [eof (*state rd-info 'URL-error i)]
      [((#\a - #\z) (#\A - #\Z) (#\0 - #\9))
       (with-stretch-buffer i c
         (*state rd-URL-addr3 k-succ type (fx+ i 1)))]
      [else (with-stretch-buffer i c
              (*state rd-info 'URL-error (fx+ i 1)))])))

(define-state (rd-URL-addr3 k-succ type i)
  (with-read-char c
    (state-case c
      [eof (*state k-succ type (substring tb 2 i) #f 0 0 c)]
      [(#\:) (*state rd-URL-port k-succ type (substring tb 2 i) 0)]
      [(#\/) (*state k-succ type (substring tb 2 i) #f 0 0 c)]
      [else (with-stretch-buffer i c
              (*state rd-URL-addr3 k-succ type (fx+ i 1)))]))) 

(define-state (rd-URL-port k-succ type addr i)
  (with-read-char c
    (state-case c
      [eof (*state k-succ type addr (string->number (substring tb 0 i)) 0 0 c)]
      [((#\0 - #\9))
       (with-stretch-buffer i c
         (*state rd-URL-port k-succ type addr (fx+ i 1)))]
      [else
       (*state k-succ type addr (string->number (substring tb 0 i)) 0 0 c)])))
    
(define-state (rd-ftp-path type addr port i j c)
  (*state rd-http-path type addr port i j c))

(define-state (rd-gopher-path type addr port i j c)
  (*state rd-http-path type addr port i j c))

(define-state (rd-http-path type addr port i j c)
  (state-case c
    [eof (*state rd-location type addr port (substring tb i j) 0)]
    [(#\#) (*state rd-location type addr port (substring tb i j) 0)]
    [(#\?) (*state rd-query type addr port (substring tb i j) 0)]
    [else (with-stretch-buffer j c
            (with-read-char c
              (*state rd-http-path type addr port i (fx+ j 1) c)))]))

(define-state (rd-location type addr port path i)
  (with-read-char c
    (state-case c
      [eof (let ([location (if (fx= 0 i) #f (substring tb 0 i))])
             (state-return URL (make-URL type addr port path location #f)))]
      [else (with-stretch-buffer i c
              (*state rd-location type addr port path (fx+ i 1)))])))

(define-state (rd-query type addr port path i)
  (with-read-char c
    (state-case c
      [eof (let ([query (if (fx= 0 i) #f (substring tb 0 i))])
             (state-return URL (make-URL type addr port path #f query)))]
      [else (with-stretch-buffer i c
              (*state rd-query type addr port path (fx+ i 1)))])))

(define-state (rd-info type i)
  (with-read-char c
    (state-case c
      [eof (state-return URL (make-URL type "" #f (substring tb 0 i) #f #f))]
      [else (with-stretch-buffer i c
              (*state rd-info type (fx+ i 1)))])))
  
(define-parser parse-URI
  (case type
   [(URL) value]
   [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example: query?pg=aq&what=web&fmt=.&q=a%0D%0Ab&r=&d0=&d1=

(define-state (wr-uri args i)
  (with-read-char c
    (state-case c
      [eof (*state wr-uri-next args i)]
      [((#\A - #\Z) (#\a - #\z) (#\0 - #\9))
       (with-stretch-buffer i c
         (*state wr-uri args (fx+ i 1)))]
      [(#\space)
       (with-stretch-buffer i #\+
         (*state wr-uri args (fx+ i 1)))]
      [(#\newline)
       (with-stretch-buffer-N i "%0D%0A"
         (*state wr-uri args (fx+ i 6)))]
      [else
       (let ([c1 (string-ref hex-digits (fxsra (char->integer c) 4))]
             [c2 (string-ref hex-digits (fxlogand 15 (char->integer c)))])
         (with-stretch-buffer i #\%
           (with-stretch-buffer (fx+ i 1) c1
             (with-stretch-buffer (fx+ i 2) c2
               (*state wr-uri args (fx+ i 3))))))])))

(define-state (wr-uri-next args i)
  (if (null? args)
      (substring tb 0 i)
      (let ([ip (open-input-string (car args))])
        (*state wr-uri (cdr args) i))))

(define-parser parse-wr-uri
  (case type
    [(URL-encoded) value]
    [else #f]))

(define hex-digits "0123456789ABCDEF")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define entity-equivs
  `(("lt" . 60) ("gt" . 62) ("amp" . 38) ("quot" . 34)

    ("nbsp"   . 160) ("iexcl"  . 161) ("cent"   . 162) ("pound"  . 163)
    ("curren" . 164) ("yen"    . 165) ("brvbar" . 166) ("sect"   . 167)
    ("uml"    . 168) ("copy"   . 169) ("ordf"   . 170) ("laquo"  . 171)
    ("not"    . 172) ("shy"    . 173) ("reg"    . 174) ("macr"   . 175)
    ("deg"    . 176) ("plusmn" . 177) ("sup2"   . 178) ("sup3"   . 179)
    ("acute"  . 180) ("micro"  . 181) ("para"   . 182) ("middot" . 183)
    ("cedil"  . 184) ("sup1"   . 185) ("ordm"   . 186) ("raquo"  . 187)
    ("frac14" . 188) ("frac12" . 189) ("frac34" . 190) ("iquest" . 191)
    ("times"  . 215) ("div"    . 247)

    ("AElig"  . 198) ("Aacute" . 193) ("Acirc"  . 194) ("Agrave" . 192)
    ("Aring"  . 197) ("Atilde" . 195) ("Auml"   . 196) ("Ccedil" . 199)
    ("ETH"    . 208) ("Eacute" . 201) ("Ecirc"  . 202) ("Egrave" . 200)
    ("Euml"   . 203) ("Iacute" . 205) ("Icirc"  . 206) ("Igrave" . 204)
    ("Iuml"   . 207) ("Ntilde" . 209) ("Oacute" . 211) ("Ocirc"  . 212)
    ("Ograve" . 210) ("Oslash" . 216) ("Otilde" . 213) ("Ouml"   . 214)
    ("THORN"  . 222) ("Uacute" . 218) ("Ucirc"  . 219) ("Ugrave" . 217)
    ("Uuml"   . 220) ("Yacute" . 221) ("aacute" . 225) ("acirc"  . 226)
    ("aelig"  . 230) ("agrave" . 224) ("aring"  . 229) ("atilde" . 227)
    ("auml"   . 228) ("ccedil" . 231) ("eacute" . 233) ("ecirc"  . 234)
    ("egrave" . 232) ("eth"    . 240) ("euml"   . 235) ("iacute" . 237)
    ("icirc"  . 238) ("igrave" . 236) ("iuml"   . 239) ("ntilde" . 241)
    ("oacute" . 243) ("ocirc"  . 244) ("ograve" . 242) ("oslash" . 248)
    ("otilde" . 245) ("ouml"   . 246) ("szlig"  . 223) ("thorn"  . 254)
    ("uacute" . 250) ("ucirc"  . 251) ("ugrave" . 249) ("uuml"   . 252)
    ("yacute" . 253) ("yuml"   . 255)
  ))

(for-each
  (lambda (x)
    (putprop (intern (car x)) 'html-entity (string (integer->char (cdr x)))))
  entity-equivs)

(set! read-html-token
  (case-lambda
    [(ip) (read-html-token ip #f)]
    [(ip pre?)
     (unless (input-port? ip)
       (assertion-violationf 'read "~s is not an input port" ip))
     (let ((tb "") (fp (most-negative-fixnum)) (pre? pre?))
       (*state/k rd-html-token parse-html))]))

(set! parse-URL
  (lambda (URI)
    (unless (string? URI)
      (assertion-violationf 'parse-URL "~s is not a string" URI))
    (let ([ip (open-input-string URI)])
      (let ((tb "") (fp (most-negative-fixnum)) (pre? #f))
         (*state/k rd-URI parse-URI)))))

(set! form-urlencode
  (lambda args
    (let ([ip 'uninit] [tb ""] [fp (most-negative-fixnum)] [pre? #f])
       (*state/k wr-uri-next parse-wr-uri args 0))))

))) ;let

(define-structure (token type value))
(define-structure (URL type addr port path loc query))

(define merge-URL
  (lambda (base URI)
    (let ([URL (parse-URL URI)])
        (case (URL-type URL)
          [rel (unless (URL? base) 
                 (assertion-violationf 'merge-URL
                        "can't use a relative address without a base URL"))
               (let ([base-type (URL-type base)]
                     [base-addr (URL-addr base)]
                     [base-port (URL-port base)]
                     [loc (URL-loc URL)]
                     [query (URL-query URL)])
                 (let ([fn (path-merge (URL-path base) (URL-path URL))])
                   (make-URL base-type base-addr base-port fn loc query)))]
          [http (when (eqv? (URL-path URL) "")
                  (set-URL-path! URL "/"))
                URL]
          [else URL]))))

(define path-merge
  (lambda (base file)
    (let ([len (string-length base)])
      (cond
        [(eqv? file "") (if (eqv? base "") "/" base)]
        [(char=? (string-ref file 0) #\/) file]
        [(and (>= len 2)
              (char=? (string-ref base (- len 1)) #\.)
              (or (char=? (string-ref base (- len 2)) #\/)
                  (and (char=? (string-ref base (- len 2)) #\.)
                       (char=? (string-ref base (- len 3)) #\/))))
         ; base ends in "/.." or "/."
         (string-append base "/" file)]
        [else (string-append (path-basename base) "/" file)]))))

(define path-basename
  (lambda (fn)
    (let loop ([i (- (string-length fn) 1)])
      (cond
        [(< i 0) ""]
        [(char=? (string-ref fn i) #\/)
         (substring fn 0 i)]
        [else (loop (fx- i 1))]))))

(define URL->string
  (lambda (URL)
    (string-append
      (symbol->string (URL-type URL))
      (if (memq (URL-type URL) '(mailto news)) ":" "://")
      (URL-addr URL)
      (let ([port (URL-port URL)])
        (if port (format ":~a" port) ""))
      (URL-path URL)
      (let ([loc (URL-loc URL)])
        (if loc (format "#~a" loc) ""))
      (let ([query (URL-query URL)]) 
        (if query (format "?~a" query) "")))))
