;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;; expects to read boot file name from stdin

;; Presently not enabling case-sensitivity because it caused problems
;; with the html viewer (e.g. opening www.google.com made it run out
;; of memory and then abort.

(define-syntax ctdef ; compile-time definitions
  (syntax-rules ()
    [(_ x e) (meta define x e)]))

(define-syntax ctdef ; compile-time definitions  (rough meta-module support)
  ; use the #%$ versions to bypass (interaction-environment)
  (lambda (x)
    (syntax-case x ()
      [(_ x e)
       (let ([genny (gensym)])
         (with-syntax ([genny (datum->syntax-object #'x genny)])
           #'(define-syntax x
               (begin
                 (eval-when (compile load eval)
                   (#%$set-top-level-value! 'genny (rec x e)))
                 (identifier-syntax (#%$top-level-value 'genny))))))])))

;;; borrowed from ../../nt/cat.ss
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

(define boot-file (read-filename))

(printf "
*******************
NOTE:
  * changed make.ss so that it always compiles file anyway
    since we took out eval-when visit around the various imports
    of the modules --- only importing when compile
     ===> had to do this so that we wouldn't import when we load
          the boot file (this was so that we wouldn't end up with
          all the supposedly hidden module things suddenly not hidden)
*******************
")
(define fremd-compile-file
  (if #t ; (memq (machine-type) '(i3nt ppcnt))
      #%compile-file
      (let ([force-recompile? #f]) ; must recompile rest if we recompile one
        (lambda (filename)         ; no fancy dependency stuff yet
          (define recompile?
            (lambda ()
              (or force-recompile?
                  (let ([ip (car (process (format "ls -t ~a.ss ~a.so" filename filename)))]
                        [len (+ (string-length filename) 3)])
                    (let ([s (make-string len)])
                      (not (and (eqv? (block-read ip s len) len)
                                (string=? s (format "~a.so" filename)))))))))
          (when (recompile?)
            (set! force-recompile? #t)
            (#%compile-file filename))))))

(define fremd-load-list '())
(define fremd-load
  (lambda (path)
    (define massage-path
     ; squashes out ., .., and empty components where possible
      (lambda (path return)
        (define split-path
          (lambda (path)
            (let ([end (string-length path)])
              (let f ((start 0) (next 0))
                (cond
                  [(fx= next end) (list (substring path start next))]
                  [(memv (string-ref path next) '(#\/ #\\))
                   (let ([next (fx+ next 1)])
                     (cons (substring path start next)
                           (f next next)))]
                  [else (f start (fx+ next 1))])))))
        (define reslash
          (lambda (path)
            (let ([ls (string->list path)])
              (list->string
                (if (memq (machine-type) '(i3nt ppcnt))
                    (substq #\\ #\/ ls)
                    (substq #\/ #\\ ls))))))
        (let loop ([ls (split-path path)] [new '()])
          (cond
            [(null? ls)
             (return (reslash (apply string-append (reverse (cdr new))))
                     (car new))]
            [(and (equal? (car ls) "/") (not (null? new)))
             (loop (cdr ls) new)]
            [(equal? (car ls) "./")
             (loop (cdr ls) new)]
            [(and (equal? (car ls) "../")
                  (not (null? new))
                  (equal? (car new) "/"))
             (loop (cdr ls) new)]
            [(and (equal? (car ls) "../")
                  (not (null? new))
                  (not (equal? (car new) "../")))
             (loop (cdr ls) (cdr new))]
            [else (loop (cdr ls) (cons (car ls) new))]))))
    (massage-path
      (string-append (current-directory) "/"
        (let ((n (string-length path)))
          (if (and (> n 2) (equal? (substring path (- n 3) n) ".ss"))
              (substring path 0 (- n 3))
              path)))
      (lambda (dir fn)
        (let ([path (string-append dir fn)])
          (unless (member path fremd-load-list)
            (parameterize ([current-directory dir])
              (fremd-compile-file fn)
; using visit here again instead of load since it seems to work now
; (memory-chewing loop was probably due to misplaced compile of scrollframe
;  which *must* precede init.ss)
              (visit (string-append fn ".so")))
            (set! fremd-load-list (cons path fremd-load-list))))))))

(reset-handler abort)

(load "../../config.scheme")

#;(define-syntax require
  (syntax-rules ()
    ((_ x . ignore) (eval-when (eval) (fremd-load x)))))

; Turns out it worked better to make compile and load of the "required"
; files explicit.

(define-syntax require
  (let ([ls '()])
    (lambda (x)
      (import scheme)
      (syntax-case x ()
        [(_ file . ignore)
         #'(begin)]))))


(for-each fremd-load '(
    "module-setup"
    "../oop/class"
    "syntax"
    "../threads/threads"
    "foreign"
    "io"
    "generics"
    "tkstream"
    "base1"
    "callback"
    "event"
    "option"
   ; preferences must follow option due to the code that "parses/unparses" options <--> fonts
    "preferences"
    "base2"
    "base64"
    "image"
    "selection"  ; must precede proto.ss due to top-level module funky exports
    "proto"
    "label"
    "button"
    "frame"
    "entry"
    "scale"
    "scrollbar"
    "listbox"
    "canvas"
    "canvasitem"
    "text"
    "markup" ;;; put this after canvas and text
    "teventloop" ;; loaded after callback.ss for sake of swl:fbqueue
    "menu"       ;; after callback.ss due to swl:fbq, after teventloop.ss for swl:fallback-queue
    "../../apps/common/scrollframe"
   ; dialog uses <scrollframe>
    "dialog"
    ;; init loaded after frame for sake of balloon help
    ;; init loaded after scrollframe for sake of swl:error-log-port
    ;;  - if we fail to do this, we get nasty loop that exhausts memory
    ;;    when we have an error in a background thread (since it's trying
    ;;    to put up the error message and suddenly the error message code
    ;;    is reporting the error that <scrollframe> is not bound)
    ;; init loaded after callback.ss for sake of swl:fbqueue
    "init"
#;    "require"

;;-------------------------------------
;; removing more namespace pollution
;; "../../apps/common/arrows.ss"
;;-------------------------------------
"../../apps/common/auxio.ss"
"../../apps/common/flex-button.ss"
"../../apps/common/warning-dialog.ss"
"../../apps/common/app.ss"
;;-------------------------------------
;; now included directly by threads.ss
;; "../../apps/common/semaphore.ss"
;;-------------------------------------
"../../apps/common/popup.ss"
;;-------------------------------------
;; removing help-text class for two reasons
;;   1) no one uses it because we never got the help text in place
;;   2) it seems to have a broken word-end word-start that breaks
;;      by-word text selection (e.g. double-click and drag) in repl
;; "../../apps/common/help-text.ss"
;;-------------------------------------
"../../apps/common/app-text.ss"
"../../apps/common/scheme-text.ss"
"../../apps/repl/repl-text.ss"
"../../apps/edit/edit-text.ss"
;;-------------------------------------
;; removing the native html viewer since it was abandoned before it
;; could ever get up to snuff (no frames, limited image support, weak
;; table support, limited http protocol support, etc.)
;; "../../apps/htmlview/html.ss"
;; "../../apps/htmlview/html-text.ss"
;; "../../apps/htmlview/www.ss"
;; "../../apps/htmlview/htmlview.ss"

   "../../apps/repl/repl.ss"
   "../../apps/edit/edit.ss"

   "console.ss"

))

;(fremd-load "../../apps/repl/repl.ss")
;(collect 4)
;(fremd-load "../../apps/edit/edit.ss")
;(collect 4)
;(fremd-load "../../apps/htmlview/htmlview.ss")
;(collect 4)

(compile-file "build-script" "build-script.so")
(set! fremd-load-list (cons "build-script" fremd-load-list))

(set! fremd-load-list
  (map (lambda (x) (string-append x ".so"))
       (reverse fremd-load-list)))

(apply make-boot-file boot-file '("scheme" "petite") fremd-load-list)

(if (memq (machine-type) '(ppcnt i3nt))
    (let ([op (open-output-file "cleanso.bat" 'truncate)])
      (for-each
        (lambda (x) (fprintf op "del ~a~%" x))
        fremd-load-list)
      (close-output-port op))
    (let ([op (open-output-file "cleanso" 'truncate)])
      (for-each
        (lambda (x) (fprintf op "/bin/rm -f ~a~%" x))
        fremd-load-list)
      (close-output-port op)))
