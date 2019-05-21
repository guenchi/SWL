; Searches for prefs in cwd then in home directory.
; Saves new prefs in home directory.
; Saves updated prefs only to the file from which prefs were loaded on startup.

(define swl:load-preferences)
(define swl:save-preferences!)

(module ()

(import swl:oop)
(import swl:macros)
(import swl:option)
(import swl:threads)

(define *prefs* #f)
(define *prefs-path* #f)
(define prefs-filename ".swlprefs")

(define swlprefs-directory
 ; returns string or #f
  (lambda ()
    (or (getenv "SWL_PREFS_DIR")
        (case (machine-type)
          [(i3nt ppcnt)
           (let ([key (format "HKEY_LOCAL_MACHINE\\Software\\Chez Scheme\\swl~a\\SwlPrefsDir" swl:version)])
             (cond
               [(swl:get-registry key #t)]
               [(getenv "APPDATA") =>
                (lambda (s) (format "~a\\Scheme Widget Library" s))]
               [else
                (warning-dialog 'noblock
                  (format
                    "Unable to locate SWL preferences file.\n\nSet registry key ~a to path."
                    key))
                #f]))]
          [else (or (getenv "HOME") "~")]))))

(define (search-paths) (list current-directory swlprefs-directory))

; cute future implementation would be to represent objects
; as records and use parameterized reader syntax and custom
; print methods to manage the "serialization"

(define unparse
 ; would be nice to have a serialize method
  (lambda (x)
    (cond
      [(not (instance? x)) x]
      [(isa? x <font>)
       (with-values (send x get-actual-values)
         (lambda (family size style)
           `(font (family ,family) (size ,size) (style ,style))))]
      [(isa? x <rgb>)
       (with-values (send x rgb-values)
         (lambda (r g b)
           `(rgb ,r ,g, b)))]
      [else (assertion-violationf 'swl:set-preference! "unexpected instance ~s" x)])))

(define parse
  (lambda (x)
    (syntax-case x (font family size style rgb)
      [(font (family fam) (size sz) (style st))
       (create <font> #'fam #'sz #'st)]
      [(rgb r g b)
       (make <rgb> #'r #'g #'b)]
      [else x])))

(define map-prefs
  (lambda (convert prefs)
    (map (lambda (app-prefs)
           (cons (car app-prefs)
                 (map (lambda (kv) (cons (car kv) (convert (cdr kv))))
                      (cdr app-prefs))))
         prefs)))

(define read-preferences
  (lambda (filename)
    (on-error #f
      (map-prefs parse
        (with-input-from-file filename
          (lambda ()
            (let ([x (read)])
              (if (eof-object? x) '() x))))))))

(define write-preferences
  (lambda (filename)
   ; perhaps should lock file 
    (with-output-to-file filename
      (lambda ()
        (display "; SWL preferences (automatically generated file, manual edits may be lost)\n")
        (pretty-print (map-prefs unparse *prefs*)))
      'truncate)))

; seems this should just be (define m (make <sem> 1)) but see the mail
; I sent to Kent (titled "surprise") about strangeness of visit evaluating 
; the rhs of non-compile-time things within a module.
;** Actually, we might need this anyway since we probably can't make a
;** sem until we've run install-thread-system.
(module (m)
(define m "tut, tut, you didn't call swl:init-preferences-hack")
(set! swl:init-preferences-hack
  (lambda ()
    (set! m (make <sem> 1))
    (when (memq (machine-type) '(i3nt ppcnt))
      (set! swl:get-registry
        (let ()
          (define expand-key
            (lambda (s)
              (let ([ip (open-input-string s)] [op (open-output-string)])
                (let f ()
                  (let ([c (read-char ip)])
                    (if (eof-object? c)
                        (get-output-string op)
                        (begin
                          (case c
                            [(#\\) (write-char #\/ op)]
                            [(#\%)
                             (let ([c (read-char ip)])
                               (if (eof-object? c)
                                   (write-char #\% op)
                                   (case c
                                     [(#\u)
                                      (display
                                        (or (getenv "USERNAME") "user")
                                        op)]
                                     [(#\t)
                                      (display
                                        (or (getenv "TEMP") "c:/temp")
                                        op)]
                                     [else
                                      (write-char #\% op)
                                      (write-char c op)])))]
                            [else (write-char c op)])
                          (f))))))))
          (rec swl:get-registry
            (case-lambda
              [(s) (swl:get-registry s #f)]
              [(s expand?)
               (let ([s (get-registry s)])
                 (if (and (string? s) expand?)
                     (expand-key s)
                     s))])))))))
)

(define warn-prefs
  (lambda (verb msg)
    (warning-dialog 'noblock (format "Unable to ~s preferences.\n\n~a" verb msg))))

(define complain
 ; could arrange to put swl:version in at compile time...
  (lambda (filenames . args)
    (apply string-append
      (append args
        (map (lambda (name)
               (format "File ~a exists, but is not recognizable as SWL ~a preferences file.\n" name swl:version))
             filenames)))))

(define make-path
  (lambda (dir)
    (and dir (string-append dir "/" prefs-filename))))

(define (shallow-copy x)
  (if (pair? x)
      (cons (car x) (shallow-copy (cdr x)))
      x))

(set! swl:load-preferences
  (lambda (tag)
    (define (default-prefs msg) (warn-prefs 'load msg) '())
    (with-mutex m
      (unless *prefs*
        (set! *prefs*
          (on-error (with-message msg (default-prefs msg))
            (let loop ([ps (search-paths)] [files '()])
              (if (null? ps)
                  (if (null? files)
                      '()
                      (default-prefs (complain (reverse files))))
                  (let ([filename (make-path ((car ps)))])
                    (if (and filename (file-exists? filename))
                        (let ([prefs (read-preferences filename)])
                          (if prefs
                              (begin
                                (set! *prefs-path* filename)
                                (unless (null? files)
                                  (warning-dialog 'noblock
                                    (complain (reverse files) "Using preferences from " filename ".\n\n")))
                                prefs)
                              (loop (cdr ps) (cons filename files))))
                        (loop (cdr ps) files))))))))
      (cond
        [(assq tag *prefs*) => (lambda (a) (map shallow-copy (cdr a)))]
        [else '()]))))

(set! swl:save-preferences!
  (lambda (tag als)
    (define do-save
      (lambda ()
        (unless (string? *prefs-path*) (assertion-violationf #f "internal error in preference code (expected *prefs-path* to be set by now)"))
        (let ([path *prefs-path*])
          (if (file-exists? path)
              (if (read-preferences path)
                  (write-preferences path)
                  (warn-prefs 'save
                    (complain (list path) "Unwilling to overwrite file " path ".\n\n")))
              (write-preferences path)))))
    (with-mutex m
      (on-error (with-message msg (warn-prefs 'save msg))
        (if *prefs*
            (let ([a (assq tag *prefs*)] [new (map shallow-copy als)])
              (if a
                  (set-cdr! a new)
                  (set! *prefs* (cons (cons tag new) *prefs*))))
            (set! *prefs* (list (cons tag (map shallow-copy als)))))
        (if *prefs-path*
            (do-save)
            (let* ([directory (swlprefs-directory)]
                   [filename (make-path directory)])
              (if (not filename)
                  (warn-prefs 'save "unable to determine directory in which to save preferences.")
                  (if (file-exists? filename)
                      (warn-prefs #f
                        (format "Saving preferences would overwrite the file ~a that was not present or not recognized as a SWL ~a preferences file when preferences were loaded on startup.\n\nPlease rename that file and try again."
                          filename swl:version))
                      (begin
                        (unless (file-exists? directory)
                          (case (machine-type)
                            [(i3nt ppcnt)
                             (let ([dosdir (list->string
                                             (substv #\\ #\/
                                               (string->list directory)))])
                               (system (format "mkdir ~s" dosdir)))]
                            [else (system (format "mkdir -p ~s" directory))]))
                        (set! *prefs-path* filename)
                        (do-save))))))))))


)

#!eof

or just store the whole file as a single string in the registry?
(only affects read and write preferences then)

(swl:tcl-eval 'package 'require 'registry 1.0)
(format "HKEY_CURRENT_USER\\SWLRC\\~a\\~a" application key)

print result of (format "~s" data) as type sz

#!eof

(repl text-font (font (family helvetica) (points 10) (style bold)))
(repl text-foreground 'blue)
(repl text-background (rgb 100 0 102))
(repl text-hilight 'gray)

(repl use-the-box #f)
(repl wrap-lines #f)

