;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(let ((suffix ".ss"))
  (define my-load
    (lambda (fn . proc)
      (let ((fn (string-append fn suffix)))
        (when (> (let ((ls (reverse (string->list fn))))
                   (if (memq #\/ ls)
                       (- (length ls) (length (memq #\/ ls)))
                       (length ls)))
                 13)
          (printf "portability problem with length of ~s~n" fn))
        (printf "loading ~a~n" fn)
        (apply load fn proc))))
  (parameterize ((reset-handler abort)
                 (case-sensitive #f))
    (my-load "syntax")
    (my-load "porthack.ss")
    (my-load "../threads/threads")
    (my-load "foreign")
    (my-load "io")
    (my-load "generics")
    (my-load "tkstream")
    (my-load "base1")
    (my-load "callback")
    (my-load "event")
    (my-load "option")
    (my-load "base2")
    (my-load "image")
    (my-load "proto")
    (my-load "label")
    (my-load "button")
    (my-load "frame")
    (my-load "init");; after frame for sake of balloon help
    (my-load "entry")
    (my-load "scale")
    (my-load "scrollbar")
    (my-load "listbox")
    (my-load "canvas")
    (my-load "canvasitem")
    (my-load "text")
    (my-load "markup") ;;; put this after canvas and text
    (my-load "menu")
    (my-load "teventloop")
    (my-load "require")))

;; If we didn't have to go back to system mode,
;; we could use $scheme-version, but bouncing in and out of
;; system mode is a nuisance.

(if (top-level-bound? 'scheme-start)
    (scheme-start swl:startup)
    (set! *scheme* swl:startup))
