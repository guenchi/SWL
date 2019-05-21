









#!eof






(include "../common/scrollframe.ss")
(include "../common/arrows.ss")
(include "../common/select-file.ss")
(include "../common/auxio.ss")
(include "../common/warning-dialog.ss")

(include "../common/popup.ss")
(include "../common/app.ss")
(include "../common/semaphore.ss")
(include "../common/help-text.ss")

;(include "../common/app-text.ss")
;(include "../repl/repl-text.ss")
;(include "../common/scheme-text.ss")

#!eof

; (eval-when (compile) (load "../common/help-text.ss"))
(include "../common/help-text.ss")

(define-class (<foobar> p) (<help-text> p)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))

#!eof

; trying this in hopes of fixing problem during compilation where
; it bails out complaining of invalid syntax (<app-text> parent)
;
; placed in separate file so that we'll do visit or load as needed
; before trying to compile the other files.

(include "../common/scrollframe.ss")
(include "../common/arrows.ss")
(include "../common/select-file.ss")
(include "../common/auxio.ss")
(include "../common/warning-dialog.ss")
(include "../common/app.ss")
(include "../repl/repl-text.ss")
(include "../common/semaphore.ss")
(include "../common/scheme-text.ss")
(include "../common/popup.ss")
(include "../common/help-text.ss")
(include "../common/app-text.ss")

