;; Copyright (c) 1996 Erik Hilsdale
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.


(printf "
inspector code out of date
 - ps, pps, ps-all now all have thread- prefix
 - threads.ss would need to export last-exception and thread-exception->k
   and thread-quantum
")

;; inspector.

;; Use:  (vinspect <some scheme object>)
;;       (vdebug)  ; grabs the last error continuation

;; Warning!  Exports the following identifiers:

;; from syntax.ss:   define-svelte-class, with-capture, tostringy,
;;                   multi-set-option!, mv-let, mv-let*, mv-set!,
;;		     simplemodule, define-constant, push-onto!

;; from util.ss:     maplist, half-pprint-and-shared,
;;		     half-pprint, flat-half-pprint,
;;		     inspector-ps, string-ps,
;;		     flatten-ps, apply-deep-ms,
;;		     pretty-print-struct, flat-pretty-print-struct

;; from hooks.ss:    inspector-rows, inspector-cols,
;;		     inspect-length, inspect-level

;; from widgets.ss:  <quiet-button>, <quiet-menubutton>,
;;		     <viewtext>, <twoscrolltext>

;; from wrappers.ss: make-inspect-object

;; from top.ss:      verb, vinspect, vdebug, vinspect-test

;(load "code/tcl-require.ss")

(unless (top-level-bound? 'require)
  (set-top-level-value! 'require (lambda (a . rest) (load a))))

; (define let-me-in! #t)	; the key to simplemodule

; (require "code/letify.ss" 'once)

(define letify (lambda (x) x))

(require "syntax.ss" 'once)
(require "hooks.ss" 'clean)
(require "widgets.ss")
(require "util.ss" 'clean)
(require "wrappers.ss" 'clean)

(require "top.ss")

